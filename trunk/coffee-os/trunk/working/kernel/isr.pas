unit ISR;
//interrupt service routines 0..32 ONLY.
//One can ADD more here, but what would the point be?
//Interrupt 80 is defined in the syscalls unit.
interface

{$ASMMODE INTEL}

type
 
 PRegisters = ^TRegisters;
  TRegisters = record
    gs,fs,es,ds: LongWord;
    edi,esi,ebp,esp,ebx,edx,ecx,eax: LongWord;
    InterruptNumber,ErrorCode: byte;   
    eip,cs,eflags,useresp1,ss: LongWord;
//cr?
  end;

 TISRHandler = procedure (var r: TRegisters);

//There can be only one definition of this, so any routines needing it MUST be placed HERE.
 
  idtarray=array [0..255] of longint;
  pidtarray=^idtarray;

  handlers=array [0..255] of record
      handler,oldhandler:PtrUInt;
  end;

var
  
   buffer:array[0..1023] of byte; //1kb
   num:integer;
   ctrlbreak,ESCPressed:boolean;
   IsPagingEnabled:boolean; external;
   i:integer;
   signr:longint;    
   lock:boolean;
   

  PROCEDURE FastWrite(Col, Row, Attr : Byte; Str : String); ASSEMBLER;
procedure PageFaultHandler(var r: TRegisters); 
procedure BadTSS(var r:TRegisters);   
procedure EnableDebugTrace(var r:tregisters); 
procedure DebugRecover(var r:tregisters);
procedure Breakpoint(var r:tregisters); 
procedure Diverror(var r:tregisters);
procedure NotDos(var r:tregisters);
procedure NotValid(var r:tregisters);
procedure NotAvailable(var r:tregisters);
procedure DosDelay(var r:tregisters);  //set the eax with a longint before you call it.
procedure DiskIO(var r:Tregisters); 
procedure NMI(var r:tregisters);
procedure NotImp(var r:tregisters); 
procedure SegFault(var r: TRegisters); 
procedure CtrlBreakHandler(var r:Tregisters); 
procedure Restart(var r: TRegisters); 
procedure panic(var r: TRegisters);
procedure InterruptHandler(var r: TRegisters); 
procedure InstallISRHandler(const ISRNo: Byte; Handler: TISRHandler);
procedure RemoveISRHandler(const ISRNo: Byte);
procedure InstallISR;
procedure ISRHandler(var r: TRegisters); 
procedure SyscallHandler(var r: TRegisters); 
procedure FPUReset(var r:tregisters);
procedure disable_nmi;  external name 'disable_nmi';
procedure enable_nmi;  external name 'enable_nmi';
procedure Remap;
implementation

uses
  console,UConsole,idt,x86,vmm,rtc,commands,keybrd,timer,mouse,syscalls,pmm; 
//signals,playmusic

var
 VidMem: PChar=PChar($B8000);
//Graphics color up to 320x240x16(older DOS code)
   SegA000: PChar=PChar($A0000);
//Monochrome RAM area(EMM386 area, hardly used, maybe debugging console)
   SegB000: PChar=PChar($B0000);
  KeyStatus: TKeyStatusSet; // Byte = 0;
  { // Old implementation using single byte and bitwise operation
    bit 0 = ctrl
    bit 1 = alt
    bit 2 = shift
    bit 3 = caps lock
    bit 4 = num lock
    bit 5 = scroll lock
    bit 6 = ???
    bit 7 = ???
  }
  ActiveKeyMap, ActiveShiftedKeyMap: TKeyMap;
  ch:char;   
  IsBCD: Boolean;
  S:string;

const
 
  ExceptionMessages: array [0..31] of String = (
    'Division By Zero/Overflow',
    'Debug',
    'Non Maskable Interrupt',
    'Breakpoint',
    'Into Detected Overflow',
    'Out of Bounds',
    'Invalid Opcode',
    'No Coprocessor',
    'Double/Triple Fault',
    'Coprocessor Segment Overrun',
    'Bad TSS',
    'Segment Not Present',
    'SegFault',
    'General Protection Fault',
    'Page Fault',
    'Reserved',
    'Coprocessor Fault',
    'Alignment Check',
    'Machine Check',
    'Reserved for Reboot',
    'Reserved',
    'DosSyscall',
    'DosTerminate',
    'CTRL-Break',
    'Critical DiskIO',
    'ABS Disk Read',
    'ABS Disk Write',
    'Dos TSR',
    'Delay',
    'Fast Console Write',
    'Reserved',
    'DPMI Functions'
  );

  //weird: int80 makes 32...
  ISRRoutines: array [0..32] of TISRHandler = (
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil,nil
  );

{$S-}

  {Note: 'Col' and 'Row' are zero relative, ie:- 0,0 for Top-Left.}

  PROCEDURE FastWrite(Col, Row, Attr : Byte; Str : String); ASSEMBLER;
//broken
ASM
 
 
  END; {FastWrite}

{$S+}


procedure PageFaultHandler(var r: TRegisters); 

{If any software running in a protection level greater then 0 attempts to execute the above instructions, the processor generates a General Protection fault (#GPF) exception.

Instruction	Description
LGDT	Loads an address of a GDT into GDTR
LLDT	Loads an address of a LDT into LDTR
LTR	Loads a Task Register into TR
MOV Control Register	Copy data and store in Control Registers
LMSW	Load a new Machine Status WORD
CLTS	Clear Task Switch Flag in Control Register CR0
MOV Debug Register	Copy data and store in debug registers
INVD	Invalidate Cache without writeback
INVLPG	Invalidate TLB Entry
WBINVD	Invalidate Cache with writeback
HLT	Halt Processor
RDMSR	Read Model Specific Registers (MSR)
WRMSR	Write Model Specific Registers (MSR)
RDPMC	Read Performance Monitoring Counter
RDTSC   Read TIme Stamp Counter(64-bit)
}

var

  FaultAddr,error_code: LongWord;
  Present,NotPresent,DuringRead,DuringWrite,KernelMode,NotReserved,ReadOnly,UserMode,Reserved,NotInstrFetch,InstrFetch: Boolean;
  new:pointer;

begin
   asm
      pushad   
      mov   cr2,eax
      mov   eax,[Faultaddr]
  end;
 
// the sigsev is a mild version of GPF.It SHOULD kill PID in question.
//This prevents page faults from killing the kernel.

  writeline; 
  writestrln('Page Fault.');
  Present:=(r.ErrorCode and 0)=0;
  NotPresent:=(r.ErrorCode and 0)=1;
  DuringRead:=(r.ErrorCode and 1)=0; 
  DuringWrite:=(r.ErrorCode and 1)=1; 
  
  KernelMode:=(r.ErrorCode and 2)=0;
  UserMode:=(r.ErrorCode and 2)=1;
  NotReserved:=(r.ErrorCode and 3)=0;
  Reserved:=(r.ErrorCode and 3)=1;
  NotInstrFetch:=(r.ErrorCode and 4)=0;
  InstrFetch:=(r.ErrorCode and 4)=1;

  writestring('Faulting address = $ ');
  writestrln(HexStr(FaultAddr,8));

  writestring('Page is ');
  if Present then begin
      writestrln('present. ');
    //  vmmngr_map_page(FaultAddr,PAGE_GET_PHYSICAL_ADDRESS(FaultAddr));
         asm
	      mov   eax, cr3
      	  mov   cr3, eax 
	      popfd //Flags must be reset or will result in GPF.
	 //     leave
	      add   esp, 4
	   end;
  end;
  if not Present then begin
        Map(FaultAddr,VirtToPhys(FaultAddr),true,true,UserMode);
	asm //flush the TLB
   		mov   eax, cr3
      		mov   cr3, eax 
	        popfd
	 //     leave
	      add   esp, 4
	end;
  end;
  if UserMode then begin
	writestrln('Page reserved for user mode.You are in Kernel Mode. ');
       // SignalHandler(SIGSEGV);
	   asm
   		mov   eax, cr3
      		mov   cr3, eax //pop back to Ring3 mode
	      popfd
	 //     leave
	      add   esp, 4
	   end;
  end;
if KernelMode then begin
	writestrln('Page reserved for Kernel mode.You are in User Mode. ');
       // SignalHandler(SIGSEGV);
	   asm
   		 mov   eax, cr3
      	 mov   cr3, eax //pop back to Ring3 mode
	     popfd
	   //   leave
	     add   esp, 4
	   end;
  end;
  if Reserved then begin
	writestrln('Memory ALREADY in USE.Failure to Allocate.');
        writestring('Location: '); writelong(FaultAddr);
        asm
		popfd
		cli	
		hlt
	end;
  end;
  if InstrFetch then begin
	writestrln('Could NOT FETCH instruction');
        writestring('Location: '); writelong(FaultAddr);
        asm
		popfd
		cli	
		hlt
	end;
	
  end;
  //lets see if we trip genericly..
  asm
    cli
    hlt
  end;	 
//should be able to return to what we were doing now.No guarantees.
// --Jazz
   writeportb($20,$20);
end;


procedure BadTSS(var r:TRegisters);   

begin
        
        asm
		  jmp $33:0 //try and recover somewhat
		end;
		textcolor(red);
	writestrln('TSS code implemented poorly.Halting OS.');
	  asm
  	    cli
	    hlt
      end;
end;


procedure EnableDebugTrace(var r:tregisters); 

begin
{$define debug}

// run ONE line of code and exit.
asm   
   //include (Hyber,ProcState) in Current_Task;
   //next.task:=current.task;
end;
//mostly used by programmers
   writestring('Debug trace enabled.');
   writeportb($20,$20);
end;

procedure DebugRecover(var r:tregisters);

{$undef debug}
begin
      writestring('Debug trace disabled.');
   writeportb($20,$20);
end;


procedure Breakpoint(var r:tregisters); 

begin
  writestrln('Set Breakpoint was hit.');
  //pid:=parent_task.pid;
   writeportb($20,$20);
end;

procedure Diverror(var r:tregisters);

var
  FaultAddr:Longword;

begin
if IsConsoleActive then
   writestring('Division by Zero @: ');
   asm
      pushad   
      mov   cr2,eax
      mov   eax,[Faultaddr]
   end;
      writelongln(FaultAddr);
   asm
     //getpid(current_task);
     //kill(current_task); 
     //switch_task
      finit //re-init FPU
      fclex //clear FPU
 //   femms //clear 3DNOW FPU
      popad
      add esp,8
   end;
   writeportb($20,$20);
end;

procedure FPUReset(var r:tregisters);
begin
    asm
      finit //re-init FPU
      fclex //clear FPU
 //   femms //clear 3DNOW FPU
   end;
   writeportb($20,$20);
end;

procedure NotDos(var r:tregisters);

begin
   writestrln('This is not dos.Ignored interrupt 21.BAD CODE DETECTED.');
   writestrln('Most likely this caused by unported DOS-based code.');   
   writeportb($20,$20);
end;

procedure NotValid(var r:tregisters);

var
 FaultAddr:longword;

begin
asm
      mov   cr2,eax
      mov   eax,Faultaddr  
end;
   textcolor(red);
   writestrln('CPU Instruction not supported on this CPU.Check your code.');
   writestring('Fault @: ');
   writelongln(FaultAddr);
   writestrln(OS_LOGO);   
   asm
     cli
     hlt
   end;
end;

procedure NotAvailable(var r:tregisters);

begin
   writestrln('That processor extension is not available.');  //:-P
   writeportb($20,$20);
end;

procedure DosDelay(var r:tregisters);  //set the eax with a longint before you call it.

var
  time:longint;

begin //get the time to delay from the registers.This is a Lousy way of calling delay, but it does work.
   asm 
       mov time,eax
   end;
   Delay(time);
   writeportb($20,$20);
end;


procedure DiskIO(var r:Tregisters); 

begin
//this is here for completeness sake. It isnt needed. The HD IRQ is setup when the HD-ATA unit loads.
   writestrln('DISK IO: WHat to do?');
   asm
     cli
	 hlt
   end;
   writeportb($20,$20);
end;

procedure NMI(var r:tregisters);

{

 		Vector not disabled via CLI. Generated by NMI signal in
                hardware. This function is called in the event of a memory
                parity error or may occur in the event of other hardware
                problems or failures depending on the specific manufacturer's
                hardware. Displays the appropriate error message and halts the
                processor.This feature can be turned ON and OFF as needed. See prt0 file.

}
   
begin
	InterruptHandler(r);
	writeline;
	writeline;
	writestrln('NON-RECOVERABLE HARDWARE ERROR.');
	writestrln('System Halted.');
        disable_nmi;
        asm

		   cli 
		   hlt 
                
	    end;
	
   writeportb($20,$20);
end;

procedure NotImp(var r:tregisters); 

begin     
   writestrln('CPU Function not implemented.');
   interrupthandler(r);
   asm
      cli
      hlt
   end;
end;

procedure SegFault(var r: TRegisters); 
var
 FaultAddr: LongWord;
begin
 
  textcolor(Red);
  writestrln('Segfaulted. Memory Address we looked for was lost.');
   asm
      pushad   // NOTE: May be we could save more registers 
      mov   cr2,eax
      mov   eax,Faultaddr  
   end;

  writestring('Faulting address = $');
  writestrln(HexStr(FaultAddr,8));
  TextColor(LightGray);
  Writeline;

 // if current_task^.page_directory=longword(PageDir^[0].tableaddr) then panic(r); //(needs tasking unit)
  {if user mode then
  asm
    add esp,8
    iret
  end;}
  asm
     cli
	 hlt
  end;
end;


procedure CtrlBreakHandler(var r:Tregisters); 

var
 ctrlbreak:boolean;

//should be false before calling, as it is NOT pressed.
//Ctrl-Break, not necessarily CTR-C..
begin
   textcolor(yellow);
   writestring('^C ');
   textcolor(8);
   //the almighty flag-tripper....solves EVERYTHING.
   ctrlbreak:=true;
   //the caller routine has an 'if ctrlbreak:=true then exit;' within it, so it knows when to stop executing the task.
   //the alternate is a PAIN to code.
   writeportb($20,$20);
end;


{ Implementation to restart the OS }
procedure Restart(var r: TRegisters); 
begin
  TextColor(LightGreen);
  writestrln('Restarting OS...');
  Delay(500);

//keyboard controller cpu reset pin, more widely supported, AKA: qemu

   writeportb($64,$FE);
   writeportb($20,$20);
end;


procedure panic(var r: TRegisters);

begin
	textcolor(red);
        
	writeline;
	writestrln('I dont know how we got here, so safely shutting down.');
	delay(250); //to allow for slower sytems or runaway processes to halt.	
	//acpi code goes here. 

	//Tell Bochs/Qemu/VBox to ACPI off itself.
	writeportw( $B004, ($0 or $2000) );

        //writeportw(1,(2 or 3)); <--these are ACPI values.
	writestrln('OS Halted. You may power off now.');	
	asm
		cli
		hlt
	end; //power off instead of reboot endlessly.
end;

procedure InterruptHandler(var r: TRegisters); 
var
  FaultAddr,error_code: LongWord;

begin
  asm
      cli
      pushad   
      mov   cr2,eax
      mov   eax,[Faultaddr]
  end;

  textcolor(Red);
  with r do begin
//Give me Both. Interrupts can trip error codes as well.
   if (InterruptNumber=5) and (r.eax=$0cd) then begin
      //interesting quirk because of IBM's FINITE wisdom on int5 handling...
      writestrln('PrintScreen called.');
      exit;
   end;
    writestring(ExceptionMessages[InterruptNumber]);
    writestring('  @ location: '); writestrln(HexStr(FaultAddr,8));
 
    writestring('EAX'); writestring('  = $');writestring(HexStr(eax,8));  writestring('  EBX'); writestring('  = $');writestrln(HexStr(ebx,8));
   
    writestring('ECX'); writestring('  = $');writestring(HexStr(ecx,8));  writestring('  EDX'); writestring('  = $');writestrln(HexStr(edx,8));

    writestring('ESI'); writestring('  = $');writestring(HexStr(esi,8));  writestring('  EDI'); writestring('  = $');writestrln(HexStr(edi,8));

    writestring('ESP'); writestring('  = $');writestring(HexStr(esp,8));   writestring('  EBP'); writestring('  = $');writestrln(HexStr(ebp,8));

    writestring('CS');  writestring('  = $');writestring(HexStr(cs,8));    writestring('  DS');  writestring('  = $');writestrln(HexStr(ds,8));

    writestring('ES');  writestring('  = $');writestring(HexStr(es,8));   writestring('  FS');  writestring('  = $');writestrln(HexStr(fs,8));

    writestring('GS');  writestring('  = $');writestring(HexStr(gs,8));   writestring('  SS');  writestring('  = $');writestrln(HexStr(ss,8));

    writestring('EIP'); writestring('  = $');writestring(HexStr(eip,8));   writestring('  EFLAGS');  writestring('  = $');writestrln(HexStr(eflags,8));

//need more registers like CR0....
    writestring('User ESP'); writestring('  = $'); writestrln(HexStr(UserESP1,8)); 
    writeline;
    writestrln (OS_LOGO);
  end;
  TextColor(LightGray);
  writeline;

  while true do; //DO NOT proceed on a GPF.
end;

procedure InstallISRHandler(const ISRNo: Byte; Handler: TISRHandler);
begin
  ISRRoutines[ISRNo]:=Handler;
end;

//used mostly for re-hooking existing interrupts to do something or another..
// this is usually reserved for programmer functions only.

procedure RemoveISRHandler(const ISRNo: Byte);
begin
  if (ISRNo < 32) or (ISRNo=80) then exit; //I WILL NOT UNLOAD MYSELF!
  
  {
  begin
    case ISRNo of


This allows us to keep going no matter if user/programmer attempts to
clear the kernel out or not.We must have interrupts 0-31 and 80 point to kernel vectors
unless inside of Ring3 programming environment(such as PASCAL).We
may be able to reuse some code here....



	00:begin
	   ISRRoutines[ISRNo]:=@DivError;
	end;

	01:begin
	   ISRRoutines[ISRNo]:=@EnableDebugTrace;
	end;

	02:begin
	   ISRRoutines[ISRNo]:=@NMI;
	end;

	03:begin
	   ISRRoutines[ISRNo]:=@Breakpoint;
	end;

	04:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	05:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	06:begin
	   ISRRoutines[ISRNo]:=@NotValid;
	end;

	07:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	08:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	09:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	10:begin
	   ISRRoutines[ISRNo]:=@BadTSS;
	end;

	11:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	12:begin
	   ISRRoutines[ISRNo]:=@SegFault;
	end;

	13:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	14:begin
	   ISRRoutines[ISRNo]:=@PageFaultHandler;
	end;

	15:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	16:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	17:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	18:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	19:begin
	   ISRRoutines[ISRNo]:=@restart;
	end;

	20:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	21:begin
	   ISRRoutines[ISRNo]:=@NotDos;
	end;

	22:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	23:begin
	   ISRRoutines[ISRNo]:=@CtrlBreakHandler;
	end;

	24:begin
	   ISRRoutines[ISRNo]:=nil;
	end;

	25:begin
	   ISRRoutines[ISRNo]:=nil;
	end;

	26:begin
	   ISRRoutines[ISRNo]:=nil;
	end;

	27:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	28:begin
	   ISRRoutines[ISRNo]:=@DosDelay;
	end;

	29:begin
	   ISRRoutines[ISRNo]:=nil;
	end;

	30:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	31:begin
	   ISRRoutines[ISRNo]:=@InterruptHandler;
	end;

	80:begin
	   ISRRoutines[ISRNo]:=@SyscallHandler;
	end;
   end;{case}}
  
  ISRRoutines[ISRNo]:=nil;

end;

procedure Remap;

var
  a1,a2:byte;

begin
//THIS is for later..
// if APICisAvail then writestrln('APCI is available') else writestrln('APCI NOT available.'); 

  a1 := readportb($21);                        // save masks
  a2 := readportb($a1);

  WritePortB($20,$11); //send 11 to both(init)
  WritePortB($A0,$11);

  WritePortB($21,$20); //set master to 20
  WritePortB($A1,$28); //set slave to 28

  WritePortB($21,$04); //Call address interval 4 (8)
  WritePortB($A1,$02); //Single (cascade) mode

  WritePortB($21,$01); //ICW4 (not) needed or 8086/88 (MCS-80/85) mode
  WritePortB($A1,$01);

  WritePortB($21,$a1); //restore saved masks
  WritePortB($A1,$a2);


{New Method:
(use APIC/local APIC)
[disables PIC,required]

  WritePortB($21,ff); //Enable APIC
  WritePortB($A1,ff);

SetupAPIC;
SetupLocalAPIC;
}

end;

procedure InstallISR;
var
  handlerTbl:handlers;
  r: TRegisters;
begin
  WriteString('Installing ISR...'); tabulate; tabulate;
 
//OK, NOW..Lets setup OUR handlers.    
//these numbers correspond to the HEX longwords assigned to them. Lave the dollar sign OFF.
  InstallISRHandler(0,@DivError); 
  InstallISRHandler(1,@EnableDebugTrace); 
  InstallISRHandler(2,@NMI); 
  InstallISRHandler(3,@Breakpoint); 
  InstallISRHandler(4,@InterruptHandler); //overflow
  InstallISRHandler(5,@InterruptHandler); //BOUNDS ERROR
  InstallISRHandler(6,@NotValid); //invalid opcode
  InstallISRHandler(7,@InterruptHandler); //no coCPU
  InstallISRHandler(8,@InterruptHandler); //DOUBLE /Triple FAULT

  InstallISRHandler(9,@InterruptHandler); //segment overrun

  InstallISRHandler(10,@BadTSS); 
  InstallISRHandler(11,@InterruptHandler); //segment not present (GDT error)
  InstallISRHandler(12,@SegFault); //stackfault/machine check
  InstallISRHandler(13,@InterruptHandler); //GPF
  InstallISRHandler(14, @PageFaultHandler); //Actual Page fault 
  InstallISRHandler(15,@InterruptHandler); //reserved/unknown

  InstallISRHandler(16,@InterruptHandler); //CoCPU Fault
  InstallISRHandler(17,@InterruptHandler); //Alignment check
  InstallISRHandler(18,@InterruptHandler); //machine check
  InstallISRHandler(19,@restart); //Trip to reboot.

  InstallISRHandler(20,@InterruptHandler); //reserved
  InstallISRHandler(21,@NotDos); //DOS syscalls
  InstallISRHandler(22,@InterruptHandler); //DOS Terminate address, should be similar as error above.
  InstallISRHandler(23,@CtrlBreakHandler); 

//24,25,26,29 unhandled here

// InstallISRHandler(24,@DiskHandler); //critical IO error(disk)
//  InstallISRHandler(25,@DiskIO); //ABS disk I/O
//  InstallISRHandler(26,@DiskIO);  //ABS Disk IO
 
  InstallISRHandler(27,@InterruptHandler); //DOS TSR 
  InstallISRHandler(28,@DosDelay); //needs a variable.

//  InstallISRHandler($29,@FastWrite(X, y, TextAttr,S)); --UNTESTED

  InstallISRHandler(30,@InterruptHandler); //reserved 
  InstallISRHandler(31,@InterruptHandler); //DPMI syscalls
// interrupts 32-48 assigned to IRQ #s.They are in the IRQ unit.
  
  InstallISRHandler(80,@SyscallHandler);  //syscalls 	
  
  textcolor(Green);
  WriteStrLn('[ OK ]');
  textcolor(8);

end;

procedure SyscallHandler (var r: TRegisters);  //[public, alias : 'SyscallHandler'];
var
	eaxVal,ret:longint;
        temp:string;
begin
   writestrln('Syshandler called.');
   if SysCallsOnline=false then exit;
   // Firstly, check if the requested syscall number is valid.
   // The syscall number is found in EAX.
   if (r.eax > Maxsyscalls) or (r.eax = $00) then begin       
	UnknownSyscall;
	exit;
   end;
   // We don't know how many parameters the function wants, so we just
   // push them all onto the stack in the correct order. The function will
   // use all the parameters it wants, and we can pop them all back off afterwards.
   
   case r.eax of
	//syscalls are invoked depending on the eax value.Point to the kernel procedural EP.

	$01:begin //writestrln
                asm
    { pusha
		      push ebx; 
     		      push edi; 
     		      push esi; 
     		      push edx; 
     		      push ecx;
     		      push ebx
 //not sure about these
 		     push ds
  		     push es
  		     push fs
  	             push gs
		    }
		    //This shouls be it, the prt0.asm handler should handle the rest.
		    mov esi,eax
		    push esi
		    push eax
		    mov eax,writestrln
        	    call eax
                    pop eax
		   { pop esi
		     pop ebx; 
		     pop edi; 
		     pop esi; 
		     pop edx; 
		     pop ecx;
		     pop ebx
//pop what we pushed, but not sure about these...
		    pop  gs
  		    pop  fs
  		    pop  es
  		    pop  ds
  		    popa 
		    add esp, 20}		   
                end;
                
	end;
	$02:begin //writestring
		asm
		      { pusha
		      push ebx; 
     		      push edi; 
     		      push esi; 
     		      push edx; 
     		      push ecx;
     		      push ebx
 //not sure about these
 		     push ds
  		     push es
  		     push fs
  	             push gs
		    }
		    //This shouls be it, the prt0.asm handler should handle the rest.
		    mov esi,eax
		    push esi
		    push eax
		    mov eax,writestring
        	    call eax
                    pop eax
		   { pop esi
		     pop ebx; 
		     pop edi; 
		     pop esi; 
		     pop edx; 
		     pop ecx;
		     pop ebx
//pop what we pushed, but not sure about these...
		    pop  gs
  		    pop  fs
  		    pop  es
  		    pop  ds
  		    popa 
		    add esp, 20}  
                end;
	end;
	$03:begin //Readkey
        	asm
    { pusha
		      push ebx; 
     		      push edi; 
     		      push esi; 
     		      push edx; 
     		      push ecx;
     		      push ebx
 //not sure about these
 		     push ds
  		     push es
  		     push fs
  	             push gs
		    }
		    //This shouls be it, the prt0.asm handler should handle the rest.
		    push eax
		    mov eax,readkey
        	    call eax
                    pop eax
		   { pop esi
		     pop ebx; 
		     pop edi; 
		     pop esi; 
		     pop edx; 
		     pop ecx;
		     pop ebx
//pop what we pushed, but not sure about these...
		    pop  gs
  		    pop  fs
  		    pop  es
  		    pop  ds
  		    popa 
		    add esp, 20}		    

                end;
	end;
	$04:begin //GotoXY
	        asm
		    { pusha
		      push ebx; 
     		      push edi; 
     		      push esi; 
     		      push edx; 
     		      push ecx;
     		      push ebx
 //not sure about these
 		     push ds
  		     push es
  		     push fs
  	             push gs
		    }
		    //This shouls be it, the prt0.asm handler should handle the rest.
		    
		    push eax
		    mov eax,gotoxy
        	    call eax
                    pop eax
		   { pop esi
		     pop ebx; 
		     pop edi; 
		     pop esi; 
		     pop edx; 
		     pop ecx;
		     pop ebx
//pop what we pushed, but not sure about these...
		    pop  gs
  		    pop  fs
  		    pop  es
  		    pop  ds
  		    popa 
		    add esp, 20}    
                end;
	end;
	$05:begin //Writeline --the empty line, remember?
	        asm
		    { pusha
		      push ebx; 
     		      push edi; 
     		      push esi; 
     		      push edx; 
     		      push ecx;
     		      push ebx
 //not sure about these
 		     push ds
  		     push es
  		     push fs
  	             push gs
		    }
		    //This shouls be it, the prt0.asm handler should handle the rest.
		    mov esi,eax
		    push esi
		    push eax
		    mov eax,writeline
        	    call eax
                    pop eax
		   { pop esi
		     pop ebx; 
		     pop edi; 
		     pop esi; 
		     pop edx; 
		     pop ecx;
		     pop ebx
//pop what we pushed, but not sure about these...
		    pop  gs
  		    pop  fs
  		    pop  es
  		    pop  ds
  		    popa 
		    add esp, 20}
                end;
	end;
	06:begin //Allows to Halt OS...
	        asm
        	    cli
		        hlt
            end;
	end;
   end;

    asm
       mov eaxVal,eax //get the EAX result.
       iret //iretf to ring3
    end;
end;


procedure ISRHandler(var r: TRegisters);  [public, alias: 'ISRHandler'];
//prt0.asm stub calls us.
var
  Handler: TISRHandler;

begin
  Handler:=ISRRoutines[r.InterruptNumber];

  asm
    cli //DONT STACK INTERRUPTS.
  end;
  writestrln('INTERRUPTED.');
  if Assigned(Handler) then Handler(r) else begin;
      writestring('ISR not Implemented for interrupt: '); writelong(r.Interruptnumber); writestring(' , ');
      interrupthandler(r);
	  asm //shouldnt get here
	     cli
		 hlt
	  end;
  end;
 
  asm
    sti //DONT STACK INTERRUPTS.
  end;
  
   // The one's here are completely INTERNAL and REQUIRED for proper OS operation.

   {   SYSCALLS: Without these, nothing but the kernel has Direct HW access. The most difficult part of programming this is to remember that user level needs three lines to execute what the kernel can do directly.Direct calls in User mode WILL CAUSE GPF.Use int80 for Direct HW access.

        DOS's calls to int21 are setup in this same manner.
        The SysCall handler calls the necessary functions, which are set in stone in a table above.
	We have THREE advantages over DOS:
		1) CPU prevents Ring3 code from ruining the OS.     	
		2) Threads/processes(same thing)
		3) 32-bit or higher code and access methods.

     	--Jazz
     }	
	
end;

begin
CommandBuffer:='';
end.
