unit IRQ;
//Interrupt Request Line(IRQ) setup routines for 32-bit OS.
interface

uses 
   isr; //there can be only ONE TRegisters...

type
  TIRQHandler = procedure (var r: TRegisters);

var
   IRQOnline:boolean;

procedure InstallIRQHandler(const IRQNo: Byte; Handler: TIRQHandler);
procedure RemoveIRQHandler(const IRQNo: Byte);
procedure InstallIRQ;
procedure IRQHandler(var r: TRegisters); cdecl; 
function APICisAvail:boolean;

procedure Unmap;
procedure cpuSetAPICBase(apic:longword);
function cpuGetAPICBase:longword;
procedure write_ioapic_register( const offset:word; var val:longint);
function read_ioapic_register( const offset:word):longword;

implementation

uses
  x86,console,UConsole,idt,timer,keybrd,mouse,rtc,cpuid,HWprinters,HWComs; //need these for the handler code

const
  IRQRoutines: array [0..15] of TIRQHandler = (
    nil,nil,nil,nil,nil,nil,nil,nil,
    nil,nil,nil,nil,nil,nil,nil,nil
  );



procedure InstallIRQHandler(const IRQNo: Byte; Handler: TIRQHandler);
begin
  if (IRQNo=2) or (IRQNo=9) or (IRQNo=10) or (IRQNo=11) then begin
    writestrln('IRQs 2,9 and 10-11 are reserved for internal CPU use. ACCESS DENIED.');
    exit;
  end else
  IRQRoutines[IRQNo]:=Handler;
end;

procedure RemoveIRQHandler(const IRQNo: Byte);
//similar to the old code
begin
 
 if (IRQNo >= 0) or (IRQNo<=15) then exit; //NOT REMOVING self.
 
 {
    case IRQNo of

//IRQ 2 and 9 are internally reserved.

	00:begin
	   IRQRoutines[IRQNo]:=@TimerHandler;
	end;
	01:begin
	   IRQRoutines[IRQNo]:=@KeyBoardHandler;
	end;
	03:begin
	   IRQRoutines[IRQNo]:=@Coms;
	end;
	04:begin
	   IRQRoutines[IRQNo]:=@Coms;
	end;
	05:begin
	   IRQRoutines[IRQNo]:=@Printers;
	end;
	06:begin
//	   IRQRoutines[IRQNo]:=@floppyHandler;
	   IRQRoutines[IRQNo]:=nil;
	end;
	07:begin
//	   IRQRoutines[IRQNo]:=@DummyHandler
	   IRQRoutines[IRQNo]:=nil;
	end;
	08:begin
	   IRQRoutines[IRQNo]:=@RTCHandler; //should be setup at this point.
	end;
	//9 is tied to 2 and 10-11 are intel reserved	
	12:begin
	   IRQRoutines[IRQNo]:=@MouseHandler;
	end;
	13:begin
	   IRQRoutines[IRQNo]:=@FPUReset; //works only if remapped
	end;
	14:begin
//	   IRQRoutines[IRQNo]:=@ATA0IRQHandler;
	   IRQRoutines[IRQNo]:=nil;
	end;
	15:begin
//	   IRQRoutines[IRQNo]:=@ATA1IRQHandler;
	   IRQRoutines[IRQNo]:=nil;
	end;
   end;
   end}
   IRQRoutines[IRQNo]:=nil;
end;




procedure Unmap;

var
  a1,a2:byte;

begin

  a1 := readportb($21);                        // save masks
  a2 := readportb($a1);

  WritePortB($20,$11); //send 11 to both(init)
  WritePortB($A0,$11);

  WritePortB($21,$08); //set master to 28
  WritePortB($A1,$70); //set slave to 70

  WritePortB($21,$04); 
  WritePortB($A1,$02);

  WritePortB($21,$01);
  WritePortB($A1,$01);

  WritePortB($21,$a1); //restore saved masks
  WritePortB($A1,$a2);
end;


procedure cpuSetAPICBase(apic:longword);

var
  a,d:dword;
//PAE:boolean;
begin
   a:=(apic and $fffff000) or $800;
{
if PAE then
   dword d:=(apic  shr 32) & $0f;
else
   dword d:=0;
}
//   cpuSetMSR($1B, a,d);
end;

{ 
 determines the physical address of the APIC registers page
   make sure you map it to virtual memory 
}
function cpuGetAPICBase:longword;
var
   a,d:dword;

begin
 //  cpuGetMSR($1B,&a,&d);

{
if PAE then
   cpuGetAPICBase:=(a and $fffff000)or((d and $0f) shl 32);
else
   cpuGetAPICBase:= (a and $fffff000);
}
end;


procedure write_ioapic_register( const offset:word; var val:longint);
 var
      apic_base:longword;
begin
     // tell IOREGSEL where we want to write to 
     apic_base := offset;
     // write the value to IOWIN 
     val:=(apic_base + $10);
end;

function read_ioapic_register( const offset:word):longword;
 var
      apic_base:longword;
 
begin
     // tell IOREGSEL where we want to read from 
     apic_base := offset;
     // return the data from IOWIN 
     read_ioapic_register :=(apic_base + $10);
 end;

{
need: 

cpusetMSR/cpugetMSR

Make sure you enable the APIC, by OR-ing the Spurious Interrupt Register ($00F0) with 0x100, before you try to configure anything else ;) 

}

function APICisAvail:boolean;
var
 eaxVal:dword;
 CPUID: TCPUID;
begin
    CPUID := GetCPUID; //get per CPU data,1st CPU Avail(minimum 1)
    if ((CPUID[3] and 9)=1) then APICisAvail:=true else APICisAvail:=false;
end;

procedure InstallIRQ;
begin
{OK. These required for "NORMAL" PC operations on ANY 32-bit x86 PC that can have paging. (486+)
The kicker is that all these routines must be finished or working.To set them up like this.
So, I have commented out the ones that I haven't had time to tweak just yet.
These are the only ones that you have to ever set up. Anything else goes into the ISR
(and yall wont likey be needing to add-in custom ISRs once our syscall handlers are online).

Normal console functionality is complete and so is this level of the kernel(minus drivers)
once these routines are completed.

This is a big step torwards a PASCAL OS.

}
  WriteString('Installing IRQ...');
  tabulate;
  tabulate;
 // Remap;


  InstallIRQHandler(0,@TimerHandler);
  InstallIRQHandler(1,@KeyBoardHandler);
  //InstallIRQHandler(3,@Coms);
//  InstallIRQHandler(4,@Coms);
  //InstallIRQHandler(5,@Printers);
//  InstallIRQHandler(6,@floppyHandler); (nearly complete)
 // InstallIRQHandler(7,@DummyHandler);

//RTC handler is in the RTC unit.
//10 and 11 are reserved, 9 and 2 are reserved for internal CPU use

 // InstallIRQHandler(12,@MouseHandler); 
 // InstallIRQHandler(13,@FPUReset); //trips on errors(MMX+ and similar math)

//  InstallIRQHandler(14,@ATA0IRQHandler);
//  InstallIRQHandler(15,@ATA1IRQHandler);

  IRQOnline:=true;
 textcolor(Green);
 
  WriteStrLn('[ OK ]');
 textcolor(8);
end;


procedure IRQHandler(var r: TRegisters); cdecl; [public, alias: 'IRQHandler'];
//So all we have to do is assign the IRQ, nothing like the ISR unit.
//The IRQ routines can stay inside of the units.

//IE: Keyboard, RTC, Timer... etc.
var
  Handler: TIRQHandler;
begin
  Handler:=IRQRoutines[r.InterruptNumber-32]; //take 32 from the actual int number to get the associated routine
  writestring('IRQ called.');
  asm
    cli
	hlt
  end;
  if Assigned(Handler) then Handler(r) else begin
      writestring('IRQ not Implemented for interrupt: '); writelong(r.Interruptnumber); writestring(' , ');
      interrupthandler(r);
  end;
  asm
     sti
  end;
  if r.InterruptNumber>=40 then WritePortB($A0,$20);
  WritePortB($20,$20);
  
end;


end.
