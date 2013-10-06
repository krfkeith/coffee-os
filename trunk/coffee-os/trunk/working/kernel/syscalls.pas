
{Syscall Handler (IE: Ring3->Ring0(Kernel) calls for HW)

  Set the [AH,AL]EBX as the function to set[putpixel,etc...] and the regs as follows...
  EAX[ax,al] == x,y coordinates
  ECX (other data)
  EDX == function status returned to int81[pass/failed] 
  ESI ==> string pointer(if needed)

  Int 81 is CoffeeOS specific graphmode calls for video modes. This in effect becomes X11.


I dont intend on copying Linux code verbatim, but some calls are necessary when we setup user mode.
The bare bones are here.Linux is a complex beast.Let us set our own settings.We don't need everything that Linux kernel uses,GNU or not. Not going to Re-GNU nor Re-POSIX nor Re-*NIX anything.

The syscalls and signal units are the ring 3 code.
--Jazz

}


unit syscalls;

{ Syscalls:

syscalls are run as an interrupt for ring 3 level code. Basically it says for anything the kernel runs or uses,
(like writeln/readln), call that procedure/function, but do in WITHIN a interrupt.(this one.)

Trying to call kernel level functions such as these (ring0 code) without using syscalls 
will cause a GPF if done with ring3 mode.This code uses the constant list and array.MUST be SPECIFIED.	
	
Test with Care.The code to activate Ring3 Mode is in the main kernel file.
That kernel Ring3 code INTENTIONALLY DISABLED for now. 

The only thing that has to change here are functions that use variables so that they take them 
directly off of CPU registers and they are not passed directly(indirect assignment)

DO NOT FORK a syscall.Syscalls run as Ring0, process ID 0 (kernel) ONLY.Any other types of calls will cause a GPF
while in ring3 mode.

registers open are:

[longword/longint]
ecx(ch,cl if  byte, cx if word or int)
edx(dh,dl if  byte, dx if word or int)
esi(string index)
edi(Dword index??)
..and a few other non standard registers

--Jazz

}


interface


type
 PRegisters = ^TRegisters;
 TRegisters = record
    gs,fs,es,ds: LongWord;
    edi,esi,ebp,esp,ebx,edx,ecx,eax: LongWord;
    InterruptNumber,ErrorCode: LongWord;
    eip,cs,eflags,useresp,ss: LongWord;
  end;


const

  MaxSyscalls=$06; //Must be hardcoded
  MaxsyscallsVESA=$00; //Nothing here for now, no graph modes.

var

 ebxVAL,ecxVAL:longint; 
 proc:pointer;
 ahVAL,VAL:byte; 
 syscallarray:array [0..6] of TProcedure; //the address of Procedure named X.Used with syscalls.
 
 SyscallsOnline:boolean; //no point in calling int80 if not brought online yet. (this is a Linux bugfix if calling from inside of a linux box and the VM or host OS launches an int 80 before we can handle it.)
 SysCallsOnlineVESA:boolean; 

procedure unknownSyscall; 

procedure SyscallHandlerVESA(var r:TRegisters); 
procedure init_syscalls;


implementation

uses
	console,UConsole,keybrd; //HWcoms,HW,printers,graph;

//I think that is it. Mouse is not invoked, generates an interrupt.
//This may change when we get a FS online. coms and printers are only needed here to send data out,
//incoming data trips an interrupt (and has right of way)

procedure unknownSyscall; 
// Return with ax set to 0 and carry flag set
begin
asm
	popf
	stc
	pushf
	xor ax, ax
end;
  writestrln('Unknown Syscall.Exiting.');
//we can writeln only if we do not exit as we are in kernel mode until then.
asm
	iret
end;

end;

//These two functions may need be in IRQ or ISR units instead.
//Eases headache in programming

procedure SyscallHandlerVESA (var r: TRegisters); //[public, alias : 'Syscall_HandlerVESA'];
var
	location:ptrUInt;
	ret:integer;

begin
   if SysCallsOnlineVESA=false then exit;
   // Firstly, check if the requested syscall number is valid.
   // The syscall number is found in EAX.
   if (r.eax >= MaxsyscallsVESA) or (r.eax = $00) then begin       
	UnknownSyscall;
	exit;
   end;
   // We don't know how many parameters the function wants, so we just
   // push them all onto the stack in the correct order. The function will
   // use all the parameters it wants, and we can pop them all back off afterwards.

   case r.eax of
	//syscalls are invoked depending on the eax value.Point to the kernel procedural EP.
	//these are for VESA/graphmode functions

//these are commented out until these functions, and these units are made available.
	$01:begin
        //	location:=PtrUInt(@SetMode);
	end;
	$02:begin
        //	location:=PtrUInt(@InitGraph);
	end;
	$03:begin
        //	location:=PtrUInt(@ClearViewport);
	end;
	$04:begin
        //	location:=PtrUInt(@GotoXYVesa);
	end;
	$05:begin
        //	location:=PtrUInt(@OutText);
	end;
	$06:begin
        //	location:=PtrUInt(@ShowMouse);
	end;
	$07:begin
        //	location:=PtrUInt(@HideMouse);
	end;

   end;

end;


procedure init_syscalls;
begin
   writestring('Syscalls for INT80 (User->Kernel Mode functions)   ');
   SyscallsOnline:=true; //tells ISR handler to setup task gate for int 80.
   textcolor(green);
   writestrln('[ OK ]');
   textcolor(7);
   writeline;
end;

end.
