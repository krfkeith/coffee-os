unit commands;
//console available commands are HERE.
//This is the command interpreter.

interface
uses
  rtc;
type
  TCommandHistory = record
    Commands: array [0..255] of string;
    Current: Byte;
  end;


var
  CommandHistory: TCommandHistory;
    Time,Date:TTime;
  

procedure ProcessCommand(const Cmd: String);
procedure AddToHistory(const Cmd: String);
function PreviousCommand: String;
function NextCommand: String;
procedure ResetCommands;
//Function  GetDPL(Sel: Word): Word;
procedure Restart;
function IsShellCommand(const Cmd: String ): Boolean;
procedure ShowDate; 
procedure PrintHelp; 
procedure DumpRegs; 
procedure Shutdown; 
procedure ShowThanks; 
procedure ShowTime; 
procedure testvideo; 
procedure show_license;


implementation

uses
  x86,multiboot,console,UConsole,cpuid,isr, timer,playmusic;

type
  TCommands = record
    Name: String;
    Description: String;
    Proc: TProcedure;
    alias: String; //a shortcut to the original procedure.
  end;

const
  MaxCmdLen = 14; // Any better idea so I don't have to maintain this maually? NO
  { Make sure these are always alphabetically sorted as

    the search function performs binary search }
ShellCommands: array [1..13] of TCommands = (
    
    ( Name:'beep' ; Description:' BEEP!'			 ; Proc: @beep ; alias: 'beep'  ),
    ( Name:'clear'     ; Description:' Clear the screen'           ; Proc: @clearscreen; alias: 'cls' ), 
    ( Name:'cpuid'   ; Description:' Get CPU ID and Vendor'      ; Proc: @DetectCPUID; alias: 'cpuid' ),
    ( Name:'date'    ; Description:' Get current date'           ; Proc: @ShowDate; alias: 'date'   ),
    ( Name:'help'    ; Description:' You''re looking at it'      ; Proc: @PrintHelp; alias: 'help'  ),
    ( Name:'licence'    ; Description:' Software agreement'      ; Proc: @show_license; alias: 'license'  ),   
    ( Name:'regs'    ; Description:' Dump register contents'     ; Proc: @DumpRegs; alias: 'regs'   ),
    ( Name:'restart' ; Description:' Restart the OS'             ; Proc: @Restart; alias: 'reboot'    ),
    ( Name:'shutdown' ; Description:' Halts the machine'          ; Proc: @Shutdown; alias: 'halt'   ),
    ( Name:'soundcheck' ; Description:'PC Speaker test' ; Proc: @TwinkleStar; alias: 'makesomenoise' ), 
    ( Name:'testvideo' ; Description:' Test Video Routines'	; Proc: @testvideo; alias: 'testvideo'   ),
    ( Name:'thanks'  ; Description:' Give Thanks list'; Proc: @ShowThanks; alias: 'thanks' ),
    ( Name:'time'    ; Description:' Get current time'           ; Proc: @ShowTime; alias: 'time'   )
 //   ( Name:'ReadFloppy'    ; Description:' Read sector from floppy'           ; Proc: @read_Sect; alias: @read_sect)
//     ( Name:'TVDemo'    ; Description:'FreeVision Demo'           ; Proc: @tvdemo; alias: @tvdemo)
//     ( Name:'VESADemo'    ; Description:'VESA/Mouse Demo'           ; Proc: @vesademo; alias: @vesademo)
    
  );


  // Any better idea so I don't have to maintain this maually? NO.
  { Make sure these are always alphabetically sorted as
    the search function performs binary search }
  
//The commented demos are not that far off. FVision will have to be rewritten. :-(
// VESA is nearing completion and it needs DPMI for int10 thunking, as int10 doesnt do anything.


procedure show_license;


begin
  clearscreen;
  TextColor(Yellow); //if you don't like it, then change it. :-P  --Jazz
  writestrln('This software covered under BSD Modified License.');
  writestrln('This OS comes with ABSOLUTELY NO WARRANTY.This is free software.');
  writestrln('... and you are encouraged to modify and redistribute it.A portion of proceeeds ');
  writestrln('should be sent to the programming staff if sold as a courtesy');
  writestrln('for all the time and effort in keeping this FREE.');
  writestrln('Please send code updates to:');
  writestrln('Richard Jasmin (frazzledjazz@gmail.com)');
  writestrln('There is no EULA. This is the end of the Licence.');
  writeline;
  writeline;
  writestrln('This License must remain intact, as written.');
  writeline;
  writeline;
  TextColor(7);
  writeline;
  pause;
  clearscreen;
end;
  

{ Implementation to restart the OS }
procedure Restart; //[public,alias: 'Reboot'];
begin
  TextColor(LightGreen);
  writestring('Restarting OS...');

//keyboard controller cpu reset pin, more widely supported, AKA: qemu
  writeportb($64,$FE); 

end;

{ Implementation to display a shutdown msg and halt the OS }
procedure ShutDown; [public, alias : 'HALT'];
var
   mbinfo:PMultiBootInfo;

begin
  clearscreen;
  beep;  
  writestring('Shutting Down....');
  delay(500);
  //poweroff QEMU/BOCHS(FINALLY!!!)
  writeportw( $B004, ($0 or $2000) );
  Textcolor(2);
  writestrln('Failed to power off. Stopping kernel.');
  asm
      cli
      hlt
  end;
end;

{ Video unit test }
{ CoffEE OS DEMO Program, some parts done by Borland. Used with permission. }


procedure testvideo;
var
  Ch: Char;
  Done: Boolean;
  scancode:byte;

begin { program body }
  ScrollDisabled:=true;
  clearscreen;
  gotoxy(1,1);
  
  writestrln('Window and text mode demo.');
  writestrln('Floods window with key pressed.');
  writeline;

  pause;
  
    TextBackground(Blue);
    TextColor(LightGreen);
  
  clearscreen;
  inc(cursorposy,22);
  writestrln('Press ESC to continue...');
  writestrln(' Ins-InsLine  Del-DelLine    Esc-Exit');
   pause;
 
    TextColor(7);
    TextBackground(0);
    clearscreen; 
    ScrollDisabled:=false;
    scroll;
    writeline;
  
end;


procedure DumpRegs;
label
  GetEIP;

var
  Reg: array [0..7] of LongWord;
  SReg: array [0..5] of Word;
  EFLAGS,CurrentEIP: LongWord;
  DPLev:word;
begin
  
  asm
    // save registers while they are not modified by another procedure call. note that
    // depending on your compiler settings, ebp may already be trashed (stack frame)

    mov dword ptr Reg[4*0],eax
    mov dword ptr Reg[4*1],ecx
    mov dword ptr Reg[4*2],edx
    mov dword ptr Reg[4*3],ebx
    mov dword ptr Reg[4*4],esp // esp is already incorrect since it was decreased by the amount of stack space the local variables require
    mov eax,16*4+6*2+4+4
    add dword ptr Reg[4*4],eax // correct esp
    mov dword ptr Reg[4*5],ebp
    mov dword ptr Reg[4*6],esi
    mov dword ptr Reg[4*7],edi
    // save segment registers
    mov word ptr SReg[2*0],ds
    mov word ptr SReg[2*1],es
    mov word ptr SReg[2*2],cs
    mov word ptr SReg[2*3],ss
    mov word ptr SReg[2*4],fs
    mov word ptr SReg[2*5],gs
    // save EFLAGS
    pushfd
    pop dword ptr EFLAGS
    // now get eip
    call GetEIP
  GetEIP:
    pop dword ptr CurrentEIP
//needs to dump the CR registers....
  end;

  writestrln('CPU Registers: ');
  TextColor(Blue);
  writestring('EAX    = '); writestring(HexStr(Reg[0],8)); writestring('  ECX    = '); writestrln(HexStr(Reg[1],8));
  writestring('EDX    = '); writestring(HexStr(Reg[2],8)); writestring('  EBX    = '); writestrln(HexStr(Reg[3],8));
  writestring('ESP    = '); writestring(HexStr(Reg[4],8)); writestring('  EBP    = '); writestrln(HexStr(Reg[5],8));
  writestring('ESI    = '); writestring(HexStr(Reg[6],8));  writestring('  EDI    = '); writestrln(HexStr(Reg[7],8));
  writeline;
  writestring('DS     = '); writestring(HexStr(SReg[0],8)); writestring('  ES     = '); writestrln(HexStr(SReg[1],8));
  writestring('CS     = '); writestring(HexStr(SReg[2],8)); writestring('  SS     = '); writestrln(HexStr(SReg[3],8));
  writestring('FS     = '); writestring(HexStr(SReg[4],8)); writestring('  GS     = '); writestrln(HexStr(SReg[5],8));
  writestring('EFLAGS = '); writestrln(HexStr(EFLAGS,8));
  writestring('EIP    = '); writestrln(HexStr(CurrentEIP,8));

  writeline; 
  TextColor(7);
end;



procedure ShowDate;
const
  Days: array [1..7] of String = (
    'Sunday', 'Monday', 'Tuesday', 'Wednesday',
    'Thursday', 'Friday', 'Saturday'
    );
  Months: array [1..12] of String = (
    'January', 'February', 'March', 'April',
    'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December'
    );
var
  s: String;
  Day,Month:string;


begin
   asm
     cli
   end;
   writestring('The Date is: ');
   Date:=GetTime; 
   with Date do begin
      writestring(Days[DayOfWeek]);
      writestring(', ');
      writestring(Months[Month]);
      writestring(' ');
       WriteLong(DayOfMonth);
      case (DayOfMonth) of
        1: writestring('st');
        2: writestring('nd');
        3: writestring('rd');
        31: writestring('st');
        else writestring('th');
      end;
      writestring(' ');      
      writelong(Century);
      writelongln(Year);
  end;
   asm
     sti
   end;

end;


procedure ShowTime; 
var
  EST:integer=+12;

begin
   writestring('Current Time is: ');
   Time:=GetTime; 
   with Time do begin
    Hour:=Hour+EST; //Is this always ASKEW?

     if Hour>24 then begin
		inc(DayofWeek);
		inc(DayofMonth);
		dec(Hour,24);
     end;

   if Hour<10 then WriteChar('0');
     WriteLong(Hour);
    WriteChar(':');
    if Minute<10 then WriteChar('0');
    WriteLong(Minute);
    WriteChar(':');
    if Second<10 then WriteChar('0');
    WriteLongln(Second);
  end;
end;


procedure ShowThanks;
const
  ThanksList: array [1..10] of string = (
    'Brendan     (http://www.osdever.net\bkerndev)',
    'Mike        (http://www.brokenthorn.com)',
    'JamesM      (http://www.jamesmolloy.co.uk)',
    'Napalm      (www.rohitab.com/discuss/topic/24959-simple-os-example/)',
    'Jazz	 (http://www.rjasmin.net and code.google.com/coffeeos)',
    'Mario Ray from FPOS (code.google.com/fpos)',
    'Xiaoming    (http://en.skelix.org)',
    'Intel Corporation (Hardware Specifications)',
    'Bouble from DelpheneOS',
    'Linus Torvaldis(Linux Kernel[in C\CPP] ftp.kernel.org)'            
    //AnonymOS 
    //math unit dev
    //thread unit dev
);

var
  i: Byte;

begin
  clearscreen;
  writestrln('Developers of Project Coffee OS would like to thank: ');
  writeline;
  i:=0;
  repeat
	TextColor(10);   
	writestrln(ThanksList[i]);
        inc(i);
  until i=10;

//  for i:=Low(ThanksList) to High(ThanksList) do begin //Low to Hi is a BASIC method wherein 
//there usually is no other means. We can use another method that is more efficient.

//  end;

  TextColor(Yellow);
    writeline;
    writestrln('The FPC Dev Team, without whose work none of this would be possible..');
    writestrln('..and tireless work from other developers.');
  TextColor(7);
  writeline;
  
end;

procedure PrintHelp;
var
  i: Byte;
begin
  i:=1; //0 creates a nil pointer reference. Use 1 instead. --Jazz
  clearscreen;
  WriteStrLn('Internal commands:');
  writeline;
  writestring('Command:');
  tabulate;
  writestring('Alias:');
  tabulate;
  writestrln('Description:');
  
 repeat
    //unless we use gotoxy on these, they will be offset.
    with ShellCommands[i] do begin 
     WriteString(Name);
     tabulate;  
     tabulate;
     WriteString(Alias);
     tabulate;
     tabulate;
     WriteStrLn(Description);
    end;
  inc(i);
  until i=maxcmdlen; 
end;

function IsShellCommand(const Cmd: String ): Boolean;
//this originally is based on BASIC...there is another way...
//the PASCAL way...LIKE THIS. Now we dont care what order we are in, as long as there is a COMMAND and and ALIAS and
// a PROCEDURE to call..

var 
I:byte;

begin
i:=0;
repeat
 if (CMD=Shellcommands[i].name) or (cmd=shellcommands[i].alias) then begin
    ShellCommands[i].Proc; 
    IsShellCommand:=true;
    exit;
 end;
 inc(i);
until i=maxcmdlen;
IsShellCommand:=false;
end;

procedure ProcessCommand(const Cmd: String);
var
  i: Byte;
begin     

 if not IsShellCommand(LowerCase(Cmd)) then  begin
  //obviously not in internal
  if (Cmd='') or (cmd=#13) or (cmd=#10#13) then 
    writestring('coffee ?> '); //this is a debug code

  // fork(Pcmd);
  //if IOError or FileNotFound then...
	     writestring(cmd);
             writestrln(' is not recognized or found anywhere.');
     end;
end;

procedure AddToHistory(const Cmd: String);
begin
  with CommandHistory do
    if Cmd <> Commands[Current] then begin
      Move(Commands[1], Commands, 255 * 256);
      // Shift 1st - 255th up, overwriting the 0th
      Commands[255] := Cmd;
      ResetCommands;
    end;
end;

function PreviousCommand: String;
begin
  with CommandHistory do begin
    PreviousCommand := Commands[Current];
    if (Current > 0) and (Commands[Current - 1] <> '') then
      Dec(Current);
  end;
end;

function NextCommand: String;
begin
  with CommandHistory do begin
    if Current < 255 then
      Inc(Current);
    NextCommand := Commands[Current];
  end;
end;

procedure ResetCommands;
begin
  CommandHistory.Current := 255;
end;

end.

