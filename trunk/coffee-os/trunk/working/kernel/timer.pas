unit timer;
{
   
Timer code from timer unit: 

We use PORTIO, the Linux standard instead of HLT when called and int $2f(which would be FINE under DOS)
8088 -> P4 3.6Ghz accurate, too.

This solves the Borland-created timing issue.

The "GUFF" is caused wherein we need to exec the calls 110 times a sec, not 55. RT200 is caused by an overflow in the 
CRT unit in TP/BP(or delay function in FPC).The math inside is STILL 16 bits. 
WE NEVER EVER FIXED THIS. YET, even with FPC.

This is caused by using a counter to count the number of times a loop can be executed in a fixed time, 
measured by the real-time clock. When Turbo Pascal was developed it ran on machines with CPUs running at 1 to 8 MHz,
and NO thought was given to the possibility of much higher speeds; but at 200MHz the 16-bit counter overflows. 

pulled from http://www.merlyn.demon.co.uk/pas-wait.htm  

I have a workaround.This instead does not require a speed check or looped execution time.
That would check the speed of the CPU.
Instead we just yield the unused processor cycles back to the processor.(once paging is enabled)

Thank you whoever put this on merlin's page.It is VERY hard to find.

 --Jazz
}
interface
uses
   isr; //,tasking;

var
  CurrentPhase: LongWord=0;
  TimerTicks:Longint=0;
  
const
  PhasePerSecond = 1000000; //Mhz resolution
//119 ticks per second, this is NOT a ms counter, it is the SYSTEM timer.
// for ms resolution, use the RTC unit.

procedure TimerPhase(Hz: LongWord); //AFFECTS kernel performance, DO NOT CHANGE.
//Use Timer 2 instead for that.
procedure InstallTimer;
procedure Delay(ms:longint); //Working now.
procedure TimerHandler(var r: TRegisters);
Function GetClockTics : QWord; Assembler;
Procedure UTime;  
procedure IOWait(ms:longint);
procedure TimerAlarm(var r:tregisters); //used with RTC possibly..or for when you need an alarm..
function TimerExpired(AmountofTime:longint):boolean;

implementation

uses
  x86, console,UConsole, irq,idt,rtc;


Function GetClockTics : QWord; Assembler;

Asm
  rdtsc
End;


Procedure UTime;  

var
  UDays,UHours,UMinutes,USeconds:longword;


  begin
   
    UDays:=(TimerTicks mod 864000);
    UHours:=(TimerTicks mod 36000);
    UMinutes:=(TimerTicks mod 600);
    USeconds:=(TimerTicks mod 110);
    writestring('There has been: ');
    writelong(UDays);
    writestring(' days ');
    writelong(UHours);
    writestring(' hours ');
    writelong(UMinutes);
    writestring(' minutes ');
    writelong(USeconds);
    writestring(' seconds ');
    writestrln(' since boot ');
    
  end;

procedure IOWait(ms:longint);
// wait until time has passed(in ms)
 
begin//RIGHT: PORT 80. Its UNUSED. This is IO Delay After all.....
//CPU speed independant. Similar to Int$2f in DOS mode.
//Linux uses same method for IO POLLING DELAYS.
 repeat
 writeportb($80,$0);
 dec(ms);
 //if ((ms mod PhasePerSecond)=1) then inc(TimerTicks);
 until ms=0;
end;

procedure Delay(ms:longint); [public, alias: 'Delay'];

//Delay is rewrite for Tasking unit.
//Sleep + Task switch...Modify this later and update your DOS-based code.
begin
  //for process[i] in procList do begin
  // num:=getCurrProcessNum;
  // process[i].sleep:=true;
  //inc(i);
  // switchTasks;
  // if TimerTicks:=TimerTicks+ms then
  // switchToTask(num); --all of the other ones go asleep.
  //end;
  IOWait(ms); //TEMPORARY
end;

procedure TimerHandler(var r: TRegisters);
var
  nextStack:longint;
begin
	Inc(TimerTicks);   
	writelongln(TimerTicks);
//	if not IsTaskingActive then begin
	//	writeportb($20,$20);
//		exit;
//	end;
{
        if IsTaskingActive and ((TimerTicks mod PhasePerSecond)=1) then begin//one second passed
		asm
			mov esp, eax  //setup the stack
			mov eax,esp
			push eax
		end;
		switch_task;
		asm
			mov esp, eax  // return value => stack
		end;        
	end;}
         writeportb($20,$20);
end;


procedure TimerAlarm(var r:tregisters);

//tripped when time elapsed or via BIOS.
begin
   sound(600);
   delay(250);
   sound(300); //kind of cheezy, but I don't know how to make a alarm ring sound. :=P  --Jazz
   delay(250);
   sound(600);
   delay(250);
   sound(300);
   delay(250);
   nosound;
   writestrln('It"s TIME!!!!'); // or SendMessage(TimerElapsed);
 
  writeportb($20,$20); //acknowledge interrupt    
end;

function TimerExpired(AmountofTime:longint):boolean;
var
  TimetoPass,now:longint;
//works in theory, butnot in practice...hmmmm..    
begin
  now:=GetTime.Second;
  repeat   
   TimetoPass:= (((AmountofTime mod 1000)+now));
   dec(Timetopass);
  until (TimetoPass=now);
  TimerExpired:=true;
end;

procedure TimerPhase(Hz: LongWord);
var
  Divisor: LongInt;

begin
  Divisor:=1193180 div Hz;  
  //1.19Mhz in 1 sec intervals
  WritePortB($43,$34); 
  WritePortB($40,(Divisor and $FF));
  WritePortB($40,((Divisor shr 8) and $FF));
end;



procedure InstallTimer;
begin
  WriteString('Installing Timer...');
  tabulate;
  tabulate; 

  TimerPhase(PhasePerSecond);
  CurrentPhase := PhasePerSecond;
 textcolor(Green);
  WriteStrLn('[ OK ]');
 textcolor(8);
end;


end.

