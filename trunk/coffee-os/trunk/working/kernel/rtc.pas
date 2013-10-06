{ 
//Date and Time(internal clock and RTC functions.
 CMOS functions have been replaced by the RTC unit for 32bit PM mode.

  Original code (C) Copyright 1989, Earl F. Glynn, Overland Park, KS.  Compuserve 73257,3527.
  All Rights Reserved.  This Turbo Pascal 5.5. UNIT may be freely distributed
  for non-commercial use.

  Modifications Copywright (C) 2011 Richard Jasmin aka "Jazz" for COFFEE OS

Pascal conversion of original C code by Uranium-239 & Napalm however...
code has been Rewritten by Jazz. There was a lot missing, even from the FPC RTL, which has been now gone thru.

This unit is under heavy rewrite.

Some help from OsDev and Jazz ingenious 'pull it out of nowhere from the hidden spec manual' programming ability.

CMOS Register Programming with RTC is how you key this info up correctly.There is a lot of PIC code out there, and none of it does basic Time/Date manipulation, rather adjusts the Hertz of the clock found here on an alternate channel. YOU WILL
speed up your Time/Date if you change the WRONG clock.

This only requires a modern day Y2K compliant BIOS, which increments a Century flag every century.
THis code should not have any future issues, however, your math on the Date/Time variables might.

We DO NOT USE CMOS functions directly with the BIOS/MEMarea 0400 or similar. This is obsolete and insecure.

-------------------------------

OFFICIAL CMOS SPECS:
(for assembler)

The CMOS memory exists outside of the normal address space and cannot
contain directly executable code. 

It is reachable through IN and OUT
commands at port number 70h (112d) and 71h (113d). To read a CMOS byte,
an OUT to port 70h is executed with the address of the byte to be read and
an IN from port 71h will then retrieve the requested information.

(SEE Below)


 0A     Status Register A - Read/Write except UIP
 ==     =========================================

        bit 7 - UIP flag, Update In Progress. When set an update
        cycle is in progress and the clock/calendar cannot be
        accessed. When clear, at least 244 microseconds are
        available to access clock/calendar bytes (it's plenty of
        time even on 6MHz AT).

        bits 6-4 - divider bits that define RTC operating frequency.
        ATs have a 32.768 kHz (wrist watch) crystal to operate RTC
        and divider should be set to '010', other values will make a
        time machine from your computer.

        bits 3-0 - Rate Selection bits that define the periodic
        interrupt rate, see another table for details. Default value
        set by BIOS is '0110'.

 0B     Status Register B - Read/Write
 ==     ==============================

        bit 7 (SET) - when set to 1, any update in progress is
        aborted and a program may initialize the
        clock/calendar/alarm bytes without an update occurring.
        Setting this bit clears UIE (bit 4). Clearing bit 7 allows
        the update cycle to continue.

        bit 6 (PIE) - Periodic Interrupt Enable, when set the
        periodic interrupt will occur at the frequency specified by
        RS bits in Status Register A.

        bit 5 (AIE) - Alarm Interrupt Enable, when set the alarm
        interrupt will be asserted once for each second that the
        current time matches the alarm time.

        bit 4 (UIE) - Update-ended Interrupt Enable, when set the
        update-ended interrupt will be asserted once each second
        after the end of update cycle. This bit is cleared when SET
        bit goes high but it is not reset when SET is cleared.

        bit 3 (SQWE) - Square Wave Enable, when set, enables the
        square wave output on the SQW pin at the frequency specified
        by the RS bits in the Status Register A. 

*** 	NOTE: The SQW pin is not
        connected to anything in the AT.
***

        bit 2 (DM) - Data Mode, indicates mode for clock/calendar
        data: 0=BCD and 1=binary, BIOS setting is 0.

        bit 1 (24/12) - controls hours byte, 0=12-hour and 1=24-hour
        format, BIOS setting is 1.

        bit 0 (DSE) - Daylight Savings Enable, when set two special
        updates will occur: last Sunday in April time will go
        01:59:59 > 03:00:00 and last Sunday in October 01:59:59 >
        01:00:00. BIOS sets it to 0 (ie. no daylight saving).

 0C     Status Register C - Read-only
 ==     =============================

        bit 7 (IRQF) - Interrupt Request Flag, when set one of the
        interrupts enabled in Status Register B has occurred.

        bit 6 (PF) - Periodic interrupt Flag, when set the periodic
        interrupt has occurred.

        bit 5 (AF) - Alarm interrupt Flag, when set the alarm
        interrupt has occurred.

        bit 4 (UF) - Update-ended interrupt Flag, when set the
        update-ended alarm interrupt has occurred.

        NOTE: PF, AF, UF are set regardless of corresponding enable
        bits in Status Register B. IRQF will be set only if the
        interrupt flag and its corresponding enable bit are set.
        These four flags are cleared each time Status Register C is
        read.

        bits 3-0 - reserved, always 0.

}
unit rtc;

interface
uses
   isr;
type
  TTime = record
    Second, Minute, Hour, DayOfWeek, DayOfMonth, Month, Year,Century: byte;
  end;

const
   DaysOfMonth: array [1..12] of Byte = (31,28,31,30,31,30,31,31,30,31,30,31);
  WeeksInAYear=52;
var
  MaxDaysThisMonth: Byte = 0; //should vary by month. 
  y2k:boolean;
  IsSetupRTC:boolean;
  RTCTicks:Longword =0;
  century:Longint;
  AmountofTime,StartTimer,TZSeconds:longint;
  IsBCD:boolean;
  GlobalTime: TTime;

function IsLeapYear(Year: Word): boolean;
function GetTime: TTime;
procedure InstallRTC;
function ReadRegister(Reg: Byte): Byte;
procedure WriteRegister(Reg, Value: Byte);
function BCDToBin(BCD: Byte): Byte;
procedure RTCHandler(var r: TRegisters);

implementation
uses
  x86, console,UConsole,irq;


procedure RTCHandler(var r: TRegisters);
var
  RTCTicks:longint; //About = to 7 days.Just a heads up.

begin
if (ReadRegister($0C) and $40) <> 0 then begin
  with GlobalTime do begin
      Inc(second);

  end;
 // if TimerExpired(AmountOfTime,StartTimer) then 
//	RTCTimerAlarm;
    

//clear the 'C' register of the RTC, so we can interrupt again.
// if we dont do it, it wont happen again.
// --from OSDev wiki.

end;
   readregister($0C);
//   writeportb($20,$20);
end;


function ReadRegister(Reg: Byte): Byte;
begin
  WritePortB($70, Reg);
  ReadRegister := ReadPortB($71);
end;

procedure WriteRegister(Reg, Value: Byte);
begin
  WritePortB($70, Reg);
  WritePortB($71, Value);
end;

function BCDToBin(BCD: Byte): Byte;
begin
  BCDToBin := ((BCD shr 4) * 10) + (BCD and $0F);
end;

procedure DisableTimer;

begin
  writeportb($21, readportb($21) or 1);
end;

procedure EnableTimer;
begin
  writeportb($21, readportb($21) and not 1 );
end;

function GetTime: TTime; [public, alias: 'GetTime']; //inline;
//Now for the update interval..it keeps setting the info incorrectly.
begin
   if ReadRegister($0C) and $40 <> 0 then begin
    asm
       cli
    end;
    with GlobalTime do begin
      if IsBCD then begin
        Second := BCDToBin(ReadRegister($00));
        if Second>60 then begin
		inc(Minute);
		dec(second,60);
	end;
        Minute := BCDToBin(ReadRegister($02));
        if Minute>60 then begin
		inc(Hour);
		dec(Minute,60);
	end;
	Hour := BCDToBin(ReadRegister($04) and 2); //corrected was giving erroreaneous results.
        if Hour>24 then begin
		inc(DayofWeek);
		inc(DayofMonth);
		dec(Hour,24);
	end;
        Month := BCDToBin(ReadRegister($08));
        Year := BCDToBin(ReadRegister($09));
        Century := BCDToBin(ReadRegister($37));
        DayOfWeek := BCDToBin(ReadRegister($06));
        DayOfMonth := BCDToBin(ReadRegister($07));
      end else begin
        Second := ReadRegister($00);
        if Second>60 then begin
		inc(Minute);
		dec(second,60);
	end;
       Minute := BCDToBin(ReadRegister($02));
 	if Minute>60 then begin
		inc(Hour);
		dec(Minute,60);
	end;
        Hour := BCDToBin(ReadRegister($04) and 2);
        if Hour>24 then begin
		inc(DayofWeek);
		inc(DayofMonth);
		dec(Hour,24);
	end;
        Month := BCDToBin(ReadRegister($08));
        Year := BCDToBin(ReadRegister($09));
        Century := BCDToBin(ReadRegister($37));
        DayOfWeek := BCDToBin(ReadRegister($06));
        DayOfMonth := BCDToBin(ReadRegister($07));
      
      end;
    end;{GlobalTime}
      GetTime := GlobalTime;
end;{event}
    asm
      sti
    end;

end;

procedure InstallRTC; 

var
  Status: Byte;
  prev:byte;

begin
  Writestring('Installing CMOS Clock(and stopwatch timer)...');
  tabulate;
  tabulate;
  asm
    cli
  end;
 //Default timer interrupt rate
  Status:=ReadRegister($0B);
  Status:=Status and $02;               // 24 Hour clock
  WriteRegister($0B,Status);

  Status:=ReadRegister($0B);
  Status:=Status and $10;              //  update ended interrupts $10
  WriteRegister($0B,Status);

  Status:=ReadRegister($0B);
  Status:=Status and not $20;            //  no alarm interrupts, otherwise ( and $20) , if on trips alarm.
  WriteRegister($0B,Status);

  Status:=ReadRegister($0B);
  Status:=Status or $40;               // enable periodic interrupt
  WriteRegister($0B,Status);

  IsBCD:=Boolean(not(Status and $04)); // check if data type is BCD

  ReadRegister($0C);

  InstallIRQHandler(8,@RTCHandler);

  prev:=ReadRegister($0B); //set the index to register B
  WriteRegister($0B,(prev or $40));  //set the index again(a read will reset the index to register D)
   //write the previous value or'd with 0x40. This turns on bit 6 of register D

  IsSetupRTC:=true;
  textcolor(Green);
  WriteStrLn('[ OK ]');
  textcolor(8);
end;

  TYPE
    ClockValue    =
      RECORD
        year      :  1900..2079;
        month     :  1..12;
        day       :  1..31;
        hour      :  0..23;
        minute    :  0..59;
        second    :  0..59;
        hundredth :  0..99;
      END;
//        FUNCTION  Elapsed:  REAL;   {elapsed timer (seconds)}
//        PROCEDURE Start (ct:  ClockType);
     
  CONST Digit:  ARRAY[0..9] OF CHAR = '0123456789';
  VAR c  :  CHAR;

    CONST
      days  :  ARRAY[0..6] OF STRING[9]
                         =('Sunday','Monday','Tuesday','Wednesday',
                           'Thursday','Friday','Saturday');
      months:  ARRAY[1..12] OF STRING[9]
                         =('January','February','March',
                           'April',  'May',     'June',
                           'July',   'August',  'September',
                           'October','November','December');
 
{

  FUNCTION  Elapsed:  REAL;
    VAR now:  ClockValue;
  BEGIN
    GetDateTime (now,mode);
    Elapsed := TimeDiff(now,StartValue)
  END;

  PROCEDURE Start (ct:  ClockType);
  BEGIN
    mode := ct;
    GetDateTime (StartValue, ct)
  END;
}
 
const
   HoursPerDay = 24;
   MinsPerHour = 60;
   SecsPerMin  = 60;
   MSecsPerSec = 1000;
   MinsPerDay  = HoursPerDay * MinsPerHour;
   SecsPerDay  = MinsPerDay * SecsPerMin;
   MSecsPerDay = SecsPerDay * MSecsPerSec;

//TDateTime, often used with Borland is an obsolete method used to calc time and date.
//We dont need it and its based off of EXCEL(M$) code.
//This is 2011, after all, we dont care for any dates long before computers existance..
   MonthDays: array [1..2,1..12] of integer =
     ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
      (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

type 

   TMonthNameArray = array [1..12] of  String;
   TWeekNameArray = array [1..7] of  String;

TFormatSettings = record
  CurrencyFormat: Byte;  //Currency format string
  NegCurrFormat: Byte;  //Negative currency format string
  ThousandSeparator: Char;  //Thousands separator character
  DecimalSeparator: Char; //	Decimal separator character
  CurrencyDecimals: Byte; //Currency decimals
  DateSeparator: Char; //Date separator character
  TimeSeparator: Char; //Time separator character
  ListSeparator: Char;  //List separator character
  CurrencyString: String;  //Currency string
  ShortDateFormat: String;  //Short date format string
  LongDateFormat: String;  //Long Date Format string
  TimeAMString: String;  // AM time indicator string
  TimePMString: String;  //PM time indicator string
  ShortTimeFormat: String; //Short time format string
  LongTimeFormat: String; //Long time format string
  ShortMonthNames: TMonthNameArray;  //Array with short month names
  LongMonthNames: TMonthNameArray;  //Array with long month names
  ShortDayNames: TWeekNameArray;  // Array with short day names
  LongDayNames: TWeekNameArray; //Long day names
  TwoDigitYearCenturyWindow: Word; // Value for 2 digit year century window
end;

type
ptimezone = ^timezone;

 timezone = packed record
  minuteswest: LongInt; //Minutes west of GMT
  OnDST: boolean;  //Daylight savings time
end;

 TSystemTime = record

  Year: Word; //Year part
  Month: Word;  //Month part
  Day: Word; //Day of month part
  Hour: Word; //Hour of the day
  Minute: Word; //Minute of the hour
  Second: Word;  //Second of the minute
  MilliSecond: Word; //Milliseconds in the second

end;

const
  DaysPerWeek        = 7;
  WeeksPerFortnight  = 2;
  MonthsPerYear      = 12;
  YearsPerDecade     = 10;
  YearsPerCentury    = 100;
  YearsPerMillennium = 1000;

  // ISO day numbers.
  DayMonday    = 1;
  DayTuesday   = 2;
  DayWednesday = 3;
  DayThursday  = 4;
  DayFriday    = 5;
  DaySaturday  = 6;
  DaySunday    = 7;

  // Fraction of a day
  OneHour        = 1/HoursPerDay;
  OneMinute      = 1/MinsPerDay;
  OneSecond      = 1/SecsPerDay;
  OneMillisecond = 1/MSecsPerDay;

  { This is actual days per year but you need to know if it's a leap year}
  DaysPerYear: array [Boolean] of Word = (365, 366);

  { Used in RecodeDate, RecodeTime and RecodeDateTime for those datetime }
  {  fields you want to leave alone }
  RecodeLeaveFieldAsIs = High(Word);

  { Average over a 4 year span. Valid for next 100 years }
  ApproxDaysPerMonth: Double = 30.4375;
  ApproxDaysPerYear: Double  = 365.25;

const
  TDateTimeEpsilon = 2.2204460493e-16;

Function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
begin
  IsValidDate:=(AYear<>0) and (AYear<10000)
          and (AMonth <12) and (AMonth >=1) 
          and (ADay<>0);

end;


Function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  IsValidTime:=(AHour=HoursPerDay) and (AMinute=0) and (ASecond=0) and (AMillisecond=0);
  IsValidTime:=IsValidTime or
          ((AHour<HoursPerDay) and (AMinute<MinsPerHour) and (ASecond<SecsPerMin) and
           (AMillisecond<MSecsPerSec));
end;


Function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  IsValidDateTime:=IsValidDate(AYear,AMonth,ADay) and
          IsValidTime(AHour,AMinute,ASecond,AMillisecond)
end;


Function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
begin
  IsValidDateDay:=(AYear<>0) and (ADayOfYear<>0) and (AYear<10000) and
          (ADayOfYear<=DaysPerYear[IsLeapYear(AYear)]);
end;


Function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
begin
  IsValidDateWeek:=(AYear<>0) and (AYear<10000)
          and (ADayOfWeek in [1..7])
          and (AWeekOfYear<>0)
          and (AWeekOfYear<=WeeksInaYear);
  { should we not also check whether the day of the week is not
    larger than the last day of the last week in the year 9999 ?? }
end;


Function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;

begin
  IsValidDateMonthWeek:=(AYear<>0) and (AYear<10000)
          and (AMonth in [1..12])
          and (AWeekOfMonth in [1..5])
          and (ADayOfWeek in [1..7]);
end;


Function Today: integer;
begin
  Today:=GlobalTime.DayOfMonth;
end;


Function Yesterday: integer;
begin
  Yesterday:=GlobalTime.DayOfMonth-1;
end;

Function Tomorrow: integer;
begin
 Tomorrow:=GlobalTime.DayOfMonth+1;
end;


const whitespace  = [' ',#13,#10];
      hrfactor    = 1/(24);
      minfactor   = 1/(24*60);
      secfactor   = 1/(24*60*60);
      mssecfactor = 1/(24*60*60*1000);

//as in if leap(2005) then....
 function Leap(Y:longword): boolean;
 begin
  if (Y mod 400) = 0 then
   Leap := true
  else
   if ((Y mod 100) = 0) or ((Y mod 4) <> 0) then
    Leap := false
   else
    Leap := true;
 end;

procedure  SetTime(Hour,Minute,Second,sec100:word);
var
  dow,Year, Month, Day : Word;
begin
//  fpSettimeofday(@tv,nil);
end;

procedure SetDate(Year,Month,Day:Word);
var
  Hour, Min, Sec, Sec100 : Word;
begin
 // fpSettimeofday(@tv,nil);
end;


{   DayOfWeek returns the Day of the week (sunday is day 1)  }

function DayOfWeek(DateTime: TDateTime): integer;
begin
  DayOfWeek:= 1 + (Abs(Trunc(DateTime) - 1) mod 7);
end ;

{   IncAMonth is the same as IncMonth, but operates on decoded date  }

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer );
var
  TempMonth, S: Integer;
begin
  NumberOfMonths:=1;
  If NumberOfMonths>=0 then
    s:=1
  else
    s:=-1;
  inc(Year,(NumberOfMonths div 12));
  TempMonth:=Month+(NumberOfMonths mod 12)-1;
  if (TempMonth>11) or
     (TempMonth<0) then
   begin
     Dec(TempMonth, S*12);
     Inc(Year, S);
   end;
  Month:=TempMonth+1;          {   Months from 1 to 12   }
 // If (Day>MonthDays[Month][IsLeapYear(Year)]) then
   // Day:=MonthDays[Month][IsLeapYear(Year)];
end;

{  IsLeapYear returns true if Year is a leap year   }

function IsLeapYear(Year: Word): boolean;
begin
  IsLeapYear := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

end.

