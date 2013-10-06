unit TIME;
//Date and Time(internal clock, but not display of RTC functions.

 {This UNIT provides REXX-like
  date/time formatting functions in FPC. CMOS functions have been replaced by the RTC unit for 32bit PM mode.

  (C) Copyright 1989, Earl F. Glynn, Overland Park, KS.  Compuserve 73257,3527.
  All Rights Reserved.  This Turbo Pascal 5.5 UNIT may be freely distributed
  for non-commercial use.

  Modifications Copywright (C) 2011 Richard jasmin aka "Jazz"

}

INTERFACE

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
    ClockType     =  (CMOSClock);
    Clock         =
      OBJECT
        mode      :  ClockType;
        StartValue:  ClockValue;
        FUNCTION  Date(s:  STRING):  STRING;
        FUNCTION  Elapsed:  REAL;   {elapsed timer (seconds)}
        PROCEDURE Start (ct:  ClockType);
        FUNCTION  Time(s:  STRING):  STRING;
      END;

  FUNCTION  DateFormat(s:  STRING; clk:  ClockValue):  STRING;
  FUNCTION  DaysSince1900(y, m, d:  WORD):  WORD;
  FUNCTION  hhmmss(seconds:  REAL):  STRING;
  FUNCTION  JulianDate(y{1900..}, m{1..12}, d{1..31}:  WORD):  WORD;
  PROCEDURE SetClock (yr,mo,d,h,m,s,hth:  WORD; VAR t:  ClockValue);
  FUNCTION  TimeDiff(t2,t1:  ClockValue):  REAL;  {t2 - t1 seconds}
  FUNCTION  TimeFormat(s:  STRING; clk:  ClockValue):  STRING;
  PROCEDURE UnPackTime (TurboTime:  LongInt; VAR Clk:  ClockValue);

IMPLEMENTATION

  CONST Digit:  ARRAY[0..9] OF CHAR = '0123456789';

  VAR c  :  CHAR;

  FUNCTION TwoDigits (w:  WORD):  STRING;
  BEGIN
    w := w MOD 100;  {just to be safe}
    TwoDigits := Digit[w DIV 10] + Digit[w MOD 10]
  END {TwoDigits};

  FUNCTION DateFormat(s:  STRING; clk:  ClockValue):  STRING;
    CONST
      days  :  ARRAY[0..6] OF STRING[9]
                         =('Sunday','Monday','Tuesday','Wednesday',
                           'Thursday','Friday','Saturday');
      months:  ARRAY[1..12] OF STRING[9]
                         =('January','February','March',
                           'April',  'May',     'June',
                           'July',   'August',  'September',
                           'October','November','December');
    VAR temp:  WORD;
  BEGIN
    IF   LENGTH(s) = 0
    THEN c := 'N' {NORMAL}
    ELSE c := UpCase(s[1]);
    CASE c OF
            {Normal (default):  dd Mmm yyyy -- no leading zero or blank}
      'N':  DateFormat := W2C(clk.day) + ' ' + COPY(months[clk.month],1,3)
                                       + ' ' + W2C(clk.year);

            {Century:  ddddd -- no leading zeros or blanks}
      'C':  DateFormat := W2C( DaysSince1900(clk.year,clk.month,clk.day) );

            {Days -- Julian date:  ddd -- no leading zeros or blanks}
      'D':  DateFormat := W2C(JulianDate(clk.year,clk.month,clk.day));

            {European:  dd/mm/yy}
      'E':  DateFormat := TwoDigits(clk.day  )  + '/' +
              TwoDigits(clk.month)  + '/' + TwoDigits(clk.year MOD 100);

            {Julian (IBM "OS" format):  yyddd}
      'J':  BEGIN
              temp := JulianDate(clk.year,clk.month,clk.day);
              DateFormat := TwoDigits(clk.year MOD 100) +
                Digit[temp DIV 100] + TwoDigits(temp MOD 100)
            END;

            {Month:  current month name in mixed case}
      'M':  DateFormat := months[clk.month];

            {Ordered:  yy/mm/dd suitable for sorting}
      'O':  DateFormat := TwoDigits(clk.year MOD 100)  + '/' +
              TwoDigits(clk.month)  + '/' + TwoDigits(clk.day);

            {Standard:  yyyymmdd -- suitable for sorting (ISO/R 2014-1971)}
      'S':  DateFormat := W2C(clk.year) + TwoDigits(clk.month) +
              TwoDigits(clk.day);

            {USA:  mm/dd/yy}
      'U':  DateFormat := TwoDigits(clk.month)  + '/' +
              TwoDigits(clk.day  )  + '/' + TwoDigits(clk.year MOD 100);

            {Weekday:  returns day of the week in mixed case}
      'W':  DateFormat :=  {January 1, 1900 was a Monday}
              days[DaysSince1900(clk.year,clk.month,clk.day) MOD 7 ]

      ELSE DateFormat := ''
    END
  END {DateFormat};

  FUNCTION DaysSince1900(y, m, d:  WORD):  WORD;

  {This function was written to determine the numeric value for the REXX
   language DATE('Century') function.  This function is called by the
   DateFormat function.

   Jan 1, 1900 = 1, Jan 2, 1900 = 2, ..., Jun 5, 2079 = 65535 (largest word).
   Jan 1, 1989 = 32508, Sep 18, 1989 = 32768, Jan 1, 1990 = 32873.

   "The Astronomical Almanac", published jointly by the U.S. Naval
   Observatory and Her Majesty's Royal Greenwich Observatory, defines the
   astronomical julian day number (see pp. K1-K4), to be the number of mean
   solar days since 4713 BC January 1 at Greenwich mean noon.  In this system
     Jan 1, 1900 = 2415020.5, Jan 1, 2000 = 2451544.5,
     Jan 1, 1989 = 2447527.5, Jan 1, 1990 = 2447892.5,
     Jun 5, 2079 = 2480554.5.  These data were used to validate this function.

   (Note:  DaysSince1900(y,m,d) MOD 7  returns day-of-week index, i.e.,
   0=Sunday, 1=Monday, etc. since January 1, 1900 was a Monday.)}
  BEGIN
    DaysSince1900 := 365*(y-1900) + INTEGER(y-1901) DIV 4 + JulianDate(y,m,d)
  END {DaysSince1900};

  FUNCTION  hhmmss(seconds:  REAL):  STRING;
    {Convert elapsed times/time differences to [hh:]mm:ss format}
    VAR
      h,h1,h2:  LONGINT;
      s      :  STRING;
      t      :  LONGINT;
  BEGIN
    IF   seconds < 0.0
    THEN BEGIN
      seconds := ABS(seconds);
      s := '-'
    END
    ELSE s:= '';
    h1 := 0;
    WHILE seconds > 2147483647.0 DO BEGIN  {fixup real-to-LONGINT problem}
      seconds := seconds - 1576800000.0;   {subtract about 50 years }
      h1 := h1 + 438000 {hours}            {add about 50 years}
    END;
    t := TRUNC(seconds);
    h2 := t DIV 3600;  {hours}
    h := h1 + h2;
    IF   h > 0
    THEN s := s + L2C(h) + ':';
    t := t - h2*3600;  {minutes and seconds left}
    hhmmss := s + TwoDigits(t DIV 60) + ':' + TwoDigits(t MOD 60)
  END {hhmmss};

  FUNCTION JulianDate(y{1900..}, m{1..12}, d{1..31}:  WORD):  WORD;
    {This function returns the numeric value for REXX DATE('Days') function.}
    CONST
      julian:  ARRAY[0..12] OF WORD =
               (0,31,59,90,120,151,181,212,243,273,304,334,365);
    VAR jd:  WORD;
  BEGIN
    jd := julian[m-1] + d;
    IF   (m > 2) AND (y MOD 4 = 0) AND (y <> 1900) {AND (y <> 2100)}
    THEN INC (jd);   {1900 and 2100 are not leap years; 2000 is}
    JulianDate := jd
  END {JulianDate};

  PROCEDURE SetClock (yr,mo,d,h,m,s,hth:  WORD; VAR t:  ClockValue);
  BEGIN
    t.year      := yr;
    t.month     := mo;
    t.day       := d;
    t.hour      := h;
    t.minute    := m;
    t.second    := s;
    t.hundredth := hth
  END {SetClock};

  FUNCTION  TimeDiff(t2,t1:  ClockValue):  REAL;
  BEGIN  {REAL arithmetic is used to avoid INTEGER/LONGINT overflows}
    TimeDiff :=   0.01*INTEGER(t2.hundredth - t1.hundredth) +
                       INTEGER(t2.second    - t1.second   ) +
                  60.0*INTEGER(t2.minute    - t1.minute   ) +
                3600.0*INTEGER(t2.hour      - t1.hour     ) +
               86400.0*LONGINT(DaysSince1900(t2.year,t2.month,t2.day) -
                       LONGINT(DaysSince1900(t1.year,t1.month,t1.day)))
  END {TimeDiff};

  FUNCTION  TimeFormat(s:  STRING; clk:  ClockValue):  STRING;
    VAR meridiem:  STRING[2];
  BEGIN
    IF   LENGTH(s) = 0
    THEN c := 'N' {NORMAL}
    ELSE c := UpCase(s[1]);
    CASE c OF

            {Normal (default):  hh:mm:ss}
      'N':  TimeFormat := TwoDigits(clk.hour  )  + ':' +
              TwoDigits(clk.minute)  + ':' + TwoDigits(clk.second);

            {Civil:  hh:mxx, for example:  11:59pm}
      'C':  BEGIN
              IF   clk.hour < 12
              THEN BEGIN
                meridiem := 'am';  {ante meridiem}
                IF   clk.hour = 0
                THEN clk.hour := 12;  {12:00am is midnight}
              END                     {12:00pm is noon}
              ELSE BEGIN
                meridiem := 'pm';  {post meridiem}
                IF   clk.hour > 12
                THEN clk.hour := clk.hour - 12
              END;
              TimeFormat := W2C(clk.hour)  + ':' +
                TwoDigits(clk.minute)  + meridiem
            END;

            {Hours:  hh -- number of hours since midnight}
      'H':  TimeFormat := W2C(clk.hour);

            {Long:  hh.mm:ss.xx (real REXX requires microseconds here)}
      'L':  TimeFormat := TwoDigits(clk.hour  )  + ':' +
              TwoDigits(clk.minute)  + ':' + TwoDigits(clk.second)  + '.' +
              TwoDigits(clk.hundredth);

            {Minutes:  mmmm -- number of minutes since midnight}
      'M':  TimeFormat := W2C(60*clk.hour + clk.minute);

            {Seconds:  sssss -- number of seconds since midnight}
      'S':  TimeFormat := L2C( 3600*LONGINT(clk.hour)
               + 60*LONGINT(clk.minute) + LONGINT(clk.second) )

      ELSE TimeFormat := ''
    END
  END {TimeFormat};


  PROCEDURE GetDateTime (VAR c:  ClockValue; ct:  ClockType);
    VAR r1,r2:  Registers;

    FUNCTION BCD (k:  BYTE):  WORD;    {convert binary-coded decimal}
    BEGIN
      BCD := 10*(k SHR 4) + (k AND $0F)
    END {BCD};
                   {See Que's "DOS and BIOS Functions Quick Reference",}
  BEGIN            {pp. 65-66, 96-97.}
    CASE ct OF
      CMOSClock:
        BEGIN
          asm
             mov AH ,04
             INT $1A      {BIOS call:  read date from real-time clock}
             mov AH,02
             Int $1A      {BIOS call:  read real-time clock}
	  end;
          SetClock (100*BCD(r1.CH) + BCD(r1.CL) {yr},
                    BCD(r1.DH) {mo}, BCD(r1.DL) {day},
                    BCD(r2.CH) {h},  BCD(r2.CL) {m}, BCD(r2.DH) {s},
                    0 {.00}, c)
        END;
     
    END
  END {GetDateTime};


  FUNCTION  Clock.Elapsed:  REAL;
    VAR now:  ClockValue;
  BEGIN
    GetDateTime (now,mode);
    Elapsed := TimeDiff(now,StartValue)
  END {Clock.Elapsed};

  PROCEDURE Clock.Start (ct:  ClockType);
  BEGIN
    mode := ct;
    GetDateTime (StartValue, ct)
  END {Clock.Start};

interface
type

   TValueRelationship = -1..1;


  DateTime = packed record

  Year: Word; //Year part
  Month: Word; //Month of the year
  Day: Word; //Day of the month
  Hour: Word; //Hour of the day
  Min: Word; //Minute of the hour
  Sec: Word; //Second in the minute

end;

TTime=clong;

var
  Year: Word; //Year part
  Month: Word;  //Month part
  DayOfMonth: Word; //Day of month part
  Hour: Word; //Hour of the day
  Minute: Word; //Minute of the hour
  Second: Word;  //Second of the minute
  MilliSecond: Word; //Milliseconds in the second

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;




function Leap(Y:longword):boolean;
procedure PackTime (var T: DateTime; var P: longint);
procedure UnpackTime (P: longint; var T: DateTime);
Function GregorianToJulian(Year,Month,Day:Longint):LongInt;
Function LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
function WeekDay (y,m,d:longint):longint;
Procedure GetDate(Var Year, Month, MDay, WDay: Word);
procedure  SetTime(Hour,Minute,Second,sec100:word);
procedure SetDate(Year,Month,Day:Word);
Function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;
Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
Procedure UnixDateToDt(SecsPast: LongInt; Var Dt: DateTime);
Function DTToUnixDate(DT: DateTime): LongInt;


 

type
  PDayTable = ^TDayTable;
  TDayTable = array[1..12] of Word;

const
   HoursPerDay = 24;
   MinsPerHour = 60;
   SecsPerMin  = 60;
   MSecsPerSec = 1000;
   MinsPerDay  = HoursPerDay * MinsPerHour;
   SecsPerDay  = MinsPerDay * SecsPerMin;
   MSecsPerDay = SecsPerDay * MSecsPerSec;

//TDateTime holds the date as the number of days since 30 Dec 1899, known as Microsoft Excel epoch
   JulianEpoch = TDateTime(-2415018.5);
   UnixEpoch = JulianEpoch + TDateTime(2440587.5);

   DateDelta = 693594;        // Days between 1/1/0001 and 12/31/1899
   UnixDateDelta = Trunc(UnixEpoch); //25569


   
   MonthDays: array [Boolean] of TDayTable =
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

var 

DefaultFormatSettings: TFormatSettings  = (CurrencyFormat:1;NegCurrFormat:5;ThousandSeparator:',';DecimalSeparator:'.';CurrencyDecimals:2;DateSeparator:'-';TimeSeparator:':';ListSeparator:',';CurrencyString:'$';ShortDateFormat:'d/m/y';LongDateFormat:'dd" "mmmm" "yyyy';TimeAMString:'AM';TimePMString:'PM';ShortTimeFormat:'hh:nn';LongTimeFormat:'hh:nn:ss';ShortMonthNames: ( 'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'  ) ;LongMonthNames: ( 'January','February','March','April','May','June','July','August','September','October','November','December' ) ;ShortDayNames: ( 'Sun','Mon','Tue','Wed','Thu','Fri','Sat' )  ;LongDayNames: ( 'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'  ) ;TwoDigitYearCenturyWindow:50;  );

   TwoDigitYearCenturyWindow : word absolute DefaultFormatSettings.TwoDigitYearCenturyWindow;
                             { Threshold to be subtracted from year before
                               age-detection.}

   {  date time formatting characters:
      c      : shortdateformat + ' ' + longtimeformat
      d      : day of month
      dd     : day of month (leading zero)
      ddd    : day of week (abbreviation)
      dddd   : day of week (full)
      ddddd  : shortdateformat
      dddddd : longdateformat
      m      : month
      mm     : month (leading zero)
      mmm    : month (abbreviation)
      mmmm   : month (full)
      y      : year (two digits)
      yy     : year (two digits)
      yyyy   : year (four digits, with century)
      h      : hour
      hh     : hour (leading zero)
      n      : minute
      nn     : minute (leading zero)
      s      : second
      ss     : second (leading zero)
      t      : shorttimeformat
      tt     : longtimeformat
      am/pm  : use 12 hour clock and display am and pm accordingly
                a/p    : use 12 hour clock and display a and p accordingly
      /      : insert date seperator
      :      : insert time seperator
      "xx"   : literal text
      'xx'   : literal text
   }

  Tzseconds: LongInt;

type

  time_t = clong;

ptimeval = ^timeval;
timeval = packed record
  tv_sec: time_t; //Seconds
  tv_usec: clong;  //Microseconds
end;

ptimezone = ^timezone;

 timezone = packed record
  minuteswest: LongInt; //Minutes west of GMT
  dsttime: LongInt;  //Daylight savings time
end;

   TTimeStamp = record
      Time: integer;   { Number of milliseconds since midnight }
      Date: integer;   { One plus number of days since 1/1/0001 }
   end ;

 TSystemTime = record

  Year: Word; //Year part
  Month: Word;  //Month part
  Day: Word; //Day of month part
  Hour: Word; //Hour of the day
  Minute: Word; //Minute of the hour
  Second: Word;  //Second of the minute
  MilliSecond: Word; //Milliseconds in the second

end;



function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: Comp): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
function TryEncodeDate(Year, Month, Day: Word;  Date: TDateTime): Boolean;
function TryEncodeTime(Hour, Min, Sec, MSec: Word;  Time: TDateTime): Boolean;
function EncodeDate(Year, Month, Day :word): TDateTime;
function EncodeTime(Hour, Minute, Second, MilliSecond:word): TDateTime;
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;
procedure DecodeDate(Date: TDateTime;  Year, Month, Day: word);
function DecodeDateFully(const DateTime: TDateTime; Year, Month, Day, DOW: Word): Boolean;
procedure DecodeTime(Time: TDateTime;  Hour, Minute, Second, MilliSecond: word);
procedure DateTimeToSystemTime(DateTime: TDateTime; SystemTime: TSystemTime);
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
function DayOfWeek(DateTime: TDateTime): integer;
function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer): TDateTime;
procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer);
function IsLeapYear(Year: Word): boolean;
function DateToStr(Date: TDateTime): string;
function TimeToStr(Time: TDateTime): string;
function DateTimeToStr(DateTime: TDateTime): string;
function StrToDate(const S: string): TDateTime;
function StrToTime(const S: string): TDateTime;
function StrToDateTime(const S: string): TDateTime;
function FormatDateTime(FormatStr: pchar; DateTime: TDateTime): string;
procedure DateTimeToString( Result: string; const FormatStr: pchar; const DateTime: TDateTime);
Function DateTimeToFileDate(DateTime : TDateTime) : Longint;
Function FileDateToDateTime (Filedate : Longint) :TDateTime;
function TryStrToDate(const S: string;  Value: TDateTime): Boolean;
function TryStrToTime(const S: string;  Value: TDateTime): Boolean;
function TryStrToDateTime(const S: string;  Value: TDateTime): Boolean;
function StrToDateDef(const S: string; const Defvalue : TDateTime): TDateTime;
function StrToTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
function StrToDateTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
function CurrentYear:Word;
Procedure GetLocalTime(var SystemTime: TSystemTime); external;
procedure ReplaceTime(var dati:TDateTime; NewTime : TDateTime); inline;
procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime); inline;


Function DateOf(const AValue: TDateTime): TDateTime;
Function TimeOf(const AValue: TDateTime): TDateTime;

Function IsInLeapYear(const AValue: TDateTime): Boolean;
Function IsPM(const AValue: TDateTime): Boolean;
Function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
Function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
Function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
Function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
Function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
Function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;

Function WeeksInYear(const AValue: TDateTime): Word;
Function WeeksInAYear(const AYear: Word): Word;
Function DaysInYear(const AValue: TDateTime): Word;
Function DaysInAYear(const AYear: Word): Word;
Function DaysInMonth(const AValue: TDateTime): Word;
Function DaysInAMonth(const AYear, AMonth: Word): Word;

Function Today: TDateTime;
Function Yesterday: TDateTime;
Function Tomorrow: TDateTime;
Function IsToday(const AValue: TDateTime): Boolean;
Function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
Function PreviousDayOfWeek (DayOfWeek : Word) : Word;

Function YearOf(const AValue: TDateTime): Word;
Function MonthOf(const AValue: TDateTime): Word;
Function WeekOf(const AValue: TDateTime): Word;
Function DayOf(const AValue: TDateTime): Word;
Function HourOf(const AValue: TDateTime): Word;
Function MinuteOf(const AValue: TDateTime): Word;
Function SecondOf(const AValue: TDateTime): Word;
Function MilliSecondOf(const AValue: TDateTime): Word;

Function StartOfTheYear(const AValue: TDateTime): TDateTime;
Function EndOfTheYear(const AValue: TDateTime): TDateTime;
Function StartOfAYear(const AYear: Word): TDateTime;
Function EndOfAYear(const AYear: Word): TDateTime;

Function StartOfTheMonth(const AValue: TDateTime): TDateTime;
Function EndOfTheMonth(const AValue: TDateTime): TDateTime;
Function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
Function EndOfAMonth(const AYear, AMonth: Word): TDateTime;

Function StartOfTheWeek(const AValue: TDateTime): TDateTime;
Function EndOfTheWeek(const AValue: TDateTime): TDateTime;
Function StartOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function StartOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // ADayOFWeek 1
Function EndOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function EndOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // const ADayOfWeek: Word = 7

Function StartOfTheDay(const AValue: TDateTime): TDateTime;
Function EndOfTheDay(const AValue: TDateTime): TDateTime;
Function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
Function StartOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;
Function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload;
Function EndOfADay(const AYear, ADayOfYear: Word): TDateTime; overload;

Function MonthOfTheYear(const AValue: TDateTime): Word;
Function WeekOfTheYear(const AValue: TDateTime): Word; overload;
Function WeekOfTheYear(const AValue: TDateTime; var AYear: Word): Word; overload;
Function DayOfTheYear(const AValue: TDateTime): Word;
Function HourOfTheYear(const AValue: TDateTime): Word;
Function MinuteOfTheYear(const AValue: TDateTime): LongWord;
Function SecondOfTheYear(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheYear(const AValue: TDateTime): Int64;

Function WeekOfTheMonth(const AValue: TDateTime): Word; overload;
Function WeekOfTheMonth(const AValue: TDateTime; var AYear, AMonth: Word): Word; overload;
Function DayOfTheMonth(const AValue: TDateTime): Word;
Function HourOfTheMonth(const AValue: TDateTime): Word;
Function MinuteOfTheMonth(const AValue: TDateTime): Word;
Function SecondOfTheMonth(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;

Function DayOfTheWeek(const AValue: TDateTime): Word;
Function HourOfTheWeek(const AValue: TDateTime): Word;
Function MinuteOfTheWeek(const AValue: TDateTime): Word;
Function SecondOfTheWeek(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;

Function HourOfTheDay(const AValue: TDateTime): Word;
Function MinuteOfTheDay(const AValue: TDateTime): Word;
Function SecondOfTheDay(const AValue: TDateTime): LongWord;
Function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;

Function MinuteOfTheHour(const AValue: TDateTime): Word;
Function SecondOfTheHour(const AValue: TDateTime): Word;
Function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;

Function SecondOfTheMinute(const AValue: TDateTime): Word;
Function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;

Function MilliSecondOfTheSecond(const AValue: TDateTime): Word;

Function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean;
Function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean;
Function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean;
Function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean;
Function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean;
Function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean;
Function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean;
Function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean;

Function YearsBetween(const ANow, AThen: TDateTime): Integer;
Function MonthsBetween(const ANow, AThen: TDateTime): Integer;
Function WeeksBetween(const ANow, AThen: TDateTime): Integer;
Function DaysBetween(const ANow, AThen: TDateTime): Integer;
Function HoursBetween(const ANow, AThen: TDateTime): Int64;
Function MinutesBetween(const ANow, AThen: TDateTime): Int64;
Function SecondsBetween(const ANow, AThen: TDateTime): Int64;
Function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;


{ YearSpan and MonthSpan are approximate values }
Function YearSpan(const ANow, AThen: TDateTime): Double;
Function MonthSpan(const ANow, AThen: TDateTime): Double;
Function WeekSpan(const ANow, AThen: TDateTime): Double;
Function DaySpan(const ANow, AThen: TDateTime): Double;
Function HourSpan(const ANow, AThen: TDateTime): Double;
Function MinuteSpan(const ANow, AThen: TDateTime): Double;
Function SecondSpan(const ANow, AThen: TDateTime): Double;
Function MilliSecondSpan(const ANow, AThen: TDateTime): Double;


Function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime;
Function IncYear(const AValue: TDateTime): TDateTime; // ; const ANumberOfYears: Integer = 1)
// Function IncMonth is in SysUtils
Function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;
Function IncWeek(const AValue: TDateTime): TDateTime; // ; const ANumberOfWeeks: Integer = 1)
Function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;
Function IncDay(const AValue: TDateTime): TDateTime; //; const ANumberOfDays: Integer = 1)
Function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
Function IncHour(const AValue: TDateTime): TDateTime; //; const ANumberOfHours: Int64 = 1
Function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
Function IncMinute(const AValue: TDateTime): TDateTime; // ; const ANumberOfMinutes: Int64 = 1
Function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
Function IncSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfSeconds: Int64 = 1
Function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;
Function IncMilliSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfMilliSeconds: Int64 = 1


Function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Procedure DecodeDateTime(const AValue: TDateTime; var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
Function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; var AValue: TDateTime): Boolean;


Function EncodeDateWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
Function EncodeDateWeek(const AYear, AWeekOfYear: Word): TDateTime; //; const ADayOfWeek: Word = 1
Procedure DecodeDateWeek(const AValue: TDateTime; var AYear, AWeekOfYear, ADayOfWeek: Word);
Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; var AValue: TDateTime; const ADayOfWeek: Word): Boolean;
Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; var AValue: TDateTime): Boolean; //; const ADayOfWeek: Word = 1


Function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
Procedure DecodeDateDay(const AValue: TDateTime; var AYear, ADayOfYear: Word);
Function TryEncodeDateDay(const AYear, ADayOfYear: Word; var AValue: TDateTime): Boolean;

Function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): TDateTime;
Procedure DecodeDateMonthWeek(const AValue: TDateTime; var AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
Function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word; var AValue: TDateTime): Boolean;


Function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
Function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
Function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
Function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
Function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
Function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
Function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
Function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
Function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
Function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; var AResult: TDateTime): Boolean;


Function CompareDateTime(const A, B: TDateTime): TValueRelationship;
Function CompareDate(const A, B: TDateTime): TValueRelationship;
Function CompareTime(const A, B: TDateTime): TValueRelationship;
Function SameDateTime(const A, B: TDateTime): Boolean;
Function SameDate(const A, B: TDateTime): Boolean;
Function SameTime(const A, B: TDateTime): Boolean;

{ For a given date these Functions tell you the which day of the week of the
  month (or year).  If its a Thursday, they will tell you if its the first,
  second, etc Thursday of the month (or year).  Remember, even though its
  the first Thursday of the year it doesn't mean its the first week of the
  year.  See ISO 8601 above for more information. }

Function NthDayOfWeek(const AValue: TDateTime): Word;

Procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; var AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);

Function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word): TDateTime;
Function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word; var AValue: TDateTime): Boolean;


Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; const ABaseDate: TDateTime);
Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); // const ABaseDate: TDateTime = 0
Procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
Procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
Procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
Procedure InvalidDayOfWeekInMonthError(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word);

Function DateTimeToJulianDate(const AValue: TDateTime): Double;
Function JulianDateToDateTime(const AValue: Double): TDateTime;
Function TryJulianDateToDateTime(const AValue: Double; var ADateTime: TDateTime): Boolean;

Function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
Function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
Function TryModifiedJulianDateToDateTime(const AValue: Double; var ADateTime: TDateTime): Boolean;

Function DateTimeToUnix(const AValue: TDateTime): Int64;
Function UnixToDateTime(const AValue: Int64): TDateTime;
Function UnixTimeStampToMac(const AValue: Int64): Int64;

Function DateTimeToMac(const AValue: TDateTime): Int64;
Function MacToDateTime(const AValue: Int64): TDateTime;
Function MacTimeStampToUnix(const AValue: Int64): Int64;


Function DateTimeToDosDateTime(const AValue: TDateTime): longint;
Function DosDateTimeToDateTime( AValue: longint): TDateTime;

{ ScanDateTime is a limited inverse of formatdatetime }
function ScanDateTime(const Pattern:string;const s:string;const fmt:TFormatSettings;startpos:integer) : tdatetime; overload;
function ScanDateTime(const Pattern:string;const s:string;startpos:integer) : tdatetime; overload;
Procedure NotYetImplemented (FN : String);
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';
function  fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;  external;
function GetTime: TTime; external name 'GetTime';

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



implementation

uses 
   SysUtils, math,sysconst;
//types

const
  TDateTimeEpsilon = 2.2204460493e-16;




Procedure NotYetImplemented (FN : String);

begin
  Raise Exception.CreateFmt('Function "%s" (dateutils) is not yet implemented',[FN]);
end;



Function DateOf(const AValue: TDateTime): TDateTime;
begin
  Result:=Trunc(AValue);
end;


Function TimeOf(const AValue: TDateTime): TDateTime;
begin
  Result:=Frac(Avalue);
end;


Function IsInLeapYear(const AValue: TDateTime): Boolean;

Var
  D,Y,M : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=IsLeapYear(Y);
end;


Function IsPM(const AValue: TDateTime): Boolean;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(H>=12);
end;


Function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
begin
  Result:=(AYear<>0) and (AYear<10000)
          and (AMonth in [1..12])
          and (ADay<>0) and (ADay<=MonthDays[IsleapYear(AYear),AMonth]);
end;


Function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  Result:=(AHour=HoursPerDay) and (AMinute=0) and (ASecond=0) and (AMillisecond=0);
  Result:=Result or
          ((AHour<HoursPerDay) and (AMinute<MinsPerHour) and (ASecond<SecsPerMin) and
           (AMillisecond<MSecsPerSec));
end;


Function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  Result:=IsValidDate(AYear,AMonth,ADay) and
          IsValidTime(AHour,AMinute,ASecond,AMillisecond)
end;


Function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
begin
  Result:=(AYear<>0) and (ADayOfYear<>0) and (AYear<10000) and
          (ADayOfYear<=DaysPerYear[IsLeapYear(AYear)]);
end;


Function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
begin
  Result:=(AYear<>0) and (AYear<10000)
          and (ADayOfWeek in [1..7])
          and (AWeekOfYear<>0)
          and (AWeekOfYear<=WeeksInaYear(AYear));
  { should we not also check whether the day of the week is not
    larger than the last day of the last week in the year 9999 ?? }
end;


Function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;

begin
  Result:=(AYear<>0) and (AYear<10000)
          and (AMonth in [1..12])
          and (AWeekOfMonth in [1..5])
          and (ADayOfWeek in [1..7]);
end;


Function WeeksInYear(const AValue: TDateTime): Word;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=WeeksInAYear(Y);
end;


Function WeeksInAYear(const AYear: Word): Word;

Var
  DOW : Word;

begin
  Result:=52;
  DOW:=DayOfTheWeek(StartOfAYear(AYear));
  If (DOW=4) or ((DOW=3) and IsLeapYear(AYear)) then
    Inc(Result);
end;


Function DaysInYear(const AValue: TDateTime): Word;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=DaysPerYear[IsLeapYear(Y)];
end;


Function DaysInAYear(const AYear: Word): Word;
begin
  Result:=DaysPerYear[Isleapyear(AYear)];
end;


Function DaysInMonth(const AValue: TDateTime): Word;

Var
  Y,M,D : Word;

begin
  Decodedate(AValue,Y,M,D);
  Result:=MonthDays[IsLeapYear(Y),M];
end;


Function DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result:=MonthDays[IsLeapYear(AYear),AMonth];
end;

Function Today: TDateTime;
begin
  Result:=Date;
end;


Function Yesterday: TDateTime;
begin
  Result:=Date-1;
end;


Function Tomorrow: TDateTime;
begin
  Result:=Date+1;
end;


Function IsToday(const AValue: TDateTime): Boolean;
begin
  Result:=IsSameDay(AValue,Date);
end;


Function IsSameDay(const AValue, ABasis: TDateTime): Boolean;

Var
  D : TDateTime;

begin
  D:=AValue-Trunc(ABasis);
  Result:=(D>=0) and (D<1);
end;

const
  DOWMap: array [1..7] of Word = (7, 1, 2, 3, 4, 5, 6);

Function PreviousDayOfWeek (DayOfWeek : Word) : Word;

begin
  If Not (DayOfWeek in [1..7]) then
    Raise EConvertError.CreateFmt(SErrInvalidDayOfWeek,[DayOfWeek]);
  Result:=DOWMap[DayOfWeek];
end;


Function YearOf(const AValue: TDateTime): Word;

Var
  D,M : Word;

begin
  DecodeDate(AValue,Result,D,M);
end;


Function MonthOf(const AValue: TDateTime): Word;

Var
  Y,D : Word;

begin
  DecodeDate(AValue,Y,Result,D);
end;


Function WeekOf(const AValue: TDateTime): Word;
begin
  Result:=WeekOfTheYear(AValue);
end;


Function DayOf(const AValue: TDateTime): Word;

Var
  Y,M : Word;

begin
  DecodeDate(AValue,Y,M,Result);
end;


Function HourOf(const AValue: TDateTime): Word;

Var
  N,S,MS : Word;

begin
  DecodeTime(AValue,Result,N,S,MS);
end;


Function MinuteOf(const AValue: TDateTime): Word;

Var
  H,S,MS : Word;

begin
  DecodeTime(AValue,H,Result,S,MS);
end;


Function SecondOf(const AValue: TDateTime): Word;

Var
  H,N,MS : Word;

begin
  DecodeTime(AVAlue,H,N,Result,MS);
end;


Function MilliSecondOf(const AValue: TDateTime): Word;

Var
  H,N,S : Word;

begin
  DecodeTime(AValue,H,N,S,Result);
end;


Function StartOfTheYear(const AValue: TDateTime): TDateTime;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=EncodeDate(Y,1,1);
end;


Function EndOfTheYear(const AValue: TDateTime): TDateTime;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=EncodeDateTime(Y,12,31,23,59,59,999);
end;


Function StartOfAYear(const AYear: Word): TDateTime;
begin
  Result:=EncodeDate(AYear,1,1);
end;


Function EndOfAYear(const AYear: Word): TDateTime;

begin
  Result:=(EncodeDateTime(AYear,12,31,23,59,59,999));
end;

Function StartOfTheMonth(const AValue: TDateTime): TDateTime;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=EncodeDate(Y,M,1);
//  MonthDays[IsLeapYear(Y),M])
end;


Function EndOfTheMonth(const AValue: TDateTime): TDateTime;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=EncodeDateTime(Y,M,MonthDays[IsLeapYear(Y),M],23,59,59,999);
end;


Function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
begin
  Result:=EncodeDate(AYear,AMonth,1);
end;


Function EndOfAMonth(const AYear, AMonth: Word): TDateTime;

begin
  Result:=EncodeDateTime(AYear,AMonth,MonthDays[IsLeapYear(AYear),AMonth],23,59,59,999);
end;


Function StartOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  Result:=Trunc(AValue)-DayOfTheWeek(AValue)+1;
end;


Function EndOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  Result:=EndOfTheDay(AValue-DayOfTheWeek(AValue)+7);
end;


Function StartOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
begin
  Result:=EncodeDateWeek(AYear,AWeekOfYear,ADayOfWeek);
end;


Function StartOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // ADayOFWeek 1
begin
  Result:=StartOfAWeek(AYear,AWeekOfYear,1)
end;


Function EndOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
begin
  Result := EndOfTheDay(EncodeDateWeek(AYear, AWeekOfYear, ADayOfWeek));
end;


Function EndOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; // const ADayOfWeek: Word = 7


begin
  Result:=EndOfAWeek(AYear,AWeekOfYear,7);
end;

Function StartOfTheDay(const AValue: TDateTime): TDateTime;
begin
  StartOfTheDay:=Trunc(Avalue);
end;


Function EndOfTheDay(const AValue: TDateTime): TDateTime;

Var
  Y,M,D : Word;

begin
  DecodeDate(AValue,Y,M,D);
  Result:=EncodeDateTime(Y,M,D,23,59,59,999);
end;


Function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result:=EncodeDate(AYear,AMonth,ADay);
end;


Function StartOfADay(const AYear, ADayOfYear: Word): TDateTime;
begin
  Result:=StartOfAYear(AYear)+ADayOfYear-1;
end;


Function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result:=EndOfTheDay(EncodeDate(AYear,AMonth,ADay));
end;


Function EndOfADay(const AYear, ADayOfYear: Word): TDateTime;


begin
  Result:=StartOfAYear(AYear)+ADayOfYear-1+EncodeTime(23,59,59,999);
end;

Function MonthOfTheYear(const AValue: TDateTime): Word;

Var
  Y,D : Word;

begin
  DecodeDate(AValue,Y,Result,D);
end;


Function WeekOfTheYear(const AValue: TDateTime): Word;

Var
  Y,DOW : Word;

begin
  DecodeDateWeek(AValue,Y,Result,DOW)
end;


Function WeekOfTheYear(const AValue: TDateTime; var AYear: Word): Word;

Var
  DOW : Word;

begin
  DecodeDateWeek(AValue,AYear,Result,DOW);
end;


Function DayOfTheYear(const AValue: TDateTime): Word;
begin
  Result:=Trunc(AValue-StartOfTheYear(AValue)+1);
end;


Function HourOfTheYear(const AValue: TDateTime): Word;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=H+((DayOfTheYear(AValue)-1)*24);
end;


Function MinuteOfTheYear(const AValue: TDateTime): LongWord;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=M+(H+((DayOfTheYear(AValue)-1)*24))*60;
end;


Function SecondOfTheYear(const AValue: TDateTime): LongWord;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(M+(H+((DayOfTheYear(AValue)-1)*24))*60)*60+S;
end;


Function MilliSecondOfTheYear(const AValue: TDateTime): Int64;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=((M+(H+((int64(DayOfTheYear(AValue))-1)*24))*60)*60+S)*1000+MS;
end;


Function WeekOfTheMonth(const AValue: TDateTime): Word;

var
  Y,M,DOW : word;

begin
  DecodeDateMonthWeek(AValue,Y,M,Result,DOW);
end;


Function WeekOfTheMonth(const AValue: TDateTime; var AYear, AMonth: Word): Word;

Var
  DOW : Word;

begin
  DecodeDateMonthWeek(AValue,AYear,AMonth,Result,DOW);
end;


Function DayOfTheMonth(const AValue: TDateTime): Word;

Var
  Y,M : Word;

begin
  DecodeDate(AValue,Y,M,Result);
end;


Function HourOfTheMonth(const AValue: TDateTime): Word;

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  Result:=(D-1)*24+H;
end;


Function MinuteOfTheMonth(const AValue: TDateTime): Word;

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  Result:=((D-1)*24+H)*60+N;
end;


Function SecondOfTheMonth(const AValue: TDateTime): LongWord;

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  Result:=(((D-1)*24+H)*60+N)*60+S;
end;


Function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  Result:=((((D-1)*24+H)*60+N)*60+S)*1000+MS;
end;


Function DayOfTheWeek(const AValue: TDateTime): Word;

begin
  Result:=DowMAP[DayOfWeek(AValue)];
end;


Function HourOfTheWeek(const AValue: TDateTime): Word;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(DayOfTheWeek(AValue)-1)*24+H;
end;


Function MinuteOfTheWeek(const AValue: TDateTime): Word;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=((DayOfTheWeek(AValue)-1)*24+H)*60+M;
end;


Function SecondOfTheWeek(const AValue: TDateTime): LongWord;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(((DayOfTheWeek(AValue)-1)*24+H)*60+M)*60+S;
end;


Function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;


Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=((((DayOfTheWeek(AValue)-1)*24+H)*60+M)*60+S)*1000+MS;
end;

Function HourOfTheDay(const AValue: TDateTime): Word;

Var
  M,S,MS : Word;

begin
  DecodeTime(AValue,Result,M,S,MS);
end;


Function MinuteOfTheDay(const AValue: TDateTime): Word;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(H*60)+M;
end;


Function SecondOfTheDay(const AValue: TDateTime): LongWord;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=((H*60)+M)*60+S;
end;


Function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;

Var
  H,M,S,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(((H*60)+M)*60+S)*1000+MS;
end;


Function MinuteOfTheHour(const AValue: TDateTime): Word;

Var
  H,S,MS : Word;

begin
  DecodeTime(AValue,H,Result,S,MS);
end;


Function SecondOfTheHour(const AValue: TDateTime): Word;

Var
  H,S,M,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=M*60+S;
end;


Function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;

Var
  H,S,M,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=(M*60+S)*1000+MS;
end;

Function SecondOfTheMinute(const AValue: TDateTime): Word;

Var
  H,M,MS : Word;

begin
  DecodeTime(AValue,H,M,Result,MS);
end;


Function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;

Var
  H,S,M,MS : Word;

begin
  DecodeTime(AValue,H,M,S,MS);
  Result:=S*1000+MS;
end;


Function MilliSecondOfTheSecond(const AValue: TDateTime): Word;

Var
  H,M,S : Word;

begin
  DecodeTime(AValue,H,M,S,Result);
end;

{ ---------------------------------------------------------------------
    Range checking functions.
  ---------------------------------------------------------------------}

Function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean;
begin
  Result:=YearsBetween(ANow,AThen)<=AYears;
end;


Function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean;
begin
  Result:=MonthsBetween(ANow,AThen)<=AMonths;
end;


Function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean;
begin
  Result:=WeeksBetween(ANow,AThen)<=AWeeks;
end;


Function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean;
begin
  Result:=DaysBetween(ANow,AThen)<=ADays;
end;


Function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean;
begin
  Result:=HoursBetween(ANow,AThen)<=AHours;
end;


Function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean;
begin
  Result:=MinutesBetween(ANow,AThen)<=AMinutes;
end;


Function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean;
begin
  Result:=SecondsBetween(ANow,Athen)<=ASeconds;
end;


Function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean;
begin
  Result:=MilliSecondsBetween(ANow,AThen)<=AMilliSeconds;
end;


{ ---------------------------------------------------------------------
    Period functions.
  ---------------------------------------------------------------------}

{
  These functions are declared as approximate by Borland.
  A bit strange, since it can be calculated exactly ?

  -- No, because you need rounding or truncating (JM)
}


Function DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
begin
  Result:= ANow - AThen;
  if (ANow>0) and (AThen<0) then
    Result:=Result-0.5
  else if (ANow<-1.0) and (AThen>-1.0) then
    Result:=Result+0.5;
end;


Function YearsBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)/ApproxDaysPerYear);
end;


Function MonthsBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)/ApproxDaysPerMonth);
end;


Function WeeksBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result:=Trunc(Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon) div 7;
end;


Function DaysBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result:=Trunc(Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon);
end;


Function HoursBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)*HoursPerDay);
end;


Function MinutesBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)*MinsPerDay);
end;


Function SecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)*SecsPerDay);
end;


Function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result:=Trunc((Abs(DateTimeDiff(ANow,AThen))+TDateTimeEpsilon)*MSecsPerDay);
end;


Function YearSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))/ApproxDaysPerYear;
end;


Function MonthSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))/ApproxDaysPerMonth;
end;


Function WeekSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen)) / 7
end;


Function DaySpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen));
end;


Function HourSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))*HoursPerDay;
end;


Function MinuteSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))*MinsPerDay;
end;


Function SecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))*SecsPerDay;
end;


Function MilliSecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result:=Abs(DateTimeDiff(ANow,AThen))*MSecsPerDay;
end;


Procedure MaybeSkipTimeWarp(OldDate: TDateTime; var NewDate: TDateTime);
begin
  if (OldDate>0) and (NewDate<0) then
    NewDate:=NewDate-0.5
  else if (OldDate<-1.0) and (NewDate>-1.0) then
    NewDate:=NewDate+0.5;
end;


Function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime;

Var
  Y,M,D,H,N,S,MS : Word;
begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  Y:=Y+ANumberOfYears;
  If (M=2) and (D=29) And (Not IsLeapYear(Y)) then
    D:=28;
  Result:=EncodeDateTime(Y,M,D,H,N,S,MS);
end;


Function IncYear(const AValue: TDateTime): TDateTime; // ; const ANumberOfYears: Integer = 1)
begin
  Result:=IncYear(Avalue,1);
end;


Function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;
begin
  Result:=AValue+ANumberOfWeeks*7;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncWeek(const AValue: TDateTime): TDateTime; // ; const ANumberOfWeeks: Integer = 1)
begin
  Result:=IncWeek(Avalue,1);
end;


Function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;
begin
  Result:=AValue+ANumberOfDays;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncDay(const AValue: TDateTime): TDateTime; //; const ANumberOfDays: Integer = 1)
begin
  Result:=IncDay(Avalue,1);
end;


Function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
begin
  Result:=AValue+ANumberOfHours/HoursPerDay;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncHour(const AValue: TDateTime): TDateTime; //; const ANumberOfHours: Int64 = 1
begin
  Result:=IncHour(AValue,1);
end;


Function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
begin
  Result:=AValue+ANumberOfMinutes / MinsPerDay;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncMinute(const AValue: TDateTime): TDateTime; // ; const ANumberOfMinutes: Int64 = 1
begin
  Result:=IncMinute(AValue,1);
end;


Function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
begin
  Result:=AValue+ANumberOfSeconds / SecsPerDay;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfSeconds: Int64 = 1
begin
  Result:=IncSecond(Avalue,1);
end;


Function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;
begin
  Result:=AValue+ANumberOfMilliSeconds/MSecsPerDay;
  MaybeSkipTimeWarp(AValue,Result);
end;


Function IncMilliSecond(const AValue: TDateTime): TDateTime; // ; const ANumberOfMilliSeconds: Int64 = 1
begin
  Result:=IncMilliSecond(AValue,1);
end;


Function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  If Not TryEncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond,Result) then
    InvalidDateTimeError(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond)
end;


Procedure DecodeDateTime(const AValue: TDateTime; var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
begin
  DecodeDate(AValue,AYear,AMonth,ADay);
  DecodeTime(AValue,AHour,AMinute,ASecond,AMilliSecond);
end;


Function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; var AValue: TDateTime): Boolean;

Var
 tmp : TDateTime;

begin
  Result:=TryEncodeDate(AYear,AMonth,ADay,AValue);
  Result:=Result and TryEncodeTime(AHour,AMinute,ASecond,Amillisecond,Tmp);
  If Result then
    Avalue:=ComposeDateTime(AValue,Tmp);
end;

Function EncodeDateWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
begin
  If Not TryEncodeDateWeek(AYear,AWeekOfYear,Result,ADayOfWeek) then
    InvalidDateWeekError(AYear,AWeekOfYear,ADayOfWeek);
end;


Function EncodeDateWeek(const AYear, AWeekOfYear: Word): TDateTime; //; const ADayOfWeek: Word = 1
begin
  Result := EncodeDateWeek(AYear,AWeekOfYear,1);
end;


Procedure DecodeDateWeek(const AValue: TDateTime; var AYear, AWeekOfYear, ADayOfWeek: Word);

var
  DOY : Integer;
  D: Word;
  YS : TDateTime;
  YSDOW, YEDOW: Word;

begin
  AYear:=YearOf(AValue);
  // Correct to ISO DOW
  ADayOfWeek:=DayOfWeek(AValue)-1;
  If ADAyOfWeek=0 then
    ADayofweek:=7;
  YS:=StartOfAYear(AYear);
  DOY:=Trunc(AValue-YS)+1;
  YSDOW:=DayOfTheWeek(YS);
  // Correct week if later than wednesday. First week never starts later than wednesday
  if (YSDOW<5) then
    Inc(DOY,YSDOW-1)
  else
    Dec(DOY,8-YSDOW);
  if (DOY<=0) then // Day is in last week of previous year.
    DecodeDateWeek(YS-1,AYear,AWeekOfYear,D)
  else
    begin
    AWeekOfYear:=DOY div 7;
    if ((DOY mod 7)<>0) then
      Inc(AWeekOfYear);
    if (AWeekOfYear>52) then // Maybe in first week of next year ?
      begin
      YEDOW:=YSDOW;
      if IsLeapYear(AYear) then
        begin
        Inc(YEDOW);
        if (YEDOW>7) then
          YEDOW:=1
        else
      end;
      if (YEDOW<4) then // Really next year.
        begin
        Inc(AYear);
        AWeekOfYear:=1;
        end;
      end;
  end;
end;



Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; var AValue: TDateTime; const ADayOfWeek: Word): Boolean;

Var
  DOW : Word;
  Rest : Integer;

begin
  Result:=IsValidDateWeek(Ayear,AWeekOfYear,ADayOfWeek);
  If Result then
    begin
    AValue:=EncodeDate(AYear,1,1)+(7*(AWeekOfYear-1));
    DOW:=DayOfTheWeek(AValue);
    Rest:=ADayOfWeek-DOW;
    If (DOW>4) then
      Inc(Rest,7);
    AValue:=AValue+Rest;
    end;
end;


Function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; var AValue: TDateTime): Boolean; //; const ADayOfWeek: Word = 1
begin
  Result:=TryEncodeDateWeek(AYear,AWeekOfYear,AValue,1);
end;

Function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
begin
  If Not TryEncodeDateDay(AYear,ADayOfYear,Result) then
    InvalidDateDayError(AYear,ADayOfYear);
end;


Procedure DecodeDateDay(const AValue: TDateTime; var AYear, ADayOfYear: Word);

Var
  M,D : Word;

begin
  DecodeDate(AValue,AYear,M,D);
  ADayOfyear:=Trunc(AValue-EncodeDate(AYear,1,1))+1;
end;


Function TryEncodeDateDay(const AYear, ADayOfYear: Word; var AValue: TDateTime): Boolean;
begin
  Result:=(ADayOfYear<>0) and (ADayOfYear<=DaysPerYear [IsleapYear(AYear)]);
  If Result then
    AValue:=EncodeDate(AYear,1,1)+ADayOfYear-1;
end;


Function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): TDateTime;
begin
  If Not TryEncodeDateMonthWeek(Ayear,AMonth,AWeekOfMonth,ADayOfWeek,Result) then
    InvalidDateMonthWeekError(AYear,AMonth,AWeekOfMonth,ADayOfWeek);
end;

Procedure DecodeDateMonthWeek(const AValue: TDateTime; var AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);

Var
  D,SDOM,EDOM : Word;
  SOM : TdateTime;
  DOM : Integer;
begin
  DecodeDate(AValue,AYear,AMonth,D);
  ADayOfWeek:=DayOfTheWeek(AValue);
  SOM:=EncodeDate(Ayear,Amonth,1);
  SDOM:=DayOfTheWeek(SOM);
  DOM:=D-1+SDOM;
  If SDOM>4 then
    Dec(DOM,7);
  // Too early in the month. First full week is next week, day is after thursday.
  If DOM<=0 Then
    DecodeDateMonthWeek(SOM-1,AYear,AMonth,AWeekOfMonth,D)
  else
    begin
    AWeekOfMonth:=(DOM div 7)+Ord((DOM mod 7)<>0);
    EDOM:=DayOfTheWeek(EndOfAMonth(Ayear,AMonth));
    // In last days of last long week, so in next month...
    If (EDOM<4) and ((DaysInAMonth(AYear,Amonth)-D)<EDOM) then
      begin
      AWeekOfMonth:=1;
      Inc(AMonth);
      If (AMonth=13) then
        begin
        AMonth:=1;
        Inc(AYear);
        end;
      end;
    end;
end;

Function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word; var AValue: TDateTime): Boolean;

var
  S : Word;
  DOM : Integer;

begin
  Result:=IsValidDateMonthWeek(AYear,AMonth,AWeekOfMonth,ADayOfWeek);
  if Result then
    begin
    AValue:=EncodeDate(AYear,AMonth,1);
    DOM:=(AWeekOfMonth-1)*7+ADayOfWeek-1;
    { Correct for first week in last month.}
    S:=DayOfTheWeek(AValue);
    Dec(DOM,S-1);
    if S in [DayFriday..DaySunday] then
      Inc(DOM,7);
    AValue:=AValue+DOM;
    end;
end;


{ ---------------------------------------------------------------------
    Replace given element with supplied value.
  ---------------------------------------------------------------------}

Const
  LFAI = RecodeLeaveFieldAsIS; // Less typing, readable code
{
  Note: We have little choice but to implement it like Borland did:
  If AValue contains some 'wrong' value, it will throw an error.
  To simulate this we'd have to check in each function whether
  both arguments are correct. To avoid it, all is routed through
  the 'central' RecodeDateTime function as in Borland's implementation.
}

Function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;

begin
  Result := RecodeDateTime(AValue,AYear,LFAI,LFAI,LFAI,LFAI,LFAI,LFAI);
end;


Function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,AMonth,LFAI,LFAI,LFAI,LFAI,LFAI);
end;


Function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,ADay,LFAI,LFAI,LFAI,LFAI);
end;


Function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,LFAI,AHour,LFAI,LFAI,LFAI);
end;


Function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,LFAI,LFAI,AMinute,LFAI,LFAI);
end;


Function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,LFAI,LFAI,LFAI,ASecond,LFAI);
end;


Function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,LFAI,LFAI,LFAI,LFAI,AMilliSecond);
end;


Function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,AYear,AMonth,ADay,LFAI,LFAI,LFAI,LFAI);
end;


Function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  Result := RecodeDateTime(AValue,LFAI,LFAI,LFAI,AHour,AMinute,ASecond,AMilliSecond);
end;


Function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  If Not TryRecodeDateTime(AValue,AYear,AMonth,ADay,AHour,AMinute,ASecond,AMilliSecond,Result) then
    InvalidDateTimeError(AYear,AMonth,ADay,AHour,AMinute,ASecond,AMilliSecond,AValue);
end;


Function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; var AResult: TDateTime): Boolean;

  Procedure FV (Var AV : Word; Arg : Word);

  begin
    If (Arg<>LFAI) then
      AV:=Arg;
  end;

Var
  Y,M,D,H,N,S,MS : Word;

begin
  DecodeDateTime(AValue,Y,M,D,H,N,S,MS);
  FV(Y,AYear);
  FV(M,AMonth);
  FV(D,ADay);
  FV(H,AHour);
  FV(N,AMinute);
  FV(S,ASecond);
  FV(MS,AMillisecond);
  Result:=TryEncodeDateTime(Y,M,D,H,N,S,MS,AResult);
end;

Function CompareDateTime(const A, B: TDateTime): TValueRelationship;
begin
  If SameDateTime(A,B) then
    Result:=EqualsValue
  else If A>B then
    Result:=GreaterThanValue
  else
    Result:=LessThanValue
end;


Function CompareDate(const A, B: TDateTime): TValueRelationship;
begin
  If SameDate(A,B) then
    Result:=EQualsValue
  else if A<B then
    Result:=LessThanValue
  else
    Result:=GreaterThanValue;
end;


Function CompareTime(const A, B: TDateTime): TValueRelationship;

begin
  If SameTime(A,B) then
    Result:=EQualsValue
  else If Frac(A)<Frac(B) then
    Result:=LessThanValue
  else
    Result:=GreaterThanValue;
end;


Function SameDateTime(const A, B: TDateTime): Boolean;
begin
  Result:=Abs(A-B)<OneMilliSecond;
end;


Function SameDate(const A, B: TDateTime): Boolean;
begin
  Result:=Trunc(A)=Trunc(B);
end;


Function SameTime(const A, B: TDateTime): Boolean;

begin
  Result:=Frac(Abs(A-B))<OneMilliSecond;
end;


Function InternalNthDayOfWeek(DoM : Word) : Word;

begin
  Result:=(Dom-1) div 7 +1;
end;

Function NthDayOfWeek(const AValue: TDateTime): Word;

begin
  Result:=InternalNthDayOfWeek(DayOfTheMonth(AValue));
end;


Procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; var AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);

var
  D: Word;

begin
  DecodeDate(AValue,AYear,AMonth,D);
  ADayOfWeek:=DayOfTheWeek(AValue);
  ANthDayOfWeek:=InternalNthDayOfWeek(D);
end;


Function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word): TDateTime;
begin
  If Not TryEncodeDayOfWeekInMonth(AYear,AMonth,ANthDayOfWeek,ADayOfWeek,Result) then
    InvalidDayOfWeekInMonthError(AYear,AMonth,ANthDayOfWeek,ADayOfWeek);
end;


Function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word; var AValue: TDateTime): Boolean;

Var
  SOM,D : Word;

begin
  SOM:=DayOfTheWeek(EncodeDate(Ayear,AMonth,1));
  D:=1+ADayOfWeek-SOM+7*(ANthDayOfWeek-1);
  If SOM>ADayOfWeek then
    D:=D+7; // Clearer would have been Inc(ANthDayOfweek) but it's a const
  Result:=TryEncodeDate(Ayear,AMonth,D,AValue);
end;



Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; const ABaseDate: TDateTime);

  Function DoField(Arg,Def : Word; Unknown: String) : String;

  begin
    If (Arg<>LFAI) then
      Result:=Format('%.*d',[Length(Unknown),Arg])
    else if (ABaseDate=0) then
      Result:=Unknown
    else
      Result:=Format('%.*d',[Length(Unknown),Arg]);
  end;

Var
  Y,M,D,H,N,S,MS : Word;
  Msg : String;

begin
  DecodeDateTime(ABasedate,Y,M,D,H,N,S,MS);
  Msg:=DoField(AYear,Y,'????');
  Msg:=Msg+DateSeparator+DoField(AMonth,M,'??');
  Msg:=Msg+DateSeparator+DoField(ADay,D,'??');
  Msg:=Msg+' '+DoField(AHour,H,'??');
  Msg:=Msg+TimeSeparator+DoField(AMinute,N,'??');
  Msg:=Msg+TimeSeparator+Dofield(ASecond,S,'??');
  Msg:=Msg+DecimalSeparator+DoField(AMilliSecond,MS,'???');
  Raise EConvertError.CreateFmt(SErrInvalidTimeStamp,[Msg]);
end;


Procedure InvalidDateTimeError(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); // const ABaseDate: TDateTime = 0
begin
  InvalidDateTimeError(AYear,AMonth,ADay,AHour,AMinute,ASecond,AMilliSecond,0);
end;


Procedure InvalidDateWeekError(const AYear, AWeekOfYear, ADayOfWeek: Word);
begin
  Raise EConvertError.CreateFmt(SErrInvalidDateWeek,[AYear,AWeekOfYear,ADayOfWeek]);
end;


Procedure InvalidDateDayError(const AYear, ADayOfYear: Word);
begin
  Raise EConvertError.CreateFmt(SErrInvalidDayOfYear,[AYear,ADayOfYear]);
end;


Procedure InvalidDateMonthWeekError(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word);
begin
  Raise EConvertError.CreateFmt(SErrInvalidDateMonthWeek,[Ayear,AMonth,AWeekOfMonth,ADayOfWeek]);
end;


Procedure InvalidDayOfWeekInMonthError(const AYear, AMonth, ANthDayOfWeek,  ADayOfWeek: Word);

begin
  Raise EConvertError.CreateFmt(SErrInvalidDayOfWeekInMonth,[AYear,AMonth,ANthDayOfWeek,ADayOfWeek]);
end;


{$ifopt R+}
{$define RangeCheckWasOn}
{$R-}
{$endif opt R+}

{$ifopt Q+}
{$define OverflowCheckWasOn}
{$Q-}
{$endif opt Q+}

Function DateTimeToJulianDate(const AValue: TDateTime): Double;
begin
  DateTimeToJulianDate := AValue - JulianEpoch;
end;


Function JulianDateToDateTime(const AValue: Double): TDateTime;
begin
  JulianDateToDateTime := AValue + JulianEpoch;
  if(AValue <= 0) or (AValue >= 10000)then
    JulianDateToDateTime := NaN;
end;


Function TryJulianDateToDateTime(const AValue: Double; var ADateTime: TDateTime): Boolean;
begin
  ADateTime := JulianDateToDateTime(AValue);
  TryJulianDateToDateTime := ADateTime <> NaN;
end;

Function DateTimeToModifiedJulianDate(const AValue: TDateTime): Double;
begin
  Result:=0;
  NotYetImplemented('DateTimeToModifiedJulianDate');
end;


Function ModifiedJulianDateToDateTime(const AValue: Double): TDateTime;
begin
  Result:=0;
  NotYetImplemented('ModifiedJulianDateToDateTime');
end;


Function TryModifiedJulianDateToDateTime(const AValue: Double; var ADateTime: TDateTime): Boolean;
begin
  Result:=False;
  NotYetImplemented('TryModifiedJulianDateToDateTime');
end;

{$ifdef RangeCheckWasOn}
{$R+}
{$undef RangeCheckWasOn}
{$endif}

{$ifdef OverflowCheckWasOn}
{$Q+}
{$undef OverflowCheckWasOn}
{$endif}


Function DateTimeToUnix(const AValue: TDateTime): Int64;
begin
  Result:=Round(DateTimeDiff(AValue,UnixEpoch)*SecsPerDay);
end;


Function UnixToDateTime(const AValue: Int64): TDateTime;
begin
  Result:=IncSecond(UnixEpoch, AValue);
end;


Function UnixTimeStampToMac(const AValue: Int64): Int64;
const
  Epoch=24107 * 24 * 3600;
begin
  Result:=AValue + Epoch;
end;

Function DateTimeToMac(const AValue: TDateTime): Int64;
var
  Epoch:TDateTime;
begin
  Epoch:=EncodeDateTime( 1904, 1, 1, 0, 0, 0, 0 );
  Result:=SecondsBetween( Epoch, AValue );
end;


Function MacToDateTime(const AValue: Int64): TDateTime;
var
  Epoch:TDateTime;
begin
  Epoch:=EncodeDateTime( 1904, 1, 1, 0, 0, 0, 0 );
  Result:=IncSecond( Epoch, AValue );
end;


Function MacTimeStampToUnix(const AValue: Int64): Int64;
const
  Epoch=24107 * 24 * 3600;
begin
  Result:=AValue - Epoch;
end;

Function DateTimeToDosDateTime(const AValue: TDateTime): longint;
var year,month,day,hour,min,sec,msec : word;
    zs : longint;
begin
  decodedatetime(avalue,year,month,day,hour,min,sec,msec);
  result:=-1980;
  result:=result+year and 127;
  result:=result shl 4;
  result:=result+month;
  result:=result shl 5;
  result:=result+day;
  result:=result shl 16;
  zs:=hour;
  zs:=zs shl 6;
  zs:=zs+min;
  zs:=zs shl 5;
  zs:=zs+sec div 2;
  result:=result+(zs and $ffff);
end;

Function DosDateTimeToDateTime( AValue: longint): TDateTime;
var year,month,day,hour,min,sec : integer;
begin
  sec:=(AValue and 31) * 2;
  avalue:=AValue shr 5;
  min:=AValue and 63;
  avalue:=AValue shr 6;
  hour:=AValue and 31;
  avalue:=AValue shr 5;
  day:=AValue and 31;
  avalue:=AValue shr 5;
  month:=AValue and 15;
  avalue:=AValue shr 4;
  year:=AValue+1980;
  result:=EncodeDateTime(year,month,day,hour,min,sec,0);
end;


{
  Inverse of formatdatetime, destined for the dateutils unit of FPC.

  Limitations/implementation details:
  - An inverse of FormatDateTime is not 100% an inverse, simply because one can put e.g. time tokens twice in the format string,
       and scandatetime wouldn't know which time to pick.
  - Strings like hn can't be reversed safely. E.g. 1:2 (2 minutes after 1) delivers 12 which is parsed as 12:00 and then
       misses chars for the "n" part.
  - trailing characters are ignored.
  - no support for Eastern Asian formatting characters since they are windows only.
  - no MBCS support.

  Extensions
  - #9 eats whitespace.
  - whitespace at the end of a pattern is optional.
  - ? matches any char.
  - Quote the above chars to really match the char.
}

const whitespace  = [' ',#13,#10];
      hrfactor    = 1/(24);
      minfactor   = 1/(24*60);
      secfactor   = 1/(24*60*60);
      mssecfactor = 1/(24*60*60*1000);

const AMPMformatting : array[0..2] of string =('am/pm','a/p','ampm');

procedure raiseexception(const s:string);

begin
  raise EConvertError.Create(s);
end;

function scandatetime(const pattern:string;const s:string;const fmt:TFormatSettings;startpos:integer) : tdatetime;

var len ,ind  : integer;
    yy,mm,dd  : integer;
    timeval   : TDateTime;
    activequote: char;

procedure intscandate(ptrn:pchar;plen:integer;poffs:integer);
// poffs is the offset to

var
    pind : integer;

function findimatch(const mnts:array of string;p:pchar):integer;
var i : integer;
begin
  result:=-1;
  i:=0;
  while (i<=high(mnts)) and (result=-1) do
    begin
      if AnsiStrLIComp(p,@mnts[i][1],length(mnts[i]))=0 then
        result:=i;
      inc(i);
    end;
end;

procedure arraymatcherror;

begin
  raiseexception(format(SNoArrayMatch,[pind+1,ind]))
end;

function findmatch(const mnts : array of string;const s:string):integer;

begin
  result:=findimatch(mnts,@s[ind]);
  if result=-1 then
    arraymatcherror
  else
    begin
      inc(ind,length(mnts[result])+1);
      inc(pind,length(mnts[result])+1);
      inc(result); // was 0 based.
    end;
end;

var
    pivot,
    i     : integer;

function scanfixedint(maxv:integer):integer;
var c : char;
    oi:integer;
begin
  result:=0;
  oi:=ind;
  c:=ptrn[pind];
  while (pind<plen) and (ptrn[pind]=c) do inc(pind);
  while (maxv>0) and (ind<=len) and (s[ind] IN ['0'..'9']) do
    begin
      result:=result*10+ord(s[ind])-48;
      inc(ind);
      dec(maxv);
    end;
  if oi=ind then
    raiseexception(format(SPatternCharMismatch,[c,oi]));
end;

procedure matchchar(c:char);

begin
  if (ind>len) or (s[ind]<>c) then
    raiseexception(format(SNoCharMatch,[s[ind],c,pind+poffs+1,ind]));
  inc(pind);
  inc(ind);
end;

function scanpatlen:integer;
var c : char;
    lind : Integer;
begin
  result:=pind;
  lind:=pind;
  c:=ptrn[lind];

  while (lind<=plen) and (ptrn[lind]=c) do
      inc(lind);
  result:=lind-result;
end;

procedure matchpattern(const lptr:string);

var len:integer;
begin
  len:=length(lptr);
  if len>0 then
    intscandate(@lptr[1],len,pind+poffs);
end;

var lasttoken,lch : char;

begin
  pind:=0;     lasttoken:=' ';
  while (ind<=len) and (pind<plen) do
     begin
       lch:=upcase(ptrn[pind]);
       if activequote=#0 then
          begin
            if (lch='M') and (lasttoken='H') then
              begin
                i:=scanpatlen;
                if i>2 then
                  raiseexception(format(Shhmmerror,[poffs+pind+1]));
                timeval:=timeval+scanfixedint(2)* minfactor;
              end
            else
            case lch of
               'H':  timeval:=timeval+scanfixedint(2)* hrfactor;
               'D':  begin
                       i:=scanpatlen;
                       case i of
                          1,2 : dd:=scanfixedint(2);
                          3   : dd:=findmatch(fmt.shortDayNames,s);
                          4   : dd:=findmatch(fmt.longDayNames,s);
                          5   : matchpattern(fmt.shortdateformat);
                          6   : matchpattern(fmt.longdateformat);
                         end;
                     end;
               'N':  timeval:=timeval+scanfixedint(2)* minfactor;
               'S':  timeval:=timeval+scanfixedint(2)* secfactor;
               'Z':  timeval:=timeval+scanfixedint(3)* mssecfactor;
               'Y':  begin
                       i:=scanpatlen;
                       yy:=scanfixedint(i);
                       if i<=2 then
                         begin
                           pivot:=YearOf(now)-fmt.TwoDigitYearCenturyWindow;
                           inc(yy, pivot div 100 * 100);
                           if (fmt.TwoDigitYearCenturyWindow > 0) and (yy < pivot) then
                              inc(yy, 100);
                         end;
                      end;
               'M': begin
                       i:=scanpatlen;
                       case i of
                          1,2: mm:=scanfixedint(2);
                          3:   mm:=findmatch(fmt.ShortMonthNames,s);
                          4:   mm:=findmatch(fmt.LongMonthNames,s);
                          end;
                    end;
               'T' : begin
                       i:=scanpatlen;
                       case i of
                       1: matchpattern(fmt.shortdateformat);
                       2: matchpattern(fmt.longtimeformat);
                       end;
                     end;
               'A' : begin
                            i:=findimatch(AMPMformatting,@ptrn[pind]);
                            case i of
                              0: begin
                                   i:=findimatch(['AM','PM'],@s[ind]);
                                   case i of
                                     0: ;
                                     1: timeval:=timeval+12*hrfactor;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[0]));
                                   inc(ind,2);
                                 end;
                              1: begin
                                    case upcase(s[ind]) of
                                     'A' : ;
                                     'P' : timeval:=timeval+12*hrfactor;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[1]));
                                   inc(ind);
                                 end;
                               2: begin
                                    i:=findimatch([fmt.timeamstring,fmt.timepmstring],@s[ind]);
                                    case i of
                                     0: inc(ind,length(fmt.timeamstring));
                                     1: begin
                                          timeval:=timeval+12*hrfactor;
                                          inc(ind,length(fmt.timepmstring));
                                        end;
                                   else
                                     arraymatcherror
                                     end;
                                   inc(pind,length(AMPMformatting[2]));
                                   inc(pind,2);
                                   inc(ind,2);
                                 end;
                            else  // no AM/PM match. Assume 'a' is simply a char
                                matchchar(ptrn[pind]);
                             end;
                         end;
               '/' : matchchar(fmt.dateSeparator);
               ':' : begin
                       matchchar(fmt.TimeSeparator);
                       lch:=lasttoken;
                     end;
               #39,'"' : begin
                           activequote:=lch;
                           inc(pind);
                         end;
               'C' : begin
                       intscandate(@fmt.shortdateformat[1],length(fmt.ShortDateFormat),pind+poffs);
                       intscandate(@fmt.longtimeformat[1],length(fmt.longtimeformat),pind+poffs);
                       inc(pind);
                     end;
               '?' : begin
                       inc(pind);
                       inc(ind);
                     end;
               #9  : begin
                       while (ind<=len) and (s[ind] in whitespace) do
                         inc(ind);
                       inc(pind);
                     end;
               else
                 matchchar(ptrn[pind]);
             end; {case}
             lasttoken:=lch;
            end
          else
            begin
              if activequote=lch then
                begin
                  activequote:=#0;
                  inc(pind);
                end
              else
                matchchar(ptrn[pind]);
            end;
     end;
   if (pind<plen) and (plen>0) and (ptrn[plen-1]<>#9) then  // allow omission of trailing whitespace
     RaiseException(format(SFullpattern,[poffs+pind+1]));
end;

var plen:integer;

begin
  startpos:=1;
  activequote:=#0;
  yy:=0; mm:=0; dd:=0;
  timeval:=0.0;
  len:=length(s); ind:=startpos;
  plen:=length(pattern);
  intscandate(@pattern[1],plen,0);
  result:=timeval;
  if (yy>0) and (mm>0) and (dd>0) then
     result:=result+encodedate(yy,mm,dd);
end;


function scandatetime(const pattern:string;const s:string;startpos:integer) : tdatetime; overload;

var
 fmt:tformatsettings;

begin
 startpos:=1;
 result:=scandatetime(pattern,s,fmt,startpos);
end;


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


{PackTime is platform independent}
procedure PackTime (var T: datetime; var P: longint);

var zs:longint;

begin
    p:=-1980;
    p:=p+t.year and 127;
    p:=p shl 4;
    p:=p+t.month;
    p:=p shl 5;
    p:=p+t.day;
    p:=p shl 16;
    zs:=t.hour;
    zs:=zs shl 6;
    zs:=zs+t.min;
    zs:=zs shl 5;
    zs:=zs+t.sec div 2;
    p:=p+(zs and $ffff);
end;

{UnpackTime is platform-independent}
procedure UnpackTime (P: longint; var T: datetime);

begin
    t.sec:=(p and 31) * 2;
    p:=p shr 5;
    t.min:=p and 63;
    p:=p shr 6;
    t.hour:=p and 31;
    p:=p shr 5;
    t.day:=p and 31;
    p:=p shr 5;
    t.month:=p and 15;
    p:=p shr 4;
    t.year:=p+1980;
end;



Function GregorianToJulian(Year,Month,Day:Longint):LongInt;
Var
  Century,XYear: LongInt;
Begin
  If Month<=2 Then
   Begin
     Dec(Year);
     Inc(Month,12);
   End;
  Dec(Month,3);
  Century:=(longint(Year Div 100)*D1) shr 2;
  XYear:=(longint(Year Mod 100)*D0) shr 2;
  GregorianToJulian:=((((Month*153)+2) div 5)+Day)+D2+XYear+Century;
End;


Function LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
{
  Transforms local time (year,month,day,hour,minutes,second) to Epoch time
   (seconds since 00:00, january 1 1970, corrected for local time zone)
}
Begin
  LocalToEpoch:=((GregorianToJulian(Year,Month,Day)-c1970)*86400)+
                (LongInt(Hour)*3600)+(Longint(Minute)*60)+Second-TZSeconds;
End;



Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then
   Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;

Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  DateNum: LongInt;
Begin
  inc(Epoch,TZSeconds);
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;

function WeekDay (y,m,d:longint):longint;
{
  Calculates th day of the week. returns -1 on error
}
var
  u,v : longint;
begin
  if (m<1) or (m>12) or (y<1600) or (y>4000) or
     (d<1) or (d>30+((m+ord(m>7)) and 1)-ord(m=2)) or
     ((m*d=58) and (((y mod 4>0) or (y mod 100=0)) and (y mod 400>0))) then
   WeekDay:=-1
  else
   begin
     u:=m;
     v:=y;
     if m<3 then
      begin
        inc(u,12);
        dec(v);
      end;
     WeekDay:=(d+2*u+((3*(u+1)) div 5)+v+(v div 4)-(v div 100)+(v div 400)+1) mod 7;
   end;
end;


Procedure GetDate(Var Year, Month, MDay, WDay: Word);
var
  tz:timeval;
  hour,min,sec : word;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,year,month,mday,hour,min,sec);
  Wday:=weekday(Year,Month,MDay);
end;



procedure  SetTime(Hour,Minute,Second,sec100:word);
var
  dow,Year, Month, Day : Word;

  tv : timeval;
begin
  GetDate (Year, Month, Day,dow);
  tv.tv_sec:= LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) ;
  fpSettimeofday(@tv,nil);
end;

procedure SetDate(Year,Month,Day:Word);
var
  Hour, Min, Sec, Sec100 : Word;
  tv : timeval;
begin
  GetTime ( Hour, Min, Sec, Sec100 );
  tv.tv_sec:= LocalToEpoch ( Year, Month, Day, Hour, Min, Sec ) ;
  fpSettimeofday(@tv,nil);
end;


Function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;
var
  tv : timeval;
begin
  tv.tv_sec:= LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) ;
  SetDatetime:=fpSettimeofday(@tv,nil)=0;
end;


Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
var
  tz:timeval;
  year,month,day : word;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,year,month,day,hour,minute,second);
  sec100:=tz.tv_usec div 10000;
end;


Procedure UnixDateToDt(SecsPast: LongInt; Var Dt: datetime);
Begin
  EpochToLocal(SecsPast,dt.Year,dt.Month,dt.Day,dt.Hour,dt.Min,dt.Sec);
End;


Function DTToUnixDate(DT: DateTime): LongInt;
Begin
  DTToUnixDate:=LocalToEpoch(dt.Year,dt.Month,dt.Day,dt.Hour,dt.Min,dt.Sec);
End;


Function DoEncodeDate(Year, Month, Day: Word): longint;

Var
  D : TDateTime;

begin
  If TryEncodeDate(Year,Month,Day,D) then
    Result:=Trunc(D)
  else
    Result:=0;
end;

function DoEncodeTime(Hour, Minute, Second, MilliSecond: word): TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Result:=0;
end;




{   ComposeDateTime converts a Date and a Time into one TDateTime   }
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;

begin
  if Date < 0 then Result := trunc(Date) - Abs(frac(Time))
  else Result := trunc(Date) + Abs(frac(Time));
end;

{   DateTimeToTimeStamp converts DateTime to a TTimeStamp   }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
begin
  result.Time := Round(abs(Frac(DateTime)) * MSecsPerDay);
  result.Date := DateDelta + trunc(DateTime);
end ;

{   TimeStampToDateTime converts TimeStamp to a TDateTime value   }

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
  Result := ComposeDateTime(TimeStamp.Date - DateDelta,TimeStamp.Time / MSecsPerDay)
end;

{   MSecsToTimeStamp   }

function MSecsToTimeStamp(MSecs: comp): TTimeStamp;
begin
  result.Date := Trunc(msecs / msecsperday);
  msecs:= msecs-comp(result.date)*msecsperday;
  result.Time := Round(MSecs);
end ;

{   TimeStampToMSecs   }

function TimeStampToMSecs(const TimeStamp: TTimeStamp): comp;
begin
  result := TimeStamp.Time + comp(timestamp.date)*msecsperday;
end ;

Function TryEncodeDate(Year,Month,Day : Word;  Date : TDateTime) : Boolean;

var
  c, ya: cardinal;
begin
  Result:=(Year>0) and (Year<10000) and
          (Month in [1..12]) and
          (Day>0) and (Day<=MonthDays[IsleapYear(Year),Month]);
 If Result then
   begin
     if month > 2 then
      Dec(Month,3)
     else
      begin
        Inc(Month,9);
        Dec(Year);
      end;
     c:= Year DIV 100;
     ya:= Year - 100*c;
     Date := (146097*c) SHR 2 + (1461*ya) SHR 2 + (153*cardinal(Month)+2) DIV 5 + cardinal(Day);
     // Note that this line can't be part of the line above, since TDateTime is
     // signed and c and ya are not
     Date := Date - 693900;
   end
end;

function TryEncodeTime(Hour, Min, Sec, MSec:word;  Time : TDateTime) : boolean;

begin
  Result:=(Hour<24) and (Min<60) and (Sec<60) and (MSec<1000);
  If Result then
    Time:=TDateTime(cardinal(Hour)*3600000+cardinal(Min)*60000+cardinal(Sec)*1000+MSec)/MSecsPerDay;
end;

{   EncodeDate packs three variables Year, Month and Day into a
    TDateTime value the result is the number of days since 12/30/1899   }

function EncodeDate(Year, Month, Day: word): TDateTime;

begin
  If Not TryEncodeDate(Year,Month,Day,Result) then
    Raise EConvertError.CreateFmt('%d-%d-%d is not a valid date specification',
                              [Year,Month,Day]);
end;

{   EncodeTime packs four variables Hour, Minute, Second and MilliSecond into
    a TDateTime value     }

function EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Raise EConvertError.CreateFmt('%d:%d:%d.%d is not a valid time specification',
                              [Hour,Minute,Second,MilliSecond]);
end;


{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }

procedure DecodeDate(Date: TDateTime;  Year, Month, Day: word);
var
  ly,ld,lm,j : cardinal;
begin
  if Date <= -datedelta then  // If Date is before 1-1-1 then return 0-0-0
    begin
    Year := 0;
    Month := 0;
    Day := 0;
    end
  else
    begin
    j := pred((Trunc(System.Int(Date)) + 693900) SHL 2);
    ly:= j DIV 146097;
    j:= j - 146097 * cardinal(ly);
    ld := j SHR 2;
    j:=(ld SHL 2 + 3) DIV 1461;
    ld:= (cardinal(ld) SHL 2 + 7 - 1461*j) SHR 2;
    lm:=(5 * ld-3) DIV 153;
    ld:= (5 * ld +2 - 153*lm) DIV 5;
    ly:= 100 * cardinal(ly) + j;
    if lm < 10 then
     inc(lm,3)
    else
      begin
        dec(lm,9);
        inc(ly);
      end;
    year:=ly;
    month:=lm;
    day:=ld;
    end;
end;


function DecodeDateFully(const DateTime: TDateTime;  Year, Month, Day, DOW: Word): Boolean;
begin
  DecodeDate(DateTime,Year,Month,Day);
  DOW:=DayOfWeek(DateTime);
  Result:=IsLeapYear(Year);
end;


{   DecodeTime unpacks Time into four values:
    Hour, Minute, Second and MilliSecond    }

procedure DecodeTime(Time: TDateTime;  Hour, Minute, Second, MilliSecond: word);
Var
  l : cardinal;
begin
 l := Round(abs(Frac(time)) * MSecsPerDay);
 Hour   := l div 3600000;
 l := l mod 3600000;
 Minute := l div 60000;
 l := l mod 60000;
 Second := l div 1000;
 l := l mod 1000;
 MilliSecond := l;
end;

{   DateTimeToSystemTime converts DateTime value to SystemTime   }

procedure DateTimeToSystemTime(DateTime: TDateTime;  SystemTime: TSystemTime);
begin
  DecodeDate(DateTime, SystemTime.Year, SystemTime.Month, SystemTime.Day);
  DecodeTime(DateTime, SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond);
end ;

{   SystemTimeToDateTime converts SystemTime to a TDateTime value   }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  result := ComposeDateTime(DoEncodeDate(SystemTime.Year, SystemTime.Month, SystemTime.Day),
                            DoEncodeTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond));
end ;

{   DayOfWeek returns the Day of the week (sunday is day 1)  }

function DayOfWeek(DateTime: TDateTime): integer;
begin
  Result := 1 + (Abs(Trunc(DateTime) - 1) mod 7);
end ;

{   Date returns the current Date   }

function Date: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  result := DoEncodeDate(SystemTime.Year, SystemTime.Month, SystemTime.Day);
end ;

{   Time returns the current Time   }

function Time: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := DoEncodeTime(SystemTime.Hour,SystemTime.Minute,SystemTime.Second,SystemTime.MilliSecond);
end ;

{   Now returns the current Date and Time    }

function Now: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  result := systemTimeToDateTime(SystemTime);
end;

{   IncMonth increments DateTime with NumberOfMonths months,
    NumberOfMonths can be less than zero   }

function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer  ): TDateTime;
var
  Year, Month, Day : word;
begin
  NumberOfMonths:=1;
  DecodeDate(DateTime, Year, Month, Day);
  IncAMonth(Year, Month, Day, NumberOfMonths);
  result := ComposeDateTime(DoEncodeDate(Year, Month, Day), DateTime);
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
  If (Day>MonthDays[IsLeapYear(Year)][Month]) then
    Day:=MonthDays[IsLeapYear(Year)][Month];
end;

{  IsLeapYear returns true if Year is a leap year   }

function IsLeapYear(Year: Word): boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

{  DateToStr returns a string representation of Date using ShortDateFormat   }

function DateToStr(Date: TDateTime): string;
begin
  result := FormatDateTime('ddddd', Date);
end ;

{  TimeToStr returns a string representation of Time using LongTimeFormat   }

function TimeToStr(Time: TDateTime): string;
begin
  result := FormatDateTime('tt', Time);
end ;

{   DateTimeToStr returns a string representation of DateTime using LongDateTimeFormat   }

function DateTimeToStr(DateTime: TDateTime): string;
begin
  result := FormatDateTime('c', DateTime);
end ;

{   StrToDate converts the string S to a TDateTime value
    if S does not represent a valid date value
    an EConvertError will be raised   }

function StrToDate(const S: string): TDateTime;
const SInvalidDateFormat = '"%s" is not a valid date format';
var
   df:string;
   d,m,y,ly:word;
   n,i:longint;
   c:word;
   dp,mp,yp,which : Byte;
   s1:string[4];
   values:array[1..3] of longint;
   LocalTime:tsystemtime;
   YearMoreThenTwoDigits : boolean;
begin
  if s = '' then
    Raise EConvertError.CreateFmt(SInvalidDateFormat,[s]);

  YearMoreThenTwoDigits := False;
  df := UpperCase(ShortDateFormat);
  { Determine order of D,M,Y }
  yp:=0;
  mp:=0;
  dp:=0;
  Which:=0;
  i:=0;
  while (i<Length(df)) and (Which<3) do
   begin
     inc(i);
     Case df[i] of
       'Y' :
         if yp=0 then
          begin
            Inc(Which);
            yp:=which;
          end;
       'M' :
         if mp=0 then
          begin
            Inc(Which);
            mp:=which;
          end;
       'D' :
         if dp=0 then
          begin
            Inc(Which);
            dp:=which;
          end;
     end;
   end;
  if Which<>3 then
   Raise EConvertError.Create('Illegal format string');
{ Get actual values }
  for i := 1 to 3 do
    values[i] := 0;
  s1 := '';
  n := 0;
  for i := 1 to length(s) do
   begin
     if s[i] in ['0'..'9'] then
      s1 := s1 + s[i];

     { space can be part of the shortdateformat, and is defaultly in slovak
       windows, therefor it shouldn't be taken as separator (unless so specified)
       and ignored }
     if (DateSeparator <> ' ') and (s[i] = ' ') then
       Continue;

     if (s[i] = dateseparator) or ((i = length(s)) and (s[i] in ['0'..'9'])) then
      begin
        inc(n);
        if n>3 then
         Raise EConvertError.CreateFmt(SInvalidDateFormat,[s]);
         // Check if the year has more then two digits (if n=yp, then we are evaluating the year.)
        if (n=yp) and (length(s1)>2) then YearMoreThenTwoDigits := True;
        val(s1, values[n], c);
        if c<>0 then
         Raise EConvertError.CreateFmt(SInvalidDateFormat,[s]);
        s1 := '';
      end
     else if not (s[i] in ['0'..'9']) then
      Raise EConvertError.CreateFmt(SInvalidDateFormat,[s]);
   end ;
  // Fill in values.
  getLocalTime(LocalTime);
  ly := LocalTime.Year;
  If N=3 then
   begin
     y:=values[yp];
     m:=values[mp];
     d:=values[dp];
   end
  Else
  begin
    Y:=ly;
    If n<2 then
     begin
       d:=values[1];
       m := LocalTime.Month;
     end
    else
     If dp<mp then
      begin
        d:=values[1];
        m:=values[2];
      end
    else
      begin
        d:=values[2];
        m:=values[1];
      end;
  end;
  if (y >= 0) and (y < 100) and not YearMoreThenTwoDigits then
    begin
    ly := ly - TwoDigitYearCenturyWindow;
    Inc(Y, ly div 100 * 100);
    if (TwoDigitYearCenturyWindow > 0) and (Y < ly) then
      Inc(Y, 100);
    end;
  Result := EncodeDate(y, m, d);
end ;


{   StrToTime converts the string S to a TDateTime value
    if S does not represent a valid time value an
    EConvertError will be raised   }

function StrToTime(const s: string): TDateTime;
var
   Len, Current: integer; PM: integer;

   function GetElement: integer;
   var
     j, c: integer;
   begin
   result := -1;
   Inc(Current);
   while (result = -1) and (Current <= Len) do
     begin
       if S[Current] in ['0'..'9'] then 
          begin
            j := Current;
            while (Current < Len) and (s[Current + 1] in ['0'..'9']) do
              Inc(Current);
            val(copy(S, j, 1 + Current - j), result, c);
          end
       else if ((TimeAMString<>'') and (S[Current] = TimeAMString[1])) or (S[Current] in ['a', 'A']) then 
          begin
            pm:=1;
            Current := 1 + Len;
          end
       else if ((TimePMString<>'') and (S[Current] = TimePMString[1])) or (S[Current] in ['p', 'P']) then 
         begin
           Current := 1 + Len;
           PM := 2;
         end
       else if (S[Current] = TimeSeparator) or (S[Current] = ' ') then
         Inc(Current)
      else
        raise EConvertError.Create('Invalid Time format');
      end ;
   end ;

var
   i: integer;
   TimeValues: array[0..4] of integer;

begin
  Current := 0;
  Len := length(s);
  PM := 0;
  for i:=0 to 4 do
    timevalues[i]:=0;
  i := 0;
  TimeValues[i] := GetElement;
  while (i < 5) and (TimeValues[i] <> -1) do 
    begin
     i := i + 1;
     TimeValues[i] := GetElement;
   end ;
  If (i<5) and (TimeValues[I]=-1) then
    TimeValues[I]:=0;
  if PM=2 then
    begin
     if (TimeValues[0] <> 12) then
       Inc(TimeValues[0], 12);
    end
  else
    begin
      if (pm=1) and ((TimeValues[0]=12)) then
        TimeValues[0]:=0;
    end;
  result := EncodeTime(TimeValues[0], TimeValues[1], TimeValues[2], TimeValues[3]);
end ;

{   StrToDateTime converts the string S to a TDateTime value
    if S does not represent a valid date and/or time value
    an EConvertError will be raised   }

function StrToDateTime(const s: string): TDateTime;
var
  i, j, k, l: integer;
  sd, st: string;
begin
  l := Length(s);
  i := 1;
  while (i <= l) and (s[i] = ' ') do
    Inc(i);
  j := i;
  while (j <= l) and (s[j] <> ' ') do
    Inc(j);
  k := j;
  while (k <= l) and (s[k] = ' ') do
    Inc(k);
  sd := Copy(s, i, j - i);
  st := Copy(s, k, l);
  if (st = '') and (Pos(TimeSeparator, sd) > 0) then
  begin
    st := sd;
    sd := '';
  end;
  if (sd <> '') and (st <> '') then
    Result := ComposeDateTime(StrToDate(sd), StrToTime(st))
  else if st = '' then
    Result := StrToDate(sd)
  else
    Result := StrToTime(st);
end ;

{   FormatDateTime formats DateTime to the given format string FormatStr   }

function FormatDateTime(FormatStr: pchar; DateTime: TDateTime): string;
var
   ResultLen: integer;
   ResultBuffer: array[0..255] of char;
   ResultCurrent: pchar;

   procedure StoreStr(Str: pchar; Len: integer);
   begin
   if ResultLen + Len < SizeOf(ResultBuffer) then begin
      StrMove(ResultCurrent, Str, Len);
      ResultCurrent := ResultCurrent + Len;
      ResultLen := ResultLen + Len;
      end ;
   end ;

   procedure StoreString(const Str: pchar);
   var Len: integer;
   begin
   Len := Length(Str);
   if ResultLen + Len < SizeOf(ResultBuffer) then begin // strmove not safe
      StrMove(pchar(ResultCurrent), pchar(Str), Len);
      ResultCurrent := ResultCurrent + Len;
      ResultLen := ResultLen + Len;
      end;
   end;

   procedure StoreInt(Value, Digits: integer);
   var S: string; Len: integer;
   begin
   S := IntToStr(Value);
   Len := Length(S);
   if Len < Digits then begin
      S := copy('0000', 1, Digits - Len) + S;
      Len := Digits;
      end ;
   StoreStr(pchar(@S[1]), Len);
   end ;

var
   Year, Month, Day, DayOfWeek, Hour, Minute, Second, MilliSecond: word;

   procedure StoreFormat(const FormatStr: pchar);
   var
      Token,lastformattoken: char;
      FormatCurrent: pchar;
      FormatEnd: pchar;
      Count: integer;
      Clock12: boolean;
      P: pchar;
      tmp:integer;

   begin
   FormatCurrent := pointer(FormatStr);
   FormatEnd := FormatCurrent + Length(FormatStr);
   Clock12 := false;
   P := FormatCurrent;
   while P < FormatEnd do begin
      Token := UpCase(P^);
      if Token in ['"', ''''] then begin
         P := P + 1;
         while (P < FormatEnd) and (P^ <> Token) do
            P := P + 1;
         end
      else if Token = 'A' then begin
         if (StrLIComp(P, 'A/P', 3) = 0) or
            (StrLIComp(P, 'AMPM', 4) = 0) or
            (StrLIComp(P, 'AM/PM', 5) = 0) then begin
            Clock12 := true;
            break;
            end ;
         end ;
      P := P + 1;
      end ;
   token:=#255;
   lastformattoken:=' ';
   while FormatCurrent < FormatEnd do
     begin
      Token := UpCase(FormatCurrent^);
      Count := 1;
      P := FormatCurrent + 1;
         case Token of
            '''', '"': begin
               while (P < FormatEnd) and (p^ <> Token) do
                  P := P + 1;
               P := P + 1;
               Count := P - FormatCurrent;
               StoreStr(FormatCurrent + 1, Count - 2);
               end ;
            'A': begin
               if StrLIComp(FormatCurrent, 'AMPM', 4) = 0 then begin
                  Count := 4;
                  if Hour < 12 then StoreString(pchar(TimeAMString))
                  else StoreString(pchar(TimePMString));
                  end
               else if StrLIComp(FormatCurrent, 'AM/PM', 5) = 0 then begin
                  Count := 5;
                  if Hour < 12 then StoreStr('am', 2)
                  else StoreStr('pm', 2);
                  end
               else if StrLIComp(FormatCurrent, 'A/P', 3) = 0 then begin
                  Count := 3;
                  if Hour < 12 then StoreStr('a', 1)
                  else StoreStr('p', 1);
                  end
               else
                 Raise EConvertError.Create('Illegal character in format string');
               end ;
            '/': StoreStr(@DateSeparator, 1);
            ':': StoreStr(@TimeSeparator, 1);
            ' ', 'C', 'D', 'H', 'M', 'N', 'S', 'T', 'Y','Z' :
              begin
                while (P < FormatEnd) and (UpCase(P^) = Token) do
                  P := P + 1;
                Count := P - FormatCurrent;
                case Token of
                   ' ': StoreStr(FormatCurrent, Count);
                   'Y': begin
                         if Count>2 then
                           StoreInt(Year, 4)
                         else
                           StoreInt(Year mod 100, 2);
                        end;
                   'M': begin
                         if lastformattoken='H' then
                           begin
                             if Count = 1 then
                               StoreInt(Minute, 0)
                             else
                               StoreInt(Minute, 2);

                           end
                         else
                           begin
                             case Count of
                                1: StoreInt(Month, 0);
                                2: StoreInt(Month, 2);
                                3: StoreString(pchar(ShortMonthNames[Month]));
                                4: StoreString(pchar(LongMonthNames[Month]));
                             end;
                           end;
                      end;
                   'D': begin
                         case Count of
                            1: StoreInt(Day, 0);
                            2: StoreInt(Day, 2);
                            3: StoreString(pchar(ShortDayNames[DayOfWeek]));
                            4: StoreString(pchar(LongDayNames[DayOfWeek]));
                            5: StoreFormat(pchar(ShortDateFormat));
                            6: StoreFormat(pchar(LongDateFormat));
                         end ;
                      end ;
                   'H': begin
                      if Clock12 then begin
                         tmp:=hour mod 12;
                         if tmp=0 then tmp:=12;
                         if Count = 1 then StoreInt(tmp, 0)
                         else StoreInt(tmp, 2);
                         end
                      else begin
                         if Count = 1 then StoreInt(Hour, 0)
                         else StoreInt(Hour, 2);
                         end ;
                      end ;
                   'N': begin
                      if Count = 1 then StoreInt(Minute, 0)
                      else StoreInt(Minute, 2);
                      end ;
                   'S': begin
                      if Count = 1 then StoreInt(Second, 0)
                      else StoreInt(Second, 2);
                      end ;
                   'Z': begin
                      if Count = 1 then StoreInt(MilliSecond, 0)
                      else StoreInt(MilliSecond, 3);
                      end ;
                   'T': begin
                      if Count = 1 then StoreFormat(pchar(ShortTimeFormat))
                      else StoreFormat(pchar(LongTimeFormat));
                      end ;
                   'C':
                     begin
                       StoreFormat(pchar(ShortDateFormat));
                       if (Hour<>0) or (Minute<>0) or (Second<>0) then
                        begin
                          StoreString(' ');
                          StoreFormat(pchar(LongTimeFormat));
                        end;
                     end;
                end;
                lastformattoken:=token;
              end;
            else
              StoreStr(@Token, 1);
         end ;
      FormatCurrent := FormatCurrent + Count;
      end ;
   end ;

begin
  DecodeDateFully(DateTime, Year, Month, Day, DayOfWeek);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
  ResultLen := 0;
  ResultCurrent := @ResultBuffer[0];
  StoreFormat(pchar(FormatStr));
  ResultBuffer[ResultLen] := #0;
  result := StrPas(@ResultBuffer[0]);
end ;

{   DateTimeToString formats DateTime to the given format in FormatStr   }

procedure DateTimeToString( Result: string; const FormatStr: pchar; const DateTime: TDateTime);
begin
  Result := FormatDateTime(pchar(FormatStr), DateTime);
end ;


Function DateTimeToFileDate(DateTime : TDateTime) : Longint;

Var YY,MM,DD,H,m,s,msec : Word;

begin
  Decodedate (DateTime,YY,MM,DD);
  DecodeTime (DateTime,h,m,s,msec);
  Result:=LocalToEpoch(yy,mm,dd,h,m,s);
end;

function CurrentYear:Word;
var yy,mm,dd : word;
begin
  Decodedate(now,yy,mm,dd);
  Result:=yy;
end;

Function FileDateToDateTime (Filedate : Longint) : TDateTime;

var
  y, mon, d, h, min, s: word;
begin
  EpochToLocal(FileDate,y,mon,d,h,min,s);
  Result:=ComposeDateTime(EncodeDate(y,mon,d),EncodeTime(h,min,s,0));
end;

function TryStrToDate(const S: string;  Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToDate(s);
    except
      on EConvertError do
        result:=false
    end;
  end;


function TryStrToTime(const S: string;  Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToTime(s);
    except
      on EConvertError do
        result:=false
    end;
  end;

function TryStrToDateTime(const S: string;  Value: TDateTime): Boolean;
  begin
    result:=true;
    try
      value:=StrToDateTime(s);
    except
      on EConvertError do
        result:=false
    end;
  end;


function StrToDateDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToDate(s,Result) Then
    result:=defvalue;
end;

function StrToTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToTime(s,Result) Then
    result:=defvalue;
end;

function StrToDateTimeDef(const S: string; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToDateTime(s,Result) Then
    result:=defvalue;
end;

procedure ReplaceTime(var dati:TDateTime; NewTime : TDateTime);inline;
begin
  dati:= ComposeDateTime(dati, newtime);
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime); inline;
var
  tmp : TDateTime;
begin
  tmp:=NewDate;
  ReplaceTime(tmp,DateTime);
  DateTime:=tmp;
end;


end.
