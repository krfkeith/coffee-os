unit console;

{
OS Barebones Console and VTerm unit.

Notes:
update_cursor on new lines, on each char/word is excessive
blinkcursor should be off able and only called once, update the cursor instead.

}

interface

type
   string255=string[255];

const
{Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

  ConsoleMaxX  = 25;
  ConsoleMaxY  = 80;


var
  HexTbl : array[0..15] of char='0123456789ABCDEF';
  ScreenHeight : longint = 25;
  ScreenWidth  : longint = 80; 

    stringline:string255;
  EchoToConsole:boolean;
  // Color attribute
  TextAttr: Word = $0F;
  Bold:Word;
  Negative:Word;
  UndoNegative:Word;
  UndoBold:Word;
  UndoBlinking:Word;  
  CursorPosX: Word = 0;
  CursorPosY: Word = 0;
 // VidMem: PChar=PChar($B8000);
  
   ScrollDisabled:boolean;
  keypressed,IsConsoleActive:boolean;
// cursornormalHI, cursornormalLO:word;   

function upcase(p : char) : char;assembler;
procedure scroll_backwards; 
Procedure Ring;
procedure PrintDecimal(Value: DWORD);
procedure change_tty (tty_index : byte); 
procedure Sound(Hertz: longword); 
Procedure click; 
procedure nosound; 
procedure beep;
PROCEDURE Boop;
PROCEDURE Blat;
PROCEDURE Tick;
Procedure Window(X1, Y1, X2, Y2: Byte);

implementation

uses
  x86, keybrd,isr,timer,vmm,pmm,UConsole;
//UserMode Console for the moment. The functions HERE need some rewrite(mostly readport/writeport)
// when Ring3 code is ready..
//UConsole will need a rewrite for the functions here when that time comes...
// The rest dont modify low-level registers enough to trip a GPF.
// (even though they *MAY* modify kernel -level ram(another no-no unless inside of an interrupt..) 
//so while this unit *MIGHT* use the other's functions..it requires some work for the obverse to be true...

var

  // Blank (space) character for current color
  Blank: Word;
  color:byte;
  CurrX,CurrY : Byte;
   
  x,y:word;
   lastmode: word;            // text mode of CRT 

  WindMinX : Word;
  WindMaxX : Word;
  WindMinY : Word;
  WindMaxY : Word;
 WindMin: Word  = $0;         //  Window upper left coordinates 
  WindMax: Word  = $0FA0;        //(4000) Window lower right coordinates

 Blink :Word =$128;

const

   screen_resolution = (80 * 25);
   screen_size = screen_resolution * 2;     { 4000 bytes MAX for TEXT mode}

{$asmmode att}
function upcase(p : char) : char;assembler;
var
  saveeax,saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        movl    p,%eax
        movl    %eax,saveeax
        movl    p,%esi
        orl     %esi,%esi
        jz      .LStrUpperNil
        movl    %esi,%edi
.LSTRUPPER1:
        lodsb
        cmpb    $97,%al
        jb      .LSTRUPPER3
        cmpb    $122,%al
        ja      .LSTRUPPER3
        subb    $0x20,%al
.LSTRUPPER3:
        stosb
        orb     %al,%al
        jnz     .LSTRUPPER1
.LStrUpperNil:
        movl    saveeax,%eax
        movl    saveedi,%edi
        movl    saveesi,%esi
end;
{$asmmode intel}

procedure scroll_backwards; 
//Works ok.NICE!!
begin
  
    blank:= $20 or (TextAttr shl 8);
    Move(VidMem^,(VidMem+2*80)^,24*2*80); //inverted move
    // fill last line
    
    CursorPosX:=0;
    CursorPosY:=23;
    update_cursor;
    //every call should scroll back 1 line.
    //reset with scroll + 'Coffee> '
end;


{$ASMMODE intel}
Procedure Ring;

var 
   i:word;
begin
  for i:=0 to 6 do
  begin
      sound(523);
      Delay(50);
      sound(659);
      Delay(50);
  end;
  nosound;
end;
//the piano is in the playmusic unit.... :-0


// print a dword in decimal
procedure PrintDecimal(Value: DWORD);
var
	I, Len: Byte;
  S: string[32];
begin
	Len := 0;
  I := 10;
  if (Value and $80000000) = $80000000 then
  begin
  	asm
	    mov   eax, Value
	    not   eax
	    inc   eax
	    mov   Value, eax
	end;
	 	writeChar('-');
  end;
  if Value = 0 then
  begin
  	writechar('0');
  end else begin
  	while Value <> 0 do
    begin
    	S[I] := Char((Value mod 10) + $30);
      Value := Value div 10;
      I := I-1;
      Len := Len+1;
    end;
    if (Len <> 10) then
    begin
    	S[0] := char(Len);
      for I := 1 to Len do
      begin
      	S[I] := S[11-Len];
        Len := Len-1;
      end;
    end else begin
    	S[0] := char(10);
    end;
    for I := 1 to ord(S[0]) do
    begin
    	writechar(S[I]);
    end;
  end;
end;

procedure change_tty (tty_index : byte); [public, alias : 'CHANGE_TTY'];

var
   ofs : word;
   current_tty:byte;

begin
   if tty_index<1 then exit;
   ofs := (tty_index) * screen_resolution;
//switch the active VRAM 4K buffer page
   writeportb($3d4,$0c);
   writeportb($3d5,hi(ofs));
   writeportb($3d4,$0d);
   writeportb($3d5,lo(ofs));
   current_tty := tty_index;
   update_cursor;

end;

procedure Sound(Hertz: longword); 
var
  freq:word; 
  Divisor:longword;
  temp:Byte;
begin
{
   Plays a tone thru the PC speaker using the 8253/8254
   Counter/Timer chip and the 8255 Programmable Peripheral
   Interface (PPI) chip on the motherboard. 
   **RING0 (KERNEL) code.**
   }
//   OVERRIDE PIT2:

    // Compute the frequency data
 Divisor := 1193180 div Hertz; //PC Speaker only handles 11Khz samples.
 //note division change, this is not the same as the system timer.

    // Prepare the 8253/8254 to receive the frequency data

  WritePortB($43, $36);
  WritePortB($42, Divisor); //HI
  WritePortB($42, Divisor shr 8); //LO

   // Read Port B of the 8255 PPI
  Temp := ReadPortB($61);
   // Tell the 8255 PPI to start the sound (and value by 3)
  if Temp <> (Temp or 3) then
   //..and Write to Port B of the 8255 PPI
    WritePortB($61, Temp or 3);
//works ok. Need to retest with QEMU.

end;

Procedure click; 
//set the freq and call delay first
var
  temp:longword;
begin
sound(1300);
Delay(30);
end;

procedure nosound; 
{turns the speaker off}
var
  Temp: Byte;
begin
  Temp := ReadPortB($61) and $FC; //(Readportb(61) and not 3)
  WritePortB($61, Temp);
end;

procedure beep;
var
i:integer;
   begin { a DOS-Like LOUD ASS beep }
        
	sound(800);
        Delay(250);
        nosound; //off
   end;

PROCEDURE Boop;
BEGIN 
	Sound(100);
	Delay(100);
  	Sound(50); 
	Delay(200); 
	NoSound;
END;

PROCEDURE Blat;
VAR I:byte;
BEGIN
	for I:=1 to 30 do begin 
		sound(2*i+60); 
		delay(15); 
	end;
	nosound;
end {blat};

PROCEDURE Tick;
BEGIN 
	Sound(500); 
	delay(10); 
	nosound; 
END;


{setmode allows for base text mode switching.
  
CRT modes:

  CO40          = 1;             40x25 Color on Color Adapter 
  BW40		=  2;
  CO80          = 3;             80x25 Color on Color Adapter
  BW80		=  4;
   
}


{
function pwcrypt(op : pchar) : pchar; //for use in password files OR :-) system auth. [never use clear text]
var
 ptr : integer;
 ip : pchar;
begin
 ip := '';
 ptr := 1;
 repeat
  ip := ip+char(((ord(op[ptr])+ord(op[sizeof(op)-ptr]) xor sizeof(op))));
  ip[ptr] := char(ord(ip[ptr])+2);
  inc(ptr);
 until ptr = sizeof(op)+1;
 pwcrypt := ip;
end;
}

Procedure Window(X1, Y1, X2, Y2: Byte);

//  Set screen window to the specified coordinates.

Begin
  if (X1>X2) or (X2>ScreenWidth) or (Y1>Y2) or (Y2>ScreenHeight) then
    exit;
  WindMinX:=X1;
  WindMaxX:=X2;
  WindMinY:=Y1;
  WindMaxY:=Y2;
  WindMin:=((Y1-1) Shl 8)+(X1-1);
  WindMax:=((Y2-1) Shl 8)+(X2-1);
  GoToXY(WindMinX,WindMinY); //the windows first x and y, not screen x=1 and y=1...
  update_cursor;
End;


BEGIN
  keypressed:=false; //IRQ driven. 
  Blink := (textattr or $80);
  Bold:= (textattr or $08);
  Negative:= (textattr shr 4);
  UndoNegative:= (textattr shl 4);
  UndoBold:= (textattr and $F7);
  UndoBlinking:=(textattr and $7F);

//Get the STANDARD blinking cursor size(I dunno what it is...)
  writeportb($3D4, $0A);
  cursornormalLO:=(readportb($3D5)shl 8);
  writeportb($3D4, $0B);
  cursornormalHI:= (cursornormalLO or readportb($3D5));
end.

