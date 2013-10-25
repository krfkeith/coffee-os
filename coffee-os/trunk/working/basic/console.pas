{
/////////////////////////////////////////////////////////
//                                                     //
//               Freepascal barebone OS                //
//                       console.pas                   //
//                                                     //
/////////////////////////////////////////////////////////
//
//      By:             De Deyn Kim <kimdedeyn@skynet.be>
//      License:        Public domain
//
}
 
unit console;
 
interface
 
var
        xpos: Integer = 0;
        ypos: Integer = 0;
 	Blank: Word;
        vidmem: PChar = PChar($b8000);
 	 TextAttr: Word = $07;
	 HexTbl : array[0..15] of char='0123456789ABCDEF';

procedure kclearscreen;
procedure Scroll;
function GetTextColor: Integer;
function GetTextBackground: Integer;
procedure textcolor(color:word);
Procedure TextBackground(Color: word);
procedure WritedwordLn(l: DWord);
procedure update_cursor;
procedure kwritechr(c: Char);
procedure kwritestr(s: PChar);
procedure kwriteint(i: Integer);
procedure WriteIntLn(i: Integer);
procedure WriteLong(l: LongWord);
procedure WriteLongLn(l: LongWord);
procedure kwritedword(i: DWORD);
procedure WriteStrLn( S: PChar);
procedure WritePCharLn(P: PChar);
procedure WritePChar(P: PChar);
function GetXY:Word;
function GetX: Word;
function GetY: Word;
procedure Tabulate;
procedure PrintHex(hex: DWORD);
function hexstr(val : longint;cnt : byte) : string;
Function Hex_Char(Number: Word): Char;
procedure PrintHexln(hex: DWORD);
implementation

uses x86;

procedure Tabulate; [public, alias: 'Tabulate'];
  Var x:Integer;
  begin
     kwritestr('        '); //eight spaces
//need to set up tab stops, though...
  end;


//could use modified version of GetXY for these, but this works ok.
function GetX: Word;
begin
  GetX := xPos;
end;

function GetY: Word;
begin
  GetY := yPos;
end;
function GetXY:Word;
var
  offset:Word;
begin
   writeportb($3D4, 14);
   offset:=(readportb($3D5) shl 8);
   writeportb($3D4, 15);
   offset:=(offset or readportb($3D5));
   GetXY:=offset;
end;

procedure WritePCharLn(P: PChar);
begin
  WritePChar(P);
  xpos:=0;
  inc(yPos);
end;

procedure WritePChar(P: PChar); [public, alias: 'WritePChar'];
begin
  while P^ <> #0 do begin
    kWriteChr(P^);
    Inc(P);
  end;
end;

procedure kclearscreen; [public, alias: 'kclearscreen'];
var
  i: Byte;
begin
  Blank := $0 or (textAttr shl 8);
  for i := 0 to 25 do
    FillWord((VidMem + i * 2 * 80)^, 80, Blank);
  XPos := 0;
  YPos := 0;
end;
 

procedure Scroll;
//this is whats causing crashes. FIXME:Virtualbox not affected.QEMU BUG?
//FIXME: noted on Launchpad.net for KVM.
 
begin
//  if scrolldisabled then exit;
      if (Ypos >= 24) then begin  //in case called before end of screen
    blank:= $20 or (TextAttr shl 8);
    Move((VidMem+(2*80))^,VidMem^,24*(2*80));
    // Empty last line
    FillWord((VidMem+(24*2*80))^,80,Blank);
    XPos:=1;
    YPos:=23;
 //   update_cursor;
  end;
end;

function GetTextColor: Integer;
begin
  GetTextColor := TextAttr and $F0;
end;

function GetTextBackground: Integer;
begin
  GetTextBackground := (TextAttr and $F0) shr 4;
end;

procedure textcolor(color:word);

Begin
  TextAttr := (TextAttr and $F0) or (Color and $0F);
End;

Procedure TextBackground(Color: word);
Begin
 TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
End;

procedure writeDwordln(l:dword); [public, alias: 'kwritedwordln'];

begin
kwritedword(l);
//should wrap to next line
inc(YPos);
XPos:=0;
end;

procedure update_cursor;
//USES CRTC regis to do this, way faster than thunking to 16-bit.
var
  Temp: smallint;
  pos:integer;

begin
  pos:=((xpos*80)*2)+ypos;
    Temp:=GetXY; //get current location and blink
    writeportb($3D4,$0F);
     writeportb($3D5,(pos and $FF));
     writeportb($3D4,$0E);
     writeportb($3D5,((pos SHR 8) and $FF));

    writeportb($3D4, 14);
    writeportb($3D5, ((temp shr 8) and $ff));
    writeportb($3D4, 15);
    writeportb($3D5, (temp and $ff));
end;

procedure kwritechr(c: Char); [public, alias: 'kwritechr'];
var
        offset: Integer;
begin
        if (ypos > 24) then
                ypos := 0;
 
        if (xpos > 79) then
                xpos := 0;
 
        offset := (xpos shl 1) + (ypos * 160);
        vidmem[offset] := c;
        offset += 1;
        vidmem[offset] := char(textattr);
        offset += 1;
 
        xpos := (offset mod 160);
        ypos := (offset - xpos) div 160;
        xpos := xpos shr 1;
end;
 
procedure kwritestr(s: PChar); [public, alias: 'kwritestr'];
var
        offset, i: Integer;
begin
        if (ypos > 24) then
                ypos := 0;
 
        if (xpos > 79) then
                xpos := 0;
 
        offset := (xpos shl 1) + (ypos * 160);
        i := 0;
 
        while (s[i] <> Char($0)) do
        begin
                vidmem[offset] := s[i];
                offset += 1;
                vidmem[offset] := char(textattr);
                offset += 1;
                i += 1;
        end;
 
        xpos := (offset mod 160);
        ypos := (offset - xpos) div 160;
        xpos := xpos shr 1;
end;
 

procedure WriteStrLn(S: PChar);
begin
  kWriteStr(S);
  xpos:=0;
  inc(yPos);
end;


Function Hex_Char(Number: Word): Char;
Begin
   If Number<10 then
	Hex_Char:=Char(Number+48)
   else
	Hex_Char:=Char(Number+55);
end; 

procedure PrintHex(hex: DWORD);
var
	C: Char;
  Hexa: Byte;
  I, Shift: Byte;
begin
  kwritechr('0');
  kwritechr('x');
  for I := 7 downto 0 do
  begin
  	Shift := I*4;
    Hexa := (hex shr Shift) and $0F;
    C := Hex_Char(Hexa);
    kwritechr(C);
  end;
end;

procedure PrintHexln(hex: DWORD);
begin 
 printhex(hex);
  xpos:=0;
  inc(yPos);
end;

function hexstr(val : longint;cnt : byte) : string;
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;
procedure kwriteint(i: Integer); [public, alias: 'kwriteint'];
var
        buffer: array [0..11] of Char;
        str: PChar;
        digit: DWORD;
        minus: Boolean;
begin
        str := @buffer[11];
        str^ := #0;
 
        if (i < 0) then
        begin
                digit := -i;
                minus := True;
        end
        else
        begin
                digit := i;
                minus := False;
        end;
 
        repeat
                Dec(str);
                str^ := Char((digit mod 10) + Byte('0'));
                digit := digit div 10;
        until (digit = 0);
 
        if (minus) then
        begin
                Dec(str);
                str^ := '-';
        end;
 
        kwritestr(str);
end;
 

procedure WriteIntLn(i: Integer);

begin
  kwriteint(i);
  xpos:=0;
  inc(yPos);
end;

procedure WriteLong(l: LongWord);
var
  s: Pchar;
begin
  //Str(l, s);
  kWriteStr(s);
end;

procedure WriteLongLn(l: LongWord);
begin
  WriteLong(l);
  xpos:=0;
  inc(yPos);
end;

procedure kwritedword(i: DWORD); [public, alias: 'kwritedword'];
var
        buffer: array [0..11] of Char;
        str: PChar;
        digit: DWORD;
begin
        for digit := 0 to 10 do
                buffer[digit] := '0';
 
        str := @buffer[11];
        str^ := #0;
 
        digit := i;
        repeat
                Dec(str);
                str^ := Char((digit mod 10) + Byte('0'));
                digit := digit div 10;
        until (digit = 0);
 
        kwritestr(@Buffer[0]);
end;
 
end.
