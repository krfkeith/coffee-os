
{*******************************************************}
{                                                       }
{       Turbo Pascal Version 7.0                        }
{       Turbo Vision Unit                               }
{                                                       }
{       Copyright (c) 1992 Borland International        }
{                                                       }
{*******************************************************}

unit TextView;

{$O+,F+,X+,I-,S-}

interface

uses Objects, Drivers, Views, Dos;

type

  { TTextDevice }

  PTextDevice = ^TTextDevice;
  TTextDevice = object(TScroller)
    function StrRead(var S: TextBuf): Byte; virtual;
    procedure StrWrite(var S: TextBuf; Count: Byte); virtual;
  end;

  { TTerminal }

  PTerminalBuffer = ^TTerminalBuffer;
  TTerminalBuffer = array[0..65534] of Char;

  PTerminal = ^TTerminal;
  TTerminal = object(TTextDevice)
    BufSize: Word;
    Buffer: PTerminalBuffer;
    QueFront, QueBack: Word;
    constructor Init(var Bounds:TRect; AHScrollBar, AVScrollBar: PScrollBar;
      ABufSize: Word);
    destructor Done; virtual;
    procedure BufDec(var Val: Word);
    procedure BufInc(var Val: Word);
    function CalcWidth: Integer;
    function CanInsert(Amount: Word): Boolean;
    procedure Draw; virtual;
    function NextLine(Pos:Word): Word;
    function PrevLines(Pos:Word; Lines: Word): Word;
    function StrRead(var S: TextBuf): Byte; virtual;
    procedure StrWrite(var S: TextBuf; Count: Byte); virtual;
    function QueEmpty: Boolean;
  end;

procedure AssignDevice(var T: Text; Screen: PTextDevice);

implementation

{ TTextDevice }

function TTextDevice.StrRead(var S: TextBuf): Byte;
begin
  StrRead := 0;
end;

procedure TTextDevice.StrWrite(var S: TextBuf; Count: Byte);
begin
end;

{ TTerminal }

constructor TTerminal.Init(var Bounds:TRect; AHScrollBar,
  AVScrollBar: PScrollBar; ABufSize: Word);
begin
  TTextDevice.Init(Bounds, AHScrollBar, AVScrollBar);
  GrowMode := gfGrowHiX + gfGrowHiY;
  BufSize := ABufSize;
  if BufSize > 65520 then BufSize := 65520;
  GetMem(Buffer, BufSize);
  QueFront := 0;
  QueBack := 0;
  SetLimit(0,1);
  SetCursor(0,0);
  ShowCursor;
end;

destructor TTerminal.Done;
begin
  FreeMem(Buffer, BufSize);
  TTextDevice.Done;
end;

procedure TTerminal.BufDec(var Val: Word);
begin
  if Val = 0 then Val := BufSize - 1
  else Dec(Val);
end;

procedure TTerminal.BufInc(var Val: Word);
begin
  Inc(Val);
  if Val >= BufSize then Val := 0;
end;

function TTerminal.CalcWidth: Integer;
var
  I, Len, Width: Integer;
  CurPos, EndPos: Integer;
begin
  Width := 0;
  CurPos := QueBack;
  for I := 1 to Limit.Y do
  begin
    EndPos := NextLine(CurPos);
    if EndPos >= CurPos then
      Len := EndPos - CurPos else
      Len := BufSize - CurPos + EndPos;
    if Buffer^[EndPos-1] = #10 then
      Dec(Len) else
      Inc(Len);
    if Len > Width then
      Width := Len;
    CurPos := EndPos;
  end;
  CalcWidth := Width;
end;

function TTerminal.CanInsert(Amount: Word): Boolean;
var
  T: Longint;
begin
  if QueFront < QueBack then T := QueFront + Amount
  else T := LongInt(QueFront) - LongInt(BufSize) + Amount;
  CanInsert := QueBack > T;
end;

procedure TTerminal.Draw;
var
  I: Integer;
  BegLine, EndLine: Word;
  S: String;
  T: Longint;
  BottomLine: Word;
begin
  BottomLine := Size.Y + Delta.Y;
  if Limit.Y > BottomLine then
  begin
    EndLine := PrevLines(QueFront, Limit.Y-BottomLine);
    BufDec(EndLine);
  end
  else EndLine := QueFront;
  if Limit.Y-1 >= Size.Y then I := Size.Y-1
  else
  begin
    for I := Limit.Y to Size.Y-1 do
      WriteChar(0, I, ' ', 1, Size.X);
    I := Limit.Y-1;
  end;
  for I := I downto 0 do
  begin
    BegLine := PrevLines(EndLine,1);
    if EndLine >= BegLine then
    begin
      T := EndLine - BegLine;
      Move(Buffer^[BegLine], S[1], T);
      S[0] := Char(T);
    end
    else
    begin
      T := BufSize - BegLine;
      Move(Buffer^[BegLine], S[1], T);
      Move(Buffer^, S[T+1], EndLine);
      S[0] := Char(T + EndLine);
    end;
    if Delta.X >= Length(S) then S := ''
    else S := Copy(S, Delta.X+1, 255);
    WriteStr(0, I, S, 1);
    WriteChar(Length(S), I, ' ', 1, Size.X);
    EndLine := BegLine;
    BufDec(EndLine);
  end;
end;

function TTerminal.NextLine(Pos:Word): Word;
begin
  if Pos <> QueFront then
  begin
    while (Buffer^[Pos] <> #10) and (Pos <> QueFront) do
      BufInc(Pos);
    if Pos <> QueFront then BufInc(Pos);
  end;
  NextLine := Pos;
end;

procedure DecDi; near; assembler;
asm
	CMP	DI,WORD PTR [SI].TTerminal.Buffer
	JA	@@1
	ADD	DI,WORD PTR [SI].TTerminal.BufSize
@@1:	DEC	DI
end;

procedure IncDi; near; assembler;
asm
	INC	DI
	MOV	AX,WORD PTR [SI].TTerminal.Buffer
	ADD	AX,[SI].TTerminal.BufSize
	CMP	DI,AX
	JB	@@1
	MOV	DI,WORD PTR [SI].TTerminal.Buffer
@@1:
end;

function TTerminal.PrevLines(Pos:Word; Lines:Word): Word; assembler;
const
  LineSeparator = #10;
asm
	PUSH	DS
	LDS	SI,Self
	LES	DI,[SI].TTerminal.Buffer
	ADD	DI,Pos
@@1:    MOV	CX,Lines
	JCXZ	@@6
	MOV	AX,[SI].TTerminal.QueBack
	ADD	AX,WORD PTR [SI].TTerminal.Buffer
	CMP	DI,AX
	JE	@@7
	CALL	DecDI
@@2:    MOV	AX,[SI].TTerminal.QueBack
	ADD	AX,WORD PTR [SI].TTerminal.Buffer
	CMP	DI,AX
	JA	@@3
	MOV	CX,DI
	SUB	CX,WORD PTR [SI].TTerminal.Buffer
	JMP	@@4
@@3:    MOV	CX,DI
	SUB	CX,AX
@@4:    MOV	AL,LineSeparator
	INC	CX
	STD
	REPNE	SCASB
	JE	@@5
	MOV	AX,DI
	SUB	AX,WORD PTR [SI].TTerminal.Buffer
	INC	AX
	CMP	AX,[SI].TTerminal.QueBack
	JE	@@8
	MOV	DI,WORD PTR [SI].TTerminal.Buffer
	ADD	DI,WORD PTR [SI].TTerminal.BufSize
	DEC	DI
	JMP	@@2
@@5:	DEC	Lines
	JNZ	@@2
@@6:    CALL	IncDI
	CALL	IncDI
	MOV	AX,DI
@@7:    SUB	AX,WORD PTR [SI].TTerminal.Buffer
@@8:    POP	DS
end;

function TTerminal.StrRead(var S: TextBuf): Byte;
begin
  StrRead := 0;
end;

procedure TTerminal.StrWrite(var S: TextBuf; Count: Byte);
var
  I, J: Word;
  ScreenLines: Word;
begin
  if Count = 0 then
    Exit else
    if Count >= BufSize then
      Count := BufSize-1;
  ScreenLines := Limit.Y;
  J := 0;
  for I := 0 to Count-1 do
    case S[I] of
      #13: Dec(Count)
      else
      begin
        if S[I] = #10 then Inc(ScreenLines);
        S[J] := S[I];
        Inc(J);
      end;
    end;

  while not CanInsert(Count) do
  begin
    QueBack := NextLine(QueBack);
    Dec(ScreenLines);
  end;

  if LongInt(QueFront) + Count >= BufSize then
  begin
    I := BufSize - QueFront;
    Move(S,Buffer^[QueFront], I);
    Move(S[I],Buffer^, Count - I);
    QueFront := Count - I;
  end
  else
  begin
    Move(S,Buffer^[QueFront],Count);
    Inc(QueFront,Count);
  end;
  SetLimit(CalcWidth,ScreenLines);
  ScrollTo(0, ScreenLines+1);
  I := PrevLines(QueFront,1);
  if I <= QueFront then I := QueFront - I
  else I := BufSize - (I - QueFront);
  SetCursor(I, ScreenLines-Delta.Y-1);
  DrawView;
end;

function TTerminal.QueEmpty: Boolean;
begin
  QueEmpty := QueBack = QueFront;
end;

{ Window Text Device Driver }

type
  WindowData = record
    Screen: PTextDevice;
    Filler: Array [1..12] of Char;
  end;

function WindowWrite(var F: TextRec): Integer; far;
begin
  with F do
  begin
    WindowData(UserData).Screen^.StrWrite(BufPtr^, BufPos);
    BufPos := 0;
  end;
  WindowWrite := 0;
end;

function WindowRead(var F: TextRec): Integer; far;
begin
  with F do
  begin
    BufPos := 0;
    BufEnd := WindowData(F.UserData).Screen^.StrRead(BufPtr^);
  end;
  WindowRead := 0;
end;

function WindowFlush(var F: TextRec): Integer; far;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  WindowFlush := 0;
end;

function WindowOpen(var F: TextRec): Integer; far;
begin
  with F do
  begin
    if Mode = fmInput then
    begin
      InOutFunc := @WindowRead;
      FlushFunc := @WindowFlush;
    end
    else
    begin
      InOutFunc := @WindowWrite;
      FlushFunc := @WindowWrite;
    end;
    WindowOpen := 0;
  end;
end;

function WindowIgnore(var F: TextRec): Integer; far;
begin
  WindowIgnore := 0;
end;

var
  Buffer: TextBuf;

procedure AssignDevice(var T: Text; Screen: PTextDevice);
begin
  with TextRec(T) do
  begin
    Handle := $FFFF;
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @WindowOpen;
    CloseFunc := @WindowIgnore;
    WindowData(UserData).Screen:= Screen;
  end;
end;

end.
