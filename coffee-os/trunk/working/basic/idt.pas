unit idt;

interface

type

  TIDTEntry = packed record
    LowBase: Word;
    Selector: Word;
    Always0: Byte;
    Flags: Byte;
    HiBase: Word;
  end;

  TIDTPtr = packed record
    Limit: Word;
    Base: LongWord;
  end;

var
  IDTList: array [0..255] of TIDTEntry;
  IDTPtr: TIDTPtr;

procedure SetIDTGate(num: Byte; base: LongWord; sel: Word; flg: Byte);
procedure InstallIDT;

implementation

uses
  console;

procedure LoadIDT; assembler; nostackframe;
asm
  lidt [IDTPtr]
end;

procedure SetIDTGate(num: Byte; base: LongWord; sel: Word; flg: Byte);
begin
  with IDTList[num] do begin
    LowBase:=base and $FFFF;
    HiBase:=(base shr 16) and $FFFF;
    Selector:=sel;
    Always0:=0;
    Flags:=flg;
  end;
end;

procedure InstallIDT;
begin
  kWriteStr('Installing IDT...');
  with IDTPtr do begin
    Limit:=SizeOf(IDTList)-1;
    Base:=PtrUInt(@IDTList);
  end;
  FillByte(IDTList,SizeOf(IDTList),0);
  LoadIDT;
  textcolor(2);
  WriteStrLn('[ OK ]');
  textcolor(7);
end;

end.
