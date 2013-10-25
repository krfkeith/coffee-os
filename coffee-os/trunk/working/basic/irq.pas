unit irq;

interface

type
  TIrqHandler = procedure (var r: TRegisters);

procedure InstallIrqHandler(const irqNo: Byte; Handler: TIrqHandler);
procedure UninstallIrqHandler(const irqNo: Byte);
procedure InstallIrq;

implementation

uses
  x86,console,idt,isr;

const
  IrqRoutines: array [0..15] of TIrqHandler = (
    nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil,nil
  );

procedure irq0; external name 'irq0';
procedure irq1; external name 'irq1';
procedure irq2; external name 'irq2';
procedure irq3; external name 'irq3';
procedure irq4; external name 'irq4';
procedure irq5; external name 'irq5';
procedure irq6; external name 'irq6';
procedure irq7; external name 'irq7';
procedure irq8; external name 'irq8';
procedure irq9; external name 'irq9';
procedure irq10; external name 'irq10';
procedure irq11; external name 'irq11';
procedure irq12; external name 'irq12';
procedure irq13; external name 'irq13';
procedure irq14; external name 'irq14';
procedure irq15; external name 'irq15';

procedure InstallIrqHandler(const irqNo: Byte; Handler: TIrqHandler);
begin
  IrqRoutines[irqNo]:=Handler;
end;

procedure UninstallIrqHandler(const irqNo: Byte);
begin
  IrqRoutines[irqNo]:=nil;
end;

procedure RemapIrq;
begin
  WritePortB($20,$11);
  WritePortB($A0,$11);
  WritePortB($21,$20);
  WritePortB($A1,$28);
  WritePortB($21,$04);
  WritePortB($A1,$02);
  WritePortB($21,$01);
  WritePortB($A1,$01);
  WritePortB($21,$0);
  WritePortB($A1,$0);
end;

procedure InstallIrq;
begin
  kWriteStr('Installing IRQ...');
  RemapIrq;
  SetIDTGate(32,PtrUInt(@irq0),$08,$8E);
  SetIDTGate(33,PtrUInt(@irq1),$08,$8E);
  SetIDTGate(34,PtrUInt(@irq2),$08,$8E);
  SetIDTGate(35,PtrUInt(@irq3),$08,$8E);
  SetIDTGate(36,PtrUInt(@irq4),$08,$8E);
  SetIDTGate(37,PtrUInt(@irq5),$08,$8E);
  SetIDTGate(38,PtrUInt(@irq6),$08,$8E);
  SetIDTGate(39,PtrUInt(@irq7),$08,$8E);
  SetIDTGate(40,PtrUInt(@irq8),$08,$8E);
  SetIDTGate(41,PtrUInt(@irq9),$08,$8E);
  SetIDTGate(42,PtrUInt(@irq10),$08,$8E);
  SetIDTGate(43,PtrUInt(@irq11),$08,$8E);
  SetIDTGate(44,PtrUInt(@irq12),$08,$8E);
  SetIDTGate(45,PtrUInt(@irq13),$08,$8E);
  SetIDTGate(46,PtrUInt(@irq14),$08,$8E);
  SetIDTGate(47,PtrUInt(@irq15),$08,$8E);
  textcolor(2);
  WriteStrLn('[ OK ]');
  textcolor(7);
end;

procedure IrqHandler(var r: TRegisters); cdecl; [public, alias: 'IRQHandler'];
var
  Handler: TIrqHandler = nil;
begin
  Handler:=IrqRoutines[r.InterruptNumber-32];
  if Assigned(Handler) then Handler(r)  else begin
	InterruptHandler(r);
  end;

  if r.InterruptNumber>=40 then WritePortB($A0,$20);
  WritePortB($20,$20);
end;

end.
