unit idt;
//Interrupt Descriptor Tables setup/INIT.
//DOES this look familiar? SURE..

//Remember Get/SEcINT Vector in dos and having to switch them out on program init/exit?
//Same thing here, only instead we reset the ENTIRE IVT (0-255) all at once
//So we can enter protected mode correctly(the old IVT gets in the way and is NOT 32-bit)
//I have had int10 working before and this is why(because the IDT wasn't properly flushed with the CPU).

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
  IDTList: array [0..48 {254}] of TIDTEntry;
  IDTPtr: TIDTPtr; //export name 'IDTPtr';

procedure SetIDTGate(Num: word; Base: LongWord; Sel: Word; Flg: Byte);
procedure InstallIDT;
procedure IDTFlush(IDTPtr1:longword); external name 'FlushIDT';
//PRT0.asm MACROS, which is why for the DOS kernel, the macros need to basically
//make the int vector(ISR pointers) look as if we are coding for DOS, when 
//we in fact are not. IDT entries have pre-init and post-complete code, 
//which is in the PRT0.asm file we normally link in.
procedure ISR0; external name 'isr0';
procedure ISR1; external name 'isr1';
procedure ISR2; external name 'isr2';
procedure ISR3; external name 'isr3';
procedure ISR4; external name 'isr4';
procedure ISR5; external name 'isr5';
procedure ISR6; external name 'isr6';
procedure ISR7; external name 'isr7';
procedure ISR8; external name 'isr8';
procedure ISR9; external name 'isr9';
procedure ISR10; external name 'isr10';
procedure ISR11; external name 'isr11';
procedure ISR12; external name 'isr12';
procedure ISR13; external name 'isr13';
procedure ISR14; external name 'isr14';
procedure ISR15; external name 'isr15';
procedure ISR16; external name 'isr16';
procedure ISR17; external name 'isr17';
procedure ISR18; external name 'isr18';
procedure ISR19; external name 'isr19';
procedure ISR20; external name 'isr20';
procedure ISR21; external name 'isr21';
procedure ISR22; external name 'isr22';
procedure ISR23; external name 'isr23';
procedure ISR24; external name 'isr24';
procedure ISR25; external name 'isr25';
procedure ISR26; external name 'isr26';
procedure ISR27; external name 'isr27';
procedure ISR28; external name 'isr28';
procedure ISR29; external name 'isr29';
procedure ISR30; external name 'isr30';
procedure ISR31; external name 'isr31';
//The first 31 are required. ISR 32-40 are in the IRQ unit.
//YES, INT33 is the KEYBOARD/MOUSE(IRQ1), though its officially on IRQ12 these days.
procedure ISR80; external name 'isr80';
//INT80 works like int 21, however, it switches to Kernel(Ring0) mode
// in addition and goes back to User mode (Ring3) 
 procedure IRQ0; external name 'irq0';
procedure IRQ1; external name 'irq1';
procedure IRQ2; external name 'irq2';
procedure IRQ3; external name 'irq3';
procedure IRQ4; external name 'irq4';
procedure IRQ5; external name 'irq5';
procedure IRQ6; external name 'irq6';
procedure IRQ7; external name 'irq7';
procedure IRQ8; external name 'irq8';
procedure IRQ9; external name 'irq9';
procedure IRQ10; external name 'irq10';
procedure IRQ11; external name 'irq11';
procedure IRQ12; external name 'irq12';
procedure IRQ13; external name 'irq13';
procedure IRQ14; external name 'irq14';
procedure IRQ15; external name 'irq15';

implementation

uses
  console,UConsole;


procedure SetIDTGate(Num: word; Base: LongWord; Sel: Word; Flg: Byte);

begin
  with IDTList[Num] do begin
    LowBase := Base and $FFFF;
	Selector := Sel;
	Always0 := 0;
    Flags := Flg;
    HiBase := (Base shr 16) and $FFFF;
  end;
end;

procedure InstallIDT;
begin
  WriteString('Installing IDT...');
  Tabulate;
  Tabulate;
  
  with IDTPtr do begin
    Limit:=SizeOf(IDTList)-1; 
    Base:=Longword(@IDTList);
  end;
//8E =interrupt gate
//8F==trap gate

  FillByte(IDTList, SizeOf(IDTList), 0);
//point to finctions in the PRT0.asm file.
  // 08 is the GDT entry(kernel mode)
  SetIDTGate(0,PtrUInt(@ISR0),$08,$8E);
  SetIDTGate(1,PtrUInt(@ISR1),$08,$8E);
  SetIDTGate(2,PtrUInt(@ISR2),$08,$8E);
  SetIDTGate(3,PtrUInt(@ISR3),$08,$8E);
  SetIDTGate(4,PtrUInt(@ISR4),$08,$8E);
  SetIDTGate(5,PtrUInt(@ISR5),$08,$8E);
  SetIDTGate(6,PtrUInt(@ISR6),$08,$8E);
  SetIDTGate(7,PtrUInt(@ISR7),$08,$8E);
  SetIDTGate(8,PtrUInt(@ISR8),$08,$8E);

  SetIDTGate(9,PtrUInt(@ISR9),$08,$8E);
  SetIDTGate(10,PtrUInt(@ISR10),$08,$8E);
  SetIDTGate(11,PtrUInt(@ISR11),$08,$8E);
  SetIDTGate(12,PtrUInt(@ISR12),$08,$8E);
  SetIDTGate(13,PtrUInt(@ISR13),$08,$8E);
  SetIDTGate(14,PtrUInt(@ISR14),$08,$8E);
  SetIDTGate(15,PtrUInt(@ISR15),$08,$8E);
  SetIDTGate(16,PtrUInt(@ISR16),$08,$8E);
  SetIDTGate(17,PtrUInt(@ISR17),$08,$8E);
  SetIDTGate(18,PtrUInt(@ISR18),$08,$8E);
  SetIDTGate(19,PtrUInt(@ISR19),$08,$8E);
  SetIDTGate(20,PtrUInt(@ISR20),$08,$8E);
  SetIDTGate(21,PtrUInt(@ISR21),$08,$8E);
  SetIDTGate(22,PtrUInt(@ISR22),$08,$8E);
  SetIDTGate(23,PtrUInt(@ISR23),$08,$8E);
  SetIDTGate(24,PtrUInt(@ISR24),$08,$8E);
  SetIDTGate(25,PtrUInt(@ISR25),$08,$8E);
  SetIDTGate(26,PtrUInt(@ISR26),$08,$8E);
  SetIDTGate(27,PtrUInt(@ISR27),$08,$8E);
  SetIDTGate(28,PtrUInt(@ISR28),$08,$8E);
  SetIDTGate(29,PtrUInt(@ISR29),$08,$8E);
  SetIDTGate(30,PtrUInt(@ISR30),$08,$8E);
  SetIDTGate(31,PtrUInt(@ISR31),$08,$8E);

  SetIDTGate(32,PtrUInt(@IRQ0),$08,$8E); 
  SetIDTGate(33,PtrUInt(@IRQ1),$08,$8E);
  SetIDTGate(34,PtrUInt(@IRQ2),$08,$8E);
  SetIDTGate(35,PtrUInt(@IRQ3),$08,$8E);
  SetIDTGate(36,PtrUInt(@IRQ4),$08,$8E);
  SetIDTGate(37,PtrUInt(@IRQ5),$08,$8E);
  SetIDTGate(38,PtrUInt(@IRQ6),$08,$8E);
  SetIDTGate(39,PtrUInt(@IRQ7),$08,$8E);
  SetIDTGate(40,PtrUInt(@IRQ8),$08,$8E);
  SetIDTGate(41,PtrUInt(@IRQ9),$08,$8E);
  SetIDTGate(42,PtrUInt(@IRQ10),$08,$8E);
  SetIDTGate(43,PtrUInt(@IRQ11),$08,$8E);
  SetIDTGate(44,PtrUInt(@IRQ12),$08,$8E);
  SetIDTGate(45,PtrUInt(@IRQ13),$08,$8E);
  SetIDTGate(46,PtrUInt(@IRQ14),$08,$8E);
  SetIDTGate(47,PtrUInt(@IRQ15),$08,$8E);
//GDT for Ring3 for Syscalls($18) --will GPF in user mode if this is for Ring0.
// $77 for interrupt, $2f for task gates
  SetIDTGate(80,PtrUInt(@ISR80),$18,$2F); 


  asm
   mov eax, [IDTPtr]  // Get the pointer to the IDT, passed as a parameter. 
   lidt [eax]        // Load the IDT pointer.
  end;
  
  textcolor(Green);
  WriteStrLn('[ OK ]');
 textcolor(8);
end;

end.

