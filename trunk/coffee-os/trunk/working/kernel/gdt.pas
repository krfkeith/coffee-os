unit gdt;
//The GDT cant be in assembler due to TSS info address determination.

interface
uses
  console,uconsole,commands;

type  
Tss= packed record
	link:longword;
	esp0:longword;  //determines where the kernel isrs are located(and how to get back to ring0)
// AKA: the kernel stack location
	ss0:longword;
	esp1:longword;
	ss1:longword;
	esp2:longword;
	ss2:longword;
	cr3:longword;
	eip:longword;
	eflags:longword;
	eax:longword;
	ecx:longword; 
	edx:longword;
	ebx:longword;
	esp:longword;
	ebp:longword;
	esi:longword;
	edi:longword;
	es:longword;
	cs:longword;
	ss:longword;
	ds:longword;
	fs:longword;
	gs:longword;
	ldt:longword;

	trap:integer;
	iomap:integer;
end;

	
  TGDTEntry = packed record
    LowBase: Word;
    MiddleBase: Byte;
	HighBase: Byte;
	LowLimit: Word;
    Access: Byte;
    Granularity: Byte;
    
  end;

  TGDTPtr = packed record
    Limit: Word;
    Base: LongWord;
  end;

var
  GDTList: array [0..7] of TGDTEntry;
  GDTPtr: TGDTPtr; export name 'gdtr';
  TssPtr:Tss; export name 'TSSPtr'; //linked list. need this pointer.

procedure SetGate(Num: Byte; Base, Limit: LongWord; Acc, Gran: Byte);
procedure InstallGDT;
procedure GDTFlush(gdt:TGDTPtr; size:longword); external name 'FlushGDT';
procedure TSSFlush; external name 'FlushTSS';


implementation



procedure SetGate(Num: Byte; Base, Limit: LongWord; Acc, Gran: Byte);
begin
  with GDTList[Num] do begin
    LowBase := (Base and $FFFF);
    MiddleBase := (Base shr 16) and $FF;
	HighBase := (Base shr 24) and $FF;
	LowLimit := (Limit and $FFFF);
	Access := Acc;
    Granularity := ((Limit shr 16) and $0F) or (Gran and $F0);
  end;
end;


procedure InstallGDT;

var

  address,size,ss0,esp0:longword;

begin
  WriteString('Installing GDT...'#9#9);
 { FillChar(GDTList,SizeOf(GDTList),#0); 
  with GDTPtr do begin
    Limit := SizeOf(GDTList) - 1;
    Base := Longword(@GDTList);
  end;
  GDT.SetGate(0, 0, 0, 0, 0); // nil descriptor

  GDT.SetGate(1, 0, $FFFFFFFF, $9A, $CF); // Kernel space code
  GDT.SetGate(2, 0, $FFFFFFFF, $92, $CF); // Kernel space data

  //SO far RING protection only.
  GDT.SetGate(3, 0, $FFFFFFFF, $FA, $CF); // User space code
  GDT.SetGate(4, 0, $FFFFFFFF, $F2, $CF); // User space data
  
   address:=longword(@tssPtr);
   size:=(sizeof(tssPtr)-1); 

   GDT.SetGate(5, address,size, $89, 0); 
 
  //initprio:=200;

   asm
	  mov [esp0],esp
   end;
   TssPtr.ss0:= $10;  // Set the kernel stack segment. 
  // TssPtr.esp0 := esp0; -- Set the kernel stack pointer. We get this at a syscall.
   TssPtr.eflags:=$102;
   // Here we set the cs, ss, ds, es, fs and gs entries in the TSS. These specify what
   // segments should be loaded when the processor switches to kernel mode. Therefore
   // they are just our normal kernel code/data segments - 0x08 and 0x10 respectively,
   // but with the last two bits set, making 0x0b and 0x13. The setting of these bits
   // sets the RPL (requested privilege level) to 3, meaning that this TSS can be used
   // to switch to kernel mode from ring 3.

   TssPtr.cs := $0b;
   TssPtr.ss := $13; 
   TssPtr.ds := $13;
   TssPtr.es := $13;
   TssPtr.fs := $13;
   TssPtr.gs :=$13;

// now set the IO bitmap (not necessary, so set above limit)
   TssPtr.iomap := sizeof( tss);

  // TSSFlush;
   GDT.SetGate(6, 0, 0, 0, 0); // TSS 3XF section

 //need to rewrite these FIVE if setting up PM EP.
  //More hassle than worth right now for 16-bit PM segments.
 //  GDT.SetGate(7, $0b800, $3FFF, $92, $CF);  
//   GDT.SetGate(8, $0a000, $FFFF, $92, $CF); 
//   GDT.SetGate(9, $0b000, $FFFF, $92, $CF);
 //  GDT.SetGate(10, $0c000, $7FFF, $9A, $CF); 
  
  //VBIOS SHADOW COPY will be put here. DONT WORRY, not using.
  //GDT.SetGate(11, $06000, $7FFFFFFF, $9A, $CF); // User space data
  
 //ALL BIOS SECTIONS start in adaptor ROM at C000.
 
 // GDT.SetGate(12, 0, $FFFFFFFF, $9A, $40); // APM1-32bit
 // GDT.SetGate(13, 0, $FFFFFFFF, $9A, $00); // APM2-16bit
 
 // GDT.SetGate(14, 0, $FFFFFFFF, $92, $40); // APM3-16bit

 // GDT.SetGate(15, 0, $FFFFFFFF, $9A, $40); // PNP BIOS CS32
 // GDT.SetGate(16, 0, $FFFFFFFF, $9A, $00); // PNP BIOS CS16
  //GDT.SetGate(17, 0, $FFFFFFFF, $92, $00); // PNP BIOS DS
  //GDT.SetGate(18, 0, $FFFFFFFF, $9A, $40); // PNP BIOS TS1
  //GDT.SetGate(19, 0, $00000000, $9A, $00); // PNP BIOS TS2
  
 // GDTFlush(GDTPtr,sizeof(GDTPtr));
 }
   textcolor(green);
	WriteStrLn(' [ OK ]');
	textcolor(8);
	update_cursor;
end;

end.
