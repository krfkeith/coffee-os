unit vmm;

interface

const
  PageDirVirtAddr = $FFBFF000;
  PageTableVirtAddr = $FFC00000;

type
  UBit3 = 0..(1 shl 3) - 1;
  UBit20 = 0..(1 shl 20) - 1;

  PPageTableEntry = ^TPageTableEntry;

  TPageTableEntry = bitpacked record
    Present, Writable, UserMode, WriteThrough,
    NotCacheable, Accessed, Dirty, AttrIndex,
    GlobalPage: Boolean;
    Avail: UBit3;
    FrameAddr: UBit20;
  end;

  PPageDirEntry = ^TPageDirEntry;

  TPageDirEntry = bitpacked record
    Present, Writable, UserMode, WriteThrough,
    NotCacheable, Accessed, Reserved, PageSize,
    GlobalPage: Boolean;
    Avail: UBit3;
    TableAddr: UBit20;
  end;

  PPageTable = ^TPageTable;
  TPageTable = array [0..1023] of TPageTableEntry;
  PPageDir = ^TPageDir;
  TPageDir = array [0..1023] of TPageDirEntry;

var
  current_directory,current_page:longword;
  kernel_directory:longword; export name 'kernel_directory';
  PageDir: PPageDir = PPageDir(PageDirVirtAddr);
  PageTables: PPageTable = PPageTable(PageTableVirtAddr);
  CurrentPageDir: PPageDir;
 
procedure InstallVMM;
procedure SwitchPageDir(const pd: PPageDir);
procedure Map(const va, pa: LongWord; const IsPresent, IsWritable, IsUserMode: Boolean);
procedure UnMap(const va: LongWord);
function GetMapping(const va: LongWord; pa: PLongWord): Boolean;

function VirtToPhys(va:Longword):Longword;

implementation

uses
  console,uconsole, isr, pmm;

function PageDirIndex(const x: LongWord): LongWord;
begin
  PageDirIndex := x div 1024;
end;

function PageTableIndex(const x: LongWord): LongWord;
begin
  PageTableIndex := x mod 1024;
end;

procedure PageFaultHandler(var r: TRegisters);
var
  FaultAddr: LongWord;
  Present, ReadOnly, UserMode, Reserved, InstrFetch: Boolean;
begin
  asm
    mov eax,cr2
    mov FaultAddr,eax
  end ['eax'];
  Present := r.ErrorCode and 1 <> 0;
  ReadOnly := r.ErrorCode and 2 = 0;
  UserMode := r.ErrorCode and 4 = 0;
  Reserved := r.ErrorCode and 8 = 0;
  InstrFetch := r.ErrorCode and 16 = 0;
  WriteStrLn('Page Fault at $' + HexStr(r.EIP, 8) + ', Faulting address = $' +
    HexStr(FaultAddr, 8));
  WriteString('Page is ');
  if Present then
    WriteString('present ');
  if ReadOnly then
    WriteString('read-only ');
  if UserMode then
    WriteString('user-mode ');
  if Reserved then
    WriteString('reserved ');
  WriteChar(#10);
  while True do ;
end;

procedure InstallVMM;
var
  i, ptIndex: LongWord;
  pd: PPageDir;
  pt: PPageTable;
begin
  WriteString('Installing VMM...'#9#9);
  // Register the page fault handler
  InstallISRHandler(14, @PageFaultHandler);
  // Create a page directory
  pd := PPageDir(AllocPage);
  // Initialise it
  FillByte(pd^, PageSize, 0);
  // Identity map the first 4 MB
  with pd^[0] do begin
    TableAddr := AllocPage shr 12;
    Present := True;
    Writable := True;
    usermode:=false;
  end;
  with pd^[1] do begin
    TableAddr := AllocPage shr 12;
    Present := True;
    Writable := True;
    usermode:=true;
  end;
 
  pt := PPageTable(pd^[0].TableAddr shl 12);
  for i := 0 to 1023 do begin
    FillByte(pt^[i], SizeOf(TPageTableEntry), 0);
    with pt^[i] do begin
      FrameAddr := (i * PageSize) shr 12;
      Present := True;
      Writable := true; 
      usermode:=false;
    end;
  end;

  pt := PPageTable(pd^[1].TableAddr shl 12);
  for i := 0 to 1023 do begin
    FillByte(pt^[i], SizeOf(TPageTableEntry), 0);
    with pt^[i] do begin
      FrameAddr := (i * PageSize) shr 12;
      Present := True;
      Writable := true; 
      usermode:=true;
    end;
  end;


  // Assign the second-last table and zero it
  with pd^[1022] do begin
    TableAddr := AllocPage shr 12;
    Present := True;
    Writable := True;
    usermode:=false;
  end;
  pt := PPageTable(pd^[1022].TableAddr shl 12);
  FillByte(pt^, PageSize, 0);
  // The last entry of the second-last table is the directory itself
  with pt^[1023] do begin
    FrameAddr := PtrUInt(pd) shr 12;
    Present := True;
    Writable := True;
    usermode:=false;
  end;
  // The last table loops back on the directory itself
  with pd^[1023] do begin
    TableAddr := PtrUInt(pd) shr 12;
    Present := True;
    Writable := True;
    usermode:=false;
  end;
 
  // We need to map the page table where the physical memory manager keeps its page stack
  // else it will panic on the first "pmm_free_page"
  ptIndex := PageDirIndex(PMMStackAddr shr 12);
  FillByte(PageDir^[ptIndex], SizeOf(TPageDirEntry), 0);
  with PageDir^[ptIndex] do begin
    TableAddr := AllocPage shr 12;
    Present := True;
    Writable := True;
    usermode:=false;    
  end;
  FillByte(PageTables^[ptIndex * 1024], PageSize, 0);
   // Set the current directory
  SwitchPageDir(pd);

  IsPagingActive := True;
  textcolor(Green);
  WriteStrLn('[ OK ]');
  textcolor(8);
end;

procedure SwitchPageDir(const pd: PPageDir);
begin
  CurrentPageDir := pd;
  asm
    mov ecx,pd
    mov cr3,ecx
  end ['eax'];
end;

procedure Map(const va, pa: LongWord; const IsPresent, IsWritable, IsUserMode: Boolean);
var
  VirtPage, ptIndex: LongWord;
begin
 // This section is for debugging. Gives us mapped PT and Addresses.
 // writeline;
 // WriteStrLn('Map $' + HexStr(va, 8) + ' to $' + HexStr(pa, 8));
//  writestring('Page Table Entry: ');
//  WriteLongLn(VirtPage);
//  writeline;
  VirtPage := va div PageSize;
  ptIndex := PageDirIndex(VirtPage);
  // Find the appropriate page table for 'va'
  if PageDir^[ptIndex].TableAddr shl 12 = 0 then begin
    // The page table holding this page has not been created yet
    with PageDir^[ptIndex] do begin
      TableAddr := AllocPage shr 12;
      Present := True;
      Writable := True;
      Usermode:=isusermode;
    end;
    FillByte(PageTables^[ptIndex * 1024], PageSize, 0);
  end;
  // Now that the page table definately exists, we can update the PTE
  with PageTables^[VirtPage] do begin
    FrameAddr := Align(pa, PageSize) shr 12;
    Present := IsPresent;
    Writable := IsWritable;
    UserMode := IsUserMode;
  end;
end;

procedure UnMap(const va: LongWord);
var
  VirtPage: LongWord;
begin
  VirtPage := va div PageSize;
  FillByte(PageTables[VirtPage], SizeOf(TPageTableEntry), 0);
  // Inform the CPU that we have invalidated a page mapping
  asm
    invlpg va
  end;
end;

function GetMapping(const va: LongWord; pa: PLongWord): Boolean;
var
  VirtPage, ptIndex: LongWord;
begin
  VirtPage := va div PageSize;
  ptIndex := PageDirIndex(VirtPage);
  // Find the appropriate page table for 'va'
  if PageDir^[ptIndex].TableAddr shl 12 = 0 then
    GetMapping := False
  else if PageTables^[VirtPage].FrameAddr <> 0 then begin
    if Assigned(pa) then
      pa := Align(Pointer(PageTables^[VirtPage]), PageSize);
    GetMapping := True;
  end;
end;


function VirtToPhys(va:Longword):Longword;
//could use a isUser mode check..
var
  pt:PPageTable;
  pd:PPageDir;
  ptindex,pdirIndex:longword;

begin   
    pdirIndex := PageDirIndex(va);
    ptindex := PageTableIndex(va);
 
    if longword(@pd^[pdirIndex]) = $FFFFF000 then begin // PageDir MUST exist
       if longword(@pt^[ ptindex]) = ($FFC00000 and ($400 * pdirIndex)) then begin //so must page table 
          VirtToPhys:=(longword( (longword(@pt^[ptindex]) and not $FFF) and (va and $FFF) ));
       end;
       //doesnt exist, so map it in...
       map(va,(longword( ( (longword(@pt^[ptindex]) and not $FFF) and (va and $FFF) ))),true,true,false);
//should be true,true,(usermode)==> true.
    end;
    exit;
end;

end.

