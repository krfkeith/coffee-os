unit Heap;

interface

const
  uHeapStart = $A0000000;
  kHeapStart = $F0000000;
  uHeapEnd = $efffffff;
  kHeapEnd = $FFBFF000;

type
  UBit31 = 0..(1 shl 31) - 1;

  PHeader = ^THeader;

  THeader = bitpacked record
    Prev, Next: PHeader;
    Allocated: Boolean;
    Length: UBit31;
  end;

  
 TFooter = record
//    Magic: LongWord;
    Header: PHeader;
  end;
  PHeap=^THeap;
  THeap=record
    StartAddress: LongWord; // The start of our allocated space.
    EndAddress: LongWord;   // The end of our allocated space. May be expanded up to MaxAddress.
    MaxAddress: LongWord;   // The maximum address the heap can be expanded to.
    IsSupervisor: Boolean;     // Should extra pages requested by us be mapped as supervisor-only?
    IsReadOnly: Boolean;       // Should extra pages requested by us be mapped as read-only?
  end;
var
  kHeapMax: LongWord = kHeapStart;
  uHeapMax: LongWord = uHeapStart;
  HeapFirst: PHeader = nil;
  placementAddress:longword;
  kHeap:PHeap;
  uHeap:PHeap;
  kernelEnd:longword; external name 'end';


procedure InstalluHeap;
procedure InstallkHeap;
function MemAlloc(l: LongWord): Pointer;
function uMemAlloc(l: LongWord): Pointer;
procedure MemFree(p: Pointer);
procedure uMemFree(p: Pointer);

implementation

uses
  console, pmm, vmm;

procedure kAllocChunk(Start, Len: LongWord);
var
  Page: LongWord;
begin
  while Start + Len > kHeapMax do begin
    Page := PtrUInt(AllocPage);
    Map(kHeapMax, page, True, True, False);
    Inc(kHeapMax, PageSize);
  end;
end;

procedure uAllocChunk(Start, Len: LongWord);
var
  Page: LongWord;
begin
  while Start + Len > uHeapMax do begin
    Page := PtrUInt(AllocPage);
    Map(uHeapMax, page, True, True, true);
    Inc(uHeapMax, PageSize);
  end;
end;

procedure kFreeChunk(Chunk: PHeader);
var
  Page: LongWord;
begin
  Chunk^.Prev^.Next := nil;
  if not Assigned(Chunk^.Prev) then
    HeapFirst := nil;
  // While the heap max can contract by a page and still be greater than the Chunk address...
  while kHeapMax - PageSize >= PtrUInt(Chunk) do begin
    Dec(kHeapMax, PageSize);
    GetMapping(kHeapMax, @page);
    FreePage(Page);
    UnMap(kHeapMax);
  end;
end;

procedure uFreeChunk(Chunk: PHeader);
var
  Page: LongWord;
begin
  Chunk^.Prev^.Next := nil;
  if not Assigned(Chunk^.Prev) then
    HeapFirst := nil;
  // While the heap max can contract by a page and still be greater than the Chunk address...

  while uHeapMax - PageSize >= PtrUInt(Chunk) do begin
    Dec(uHeapMax, PageSize);
    GetMapping(uHeapMax, @page);
    FreePage(Page);
    UnMap(uHeapMax);
  end;
end;

procedure SplitChunk(Chunk: PHeader; Len: LongWord);
var
  NewChunk: PHeader;
begin
  // In order to split a Chunk, once we split we need to know that there will be enough
  // space in the new Chunk to store the Chunk header, otherwise it just isn't worthwhile.
  if Chunk^.Length - Len > SizeOf(THeader) then begin
    NewChunk := PHeader(PtrUInt(Chunk) + Chunk^.Length);
    with NewChunk^ do begin
      Prev := Chunk;
      Next := nil;
      Allocated := False;
      Length := Chunk^.Length - Len;
    end;
    Chunk^.Next := NewChunk;
    Chunk^.Length := Len;
  end;
end;

procedure kGlueChunk(Chunk: PHeader);
begin
  with Chunk^ do begin
    if Assigned(Next) and not Next^.Allocated then begin
      Length := Length + Next^.Length;
      Next^.Next^.Prev := Chunk;
      Next := Next^.Next;
    end;
    if Assigned(Prev) and not Prev^.Allocated then begin
      Prev^.Length := Prev^.Length + Length;
      Prev^.Next := Next;
      Next^.Prev := Prev;
      Chunk := Prev;
    end;
    if not Assigned(Next) then
      kFreeChunk(Chunk);
  end;
end;

procedure uGlueChunk(Chunk: PHeader);
begin
  with Chunk^ do begin
    if Assigned(Next) and not Next^.Allocated then begin
      Length := Length + Next^.Length;
      Next^.Next^.Prev := Chunk;
      Next := Next^.Next;
    end;
    if Assigned(Prev) and not Prev^.Allocated then begin
      Prev^.Length := Prev^.Length + Length;
      Prev^.Next := Next;
      Next^.Prev := Prev;
      Chunk := Prev;
    end;
    if not Assigned(Next) then
      uFreeChunk(Chunk);
  end;
end;

procedure InstallkHeap;

var
  Hole: PHeader;
  StartHeap:longword;

begin
  kWriteStr('Installing *kernel* Heap...');
  //Tabulate;
  //Tabulate;
//This is for Paging and Threading init...
  PlacementAddress:=LongWord(@KernelEnd+4096); //do not put the heap right after the kernel. The initrd info is there.

  // Initialise the index.
  kAllocChunk(kHeapStart,$4000); //start with 4K in total size.
  //explains why FOUR pages were used when page size was off..
  
  // Make sure the start address is page-aligned.
  if (kHeapStart and $FFFFF000)<>0 then  StartHeap:=((kHeapStart and $FFFFF000)+$1000) else startheap:=kheapstart;

  // Write the StartHeap, end and max addresses into the heap structure.
  kHeap^.StartAddress:=StartHeap;
  kHeap^.EndAddress:=kHeapEnd;
  kHeap^.MaxAddress:=kHeapEnd;
  kHeap^.IsSupervisor:=true;
  kHeap^.IsReadOnly:=false;

  // We start off with one large hole in the index.
  Hole:=PHeader(kHeapStart);
  with Hole^ do begin
    Length:=kHeapEnd-kHeapStart;
    Allocated:=false;
  end;

  textcolor(2);
  WriteStrLn('[ OK ]');
  textcolor(7);
end;

procedure InstalluHeap;

var

  Hole: PHeader;
  StartHeap:longword;

begin
  kWriteStr('Installing User Mode Heap...');
//  Tabulate;
//  Tabulate;

  // Initialise the index.
  uAllocChunk(uHeapStart,$4000); //start with 4K in total size.
  // Make sure the start address is page-aligned.
  if (uHeapStart and $FFFFF000)<>0 then  StartHeap:=((uHeapStart and $FFFFF000)+$1000) else startheap:=uheapstart;

  // Write the StartHeap, end and max addresses into the heap structure.
  uHeap^.StartAddress:=StartHeap;
  uHeap^.EndAddress:=kHeapEnd;
  uHeap^.MaxAddress:=kHeapEnd;
  uHeap^.IsSupervisor:=true;
  uHeap^.IsReadOnly:=false;

  // We start off with one large hole in the index.
  Hole:=PHeader(uHeapStart);
  with Hole^ do begin
    Length:=uHeapEnd-uHeapStart;
    Allocated:=false;
  end;

  textcolor(2);
  WriteStrLn('[ OK ]');
  textcolor(7);
end;


function MemAlloc(l: LongWord): Pointer; [public, alias: 'MemAlloc'];
var
  CurHeader, PrevHeader: PHeader;
  ChunkStart: LongWord;
begin
  Inc(l, SizeOf(THeader));
  CurHeader := HeapFirst;
  PrevHeader := nil;
  while Assigned(CurHeader) do begin
    if not CurHeader^.Allocated and (CurHeader^.Length >= l) then begin
      SplitChunk(CurHeader, l);
      CurHeader^.Allocated := True;
      MemAlloc := Pointer(PtrUInt(CurHeader) + SizeOf(THeader));
      Exit;
    end;
    PrevHeader := CurHeader;
    CurHeader := CurHeader^.Next;
  end;

  if PrevHeader <> nil then
    ChunkStart := PtrUInt(PrevHeader) + PrevHeader^.Length
  else begin
    ChunkStart := kHeapStart;
    HeapFirst := PHeader(ChunkStart);
  end;

  kAllocChunk(ChunkStart, l);
  CurHeader := PHeader(ChunkStart);
  with CurHeader^ do begin
    Prev := PrevHeader;
    Next := nil;
    Allocated := True;
    Length := l;
  end;

  PrevHeader^.Next := CurHeader;
  MemAlloc := Pointer(ChunkStart + SizeOf(THeader));
end;

//for usermode
function uMemAlloc(l: LongWord): Pointer; [public, alias: 'uMemAlloc'];
var
  CurHeader, PrevHeader: PHeader;
  uChunkStart: LongWord;
begin
  Inc(l, SizeOf(THeader));
  CurHeader := HeapFirst;
  PrevHeader := nil;
  while Assigned(CurHeader) do begin
    if not CurHeader^.Allocated and (CurHeader^.Length >= l) then begin
      SplitChunk(CurHeader, l);
      CurHeader^.Allocated := True;
      uMemAlloc := Pointer(PtrUInt(CurHeader) + SizeOf(THeader));
      Exit;
    end;
    PrevHeader := CurHeader;
    CurHeader := CurHeader^.Next;
  end;

  if PrevHeader <> nil then
    uChunkStart := PtrUInt(PrevHeader) + PrevHeader^.Length
  else begin
    uChunkStart := uHeapStart;
    HeapFirst := PHeader(uChunkStart);
  end;

  uAllocChunk(uChunkStart, l);
  CurHeader := PHeader(uChunkStart);
  with CurHeader^ do begin
    Prev := PrevHeader;
    Next := nil;
    Allocated := True;
    Length := l;
  end;

  PrevHeader^.Next := CurHeader;
  uMemAlloc := Pointer(uChunkStart + SizeOf(THeader));
end;

procedure MemFree(p: Pointer); [public, alias: 'MemFree'];
var
  Header: PHeader;
begin
  Header := PHeader(PtrUInt(p) - SizeOf(THeader));
  Header^.Allocated := False;
  kGlueChunk(Header);
end;

procedure uMemFree(p: Pointer); [public, alias: 'uMemFree'];
var
  Header: PHeader;
begin
  Header := PHeader(PtrUInt(p) - SizeOf(THeader));
  Header^.Allocated := False;
  uGlueChunk(Header);
end;

end.

