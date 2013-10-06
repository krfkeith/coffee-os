unit pmm;

interface

uses
  multiboot;

const
  PMMStackAddr = $FF000000; //above the page tables.
  PageSize = $4000; //4K pages
 
var
  StartESP: LongWord; export name 'StartESP';
  UserESP:Longword; export name 'UserESP';
  VBIOSESP:Longword; export name 'VBIOSESP';
  IsPagingActive: Boolean;
  PMMStackLocation: LongWord = PMMStackAddr;
  PMMStackMax: LongWord = PMMStackAddr;
  PMMLocation: LongWord;

procedure InstallPMM(const Start: LongWord);
procedure FindUsableRAM(MB: PMultiBootInfo );
function AllocPage: LongWord;
procedure FreePage(const p: LongWord);

implementation

uses
  console,uconsole, vmm;

procedure InstallPMM(const Start: LongWord);
begin
  WriteString('Installing PMM...'#9#9);
  PMMLocation := Align(Start + PageSize, PageSize);
  textcolor(Green);
  WriteStrLn('[ OK ]');
  textcolor(8);
end;

procedure FindUsableRAM( MB:PMultiBootInfo );

var
  i, j,m: LongWord;
  ME: PMemoryMap;

begin
  i:=0;
    ME:=PMemoryMap(i); 
    while (i < (ME^.BaseAddress + ME^.Length)) do begin
	if ME^.MType=01 then begin
	writestring('Found useable area of RAM: ');
 	writelong(ME^.Length+ME^.BaseAddress);  writestring(' - '); writelongln(ME^.BaseAddress); 
        end;
  //      else begin
     //     writestrln(' Mem Type: Not Available');
//          writestring('Type: '); writelongln(ME^.MType); 
    //    end;     
     Inc(i);
     ME:=PMemoryMap(i); 
  end;
end;



function AllocPage: LongWord;

begin

  if PMMLocation < PMMStackMax then begin //(current) location within the stack, located HIGH at 4.2GB virtual.
//this is NOT the current location of the stack itself, but the position within it.
    AllocPage := PMMLocation;
	Inc(PMMLocation, PageSize); //4kB Pages
    exit;
  end else begin
      //NEVER go beyond end of RAM.

    //if not (swapEnabled) or (SwapFull=true) then begin
//      TextColor(Red);
    // writeline;
      writechar(#10);
    
      writestring('StackMax: ');
      writelongln(PMMStackMax);

      WriteStrln('Error Allocating beyond End of RAM');
  //    setTextColor(cblack,cLightGray);
 //     Writeline;
      writechar(#10);
      asm
        cli
        hlt
      end;
  //end;
      //swap to disk or exit.
   
  end;
end;

procedure FreePage(const p: LongWord);
var
  Stack: LongWord;
begin
  // Ignore any page under "location", as it may contain important data initialised at boot (like paging structures!)
  if p < PMMStackLocation then
    Exit;
  // If we've run out of space on the stack...
  if PMMStackMax <= PMMStackLocation then begin
  
    // Increase the free page stack's size by one page
    Dec(PMMStackLocation, PageSize);
  end else begin
    // Else we have space on the stack, so push
    Stack := PMMStackLocation;
//stack:=p;
    Inc(PMMStackLocation, PageSize);
  end;
end;


end.

