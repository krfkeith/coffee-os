
Unit Vesa; //(NEEDS HEAP for Record ALLOC, like PCI unit)

interface

{
Most of spec is here. ANything not here is either undocumented and non-existant or really should be in another unit.


The following VESA mode numbers have been defined:
Note: for Linear Frame Buffer(AKA no bank switching needed) add $4000
It is not supported on ALL modes, so be careful.
If bit 7 of VESAModeInfo.ModeAttributes is set, you can do this.

Example to set LFB on current mode:

var
mode:longint;

with VESAModeList do
	if ModeAttributes and 7 then mode:=mode + $4000 //set LFB
	
This makes it so we dont have to worry about switching memory banks when reading/writing to vram.
Most banks are 64k on most cards, but use WinGranularity to be safe.	
These are VESA modes and are non-card specific.


                GRAPHICS                			                TEXT

15-bit   7-bit    Resolution   Colors  	RGBA DAC		 15-bit   7-bit    Columns   Rows
mode     mode                          	Depth			 mode     mode
number   number                        				 number   number
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  				 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
100h     -        640x400      256     				 108h     -        80        60
101h     -        640x480      256
                                       				 109h     -        132       25
102h     6Ah      800x600      16      				 10Ah     -        132       43
103h     -        800x600      256     			 	 10Bh     -        132       50
                                        			 10Ch     -        132       60
104h     -        1024x768     16
105h     -        1024x768     256

106h     -        1280x1024    16
107h     -        1280x1024    256

10Dh     -        320x200      32K   (1:5:5:5) --not supported on DVI
10Eh     -        320x200      64K   (5:6:5) --not supported on DVI
10Fh     -        320x200      16.8M (8:8:8) --not supported on DVI

110h     -        640x480      32K   (1:5:5:5)
111h     -        640x480      64K   (5:6:5)
112h     -        640x480      16.8M (8:8:8)

113h     -        800x600      32K   (1:5:5:5)
114h     -        800x600      64K   (5:6:5)
115h     -        800x600      16.8M (8:8:8)

116h     -        1024x768     32K   (1:5:5:5)
117h     -        1024x768     64K   (5:6:5)
118h     -        1024x768     16.8M (8:8:8)
119h     -        1280x1024    32K   (1:5:5:5)
11Ah     -        1280x1024    64K   (5:6:5)
11Bh     -        1280x1024    16.8M (8:8:8)
11Ch     -        1600x1200    256

--the reason they stopped here in case you want to know is that you can probe what is available from the card, 
and according to the DVI specs(apparently, the available output resolutions supported (from the screen) as well).
Reduces a lot of overhead on the manufacturing part if you can probe this list instead of having it hard coded.


800x600 in order of higher color depth:

Mode:
-----
102(depreciated)
103 (64K)
113 (millions)
114 (true color)
115 (not used)

--with this, we dont need no stinking video drivers..... ---

Really we don't but cards like the ATI series need tweaks. I will address those as they come up.
IFDEFS come in really nice for this.

256 is 1 palette(16*16) ==8 bit
64K is 256 of them (256*256)== 16 bit


24 bit mode+ DOES NOT USE PALLETES.
32bit mode is not commonly used.

1.3M is 64K[a memory chunk] of them (memptr squared)== 32 bit
[not fully supported,use 24 bit instead.]. Just use direct r,g,b or r,g,b,a values here, where a is the luminance value.

There are lots of resources to build a unit like this, and I took a lot of them and merged them together.There is a lot of repeated code.Most of this is for 16bit operation. Supposedly you cant call int 10h with 4F0x functions in PM.

VBE 2.0 was supposedly a fix for having to drop to RM to do vesa functions.
32-bit PM interface didn't come out until VBE 3.0, and only certain cards have VBE 3.0 fully implemented correcty
(nVidia is ONE).Nonetheless, we have it, and I dont for one, feel like writing card specific drivers.

Drivers are a royal waste of time and Royal Pain to code.



var

mbinfo: PMultiBootInfo;

begin
VESAEP_seg:=mbinfo^. vbe_interface_seg; //A000
VESAEP_off:=mbinfo^. vbe_interface_off; //page offset
VESAEP_len:=mbinfo^. vbe_interface_len; //length of page
end;


--Jazz

}

const
 VBE_BIOS_SIZE =8000 // 32 Kb

var
 bios:array[1..8000] of char; // this holds our bios

PMInfoBlock=record
	Signature: array[1..4] of byte;  { 0x44494D50 = 'DIMP' (PMID) }
	EntryPoint:word;
	PMInitialize:word;
	BIOSDataSel:word;
	A0000Sel:word;
	B0000Sel:word; //no A8000 selector??
	B8000Sel:word;
	CodeSegSel:word;
	InProtectMode:byte;
	Cheksum:byte;
end;

lpPMInfoBlock:^PMInfoBlock;

const CurWriteBank:word;
      CurReadBank:word;
      SegC000:word      = $c000;
      Seg0000:word      = $0000;
      Seg0040:word      = $0040; //??
      SegA000:word      = $a000;

//VESA modes, all are longints.

  //    _320x200x8bpp     = $13 --not supported these days, especially via DVI
      _640x400x8bpp     = $100;
      _640x480x8bpp     = $101;
      _800x600x8bpp     = $103;
      _1024x768x8bpp    = $105;
      _1280x1024x8bpp   = $107;

  { Textmode modes for VESA }
  _80x60t        = $108;
  _132x25t       = $109;
  _132x43t       = $10A;
  _132x50t       = $10B;
  _132x60t       = $10C;

      _320x200x15bpp    = $10D; //1:5:5:5 --unsupported
      _320x200x16bpp    = $10E; //5:6:5 --unsupported
      _320x200x24bpp    = $10f; //8:8:8 --unsupported
      _640x480x15bpp    = $110; {1:5:5:5}
      _640x480x16bpp    = $111; {5:6:5}
      _640x480x24bpp    = $112; {8:8:8}
      _800x600x15bpp    = $113; {1:5:5:5}
      _800x600x16bpp    = $114; {5:6:5}
      _800x600x24bpp    = $115; {8:8:8}
      _1024x768x15bpp   = $116; {1:5:5:5}
      _1024x768x16bpp   = $117; {5:6:5}
      _1024x768x24bpp   = $118; {8:8:8}
      _1280x1024x15bpp  = $119; {1:5:5:5}
      _1280x1024x16bpp  = $11A; {5:6:5}
      _1280x1024x24bpp  = $11B; {8:8:8}
      _1600x1200x8bpp   = $11c;
      _1600x1200x15bpp  = $11d; {Unverified}
      _1600x1200x16bpp  = $11e; {5:6:5}

      VESA_ok=$4F;

Var
  xMax,
  yMax: word; { VERY important you set these upon init'ing }
  Current_bank: byte;
  Pp: byte;

Type
  tRGB = record R,G,B: byte; end; //array of char/byte 256*3
  tDAC = array[0..255] of tRGB; //RGB vaules PER color(for reference with TrueColor Modes)

  TWordArray = array [byte] of Word;


  TVESAInfo = record
       VBESignature       : array [0..3] of Char; //'VESA' or 'VBE2', this is sset prior to int10h,determines 
// which specs to pull, VBE1 or VBE2. VBE3 implementation does not affect this structure.
       minVersion         : Byte;
       majVersion         : Byte; //will reflect version available in chipset.May read back as version 3.
       OEMStringPtr       : Pchar; //vendor
       Capabilities       : LongInt;
       VideoModePtr       : ^TWordArray;
       TotalMemory        : word;

       {VESA 2.0}
       OemSoftwareRev     : word;
       OemVendorNamePtr   : Pchar;
       OemProductNamePtr  : Pchar;
       OemProductRevPtr   : Pchar;
       Paddington: array [35..512] of Byte; {Change the upper bound to 512}
                                            {if you are using VBE2.0}
  end;


{
VESAModeInfo:=mbinfo^.vbe_mode_info; //VESA ModeInfoTable
CurrentVESAMode:= mbinfo^.vbe_mode; //current VESA mode set, if any;
 }
 
  TVESAModeInfo = record
       ModeAttributes     : Word;
       WindowAFlags       : Byte;
       WindowBFlags       : Byte;
       Granularity        : Word;
       WindowSize         : Word;
       WindowASeg         : Word;
       WindowBSeg         : Word;
       BankSwitch         : Pointer;
       BytesPerLine       : Word;
       XRes,YRes          : Word;
       CharWidth          : Byte;
       CharHeight         : Byte;
       NumBitplanes       : Byte;
       BitsPerPixel       : Byte;
       NumberOfBanks      : Byte;
       MemoryModel        : Byte;
       BankSize           : Byte;
       NumOfImagePages    : byte;
       Reserved           : byte;
       {Direct Colour fields (required for Direct/6 and YUV/7 memory models}
       RedMaskSize        : byte;
       RedFieldPosition   : Byte;
       GreenMaskSize      : Byte;
       GreenFieldPosition : Byte;
       BlueMaskSize       : Byte;
       BlueFieldPosition  : Byte;
       RsvdMaskSize       : Byte;
       RsvdFieldPosition  : Byte;
       DirectColourMode   : Byte;
       {VESA 2.0 stuff}
       PhysBasePtr        : longint;
       OffScreenMemOffset : pointer;
       OffScreenMemSize   : word;
       paddington: array [49..512] of Byte; {Change the upper bound to 512}
                                            {if you are using VBE2.0}
  end;

    VESARec:^TVESARec;
    ModeRec:^TModeRec;
    SwitchBank:procedure;
    ReadWindow:word;
    LineOffsets:array[0..1199] of longint;
    BankVals:array[0..480] of word;
    Xres,Yres,GetMaxX,GetMaxY:word;


  palrec = packed record              { record used for set/get DAC palette }
       blue, green, red, alpha: byte;
  end;

const
  { VESA attributes     }
  attrSwitchDAC        = $01;    { DAC is switchable           (1.2)   }
  attrNotVGACompatible = $02;    { Video is NOT VGA compatible (2.0)   }
  attrSnowCheck        = $04;    { Video must use snow checking(2.0)   }

  { mode attribute bits }
  modeAvail          = $01;      { Hardware supports this mode (1.0)   }
  modeExtendInfo     = $02;      { Extended information        (1.0)   }
  modeBIOSSupport    = $04;      { TTY BIOS Support            (1.0)   }
  modeColor          = $08;      { This is a color mode        (1.0)   }
  modeGraphics       = $10;      { This is a graphics mode     (1.0)   }
  modeNotVGACompatible = $20;    { this mode is NOT I/O VGA compatible (2.0)}
  modeNoWindowed     = $40;      { This mode does not support Windows (2.0) }
  modeLinearBuffer   = $80;      { This mode supports linear buffers  (2.0) }

  { window attributes }
  winSupported       = $01;
  winReadable        = $02;
  winWritable        = $04;

//shouldnt be used
  { memory model }
  modelText          = $00;
  modelCGA           = $01;
  modelHerc          = $02;
  model4plane        = $03;
  modelPacked        = $04;
  modelModeX         = $05;
  modelRGB           = $06;
  modelYUV           = $07;

  vesa12 = $0102;
  vesa2  = $0200;
  vesa3  = $0300;

TYPE

  pModeList = ^tModeList;
  tModeList = Array [0..255] of word; {list of modes terminated by -1}
                                      {VESA modes are >=100h}

var
  VESAInfo    : TVESAInfo;         { VESA Driver information  }
  VESAModeInfo    : TVESAModeInfo;     { Current Mode information }
  hasVesa: Boolean;       { true if we have a VESA compatible graphics card}

  BytesPerLine: word;              { Number of bytes per scanline }
  YOffset : word;                  { Pixel offset for VESA page flipping }

  { window management }
  ReadWindow : byte;      { Window number for reading. }
  WriteWindow: byte;      { Window number for writing. }
  winReadSeg : word;      { Address of segment for read  }
  winWriteSeg: word;      { Address of segment for writes}
  CurrentReadBank : smallint; { active read bank          }
  CurrentWriteBank: smallint; { active write bank         }

  BankShift : word;       { address to shift by when switching banks. }

  { linear mode specific stuff }
  InLinear  : boolean;    { true if in linear mode }
  LinearPageOfs : longint; { offset used to set active page }
  FrameBufferLinearAddress : longint;

  ScanLines: word;        { maximum number of scan lines for mode }


procedure set_vesa_bank(bank:integer); assembler; 
procedure copyFromFB(memory_buffer:FBuffer; ScreenSize:integer);
function getNumPages:integer;
Procedure GetVESAStateSize:longint;
function GetState:buffer; 
function SetState;
function SetDisplayStart(pixel,line:longint);
function GetDisplayStart:longint;
Procedure SaveStateVESA; 
procedure RestoreStateVESA; 
procedure SetDACPaletteControl(paletteWidth:longint);
function GetDACPaletteControl:longint;
function GetVESAMode:longint;
procedure SetLFB; assembler;
function setMode(mode:word):boolean;
function setVESAMode(mode:word):boolean;
function getMode:word;assembler;
function findMode(x,y:word;model:tMemModel;nBits,nPlanes,nBanks:byte):word;
Procedure LoadPal256(fn: pathstr);
Procedure SetColor(color,r,g,b: Byte); Assembler;
Procedure GetColor(Color: byte; var r,g,b: byte); Assembler;
function hexstr(val : longint;cnt : byte) : string;
function getVESAInfo(var VESAInfo: TVESAInfo) : boolean; assembler;
function getVESAModeInfo(var ModeInfo: TVESAModeInfo;mode:word):boolean;assembler;
function SearchVESAModes(mode: Word): boolean;
procedure SetBankIndex(win: byte; BankNr: smallint); assembler;
procedure SetReadBank(BankNr: smallint);
procedure SetWriteBank(BankNr: smallint);
procedure PutPixVESA256(x, y : smallint; color : word); 
procedure DirectPutPixVESA256(x, y : smallint); 
function GetPixVESA256(x, y : smallint): word; 
Procedure GetScanLineVESA256(x1, x2, y: smallint; var data);
procedure HLineVESA256(x,x2,y: smallint); 
procedure VLineVESA256(x,y,y2: smallint);
procedure PatternLineVESA256(x1,x2,y: smallint); 
procedure DirectPutPixVESA256Linear(x, y : smallint); 
procedure PutPixVESA256Linear(x, y : smallint; color : word);
function GetPixVESA256Linear(x, y : smallint): word;
procedure PutPixVESA32kOr64k(x, y : smallint; color : word); 
function GetPixVESA32kOr64k(x, y : smallint): word; 
procedure DirectPutPixVESA32kOr64k(x, y : smallint); 
procedure PutPixVESA32kor64kLinear(x, y : smallint; color : word); 
function GetPixVESA32kor64kLinear(x, y : smallint): word; 
procedure DirectPutPixVESA32kor64kLinear(x, y : smallint); 
procedure PutPixVESA16(x, y : smallint; color : word); 
Function GetPixVESA16(X,Y: smallint):word; 
procedure DirectPutPixVESA16(x, y : smallint); 
Procedure SetVESARGBPalette(ColorNum, RedValue, GreenValue,   BlueValue : smallint); 
Procedure GetVESARGBPalette(ColorNum: smallint; Var RedValue, GreenValue,  BlueValue : smallint); 
function SetupLinear(var ModeInfo: TVESAModeInfo;mode : word) : boolean;
procedure SetupWindows(var ModeInfo: TVESAModeInfo);
function IsVESAInstalled: word; assembler;
function GetModeInfo (mode: Word): word; assembler;
function GetMaxScanLines: word; assembler;
procedure SetVisualVESA(page: word); 
procedure SetActiveVESA(page: word);
procedure AllocVesaStrucs;
procedure DeAllocVesaStrucs;
procedure CardSwitchBank;  assembler;


var
  vesainfo : tvesainfo;
  modeinfo : tmodeinfo;
  selector : longint;
  shifter  : byte;
  twowins  : boolean; //two windows needed?
  modeptr  : pmode;

implementation

uses
   multiboot,paging; // heap; GET table info data from grub. I dont think we can pull it here in PM.

var
   VESAInfo.VBESignature='VBE2';	//set it, and ALLOW int10h to CHANGE it.
   //must report back as 'VESA' to be valid, regardless of version requested(1 or 2, never 3).
   //we could request VESA instead of VBE2, but that may require RM access to hardware and we dont want that.
   VESAError:boolean; //only sets if info returned not valid from card.(IE: not 004H)



{
granularity(aka window buffer size at A0000) may not be 64k, but USUALLY is.
written this way, it doesn't matter. We execute the same.

The single page at A0000 is where you get mode 13 from, with higher modes, you add video banks to get the needed storage space.
(and switch them accordingly)

This would work better as an offscreen render, than updating screen on each update.
CRT screens do that anyway and LCD ones only update changes, which is why Linux sometimes leave artifacts behind.

Why this is not caught is still beyond me.


I have these values bookmarked, but they aren't in here yet.	
}

//BOCHS/QEMU VBE interface
procedure BgaWriteRegister(IndexValue,DataValue:usint; )
begin
    writeportw(VBE_DISPI_IOPORT_INDEX, IndexValue);
    writeportw(VBE_DISPI_IOPORT_DATA, DataValue);
end
 
 
function BgaReadRegister(IndexValue:usint):usint;
begin
    writeportw(VBE_DISPI_IOPORT_INDEX, IndexValue);
    BgaReadRegister:= readportw(VBE_DISPI_IOPORT_DATA);
end;
 
function BgaIsAvailable:boolean
begin
    BgaIsAvailable:= (BgaReadRegister(VBE_DISPI_INDEX_ID) = VBE_DISPI_ID4);
end;
 
procedure BgaSetVideoMode( Width, Height, BitDepth:usint;);
begin
    BgaWriteRegister(VBE_DISPI_INDEX_ENABLE, VBE_DISPI_DISABLED);
    BgaWriteRegister(VBE_DISPI_INDEX_XRES, Width);
    BgaWriteRegister(VBE_DISPI_INDEX_YRES, Height);
    BgaWriteRegister(VBE_DISPI_INDEX_BPP, BitDepth);
      
    BgaWriteRegister(VBE_DISPI_INDEX_ENABLE, VBE_DISPI_ENABLED or ( VBE_DISPI_LFB_ENABLED) or (VBE_DISPI_CLEARMEM));
end;
 
procedure BgaSetBank(unsigned short BankNumber);
begin
    BgaWriteRegister(VBE_DISPI_INDEX_BANK, BankNumber);
end;



function RMPtrToPMPtr(segment,offset:pointer):pointer;

{Useage:

VBE_EP:=RMPtrToPMPtr(VESA_info.seg,VESA_info.off);
//gives current position in VESA RAM.

It converts the pointers given from VESA / GRUB to PM ones.
}


begin
    RMPtrToPMPtr = (segment*16)+offset;
end;


//Framebuffer for VBE 2.0
{ For this to work, write all video ouput /draws to a memory buffer, the exact size of your screen.
This improves putpixel and such routines drastically, as you copy 64K or more to the screen in one pass, no 50,000 individual updates or bank switches.Program accordingly.

Hence: Linux Framebuffer (and why its so slow.)

}


procedure copyFromFB(memory_buffer:FBuffer; ScreenSize:integer);

var

  bank_size,bank_granularity,bank_number,todo1,copy_size:integer;
  

begin
      bank_size := mode_info.WinSize*1024;
      bank_granularity := mode_info.WinGranularity*1024;
      bank_number := 0;
      todo1 := screen_size;
      copy_size;

      while (todo > 0) begin
	 // select the appropriate bank 
	 set_vesa_bank(bank_number);

	 // how much can we copy in one go? 
	 if (todo > bank_size) then
	    copy_size := bank_size;
	 else
	    copy_size := todo;

	 // copy a bank of data to the screen 
	 move(memory_buffer,$A0000,copy_size);

	 // move on to the next bank of data 
	 dec(todo1,copy_size);
	 inc(memory_buffer,copy_size);
	 inc(bank_number,(bank_size/bank_granularity));
      end;
 end;


function getNumPages:integer;
begin
        getNumPages:=modeInfo.imagePages + 1;
end;


Procedure GetVESAStateSize:longint; 
begin
   asm
     push    es
      push    di
      push    cx
      mov     ax, 4F04h
      mov dl,0
      int     10h

      mov     ax, size
      pop     cx
      pop     di
      pop     es   
      int 10h
   end;
    size:= size * 64;
   
    GetVESAStateSize:=size;
end;


 Procedure SaveStateVESA; 
  begin
    SavePtr := nil;
    SaveSupported := FALSE;
    { Get the video mode }
    asm
      mov  ah,0fh
      int  10h
      mov  [VideoMode], al
    end;
    { Prepare to save video state...}
    asm
      mov  ax, 4f04h       { get buffer size to save state }
      mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
      mov  dx, 00h
      int  10h
      mov  [StateSize], bx
      cmp  al,04fh
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      Begin
        GetMem(SavePtr, 64*StateSize); { values returned in 64-K blocks }
        if not assigned(SavePtr) then
           RunError(203);
        asm
         mov  ax, 4F04h       { save the state buffer                   }
         mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
         mov  dx, 01h
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        asm
         mov  ax, 4F04h       { save the state buffer                   }
         mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
         mov  dx, 02h
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
      end;
  end;

 procedure RestoreStateVESA; 
  begin
     { go back to the old video mode...}
     asm
      mov  ah,00
      mov  al,[VideoMode]
      int  10h
     end;

     { then restore all state information }
     if assigned(SavePtr) and (SaveSupported=TRUE) then
       begin
         { restore state, according to Ralph Brown Interrupt list }
         asm
           mov  ax, 4F04h       { save the state buffer                   }
           mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
           mov  dx, 02h         { restore state                           }
           mov  es, WORD PTR [SavePtr+2]
           mov  bx, WORD PTR [SavePtr]
           int  10h
         end;
         FreeMem(SavePtr, 64*StateSize);
         SavePtr := nil;
       end;
  end;

function vbeSetScanLineLength(length:uint16):boolean;

var

  axVal:longint;

begin
    asm
        mov ax,$4f06
        mov bx,$00
        mov cx, length
        int 10h
    end;
    if axVal=$004f then
	    vbeSetScanLineLength:=true 
	else vbeSetScanLineLength:=false ;
    
end;

//---------------------------------------------------
//
// Get the logical scan line length
//

function vbeGetScanLineLength:boolean;
//blame the logic in this function on lousy C.

var
  bytesPerScanLine,pixelsPerScanLine,maxScanLines:longint;
  axVal,bxVal,cxVal,dxVal:longint;

begin
    asm
        mov ax,$4f06
        mov bx,$01
        int 10h
        mov axVal,ax
    //if called, why not pull these back when done? its a lot easier than trying to pass multiple variables.
        mov bxVal,bx
        mov cxVal,cx
		mov dxVal,dx
    end;
    //these need to be returned
    
    bytesPerScanLine := bxVal;
    pixelsPerScanLine := cxVal;
    maxScanLines := dxVal;

    if axVal=$004f then
	    vbeGetScanLineLength:=true 
	else vbeGetScanLineLength:=false ;
end;



//---------------------------------------------------
//
// Used for panning, scrolling and page flipping
//


function SetDisplayStart(pixel,line:longint);
begin
 
    asm
      push    es
      push    di
      push    cx

      mov     ax, 4F07h
      mov     bl,00
      mov     bh,00
      mov     pixel,cx
      mov     line,dx

      int     10h

      pop     cx
      pop     di
      pop     es
  end;
end;

//---------------------------------------------------
//
// Find out where the current start of the display is.
//

function GetDisplayStart:longint;

var
 pixel,line:longint;

begin
   asm
      push    es
      push    di
      push    cx

      mov     ax, 4F07h
      mov     bl,01
      mov     bh,00
      int     10h
      mov     pixel
      mov     line

      pop     cx
      pop     di
      pop     es
  end;

end;


FUNCTION GetVESAMemorySize:LONGINT;
{return the amount of accessable video memory in bytes,
 note that this function does not have to return the
 actual amount of memory mounted on the video card!}
BEGIN
  GetVESAMemorySize := VideoMemorySize;
END; {GetVESAMemorySize}


FUNCTION GetVESAVirtualHeight:INTEGER;
{returns the possible height of the virtual screen,
 this simply depends on the amount of memory}
BEGIN
  GetVESAVirtualHeight := VideoMemorySize DIV BytesPerLine;
END; {GetVESAVirtualHeight}



FUNCTION GetVESAAbsAddress(X,Y:INTEGER):LONGINT;
{returns the absolute adress of the start of the given line}
BEGIN
  GetVESAAbsAddress := LONGINT(Y)*BytesPerLine+X;
END; {GetVESAAbsAddress}


PROCEDURE SetVESAVirtualTop(Y:INTEGER);
{set the y position in the virtual display
 where the extranal video signal should start
 by settting the extended display start address}
BEGIN
  ASM
    MOV AX, $4F07
    MOV BX, $0000
    MOV CX, $0000
    MOV DX, Y
    INT $10
    MOV VESAStatus, AX
  END;
END; {SetVESAVirtualTop}



PROCEDURE _StoreVideoMode;
{this way you can store the current video mode before VESA}
VAR Size:WORD;
BEGIN
{retrieve needed size}
  ASM
    MOV AX, $4F04                   {function number}
    MOV CX, $000F                   {store everything}
    XOR DX, DX                      {sub function}
    INT $10                         {BIOS-call 10h}
    SHL AX, 6                       {set size to one byte (used to be 64)}
    MOV Size, AX                    {copy}
  END;
{allocate memory}
  SVGAModeBufferSize := Size;
  GetMem(SVGAModeBufferPtr,SVGAModeBufferSize);
{store}
  ASM
    MOV AX, $4F04                   {function number}
    MOV CX, $000F                   {store everything}
    XOR DX, DX                      {sub function}
    INC DX                          {just one higher}
    LES DI, SVGAModeBufferPtr       {address of buffer}
    MOV BX, DI                      {in the right register}
    INT $10                         {BIOS-call 10h}
  END;
{set flag}
  VideoModeStored := TRUE;
END; {_StoreVideoMode}


PROCEDURE _RetrieveVideoMode;//vesa
BEGIN
{protect}
  IF NOT VideoModeStored THEN EXIT;
{restore}
  ASM
    MOV AX, $4F04                   {function number}
    MOV CX, $000F                   {store everything}
    XOR DX, DX                      {sub function}
    INC DX                          {just one higher}
    INC DX                          {and one more!}
    LES DI, SVGAModeBufferPtr       {address of buffer}
    MOV BX, DI                      {in the right register}
    INT $10                         {BIOS-call 10h}
  END;
{deallocate memory}
  FreeMem(SVGAModeBufferPtr,SVGAModeBufferSize);
{reset flag}
  VideoModeStored := FALSE;
END; {_RetrieveVideoMode}


PROCEDURE SelectVESASingleMode;
{switch to single mode}
BEGIN
  ActiveBank := $FF;
  _SetSingleMode;
END; {SelectVESASingleMode}


PROCEDURE SelectVESAReadBank(Bank:BYTE);
{select bank for reading in dual mode}
BEGIN
  IF Bank <> ActiveReadBank THEN
  BEGIN
    _SetVESAReadBank(Bank);
    ActiveReadBank := Bank;
  END;
END; {SelectVESAReadBank}


PROCEDURE NextVESAReadBank;
{goto next bank for reading in dual mode}
BEGIN
  Inc(ActiveReadBank);
  _SetVESABank(ActiveReadBank);
END; {NextVESAReadBank}


FUNCTION SelectVESADualMode:BOOLEAN;
{switch to dual mode}
BEGIN
  IF DualModePossible THEN
  BEGIN
    ActiveWriteBank := $FF;
    ActiveReadBank  := $FF;
    _SetDualMode;
  END;
{return}
  SelectVESADualMode := DualModePossible;
END; {SelectVESADualMode}


PROCEDURE SelectVESAWriteBank(Bank:BYTE);
{select bank for writing in dual mode}
BEGIN
  IF Bank <> ActiveWriteBank THEN
  BEGIN
    _SetVESAWriteBank(Bank);
    ActiveWriteBank := Bank;
  END;
END; {SelectVESAWriteBank}


PROCEDURE NextVESAWriteBank;
{goto next bank for writing in dual mode}
BEGIN
  Inc(ActiveWriteBank);
  _SetVESABank(ActiveWriteBank);
END; {NextVESAWriteBank}


PROCEDURE SelectVESABank(Bank:BYTE);
{select bank for reading and writing in single mode}
BEGIN
  IF Bank <> ActiveBank THEN {we need to select another bank}
  BEGIN
    _SetVESABank(Bank);
    ActiveBank := Bank;
  END;
END; {SelectVESABank}


PROCEDURE NextVESABank;
{goto next bank for reading and writing in single mode}
BEGIN
  Inc(ActiveBank);
  _SetVESABank(ActiveBank);
END; {NextVESABank}


FUNCTION _InitVESA(ModeNr:BYTE):BOOLEAN;
{initialize the card to the given mode, returns succes of operation}
VAR ModeInfo:VESAModeInfoBlock;
    Flag    :BOOLEAN;
    Name    :PCHAR;

FUNCTION __IsSingle:BOOLEAN;
{returns true when only one window is used for reading and writing}
BEGIN
  {start pessimistic}
  __IsSingle := FALSE;
  WITH ModeInfo DO
  BEGIN
    IF M_WinSize = 64 THEN {we should have a window of 64Kb}
    BEGIN
      IF (M_WinAAttr AND $07 = $07) AND (M_WinASegment = VGASeg) THEN
      BEGIN
        BankWindow1 := 0;    {window A is used for reading and writing}
        __IsSingle  := TRUE;
      END
      ELSE
      IF (M_WinBAttr AND $07 = $07) AND (M_WinBSegment = VGASeg) THEN
      BEGIN
        BankWindow1 := 1;    {window B is used for reading and writing}
        __IsSingle  := TRUE;
      END;
    END;
  END;
END; {__IsSingle}

FUNCTION __IsDual:BOOLEAN;
{returns true when one windows is used for reading and
 another for writing, this is called the dual mode}
BEGIN
  {start pessimistic}
  __IsDual := FALSE;
  WITH ModeInfo DO
  BEGIN
    IF (M_WinSize = 64) AND           {we should have windows of 64Kb}
       (M_WinASegment = VGASeg) AND   {both windows A and B at $A0000}
       (M_WinBSegment = VGASeg) THEN
    BEGIN
      IF (M_WinAAttr AND $07 = $05) AND (M_WinBAttr AND $07 = $03) THEN
      BEGIN
        {windows A used for writing and B for reading}
        BankWindow1 := 0;
        BankWindow2 := 1;
        __IsDual    := TRUE;
      END
      ELSE
      IF (M_WinAAttr AND $07 = $03) AND (M_WinBAttr AND $07 = $05) THEN
      BEGIN
        {windows B used for writing and A for reading}
        BankWindow1 := 1;
        BankWindow2 := 0;
        __IsDual    := TRUE;
      END;
    END;
  END;
END; {__IsDual}

FUNCTION __IsSplit:BOOLEAN;
{returns true when VESA uses the split mode; there are
 two windows of 32Kb, both for reading and writing but
 for two different part of the display memory}
BEGIN
  {start pessimistic}
  __IsSplit := FALSE;
  WITH ModeInfo DO
  BEGIN
    IF (M_WinSize = 32) AND            {we should have windows of 32Kb}
       (M_WinAAttr AND $07 = $07) AND  {both reading and writing}
       (M_WinBAttr AND $07 = $07) THEN
    BEGIN
      IF (M_WinASegment = $A000) AND (M_WinBSegment = $A800) THEN
      BEGIN
        {window A above window B}
        BankWindow1 := 0;
        BankWindow2 := 1;
        __IsSplit   := TRUE;
      END
      ELSE IF (M_WinASegment = $A800) AND (M_WinBSegment = $A000) THEN
      BEGIN
        {window B above window A}
        BankWindow1 := 1;
        BankWindow2 := 0;
        __IsSplit   := TRUE;
      END;
    END;
  END;
END; {__IsSplit}
BEGIN {_InitVESA}
{start pessimistic}
  _InitVESA := FALSE;
{get modus information}
  _VESA_ModeInfo(Word(ModeNr) OR $100,@ModeInfo);
  WITH ModeInfo DO
  BEGIN
    IF _VESA_Valid AND (M_WinFuncPtr <> NIL) THEN
    BEGIN
      {yes, it's there, copy the bank call routine pointer}
      BankCall := M_WinFuncPtr;
      IF __IsDual THEN {dual mode implementation}
      BEGIN
        _SetVESABank      := _SetDualBank_VESA;
        _SetVESAWriteBank := _SetWriteBank_VESA;
        _SetVESAReadBank  := _SetReadBank_VESA;
        DualModePossible   := TRUE;
      END
      ELSE
      IF __IsSingle THEN {single mode implementation}
      BEGIN
        _SetVESABank      := _SetBank_VESA;
        _SetVESAWriteBank := _SetBank_VESA;
        _SetVESAReadBank  := _SetBank_VESA;
        DualModePossible   := FALSE;
      END
      ELSE
      IF __IsSplit THEN {split mode implementation}
      BEGIN
        _SetVESABank      := _SetSplitBank_VESA;
        _SetVESAWriteBank := _SetSplitBank_VESA;
        _SetVESAReadBank  := _SetSplitBank_VESA;
        DualModePossible   := FALSE;
      END
      ELSE EXIT; {not a known implementation}
      {determine the shift to ajust window granularity}
      ShiftCount := 0;
      IF M_WinGranularity <> 0 THEN
      BEGIN
        WHILE (M_WinGranularity SHL ShiftCount < 64) DO Inc(ShiftCount);
      END;
      {determine amount of memory}
      VideoMemorySize := _VESAMemorySize;
      {actually start the VESA modus}
      _VESA_SetMode(Word(ModeNr) OR $100);
      {return succes}
      _InitVESA := _VESA_Valid;
    END;
  END;
END;

PROCEDURE _SetDualBank_VESA(Bank:BYTE); FAR;
{emulate a single mode while VESA supports dual mode only}
BEGIN
  _BankCall_VESA(BankWindow1,Word(Bank) SHL ShiftCount);
  _BankCall_VESA(BankWindow2,Word(Bank) SHL ShiftCount);
END; {_SetDualBank_VESA}


PROCEDURE _SetSplitBank_VESA(Bank:BYTE); FAR;
{emulate a single mode while VESA supports split mode only}
BEGIN
  _BankCall_VESA(BankWindow1,Word(Bank) SHL ShiftCount);
  _BankCall_VESA(BankWindow2,Word(Bank) SHL ShiftCount + 1 SHL (ShiftCount-1));
END; {_SetSplitBank_VESA}


FUNCTION _VESAProducer:PCHAR;
{returns the name of the VESA product}
VAR Info:VESAInfoBlock;
BEGIN
  _VESA_Info(@Info);
  _VESAProducer := Info.I_OEMStringPtr;                {who did it?}
END; {_VESAProducer}


PROCEDURE _BankCall_VESA(Window,Bank:WORD); ASSEMBLER;
{goto the given window and bank}
ASM
  MOV DX, Bank             {get bank}
  MOV BX, Window           {get window}
  MOV BH, 0                {and set BH to zero}
  CALL BankCall            {call VESA BIOS}
END; {_BankCall_VESA}


PROCEDURE _SetBank_VESA(Bank:BYTE); FAR;
{set the 64Kb read and write bank}
BEGIN
  _BankCall_VESA(BankWindow1,(WORD(Bank) SHL ShiftCount));
END; {_SetBank_VESA}


PROCEDURE _SetWriteBank_VESA(Bank:BYTE); FAR;
{set the 64Kb write bank}
BEGIN
  _BankCall_VESA(BankWindow1,(WORD(Bank) SHL ShiftCount));
END; {_SetWriteBank_VESA}


PROCEDURE _SetReadBank_VESA(Bank:BYTE); FAR;
{set the 64Kb read bank}
BEGIN
  _BankCall_VESA(BankWindow2,WORD(Bank) SHL ShiftCount);
END; {_SetReadBank_VESA}



FUNCTION checkforVESAsig:BOOLEAN;
{check whether there is a VESA BIOS interface section}
VAR Info:VESAInfoBlock;
BEGIN
  GetVESAInfo(@Info);
  checkforVESAsig := _VESA_Valid AND (Info.I_VESASignature = 'VESA');
END;


FUNCTION _VESAMemorySize:LONGINT;
{returns the amount of memory available to the VESA}
VAR Info     :VESAInfoBlock;
    BlockSize:LONGINT;
BEGIN
  _VESA_Info(@Info);
  BlockSize        :=  $10000;                          {64 Kb block}
  _VESAMemorySize :=  BlockSize*Info.I_TotalMemory;    {number of blocks}
END; {_VESAMemorySize}

PROCEDURE _VESA_ModeInfo(Mode:WORD;Info:POINTER);
{get the VESA information block of a paticular video modus}
BEGIN
{clear it to be on the safe side}
  FillChar(Info^,SizeOf(VESAInfoBlock),0);
{request info}
  ASM
    MOV AX, $4F01
    MOV CX, Mode
    LES DI, Info
    INT $10
    MOV VESAStatus, AX
  END;
END;

FUNCTION _VESA_Valid:BOOLEAN;
{was the last VESA operation succesfull?}
BEGIN
  _VESA_Valid := (VESAStatus = $004F); //the last mov command sets this variable for us, not requiring the check of the AX registers.
END; 


//HOW TO SET VESA SVGA MODES PROPERLY:

PROCEDURE GetVESAInfo(Info:POINTER);
{get the general VESA information block}
{might change stack when no VESA is present!!!!}
BEGIN
{clear it to be on the safe side}
  FillChar(Info^,SizeOf(VESAInfoBlock),0);
{request info}
  ASM
    MOV AX, $4F00
    LES DI, Info
    INT $10
    MOV VESAStatus, AX
  END;

END;

type

procedure bankSwitch; assembler;
asm	
	mov ax,4F05h
	xor bx,bx
	int 10h

end;


const
  vesa12 = $0102;
  vesa2  = $0200;
  vesa3  = $0300;


Var
 ActualBank :Word;
 BankSwitch :Pointer;


//---------------------------------------------------
//
// Set the desired number of bits of color in the
// palette. Normal VGA is 6. Many SVGAs support 8.
//

procedure SetDACPaletteControl(paletteWidth:longint);
begin

  asm
      push    es
      push    di
      push    cx

      mov     ax, 4F08h
      mov     bl,00
      mov     paletteWidth,bx
    
      int     10h
      pop     cx
      pop     di
      pop     es

    end;
    GetDACPaletteControl:= paletteWidth;

end;

//---------------------------------------------------
//
// Get the current width of colors
//

function GetDACPaletteControl:longint;

var
 paletteWidth:longint;

begin

   asm
      push    es
      push    di
      push    cx

      mov     ax, 4F08h
      mov     bl,01
      int     10h
      mov     bx,paletteWidth
      pop     cx
      pop     di
      pop     es

    end;
    GetDACPaletteControl:= paletteWidth;

end;


function GetVESAMode:word;

var
 axVAL,bxVAL:word;

begin
    asm
      push    es
      push    di
      push    cx

      mov     ax, 4F03h
      int     10h

      mov     axVAL,ax
      mov     bxVAL,bx
      pop     cx
      pop     di
      pop     es

    end;
    if axVAL=$004F then
	    GetVESAMode:=bxVAL
	else VESAError:=true; //cant get bad mode info.
end;



procedure SetLFB; assembler;
//there is a little bit more involved, but dont we need dpmi/fileopts working to enable that?

asm
   or    bx,4000h //mode = mode +4000 to be linear.
   mov   ax,4F02h
   int 10h
   //get result and compare to $004F
end;


  function setVESAMode(mode:word):boolean;
    var i:word;
        res: boolean;
  begin
   { Init mode information, for compatibility with VBE < 1.1 }
   FillChar(VESAModeInfo, sizeof(TVESAModeInfo), #0);
   { get the video mode information }
   getVESAModeInfo(VESAmodeinfo, mode);      { checks if the hardware supports the video mode. }
     if (VESAModeInfo.attr and modeAvail) = 0 then
       begin
         SetVESAmode := FALSE;

         _GraphResult := Error;
         exit;
       end;

     SetVESAMode := TRUE;
     BankShift := 0;
     while (64 shr BankShift) <> VESAModeInfo.WinGranularity do
        Inc(BankShift);
     CurrentWriteBank := -1;
     CurrentReadBank := -1;
     BytesPerLine := VESAModeInfo.BytesPerScanLine;

     { These are the window adresses ... }
     WinWriteSeg := 0;  { This is the segment to use for writes }
     WinReadSeg := 0;   { This is the segment to use for reads  }
     ReadWindow := 0;
     WriteWindow := 0;

     { VBE 2.0 and higher supports >= non VGA linear buffer types...}
     { this is backward compatible.                                 }
     if (((VESAModeInfo.Attr and ModeNoWindowed) <> 0) or UseLFB) and
          ((VESAModeInfo.Attr and ModeLinearBuffer) <> 0) then
        begin
          if not SetupLinear(VESAModeInfo,mode) then
            SetUpWindows(VESAModeInfo);
        end
     else
     { if linear and windowed is supported, then use windowed }
     { method.                                                }
        SetUpWindows(VESAModeInfo);

{$ifdef debug}
  bochs_debugline('Entering vesa mode '+strf(mode));
  bochs_debugline('Read segment: $'+hexstr(winreadseg,4));
  bochs_debugline('Write segment: $'+hexstr(winwriteseg,4));
  bochs_debugline('Window granularity: '+strf(VESAModeInfo.WinGranularity)+'kb');
  bochs_debugline('Window size: '+strf(VESAModeInfo.winSize)+'kb');
  bochs_debugline('Bytes per line: '+strf(bytesperline));
{$endif }
   { Select the correct mode number if we're going to use linear access! }
   if InLinear then
     inc(mode,$4000);

   asm
    mov ax,4F02h
    mov bx,mode
    push ebp
    push esi
    push edi
    push ebx
    int 10h
    pop ebx
    pop edi
    pop esi
    pop ebp
    sub ax,004Fh
    cmp ax,1
    sbb al,al
    mov res,al
   end ['EBX','EAX'];
   
  end;
 end;


{will find a color graphics mode that matches parms
if parm is 0, finds best mode for that parm
Moderately useful.

}
function findMode(x,y:word;nBits,nPlanes,nBanks:byte):word;

var 

	p:^word; 
	m:word; 
	gx,gy,gb,lp,lb:word;

begin
 gx:=$00;
 gy:=$00;
 gb:=$08;
 lp:=255;
 lb:=255;
 p:=pointer(VESAInfo.modeList);
 m:=$FFFF;
 while p^<>$FFFF do begin
  if getModeInfo(p^) then
   with modeInfo do begin
    
	 if ((xRes=x)or((x=$00)and(gx<=xRes)))
      and((yRes=y)or((y=$00)and(gy<=yRes)))
      and((bitsPixel=nBits)or((nBits>=$08)and(gb<=bitsPixel)))
      and((planes=nPlanes)or((nPlanes=$00)and(lp>=planes)))
      and((banks=nBanks)or((nBanks=$00)and(lb>=banks)))
      then begin
       gx:=xRes;
       gy:=yRes;
       gb:=bitsPixel;
       lp:=planes;
       lb:=banks;
       m:=p^;
   end;    
  inc(p); //next mode in list
  end; //mode list is empty
 if m<>$FFFF then getModeInfo(m); //figure out which mode number we are in and return with it.
 findMode:=m;  {0FFFFh if not found. Try a standard mode number then.}
 end;


Procedure SetColor(color,r,g,b: Byte); Assembler;
Asm
  mov  dx, 3C8h   { Color port }
  mov  al, color  { Number of color to change }
  out  dx, al
  inc  dx         { Inc dx to write }
  mov  al, r      { Red value }
  out  dx, al
  mov  al, g      { Green }
  out  dx, al
  mov  al, b      { Blue }
  out  dx, al
End;

Procedure GetColor(Color: byte; var r,g,b: byte); Assembler;
{ This reads the values of the Red, Green and Blue DAC values of a
  certain color and returns them to you in r (red), g (green), b (blue) }
asm
  mov  dx, 3C7h
  mov  al, color
  out  dx, al
  add  dx, 2
  in   al, dx
  les  di, r
  stosb
  in   al, dx
  les  di, g
  stosb
  in   al, dx
  les  di, b
  stosb
end;


{not until fileopts and FS is up....
Procedure LoadPal256(Fn: PathStr);
Var
  DAC: tDAC;
  F: file;
  Loop: integer;
Begin
  Assign(f,Fn);
  Reset(f,1);
  If ioresult <> 0 then exit;
  BlockRead(f,DAC,Sizeof(DAC)); //256 bit array of r,g,b of byte
  //reads the whole file at once
  Close(f);
  for Loop := 0 to 255 do with dac[loop] do SetColor(Loop,r,g,b);
end;

file contains:
FF00CC (color value for index 1)
.
.
.
(until 256 values reached)

}

function getVESAInfo(var VESAInfo: TVESAInfo) : VESAInfo;
var
	vesa_pm_info:longint;  


begin
  VESAError:=false;
  vesa_pm_info := $C0000;
  asm
       mov ax,4F00h
       les di,VESAInfo
       int 10h
       mov ax,axVAL
  end;
  if axVAL=$004F then begin //no vesa errors
       with VESAInfo do begin

	 repeat
		if (signature = $44494D50) then
		begin
			kwritestrln('A protected mode entry point has been found at ');
			kwritelongln(vesa_pm_info);
			break;
		end;
		inc(longint(vesa_pm_info),4);
	 until (longint(vesa_pm_info) >= $EFFFF);
         if (longint(vesa_pm_info) >= $EFFFF) then kwritestrln('No protected mode entry point found.');

           if VBESignature<>'VESA' then begin//if 'vesa' not returned, then not ok
               writeln('VESA signature not valid.');
	       writeln('VESA is not supported.');
	       asm 
		      mov al,0
     	       end;
     	       VESAError:=true;
               exit;    
           end;
        end;
        exit;      
  end;
     writeln('VESA compatible card not found.Exiting.');
  end; 

  function getVESAModeInfo(var ModeInfo: TVESAModeInfo;mode:word):ModeInfo;
  //should return info about (mode)
  begin
   asm
     mov ax,4F01h
     mov cx,mode
     les di,ModeInfo  //given empty table, get mode info for (mode)
     int 10h
     mov  ModeInfo,di //need the table back, full of info
   end;
     if axVal<>$004F  then VESAError:=true else VESAError:=false;
          
   end;


  function IsVESAModeAvail(mode: Word): boolean;
  {********************************************************}
  { Searches for a specific DEFINED vesa mode. If the mode }
  { is not available for some reason, then returns FALSE   }
  { otherwise returns TRUE.                                }
  {********************************************************}
   var
     i: word;
     ModeSupported : Boolean;
    begin
      i:=0;
      { let's assume it's not available ... }
      ModeSupported := FALSE;
      { This is a STUB VESA implementation  }
      if VESAInfo.ModeList^[0] = $FFFF then exit;
      repeat
        if VESAInfo.ModeList^[i] = mode then
         begin
            { we found it, the card supports this mode... }
            ModeSupported := TRUE;
            break;
         end;
        Inc(i);
      until VESAInfo.ModeList^[i] = $ffff;
      { now check if the hardware supports it... }
      If ModeSupported then
        begin
          { we have to init everything to zero, since VBE < 1.1  }
          { may not setup fields correctly.                      }
          FillChar(VESAModeInfo, sizeof(VESAModeInfo), #0);
          If GetVESAModeInfo(VESAModeInfo, Mode) And
             ((VESAModeInfo.attr and modeAvail) <> 0) then
            ModeSupported := TRUE
          else
            ModeSupported := FALSE;
        end;
       SearchVESAModes := ModeSupported;
    end;

  {********************************************************}
  { There are two routines for setting banks. This may in  }
  { in some cases optimize a bit some operations, if the   }
  { hardware supports it, because one window is used for   }
  { reading and one window is used for writing.            }
  {********************************************************}
//managed to avoid assembler here.

  procedure SetReadBank(BankNr: smallint);
   begin
     { check if this is the current bank... if so do nothing. }
     if BankNr = CurrentReadBank then exit;
{$ifdef logging}
{     LogLn('Setting read bank to '+strf(BankNr));}
{$endif logging}
     CurrentReadBank := BankNr;          { save current bank number     }
     BankNr := BankNr shl BankShift;     { adjust to window granularity }
     { we set both banks, since one may read only }
     SetBankIndex(ReadWindow, BankNr);
     { if the hardware supports only one window }
     { then there is only one single bank, so   }
     { update both bank numbers.                }
     if ReadWindow = WriteWindow then
       CurrentWriteBank := CurrentReadBank;
   end;

  procedure SetWriteBank(BankNr: smallint);
   begin
     { check if this is the current bank... if so do nothing. }
     if BankNr = CurrentWriteBank then exit;
{$ifdef logging}
{     LogLn('Setting write bank to '+strf(BankNr));}
{$endif logging}
     CurrentWriteBank := BankNr;          { save current bank number     }
     BankNr := BankNr shl BankShift;     { adjust to window granularity }
     { we set both banks, since one may read only }
     SetBankIndex(WriteWindow, BankNr);
     { if the hardware supports only one window }
     { then there is only one single bank, so   }
     { update both bank numbers.                }
     if ReadWindow = WriteWindow then
       CurrentReadBank := CurrentWriteBank;
   end;


 {************************************************************************}
 {*                     VESA Palette entries                             *}
 {************************************************************************}

   Procedure SetVESARGBPalette(ColorNum, RedValue, GreenValue,   BlueValue : smallint); 
    var
     FunctionNr : byte;   { use blankbit or normal RAMDAC programming? }
     pal: ^palrec;
     Error : boolean;     { VBE call error                             }
    begin
      if DirectColor then
        Begin
          VESAError := true;
          exit;
        end;
        Error := FALSE;
        new(pal);
        if not assigned(pal) then RunError(203);
        pal^.align := 0;
        pal^.red := byte(RedValue);
        pal^.green := byte(GreenValue);
        pal^.blue := byte(BlueValue);
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            { check if blanking bit must be set when programming }
            { the RAMDAC.                                        }
            if (VESAInfo.caps and attrSnowCheck) <> 0 then
              FunctionNr := $80
            else
              FunctionNr := $00;
            asm
              mov  ax, 4F09h         { Set/Get Palette data    }
              mov  bl, [FunctionNr]  { Set palette data        }
              mov  cx, 01h           { update one palette reg. }
              mov  dx, [ColorNum]    { register number to update }
              les  di, [pal]         { get palette address     }
              int  10h
              cmp  ax, 004Fh         { check if success        }
              jz   @noerror
              mov  [Error], TRUE
             @noerror:
            end;
            if not Error then
                Dispose(pal)
            else
              begin
                VESAError := true;
                exit;
              end;
          end
        else
          { assume it's fully VGA compatible palette-wise. }
          Begin
            SetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);
          end;
    end;




  Procedure GetVESARGBPalette(ColorNum: smallint; Var RedValue, GreenValue,  BlueValue : smallint); 
   var
    Error: boolean;
    pal: ^palrec;
   begin
      if DirectColor then
        Begin
          VESAError := true;
          exit;
        end;
      Error := FALSE;
      new(pal);
      if not assigned(pal) then RunError(203);
      FillChar(pal^, sizeof(palrec), #0);
      { use the set/get palette function }
      if VESAInfo.Version >= $0200 then
        Begin
          asm
            mov  ax, 4F09h         { Set/Get Palette data    }
            mov  bl, 01h           { Set palette data        }
            mov  cx, 01h           { update one palette reg. }
            mov  dx, [ColorNum]    { register number to update }
            les  di, [pal]         { get palette address     }
            int  10h
            cmp  ax, 004Fh         { check if success        }
            jz   @noerror
            mov  [Error], TRUE
          @noerror:
          end;
          if not Error then
            begin
              RedValue := smallint(pal^.Red);
              GreenValue := smallint(pal^.Green);
              BlueValue := smallint(pal^.Blue);
              Dispose(pal);
            end
          else
            begin
              VESAError := true;
              exit;
            end;
        end
        else
            GetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);

   end;

{
//do we even need this?
  function SetupLinear(var ModeInfo: TVESAModeInfo;mode : word) : boolean;
   begin
     SetUpLinear:=false;
     case mode of
       m320x200x32k,
       m320x200x64k,
       m640x480x32k,
       m640x480x64k,
       m800x600x32k,
       m800x600x64k,
       m1024x768x32k,
       m1024x768x64k,
       m1280x1024x32k,
       m1280x1024x64k :
         begin
           DirectPutPixel:=@DirectPutPixVESA32kor64kLinear;
           PutPixel:=@PutPixVESA32kor64kLinear;
           GetPixel:=@GetPixVESA32kor64kLinear;
           // linear mode for lines not yet implemented PM 
           HLine:=@HLineDefault;
           VLine:=@VLineDefault;
           GetScanLine := @GetScanLineDefault;
           PatternLine := @PatternLineDefault;
         end;
       m640x400x256,
       m640x480x256,
       m800x600x256,
       m1024x768x256,
       m1280x1024x256:
         begin
           DirectPutPixel:=@DirectPutPixVESA256Linear;
           PutPixel:=@PutPixVESA256Linear;
           GetPixel:=@GetPixVESA256Linear;
           // linear mode for lines not yet implemented PM 
           HLine:=@HLineDefault;
           VLine:=@VLineDefault;
           GetScanLine := @GetScanLineDefault;
           PatternLine := @PatternLineDefault;
         end;
     else
       exit;
     end;
 
 
     FrameBufferLinearAddress:=Get_linear_addr(VESAModeInfo.PhysAddress and $FFFF0000,
       VESAInfo.TotalMem shl 16);
//$ifdef debug
     bochs_debugline('framebuffer linear address: '+hexstr(FrameBufferLinearAddress div (1024*1024)));
     
//$endif
     if int31error<>0 then
       begin
//$ifdef debug
         bochs_debugline('Unable to get linear address for '+hexstr(VESAModeInfo.PhysAddress));
//$endif 
        
         exit;
       end;
     if UseNoSelector then
       begin

         LFBPointer:=pointer(FrameBufferLinearAddress-get_segment_base_address(get_ds));
         if dword(LFBPointer)+dword(VESAInfo.TotalMem shl 16)-1 > dword(get_segment_limit(get_ds)) then
           set_segment_limit(get_ds,dword(LFBPointer)+dword(VESAInfo.TotalMem shl 16)-1);
       end
     else
       begin
         WinWriteSeg:=allocate_ldt_descriptors(1);

         set_segment_base_address(WinWriteSeg,FrameBufferLinearAddress);
         set_segment_limit(WinWriteSeg,(VESAInfo.TotalMem shl 16)-1);
         lock_linear_region(FrameBufferLinearAddress,(VESAInfo.TotalMem shl 16));
         if int31error<>0 then
           begin

             writeln(stderr,'Error in linear memory selectors creation');
             exit;
           end;

       end;
     LinearPageOfs := 0;
     InLinear:=true;
     SetUpLinear:=true;
      WinSize:=(VGAInfo.TotalMem shl 16);
     WinLoMask:=(VGAInfo.TotalMem shl 16)-1;
     WinShift:=15;
     Temp:=VGAInfo.TotalMem;
     while Temp>0 do
       begin
         inc(WinShift);
         Temp:=Temp shr 1;
       end; 
   end;
}

  procedure SetupWindows(var ModeInfo: TVESAModeInfo);
   begin
     InLinear:=false;
     { now we check the windowing scheme ...}
     if (ModeInfo.WinAAttr and WinSupported) <> 0 then
       { is this window supported ... }
       begin
         { now check if the window is R/W }
         if (ModeInfo.WinAAttr and WinReadable) <> 0 then
         begin
           ReadWindow := 0;
           WinReadSeg := ModeInfo.WinASeg;
         end;
         if (ModeInfo.WinAAttr and WinWritable) <> 0 then
         begin
           WriteWindow := 0;
           WinWriteSeg := ModeInfo.WinASeg;
         end;
       end;
     if (ModeInfo.WinBAttr and WinSupported) <> 0 then
       { is this window supported ... }
       begin

         { OPTIMIZATION ... }
         { if window A supports both read/write, then we try to optimize }
         { everything, by using a different window for Read and/or write.}
         if (WinReadSeg <> 0) and (WinWriteSeg <> 0) then
           begin
              { check if winB supports read }
              if (ModeInfo.WinBAttr and winReadable) <> 0 then
                begin
                  WinReadSeg := ModeInfo.WinBSeg;
                  ReadWindow := 1;
                end
              else
              { check if WinB supports write }
              if (ModeInfo.WinBAttr and WinWritable) <> 0 then
                begin
                  WinWriteSeg := ModeInfo.WinBSeg;
                  WriteWindow := 1;
                end;
           end
         else
         { Window A only supported Read OR Write, no we have to make }
         { sure that window B supports the other mode.               }
         if (WinReadSeg = 0) and (WinWriteSeg<>0) then
           begin
              if (ModeInfo.WinBAttr and WinReadable <> 0) then
                begin
                  ReadWindow := 1;
                  WinReadSeg := ModeInfo.WinBSeg;
                end
              else
                { impossible, this VESA mode is WRITE only! }
                begin
                  WriteLn('Invalid VESA Window attribute.');
                  exit;
                end;
           end
         else
         if (winWriteSeg = 0) and (WinReadSeg<>0) then
           begin
             if (ModeInfo.WinBAttr and WinWritable) <> 0 then
               begin
                 WriteWindow := 1;
                 WinWriteSeg := ModeInfo.WinBSeg;
               end
             else
               { impossible, this VESA mode is READ only! }
               begin
                  WriteLn('Invalid VESA Window attribute.');
                  exit;
               end;
           end
         else
         if (winReadSeg = 0) and (winWriteSeg = 0) then
         { no read/write in this mode! }
           begin
                  WriteLn('Invalid VESA Window attribute.');
                  exit;
           end;
         YOffset := 0;
       end;

     { if both windows are not supported, then we can assume }
     { that there is ONE single NON relocatable window.      }
     if (WinWriteSeg = 0) and (WinReadSeg = 0) then
       begin
         WinWriteSeg := ModeInfo.WinASeg;
         WinReadSeg := ModeInfo.WinASeg;
       end;

   end;


  function GetMaxScanLines: word;
  
  var
     axVal:word;
  
  begin
     asm
	      mov ax, 4f06h
	      mov bx, 0001h
	      int 10h
	      mov axVal,ax
     end;
      
      GetMaxScanlines:=axVal;
   end;



 {************************************************************************}
 {*                     VESA Page flipping routines                      *}
 {************************************************************************}
 { Note: These routines, according  to the VBE3 specification, will NOT   }
 { work with the 24 bpp modes, because of the alignment.                  }
 {************************************************************************}

  {******************************************************** }
  { Procedure SetVisualVESA()                               }
  {-------------------------------------------------------- }
  { This routine changes the page which will be displayed   }
  { on the screen, since the method has changed somewhat    }
  { between VBE versions , we will use the old method where }
  { the new pixel offset is used to display different pages }
  {******************************************************** }

 procedure SetVisualVESA(page: word); 
//setPage
  var
   newStartVisible : word;
  begin
    if page > HardwarePages then
      begin
        _graphresult := grError;
        exit;
      end;
    newStartVisible := (MaxY+1)*page;
    if newStartVisible > ScanLines then
      begin
        _graphresult := grError;
        exit;
      end;
    asm
      mov ax, 4f07h
      mov bx, 0000h   { set display start }
      mov cx, 0000h   { pixel zero !      }
      mov dx, [NewStartVisible]  { new scanline }
      push    ebp
      push    esi
      push    edi
      push    ebx
      int     10h
      pop     ebx
      pop     edi
      pop     esi
      pop     ebp
    end ['EDX','ECX','EBX','EAX'];
  end;


procedure AllocVesaStrucs;
begin

 new(VesaRec);
 new(ModeRec)

end;

procedure DeAllocVesaStrucs;
begin

 dispose(VesaRec);
 dispose(ModeRec)

end;


procedure fillmodetable_vesa(var table);
type
  tvideomode = array[0..511] of word;
const
  supportedbpp : set of byte=[8,15,16,24,32];
var
  i,rtab    : word;
  videomode : ^tvideomode;
begin
  i          := 0;
  getmem(videomode,1024);
  move(ptr(getrmselector(longint(vesainfo.videomodeptr) shr 16),longint(vesainfo.videomodeptr) and $FFFF)^,
       videomode^,1024);
  for rtab := 0 to 4 do tmodetable(table)[rtab].avmodes := 0;
  while (videomode^[i] <> $FFFF)and(i < 512) do begin
    if getmodeinfo(modeinfo,videomode^[i]) then
    if (modeinfo.memorymodel <> 0)and(modeinfo.bitsperpixel in supportedbpp)
    then begin
      case modeinfo.bitsperpixel of
        8  : rtab := 0;
        15,16 : rtab := modeinfo.greenmasksize-4;
        24,32 : if modeinfo.rsvdmasksize = 8 then rtab := 4 else rtab := 3;
        else rtab := 5;
      end;
      if (rtab < 5)and(tmodetable(table)[rtab].avmodes < 25)and(
         longint(modeinfo.bytesperscanline)*modeinfo.yresolution <=
         longint(vesainfo.totalmemory) shl 16)and
         (getbit(modeinfo.modeattributes,4))and
         (getbit(modeinfo.modeattributes,0)) then begin
        tmodetable(table)[rtab].mode[tmodetable(table)[rtab].avmodes].resx := modeinfo.xresolution;
        tmodetable(table)[rtab].mode[tmodetable(table)[rtab].avmodes].resy := modeinfo.yresolution;
        tmodetable(table)[rtab].mode[tmodetable(table)[rtab].avmodes].mode := videomode^[i];
        inc(tmodetable(table)[rtab].avmodes);
      end;
    end;
    inc(i);
  end;
  freemem(videomode,1024);
end;

function setvesamode(mode : word;lfb : boolean) : boolean;assembler;
asm
           mov   bx,mode
           cmp   lfb,true
           jne   @nolfb   { Shall we use LFB?                         }
           or    bx,4000h { Yep - set LFB bit                         }
  @nolfb:  mov   ax,4F02h
           int   10h
           xor   ax,004Fh { ah <> 0 if mode fail, al <> 4F if no vesa }
           jnz   @fail
           mov   ax,1     { true                                      }
           jmp   @end
  @fail:   xor   ax,ax    { false                                     }
  @end:
end;

procedure setbank_vesa(bank : word);far;assembler;
{set the graphical bank of vesa}
asm
  push  ax
  push  bx
  push  cx
  push  dx
  mov   dx,bank
  cmp   dx,lastbank
  je    @end
  mov   lastbank,dx
  mov   cl,shifter

  shl   dx,cl
  xor   cl,cl
  xor   bx,bx
  mov   ax,4F05h
  int   10h
  cmp   twowins,0
  je    @end
    mov   bx,0001h
    mov   ax,4F05h
// dx ,4
    int   10h
  @end:
  pop   dx
  pop   cx
  pop   bx
  pop   ax
end;

procedure decbank_vesa;far;assembler;
{decrease the graphical bank of vesa}
asm
  push  ax
  push  bx
  push  cx
  push  dx
  mov   dx,lastbank
  dec   dx
  mov   lastbank,dx
  mov   cl,shifter
  shl   dx,cl
  xor   cl,cl
  xor   bx,bx
  mov   ax,4F05h
  int   10h
  cmp   twowins,0
  je    @end
    mov   bx,0001h
    mov   ax,4F05h
    int   10h
  @end:
  pop   dx
  pop   cx
  pop   bx
  pop   ax
end;

procedure incbank_vesa;far;assembler;
{increase the graphical bank of vesa}
asm
  push  ax
  push  bx
  push  cx
  push  dx
  mov   dx,lastbank
  inc   dx
  mov   lastbank,dx
  mov   cl,shifter
  shl   dx,cl
  xor   cx,cx
  xor   bx,bx
  mov   ax,4F05h
  int   10h
  cmp   twowins,0
  je    @end
    mov   bx,0001h
    mov   ax,4F05h
    int   10h
  @end:
  pop   dx
  pop   cx
  pop   bx
  pop   ax
end;

procedure gotoxy_vesa(x,y : word);far;assembler;
asm
  mov   ax,4F07h
  xor   bx,bx
  mov   cx,x
  mov   dx,y
  int   10h
end;

function setvirtualscreenwidth_vesa(width : word) : word;far;
var
  state,bpl : word;
begin
  if (modeptr^.flags and mdlinear <> 0) then begin
    width := width*modeptr^.pixel;
    asm
      mov   ax,4F06h
      mov   bx,02h
      mov   cx,width
      int   10h
      mov   state,ax
      mov   bpl,bx
    end;
    setvirtualscreenwidth_vesa := bpl div modeptr^.pixel;
  end else
  begin
    asm
      mov   ax,4F06h
      xor   bx,bx
      mov   cx,width
      int   10h
      mov   state,ax
      mov   bpl,bx
    end;
    setvirtualscreenwidth_vesa := bpl;
  end;
  if (state <> $004F) then setvirtualscreenwidth_vesa := 0;
end;


function detect_vesa : word;
begin
  detect_vesa := 0;
  vesainfo.vesasignature := 'VBE2';{ We want some VBE2+ info }
  if not getvesainfo(vesainfo) then exit;
  if (vesainfo.vesasignature = 'VESA')and(vesainfo.vesaversion = vesa12) or (vesainfo.vesaversion = vesa20) or (vesainfo.vesaversion = vesa30) then
  { Signature OK and version >= 1.2? }
  begin { Ok, let's start... }
    detect_vesa := 1;
    with vbedriver.info do begin
      name     := inttostr(hi(vesainfo.vesaversion))+'.'+inttostr(lo(vesainfo.vesaversion));
      rev      := vesainfo.vesaversion;
      memory   := longint(vesainfo.totalmemory) shl 16;
    end;
  end;
end;


//doesnt do anything but find the VBE interface.
procedure init_vbe;

var
 pmid:pchar;
 i:integer;

begin
writeln("Loading VBE 3.0 Module...");

// copy BIOS into buffer
move (bios, c0000, 4096);

bios_search_ptr := ^bios; // pointer to our BIOS buffer
pmode_block:PMInfoBlock; // our info block
pmid: = "PMID"; // Signature to look for

// lookup the buffer:
i:=0;
repeat 
	// fill pmode_block
	move(pmode_block, bios_search_ptr, sizeof(PMInfoBlock));
	// if they are the same we have found the block
	if(memcmp(pmid, pmode_block.Signature) = 0)
	begin
		writeln(" VBE BIOS block found");
	end;

	// increase the pointer to scan the next PMInfoBlock
	inc(bios_search_ptr);
	inc(i);
until  i < (VBE_BIOS_SIZE - (sizeof(PMInfoBlock)) - 1);
end;


end.
