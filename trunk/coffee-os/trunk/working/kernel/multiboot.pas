unit multiboot;

{Where GRUB leaves us: (NO GUESSING!)

   1. CS points to a code segment descriptor with base address 0 and limit 4 gig - 1
   2. DS, SS, ES, FS, and GS point to a data segment descriptor with base address 0 and limit 4 gig - 1
   3. A20 is enabled
**   4. Paging is disabled
   5. Interrupts are disabled. No IDT is defined.
   6. The size and location of the GDT and selector values are undefined. Your kernel should create it's own GDT as soon as possible.
   7. EAX=0x2BADB002
   8. EBX contains the linear address of (i.e. a pointer to) a block of system and bootstrap information: 
          [Muiltiboot_info]
   9. We are in PROTECTED MODE. THERE IS NO WAY OUT. :(
}

interface

const

  MultiBootloaderMagic = $2BADB002;
  MULTIBOOT_HEADER_MAGIC  =$1BADB002;


type

 PELFHeaderSection = ^TELFHeaderSection;
  TELFHeaderSection = packed record
    Num: integer;
    Size: longint;
    Address: PtrUInt;
    Shndx: DWord;
  end;

  PAOUTHeaderSection = ^TAOUTHeaderSection;
  TAOUTHeaderSection = packed record
    TableSize: longint;
    StringSize: longint;
    Address: PtrUInt;
    reserved: DWord;
  end;

  PMemoryMap = ^TMemoryMap;
  TMemoryMap = packed record
    Size: LongWord;
    BaseAddress: QWord;
    Length: QWord;
    MType: byte; //If not 1 then not available..byte?
  end;
  

//GRUB modules, if any

  PModule = ^TModule;
  TModule = packed record
    ModuleStart: LongWord;
    ModuleEnd: LongWord;
    Name: string;
    Reserved: LongWord;
  end;

PDRIVE_ENT = ^DRIVE_ENT;
DRIVE_ENT =packed record
	size:longword;			//size of structure
	driveNumber:word; 
	driveMode:boolean;  //0=CHS,1=LBA
	driveCylinders:integer;
	driveHead:integer;
	driveSectors:integer;
	ports:array [1..8] of pchar;	//IOports used to access.
end;

//code,data setup in GDT(prt0.asm)
//code:offset should equal EP, but there is a bit of hacking involved
//this is generally real mode memory addressing, 64bits systems use APIC APCI off.

PAPM_Table = ^TAPM_Table;
TAPM_Table=packed record
     version:Word;              
     cseg:Word;                 
     offset:Longword;               
     cseg_16:Word;              
     dseg:Word;                 
     flags:Word;                
     cseg_len:Word;             
     cseg_16_len:Word;          
     dseg_len:Word;    
     get_power_status_broken:integer;
     get_power_status_swabinminutes:integer;
     allow_ints:boolean;
     forbid_idle:boolean;
     realmode_power_off:boolean;
     disabled:boolean;
end;

 Pmultiboot_header =^Tmultiboot_header;
  Tmultiboot_header=packed record
     
       magic:longword;
       flags:longword;
// The above fields plus this one must equal 0 mod 2^32.
       checksum:longword;
// if A.OUT KLUDGE SET
       header_addr:longword;
       load_addr:longword;
       load_end_addr:longword;
       bss_end_addr:longword;
       entry_addr:longword;
//if MULTIBOOT_VIDEO is set
       mode_type:Word;
       width:Word;
       height:word;
       depth:word;
   end;

//To get the info for the VESA unit..we get this info from GRUB.

  tModeList = Array [0..512] of word; {list of modes terminated by -1}
  {VESA modes are >=100h}
  
string4=string[4];  

PTVESAInfo = ^TVESAInfo; 
TVESAInfo = packed record
  //     VBESignature       :string4; //'VESA' or 'VBE2', this is sset prior to int10h,determines 
// which specs to pull, VBE1 or VBE2. GRUB pulled for us.
	   VBESignature       : array[0..3] of Char;
       version:word;
       OEMStringPtr       : Pointer;
       Capabilities       : Longint;
       VideoModePtr       : TModeList; //^dword to mode_list (list of valid modes)
       TotalMemory        : word; //in 64K chunks

       {VESA 2.0}
       OemSoftwareRev     : word;
       OemVendorNamePtr   : Pointer; 
       OemProductNamePtr  : Pointer;
       OemProductRevPtr   : Pointer;
       Padding: array [1..478] of Byte; {Change the upper bound to 512}
                                            {if you are using VBE2.0}
  end;
  

PTVESAModeInfo = ^TVESAModeInfo; 
  TVESAModeInfo = packed record
       ModeAttributes     : Word;
       WindowAFlags       : Byte;
       WindowBFlags       : Byte;
       Granularity        : Word;
       WindowSize         : Word;
       WindowASeg         : Word;
       WindowBSeg         : Word;
       BankSwitch         : Pointer;
       BytesPerLine       : byte;
//VB 1.1 info:
       XRes,YRes          : byte;
       CharWidth          : Byte; //xsize
       CharHeight         : Byte; //ysize
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
       {VESA 2.0+ stuff}
       PhysBasePtr        : Pointer; //FrameBuffer physical address
       OffScreenMemOffset : Pointer; //pointer to offscreen memory in VRAM
       OffScreenMemSize   : word; //in 1KB units
       padding: array [1..478] of Byte; {Change the upper bound to 512}
                                            {if you are using VBE2.0}
  end;

//use pointers to these records and they will get filled by grub for you.
//GRUB does not support APM BIOS, however.

  PMultiBootInfo = ^TMultiBootInfo; 
  TMultiBootInfo = packed record 
    Flags: longWord;
    LowerMemory: integer; //to access <640k base mem
    UpperMemory: longword;  //to access >1MB base mem
    
    BootDevice: array [1..4] of byte; //device which booted us.(4 bytes)
    CmdLine: string; //cmdline arguments given ("linux single"....etc.)
    ModuleCount: integer; //number of grub modules
    ModAddress: Longint;  //boot  grub modules location
    aout_sym: PAOUTHeaderSection;
    elf_sec:  PELFHeaderSection; 
    MemoryMapLength: longword;  //sizeof(MMAP)
    MemoryMapAddress:longword; //current memory location

    drives_length:integer; //size of boot volume in bytes (mod 1024)
    drives_addr:PDRIVE_ENT; //address of drives table 
    config_table:Longint; //BIOS Config table
    boot_loader_name:pchar; //='GNU GRUB'
    apm_table:PAPM_Table;  //Location of APM memory locations 
    vbe_control_info:TVESAInfo;  //vesa info
    vbe_mode_info:TVESAModeInfo; //vesa ModeInfo
    vbe_mode:Word; //current VESA Mode

    vbe_interface_seg:Word; 
    vbe_interface_off:Word;
    vbe_interface_len:Word; //VBE bios size (in 64K chunks)  
 end;

//only useful to check, not to setup.
 
   PTPMInfo = ^TPMInfo; 
   TPMInfo=packed record 
	Signature: string4;  { 0x44494D50 = PMID }
	EntryPoint:word; //This would be within ROM area C000-FFFF.... (C000:EP)
	PMInitialize:word; //call this before using
	BIOSDataSel:word; //6000h  copy of VBIOS
	A0000Sel:word;
	B0000Sel:word; 
	B8000Sel:word;
	InProtectMode:byte;
	Checksum:byte;
end;

{

Format of VESA VBE 3.0 Protected Mode Information Block:
(size:20 bytes)

Offset	Size	Description	(Table M0127)
 00h  4 BYTEs	signature "PMID"
 04h	WORD	offset of protected-mode entry point within BIOS
 06h	WORD	offset of protected-mode initialization entry point
 08h	WORD	selector for BIOS data area emulation block
		(default 0000h, must be set by protected-mode OS to 16-bit
		  read/write data selector with limit of at least 0600h)
 0Ah	WORD	selector to access physical memory at A0000h
		(default A000h, must be set by protected-mode OS to 16-bit
		  read/write data selector with 64K limit)
 0Ch	WORD	selector to access physical memory at B0000h
		(default B000h, must be set by protected-mode OS to 16-bit
		  read/write data selector with 64K limit)
 0Eh	WORD	selector to access physical memory at B8000h
		(default B800h, must be set by protected-mode OS to 16-bit
		  read/write data selector with 32K limit)
 10h	BYTE	protected-mode execution (default 00h; set to 01h by OS when
		  BIOS image is running in protected mode)
 11h	BYTE	checksum byte for entire structure (this byte forces 8-bit
		  sum of all bytes to 00h)



}

//THANKS, GRUB...~Half my work is done~
var
   PMinfo: PTPMInfo;
   mbinfo: PMultiBootInfo; export name 'mbinfo'; 
   modeinfo: PTVESAModeInfo; 
   vesaInfo: PTVESAInfo ;
   //mbinfo is specified in the pmm unit.
   //yes, these redefinitions are needed to access the 'records'.
   ELFHeaderSection:PELFHeaderSection;
   AOUTHeaderSection:PAOUTHeaderSection;
   MemoryMap:PMemoryMap; 
   GModules:PModule;  //grub modules
   DInfo:PDRIVE_ENT;  //drive info
   APM_Table:PAPM_Table;
   multiboot_header:Pmultiboot_header;
   
implementation

end.

