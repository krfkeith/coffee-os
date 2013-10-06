{ATA HD access unit. Needs heavy rework. For now I threw all similar units together.

--Jazz
}

var

  ch:char;
  I:byte;

  LastSecNo:longint;               { Save last sector number... }
  LastError:word;                  { ... and error code for error report }

  Waiting : boolean;
  Size : LongWord;
  {$Align 32}
  Buffer : array[1..$4800] of byte; //4K buffer and some extra bytes 
  
Type
 PIDEBlockDisk = ^TIDEBlockDisk;
 PIDEController = ^TIDEController;
 PPartitionEntry = ^TPartitionEntry;
 
 // IDE Block Disk structure
 TIDEBlockDisk = record
   StartSector : longint;
   Size: longint;
   FsType: longint;
   FileDesc: TFileBlock;
   next: PIDEBlockDisk;
 end;
 
// IDE Controller Disk
 TIDEController = record
   IOPort: longint; //1F0 ...etc.
   irq: longint;
   IrqReady: boolean;
   IrqHandler: PtrUInt;
   Minors :array[0..MAX_ATA_MINORS-1] of  TIDEBlockDisk; //(partitions on the drive) 
 end;

  THardDiskParam = record
    Cylinders: LongWord;
    Heads: LongWord;
    Sectors: LongWord;
  end;


  JmpRec=record       { The starting jump in the bootsector }
    Code:byte; { $E9 XX XX / $EB XX XX}
    case byte of
       0:(Adr1:byte; NOP:byte);
       1:(Adr2:word);
    end;
  end;

 BpbRec=record       { The Bios Data Block ( stored in }
 BytesPSec:word;    { the bootsector }  

  SecPClus:byte;
  ResSec:word;

  NrFATs:byte;
  NrROOT:word;
  
  TotalSec:word;
  MDB:byte;
  SecPFAT:word;
  SecPSpoor:word;
  NrHead:word;
  HidSec32:longint;
  TotalSec32:longint;
 end;
 
 BootSec=record      { The bootsector format }
  JmpCode:JmpRec;
  Name:array[0..7] of char; { Isn't meaningfull at all, just FORMAT prg name }
  Bpb:BpbRec;
 end;

 BootSecP=^BootSec;
 
   partition = record
      p_type  : byte;  { Partition type }
      res1    : byte;
      res2    : byte;
      res3    : byte;
      p_begin : dword; { Partition first logical sector }
      p_size  : dword; { Partition size (number of sectors) }
   end;


   { ide_struct registers drives information }

   ide_struct = record
      ide_type    : byte;
      dword_io    : byte;     { = 1 if 32bits I/O are supported }
      IO_base     : word;
      irq         : byte;     { IDE controller IRQ }
      ide_sem     : byte;     { Semaphore (not used) }
      cyls        : word;     { Nb of cylinders }
      heads       : word;     { Nb of heads }
      sectors     : word;     { Nb of sectors per track }
      lba_sectors : dword;    { Total number of sectors }
      part        : array[1..MAX_NR_PART] of partition;   { Partitions information }
   end;

   { ide_struct details :

     - ide_type :   $FF -> No drive
                    $82 -> Hard drive using LBA
                    $05 -> CD-ROM or DVD-ROM
                    $02 -> Hard drive without LBA
                    $01 -> IDE TAPE
                    $00 -> IDE FLOPPY (zip drive)

     cyls, heads and sectors are initialized only if drive has type $02.

     lba_sectors is initialized only if drive has type $82

   }

   partition_entry = record
      boot          : byte;   { Indicates if partition is bootable }
      begin_head    : byte;   { Partition first head }
      begin_sec_cyl : word;   { Partition first sector and cylinder }
      type_part     : byte;   { Partition type }
      end_head      : byte;   { Partition last head }
      end_sec_cyl   : word;   { Partition last sector and cylinder }
      dist_sec      : dword;  { 1st sector distance }
      taille_part   : dword;  { Partition size (number of sectors) }
   end;

   partition_table = record
      nop        : array[0..445] of byte;
      entry      : array[1..4]   of partition_entry;
      magic_word : word;
   end;

   { The following structure (drive_id) has been taken from Linux hdreg.h,
     it registers information returned by IDE drives 'identify' command
     (ATA-2 standard) }

   P_drive_id = ^drive_id;

   drive_id = record
      config         : word;    { General configuration (obselete) }
      cyls           : word;    { Number of cylinders }
      reserved2      : word;    { Specific configuration }
      heads          : word;    { Number of logical heads }
      track_bytes    : word;    { Obsolete }
      sector_bytes   : word;    { Obsolete }
      sectors        : word;    { Number of logical sectors per logical track }
      vendor0        : word;    { vendor unique }
      vendor1        : word;    { vendor unique }
      vendor2        : word;    { vendor unique }
      serial_no      : array[1..20] of char;   { Serail number }
      buf_type       : word;    { Obsolete }
      buf_size       : word;    { 512 byte increments; 0 = not_specified }
      ecc_bytes      : word;    { Obsolete }
      fw_rev         : array[1..8] of char;      { Firmware revision }
      model          : array[1..40] of char;     { Model number }
      max_mulsect    : byte;    { read/write multiple support }
      vendor3        : byte;    { vendor unique }
      dword_io       : word;    { 0 = not_implemented; 1 = implemented }
      vendor4        : byte;    { vendor unique }
      capability     : byte;    { bits 0:DMA  1:LBA  2:IORDYsw  3:IORDYsup }
      reserved50     : word;    { reserved (word 50) }
      vendor5        : byte;    { vendor unique }
      tPIO           : byte;    { 0=slow, 1=medium, 2=fast }
      vendor6        : byte;    { vendor unique }
      tDMA           : byte;    { speed of DMA ; 0=slow, 1=medium, 2=fast }
      field_valid    : word;    { bits 0:cur_ok 1:eide_ok }
      cur_cyls       : word;    { cylindres logiques }
      cur_heads      : word;    { tetes logic }
      cur_sectors    : word;    { logical sector par piste }
      cur_capacity0  : word;    { number of total logical sectors }
      cur_capacity1  : word;    { 2 words, misaligned int }
      multsect       : byte;    { compteur secteur multiple courrant }
      multsect_valid : byte;    { quand (bit0==1) multsect is ok }
      lba_capacity   : dword;   { nombre total de secteur }
      dma_1word      : word;    { informations sur le DMA single-word}
      dma_mword      : word;    { multiple-word dma info }
      eide_pio_modes : word;    { bits 0:mode3 1:mode4 }
      eide_dma_min   : word;    { min mword dma cycle time (ns) }
      eide_dma_time  : word;    { recommended mword dma cycle time (ns) }
      eide_pio       : word;    { min cycle time (ns), no IORDY }
      eide_pio_iordy : word;    { min cycle time (ns), with IORDY }
      word69         : word;
      word70         : word;
      word71         : word;
      word72         : word;
      word73         : word;
      word74         : word;
      word75         : word;
      word76         : word;
      word77         : word;
      word78         : word;
      word79         : word;
      word80         : word;
      word81         : word;
      command_set_1  : word;		{  15: Obsolete
      	             	      		14: NOP command
				   							13: READ_BUFFER
				   							12: WRITE BUFFER
				   							11: Obsolete
				   							10: Host Protected Area
				   							09: DEVICE Reset
				   							08: SERVICE Interrupt
				   							07: Release Interrupt
				   							06: look-ahead
				   							05: write cache
				   							04: PACKET Command
				   							03: Power Management Feature Set
				   							02: Removable Feature Set
				   							01: Security Feature Set
				   							00: SMART Feature Set }

      command_set_2  : word;     {  15: Shall be ZERO
      	             	      	 	14: Shall be ONE
				   							13: FLUSH CACHE EXT
				   							12: FLUSH CACHE
				   							11: Device Configuration Overlay

				   							10: 48-bit Address Feature Set
				   							09: Automatic Acoustic Management
				   							08: SET MAX security
				   							07: reserved 1407DT PARTIES
				   							06: SetF sub-command Power-Up
				   							05: Power-Up in Standby Feature Set

				   							04: Removable Media Notification
				   							03: APM Feature Set
				   							02: CFA Feature Set
				   							01: READ/WRITE DMA QUEUED
				   							00: Download MicroCode }
      word84         : word;
      cfs_enable_1   : word;     {  15: Obsolete

      	             	      	   14: NOP command
				   							13: READ_BUFFER
				   							12: WRITE BUFFER
				   							11: Obsolete
				   							10: Host Protected Area
				   							09: DEVICE Reset
				   							08: SERVICE Interrupt

				   							07: Release Interrupt
				   							06: look-ahead
				   							05: write cache
				   							04: PACKET Command
				   							03: Power Management Feature Set
				   							02: Removable Feature Set
				   							01: Security Feature Set

				   							00: SMART Feature Set }

      cfs_enable_2   : word;     {  15: Shall be ZERO
      	             	      	   14: Shall be ONE
				   							13: FLUSH CACHE EXT
				   							12: FLUSH CACHE
				   							11: Device Configuration Overlay
				   							10: 48-bit Address Feature Set
				   							09: Automatic Acoustic Management
				   							08: SET MAX security

				   							07: reserved 1407DT PARTIES

				   							06: SetF sub-command Power-Up
				   							05: Power-Up in Standby Feature Set
				   							04: Removable Media Notification
				   							03: APM Feature Set
				   							02: CFA Feature Set
				   							01: READ/WRITE DMA QUEUED
				   							00: Download MicroCode }
      word87         : word;
      dma_ultra      : word;
      word89         : word;
      word90         : word;
      word91         : word;
      word92         : word;
      word93         : word;
      word94         : word;
      word95         : word;
      word96         : word;
      word97         : word;
      word98         : word;
      word99         : word;
      word100        : word;
      word101        : word;
      word102        : word;
      word103        : word;
      word104        : word;
      word105        : word;
      word106        : word;
      word107        : word;
      word108        : word;
      word109        : word;
      word110        : word;
      word111        : word;
      word112        : word;
      word113        : word;
      word114        : word;
      word115        : word;
      word116        : word;
      word117        : word;
      word118        : word;
      word119        : word;
      word120        : word;
      word121        : word;
      word122        : word;
      word123        : word;
      word124        : word;
      word125        : word;
      word126        : word;
      word127        : word;
      security       : word;    { bits 0:support 1:enable 2:locked 3:frozen }
      reserved       : array[1..127] of word;
   end;
     
 // entry in Partition Table.
 TPartitionEntry = record
  boot: byte;
  BeginHead: byte;
  BeginSectCyl: word;
  pType: byte;
  EndHead: byte;
  EndSecCyl: word;
  FirstSector: dword;
  Size: dword;
 end;

{
procedure read_file;
var
  sector:integer;
begin
   sector:=0; //bootsector
   IDEInit;
   ATASelectDisk(0,0); //HD 1
   ATAReadBlock(sectorStart,sectorEnd,buffer);
end;

procedure write_file;
var
  sector:integer;
begin
   sector:=0; //bootsector
   IDEInit;
   ATASelectDisk(0,0); //HD 1
   ATAWriteBlock(sectorStart,sectorEnd,buffer);
end;

}

//useage: ATADetectPartition(0,minor);
//needs some slight modification
//if Minors[Minor+I].FsType:= (Linux Part type) then writeln('found root ext FS v2-4 on HD.')
// Linux Ext2=83;
// Linux Swap=82;
// 5 means extended partition type (keep looking)

procedure ATADetectPartition(Minor: longint); //specify number to scan to get type
var 
 i: longint;
 Buff: array[0..511] of byte;
 Entry: PPartitionEntry;
begin

  writeportb($1F1, $00);
   writeportb($1F2, $01);
   writeportb($1F3, addr); //block address
   writeportb($1F4,(addr >> 8);
   writeportb($1F5,(addr >> 16);
   writeportb($1F6, $E0 or (drive << 4) or ((addr >> 24) and $0F));
   writeportb($1F7, $20);
//  while ReadPortL(Ord(Status)) and $80<>0 do ;

if not(AtaError(Ctr)) and ATADataReady(Ctr) then
begin
 ATAIn(Ctr,@Buff[0]);

 if (Buff[511]=$AA) and (Buff[510]=$55) then
 begin
 Entry:= @Buff[446];
 for i:= 1 to 4 do 
  begin
   if Entry^.pType<>0 then
   begin
    Minors[Minor+I].StartSector:= Entry^.FirstSector;
    Minors[Minor+I].Size:= Entry^.Size;
    Minors[Minor+I].FsType:= Entry^.pType;
    Minors[Minor+I].FileDesc.Minor:=Minor+I;
    Minors[Minor+I].FileDesc.BlockSize:= BLKSIZE;
    Minors[Minor+I].FileDesc.next:=nil;
//ifdef debug
	WriteConsole('IdeDisk: ', []);
	WriteConsole(', Partition: ',Minor+I, 'Size: ',Entry^.Size div 2048,' Mb, Type: ',Entry^.pType);
   end; 
   Entry := Entry +1;
 end;
 end;
end;
end;


procedure ATADetectController;
var 
 i,Drv:longint;
 ATA_Buffer: ATAIdent;
begin
for i:= 0 to (MAX_ATA_CONTROLLER-1) do
begin
 if (IOPort<>0) and (ATAProbeController(@ATAControllers[I])) then
 begin
  for Drv:= 0 to 1 do
  begin

   ATASelectDisk(@ATAControllers[I],Drv*5);
   writeportb($1F7, $EC);

   While ATABUSY(@ATAControllers[I]) do;
   if ATAError(@ATAControllers[I]) or not(ATADataReady(@ATAControllers[I])) then
     //error
   else
   begin
    ATAIn(@ATAControllers[I],@ATA_Buffer);

    Minors[Drv*5].StartSector:= 0;
    Minors[Drv*5].Size:= ATA_Buffer.LBA_Capacity;
    Minors[Drv*5].FSType:= NOT_FILESYSTEM;
    Minors[Drv*5].FileDesc.Minor:= Drv*5;
    Minors[Drv*5].FileDesc.BlockSize:= BLKSIZE;
    Minors[Drv*5].FileDesc.Next:= nil;
	
	WriteConsole(' Partition:',Drv*5,' Size: ',ATA_Buffer.LBA_Capacity div 2048,' Mb, Type: ', ,NOT_FILESYSTEM);
	ATADetectPartition(@ATAControllers[I],Drv*5);
   end;
  end;
  // Irq Handlers
   Irq_On(ATAControllers[I].Irq);
   HDEnable:=true;
 end;
end;
end;
 

//AKA:blockread and blockwrite for fileopts...thanks.

function ATAReadBlock(FileDesc: PFileBlock;Block,Count: longint;Buffer: pointer):longint;
var
 ncount: longint;
 Ctr: PIDEController;
begin

Block:=Block + Minors[FileDesc^.Minor].StartSector;
Ctr^.IrqReady:= false;
ncount:= 0;
// Sending Commands 
ATAPrepare(Ctr,FileDesc^.Minor,Block,Count);

ATASendCommand(Ctr,$20);
// reading
repeat
// Waiting for Irq ; more time if Thread sleep for Irq and then Wake Up when Irq happens
 while not(Ctr^.IrqReady) do
//  ThreadSwitch;
 // error in operation
 if not(ATADataReady(Ctr)) or ATAError(Ctr) then 
   break;
 IrqReady:= false;
 ATAIn(Ctr,Buffer);
 Buffer:= Buffer + 512;
 ncount:= ncount+1;
until (ncount=Count);
// exiting with the number of blocks readed
result:=ncount;
end;


function ATAWriteBlock(FileDesc: PFileBlock;Block,Count: longint;Buffer: pointer):longint;
var
 ncount: longint;
 Ctr: PIDEController;
begin

Ctr:= @ATAControllers[FileDesc^.BlockDriver^.Major];
// for NOT_FILESYSTEM type that is not important because StartSector is equal to 0
Block:=Block + Minors[FileDesc^.Minor].StartSector;
ncount:= 0;

ATAPrepare(Ctr,FileDesc^.Minor,Block,Count);

ATASendCommand(Ctr,$30);
// writing
repeat
 FileDesc^.BlockDriver^.WaitOn^.state := tsSuspended;

 ATAOut(Ctr,Buffer);

 // Waiting the IRQ
 ThreadSwitch;
 if ATAError(Ctr) then break;
 Buffer:= Buffer+512;
 ncount:= ncount+1;
 Block:= Block+1
until (ncount=Count);
// exiting with numbers of blocks written
result:=ncount;

end;

{
var
 Bpb:BpbRec;  Global copy of Bios Parameter block, for ClusToSec 

function ClusToSec(C:word):longint;
 Convert clusternumber to sector number, because the cluster is often bigger 
 than one sector, you need to read multiple succeeding sectors to read the 
 whole cluster (number of sectors in a cluster is in BPB (BPB.SecPClus)) 
 Uses global BpbRec Bpb and global longint DATASec 
begin
 ClusToSec:=((C-2)*Bpb.SecPClus)+DATASec;
end;
}

procedure DiskRError; 
begin

 WriteLn('Error reading disk! Sector:',LastSecNo,' Errorcode:',LastError);
 asm
	int 24h //trip our handler for 'abort,retry.....'
 end;
end;


Unit BaseHD;
{ PLEASE: keep in mind this is BARE BONES HW access and only applies ATM to IDE drives.
SATA usually sit on seperate controllers and/or USB controller hardware(NOT on PCI bus)

Most drives support LBA28, but not all drives support LBA48. 
This is ALPHA level code and has NOT been tested YET.
I am still going thru some of this.(probing/blockIO...)

This has been shortened for simplicity.

  HardDiskPorts:
1st IDE controler STD: $1F0 [and 1-7] 
2nd IDE controler STD: $170 [and 1-7] 

    Data1:=$1F0
    Error1:=$1F1        
    SectorCount1:=$1F2 
    SectorNumber1:=$1f3 
    LowCylinder1:=$1F4  
    HighCylinder1:=$1f5 
    Head1:=$1f6         
    CmdStatus1:=$1f7       
  
    Data2:=$1F0
    Error2:=$1F1        
    SectorCount2:=$1F2 
    SectorNumber2:=$1f3 
    LowCylinder2:=$1F4  
    HighCylinder2:=$1f5 
    Head2:=$1f6         
    CmdStatus2:=$1f7       

SATA:(implement here, commands are SAME)


  HardDiskCommands: (only two)
    ReadHD:=$20
    WriteHD:=$30

//Master controller
ATAControllers[0].Irq:= 14;

// Slave Controller
ATAControllers[1].Irq := 15;

This unit should give us all of the IDE acess command functions that we need.

Now, all we need to do is 'mount' the Linux partition within the (/) root structure and repoint the (/) root
folder from the initRD to the actual partition.That pointer is in RAM (AKA, RAMdisk) via GRUB module.

FS specific code (mostly headers) is elsewhere

Partition Table Information and location:

Format of hard disk master boot record:
. Offset  Size    Description
. 000h 446 BYTEs  Master bootstrap loader code


. 1BEh 16 BYTEs   partition record for partition 1 
. 1CEh 16 BYTEs   partition record for partition 2
. 1DEh 16 BYTEs   partition record for partition 3
. 1EEh 16 BYTEs   partition record for partition 4

. 1FEh    WORD    signature, AA55h indicates valid BIOS extension block
.
Total Size==512 BYTES.

Format of partition record:
Offset  Size    Description
 00h    BYTE    boot indicator (80h = active, 00H = inactive )
 01h    BYTE    partition start head
 02h    BYTE    partition start sector (bits 0-5)
 03h    BYTE    partition start track (bits 8,9 in bits 6,7 of sector)
 04h    BYTE    operating system indicator (see below)
 05h    BYTE    partition end head
 06h    BYTE    partition end sector (bits 0-5)
 07h    BYTE    partition end track (bits 8,9 in bits 6,7 of sector)
 08h    DWORD   sectors preceding partition
 0Ch    DWORD   length of partition in sectors

Remember, we got the booted drive number and partition from GRUB, all we have to do is use that info
to scan the Partition Types on Disk and Mount an ext FS via changing the pointer to /.

--Jazz

}

const
   { Block devices }

   FLOPPY = 2;
   IDE0   = 3;
   IDE1   = 4;
   IDE2   = 5;
   IDE3  = 6;
   NB_RETRY = 3;  { Nb of times we retry when a command fails }
   ATAPI_IDENTIFY = $A1;

   { Status register bits }
   ERR_STAT   = $01; 	{ error register contains error information }
   INDEX_STAT = $02; 	{ disk index has just passed }
   ECC_STAT   = $04; 	{ Data error }
   DRQ_STAT   = $08; 	{ Data can be transferred }
   SEEK_STAT  = $10; 	{ Head positioning (seek) complete }
   WRERR_STAT = $20; 	{ Write fault }
   READY_STAT = $40; 	{ Drive is ready }
   BUSY_STAT  = $80; 	{ Drive is busy }


 // Max number of drives supported
 MAX_ATA_DISK = 8;
 MAX_ATA_CONTROLLER= 2;
 MAX_SATA_DISK = 32;
 MAX_ATA_MINORS= 10; //Max Partitions
 NOT_FILESYSTEM = $ff; //or $00
 
 // ATA Commands, fully supported by SATA. Someone else tested this. 
 ATA_IDENTIFY= $EC;
 
 // Size of physical blocks
 BLKSIZE= 512;
 BigBLKSIZE=1024; //found on newer drives.
 LArgeBLKSIZE=2048; //found on newer drives.
 HugeBLKSIZE=4096; //found on newer drives.

 // PCI header identificators
 PCI_STORANGE_CLASS = 1;
 PCI_IDE_SUBCLASS = 1;
 PCI_AHCI_SUBCLASS = 6;

interface

procedure ReadSectLBA28(addr:char);
procedure WriteSectLBA28(addr:char);
procedure ReadSectLBA48(addr:char); 
procedure WriteSectLBA48(addr:char); 
procedure ProbeIDE;
//Now that we know what drives are here, we can do partition probing and actual sector access via NFS and other FS methods.
implementation

function data_ready (major, minor : byte) : boolean; [public, alias : 'DATA_READY'];

var
   i    : dword;
   base : word;

begin

   base   := IO_base;
   result := FALSE;

   for i := 1 to 70000 do
   begin
      if (readportb(base + 7) and (DRQ_STAT or BUSY_STAT)) = DRQ_STAT then
      begin
         result := TRUE;
	 exit;
      end;
   end;

end;

function hd_result (base : word) : boolean;

var
   status : byte;

begin

   status := readportb(portIO+ 7);

   if ((status and (BUSY_STAT or READY_STAT or WRERR_STAT or SEEK_STAT or
        ERR_STAT)) = (READY_STAT or SEEK_STAT)) then
      result := TRUE
   else
      result := FALSE;
end;

function drive_busy (major, minor : byte) : boolean; [public, alias : 'DRIVE_BUSY'];

var
   i    : dword;
   base : word;

begin

   base   := drive_info[major, minor div 64].IO_base;
   result := TRUE;

   for i := 1 to 70000 do
   begin
      if (readportb(base + 7) and BUSY_STAT) = 0 then
      begin
      	 result := FALSE;
	 exit;
      end;
   end;

end;

function lba_is_ok(block:u64; n_block:u32):integer;

   {*
    * The ATA spec tells large drives to return
    * C/H/S = 16383/16/63 independent of their size.
    * Some drives can be jumpered to use 15 heads instead of 16.
    * Some drives can be jumpered to use 4092 cyls instead of 16383.
    *}
   if (((id^.cyls = 16383) or ((id^.cyls = 4092) and (id^.cur_cyls = 16383)))
      and (id^.sectors = 63) and ((id^.heads = 15) or (id^.heads = 16))
      and (id^.lba_capacity >= 16383*63*id^.heads)) then
   begin
      result := TRUE;
      exit;
   end;

   lba_sects := id^.lba_capacity;
   chs_sects := id^.cyls * id^.heads * id^.sectors;

   {* perform a rough sanity check on lba_sects:  within 10% is OK *}
   if ((lba_sects - chs_sects) < chs_sects div 10) then
   begin
      result := TRUE;
      exit;
   end;

   {* some drives have the word order reversed *}
   asm
      mov   eax, lba_sects
      shr   eax, 16
      and   eax, $FFFF
      mov   head, eax	{ head = ((lba_sects >> 16) & 0xffff); }
   end;
   tail := lba_sects and $FFFF;
   asm
      mov   eax, tail
      shl   eax, 16
      or    eax, head
      mov   lba_sects, eax
   end;
   if ((lba_sects - chs_sects) < chs_sects div 10) then
   begin
      id^.lba_capacity := lba_sects;
      result := TRUE;
      exit;   {* lba_capacity is (now) good *}
   end;

   result := FALSE;   {* lba_capacity value may be bad *}

end;

{
begin
	// check the ending block number 
	return ((block + n_block) < (1 shl 28)) and (n_block <= 256);
end;

function lba_48_ok(block:u64;  n_block:u32):integer;
begin
	// check the ending block number 
	return ((block + n_block - 1) < (1 shl 48)) && (n_block <= 65536);
end;
}
procedure ReadSectLBA28(addr:char); //addr:unsigned char
//FIXME: if addr> maxsize then writestring('Requesed sector is beyond drive boundary');
// exit;

  { Loop until disk is ready }
//  while ReadPortL((Ord(Status) and $C0)<>$40 do ;
 


   writeportb($1F1, $00);
   writeportb($1F2, $01);
   writeportb($1F3, addr); //block address
   writeportb($1F4,(addr >> 8);
   writeportb($1F5,(addr >> 16);
   writeportb($1F6, $E0 or (drive << 4) or ((addr >> 24) and $0F));
   writeportb($1F7, $20);
//  while ReadPortL(Ord(Status)) and $80<>0 do ;


end;

procedure WriteSectLBA28(addr:char);
   writeportb($1F1, $00);
   writeportb($1F2, $01);
   writeportb($1F3, addr); //block address
   writeportb($1F4,(addr >> 8);
   writeportb($1F5,(addr >> 16);
   writeportb($1F6, $E0 or (drive << 4) or ((addr >> 24) and $0F));
   writeportb($1F7, $30);
end;

procedure ReadSectLBA48(addr:char); //much bigger drives but not QEMU supported
//Sector: longint (addr)
   writeportb($1F1, $00); 
   writeportb($1F1, $00);
   writeportb($1F2, $00); 
   writeportb($1F2, $01);
   writeportb($1F3, (addr >> 24));
   writeportb($1F3, addr);
   writeportb($1F4, (addr >> 32));
   writeportb($1F4, (addr >> 8));
   writeportb($1F5, (addr >> 40));
   writeportb($1F5, (addr >> 16));
   writeportb($1F6, $40 or (drive << 4));
   writeportb($1F7, $24); //read sector

//Wait for the drive to signal that it's ready:
while (readportb($1F7) and $08) do begin 

	for (idx = 0; idx < 256; idx++)
	begin
		tmpword = readportw($1F0);
		buffer[idx * 2] = tmpword;
		buffer[idx * 2 + 1] = (tmpword >> 8);
	end;
end;


//codes to check drive status
function ATABusy(Ctr:PIDEController):boolean;{$IFDEF Inline} inline;{$ENDIF}
var
 tmp:byte;
begin
tmp:=read_portb(IOPort+7);
result:=Bit_Test(@tmp,7);
end;


function ATAError(Ctr:PIDEController): boolean;{$IFDEF Inline} inline;{$ENDIF}
var
 tmp:byte;
begin
tmp:=read_portb(IOPort+7);
result:=Bit_Test(@tmp,0);
end;


function ATADataReady (Ctr:PIDEController): boolean;{$IFDEF Inline} inline;{$ENDIF}
var
 tmp:byte;
begin
tmp:=read_portb(IOPort+7);
result:=Bit_Test(@tmp,3);
end;



procedure ATASelectDisk;{$IFDEF Inline} inline;{$ENDIF}
begin
// IOPort= which drive??
   write_portb($A0,(IOPort+6));
end;

procedure WriteSectLBA48(addr:char); //much bigger drives but not QEMU supported

   writeportb($1F1, $00); 
   writeportb($1F1, $00);
   writeportb($1F2, $00); 
   writeportb($1F2, $01);
   writeportb($1F3, (addr >> 24));
   writeportb($1F3, addr);
   writeportb($1F4, (addr >> 32));
   writeportb($1F4, (addr >> 8));
   writeportb($1F5, (addr >> 40));
   writeportb($1F5, (addr >> 16));
   writeportb($1F6, $40 or (drive << 4));
   writeportb($1F7, $34); //write sector

//Wait for the drive to signal that it's ready:
while (readportb($1F7) and $08) do begin 

	for (idx = 0; idx < 256; idx++)
	begin
		tmpword = buffer[8 + idx * 2] or (buffer[8 + idx * 2 + 1] << 8);
		writeportw($1F0, tmpword);
	end;
end;

procedure ProbeIDE;

var
   maj             : byte;
   i, j            : dword;
   high_16         : word;
   pci_devices     : P_pci_device;
   udma_speed_flag : byte;

begin

   maj := 5; //HD

   pci_devices := first_pci_device;
   for i := 1 to nb_pci_devices do
   begin
      if ((pci_devices^.main_class = $01) and //storage controllers
          (pci_devices^.sub_class  = $80)) then //ATA type
      begin

         if (pci_devices^.vendor_id = $105A) and (pci_devices^.device_id = $4D38) then
	 begin 
	    { Try to init pdc202xx controller }
	    IO_base1 := pci_devices^.io[0]; //addr and 0
	    irq     := pci_devices^.irq; //14
	    IO_base2  := pci_devices^.io[0]; //addr and 0
	    irq      := pci_devices^.irq; //14
	    maj += 1;
	    IO_base1 := pci_devices^.io[2]; //addr and 2
	    irq     := pci_devices^.irq; //15
	    IO_base2  := pci_devices^.io[2]; //addr and 2
	    irq      := pci_devices^.irq; //15
	   
	    high_16 := lo(pci_devices^.io[4]);
	    udma_speed_flag := readportb(high_16 + $001F);
	    writeportb(high_16 + $001F, udma_speed_flag or $10);
	    delay(250);
	    writeportb(high_16 + $001F, udma_speed_flag and (not $10));
	    delay(250);
	    delay(250);
	    exit;
	 end else //any other controller
	 begin
            for j := 0 to 5 do //up to 6..possibly SATA controllers
	    begin
	       if (probe_controller(pci_devices^.io[j])) then
	       begin
	            IO_base1 := pci_devices^.io[0]; //addr and 0
	            irq     := pci_devices^.irq; //14
	            IO_base2  := pci_devices^.io[0]; //addr and 0
	            irq      := pci_devices^.irq; //14
	            maj += 1;
	            IO_base1 := pci_devices^.io[2]; //addr and 2
	            irq     := pci_devices^.irq; //15
	            IO_base2  := pci_devices^.io[2]; //addr and 2
	            irq      := pci_devices^.irq; //15
  		    exit;
	       end;
	    end;
	    exit;
	 end;
      end;
      pci_devices := pci_devices^.next;
   end;

//This is for Standard IDE drives(on PCI controller)

//Do the controllers actually exist? and if so, probe for a drive on ALL available channels.
   writeportb($1F3, $88);
   if readportb($1f3)=$88 then begin
	//Drives on 1st standard controller
	writeportb($1F6, $A0); // use 0xB0 instead of 0xA0 to test the second drive on the controller
	delay(250); // wait 1/250th of a second
	tmpword: = readportb($1F7); // read the status port
	if (tmpword and $40) then writestring("Primary HD exists"); // see if the busy bit is set

	writeportb($1F6, $B0); // use 0xB0 instead of 0xA0 to test the second drive on the controller
	delay(250); // wait 1/250th of a second
	tmpword: = readportb($1F7); // read the status port
	if (tmpword and $40) then writestring("Secordary HD exists"); // see if the busy bit is set
   end;
   //We could be SATA here

   writeportb($173, $88);
   if readportb($173)=$88 then begin
	//Drive on 2nd strandard controller
	writeportb($176, $A0); // use 0xB0 instead of 0xA0 to test the second drive on the controller
	delay(250); // wait 1/250th of a second
	tmpword: = readportb($1F7); // read the status port
	if (tmpword and $40) then writestring("Primary HD exists"); // see if the busy bit is set
	
	writeportb($176, $B0); // use 0xB0 instead of 0xA0 to test the second drive on the controller
	delay(250); // wait 1/250th of a second
	tmpword: = readportb($1F7); // read the status port
	if (tmpword and $40) then writestring("Secordary HD exists"); // see if the busy bit is set
   end;
   //We could be SATA here



{ Maybe we have to reset all the controllers (software reset). Don't know. }

{
   writeportb($1F6, 4);
   drive_busy(4, 0);
   writeportb($1F6, 0);
   drive_busy(4, 0);

   writeportb($176, 4);
   drive_busy(4, 0);
   writeportb($176, 0);
   drive_busy(4, 0);

}

end;

procedure LBA2CHS(LBA:DWord; var cyl,head,sec : byte);


var
   tmp : dword;

begin
sec := (LBA mod 18) + 1;
cyl := LBA Div (sec*36);
head := (LBA Div 18) mod 2;
end;

 
procedure ATAHandler(Controller:longint);
begin

waitstate:=ready;
ATAControllers[Controller].Driver.WaitOn^.state := tsReady;
ATAControllers[Controller].IrqReady:=not(ATAControllers[Controller].IrqReady);
writeportb($20,$20);

end;



procedure ATA0IrqHandler;[nostackframe];assembler;
asm
 // save registers
 push rbp
 push rax
 push rbx
 push rcx
 push rdx
 push rdi
 push rsi
 push r8
 push r9
 push r13
 push r14
 // protect the stack
 mov r15 , rsp
 mov rbp , r15
 sub r15 , 32
 mov  rsp , r15
 // set interruption
 sti
 xor rcx , rcx
 // call handler
 Call ATAHandler
 mov rsp , rbp
 // restore the registers

 pop r14
 pop r13
 pop r9
 pop r8
 pop rsi
 pop rdi
 pop rdx
 pop rcx
 pop rbx
 pop rax
 pop rbp
 db $48
 db $cf
end;


procedure ATA1IrqHandler;[nostackframe];assembler;
asm
 // save registers
 push rbp
 push rax
 push rbx
 push rcx
 push rdx
 push rdi
 push rsi
 push r8
 push r9
 push r13
 push r14
 // protect the stack
 mov r15 , rsp
 mov rbp , r15
 sub r15 , 32
 mov  rsp , r15
 // set interruption
 sti
 mov rcx , 1
 // call handler
 Call ATAHandler
 mov rsp , rbp
 // restore the registers
 pop r14
 pop r13
 pop r9
 pop r8
 pop rsi
 pop rdi
 pop rdx
 pop rcx
 pop rbx
 pop rax
 pop rbp
 db $48
 db $cf
end;


