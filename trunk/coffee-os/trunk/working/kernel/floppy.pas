{
 Floppy unit.

Created from scratch from example sources on the net.

Thank Mike for some of this.Wouldn't have been able to do it without him.
I ported from his C.

We are ALMOST able to test this.
UNIT is NEARLY COMPLETE.

}

unit floppy;
interface

type
  Registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;


  PRegisters = ^TRegisters;
  TRegisters = record
    gs,fs,es,ds: LongWord;
    edi,esi,ebp,esp,ebx,edx,ecx,eax: LongWord;
    InterruptNumber,ErrorCode: LongWord;
    eip,cs,eflags,useresp,ss: LongWord;
  end;


const
  
	FDC_BASE=$3f0;
	FDC_IRQ=6;
	FDC_DMA=2;
	
//	Controller I/O Ports. 

	FLPYDSK_DOR		=	$3f2;
	FLPYDSK_MSR		=	$3f4;
	FLPYDSK_FIFO	=	$3f5;
	FLPYDSK_CTRL	=	$3f7;
FLPY_SECTORS_PER_TRACK=18; //maximum number of floppy sectors

//	Bits 0-4 of command byte.

	FDC_CMD_READ_TRACK	=	$02;
	FDC_CMD_SPECIFY		=	$03;
	FDC_CMD_CHECK_STAT	=	$04;
	FDC_CMD_WRITE_SECT	=	$05;
	FDC_CMD_READ_SECT	=	$06;
	FDC_CMD_CALIBRATE	=	$07;
	FDC_CMD_CHECK_INT	=	$08;
	FDC_CMD_FORMAT_TRACK=	$0d;
	FDC_CMD_SEEK		=	$0f;

	FDC_CMD_EXT_SKIP		=	$20;	//00100000
	FDC_CMD_EXT_DENSITY		=	$40;	//01000000
	FDC_CMD_EXT_MULTITRACK	=	$80;	//10000000

//	Digital Output Register

	FLPYDSK_DOR_MASK_DRIVE0			=	0;	//00000000	= here for completeness sake
	FLPYDSK_DOR_MASK_DRIVE1			=	1;	//00000001
	FLPYDSK_DOR_MASK_RESET			=	4;	//00000100
	FLPYDSK_DOR_MASK_DMA			=	8;	//00001000
	FLPYDSK_DOR_MASK_DRIVE0_MOTOR	=	16;	//00010000
	FLPYDSK_DOR_MASK_DRIVE1_MOTOR	=	32;	//00100000

//	Main Status Register

	FLPYDSK_MSR_MASK_DRIVE1_POS_MODE	=	1;	//00000001
	FLPYDSK_MSR_MASK_DRIVE2_POS_MODE	=	2;	//00000010
	FLPYDSK_MSR_MASK_BUSY				=	16;	//00010000
	FLPYDSK_MSR_MASK_DMA				=	32;	//00100000
	FLPYDSK_MSR_MASK_DATAIO				=	64; //01000000
	FLPYDSK_MSR_MASK_DATAREG			=	128;	//10000000

//	Controller Status Port 0

	FLPYDSK_ST0_MASK_DRIVE0		=	0;		//00000000	=	for completness sake
	FLPYDSK_ST0_MASK_DRIVE1		=	1;		//00000001
	FLPYDSK_ST0_MASK_HEADACTIVE	=	4;		//00000100
	FLPYDSK_ST0_MASK_NOTREADY	=	8;		//00001000
	FLPYDSK_ST0_MASK_UNITCHECK	=	16;		//00010000
	FLPYDSK_ST0_MASK_SEEKEND	=	32;		//00100000
	FLPYDSK_ST0_MASK_INTCODE	=	64;		//11000000

	FLPYDSK_ST0_TYP_NORMAL		=	0;
	FLPYDSK_ST0_TYP_ABNORMAL_ERR=	1;
	FLPYDSK_ST0_TYP_INVALID_ERR	=	2;
	FLPYDSK_ST0_TYP_NOTREADY	=	3;

//	GAP 3 sizes

	FLPYDSK_GAP3_LENGTH_STD = 42;
	FLPYDSK_GAP3_LENGTH_5_14= 32;
	FLPYDSK_GAP3_LENGTH_3_5= 27;

//	Formula: 2^sector_number * 128, where ^ denotes "to the power of"

	FLPYDSK_SECTOR_DTL_128	=	0;
	FLPYDSK_SECTOR_DTL_256	=	1;
	FLPYDSK_SECTOR_DTL_512	=	2;
	FLPYDSK_SECTOR_DTL_1024	=	4;

var
	DMAError:boolean;	


procedure FloppyHandler(var r: TRegisters);
procedure InitFloppyDMA;
procedure read_sector;
procedure write_sector; //USE CAUTION!
procedure waitFDCDone; 
procedure Drive_Spinup;
procedure flpydsk_write_dor (val:byte); 
function flpydsk_read_status:byte;
procedure flpydsk_wait_irq; 
procedure flpydsk_send_command (cmd:byte); 
function flpydsk_read_data:byte; 
function flpydsk_check_int:byte;
procedure flpydsk_control_motor (b:boolean); 
function detect_floppy:char;   
procedure flpydsk_read_sector_imp (head,track,sector:uint8); 
procedure lba_to_chs (lba:integer; head,track,sector:PtrUInt);
function flpydsk_seek (  cyl:uint32; head:uint32 ):integer;
function flpydsk_read_sector (sectorLBA:integer):byte; //shouldnt it be array of byte?
procedure write_sector(sector:integer; buffer:DMA_BUFFER);
procedure read_sect; 
procedure  flpydsk_write_ccr (val:uint8); 
procedure flpydsk_drive_data (stepr,loadt,unloadt: uint32; dma:boolean ); 
function  flpydsk_calibrate (drive:uint32):integer;
procedure flpydsk_disable_controller; 
procedure flpydsk_enable_controller; 
procedure flpydsk_reset;
procedure flpydsk_set_working_drive (drive:integer); 
function flpydsk_get_working_drive:integer;
procedure InstallFloppy;


implementation

uses
   x86,isr,irq,timer,video;
//irq

var
   fdcDone:boolean;
// current working drive. Defaults to 0 which should be fine on most systems, as we only have ONE these days.

	CurrentDrive:integer;
FloppyDiskIRQ:boolean;
DMA_BUFFER:byte;



procedure FloppyHandler(var r: TRegisters);

var
	s : string;
	waiting,DMAError:boolean;

begin

	fdcDone:=true;
 //   FloppyDiskIRQ := 1; //duplicate of above
// not supposed to do much, just allow other routines to check to see if transfer is complete.
   
   writeportb($20,$20); // end the interrupt. 
end;


procedure InitFloppyDMA; 
//Initialises the DMA controller.

begin
// Uses phys addr 1k-64k
	writeportb ($0a,$06);	//mask dma channel 2
	writeportb ($d8,$ff);	//reset master flip-flop
	writeportb ($04, $00);     //address=0x1000 
	writeportb ($04, $10);
	writeportb ($d8, $ff);  //reset master flip-flop
	writeportb ($05, $ff);  //count to 23ff (number of bytes in a 3.5" floppy disk track)
	writeportb ($05, $23);
	writeportb ($80, $00);     //external page register = 0
	writeportb ($0a, $02);  //unmask dma channel 2
end;


procedure waitFDCDone;  //in case of delay in drive access.
//waits as appropriate for drive to catch up, if busy or disk error.

begin
   if fdcDone then begin
	   asm
		hlt
	   end;
     	     fdcDone:=false;
	     exit;
   end;
end;

procedure DMARead; 
//Locks to current thread for buffered IO on DMA bus. (up to 64k at a time.)

begin
	spin_lock;
	writeportb ($0a, $06); //mask dma channel 2
	writeportb ($0b, $56); //single transfer, address increment, autoinit, read, channel 2
	writeportb ($0a, $02); //unmask dma channel 2
	spin_unlock;

end;

procedure DMAWrite; 
//Locks to current thread for bufferedIO on DMA bus, up to 64k at a time.
begin
	spin_lock;
	writeportb ($0a, $06); //mask dma channel 2
	writeportb ($0b, $5a); //single transfer, address increment, autoinit, write, channel 2
	writeportb ($0a, $02); //unmask dma channel 2
	spin_unlock;
end;


procedure Drive_Spinup;


begin
{
	
 A drive motor is activated by setting the corresponding field to 1, and disabled by setting it to 0. The dma field can be used to enable (1) or disable (0) the use of DMA. The FDCs in most PCs won't function without DMA, so the field should be set to 1. The rst field can be used to reset the controller; this is done by setting it to 0. Since we don't want to reset the FDC now, we will set it to 1. Finally, the last two bits select the disk drive. I don't know what effect that has, but it seems a good idea to select the disk drive whose motor you're activating. Finally, after instructing the FDC to spin up a drive, we need to wait a bit while it gets up to speed. The FDC does not seem to give any sign when this is done, so we'll just have to guess how long it takes. Half a second seems long enough.

}

//Spin up Floppy drive #1
asm
mov dx, FDC_DOR
mov al, 00011100b	// start motor 0, activate DMA/IRQ, no reset, drive 00
out dx, al
end;

delay(500); //Half a second spinup time.
asm
	pop ax
end;

end;


// write to the fdc dor
procedure flpydsk_write_dor (val:byte); 
begin
	// write the digital output register
	writeportb (FLPYDSK_DOR, val);
end;


// return fdc status
function flpydsk_read_status:byte;
begin

	// just return main status register
	flpydsk_read_status:=readportb (FLPYDSK_MSR);
end;

// wait for irq to fire
procedure flpydsk_wait_irq; 

//this is a polling loop and consumes valuable cpu power.

begin
	
	while (FloppyDiskIRQ=false ) do asm  //do nothing
 		nop
 	end;	
end;

//! send command byte to fdc
procedure flpydsk_send_command (cmd:byte); 

var
	i:integer;

begin
	i:=0;
	// wait until data register is ready. We send commands to the data register
	repeat	
		if ( flpydsk_read_status  and FLPYDSK_MSR_MASK_DATAREG )=0 then
			 writeportb (FLPYDSK_FIFO, cmd); //this never returns anything.
	inc(i);
	until i>500;
end;

//! get data from fdc
function flpydsk_read_data:byte; 

var
   i:integer;
begin

	//! same as above function but returns data register for reading
	i:=0;
	repeat
		if ( flpydsk_read_status and FLPYDSK_MSR_MASK_DATAREG )=0 then
			flpydsk_read_data:=readportb (FLPYDSK_FIFO);
		inc(i);
        until i>500;
	flpydsk_read_data:= 0;
end;




{
 * disk change.
 * This routine is responsible for maintaining the FD_DISK_CHANGE flag,
 * and the last_checked date.
 *
 * last_checked is the date of the last check which showed 'no disk change'
 * FD_DISK_CHANGE is set under two conditions:
 * 1. The floppy has been changed after some i/o to that floppy already
 *    took place.
 * 2. No floppy disk is in the drive. This is done in order to ensure that
 *    requests are quickly flushed in case there is no disk in the drive. It
 *    follows that FD_DISK_CHANGE can only be cleared if there is a disk in
 *    the drive.
 *
 * For 1., maxblock is observed. Maxblock is 0 if no i/o has taken place yet.
 * For 2., FD_DISK_NEWCHANGE is watched. FD_DISK_NEWCHANGE is cleared on
 *  each seek. If a disk is present, the disk change line should also be
 *  cleared on each seek. Thus, if FD_DISK_NEWCHANGE is clear, but the disk
 *  change line is set, this means either that no disk is in the drive, or
 *  that it has been removed since the last seek.
 *
 * This means that we really have a third possibility too:
 *  The floppy has been changed after the last seek.
 */

static int disk_change(int drive)
begin
	int fdc = FDC(drive);




	if (UDP->flags & FD_BROKEN_DCL)
		return UTESTF(FD_DISK_CHANGED);
	if ((fd_inb(FD_DIR) ^ UDP->flags) & 0x80) begin
		USETF(FD_VERIFY);	/* verify write protection */
		if (UDRS->maxblock) begin
			/* mark it changed */
			USETF(FD_DISK_CHANGED);
		end;

		/* invalidate its geometry */
		if (UDRS->keep_data >= 0) begin
			if ((UDP->flags & FTD_MSG) &&
			    current_type[drive] != NULL)
				DPRINT("Disk type is undefined after "
				       "disk change\n");
			current_type[drive] = NULL;
			floppy_sizes[TOMINOR(drive)] = MAX_DISK_SIZE << 1;
		end;

		return 1;
	end else begin
		UDRS->last_checked = jiffies;
		UCLEARF(FD_DISK_NEWCHANGE);
	end;
	return 0;
end;


/*
 * OK, this error interpreting routine is called after a
 * DMA read/write has succeeded
 * or failed, so we check the results, and copy any buffers.

 */
static int interpret_errors(void)
begin
	char bad;

	if (inr != 7) begin
		DPRINT("-- FDC reply error");
		FDCS->reset = 1;
		return 1;
	end;

	/* check IC to find cause of interrupt */
	switch (ST0 & ST0_INTR) begin
	case 0x40:		/* error occurred during command execution */
		if (ST1 & ST1_EOC)
			return 0;	/* occurs with pseudo-DMA */
		bad = 1;
		if (ST1 & ST1_WP) begin
			DPRINT("Drive is write protected\n");
			CLEARF(FD_DISK_WRITABLE);
			cont->done(0);
			bad = 2;
		end else if (ST1 & ST1_ND) begin
			SETF(FD_NEED_TWADDLE);
		end else if (ST1 & ST1_OR) begin
			if (DP->flags & FTD_MSG)
				DPRINT("Over/Underrun - retrying\n");
			bad = 0;
		end else if (*errors >= DP->max_errors.reporting) begin
			DPRINT("");
			if (ST0 & ST0_ECE) begin
				printk("Recalibrate failed!");
			end else if (ST2 & ST2_CRC) begin
				printk("data CRC error");
				tell_sector();
			end else if (ST1 & ST1_CRC) begin
				printk("CRC error");
				tell_sector();
			end else if ((ST1 & (ST1_MAM | ST1_ND))
				   || (ST2 & ST2_MAM)) begin
				if (!probing) begin
					printk("sector not found");
					tell_sector();
				end else
					printk("probe failed...");
			end else if (ST2 & ST2_WC) begin	/* seek error */
				printk("wrong cylinder");
			end else if (ST2 & ST2_BC) begin	/* cylinder marked as bad */
				printk("bad cylinder");
			end else begin
				printk
				    ("unknown error. ST[0..2] are: 0x%x 0x%x 0x%x",
				     ST0, ST1, ST2);
				tell_sector();
			end;
			printk("\n");
		end;
		if (ST2 & ST2_WC || ST2 & ST2_BC)
			/* wrong cylinder => recal */
			DRS->track = NEED_2_RECAL;
		return bad;
	case 0x80:		/* invalid command given */
		DPRINT("Invalid FDC command given!\n");
		cont->done(0);
		return 2;
	case 0xc0:
		DPRINT("Abnormal termination caused by polling\n");
		cont->error();
		return 2;
	default:		/* (0) Normal command termination */
		return 0;
	end;
end;


static void check_wp(void)
begin
	if (TESTF(FD_VERIFY)) begin
		/* check write protection */
		output_byte(FD_GETSTATUS);
		output_byte(UNIT(current_drive));
		if (result() != 1) begin
			FDCS->reset = 1;
			return;
		end;
		CLEARF(FD_VERIFY);
		CLEARF(FD_NEED_TWADDLE);
#ifdef DCL_DEBUG
		if (DP->flags & FD_DEBUG) begin
			DPRINT("checking whether disk is write protected\n");
			DPRINT("wp=%x\n", ST3 & 0x40);
		end;
#endif
		if (!(ST3 & 0x40))
			SETF(FD_DISK_WRITABLE);
		else
			CLEARF(FD_DISK_WRITABLE);
	end;
end;

#define CODE2SIZE (ssize = ((1 << SIZECODE) + 3) >> 2)
#define FM_MODE(x,y) ((y) & ~(((x)->rate & 0x80) >>1))
#define CT(x) ((x) | 0xc0)
static void setup_format_params(int track)
begin
	int n;
	int il;
	int count;
	int head_shift;
	int track_shift;
	struct fparm begin
		unsigned char track, head, sect, size;
	end; *here = (struct fparm *)floppy_track_buffer;

	raw_cmd = &default_raw_cmd;
	raw_cmd->track = track;

	raw_cmd->flags = FD_RAW_WRITE | FD_RAW_INTR | FD_RAW_SPIN |
	    FD_RAW_NEED_DISK | FD_RAW_NEED_SEEK;
	raw_cmd->rate = _floppy->rate & 0x43;
	raw_cmd->cmd_count = NR_F;
	COMMAND = FM_MODE(_floppy, FD_FORMAT);
	DR_SELECT = UNIT(current_drive) + PH_HEAD(_floppy, format_req.head);
	F_SIZECODE = FD_SIZECODE(_floppy);
	F_SECT_PER_TRACK = _floppy->sect << 2 >> F_SIZECODE;
	F_GAP = _floppy->fmt_gap;
	F_FILL = FD_FILL_BYTE;

	raw_cmd->kernel_data = floppy_track_buffer;
	raw_cmd->length = 4 * F_SECT_PER_TRACK;

	/* allow for about 30ms for data transport per track */
	head_shift = (F_SECT_PER_TRACK + 5) / 6;

	/* a ``cylinder'' is two tracks plus a little stepping time */
	track_shift = 2 * head_shift + 3;

	/* position of logical sector 1 on this track */
	n = (track_shift * format_req.track + head_shift * format_req.head)
	    % F_SECT_PER_TRACK;

	/* determine interleave */
	il = 1;
	if (_floppy->fmt_gap < 0x22)
		il++;

	/* initialize field */
	for (count = 0; count < F_SECT_PER_TRACK; ++count) begin
		here[count].track = format_req.track;
		here[count].head = format_req.head;
		here[count].sect = 0;
		here[count].size = F_SIZECODE;
	end;
	/* place logical sectors */
	for (count = 1; count <= F_SECT_PER_TRACK; ++count) begin
		here[n].sect = count;
		n = (n + il) % F_SECT_PER_TRACK;
		if (here[n].sect) begin	/* sector busy, find next free sector */
			++n;
			if (n >= F_SECT_PER_TRACK) begin
				n -= F_SECT_PER_TRACK;
				while (here[n].sect)
					++n;
			end;
		end;
	end;
	if (_floppy->stretch & FD_SECTBASEMASK) begin
		for (count = 0; count < F_SECT_PER_TRACK; count++)
			here[count].sect += FD_SECTBASE(_floppy) - 1;
	end;
end;

static void redo_format(void)
begin
	buffer_track = -1;
	setup_format_params(format_req.track << STRETCH(_floppy));
	floppy_start();
	debugt("queue format request");
end;

/*
 * Check if the disk has been changed or if a change has been faked.
 */

static int do_format(int drive, struct format_descr *tmp_format_req)
begin
	int ret;

	LOCK_FDC(drive, 1);
	set_floppy(drive);
	if (!_floppy ||
	    _floppy->track > DP->tracks ||
	    tmp_format_req->track >= _floppy->track ||
	    tmp_format_req->head >= _floppy->head ||
	    (_floppy->sect << 2) % (1 << FD_SIZECODE(_floppy)) ||
	    !_floppy->fmt_gap) begin
		process_fd_request();
		return -EINVAL;
	end;
	format_req = *tmp_format_req;
	format_errors = 0;
	cont = &format_cont;
	errors = &format_errors;
	IWAIT(redo_format);
	process_fd_request();
	return ret;
end;


/*
 * Move data from/to the track buffer to/from the buffer cache.
 */
static void copy_buffer(int ssize, int max_sector, int max_sector_2)
begin
	int remaining;		/* number of transferred 512-byte sectors */
	struct bio_vec *bv;
	char *buffer;
	char *dma_buffer;
	int size;
	struct req_iterator iter;

	max_sector = transfer_size(ssize,
				   min(max_sector, max_sector_2),
				   blk_rq_sectors(current_req));

	if (current_count_sectors <= 0 && CT(COMMAND) == FD_WRITE &&
	    buffer_max > fsector_t + blk_rq_sectors(current_req))
		current_count_sectors = min_t(int, buffer_max - fsector_t,
					      blk_rq_sectors(current_req));

	remaining = current_count_sectors << 9;
#ifdef FLOPPY_SANITY_CHECK
	if (remaining > blk_rq_bytes(current_req) && CT(COMMAND) == FD_WRITE) begin
		DPRINT("in copy buffer\n");
		printk("current_count_sectors=%ld\n", current_count_sectors);
		printk("remaining=%d\n", remaining >> 9);
		printk("current_req->nr_sectors=%u\n",
		       blk_rq_sectors(current_req));
		printk("current_req->current_nr_sectors=%u\n",
		       blk_rq_cur_sectors(current_req));
		printk("max_sector=%d\n", max_sector);
		printk("ssize=%d\n", ssize);
	end;
#endif

	buffer_max = max(max_sector, buffer_max);

	dma_buffer = floppy_track_buffer + ((fsector_t - buffer_min) << 9);

	size = blk_rq_cur_bytes(current_req);

	rq_for_each_segment(bv, current_req, iter) begin
		if (!remaining)
			break;

		size = bv->bv_len;
		SUPBOUND(size, remaining);

		buffer = page_address(bv->bv_page) + bv->bv_offset;
#ifdef FLOPPY_SANITY_CHECK
		if (dma_buffer + size >
		    floppy_track_buffer + (max_buffer_sectors << 10) ||
		    dma_buffer < floppy_track_buffer) begin
			DPRINT("buffer overrun in copy buffer %d\n",
			       (int)((floppy_track_buffer -
				      dma_buffer) >> 9));
			printk("fsector_t=%d buffer_min=%d\n",
			       fsector_t, buffer_min);
			printk("current_count_sectors=%ld\n",
			       current_count_sectors);
			if (CT(COMMAND) == FD_READ)
				printk("read\n");
			if (CT(COMMAND) == FD_WRITE)
				printk("write\n");
			break;
		end;
		if (((unsigned long)buffer) % 512)
			DPRINT("%p buffer not aligned\n", buffer);
#endif
		if (CT(COMMAND) == FD_READ)
			memcpy(buffer, dma_buffer, size);
		else
			memcpy(dma_buffer, buffer, size);

		remaining -= size;
		dma_buffer += size;
	end;
#ifdef FLOPPY_SANITY_CHECK
	if (remaining) begin
		if (remaining > 0)
			max_sector -= remaining >> 9;
		DPRINT("weirdness: remaining %d\n", remaining >> 9);
	end;
#endif
end;


static int check_floppy_change(struct gendisk *disk)
begin
	int drive = (long)disk->private_data;

	if (UTESTF(FD_DISK_CHANGED) || UTESTF(FD_VERIFY))
		return 1;

	if (time_after(jiffies, UDRS->last_checked + UDP->checkfreq)) begin
		lock_fdc(drive, 0);
		poll_drive(0, 0);
		process_fd_request();
	end;

	if (UTESTF(FD_DISK_CHANGED) ||
	    UTESTF(FD_VERIFY) ||
	    test_bit(drive, &fake_change) ||
	    (!ITYPE(UDRS->fd_device) && !current_type[drive]))
		return 1;
	return 0;
end;



}

// check interrupt status command
function flpydsk_check_int:byte;
var
  st0,cyl:UInt32; //longword
begin

	flpydsk_send_command (FDC_CMD_CHECK_INT);
	//st0: = pointer(flpydsk_read_data);
	//cyl: = pointer(flpydsk_read_data);
       // flpydsk_check_int:=;(result byte from drive)
end;

// turns the current floppy drives motor on/off
procedure flpydsk_control_motor (b:boolean); 
var
	motor:uint32;

begin

	//! sanity check: invalid drive
	if (CurrentDrive > 1) then
		exit;
	motor:=0;
	//! select the correct mask based on current drive
	if CurrentDrive=0 then begin
			motor := FLPYDSK_DOR_MASK_DRIVE0_MOTOR;	
		end;
	if CurrentDrive=1 then begin
			motor := FLPYDSK_DOR_MASK_DRIVE1_MOTOR;
	 	end;
	
	//! turn on or off the motor of that drive
	if (b) then
		flpydsk_write_dor (CurrentDrive or motor or FLPYDSK_DOR_MASK_RESET or FLPYDSK_DOR_MASK_DMA)
	else
		flpydsk_write_dor (FLPYDSK_DOR_MASK_RESET);

	// in all cases; wait a little bit for the motor to spin up/turn off
	delay (200);
end;

function detect_floppy:char;   
//returns 'a' or 'b'.This is from C.

var
 flags:longint;
 val:longword;
begin							
					
	spin_lock;		//locks the DMA channel to the thread.
	val := (readportb($10) shr 4) and 15;		
	spin_unlock;	
	detect_floppy:=chr(val); //====> 'A' or 'B' only.
							
end;

//! read a sector
procedure flpydsk_read_sector_imp (head,track,sector:uint8); 

var
	j:integer;
	st0,cyl:uint32;
	SectBeyondDisk:boolean;

begin

	//! set the DMA for read transfer
	DmaRead;

	//! read in a sector
	flpydsk_send_command (FDC_CMD_READ_SECT or FDC_CMD_EXT_MULTITRACK or FDC_CMD_EXT_SKIP or FDC_CMD_EXT_DENSITY);
	flpydsk_send_command ( head shl 2 or CurrentDrive );
	flpydsk_send_command ( track);
	flpydsk_send_command ( head);
	flpydsk_send_command ( sector);
	flpydsk_send_command ( FLPYDSK_SECTOR_DTL_512 );
//cant read beyond disk boundaries, must trip error if true. 
    if ( sector + 1  <= FLPY_SECTORS_PER_TRACK ) then SectBeyondDisk:=false else begin
    	SectBeyondDisk:=true;
    	exit;
    end;	
	flpydsk_send_command ( FLPYDSK_GAP3_LENGTH_3_5 );
	flpydsk_send_command ( $ff );

	//! wait for irq
	flpydsk_wait_irq;
	//! read status info
	j:=0;
	repeat
		flpydsk_read_data ();
        	inc(j);
	until j>7;
	//! let FDC know we handled interrupt
	flpydsk_check_int (st0,cyl);
end;

procedure lba_to_chs (lba:integer; head,track,sector:PtrUInt);

begin

   head := ( lba mod ( FLPY_SECTORS_PER_TRACK * 2 ) ) div ( FLPY_SECTORS_PER_TRACK );
   track := lba div ( FLPY_SECTORS_PER_TRACK * 2 );
   sector := lba mod FLPY_SECTORS_PER_TRACK + 1;
end;

// seek to given track/cylinder
function flpydsk_seek (  cyl:uint32; head:uint32 ):integer;

var
	st0,cyl0:uint32;
	i:integer;
begin

	if (CurrentDrive >= 2) then
		flpydsk_seek  :=-1;
	i:=0;
	repeat

		//! send the command
		flpydsk_send_command (FDC_CMD_SEEK);
		flpydsk_send_command ( (head) shl 2 or CurrentDrive);
		flpydsk_send_command (cyl);

		//! wait for the results phase IRQ
		flpydsk_wait_irq;
		flpydsk_check_int (st0,cyl0);

		//! found the cylinder?
		if ( cyl0 = cyl)then 
			flpydsk_seek :=0;
		inc(i);
	until i >10;

	flpydsk_seek :=-1;

end;

function flpydsk_read_sector (sectorLBA:integer):byte;

var
   head,track,sector:integer;


 begin

	if (CurrentDrive >= 2) then
		flpydsk_read_sector:= 0;

	//! convert LBA sector to CHS
	 
        head:=0;
        track:=0; 
        sector:=1;
	lba_to_chs (sectorLBA, head, track, sector);

	//! turn motor on and seek to track
	flpydsk_control_motor (true);
	if (flpydsk_seek (track, head) <> 0) then
		flpydsk_read_sector:= 0;

	//! read sector and turn motor off
	flpydsk_read_sector_imp (head, track, sector);
	flpydsk_control_motor (false);

	//! warning: this is a bit hackish
	flpydsk_read_sector:=  DMA_BUFFER;
end;

//USE this to demonstate ability to read/write the disk

procedure write_sector(sector:integer; buffer:DMA_BUFFER);
begin
//writes DATA to sector
end;

procedure read_sect; 

var
	i,c,j:integer;
	sectnum:longint;
	sectornumbuf:string [4];
	numstr:integer = 0;
 	ch:char;
	sector:UInt8; //Byte
begin
	

	writeln ('Please type in the sector number [0 is default] > ');
//	readline(line);  --working on reimplementation of this.
	sectornum = atoi (line);
        numstr:=num2str(sectornum); 
	writeln ('Sector  contents: ');
	//! read sector from disk
	sector := flpydsk_read_sector ( sectnum );
	//! display sector
	if (sectnum<> 0) then begin

		 i := 0;
		 c:=0;
		repeat
			j:=0;
			repeat
				write ('0x ');
				writelongln(sectnum);
				inc(j);
			until (j >128);			
			inc(i,128);

			writeln('Press any key to continue');
			ch:=readkey;
			inc(c);
		until (c>4);
	end
	else
		writeln ('*** Error reading sector from disk***');

	writeln ('Done.');
end;



{

	DMA Routines.
	The DMA (Direct Memory Access) controller allows the FDC to send data to the DMA,
	which can put the data in memory. While the FDC can be programmed to not use DMA,
  it is not very well supported on emulators or virtual machines. Because of this, we
  will be using the DMA for data transfers. 

}


//! write to the configuation control register
procedure  flpydsk_write_ccr (val:uint8); 
begin
	// write the configuation control
	writeportb (FLPYDSK_CTRL, val);
end;

//	Controller Command Routines

// configure drive
procedure flpydsk_drive_data (stepr,loadt,unloadt: uint32; dma:boolean ); 
var
	data:uint32=0;
    dmanum:integer;
begin
    if dma then dmanum:=1 else dmanum:=0;

	//! send command
	flpydsk_send_command (FDC_CMD_SPECIFY);
	data := ( (stepr and $0f) shl 4) or (unloadt and $0f);
		flpydsk_send_command (data);
	data := (((loadt) shl 1) or (not dmanum));  
		flpydsk_send_command (data);
end;


// calibrates the drive
function  flpydsk_calibrate (drive:uint32):integer;
var
	i:integer;
	st0,cyl:uint32;

begin

	if (drive >= 2) then
		flpydsk_calibrate:=-2;

	//! turn on the motor
	flpydsk_control_motor (true);
	i:=0;
	repeat
		//! send command
		flpydsk_send_command ( FDC_CMD_CALIBRATE );
		flpydsk_send_command ( drive );
		flpydsk_wait_irq;
		flpydsk_check_int ( st0, cyl);

		//! did we fine cylinder 0? if so, we are done
		if ( not cyl=0) then begin
			flpydsk_control_motor (false);
			flpydsk_calibrate:= 0;
		end;
		inc(i);
	until i>10;

	flpydsk_control_motor (false);
	flpydsk_calibrate:=-1;
end;

// disable controller

procedure flpydsk_disable_controller; 
begin

	flpydsk_write_dor (0);
end;

// enable controller
procedure flpydsk_enable_controller; 
begin

	flpydsk_write_dor ( FLPYDSK_DOR_MASK_RESET or FLPYDSK_DOR_MASK_DMA);
end;


//! reset controller
procedure flpydsk_reset;

var
	st0,cyl:uint32;
	i:integer;
 
begin

	//! reset the controller
	flpydsk_disable_controller;
	flpydsk_enable_controller;
	flpydsk_wait_irq;
	i:=0;
	//! send CHECK_INT/SENSE INTERRUPT command to all drives
	repeat
		flpydsk_check_int (st0,cyl);
		inc(i);
	until i>2;
	//! transfer speed 500kb/s
	flpydsk_write_ccr (0);

	//! pass mechanical drive info. steprate=3ms, unload time=240ms, load time=16ms
	flpydsk_drive_data (3,16,240,true);

	//! calibrate the disk
	flpydsk_calibrate ( CurrentDrive );
end;

//Wrapped functions:
// set current working drive
procedure flpydsk_set_working_drive (drive:integer); 
begin

	if (drive < 2) then
		CurrentDrive := drive;
end;

// get current working drive
function flpydsk_get_working_drive:integer;
begin

	flpydsk_get_working_drive:=CurrentDrive;
end;

procedure InstallFloppy;
begin
  DMAError:=false; //set to true if error, in another subroutine
  write('Installing Floppy...');
  tabulate;
  tabulate;
  InstallIRQHandler(6,@FloppyHandler); 
  InitFloppyDMA;
flpydsk_reset (); //only attempt to reset the disk.
// We can call other routines later on.
flpydsk_drive_data (13, 1, $0f, true);
  
  if DMAError=false then 
	  writeln('[ OK ]')
  else
	  writeln('[ FAILED ] Couldnt setup drive or DMA.');
	  writestrln('Possible Drive not Present.');
end;

{
A few untranslated routines left to implement:
WriteProtect (the notch on some disks)
Format
DiskChange
DiskIO(FD errors)

--see above for C for this..
function disk_change(drive:integer):boolean;

begin
end;

function FDErrors:integer;
end;

funtions CheckWriteProtect:boolean;
begin
end;

procedure Format; //init disk for use
begin
end;

}

begin
currentDrive:=0;
end.
