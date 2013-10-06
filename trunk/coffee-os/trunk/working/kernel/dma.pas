unit dma;
//Unit DMA for use in Drive access.
//Better we dont, this is for 16K/32K access for RM DOS. USe PCI DMA[packet modes] instead, this is MUCH
// MUCH faster and does not have these limits.
interface

uses 
    irq; //spinlocking to thread in question.
   	
const
// need to make some last minute adjustments, but otherwise ok.
// this unit is needed so the drives on the computer dont send spurious interrupts on every bit or byte read/written.
// instead, they use a circular FIFO with data that the DMA controller(here) is programmed with.

// see here: http://www.isdaman.com/alsos/hardware/dma/howdma.htm

    CHANNEL0 = 0 ;
    CHANNEL1 = 1 ;
    CHANNEL2 = 2 ;
    CHANNEL3 = 3 ;
    
    VERIF_TRANS   = 0 ;
    WRITE_TRANS   = 1 ;
    READ_TRANS    = 2 ;
    CASCADE_TRANS = 3 ; 
    DEMAND_MODE  = 0 ;
    SINGLE_MODE  = 1 ;
    BLOCK_MODE   = 2 ;
    CASCADE_MODE = 3 ;
    
    DMA_MASK  : array[0..7] of byte = ($0a, $0a, $0a, $0a, $d4, $d4, $d4, $d4) ;
    DMA_MODE  : array[0..7] of byte = ($0b, $0b, $0b, $0b, $d6, $d6, $d6, $d6) ;  
    DMA_CLEAR : array[0..7] of byte = ($0c, $0c, $0c, $0c, $d8, $d8, $d8, $d8) ;

    DMA_PAGE  : array[0..7] of byte = ($87, $83, $81, $82, $8f, $8b, $89, $8a) ;
    DMA_ADDR  : array[0..7] of byte = ($00, $02, $04, $06, $c0, $c4, $c8, $cc) ;
    DMA_COUNT : array[0..7] of byte = ($01, $03, $05, $07, $c2, $c6, $ca, $ce) ;
//(DMALENGTH)

procedure SetupDMARead(chan,length:cardinal; buffer:pointer);
procedure SetupDMAWrite(chan,length:cardinal; buffer:pointer);
function getdmaremaining(chan:cardinal):cardinal;
procedure enabledma(chan:cardinal);
procedure disabledma(chan:cardinal);

procedure dma_setup(canal : byte; buffer : dword; length : word; auto_init : boolean; addr_inc : boolean) ; [public, alias : 'DMA_SETUP'];

{used by DMASetup

procedure dma_send_lenght(canal : byte; length : dword) ;
procedure dma_send_page(canal : byte; page : byte) ;
procedure dma_send_offset(canal : byte; offset : dword) ;

procedure dma_stop(canal : byte) ; [public, alias : 'DMA_STOP'];
procedure dma_resume(canal : byte) ; [public, alias : 'DMA_UNPAUSE'];
procedure dma_pause(chan: cardnal) ; [public, alias : 'DMA_PAUSE'];
}

implementation

var
        dmainuse:array [0..7] of boolean;
        lock:boolean;                       // The lock variable. 1 = locked, 0 = unlocked.

{$ASMMODE intel}

Function SubPtr(p: Pointer; d: LongInt): Pointer;
Begin
  SubPtr := Long2Ptr(Ptr2Long(p)-d);
End;

Function AddPtr(p: Pointer; d: LongInt): Pointer;
Begin
  AddPtr := Long2Ptr(Ptr2Long(p)+d);
End; 


procedure testandset(var data:boolean);
begin
        data:=true;
end;

procedure testandreset(var data:boolean);
begin
        data:=false;
end;

procedure enabledma(chan:cardinal);
begin
        if chan<4 then begin
                writeportb(chan,DMAMaskL);
        end else if ((chan>3) and (chan<8)) then begin
                writeportb(chan-4,DMAMaskH);
        end;
end;
procedure disabledma(chan:cardinal);
begin
        if chan<4 then begin
                writeportb(4+chan,DMAMaskL);
        end else if ((chan>3) and (chan<8)) then begin
                writeportb(chan,DMAMaskH);
        end;
end;

function getdmaremaining(chan:cardinal):cardinal;
var
    count:word;
    temp:longint;
    
begin
        count:=0;
        if chan<7 then begin
                disabledma(chan);
                temp:=DMALen[chan]+1;
                count:=readportb(temp);
                temp:=DMALen[chan]<<8;
                inc(count,(readportb(temp)));   //count:=count+;
                if count<>0 then enabledma(chan) else testandreset(dmainuse[chan]);
        end;
        getdmaremaining:=count;
end;

{These 2 routines program the dma controler ready for dma
both will lock and yeild in the testandset routine if the channel is unavailable
and will stay yeilded until the channel becomes available again

The routines on the bottom vary the bits sent by channel used.
The ones on the top hard key them.

useage:

DMA is usually used for disk IO
buffer:array [1..512] of byte
buffer2:array [1..1024] of byte;//newer HDs as of late  --see my code IS modular...
BigBuffer:array[1..2048] of byte;

//going any bigger can interfere with cdrom access (2048 bytes per sector)

//For Floppy. Channel number IS important.

EnableDMA;
SetupDMARead(5,512,pointer(buffer))
ATASendCommand(ReadSector,Controller); //for HD anyway...

DisableDMA;  //dont leave the process locked to the drive, other ones might want to use it.


}
procedure SetupDMARead(chan,length:cardinal; buffer:pointer);
var
        ad,page:cardinal;
begin
        if chan<4 then begin
                ad:=ptr2long(buffer);
                page:=(ad shr 16) and $ff;
                ad:=ad and $ffff;
        end else begin
                ad:=ptr2long(buffer);
                page:=(ad shr 16) and $fe;
                ad:=(ad shr 1) and $ffff
        end;
        if ((chan>7) or (chan=4)) then begin
                writeln('Cannot do DMA on a channel that doesnt exist');
                
                exit;
        end else begin
                testandset(dmainuse[chan]);
                disabledma(chan);
                if chan<4 then begin
                        writeportb(0,DMAClearL);
                        writeportb($44,DMAModeL);
                        writeportb(ad and $ff,DMAaddr[chan]);
                        writeportb((ad and $ff00) div $100, DMAaddr[chan]);
                        writeportb(length and $ff,DMAlen[chan]);
                        writeportb((length shr 8) and $ff , DMAlen[chan]);
                        writeportb(page and $ff, DMAPage[chan]);
                end else begin
                        writeportb(0,DMAClearH);
                        writeportb($44,DMAModeH);
                        writeportb(ad and $ff,DMAaddr[chan]);
                        writeportb((ad and $ff00) div $100, DMAaddr[chan]);
                        writeportb((length shr 1) and $ff,DMAlen[chan]);
                        writeportb((length shr 9) and $ff, DMAlen[chan]);
                        writeportb(page and $ff, DMAPage[chan]);

                end;
        end;
end;

procedure SetupDMAWrite(chan,length:cardinal; buffer:pointer);
var
        ad,page:cardinal;
begin
        if chan<4 then begin
                ad:=ptr2long(buffer);
                page:=(ad shr 16) and $ff;
                ad:=ad and $ffff;
        end else begin
                ad:=ptr2long(buffer);
                page:=(ad shr 16) and $fe;
                ad:=(ad shr 1) and $ffff
        end;
        if ((chan>7) or (chan=4)) then begin
                writeln('Cannot do DMA on a channel that doesnt exist');
                
                exit;
        end else begin
                testandset(dmainuse[chan]);
                disabledma(chan);
                if chan<4 then begin
                        writeportb(0,DMAClearL);
                        writeportb($48,DMAModeL);
                        writeportb(ad and $ff,DMAaddr[chan]);
                        writeportb((ad and $ff00) div $100, DMAaddr[chan]);
                        writeportb(length and $ff,DMAlen[chan]);
                        writeportb((length shr 8) and $ff , DMAlen[chan]);
                        writeportb(page and $ff, DMAPage[chan]);
                end else begin
                        writeportb(0,DMAClearH);
                        writeportb($48,DMAModeH);
                        writeportb(ad and $ff,DMAaddr[chan]);
                        writeportb((ad and $ff00) div $100, DMAaddr[chan]);
                        writeportb((length shr 1) and $ff,DMAlen[chan]);
                        writeportb((length shr 9) and $ff, DMAlen[chan]);
                        writeportb(page and $ff, DMAPage[chan]);

                end;
        end;
end;

//these from DaphineOS - GPLv2

procedure dma_pause(chan: cardnal) ; [public, alias : 'DMA_PAUSE'];
begin
    writeportb(chan, ((chan mod 4) shr 6) or (1 shr 5)) ;
end ;

procedure dma_unpause(canal : byte) ; [public, alias : 'DMA_UNPAUSE'];
begin
    writeportb(DMA_MASK[canal], ((canal mod 4) shr 6)) ;
end ;

procedure dma_stop(canal : byte) ; [public, alias : 'DMA_STOP'];
begin
    dma_pause(canal) ;
    writeportb(DMA_CLEAR[canal], 0) ;
    dma_unpause(canal) ;
end ;

procedure dma_send_offset(canal : byte; offset : dword) ;
begin
    writeportb(DMA_ADDR[canal], offset and $ff) ;
    writeportb(DMA_ADDR[canal], (offset and $ff00) shl 8) ;
end ;

procedure dma_send_page(canal : byte; page : byte) ;
begin
    writeportb(DMA_PAGE[canal], page) ;
end;

procedure dma_send_lenght(canal : byte; length : dword) ;
begin
    writeportb(DMA_COUNT[canal], length and $ff) ;
    writeportb(DMA_COUNT[canal], (length and $ff00) shl 8) ;
end;

procedure dma_setup(canal : byte; buffer : dword; length : word; auto_init : boolean; addr_inc : boolean) ; [public, alias : 'DMA_SETUP'];

var
    offset, page : word ;
    config       : byte ;
        i:cardinal;

begin

        for i:=0 to 7 do dmainuse[i]:=false;
        dmainuse[4]:=true;//cascade channel

    offset := word(buffer and $FFFF) ;
    page   := word(buffer shl 16) ;
    config := 0 ;
    config := (canal mod 4) shr 6 ;
    config := config or (transfert shr 4) ;

    if (auto_init = true) then
        config := config or (1 shr 3) ;

    if (addr_inc = true) then
        config := config or (1 shr 2) ;

    config := config or mode ;

    asm
        pushfd
        cli
    end ;

    dma_pause(canal) ;
    writeportb(DMA_CLEAR[canal], 0) ;
    writeportb(DMA_MODE[canal], config) ;
    dma_send_offset(canal, offset) ;
    dma_send_page(canal, page) ;
{   dma_send_lenght(canal, length) ;}
    dma_unpause(canal) ;

    asm
        popfd
    end;
end;


end.
