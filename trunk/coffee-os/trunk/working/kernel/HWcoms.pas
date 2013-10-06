{
Hardware Ports unit(Comms mostly).
	
Equivalent to PortIO in C.

UNIX, just wraps hardware calls.UNIX/Linux DOES do raw hardware access.It just doesn't LOOK like it does.
this is ring0 code. I haven't coded ring3 yet.

A LOT depends on THIS unit.

This unit rewritten 3 times from scratch before I got it right. Linux kernel doesn't even have this code,let alone in
 an easy to read format. Same goes for much of the kernel sources....


COM port help from Oberon/BluebottleOS and the following author:

Kevin R. Bulgrien  
(c) Copyright October 1989
Code Abandoned, but originally written for TP3-5.

References:
			Serial and UART Tutorial by Frank Durda
			"http://freebsd.org/doc/en_US.ISO8859-1/articles/serial-uart"
			"http://www.lammertbies.nl/comm/info/RS-232_uart.html"
			"http://docs.kde.org/stable/en/kdenetwork/kppp/appendix-hayes-commands.html" (modems)
			"http://www.marshallsoft.com/serial.htm" (also includes modems)
				
and SERIAL PORT PROGRAMMING(in Docs folder)

USE EITHER port (1 or 3 ) and port (2 or 4), as they use the same IRQ.DO NOT USE BOTH at same time. It wont work.

	

8250 [the OLD OLD COMM port processor] REGISTER LISTING
(even 16550s use these instructions, and with some **slight** modifications we can send/recieve MULTIPLE bytes at a time, greatly improving both throughput and speed and lessening impact on CPU.)
                           
                           
 To write to an 8250 register, you write to the base address of the chip
 plus an offset. The base address is 2e8h for COM1 and 3e8h for COM2.
 Register 0:     RHR (Receive Holding Register; Receive Buffer
                 Register in some literature).  Doubles as the THR
                 (Transmitter Holding Register).  Is also the LSB
                 of the DLR (Divisor Latch Register) occasionally;
                 don't worry about that yet, but remember it.

 Purpose:        This register is where you both read and write data
                 for the serial port.

 Bits:           Bits 0-4 contain data bits 0-4.
                 Bits 5-7 may or may not be defined, depending upon
                 whether the UART has been instructed to use 5, 6, 7,
                 or 8 bit words.
 Register 1:     IER (Interrupt Enable Register).  Also the MSB of
                 the DLR (Divisor Latch Register) occasionally; don't
                 worry about that yet, but remember it.

 Purpose:        Tells the UART to generate an interrupt when different
                 things occur.

 Bits:           Bit 0: RHRI (Receive Holding Register Interrupt; RxRDY
                 in some literature).  The UART will generate an
                 interrupt when a character is received in the RHR if
                 this bit is set.
                 Bit 1: THRI (Transmit Holding Register Interrupt; TxRDY).
                 If set, the UART generates an interrupt when a
                 character is moved from the THR to the Internal Shift
                 Register.
                 Bit 2: RLSI (Receive Line Status Interrupt; ERROR).
                 If set, the UART interrupts when a parity or overrun
                 error occurs, or when a break condition is encountered.
                 Bit 3: MSI (Modem Status Interrupt; DELTA).  If set,
                 the UART interrupts whenever an RS-232 line changes
                 state.
                 Bits 4-7: Unused

 Register 2:     ISR (Interrupt Status Register; also refered to as
                 the Interrupt Identification Register).

 Purpose:        Tells what event caused a UART interrupt.

 Bits:           Bit 0: Flags if an interrupt has occurred
                 Bits 1-2: Indicates what caused interrupt:
                           00 -> RS-232 line change
                           01 -> THR emptied
                           10 -> RHR contains character
                           11 -> Error condition
                 Bits 3-7: Unused

 Register 3:     LCR (Line Control Register).

 Purpose:        Configures the UART.  Also  flags the use of
                 registers 0 and 1 for the DLR (Divisor Latch
                 Register).  More about that shortly.

 Bits:           Bits 0-1: Sets the number of data bits in a
                 serial word:
                           00 -> 5-bit data
                           01 -> 6-bit data
                           10 -> 7-bit data
                           11 -> 8-bit data
                 Bit 2: Stop bits; 0 flags 1 stop bit per word,
                 1 flags 2 stop bits per word.
                 Bits 3-5: Sets the parity
                          000 -> No parity
                          001 -> Odd
                          011 -> Even
                          101 -> Mark
                          111 -> Space
                 Bit 6: Break control; sends the receiver a break
                 condition.
                 Bit 7: DLR access enable; if set, registers 0
                 and 1 become one big word register (the DLR)
                 that stores the baud rate divisor for calculating
                 the baud rate of the UART.

 Register 4:     MCR (Modem Control Register).

 Purpose:        Controls the lines on the RS-232 interface.

 Bits:           Bit 0: Is reflected on RS-232 DTR (Data
                 Terminal Ready) line.
                 Bit 1: Reflected on RS-232 RTS (Request to
                 Send) line.
                 Bit 2: GPO1 (General Purpose Output 1).
                 Bit 3: GPO2 (General Purpose Output 2).
                 Enables interrupts to be sent from the UART
                 to the PIC.
                 Bit 4: Echo (loop back) test.  All characters
                 sent will be echoed if set.
                 Bits 5-7: Unused.

 Register 5:     LSR (Line Status Register).

 Purpose:        Stores general status information about the UART.

 Bits:           Bit 0: Set if RHR contains a character (called
                 RxRDY or RDR, depending on literature).
                 Bit 1: Overrun error (character overwrote the last
                 in the RHR)
                 Bit 2: Parity error
                 Bit 3: Framing error (stop bit was set to 0 instead
                 of 1).
                 Bit 4: Break condition
                 Bit 5: THE (or TBE).  Transmit Buffer Empty.  If
                 set, the UART sent data from the THR to the OSR
                 (Output Shift Register) and data can be safely
                 written without overwriting anything.
                 Bit 6: Transmitter empty; both the THR and shift
                 register are empty if this is set.
                 Bit 7: Unused on the 8250.

 Register 6:     MSR (Modem Status Register).

 Purpose:        Displays the status of the modem control lines.
                 After bits 0-3 are read they are reset.

 Bits:           Bit 0: CTS (Clear To Send) line has changed
                 (since last read of MSR).
                 Bit 1: DSR (Data Set Ready) has changed.
                 Bit 2: RI (Ring Indicator) has been set since
                 the last time the MSR was read.
                 Bit 3: CD (Carrier Detect) has changed.
                 Bit 4: Value of CTS
                 Bit 5: Value of DSR
                 Bit 6: Value of RI
                 Bit 7: Value of CD

 Register 7:     SPR (Scratch Pad Register)

 Purpose:        Just what the name implies; a scratch pad, for
                 nothing.

 Bits:           Bit 0-7: As you will


Before setting up any interrupts, you must set bit 3 of the MCR (UART
register 4) to 1. This toggles the GPO2, which puts the UART out of
tri-state, and allows it to service interrupts. [don't ask; I didn't design
the thing] Then, set the bits of the IER (register 1) that represent the
interrupts you want the UART to generate.

Next, you set up the (A) PIC (Programmable Interrupt Controller) to allow
interrupts from the COM ports.  COM1 and COM3 are on IRQ4 and COM2 and COM4 are on
IRQ3, so you enable the interrupt by zeroing the 3rd (IRQ3) or 4th (IRQ4)
bit of the PIC's Interrupt Mask Register (be sure to keep the other bits
intact!).

 Your ISR should check the LSR (register 5)[COM1 and 5, etc..] to determine what caused the interrupt, then handle
it. 

(case (COM1 and 5[$3FD]) of)

Sending characters:

To send a character out the serial port, you write it to the THR (register
0). What if there's another character in the THR waiting to be sent? On the
8250, it'll be overwritten. However, you can wait for it to be sent. Test
bit 3 of the LSR to determine if the last character in the THR was shifted
out of the buffer before writing a character. 


To determine if a character is in the THR, you read bit 0 of the LSR. When
this bit is set, a character is in the THR, and you can retrieve it by
reading register 0 of the UART.

 **NEEDS TESTING** AND  nearly(90%) complete.
 (Grab another PC and open a HyperTerm/Minicom session or use bochs/Qemu/VBox)
 
 COM Port does not support higher than 115200.
 A data port such as USB, though, might.

 --Jazz }	


unit HWcoms;

interface
uses isr;

type
	str80= string[80];

	
  Registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;
 
  PRegisters = ^TRegisters;
{
  TRegisters = record
    gs,fs,es,ds: LongWord;
    edi,esi,ebp,esp,ebx,edx,ecx,eax: LongWord;
    InterruptNumber,ErrorCode: LongWord;
    eip,cs,eflags,useresp,ss: LongWord;
  end;
}
  Tbuffer=array[1..1025] of char;
  Pbuffer=^Tbuffer;
{
	
  1.5 stop bits are used when StopBits = 2 AND DataBits = 5, otherwise StopBits will set the     
 indicated number of stop bits in the range 1 to 2. 
 
  DataBits may be set with 5 to 8 for the number of data  bits to use.  
  
  Mark parity means that the parity bit is always set to 0. 
  Space parity means that the parity  bit is always set to 1.  
  

}	  
  
// Here for completeness sake  
//ParityType(4) gives the E in 8E1

 ParityType = (None, Odd, Even,  Mark, Space); //Used, but not much.
  FlowType = (No, RtsCts, XonXoff); //Used during XMIT/RECV
   LengthType = (D5, D6, D7, D8); //7 and 8 are ASCII, 5 and 6 are boudot(HAMS mostly use these)
    StopType = (S1, S2);
    BaudType = (B110,B300,B350,B600,B1200,B2400,B4800,B9600,B19200,B38400,B57600, B115200);


var
  connected,method,IOerror:boolean; //medthod is for I/O.shouldnt be using it though.
  Latch_Low,Latch_High:byte;
  CTS, DSR, RI, CD : ARRAY [1..4] OF BOOLEAN;  //--might not work, but this is set below.
  //needs to write back this data to regs....(TODO)
  DTR_RTS : ARRAY [1..4] OF BOOLEAN;   
    ComFlowControl : array [1 .. 4] of FlowType;
    ComFlowHalted : array [1 .. 4] of boolean;
    ComXoffReceived : array [1 .. 4] of boolean;
  //Length type bits
   Parity,Framing,Overrun,BreakInt:array[1..4] of longint;
  //Timeout,THR,IMR need be set	
  ComInUse,ComInUse2,ComInUse3,ComInUse4:boolean;                          
  Temporary : BYTE;  
  
const
   buffersize=1025; //remember to change the array size as well when changing this.
   //should be 1.2*1024*1024*1024 byte/chars on USB 1.0 and (1.2MB)
   // 12*1024*1024*1024 byte/chars on USB1.0 Hi-Speed and (12MB)
   // 120*1024*1024*1024 byte/chars on USB2.0/FW and (120MB)
   //  300*1024*1024*1024 on USB 3.0 (still experimental) (300MB)
   // --IIRC.
   COM1=$3f8;
   COM2=$2f8;
//Usually end here.   
   COM3=$3E8;
   COM4=$2e8;
   
   LPT1=$378;
//from 8088 Bios sources
   LPT2=$278;
   LPT3=$3BC;
   //LPT2 (and 3) Ignored, not normally used.

//COM port flow control
{$warning No way to use COM FLOW Yet.}
   XON_CH     = #11;   { XON protocol control character.    } 
   XOFF_CH    = #13;   { XOFF protocol control character.   } 
   EOT        = #04;   { End of transmission character.     }
  Word5 = 0;
  Word6 = 1;
  Word7 = 2;
  Word8 = 3;
 //Stop Bits
  Bit1 = 0;
  Bit2 = 2;
  
   

	// Port registers --add this to base port address to get resulting address.
	// all numbers in HEX.
	
	// RBR = 0;	 Select with DLAB = 0 - Receive Buffer Register - read only
//				 Select with DLAB = 1 - Baud Rate Divisor LSB 
    				 
	IER = 1;	// Select with DLAB = 0 - Interrupt Enable Register -  R/W
				// Select with DLAB = 1 - Baud Rate Divisor MSB 
	IIR = 2;	// Interrupt Identification Register - read only 
	FCR = 2;	// 16550 FIFO Control Register write only 
	LCR = 3;	// Line Control Register -  R/W 
{
Line Control Register (LCR):

Bit 7
	1 Divisor Latch Access Bit(DLAB)
        0 Access to Receiver buffer, Transmitter buffer & Interrupt Enable Register
Bit 6
	Set Break Enable
	
Bit 5		Bit 4		Bit 3 		Parity Select
0		0		0		No Parity
0		0		1		Odd Parity
0		1		1		Even Parity
1		0		1		High Parity (Sticky)
1		1		1		Low Parity (Sticky)

Bit 2
0	One Stop Bit
1	2 Stop bits for words of length 6,7 or 8 bits or 1.5 Stop Bits for Word lengths of 5 bits.

Bit 1		Bit 0		Word Length
0		0		5 Bits
0		1		6 Bits
1		0		7 Bits
1		1		8 Bits 
}
	
	
	MCR = 4;	// Modem Control Register -  R/W 
	LSR = 5;	// Line Status Register -  read only
	MSR = 6;	// Modem Status Register - R/W 
	SCR = 7;	// Scratch Register - R/W 

	// Modem control lines 
	DTR = 0;
	RTS = 1;	// output 
	BreakIO = 2;	// input/output - Bit 6 in LCR 
 
	DCD = 6;	// input 


//LCR-related constants(this is what the value is, but they are hard to read.)
PARITY_NONE =    0;
PARITY_ODD  =    8;
PARITY_EVEN  =   24;
PARITY_MARK   =  20;
PARITY_SPACE  =  28;

STOP_ONE      =  0;
STOP_TWO      =  4;
BITS_5        =  0;
BITS_6        =  1;
BITS_7        =  2;
BITS_8        =  3;
DLR_ON        =  128;


//This is the bochs_debugger Console output port.
// KNOWN to work on VBox, Qemu AND Bochs. On real HW, this has no effect, as port $e9 is not mapped.
// If you need a debugger console bad enough, use the comm port and connect via Terminal software.
// I have no clue how to actually attach that way, I haven't done it in some time.  --Jazz


//write to Debugging Console Output

procedure bochs_debug(ch:char);
procedure bochs_debugline(line:str80); //(wrapped routine for whole lines) 

PROCEDURE Set_DTR_RTS (port:integer; Status : BOOLEAN);
procedure SetPortValues(NewRate: longint;  databits:integer; newparity:char; stopbits:integer);
function RingDetect:BOOLEAN;
function GetParity:ParityType;
function GetBaudRate(port:integer):LongInt;
function GetStopBits(port:integer):integer;
function GetDataBits(port:integer):integer;

procedure InitCom(port,baud,DataBits,StopBits:integer; Parity:parityType);
procedure closeCom(port:integer);
procedure BaudChanged(OldSpeed,NewSpeed,port:integer);
function IdentifyUART: String;

//only trip interrupt on READ.
function ReadCom(port:integer):Pbuffer; //functions have to use pointers.
procedure writeCom(port:integer;  var buffer2:Tbuffer); //need the 'var' here.its reqd.
procedure InstallPorts;
procedure writeCOMln(port:integer; line:str80);
procedure Coms(var r: TRegisters); 


implementation

uses
//keyboard unit required for readkey function.
  x86,keybrd,console,uconsole,timer; 

// {$i modemcmd.inc} Hayes modem commands
//  {$i lprcmd.inc}  Generic/Specific Printer Character commands


procedure bochs_debug(ch:char);

begin
   writeportb($e9,ord(ch));
end;

procedure bochs_debugline(line:str80); [public, alias : 'Bochs_debugline'];
//should work with pchar no problem
var
   s:integer;

begin
   for s:=1 to length(line) do
       writeportb($e9,ord(line[s])); //should do the trick.       
end;

                                               
procedure writeCom(port:integer; var buffer2:Tbuffer);


var
   ready,timeout:boolean;
   timeloop:integer;
   ch:char;

// The following code writes the current character directly to the serial port
// because the 8250 is not transmitting anything now and we will never again
// get a transmit holding register empty interrupt (at least, not until we
// write data directly to the port). We use FIFOs so we dont deal directly with the RS232 but the 8250/16550 
// UART controller instead.
//Same process with IDE/SATA Drives.

begin
    timeout:=false;
	case port of

    1:begin
	    x:=1;
	    repeat
       		if ctrlbreak=true then begin
       		    IOError:=true;
       			exit; //User interrupted.
       		end;	
       		while not Timeout do begin
       			repeat	
			        Ready := (readPORTb(Com1 and LSR) AND $20 <> 0); //readportb()                      
			        INC (TimeLoop);
			        delay(10); //1 ms seems a bit short.                                                     
			    UNTIL Ready OR (TimeLoop = 65535);         
		        IF Ready =true   THEN BEGIN                                              		              
		              writePORTb ((COM1 and LCR),(readPORTb (COM1 and LCR) AND $7F));    {-Allow THR,RHR & IER access }
		              writePORTb ((COM1 and 0), ( ORD (buffer[x])) );                {-Put the data to send in    }
		              if keypressed then begin
               			ch:=char(readkey);
               			writePortb((Com1 and 0),(ORD (buffer[x])));	//write BYTE to comm port, not CHAR to comm port.
               			writechar(ch); //Update screen		 
       			      end;	
       			                  	
       			      inc(x);
       			      
	             END;                                                             
	                
       		  end;
       		   //not ready or timer expired for waiting.
	                 TimeOut := TRUE; 
       		 		 IOerror:=true;
			         CloseCom(1);
       		                 			     
	  	until  x=1024;
   		if x=1024 then	x:=0;
    end;

    2:begin
	     x:=1;
	    repeat
       		if ctrlbreak=true then begin
       		    IOError:=true;
       			exit; //User interrupted.
       		end;	
       		while not Timeout do begin
       			repeat	
			         Ready := (readPORTb(Com2 and LSR) AND $20 <> 0); //readportb()                       
			        INC (TimeLoop);
			        delay(10); //1 ms seems a bit short.                                                     
			    UNTIL Ready OR (TimeLoop = 65535);         
		        IF Ready =true   THEN BEGIN                                              		              
		              writePORTb ((COM2 and LCR),(readPORTb (COM2 and LCR) AND $7F));    {-Allow THR,RHR & IER access }
		              writePORTb ((COM2 and 0), ( ORD (buffer[x])) );                {-Put the data to send in    }
		              if keypressed then begin
               			ch:=char(readkey);
               			writePortb((Com2 and 0),(ORD (buffer[x])) );	//write BYTE to comm port, not CHAR to comm port.
               			writechar(ch); //Update screen		 
       			      end;	
       			                  	
       			      inc(x);
       			      
	             END;                                                             
	                
       		  end;
       		   //not ready or timer expired for waiting.
	                 TimeOut := TRUE; 
       		 		 IOerror:=true;
			         CloseCom(2);
       		                 			     
	  	until  x=1024;
   		if x=1024 then	x:=0;
   		end;

    3:begin
	      x:=1;
	    repeat
       		if ctrlbreak=true then begin
       		    IOError:=true;
       			exit; //User interrupted.
       		end;	
       		while not Timeout do begin
       			repeat	
			        Ready := (readPORTb(Com3 and LSR) AND $20 <> 0); //readportb()                     
			        INC (TimeLoop);
			        delay(10); //1 ms seems a bit short.                                                     
			    UNTIL Ready OR (TimeLoop = 65535);         
		        IF Ready =true   THEN BEGIN                                              		              
		              writePORTb ((COM3 and LCR),(readPORTb (COM3 and LCR) AND $7F) );    {-Allow THR,RHR & IER access }
		              writePORTb ((COM3 and 0), ( ORD (buffer[x])) );                {-Put the data to send in    }
		              if keypressed then begin
               			ch:=char(readkey);
               			writePortb((Com3 and 0),(ORD (buffer[x])) );	//write BYTE to comm port, not CHAR to comm port.
               			writechar(ch); //Update screen		 
       			      end;	
       			                  	
       			      inc(x);
       			      
	             END;                                                             
	                
       		  end;
       		   //not ready or timer expired for waiting.
	                 TimeOut := TRUE; 
       		 		 IOerror:=true;
			         CloseCom(3);
       		                 			     
	  	until  x=1024;
   		if x=1024 then	x:=0;
    end;

    4:begin
	      x:=1;
	    repeat
       		if ctrlbreak=true then begin
       		    IOError:=true;
       			exit; //User interrupted.
       		end;	
       		while not Timeout do begin
       			repeat	
			        Ready := (readPORTb(Com4 and LSR) AND $20 <> 0); //readportb()                        
			        INC (TimeLoop);
			        delay(10); //1 ms seems a bit short.                                                     
			    UNTIL Ready OR (TimeLoop = 65535);         
		        IF Ready =true   THEN BEGIN                                              		              
		              writePORTb ((COM4 and LCR),(readPORTb (COM4 and LCR) AND $7F));    {-Allow THR,RHR & IER access }
		              writePORTb ((COM4 and 0), ( ORD (buffer[x])) );                {-Put the data to send in    }
		              if keypressed then begin
               			ch:=char(readkey);
               			writePortb((Com4 and 0),(ORD (buffer[x])) );	//write BYTE to comm port, not CHAR to comm port.
               			writechar(ch); //Update screen		 
       			      end;	
       			                  	
       			      inc(x);
       			      
	             END;                                                             
	                
       		  end;
       		   //not ready or timer expired for waiting.
	                 TimeOut := TRUE; 
       		 		 IOerror:=true;
			         CloseCom(4);
       		                 			     
	  	until  x=1024;
   		if x=1024 then	x:=0;
    end;
    else begin
    {$ifdef Debug}
   		bochs_debugline('Invalid Port.');
   		{$endif}
   		writeln('Invalid Port.');
     end;
  end;{case}
end;

procedure writeCOMln(port:integer; line:str80);
//ENSURE Port is ACTIVE before using, makes no checks.
// Should use Pchar or Pstring as line limit is set at 255 internally.

var
   s:integer;
   buff:TBuffer; //^TBuffer

begin
   for s:=1 to length(line) do begin
       //should update pointer, one sec....
       buff[s]:=line[s]; //keeping in mind that we have a (for now) 1024 byte buffer(1KB)
       
    end;
    case port of
            1:begin
                   writecom(COM1,buff); //should do the trick.
                   delay(10); //just to be safe       
            end;
            2:begin
                   writecom(COM2,buff); //should do the trick.       
                   delay(10); //just to be safe
            end;
            3:begin
                   writecom(COM3,buff); //should do the trick.       
                   delay(10); //just to be safe
            end;
            4:begin
                   writecom(COM4,buff); //should do the trick.       
                   delay(10); //just to be safe
            end;
            else begin
            	writeln('Invalid Port.');
            end;       
   end;{case}
   //ClearBuffer; --just in case              
end;
	                                                                            
PROCEDURE Set_DTR_RTS (port:integer; Status : BOOLEAN);     
BEGIN                                            
  case port of
     1:begin 
	  	IF Status=true     
	  	   { prevent the  port/modem from being hung up during port setups due to dropping DTR and/or RTS.  }
           THEN writePORTb ((COM1 and MCR),(readportb(COM1 and MCR) AND $FC))   
           ELSE writePORTb ((COM1 and MCR) ,(readportb(COM1 and MCR) OR $03));    
  		 
  	end;	
  
   2:begin 
	  	IF Status=true                                                        
           THEN writePORTb ((COM2 and MCR),(readportb(COM2 and MCR) AND $FC))   
           ELSE writePORTb ((COM2 and MCR) ,(readportb(COM2 and MCR) OR $03));  
  		
  	end;	

   3:begin 
	  	IF Status=true                                                     
           THEN writePORTb ((COM3 and MCR),(readportb(COM3 and MCR) AND $FC))   
           ELSE writePORTb ((COM3 and MCR) ,(readportb(COM3 and MCR) OR $03));     
  	end;	
  
   4:begin 
	  	IF Status=true                                                    
           THEN writePORTb ((COM4 and MCR),(readportb(COM4 and MCR) AND $FC))   
           ELSE writePORTb ((COM4 and MCR) ,(readportb(COM4 and MCR) OR $03)); 
  		 
  end;
  
  else begin
  		writeln('Invalid Port.');
  end;  
end; 
end; 


//It sets everything up.This is for users.You want the fastest connection possible.
procedure SetPortValues(NewRate: longint;  databits:integer; newparity:char; stopbits:integer);
// Usage: SetPortValues(port,115200,8,'N',1);
//case (port) of

var
  DivisorLatch,setup: Word;
  setParity:integer;
  
begin
  DivisorLatch := 115200 div NewRate;
  setup:=$00;
  writeportb((COM1 and LCR),$80); //Set DLAB
  case NewRate of
    50:begin
        Latch_Low:= $09;
        Latch_High := $00;     
    end;
    110:begin
        Latch_Low := $04;
        Latch_High := $17;     
    end;
    150:begin
        Latch_Low := $03;
        Latch_High := $00;     
    end;
    300:begin
        Latch_Low := $01;
        Latch_High := $80;     
    end;
    600:begin
        Latch_Low := $00;
        Latch_High := $C0;     
    end;
    1200:begin
        Latch_Low := $00;
        Latch_High := $60;     
    end;
    2400:begin
        Latch_Low := $00;
        Latch_High := $30;     
    end;
    4800:begin
        Latch_Low := $00;
        Latch_High := $18;     
    end;
    9600:begin
        Latch_Low := $00;
        Latch_High := $0C;     
    end;
    19200:begin
        Latch_Low := $00;
        Latch_High := $06;     
    end;
    38400:begin
        Latch_Low := $00;
        Latch_High := $03;     
    end;
    57600:begin
        Latch_Low := $00;
        Latch_High := $02;     
    end;
    115200:begin
        Latch_Low := $00;
        Latch_High := $01;     
    end;
    
    else begin
		writeln('Invalid Modem/Port speed');
		exit;
    end;
  end;    
//rates higher than this are not supported with this controller.
{
 CASE databits OF //I think 'setup' is supposed to be 'databits' variable. Will Test.
    7  : setup := setup OR $07;
    8  : setup := setup OR $08;
    ELSE begin
      writeln('Invalid Data Bits');
      exit;
    End;
 end;
 CASE stopbits OF
    1  : setup := setup OR $01;
    2  : setup := setup OR $02;
    ELSE begin
      writeln('Invalid Stop Bits.');
      exit;
    End;
 end;
}
 //set the number, not the value.
  if newparity='N' then SetParity:=ord(ParityType(0));	

//just set it directly, use this for reading type.(if [x] in paritytype)
  if newparity='O' then SetParity:=ord(ParityType(1));
  if newparity='E' then SetParity:=ord(ParityType(2));	
  if newparity='M' then SetParity:=ord(ParityType(3));
  if newparity='S' then SetParity:=ord(ParityType(4));
  	
 		
  { Send final (calculated) setup Value to the communications port. }

  Temporary := (((DataBits - 5) AND $03) OR (((StopBits - 1) SHL 2) AND $04));     
  writePORTb ((COM1 and LCR), (Temporary OR ((setParity SHL 3) AND $38)));    

  writePortb((COM1 and Latch_High),(DivisorLatch shr 8));  
  writePortb((COM1 and Latch_Low),(DivisorLatch and $FF));

end;


function RingDetect:BOOLEAN;
                             // Routine to detect whether or not the    
                             //  phone is ringing by checking the comport
var
   IsRinging:boolean;
                             
BEGIN
    IsRinging:=false;
{  	if ( readportb(COM1 shr 6) )=odd then begin 
  		Ring; //ONE ringy dingy....
  		IsRinging:=true;
  	end;	
   if ODD( readPORTb(COM2) SHR 6) ) then begin
	    Ring;
	    IsRinging:=true;
    end;
     if  ODD( readPORTb(COM3) SHR 6) ) then begin
         Ring;
         IsRinging:=true;
     end;
     if ODD( readPORTb(COM4) SHR 6) ) then begin
         Ring;
         IsRinging:=true;
     end;
}
END;

{
    LineStatus    Bits 76543210        Meaning
                           1            time-out error
                            1           transfer shift register empty
                             1          transfer holding register empty
                              1         break interrupt detected
                               1        framing error
                                1       parity error
                                 1      overrun error
                                  1     data ready

        ModemStatus   Bits 76543210         Meaning
                           1            receive line signal detect
                            1           ring indicator
                             1          data set ready (DSR)
                              1         clear to send (CTS)
                               1        delta receive line signal detect
                                1       trailing edge ring detector
                                 1      delta data set ready (DDSR)
                                  1     delta clear to send  (DCTS)

}

function GetParity:ParityType;


var
  Parity:ParityType;
 begin
//case (comport) of 
     if (readportb(COM1 and LCR) and 3)=0 then GetParity:=ParityType(0);      
    if (readportb(COM1 and LCR) and 3)=1 then begin
	    if (readportb(COM1 and LCR) and 4)=0 then begin //Mark or Odd
    		
    		if (readportb(COM1 and LCR) and 5)=0 then  GetParity:=ParityType(3) else GetParity:=ParityType(1); 
    	end;	    
	    if (readportb(COM1 and LCR) and 4)=1 then begin //Space or Even    		
    		if (readportb(COM1 and LCR) and 5)=0 then  GetParity:=ParityType(4) else GetParity:=ParityType(3);	
    	end;	
    		
    end;

end;


function GetBaudRate(port:integer):LongInt;
var
  DivisorLatch: Word;

begin
  case port of
      1:begin
		  DivisorLatch := (readPortb(COM1 + Latch_High) shl 8) + readPortb(COM1 + Latch_Low);
		  
	  end;
	  2:begin
		  DivisorLatch := (readPortb(COM2 + Latch_High) shl 8) + readPortb(COM2 + Latch_Low);
		
	  end;
	  3:begin
		  DivisorLatch := (readPortb(COM3 + Latch_High) shl 8) + readPortb(COM3 + Latch_Low);
		
	  end;
	  4:begin
		  DivisorLatch := (readPortb(COM4 + Latch_High) shl 8) + readPortb(COM4 + Latch_Low);
		 
	  end else begin
	     writeln('Invalid port');
	     exit;
	  end;
  end;
  case DivisorLatch of
      $0900:begin
            GetBaudRate := 50;
      end;      
      $0417:begin
            GetBaudRate := 110;
      end;      
      $0300:begin
            GetBaudRate := 150;
      end;      
      $0180:begin
            GetBaudRate := 300;
      end;      
      $00C0:begin
            GetBaudRate := 600;
      end;      
      $0060:begin
            GetBaudRate := 1200;
      end;      
      $0030:begin
            GetBaudRate := 2400;
      end;      
      $0018:begin
            GetBaudRate := 4800;
      end;      
      $000C:begin
            GetBaudRate := 9600;
      end;      
      $0006:begin
            GetBaudRate := 19200;
      end;      
      $0003:begin
            GetBaudRate := 38400;
      end;      
      $0002:begin
            GetBaudRate := 57600;
      end;      
      $0001:begin
            GetBaudRate := 115200;
      end;   
// higher than 115200 not supported with this controller.
      else begin
          writeln('Invalid Baud recieved from Modem/Port');
          exit;
      end;         
  end;
end;

function GetStopBits(port:integer):integer;

var
    stop_bits:longint;

begin

 case port of
      1:begin
		  stop_bits:=(Readportb(COM1 and LCR) and 2);
	  end;
	  2:begin
		  stop_bits:=(Readportb(COM2 and LCR) and 2);
	  end;
	  3:begin
		  stop_bits:=(Readportb(COM3 and LCR) and 2);
	  end;
	  4:begin
		  stop_bits:=(Readportb(COM4 and LCR) and 2);
	  end;
	  else begin
	     writeln('Invalid port');
	     exit;
	  end;
 end;	  	  
 case stop_bits of
      	0:begin
		GetStopBits:=1;      	
      	end;
      
      	1:begin
      		GetStopBits:=2;
      	end;
 end;
end;

function GetDataBits(port:integer):integer;        

var
 data_bits0,data_bits1:byte;

        
begin

  case port of 
      1:begin

  	  	  
		  data_bits0:=(readportb(COM1 and LCR) and 0);
		  if data_bits0=0 then begin //either 5 or 7 in binary
			  data_bits1:=(readportb(COM1 and LCR) and 1);
		      if data_bits1=0 then GetDataBits:=5 else GetDataBits:=7;
		  end;
		  //else it is either 8 or 6 in binary.
		  data_bits1:=(readportb(COM1 and LCR) and 1);
	      if data_bits1=0 then GetDataBits:=6 else GetDataBits:=8;

	  end;
	  2:begin

  	  	  
		  data_bits0:=(readportb(COM2 and LCR) and 0);
		  if data_bits0=0 then begin //either 5 or 7 in binary
			  data_bits1:=(readportb(COM2 and LCR) and 1);
		      if data_bits1=0 then GetDataBits:=5 else GetDataBits:=7;
		  end;
		  //else it is either 8 or 6 in binary.
		  data_bits1:=(readportb(COM2 and LCR) and 1);
	      if data_bits1=0 then GetDataBits:=6 else GetDataBits:=8;

	  end;
	  3:begin
 	  	  
		  data_bits0:=(readportb(COM3 and LCR) and 0);
		  if data_bits0=0 then begin //either 5 or 7 in binary
			  data_bits1:=(readportb(COM3 and LCR) and 1);
		      if data_bits1=0 then GetDataBits:=5 else GetDataBits:=7;
		  end;
		  //else it is either 8 or 6 in binary.
		  data_bits1:=(readportb(COM3 and LCR) and 1);
	      if data_bits1=0 then GetDataBits:=6 else GetDataBits:=8;

	  end;
	  4:begin

		  data_bits0:=(readportb(COM4 and LCR) and 0);
		  if data_bits0=0 then begin //either 5 or 7 in binary
			  data_bits1:=(readportb(COM4 and LCR) and 1);
		      if data_bits1=0 then GetDataBits:=5 else GetDataBits:=7;
		  end;
		  //else it is either 8 or 6 in binary.
		  data_bits1:=(readportb(COM4 and LCR) and 1);
	      if data_bits1=0 then GetDataBits:=6 else GetDataBits:=8;

	  end;
	  else begin
	     writeln('Invalid port');
	     exit;
	  end;
  end;
end;        
 


procedure InitCom(port,baud,DataBits,StopBits:integer; Parity:parityType);   

{

 This procedure installs and enables the specified serial port interrupt.  All port input line monitoring   
 variables are set to match the actual line states.  DTR and RTS are forced to the ready state.   

It uses specified parameters, not FORCED settings(as we had before).


 The 8250 interrupts are enabled by ORing the MCR with $08.  To enable all four 8250 interrupt types, write 
 $0F to the IER.  (Receive Buffer Full, Line Status, & Modem Status interrupts are enabled by writing $0D   
 to the IER).  ORing $EF with readPORTb($21) enables IRQ4 (COM1 or COM3), while $F7 enables IRQ3 (COM2 or COM4).
 Hardware interrupts should be disabled during the installation process since the 8259 ports are being set. 

DLAB:

 Divisor latch access bit. WHILE OPERATING IN INTERRUPT MODE, THE
 DIVISOR ACCESS LATCH BIT MUST ALWAYS BE ZERO. If for some horrible reason
 you need to change the baud rate in the middle of a transmission (or while
 the interrupts are enabled) clear the interrupt flag, do your dirty work,
 clear the divisor latch bit, and finally restore interrupts.
        

}


const
    BaudReg : array [B110 .. B115200] of word =
      ($0417, $0300, $0180, $00C0, $0060, $0030,
       $0018, $000C, $0006, $0003, $0002, $0001);

    ParityReg : array [None..Space] of byte =
      ($00, $08, $18, $28, $38);
    LengthReg : array [D5 .. D8] of byte =
      ($00, $01, $02, $03);
    StopReg : array [S1 .. S2] of byte =
      ($00, $04);

begin
   if ctrlbreak=true then exit;

//case port of 1..4 --do to all 4 or which one??

   asm
     cli
   end;
	
	readportb (COM1 and 5); // Clear any pending interrupts
	readportb(COM1 and 6);   //Clear Receiver line status

	readportb(COM1 and 2);  //Clear CTS/DSR/RI Interrupts
	readportb($3F8);   //Clear xmtr empty interrupt
        

{--if exist....

	readportb (COM2 and 5); // Clear any pending interrupts
	readportb(COM2 and 6);   //Clear Receiver line status
	readportb(COM2 and 2);  //Clear CTS/DSR/RI Interrupts
	readportb($3F8);   //Clear xmtr empty interrupt

	readportb (COM3 and 5); // Clear any pending interrupts
	readportb(COM3 and 6);   //Clear Receiver line status
	readportb(COM3 and 2);  //Clear CTS/DSR/RI Interrupts
	readportb($3F8);   //Clear xmtr empty interrupt

	readportb (COM4 and 5); // Clear any pending interrupts
	readportb(COM4 and 6);   //Clear Receiver line status
	readportb(COM4 and 2);  //Clear CTS/DSR/RI Interrupts
	readportb($3F8);   //Clear xmtr empty interrupt

}


     
//Clear data available intr.


   case port of
       1:begin
	   
//originally 115200 baud, 8 data bits, 1 stop bit, and no parity.
//as you can see, this gets complex, QUICK.

                                                                {-Accessing 8259 hardware    }
        Temporary := readPORTb (COM1 and MSR);                                  
        CD  [port] := ($80 AND Temporary <> 0);                                  {-Carrier Detect status      }
        CTS [port] := ($10 AND Temporary <> 0);                                  {-Clear to Send status       }
        DSR [port] := ($20 AND Temporary <> 0);                                  {-Data Set Ready status      }
        RI  [port] := ($40 AND Temporary <> 0);                                  {-Ring Indicator status      }
        writePORTb ((COM1  and MCR), ((COM1 and MCR) OR $08));        {-Enable 8250 interrupt line }
        writePORTb ((COM1 and LCR), ((COM1 and LCR) AND $7F));       {-Set THR/RHR/IER active     }
        writePORTb ((COM1 and IER) ,$01);                                      {-Enable 8250 interrupts     }
		 Set_DTR_RTS (Com1, FALSE);   
            writePortb((Com1 and $03),(readportb(Com1 and $03)or $80));  //LCR send set baud rate command 'set DLAB'
           SetPortValues(115200,8,'N',1); //set to fastest we can go.THis may be port anyway, not modem, so do it.
            
            //LCR --011(3) [8 bits, 1 stop bit, no parity-standardized a few years back.(8N1)]
            
           writePortb((Com1 and 2),$07); {clearing the FIFOs}
           writePortb((Com1 and 2),$00); {disabling FIFOs}
           writePortb((Com1 and 4),$0B); //MCR -aux output 2
           writePortb((readPortb($21) and $EF),$21); //Comm port Mask
           writePortb((Com1 and 1),$01); //IER 
           writePortb((COM1 and 3),(readPortb(COM1 and 3) and $7F)); //Clear DLAB
//finally, some flow control...WHEW. Hard to find this stuff..
          ComFlowControl[1] := RtsCts; //rtscts or xonxoff?
          ComFlowHalted[1] := false;
          ComXoffReceived[1] := false;
 		Set_DTR_RTS (Com1, TRUE);                     //tell remote device ready to recieve            

       end;
       2:begin
	
//originally 115200 baud, 8 data bits, 1 stop bit, and no parity.
//as you can see, this gets complex, QUICK.

                                                         {-Accessing 8259 hardware    }
        Temporary := readPORTb (COM2 and MSR);                                  
        CD  [port] := ($80 AND Temporary <> 0);                                  {-Carrier Detect status      }
        CTS [port] := ($10 AND Temporary <> 0);                                  {-Clear to Send status       }
        DSR [port] := ($20 AND Temporary <> 0);                                  {-Data Set Ready status      }
        RI  [port] := ($40 AND Temporary <> 0);                                  {-Ring Indicator status      }
        writePORTb ((COM2  and MCR), ((COM2 and MCR) OR $08));        {-Enable 8250 interrupt line }
        writePORTb ((COM2 and LCR), ((COM2 and LCR) AND $7F));       {-Set THR/RHR/IER active     }
        writePORTb ((COM2 and IER) ,$01);                                      {-Enable 8250 interrupts     }
		 Set_DTR_RTS (Com2, FALSE);   
            writePortb((Com2 and $03),(readportb(Com2 and $03)or $80));  //LCR send set baud rate command 'set DLAB'
           SetPortValues(115200,8,'N',1); //set to fastest we can go.THis may be port anyway, not modem, so do it.
            
            //LCR --011(3) [8 bits, 1 stop bit, no parity-standardized a few years back.(8N1)]
            
           writePortb((Com2 and 2),$07); {clearing the FIFOs}
           writePortb((Com2 and 2),$00); {disabling FIFOs}
           writePortb((Com2 and 4),$0B); //MCR -aux output 2
           writePortb((readPortb($21) and $EF),$21); //Comm port Mask
           writePortb((Com2 and 1),$01); //IER 
           writePortb((COM2 and 3),(readPortb(COM2 and 3) and $7F)); //Clear DLAB
//finally, some flow control...WHEW. Hard to find this stuff..
         ComFlowControl[2] := RtsCts;
         ComFlowHalted[2] := false;
         ComXoffReceived[2] := false;

 		Set_DTR_RTS (Com2, TRUE);                     //tell remote device ready to recieve            

       end;     
       3:begin
	  //originally 115200 baud, 8 data bits, 1 stop bit, and no parity.
//as you can see, this gets complex, QUICK.

                                                                  {-Accessing 8259 hardware    }
        Temporary := readPORTb (COM3 and MSR);                                  
        CD  [port] := ($80 AND Temporary <> 0);                                  {-Carrier Detect status      }
        CTS [port] := ($10 AND Temporary <> 0);                                  {-Clear to Send status       }
        DSR [port] := ($20 AND Temporary <> 0);                                  {-Data Set Ready status      }
        RI  [port] := ($40 AND Temporary <> 0);                                  {-Ring Indicator status      }
        writePORTb ((COM3  and MCR), ((COM3 and MCR) OR $08));        {-Enable 8250 interrupt line }
        writePORTb ((COM3 and LCR), ((COM3 and LCR) AND $7F));       {-Set THR/RHR/IER active     }
        writePORTb ((COM3 and IER) ,$01);                                      {-Enable 8250 interrupts     }
		 Set_DTR_RTS (Com3, FALSE);   
            writePortb((Com3 and $03),(readportb(Com3 and $03)or $80));  //LCR send set baud rate command 'set DLAB'
           SetPortValues(115200,8,'N',1); //set to fastest we can go.THis may be port anyway, not modem, so do it.
            
            //LCR --011(3) [8 bits, 1 stop bit, no parity-standardized a few years back.(8N1)]
            
           writePortb((Com3 and 2),$07); {clearing the FIFOs}
           writePortb((Com3 and 2),$00); {disabling FIFOs}
           writePortb((Com3 and 4),$0B); //MCR -aux output 2
           writePortb((readPortb($21) and $EF),$21); //Comm port Mask
           writePortb((Com3 and 1),$01); //IER 
           writePortb((COM3 and 3),(readPortb(COM3 and 3) and $7F)); //Clear DLAB
//finally, some flow control...WHEW. Hard to find this stuff..
         ComFlowControl[3] := RtsCts;
         ComFlowHalted[3] := false;
         ComXoffReceived[3] := false;

 		Set_DTR_RTS (Com3, TRUE);                     //tell remote device ready to recieve            

      end;           
              
       4:begin
	     //originally 115200 baud, 8 data bits, 1 stop bit, and no parity.
//as you can see, this gets complex, QUICK.

                                                           {-Accessing 8259 hardware    }
        Temporary := readPORTb (COM4 and MSR);                                  
        CD  [port] := ($80 AND Temporary <> 0);                                  {-Carrier Detect status      }
        CTS [port] := ($10 AND Temporary <> 0);                                  {-Clear to Send status       }
        DSR [port] := ($20 AND Temporary <> 0);                                  {-Data Set Ready status      }
        RI  [port] := ($40 AND Temporary <> 0);                                  {-Ring Indicator status      }
        writePORTb ((COM4  and MCR), ((COM4 and MCR) OR $08));        {-Enable 8250 interrupt line }
        writePORTb ((COM4 and LCR), ((COM4 and LCR) AND $7F));       {-Set THR/RHR/IER active     }
        writePORTb ((COM4 and IER) ,$01);                                      {-Enable 8250 interrupts     }
		 Set_DTR_RTS (Com1, FALSE);   
            writePortb((Com4 and $03),(readportb(Com4 and $03)or $80));  //LCR send set baud rate command 'set DLAB'
           SetPortValues(115200,8,'N',1); //set to fastest we can go.THis may be port anyway, not modem, so do it.
            
            //LCR --011(3) [8 bits, 1 stop bit, no parity-standardized a few years back.(8N1)]
            
           writePortb((Com4 and 2),$07); {clearing the FIFOs}
           writePortb((Com4 and 2),$00); {disabling FIFOs}
           writePortb((Com4 and 4),$0B); //MCR -aux output 2
           writePortb((readPortb($21) and $EF),$21); //Comm port Mask
           writePortb((Com4 and 1),$01); //IER 
           writePortb((COM4 and 3),(readPortb(COM4 and 3) and $7F)); //Clear DLAB
//finally, some flow control...WHEW. Hard to find this stuff..
         ComFlowControl[4] := RtsCts;
         ComFlowHalted[4] := false;
         ComXoffReceived[4] := false;

 		Set_DTR_RTS (Com4, TRUE);                     //tell remote device ready to recieve            

       end; 
       else begin
          beep;
          writeln('Invalid port.Must be Comm 1-4.');
          
       end;
    end; //case
    asm

                sti                    //Restore interrupt disable flag.
   end;

end;

//flow control uses this.
{ ---------------------------------------------------------------------------
  ComAllowed;

  With this function it is possible to check if writing data to the COM port
  is allowed. When there is no flow control no check is made on any control
  line and the result will always be true. When hardware type flow control is
  enabled, DSR (and CD) and CTS must be high. In case of software flow
  control DSR must be high and a check is made if an XOFF byte was received.
  ---------------------------------------------------------------------------
}

  function  ComAllowed (Port : integer) : boolean; 
//checks to see if COM port IO is allowed given flow control state.
  begin
    ComAllowed := true;
    if (ComFlowControl[Port] = RtsCts) then //hardware
    begin
      { replace in next line both $30 with $B0 for checking on CD, DSR and CTS}
      if ((readPortb(Com1 and MSR) and $30) <> $30) or ((readPortb(Com2 and MSR) and $30) <> $30) or ((readPortb(Com3 and MSR) and $30) <> $30) or ((readPortb(Com4 and MSR) and $30) <> $30) then { no DSR or CTS}
        ComAllowed := false;

    end
    else if (ComFlowControl[Port] = XonXoff) then //software
    begin
      { replace in next line both $20 with $A0 for checking on CD and DSR }
      if ((readPortb(Com1 and MSR) and $20) <> $20) or (ComXoffReceived[1]=true) then { XOFF received }
        ComAllowed := false;
      if ((readPortb(Com2 and MSR) and $20) <> $20) or (ComXoffReceived[2]=true) then { XOFF received }
        ComAllowed := false;
      if ((readPortb(Com3 and MSR) and $20) <> $20) or (ComXoffReceived[3]=true) then { XOFF received }
        ComAllowed := false;
      if ((readPortb(Com4 and MSR) and $20) <> $20) or (ComXoffReceived[4]=true) then { XOFF received }
        ComAllowed := false;
    end
  end;


procedure closeCom(port:integer);
//this needs some more work.

begin
   asm
          cli                     //Dont allow interrupts while messing
   end;
   if ctrlbreak=true then exit;
   case port of
   1:begin
   
    {from SWAG archive:
   	
   	 DisableInterrupts;
      i := Port[$21];        // get the interrupt mask register 
      m := 1 shl 4;        // set mask to turn off interrupt  (irq) 3
      Port[$21] := i or m;

   }

    //Hangup by dropping the DTR line.Works on Modems, too.
    //This is how we do it.Fix the rest.
	
     writeportb((COM1 and LCR), (readportb(COM1 and LCR) and $7F)); //get int mask register
     writeportb((COM1 and 1), $00);  //disable data ready
     writeportb((COM1 and 4),$00); //disable OUT2
      writePortb((readPortb($21) or $10),$21); //mask off the irq.[disable com port x]
   end;
  
   2:begin

   writeportb((COM2 and LCR), (readportb(COM2 and LCR) and $7F)); //get int mask register
     writeportb((COM2 and 1), $00);  //disable data ready
     writeportb((COM2 and 4),$00); //disable OUT2
      writePortb((readPortb($21) or $10),$21); //mask off the irq.[disable com port x]
      

   end;
   3:begin
 writeportb((COM3 and LCR), (readportb(COM3 and LCR) and $7F)); //get int mask register
     writeportb((COM3 and 1), $00);  //disable data ready
     writeportb((COM3 and 4),$00); //disable OUT2
      writePortb((readPortb($21) or $10),$21); //mask off the irq.[disable com port x]
      

   end;
   4:begin
     
    writeportb((COM4 and LCR), (readportb(COM4 and LCR) and $7F)); //get int mask register
     writeportb((COM4 and 1), $00);  //disable data ready
     writeportb((COM4 and 4),$00); //disable OUT2
      writePortb((readPortb($21) or $10),$21); //mask off the irq.[disable com port x]
      
   
   end;

   end; //case
   asm   
          sti
   end;
end;

procedure BaudChanged(OldSpeed,NewSpeed,port:integer);


begin
        case (port) of
	
	1:begin
		    IF OldSpeed <> NewSpeed THEN begin //only trip if changed.
	        writeportb ((COM1 and 3), (readportb(COM1 and 3) OR $80)); // Enable DLAB
	        writeportb (COM1, ((115200 div NewSpeed) mod $100));     // Send LSB
	        writeportb ((COM1 and 1), ((115200 div NewSpeed) div $100));   //Send MSB
	        writeportb ((COM1 and 3), (readportb(COM1 and 3) AND $7F)); //Disable DLAB
        END; 
   end;
   2:begin
  		  IF OldSpeed <> NewSpeed THEN begin //only trip if changed.
	        writeportb ((COM2 and 3), (readportb(COM2 and 3) OR $80)); // Enable DLAB
	        writeportb (COM2, ((115200 div NewSpeed) mod $100));     // Send LSB
	        writeportb ((COM2 and 1), ((115200 div NewSpeed) div $100));   //Send MSB
	        writeportb ((COM2 and 3), (readportb(COM2 and 3) AND $7F)); //Disable DLAB
        END; 
	end;
	3:begin
	            IF OldSpeed <> NewSpeed THEN begin //only trip if changed.
	        writeportb ((COM3 and 3), (readportb(COM3 and 3) OR $80)); // Enable DLAB
	        writeportb (COM3, ((115200 div NewSpeed) mod $100));     // Send LSB
	        writeportb ((COM3 and 1), ((115200 div NewSpeed) div $100));   //Send MSB
	        writeportb ((COM3 and 3), (readportb(COM3 and 3) AND $7F)); //Disable DLAB

        END; 

	
	end;
	4:begin
	
           IF OldSpeed <> NewSpeed THEN begin //only trip if changed.
	        writeportb ((COM4 and 3), (readportb(COM4 and 3) OR $80)); // Enable DLAB
	        writeportb (COM4, ((115200 div NewSpeed) mod $100));     // Send LSB
	        writeportb ((COM4 and 1), ((115200 div NewSpeed) div $100));   //Send MSB
	        writeportb ((COM4 and 3), (readportb(COM4 and 3) AND $7F)); //Disable DLAB
        END; 
	
	

	end;
	else begin
	   writeln('Invalid COM port.');
		exit;
	end;
	end;{case}
end;

function IdentifyUART: String;
//This has a Use, but all of these onboard ones will use the same code.
{
	
Here is the difference:

IN:
The 16550 can be programmed so that a receive (RX) interrupt is not triggered until 4 (or 8 or 14) bytes have been received, while the 16650 can be triggered at up to 30 bytes, and the 16750 can be triggered at up to 56 bytes. This can significantly reduce the CPU processing time, since 14 (or 30 or 56) bytes can be moved at once.	

OUT:
Up to 16 bytes can be written at once to the transmitter FIFO buffer while processing one transmitter interrupt if an 16550 UART is used, while the 16650 can write up to 32 bytes at once, and the 16750 can write up to 64 bytes at once.	

Change the IID Register to set this. For now, set at single byte level. Not Efficient, but gets the job done.
	
}

var
  Test: Byte;
begin
  writePortb((COM1 and FCR),$e7);
  Test := readPortb(COM1 and IIR);
  if (Test and $40) > 0 then begin
    if (Test and $80) > 0 then begin
      if (Test and $20) > 0 then begin
        IdentifyUART := '16750'
      end;
        IdentifyUART := '16550A';
      end;
      IdentifyUART := '16550';
   end;
   
    writePortb((COM1 and SCR),$2A);
    if readPortb(COM1 and SCR) = $2A then
      IdentifyUART := '16450'
    else
      IdentifyUART := '8250';
  //com1 limited: use case (commport) of
end;

{ BreakCom;

  With this routine the TD line can be lowered for 200 msec, which is a so-
  called break signal.
}

//com1 limited: use case (commport) of instead

  procedure BreakCom;

  begin
    writePortb((Com1 and LCR), (readPortb(Com1 and LCR) or $40));
    Delay (100);  { 0.1 seconds }
    writePortb((Com1 and LCR), (readPortb(Com1 and LCR) and $BF));
  end;
 
//com1 limited: use case (commport) of 
Procedure TimeoutwModemReset;
begin
  writeln('Modem timeout');
  delay(1000);
  {$warning FIXME: Assumes COM1, no way to check which is open YET.}
  writeCOMln(1,'+++');
  delay(1000);
  writeCOMln(1,'ATH');
end;

{DONT ASK. Jazz thought it USEFUL.
procedure sendAFAX(number:integer); //get number to fax to

faxinit:
// need to wait for modem reply of 'ok' on recv buffer on all commands
begin
sendstrln('AT&FE0');
sendstrln('AT+FCLASS=2');
sendstr('AT+FLID=');
sendstrln(number);
sendstrln('AT+FDCC=0,3,0,2');

dial:
sendstr('ATD'+tp+nr+#13);
delay(TimerTicks+60*18); //give enough time to dial
sendstr('AT+FDT'+#13);
delay(TimerTicks+30*18); //give enough time for 'is fax there' tone
//wait for 'connect' on recv buffer

newpage:
 sendchar(#16);
 sendchar(#3);
//wait for 'ok' ACK packet
sendstr('AT+FET=2'+#13);
delay(TimerTicks+30*18); //give enough time for it to occur remotely
//wait for 'ok' ACK packet
 
//make heads and tails of white and black codes and the rest is easy. Use existing XMIT/RECV procedures/functions to send
and recv fax char/string data.

end;

}


function ReadCom(port:integer):Pbuffer;


var
   bufferin:integer;
   buff:Pbuffer;
 cee,temp:byte;

begin
   New(buff);
   case port of
   
   1:begin
		bufferin:=1; //reset input buffer
	    cee:=(readportb(COM1 and 5)); //LSR  ==$3FD
	    while (cee and 1)=0 do begin        //check bit one to see if data is waiting. If not, exit.
     	     buff^[bufferin]:=(chr(readportb(COM1)));
        	 inc(bufferin);
         	 if bufferin= 1025 then bufferin:=0;
          //According to this code, we should never overrun on input, as the full buffer resets itself when full.
          // (always hungry for more)
   		end;   
   end;
   
   2:begin
		bufferin:=1; //reset input buffer
	    cee:=(readportb(COM2 and 5)); //LSR  ==$3FD
	    while (cee and 1)=0 do begin        //check bit one to see if data is waiting. If not, exit.
     	     buff^[bufferin]:=(chr(readportb(COM2)));
        	 inc(bufferin);
         	 if bufferin= 1025 then bufferin:=0;
          //According to this code, we should never overrun on input, as the full buffer resets itself when full.
          // (always hungry for more)
   		end; 
   end;
   
   3:begin
		bufferin:=1; //reset input buffer
	    cee:=(readportb(COM3 and 5)); //LSR  ==$3FD
	    while (cee and 1)=0 do begin        //check bit one to see if data is waiting. If not, exit.
     	     buff^[bufferin]:=(chr(readportb(COM3)));
        	 inc(bufferin);
         	 if bufferin= 1025 then bufferin:=0;
          //According to this code, we should never overrun on input, as the full buffer resets itself when full.
          // (always hungry for more)
   		end;   
   end;
   
   4:begin
		bufferin:=1; //reset input buffer
	    cee:=(readportb(COM4 and 5)); //LSR  ==$3FD
	    while (cee and 1)=0 do begin        //check bit one to see if data is waiting. If not, exit.
     	     buff^[bufferin]:=(chr(readportb(COM4)));
        	 inc(bufferin);
         	 if bufferin= 1025 then bufferin:=0;
          //According to this code, we should never overrun on input, as the full buffer resets itself when full.
          // (always hungry for more)
   		end; 
   end;
   else begin{$ifdef debug}
   		bochs_debugline('Invalid Port.'); {$endif}
   		writeln('Invalid Port.');
   end;
   end;{case}
   ReadCom:=Buff;
end;


procedure Coms(var r: TRegisters); 
//USES FIFOs.
//interrupt driven, not COM polling method.

{ This procedure handles all interrupts from the 8250 communications chip.  All interrupt types are at least  minimally supported. 

  Incoming data is ignored if the buffer is full, otherwise it is placed into the Buffer    
 circular queue. 
 
 Data to be transmitted by interrupt is taken from the Buffer queue.  The transmitter is 
 the only interrupt which has to be manually invoked.  Do so by placing the first character of each trans-  
 mission into the THR.  It automatically shuts off when all the data in the buffer has been sent.
 
  The other interrupt types will take care of themselves once enabled.
  
    BOOLEAN arrays CTS, DSR, RI, and CD always show current status of these lines if the Modem Status
 Change interrupt has been enabled.  A TRUE indicates the signal is  active.  CD and RI are very helpful for modem
  related programs.  Line Status errors are counted.
  
   All ports with interrupts pending on the IRQ level are processed regardless of which port generated the actual interrupt.            
 

THIS IS THE FLOW.....
(I hate ACK-ro-nyms.... )


Ring->Answer
Carrier Detect -->No Carrier(dropped call/line interrupted)

(1-WAY communication)
Data Set Ready
DCE/DTE(depending)->Clear to Send 
DTE/DCE->(Request to Send)
Send->Recv

Recv-> Data Waiting

(finish porting this)
//data must be between these boundaries:
    ComBufferSize = 4096;
    ComFlowLower = 256;
    ComFlowUpper = 1024;


//recieving

 if ComFlowControl[1] = XonXoff then
      begin
        if Buffer[1] = $11 then // XON 
          ComXoffReceived[1] := false
        else if Buffer[1] = $13 then // XOFF 
          ComXoffReceived[1] := true;
      end;

//if flow is off and buffer has read enough(1/2 size buffer approx     ):

 begin // buffer has emptied enough 
        if ComFlowControl[PortNumber] = RtsCts then
   writePortb((ComPort and MCR),(ComPort and MCR) or $02)); //raise RTS 
        else if ComFlowControl[PortNumber] = XonXoff then
   WritecharCom (PortNumber, 11); // send XON 
        ComFlowHalted[PortNumber] := false;
      end;


//sending

//if flow is ON and buffer is half full or more:

if ComFlowControl[port] = RtsCts then
     writePortb((Com1 and MCR), (readPortb(COM1 and MCR) and $FD)); // lower RTS 
   else if ComFlowControl[port] = XonXoff then
     WritecharCom (COM1, 13); // send XOFF 
   ComFlowHalted[COM1] := true;
   

}


var
	s : string;
	connected:boolean; //ideally, some useful number we can reference later.
         c,x:integer;
        ch:char;
       eaxVAL:longword;

  bufferin, bufferout:integer;
  buffer:array [1..1025] of byte; //Use char(ch), they come in as bytes.
  //and ord(ch) [leave as bytes] going out.
  cee,temp:byte;
  IMR:shortint; 
{chr()== char()}

{interrupt will trip on incoming or outgoing data.}


begin{COM Handler}
// We dont deal with RS232 data pins directly, there simply is NO NEED anymore.

//this is status check.ALWAYS check after Com/Printer and Disk I/O.

 if ctrlbreak=true then exit;
//thank QuickBasic for this. :-P


 IF ((readportb(COM1 and 6) AND 128) = 0) or ((readportb(COM2 and 6) AND 128) = 0) or 
  ((readportb(COM3 and 6) AND 128) = 0) or  ((readportb(COM4 and 6) AND 128) = 0) Then begin
           writeln(' FM Carrier Signal lost.Connection Closed.');
	   connected:=false;
           IOerror:=true; 
 end;
 
 if connected then begin //do only if line still active. The Handler makes sure we keep rechecking the line status.


//Identify COM port interrupt type    
//Do this in order as we dont know which COMM port called us.


  writePORTb ((COM1 and LCR),(readportb(COM1 and LCR) AND $7F));    {Set THR, RHR & IER active  }	
  CASE (readPORTb (COM1 and IIR) AND $07) OF    

	0:begin
	 //       Delta clear to send
	 Temp := (readPORTb (COM1  and MSR));              {    MODEM STATUS CHANGES  (in bits)  }
     CD  [1] := ($80 AND Temp <> 0);         {       Carrier Detect       }
     CTS [1] := ($10 AND Temp <> 0);          {       Clear To Send        }
     DSR [1] := ($20 AND Temp <> 0);          {       Data Set Ready       }
     RI  [1] := ($40 AND Temp <> 0);          {       Ring Indicator       }
        
	end;
	   2 : BEGIN                       //Transmit register empty                                     
           IF x=length(buffer)  THEN                                                              
                writePORTb ((COM1 and IER),(readportb(COM1 and IER) AND $FD)) 
              //shut off transmitter
             ELSE  begin       //send next byte, but remove it from buffer                                                                   
                                                                       
               writePORTb (COM1,(ord( Buffer[x])));  
               inc(x);
               
              END;                                                           
           END;                                                                
       4 : BEGIN               //Recieve buffer full                                                 
            IF (length(buffer) + 1) <> length(buffer)    //if we can add the character to the end of buffer      
             THEN                                                     
                  BEGIN                                                             
               		buffer[x] := readPORTb (COM1);      
               		inc(x);
               		
                  END                                                          
             ELSE        //read in, but not stored.
                BEGIN                                                            
	               IF readportb(Com1)  = $00 THEN exit; { DO Nothing }    
              END;                                                            
           END;                                                              
       6 : BEGIN                   //line status change          
             Temp :=(readPORTb (COM1 and LSR) AND $1E);                        {   Just count the errors    }
             IF (Temp AND $02 <> 0) THEN begin       {       Overrun Error        }
                    INC (Overrun[1]); //inc counter or inc counter[port]?                 
             		//writeportb(Com1,FlowStop);
             		if x=length(buffer) then x:=0;  //reset buffer
             		asm  //wait for incoming stream to match buffer size
             			nop
             			nop
             			nop
             			nop
             		end;	
             		//and try again.
             end;		
             IF (Temp AND $04 <> 0) THEN begin
             		INC (Parity [1]);                    {       Parity Error         }
             		//writeportb(Com1,FlowStop);
             		asm  //wait for hardware on remote end to catch up
             			nop
             			nop
             			nop
             			nop
             		end;	
             		
             		//rematch parity
//             		SetBaudRate(GetBaudrate,GetBitSize,GetParity,GetStopBits);  -some functions still being worked on.
             		//and retry
             end;		
             IF (Temp AND $08 <> 0) THEN begin
             		INC (Framing [1]);                   {       Framing Error        }
             		{$ifdef debug}
             		bochs_debugline('Error making COM/Modem packet for some reason.');
             		{$endif}
             end;		
             IF (Temp AND $10 <> 0) THEN begin
             		INC (BreakInt [1]);                     {       Break Interrupt      }
             		{$ifdef debug}
             		bochs_debugline('CTRL-Break or CTRL-C pressed during COM/modem port operation.');
             		{$endif}
             		ctrlbreak:=true; //Terminate communications
             		exit;
             end;		
           END;                                  
           
    end; // case
    writePORTb ((COM2 and LCR),(readportb(COM2 and LCR) AND $7F));    {Set THR, RHR & IER active  }
   CASE (readPORTb (COM2 and IIR) AND $07) OF    

	0:begin
	 //       Delta clear to send
	 Temp := (readPORTb (COM2  and MSR));              {    MODEM STATUS CHANGES  (in bits)  }
     CD  [2] := ($80 AND Temp <> 0);         {       Carrier Detect       }
     CTS [2] := ($10 AND Temp <> 0);          {       Clear To Send        }
     DSR [2] := ($20 AND Temp <> 0);          {       Data Set Ready       }
     RI  [2] := ($40 AND Temp <> 0);          {       Ring Indicator       }
        
	end;
	   2 : BEGIN                       //Transmit register empty                                     
           IF x=length(buffer)  THEN                                                              
                writePORTb ((COM2 and IER),(readportb(COM2 and IER) AND $FD)) 
              //shut off transmitter
             ELSE  begin       //send next byte, but remove it from buffer                                                                   
                                                                       
               writePORTb ((COM2),(ord( Buffer[x])));  
               inc(x);
               
              END;                                                           
           END;                                                                
       4 : BEGIN               //Recieve buffer full                                                 
            IF (length(buffer) + 1) <> length(buffer)    //if we can add the character to the end of buffer      
             THEN                                                     
                  BEGIN                                                             
               		buffer[x] := readPORTb (COM2);      
               		inc(x);
               		
                  END                                                          
             ELSE        //read in, but not stored.
                BEGIN                                                            
	               IF readportb(Com2)  = $00 THEN { DO Nothing } exit;    
              END;                                                            
           END;                                                              
       6 : BEGIN                   //line status change          
             Temp :=(readPORTb (COM2 and LSR) AND $1E);                        {   Just count the errors    }
             IF (Temp AND $02 <> 0) THEN begin       {       Overrun Error        }
                    INC (Overrun [2]);                 
             		//writeportb(Com1,FlowStop);
             		if x=length(buffer) then x:=0;  //reset buffer
             		asm  //wait for incoming stream to match buffer size
             			nop
             			nop
             			nop
             			nop
             		end;	
             		//and try again.
             end;		
             IF (Temp AND $04 <> 0) THEN begin
             		INC (Parity [2]);                    {       Parity Error         }
             		//writeportb(Com1,FlowStop);
             		asm  //wait for hardware on remote end to catch up
             			nop
             			nop
             			nop
             			nop
             		end;	
             		
             		//rematch parity
//             		SetBaudRate(GetBaudrate,GetBitSize,GetParity,GetStopBits);  -some functions still being worked on.
             		//and retry
             end;		
             IF (Temp AND $08 <> 0) THEN begin
             		INC (Framing [2]);                   {       Framing Error        }
             		{$ifdef Debug}
             		bochs_debugline('Error making COM/Modem packet for some reason.');
             		{$endif}
             end;		
             IF (Temp AND $10 <> 0) THEN begin
             		INC (BreakInt [2]);                     {       Break Interrupt      }
             		{$ifdef Debug}
             		bochs_debugline('CTRL-Break or CTRL-C pressed during COM/modem port operation.');
             		{$endif}
             		ctrlbreak:=true; //Terminate communications
             		exit;
             end;		
           END;                                  
           
    end; // case
    
	writePORTb ((COM3 and LCR),(readportb(COM3 and LCR) AND $7F));    {Set THR, RHR & IER active  }
	
	 CASE (readPORTb (COM3 and IIR) AND $07) OF    

	0:begin
	 //       Delta clear to send
	 Temp := (readPORTb (COM3  and MSR));              {    MODEM STATUS CHANGES  (in bits)  }
     CD  [3] := ($80 AND Temp <> 0);         {       Carrier Detect       }
     CTS [3] := ($10 AND Temp <> 0);          {       Clear To Send        }
     DSR [3] := ($20 AND Temp <> 0);          {       Data Set Ready       }
     RI  [3] := ($40 AND Temp <> 0);          {       Ring Indicator       }
        
	end;
	   2 : BEGIN                       //Transmit register empty                                     
           IF x=length(buffer)  THEN                                                              
                writePORTb ((COM3 and IER),(readportb(COM3 and IER) AND $FD)) 
              //shut off transmitter
             ELSE  begin       //send next byte, but remove it from buffer                                                                   
                                                                       
               writePORTb ((COM3),(ord( Buffer[x])));  
               inc(x);
               
              END;                                                           
           END;                                                                
       4 : BEGIN               //Recieve buffer full                                                 
            IF (length(buffer) + 1) <> length(buffer)    //if we can add the character to the end of buffer      
             THEN                                                     
                  BEGIN                                                             
               		buffer[x] := readPORTb (COM3);      
               		inc(x);
               		
                  END                                                          
             ELSE        //read in, but not stored.
                BEGIN                                                            
	               IF readportb(Com3)  = $00 THEN { DO Nothing } exit;    
              END;                                                            
           END;                                                              
       6 : BEGIN                   //line status change          
             Temp :=(readPORTb (COM3 and LSR) AND $1E);                        {   Just count the errors    }
             IF (Temp AND $02 <> 0) THEN begin       {       Overrun Error        }
                    INC (Overrun [3]);                 
             		//writeportb(Com1,FlowStop);
             		if x=length(buffer) then x:=0;  //reset buffer
             		asm  //wait for incoming stream to match buffer size
             			nop
             			nop
             			nop
             			nop
             		end;	
             		//and try again.
             end;		
             IF (Temp AND $04 <> 0) THEN begin
             		INC (Parity [3]);                    {       Parity Error         }
             		//writeportb(Com1,FlowStop);
             		asm  //wait for hardware on remote end to catch up
             			nop
             			nop
             			nop
             			nop
             		end;	
             		
             		//rematch parity
//             		SetBaudRate(GetBaudrate,GetBitSize,GetParity,GetStopBits);  -some functions still being worked on.
             		//and retry
             end;		
             IF (Temp AND $08 <> 0) THEN begin
             		INC (Framing [3]);                   {       Framing Error        }
             		{$ifdef Debug}
             		bochs_debugline('Error making COM/Modem packet for some reason.');
             		{$endif}
             end;		
             IF (Temp AND $10 <> 0) THEN begin
             		INC (BreakInt [3]);                     {       Break Interrupt      }
             		{$ifdef Debug}
             		bochs_debugline('CTRL-Break or CTRL-C pressed during COM/modem port operation.');
             		{$endif}
             		ctrlbreak:=true; //Terminate communications
             		exit;
             end;		
           END;                                  
           
    end; // case
    writePORTb ((COM4 and LCR),(readportb(COM4 and LCR) AND $7F));    {Set THR, RHR & IER active  }
	
	 CASE (readPORTb (COM4 and IIR) AND $07) OF    

	0:begin
	 //       Delta clear to send
	 Temp := (readPORTb (COM4  and MSR));              {    MODEM STATUS CHANGES  (in bits)  }
     CD  [4] := ($80 AND Temp <> 0);         {       Carrier Detect       }
     CTS [4] := ($10 AND Temp <> 0);          {       Clear To Send        }
     DSR [4] := ($20 AND Temp <> 0);          {       Data Set Ready       }
     RI  [4] := ($40 AND Temp <> 0);          {       Ring Indicator       }
        
	end;
	   2 : BEGIN                       //Transmit register empty                                     
           IF x=length(buffer)  THEN                                                              
                writePORTb ((COM4 and IER),(readportb(COM4 and IER) AND $FD)) 
              //shut off transmitter
             ELSE  begin       //send next byte, but remove it from buffer                                                                   
                                                                       
               writePORTb ((COM4),(ord( Buffer[x])));  
               inc(x);
               
              END;                                                           
           END;                                                                
       4 : BEGIN               //Recieve buffer full                                                 
            IF (length(buffer) + 1) <> length(buffer)    //if we can add the character to the end of buffer      
             THEN                                                     
                  BEGIN                                                             
               		buffer[x] := readPORTb (COM4);      
               		inc(x);
               		
                  END                                                          
             ELSE        //read in, but not stored.
                BEGIN                                                            
	               IF readportb(Com4)  = $00 THEN { DO Nothing } exit;    
              END;                                                            
           END;                                                              
       6 : BEGIN                   //line status change          
             Temp :=(readPORTb (COM4 and LSR) AND $1E);                        {   Just count the errors    }
             IF (Temp AND $02 <> 0) THEN begin       {       Overrun Error        }
                    INC (Overrun [4]);                 
             		//writeportb(Com1,FlowStop);
             		if x=length(buffer) then x:=0;  //reset buffer
             		asm  //wait for incoming stream to match buffer size
             			nop
             			nop
             			nop
             			nop
             		end;	
             		//and try again.
             end;		
             IF (Temp AND $04 <> 0) THEN begin
             		INC (Parity [4]);                    {       Parity Error         }
             		//writeportb(Com1,FlowStop);
             		asm  //wait for hardware on remote end to catch up
             			nop
             			nop
             			nop
             			nop
             		end;	
             		
             		//rematch parity
//             		SetBaudRate(GetBaudrate,GetBitSize,GetParity,GetStopBits);  -some functions still being worked on.
             		//and retry
             end;		
             IF (Temp AND $08 <> 0) THEN begin
             		INC (Framing [4]);                   {       Framing Error        }
             		{$ifdef Debug}
             		bochs_debugline('Error making COM/Modem packet for some reason.');
             		{$endif}
             end;		
             IF (Temp AND $10 <> 0) THEN begin
             		INC (BreakInt [4]);                     {       Break Interrupt      }
             		{$ifdef Debug}
             		bochs_debugline('CTRL-Break or CTRL-C pressed during COM/modem port operation.');
             		{$endif}
             		ctrlbreak:=true; //Terminate communications
             		exit;
             end;		
           END;                                  
           
    end; // case
   
   end; //if connected 
   if ComInUse or CominUse3 then IMR:=4;
   if ComInUse2 or ComInUse4 then IMR:=3;
   writeportb($21,IMR);  //mask off IRQ. Interrupt Mask Register (IRQ3 or IRQ4)
   writeportb($20,$20);
//End IRQs  with 20=20.
end;


procedure InstallPorts;

var
   loop:integer;

begin
  writestring('Setup COMM Ports 1 thru 4(Available or not): ');
  tabulate;
  tabulate;
      CD  [1] := ($80 AND (readPORTb (COM1 and MSR)) <> 0);                   {-Carrier Detect status set  }
      CTS [1] := ($10 AND (readPORTb (COM1 and MSR)) <> 0);                   {-Clear To Send status set   }
      DSR [1] := ($20 AND (readPORTb (COM1 and MSR)) <> 0);                   {-Data Set Ready status set  }
      RI  [1] := ($40 AND (readPORTb (COM1 and MSR)) <> 0);                   {-Ring Indicator status set  }
      
      CD  [2] := ($80 AND (readPORTb (COM2 and MSR)) <> 0);                   {-Carrier Detect status set  }
      CTS [2] := ($10 AND (readPORTb (COM2 and MSR)) <> 0);                   {-Clear To Send status set   }
      DSR [2] := ($20 AND (readPORTb (COM2 and MSR)) <> 0);                   {-Data Set Ready status set  }
      RI  [2] := ($40 AND (readPORTb (COM2 and MSR)) <> 0);                   {-Ring Indicator status set  }
      
      CD  [3] := ($80 AND (readPORTb (COM3 and MSR)) <> 0);                   {-Carrier Detect status set  }
      CTS [3] := ($10 AND (readPORTb (COM3 and MSR)) <> 0);                   {-Clear To Send status set   }
      DSR [3] := ($20 AND (readPORTb (COM3 and MSR)) <> 0);                   {-Data Set Ready status set  }
      RI  [3] := ($40 AND (readPORTb (COM3 and MSR)) <> 0);                   {-Ring Indicator status set  }
      
      CD  [4] := ($80 AND (readPORTb (COM4 and MSR)) <> 0);                   {-Carrier Detect status set  }
      CTS [4] := ($10 AND (readPORTb (COM4 and MSR)) <> 0);                   {-Clear To Send status set   }
      DSR [4] := ($20 AND (readPORTb (COM4 and MSR)) <> 0);                   {-Data Set Ready status set  }
      RI  [4] := ($40 AND (readPORTb (COM4 and MSR)) <> 0);                   {-Ring Indicator status set  }
      
      for loop:=1 to 4 do begin
	      Framing [Loop] := 0;                                                      {-Reset framing error count  }
	      Overrun [Loop] := 0;                                                      {-Reset overrun error count  }
	      Parity [Loop] := 0;                                                       {-Reset parity error count   }
	      BreakInt [Loop] := 0;   
	  end;
  IOerror:=false;
  textcolor(green);
  writestrln('[ OK ]');
  textcolor(8);
  
end;

end.
