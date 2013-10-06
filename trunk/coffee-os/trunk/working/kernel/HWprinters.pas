//Hardware printer support UNIT --COFFEE OS.
Unit HWprinters;
{
Unit needs some testing, sould be complete by now.

For example, a Pentium based PC is capable of sending several hundred million characters a second to a printer, but that printer is (probably) unable to print that many characters each second. 


 USB printer support will be added here later when USB support is added in.
}

interface
uses
   isr,irq;
type 
  str80=string[80];

const 
//from 8088 Bios sources

   LPT1=$378;
   LPT2=$278;
   LPT3=$3BC;
   //LPT2 (and 3) Ignored, not normally used.

{
LPT1:

 the data register is at I/O address 378, (port+0)
 the status register is at I/O address 379, (port+1)
 and the control register is at I/O address 37A. (port+2)


LPT2:

 the data register is at I/O address 278, (port+0)
 the status register is at I/O address 279, (port+1)
 and the control register is at I/O address 27A. (port+2)


LPT3:

 the data register is at I/O address 3BC, (port+0)
 the status register is at I/O address 3BD, (port+1)
 and the control register is at I/O address 3BE. (port+2)


Functions (Pretty Basic) (See also: Dos INT 17)

 Subfunction 0 -- Print the character in AL to the printer.  Printer status is
         returned.  If bit #0 = 1 then a timeout error occurred.

 Subfunction 1 -- Initialize printer.  
 Subfunction 2 -- Return printer status.

 Note that the hardware returns bit 3 with zero if an error has occurred,
 with one if there is no error(IOError).  The software normally inverts this bit
 before returning it to the caller.

 Data (register) output port- 8-bit data/ char to print is transmitted to the printer via this port.


	Compatibility mode or "Centronics Mode" as it is commonly known, can only send data in the
forward direction at a typical speed of 50 kbytes per second but can be as high as 150+ kbytes a
second. In order to receive data, you must change the mode to either Nibble or Byte mode. Nibble
mode can input a nibble (4 bits) in the reverse direction. E.g. from device to computer. Byte mode
uses the Parallel's bi-directional feature (found only on some cards) to input a byte (8 bits) of data in
the reverse direction.

Dont forget once FF(on a line) to send appropriate page eject codes for the printer.

Input status port(3BD):

	Bit 7 Busy
	Bit 6 Ack
	Bit 5 Paper Out
	Bit 4 Select In
	Bit 3 Error
	Bit 2 IRQ (INVERSE)
	Bit 1 Reserved
	Bit 0 Reserved

 Output control port(3BE):

	Bit 7 Unused
	Bit 6 Unused
	Bit 5 Enable bi-directional Port
	Bit 4 Enable IRQ Via Ack Line
	Bit 3 Select Printer
	Bit 2 Initialize Printer (Reset)
	Bit 1 Auto Linefeed
	Bit 0 Strobe


Standard & Bidirectional

	(SPP) Mode is the bi-directional mode. Using this mode, bit 5 of the Control Port will
reverse the direction of the port, so you can read back a value on the data lines.

	EPP1.9 and SPP Mode is just like the previous mode, only it uses EPP Version 1.9 this time.
As in the other mode, you will have access to the SPP registers, including Bit 5 of the control port.
However this differs from EPP1.7 and SPP Mode as you should have access to the EPP Timeout bit.
ECP Mode will give you an Extended Capabilities Port. The mode of this port can then be set
using the ECP’s Extended Control Register (ECR). However in this mode from BIOS the EPP Mode
(100) will not be available. We will further discuss the ECP’s Extended Control Register in this
document, but if you want further information on the ECP port, consult Interfacing the Extended
Capabilities Port3.

	ECP and EPP1.7 Mode & ECP and EPP1.9 Mode will give you an Extended Capabilities
Port, just like the previous mode. However the EPP Mode in the ECP’s ECR will now be available.
Should you be in ECP and EPP1.7 Mode you will get an EPP1.7

	The EPP1.7 mode had a few problems in regards to the Data and Address Strobes being asserted to start a cycle
regardless of the wait state, thus this mode if not typically used now. Best set your Parallel Port to
ECP and EPP1.9 Mode.

	When set to ECP Mode, a new set of registers become available at Base + 0x400h. A
discussion of these registers are available in Interfacing the Extended Capabilities Port3. Here we are
only interested in the Extended Control Register (ECR) which is mapped at Base + 0x402h. It should
be stated that the ECP’s registers are not available for port’s with a base address of 0x3BCh.

	Under some of the modes, the SPP registers may disappear or not work correctly. If you
are using SPP, then set the ECR to Standard Mode. This is one of the most common mistakes that
people make.

ECR reg:

Bit Function (7-5 Selects Current Mode of Operation)

	000 Standard Mode
	001 Byte Mode
	010 Parallel Port FIFO Mode
	011 ECP FIFO Mode
	100 EPP Mode
	101 Reserved
	110 FIFO Test Mode
	111 Configuration Mode

	4 ECP Interrupt Bit(1/0)
	3 DMA Enable Bit(1/0)
	2 ECP Service Bit(1/0)
	1 FIFO Full(1/0)
	0 FIFO Empty(1/0)

}

procedure ClearLPRError;
procedure CheckStatusPrinter;
function InitPrinter:boolean; //either it inited ok or not.
procedure WaitForCharPrinted1;
procedure WaitForCharPrinted2;
procedure WaitForCharPrinted3;
procedure PrintChar1(var ch:char);
procedure PrintChar2(var ch:char);
procedure PrintChar3(var ch:char);
procedure PrintLine(var line:str80); //need paper line dimensions(and word wrapping?)
procedure Printers(var r: TRegisters); //interrupt handler


implementation

uses
 x86,console,uconsole;

var
  IRQSet,IOError,Selected,PaperOut,PrinterAck,PrinterBusy,Timeout:boolean;
  ctrlbreak,connected:boolean;

procedure ClearLPRError;
//call just before finishing interrupt
var
     statusbyte:byte;

 begin
	 statusbyte:=readportb($378);
	 writeportb (($37E),((statusbyte and 3)xor $08));
	 statusbyte:=readportb($278);
	 writeportb (($27E),((statusbyte and 3)xor $08));
	 statusbyte:=readportb($3BC);
	 writeportb (($3BE),((statusbyte and 3)xor $08));

 end;


procedure CheckStatusPrinter;
//we treat ALL printers the same, as we do not know if there is more than one.
//why use more than one LPT based one anyways these days??
var
   status,status2,status3:byte;

begin

 status:=readportb($378);
 status2:=readportb($278);
 status3:=readportb($3bc);

// First TWO bits CANNOT be utilized.
 if (status and 2)=0 then IRQSet:=true;//unused
 if (status and 3)=1 then IOError:=true;
 if (status and 4)=1 then Selected:=true; //is it ONLINE?
//next three self-explanitory
 
 if (status and 5)=1 then PaperOut:=true;
 if (status and 6)=0 then PrinterAck:=true; //hmmm...
 if (status and 7)=0 then PrinterBusy:=true;

// First TWO bits CANNOT be utilized.
 if (status2 and 2)=0 then IRQSet:=true;//unused
 if (status2 and 3)=1 then IOError:=true;
 if (status2 and 4)=1 then Selected:=true; //is it ONLINE?
//next three self-explanitory
 
 if (status2 and 5)=1 then PaperOut:=true;
 if (status2 and 6)=0 then PrinterAck:=true; //hmmm...
 if (status2 and 7)=0 then PrinterBusy:=true;

// First TWO bits CANNOT be utilized.
 if (status3 and 2)=0 then IRQSet:=true;//unused
 if (status3 and 3)=1 then IOError:=true;
 if (status3 and 4)=1 then Selected:=true; //is it ONLINE?
//next three self-explanitory
 
 if (status3 and 5)=1 then PaperOut:=true;
 if (status3 and 6)=0 then PrinterAck:=true; //hmmm...
 if (status3 and 7)=0 then PrinterBusy:=true;

end;


function InitPrinter:boolean; //either it inited ok or not.

var
   exists1,exists2,exists3:boolean;	
   status:byte;
begin
 
           writeportb($378,$52);
	   status:=readportb($378);

	   if (status xor status ) > 0 then 
   		exists1:=false else exists1:=true;

	   if exists1=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT1);
			exit;              //Quit if no such printer.
   	   end;


           writeportb($278,$52);
	   status:=readportb($278);

	   if (status xor status ) > 0 then 
   		exists2:=false else exists2:=true;

	   if exists2=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT2);
			exit;              //Quit if no such printer.
   	   end;

           writeportb($3BC,$52);
	   status:=readportb($3BC);

	   if (status xor status ) > 0 then 
   		exists3:=false else exists3:=true;

	   if exists3=false then begin
			writestrln('No Printer available port:  ');
			writelong(LPT3);
			exit;              //Quit if no such printer.
   	   end;


           CheckStatusPrinter;                    //Read current status.

//we WANT it like THIS.
  
{
// Strobe:=(writeStatus and 0) =1;
// Feed:=(writeStatus and 1)=1;
// Init:=(writeStatus and 2)=0;
// Selected:=(writeStatus and 3)=1;
// IRQOn:=(writeStatus and 4)=1;
// BiDirection:=(writeStatus and 5)=0; //write(out)
// Unused1:=(writeStatus and 6)=1;
// Unused2:=(writeStatus and 7) =1;
}

//writestatus:=$DB; --above thrown into HEX.

           if exists1=true then writeportb($378,$DB);
           if exists2=true then writeportb($278,$DB);
           if exists3=true then writeportb($3BC,$DB);
        
   //need to check the status of printer.
   CheckStatusPrinter;
   if IOError then begin
	ClearLPRError;
	exit;
   end;

//deal with other scenarios....

   InitPrinter:=boolean(not IOerror);
end;

procedure WaitForCharPrinted1;

var
	i:longint; 
	status:byte;	
begin
i:=15000000;
repeat
	status:=readportb($379);
	status:=status and $80; //clear Z flag
	Dec(i);
until ((status and $80)<>0) or (i=0);
	status:=readportb($379);
	if (status and $80)<>0 then begin
		IOError:=true;
		Timeout:=true;
        end;
end;

procedure WaitForCharPrinted2;

var
	i:integer; 
	status:byte;	
begin
i:=15000000;
repeat
	status:=readportb($279);
	status:=status and $80; //clear Z flag
	Dec(i);
until ((status and $80)<>0) or (i=0);
	status:=readportb($279);
	if (status and $80)<>0 then begin
		IOError:=true;
		Timeout:=true;
        end;
end;

procedure WaitForCharPrinted3;

var
	i:integer; 	
        status:byte;
begin
i:=15000000;
repeat
	status:=readportb($3BD);
	status:=status and $80; //clear Z flag
	Dec(i);
until ((status and $80)<>0) or (i=0);
	status:=readportb($3BD);
	if (status and $80)<>0 then begin
		IOError:=true;
		Timeout:=true;
        end;
end;

procedure PrintChar1(var ch:char);
//prints a char onto the paper in the printer's bin
//very generic (for LPT-port based printers) code
label
	GotAck;
var
   status:byte;
   i:longint;
begin

asm
	cli
end;

writeportb($378,(byte(ch)));  //ord(ch)  --put the byte in

// The following code checks to see if an acknowlege was received from
// the printer.  If this code waits too long, a time-out error is returned.
// Acknowlege is supplied in bit #7 of the printer status port.
               
status:=readportb($378);
if (status and $80)=1 then goto GotAck;

i:=65536;
repeat 
   if (status and $80)=1 then goto GotAck;
   status:=status and $80; //clear Z flag
   dec(i);
until i=0;
if (status and $80)=1 then goto GotAck;
exit;

GotAck:

writeportb( $378,(byte(ch)) ); 

//wait 16 cycles...
	asm
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;

{ now we strobe.

 Also note that this clears bit 5 of the
 control port.  This ensures that the port continues to operate as an
 output port if it is a bidirectional device.  This code also clears bits
 six and seven which IBM claims should be left zero.
}
	status:=readportb($37A); // Get the current port setting. 
	writeportb( $37A,(status and 0) or 1); // Set the strobe line high.  
	 
	 asm

//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
         end;
	 writeportb( $37A,(status and 0) or 1); // Set the strobe line low. 
	asm
//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;
 	writeportb( $37A,(status and 0) and $FE); // Set the strobe line low. 
  	
        asm
		sti
	end;
         if ctrlbreak=true then exit;
         if connected=false then exit;     

end;

procedure PrintChar2(var ch:char);
//prints a char onto the paper in the printer's bin
//very generic (for LPT-port based printers) code
label
	GotAck2;

var
   status:byte;
   i:longint;

begin

asm
	cli
end;

writeportb($278,(byte(ch)));  //ord(ch)  --put the byte in

// The following code checks to see if an acknowlege was received from
// the printer.  If this code waits too long, a time-out error is returned.
// Acknowlege is supplied in bit #7 of the printer status port.
               
status:=readportb($278);
if (status and $80)=1 then goto GotAck2;

i:=65536;
repeat 
   if (status and $80)=1 then goto GotAck2;
   status:=status and $80; //clear Z flag
   dec(i);
until i=0;
if (status and $80)=1 then goto GotAck2;
exit;

GotAck2:

writeportb( $278,(byte(ch)) ); 

//wait 16 cycles...
	asm
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;

{ now we strobe.

 Also note that this clears bit 5 of the
 control port.  This ensures that the port continues to operate as an
 output port if it is a bidirectional device.  This code also clears bits
 six and seven which IBM claims should be left zero.
}
	status:=readportb($27A); // Get the current port setting. 
	 writeportb( $27A,(status and 0) or 1); // Set the strobe line high.  
	 
	 asm

//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
         end;
	 writeportb( $27A,(status and 0) or 1); // Set the strobe line low. 
	asm
//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;
 	writeportb( $27A,(status and 0) and $FE); // Set the strobe line low. 
  	
        asm
		sti
	end;
         if ctrlbreak=true then exit;
         if connected=false then exit;     

end;

procedure PrintChar3(var ch:char);
//prints a char onto the paper in the printer's bin
//very generic (for LPT-port based printers) code
label
	GotAck3;

var
   status:byte;
   i:longint;

begin

asm
	cli
end;

writeportb($3BC,(byte(ch)));  //ord(ch)  --put the byte in

// The following code checks to see if an acknowlege was received from
// the printer.  If this code waits too long, a time-out error is returned.
// Acknowlege is supplied in bit #7 of the printer status port.
               
status:=readportb($3BC);
if (status and $80)=1 then goto GotAck3;

i:=65536;
repeat 
   if (status and $80)=1 then goto GotAck3;
   status:=status and $80; //clear Z flag
   dec(i);
until i=0;
if (status and $80)=1 then goto GotAck3;
exit;

GotAck3:

writeportb( $3BC,(byte(ch)) ); 

//wait 16 cycles...
	asm
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;

{ now we strobe.

 Also note that this clears bit 5 of the
 control port.  This ensures that the port continues to operate as an
 output port if it is a bidirectional device.  This code also clears bits
 six and seven which IBM claims should be left zero.
}
	status:=readportb($3BD); // Get the current port setting. 
	 writeportb( $3BD,(status and 0) or 1); // Set the strobe line high.  
	 
	 asm

//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
         end;
	 writeportb( $3BD,(status and 0) or 1); // Set the strobe line low. 
	asm
//wait 16 cycles...
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
                nop
		nop
		nop
		nop
		nop
		nop
	end;
 	writeportb( $3BD,(status and 0) and $FE); // Set the strobe line low. 
  	
        asm
		sti
	end;
         if ctrlbreak=true then exit;
         if connected=false then exit;     

end;

procedure PrintLine(var line:str80); //need paper line dimensions(and word wrapping?)
//checks which printer is available and then prints to the correct one.
var

   i:integer;
   exists1,exists2,exists3:boolean;
   status:byte;
begin
 
           writeportb($378,$52);
	   status:=readportb($378);

	   if (status xor status ) > 0 then 
   		exists1:=false else exists1:=true;

	   if exists1=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT1);
			exit;              //Quit if no such printer.
   	   end;


           writeportb($278,$52);
	   status:=readportb($278);

	   if (status xor status ) > 0 then 
   		exists2:=false else exists2:=true;

	   if exists2=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT2);
			exit;              //Quit if no such printer.
   	   end;

           writeportb($3BC,$52);
	   status:=readportb($3BC);

	   if (status xor status ) > 0 then 
   		exists3:=false else exists3:=true;

	   if exists3=false then begin
			writestrln('No Printer available port:  ');
			writelong(LPT3);
			exit;              //Quit if no such printer.
   	   end;

	i:=0;
	repeat	        
                if exists1=true then  begin 
			printchar1(line[i]);
			waitForCharPrinted1;
		end; 
                if exists2=true then begin
			printchar2(line[i]);			
			waitForCharPrinted2;
		end; 
                if exists3=true then begin
		        printchar3(line[i]);
			waitForCharPrinted3;
                end; 
	        inc(i);
	until i=length(line);
	//PrintChar(char(07));        --print line feed char

//end of page is two line feeds, then a fom feed.
end;


procedure Printers(var r: TRegisters); //interrupt handler

//this is only called on LPR (LPT:) interrupts.
var
  status:byte;
   exists1,exists2,exists3:boolean;

begin
	asm
		cli
	end;

     if ctrlbreak=true then exit;

//should be done after everyIO operation.Checks the status of the Printer.

           writeportb($378,$52);
	   status:=readportb($378);

	   if (status xor status ) > 0 then 
   		exists1:=false else exists1:=true;

	   if exists1=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT1);
			exit;              //Quit if no such printer.
   	   end;


           writeportb($278,$52);
	   status:=readportb($278);

	   if (status xor status ) > 0 then 
   		exists2:=false else exists2:=true;

	   if exists2=false then begin
			writestring('No Printer available port:  ');
			writelongln(LPT2);
			exit;              //Quit if no such printer.
   	   end;

           writeportb($3BC,$52);
	   status:=readportb($3BC);

	   if (status xor status ) > 0 then 
   		exists3:=false else exists3:=true;

	   if exists3=false then begin
			writestrln('No Printer available port:  ');
			writelong(LPT3);
			exit;              //Quit if no such printer.
   	   end;


	CheckStatusPrinter;
	
	//IRQs already setup with writeportb(20,20).
	asm
	   sti
	end;

end;


end.
