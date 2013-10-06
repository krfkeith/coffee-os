unit keybrd;
{ Keyboard handler and such.}

//the isr unit procedure should just get the scancode,readkey needs to be allowed to do the rest.
// and we need a FEW procedures to handle the codes themselves.
//This way we dont need to finish the keycode if..then list on keys pressed. The GUI
// will take what it wants, and the console, what it wants. Both have different needs.
//games unit is for games programming, do what you wish with the keys there. Does NOT require this unit.


interface

uses
   isr,irq,mouse;

type
  TKeyMap = array [0..127] of Char;
 { Implementation using single byte and bitwise operation

    bit 0 = Normal
    bit 1 = Either Shift
    bit 2 = Ctrl
    bit 3 = Alt
    bit 4 = scroll lock
    bit 5 = num lock[toggle keymaps on arrow keys]
    bit 6 = caps lock
    bit 7 = insert state[toggle cursor(mid string function or type-over function swap)]

}
  TKeyStatus = (Shift,Ctrl, Alt,ScrollLock ,NumLock, CapsLock,Insert);
  //This is a bit driven byte.
  //if (keystatus and 2) then CTRLPressed:=true
{Some specs say Right,then Left shift bits}

  TKeyStatusSet = set of TKeyStatus;

var
  CommandBuffer: String;
  KeyStatus: TKeyStatusSet; 
 
  ActiveKeyMap, ActiveShiftedKeyMap: TKeyMap;
  UseKeyMouse,KeyMouseEnabled:boolean;
  IsFuncKey,ScrollDisabled,NumKeys:boolean;
  Ack: Byte; //Ack byte recieved or keyboard controller will keep sending data to OS.
  scancode:longint; //integer?
  ch:char;

const
//this is what is displayed to the user
  USKeyMap: TKeyMap = (
    #00,{ 0 }
    #27,{ 1 - Esc }
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=',{ 13 }
    #08,{ 14 - Backspace }
    #09,{ 15 - Tab }
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']',{ 27 }
    #10,{ 28 - Enter }
    #00,{ 29 - Ctrl }
    'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',{ 39 }
    '''',{ 40 - ' }
    '`',{ 41 }
    #00,{ 42 - Left Shift }
    '\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',{ 53 }
    #00,{ 54 - Right Shift }
    '*',{ 55 }
    #00,{ 56 - Alt }
    ' ',{ 57 - Space bar }
    #0,{ 58 - Caps lock }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 59 - F1 up to 68 - F10 }
    #0,{ 69 - Num lock}
    #0,{ Scroll Lock }
    #0,{ Home key }
    #0,{ Up Arrow }
    #0,{ Page Up }
    '-',
    #0,{ Left Arrow }
    #0,
    #0,{ Right Arrow }
    '+',
    #0,{ 79 - End key}
    #0,{ Down Arrow }

    #0,{ Page Down }
    #0,{ Insert Key }
    #0,{ Delete Key }
    #0, #0, #0,{ 86 }
    #0,{ F11 Key }
    #0,{ F12 Key }
    #0,{ All other keys are undefined }
    #0,{ 90 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 100 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 110 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 120 }
    #0, #0, #0, #0, #0, #0, #0{ 127 }
    );

  ShiftedUSKeyMap: TKeyMap = (
    #00,{ 0 }
    #27,{ 1 - Esc }
    '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+',{ 13 }
    #08,{ 14 - Backspace }
    #09,{ 15 - Tab }
    'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}',{ 27 }
    #10,{ 28 - Enter }
    #00,{ 29 - Ctrl }
    'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':',{ 39 }
    '"',{ 40 - ' }
    '~',{ 41 }
    #00,{ 42 - Left Shift }
    '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',{ 53 }
    #00,{ 54 - Right Shift }
    '*',{ 55 - Numpad * }
    #00,{ 56 - Alt }
    ' ',{ 57 - Space bar }
    #0,{ 58 - Caps lock }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 59 - F1 up to 68 - F10 }
    #0,{ 69 - Num lock }
    #0,{ Scroll Lock }
    { 71 - 83 are numpad keys }
    #0,{ Home key (7) }
    #0,{ Up Arrow (8) }
    #0,{ Page Up (9) }
    '-',
    #0,{ Left Arrow (4) }
    #0,{ (5) }
    #0,{ Right Arrow (6) }
    '+',
    #0,{ End key (1) }
    #0,{ Down Arrow (2) }
    #0,{ Page Down (3) }
    #0,{ Insert Key (0) }
    #0,{ Delete Key (.) }
    { end of numpad keys }
    #0, #0, #0,{ 86 }
    #0,{ F11 Key }
    #0,{ F12 Key }
    { All other keys are undefined }
    #0, #0,{ 90 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 100 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 110 }
    #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,{ 120 }
    #0, #0, #0, #0, #0, #0, #0{ 127 }
    );

NumUSKeyMap: TKeyMap = (
    #00,{ 0 }
    #27,{ 1 - Esc }
    '1','2','3','4','5','6','7','8','9','0','-','=',{ 13 }
    #08,{ 14 - Backspace }
    #09,{ 15 - Tab }
    'q','w','e','r','t','y','u','i','o','p','[',']',{ 27 }
    #10,{ 28 - Enter }
    #00,{ 29 - Ctrl }
    'a','s','d','f','g','h','j','k','l',';',{ 39 }
    '''',{ 40 - ' }
    '`',{ 41 }
    #00,{ 42 - Left Shift }
    '\','z','x','c','v','b','n','m',',','.','/',{ 53 }
    #00,{ 54 - Right Shift }
    '*',{ 55 }
    #00,{ 56 - Alt }
    ' ',{ 57 - Space bar }
    #0,{ 58 - Caps lock }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 59 - F1 up to 68 - F10 }
    #0,{ 69 - Num lock}
    #0,{ Scroll Lock }
    // the magical num keys button happened.....POOF!
    '7',{ Home key }
    '8',{ Up Arrow }
    '9',{ Page Up }
    '-',
    '4',{ Left Arrow }
    #0,
    '6',{ Right Arrow }
    '+',
    '1',{ 79 - End key}
    '2',{ Down Arrow }
    '3',{ Page Down }
    '0',{ Insert Key }
    '.',{ Delete Key }
    #0,#0,#0,{ 86 }
    #0,{ F11 Key }
    #0,{ F12 Key }
    #0,{ All other keys are undefined }
    #0,{ 90 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 100 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 110 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 120 }
    #0,#0,#0,#0,#0,#0,#0{ 127 }
  );

//this is for emulated mouse function ONLY. COMPLETELY DISABLES KEYPAD.
MouseKeyMap:TKeyMap = (
    #00,{ 0 }
    #27,{ 1 - Esc }
    '1','2','3','4','5','6','7','8','9','0','-','=',{ 13 }
    #08,{ 14 - Backspace }
    #09,{ 15 - Tab }
    'q','w','e','r','t','y','u','i','o','p','[',']',{ 27 }
    #10,{ 28 - Enter }
    #00,{ 29 - Ctrl }
    'a','s','d','f','g','h','j','k','l',';',{ 39 }
    '''',{ 40 - ' }
    '`',{ 41 }
    #00,{ 42 - Left Shift }
    '\','z','x','c','v','b','n','m',',','.','/',{ 53 }
    #00,{ 54 - Right Shift }
    '*',{ 55 }
    #00,{ 56 - Alt }
    ' ',{ 57 - Space bar }
    #0,{ 58 - Caps lock }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 59 - F1 up to 68 - F10 }
    #0,{ 69 - Num lock}
    #0,{ Scroll Lock }
    // here comes the mouse.
    #00,{ Home key }
    #00,{ Up Arrow }
    #00,{ Page Up }
    #00,
    #00,{ Left Arrow }
    #0,
    #00,{ Right Arrow }
    #00,
    #00,{ 79 - End key}
    #00,{ Down Arrow }
    #00,{ Page Down }
    #00,{ Insert Key }
    #00,{ Delete Key }
    #0,#0,#0,{ 86 }
    #0,{ F11 Key }
    #0,{ F12 Key }
    #0,{ All other keys are undefined }
    #0,{ 90 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 100 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 110 }
    #0,#0,#0,#0,#0,#0,#0,#0,#0,#0,{ 120 }
    #0,#0,#0,#0,#0,#0,#0{ 127 }
);

const 
{ list of all scancode for keypressed }
//in case you are wondering where we got the scancodes from, it is this list.

{these values are in HEX..stand-by..will convert them..}
      KbShiftUp    = $f0;
      KbShiftLeft  = $f1;
      KbShiftRight = $f2;
      KbShiftDown  = $f3;
      KbShiftHome  = $f4;
      KbShiftEnd   = $f5;

   AltEsc      = $01;  {Alt+Esc = scancode 01, ascii code 0.}
   Esc         = $01;  {Esc     = scancode 01, ascii code 27.}

   //how to tell diff?
   AltSpace    = $02;
   CtrlIns     = $04;
   ShiftIns    = $05;
   CtrlDel     = $06;
   ShiftDel    = $07;
   AltBack     = $08;
   AltShiftBack= $09;
   ShiftTab    = $0F;

   AltLftBrack = $1A;
   AltRgtBrack = $1B;
   AltSemiCol  = $27;
   AltQuote    = $28;
   AltOpQuote  = $29;
   AltBkSlash  = $2B;
   AltComma    = $33;
   AltPeriod   = $34;
   AltSlash    = $35;
   AltGreyAst  = $37;
 
   Up          = $48;
   PgUp        = $49;
   Left        = $4B;
   Center      = $4C;
   Right       = $4D;
   AltGrayPlus = $4E;
   KeyEnd         = $4F;
   Down        = $50;
   PgDn        = $51;

   ShiftF1     = $54;
   ShiftF2     = $55;
   ShiftF3     = $56;
   ShiftF4     = $57;
   ShiftF5     = $58;
   ShiftF6     = $59;
   ShiftF7     = $5A;
   ShiftF8     = $5B;
   ShiftF9     = $5C;
   ShiftF10    = $5D;

   CtrlF1      = $5E;
   CtrlF2      = $5F;
   CtrlF3      = $60;
   CtrlF4      = $61;
   CtrlF5      = $62;

   CtrlF6      = $63;
   CtrlF7      = $64;
   CtrlF8      = $65;
   CtrlF9      = $66;
   CtrlF10     = $67;

   AltF1       = $68;
   AltF2       = $69;
   AltF3       = $6A;
   AltF4       = $6B;
   AltF5       = $6C;
   AltF6       = $6D;
   AltF7       = $6E;
   AltF8       = $6F;
   AltF9       = $70;
   AltF10      = $71;

   CtrlPrtSc   = $72;
   CtrlLeft    = $73;
   CtrlRight   = $74;
   Ctrlend     = $75;
   CtrlPgDn    = $76;
   CtrlHome    = $77;

   Alt1        = $78;
   Alt2        = $79;
   Alt3        = $7A;
   Alt4        = $7B;
   Alt5        = $7C;
   Alt6        = $7D;
   Alt7        = $7E;
   Alt8        = $7F;
   Alt9        = $80;
   Alt0        = $81;
   AltMinus    = $82;
   AltEqual    = $83;
   CtrlPgUp    = $84;

   ShiftF11    = $87;
   ShiftF12    = $88;
   CtrlF11     = $89;
   CtrlF12     = $8A;
   AltF11      = $8B;
   AltF12      = $8C;

   CtrlUp      = $8D;
   CtrlMinus   = $8E;
   CtrlCenter  = $8F;
   CtrlGreyPlus= $90;
   CtrlDown    = $91;
   CtrlTab     = $94;

   AltHome     = $97;
   AltUp       = $98;
   AltPgUp     = $99;
   AltLeft     = $9B;
   AltRight    = $9D;
   Altend      = $9F;
   AltDown     = $A0;
   AltPgDn     = $A1;
   AltIns      = $A2;
   AltDel      = $A3;

   AltTab      = $A5;

   kbLeftShift	=$AA;
   Missing =$00; //(hole in keypad.)


KeyCodeCtrlLeft  =29; 
KeyCodeShiftLeft =42;
KeyCodeBackSlashL=43;

KeyCodeComma     =51;
KeyCodeColon     =39; 
KeyCodeDot       =52;
 
KeyCodeMinus=12; 
KeyCodeOpenBr =26; 
KeyCodeDQuote    =40; 
KeyCodeSlash     =53;

KeyCodeEqual=13; 
KeyCodeCloseBr=27; 
KeyCodeBackSlashR=41; 
KeyCodeShiftRight=54;

 KeyCodeBack =14; 
//same as shitft F2/F3
 KeyCodePadTimes = 55;
 KeyCodeAltLeft  = 56;

 KeyCodePad7     = 71;   KeyCodePad4   = 75;  KeyCodePad1   = 79;
 KeyCodePad8     = 72;   KeyCodePad5   = 76;  KeyCodePad2   = 80;
 KeyCodePad9     = 73;   KeyCodePad6   = 77;  KeyCodePad3   = 81;
 KeyCodePadMinus = 74;   KeyCodePadPlus= 78;  KeyCodePad0   = 82;
                                              KeyCodePadDot = 83;

 KeyCodePadSlash = 86;

 KeyCode2PadEnter = 28;
 KeyCode2PadSlash = 53;

 KeyCode2CtrlRight= 29;

 KeyCode2Print    = 55;

 KeyCode2AltRight = 56;

 KeyCode2Home     = 71;
 KeyCode2Up       = 72;
 KeyCode2PgUp     = 73;
 KeyCode2Left     = 75;
 KeyCode2Right    = 77;
 KeyCode2End      = 79;
 KeyCode2Down     = 80;
 KeyCode2PgDown   = 81;
 KeyCode2Insert   = 82;
//weird keys, generate that two bytes
  kbdApps      = $FF17;

procedure KeyboardHandler(var r: TRegisters); 
procedure LoadKeyMap(const KeyMap, ShiftedKeyMap: TKeyMap);
procedure InstallKeyb;
procedure FlipNumPad;
procedure FlipKeyMouse;
procedure wait_keyboard;
procedure kbInit;
procedure EnableKeyb;
procedure disableKeyb;
function readkey:byte;
procedure ConsoleKeybHandler(scancode:byte);
procedure StartKeyMouse;
Procedure DoneKeyMouse;
procedure KeybIsMouseHandler(scancode:byte);
procedure return16; external name 'return16';
implementation

uses
  x86, console, UConsole,commands;

//Emulated Mouse functions

{keyboard mouse handler in case of no PS2 installed mouse(not likely)

FREEDOS incorrectly reports this when one is installed on IRQ12, as is normally the case.
IRQ12 isnt handled without a mouse 'driver' in 16-bit DOS. Maybe this is why?
}

procedure StartKeyMouse;
{Call me as soon as graph mode is set. Will show the mouse in the center of the screen.}

begin
   KeyMouseEnabled:=true;
   FlipKeyMouse; //adjust keyboard input to match mouse keys
  // ShowMouse;
end;

Procedure DoneKeyMouse;
{Call upon emulated keyb mouse no longer needed or graph mode exit.}
begin
   //HideMouse; 
   KeyMouseEnabled:=false; 
   FlipKeyMouse;  //restore arrow keys and like to normal functions.
end;

procedure KeybIsMouseHandler(scancode:byte);

begin
 
//these are the 'GREY keypad keys on 101 and 104 key keyboards..and present on SOME notebooks...
  
   case Scancode of //Double check with what the mouse cursur actually does..believe this is correct.
	     47,102:begin //Home
             	MX:=MX-1;
				MY:=MY-1;
				updateMouse;
	     end;
             
         107:begin //End
			MX:=MX+1;
			MY:=MY+1;
			updateMouse;	                
 	     end;

          104:begin //PgUp
           	MX:=MX+1;
			MY:=MY-1;
	        updateMouse;
	     end;
                          
          109:begin //PgDown
            	MX:=MX-1;
				MY:=MY+1;
				updateMouse;
   	     end;

	     72: begin // Up arrow
			MX:=oldMX;
			MY:=MY-1;
			updateMouse;
         end;

         80: begin // Down arrow
             	MX:=oldMX;
				MY:=MY+1;
				updateMouse;
	     end;     
             
         75: begin //Left
			MX:=MX-1;
			MY:=oldMY;
			updateMouse;
	     end;
		 
         77: begin //Right
			MX:=MX+1;
			MY:=oldMY;
			updateMouse;
	     end;
      end;//case	

//The other two buttons that matter, INSERT and DELETE are handled inside of the keyboard handler.

//DO NOT reset the interrupt here, we are still technically under the keyboard handler until exit.
end;


//END emulated mouse functions

procedure LoadKeyMap(const KeyMap, ShiftedKeyMap: TKeyMap);
begin
  ActiveKeyMap := KeyMap;
  ActiveShiftedKeyMap := ShiftedKeyMap;
end;


procedure FlipNumPad;

begin
if (NumKeys) then ActiveKeyMap:=NumUSKeyMap else if not (NumKeys) then ActiveKeyMap:=USKeyMap;
//flags are a heaven send.

end;

procedure FlipKeyMouse;

begin
//Only turn mouse on if we are using it.
	if (MouseEnabled=true) then ActiveKeyMap:=MouseKeyMap else if (MouseEnabled=false) then ActiveKeyMap:=USKeyMap;
//	if (MouseEnabled=true) and (MouseIsOK=false) then KeyMouseEnabled:=true; //internal to mouse handler
end;


//ALWAYS ACK after writing to port $64... $FA indicates ACK.
procedure kbInit; //Init keyboard

var
  k:longword;
begin
   enableKeyb;
   writeportb($64,$AA); //enable reset+self-test
   //Set all keys make+break(w/repeat 10.9 cps every 500 ms)
                 
	  if (readportb($60)<> $55)then begin 
		writestrln('Keyboard controller error.');
		writestrln('Halting OS.');
	    asm
			cli
			hlt
		end;
	 end;
end;

{
function Dncase(ch:char):char;
//curtesy 'mastering TP6'
//usage: if Dncase(ch) then 
//this performs opposite of Upcase(ch)
begin
 if ('A' <=CH) and ('Z'>=Ch)
   then Dncase:=Char(byte(ch)+32)
   else Dncase:=Ch;
end;

function Upcase(ch:char):char;
//curtesy 'mastering TP6'
//usage: if Dncase(ch) then 
//this performs opposite of Dncase(ch)
begin
 if ('a' <=CH) and ('z'>=Ch)
   then Upcase:=Char(byte(ch)-32)
   else Upcase:=Ch;
end;
}

//part of console locking methods..
//turn on/off keyboard.

procedure EnableKeyb;
begin
 writeportb($64,$AE);  
 readportb($60); //trash ACK byte sent
end;

procedure DisableKeyb;
begin
 writeportb($64,$AD);   
 readportb($60); //trash ACK byte sent
end;


procedure InstallKeyb;
begin
  WriteString('Installing Keyboard...');
  tabulate;
  tabulate;
  LoadKeyMap(USKeyMap, ShiftedUSKeyMap);
  kbInit; 
  ResetCommands;
 textcolor(Green);
  WriteStrLn('[ OK ]');
 textcolor(8);
end;

procedure wait_keyboard;
//wait for keyboard not busy 
var
   tmp : byte;

begin
//while data waiting, get data 
   while ((readportb($64) and 1) =1) do
          tmp := readportb($60);
end;

procedure KeyboardHandler(var r: TRegisters); 
//must ab-so-LUTE-LY MUST RETURN key pressed as well as (optionally) out put it to screen.

var
  scancode:byte;
  scancode1:longword;
  old,i,x,y:integer;

begin


 if ((readportb($64) and 1)=1) and ((readportb($64) and 5)<>1) then begin //(byte and x) indicates bit definition inside of a byte(cannot be other than 0,1)
      
//DO NOT proceed until a key event occurs and make sure it is not AUX(mouse) input.
//Mouse is on IRQ12..

//You can still call readkey by itself later on this way and still get the scancode.
// a separate call to ConsoleHandler will provide the appropriate keymapping and char.
//A little different from the past, but it works(and in 32-bit mode) without muddling with 16-bit
//variables or BIOS (400:0) hooks.
   
   
     if (ScanCode and 128)=0 then begin //Key Down
        //the reason for this: 1-we only want to handle keyDown events.
		//2-mouse input generally will be skipped until determined NOT from keyboard.
        KeyPressed:=true;
		Scancode:=readportb($60);
		ConsoleKeybHandler(scancode); //(else call GUIKeybHandler(scancode))
   end;
   
   if (ScanCode and 128)=1 then begin   // A key has been released
	  // Turn off 7th bit
	  Scancode:=readportb($60);
      ScanCode:=(ScanCode and not 128);
      //don't leave CTRL and ALT set after released.
      Exclude(KeyStatus,Alt);
      Exclude(KeyStatus,Ctrl);
      case ScanCode of
        42,54: begin
             Exclude(KeyStatus,Shift);
             ch:=USKeyMap[ScanCode];
        end;
	  end;{case}
	  KeyPressed:=false; //should be able to poll on interrupt to test.
	  ch:=#0;
	  Scancode:=0;
	  asm
		sti
	  end;
	  exit;
  end;{keyreleased}
 
  end{end keyb but something is waiting}
  else if ((readportb($64) and 1)=1) and ((readportb($64) and 5)=1) then begin;{feed the mouse...}
  asm
     cli
  end;
  //Let the mouse IRQ handle this.
  //MouseHandler(scancode)
   asm
		sti
	  end;
  end;
  //ELSE case is no key waiting for us, so exit.
end;

function readkey:byte;
//Useage: ch=:char(readkey);
//Invoking directly returns a byte, not a char.

var
  scancode:byte;

begin
   //get the key. This instance is invoked without keyboard handler.
   //This pulls it from and waits for key directly from BIOS.
   //Is there a way to do this in Protected Mode?
 
 
    //cant turn off interrupt if waiting on input.
    repeat
	until ((readportb($64) and 1)=1); //and ((readportb($64) and 5)=0);
	
	KeyPressed:=true;
    readkey:=readportb($60);
	
	{
    asm
	  cli;
    end;	  
    if (ScanCode and 128)=0 then begin //Key Down
        //the reason for this: 1-we only want to handle keyDown events.
		//2-mouse input generally will be skipped.
        KeyPressed:=true;
		readkey:=readportb($60);
		exit;
   end;
   
   if (ScanCode and 128)=1 then begin   // A key has been released
	  // Turn off 7th bit
	  Scancode:=readportb($60);
      ScanCode:=(ScanCode and not 128);
      //don't leave CTRL and ALT set after released.
      Exclude(KeyStatus,Alt);
      Exclude(KeyStatus,Ctrl);
      case ScanCode of
        42,54: begin
             Exclude(KeyStatus,Shift);
             ch:=USKeyMap[ScanCode];
        end;
	  end;
	  KeyPressed:=false; //should be able to poll on interrupt to test.
	  ch:=#0;
	  Readkey:=0;
	  asm
	    sti;
	  end;	
	  exit;
    end;
 
   }

end;

   
procedure ConsoleKeybHandler(scancode:byte);
//This does not need to be finished, not all keys are mapped in the console anyway..
//Most are plugged in at the application(ring3) level and not the kernel.
//this new separation also allows another function/procedure to grab the keys pressed by scancode shoulld the need
//arise so that specific keys can be caught later on. ('ALT+F4'...etc.)
var
  valuenum:integer; //since we cant use true or false with a 'or' operation, we use this instead.
  value:boolean;

begin
     if (scancode= $E0) or (scancode = $E1) or (scancode = $02) {laptop Fn key} or (scancode = $00) then begin
	IsFuncKey:=true;
        ScanCode:=Readkey; //read the next code
        case scancode of
  
       //PgUp and PgDn are scroll and reverse scroll (vidmem screen) [in window] 
      //Home and End need be programmed and windows MEta keys.
      //Put them here.  --Jazz
	
 //F1-F5 are in HEX (3B-3F)
	 59:begin //F1
	 end;

	 60:begin //F2
	 end;

	 61:begin //F3
	 end;

	 62:begin //F4
	 end;

	 63:begin //F5
	 end;

	 40:begin //F6
	 end;

	 41:begin //F7
	 end;

{         42:begin //F8
	 end;
 }
	 43:begin //F9
	 end;

	 44:begin //F10
         end;

         57:begin //F11
	 end;

         86:begin //F12
	 end;
         //reserved for MS keyboards
	 125,91:begin //Left MSWIN
	 end;
	 126,92:begin //Right MSWIN
	 end;
	 127,93:begin //Right Click Menu
	 end;

         //normal key and keypad keys
         98:begin //KeyPad Slash
         end;

         47,102:begin //Home
	    if KeyMouseEnabled=true then begin 
		KeybIsMouseHandler(scancode);
		exit;
	    end;
         end;
             
         104:begin //PgUp
	    if KeyMouseEnabled=true then begin 
		KeybIsMouseHandler(scancode);
		exit;
	    end;
         end;
             
         107:begin //End
	    if KeyMouseEnabled=true then begin 
		KeybIsMouseHandler(scancode);
		exit;
	    end;
         end;
             
         109:begin //PgDown
	    if KeyMouseEnabled=true then begin 
		KeybIsMouseHandler(scancode);
		exit;
	    end;
         end;
        
 
         119:begin //'Pause' key [ALT+Break]
         //e1 1d 45 e1 9d c5 on press, nothing on release
         	readportb($60);
         	readportb($60);
         	readportb($60);
         	readportb($60);
         	readportb($60);
		readportb($60);
         	//now do our function.
			  keypressed:=false;
              while not keypressed do //loop until keypressed(should deadlock until interrupt. see also: int 16, func 00 repeat until ah<>0)
      	 end;
         // Left AND Right Shift
         //shift and LED Lock keys are ok here.

         54,42:begin //SHIFTED 	
              if not(Shift in KeyStatus) THEN  Include(KeyStatus,Shift) 
                  else if (Shift in KeyStatus) THEN   Exclude(KeyStatus,Shift);
              ch:=ShiftedUSKeyMap[ScanCode];
      	 end;

         55:begin //PrintScreen key[111?]
           //move($C000,printbuf,sizeof($C000)); //64K move to printer buffer from screen.
		   //skip every other byte or indicate color somehow..
         end;
           58: begin      //CAPSLOCK 
		 //ok some more edits coming, misread the C.
		 
	        if not(CapsLock in KeyStatus) then begin
			Include(KeyStatus,CapsLock);
			 asm
        		           cli
	                 end;
          //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);

			// C BUGFIX: 121409 and 10132010 and 01232013 by Jazz
			value:= (CapsLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
			
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				WritePortb($60,(valuenum or $04));
         // Turns on Caps Lock light via controller in keyboard.See 'art of assembler'.
        		asm
        	           sti
			end;
        		readportb($60);
		end
		else if (CapsLock in KeyStatus) then begin
			 Exclude(KeyStatus,CapsLock);

			asm
        	           cli
        	        end;
			value:= (CapsLock in KeyStatus);
			
			
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum and $04));
          // Turns on Caps Lock light via controller in keyboard.See 'art of assembler'.
        		asm
	                   sti
			end;
	        	readportb($60);
  		end;

         //OK, you will see some redundant code here.LEAVE it. It is an very very old BIOS BUG workaround.
         //Allows LEDs to work properly.

	       if (CapsLock in KeyStatus) and (NumLock in KeyStatus) then begin
         //should keep LEDs from clearing other LEDs.
			asm
                 	  cli
            end;
			value:= (NumLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
			
  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);     
  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  				  
			WritePortb($60,(valuenum or $02));
			asm
                   	  sti
			end;
                        readportb($60);

              end;
	      if (CapsLock in KeyStatus) and (ScrollLock in KeyStatus) then begin
         //should keep LEDs from clearing other LEDs.
			asm
                 	  cli
                	end;
        	       value:= (NumLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
			
			
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				WritePortb($60,(valuenum or $01));
        		asm
                   	  sti
			end;
                        readportb($60);
              end;
       end;


	  //Num Lock
       69:begin
	        if not(NumLock in KeyStatus) then begin
			Include(Keystatus,NumLock);
	                if not(NumKeys) then NumKeys:=true;
	              // FlipNumPad;
	               asm
				cli
			end;
			value:= (NumLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
			
  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
			WritePortb($60,(valuenum or $02));
			asm
				sti
			end;
	               readportb($60);
		end else if (NumLock in KeyStatus) then begin
			Exclude(Keystatus,NumLock);
 		        if (NumKeys) then NumKeys:=false;
			asm
        	           cli
        	        end;
			value:= (NumLock in KeyStatus);
		  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum and $02));
        		asm
	                   sti
			end;
	        	readportb($60);

	       end;

	       if (NumLock in KeyStatus) and (CapsLock in KeyStatus) then begin
			asm
                 	  cli
                	end;
			value:= (CapsLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum or $04));
			asm
                   	  sti
			end;
                        readportb($60);

              end;
	      if (NumLock in KeyStatus) and (ScrollLock in KeyStatus) then begin
			asm
                 	  cli
                	end;
        	       value:= (ScrollLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1
			else if value=false then valuenum:=0;
		  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
				  WritePortb($60,$ED);
				    //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum or $01));
        		asm
                   	  sti
			end;
                        readportb($60);

              end;
        end;
        72: begin // Up arrow
		if KeyMouseEnabled=true then begin 
			KeybIsMouseHandler(scancode);
			exit;
		end;
                        if sizeof(CommandBuffer)>0 then begin
	     	  CommandBuffer:=PreviousCommand;
                  gotoxy(1,CurSorPosy);
	     	  clreol;
             	  writestring(CommandBuffer);
               end else begin
                   beep;       
	       end;	
        end;
          
        01: begin //ESC key
		ESCPressed:=true;
        end;
        75: begin //Left
		if KeyMouseEnabled=true then begin 
			KeybIsMouseHandler(scancode);
			exit;
		end;
        	while  (CurSorPosX>6) do begin	
			gotoxy(x-1,y); // Left arrow ,define a stop at beginning of line for this key.

			update_cursor; //update screen
		end;
	      beep;
	end;
        77: begin //Right
		if KeyMouseEnabled=true then begin 
			KeybIsMouseHandler(scancode);
			exit;
		end;
        
		while  CurSorPosX<length(CommandBuffer) do begin
			gotoxy(x+1,y); // Right arrow.otherwise this goes on forever.
			update_cursor;
		end;
		beep;
	end;
	     
        56:begin // alt;
              Include(KeyStatus,Alt);
		       
        end;
        29:begin//ctrl
             Include(KeyStatus,Ctrl);       	      
        end;

	70: begin //scroll lock hit
	        
                if not(ScrollDisabled) then begin
		     ScrollDisabled:=true;
                     Include(Keystatus,ScrollLock);
    	             asm
				cli
		     end;  
		     value:= (ScrollLock in KeyStatus); //based on this..... set the LED status.
		     if value=true then valuenum:=1 
		     else if value=false then valuenum:=0; 
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;   
		     WritePortb($60,$ED); 
  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  	        
			 WritePortb($60,(valuenum or $01));  
   	     	     asm
				sti
		     end;
                     pause;
		end
            else if (ScrollDisabled) then begin
				ScrollDisabled:=false;
                Exclude(Keystatus,ScrollLock);
                        asm
							cli
						end;   
			value:= (ScrollLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1 
			else if value=false then valuenum:=0; 
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
			WritePortb($60,$ED); 
	        	  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
		        WritePortb($60,(valuenum and $01));  
			asm
				sti
			end;
                        readportb($60);
		        if not ScrollDisabled then scroll else clearscreen;
	       end;

	       if (ScrollLock in KeyStatus) and (CapsLock in KeyStatus) then begin 
			    asm 
                 	  cli
                end;
			value:= (CapsLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1 
			else if value=false then valuenum:=0; 
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
			WritePortb($60,$ED); 
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum or $04));                 	    
			asm
                   	  sti
			end;
                        readportb($60);
                        
              end;
	      if (ScrollLock in KeyStatus) and (NumLock in KeyStatus) then begin 
			asm 
                 	  cli
                	end;
        	       value:= (ScrollLock in KeyStatus); //based on this..... set the LED status.
			if value=true then valuenum:=1 
			else if value=false then valuenum:=0; 
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
			WritePortb($60,$ED);
			  //WHOA! are we busy first?
		        while true do begin
				if (readportb($64) and 1)=1 then //wait to clear buffer
        		  break;
				end;  
        		WritePortb($60,(valuenum or $02));  
        		asm
                   	  sti
			    end;
                        readportb($60);
                        
              end;
	       
	end;
         
        80: begin // Down arrow
		if KeyMouseEnabled=true then begin 
			KeybIsMouseHandler(scancode);
			exit;
		end;
            if length(CommandBuffer)>0 then begin
	  		CommandBuffer:=NextCommand;
			gotoxy(1,CurSorPosy);
	     	  	clreol;
			//writestring('Coffee> ');
                	writestring(CommandBuffer);
                    end else begin
			beep;
		    end;
        end;     

     end;//case 
    end;//function keys

    case ScanCode of 
    //scancodes..if the above function keys listed dont catch..bring them HERE.
          82:begin //Insert pressed
		
		if KeyMouseEnabled=true then begin
			LeftButtonPressed:=true;
			exit;
		end;
	     
		if (Insert in Keystatus) then begin
                    Exclude(KeyStatus,Insert);		   
		    CursorNormal;
                end else begin
		    Include(Keystatus,Insert);		   
		    CursorBig; 
		end; 		
		
          end;

          83:begin //Delete key
		if KeyMouseEnabled=true then begin
			RightButtonPressed:=true;
			exit;
		end;
		if ((ctrl in keystatus) and (alt in keystatus)) then restart;
		   
                if (length(CommandBuffer)>0) and (CurSorPosx>11) then begin 
	       		 CommandBuffer:=CommandBuffer[Length(CommandBuffer)-1];
			 writechar(#32);
			 dec(CurSorPosX,1); 
			 
        	end;   
                
         end;

         28:begin //Enter                
		  if CurSorPosy>24 then begin
                      if ScrollDisabled then clearscreen else scroll; 
		  end;
		  cursorposX:=0;
		  inc(cursorposy);
                  ProcessCommand(CommandBuffer);
		  writestring('Coffee > ');  
                  if sizeof(CommandBuffer)>0 then begin
                         AddToHistory(CommandBuffer);
                         CommandBuffer:='';

                  end;
                  CommandBuffer:='';
         	  update_cursor;           
          end;
	 
	  14: begin // Backspace 

		if (length(CommandBuffer)>0) and (CurSorPosx>8) then begin
			Delete(CommandBuffer,(length(CommandBuffer)),1);
			dec(CurSorPosX,1);	 
			writechar(#32);		 
			dec(CurSorPosX,1);			 			 
		
        	end;   
     	  end;
          15: begin //TAB
			tabulate; //anyone want some tab?
	  end;
	
    end; //scancode

    //get the rest of the keyboard keys
    if (CapsLock in KeyStatus) then ch:=ShiftedUSKeyMap[ScanCode];
         ch:=USKeyMap[ScanCode];
    case ch of
      
       #32 .. #126:begin // Other characters, non function key
           if length(CommandBuffer)<=255 then begin // ShortString limit
//add ch (keypress) to commandbuffer. MUST DO this or Commandbuffer comes up empty when ENTER/RETURN is hit.
	      CommandBuffer:=CommandBuffer+ch;
              
          //Did we ask for password secrecy?
         if EchoToConsole=true then  writechar(ch);      
          
       end else if length(CommandBuffer)>=255 then begin  
            inc(CurSorPosy);
	   CurSorPosx:=1;
           textcolor(Red);
           writestrln(' Maximum command length is 255!');
	   textcolor(7);
	   writestring('Coffee > ');
       end;        
     end;
  end;

end;

end.

