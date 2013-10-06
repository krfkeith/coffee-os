{Mouse Unit
Notes:

I dont know how many times I have tried to get mice working and for some reason either they failed completely or there was no driver for them.Worst case is that DOS/FREEDOS refuses to provide one for a program or GUI I need.
In *nix, gdm (ugly ugly) is supposed to take over, but without X11...good luck on that.
We should always provide a mouse. Mice make light work of a lot of tasks and there is no need
even with a TUI or Function bar on botton as to why we cant have one, even in TEXT mode.

Cleaned up some mis-behaving code.

--Jazz
}


unit mouse;

{
AH FINALLY! GASP! Mouse support....

from Mouse.inc by SANiK.
 converted from C to Pascal by JAzz.  
With nice cursors.  :-)

}

//These are Generic mouse specs.
//Note the common Icons are here also, saving files from having them.(Quickload internal RES)

interface
uses
  isr;

//this is size of pointer
const mouse_ptr_maxx = 31; //normal size, allow smaller and bigger
      mouse_ptr_maxy = 31;
      largemouse_ptr_maxx = 63;
      largemouse_ptr_maxy = 63;
      smallmouse_ptr_maxx = 15;
      smallmouse_ptr_maxy = 15;

var
  bytenum,MX,MY,OldMX,OldMY,x,y,vert,horiz:integer;
  LeftButtonPressed,LeftButtonReleased,mouselock,RightButtonPressed,RightButtonReleased,mouseisvisible:boolean;
  MidButtonPressed,MidButtonReleased,WheelMoved:boolean;
  MouseExists:boolean;


type 
   tmouse_ptr = array[0..mouse_ptr_maxx, 0..mouse_ptr_maxy] of longint;
   largetmouse_ptr = array[0..largemouse_ptr_maxx, 0..largemouse_ptr_maxy] of longint;
   smalltmouse_ptr = array[0..smallmouse_ptr_maxx, 0..smallmouse_ptr_maxy] of longint;
 
   Cursor= Array[0..15,0..15] of longint; 
   CursorType = Array[0..15,0..15] of longint;   { to store the cursor shape-16x16 size icons.internalised for now. }
   OldIcon =Array[1..8,1..8] of longint;
   Icon = Array[1..8,1..8] of longint;  //see implementation below.8x8 size icons.

//A pseudo Python-like Pascal button event...
   buttonBox=record
	X1:integer;
	Y1:integer;
	X2:integer;	
	Y2:integer;
	text:Pchar; //button text. Maybe string.
	event:procedure;    //button.bind in python. Defines what to do.
   end;
//button.event:=kclearscreen; //self.die, self.ok .....etc.

var 

{Every number represents a colour}
{98 = Transparent. 0 = Black 31 = White}
{for other colors see charts}

HourGlassScreen:Icon = ((98,31,31,31,31,31,31,98),
			(98,0,98,98,98,31,0,98), 
                        (98,0,98,98,98,31,0,98),
                        (98,0,0,31,31,0,0,98),
                        (98,0,0,31,31,0,0,98),
                        (98,0,98,98,98,98,0,98),
                        (98,0,98,98,98,98,0,98),
			(98,31,31,31,31,31,31,98));
			//hard to see. looks about right.


HourGlassScreen2 : CursorType =
   ((0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31), 
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31), 
   (31,0,0,0,0,0,0,0,0,0,0,0,0,0,31,31), 
   (31,31,0,0,0,98,98,98,98,98,0,0,0,31,31,31), 
   (31,31,31,0,0,0,98,98,98,0,0,0,31,31,31,31), 
   (31,31,31,31,0,0,0,98,0,0,0,31,31,31,31,31), 
   (31,31,31,31,31,0,0,0,0,0,31,31,31,31,31,31), 
   (31,31,31,31,31,31,0,0,0,31,31,31,31,31,31,31), 
   (31,31,31,31,31,0,0,0,0,0,31,31,31,31,31,31), 
   (31,31,31,31,0,0,0,98,0,0,0,31,31,31,31,31), 
   (31,31,31,0,0,0,98,98,98,0,0,0,31,31,31,31), 
   (31,31,0,0,0,98,98,98,98,98,0,0,0,31,31,31), 
   (31,0,0,0,0,0,0,0,0,0,0,0,0,0,31,31), 
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31), 
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31),
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) 

); 


HourGlassMask:CursorType=
 
   ((0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
   (0,31,31,31,31,31,31,31,31,31,31,31,31,31,0,0),    
   (0,0,31,0,0,0,0,0,0,0,0,0,31,0,0,0), 
   (0,0,0,0,31,0,0,0,0,0,0,31,0,0,0,0),   
   (0,0,0,0,0,31,0,0,0,0,31,0,0,0,0,0),
   (0,0,0,0,0,0,31,0,0,31,0,0,0,0,0,0),
   (0,0,0,0,0,0,31,0,31,0,0,0,0,0,0,0),   
   (0,0,0,0,0,0,0,31,0,0,0,0,0,0,0,0),
   (0,0,0,0,0,0,31,0,31,0,0,0,0,0,0,0),
   (0,0,0,0,0,0,31,0,0,31,0,0,0,0,0,0),
   (0,0,0,0,0,31,0,0,0,0,31,0,0,0,0,0),
   (0,0,0,0,31,0,0,0,0,0,0,31,0,0,0,0),
   (0,0,31,0,0,0,0,0,0,0,0,0,31,0,0,0), 
   (0,31,31,31,31,31,31,31,31,31,31,31,31,31,0,0),    
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)); 
   

MousePointer : CursorType =  
((31,0,0,0,31,0,0,31,31,31,31,31,31,31,31,31), 
(31,0,0,0,0,31,0,31,31,31,31,31,31,31,31,31), 
(31,0,0,0,0,0,31,31,31,31,31,31,31,31,31,31),
(31,0,0,0,0,0,31,31,31,31,31,31,31,31,31,31),  
(31,0,0,0,0,0,31,0,31,31,31,31,31,31,31,31),    
(31,0,0,0,0,0,0,0,0,31,31,31,31,31,31,31),    
(31,0,0,0,0,0,0,0,0,0,31,31,31,31,31,31),    
(31,0,0,0,0,0,0,0,0,0,31,31,31,31,31,31),
(31,0,0,0,0,0,0,0,0,0,0,31,31,31,31,31), 
(31,0,0,0,0,0,0,0,0,0,0,31,31,31,31,31), 
(31,0,0,0,0,31,0,0,0,31,31,31,31,31,31,31),
(31,0,0,0,0,31,0,0,0,31,31,31,31,31,31,31),  
(31,0,0,0,31,0,0,0,0,0,31,31,31,31,31,31),    
(31,31,31,31,0,31,0,0,0,0,31,31,31,31,31,31),    
(31,31,31,31,0,31,0,0,0,0,31,31,31,31,31,31),    
(31,31,31,31,0,31,31,0,0,0,0,31,31,31,31,31)); 
   

MousePointerMask:CursorType=
((0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
(0,0,31,0,0,0,0,0,0,0,0,0,0,0,0,0), 
(0,0,31,31,0,0,0,0,0,0,0,0,0,0,0,0),
(0,0,31,31,31,0,0,0,0,0,0,0,0,0,0,0), 
(0,0,31,31,31,31,0,0,0,0,0,0,0,0,0,0), 
(0,0,31,31,31,31,31,0,0,0,0,0,0,0,0,0), 
(0,0,31,31,31,31,31,31,0,0,0,0,0,0,0,0), 
(0,0,31,31,31,31,31,31,31,0,0,0,0,0,0,0), 
(0,0,31,31,31,31,31,31,31,31,0,0,0,0,0,0), 
(0,0,31,31,31,31,31,0,0,0,0,0,0,0,0,0), 
(0,0,31,31,0,31,31,0,0,0,0,0,0,0,0,0), 
(0,0,31,0,0,0,31,31,0,0,0,0,0,0,0,0), 
(0,0,0,0,0,0,31,31,0,0,0,0,0,0,0,0), 
(0,0,0,0,0,0,0,31,31,0,0,0,0,0,0,0), 
(0,0,0,0,0,0,0,31,31,0,0,0,0,0,0,0), 
(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)); 

{
//calc convert to something I can actually see.

ArrowScreenMask : ($1FFF,$0FFF,$07FF,$03FF,
                   $01FF,$00FF,$007F,$003F,
                   $001F,$003F,$01FF,$01FF,
                   $E0FF,$F0FF,$F8FF,$F8FF);
    
ArrowCursorMask : ($0000,$4000,$6000,$7000,
                   $7800,$7C00,$7E00,$7F00,
                   $7F80,$7C00,$4C00,$0600,
                   $0600,$0300,$0400,$0000);

sweet@1,1

CheckScreenMask : ($FFF0,$FFE0,$FFC0,$FF81,
                   $FF03,$0607,$000F,$001F,
                   $803F,$C07F,$E0FF,$F1FF,
                   $FFFF,$FFFF,$FFFF,$FFFF);
        
CheckCursorMask : ($0000,$0006,$000C,$0018,
                   $0030,$0060,$70C0,$3980,
                   $1F00,$0E00,$0400,$0000,
                   $0000,$0000,$0000,$0000);

sweet@5,10


CrossScreenMask : ($F01F,$E00F,$C007,$8003,
                   $0441,$0C61,$0381,$0381,
                   $0381,$0C61,$0441,$8003,
                   $C007,$E00F,$F01F,$FFFF);

CrossCursorMask : ($0000,$07C0,$0920,$1110,
                   $2108,$4004,$4004,$783C,
                   $4004,$4004,$2108,$1110,
                   $0920,$07C0,$0000,$0000);

sweet@7,7


GloveScreenMask : ($F3FF,$E1FF,$E1FF,$E1FF
                   $E1FF,$E049,$E000,$8000,
                   $0000,$0000,$07FC,$07F8,
                   $9FF9,$8FF1,$C003,$E007);

GloveCursorMask : ($0C00,$1200,$1200,$1200,
                   $1200,$13B6,$1249,$7249,
                   $9249,$9001,$9001,$8001,
                   $4002,$4002,$2004,$1FF8)

sweet@4,0

I-BEAMScreenMask : ($F3FF,$E1FF,$E1FF,$E1FF,
                    $E1FF,$E049,$E000,$8000,
                    $0000,$0000,$07FC,$07F8,
                    $9FF9,$8FF1,$C003,$E007);
      
I-BEAMCursorMask : ($0C30,$0240,$0180,$0180,
                    $0180,$0180,$0180,$0180,
                    $0180,$0180,$0180,$0180,
                    $0180,$0180,$0240,$0C30);
sweet@7,7


XScreenMask : ($1FF8,$0FF0,$07E0,$03C0,
               $8181,$C003,$E007,$F00F,
               $F81F,$F00F,$E007,$C003,
               $8181,$03C0,$07E0,$0FF0 );

XCursorMask : ($8001,$C003,$6006,$300C,
               $1818,$0C30,$0660,$03C0,
               $0180,$03C0,$0660,$0C30,
               $1818,$300C,$6006,$C003 );

sweet@7,8

}

HourglassHotSpotX : Word = 1;
HourglassHotSpotY : Word = 0;
TextMouseCursor:char=#160;
MousePointerHotSpotX : Word = 1;
MousePointerHotSpotY : Word = 0;

  MouseISOK,mouseEnabled:boolean;

   PrevButton:^ButtonBox;
   NextButton:^ButtonBox;

procedure mouseWait( a_type:byte);
procedure mouse_write( a_write:byte);
function mouse_read:byte;

procedure installmouse;
//need graph mode up for these
//Procedure DrawIcon(X,Y : Integer;Var Ic : Icon{or cursortype}); //draw Mouse cursor
//Procedure GetIcon(X,Y : Integer; Var GetIcon : Icon);
//procedure GraphUpdateMouse;
Procedure UpdateMouse;

procedure HideMouse;
procedure ShowMouse;

Function GetMouseX : integer;
Function GetMouseY :  integer;
procedure EnableMouse;
procedure DisableMouse;
procedure MouseHandler(var r: TRegisters); 

implementation

uses
   irq,x86,{Graph,} {which calls VESA}
   console,UConsole,keybrd;


procedure MouseHandler(var r: TRegisters); 
//trips on  IRQ 12
	
var
   flags:DWord;
   OldIcon:Icon;
  MouseByte:array[0..2] of byte;
  errors:longint;

begin 
disablekeyb; //faster than turning on/off interrupts
  errors:=0;
  LeftButtonPressed:=false;
  RightButtonPressed:=false;
  MidButtonPressed:=false; 
  OldMX := GetMouseX; {Initially, the old position is where the new one is}
  OldMX := GetMouseY;
  MX := OldMX;
  MY := OldMY;

  writestrln('Mouse');
  if ((readportb($64) and 1)=1) and ((readportb($64) and 5)<>5) then begin  
  //is data packet waiting?
  
  //Reads all of the mouse bytes (in groups of threes)

    x:=0;
    repeat
	mousebyte[x]:=readportb($60);
        inc(x);
    until x=2;

      mx:=mousebyte[1]; //deltas..correct in a minute
      my:=mousebyte[2];
      if mousebyte[1]>= 128 then
         mousebyte[1]:=mousebyte[1]-256;
      if mousebyte[2]>= 128 then
         mousebyte[2]:=mousebyte[2]-256;
      
      writestring('X:');				
      writelong(mx);
      writestring(' Y:');				
      writelong(mx);

//		mz:=mousebyte[3];  //mouse wheel is z axis

	    	    
        //with some help from Oberon/BluebottleOS
{	if (mx and 6=0) or (mx and 7=0) then //check 6/7 th bits for errors
	     inc(errors)  else begin
   	         x:=0;
             end;	}
        //Left right and center buttons, usually caught via set or array.
	if (mousebyte[0] and 1=1) then LeftButtonPressed:=true; //Left
	if (mousebyte[0] and 2=1) then RightButtonPressed:=true; //Right
//not until we hit mouse systenms mode(4 packets)
//	if (mousebyte[0] and 2=1) then MidButtonPressed:=true; //Middle				
		 

//end;{mouseevent}
end;{data waiting}

  If (MX <> OldMX) Or (MY <> OldMY) then Begin {On mouse move}

	//not until graphmode is enable.
      // if IsGraphEnabled then  GraphUpdateMouse else
             UpdateMouse;	
  End;
  OldMX := MX;
  OldMy := MY;
enablekeyb;
end;


procedure mouseWait(a_type:byte);

var
	time_out:longint;


begin
	time_out:=100000;
	if(a_type=$0)then begin
		while(time_out>1) do begin //Data
			if (readportb($64 and 1)=1) then begin
				exit;
			end;
			dec(time_out);
		end;
		exit;
	end else begin
		while(time_out>1) do begin //Signal
			if (readportb($64 and 2)=0) then begin
				exit;
			end;
			dec(time_out);
		end;
	exit;
	end;
end;


procedure mouse_write( a_write:byte);


begin

//Wait to be able to send a command
	mouseWait($01);
//Tell the mouse we are sending a command
	writeportb($64, $D4); 
//Wait for the final part
	mouseWait($01);
//Finally write
	writeportb($60, a_write);
end;


function mouse_read:byte;

begin
//Get's response from mouse
      if readportb($64) and 5 =1 then
	mouse_read:=readportb($60);        
end;

//difficult to get the math right, however...
PROCEDURE SetRate(r: LONGINT);
//we can change the movement rate with this, and resolution with mickeys.
BEGIN
//TODO: check range for valid values
   mouse_write($F3);
   mouse_write(byte(r));
END;

procedure installmouse;
//every write_mouse function returns check byte, so read it back after writing to the mouse.

var
  MouseByte:array[0..2] of byte;

    status:byte;
    numb,h:integer; //number of buttons?
    done:boolean;

begin
 done:=false;
 writestring('Installing emulated USB or PS2 Mouse... ');
 tabulate;
 tabulate;
  asm
    cli
  end;
  
  { Initialize some of the variables we'll be using }
  bytenum := 0;
  x := 0;
  y := 0;
  LeftbuttonPressed := false;
  RightbuttonPressed := false;
  MidButtonPressed:=false;
  WheelMoved:=false;
        
        writeportb($64,$A9); //test if mouse is ok.
        x:=mouse_Read;
        if (x<> $00) then begin
		writestrln('Mouse Init error.');
		writestrln('Keyboard used as emulated mouse.');
		MouseExists:=false;
		flipkeymouse;
		KeyMouseEnabled:=true;
	        exit;
	end;

//Enable the auxiliary mouse device(mouse)
	writeportb($64, $A8);
	x:=mouse_Read;
//We are reading MOUSE EVENTS RIGHT NOW!
     
	// enable MS Intellimouse 3rd button (wheel)
		SetRate(200);
		SetRate(100); //200 enables 4th byte, but its tricky to handle.
		SetRate(80);

		Mouse_Write($E8);
		Mouse_Write($03);	// 8 counts/mm 
		Mouse_write($E7);	 //this is 'mickeys', 2:1 scale
	        
                mouse_write($47); //Controller Interrupts =on	
	        readportb($60);


//Enable the interrupts
	mouseWait(1);
        writeportb($64, $20);
	mouseWait(0);
	
        status:=(readportb($60) or 2);
        status:=status and $DF;
	mouseWait(1);

	writeportb($64, $60);

	mouseWait(1);
	writeportb($60, status);

//Enable the mouse
	mouse_write($F4);	
	x:=mouse_read;  //Acknowledge
        if (x<> $fa) then begin //not ack
		writestrln('Mouse not ready.');
		writestrln('Keyboard used as emulated mouse.');
		MouseExists:=false;
		flipkeymouse;
		KeyMouseEnabled:=true;
	        exit;

	end;

        MouseExists:=true;
	TextColor(Green);
	writestrln('[ OK ]');
	TextColor(8);
        
asm
  sti
end;
end;


{$T+}
{
//only works if Graph unit used.
Procedure DrawIcon(X,Y : Integer;Var Ic : Icon); //draw Mouse cursor
Var i, j : Integer;
Begin
 For j := 1 to 8 do
   For i := 1 to 8 do
    If (Ic[j,i] <> 98) And (X+(i-1) < MaxX) And 
      (Y+(j-1) < MaxY) then PutPixel(X+(i-1),Y+(j-1),Ic[j,i],GraphVidMem);
End;

Procedure GetIcon(X,Y : Integer; Var GetIcon : Icon);
Var i, j : integer;
Begin
 For j := 1 to 8 do
   For i := 1 to 8 do
    GetIcon[j,i] := GetPixel(X+(i-1),Y+(j-1),GraphVidMem);
End;

Procedure DrawCursor(X,Y : Integer;Var Ic : Cursor); //draw Mouse cursor
Var i, j : Integer;
Begin
 For j := 1 to 16 do
   For i := 1 to 16 do
    If (Ic[j,i] <> 98) And (X+(i-1) < MaxX) And 
      (Y+(j-1) < MaxY) then PutPixel(X+(i-1),Y+(j-1),Ic[j,i],GraphVidMem);
End;

Procedure GetCursor(X,Y : Integer; Var GetIcon : Cursor);
Var i, j : integer;
Begin
 For j := 1 to 16 do
   For i := 1 to 16 do
    GetIcon[j,i] := GetPixel(X+(i-1),Y+(j-1),GraphVidMem);
End;

Procedure GraphUpdateMouse;
Begin
lockmouse;
    DrawIcon(OldMX,OldMY,OldIcon); //Clean mouse at its old position
    OldMX := MX; //Update the mouse's old position - it was moved
    OldMY := MY;
    GetIcon(MX,MY, OldIcon); //Get its new "Background"
    DrawIcon(MX,MY,MouseIcon); //And finally draw it.
unlockmouse;
End;
 }  

Procedure UpdateMouse;
//we dont want to overwrite the text on screen, rather 'xor' a mouse cursor over it.
Begin

scrolldisabled:=true;
      
if LeftButtonPressed=true then writestring('Left '); 
if RightButtonPressed=true then writestring('Right '); 

	OldMX := MX; {Update the mouse's old position - it was moved}
	OldMY := MY; 
        GotoXY(OldMX,OldMY); //re-write new 'mouse'
	cursorbig;
	//TextColor(Red);
	//writechar(textmousecursor); //for now..
	//TextColor(8);
        
scrolldisabled:=false;
End;

Procedure HideMouse;
Begin
//{$ifdef UseGUI}
//  DrawIcon(OldMx,OldMy,OldIcon);
//{$else}
	GotoXY(OldMX,OldMY); //clear old mouse position
	//TextAttr:=TextAttr and UndoNegative; //written mouse is Negative, so make last position positive.
	CursorNormal;
        
//{$endif}
End;

Procedure ShowMouse;
Begin

//{$ifdef UseGUI}
//  GetIcon(MX,MY, OldIcon);
//  GraphUpdateMouse;
//{$else}
//not worried about Icons.Text Mode.
	updatemouse;
//{$endif}
End;

procedure ButtonHandler;
//based from Python(super OOP C on steroids) --why recreate the mouse-wheel?
begin
// (how many buttons on the screen?)
 if RightButtonPressed or LeftButtonPressed then begin  //is Mouse button down?
	// if (MouseInBox (PButtonBox[x])) and (ButtonPressed(LeftButton)) then begin
	//	invertButton; //'push' the visual button
	//	PButtonBox^[x].Proc;
	    end else begin
		//clicked into textarea, desktop, etc..
	    end;
end;

Function MouseInBox(Mx,My : Integer; Var Btn:ButtonBox) : Boolean;
Begin   
        
//a bounds check.
MouseInBox := boolean(((MX > Btn.X1) And (MX < Btn.X2)) And 
                      ((MY > Btn.Y1) And (MY < Btn.Y2)));
//leaves 'click' to onClick event handler.
End;

{$ASMMODE ATT}

{ if the cursor is drawn by this the unit, we must be careful }
{ when drawing while the interrupt handler is called          }


Function GetMouseX :  integer;

begin
  GetMouseX:=MX;
end;

Function GetMouseY :  integer;

begin
  GetMouseY:=MY;
end;

procedure EnableMouse; 

begin
	writeportb($64, $A8); //turn on mouse
	readportb($60);
	ShowMouse;
end;

procedure DisableMouse; 

begin
    HideMouse;
    writeportb($64,$A7); //turn off mouse
    readportb($60);
end;

{$T-}
begin
  MX:=0;
  MY:=0;
end.

