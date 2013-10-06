//this is a mode13h example. GVision by default uses mode 13h.

//this is setting up mode 13h, typical for old dos games.
//note there is no 'uses' clause, we dont need one.
//320*240*256 colors mode is set here with flat 64K memory address.

TYPE
  ScreenBufferType = ARRAY[0..63999] OF BYTE;
  ScreenBufferPtr = ^ScreenBufferType;

VAR
  Screen : ScreenBufferPtr; 

PROCEDURE InitGraph;
BEGIN
  ASM
    mov AX, 0013h    { Function 0, mode 13h }
    int 10h
  END;
  Screen := PTR($A000, 0);   { Set up the screen buffer pointer }
END;

PROCEDURE SetPixel(X, Y : WORD; Color : BYTE);
VAR
  Address : WORD;
BEGIN
  Address := Y*320+X; //each line is offset by 320 pixels
  Screen^[Address] := Color;
END;


