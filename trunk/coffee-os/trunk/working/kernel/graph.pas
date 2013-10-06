unit graph;

{Graph AKA VESA unit.
INCOMPLETE but workable.

VESA NOTE:

PM VBE interface AKA VBE2.0 +
Functions are HERE:

mov ax,$4f10	
int 10

You will not find this info digging around on the net.I so happen to come across it.

THESE Functions WILL WORK in PM.

graphics is handled either by drawing routines OR put/get pixels routines.
It can be done in pages and redrawn for games and windows.
matter of fact there IS a VESA routine for window buffer in hardware.For simplicity's sake I left it out.
Will need to add this back soon.


THIS has NOT been through the compiler yet.
Im running from specs found on the net and the existing LCL and available sources I have.
Most routines are pulled from FPC graph Unit and Rediesgned. Copywright is below.

 This file WAS part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl

    This file implements the linux  support for the graph unit
    with circles, ellipses, AND image manipulation as per the LCL.

    This program is distributed knowing that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Advanced 3D routines are in advgraph unit, to include OpenGL support.

FV and GUI models are elsewhere, but around for the OS.

 Differences with TP Graph unit:                       
 -  default putimage and getimage only support a max.  
    of 64K colors on screen, because all pixels are    
    saved as words.  --(    Make it Dword/LongWord to fix.)
                              
 -  Set RGB Palette is not used, SetPalette must be    
    used instead.      --on less than true color 24 bits.                                
 -  In the TP graph unit, Clipping is always performed 
    on strings written with OutText, and this clipping 
    is done on a character per character basis (for    
    example, if ONE part of a character is outside the 
    viewport , then that character is not written at   
    all to the screen. In FPC Pascal, clipping is done 
    on a PIXEL basis, not a character basis, so part of 
    characters which are not entirely in the viewport  
    may appear on the screen.                         
 -  SetTextStyle only conforms to the TP version when  
    the correct (and expected) values are used for     
    CharSize for stroked fonts (4 = stroked fonts)     
 - DrawPoly XORPut mode is not exactly the same as in  
   the TP graph unit.                                  
 - Imagesize returns a longint instead of a word       
             
 AUTHORS:                                              
   Gernot Tenchio      - original version              
   Florian Klaempfl    - major updates                 
   Richard Jasmin      -bugfixes optimizations and overhaul to VESA standard.Removal of outdated BGI interface.

   Pierre Mueller      - major bugfixes                
   Carl Eric Codere    - complete rewrite              
   Thomas Schatzl      - optimizations,routines and    
                           suggestions.                
   Jonas Maebe         - bugfixes and optimizations   

 Credits (external):                                   
   - Original FloodFill code by                        
        Menno Victor van der star                      
     (the code has been heavily modified)          

Hardware supported bank swiching is available in VESA modes.
 ALSO, use HW for 3D acceleration if possible.

needs zlib for pcx support, so I left that part out.Same for JPEG support.
THough there are jpeg sources, at least for GPC, I havent the time ATM.

Im working on HD/WS views, as my notebook has non-standard resolutions.Need a way to probe for these.
Image manipulations are in another unit.

Execute this code as Ring3 layer on int81. This eases programming and does two things:

1) separates ring3 from kernel code.(Linux stable)
2) makes the syscallhandler easier to code and provides similar interface to X11 standards.
[wherein the video output routines/drawing primitives and GUI code are thier own 'Layer' in a OS API.]
(This prevents me from rewriting X11 C sources.)

Some functions here will need to be modified later to use syscalls.(drawing functions/mode changes/bank changes)

In effect, all drawing functions should be done in buffer if possible, THEN written to screen 
by flushing the buffer to Video RAM with a call to int 81.Do to the size of current HW RAM and
abilities, VRAM may also be used for buffering(off screen rendering/pages), thus creating a double buffer.

Triple buffering I believe, makes use of a buffer on disk, swapping the Video data into the Paged RAM on demand.

Card specific workaround to VESA 2 and 3 implementations will be looked at once this code is complete.
It will be implemented as an $IFDEF.(if required) I realize these functions are slower, however, they are MOST compatible.
Fast code is nice, but as is case of with Intel Linux drivers, NOT always STABLE.

I need low-level RM code to get me the EP of PM interfaces to use VBE2+ on modern hardware.
This data can be provided via the stage2 loader(stub.asm) as soon as I find the code I need, which is
available in RM.Some bootloader testing should reveal the assembler I need.
(Hence the need for a custom bootloader.A 1.44MB floppy image for CDROM will be provided to kickstart the OS at said time.)

Will attempt to pull the VESA EP from GRUB to ease headache, especially with ATI cards.
(this should point to $A000 and or current video mode/page IIRC)

This code is otherwise complete.

There is NO(nor will there be, you should know how to use DOS HW by now in Pascal and how to render 
or rotate an image) SDL layer available. Use OPenGL/Mesa/3D graph units instead.

OpenGL(high-level primitives and rendering as well as image manipulation/shaders/bumpmaps etc...) will
EVENTUALLY be ported in.There is plenty of code for such already available for FPC.

We support ALL modes/color depths and are NOT limited to mode 13h.
Adjust your code accordingly. There are some indexes made avable for legacy code. They will NOT be removed.

Fonts: 
This code will be reviewed once a FS is online. FileSystem is REQUIRED to use them.
Font files indicate how to draw(at binary level) characters for video mode.

Standard BGI fonts will be rendered accordingly once I figure out how they are supposedly displayed.
Fonts are low-level (bit by bit) work.
A basic 8x8 font is provided. Will look at linux kernel fonts  for others.

The VESA part, should be semi-complete.Instead of low-memory calls, we just do them in free ram via a buffer[array].

You dont need DPMI or low level calls with VBE2+
This is instead avoided. All DPMI calls are avoided.

Should init on VBE2+ Hardware just fine, but will have to test this to see.
We never drop from PM mode the whole time.
Am looking into further on 32bit PM VBE 2 calls, but so far haven't found much.You have to find the GRUB hook
or dump the PM interface when in RM.

The draw routines are same as BGI and are found in graph draw unit.(FPC RTL DrawCanvas)
The advgraph unit has hooks for OpenGL/Physics engines and ties to the advmath unit.
Freetype /TrueType and others are in respected Font units.
Imagery unit(BMP,PCM,JPEG(CHUNK of units)) are in Imagery unit.

With exception of VESA routines, the rest are reserved for Ring3 processes.
This is done as VESA calls int10 for function 4Fxx quite a bit and requires hardware Ring0 access to do so.
You HAVE to think 'Modular' programming style to code this correctly.

Im not writing this code three different ways from Tuesday, 
like has been done with Linux.Most video routines wind up calling SVGAlib anyway at one stage or another.
This includes SDL.

Its all GPL v2+ code, and THAT is what matters.
The separation of the FPC RTL and libs has me pulling my hair out trying to find supporting units to do what I need.

Whoever designed that MESS needs to be SHOT.

VESA clearscreen and change color example in assembler:

                mov ax,012h    ;VGA mode
                int 10h        ;640 x 480 16 colors.
                
                mov ax,0A000h
                mov es,ax      ;ES points to the video memory.
                mov dx,03C4h   ;dx = indexregister
                mov ax,0002h   ;INDEX = MASK MAP,       
; change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
                out dx,ax      ;write all the bitplanes.
                mov di,0       ;DI pointer in the video memory.
                mov cx,38400   ;(640 * 480)/8 = 38400
                mov ax,0FFh    ;write to every pixel.
                rep stosb      ;fill the screen
                mov ah,4Ch 

Graph Modes:
(in hex)
[thinking ahead, as apparently *NIX programmers HAVE NOT]

--USEFUL FOR SSTV modes--
*	4			 graph (CGA) 320 x 200 color --unsupported, use a window
*	5			 graph (CGA) 320 x 200 black / white --unsupported, use a window
	6			 graph (CGA) 640 x 200 black / white 
--USELESS, non-standard-- 
	F			 graph (EGA,VGA) 640x350 grey
	10			 graph (EGA,VGA) 640x350 16 colors

**GVision Code is set here.
        13			 graph  (VGA) 256 colors 320*240 ([classic] mode 13h) --useless, use a window


 [true color =65535 colors, use #rrggbb for access]
 [real color =[24bit] and above, adds intensity attributes. ]

COMMON STANDARD MODES:

101 			*	graph 640x480   256 colors
103 			*	graph 800x600   256 colors
105 			*	graph 1024x768  256 colors
107 			*	graph 1280x1024 256 colors



GVision handles the drawing stuff for the windows and such, and with the FONT code above, will produce a NICE
interface for the User/Admins.GVision needs a significant rewrite.
Should produce a similar to MacOS Interface style with Windows and some sort of Desktop image manipulation.

Graphic effects are defined in the INTERFACE code for GVision, NOT ELSEWHERE.
THIS is what needs the work.

T/Free Vision code gives us the basic expandable framework for the window interface, 
which GVision sits on top of for Graph modes. (if GraphEnabled call Gvision instead..)

GVision is meant to be an abstraction layer and nothing more, 
so in fact if code does not exist yet for what we need to do, 
then that means that we need to rewrite the Free Vision sources to include it, 
THEN Objectively wrap/reuse that new function in the GVision code.

This is more efficient than rewriting the GUI itself.
I have EXTREMELY BASIC Tp/Bp code for Icons, basic apps, and such.
THis code DOES NOT include color details,I have had to look around for such so please bear with me.
Those that have the follow books are encouraged to help.

"Power Graphics using Turbo Pascal 6" by K.Weiskamp and L.Heiny
I have this book also at my disposal.

}

{  see http://www.wagemakers.be/english/doc/vga for details.
  also http://www.programmersheaven.com/mb/x86_asm/165526/165526/vga-driver/?S=B20000
and  http://www.monstersoft.com/tutorial1/VESA_intro.html
---THANK YOU!!


I have our GUI. Implement TVision the way you used to and watch the magick.
I also have the FPC FV sources from a working Linux install of FPC.Start with FVision, then integrate
G-Vision/ Extended G, and there you GO, WinXP like-interface in less than a year.

Gvision does for pascal what the 'dialog' package does for ncurses on Linux, and its designed for FPC.

AKA: It magically transforms text mode interface into GUI mode grafical interface. Now if I can adjust
some variables and merge some things from another project(PGE), I might have a WinXP cloned interface.

 
 
   see example below: setmode()
   INT 10h
   AX = 4F02h (VESA MODE)
   BX = mode (see below)
   note: 16 bit and above uses color of type #rrggbbaa  where a is intensity(and palettes),
       where as 24bit and above use similar without palettes.
 
 color types:
 [standard 0-15h]
 [indexed color pallette of 16x16 array [0-254]]  GIFs files use this a lot.

 [true color 65535 colors, use #rrggbb for access]

 All modes is hex
 * are standard modes

15-bit   7-bit    Resolution   Colors   
mode     mode                           
number   number                         
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
100 h       			graph 640x400   256 colors
101 h			*	graph 640x480   256 colors
102 h	6Ah			graph 800x600   16   colors
103 h			*	graph 800x600   256 colors
104 h				graph 1024x768   16   colors
105 h			*	graph 1024x768  256 colors
106 h				graph 1280x1024 16   colors
107 h			*	graph 1280x1024 256 colors

[TEXT]
108 h				text 80x60
109 h				text 132x25
10A h				text 132x43
10B h				text 132x50
10C h				text 132x60 


10Dh     -        320x200      32K   (1:5:5:5)
10Eh     -        320x200      64K   (5:6:5)
10Fh     -        320x200      16.8M (8:8:8)
110h     -        640x480      32K   (1:5:5:5)
111h     -        640x480      64K   (5:6:5) (thousands)
112h     -      *  640x480      16.8M (8:8:8) (millions)
113h     -        800x600      32K   (1:5:5:5)
114h     -        800x600      64K   (5:6:5)
115h     -      *  800x600      16.8M (8:8:8)
116h     -        1024x768     32K   (1:5:5:5)
117h     -        1024x768     64K   (5:6:5)
118h     -      *  1024x768     16.8M (8:8:8)
119h     -        1280x1024    32K   (1:5:5:5)
11Ah     -        1280x1024    64K   (5:6:5)
11Bh     -      *  1280x1024    16.8M (8:8:8)

Video Buffer code:
 THIS is the SOLE reason WHY OSes use a CHUNK of VRAM and RAM when using Games that use Graphics.
 (The Resources are INTENSE.)
 [Hence the use of Large (4M) CPU memory move, SCREEMING of the processor engine(FAN),etc....]

Up to FOUR Bytes(DWord) hold the color data(max 32-bits)
[becomes vendor specific beyond 24-bit ( VBE2 specs), which is why X11 has issues with it. 
Windows doesn't for obvious reasons, the vendor (Nvidia,ATI,Intel) SUPPORTS the OS].

 FrameBuffer:array[1..ScreenSize] of DWord; 
Keep in mind, page flipping, too. [up to four copies of screen used with flipping code]

Linux VTerm layout:
(consoles--4K each)
 Vbuffer1:array[1..Screensize] of char; 
 Vbuffer2:array[1..Screensize] of char; 
 Vbuffer3:array[1..Screensize] of char; 
 Vbuffer4:array[1..Screensize] of char; 
 Vbuffer5:array[1..Screensize] of char; 
 Vbuffer6:array[1..Screensize] of char; 

Where X11 normally is ran, VT7(Though we could in theory use up to FOUR)
Graphbuffer:array[1..Screensize] of DWord; //RGBA*4

 Debugging/Message Consoles(can put anything here, scroll it)
 Vbuffer:array[1..Screensize] of char; 
 Vbuffer:array[1..Screensize] of char; 
 Vbuffer:array[1..Screensize] of char; 

I think the last two aren't used [CTRL-ALT-F11/F12]

}


interface

const

{clipping constants}
  ClipOn = true;
  ClipOff = false;
{raster operation constants}
  CopyPut = word( 0);
  XorPut = word( 1);
  OrPut = word( 2);
  AndPut = word( 3);
  NotPut = word( 4);
  NotOrPut = word( 5);
  InvBitOrPut = word( 6);
  InvScrAndPut = word( 7);
  TransPut = word( 8);
  MaskPut = word( 9);
  BkgPut = word(10);
  NormalPut = CopyPut;
{drawing modes on screen}
  CopyMode = smallint( 0);
  XorMode = smallint( 1);
  OrMode = smallint( 2);
  AndMode = smallint( 3);
  NotMode = smallint( 4);
  NotScrMode = smallint( 5);
  NotXorMode = smallint( 6);
  NotOrMode = smallint( 7);
  NotAndMode = smallint( 8);
  InvColAndMode = smallint( 9);
  InvColOrMode = smallint(10);
  InvScrAndMode = smallint(11);
  InvScrOrMode = smallint(12);
  BlackMode = smallint(13);
  WhiteMode = smallint(14);
  EmptyMode = smallint(15);
  Transparent = smallint(00);
  Opaque = smallint(16);

{alphabetical color names}
var AliceBlue,AlizarinCrimson,Amber,Amethyst,AntiqueWhite,Aquamarine,Asparagus,Azure,
    Beige,Bisque,Bistre,BitterLemon,Black,BlanchedAlmond,BlazeOrange,Blue,
    BlueViolet,BondiBlue,Brass,BrightGreen,BrightTurquoise,BrightViolet,Bronze,Brown,
    Buff,Burgundy,BurlyWood,BurntOrange,BurntSienna,BurntUmber,CadetBlue,CamouflageGreen,
    Cardinal,Carmine,Carrot,Casper,Celadon,Cerise,Cerulean,CeruleanBlue,
    Chartreuse,Chocolate,Cinnamon,Cobalt,Copper,Coral,Corn,CornflowerBlue,
    Cornsilk,Cream,Crimson,Cyan,DarkBlue,DarkBrown,DarkCerulean,DarkChestnut,
    DarkCoral,DarkCyan,DarkGoldenrod,DarkGray,DarkGreen,DarkIndigo,DarkKhaki,DarkMagenta,
    DarkOlive,DarkOliveGreen,DarkOrange,DarkOrchid,DarkPastelGreen,DarkPink,DarkRed,DarkSalmon,
    DarkScarlet,DarkSeaGreen,DarkSlateBlue,DarkSlateGray,DarkSpringGreen,DarkTan,DarkTangerine,DarkTeaGreen,
    DarkTerraCotta,DarkTurquoise,DarkViolet,DeepPink,DeepSkyBlue,Denim,DimGray,DodgerBlue,
    Emerald,Eggplant,FernGreen,FireBrick,Flax,FloralWhite,ForestGreen,Fractal,
    Fuchsia,Gainsboro,Gamboge,GhostWhite,Gold,Goldenrod,Gray,GrayAsparagus,
    GrayTeaGreen,Green,GreenYellow,Heliotrope,Honeydew,HotPink,IndianRed,Indigo,
    InternationalKleinBlue,InternationalOrange,Ivory,Jade,Khaki,Lavender,LavenderBlush,LawnGreen,
    Lemon,LemonChiffon,LightBlue,LightBrown,LightCoral,LightCyan,LightGoldenrodYellow,LightGray,
    LightGreen,LightMagenta,LightPink,LightRed,LightSalmon,LightSeaGreen,LightSkyBlue,LightSlateGray,
    LightSteelBlue,LightYellow,Lilac,Lime,LimeGreen,Linen,Magenta,Malachite,
    Maroon,Mauve,MediumAquamarine,MediumBlue,MediumOrchid,MediumPurple,MediumSeaGreen,MediumSlateBlue,
    MediumSpringGreen,MediumTurquoise,MediumVioletRed,MidnightBlue,MintCream,MistyRose,Moccasin,MoneyGreen,
    Monza,MossGreen,MountbattenPink,Mustard,NavajoWhite,Navy,Ochre,OldGold,
    OldLace,Olive,OliveDrab,Orange,OrangeRed,Orchid,PaleBrown,PaleCarmine,
    PaleChestnut,PaleCornflowerBlue,PaleGoldenrod,PaleGreen,PaleMagenta,PaleMauve,PalePink,PaleSandyBrown,
    PaleTurquoise,PaleVioletRed,PapayaWhip,PastelGreen,PastelPink,Peach,PeachOrange,PeachPuff,
    PeachYellow,Pear,Periwinkle,PersianBlue,Peru,PineGreen,Pink,PinkOrange,
    Plum,PowderBlue,PrussianBlue,Puce,Pumpkin,Purple,RawUmber,Red,
    Reef,RobinEggBlue,RosyBrown,RoyalBlue,Russet,Rust,SaddleBrown,Saffron,
    Salmon,SandyBrown,Sangria,Sapphire,Scarlet,SchoolBusYellow,SeaGreen,SeaShell,
    SelectiveYellow,Sepia,Sienna,Silver,SkyBlue,SlateBlue,SlateGray,Snow,
    SpringGreen,SteelBlue,SwampGreen,Taupe,Tangerine,Teal,TeaGreen,Tenne,
    TerraCotta,Thistle,Tomato,Turquoise,Ultramarine,Vermilion,Violet,VioletEggplant,
    Viridian,Wheat,White,WhiteSmoke,Wisteria,Yellow,YellowGreen,Zinnwaldite: longword;

//Legacy code
var Black,Blue,Brown,Cyan,DarkGray,Green,LightBlue,LightCyan,
    LightGray,LightGreen,LightMagenta,LightRed,Magenta,Red,White,Yellow: longword;

var
  graphmem:PChar = PChar($A0000); //Depreciated

type 

	PaletteType = record
		size  : word;
        	colors: array[0..255] of longword;
        end;


    PointType = record
    	x,y: longint;
    end;

    TResolutionRec = record
      x,y: longint;
    end;

const
       maxsmallint = high(smallint);

       { Color constants for setpalette }
       black     = 0;
       blue      = 1;
       green     = 2;
       cyan      = 3;
       red       = 4;
       magenta   = 5;
       brown     = 6;
       lightgray = 7;
       darkgray  = 8;
       lightblue = 9;
       lightgreen = 10;
       lightcyan = 11;
       lightred  = 12;
       lightmagenta = 13;
       yellow    = 14;
       white     = 15;


       UserCharSize = 0;

       ClipOn = true;
       ClipOff = false;

       { SetTextJustify constants }
       LeftText   = 0;
       CenterText = 1;
       RightText  = 2;

       BottomText = 0;
       TopText    = 2;

       Default = 0;

       256MaxColors   = 255;   { Maximum possible colors using a palette }
                          { otherwise, direct color encoding        }

    type
       RGBRec = packed record
         Red: smallint;
         Green: smallint;
         Blue : smallint;
       end;

       PaletteType = record
             Size   : longint;
             Colors : array[0..MaxColors] of RGBRec;
       end;


       PointType = record
             x,y : smallint;
       end;

       ViewPortType = record
             x1,y1,x2,y2 : smallint;
             Clip : boolean;
       end;

     
        graph_float = double;   { the platform's preferred floating point size }

const
{ maximum supported Y resultion }

  MaxYRes = 2048;
  YResDiv = 4;

type

  TDrawnList  = Array[0..(MaxYRes - 1) div YResDiv] of PFloodLine;

var
   DrawnList : TDrawnList;
   Buffer : Record              
     ByteIndex : Word;
     WordIndex : Word;
     Case Boolean Of
        False : (Bytes : Array [0..StdBufferSize-1] Of Byte);
        True  : (Words : Array [0..(StdBufferSize DIV 2)-1] Of Word);
     End;

  s1, s2, s3 : PWordArray;                { Three buffers for scanlines                 }

    palExist          : boolean;
    screenWidth,screenHeight    : longint;

    internColor                 : array[0..255] of ^longword;
    customWidth,customHeight    : longint;
    windowWidth,windowHeight    : longint;
    windowStyle                 : DWORD;
    bitPixel                    : byte;
    maxColors,palSize           : word;
    maxX,maxY,origX,origY,
    actX,actY,aspX,aspY         : longint;
 
    
    colTable                    : array of RGBQUAD;
    visualPage,activePage       : word;
    
    instFont                    : array[0..NrMaxFonts-1] of TFontString;
    grClip                      : HRGN;
    frColor,bkColor             : longword;
    
        
    viewPortWidth,viewPortHeight: smallint;
    globalTemp                  : longint; //used for some tricky techniques

	LeftText = word(0);
        CenterText = word(1);
        RightText = word(2);
        TopText = word(0);
        BottomText = word(1);
        BaselineText = word(2);
	HorizDir = word( 0);
        VertDir = word(90);
	NrVideoPages = 4; //number of available video pages
        Rad = Pi/180.0;
        NrMaxFonts = 15;
        MinCharSize = 8;
        DefaultVGAPalette: array[0..255] of longword =
      ($000000,$A80000,$00A800,$A8A800,$0000A8,$A800A8,$0054A8,$A8A8A8,
       $545454,$FC8484,$54FC54,$FCFC54,$5454FC,$FC54FC,$54FCFC,$FCFCFC,
       $000000,$141414,$202020,$2C2C2C,$383838,$444444,$505050,$606060,
       $707070,$808080,$909090,$A0A0A0,$B4B4B4,$C8C8C8,$E0E0E0,$FCFCFC,
       $FC0000,$FC0040,$FC007C,$FC00BC,$FC00FC,$BC00FC,$7C00FC,$4000FC,
       $0000FC,$0040FC,$007CFC,$00BCFC,$00FCFC,$00FCBC,$00FC7C,$00FC40,
       $00FC00,$40FC00,$7CFC00,$BCFC00,$FCFC00,$FCBC00,$FC7C00,$FC4000,
       $FC7C7C,$FC7C9C,$FC7CBC,$FC7CDC,$FC7CFC,$DC7CFC,$BC7CFC,$9C7CFC,
       $7C7CFC,$FC9CFC,$7CBCFC,$7CDCFC,$7CFCFC,$7CFCDC,$7CFCBC,$7CFC9C,
       $7CFC7C,$9CFC7C,$BCFC7C,$DCFC7C,$FCFC7C,$FCDC7C,$FCBC7C,$FC9C7C,
       $FCB4B4,$FCB4C4,$FCB4D8,$FCB4E8,$FCB4FC,$E8B4FC,$D8B4FC,$C4B4FC,
       $B4B4FC,$B4C4FC,$B4D8FC,$B4E8FC,$B4FCFC,$B4FCE8,$B4FCD8,$B4FCC4,
       $B4FCB4,$C4FCB4,$D8FCB4,$E8FCB4,$FCFCB4,$FCE8B4,$FCD8B4,$FCC4B4,
       $700000,$70001C,$700038,$700054,$700070,$540070,$380070,$1C0070,
       $000070,$001C70,$003870,$005470,$007070,$007054,$007038,$00701C,
       $007000,$1C7000,$387000,$547000,$707000,$705400,$703800,$701C00,
       $703838,$703844,$703854,$703860,$703870,$603870,$543870,$443870,
       $383870,$384470,$385470,$386070,$387070,$387060,$387054,$387044,
       $387038,$447038,$547038,$607038,$707038,$706038,$705438,$704438,
       $705050,$705058,$705060,$705068,$705070,$685070,$605070,$585070,
       $505070,$505870,$506070,$506870,$507070,$507068,$507060,$507058,
       $507050,$587050,$607050,$687050,$707050,$706850,$706050,$705850,
       $400000,$400010,$400020,$400030,$400040,$300040,$200040,$100040,
       $000040,$001040,$002040,$003040,$004040,$004030,$004020,$004010,
       $004000,$104000,$204000,$304000,$404000,$403000,$402000,$401000,
       $402020,$402028,$402030,$402038,$402040,$382040,$302040,$282040,
       $202040,$202840,$203040,$203840,$204040,$204038,$204030,$204028,
       $204020,$284020,$304020,$384020,$404020,$403820,$403020,$402820,
       $402C2C,$402C30,$402C34,$402C3C,$402C40,$3C2C40,$342C40,$302C40,
       $2C3040,$2C3440,$2C3C40,$2C4040,$2C403C,$2C4034,$2C4030,$2C402C,
       $30402C,$34402C,$3C402C,$40402C,$403C2C,$40342C,$40302C,$000000,
       $000000,$000000,$000000,$000000,$000000,$000000,$000000,$000000);
      Nr256ColorNames = 256;
      NamesPalette: array[0..NrColorNames-1] of longword =
      ($FFF8F0,$3626E3,$00BFFF,$CC6699,$D7EBFA,$D4FF7F,$5BA07B,$FFFFF0,
       $DCF5F5,$C4E4FF,$1F2B3D,$0DE0CA,$000000,$CDEBFF,$0099FF,$FF0000,
       $E22B8A,$B69500,$42A6B5,$00FF66,$DEE808,$CD00CD,$327FCD,$2A2AA5,
       $82DCF0,$200090,$87B8DE,$0055CC,$5174E9,$24338A,$A09E5F,$6B8678,
       $3A1EC4,$180096,$2191ED,$D1BEAD,$AFE1AC,$6331DE,$A77B00,$BE522A,
       $00FF7F,$1E69D2,$003F7B,$AB4700,$3373B8,$507FFF,$5DECFB,$ED9564,
       $DCF8FF,$D0FDFF,$3C14DC,$FFFF00,$8B0000,$214365,$7E4508,$606998,
       $455BCD,$8B8B00,$0B86B8,$545454,$006400,$620031,$6BB7BD,$8B008B,
       $326855,$2F6B55,$008CFF,$CC3299,$3CC003,$8054E7,$00008B,$7A96E9,
       $190356,$8FBC8F,$8B3D48,$4F4F2F,$457217,$518191,$12A8FF,$ADDBBA,
       $5C4ECC,$D1CE00,$D30094,$9314FF,$FFBF00,$BD6015,$696969,$FF901E,
       $78C850,$660099,$42794F,$2222B2,$82DCEE,$F0FAFF,$228B22,$808080,
       $A100F4,$DCDCDC,$0F9BE4,$FFF8F8,$00D7FF,$20A5DA,$7E7E7E,$455946,
       $BADACA,$008000,$2FFFAD,$FF73DF,$F0FFF0,$B469FF,$5C5CCD,$82004B,
       $A72F00,$004FFF,$F0FFFF,$6BA800,$8CE6F0,$FAE6E6,$F5F0FF,$00FC7C,
       $10E9FD,$CDFAFF,$E6D8AD,$8CB4D2,$8080F0,$FFFFE0,$D2FAFA,$A8A8A8,
       $90EE90,$FF80FF,$C1B6FF,$8080FF,$7AA0FF,$AAB220,$FACE87,$998877,
       $DEC4B0,$E0FFFF,$C8A2C8,$00FF00,$32CD32,$E6F0FA,$FF00FF,$51DA0B,
       $000080,$FFB0E0,$AACD66,$CD0000,$D355BA,$DB7093,$71B33C,$EE687B,
       $9AFA00,$CCD148,$8515C7,$701919,$FAFFF5,$E1E4FF,$B5E4FF,$C0DCC0,
       $1E03C7,$ADDFAD,$8D7A99,$58DBFF,$ADDEFF,$800000,$2277CC,$3BB5CF,
       $E6F5FD,$008080,$238E6B,$00A5FF,$0045FF,$D670DA,$547698,$3540AF,
       $AFADDD,$EFCDAB,$AAE8EE,$98FB98,$E584F9,$666699,$DDDAFA,$ABBDDA,
       $EEEEAF,$9370DB,$D5EFFF,$77DD77,$DCD1FF,$B4E5FF,$99CCFF,$B9DAFF,
       $ADDFFA,$31E2D1,$FFCCCC,$FF0066,$3F85CD,$6F7901,$CBC0FF,$6699FF,
       $DDA0DD,$E6E0B0,$533100,$9988CC,$1875FF,$800080,$124A73,$0000FF,
       $A2FFC9,$CCCC00,$8F8FBC,$E16941,$1B4680,$0E41B7,$13458B,$30C4F4,
       $7280FA,$60A4F4,$0A0092,$672508,$0024FF,$00D8FF,$578B2E,$EEF5FF,
       $00BAFF,$144270,$2D52A0,$C0C0C0,$EBCE87,$CD5A6A,$908070,$FAFAFF,
       $7FFF00,$B48246,$8EB7AC,$7E98BC,$00CCFF,$808000,$C0F0D0,$0057CD,
       $5B72E2,$D8BFD8,$4763FF,$D0E040,$8F0A12,$004DFF,$EE82EE,$991199,
       $6D8240,$B3DEF5,$FFFFFF,$F5F5F5,$DCA0C9,$00FFFF,$32CD9A,$AFC2EB);
      Nr16ColorNames = 16;
      NamesPalette: array[0..NrColorNames-1] of longword =
      ($000000,$A80000,$0054A8,$A8A800,$545454,$00A800,$FC8484,$FCFC54,
       $A8A8A8,$54FC54,$FC54FC,$5454FC,$A800A8,$0000A8,$FCFCFC,$54FCFC);


implementation


uses
  vesa;  //Everything else is in another unit.
//this unit has ONLY setup(init),close, color(palette), and putpixel routines.

procedure Set256InternColors;
begin
  internColor[000]:=@AliceBlue;
  internColor[001]:=@AlizarinCrimson;
  internColor[002]:=@Amber;
  internColor[003]:=@Amethyst;
  internColor[004]:=@AntiqueWhite;
  internColor[005]:=@Aquamarine;
  internColor[006]:=@Asparagus;
  internColor[007]:=@Azure;
  internColor[008]:=@Beige;
  internColor[009]:=@Bisque;
  internColor[010]:=@Bistre;
  internColor[011]:=@BitterLemon;
  internColor[012]:=@Black;
  internColor[013]:=@BlanchedAlmond;
  internColor[014]:=@BlazeOrange;
  internColor[015]:=@Blue;
  internColor[016]:=@BlueViolet;
  internColor[017]:=@BondiBlue;
  internColor[018]:=@Brass;
  internColor[019]:=@BrightGreen;
  internColor[020]:=@BrightTurquoise;
  internColor[021]:=@BrightViolet;
  internColor[022]:=@Bronze;
  internColor[023]:=@Brown;
  internColor[024]:=@Buff;
  internColor[025]:=@Burgundy;
  internColor[026]:=@BurlyWood;
  internColor[027]:=@BurntOrange;
  internColor[028]:=@BurntSienna;
  internColor[029]:=@BurntUmber;
  internColor[030]:=@CadetBlue;
  internColor[031]:=@CamouflageGreen;
  internColor[032]:=@Cardinal;
  internColor[033]:=@Carmine;
  internColor[034]:=@Carrot;
  internColor[035]:=@Casper;
  internColor[036]:=@Celadon;
  internColor[037]:=@Cerise;
  internColor[038]:=@Cerulean;
  internColor[039]:=@CeruleanBlue;
  internColor[040]:=@Chartreuse;
  internColor[041]:=@Chocolate;
  internColor[042]:=@Cinnamon;
  internColor[043]:=@Cobalt;
  internColor[044]:=@Copper;
  internColor[045]:=@Coral;
  internColor[046]:=@Corn;
  internColor[047]:=@CornflowerBlue;
  internColor[048]:=@Cornsilk;
  internColor[049]:=@Cream;
  internColor[050]:=@Crimson;
  internColor[051]:=@Cyan;
  internColor[052]:=@DarkBlue;
  internColor[053]:=@DarkBrown;
  internColor[054]:=@DarkCerulean;
  internColor[055]:=@DarkChestnut;
  internColor[056]:=@DarkCoral;
  internColor[057]:=@DarkCyan;
  internColor[058]:=@DarkGoldenrod;
  internColor[059]:=@DarkGray;
  internColor[060]:=@DarkGreen;
  internColor[061]:=@DarkIndigo;
  internColor[062]:=@DarkKhaki;
  internColor[063]:=@DarkMagenta;
  internColor[064]:=@DarkOlive;
  internColor[065]:=@DarkOliveGreen;
  internColor[066]:=@DarkOrange;
  internColor[067]:=@DarkOrchid;
  internColor[068]:=@DarkPastelGreen;
  internColor[069]:=@DarkPink;
  internColor[070]:=@DarkRed;
  internColor[071]:=@DarkSalmon;
  internColor[072]:=@DarkScarlet;
  internColor[073]:=@DarkSeaGreen;
  internColor[074]:=@DarkSlateBlue;
  internColor[075]:=@DarkSlateGray;
  internColor[076]:=@DarkSpringGreen;
  internColor[077]:=@DarkTan;
  internColor[078]:=@DarkTangerine;
  internColor[079]:=@DarkTeaGreen;
  internColor[080]:=@DarkTerraCotta;
  internColor[081]:=@DarkTurquoise;
  internColor[082]:=@DarkViolet;
  internColor[083]:=@DeepPink;
  internColor[084]:=@DeepSkyBlue;
  internColor[085]:=@Denim;
  internColor[086]:=@DimGray;
  internColor[087]:=@DodgerBlue;
  internColor[088]:=@Emerald;
  internColor[089]:=@Eggplant;
  internColor[090]:=@FernGreen;
  internColor[091]:=@FireBrick;
  internColor[092]:=@Flax;
  internColor[093]:=@FloralWhite;
  internColor[094]:=@ForestGreen;
  internColor[095]:=@Fractal;
  internColor[096]:=@Fuchsia;
  internColor[097]:=@Gainsboro;
  internColor[098]:=@Gamboge;
  internColor[099]:=@GhostWhite;
  internColor[100]:=@Gold;
  internColor[101]:=@Goldenrod;
  internColor[102]:=@Gray;
  internColor[103]:=@GrayAsparagus;
  internColor[104]:=@GrayTeaGreen;
  internColor[105]:=@Green;
  internColor[106]:=@GreenYellow;
  internColor[107]:=@Heliotrope;
  internColor[108]:=@Honeydew;
  internColor[109]:=@HotPink;
  internColor[110]:=@IndianRed;
  internColor[111]:=@Indigo;
  internColor[112]:=@InternationalKleinBlue;
  internColor[113]:=@InternationalOrange;
  internColor[114]:=@Ivory;
  internColor[115]:=@Jade;
  internColor[116]:=@Khaki;
  internColor[117]:=@Lavender;
  internColor[118]:=@LavenderBlush;
  internColor[119]:=@LawnGreen;
  internColor[120]:=@Lemon;
  internColor[121]:=@LemonChiffon;
  internColor[122]:=@LightBlue;
  internColor[123]:=@LightBrown;
  internColor[124]:=@LightCoral;
  internColor[125]:=@LightCyan;
  internColor[126]:=@LightGoldenrodYellow;
  internColor[127]:=@LightGray;
  internColor[128]:=@LightGreen;
  internColor[129]:=@LightMagenta;
  internColor[130]:=@LightPink;
  internColor[131]:=@LightRed;
  internColor[132]:=@LightSalmon;
  internColor[133]:=@LightSeaGreen;
  internColor[134]:=@LightSkyBlue;
  internColor[135]:=@LightSlateGray;
  internColor[136]:=@LightSteelBlue;
  internColor[137]:=@LightYellow;
  internColor[138]:=@Lilac;
  internColor[139]:=@Lime;
  internColor[140]:=@LimeGreen;
  internColor[141]:=@Linen;
  internColor[142]:=@Magenta;
  internColor[143]:=@Malachite;
  internColor[144]:=@Maroon;
  internColor[145]:=@Mauve;
  internColor[146]:=@MediumAquamarine;
  internColor[147]:=@MediumBlue;
  internColor[148]:=@MediumOrchid;
  internColor[149]:=@MediumPurple;
  internColor[150]:=@MediumSeaGreen;
  internColor[151]:=@MediumSlateBlue;
  internColor[152]:=@MediumSpringGreen;
  internColor[153]:=@MediumTurquoise;
  internColor[154]:=@MediumVioletRed;
  internColor[155]:=@MidnightBlue;
  internColor[156]:=@MintCream;
  internColor[157]:=@MistyRose;
  internColor[158]:=@Moccasin;
  internColor[159]:=@MoneyGreen;
  internColor[160]:=@Monza;
  internColor[161]:=@MossGreen;
  internColor[162]:=@MountbattenPink;
  internColor[163]:=@Mustard;
  internColor[164]:=@NavajoWhite;
  internColor[165]:=@Navy;
  internColor[166]:=@Ochre;
  internColor[167]:=@OldGold;
  internColor[168]:=@OldLace;
  internColor[169]:=@Olive;
  internColor[170]:=@OliveDrab;
  internColor[171]:=@Orange;
  internColor[172]:=@OrangeRed;
  internColor[173]:=@Orchid;
  internColor[174]:=@PaleBrown;
  internColor[175]:=@PaleCarmine;
  internColor[176]:=@PaleChestnut;
  internColor[177]:=@PaleCornflowerBlue;
  internColor[178]:=@PaleGoldenrod;
  internColor[179]:=@PaleGreen;
  internColor[180]:=@PaleMagenta;
  internColor[181]:=@PaleMauve;
  internColor[182]:=@PalePink;
  internColor[183]:=@PaleSandyBrown;
  internColor[184]:=@PaleTurquoise;
  internColor[185]:=@PaleVioletRed;
  internColor[186]:=@PapayaWhip;
  internColor[187]:=@PastelGreen;
  internColor[188]:=@PastelPink;
  internColor[189]:=@Peach;
  internColor[190]:=@PeachOrange;
  internColor[191]:=@PeachPuff;
  internColor[192]:=@PeachYellow;
  internColor[193]:=@Pear;
  internColor[194]:=@Periwinkle;
  internColor[195]:=@PersianBlue;
  internColor[196]:=@Peru;
  internColor[197]:=@PineGreen;
  internColor[198]:=@Pink;
  internColor[199]:=@PinkOrange;
  internColor[200]:=@Plum;
  internColor[201]:=@PowderBlue;
  internColor[202]:=@PrussianBlue;
  internColor[203]:=@Puce;
  internColor[204]:=@Pumpkin;
  internColor[205]:=@Purple;
  internColor[206]:=@RawUmber;
  internColor[207]:=@Red;
  internColor[208]:=@Reef;
  internColor[209]:=@RobinEggBlue;
  internColor[210]:=@RosyBrown;
  internColor[211]:=@RoyalBlue;
  internColor[212]:=@Russet;
  internColor[213]:=@Rust;
  internColor[214]:=@SaddleBrown;
  internColor[215]:=@Saffron;
  internColor[216]:=@Salmon;
  internColor[217]:=@SandyBrown;
  internColor[218]:=@Sangria;
  internColor[219]:=@Sapphire;
  internColor[220]:=@Scarlet;
  internColor[221]:=@SchoolBusYellow;
  internColor[222]:=@SeaGreen;
  internColor[223]:=@SeaShell;
  internColor[224]:=@SelectiveYellow;
  internColor[225]:=@Sepia;
  internColor[226]:=@Sienna;
  internColor[227]:=@Silver;
  internColor[228]:=@SkyBlue;
  internColor[229]:=@SlateBlue;
  internColor[230]:=@SlateGray;
  internColor[231]:=@Snow;
  internColor[232]:=@SpringGreen;
  internColor[233]:=@SteelBlue;
  internColor[234]:=@SwampGreen;
  internColor[235]:=@Taupe;
  internColor[236]:=@Tangerine;
  internColor[237]:=@Teal;
  internColor[238]:=@TeaGreen;
  internColor[239]:=@Tenne;
  internColor[240]:=@TerraCotta;
  internColor[241]:=@Thistle;
  internColor[242]:=@Tomato;
  internColor[243]:=@Turquoise;
  internColor[244]:=@Ultramarine;
  internColor[245]:=@Vermilion;
  internColor[246]:=@Violet;
  internColor[247]:=@VioletEggplant;
  internColor[248]:=@Viridian;
  internColor[249]:=@Wheat;
  internColor[250]:=@White;
  internColor[251]:=@WhiteSmoke;
  internColor[252]:=@Wisteria;
  internColor[253]:=@Yellow;
  internColor[254]:=@YellowGreen;
  internColor[255]:=@Zinnwaldite;
end;

procedure Set16InternColors;
begin
  internColor[00]:=@Black;
  internColor[01]:=@Blue;
  internColor[02]:=@Brown;
  internColor[03]:=@Cyan;
  internColor[04]:=@DarkGray;
  internColor[05]:=@Green;
  internColor[06]:=@LightBlue;
  internColor[07]:=@LightCyan;
  internColor[08]:=@LightGray;
  internColor[09]:=@LightGreen;
  internColor[10]:=@LightMagenta;
  internColor[11]:=@LightRed;
  internColor[12]:=@Magenta;
  internColor[13]:=@Red;
  internColor[14]:=@White;
  internColor[15]:=@Yellow;
end;


procedure Enter4PlaneMode; { so called x-mode. }
begin
  writePortb($3CE,$05);
  writePortb($3CF,(readPortb($3CF) and $FB));
  writePortb($3CE,$06);   
  writePortb($3CF,(readPortb($3CF) and $FD));
  writePortb($3C4,$04);   
  writePortb($3C5,((readPortb($3C5) and $F7) or 4));
  writePortb($3D4,$14); 
  writePortb($3D5,(readPortb($3D5) and $BF));
  writePortb($3D4,$17); 
  writePortb($3D5,(readPortb($3D5) or $40));
end;



procedure ClearGraph_640x480; assembler;
{$WARNING: ASSUMES Graph Mode is SET.Add a flag to check.}
asm
   mov ax,0A000h
   mov es,ax      ;ES points to the video memory.
   mov dx,03C4h   ;dx = indexregister
   mov ax,0002h   ;INDEX = MASK MAP,     --change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
   out dx,ax      ;write all the bitplanes.
   mov di,0       ;DI pointer in the video memory.
   mov cx,38400   ;(640 * 480)/8 = 38400
   mov ax,0FFh    ;write to every pixel.
   rep stosb      ;fill the screen
   mov ah,4ch 
end;


procedure putpixel640x480x256(screen:pchar; x,y,color:integer); assembler;

var
  ofs:integer;

label
   noc,same;

asm     
   	 mov ax,640
	 mov bx,y
	 mul bx         // ax = 640*y, dx = bank 
	 add ax,x       // ax = (640*y + x) mod (64k) 
	 jnc noc        // if the add went into a new bank (carry set) 
	 inc dx         // then bank = bank + 1 
noc:
	 mov ofs,ax     // mov ax (offset into bank) to ofs 
	 cmp dx,bank    // compare this pixels bank to the current bank 
	 jz same        // if its the same, dont set the bank 
	 mov bank,dx    // else update bank 
	 mov ax,4f05h     // VESA switch bank 
	 xor bx,bx        // bx should be 0 
	 mov dx,bank      // dx = bank*16 
	 shl dx,4
	 int 10h          // and do it 
same:
	 push di
	 mov ax,0a000h  // and put the pixel into a000:ofset 
	 mov es,ax
	 mov di,ofs
	 mov al,col
	 mov [es:di],al //es:di ==> es+di
	 pop di
end;


//these are not accurate.WONT WORK as written.
procedure putpixel800x600x8(screen:pchar; x,y,color:integer;)

var
  where:integer;

begin
    //clip to screen
    if (x < 0 or x > maxx or y < 0 or y > maxy)
    begin
        exit;
    end;

     where:=x*2+y*1600;
     screen[where]=color&255;         // BLUE
     screen[where+1]=(color>>8)&255;  // GREEN
     screen[where+2]=(color>>16)&255; // RED
end;

procedure putpixel_vesa_640x480(int x, int y, int color);
 
 var  
   address,bank_size,bank_number,bank_offset:integer;


   begin
      address := y*640+x;
      bank_size := mode_info.WinGranularity*1024;
      bank_number := address/bank_size;
      bank_offset := address%bank_size;
      set_vesa_bank(bank_number);
      writeportb(($A0000+bank_offset), color);
   end;


procedure putpixel8000x600x16(screen:pchar; x,y,color:integer;)

var
  where:integer;

begin
     where:=x*3+y*2400;
     screen[where]=color&255;         // BLUE
     screen[where+1]=(color>>8)&255;  // GREEN
     screen[where+2]=(color>>16)&255; // RED
end;

procedure putpixel800x600x32(screen:pchar; x,y,color:integer;)


var
  where:integer;

begin
     where:=x*4+y*3200;
     screen[where]=color&255;          // BLUE
     screen[where+1]=(color>>8)&255;   // GREEN
     screen[where+2]=(color>>16)&255;  // RED
end;


procedure ClearGraph_800x600; assembler;
{$WARNING: ASSUMES Graph Mode is SET.Add a flag to check.}
asm
   mov ax,0A000h
   mov es,ax      ;ES points to the video memory.
   mov dx,03C4h   ;dx = indexregister
   mov ax,0002h   ;INDEX = MASK MAP,         --change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
   out dx,ax      ;write all the bitplanes.
   mov di,0       ;DI pointer in the video memory.
   mov cx,60000   ;(800 * 6000)/8 = 60000
   mov ax,0FFh    ;write to every pixel.
   rep stosb      ;fill the screen
   mov ah,4ch 
end;

procedure ClearGraph_1024x768; assembler; //slightly higher than 720i/p
{$WARNING: ASSUMES Graph Mode is SET.Add a flag to check.}
asm
   mov ax,0A000h
   mov es,ax      ;ES points to the video memory.
   mov dx,03C4h   ;dx = indexregister
   mov ax,0002h   ;INDEX = MASK MAP,         --change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
   out dx,ax      ;write all the bitplanes.
   mov di,0       ;DI pointer in the video memory.
   mov cx,98304   ;(1024 * 768)/8 = 98304
   mov ax,0FFh    ;write to every pixel.
   rep stosb      ;fill the screen
   mov ah,4ch 
end;

procedure ClearGraph_1280x1024; assembler; //slightly smaller than 1080i/p
{$WARNING: ASSUMES Graph Mode is SET.Add a flag to check.}
//why go higher than this?? the fonts get hard to read.
//HD 1080i/p

asm
   mov ax,0A000h
   mov es,ax      ;ES points to the video memory.
   mov dx,03C4h   ;dx = indexregister
   mov ax,0002h   ;INDEX = MASK MAP,         --change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
   out dx,ax      ;write all the bitplanes.
   mov di,0       ;DI pointer in the video memory.
   mov cx,163840   ;(1280 * 1024)/8 = 163840
   mov ax,0FFh    ;write to every pixel.
   rep stosb      ;fill the screen
   mov ah,4ch 
end;

procedure ClearGraph_1600x1280; assembler; //slightly smaller than 1080i/p
{$WARNING: ASSUMES Graph Mode is SET.Add a flag to check.}
//why go higher than this?? the fonts get hard to read.

asm
   mov ax,0A000h
   mov es,ax      ;ES points to the video memory.
   mov dx,03C4h   ;dx = indexregister
   mov ax,0002h   ;INDEX = MASK MAP,         --change here to 0102h for blue, 0F02 for white,0002 for clear(black) 
   out dx,ax      ;write all the bitplanes.
   mov di,0       ;DI pointer in the video memory.
   mov cx,240000   ;(1600 * 1200)/8 = 240000
   mov ax,0FFh    ;write to every pixel.
   rep stosb      ;fill the screen
   mov ah,4ch 
end;


procedure ClearViewPort(screen:longint);
begin
  //setactiveVESAPage[1..4] of SupportedVESAPAges --which is what longint is.
  MoveTo(0,0);
  SetFillStyle(SolidFill,Black);
  Bar(0,0,viewPortWidth,viewPortHeight);
end;


procedure GetLineSettings(out lineinfo:LineSettingsType);
begin
  lineinfo:=lineSettings;
end;


procedure MoveRel(dx,dy:smallint);
begin
  Inc(actX,dx); Inc(actY,dy);
  MoveTo(actX,actY);
end;

procedure MoveTo(x,y:smallint);
begin
  if not(GraphEnabled) then begin
       Exit;
  end;
  actX:=x; actY:=y;
  Inc(x,origX); Inc(y,origY);
  MoveToEx(grWindow,x,y,nil);
  MoveToEx(grMemory,x,y,nil);
end;


procedure SetLineStyle(linestyle,pattern,thickness:word);
var lgpn  : LOGPEN;
    lstyle: word;
    old   : HPEN;
begin
  grResult:=grOK;
  if not(graphEnabled) then begin
       Exit;
  end;
  case linestyle of
    SolidLn         : lstyle:=PS_SOLID;
    DashedLn        : lstyle:=PS_DASH;
    DottedLn        : lstyle:=PS_DOT;
    DashDotLn       : lstyle:=PS_DASHDOT;
    DashDotDotLn    : lstyle:=PS_DASHDOTDOT;
    UserBitLn,NullLn: lstyle:=PS_NULL;
  else
    grResult:=grInvalidParam;
    Exit;
  end;
  lineSettings.linestyle:=linestyle;
  lineSettings.pattern:=pattern;
  lineSettings.thickness:=thickness;
  with lgpn do begin
                 lopnStyle:=lstyle;
                 lopnWidth.x:=thickness;
                 lopnColor:=MapColor(frColor);
               end;
  EnterCriticalSection(protect_devices);
  grPen:=CreatePenIndirect(lgpn); //on NT-based systems can be improved with ExtCreatePen
  old:=SelectObject(grWindow,grPen); 
  SelectObject(grMemory,grPen);
  if (old <> old_Pen) then DeleteObject(old);
  LeaveCriticalSection(protect_devices);
end;



TYPE

    TNewModeInfo = record
      modeInfo: array[lowNewDriver..highNewDriver] of PModeInfo;
      loHiModeNr: array[lowNewDriver..highNewDriver] of record
        lo,hi: smallint;
      end;
    end;


type
  TCharsetTransTable = array[Char] of Char;
  PCharsetTransTable = ^TCharsetTransTable;

const

  { The following table can be used for translating characters from the
    Ansi charset (ISO8859-1) to the DOS ASCII charset (CP437).
    To use this table, add the following line of code to your program:
    GraphStringTransTable := @AnsiToASCIITransTable;
  }

  AnsiToASCIITransTable: TCharsetTransTable =
    (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,   { $00 - $07 }
     #$08, #$09, #$0a, #$0b, #$0c, #$0d, #$0e, #$0f,   { $08 - $0f }
     #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { $10 - $17 }
     #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f,   { $18 - $1f }
     #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { $20 - $27 }
     #$28, #$29, #$2a, #$2b, #$2c, #$2d, #$2e, #$2f,   { $28 - $2f }
     #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { $30 - $37 }
     #$38, #$39, #$3a, #$3b, #$3c, #$3d, #$3e, #$3f,   { $38 - $3f }
     #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { $40 - $47 }
     #$48, #$49, #$4a, #$4b, #$4c, #$4d, #$4e, #$4f,   { $48 - $4f }
     #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { $50 - $57 }
     #$58, #$59, #$5a, #$5b, #$5c, #$5d, #$5e, #$5f,   { $58 - $5f }
     #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { $60 - $67 }
     #$68, #$69, #$6a, #$6b, #$6c, #$6d, #$6e, #$6f,   { $68 - $6f }
     #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,   { $70 - $77 }
     #$78, #$79, #$7a, #$7b, #$7c, #$7d, #$7e, #$7f,   { $78 - $7f }
     '?' , '?' , '?' , '?' , '?' , '?' , '?' , '?' ,   { $80 - $87 }
     '?' , '?' , '?' , '?' , '?' , '?' , '?' , '?' ,   { $88 - $8f }
     '?' , '?' , '?' , '?' , '?' , '?' , '?' , '?' ,   { $90 - $97 }
     '?' , '?' , '?' , '?' , '?' , '?' , '?' , '?' ,   { $98 - $9f }
     #$ff, #$ad, #$9b, #$9c, '?' , #$9d, '?' , '?' ,   { $a0 - $a7 }
     '?' , '?' , #$a6, #$ae, #$aa, '?' , '?' , '?' ,   { $a8 - $af }
     #$f8, #$f1, #$fd, '?' , '?' , #$e6, '?' , #$fa,   { $b0 - $b7 }
     '?' , '?' , #$a7, #$af, #$ac, #$ab, '?' , #$a8,   { $b8 - $bf }
     '?' , '?' , '?' , '?' , #$8e, #$8f, #$92, #$80,   { $c0 - $c7 }
     '?' , #$90, '?' , '?' , '?' , '?' , '?' , '?' ,   { $c8 - $cf }
     '?' , #$a5, '?' , '?' , '?' , '?' , #$99, '?' ,   { $d0 - $d7 }
     '?' , '?' , '?' , '?' , #$9a, '?' , '?' , #$e1,   { $d8 - $df }
     #$85, #$a0, #$83, '?' , #$84, #$86, #$91, #$87,   { $e0 - $e7 }
     #$8a, #$82, #$88, #$89, #$8d, #$a1, #$8c, #$8b,   { $e8 - $ef }
     '?' , #$a4, #$95, #$a2, #$93, '?' , #$94, #$f6,   { $f0 - $f7 }
     '?' , #$97, #$a3, #$96, #$81, '?' , '?' , #$98);  { $f8 - $ff }


  GraphStringTransTable: PCharsetTransTable = nil;
  DrawTextBackground : boolean = false;

Function  GetMaxX: smallint;
Function  GetMaxY: smallint;
Procedure SetViewPort(X1, Y1, X2, Y2: smallint; Clip: Boolean);
function  GetModeName(ModeNumber: smallint): string;
procedure SetGraphMode(Mode: smallint);
function GetMaxMode: smallint;

procedure GetModeRange(GraphDriver: smallint; var LoMode, HiMode: smallint);

Function  GetX: smallint;
Function  GetY: smallint;
procedure GetViewSettings(var viewport : ViewPortType);

 procedure MoveRel(Dx, Dy: smallint);
 procedure MoveTo(X,Y: smallint);

 procedure SetBkColor(ColorNum: Word);
 function  GetColor: Word;
 function  GetBkColor: Word;
 procedure SetColor(Color: Word);
 function  GetMaxColor: word;

 procedure SetPalette(ColorNum: word; Color: shortint);
 procedure GetPalette(var Palette: PaletteType);
 function GetPaletteSize: smallint;
 procedure GetDefaultPalette(var Palette: PaletteType);

 procedure GetAspectRatio(var Xasp,Yasp : word);
 procedure SetAspectRatio(Xasp, Yasp : word);

 procedure GetTextSettings(var TextInfo : TextSettingsType);
 function  TextHeight(const TextString : string) : word;
 function  TextWidth(const TextString : string) : word;
 procedure SetTextJustify(horiz,vert : word);
 procedure SetTextStyle(font,direction : word;charsize : word);
 procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);

 procedure OutText(const TextString : string);

type 
  ColorTable= array [1..4] of Word;
 
Const

//reimplement as:
// r[1]
// g[1]
// b[1]
// a[1]

//  SixTeenColors: array[0..15] of ColorTable = (
 //   (r: $0000; g: $0000; b: $0000; a: $0000),
//    (r: $0000; g: $0000; b: $8000; a: $0000),
 //   (r: $0000; g: $8000; b: $0000; a: $0000),
//    (r: $0000; g: $8000; b: $8000; a: $0000),
//    (r: $8000; g: $0000; b: $0000; a: $0000),
 //   (r: $8000; g: $0000; b: $8000; a: $0000),
//    (r: $8000; g: $8000; b: $0000; a: $0000),
//    (r: $C000; g: $C000; b: $C000; a: $0000),
//    (r: $8000; g: $8000; b: $8000; a: $0000),
//    (r: $0000; g: $0000; b: $FFFF; a: $0000),
//    (r: $0000; g: $FFFF; b: $0000; a: $0000),
//    (r: $0000; g: $FFFF; b: $FFFF; a: $0000),
//    (r: $FFFF; g: $0000; b: $0000; a: $0000),
//    (r: $FFFF; g: $0000; b: $FFFF; a: $0000),
//    (r: $FFFF; g: $FFFF; b: $0000; a: $0000),
//    (r: $FFFF; g: $FFFF; b: $FFFF; a: $0000));

//need another for 256 bit modes, as they use indexed colors.

uses
  video;

var
  ExitSave: pointer;

type
  tinttable = array[0..16383] of smallint;
  pinttable = ^tinttable;

  WordArray = Array [0..StdbufferSize] Of word;
  PWordArray = ^WordArray;

const
   { Mask for each bit in byte used to determine pattern }
   BitArray: Array[0..7] of byte =
     ($01,$02,$04,$08,$10,$20,$40,$80);
   RevbitArray: Array[0..7] of byte =
     ($80,$40,$20,$10,$08,$04,$02,$01);

var
  CurrentColor:     Word;
  CurrentBkColor: Word;
  CurrentX : smallint;   { viewport relative }
  CurrentY : smallint;   { viewport relative }
  ClipPixels: Boolean;  { Should cliiping be enabled }
  CurrentWriteMode: smallint;

  { information for Text Output routines }
  CurrentTextInfo : TextSettingsType;
  CurrentXRatio, CurrentYRatio: graph_float;
  installedfonts: longint;  { Number of installed fonts }
  StartXViewPort: smallint; { absolute }
  StartYViewPort: smallint; { absolute }
  ViewWidth : smallint;
  ViewHeight: smallint;
  IsGraphModeSet : Boolean; { Indicates if we are in graph mode or not }

  XAspect : word;
  YAspect : word;
  MaxX : smallint;       { Maximum resolution - ABSOLUTE }
  MaxY : smallint;       { Maximum resolution - ABSOLUTE }
  MaxColor : Longint;
  PaletteSize : longint; { Maximum palette entry we can set, usually equal}
                         { maxcolor.                                      }
  HardwarePages : byte;  { maximum number of hardware visual pages        }
  DirectColor : Boolean ; { Is it a direct color mode? }
  ModeList : PModeInfo;
  newModeList: TNewModeInfo;


Procedure ClearViewPortDefault; {$ifndef fpc}far;{$endif fpc}
var
 j: smallint;
 OldWriteMode, OldCurColor: word;
 LineSets : LineSettingsType;
Begin
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;

  { Save all old settings }
  OldCurColor := CurrentColor;
  CurrentColor:=CurrentBkColor;
  OldWriteMode:=CurrentWriteMode;
  CurrentWriteMode:=NormalPut;
  GetLineSettings(LineSets);
  { reset to normal line style...}
  SetLineStyle(SolidLn, 0, NormWidth);
  { routines are relative here...}
  { ViewHeight is Height-1 !     }
  for J:=0 to ViewHeight do
       HLine(0, ViewWidth , J);

  { restore old settings...}
  SetLineStyle(LineSets.LineStyle, LineSets.Pattern, LineSets.Thickness);
  CurrentColor := OldCurColor;
  CurrentWriteMode := OldWriteMode;
end;


Procedure SetViewPort(X1, Y1, X2, Y2: smallint; Clip: Boolean);
Begin
  if (X1 > GetMaxX) or (X2 > GetMaxX) or (X1 > X2) or (X1 < 0) then
  Begin

    exit;
  end;
  if (Y1 > GetMaxY) or (Y2 > GetMaxY) or (Y1 > Y2) or (Y1 < 0) then
  Begin

    exit;
  end;
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;
  StartXViewPort := X1;
  StartYViewPort := Y1;
  ViewWidth :=  X2-X1;
  ViewHeight:=  Y2-Y1;
  ClipPixels := Clip;
end;


procedure GetViewSettings(var viewport : ViewPortType);
begin
  ViewPort.X1 := StartXViewPort;
  ViewPort.Y1 := StartYViewPort;
  ViewPort.X2 := ViewWidth + StartXViewPort;
  ViewPort.Y2 := ViewHeight + StartYViewPort;
  ViewPort.Clip := ClipPixels;
end;

procedure ClearDevice;
var
  ViewPort: ViewPortType;
begin
  { Reset the CP }
  CurrentX := 0;
  CurrentY := 0;
  { save viewport }
  ViewPort.X1 :=  StartXviewPort;
  ViewPort.X2 :=  ViewWidth - StartXViewPort;
  ViewPort.Y1 :=  StartYViewPort;
  ViewPort.Y2 :=  ViewHeight - StartYViewPort;
  ViewPort.Clip := ClipPixels;
  { put viewport to full screen }
  StartXViewPort := 0;
  ViewHeight := MaxY;
  StartYViewPort := 0;
  ViewWidth := MaxX;
  ClipPixels := TRUE;
  ClearViewPort;
  { restore old viewport }
  StartXViewPort := ViewPort.X1;
  ViewWidth := ViewPort.X2-ViewPort.X1;
  StartYViewPort := ViewPort.Y1;
  ViewHeight := ViewPort.Y2-ViewPort.Y1;
  ClipPixels := ViewPort.Clip;
end;


  Procedure GetScanlineDefault (X1, X2, Y : smallint; Var Data); {$ifndef fpc}far;{$endif fpc}
  {**********************************************************}
  { Procedure GetScanLine()                                  }
  {----------------------------------------------------------}
  { Returns the full scanline of the video line of the Y     }
  { coordinate. The values are returned in a WORD array      }
  { each WORD representing a pixel of the specified scanline }
  { note: we only need the pixels inside the ViewPort! (JM)  }
  { note2: extended so you can specify start and end X coord }
  {   so it is usable for GetImage too (JM)                  }
  {**********************************************************}


  Var
    x : smallint;
  Begin
     For x:=X1 to X2 Do
       WordArray(Data)[x-x1]:=GetPixel(x, y);
  End;
 
  procedure SetColor(Color: Word);

   Begin
     CurrentColor := Color;
   end;


  function GetColor: Word;

   Begin
     GetColor := CurrentColor;
   end;

  function GetBkColor: Word;

   Begin
     GetBkColor := CurrentBkColor;
   end;


  procedure SetBkColor(ColorNum: Word);
  { Background color means background screen color in this case, and it is  }
  { INDEPENDANT of the viewport settings, so we must clear the whole screen }
  { with the color.                                                         }
   var
     ViewPort: ViewportType;
   Begin
     GetViewSettings(Viewport);
     SetViewPort(0,0,MaxX,MaxY,FALSE);
     CurrentBkColor := ColorNum;
     {ClearViewPort;}
     if not DirectColor and (ColorNum<256) then
      SetRGBPalette(0,
          DefaultColors[ColorNum].Red,
          DefaultColors[ColorNum].Green,
          DefaultColors[ColorNum].Blue);
     SetViewport(ViewPort.X1,Viewport.Y1,Viewport.X2,Viewport.Y2,Viewport.Clip);
   end;


  function GetMaxColor: word;
  { Checked against TP VGA driver - CEC }

   begin
      GetMaxColor:=MaxColor-1; { based on an index of zero so subtract one }
   end;

  Function GetMaxX: smallint;
  { Routine checked against VGA driver - CEC }
   Begin
     GetMaxX := MaxX;
   end;

  Function GetMaxY: smallint;
  { Routine checked against VGA driver - CEC }
   Begin
    GetMaxY := MaxY;
   end;


  Function GetX: smallint;
   Begin
     GetX := CurrentX;
   end;


  Function GetY: smallint;
   Begin
     GetY := CurrentY;
   end;

    var
     i: smallint;

  procedure GetAspectRatio(var Xasp,Yasp : word);
  begin
    XAsp:=XAspect;
    YAsp:=YAspect;
  end;

  procedure SetAspectRatio(Xasp, Yasp : word);
  begin
    Xaspect:= XAsp;
    YAspect:= YAsp;
  end;


  procedure SetWriteMode(WriteMode : smallint);
//this should be updated..linux only supports two modes, but there are many many more.
   begin
     Case writemode of
       xorput, andput: CurrentWriteMode := XorPut;
       notput, orput, copyput: CurrentWriteMode := CopyPut;
     End;
   end;



  type
    PByte = ^Byte;
    PLongInt = ^LongInt;

    PByteArray = ^TByteArray;
    TByteArray = array [0..MAXINT - 1] of Byte;

Const

 { Types }
 type

  PGraphicsContext = ^TGraphicsContext;
  TGraphicsContext = record
                       ModeType: Byte;
                       ModeFlags: Byte;
                       Dummy: Byte;
                       FlipPage: Byte;
                       Width: LongInt;
                       Height: LongInt;
                       BytesPerPixel: LongInt;
                       Colors: LongInt;
                       BitsPerPixel: LongInt;
                       ByteWidth: LongInt;
                       VBuf: pointer;
                       Clip: LongInt;
                       ClipX1: LongInt;
                       ClipY1: LongInt;
                       ClipX2: LongInt;
                       ClipY2: LongInt;
                       ff: pointer;
                     end;

var
  LastColor: smallint;   {Cache the last set color to improve speed}


procedure InitColors(nrColors: longint);

var
  i: smallint;
begin
  for i:=0 to nrColors do
    vga_setpalette(I,DefaultColors[i].red shr 2,
      DefaultColors[i].green shr 2,DefaultColors[i].blue shr 2)
end;

procedure pixelproc(X,Y: smallint);

Var Color : Word;

begin
//  If Not ClipCoords(X,Y) Then exit;

  case CurrentWriteMode of
    XORPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor Xor Color;
      end;
    OrPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor Or Color;
      end;
    AndPut:
      begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor And Color;
      end;
    NotPut:
      begin
      Color := Not Color;
      end
  else
    Color:=CurrentColor;
  end;
  SetColor(Color);
  vga_drawpixel(x, y);
end;


procedure setrgbpalette(ColorNum, RedValue, GreenValue, BlueValue: smallint);
begin
  setpalette(ColorNum,RedValue shr 2,GreenValue shr 2,BlueValue shr 2);
end;

procedure getrgbpalette (ColorNum: smallint;
                                    var RedValue, GreenValue, BlueValue: smallint);

Var R,G,B : longint;

begin
  vga_getpalette(ColorNum,R,G,B);
  RedValue:=R * 255 div 63;
  GreenValue:=G * 255 div 63;
  BlueValue:=B * 255 div 63;
end;


 ViewPortType  = RECORD
                    V_X1,
                    V_Y1,
                    V_X2,
                    V_Y2            :INTEGER;
                    V_ClipOn        :BOOLEAN;
                  END;
    PByte = ^Byte;
    PLongInt = ^LongInt;

    PByteArray = ^TByteArray;
    TByteArray = array [0..MAXINT - 1] of Byte;
  
    TResolutionRec = record
      x,y: longint;
    end;
    RGBRec = packed record
         Red: smallint;
         Green: smallint;
         Blue : smallint;
    end;

type
       PaletteType = record
             Size   : longint;
             Colors : array[0..MaxColors] of RGBRec;
       end;

       LineSettingsType = record
             linestyle : word;
             pattern : word;
             thickness : word;
       end;

       TextSettingsType = record
             font : word;
             direction : word;
             charsize : word;
             horiz : word;
             vert : word;
       end;

       FillSettingsType = record
             pattern : word;
             color : word;
       end;

       FillPatternType = array[1..8] of byte;

       PointType = record
             x,y : smallint;
       end;
     

  graph_float = double;   { the platform's preferred floating point size }

  TCharsetTransTable = array[Char] of Char;
  PCharsetTransTable = ^TCharsetTransTable;
 

TOffsetTable = packed record
	SfntVersionHi : Word;
	SfntVersionLo : Word;
	NumTables : Word;
	SearchRange : Word;
	EntrySelector : Word;
	RangeShift : Word;
end;

  
  type
  
      opcodes = (_END_OF_CHAR, _DO_SCAN, _DRAW := 253, _MOVE := 254 );

  tinttable = array[0..16383] of smallint;
  pinttable = ^tinttable;

  WordArray = Array [0..StdbufferSize] Of word;
  PWordArray = ^WordArray;

 
   TColorRGB=packed record
     B,G,R:Byte;
   end;
   PColorRGB = ^TColorRGB;

   TColorRGBA=packed record
   case Boolean of
      False:(B,G,R,A:Byte);
      True:(RGB:TColorRGB);
   end;
   PColorRGBA = ^TColorRGBA;

{54+?? : Color map : Lenght of color map is 4 bytes + the rest until the beginning of image data fixed in BFH.bfOffset}
    TColorMap=TColorRGBA;

type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);


var
  XAspect : word;
  YAspect : word;
  MaxX : smallint;       { Maximum resolution - ABSOLUTE }
  MaxY : smallint;       { Maximum resolution - ABSOLUTE }
  MaxColor : Longint;
  PaletteSize : longint; { Maximum palette entry we can set, usually equal}
                         { maxcolor.                                      }
  HardwarePages : byte;  { maximum number of hardware visual pages        }
  DirectColor : Boolean ; { Is it a direct color mode? }
  ModeList : PModeInfo;
 
  ImageHandlers : TImageHandlersManager;
  GrayConvMatrix : TGrayConvMatrix;
  FontMgr : TFontManager;
 LastColor: smallint;   {Cache the last set color to improve speed}

       fonts : array[1..maxfonts] of tfontrec;
       Strokes: TStrokes; 

  ExitSave: pointer;
  CurrentColor:     Word;
  CurrentBkColor: Word;
  CurrentX : smallint;   { viewport relative }
  CurrentY : smallint;   { viewport relative }
  ClipPixels: Boolean;  { Should cliiping be enabled }
  CurrentWriteMode: smallint;

  LineInfo : LineSettingsType;
  FillSettings: FillSettingsType;

  { information for Text Output routines }
  CurrentTextInfo : TextSettingsType;
  CurrentXRatio, CurrentYRatio: graph_float;
  installedfonts: longint;  { Number of installed fonts }

  StartXViewPort: smallint; { absolute }
  StartYViewPort: smallint; { absolute }
  ViewWidth : smallint;
  ViewHeight: smallint;

 VerticalPosInfo: TVerticalPosInfo;   { get your own info here }
 BankTable      : ARRAY[0..MaxBanks] OF Word;

 CurrentBank,
 WritePage,
 VisualPage       : word;
 WinSizeBytes     : word;  { winsize in bytes..   0 = 64k }


{   Color definitions and functions.}
  BytesNeeded : array[TColorFormat] of byte =
      (1,1,1,1,2,3,1,2,4,2,2,3,4,6,1,2,4,8,2,2,3,4,6,1,2,4,8);

  alphaTransparent = $0000;
  alphaOpaque      = $FFFF;
  Transparent: TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaTransparent);
  Black      : TFPColor = (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  Blue       : TFPColor = (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  Green      : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  Cyan       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  Red        : TFPColor = (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  Magenta    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  Yellow     : TFPColor = (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  White      : TFPColor = (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);
  Gray       : TFPColor = (Red: $8000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  LtGray     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  DkGray     : TFPColor = (Red: $4000; Green: $4000; Blue: $4000; Alpha: alphaOpaque);
  DkBlue     : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  DkGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  DkCyan     : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  DkRed      : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  DkMagenta  : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  DkYellow   : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  Maroon     : TFPColor = (Red: $8000; Green: $0000; Blue: $0000; Alpha: alphaOpaque);
  LtGreen    : TFPColor = (Red: $0000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  Olive      : TFPColor = (Red: $8000; Green: $8000; Blue: $0000; Alpha: alphaOpaque);
  Navy       : TFPColor = (Red: $0000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  Purple     : TFPColor = (Red: $8000; Green: $0000; Blue: $8000; Alpha: alphaOpaque);
  Teal       : TFPColor = (Red: $0000; Green: $8000; Blue: $8000; Alpha: alphaOpaque);
  Silver     : TFPColor = (Red: $c000; Green: $c000; Blue: $c000; Alpha: alphaOpaque);
  Lime       : TFPColor = (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque);
  Fuchsia    : TFPColor = (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque);
  Aqua       : TFPColor = (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque);

  GraphStringTransTable: PCharsetTransTable = nil;
  DrawTextBackground : boolean = false;
       maxsmallint = high(smallint);
     

{BMP magic word is always 19778 : 'BM'}
  BMmagic=19778;

{ Values for Compression field }
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;
  BI_JPEG = 4;
  BI_PNG = 5;
  WhiteSpaces=[#9,#10,#13,#32]; {Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}

  PatternBitCount = sizeof(longword) * 8;
  EFont = 'Font';
  EPen = 'Pen';
  EBrush = 'Brush';
  ErrAllocation = '%s %s be allocated.';
  ErrAlloc : array [boolean] of string = ('may not','must');
  ErrCouldNotCreate = 'Could not create a %s.';
  ErrNoLock = 'Canvas not locked.';

  KeyIdentification = 'ID';
  Signature    : Array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

  MaxChunkLength = $7FFFFFFF;
  All1Bits : longword = $FFFFFFFF;

  ChunkAncillary = $10000000;
  ChunkPrivate   = $00100000;
  ChunkReserved  = $00001000;
  ChunkSafeCopy  = $00000010;


  StartRow     : Array[0..7] of Integer = (0, 0, 0, 4, 0, 2, 0, 1);
  StartCol     : Array[0..7] of Integer = (0, 0, 4, 0, 2, 0, 1, 0);
  RowInc       : Array[0..7] of Integer = (1, 8, 8, 8, 4, 4, 2, 2);
  ColInc       : Array[0..7] of Integer = (1, 8, 8, 4, 4, 2, 2, 1);
  BlockHght    : Array[0..7] of Integer = (1, 8, 8, 4, 4, 2, 2, 1);
  BlockWdth    : Array[0..7] of Integer = (1, 8, 4, 4, 2, 2, 1, 1);

  sErrErrorsInCleanup : string = '%d errors detected while freeing a Font Manager object';
  sErrFontFileNotFound : string = 'Font file "%s" not found';
  sErrFreeType : string = 'Error %d while %s';
  sInitializing : string = 'initializing font engine';
  sDestroying : string = 'destroying font engine';
  sErrErrorInCleanup : string = 'freeing Font Manager object';
  sErrSetPixelSize : string = 'setting pixel size %d (resolution %d)';
  sErrSetCharSize : string = 'setting char size %d (resolution %d)';
  sErrLoadingGlyph : string = 'loading glyph';
  sErrKerning : string = 'determining kerning distance';
  sErrMakingString1 : string = 'making string bitmaps step 1';
  sErrMakingString2 : string = 'making string bitmaps step 2';
  sErrMakingString3 : string = 'making string bitmaps step 3';
  sErrMakingString4 : string = 'making string bitmaps step 4';
  sErrLoadFont : string = 'loading font %d from file %s';
  sErrInitializing : string = 'initializing FreeType';
  sErrDestroying : string = 'finalizing FreeType';

  DefaultFontExtention : string = '.ttf';
  DefaultSearchPath : string = '';
  DefaultResolution : integer = 97;

  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_GRAY_IMAGE = 3;

  sErrNoImage:string = 'No brush image specified';
  sErrNotAvailable:string = 'Not availlable';

 // MG: ToDo: move to implementation and add a function to map to resourcestrings
  ErrorText : array[TErrorTextIndices] of string =
    ('Invalid %s index %d',
     'No image to write',
     'File "%s" does not exist',
     'No stream to write to',
     'palette',
     'horizontal pixel',
     'vertical pixel',
     'extra',
     'Image type "%s" already exists',
     'Image type "%s" already has a reader class',
     'Image type "%s" already has a writer class',
     'Error while determining image type of stream: %s',
     'Can''t determine image type of stream',
     'Error while reading stream: %s',
     'Error while writing stream: %s',
     'No palette available'
     );
  GCM_NTSC : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_JPEG : TGrayConvMatrix = (red:0.299; green:0.587; blue:0.114);
  GCM_Mathematical : TGrayConvMatrix = (red:0.334; green:0.333; blue:0.333);
  GCM_Photoshop : TGrayConvMatrix = (red:0.213; green:0.715; blue:0.072);
  DefPalChars = '.,-*abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#;:=+%$()[]';



{ maximum supported Y resultion }
const
  MaxYRes = 2048;
  { changing this to 1 or 2 doesn't improve performance noticably }
  YResDiv = 4;

type
  PFloodLine = ^TFloodLine;
  TFloodLine = record
    next: PFloodLine;
    x1 : smallint;
    x2 : smallint;
    y  : smallint;
  end;

  TDrawnList  = Array[0..(MaxYRes - 1) div YResDiv] of PFloodLine;

var
   DrawnList : TDrawnList;
   Buffer : Record                         { Union for byte and word addressing of buffer }
     ByteIndex : Word;
     WordIndex : Word;
     Case Boolean Of
        False : (Bytes : Array [0..StdBufferSize-1] Of Byte);
        True  : (Words : Array [0..(StdBufferSize DIV 2)-1] Of Word);
     End;

  s1, s2, s3 : PWordArray;                { Three buffers for scanlines                 }


const
  //bits : array[0..7] of byte = (1,2,4,8,16,32,64,128);
  bits : array[0..7] of byte = (128,64,32,16,8,4,2,1);


{    Pixel drawing routines}

type
  TLinePoints = array[0..PatternBitCount-1] of boolean;
  PLinePoints = ^TLinePoints;
  fontSize = (font8,font16,font32,font64);
	
vgaColor=packed record //DWord to be Precise
//alpha:longint;
    red:longint;
    green:longint;
    blue:longint;
end;

 PAL_WRITE_ADDR=$03c8      // palette write address
 PAL_READ_ADDR=$03c7      // palette read address
 PAL_DATA=$03c9      // palette data register

Var
 // direct memory array[flat] access: use A0000 [CGA and above] instead of 0xB8000 [text area]

 Screen: PChar = PChar($A0000);  //this is the graph memory,not text mode memory
 HasVesa:boolean;
 ScreenSize:integer=(TModeInfo.XResolution*TModeInfo.YResolution);
 
 baseBuffer:array [1..255] of byte; //256 color
 64KBuffer:array [1..65534] of byte; //Thousands
 4MBuffer:array [1..16776704] of byte; //Millions(True 24-bit)
 64MBuffer:array [1..4294705157] of byte; //True 32-bit

const

  VideoOfs = 0;

var

  VidMem: PByteArray;
  ScrWidth: SmallInt;

Function  GetMaxX: smallint;
Function  GetMaxY: smallint;
Procedure SetViewPort(X1, Y1, X2, Y2: smallint; Clip: Boolean);

Function  GetX: smallint;
Function  GetY: smallint;
procedure SetWriteMode(WriteMode : smallint);

 procedure SetBkColor(ColorNum: Word);
 function  GetColor: Word;
 function  GetBkColor: Word;
 procedure SetColor(Color: Word);
 function  GetMaxColor: word;

 procedure SetPalette(ColorNum: word; Color: shortint);
 procedure GetPalette(var Palette: PaletteType);
 function GetPaletteSize: smallint;
 procedure GetDefaultPalette(var Palette: PaletteType);

 procedure GetTextSettings(var TextInfo : TextSettingsType);
 function  TextHeight(const TextString : string) : word;
 function  TextWidth(const TextString : string) : word;
 procedure SetTextJustify(horiz,vert : word);
 procedure SetTextStyle(font,direction : word;charsize : word);
 procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);

function TFPReaderPNM.InternalCheck(Stream:TStream):boolean;

procedure DecRect (var rect : TRect; delta:integer);
procedure IncRect (var rect : TRect; delta:integer);
procedure DecRect (var rect : TRect);
procedure IncRect (var rect : TRect);

function FPColor2Packed(Col : TFPColor) : TFPPackedColor;
function Packed2FPColor(Col : TFPPackedColor) : TFPColor;
function CompareColors(const Color1, Color2: TFPColor): integer;
function CalculateGray (const From : TFPColor) : word;
function FPColor (r,g,b,a:word) : TFPColor;
function FPColor (r,g,b:word) : TFPColor;

function CreateBlackAndWhitePalette : TFPPalette;
function CreateWebSafePalette : TFPPalette;
function CreateGrayScalePalette : TFPPalette;
function CreateVGAPalette : TFPPalette;

procedure SwapBMPFileHeader(var BFH : TBitMapFileHeader);
procedure SwapBMPInfoHeader(var BFI : TBitMapInfoHeader);

function PointInside (const x,y:integer; bounds:TRect) : boolean;
procedure CheckRectClipping (ClipRect:TRect; var Rect:Trect);
procedure CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);


function CalculateGray (const From : TFPColor) : word;
function FPColor (r,g,b,a:word) : TFPColor;
function FPColor (r,g,b:word) : TFPColor;

operator = (const c,d:TFPColor) : boolean;
operator or (const c,d:TFPColor) : TFPColor;
operator and (const c,d:TFPColor) : TFPColor;
operator xor (const c,d:TFPColor) : TFPColor;
function CompareColors(const Color1, Color2: TFPColor): integer;
function CreateBlackAndWhitePalette : TFPPalette;
function CreateWebSafePalette : TFPPalette;
function CreateGrayScalePalette : TFPPalette;
function CreateVGAPalette : TFPPalette;
function Swap(This : qword): qword;
function Swap(This : int64): int64;
function Swap(This : Longword): longword;
function Swap(This : integer): integer;
function Swap(This : Word): Word;
function CalculateCRC (var data; alength:integer) : longword;
function CalculateCRC (CRC:longword; var data; alength:integer) : longword;
procedure InitEngine;
procedure DoneEngine;

Function ToWord(AWord : TWordRec) : Word;
Function FromWord(AWord : Word) : TWordRec;

var
 VideoMode: array [1..5] of string= ('640x480','800x600','1024x768','1280x1024' '1600x1280');
//320x240 not supported on DVI, not enough data. 
WSVideoMode:array[1..2] of string=('800x600'.'1024x600');
 VideoModePtr: ^VideoMode; 
 mode,oldmode:word;
 nextMode:^VideoMode


begin
 isgraphmode := false;
 SaveVideoState := nil;
 RestoreVideoState := nil;

 GetVESAInfo;

{

this comment plus drivers is how X11 works.....

if hi(VESAVersion):=1...
if hi(VESAVersion):=2...

x:=1;
while nextMode(x) <>'' DO BEGIN
	if ModeAttributes.D0=0 then mode:=nextMode(x);
	getmode(oldmode)
	setmode(mode)
	if NextMode(x):='' then begin
		restoremode(oldmode)	
		exit; 
	end;
	inc(x);
end;
}

 _StoreVideoMode;
  ModeNr := ModeTable[VESAMode];
{attach dummy procs to the bank selectors}
  _SetSingleMode := _DummyProc;
  _SetDualMode   := _DummyProc;
  
  IF CheckforVESAsig THEN
  IF InitVESA(ModeNr) THEN
  BEGIN
    {always start with the single mode}
    SelectVESASingleMode;
    {get some info}
    VESAMaxX     := ModeInfo[VESAMode].M_W-1;
    VESAMaxY     := ModeInfo[VESAMode].M_H-1;
    BytesPerLine := ModeInfo[VESAMode].M_Line;
    GraphicsOn   := TRUE;
    SetViewPort(0,0,VESAMaxX,VESAMaxY,TRUE);
    SetDefaultPalette;
    ClearGraphics(0);

{asm
	mov AX,4F02h //sets VESA mode
	mov BX,mode   // ($107) sets SVGA 1280x1024 see below
	int 10 
        MOV VESAStatus, AX //report status back
end;
}

  if (nrcolors=256) then
    InitColors(nrColors);
  SetRawMode(True);

    ArcCall.X := 0;
    ArcCall.Y := 0;
    ArcCall.XStart := 0;
    ArcCall.YStart := 0;
    ArcCall.XEnd := 0;
    ArcCall.YEnd := 0;
   
end;

PROCEDURE WaitForRetrace;
{will wait until the vertical retrace starts, normal VGA is 60 Hz}
BEGIN
repeat
	var1:=(readportb($3DA) and $08);
until var1<> $00;
repeat
	var1:=(readportb($3DA) and $08);
until var1= $00;

END; 

{

void cyclepal(void)
{
char r,g,b;
int i;
r=pal[0];
g=pal[1];
b=pal[2];
for(i=0;i<(PALSIZE-1)*3;i++) pal[i]=pal[i+3];
pal[(PALSIZE-1)*3+0]=r;
pal[(PALSIZE-1)*3+1]=g;
pal[(PALSIZE-1)*3+2]=b;
wtsync();
setpal();
}


}

FUNCTION GetHighestPageNr:BYTE;
{get highest page number; improvement on borland's pascal}
BEGIN
  GetHighestPageNr := (GetVESAVirtualHeight DIV GetScreenHeight)-1;
END; {GetHighestPageNr}


FUNCTION ExistingPage(PageNr:BYTE):BOOLEAN;
{test whether a page does exist; improvement on borland's pascal}
BEGIN
{return}
  ExistingPage := GetVESAVirtualHeight >= (PageNr+1)*GetScreenHeight;
END; {ExistingPage}


PROCEDURE SetVisualPage(PageNr:BYTE);
{same as in borland's pascal graph unit}
BEGIN
{protect}
  IF NOT ExistingPage(PageNr) THEN EXIT;
{do it}
  SetVESAVirtualTop(PageNr*(VESAMaxY+1));
{remember that}
  VisualPageNr := PageNr;
{and wait.... until that page is indeed show!}
  WaitTOF;
END; {SetVisualPage}


FUNCTION GetVisualPage:BYTE;
{return the currently selected visual page}
BEGIN
  GetVisualPage := VisualPageNr;
END; {GetVisualPage}


PROCEDURE SetActivePage(PageNr:BYTE);
{same as in borland's pascal graph unit}
BEGIN
{protect}
  IF NOT ExistingPage(PageNr) THEN EXIT;
  IF ActivePageNr = PageNr THEN EXIT;
{do it}
  ActivePageTop := PageNr*(VESAMaxY+1);
{reset viewport!}
  SetViewPort(0,0,GetMaxX,GetMaxY,TRUE);
{remember that}
  ActivePageNr  := PageNr;
END; {SetActivePage}


FUNCTION GetActivePage:BYTE;
{return the currently selected active page}
BEGIN
{return}
  GetActivePage := ActivePageNr;
END; {GetActivePage}


PROCEDURE ScrollToPage(PageNr:BYTE;ScrollTime:REAL);
{scroll to the given page with the given speed (assuming 60 Hz)}
CONST VESAFrequency = 60;
VAR SourceY,DestY,HalfY,Index,Size,Steps,CurrentY:INTEGER;
    HalfPiInvSteps,HalfSize                      :REAL;
BEGIN
{protect}
  IF NOT ExistingPage(PageNr) THEN EXIT;
  ScrollTime := Abs(ScrollTime);
  IF ScrollTime > 15.0 THEN ScrollTime := 15.0;
{compute the route}
  SourceY        := VisualPageNr*(VESAMaxY+1);
  DestY          := PageNr*(VESAMaxY+1);
  HalfY          := (SourceY+DestY) DIV 2;
  Size           := Abs(DestY-SourceY);
  Steps          := Round(ScrollTime*VESAFrequency);
  HalfPiInvSteps := 0.5*Pi/Steps;
  HalfSize       := 0.5*Size;
  FOR Index := -Steps TO Steps DO
  BEGIN
    CurrentY := Round(HalfSize*Sin(HalfPiInvSteps*Index));
    IF SourceY < DestY THEN CurrentY := HalfY+CurrentY
                       ELSE CurrentY := HalfY-CurrentY;
    WaitTOF;
    SetVESAVirtualTop(CurrentY);
  END;
{make sure we get there}
  WaitForRetrace;
  SetVESAVirtualTop(DestY);
{remember that}
  VisualPageNr := PageNr;
{it is also activated!}
  SetActivePage(PageNr);
END; {ScrollToPage}


FUNCTION GetMaxX:INTEGER;
{same function, but corrected for text mode 8x16 font}
BEGIN
  ASM
    MOV AX,  VESAMaxX
    INC AX
    AND AX,  $FFF8       {mask out lower 3 bits}
    DEC AX
    MOV [@Result], AX
  END;
END; {GetMaxX}


FUNCTION GetMaxY:INTEGER;
{same function, but corrected for text mode 8x16 font}
BEGIN
  ASM
    MOV AX,  VESAMaxY
    INC AX
    AND AX,  $FFF0       {mask out lower 4 bits}
    DEC AX
    MOV [@Result], AX
  END;
END; {GetMaxY}


FUNCTION GetScreenWidth:INTEGER;
{corrected for text mode 8x16 font}
BEGIN
  GetScreenWidth := GetMaxX+1;
END; {GetScreenWidth}


FUNCTION GetScreenHeight:INTEGER;
{corrected for text mode 8x16 font}
BEGIN
  GetScreenHeight := GetMaxY+1;
END; {GetScreenHeight}


PROCEDURE GetAspectRatio(VAR AspectX,AspectY:WORD);
{return the aspect ratio of the currently displayed mode}
BEGIN
  AspectX := 10000;
  AspectY := Round(13333.33333*(GetScreenHeight/GetScreenWidth));
END; {GetAspectRatio}

{
PROCEDURE BoxInOrder(VAR X1,Y1,X2,Y2:INTEGER); ASSEMBLER;
//some routines require X1 <= X2 and Y1 <= Y2
ASM
//highly optimized code:
  LES DI,      [X1]
  MOV AX,      [ES:DI]           //AX contains X1
  LES DI,      [X2]
  MOV BX,      [ES:DI]           //BX contains X2
  CMP AX, BX
  JLE @X_OK                      //check order
  MOV [ES:DI], AX                //put AX into X2
  LES DI,      [X1]
  MOV [ES:DI], BX                //put BX into X1
@X_OK:
  LES DI,      [Y1]
  MOV AX,      [ES:DI]           //AX contains Y1
  LES DI,      [Y2]
  MOV BX,      [ES:DI]           //BX contains Y2
  CMP AX, BX
  JLE @Y_OK                      //check order
  MOV [ES:DI], AX                //put AX into Y2
  LES DI,      [Y1]
  MOV [ES:DI], BX                //put BX into Y1
@Y_OK:
END; //BoxInOrder
}

PROCEDURE GetViewSettings(VAR ViewPort:ViewPortType);
{same as in borland's pascal graph unit}
BEGIN
{fill record}
  WITH ViewPort DO
  BEGIN
    V_X1     := ViewPortLeft;
    V_X2     := ViewPortRight;
    V_Y1     := ViewPortTop-ActivePageTop;
    V_Y2     := ViewPortBottom-ActivePageTop;
    V_ClipOn := ViewPortClipOn;
  END;
END; {GetViewSettings}


PROCEDURE SetViewSettings(ViewPort:ViewPortType);
{setting it using a predefined record}
BEGIN
{do it}
  WITH ViewPort DO SetViewPort(V_X1,V_Y1,V_X2,V_Y2,V_ClipOn);
END; {SetViewSettings}


PROCEDURE SetViewPort(X1,Y1,X2,Y2:INTEGER;Clip:BOOLEAN);
{same as in borland's pascal graph unit}
BEGIN
{protect}
  BoxInOrder(X1,Y1,X2,Y2);
  IF X1 < 0 THEN X1 := 0;
  IF Y1 < 0 THEN Y1 := 0;
  IF X2 > GetMaxX THEN X2 := GetMaxX;
  IF Y2 > GetMaxY THEN Y2 := GetMaxY;
{do it}
  ViewPortWidth  := X2-X1+1;
  ViewPortHeight := Y2-Y1+1;
  ViewPortLeft   := X1;
  ViewPortRight  := X2;
  ViewPortTop    := Y1+ActivePageTop;
  ViewPortBottom := Y2+ActivePageTop;
  ViewPortClipOn := Clip;
END; {SetViewPort}

PROCEDURE ClipGraphics(X,Y,W,H:INTEGER);
{select a part of the display}
BEGIN
{protect}
  ValidateGraphicsBox(X,Y,W,H);
{select the area}
  SetViewPort(X,Y,X+W-1,Y+H-1,TRUE);
END; {ClipGraphics}


PROCEDURE UnClipGraphics;
{selects the whole display}
BEGIN
{select the area specific to text mode!}
  SetViewPort(0,0,GetMaxX,GetMaxY,TRUE);
END; {UnClipGraphics}



PROCEDURE SetBGColor(Color:BYTE);
{set the background color}
BEGIN
  DrawBGColor := Color;
END; {SetBGColor}


PROCEDURE _MoveCursor(X,Y:INTEGER;WhileDrawing:BOOLEAN);
{move cursor to x,y}
BEGIN
{move}
  CursorX    := X;
  CursorY    := Y;
  CursorDown := WhileDrawing;
END; {_MoveCursor}


PROCEDURE MoveTo(X,Y:INTEGER);
{move cursor to x,y}
BEGIN
  _MoveCursor(X,Y,FALSE);
END; {MoveTo}


PROCEDURE RelMoveTo(X,Y:INTEGER);
{relative move cursor to x,y}
BEGIN
  _MoveCursor(CursorX+X,CursorY+Y,FALSE);
END; {RelMoveTo}



PROCEDURE FadeOutPalette(R,G,B:BYTE;FadeTime:REAL);
{fade out the complete palette}
CONST VESAFrequency = 60;
VAR Steps    :LONGINT;
    StepNr   :INTEGER;
    Palette  :PaletteType;
    ColorNr  :BYTE;
    Rol1,Rol2:REAL;
BEGIN
{get the current palette}
  Palette := CurrentPalette;
{determine the number of steps to get it faded out}
  Steps := Round(FadeTime*VESAFrequency)+1;
{and then perform that amount of steps}
  StepNr := Steps;
  REPEAT
    Dec(StepNr);
    {you need a somewhat fast computer to do this in less than 15 ms}
    FOR ColorNr := 0 TO 255 DO
    WITH Palette[ColorNr] DO
    BEGIN
      Rol1 := StepNr/Steps;
      Rol2 := 1.0-Rol1;
      C_R  := Round(Rol1*CurrentPalette[ColorNr].C_R+Rol2*R);
      C_G  := Round(Rol1*CurrentPalette[ColorNr].C_G+Rol2*G);
      C_B  := Round(Rol1*CurrentPalette[ColorNr].C_B+Rol2*B);
    END;
    _SetFullPalette(Palette)
  UNTIL StepNr < 1;
END; {FadeOutPalette}


PROCEDURE FadeInPalette(FadeTime:REAL);
{fade in the complete palette}
CONST VESAFrequency = 60;
VAR Steps    :LONGINT;
    StepNr   :INTEGER;
    Palette  :PaletteType;
    ColorNr  :BYTE;
    Rol1,Rol2:REAL;
    R,G,B    :BYTE;
BEGIN
{get the faded out palette}
  _GetFullPalette(Palette);
  WITH Palette[0] DO
  BEGIN
    R := C_R;
    G := C_G;
    B := C_B;
  END;
{determine the number of steps to get it back to normal}
  Steps := Round(FadeTime*VESAFrequency)+1;
{and then perform that amount of steps}
  StepNr := 1;
  REPEAT
    {you need a somewhat fast computer to do this in less than 15 ms}
    FOR ColorNr := 0 TO 255 DO
    WITH Palette[ColorNr] DO
    BEGIN
      Rol1 := StepNr/Steps;
      Rol2 := 1.0-Rol1;
      C_R  := Round(Rol1*CurrentPalette[ColorNr].C_R+Rol2*R);
      C_G  := Round(Rol1*CurrentPalette[ColorNr].C_G+Rol2*G);
      C_B  := Round(Rol1*CurrentPalette[ColorNr].C_B+Rol2*B);
    END;
    _SetFullPalette(Palette);
    Inc(StepNr);
  UNTIL StepNr > Steps;
END; {FadeInPalette}


FUNCTION GetColorDifference(RefColor,TestColor:RGBColor):INTEGER;
{compute the difference between the two given colors}
BEGIN
  WITH RefColor DO
  BEGIN
    GetColorDifference := Abs(C_R-TestColor.C_R)+
                          Abs(C_G-TestColor.C_G)+
                          Abs(C_B-TestColor.C_B)+
                          Abs((C_R-C_G)-(TestColor.C_R-TestColor.C_G))+
                          Abs((C_G-C_B)-(TestColor.C_G-TestColor.C_B))+
                          Abs((C_B-C_R)-(TestColor.C_B-TestColor.C_R));
  END;
END; {GetColorDifference}


FUNCTION _GetPattern(Ch:CHAR;RowNr:BYTE):BYTE;
{returns the requested pattern of the bitmap character}
BEGIN
  CASE FontHeight OF
  {mostly we will be using 16 or 8}
    16 : _GetPattern := RAM8x16FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
     8 : _GetPattern := RAM8x8FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
    14 : _GetPattern := RAM8x14FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
    13 : _GetPattern := RAM8x13FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
    12 : _GetPattern := RAM8x12FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
    11 : _GetPattern := RAM8x11FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
    10 : _GetPattern := RAM8x10FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
     9 : _GetPattern := RAM8x9FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
     7 : _GetPattern := RAM8x7FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
     6 : _GetPattern := RAM8x6FontPtr(FontDataPtr)^[BYTE(Ch),RowNr];
  END;
END; {_GetPattern}


PROCEDURE _UpdatePropInfo(FontNr:BYTE);
{update the proportional information}
VAR RowNr,B1,B2,
    Left,Right  :BYTE;
    Ch          :CHAR;
BEGIN
  FOR Ch := #0 TO #255 DO
  BEGIN
    {scan the character and press it into one byte}
    B1 := 0;
    FOR RowNr := 0 TO FontHeight-1 DO B1 := B1 OR _GetPattern(Ch,RowNr);
    {determine open space in front of character}
    Left := 0;
    B2   := B1;
    WHILE (B2 AND $80 = $00) AND (Left < 8) DO
    BEGIN
      B2 := B2 SHL 1;
      Inc(Left);
    END;
    {correct for spaces}
    IF Left > 7 THEN Left := 1;
    {determine open space after character}
    Right := 0;
    B2    := B1;
    WHILE (B2 AND $01 = $00) AND (Right < 8) DO
    BEGIN
      B2 := B2 SHR 1;
      Inc(Right);
    END;
    {correct for spaces}
    IF Right > 7 THEN Right := 1;
    {try to keep one pixel in front}
    IF Right > 0 THEN Dec(Right);
    {set the prop info}
    PropInfo[Ch] := (Left SHL 4)+(8-Left-Right);
  END;
END; {_UpdatePropInfo}


FUNCTION GetTextWidth(S:string;Proportional:BOOLEAN;FontNr:BYTE):INTEGER;
{returns the actual width of this text with the given settings}
VAR Index,Width:WORD;
    other:pchar;
BEGIN
  other:=StrPCopy(other,s);
{make sure we loaded the right font!}
  {IF FontNr <> CurrentFontNr THEN LoadRAMFont(FontNr);}
{now we compute it}
  IF Proportional THEN
  BEGIN
  {make sure we have updated the proportional information for this font}
    {IF FontNr <> CurrentPropNr THEN _UpdatePropInfo(FontNr);
  {walk through all the individual characters and summ their width}
    Width := 0;
    FOR Index := 0 TO StrLen(other)-1 DO Inc(Width,PropInfo[other[Index]] AND $0F);
  END
  ELSE Width := 8*StrLen(other); {non-proportional}
{return that}
  GetTextWidth := Width;
END; {GetTextWidth}


    procedure OutTextXYDefault(x,y : smallint;const TextString : string);

      type
       Tpoint = record
         X,Y: smallint;
       end;
      var
         ConvString    : String;
         i,j,k,c       : longint;
         xpos,ypos     : longint;
         counter       : longint;
         cnt1,cnt2     : smallint;
         cnt3,cnt4     : smallint;
         charsize      : word;
         WriteMode     : word;
         curX2, curY2, xpos2, ypos2, x2, y2: graph_float;
         oldvalues     : linesettingstype;
         fontbitmap    : TBitmapChar;
         chr           : char;
         curx2i,cury2i,
         xpos2i,ypos2i : longint;

      begin
         { save current write mode }
         WriteMode := CurrentWriteMode;
         CurrentWriteMode := NormalPut;
         GetTextPosition(xpos,ypos,textstring);
         X:=X-XPos; Y:=Y+YPos;
         XPos:=X; YPos:=Y;

         ConvString := ConvertString(TextString);
         CharSize := CurrentTextInfo.Charsize;
         if Currenttextinfo.font=DefaultFont then
         begin
           c:=length(ConvString);
           if CurrentTextInfo.direction=HorizDir then
           { Horizontal direction }
             begin
                for i:=0 to c-1 do
                  begin
                     xpos:=x+(i*8)*Charsize;
                     { we copy the character bitmap before accessing it }
                     { this improves speed on non optimizing compilers  }
                     { since it is one less address calculation.        }
                     Fontbitmap:=TBitmapChar(DefaultFontData[ConvString[i+1]]);
                     { no scaling }
                     if CharSize = 1 then
                      Begin
                        for j:=0 to 7 do
                           for k:=0 to 7 do
                             if Fontbitmap[j,k]<>0 then
                               PutPixel(xpos+k,j+y,CurrentColor)
                             else if DrawTextBackground then
                               PutPixel(xpos+k,j+y,CurrentBkColor);
                      end
                     else
                      { perform scaling of bitmap font }
                      Begin
                        j:=0;
                        cnt3:=0;

                        while j <= 7 do
                        begin
                          { X-axis scaling }
                          for cnt4 := 0 to charsize-1 do
                           begin
                             k:=0;
                             cnt2 := 0;
                             while k <= 7  do
                                begin
                                  for cnt1 := 0 to charsize-1 do
                                    begin
                                       If FontBitmap[j,k] <> 0 then
                                           PutPixel(xpos+cnt1+cnt2,y+cnt3+cnt4,CurrentColor)
                                       else if DrawTextBackground then
                                           PutPixel(xpos+cnt1+cnt2,y+cnt3+cnt4,CurrentBkColor);
                                    end;
                                    Inc(k);
                                    Inc(cnt2,charsize);
                                end;
                           end;
                          Inc(j);
                          Inc(cnt3,charsize);
                        end;
                      end;
                  end;
             end
           else
           { Vertical direction }
            begin
              for i:=0 to c-1 do
              begin

                chr := ConvString[i+1];
                Fontbitmap:=TBitmapChar(DefaultFontData[chr]);
                ypos := y-(i shl 3)*CharSize;

                { no scaling }
                if CharSize = 1 then
                 Begin
                   for j:=0 to 7 do
                      for k:=0 to 7 do
                        if Fontbitmap[j,k] <> 0 then
                          PutPixel(xpos+j,ypos-k,CurrentColor)
                        else if DrawTextBackground then
                          PutPixel(xpos+j,ypos-k,CurrentBkColor);
                 end
                else
                 { perform scaling of bitmap font }
                 Begin
                   j:=0;
                   cnt3:=0;

                   while j<=7 do
                   begin
                     { X-axis scaling }
                     for cnt4 := 0 to charsize-1 do
                      begin
                        k:=0;
                        cnt2 := 0;
                        while k<=7  do
                           begin
                             for cnt1 := 0 to charsize-1 do
                               begin
                                  If FontBitmap[j,k] <> 0 then
                                    PutPixel(xpos+cnt3-cnt4,ypos+cnt1-cnt2,CurrentColor)
                                  else if DrawTextBackground then
                                    PutPixel(xpos+cnt3-cnt4,ypos+cnt1-cnt2,CurrentBkColor);
                               end;
                               Inc(k);
                               Inc(cnt2,charsize);
                           end;
                      end;
                     Inc(j);
                     Inc(cnt3,charsize);
                   end;
                 end;
              end;
            end;
         end else
         { This is a stroked font which is already loaded into memory }
           begin
              getlinesettings(oldvalues);
              { reset line style to defaults }
              setlinestyle(solidln,oldvalues.pattern,normwidth);
              if Currenttextinfo.direction=vertdir then
                 xpos:=xpos + Textheight(ConvString);
              CurX2:=xpos; xpos2 := curX2; x2 := xpos2;
              CurY2:=ypos; ypos2 := curY2; y2 := ypos2;

              for i:=1 to length(ConvString) do
                begin
                   c:=byte(ConvString[i]);
                   unpack( fonts[CurrentTextInfo.font].instr,
                     fonts[CurrentTextInfo.font].Offsets[c], Strokes );
                   counter:=0;
                   while true do
                     begin
                         if CurrentTextInfo.direction=VertDir then
                           begin
                             xpos2:=x2-(Strokes[counter].Y*CurrentYRatio);
                             ypos2:=y2-(Strokes[counter].X*CurrentXRatio);
                           end
                         else
                           begin
                             xpos2:=x2+(Strokes[counter].X*CurrentXRatio) ;
                             ypos2:=y2-(Strokes[counter].Y*CurrentYRatio) ;
                           end;
                         case opcodes(Strokes[counter].opcode) of
                           _END_OF_CHAR: break;
                           _DO_SCAN: begin
                                    end;
                           _MOVE : Begin
                                     CurX2 := XPos2;
                                     CurY2 := YPos2;
                                   end;
                           _DRAW: Begin
                                    curx2i:=trunc(CurX2);
                                    cury2i:=trunc(CurY2);
                                    xpos2i:=trunc(xpos2);
                                    ypos2i:=trunc(ypos2);
                                    Line(curx2i,cury2i,xpos2i,ypos2i);
                                    CurX2:=xpos2;
                                    CurY2:=ypos2;
                                  end;
                             else
                               Begin
                               end;
                            end;
                        Inc(counter);
                     end; { end while }
                   if Currenttextinfo.direction=VertDir then
                     y2:=y2-(byte(fonts[CurrenttextInfo.font].widths[c])*CurrentXRatio)
                   else
                     x2:=x2+(byte(fonts[Currenttextinfo.font].widths[c])*CurrentXRatio);
                end;
              setlinestyle( oldvalues.linestyle, oldvalues.pattern, oldvalues.thickness);
           end;
        { restore write mode }
        CurrentWriteMode := WriteMode;
      end;


    procedure OutText(const TextString : string);
      var x,y:smallint;
      begin
         { Save CP }
         x:=CurrentX;
         y:=CurrentY;
         OutTextXY(CurrentX,CurrentY,TextString);
         { If the direction is Horizontal and the justification left }
         { then and only then do we update the CP                    }
         if (Currenttextinfo.direction=HorizDir) and
           (Currenttextinfo.horiz=LeftText) then
               inc(x,textwidth(TextString));
         { Update the CP }
         CurrentX := X;
         CurrentY := Y;
      end;

    procedure SetTextJustify(horiz,vert : word);

      begin
         if (horiz<0) or (horiz>2) or
            (vert<0) or (vert>2) then
           begin
              exit;
           end;
         Currenttextinfo.horiz:=horiz;
         Currenttextinfo.vert:=vert;
      end;

    procedure GetTextSettings(var TextInfo : TextSettingsType);

      begin
         textinfo:=currenttextinfo;
      end;

    procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);
      begin
         CurrentXRatio := MultX / DivX;
         CurrentYRatio := MultY / DivY;
      end;

  Procedure PushPoint (x, y : smallint);
  
  Begin
    If Buffer.WordIndex<(StdBufferSize DIV 2)-3 then
     Begin
       Buffer.Words[Buffer.WordIndex]:=x;
       Buffer.Words[Buffer.WordIndex+1]:=y;
       Inc (Buffer.WordIndex,2);
     End
  End;


  Procedure AddLinePoints(x1,x2,y: smallint);
   var temp: PFloodLine;
   begin
     new(temp);
     temp^.x1 := x1;
     temp^.x2 := x2;
     temp^.y := y;
     temp^.next := DrawnList[y div YResDiv];
     DrawnList[y div YResDiv] := temp;
   end;



    procedure GetPalette(var Palette: PaletteType);
      var
        i: longint;
        size : longint;
      begin
        { palette routines do not work in DirectColor mode }
        if DirectColor then
         begin
           exit;
         end;
        Palette.Size := PaletteSize;
        { index at zero }
        size := PaletteSize - 1;
        for i:=0 to size do
          GetRGBPalette(i,
             Palette.Colors[i].Red,
             Palette.Colors[i].Green,
             Palette.Colors[i].Blue);
      end;

    function GetPaletteSize: smallint;
     begin
       GetPaletteSize := PaletteSize;
     end;

    procedure GetDefaultPalette(var Palette: PaletteType);
      begin
        System.move(DefaultColors, Palette.Colors, sizeof(DefaultColors));
        { The default palette always has 256 entries, but in reality }
        { it depends on the number of colors possible.               }
        Palette.Size := PaletteSize;
        if PaletteSize > 256 then Palette.Size := 256;
      end;


  procedure SetAllPaletteDefault(const Palette:PaletteType);
   var
    i: longint;
    Size: longint;
   begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         exit;
       end;
     Size:=Palette.Size;  { number of entries...}
     { first determine if we are not trying to }
     { change too much colors...               }
     if Palette.Size > PaletteSize then
      begin
        exit;
      end;
     Dec(Size); { Color arrays are indexed according to zero }
     for i:=0 to Size do
      begin
        { skip if RGB values are -1 , as stated in the TP manual }
        if (Palette.Colors[i].Red <> -1) and (Palette.Colors[i].Green <> -1)
           and (Palette.Colors[i].Blue <> -1) then
              SetRGBPalette(i,
                Palette.Colors[i].Red,
                Palette.Colors[i].Green,
                Palette.Colors[i].Blue);
      end;
   end;

  {********************************************************}
  { Procedure SetPalette()                                 }
  {--------------------------------------------------------}
  { This routine changes the colorNum to the default       }
  { palette entry specified in the second parameter.       }
  { For example, SetPalette(0, Lightcyan) makes the        }
  { 0th palette entry to the default Light Cyan Color .    }
  {********************************************************}
   Procedure SetPalette(ColorNum: word; Color: shortint);
    begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         exit;
       end;
      { Check if we can actually change that palette color }
      if ColorNum > PaletteSize then
        Begin
          exit;
        end
      else
      { Check if the max. default color is reached...}
      { no, this disables palette setting for 256 color modes! (JM) }
{      if Color > EGAWhite then
        begin
          _GraphResult := grError;
          exit;
        end;}
      SetRGBPalette(ColorNum,
          DefaultColors[Color].Red,
          DefaultColors[Color].Green,
          DefaultColors[Color].Blue);
    end;



  function LineClipped(var x1, y1,x2,y2: smallint; xmin, ymin,
      xmax, ymax:smallint): boolean;
  {********************************************************}
  { Function LineClipped()                                 }
  {--------------------------------------------------------}
  { This routine clips the line coordinates to the         }
  { min. and max. values of the window. Returns TRUE if    }
  { the ENTIRE line was clipped.  Updated                  }
  { clipped line endpoints are also returned.              }
  { This algorithm is the classic Cohen-Sutherland line    }
  { clipping algorithm.                                    }
  {--------------------------------------------------------}
  var
   code1, code2: longint;
   code: longint;
   newx,newy: smallint;
   done:boolean;


    function outcode(x,y:smallint): longint;
    {********************************************************}
    { Function OutCode()                                     }
    {--------------------------------------------------------}
    { This routine determines if the specified end point     }
    { of a line lies within the visible window, if not it    }
    { determines in which window the point is.               }
    {--------------------------------------------------------}

    var
     code: longint;
    begin
      code := 0;
      if (x<xmin) then
        code:=code or LEFT
      else if (x>xmax) then
        code:=code or RIGHT;
      if (y>ymax) then
        code:=code or BOTTOM
      else if (y<ymin) then
        code:=code or TOP;

      outcode:=code;
    end;

  begin
    done:=false;
    code1:= OutCode(x1,y1);
    code2:= OutCode(x2,y2);

    while not done do
     begin
       { Accept trivially }
       { both points are in window }
       if ((code1=0) and (code2=0)) then
         begin
           done:=TRUE;
           LineClipped:=FALSE;
               exit;
             end
       else
       { Reject trivially }
       { Neither points are in window }
       if (code1 and code2) <> 0 then
         begin
           done:=true;
           LineClipped:=TRUE;
           exit;
         end
       else
          begin
            { Some points are partially out of the window }
            { find the new end point of the lines...      }
            if code1 = 0 then
             code:=code2
            else
             code:=code1;
            if (code and LEFT) <> 0 then
              begin
                newy:=y1+((y2-y1)*(xmin-x1)) div (x2-x1);
                newx:=xmin;
              end
            else
            if (code and RIGHT) <> 0 then
              begin
                newy:=y1+((y2-y1)*(xmax-x1)) div (x2-x1);
                newx:=xmax;
              end
            else
            if (code and BOTTOM) <> 0 then
              begin
                newx:=x1+((x2-x1)*(ymax-y1)) div (y2-y1);
                newy:=ymax;
              end
            else
            if (code and TOP) <> 0 then
              begin
                newx:=x1+((x2-x1)*(ymin-y1)) div (y2-y1);
                newy:=ymin;
              end;
           if (code1 = code) then
            begin
              x1 := newx;  y1:= newy;
              code1:=outcode(x1,y1)
            end
               else
            begin
              x2:= newx; y2:= newy;
              code2:=outcode(x2,y2);
            end
         end;
      end;
  LineClipped:=FALSE;
end;

  procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);

   begin
    Activelineinfo:=Lineinfo;
   end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

{ Functions to create standard palettes, by Giulio Bernardi 2005 }

{ A simple 1 bit black and white palette }
function CreateBlackAndWhitePalette : TFPPalette;
var fppal : TFPPalette;
    Col : TFPColor;
begin
  fppal:=TFPPalette.Create(2);
  Col.Alpha:=AlphaOpaque;
  Col.Red:=$FFFF; Col.Green:=$FFFF; Col.Blue:=$FFFF;
  fppal.Color[0]:=Col;
  Col.Red:=$0000; Col.Green:=$0000; Col.Blue:=$0000;
  fppal.Color[1]:=Col;
  Result:=fppal;
end;

{ The "standard" netscape 216-color palette (aka: web safe palette) }
function CreateWebSafePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(216);
  Col.Alpha:=AlphaOpaque;
  i:=0;
  Col.Red:=$FFFF;
  while true do
  begin
    Col.Green:=$FFFF;
    while true do
    begin
      Col.Blue:=$FFFF;
      while true do
      begin
        fppal.Color[i]:=Col;
        if Col.Blue=0 then break;
        dec(Col.Blue,$3333);
      end;
      if Col.Green=0 then break;
      dec(Col.Green,$3333);
    end;
    if Col.Red=0 then break;
    dec(Col.Red,$3333);
  end;
  Result:=fppal;
end;

{ A grayscale palette. Not very useful. }
function CreateGrayScalePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(256);
  Col.Alpha:=AlphaOpaque;
  for i:=0 to $FF do
  begin
    Col.Red:=i;
    Col.Red:=(Col.Red shl 8) + Col.Red;
    Col.Green:=Col.Red;
    Col.Blue:=Col.Red;
    fppal.Color[i]:=Col;
  end;
  Result:=fppal;
end;

{ Standard VGA 16 color palette. }
function CreateVGAPalette : TFPPalette;
var fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(16);
  fppal.Color[0]:=colBlack;
  fppal.Color[1]:=colNavy;
  fppal.Color[2]:=colBlue;
  fppal.Color[3]:=colMaroon;
  fppal.Color[4]:=colPurple;
  fppal.Color[5]:=colDkGreen;
  fppal.Color[6]:=colRed;
  fppal.Color[7]:=colTeal;
  fppal.Color[8]:=colFuchsia;
  fppal.Color[9]:=colOlive;
  fppal.Color[10]:=colGray;
  fppal.Color[11]:=colLime;
  fppal.Color[12]:=colAqua;
  fppal.Color[13]:=colSilver;
  fppal.Color[14]:=colYellow;
  fppal.Color[15]:=colWhite;
  Result:=fppal;
end;

function RGBAToFPColor(Const RGBA: TColorRGBA) : TFPcolor;

begin
  with Result, RGBA do
    begin
    Red   :=(R shl 8) or R;
    Green :=(G shl 8) or G;
    Blue  :=(B shl 8) or B;
    Alpha :=255-A;
    Alpha :=(Alpha shl 8) or Alpha
    end;
end;

Function RGBToFPColor(Const RGB : TColorRGB) : TFPColor;

begin
  with Result,RGB do
    begin  {Use only the high byte to convert the color}
    Red   := (R shl 8) + R;
    Green := (G shl 8) + G;
    Blue  := (B shl 8) + B;
    Alpha := AlphaOpaque;
    end;
end;


function DropWhiteSpaces(Stream : TStream) :Char;

begin
  with Stream do
    begin
    repeat
      ReadBuffer(DropWhiteSpaces,1);
{If we encounter comment then eat line}
      if DropWhiteSpaces='#' then
      repeat
        ReadBuffer(DropWhiteSpaces,1);
      until DropWhiteSpaces=#10;
    until not(DropWhiteSpaces in WhiteSpaces);
    end;
end;

function ReadInteger(Stream : TStream) :Integer;

var
  s:String[7];

begin
  s:='';
  s[1]:=DropWhiteSpaces(Stream);
  with Stream do
    repeat
      Inc(s[0]);
      ReadBuffer(s[Length(s)+1],1)
    until s[Length(s)+1] in WhiteSpaces;
  Result:=StrToInt(s);
end;

Function FPColorToRGB(Const Color : TFPColor) : TColorRGB;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    end;
end;

Function FPColorToRGBA(Const Color : TFPColor) : TColorRGBA;

begin
  With Result,Color do
    begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    A:=(Alpha and $FF00) shr 8;
    end;
end;


procedure PatternToPoints (const APattern:TPenPattern; LinePoints:PLinePoints);
var r : integer;
    i : longword;
begin
  i := 1;
  for r := PatternBitCount-1 downto 1 do
    begin
    LinePoints^[r] := (APattern and i) <> 0;
    i := i shl 1;
    end;
  LinePoints^[0] := (APattern and i) <> 0;
end;
type
  TFuncSetColor = procedure (Canv:TFPCustomCanvas; x,y:integer; data:pointer);

  PDoneRec = ^TDoneRec;
  TDoneRec = record
    x, min, max : integer;
    next : PDoneRec;
  end;

  PFloodFillData = ^TFloodFillData;
  TFloodFillData = record
    Canv : TFPCustomCanvas;
    ReplColor : TFPColor;
    SetColor : TFuncSetColor;
    ExtraData : pointer;
    DoneList : TList;
  end;

function FindDoneIndex (const data:PFloodFillData; x:integer; var index:integer):boolean;
begin
  with data^.DoneList do
    begin
    index := 0;
    while (index < count) and (PDoneRec(items[index])^.x <> x) do
      inc (index);
    result := (index < count) and (PDoneRec(items[index])^.x = x);
    end;
end;

procedure FreeDoneList (const data:TFloodFillData);
  procedure FreeList (p:PDoneRec);
  var n : PDoneRec;
  begin
    while assigned(p) do
      begin
      n := p^.Next;
      dispose (p);
      p := n;
      end;
  end;
var r : integer;
begin
  with data do
  for r := 0 to DoneList.Count-1 do
    FreeList (PDoneRec(DoneList[r]));
end;
  procedure CheckAboveRange;
  var t,b : integer;
  begin
    with data^ do
      begin
      t := top - 1;
      while (t >= 0) and (Canv.colors[x,t]=ReplColor) do
        begin
        SetColor(Canv, x,t, ExtraData);
        dec (t);
        end;
      t := t + 1;
      b := top - 1;
      if t <= b then
        begin
        CheckFloodFillColor (x-1, t, b, -1, data);
        CheckFloodFillColor (x+1, t, b, 1, data);
        end;
      end;
  end;

  procedure CheckBelowRange;
  var r,t,b : integer;
  begin
    with data^ do
      begin
      r := Canv.Height;
      b := bottom + 1;
      t := b;
      while (b < r) and (Canv.colors[x,b]=ReplColor) do
        begin
        SetColor (Canv,x,b,ExtraData);
        inc (b);
        end;
      b := b - 1;
      if t <= b then
        begin
        CheckFloodFillColor (x-1, t, b, -1, data);
        CheckFloodFillColor (x+1, t, b, 1, data);
        end;
      end;
  end;

var DoAbove, DoBelow : boolean;

begin
  with data^ do
    begin
    if (x >= Canv.width) or (x < 0) then
      Exit;
    if top < 0 then
      top := 0;
    if bottom >= Canv.Height then
      bottom := Canv.Height-1;
    DoAbove := (Canv.colors[x,top] = ReplColor);
    DoBelow := (Canv.colors[x,bottom] = ReplColor);
    end;
  CheckRange;
  if DoAbove then
    CheckAboveRange;
  if DoBelow then
    CheckBelowRange;
end;

Function ToWord(AWord : TWordRec) : Word;

begin
  Result:=(AWord.Lo) or (AWord.Hi shl 8);
end;

Function FromWord(AWord : Word) : TWordRec;

begin
  Result.Lo:=AWord and $FF;
  Result.Hi:=(AWord shr 8) and $FF;
end;


procedure vgaSetColor(index,r,g,b:longint);
begin
    if (index < 0 or index > 255)
    begin
        exit;
    end;

    wait_retrace;

    writeportb(PAL_WRITE_ADDR, index);
    writeportb(PAL_DATA, r);
    writeportb(PAL_DATA, g);
    writeportb(PAL_DATA, b);
end;


procedure vgaSetPalette(start,count:longint;p:vgaColor);

var
  i:integer;
begin

    if (start < 0 or (start + count - 1) > 255)
    begin
        exit;
    end;

    wait_retrace
    writeportb(PAL_WRITE_ADDR, start);
    i:=0;
    repeat
        writeportb(PAL_DATA, p.red);
        writeportb(PAL_DATA, p.green);
        writeportb(PAL_DATA, p.blue);
        inc(p);
        inc(i);
    until i > count;
    
end;

^^^^^
procedure InitGraph(mode:word; clear:boolean);

var
  temp:longint;
  origMode:word; 
  Supported,VESASupported:boolean;

begin
   AllocVesaStrucs; //put the VBEInfo records in RAM

//During this process we set one of two variables by setting VBE2 or VESA in ModeInfo block and get the result.
//TODO: code needs be added for this.

   VESASupported:=checkVESASupported;
   if VESASupported=false then begin
       textmode(3);
       writeln('Card does not SUPPORT VESA, Required for GRAPH mode operation.');	
   end;
   //is VESA 3.0 supported?? Use init_VBE if so to get PM Interface EP.
   // Version 2+ allow direct calls to PM interface via standard calls like used to do for v1.2 in RM.
   // The HW take care of the rest for VBE 2.
   // GRUB Provides a VBE 2 Interface EP if Available. (replace int10 with EP??)
   GetVesaModes;
   Supported:=CheckInSupportedModes(mode); //are we there yet?
   if Supported=false then begin
       textmode(3);
       writeln('Specific VESA Mode not available.Exiting.');	
   end;
    
    temp := (temp + 0xffffL) shr 16;  //generally a SAFE bet, 64K page size
    banksPerPage := temp;
   GetModeInfo(Mode);  //Useful information sometimes gleamed here
   vbeGetMode(origMode);      // save the current mode, the one BEFORE we got called, to we can RETURN to it.
   SetVesaMode(Mode,Clear); //Sets the NEW VESA mode with optional clear screen.
end;

//YES. Its THAT EASY.It took me a minute to figure out as I was knee deep i nassembler for no reason yet AGAIN.


procedure CloseGraph;

begin
  ClearGraph;  //Just in case.
  DeAllocVesaStrucs; //FREE RAM that was used for VBEInfo records
  SetMode(3);
  Clrscr;  //might be done already.Doing it agian wont hurt.
  
  //As this unit INTENDS on being REUSEABLE in other projects, everyone else can cut the procedure off here.
  
  
  //in our case ONLY, FPOS specific.
  //  SwitchActiveConsole(Buffer1);  
  //Switch to the FIRST vTerm on exit Graph mode. Leave here as an option.
  
  {were are not at multiple logins yet, so Im keeping it simple}
  
  writeln('FPOS> ');
  BlinkCursor;  
  ScrollDisabled:=false; // we want to resume scroling for a VT.
  
  
  //TECHnically, we can stay in GraphMode, clear the screen(which you dont do inside a 2D window mode)
  // and then POWER OFF with a Blank Screen.
  
end;

procedure bytemove(var source, dest; count: SmallInt);
var
  s, d: PByte;
begin
  s := PByte(@source);
  d := PByte(@dest);
  while count > 0 do begin
    d^ := s^;
    Inc(d);
    Inc(s);
    Dec(count);
  end;
end;

procedure PutPixel16(X,Y : SmallInt; Pixel: Word);
var
  offset: word;
  dummy: byte;
begin
  Inc(x, StartXViewPort);
  Inc(y, StartYViewPort);
  { convert to absolute coordinates and then verify clipping...}
  if ClipPixels then
  begin
    if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
      exit;
    if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
      exit;
  end;
  offset := (y * 80 + (x shr 3) + VideoOfs);
  WritePortW($3ce, $0f01);       { Index 01 : Enable ops on all 4 planes }
  WritePortW($3ce, ((Pixel and $ff) shl 8)); { Index 00 : Enable correct plane and write color }

  WritePortW($3ce, (8 or ($8000 shr (x and $7))));{ Select correct bits to modify }
  dummy := VidMem^[offset];     { Read data byte into VGA latch register }
  VidMem^[offset] := dummy;     { Write the data into video memory }
end;


function GetPixel16(X,Y: SmallInt):word;
var
  dummy, offset: Word;
  shift: byte;
begin
  Inc(x, StartXViewPort);
  Inc(y, StartYViewPort);
  offset := Y * 80 + (x shr 3) + VideoOfs;
  WritePortW($3ce, 4);
  shift := 7 - (X and 7);
  dummy := (VidMem^[offset] shr shift) and 1;
  WritePortB($3cf, 1);
  dummy := dummy or (((VidMem^[offset] shr shift) and 1) shl 1);
  WritePortB($3cf, 2);
  dummy := dummy or (((VidMem^[offset] shr shift) and 1) shl 2);
  WritePortB($3cf, 3);
  dummy := dummy or (((VidMem^[offset] shr shift) and 1) shl 3);
  GetPixel16 := dummy;
end;


procedure GetScanLine16(x1, x2, y: SmallInt; var data);
var
  dummylong: longint;
  Offset, count, count2, amount, index: word;
  plane: byte;
begin
  inc(x1,StartXViewPort);
  inc(x2,StartXViewPort);
  offset := (Y + StartYViewPort) * 80 + (x1 shr 3) + VideoOfs;
  { first get enough pixels so offset is 32bit aligned }
  amount := 0;
  index := 0;
  If ((x1 and 31) <> 0) Or
     ((x2-x1+1) < 32) Then
    Begin
      If ((x2-x1+1) >= 32+32-(x1 and 31)) Then
        amount := 32-(x1 and 31)
      Else amount := x2-x1+1;
      For count := 0 to amount-1 do
        WordArray(Data)[Count] := getpixel16(x1-StartXViewPort+Count,y);
      index := amount;
      Inc(Offset,(amount+7) shr 3);
    End;
  amount := x2-x1+1 - amount;
  If amount = 0 Then Exit;
  WritePortB($3ce, 4);
  { first get everything from plane 3 (4th plane) }
  WritePortB($3cf, 3);
  Count := 0;
  For Count := 1 to (amount shr 5) Do
    Begin
      dummylong := PLongInt(@VidMem^[offset+(Count-1)*4])^;
      dummylong :=
        ((dummylong and $ff) shl 24) or
        ((dummylong and $ff00) shl 8) or
        ((dummylong and $ff0000) shr 8) or
        ((dummylong and $ff000000) shr 24);
      For Count2 := 31 downto 0 Do
        Begin
          WordArray(Data)[index+Count2] := DummyLong and 1;
          DummyLong := DummyLong shr 1;
        End;
      Inc(Index, 32);
    End;
{ Now get the data from the 3 other planes }
  plane := 3;
  Repeat
    Dec(Index,Count*32);
    Dec(plane);
    WritePortB($3cf, plane);
    Count := 0;
    For Count := 1 to (amount shr 5) Do
      Begin
        dummylong := PLongInt(@VidMem^[offset+(Count-1)*4])^;
        dummylong :=
          ((dummylong and $ff) shl 24) or
          ((dummylong and $ff00) shl 8) or
          ((dummylong and $ff0000) shr 8) or
          ((dummylong and $ff000000) shr 24);
        For Count2 := 31 downto 0 Do
          Begin
            WordArray(Data)[index+Count2] :=
              (WordArray(Data)[index+Count2] shl 1) or (DummyLong and 1);
            DummyLong := DummyLong shr 1;
          End;
        Inc(Index, 32);
      End;
  Until plane = 0;
  amount := amount and 31;
  Dec(index);
  For Count := 1 to amount Do
    WordArray(Data)[index+Count] := getpixel16(index+Count,y);
end;


procedure DirectPutPixel16(X,Y : SmallInt);
{ x,y -> must be in global coordinates. No clipping. }
var
  color: word;
  offset: word;
  dummy: byte;
begin
  case CurrentWriteMode of
    XORPut:
      begin
        { getpixel wants local/relative coordinates }
        Color := GetPixel(x - StartXViewPort, y - StartYViewPort);
        Color := CurrentColor xor Color;
      end;
    OrPut:
      begin
        { getpixel wants local/relative coordinates }
        Color := GetPixel(x - StartXViewPort, y - StartYViewPort);
        Color := CurrentColor or Color;
      end;
    AndPut:
      begin
        { getpixel wants local/relative coordinates }
        Color := GetPixel(x - StartXViewPort, y - StartYViewPort);
        Color := CurrentColor and Color;
      end;
    NotPut:
      Color := Not Color;
    else
      Color := CurrentColor;
  end;
  offset := (Y * 80 + (X shr 3) + VideoOfs);
  WritePortW($3ce, $f01);
  WritePortW($3ce, (Color shl 8));
  WritePortW($3ce, (8 or $8000 shr (X and 7)));
  dummy := VidMem^[offset];
  VidMem^[offset] := dummy;
end;




function ConvertString(const OrigString: String): String;
var
  i: Integer;
  ConvResult: String;
begin
  if GraphStringTransTable = nil then
    ConvertString := OrigString
  else
  begin
    SetLength(ConvResult, Length(OrigString));
    for i := 1 to Length(OrigString) do
      ConvResult[i] := GraphStringTransTable^[OrigString[i]];
    ConvertString := ConvResult;
  end;
end;


    procedure GetTextPosition(var xpos,ypos: longint; const TextString: string);
     begin
         if CurrentTextInfo.Font = DefaultFont then
          begin
           if Currenttextinfo.direction=horizdir then
            begin
              case Currenttextinfo.horiz of
                   centertext : XPos:=(textwidth(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textwidth(textstring);
              end;
              case Currenttextinfo.vert of
                  centertext : YPos:=-(textheight(textstring) shr 1);
                  bottomtext : YPos:=-textheight(textstring);
                  toptext    : YPos:=0;
              end;
            end else
            begin
              case Currenttextinfo.horiz of
                   centertext : XPos:=(textheight(textstring) shr 1);
                   lefttext   : XPos:=textheight(textstring);
                   righttext  : XPos:=textheight(textstring);
              end;
              case Currenttextinfo.vert of
                  centertext : YPos:=(textwidth(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textwidth(textstring);
              end;
            end;
          end
         else
          begin
            if Currenttextinfo.direction=horizdir then
            begin
              case CurrentTextInfo.horiz of
                   centertext : XPos:=(textwidth(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textwidth(textstring);
              end;
              case CurrentTextInfo.vert of
                  centertext : YPos:=(textheight(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textheight(textstring);
              end;
            end else
            begin
              case CurrentTextInfo.horiz of
                   centertext : XPos:=(textheight(textstring) shr 1);
                   lefttext   : XPos:=0;
                   righttext  : XPos:=textheight(textstring);
              end;
              case CurrentTextInfo.vert of
                  centertext : YPos:=(textwidth(textstring) shr 1);
                  bottomtext : YPos:=0;
                  toptext    : YPos:=textwidth(textstring);
              end;
           end;
          end;
     end;

    procedure GetTextSettings(var TextInfo : TextSettingsType);

      begin
         textinfo:=currenttextinfo;
      end;

  procedure SetTextJustify(horiz,vert : word);

      begin
         if (horiz<0) or (horiz>2) or
            (vert<0) or (vert>2) then
           begin
              _graphresult:=grError;
              exit;
           end;
         Currenttextinfo.horiz:=horiz;
         Currenttextinfo.vert:=vert;
      end;

    function TextHeight(const TextString : string) : word;

      begin
         if Currenttextinfo.font=DefaultFont
            then TextHeight:=8*CurrentTextInfo.CharSize
            else
              TextHeight:=Trunc((fonts[Currenttextinfo.font].header.org_to_cap-
                fonts[Currenttextinfo.font].header.org_to_dec) * CurrentYRatio);
      end;

    function TextWidth(const TextString : string) : word;
      var i,x : smallint;
          c   : byte;
          s   : String;
      begin
         x := 0;
         { if this is the default font ... }
         if Currenttextinfo.font = Defaultfont then
            TextWidth:=length(TextString)*8*CurrentTextInfo.CharSize
         { This is a stroked font ... }
            else begin
               s := ConvertString(TextString);
               for i:=1 to length(s) do
                begin
                   c:=byte(s[i]);
{                   dec(c,fonts[Currenttextinfo.font].header.first_char);}
                   if (c-fonts[Currenttextinfo.font].header.first_char>=
                       fonts[Currenttextinfo.font].header.nr_chars) then
                     continue;
                   x:=x+byte(fonts[Currenttextinfo.font].widths[c]);
               end;
             TextWidth:=round(x * CurrentXRatio) ;
            end;
      end;

   
    procedure OutText(const TextString : string);
      var x,y:smallint;
      begin
         { Save CP }
         x:=CurrentX;
         y:=CurrentY;
         OutTextXY(CurrentX,CurrentY,TextString);
         { If the direction is Horizontal and the justification left }
         { then and only then do we update the CP                    }
         if (Currenttextinfo.direction=HorizDir) and
           (Currenttextinfo.horiz=LeftText) then
               inc(x,textwidth(TextString));
         { Update the CP }
         CurrentX := X;
         CurrentY := Y;
      end;

    procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);
      begin
         CurrentXRatio := MultX / DivX;
         CurrentYRatio := MultY / DivY;
      end;

 procedure res2Mode(x, y, maxColor: longint; var mode: smallInt);
 var
   l: longint;
 begin
   case maxColor of
     2: driver := D1bit;
     4: driver := D2bit;
     16: driver := D4bit;
     64: driver := D6bit;
     256: driver := D8bit;
     4096: driver := D12bit;
     32768: driver := D15bit;
     65536: driver := D16bit;
     16777216: driver := D24bit;
     4294836225: driver := D32bit;
   end;
   { Check whether this is known/predefined mode }
   for l := lowNewMode to highNewMode do
     if (resolutions[l].x = x) and
        (resolutions[l].y = y) then
       begin
         { Found! }
         mode := l;
         exit;
       end;
   { Not Found }
   mode := maxsmallint;
 end;

function mode2res(modeNr: smallInt; var x,y: longint): boolean;
begin
  if (modeNr < lowNewMode) or
     (modeNr > highNewMode) then
    begin
      mode2res := false;
      exit;
    end;
  mode2res := true;
  x := resolutions[modeNr].x;
  y := resolutions[modeNr].y;
end;


   function GetGraphMode: smallint;
     begin
      GetGraphMode := IntCurrentMode;
     end;

  procedure SetAllPaletteDefault(const Palette:PaletteType);
   var
    i: longint;
    Size: longint;
   begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         _GraphResult := grError;
         exit;
       end;
     Size:=Palette.Size;  { number of entries...}
     { first determine if we are not trying to }
     { change too much colors...               }
     if Palette.Size > PaletteSize then
      begin
        _GraphResult := grError;
        exit;
      end;
     Dec(Size); { Color arrays are indexed according to zero }
     for i:=0 to Size do
      begin
        { skip if RGB values are -1 , as stated in the TP manual }
        if (Palette.Colors[i].Red <> -1) and (Palette.Colors[i].Green <> -1)
           and (Palette.Colors[i].Blue <> -1) then
              SetRGBPalette(i,
                Palette.Colors[i].Red,
                Palette.Colors[i].Green,
                Palette.Colors[i].Blue);
      end;
   end;

   Procedure SetPalette(ColorNum: word; Color: shortint);
    begin
     { palette routines do not work in DirectColor mode }
     if DirectColor then
       begin
         _GraphResult := grError;
         exit;
       end;
      { Check if we can actually change that palette color }
      if ColorNum > PaletteSize then
        Begin
          _GraphResult := grError;
          exit;
        end
      else
      SetRGBPalette(ColorNum,
          DefaultColors[Color].Red,
          DefaultColors[Color].Green,
          DefaultColors[Color].Blue);
    end;


    procedure GetPalette(var Palette: PaletteType);
      var
        i: longint;
        size : longint;
      begin
        { palette routines do not work in DirectColor mode }
        if DirectColor then
         begin
           _GraphResult := grError;
           exit;
         end;
        Palette.Size := PaletteSize;
        { index at zero }
        size := PaletteSize - 1;
        for i:=0 to size do
          GetRGBPalette(i,
             Palette.Colors[i].Red,
             Palette.Colors[i].Green,
             Palette.Colors[i].Blue);
      end;

    function GetPaletteSize: smallint;
     begin
       GetPaletteSize := PaletteSize;
     end;

    procedure GetDefaultPalette(var Palette: PaletteType);
      begin
        System.move(DefaultColors, Palette.Colors, sizeof(DefaultColors));
        { The default palette always has 256 entries, but in reality }
        { it depends on the number of colors possible.               }
        Palette.Size := PaletteSize;
        if PaletteSize > 256 then Palette.Size := 256;
      end;


  Procedure PopPoint (Var x, y : smallint);
  Begin
   If Buffer.WordIndex>1 then
    Begin
      x:=Buffer.Words[Buffer.WordIndex-2];
      y:=Buffer.Words[Buffer.WordIndex-1];
      Dec (Buffer.WordIndex,2);
    End
   Else
    Begin
      x:=-1;
      y:=-1;
    End;
  End;

 Function AlreadyDrawn(x, y: smallint): boolean;
  var
    temp : PFloodLine;
   begin
    AlreadyDrawn := false;
    temp := DrawnList[y div YResDiv];
    while assigned(temp) do
      begin
        if (temp^.y = y) and
           (temp^.x1 <= x) and
           (temp^.x2 >= x) then
          begin
            AlreadyDrawn := true;
            exit;
          end;
        temp := temp^.next;
      end;
   end;

  Procedure CleanUpDrawnList;
  var
    l: longint;
    temp1, temp2: PFloodLine;
  begin
    for l := 0 to high(DrawnList) do
      begin
        temp1 := DrawnList[l];
        while assigned(temp1) do
          begin
            temp2 := temp1;
            temp1 := temp1^.next;
            dispose(temp2);
          end;
      end;
  end;

begin

   { Reset to default values }
    IntCurrentMode := 0;
    XAspect := 0;
    YAspect := 0;
    MaxX := 0;
    MaxY := 0;
    MaxColor := 0;
    PaletteSize := 0;
    DirectColor := FALSE;
    HardwarePages := 0;

    // ArcCall:=0;
     lineinfo.linestyle:=solidln;
     lineinfo.thickness:=normwidth;
     { reset line style pattern }
     for i:=0 to 15 do
       LinePatterns[i] := TRUE;

     { By default, according to the TP prog's reference }
     { the default pattern is solid, and the default    }
     { color is the maximum color in the palette.       }
     fillsettings.color:=GetMaxColor;
     fillsettings.pattern:=solidfill;
     { GraphDefaults resets the User Fill pattern to $ff }
     for i:=1 to 8 do
       FillPatternTable[UserFill][i] := $ff;
     CurrentColor:=white;

     ClipPixels := TRUE;
     { Reset the viewport }
     StartXViewPort := 0;
     StartYViewPort := 0;
     ViewWidth := MaxX;
     ViewHeight := MaxY;

     { Reset CP }
     CurrentX := 0;
     CurrentY := 0;

     SetBkColor(Black);

     { normal write mode }
     CurrentWriteMode := CopyPut;

     { set font style }
     CurrentTextInfo.font := DefaultFont;
     CurrentTextInfo.direction:=HorizDir;
     CurrentTextInfo.charsize:=1;
     CurrentTextInfo.horiz:=LeftText;
     CurrentTextInfo.vert:=TopText;

     XAspect:=10000; 
     YAspect:=10000;

end.
