
Unit graphdraw;
{
ALL of the Drawing and canvas options for GraphMode.
These might need be rewritten due to the breakage of the BGI.
The reason here is that the BGI is outdated and needs a VESA/libSVGA rewrite anyway.

OBJPAS is NOT supported here because we have no way to implement classes.
All of the other default standard *nix pointers point to BS. I've already looked. 

unit needs to have duplicates removed and be put together.
}

{Bar3D constants}
const  TopOn = true;
      TopOff = false;

{flood mode constants}
const  BorderFlood = smallint(0);
      SurfaceFlood = smallint(1);

procedure Bar(x1,y1,x2,y2:smallint);
procedure Bar3D(x1,y1,x2,y2:smallint; depth:word; top:boolean);
procedure Chord(x,y:smallint; start,stop,xradius,yradius:word);
procedure FillEllipse(x,y:smallint;xradius,yradius:word);
procedure FillPoly(nrpoints:word; var polypoints);
procedure FillRect(x1,y1,x2,y2:smallint);
procedure FloodFill(x,y:smallint; color:longword);
procedure GetFillPattern(out fillpattern:FillPatternType);
procedure GetFillSettings(out fillinfo:FillSettingsType);
procedure InvertRect(x1,y1,x2,y2:smallint);
procedure PieSlice(x,y:smallint; start,stop,radius:word);
procedure RoundRect(x1,y1,x2,y2,r:smallint);
procedure Sector(x,y:smallint; start,stop,xradius,yradius:word);
procedure SetFillPattern(fillpattern:FillPatternType; color:longword);
procedure SetFillStyle(pattern:word; color:longword);
procedure SetFloodMode(floodmode:smallint);

procedure Arc(x,y:smallint; start,stop,radius:word);
procedure Circle(x,y:smallint; radius:word);
procedure DrawBezier(nrpoints:word; var polypoints);
procedure DrawPoly(nrpoints:word; var polypoints);
procedure Ellipse(x,y:smallint; start,stop,xradius,yradius:word);
procedure Line(x1,y1,x2,y2:smallint);
procedure LineRel(dx,dy:smallint);
procedure LineTo(x,y:smallint);
procedure MoveRel(dx,dy:smallint);
procedure MoveTo(x,y:smallint);
procedure PutPixel(x,y:smallint; color:longword);
procedure Rectangle(x1,y1,x2,y2:smallint);
procedure RotEllipse(x,y,rot:smallint; xradius,yradius:word);

procedure Rectangle(x1, y1, x2, y2 : word; Color : byte);
procedure Line(X1,Y1,X2,Y2: Integer; Color: Byte);
Procedure HLine(x,y,x2: integer; color: byte);
Procedure VLine(x,y,y2: integer; color: byte);
procedure fillrect_bpp16(graphmem:Pchar; r,g, b,h,w:char);
procedure fillrect_bpp32(graphmem:Pchar; r,g, b,h,w:char);

procedure Rectangle(x1,y1,x2,y2: word; Color: byte);
begin
  Line(x1,y1,x2,y1,Color);
  Line(x2,y1,x2,y2,Color);
  Line(x2,y2,x1,y2,Color);
  Line(x1,y2,x1,y1,Color);
end;


procedure HUGEfillRect(x,y:int16;width, height,color:int16);
var
  i,lines,x1,x2,y1,y2:int16;
  endOffset,endBank,offset,startBank:int32;
  address,here:PtrUInt8;
  start:uint32;

begin
    x1 = x;
    x2 = x + width - 1;
    y1 = y;
    y2 = y + height - 1;

    //
    // first off, clip to the screen.
    //
    if (x1 < 0) then
    begin
        x1 = 0;
    end;

    if (x2 > maxx) then
    begin
        x2 = maxx;
    end;

    if (y1 < 0)then 
    begin
        y1 = 0;
    end;

    if (y2 > maxy) then
    begin
        y2 = maxy;
    end;

    //
    // is there a rectangle left?
    //
    if (y2 < y1) or (x2 < x1) then
    begin
        exit;
    end;

    width = x2 - x1 + 1;

    //
    // compute the start and end pages
    //
    offset = (y1 * modeInfo.width) + x1;
    startBank = (offset shr 16) + activePageOffset;

    endOffset = (y2 * modeInfo.width) + x2;
    endBank = (endOffset shr 16) + activePageOffset;

    //
    // select the video page if we need to
    //
    if (startBank <> currentBank) then
    begin
        currentBank = startBank;
        vbeSetBankAAddr(currentBank);
    end;

    //
    // if the rectangle is all in one page
    // draw it the easy way
    //
    offset:= offset  and $ffff;
    if (startBank = endBank) then
    begin
        address = farPtr($a000, offset);
	i:=y1;
        repeat 
        begin
            memset(address, color, width);
            address:=address + modeInfo.width;
	    inc(i);
        until i>=y2;
    end;
    else
    begin
        //
        // the rectangle spans more than one page
        // do as much as you can in each page
        //
        height = y2 - y1 + 1;
        while (height > 0)
        begin
            
            //
            // how many scan lines of the rectangle
            // fit on this page?
            // 
            lines = ($10000 - offset) / modeInfo.width;
            lines = min(lines, height);

            height:=height - lines;

            //
            // fill as many lines as possible
            // in this page
            //
 	    i:=0;
            repeat
                memset(farPtr ($a000, offset),color,width);
                offset:offset + modeInfo.width;
		inc(i);
            until (i>lines);

            //
            // handle the case where a scan line crosses
            // a page boundry
            //
            if (height > 0) then
            begin
                dec(height);
                start = $10000 - offset;
                if (start >= width) then
                begin
                    memset( farPtr($a000,  offset),color,width);
                    inc(currentBank);
                    vbeSetBankAAddr(currentBank);
                end;
                else
                begin
                    memset( farPtr($a000,  offset),color,start);
                    inc(currentBank);
                    vbeSetBankAAddr(currentBank);
                    memset(farPtr($a000, 0), color,width - start);
                end;
            end;
            offset:=offset + modeInfo.width;
            offset:offset and  $ffff;
        end;
    end;
end;




procedure GetFillPattern(out fillpattern:FillPatternType);
begin
  fillpattern:=wingraph.fillPattern;
end;

procedure GetFillSettings(out fillinfo:FillSettingsType);
begin
  fillinfo:=fillSettings;
end;

procedure SetFloodMode(floodmode:smallint);
begin
  case floodmode of
    BorderFlood : wingraph.floodMode:=FLOODFILLBORDER;
    SurfaceFlood: wingraph.floodMode:=FLOODFILLSURFACE;
  end;
end;

   grBitmap                    : array[0..NrVideoPages-1] of HBITMAP;
    lastArcCoords               : ArcCoordsType;

var

  ArcCall: ArcCoordsType;   { Information on the last call to Arc or Ellipse }

  { Custom canvas-like object that handles postscript code }
  TPostScriptCanvas = class(TFPCustomCanvas)
  private
    FHeight,FWidth : Integer;
    FStream : TStream;
    FLineSpacing: Integer;
    LastX: Integer;
    LastY: Integer;
    function TranslateY(Ycoord: Integer): Integer; // Y axis is backwards in postscript
    procedure AddFill;
    procedure ResetPos; // reset back to last moveto location
    procedure SetWidth (AValue : integer); override;
    function  GetWidth : integer; override;
    procedure SetHeight (AValue : integer); override;
    function  GetHeight : integer; override;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    procedure DrawRectangle(const Bounds: TRect; DoFill : Boolean);
    procedure DrawEllipse(const Bounds: TRect; DoFill : Boolean);
  public
    constructor Create(AStream : TStream);
    destructor Destroy; override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    Procedure DoMoveTo(X1,Y1 : Integer); override;
    Procedure DoLineTo(X1,Y1 : Integer); override;
    Procedure DoLine(X1,Y1,X2,Y2 : Integer); override;
    Procedure DoRectangle(Const Bounds : TRect); override;
    Procedure DoRectangleFill(Const Bounds : TRect); override;
    procedure DoPolyline(Const Points: Array of TPoint); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoPie(x,y,awidth,aheight,angle1,angle2 : Integer);

    procedure Writeln(AString: String);
    procedure TextOut(X,Y: Integer; const Text: String);

    Property Stream : TStream read FStream;
  end;

  { Encapsulates ALL the postscript and uses the TPostScriptCanvas object for a single page }
  TPostScript = class(TComponent)
  private
    FDocStarted : Boolean;
    FCreator : String;
    FStream : TStream;
    FCanvas: TPostScriptCanvas;
    FHeight: Integer;
    FLineSpacing: Integer;
    FPageNumber: Integer;
    FTitle: String;
    FWidth: Integer;
    FPatterns: TList;   // array of pointers to pattern objects
    procedure SetHeight(const AValue: Integer);
    procedure SetLineSpacing(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure UpdateBoundingBox;
    procedure PatternChanged(Sender: TObject);
    procedure InsertPattern(APattern: TPSPattern); // adds the pattern to the postscript
    Procedure SetStream (Value : TStream);
    Function GetCreator : String;
  Protected
    Procedure WritePS(Const Cmd : String);
    Procedure WritePS(Const Fmt : String; Args : Array of Const);
    Procedure WriteDocumentHeader; virtual;
    Procedure WriteStandardFont; virtual;
    Procedure WritePage; virtual;
    Procedure FreePatterns;
    Procedure CheckStream;
  public
    Constructor Create(AOwner : TComponent);
    destructor Destroy; override;

    procedure AddPattern(APSPattern: TPSPattern);
    function FindPattern(AName: String): TPSPattern;
    function DelPattern(AName: String): Boolean;
    function NewPattern(AName: String): TPSPattern;
    property Canvas: TPostScriptCanvas read FCanvas;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property PageNumber: Integer read FPageNumber;
    property Title: String read FTitle write FTitle;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    procedure BeginDoc;
    procedure NewPage;
    procedure EndDoc;
    Property Stream : TStream Read FStream Write SetStream;
    Property Creator : String Read GetCreator Write FCreator;
  end;


  TPostScript = class;
  TPSPaintType = (ptColored, ptUncolored);
  TPSTileType = (ttConstant, ttNoDistortion, ttFast);
  TPostScriptCanvas = class; // forward reference

  TPSPattern = class(TFPCanvasHelper)
  private
    FStream : TMemoryStream;
    FPatternCanvas : TPostScriptCanvas;
    FOldName: String;
    FOnChange: TNotifyEvent;
    FBBox: TRect;
    FName: String;
    FPaintType: TPSPaintType;
    FPostScript: TStringList;
    FTilingType: TPSTileType;
    FXStep: Real;
    FYStep: Real;
    function GetpostScript: TStringList;
    procedure SetBBox(const AValue: TRect);
    procedure SetName(const AValue: String);
    procedure SetPaintType(const AValue: TPSPaintType);
    procedure SetTilingType(const AValue: TPSTileType);
    procedure SetXStep(const AValue: Real);
    procedure SetYStep(const AValue: Real);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    property BBox: TRect read FBBox write SetBBox;
    property PaintType: TPSPaintType read FPaintType write SetPaintType;
    property TilingType: TPSTileType read FTilingType write SetTilingType;
    property XStep: Real read FXStep write SetXStep;
    property YStep: Real read FYStep write SetYStep;
    property Name: String read FName write SetName;
    property GetPS: TStringList read GetPostscript;
    property OldName: string read FOldName write FOldName; // used when notifying that name changed
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    Property PatternCanvas : TPostScriptCanvas Read FPatternCanvas;
  end;
  PPSPattern = ^TPSPattern; // used for array

  TPSFont = Class(TFPCustomFont)
  end;

{  Basic canvas definitions}

{ TODO : take resolution in account to find the size }
{ TODO : speed optimization: search glyphs with a hash-function/tree/binary search/... }
{ TODO : memory optimization: TStringBitmaps keeps for each differnet character
         only 1 bitmap }
{ TODO : load other files depending on the extention }
{ possible TODO : different sizes/resolutions for x and y }
{ possible TODO : TFontmanager can fill a list of all the fonts he can find
              fontfiles and faces available in a fontfile }

{   Image Canvas - canvas which draws on an image.}
  TFPImageCanvas = class (TFPPixelCanvas)
  private
    FImage : TFPCustomImage;
  protected
    procedure SetColor (x,y:integer; const AValue:TFPColor); override;
    function  GetColor (x,y:integer) : TFPColor; override;
    procedure SetHeight (AValue : integer); override;
    function  GetHeight : integer; override;
    procedure SetWidth (AValue : integer); override;
    function  GetWidth : integer; override;
  public
    constructor create (AnImage : TFPCustomImage);
    destructor destroy; override;
    property Image : TFPCustomImage read FImage write FImage;
  end;


  TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,psClear);
  TFPPenStyleSet = set of TFPPenStyle;
  TFPPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
                pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
                pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  TPenPattern = Longword;

  TFPCustomPen = class (TFPCanvasHelper)
  private
    FStyle : TFPPenStyle;
    FWidth : Integer;
    FMode : TFPPenMode;
    FPattern : longword;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetMode (AValue : TFPPenMode); virtual;
    procedure SetWidth (AValue : Integer); virtual;
    procedure SetStyle (AValue : TFPPenStyle); virtual;
    procedure SetPattern (AValue : longword); virtual;
  public
    function CopyPen : TFPCustomPen;
    // Creates a copy of the pen with all properties the same, but not allocated
    property Style : TFPPenStyle read FStyle write SetStyle;
    property Width : Integer read FWidth write SetWidth;
    property Mode : TFPPenMode read FMode write SetMode;
    property Pattern : longword read FPattern write SetPattern;
  end;
 


      tfontrec = packed record
        name : string[8];
        header : THeader;        { font header   }
        pheader : TFHeader;      { prefix header }
        offsets : TOffsetTable;
        widths : TWidthTable;
        instrlength: longint;    { length of instr, because instr can }
        instr : pchar;           { contain null characters            }
      end;

{      pStroke = ^TStroke;}
      TStroke = packed record
        opcode: byte;
        x: smallint;  { relative x offset character }
        y: smallint;  { relative y offset character }
      end;


      TStrokes = Array[0..1000] of TStroke;

  ArcCoordsType = record
             x,y : smallint;
             xstart,ystart : smallint;
             xend,yend : smallint;
       end;


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
   {the draw mode type}
  DrawModeType  = RECORD
                    D_Mode,
                    D_FGColor,
                    D_BGColor,
                    D_Pattern,
                    D_Width,
                    D_BitBlit       :BYTE;
                  END;

Procedure HLine(x,y,x2: integer; color: byte);
Begin
  for x := x to x2 do putpix(x,y,color);
End;

Procedure VLine(x,y,y2: integer; color: byte);
Begin
  for y := y to y2 do putpix(x,y,color);
End;
 
procedure Line(X1, Y1, X2, Y2: Integer; Color: Byte);
var X, Y, Dx, Dy, Xs, Ys, Direction: Integer;
begin
  if x1 = x2 then hline(x1,y1,y2,color)
  else if y1 = y2 then vline(x1,y1,x2,color)
  else begin
    X := X1; Y := Y1; Xs := 1; Ys := 1;
    if X1 > X2 then Xs := -1;
    if Y1 > Y2 then Ys := 01;
    Dx := Abs(X2 - X1); Dy := Abs(Y2 - Y1);
    if Dx = 0 then direction := -1
    else Direction := 0;
    while not ((X = X2) and (Y = Y2)) do begin
      PutPix(X,Y,Color);
      if Direction < 0 then begin                               
        Inc(Y,Ys);
        Inc(Direction,Dx);
      end 
      else begin
        Inc(x,Xs);
        Dec(Direction,Dy);
      end;
    end;
  end;
end;  { Line }


Procedure line(a,b,c,d,col:integer);
  { This draws a line from a,b to c,d of color col. }
Function sgn(a:real):integer;
   BEGIN
        if a>0 then sgn:=+1;
        if a<0 then sgn:=-1;
        if a=0 then sgn:=0;
   END;

var u,s,v,d1x,d1y,d2x,d2y,m,n:real;
    i:integer;
BEGIN
     u:= c - a;
     v:= d - b;
     d1x:= SGN(u);
     d1y:= SGN(v);
     d2x:= SGN(u);
     d2y:= 0;
     m:= ABS(u);
     n := ABS(v);
     IF NOT (M>N) then
     BEGIN
          d2x := 0 ;
          d2y := SGN(v);
          m := ABS(v);
          n := ABS(u);
     END;
     s := INT(m / 2);
     FOR i := 0 TO round(m) DO
     BEGIN
          putpixel(a,b,col);
          s := s + n;
          IF not (s<m) THEN
          BEGIN
               s := s - m;
               a:= a +round(d1x);
               b := b + round(d1y);
          END
          ELSE
          BEGIN
               a := a + round(d2x);
               b := b + round(d2y);
          END;
     END;
END;


procedure DrawEllipse (mx,my, a,b, color: integer);

var   x,  mx1,mx2,  my1,my2: integer;
      aq,bq, dx,dy, r,rx,ry: longint;

begin
  PutPixel (mx + a, my, color);
  PutPixel (mx - a, my, color);

  mx1 := mx - a;   my1 := my;
  mx2 := mx + a;   my2 := my;

  aq := longint (a) * a;        {calc sqr}
  bq := longint (b) * b;
  dx := aq shl 1;               {dx := 2 * a * a}
  dy := bq shl 1;               {dy := 2 * b * b}
  r  := a * bq;                 {r  := a * b * b}
  rx := r shl 1;                {rx := 2 * a * b * b}
  ry := 0;                      {because y = 0}
  x := a;

  while x > 0
  do begin
    if r > 0
    then begin                  { y + 1 }
      inc (my1);   dec (my2);
      inc (ry, dx);             {ry = dx * y}
      dec (r, ry);              {r = r - dx + y}
    end;
    if r <= 0
    then begin                  { x - 1 }
      dec (x);
      inc (mx1);   dec (mx2);
      dec (rx, dy);             {rx = dy * x}
      inc (r, rx);              {r = r + dy * x}
    end;
    PutPixel (mx1, my1, color);
    PutPixel (mx1, my2, color);
    PutPixel (mx2, my1, color);
    PutPixel (mx2, my2, color);
  end;
end;


procedure drawbox(x,y : word;c : byte);
var
  i,j,a : word;
begin
{ Port writes are used to define which bitplane is used. }
  writePortb($3C4,$02);
  for i:=x to x+10 do
  begin
    writeportb($3C5,(1 shl (i and 3)));
    a:=(scrofs+i div 4+80*y);
    for j:=y to y+10 do
    begin
      Vidmem+a:=c; //see video unit, writechar procedure for example.
      inc(a,80)
    end;
  end;
end;


procedure animate1(WaitRetrace:boolean);
{ No buffering example. }
var
  x,y : integer;
begin
  repeat
    clearViewPort(0);
    drawbox(round(150+60*cos(time/speed)),
            round(90+60*sin(time/speed)),7);
    if WaitRetrace then
       WaitForRetrace; 
    inc(time)       
  until keypressed;
end;



procedure animate2(screen:longint);
{ Double buffering example. There are two virtual screens -
  one at $A000:0, second at $A000:$4000 }
var
  x,y  : integer;
begin
  repeat
    ClearViewPort(screen);
    drawbox(round(150+60*cos(time/speed)),
            round(90+60*sin(time/speed)),15);
    inc(time);
    setstartaddress(screen);
    WaitForRetrace; { Screen is flickering without this. }
    screen:=$4000-screen;
  until keypressed;
end;



procedure animate3(screen:longint);
{ Triple buffering example. In fact there are four virtual screens,
  at $A000:0, $A000:$4000, $A000:$8000 and at $A000:$C000, but if you
  use three screens the effect will remain the same.
  No waiting for retrace, no flickering. Speed of rectangle moves
  depends _only_ on a computer speed. You may add WaitForRetrace to
  allow synchronization. }
var
  x,y  : integer;
begin
  repeat
    clrscreen(screen);
    drawbox(round(150+60*cos(time/speed)),
            round(90+60*sin(time/speed)),13);
    inc(time);
    setstartaddress(screen);
    inc(screen,$4000);
  until keypressed;
end;


procedure fillrect_bpp8(graphmem:Pchar; r,g, b,h,w:char); 

var

  i,j,where:integer;

begin
   i:=0;
   j:=0;
   
   repeat
     repeat
       putpixel(vram,64+j,64+i,(r<<16)+(g<<8)+b);
      //this uses palettized colors, so needs some help.
       inc(j);
     until j>h;
     inc(where,3200);
     inc(i);
   until w>i;
end;

procedure fillrect_bpp16(graphmem:Pchar; r,g, b,h,w:char); 

var

  i,j,where:integer;

begin
   i:=0;
   j:=0;
   
   repeat
     repeat
       putpixel(vram,64+j,64+i,(r<<16)+(g<<8)+b);
       where[j*2]=r;
       where[j*2+1]=g;
       where[j*2+2]=b;
       inc(j);
     until j>h;
     inc(where,3200);
     inc(i);
   until w>i;
end;

procedure fillrect_bpp32(graphmem:Pchar; r,g, b,h,w:char); 

var

  i,j,where:integer;

begin
   i:=0;
   j:=0;
   
   repeat
     repeat
       putpixel(vram,64+j,64+i,(r<<16)+(g<<8)+b);
       where[j*4]=r;
       where[j*4+1]=g;
       where[j*4+2]=b;
       inc(j);
     until j>h;
     inc(where,3200);
     inc(i);
   until w>i;
end;


procedure Arc(x,y:smallint; start,stop,radius:word);
begin
  Ellipse(x,y,start,stop,radius,radius);
end;

procedure Circle(x,y:smallint; radius:word);
begin
  Ellipse(x,y,0,360,radius,radius);
end;


procedure DrawBezier(nrpoints:word; var polypoints);
var size,i: longint;
    points: array of TPoint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  SetLength(points,nrpoints);
  size:=nrpoints*SizeOf(PointType);
  Move(polypoints,points[0],size);
  for i:=0 to nrpoints-1 do with points[i] do
  begin
    Inc(x,origX); Inc(y,origY);
  end;
  if (nrpoints >= 4) then
  begin
    EnterCriticalSection(protect_devices);
    if grDirect then PolyBezier(grWindow,points[0],nrpoints);
    PolyBezier(grMemory,points[0],nrpoints);
    LeaveCriticalSection(protect_devices);
  end                else grResult:=grInvalidParam;
  SetLength(points,0);
end;

procedure DrawPoly(nrpoints:word; var polypoints);
var size,i: longint;
    points: array of TPoint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  if (nrpoints < 2) then begin
                           grResult:=grInvalidParam;
                           Exit;
                         end;
  SetLength(points,nrpoints);
  size:=nrpoints*SizeOf(PointType);
  Move(polypoints,points[0],size);
  for i:=0 to nrpoints-1 do with points[i] do
  begin
    Inc(x,origX); Inc(y,origY);
  end;
  EnterCriticalSection(protect_devices);
  if grDirect then Polyline(grWindow,points[0],nrpoints);
  Polyline(grMemory,points[0],nrpoints);
  LeaveCriticalSection(protect_devices);
  SetLength(points,0);
end;

procedure Ellipse(x,y:smallint; start,stop,xradius,yradius:word);
var nXStartArc,nYStartArc,nXEndArc,nYEndArc: longint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  lastArcCoords.x:=x; lastArcCoords.y:=y;
  Inc(x,origX); Inc(y,origY);
  nXStartArc:=Round(xradius*Cos(start*Rad)); nXEndArc:=Round(xradius*Cos(stop*Rad));
  nYStartArc:=Round(yradius*Sin(start*Rad)); nYEndArc:=Round(yradius*Sin(stop*Rad));
  if not(defAspectRatio) then
  begin
    xradius:=10000*xradius div aspX; yradius:=10000*yradius div aspY;
  end;
  EnterCriticalSection(protect_devices);
  if grDirect then
    windows.Arc(grWindow,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXStartArc,
                         y-nYStartArc,x+nXEndArc,y-nYEndArc);
  windows.Arc(grMemory,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXStartArc,
                       y-nYStartArc,x+nXEndArc,y-nYEndArc);
  LeaveCriticalSection(protect_devices);
  with lastArcCoords do
  begin
    xstart:=x+nXStartArc; ystart:=y-nYStartArc;
    xend:=x+nXEndArc; yend:=y-nYEndArc;
  end;
end;

procedure Line(x1,y1,x2,y2:smallint);
begin
  MoveTo(x1,y1);
  LineTo(x2,y2);
end;

procedure LineRel(dx,dy:smallint);
begin
  LineTo(actX+dx,actY+dy);
end;

procedure LineTo(x,y:smallint);
var x0,y0: smallint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  x0:=x; y0:=y;
  Inc(x,origX); Inc(y,origY);
  with lineSettings do
  begin
    if (linestyle <> UserBitLn) then
    begin
      EnterCriticalSection(protect_devices);
      if grDirect then windows.LineTo(grWindow,x,y);
      windows.LineTo(grMemory,x,y);
      LeaveCriticalSection(protect_devices);
      if (thickness = NormWidth) then PutPixel(x0,y0,frColor);
    end                         else
    begin
      EnterCriticalSection(protect_devices);
      globalTemp:=0;
      LineDDA(actX,actY,x0,y0,@LineProc,pattern);
      LeaveCriticalSection(protect_devices);
    end;
    MoveTo(x0,y0);
  end;
end;


procedure RotEllipse(x,y,rot:smallint; xradius,yradius:word);
var pt           : array[1..7] of TPoint;
    cosrot,sinrot: double;
    x1,y1,i      : longint;
begin
  xradius:=Round(1.3333*xradius);
  cosrot:=Cos(rot*Rad); sinrot:=Sin(rot*Rad);
  pt[1].x:=0;        pt[1].y:=-yradius;
  pt[2].x:= xradius; pt[2].y:=-yradius;
  pt[3].x:= xradius; pt[3].y:= yradius;
  pt[4].x:=0;        pt[4].y:= yradius;
  pt[5].x:=-xradius; pt[5].y:= yradius;
  pt[6].x:=-xradius; pt[6].y:=-yradius;
  pt[7].x:=0;        pt[7].y:=-yradius;
  for i:=1 to 7 do begin
                     x1:=pt[i].x; y1:=pt[i].y; //perform rotation
                     pt[i].x:=x+Round( x1*cosrot+y1*sinrot);
                     pt[i].y:=y+Round(-x1*sinrot+y1*cosrot);
                   end;
  DrawBezier(7,pt);
end;

{filled drawings routines}

procedure Bar(x1,y1,x2,y2:smallint);
var rc: TRect;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  if (x1 > x2) or (y1 > y2) then begin
                                   grResult:=grInvalidParam;
                                   Exit;
                                 end;
  Inc(x1,origX); Inc(y1,origY); Inc(x2,origX); Inc(y2,origY);
  SetRect(rc,x1,y1,x2+1,y2+1);
  EnterCriticalSection(protect_devices);
  if grDirect then windows.FillRect(grWindow,rc,grBrush);
  windows.FillRect(grMemory,rc,grBrush);
  LeaveCriticalSection(protect_devices);
end;

procedure Bar3D(x1,y1,x2,y2:smallint; depth:word; top:boolean);
var pt: array[1..4] of TPoint;
begin
  FillRect(x1,y1,x2,y2); if (grResult <> grOK) then Exit;
  Inc(x1,origX); Inc(y1,origY); Inc(x2,origX); Inc(y2,origY);
  EnterCriticalSection(protect_devices);
  if top then begin
                pt[1].x:=x1;       pt[1].y:=y1;
                pt[2].x:=x1+depth; pt[2].y:=y1-depth;
                pt[3].x:=x2+depth; pt[3].y:=y1-depth;
                pt[4].x:=x2;       pt[4].y:=y1;
                if grDirect then Polyline(grWindow,pt,4);
                Polyline(grMemory,pt,4);
              end;
  if (depth <> 0) then begin
                         pt[1].x:=x2+depth; pt[1].y:=y1-depth;
                         pt[2].x:=x2+depth; pt[2].y:=y2-depth;
                         pt[3].x:=x2;       pt[3].y:=y2;
                         if grDirect then Polyline(grWindow,pt,3);
                         Polyline(grMemory,pt,3);
                       end;
  LeaveCriticalSection(protect_devices);
end;

procedure Chord(x,y:smallint; start,stop,xradius,yradius:word);
var nXRadial1,nYRadial1,nXRadial2,nYRadial2: longint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  Inc(x,origX); Inc(y,origY);
  nXRadial1:=Round(xradius*Cos(start*Rad)); nXRadial2:=Round(xradius*Cos(stop*Rad));
  nYRadial1:=Round(yradius*Sin(start*Rad)); nYRadial2:=Round(yradius*Sin(stop*Rad));
  if not(defAspectRatio) then
  begin
    xradius:=10000*xradius div aspX; yradius:=10000*yradius div aspY;
  end;
  EnterCriticalSection(protect_devices);
  if grDirect then
    windows.Chord(grWindow,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXRadial1,
                           y-nYRadial1,x+nXRadial2,y-nYRadial2);
  windows.Chord(grMemory,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXRadial1,
                         y-nYRadial1,x+nXRadial2,y-nYRadial2);
  LeaveCriticalSection(protect_devices);
end;

procedure FillEllipse(x,y:smallint;xradius,yradius:word);
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  Inc(x,origX); Inc(y,origY);
  if not(defAspectRatio) then
  begin
    xradius:=10000*xradius div aspX; yradius:=10000*yradius div aspY;
  end;
  EnterCriticalSection(protect_devices);
  if grDirect then
    windows.Ellipse(grWindow,x-xradius,y-yradius,x+xradius+1,y+yradius+1);
  windows.Ellipse(grMemory,x-xradius,y-yradius,x+xradius+1,y+yradius+1);
  LeaveCriticalSection(protect_devices);
end;

procedure PieSlice(x,y:smallint; start,stop,radius:word);
begin
  Sector(x,y,start,stop,radius,radius);
end;

procedure Sector(x,y:smallint; start,stop,xradius,yradius:word);
var nXRadial1,nYRadial1,nXRadial2,nYRadial2: longint;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  Inc(x,origX); Inc(y,origY);
  nXRadial1:=Round(xradius*Cos(start*Rad)); nXRadial2:=Round(xradius*Cos(stop*Rad));
  nYRadial1:=Round(yradius*Sin(start*Rad)); nYRadial2:=Round(yradius*Sin(stop*Rad));
  if not(defAspectRatio) then
  begin
    xradius:=10000*xradius div aspX; yradius:=10000*yradius div aspY;
  end;
  EnterCriticalSection(protect_devices);
  if grDirect then
    Pie(grWindow,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXRadial1,
                 y-nYRadial1,x+nXRadial2,y-nYRadial2);
  Pie(grMemory,x-xradius,y-yradius,x+xradius+1,y+yradius+1,x+nXRadial1,
               y-nYRadial1,x+nXRadial2,y-nYRadial2);
  LeaveCriticalSection(protect_devices);
end;

procedure SetFillPattern(fillpattern:FillPatternType; color:longword);
var i,j      : longint;
    col0,col1: COLORREF;
    b        : byte;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  wingraph.fillPattern:=fillpattern;
  col1:=MapColor(color); col0:=MapColor(bkColor);
  EnterCriticalSection(protect_devices);
  if (grPattern <> 0) then DeleteObject(grPattern);
  grPattern:=CreateCompatibleBitmap(grMemory,8,8);
  SelectObject(grTemp,grPattern);
  for i:=0 to 7 do
  begin
    b:=fillpattern[i+1];
    for j:=7 downto 0 do
    begin
      if (b and $01 <> 0) then SetPixelV(grTemp,j,i,col1)
                          else SetPixelV(grTemp,j,i,col0);
      b:=b shr 1;
    end;
  end;
  SelectObject(grTemp,old_Bitmap);
  LeaveCriticalSection(protect_devices);
  SetFillStyle(UserFill,color);
end;

procedure SetFillStyle(pattern:word; color:longword);
var lplb: LOGBRUSH;
    old : HBRUSH;
begin
  grResult:=grOK;
  if not(grEnabled) then begin
                           grResult:=grNoInitGraph;
                           Exit;
                         end;
  with lplb do
  begin
    lbStyle:=BS_HATCHED;
    lbHatch:=0;
    case pattern of
      SolidFill  : lbStyle:=BS_SOLID;
      EmptyFill  : begin
                     lbStyle:=BS_SOLID;
                     color:=bkColor;
                   end;
      LineFill   : lbHatch:=HS_HORIZONTAL;
      ColFill    : lbHatch:=HS_VERTICAL;
      HatchFill  : lbHatch:=HS_CROSS;
      SlashFill  : lbHatch:=HS_BDIAGONAL;
      BkSlashFill: lbHatch:=HS_FDIAGONAL;
      XHatchFill : lbHatch:=HS_DIAGCROSS;
      UserFill   : begin
                     lbStyle:=BS_PATTERN;
                     lbHatch:=longint(grPattern);
                   end;
      NoFill     : lbStyle:=BS_NULL;
    else
      grResult:=grInvalidParam;
      Exit;
    end;
    lbColor:=MapColor(color);
  end;
  fillSettings.pattern:=pattern;
  fillSettings.color:=color;
  EnterCriticalSection(protect_devices);
  grBrush:=CreateBrushIndirect(lplb);
  old:=SelectObject(grWindow,grBrush); SelectObject(grMemory,grBrush);
  if (old <> old_Brush) then DeleteObject(old);
  LeaveCriticalSection(protect_devices);
end;

       { Bar3D constants }
       TopOn = true;
       TopOff = false;

       { fill pattern for Get/SetFillStyle: }
       EmptyFill      = 0;
       SolidFill      = 1;
       LineFill       = 2;
       LtSlashFill    = 3;
       SlashFill      = 4;
       BkSlashFill    = 5;
       LtBkSlashFill  = 6;
       HatchFill      = 7;
       XHatchFill     = 8;
       InterleaveFill = 9;
       WideDotFill    = 10;
       CloseDotFill   = 11;
       UserFill       = 12;

       { bitblt operators  }
       NormalPut     = 0;
       CopyPut       = 0;
       XORPut        = 1;
       OrPut         = 2;
       AndPut        = 3;
       NotPut        = 4;

procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);

procedure SetWriteMode(WriteMode : smallint);
procedure GetFillSettings(var Fillinfo:Fillsettingstype);
procedure GetFillPattern(var FillPattern:FillPatternType);
procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);

procedure SetFillStyle(Pattern : word; Color: word);
procedure SetFillPattern(Pattern: FillPatternType; Color: word);

 procedure Rectangle(x1,y1,x2,y2:smallint);
 procedure Bar(x1,y1,x2,y2:smallint);
 procedure Bar3D(x1, y1, x2, y2 : smallint;depth : word;top : boolean);
 procedure FillPoly(NumPoints: word; Var PolyPoints);
 procedure DrawPoly(NumPoints : word;var polypoints);
 procedure LineRel(Dx, Dy: smallint);
 procedure LineTo(X,Y : smallint);
 procedure FloodFill(x : smallint; y : smallint; Border: word);


 procedure GetArcCoords(var ArcCoords: ArcCoordsType);
 procedure Arc(X,Y : smallint; StAngle,EndAngle,Radius: word);
 procedure PieSlice(X,Y,stangle,endAngle:smallint;Radius: Word);
 procedure FillEllipse(X, Y: smallint; XRadius, YRadius: Word);
 procedure Sector(x, y: smallint; StAngle,EndAngle, XRadius, YRadius: Word);
 procedure Ellipse(X,Y : smallint; stAngle, EndAngle: word; XRadius,
   YRadius: word);

const
   StdBufferSize = 4096;   { Buffer size for FloodFill }

  LineInfo : LineSettingsType;
  FillSettings: FillSettingsType;

  ArcCall: ArcCoordsType;   { Information on the last call to Arc or Ellipse }


  procedure HLineDefault(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    xtmp: smallint;
   Begin

    { must we swap the values? }
    if x >= x2 then
      Begin
        xtmp := x2;
        x2 := x;
        x:= xtmp;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    X2  := X2 + StartXViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for x:= x to x2 do
      DirectPutPixel(X,Y);
   end;


  procedure VLineDefault(x,y,y2: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    ytmp: smallint;
  Begin
    { must we swap the values? }
    if y >= y2 then
     Begin
       ytmp := y2;
       y2 := y;
       y:= ytmp;
     end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    Y2  := Y2 + StartYViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for y := y to y2 do Directputpixel(x,y)
  End;

  Procedure DirectPutPixelClip(x,y: smallint);
  { for thickwidth lines, because they may call DirectPutPixel for coords }
  { outside the current viewport (bug found by CEC)                       }
  Begin
    If (Not ClipPixels) Or
       ((X >= StartXViewPort) And (X <= (StartXViewPort + ViewWidth)) And
        (Y >= StartYViewPort) And (Y <= (StartYViewPort + ViewHeight))) then
      Begin
        DirectPutPixel(x,y)
      End
  End;

  procedure LineDefault(X1, Y1, X2, Y2: smallint); {$ifndef fpc}far;{$endif fpc}

  var X, Y :           smallint;
      deltax, deltay : smallint;
      d, dinc1, dinc2: smallint;
      xinc1          : smallint;
      xinc2          : smallint;
      yinc1          : smallint;
      yinc2          : smallint;
      i              : smallint;
      Flag           : Boolean; { determines pixel direction in thick lines }
      NumPixels      : smallint;
      PixelCount     : smallint;
      OldCurrentColor: Word;
      swtmp          : smallint;
      TmpNumPixels   : smallint;
 begin
{******************************************}
{  SOLID LINES                             }
{******************************************}
  if lineinfo.LineStyle = SolidLn then
    Begin
       { we separate normal and thick width for speed }
       { and because it would not be 100% compatible  }
       { with the TP graph unit otherwise             }
       if y1 = y2 then
        Begin
     {******************************************}
     {  SOLID LINES HORIZONTAL                  }
     {******************************************}
          if lineinfo.Thickness=NormWidth then
            hline(x1,x2,y2)
          else
            begin
               { thick width }
               hline(x1,x2,y2-1);
               hline(x1,x2,y2);
               hline(x2,x2,y2+1);
            end;
        end
    else
    if x1 = x2 then
        Begin
     {******************************************}
     {  SOLID LINES VERTICAL                    }
     {******************************************}
          if lineinfo.Thickness=NormWidth then
            vline(x1,y1,y2)
          else
            begin
            { thick width }
              vline(x1-1,y1,y2);
              vline(x1,y1,y2);
              vline(x1+1,y1,y2);
            end;
        end
    else
    begin
     { Convert to global coordinates. }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y1 := y1 + StartYViewPort;
     y2 := y2 + StartYViewPort;
     { if fully clipped then exit... }
     if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;
     {******************************************}
     {  SLOPED SOLID LINES                      }
     {******************************************}
           oldCurrentColor :=
           CurrentColor;
           { Calculate deltax and deltay for initialisation }
           deltax := abs(x2 - x1);
           deltay := abs(y2 - y1);

          { Initialize all vars based on which is the independent variable }
          if deltax >= deltay then
            begin

             Flag := FALSE;
             { x is independent variable }
             numpixels := deltax + 1;
             d := (2 * deltay) - deltax;
             dinc1 := deltay Shl 1;
             dinc2 := (deltay - deltax) shl 1;
             xinc1 := 1;
             xinc2 := 1;
             yinc1 := 0;
             yinc2 := 1;
            end
          else
            begin

             Flag := TRUE;
             { y is independent variable }
             numpixels := deltay + 1;
             d := (2 * deltax) - deltay;
             dinc1 := deltax Shl 1;
             dinc2 := (deltax - deltay) shl 1;
             xinc1 := 0;
             xinc2 := 1;
             yinc1 := 1;
             yinc2 := 1;
            end;

         { Make sure x and y move in the right directions }
         if x1 > x2 then
           begin
            xinc1 := - xinc1;
            xinc2 := - xinc2;
           end;
         if y1 > y2 then
          begin
           yinc1 := - yinc1;
           yinc2 := - yinc2;
          end;

         { Start drawing at <x1, y1> }
         x := x1;
         y := y1;


         If LineInfo.Thickness=NormWidth then

          Begin

            { Draw the pixels }
            for i := 1 to numpixels do
              begin
                DirectPutPixel(x, y);
                if d < 0 then
                  begin
                   d := d + dinc1;
                   x := x + xinc1;
                   y := y + yinc1;
                  end
                else
                  begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                  end;
                  CurrentColor := OldCurrentColor;
             end;
          end
        else
         { Thick width lines }
          begin
            { Draw the pixels }
             for i := 1 to numpixels do
               begin
                { all depending on the slope, we can determine         }
                { in what direction the extra width pixels will be put }
                If Flag then
                  Begin
                    DirectPutPixelClip(x-1,y);
                    DirectPutPixelClip(x,y);
                    DirectPutPixelClip(x+1,y);
                  end
                else
                  Begin
                    DirectPutPixelClip(x, y-1);
                    DirectPutPixelClip(x, y);
                    DirectPutPixelClip(x, y+1);
                  end;
                if d < 0 then
                  begin
                    d := d + dinc1;
                    x := x + xinc1;
                    y := y + yinc1;
                  end
                else
                  begin
                    d := d + dinc2;
                    x := x + xinc2;
                    y := y + yinc2;
                  end;
                CurrentColor := OldCurrentColor;
               end;
          end;
        end;
  end
   else
{******************************************}
{  begin patterned lines                   }
{******************************************}
    Begin
      { Convert to global coordinates. }
      x1 := x1 + StartXViewPort;
      x2 := x2 + StartXViewPort;
      y1 := y1 + StartYViewPort;
      y2 := y2 + StartYViewPort;
      { if fully clipped then exit... }
      if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;

      OldCurrentColor := CurrentColor;
      PixelCount:=0;
      if y1 = y2 then
            Begin
             { Check if we must swap }
         if x1 >= x2 then
               Begin
                 swtmp := x1;
                 x1 := x2;
                 x2 := swtmp;
               end;
         if LineInfo.Thickness = NormWidth then
              Begin
               for PixelCount:=x1 to x2 do
                     { optimization: PixelCount mod 16 }
                     if LinePatterns[PixelCount and 15] = TRUE then
                      begin
                        DirectPutPixel(PixelCount,y2);
                      end;
              end
             else
              Begin
               for i:=-1 to 1 do
                     Begin
                       for PixelCount:=x1 to x2 do
                         { Optimization from Thomas - mod 16 = and 15 }
                         {this optimization has been performed by the compiler
                          for while as well (JM)}
                         if LinePatterns[PixelCount and 15] = TRUE then
                           begin
                                 DirectPutPixelClip(PixelCount,y2+i);
                           end;
                     end;
              end;
        end
      else
      if x1 = x2 then
           Begin
            { Check if we must swap }
            if y1 >= y2 then
              Begin
                swtmp := y1;
                y1 := y2;
                y2 := swtmp;
              end;
            if LineInfo.Thickness = NormWidth then
              Begin
                for PixelCount:=y1 to y2 do
                    { compare if we should plot a pixel here , compare }
                    { with predefined line patterns...                 }
                    if LinePatterns[PixelCount and 15] = TRUE then
                      begin
                        DirectPutPixel(x1,PixelCount);
                      end;
              end
            else
              Begin
                for i:=-1 to 1 do
                     Begin
                       for PixelCount:=y1 to y2 do
                       { compare if we should plot a pixel here , compare }
                       { with predefined line patterns...                 }
                         if LinePatterns[PixelCount and 15] = TRUE then
                           begin
                             DirectPutPixelClip(x1+i,PixelCount);
                           end;
                     end;
              end;
           end
      else
           Begin
             oldCurrentColor := CurrentColor;
             { Calculate deltax and deltay for initialisation }
             deltax := abs(x2 - x1);
             deltay := abs(y2 - y1);

             { Initialize all vars based on which is the independent variable }
             if deltax >= deltay then
               begin

                 Flag := FALSE;
                 { x is independent variable }
                 numpixels := deltax + 1;
                 d := (2 * deltay) - deltax;
                 dinc1 := deltay Shl 1;
                 dinc2 := (deltay - deltax) shl 1;
                 xinc1 := 1;
                 xinc2 := 1;
                 yinc1 := 0;
                 yinc2 := 1;
              end
            else
              begin

                Flag := TRUE;
                { y is independent variable }
                numpixels := deltay + 1;
                d := (2 * deltax) - deltay;
                dinc1 := deltax Shl 1;
                dinc2 := (deltax - deltay) shl 1;
                xinc1 := 0;
                xinc2 := 1;
                yinc1 := 1;
                yinc2 := 1;
              end;

            { Make sure x and y move in the right directions }
            if x1 > x2 then
              begin
                xinc1 := - xinc1;
                xinc2 := - xinc2;
              end;
            if y1 > y2 then
              begin
                yinc1 := - yinc1;
                yinc2 := - yinc2;
              end;

            { Start drawing at <x1, y1> }
            x := x1;
            y := y1;

            If LineInfo.Thickness=ThickWidth then

             Begin
               TmpNumPixels := NumPixels-1;
               { Draw the pixels }
               for i := 0 to TmpNumPixels do
                 begin
                     { all depending on the slope, we can determine         }
                     { in what direction the extra width pixels will be put }
                       If Flag then
                          Begin
                            { compare if we should plot a pixel here , compare }
                            { with predefined line patterns...                 }
                            if LinePatterns[i and 15] = TRUE then
                              begin
                                DirectPutPixelClip(x-1,y);
                                DirectPutPixelClip(x,y);
                                DirectPutPixelClip(x+1,y);
                              end;
                          end
                       else
                          Begin
                            { compare if we should plot a pixel here , compare }
                            { with predefined line patterns...                 }
                            if LinePatterns[i and 15] = TRUE then
                             begin
                               DirectPutPixelClip(x,y-1);
                               DirectPutPixelClip(x,y);
                               DirectPutPixelClip(x,y+1);
                             end;
                          end;
                   if d < 0 then
                         begin
                           d := d + dinc1;
                           x := x + xinc1;
                           y := y + yinc1;
                         end
                   else
                         begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                         end;
                end;
            end
           else
            Begin
             { instead of putting in loop , substract by one now }
             TmpNumPixels := NumPixels-1;
            { NormWidth }
             for i := 0 to TmpNumPixels do
             begin
                  if LinePatterns[i and 15] = TRUE then
                    begin
                      DirectPutPixel(x,y);
                    end;
             if d < 0 then
                 begin
                   d := d + dinc1;
                   x := x + xinc1;
                   y := y + yinc1;
                 end
             else
                 begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
                 end;
             end;
            end
        end;
{******************************************}
{  end patterned lines                     }
{******************************************}
       { restore color }
       CurrentColor:=OldCurrentColor;
   end;
 end;  { Line }


  {********************************************************}
  { Procedure DummyPatternLine()                           }
  {--------------------------------------------------------}
  { This is suimply an procedure that does nothing which   }
  { can be passed as a patternlineproc for non-filled      }
  { ellipses                                               }
  {********************************************************}
  Procedure DummyPatternLine(x1, x2, y: smallint); {$ifdef tp} far; {$endif tp}
  begin
  end;


  {********************************************************}
  { Procedure InternalEllipse()                            }
  {--------------------------------------------------------}
  { This routine first calculates all points required to   }
  { draw a circle to the screen, and stores the points     }
  { to display in a buffer before plotting them. The       }
  { aspect ratio of the screen is taken into account when  }
  { calculating the values.                                }
  {--------------------------------------------------------}
  { INPUTS: X,Y : Center coordinates of Ellipse.           }
  {  XRadius - X-Axis radius of ellipse.                   }
  {  YRadius - Y-Axis radius of ellipse.                   }
  {  stAngle, EndAngle: Start angle and end angles of the  }
  {  ellipse (used for partial ellipses and circles)       }
  {  pl: procedure which either draws a patternline (for   }
  {      FillEllipse) or does nothing (arc etc)            }
  {--------------------------------------------------------}
  { NOTE: -                                                }
  {       -                                                }
  {********************************************************}

  Procedure InternalEllipseDefault(X,Y: smallint;XRadius: word;
    YRadius:word; stAngle,EndAngle: word; pl: PatternLineProc); {$ifndef fpc}far;{$endif fpc}
   Const ConvFac = Pi/180.0;

   var
    j, Delta, DeltaEnd: graph_float;
    NumOfPixels: longint;
    TempTerm: graph_float;
    xtemp, ytemp, xp, yp, xm, ym, xnext, ynext,
      plxpyp, plxmyp, plxpym, plxmym: smallint;
    BackupColor, TmpAngle, OldLineWidth: word;
  Begin
   If LineInfo.ThickNess = ThickWidth Then
    { first draw the two outer ellipses using normwidth and no filling (JM) }
     Begin
       OldLineWidth := LineInfo.Thickness;
       LineInfo.Thickness := NormWidth;
       InternalEllipseDefault(x,y,XRadius,YRadius,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       InternalEllipseDefault(x,y,XRadius+1,YRadius+1,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       If (XRadius > 0) and (YRadius > 0) Then
         { draw the smallest ellipse last, since that one will use the }
         { original pl, so it could possibly draw patternlines (JM)    }
         Begin
           Dec(XRadius);
           Dec(YRadius);
         End
       Else Exit;
       { restore line thickness }
       LineInfo.Thickness := OldLineWidth;
     End;
   If xradius = 0 then inc(xradius);
   if yradius = 0 then inc(yradius);
   { check for an ellipse with negligable x and y radius }
   If (xradius <= 1) and (yradius <= 1) then
     begin
       putpixel(x,y,CurrentColor);
       ArcCall.X := X;
       ArcCall.Y := Y;
       ArcCall.XStart := X;
       ArcCall.YStart := Y;
       ArcCall.XEnd := X;
       ArcCall.YEnd := Y;
       exit;
     end;
   { check if valid angles }
   stangle := stAngle mod 361;
   EndAngle := EndAngle mod 361;
   { if impossible angles then swap them! }
   if Endangle < StAngle then
     Begin
       TmpAngle:=EndAngle;
       EndAngle:=StAngle;
       Stangle:=TmpAngle;
     end;
   { approximate the number of pixels required by using the circumference }
   { equation of an ellipse.                                              }
   { Changed this formula a it (trial and error), but the net result is that }
   { less pixels have to be calculated now                                   }
   NumOfPixels:=Round(Sqrt(3)*sqrt(sqr(XRadius)+sqr(YRadius)));
   { Calculate the angle precision required }
   Delta := 90.0 / NumOfPixels;
   { for restoring after PatternLine }
   BackupColor := CurrentColor;
   { removed from inner loop to make faster }
   { store some arccall info }
   ArcCall.X := X;
   ArcCall.Y := Y;
   TempTerm := (StAngle)*ConvFac;
   ArcCall.XStart := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YStart := round(YRadius*Sin(TempTerm+Pi)) + Y;
   TempTerm := (EndAngle)*ConvFac;
   ArcCall.XEnd := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YEnd := round(YRadius*Sin(TempTerm+Pi)) + Y;
   { Always just go over the first 90 degrees. Could be optimized a   }
   { bit if StAngle and EndAngle lie in the same quadrant, left as an }
   { exercise for the reader :) (JM)                                  }
   j := 0;
   { calculate stop position, go 1 further than 90 because otherwise }
   { 1 pixel is sometimes not drawn (JM)                             }
   DeltaEnd := 91;
   { Calculate points }
   xnext := XRadius;
   ynext := 0;
   Repeat
     xtemp := xnext;
     ytemp := ynext;
     { this is used by both sin and cos }
     TempTerm := (j+Delta)*ConvFac;
     { Calculate points }
     xnext := round(XRadius*Cos(TempTerm));
     ynext := round(YRadius*Sin(TempTerm+Pi));

     xp := x + xtemp;
     xm := x - xtemp;
     yp := y + ytemp;
     ym := y - ytemp;
     plxpyp := maxsmallint;
     plxmyp := -maxsmallint-1;
     plxpym := maxsmallint;
     plxmym := -maxsmallint-1;
     If (j >= StAngle) and (j <= EndAngle) then
       begin
         plxpyp := xp;
         PutPixel(xp,yp,CurrentColor);
       end;
     If ((180-j) >= StAngle) and ((180-j) <= EndAngle) then
       begin
         plxmyp := xm;
         PutPixel(xm,yp,CurrentColor);
       end;
     If ((j+180) >= StAngle) and ((j+180) <= EndAngle) then
       begin
         plxmym := xm;
         PutPixel(xm,ym,CurrentColor);
       end;
     If ((360-j) >= StAngle) and ((360-j) <= EndAngle) then
       begin
         plxpym := xp;
         PutPixel(xp,ym,CurrentColor);
       end;
     If (ynext <> ytemp) and
        (xp - xm >= 1) then
       begin
         CurrentColor := FillSettings.Color;
         pl(plxmyp+1,plxpyp-1,yp);
         pl(plxmym+1,plxpym-1,ym);
         CurrentColor := BackupColor;
       end;
     j:=j+Delta;
   Until j > (DeltaEnd);
  end;


  procedure PatternLineDefault(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
  {********************************************************}
  { Draws a horizontal patterned line according to the     }
  { current Fill Settings.                                 }
  {********************************************************}
  { Important notes:                                       }
  {  - CurrentColor must be set correctly before entering  }
  {    this routine.                                       }
  {********************************************************}
   var
    NrIterations: smallint;
    i           : smallint;
    j           : smallint;
    TmpFillPattern : byte;
    OldWriteMode : word;
    OldCurrentColor : word;
   begin
     { convert to global coordinates ... }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y  := y + StartYViewPort;
     { if line was fully clipped then exit...}
     if LineClipped(x1,y,x2,y,StartXViewPort,StartYViewPort,
        StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
         exit;

     OldWriteMode := CurrentWriteMode;
     CurrentWriteMode := NormalPut;


     { Get the current pattern }
     TmpFillPattern := FillPatternTable
       [FillSettings.Pattern][(y and $7)+1];

     Case TmpFillPattern Of
       0:
         begin
           OldCurrentColor := CurrentColor;
           CurrentColor := CurrentBkColor;
  { hline converts the coordinates to global ones, but that has been done }
  { already here!!! Convert them back to local ones... (JM)                }
           HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
           CurrentColor := OldCurrentColor;
         end;
      $ff:
        begin
          OldCurrentColor := CurrentColor;
          CurrentColor := FillSettings.Color;
          HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
          CurrentColor := OldCurrentColor;
        end;
       else
         begin
           { number of times to go throuh the 8x8 pattern }
           NrIterations := abs(x2 - x1+8) div 8;
           For i:= 0 to NrIterations do
             Begin
               for j:=0 to 7 do
                    Begin
                    { x1 mod 8 }
                    if RevBitArray[x1 and 7] and TmpFillPattern <> 0 then
                      begin
                        OldCurrentColor := CurrentColor;
                        CurrentColor := FillSettings.Color;
                        DirectPutpixel(x1,y);
                        CurrentColor := OldCurrentColor;
                      end
                    else
                      begin
                        { According to the TP graph manual, we overwrite everything }
                        { which is filled up - checked against VGA and CGA drivers  }
                        { of TP.                                                    }
                        OldCurrentColor := CurrentColor;
                        CurrentColor := CurrentBkColor;
                        DirectPutPixel(x1,y);
                        CurrentColor := OldCurrentColor;
                      end;
                    Inc(x1);
                    if x1 > x2 then
                     begin
                       CurrentWriteMode := OldWriteMode;
                       exit;
                     end;
                   end;
             end;
          end;
     End;
     CurrentWriteMode := OldWriteMode;
   end;


  procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);

   begin
    Activelineinfo:=Lineinfo;
   end;


  procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);

   var
    i: byte;
    j: byte;

   Begin
    if (LineStyle > UserBitLn) or ((Thickness <> Normwidth) and (Thickness <> ThickWidth)) then
      _GraphResult := grError
    else
      begin
       LineInfo.Thickness := Thickness;
       LineInfo.LineStyle := LineStyle;
       case LineStyle of
            UserBitLn: Lineinfo.Pattern := pattern;
            SolidLn:   Lineinfo.Pattern  := $ffff;  { ------- }
            DashedLn : Lineinfo.Pattern := $F8F8;   { -- -- --}
            DottedLn:  LineInfo.Pattern := $CCCC;   { - - - - }
            CenterLn: LineInfo.Pattern :=  $FC78;   { -- - -- }
       end; { end case }
       { setup pattern styles }
       j:=16;
       for i:=0 to 15 do
        Begin
         dec(j);
         { bitwise mask for each bit in the word }
         if (word($01 shl i) AND LineInfo.Pattern) <> 0 then
               LinePatterns[j]:=TRUE
             else
               LinePatterns[j]:=FALSE;
        end;
      end;
   end;


  Procedure GetArcCoords(var ArcCoords: ArcCoordsType);
   Begin
     ArcCoords.X := ArcCall.X;
     ArcCoords.Y := ArcCall.Y;
     ArcCoords.XStart := ArcCall.XStart;
     ArcCoords.YStart := ArcCall.YStart;
     ArcCoords.XEnd := ArcCall.XEnd;
     ArcCoords.YEnd := ArcCall.YEnd;
   end;


  Procedure Arc(X,Y : smallint; StAngle,EndAngle,Radius: word);

{   var
    OldWriteMode: word;}

   Begin
     { Only if we are using thickwidths lines do we accept }
     { XORput write modes.                                 }
{     OldWriteMode := CurrentWriteMode;
     if (LineInfo.Thickness = NormWidth) then
       CurrentWriteMode := NormalPut;}
     InternalEllipse(X,Y,Radius,Radius,StAngle,Endangle,{$ifdef fpc}@{$endif}DummyPatternLine);
{     CurrentWriteMode := OldWriteMode;}
   end;


 procedure Ellipse(X,Y : smallint; stAngle, EndAngle: word; XRadius,YRadius: word);
  Begin
    InternalEllipse(X,Y,XRadius,YRadius,StAngle,Endangle,{$ifdef fpc}@{$endif}DummyPatternLine);
  end;


 procedure FillEllipse(X, Y: smallint; XRadius, YRadius: Word);
  {********************************************************}
  { Procedure FillEllipse()                                }
  {--------------------------------------------------------}
  { Draws a filled ellipse using (X,Y) as a center point   }
  { and XRadius and YRadius as the horizontal and vertical }
  { axes. The ellipse is filled with the current fill color}
  { and fill style, and is bordered with the current color.}
  {********************************************************}
  begin
    InternalEllipse(X,Y,XRadius,YRadius,0,360,PatternLine)
  end;



 procedure CircleDefault(X, Y: smallint; Radius:Word);
  {********************************************************}
  { Draws a circle centered at X,Y with the given Radius.  }
  {********************************************************}
  { Important notes:                                       }
  {  - Thickwidth circles use the current write mode, while}
  {    normal width circles ALWAYS use CopyPut/NormalPut   }
  {    mode. (Tested against VGA BGI driver -CEC 13/Aug/99 }
  {********************************************************}
  var OriginalArcInfo: ArcCoordsType;
      OldWriteMode: word;

  begin
     if (Radius = 0) then
          Exit;

     if (Radius = 1) then
     begin
      { only normal put mode is supported by a call to PutPixel }
          PutPixel(X, Y, CurrentColor);
          Exit;
     end;

     { save state of arc information }
     { because it is not needed for  }
     { a circle call.                }
     System.move(ArcCall,OriginalArcInfo, sizeof(ArcCall));
     if LineInfo.Thickness = Normwidth then
       begin
             OldWriteMode := CurrentWriteMode;
             CurrentWriteMode := CopyPut;
       end;
     { Adjust for screen aspect ratio }
     InternalEllipse(X,Y,Radius,(longint(Radius)*XAspect) div YAspect,0,360,{$ifdef fpc}@{$endif}DummyPatternLine);
     if LineInfo.Thickness = Normwidth then
         CurrentWriteMode := OldWriteMode;
     { restore arc information }
     System.move(OriginalArcInfo, ArcCall,sizeof(ArcCall));
 end;

 procedure SectorPL(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
 var plx1, plx2: smallint;
 begin
   If (x1 = -maxsmallint) Then
     If (x2 = maxsmallint-1) Then
       { no ellipse points drawn on this line }
       If (((Y < ArcCall.Y) and (Y > ArcCall.YStart)) or
          ((Y > ArcCall.Y) and (Y < ArcCall.YStart))) Then
         { there is a part of the sector at this y coordinate, but no    }
         { ellips points are plotted on this line, so draw a patternline }
         { between the lines connecting (arccall.x,arccall.y) with       }
         { the start and the end of the arc (JM)                         }
         { use: y-y1=(y2-y1)/(x2-x1)*(x-x1) =>                           }
         { x = (y-y1)/(y2-y1)*(x2-x1)+x1                                 }
         Begin
           plx1 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                   div (ArcCall.YStart-ArcCall.Y)+ArcCall.X;
           plx2 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                   div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X;
           If plx1 > plx2 then
             begin
               plx1 := plx1 xor plx2;
               plx2 := plx1 xor plx2;
               plx1 := plx1 xor plx2;
             end;
         End
       { otherwise two points which have nothing to do with the sector }
       Else exit
     Else
       { the arc is plotted at the right side, but not at the left side, }
       { fill till the line between (ArcCall.X,ArcCall.Y) and            }
       { (ArcCall.XStart,ArcCall.YStart)                                 }
       Begin
         If (y < ArcCall.Y) then
           begin
             plx1 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                     div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X
           end
         else if (y > ArcCall.Y) then
           begin
             plx1 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                     div (ArcCall.YStart-ArcCall.Y)+ArcCall.X
             end
         else plx1 := ArcCall.X;
         plx2 := x2;
       End
   Else
     If (x2 = maxsmallint-1) Then
       { the arc is plotted at the left side, but not at the rigth side.   }
       { the right limit can be either the first or second line. Just take }
       { the closest one, but watch out for division by zero!              }
       Begin
         If (y < ArcCall.Y) then
           begin
             plx2 := (y-ArcCall.Y)*(ArcCall.XStart-ArcCall.X)
                     div (ArcCall.YStart-ArcCall.Y)+ArcCall.X
           end
         else if (y > ArcCall.Y) then
           begin
             plx2 := (y-ArcCall.Y)*(ArcCall.XEnd-ArcCall.X)
                     div (ArcCall.YEnd-ArcCall.Y)+ArcCall.X
           end
         else plx2 := ArcCall.X;
         plx1 := x1;
       End
     Else
       { the arc is plotted at both sides }
       Begin
         plx1 := x1;
         plx2 := x2;
       End;
   If plx2 > plx1 then
     Begin
       PatternLine(plx1,plx2,y);
     end;
 end;

 procedure Sector(x, y: smallint; StAngle,EndAngle, XRadius, YRadius: Word);
  begin
     internalellipse(x,y,XRadius, YRadius, StAngle, EndAngle, {$ifdef fpc}@{$endif}SectorPL);
     Line(ArcCall.XStart, ArcCall.YStart, x,y);
     Line(x,y,ArcCall.Xend,ArcCall.YEnd);
  end;



   procedure SetFillStyle(Pattern : word; Color: word);

   begin
     { on invalid input, the current fill setting will be }
     { unchanged.                                         }
     if (Pattern > UserFill) or (Color > GetMaxColor) then
      begin
{$ifdef logging}
           logln('invalid fillstyle parameters');
{$endif logging}
           _GraphResult := grError;
           exit;
      end;
     FillSettings.Color := Color;
     FillSettings.Pattern := Pattern;
   end;


  procedure SetFillPattern(Pattern: FillPatternType; Color: word);
  {********************************************************}
  { Changes the Current FillPattern to a user defined      }
  { pattern and changes also the current fill color.       }
  { The FillPattern is saved in the FillPattern array so   }
  { it can still be used with SetFillStyle(UserFill,Color) }
  {********************************************************}
   var
    i: smallint;

   begin
     if Color > GetMaxColor then
       begin
{$ifdef logging}
            logln('invalid fillpattern parameters');
{$endif logging}
            _GraphResult := grError;
            exit;
       end;

     FillSettings.Color := Color;
     FillSettings.Pattern := UserFill;

     { Save the pattern in the buffer }
     For i:=1 to 8 do
       FillPatternTable[UserFill][i] := Pattern[i];

   end;

  procedure Bar(x1,y1,x2,y2:smallint);
  {********************************************************}
  { Important notes for compatibility with BP:             }
  {     - WriteMode is always CopyPut                      }
  {     - No contour is drawn for the lines                }
  {********************************************************}
  var y               : smallint;
      origcolor       : longint;
      origlinesettings: Linesettingstype;
      origwritemode   : smallint;
   begin
     origlinesettings:=lineinfo;
     origcolor:=CurrentColor;
     if y1>y2 then
       begin
          y:=y1;
          y1:=y2;
          y2:=y;
       end;

     { Always copy mode for Bars }
     origwritemode := CurrentWriteMode;
     CurrentWriteMode := CopyPut;

     { All lines used are of this style }
     Lineinfo.linestyle:=solidln;
     Lineinfo.thickness:=normwidth;

     case Fillsettings.pattern of
     EmptyFill :
       begin
         Currentcolor:=CurrentBkColor;
         for y:=y1 to y2 do
           Hline(x1,x2,y);
       end;
     SolidFill :
       begin
         CurrentColor:=FillSettings.color;
           for y:=y1 to y2 do
              Hline(x1,x2,y);
       end;
     else
      Begin
        CurrentColor:=FillSettings.color;
        for y:=y1 to y2 do
          patternline(x1,x2,y);
      end;
    end;
    CurrentColor:= Origcolor;
    LineInfo := OrigLineSettings;
    CurrentWriteMode := OrigWritemode;
   end;




procedure bar3D(x1, y1, x2, y2 : smallint;depth : word;top : boolean);
var
 origwritemode : smallint;
 OldX, OldY : smallint;
begin
  origwritemode := CurrentWriteMode;
  CurrentWriteMode := CopyPut;
  Bar(x1,y1,x2,y2);
  Rectangle(x1,y1,x2,y2);

  { Current CP should not be updated in Bar3D }
  { therefore save it and then restore it on  }
  { exit.                                     }
  OldX := CurrentX;
  OldY := CurrentY;

  if top then begin
    Moveto(x1,y1);
    Lineto(x1+depth,y1-depth);
    Lineto(x2+depth,y1-depth);
    Lineto(x2,y1);
  end;
  if Depth <> 0 then
    Begin
      Moveto(x2+depth,y1-depth);
      Lineto(x2+depth,y2-depth);
      Lineto(x2,y2);
    end;
  { restore CP }
  CurrentX := OldX;
  CurrentY := OldY;
  CurrentWriteMode := origwritemode;
end;



  procedure GetFillSettings(var Fillinfo:Fillsettingstype);
   begin
     Fillinfo:=Fillsettings;
   end;

  procedure GetFillPattern(var FillPattern:FillPatternType);
   begin
     FillPattern:=FillpatternTable[UserFill];
   end;

  procedure DrawPoly(numpoints : word;var polypoints);
    type
      ppointtype = ^pointtype;
      pt = array[0..16000] of pointtype;
    var
      i : longint;
    begin
      if numpoints < 2 then
        begin
          _GraphResult := grError;
          exit;
        end;
      for i:=0 to numpoints-2 do
        line(pt(polypoints)[i].x,
             pt(polypoints)[i].y,
             pt(polypoints)[i+1].x,
             pt(polypoints)[i+1].y);
    end;


  procedure PieSlice(X,Y,stangle,endAngle:smallint;Radius: Word);
  begin
    Sector(x,y,stangle,endangle,radius,radius);
  end;


Function ClipCoords (Var X,Y : smallint) : Boolean;
{ Adapt to viewport, return TRUE if still in viewport,
  false if outside viewport}

begin
  X:= X + StartXViewPort;
  Y:= Y + StartYViewPort;
  ClipCoords:=Not ClipPixels;
  if ClipPixels then
    Begin
    ClipCoords:=(X < StartXViewPort) or (X > (StartXViewPort + ViewWidth));
    ClipCoords:=ClipCoords or
               ((Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)));
    ClipCoords:=Not ClipCoords;
    end;
end;


{   Drawing of ellipses and arcs, and filling ellipses and pies.}

  PEllipseInfoData = ^TEllipseInfoData;
  TEllipseInfoData = record
    x, ytopmax, ytopmin, ybotmax, ybotmin : integer;
    OnlyTop : boolean;
  end;

  TEllipseInfo = class
  private
    fcx, fcy, frx,fry,
    fa1, fa2, frot : real;
    fx1,fy1, fx2,fy2 : integer;
    InfoList : TList;
    procedure FreeList;
    procedure ClearList;
    function FindXIndex (x:integer) : integer;
    procedure PrepareCalculation (var np:integer; var delta:real);
    function NewInfoRec (anX:integer) : PEllipseInfoData;
    procedure CalculateCircular (const b:TRect; var x,y,rx,ry:real);
  public
    constructor create;
    destructor destroy; override;
    function GetInfoForX (x:integer; var ytopmax,ytopmin,ybotmax,ybotmin:integer):boolean;
    function GetInfoForX (x:integer; var Info:PEllipseInfoData):boolean;
    procedure GatherEllipseInfo (const bounds:TRect);
    procedure GatherArcInfo (const bounds:TRect; alpha1,alpha2:real);
    property cx : real read fcx; // center point
    property cy : real read fcy;
    property rhor : real read frx; // radius
    property rver : real read fry;
    { only usable when created with GatherArcInfo }
    property a1 : real read fa1;    // angle 1 and point on ellipse
    property x1 : integer read fx1;
    property y1 : integer read fy1;
    property a2 : real read fa2;    // angle 2 and point on ellipse
    property x2 : integer read fx2;
    property y2 : integer read fy2;
  end;


  TFPCanvasException = class (Exception);
  TFPPenException = class (TFPCanvasException);
  TFPBrushException = class (TFPCanvasException);
  TFPFontException = class (TFPCanvasException);

  TFPCustomCanvas = class;

  { TFPCanvasHelper }

  TFPCanvasHelper = class(TPersistent)
  private
    FDelayAllocate: boolean;
    FFPColor : TFPColor;
    FAllocated,
    FFixedCanvas : boolean;
    FCanvas : TFPCustomCanvas;
    FFlags : word;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure NotifyCanvas;
  protected
    // flags 0-15 are reserved for FPCustomCanvas
    function GetAllocated: boolean; virtual;
    procedure SetFlags (index:integer; AValue:boolean); virtual;
    function GetFlags (index:integer) : boolean; virtual;
    procedure CheckAllocated (ValueNeeded:boolean);
    procedure SetFixedCanvas (AValue : boolean);
    procedure DoAllocateResources; virtual;
    procedure DoDeAllocateResources; virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); virtual;
    procedure SetFPColor (const AValue:TFPColor); virtual;
    procedure Changing; dynamic;
    procedure Changed; dynamic;
    Procedure Lock;
    Procedure UnLock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // prepare helper for use
    procedure AllocateResources (ACanvas : TFPCustomCanvas;
                                 CanDelay: boolean = true);
    // free all resource used by this helper
    procedure DeallocateResources;
    property Allocated : boolean read GetAllocated;
    // properties cannot be changed when allocated
    property FixedCanvas : boolean read FFixedCanvas;
    // Canvas for which the helper is allocated
    property Canvas : TFPCustomCanvas read FCanvas;
    // color of the helper
    property FPColor : TFPColor read FFPColor Write SetFPColor;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DelayAllocate: boolean read FDelayAllocate write FDelayAllocate;
  end;


  TFPBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
                   bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern);
  TBrushPattern = array[0..PatternBitCount-1] of TPenPattern;
  PBrushPattern = ^TBrushPattern;

 TFPCustomPenClass = class of TFPCustomPen;
  

  TFPCustomBrush = class (TFPCanvasHelper)
  private
    FStyle : TFPBrushStyle;
    FImage : TFPCustomImage;
    FPattern : TBrushPattern;
  protected
    procedure SetStyle (AValue : TFPBrushStyle); virtual;
    procedure SetImage (AValue : TFPCustomImage); virtual;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
  public
    function CopyBrush : TFPCustomBrush;
    property Style : TFPBrushStyle read FStyle write SetStyle;
    property Image : TFPCustomImage read FImage write SetImage;
    property Pattern : TBrushPattern read FPattern write FPattern;
  end;
  TFPCustomBrushClass = class of TFPCustomBrush;

 { TFPCustomCanvas }

  TFPCustomCanvas = class(TPersistent)
  private
    FClipping,
    FManageResources: boolean;
    FRemovingHelpers : boolean;
    FDefaultFont,
    FFont : TFPCustomFont;
    FDefaultBrush,
    FBrush : TFPCustomBrush;
    FDefaultPen,
    FPen : TFPCustomPen;
    FPenPos : TPoint;
    FClipRect : TRect;
    FHelpers : TList;
    FLocks : integer;
    FInterpolation : TFPCustomInterpolation;
    function AllowFont (AFont : TFPCustomFont) : boolean;
    function AllowBrush (ABrush : TFPCustomBrush) : boolean;
    function AllowPen (APen : TFPCustomPen) : boolean;
    function CreateDefaultFont : TFPCustomFont;
    function CreateDefaultPen : TFPCustomPen;
    function CreateDefaultBrush : TFPCustomBrush;
    procedure RemoveHelpers;
    function GetFont : TFPCustomFont;
    function GetBrush : TFPCustomBrush;
    function GetPen : TFPCustomPen;
  protected
    function DoCreateDefaultFont : TFPCustomFont; virtual; abstract;
    function DoCreateDefaultPen : TFPCustomPen; virtual; abstract;
    function DoCreateDefaultBrush : TFPCustomBrush; virtual; abstract;
    procedure SetFont (AValue:TFPCustomFont); virtual;
    procedure SetBrush (AValue:TFPCustomBrush); virtual;
    procedure SetPen (AValue:TFPCustomPen); virtual;
    function  DoAllowFont (AFont : TFPCustomFont) : boolean; virtual;
    function  DoAllowPen (APen : TFPCustomPen) : boolean; virtual;
    function  DoAllowBrush (ABrush : TFPCustomBrush) : boolean; virtual;
    procedure SetColor (x,y:integer; const Value:TFPColor); Virtual; abstract;
    function  GetColor (x,y:integer) : TFPColor; Virtual; abstract;
    procedure SetHeight (AValue : integer); virtual; abstract;
    function  GetHeight : integer; virtual; abstract;
    procedure SetWidth (AValue : integer); virtual; abstract;
    function  GetWidth : integer; virtual; abstract;
    function  GetClipRect: TRect; virtual;
    procedure SetClipRect(const AValue: TRect); virtual;
    procedure SetPenPos(const AValue: TPoint); virtual;
    procedure DoLockCanvas; virtual;
    procedure DoUnlockCanvas; virtual;
    procedure DoTextOut (x,y:integer;text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer); virtual; abstract;
    function  DoGetTextHeight (text:string) : integer; virtual; abstract;
    function  DoGetTextWidth (text:string) : integer; virtual; abstract;
    procedure DoRectangle (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleFill (Const Bounds:TRect); virtual; abstract;
    procedure DoRectangleAndFill (Const Bounds:TRect); virtual;
    procedure DoEllipseFill (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipse (Const Bounds:TRect); virtual; abstract;
    procedure DoEllipseAndFill (Const Bounds:TRect); virtual;
    procedure DoPolygonFill (const points:array of TPoint); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
    procedure DoPolygonAndFill (const points:array of TPoint); virtual;
    procedure DoPolyline (const points:array of TPoint); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoMoveTo (x,y:integer); virtual;
    procedure DoLineTo (x,y:integer); virtual;
    procedure DoLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect); virtual; abstract;
    procedure DoDraw (x,y:integer; Const image:TFPCustomImage); virtual; abstract;
    procedure CheckHelper (AHelper:TFPCanvasHelper); virtual;
    procedure AddHelper (AHelper:TFPCanvasHelper);
  public
    constructor create;
    destructor destroy; override;
    procedure LockCanvas;
    procedure UnlockCanvas;
    function Locked: boolean;
    function CreateFont : TFPCustomFont;
    function CreatePen : TFPCustomPen;
    function CreateBrush : TFPCustomBrush;
    // using font
    procedure TextOut (x,y:integer;text:string);
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    // using pen and brush
    procedure Ellipse (Const Bounds:TRect);
    procedure Ellipse (left,top,right,bottom:integer);
    procedure EllipseC (x,y:integer; rx,ry:longword);
    procedure Polygon (Const points:array of TPoint);
    procedure Polyline (Const points:array of TPoint);
    procedure Rectangle (Const Bounds:TRect);
    procedure Rectangle (left,top,right,bottom:integer);
    // using brush
    procedure FloodFill (x,y:integer);
    procedure Clear;
    // using pen
    procedure MoveTo (x,y:integer);
    procedure MoveTo (p:TPoint);
    procedure LineTo (x,y:integer);
    procedure LineTo (p:TPoint);
    procedure Line (x1,y1,x2,y2:integer);
    procedure Line (const p1,p2:TPoint);
    procedure Line (const points:TRect);
    // other procedures
    procedure CopyRect (x,y:integer; canvas:TFPCustomCanvas; SourceRect:TRect);
    procedure Draw (x,y:integer; image:TFPCustomImage);
    procedure StretchDraw (x,y,w,h:integer; source:TFPCustomImage);
    procedure Erase;virtual;
    // properties
    property Font : TFPCustomFont read GetFont write SetFont;
    property Pen : TFPCustomPen read GetPen write SetPen;
    property Brush : TFPCustomBrush read GetBrush write SetBrush;
    property Interpolation : TFPCustomInterpolation read FInterpolation write FInterpolation;
    property Colors [x,y:integer] : TFPColor read GetColor write SetColor;
    property ClipRect : TRect read GetClipRect write SetClipRect;
    property Clipping : boolean read FClipping write FClipping;
    property PenPos : TPoint read FPenPos write SetPenPos;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
    property ManageResources: boolean read FManageResources write FManageResources;
  end;

  TFPCustomDrawFont = class (TFPCustomFont)
  private
    procedure DrawText (x,y:integer; text:string);
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
  protected
    procedure DoDrawText (x,y:integer; text:string); virtual; abstract;
    procedure DoGetTextSize (text:string; var w,h:integer); virtual; abstract;
    function DoGetTextHeight (text:string) : integer; virtual; abstract;
    function DoGetTextWidth (text:string) : integer; virtual; abstract;
  end;

  TFPEmptyFont = class (TFPCustomFont)
  end;

  TFPCustomDrawPen = class (TFPCustomPen)
  private
    procedure DrawLine (x1,y1,x2,y2:integer);
    procedure Polyline (const points:array of TPoint; close:boolean);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Rectangle (left,top, right,bottom:integer);
  protected
    procedure DoDrawLine (x1,y1,x2,y2:integer); virtual; abstract;
    procedure DoPolyline (const points:array of TPoint; close:boolean); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
  end;

  TFPEmptyPen = class (TFPCustomPen)
  end;

  TFPCustomDrawBrush = class (TFPCustomBrush)
  private
    procedure Rectangle (left,top, right,bottom:integer);
    procedure FloodFill (x,y:integer);
    procedure Ellipse (left,top, right,bottom:integer);
    procedure Polygon (const points:array of TPoint);
  public
    procedure DoRectangle (left,top, right,bottom:integer); virtual; abstract;
    procedure DoEllipse (left,top, right,bottom:integer); virtual; abstract;
    procedure DoFloodFill (x,y:integer); virtual; abstract;
    procedure DoPolygon (const points:array of TPoint); virtual; abstract;
  end;

  TFPEmptyBrush = class (TFPCustomBrush)
  end;


  PixelCanvasException = class (TFPCanvasException);

  TFPPixelCanvas = class (TFPCustomCanvas)
  private
    FHashWidth : word;
    FRelativeBI : boolean;
  protected
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoTextOut (x,y:integer;text:string); override;
    procedure DoGetTextSize (text:string; var w,h:integer); override;
    function  DoGetTextHeight (text:string) : integer; override;
    function  DoGetTextWidth (text:string) : integer; override;
    procedure DoRectangle (const Bounds:TRect); override;
    procedure DoRectangleFill (const Bounds:TRect); override;
    procedure DoEllipseFill (const Bounds:TRect); override;
    procedure DoEllipse (const Bounds:TRect); override;
    procedure DoPolygonFill (const points:array of TPoint); override;
    procedure DoPolygon (const points:array of TPoint); override;
    procedure DoPolyline (const points:array of TPoint); override;
    procedure DoFloodFill (x,y:integer); override;
    procedure DoLine (x1,y1,x2,y2:integer); override;
  public
    constructor create;
    property HashWidth : word read FHashWidth write FHashWidth;
    property RelativeBrushImage : boolean read FRelativeBI write FRelativeBI;
  end;


  { Pen and brush object both right now...}
  TPSPen = class(TFPCustomPen)
  private
    FPattern: TPSPattern;
    procedure SetPattern(const AValue: TPSPattern);
  public
    destructor Destroy; override;
    property Pattern: TPSPattern read FPattern write SetPattern;
    function AsString: String;
  end;

  TPSBrush = Class(TFPCustomBrush)
  Private
    Function GetAsString : String;
  Public
    Property AsString : String Read GetAsString;
  end;

   //procedure Pie(x,y,width,height,SX,SY,EX,EY : Integer);
    //procedure Chord(x,y,width,height,angle1,angle2 : Integer);
    //procedure Chord(x,y,width,height,SX,SY,EX,EY : Integer);
    //procedure PolyBezier(Points: PPoint; NumPts: Integer;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint;
    //                     Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                     Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint);
    //procedure Polygon(const Points: array of TPoint;
    //                  Winding: Boolean{$IFDEF VER1_1} = False{$ENDIF};
    //                  StartIndex: Integer{$IFDEF VER1_1} = 0{$ENDIF};
    //                  NumPts: Integer {$IFDEF VER1_1} = -1{$ENDIF});
    //procedure Polygon(Points: PPoint; NumPts: Integer;
    //                  Winding: boolean{$IFDEF VER1_1} = False{$ENDIF});
    //Procedure Polygon(const Points: array of TPoint);
    //Procedure FillRect(const Rect : TRect);
    //procedure FloodFill(X, Y: Integer; FillColor: TFPColor; FillStyle: TFillStyle);
    //Procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
    //Procedure RoundRect(const Rect : TRect; RX,RY : Integer);

var

  ArcCall: ArcCoordsType;   { Information on the last call to Arc or Ellipse }
procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);

procedure GetFillSettings(var Fillinfo:Fillsettingstype);
procedure GetFillPattern(var FillPattern:FillPatternType);
procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);
procedure SetFillStyle(Pattern : word; Color: word);
procedure SetFillPattern(Pattern: FillPatternType; Color: word);

 procedure MoveRel(Dx, Dy: smallint);
 procedure MoveTo(X,Y: smallint);


procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; c:TFPColor);
procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Width:integer; c:TFPColor);
procedure DrawPatternEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TPenPattern; c:TFPColor);

procedure FillEllipseColor (Canv:TFPCustomCanvas; const Bounds:TRect; c:TFPColor);
procedure FillEllipsePattern (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TBrushPattern; c:TFPColor);
procedure FillEllipseHashHorizontal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashVertical (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashBackDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashDiagCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseHashCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
procedure FillEllipseImage (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
procedure FillEllipseImageRel (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);

procedure SortRect (var rect : TRect);
procedure SortRect (var left,top, right,bottom : integer);

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);

PROCEDURE SetLineMode(Color,Pattern,Width,BitBlit:BYTE);
{set all the things needed for line drawing, optimized!}
BEGIN
  DrawMode    := LineDrawing;
  DrawPattern := Pattern;
  DrawWidth   := Width;
  _SetColorAndBitBlit(Color,BitBlit);
  SetBGColor(Black);
END; {SetLineMode}


PROCEDURE SetFillMode(Color,Pattern,BitBlit:BYTE);
{set all the things needed for fill drawing, optimized!}
BEGIN
  DrawMode    := FillDrawing;
  DrawPattern := Pattern;
  _SetColorAndBitBlit(Color,BitBlit);
  SetBGColor(Black);
END; {SetLineMode}


PROCEDURE GetDrawingMode(VAR Mode:DrawModeType);
{get the complete drawing mode}
BEGIN
  WITH Mode DO
  BEGIN
    D_Mode    := DrawMode;
    D_Pattern := DrawPattern;
    D_Width   := DrawWidth;
    D_FGColor := DrawFGColor;
    D_BGColor := DrawBGColor;
    D_BitBlit := DrawBitBlit;
  END;
END; {GetDrawingMode}


PROCEDURE SetDrawingMode(VAR Mode:DrawModeType);
{set the complete drawing mode}
BEGIN
  WITH Mode DO
  BEGIN
    DrawMode    := D_Mode;
    DrawPattern := D_Pattern;
    DrawWidth   := D_Width;
    DrawBGColor := D_BGColor;
    _SetColorAndBitBlit(D_FGColor,D_BitBlit);
  END;
END; {SetDrawingMode}

PROCEDURE _ThickLine(X1,Y1,X2,Y2:INTEGER);
{draw a line that is three pixels width by simply drawing three lines;
 this is not a good solution but it is the one that is used by
 borland as well, so we are still compatible with their graph unit,
 note that this code is not optimized for speed, it just works...}
VAR DX,DY:INTEGER;
BEGIN
  {compute steepness of line}
  DX := X2-X1;
  DY := Y2-Y1;
  {is it a more horizontal, or a more vertical line?}
  IF Abs(DX) > Abs(DY) THEN                 {it's more horizontal}
  BEGIN
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1,Y1+1,X2,Y2+1);
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1,Y1,X2,Y2);
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1,Y1-1,X2,Y2-1);
  END
  ELSE                                      {it's more vertical}
  BEGIN
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1+1,Y1,X2+1,Y2);
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1,Y1,X2,Y2);
    LineBits := Lining[DrawPattern];
    _ViewPortLine(X1-1,Y1,X2-1,Y2);
  END;
END; {_ThickLine}



  procedure HLineDefault(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    xtmp: smallint;
   Begin

    { must we swap the values? }
    if x >= x2 then
      Begin
        xtmp := x2;
        x2 := x;
        x:= xtmp;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    X2  := X2 + StartXViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for x:= x to x2 do
      DirectPutPixel(X,Y);
   end;


  procedure VLineDefault(x,y,y2: smallint); {$ifndef fpc}far;{$endif fpc}

   var
    ytmp: smallint;
  Begin
    { must we swap the values? }
    if y >= y2 then
     Begin
       ytmp := y2;
       y2 := y;
       y:= ytmp;
     end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    Y2  := Y2 + StartYViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    for y := y to y2 do Directputpixel(x,y)
  End;

 Procedure DirectPutPixelClip(x,y: smallint);
  { for thickwidth lines, because they may call DirectPutPixel for coords }
  { outside the current viewport (bug found by CEC)                       }
  Begin
    If (Not ClipPixels) Or
       ((X >= StartXViewPort) And (X <= (StartXViewPort + ViewWidth)) And
        (Y >= StartYViewPort) And (Y <= (StartYViewPort + ViewHeight))) then
      Begin
        DirectPutPixel(x,y)
      End
  End;



procedure RoundRectangle(x1,y1,x2,y2:smallint);
//see the use for it later.Makes a curved window.At least it SHOULD, my coords might me off.
   begin
     { Do not draw the end points }
     Line(x1,y1,x2-16,y1);
     Arc(x2-15,y1,0,90,15);     //should be 15pt arc.
     
     Line(x1,y1+16,x1,y2);
     Arc(x1,y2,0,90,15);
    
     Line(x2,y1,x2,y2-16);
     Arc(x2,y2-15,0,90,15);
    
     Line(x1+16,y2,x2,y2);
     Arc(x2,y2,0,90,15);
 end;



constructor TEllipseInfo.Create;
begin
  inherited;
  InfoList := TList.Create;
end;

destructor TEllipseInfo.Destroy;
begin
  FreeList;
  inherited;
end;

procedure TEllipseInfo.ClearList;
var r : integer;
    d : PEllipseInfoData;
begin
  if assigned (InfoList) then
    begin
    for r := 0 to infolist.count-1 do
      begin
      d := PEllipseInfoData(InfoList[r]);
      dispose (d);
      end;
    InfoList.clear;
    end;
end;

procedure TEllipseInfo.FreeList;
begin
  if assigned (InfoList) then
    begin
    ClearList;
    InfoList.Free;
    InfoList := nil;
    end;
end;

function TEllipseInfo.GetInfoForX (x:integer; var ytopmax,ytopmin,ybotmax,ybotmin:integer):boolean;
var r : PEllipseInfoData;
begin
  result := GetInfoForX (x, r);
  if assigned(r) then
    begin
    ytopmax := ytopmax;
    ytopmin := ytopmin;
    ybotmax := ybotmax;
    ybotmin := ybotmin;
    end;
end;

function TEllipseInfo.FindXIndex (x : integer) : integer;
begin
  result := InfoList.Count;
  repeat
    dec (result);
  until (result < 0) or (x = PEllipseInfoData(InfoList[result])^.x);
end;

function TEllipseInfo.GetInfoForX (x:integer; var Info:PEllipseInfoData):boolean;
var r : integer;
begin
  r := FindXIndex (x);
  result := (r >= 0);
  if result then
    Info := PEllipseInfoData(InfoList[r])
end;

procedure TEllipseInfo.PrepareCalculation (var np:integer; var delta:real);
begin
  np := round(1.5708 * sqrt(sqr(frx)+sqr(fry)) );
  // number of pixel in quarter circel to calculate without gaps in drawing
  delta := pi / (2 * np);
end;

function TEllipseInfo.NewInfoRec (anX:integer) : PEllipseInfoData;
begin
  new (result);
  result^.x := anX;
  infolist.Add (result);
  with result^ do
    begin
    ytopmax := -1;
    ytopmin := maxint;
    ybotmax := -1;
    ybotmin := maxint;
    end;
end;

procedure TEllipseInfo.CalculateCircular (const b:TRect; var x,y,rx,ry:real);
begin
  with b do
    begin
    x := (right+left) / 2;
    y := (top+bottom) / 2;
    rx := abs(right-left) / 2;
    ry := abs(bottom-top) / 2;
    end;
end;

procedure TEllipseInfo.GatherEllipseInfo (const bounds:TRect);
var infoP, infoM : PEllipseInfoData;
    halfnumber,
    r, NumberPixels, xtemp,yt,yb : integer;
    pPy, pMy, x,y, rx,ry, xd,yd,ra, rdelta : real;
begin
  ClearList;
  CalculateCircular (bounds, x,y,rx,ry);
  with bounds do
  fcx := x;
  fcy := y;
  frx := rx;
  fry := ry;
  if (rx < 0.5) and (ry < 0.5) then
    with NewInfoRec (round(x))^ do
      begin
      ytopmax := round(y);
      ytopmin := ytopmax;
      ybotmax := ytopmax;
      ybotmin := ytopmax;
      end
  else
    begin
    PrepareCalculation (NumberPixels, rdelta);
    halfnumber := NumberPixels div 2;
    pPy := maxint;
    pMy := maxint;
    ra := 0;
    infoP := NewInfoRec (round(x + rx));
    infoM := NewInfoRec (round(x - rx));
    for r := 0 to NumberPixels do
      begin
      xd := rx * cos(ra);
      yd := ry * sin(ra);
      // take all 4 quarters
      yt := round(y - yd);
      yb := round(y + yd);
      xtemp := round (x + xd);
      // quarter 1 and 4 at the same x line
      if infoP^.x <> xtemp then                  // has correct record ?
        begin
        with infoP^ do                           // ensure single width
          begin
          if r < halfnumber then
            begin
            if ytopmin = yt then
              begin
              inc (ytopmin);
              dec (ybotmax);
              end;
            end
          else
            begin
            if (ytopmax = pPy) and (ytopmax <> ytopmin) then
              begin
              dec (ytopmax);
              inc (ybotmin);
              end;
            end;
          pPy := ytopmin;
          end;
        if not GetInfoForX (xtemp, infoP) then  // record exists already ?
          infoP := NewInfoRec (xtemp);          // create a new recod
        end;
      // lower y is top, min is lowest
      with InfoP^ do
        begin
        if yt < ytopmin then
          ytopmin := yt;
        if yb < ybotmin then
          ybotmin := yb;
        if yt > ytopmax then
          ytopmax := yt;
        if yb > ybotmax then
          ybotmax := yb;
        end;
      // quarter 2 and 3 on the same x line
      xtemp := round(x - xd);
      if infoM^.x <> xtemp then                  // has correct record ?
        begin
        with infoM^ do             // ensure single width
          begin
          if r < halfnumber then
            begin
            if ytopmin = yt then
              begin
              inc (ytopmin);
              dec (ybotmax);
              end;
            end
          else
            begin
            if (ytopmax = pMy) and (ytopmax <> ytopmin) then
              begin
              dec (ytopmax);
              inc (ybotmin);
              end;
            end;
          pMy := ytopmin;
          end;
        if not GetInfoForX (xtemp, infoM) then  // record exists already ?
          infoM := NewInfoRec (xtemp);          // create a new recod
        end;
      // lower y is top, min is lowest
      with InfoM^ do
        begin
        if yt < ytopmin then
          ytopmin := yt;
        if yb < ybotmin then
          ybotmin := yb;
        if yt > ytopmax then
          ytopmax := yt;
        if yb > ybotmax then
          ybotmax := yb;
        end;
      ra := ra + rdelta;
      end;
    end;
end;

procedure TEllipseInfo.GatherArcInfo (const bounds:TRect; alpha1,alpha2:real);
var stAngle,endAngle:real;

  procedure CheckAngles;
  begin
    if a1 < a2 then
      begin
      stAngle := a1;
      endAngle := a2;
      end
    else
      begin
      stAngle := a2;
      endAngle := a1;
      end;
  end;

begin
end;


{ The drawing routines }

type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
  TLinePoints = array[0..PatternBitCount-1] of boolean;
  PLinePoints = ^TLinePoints;

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

procedure PutPixelCopy(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := color;
end;

procedure PutPixelXor(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] xor color;
end;

procedure PutPixelOr(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] or color;
end;

procedure PutPixelAnd(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] and color;
end;

procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    MyPutPix : TPutPixelProc;
begin
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  info := TEllipseInfo.Create;
  with Canv, info do
    try
      GatherEllipseInfo (bounds);
      for r := 0 to InfoList.count-1 do
        with PEllipseInfoData(InfoList[r])^ do
          begin
          for y := ytopmin to ytopmax do
            MyPutPix (Canv, x,y, c);
          for y := ybotmin to ybotmax do
            MyPutPix (Canv, x,y, c);
          end;
    finally
      info.Free;
    end;
end;

procedure DrawSolidEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Width:integer; c:TFPColor);
var infoOut, infoIn : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    MyPutPix : TPutPixelProc;
begin
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  infoIn := TEllipseInfo.Create;
  infoOut := TEllipseInfo.Create;
  dec (width);
  try
    infoOut.GatherEllipseInfo(bounds);
    with bounds do
      infoIn.GatherEllipseInfo (Rect(left+width,top+width,right-width,bottom-width));
    with Canv do
      for r := 0 to infoOut.infolist.count-1 do
        with PEllipseInfoData (infoOut.infolist[r])^ do
          begin
          if infoIn.GetInfoForX (x, id) then
            begin
            for y := ytopmin to id^.ytopmax do
              MyPutPix (canv, x,y, c);
            for y := id^.ybotmin to ybotmax do
              MyPutPix (canv, x,y, c);
            end
          else
            begin // no inner circle found: draw all points between top and bottom
            for y := ytopmin to ybotmax do
              MyPutPix (canv, x,y, c);
            end;
          end;
    finally
      infoOut.Free;
      infoIn.Free;
    end;
end;

procedure DrawPatternEllipse (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TPenPattern; c:TFPColor);
var info : TEllipseInfo;
    xx, y : integer;
    LinePoints : TLinePoints;
    MyPutPix : TPutPixelProc;
    id : PEllipseInfoData;
    CountDown, CountUp, half : integer;
begin
  with canv.pen do
    case mode of
      pmMask : MyPutPix := @PutPixelAnd;
      pmMerge : MyPutPix := @PutPixelOr;
      pmXor : MyPutPix := @PutPixelXor;
      else MyPutPix := @PutPixelCopy;
    end;
  PatternToPoints (pattern, @LinePoints);
  info := TEllipseInfo.Create;
  with Canv, info do
    try
      GatherEllipseInfo (bounds);
      CountUp := 0;
      CountDown := PatternBitCount - 1;
      half := round (cx);
      for xx := bounds.left to half do
        if GetInfoForX (xx, id) then
          begin
          with id^ do
            begin
            for y := ytopmax downto ytopmin do
              begin
              if LinePoints[CountUp mod PatternBitCount] then
                MyPutPix (Canv, xx,y, c);
              inc (CountUp);
              end;
            for y := ybotmin to ybotmax do
              begin
              if LinePoints[PatternBitCount - (CountDown mod PatternBitCount) - 1] then
                MyPutPix (Canv, xx,y, c);
              inc (CountDown);
              end;
            end;
          end;
      for xx := half+1 to bounds.right do
        if GetInfoForX (xx, id) then
          begin
          with id^ do
            begin
            for y := ytopmin to ytopmax do
              begin
              if LinePoints[CountUp mod PatternBitCount] then
                MyPutPix (Canv, xx,y, c);
              inc (CountUp);
              end;
            for y := ybotmax downto ybotmin do
              begin
              if LinePoints[Patternbitcount - (CountDown mod PatternBitCount) - 1] then
                MyPutPix (Canv, xx,y, c);
              inc (CountDown);
              end;
            end;
          end;
    finally
      info.Free;
    end;
end;

procedure FillEllipseColor (Canv:TFPCustomCanvas; const Bounds:TRect; c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    with Canv do
      for r := 0 to info.infolist.count-1 do
        with PEllipseInfoData (info.infolist[r])^ do
          for y := ytopmin to ybotmax do
            colors[x,y] := c;
  finally
    info.Free;
  end;
end;

procedure FillEllipsePattern (Canv:TFPCustomCanvas; const Bounds:TRect; Pattern:TBrushPattern; c:TFPColor);
begin
end;

procedure FillEllipseHashHorizontal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        for y := ytopmin to ybotmax do
          if (y mod width) = 0 then
            canv.colors[x,y] := c;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashVertical (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        if (x mod width) = 0 then
          for y := ytopmin to ybotmax do
            canv.colors[x,y] := c;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := width - 1 - (x mod width);
        for y := ytopmin to ybotmax do
          if (y mod width) = w then
            canv.colors[x,y] := c;
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashBackDiagonal (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := (x mod width);
        for y := ytopmin to ybotmax do
          if (y mod width) = w then
            canv.colors[x,y] := c;
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashDiagCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    wy,w1,w2 : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w1 := (x mod width);
        w2 := width - 1 - (x mod width);
        for y := ytopmin to ybotmax do
          begin
          wy := y mod width;
          if (wy = w1) or (wy = w2) then
            canv.colors[x,y] := c;
          end;
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseHashCross (Canv:TFPCustomCanvas; const Bounds:TRect; width:integer; const c:TFPColor);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        if (x mod width) = 0 then
          for y := ytopmin to ybotmax do
            canv.colors[x,y] := c
        else
          for y := ytopmin to ybotmax do
            if (y mod width) = 0 then
              canv.colors[x,y] := c;
  finally
    info.Free;
  end;
end;

procedure FillEllipseImage (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    w : integer;
begin
  info := TEllipseInfo.Create;
  try
    info.GatherEllipseInfo(bounds);
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        w := (x mod image.width);
        for y := ytopmin to ybotmax do
          canv.colors[x,y] := Image.colors[w, (y mod image.height)];
        end;
  finally
    info.Free;
  end;
end;

procedure FillEllipseImageRel (Canv:TFPCustomCanvas; const Bounds:TRect; const Image:TFPCustomImage);
var info : TEllipseInfo;
    r, y : integer;
    id : PEllipseInfoData;
    xo,yo, xi,yi : integer;
begin
  info := TEllipseInfo.Create;
  try
    with info do
      begin
      GatherEllipseInfo(bounds);
      xo := round(cx) - (image.width div 2);
      yo := round(cy) - (image.height div 2);
      end;
    for r := 0 to info.infolist.count-1 do
      with PEllipseInfoData (info.infolist[r])^ do
        begin
        xi := (x - xo) mod image.width;
        if xi < 0 then
          inc (xi, image.width);
        for y := ytopmin to ybotmax do
          begin
          yi := (y - yo) mod image.height;
          if yi < 0 then
            inc (yi, image.height);
          canv.colors[x,y] := Image.colors[xi, yi];
          end;
        end;
  finally
    info.Free;
  end;
end;


  procedure GetFillSettings(var Fillinfo:Fillsettingstype);
   begin
     Fillinfo:=Fillsettings;
   end;

  procedure GetFillPattern(var FillPattern:FillPatternType);
   begin
     FillPattern:=FillpatternTable[UserFill];
   end;

  procedure DrawPoly(numpoints : word;var polypoints);
    type
      ppointtype = ^pointtype;
      pt = array[0..16000] of pointtype;
    var
      i : longint;
    begin
      if numpoints < 2 then
        begin
          _GraphResult := grError;
          exit;
        end;
      for i:=0 to numpoints-2 do
        line(pt(polypoints)[i].x,
             pt(polypoints)[i].y,
             pt(polypoints)[i+1].x,
             pt(polypoints)[i+1].y);
    end;


  procedure PieSlice(X,Y,stangle,endAngle:smallint;Radius: Word);
  begin
    Sector(x,y,stangle,endangle,radius,radius);
  end;




function max(a, b : Longint) : Longint;
begin
  max := b;
  if (a > b) then max := a;
end;


function min(a, b : Longint) : Longint;
begin
  min := b;
  if (a < b) then min := a;
end;

procedure fillpoly(numpoints : Word; var polypoints);

{ disable range check mode }
{$ifopt R+}
{$define OPT_R_WAS_ON}
{$R-}
{$endif}
type
  pedge = ^tedge;
  tedge = packed record
    yMin, yMax, x, dX, dY, frac : Longint;
  end;

  pedgearray = ^tedgearray;
  tedgearray = array[0..0] of tedge;

  ppedgearray = ^tpedgearray;
  tpedgearray = array[0..0] of pedge;

var
  nActive, nNextEdge : Longint;
  p0, p1 : pointtype;
  i, j, gap, x0, x1, y, nEdges : Longint;
  ET : pedgearray;
  GET, AET : ppedgearray;
  t : pedge;

  ptable : ^pointtype;


begin
{ /********************************************************************
  * Add entries to the global edge table.  The global edge table has a
  * bucket for each scan line in the polygon. Each bucket contains all
  * the edges whose yMin == yScanline.  Each bucket contains the yMax,
  * the x coordinate at yMax, and the denominator of the slope (dX)
*/}
  getmem(et, sizeof(tedge) * numpoints);
  getmem(get, sizeof(pedge) * numpoints);
  getmem(aet, sizeof(pedge) * numpoints);

  ptable := @polypoints;

 { check for getmem success }

  nEdges := 0;
  for i := 0 to (numpoints-1) do begin
    p0 := ptable[i];
    if (i+1) >= numpoints then p1 := ptable[0]
    else p1 := ptable[i+1];
   { ignore if this is a horizontal edge}
    if (p0.y = p1.y) then continue;
    {swap ptable if necessary to ensure p0 contains yMin}
    if (p0.y > p1.y) then begin
      p0 := p1;
      p1 := ptable[i];
    end;
   { create the new edge }
    et^[nEdges].ymin := p0.y;
    et^[nEdges].ymax := p1.y;
    et^[nEdges].x := p0.x;
    et^[nEdges].dX := p1.x-p0.x;
    et^[nEdges].dy := p1.y-p0.y;
    et^[nEdges].frac := 0;
    get^[nEdges] :=  @et^[nEdges];
    inc(nEdges);
  end;
 { sort the GET on ymin }
  gap := 1;
  while (gap < nEdges) do gap := 3*gap+1;
  gap := gap div 3;
  while (gap > 0) do begin
    for i := gap to (nEdges-1) do begin
      j := i - gap;
      while (j >= 0) do begin
        if (GET^[j]^.ymin <= GET^[j+gap]^.yMin) then break;
        t := GET^[j];
        GET^[j] := GET^[j+gap];
        GET^[j+gap] := t;
        dec(j, gap);
      end;
    end;
    gap := gap div 3;
  end;
  { initialize the active edge table, and set y to first entering edge}
  nActive := 0;
  nNextEdge := 0;

  y := GET^[nNextEdge]^.ymin;
  { Now process the edges using the scan line algorithm.  Active edges
  will be added to the Active Edge Table (AET), and inactive edges will
  be deleted.  X coordinates will be updated with incremental integer
  arithmetic using the slope (dY / dX) of the edges. }
  while (nNextEdge < nEdges) or (nActive <> 0) do begin
    {Move from the ET bucket y to the AET those edges whose yMin == y
    (entering edges) }
    while (nNextEdge < nEdges) and (GET^[nNextEdge]^.ymin = y) do begin
      AET^[nActive] := GET^[nNextEdge];
      inc(nActive);
      inc(nNextEdge);
    end;
    { Remove from the AET those entries for which yMax == y (leaving
    edges) }
    i := 0;
    while (i < nActive) do begin
      if (AET^[i]^.yMax = y) then begin
        dec(nActive);
        System.move(AET^[i+1], AET^[i], (nActive-i)*sizeof(pedge));
      end else
        inc(i);
    end;

    if (y >= 0) then begin
    {Now sort the AET on x.  Since the list is usually quite small,
    the sort is implemented as a simple non-recursive shell sort }

    gap := 1;
    while (gap < nActive) do gap := 3*gap+1;

    gap := gap div 3;
    while (gap > 0) do begin
      for i := gap to (nActive-1) do begin
        j := i - gap;
        while (j >= 0) do begin
          if (AET^[j]^.x <= AET^[j+gap]^.x) then break;
          t := AET^[j];
          AET^[j] := AET^[j+gap];
          AET^[j+gap] := t;
          dec(j, gap);
        end;
      end;
      gap := gap div 3;
    end;

    { Fill in desired pixels values on scan line y by using pairs of x
    coordinates from the AET }
    i := 0;
    while (i < nActive) do begin
      x0 := AET^[i]^.x;
      x1 := AET^[i+1]^.x;
      {Left edge adjustment for positive fraction.  0 is interior. }
      if (AET^[i]^.frac > 0) then inc(x0);
      {Right edge adjustment for negative fraction.  0 is exterior. }
      if (AET^[i+1]^.frac <= 0) then dec(x1);

      x0 := max(x0, 0);
      x1 := min(x1, viewWidth);
      { Draw interior spans}
      if (x1 >= x0) then begin
        PatternLine(x0, x1, y);
      end;

      inc(i, 2);
    end;

    end;

    { Update all the x coordinates.  Edges are scan converted using a
    modified midpoint algorithm (Bresenham's algorithm reduces to the
    midpoint algorithm for two dimensional lines) }
    for i := 0 to (nActive-1) do begin
      t := AET^[i];
      { update the fraction by dX}
      inc(t^.frac, t^.dX);

      if (t^.dX < 0) then
        while ( -(t^.frac) >= t^.dY) do begin
          inc(t^.frac, t^.dY);
          dec(t^.x);
        end
      else
        while (t^.frac >= t^.dY) do begin
          dec(t^.frac, t^.dY);
          inc(t^.x);
        end;
    end;
    inc(y);
    if (y >= ViewHeight) then break;
  end;
  System.freemem(et, sizeof(tedge) * numpoints);
  System.freemem(get, sizeof(pedge) * numpoints);
  System.freemem(aet, sizeof(pedge) * numpoints);
end;

 
  Procedure FloodFill (x, y : smallint; Border: word);
  {********************************************************}
  { Procedure FloodFill()                                  }
  {--------------------------------------------------------}
  { This routine fills a region of the screen bounded by   }
  { the <Border> color. It uses the current fillsettings   }
  { for the flood filling. Clipping is supported, and      }
  { coordinates are local/viewport relative.               }
  {********************************************************}
  Var
   stemp: PWordArray;
   Beginx : smallint;
   d, e : Byte;
   Cont : Boolean;
   BackupColor : Word;
   x1, x2, prevy: smallint;
  Begin
    FillChar(DrawnList,sizeof(DrawnList),0);
    { init prevy }
    prevy := 32767;
    { Save current drawing color }
    BackupColor := CurrentColor;
    CurrentColor := FillSettings.Color;
    { MaxX is based on zero index }
    GetMem (s1,(ViewWidth+1)*2);  { A pixel color represents a word }
    GetMem (s2,(ViewWidth+1)*2);  { A pixel color represents a word }
    GetMem (s3,(ViewWidth+1)*2);  { A pixel color represents a word }
    if (not assigned(s1)) or (not assigned(s2)) or (not assigned(s3)) then
      begin
        _GraphResult := grNoFloodMem;
        exit;
      end;
    If (x<0) Or (y<0) Or
       (x>ViewWidth) Or (y>ViewHeight) then Exit;
    { Index of points to check  }
    Buffer.WordIndex:=0;
    PushPoint (x,y);
    While Buffer.WordIndex>0 Do
     Begin
       PopPoint (x,y);
       { Get the complete lines for the following }
       If y <> prevy then
         begin
           If (prevy - y = 1) then
             { previous line was one below the new one, so the previous s2 }
             { = new s1                                                    }
             Begin
               stemp := s3;
               s3 := s1;
               s1 := s2;
               s2 := stemp;
               GetScanline(0,ViewWidth,y-1,s2^);
             End
           Else If (y - prevy = 1) then
             { previous line was one above the new one, so the previous s3 }
             { = new s1                                                    }
             Begin
               stemp := s2;
               s2 := s1;
               s1 := s3;
               s3 := stemp;
               GetScanline(0,ViewWidth,y+1,s3^);
             End
           Else
             begin
               GetScanline(0,ViewWidth,y-1,s2^);
               GetScanline(0,ViewWidth,y,s1^);
               GetScanline(0,ViewWidth,y+1,s3^);
             end;
         end;
       prevy := y;
       { check the current scan line }
       While (s1^[x]<>Border) And (x<=ViewWidth) Do Inc (x);
       d:=0;
       e:=0;
       dec(x);
       Beginx:=x;
       REPEAT
         { check the above line }
         If y<ViewHeight then
           Begin
              Cont:=(s3^[x]<>Border) and (not AlreadyDrawn(x,y+1));
              If (e=0) And Cont then
                Begin
                  PushPoint (x,y+1);
                  e:=1;
                End
              Else
                If (e=1) And Not Cont then e:=0;
           End;
        { check the line below }
        If (y>0) then
          Begin
            Cont:=(s2^[x]<>Border) and (not AlreadyDrawn(x,y-1));
            If (d=0) And Cont then
              Begin
                PushPoint (x,y-1);
                d:=1;
              End
            Else
              If (d=1) And Not Cont then d:=0;
          End;
        Dec (x);
       Until (x<0) Or (s1^[x]=Border);
       { swap the values }
       x1:=x+1;
       x2:=BeginX;
       if x1 > x2 then
         Begin
           x:=x1;
           x1:=x2;
           x2:=x;
         end;
       { Add to the list of drawn lines }
       AddLinePoints(x1,x2,y);
       PatternLine (x1,x2,y);
     End; { end while }

    System.FreeMem (s1,(ViewWidth+1)*2);
    System.FreeMem (s2,(ViewWidth+1)*2);
    System.FreeMem (s3,(ViewWidth+1)*2);
    CleanUpDrawnList;
    CurrentColor := BackUpColor;
  End;

{ restore previous range check mode }
{$ifdef OPT_R_WAS_ON}
{$R+}
{$endif}



{    TFPCustomBrush implementation.}

procedure TFPCustomBrush.SetStyle (AValue : TFPBrushStyle);
begin
  FStyle := AValue
end;

procedure TFPCustomBrush.SetImage (AValue : TFPCustomImage);
begin
  FImage := AValue;
end;

procedure TFPCustomBrush.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomBrush do
    begin
    self.Style := Style;
    self.Image := Image;
    end;
  inherited DoCopyProps(From);
end;

function TFPCustomBrush.CopyBrush : TFPCustomBrush;
begin
  result := TFPCustomBrush(self.ClassType.Create);
  result.DoCopyProps (self);
end;


{   TFPCustomCanvas implementation.}

constructor TFPCustomCanvas.Create;
begin
  inherited create;
  FClipRect := Rect(-1,-1,-1,-1);
  FClipping := false;
  FRemovingHelpers := false;
  FHelpers := TList.Create;
  FDefaultFont := CreateDefaultFont;
  FDefaultPen := CreateDefaultPen;
  FDefaultBrush := CreateDefaultBrush;
end;

destructor TFPCustomCanvas.Destroy;
begin
  FRemovingHelpers := True;
  // first remove all helper references
  RemoveHelpers;
  // then free helpers
  FDefaultFont.Free;
  FDefaultBrush.Free;
  FDefaultPen.Free;
  FHelpers.Free;
  FRemovingHelpers := False;
  inherited;
end;

procedure TFPCustomCanvas.CheckHelper (AHelper:TFPCanvasHelper);
// remove references to AHelper
begin
  if AHelper = FPen then
    FPen := nil
  else if AHelper = FFont then
    FFont := nil
  else if AHelper = FBrush then
    FBrush := nil;
  if not FRemovingHelpers then
    begin
    if AHelper = FDefaultFont then
      FDefaultFont := CreateDefaultFont
    else if AHelper = FDefaultPen then
      FDefaultPen := CreateDefaultPen
    else if AHelper = FDefaultBrush then
      FDefaultBrush := CreateDefaultBrush;
    end;
  FHelpers.Remove (AHelper);
end;

procedure TFPCustomCanvas.RemoveHelpers;
var r : integer;
    OldState : boolean;
begin
  for r := FHelpers.count-1 downto 0 do
    with TFPCanvasHelper(FHelpers[r]) do
      if FCanvas = self then
        if FFixedCanvas then
          DeallocateResources
        else
          FCanvas := nil;
  FHelpers.Clear;
end;

procedure TFPCustomCanvas.AddHelper (AHelper : TFPCanvasHelper);
var r : integer;
begin
  r := FHelpers.IndexOf (AHelper);
  if r < 0 then
    FHelpers.Add (AHelper);
end;

function TFPCustomCanvas.CreateDefaultFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EFont])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultPen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EPen])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EBrush])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.GetClipRect: TRect;
begin
  Result:=FClipRect;
end;

function TFPCustomCanvas.CreateFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
end;

function TFPCustomCanvas.CreatePen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
end;

function TFPCustomCanvas.CreateBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
end;

function TFPCustomCanvas.AllowFont (AFont : TFPCustomFont) : boolean;
begin
  if AFont is TFPCustomDrawFont then
    result := true
  else
    result := DoAllowFont (AFont);
end;

procedure TFPCustomCanvas.SetFont (AValue:TFPCustomFont);
begin
  if (AValue <> FFont) and AllowFont(AValue) then
    begin
      if FManageResources then
        FFont.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FFont := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetFont : TFPCustomFont;
begin
  if assigned (FFont) then
    result := FFont
  else
    result := FDefaultFont;
end;

function TFPCustomCanvas.DoAllowFont (AFont : TFPCustomFont) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  if ABrush is TFPCustomDrawBrush then
    result := true
  else
    result := DoAllowBrush (ABrush);
end;

procedure TFPCustomCanvas.SetBrush (AValue:TFPCustomBrush);
begin
  if (AValue <> FBrush) and AllowBrush(AValue) then
    begin
      if FManageResources then
        FBrush.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FBrush := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetBrush : TFPCustomBrush;
begin
  if assigned (FBrush) then
    result := FBrush
  else
    result := FDefaultBrush
end;

function TFPCustomCanvas.DoAllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowPen (APen : TFPCustomPen) : boolean;
begin
  if APen is TFPCustomDrawPen then
    result := true
  else
    result := DoAllowPen (APen);
end;

procedure TFPCustomCanvas.SetPen (AValue:TFPCustomPen);
begin
  if (AValue <> FPen) and AllowPen (AValue) then
    begin
      if FManageResources then
        FPen.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FPen := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetPen : TFPCustomPen;
begin
  if assigned (FPen) then
    result := FPen
  else
    result := FDefaultPen;
end;


procedure TFPCustomCanvas.SetClipRect(const AValue: TRect);
begin
  FClipRect:=AValue;
end;

procedure TFPCustomCanvas.SetPenPos(const AValue: TPoint);
begin
  FPenPos:=AValue;
end;

function TFPCustomCanvas.DoAllowPen (APen : TFPCustomPen) : boolean;
begin
  result := false;
end;

procedure TFPCustomCanvas.DoLockCanvas;
begin
end;

procedure TFPCustomCanvas.DoUnlockCanvas;
begin
end;



procedure TFPCustomCanvas.LockCanvas;
begin
  if FLocks = 0 then
    DoLockCanvas;
  inc (FLocks);
end;

procedure TFPCustomCanvas.UnlockCanvas;
begin
  if FLocks > 0 then
    begin
    dec (FLocks);
    if FLocks = 0 then
      DoUnlockCanvas;
    end
  else
    raise TFPCanvasException.Create (ErrNoLock);
end;

function TFPCustomCanvas.Locked: boolean;
begin
  Result:=FLocks>0;
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:string);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:string; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:string) : integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.GetTextWidth (text:string) : integer;
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.DoMoveTo (x,y:integer);
begin
end;

procedure TFPCustomCanvas.DoLineTo (x,y:integer);
begin
  DoLine (FPenPos.X,FPenPos.y, x,y);
end;

procedure TFPCustomCanvas.MoveTo (x,y:integer);
begin
  FPenPos.x := x;
  FPenPos.y := y;
  DoMoveTo (x,y);
end;

procedure TFPCustomCanvas.MoveTo (p:TPoint);
begin
  FPenPos := p;
  DoMoveTo (p.x,p.y);
end;

procedure TFPCustomCanvas.LineTo (x,y:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (FPenPos.x, FPenPos.y, x, y)
    else
      DoLineTo (x,y);
  FPenPos.x := x;
  FPenPos.y := y;
end;

procedure TFPCustomCanvas.LineTo (p:TPoint);
begin
  LineTo (p.x, p.y);
end;

procedure TFPCustomCanvas.Line (x1,y1,x2,y2:integer);
begin
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (x1,y1, x2,y2)
    else
      DoLine (x1,y1, x2,y2);
  FPenPos.x := x2;
  FPenPos.y := y2;
end;

procedure TFPCustomCanvas.Line (const p1,p2:TPoint);
begin
  Line (p1.x,p1.y,p2.x,p2.y);
end;

procedure TFPCustomCanvas.Line (const points:TRect);
begin
  with points do
    Line (left,top, right,bottom);
end;

procedure TFPCustomCanvas.Polyline (Const points:array of TPoint);
begin
  if Pen.Style <> psClear then
   if Pen is TFPCustomDrawPen then
     TFPCustomDrawPen(Pen).Polyline (points,false)
   else
     DoPolyline (points);
  FPenPos := points[high(points)];
end;

procedure TFPCustomCanvas.Clear;
var r : TRect;
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush(Brush).Rectangle(0,0, width, height)
    else
      begin
      r := Rect(0,0, width, height);
      DoRectangleFill (r);
      end;
    end;
end;

procedure TFPCustomCanvas.Erase;
var
  x,y:Integer;
begin
  for x:=0 to Width-1 do
    for y:=0 to Height-1 do
      Colors[x,y]:=colTransparent;
end;

procedure TFPCustomCanvas.DoRectangleAndFill (const Bounds:TRect);
begin
  DoRectangleFill (Bounds);
  DoRectangle (Bounds);
end;

procedure TFPCustomCanvas.DoEllipseAndFill (const Bounds:TRect);
begin
  DoEllipseFill (Bounds);
  DoEllipse (Bounds);
end;

procedure TFPCustomCanvas.DoPolygonAndFill (const points:array of TPoint);
begin
  DoPolygonFill (points);
  DoPolygon (points);
end;

procedure TFPCustomCanvas.Ellipse (const Bounds:TRect);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  pb := false;
  dp:=False;
  db:=False;
  if p and (Pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (Brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoEllipseAndFill (bounds)
  else
    begin
    if p then
      DoEllipse (bounds)
    else if dp then
      with bounds do
        TFPCustomDrawPen(Pen).Ellipse (left,top,right,bottom);
    if b then
      DoEllipseFill (bounds)
    else if db then 
      with bounds do
        TFPCustomDrawBrush(Brush).Ellipse (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.Ellipse (left,top,right,bottom:integer);
begin
  Ellipse (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.EllipseC (x,y:integer; rx,ry:longword);
begin
  Ellipse (Rect(x-rx,y-ry,x+rx,y+ry));
end;

procedure TFPCustomCanvas.Rectangle (left,top,right,bottom:integer);
begin
  Rectangle (Rect(left,top,right,bottom));
end;


procedure TFPCustomCanvas.Rectangle (const Bounds:TRect);
var np,nb,dp,db,pb : boolean;
begin
  np:= Pen.style <> psClear; // Need pen ?
  nb:= Brush.style <> bsClear;  // Need brush ?
  dp:=(pen is TFPCustomDrawPen); // Pen draws ?
  db:=(brush is TFPCustomDrawBrush); // Brush draws ?
  if (np and nb) and not (db or db) then
    DoRectangleAndFill (bounds)
  else
    begin
    if np then
      begin
      If not dp then
        DoRectangle (bounds)
      else
        with bounds do
          TFPCustomDrawPen(Pen).Rectangle (left,top,right,bottom);
      end;
    if Nb then
      begin
      if not db then
        DoRectangleFill (bounds)
      else 
        with bounds do
          TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
      end;
    end;
end;

procedure TFPCustomCanvas.FloodFill (x,y:integer);
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush (Brush).FloodFill (x,y)
    else
      DoFloodFill (x,y);
    end;
end;

procedure TFPCustomCanvas.Polygon (const points:array of TPoint);
var p,b,dp,db,pb : boolean;
begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  dp:=false;
  db:=false;
  pb:=False;
  if p and (pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoPolygonAndFill (points)
  else
    begin
    if p then
      DoPolygon (points)
    else if dp then
      TFPCustomDrawPen(Pen).Polyline (points, true);
    if b then
      DoPolygonFill (points)
    else if db then
      TFPCustomDrawBrush(Brush).Polygon (points);
    end;
end;

procedure TFPCustomCanvas.CopyRect (x,y:integer; canvas:TFPCustomCanvas;
  SourceRect:TRect);
var xx,r,t : integer;
begin
  SortRect (SourceRect);
  with SourceRect do
    for r := left to right do
      begin
      xx := r - left + x;
      for t := bottom to top do
        colors[xx,(t - bottom + y)] := canvas.colors[r,t];
      end;
end;

procedure TFPCustomCanvas.Draw (x,y:integer; image:TFPCustomImage);
var xx,xi,yi,xm,ym,r,t : integer;
begin
  xm := x + image.width-1;
  if xm >= width then
    xm := width - 1;
  ym := y + image.height-1;
  if ym >= height then
    ym := height - 1;
  xi := x;
  yi := y;
  if clipping then
    CheckRectClipping (ClipRect, xi,yi, xm,ym);
  for r := xi to xm do
    begin
    xx := r - x;
    for t := yi to ym do
      colors [r,t] := image.colors[xx,t-y];
    end;
end;

procedure TFPCustomCanvas.StretchDraw(x, y, w, h: integer; source: TFPCustomImage);
var i : TFPCustomInterpolation;
    FreeInterpolation : boolean;
    IP : TFPCustomInterpolation;
begin
  FreeInterpolation := not assigned (FInterpolation);
  if FreeInterpolation then
    IP := TMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    with IP do
      begin
      Initialize (source, self);
      Execute (x,y,w,h);
      end;
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;


procedure DecRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left + delta;
    right := right - delta;
    top := top + delta;
    bottom := bottom - delta;
    end;
end;

procedure DecRect (var rect : trect);
begin
  DecRect (rect, 1);
end;

procedure IncRect (var rect : trect);
begin
  IncRect (rect, 1);
end;

procedure IncRect (var rect : TRect; delta:integer);
begin
  with rect do
    begin
    left := left - delta;
    right := right + delta;
    top := top - delta;
    bottom := bottom + delta;
    end;
end;

{    TDrawObjects implementation}
{ TFPCustomDrawPen }

procedure TFPCustomDrawPen.DrawLine (x1,y1,x2,y2:integer);
begin
  DoDrawLine (x1,y1,x2,y2);
end;

procedure TFPCustomDrawPen.Polyline (const points:array of TPoint; close:boolean);
begin
  DoPolyLine (points, false);
end;

procedure TFPCustomDrawPen.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawPen.Rectangle (left,top, right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

{ TFPCustomDrawBrush }

procedure TFPCustomDrawBrush.Rectangle (left,top,right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.FloodFill (x,y:integer);
begin
  DoFloodFill (x,y);
end;

procedure TFPCustomDrawBrush.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.Polygon (const points:array of TPoint);
begin
  DoPolygon (points);
end;

{ TFPCustomDrawFont }

procedure TFPCustomDrawFont.DrawText (x,y:integer; text:string);
begin
  DoDrawText (x,y, text);
end;

procedure TFPCustomDrawFont.GetTextSize (text:string; var w,h:integer);
begin
  DoGetTextSize (text, w,h);
end;

function TFPCustomDrawFont.GetTextHeight (text:string) : integer;
begin
  result := DoGetTextHeight (Text);
end;

function TFPCustomDrawFont.GetTextWidth (text:string) : integer;
begin
  result := DoGetTextWidth (Text);
end;


constructor TFPImageCanvas.create (AnImage : TFPCustomImage);
begin
  inherited Create;
  FImage := AnImage;
end;

destructor TFPImageCanvas.destroy;
begin
  inherited destroy;
end;

procedure TFPImageCanvas.SetColor (x,y:integer; const AValue:TFPColor);
begin
  if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
    if not clipping or PointInside (x,y, ClipRect) then
      FImage.Colors[x,y] := AValue;
end;

function  TFPImageCanvas.GetColor (x,y:integer) : TFPColor;
begin
  if (x >= 0) and (x < width) and (y >= 0) and (y < height) then
    result := FImage.Colors[x,y]
  else
    result := colTransparent;
end;

procedure TFPImageCanvas.SetHeight (AValue : integer);
begin
  FImage.Height := AValue;
end;

function  TFPImageCanvas.GetHeight : integer;
begin
  result := FImage.Height;
end;

procedure TFPImageCanvas.SetWidth (AValue : integer);
begin
  FImage.Width := AValue;
end;

function  TFPImageCanvas.GetWidth : integer;
begin
  result := FImage.Width;
end;



{TFPCustomPen implementation}
{ TFPCustomPen }

procedure TFPCustomPen.SetMode (AValue : TFPPenMode);
begin
  FMode := AValue;
end;

procedure TFPCustomPen.SetWidth (AValue : Integer);
begin
  if AValue < 1 then
    FWidth := 1
  else
    FWidth := AValue;
end;

procedure TFPCustomPen.SetStyle (AValue : TFPPenStyle);
begin
  FStyle := AValue;
end;

procedure TFPCustomPen.SetPattern (AValue : longword);
begin
  FPattern := AValue;
end;

procedure TFPCustomPen.DoCopyProps (From:TFPCanvasHelper);
begin
  with From as TFPCustomPen do
    begin
    self.Style := Style;
    self.Width := Width;
    self.Mode := Mode;
    self.pattern := pattern;
    end;
  inherited;
end;

function TFPCustomPen.CopyPen : TFPCustomPen;
begin
  result := TFPCustomPen(self.ClassType.Create);
  result.DoCopyProps (self);
end;

{   TPixelCanvas class.}

const
  DefaultHashWidth = 15;

procedure NotImplemented;
begin
  raise PixelCanvasException.Create(sErrNotAvailable);
end;

constructor TFPPixelCanvas.Create;
begin
  inherited;
  FHashWidth := DefaultHashWidth;
end;

function TFPPixelCanvas.DoCreateDefaultFont : TFPCustomFont;
begin
  result := TFPEmptyFont.Create;
  with result do
    begin
    Size := 10;
    FPColor := colBlack;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultPen : TFPCustomPen;
begin
  result := TFPEmptyPen.Create;
  with result do
    begin
    FPColor := colBlack;
    width := 1;
    pattern := 0;
    Style := psSolid;
    Mode := pmCopy;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultBrush : TFPCustomBrush;
begin
  result := TFPEmptyBrush.Create;
  with result do
    begin
    Style := bsClear;
    end;
end;

procedure TFPPixelCanvas.DoTextOut (x,y:integer;text:string);
begin
  NotImplemented;
end;

procedure TFPPixelCanvas.DoGetTextSize (text:string; var w,h:integer);
begin
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextHeight (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextWidth (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

procedure TFPPixelCanvas.DoRectangle (const Bounds:TRect);
var pattern : longword;

  procedure CheckLine (x1,y1, x2,y2 : integer);
  begin
    if clipping then
      CheckLineClipping (ClipRect, x1,y1, x2,y2);
    if x1 >= 0 then
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor)
  end;

  procedure CheckPLine (x1,y1, x2,y2 : integer);
  begin
    if clipping then
      CheckLineClipping (ClipRect, x1,y1, x2,y2);
    if x1 >= 0 then
      DrawPatternLine (self, x1,y1, x2,y2, pattern, Pen.FPColor)
  end;

var b : TRect;
    r : integer;

begin
  b := bounds;
  if pen.style = psSolid then
    for r := 1 to pen.width do
      begin
      with b do
        begin
        CheckLine (left,top,left,bottom);
        CheckLine (left,bottom,right,bottom);
        CheckLine (right,bottom,right,top);
        CheckLine (right,top,left,top);
        end;
      DecRect (b);
      end
  else if pen.style <> psClear then
    begin
    if pen.style = psPattern then
      pattern := Pen.pattern
    else
      pattern := PenPatterns[pen.style];
    with b do
      begin
      CheckPLine (left,top,left,bottom);
      CheckPLine (left,bottom,right,bottom);
      CheckPLine (right,bottom,right,top);
      CheckPLine (right,top,left,top);
      end;
    end;
end;

procedure TFPPixelCanvas.DoRectangleFill (const Bounds:TRect);
var b : TRect;
begin
  b := Bounds;
  SortRect (b);
  if clipping then
    CheckRectClipping (ClipRect, B);
  with b do
    case Brush.style of
      bsSolid : FillRectangleColor (self, left,top, right,bottom);
      bsPattern : FillRectanglePattern (self, left,top, right,bottom, brush.pattern);
      bsImage :
        if assigned (brush.image) then
          if FRelativeBI then
            FillRectangleImageRel (self, left,top, right,bottom, brush.image)
          else
            FillRectangleImage (self, left,top, right,bottom, brush.image)
        else
          raise PixelCanvasException.Create (sErrNoImage);
      bsBDiagonal : FillRectangleHashDiagonal (self, b, FHashWidth);
      bsFDiagonal : FillRectangleHashBackDiagonal (self, b, FHashWidth);
      bsCross :
        begin
        FillRectangleHashHorizontal (self, b, FHashWidth);
        FillRectangleHashVertical (self, b, FHashWidth);
        end;
      bsDiagCross :
        begin
        FillRectangleHashDiagonal (self, b, FHashWidth);
        FillRectangleHashBackDiagonal (self, b, FHashWidth);
        end;
      bsHorizontal : FillRectangleHashHorizontal (self, b, FHashWidth);
      bsVertical : FillRectangleHashVertical (self, b, FHashWidth);
    end;
end;

procedure TFPPixelCanvas.DoEllipseFill (const Bounds:TRect);
begin
  case Brush.style of
    bsSolid : FillEllipseColor (self, Bounds, Brush.FPColor);
    bsPattern : FillEllipsePattern (self, Bounds, brush.pattern, Brush.FPColor);
    bsImage :
      if assigned (brush.image) then
        if FRelativeBI then
          FillEllipseImageRel (self, Bounds, brush.image)
        else
          FillEllipseImage (self, Bounds, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillEllipseHashDiagonal (self, Bounds, FHashWidth, Brush.FPColor);
    bsFDiagonal : FillEllipseHashBackDiagonal (self, Bounds, FHashWidth, Brush.FPColor);
    bsCross : FillEllipseHashCross (self, Bounds, FHashWidth, Brush.FPColor);
    bsDiagCross : FillEllipseHashDiagCross (self, Bounds, FHashWidth, Brush.FPColor);
    bsHorizontal : FillEllipseHashHorizontal (self, Bounds, FHashWidth, Brush.FPColor);
    bsVertical : FillEllipseHashVertical (self, Bounds, FHashWidth, Brush.FPColor);
  end;
end;

procedure TFPPixelCanvas.DoEllipse (const Bounds:TRect);
begin
  with pen do
    case style of
      psSolid :
        if pen.width > 1 then
          DrawSolidEllipse (self, Bounds, width, FPColor)
        else
          DrawSolidEllipse (self, Bounds, FPColor);
      psPattern:
        DrawPatternEllipse (self, Bounds, pattern, FPColor);
      psDash, psDot, psDashDot, psDashDotDot :
        DrawPatternEllipse (self, Bounds, PenPatterns[Style], FPColor);
    end;
end;

procedure TFPPixelCanvas.DoPolygonFill (const points:array of TPoint);
begin  //TODO: how to find a point inside the polygon ?
end;

procedure TFPPixelCanvas.DoFloodFill (x,y:integer);
begin
  case Brush.style of
    bsSolid : FillFloodColor (self, x,y);
    bsPattern : FillFloodPattern (self, x,y, brush.pattern);
    bsImage :
      if assigned (brush.image) then
        if FRelativeBI then
          FillFloodImageRel (self, x,y, brush.image)
        else
          FillFloodImage (self, x,y, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillFloodHashDiagonal (self, x,y, FHashWidth);
    bsFDiagonal : FillFloodHashBackDiagonal (self, x,y, FHashWidth);
    bsCross : FillFloodHashCross (self, x,y, FHashWidth);
    bsDiagCross : FillFloodHashDiagCross (self, x,y, FHashWidth);
    bsHorizontal : FillFloodHashHorizontal (self, x,y, FHashWidth);
    bsVertical : FillFloodHashVertical (self, x,y, FHashWidth);
  end;
end;

procedure TFPPixelCanvas.DoPolygon (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
  DoLine (p.x,p.y, points[i].x,points[i].y);
end;

procedure TFPPixelCanvas.DoPolyline (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
end;

procedure TFPPixelCanvas.DoLine (x1,y1,x2,y2:integer);

  procedure DrawOneLine (xx1,yy1, xx2,yy2:integer);
  begin
    if Clipping then
      CheckLineClipping (ClipRect, xx1,yy1, xx2,yy2);
    DrawSolidLine (self, xx1,yy1, xx2,yy2, Pen.FPColor);
  end;

  procedure SolidThickLine;
  var w1, w2, r : integer;
      MoreHor : boolean;
  begin
    // determine lines above and under
    w1 := pen.width div 2;
    w2 := w1;
    if w1+w2 = pen.width then
      dec (w1);
    // determine slanting
    MoreHor := (abs(x2-x1) < abs(y2-y1));
    if MoreHor then
      begin  // add lines left/right
      for r := 1 to w1 do
        DrawOneLine (x1-r,y1, x2-r,y2);
      for r := 1 to w2 do
        DrawOneLine (x1+r,y1, x2+r,y2);
      end
    else
      begin  // add lines above/under
      for r := 1 to w1 do
        DrawOneLine (x1,y1-r, x2,y2-r);
      for r := 1 to w2 do
        DrawOneLine (x1,y1+r, x2,y2+r);
      end;
  end;

begin
  if Clipping then
    CheckLineClipping (ClipRect, x1,y1, x2,y2);
  case Pen.style of
    psSolid :
      begin
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor);
      if pen.width > 1 then
        SolidThickLine;
      end;
    psPattern:
      DrawPatternLine (self, x1,y1, x2,y2, pen.pattern);
      // Patterned lines have width always at 1
    psDash, psDot, psDashDot, psDashDotDot :
      DrawPatternLine (self, x1,y1, x2,y2, PenPatterns[Pen.Style]);
  end;
end;


{ Clipping support.}

procedure SortRect (var rect : TRect);
begin
  with rect do
    SortRect (left,top, right,bottom);
end;

procedure SortRect (var left,top, right,bottom : integer);
var r : integer;
begin
  if left > right then
    begin
    r := left;
    left := right;
    right := r;
    end;
  if top > bottom then
    begin
    r := top;
    top := bottom;
    bottom := r;
    end;
end;

function PointInside (const x,y:integer; bounds:TRect) : boolean;
begin
  SortRect (bounds);
  with Bounds do
    result := (x >= left) and (x <= right) and
              (y >= bottom) and (y <= top);
end;

procedure CheckRectClipping (ClipRect:TRect; var Rect:Trect);
begin
  with ClipRect do
    CheckRectClipping (ClipRect, left,top,right,bottom);
end;

procedure CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
  procedure ClearRect;
  begin
    x1 := -1;
    x2 := -1;
    y1 := -1;
    y2 := -1;
  end;
begin
  SortRect (ClipRect);
  SortRect (x1,y1, x2,y2);
  with ClipRect do
    begin
    if ( x1 < Left ) then // left side needs to be clipped
      x1 := left;
    if ( x2 > right ) then // right side needs to be clipped
      x2 := right;
    if ( y1 < top ) then // top side needs to be clipped
      y1 := top;
    if ( y2 > bottom ) then // bottom side needs to be clipped
      y2 := bottom;
    if (x1 > x2) or (y1 < y2) then
      ClearRect;
    end;
end;

procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
var a,b : single;
    Calculated : boolean;
    xdiff,n : integer;
  procedure CalcLine;
    begin
    if not Calculated then
      begin
      xdiff := (x1-x2);
      a := (y1-y2) / xdiff;
      b := (x1*y2 - x2*y1) / xdiff;
      Calculated := true;
      end;
    end;
  procedure ClearLine;
    begin
    x1 := -1;
    y1 := -1;
    x2 := -1;
    y2 := -1;
    end;
begin
  Calculated := false;
  SortRect (ClipRect);
  xdiff := (x1-x2);
  with ClipRect do
    if xdiff = 0 then
      begin  // vertical line
      if y1 > bottom then
        y1 := bottom
      else if y1 < top then
        y1 := top;
      if y2 > bottom then
        y2 := bottom
      else if y2 < top then
        y2 := top;
      end
    else if (y1-y2) = 0 then
      begin  // horizontal line
      if x1 < left then
        x1 := left
      else if x1 > right then
        x1 := right;
      if x2 < left then
        x2 := left
      else if x2 > right then
        x2 := right;
      end
    else
      if ( (y1 < top) and (y2 < top) ) or
         ( (y1 > bottom) and (y2 > bottom) ) or
         ( (x1 > right) and (x2 > right) ) or
         ( (x1 < left) and (x2 < left) ) then
        ClearLine // completely outside ClipRect
      else
        begin
        if (y1 < top) or (y2 < top) then
          begin
          CalcLine;
          n := round ((top - b) / a);
          if (n >= left) and (n <= right) then
            if (y1 < top) then
              begin
              x1 := n;
              y1 := top;
              end
            else
              begin
              x2 := n;
              y2 := top;
              end;
          end;
        if (y1 > bottom) or (y2 > bottom) then
          begin
          CalcLine;
          n := round ((bottom - b) / a);
          if (n >= left) and (n <= right) then
            if (y1 > bottom) then
              begin
              x1 := n;
              y1 := bottom;
              end
            else
              begin
              x2 := n;
              y2 := bottom;
              end;
          end;
        if (x1 < left) or (x2 < left) then
          begin
          CalcLine;
          n := round ((left * a) + b);
          if (n <= bottom) and (n >= top) then
            if (x1 < left) then
              begin
              x1 := left;
              y1 := n;
              end
            else
              begin
              x2 := left;
              y2 := n;
              end;
          end;
        if (x1 > right) or (x2 > right) then
          begin
          CalcLine;
          n := round ((right * a) + b);
          if (n <= bottom) and (n >= top) then
            if (x1 > right) then
              begin
              x1 := right;
              y1 := n;
              end
            else
              begin
              x2 := right;
              y2 := n;
              end;
          end;
        end;
end;

{ TFPCanvasHelper }

constructor TFPCanvasHelper.Create;
begin
  inherited create;
  FCanvas := nil;
  FFixedCanvas := false;
  FAllocated := false;
end;

destructor TFPCanvasHelper.destroy;
begin
  if Allocated then
    DeAllocateResources;
  inherited;
end;

procedure TFPCanvasHelper.SetFixedCanvas (AValue : boolean);
begin
  FFixedCanvas := AValue;
end;

procedure TFPCanvasHelper.NotifyCanvas;
// called to unbind from canvas
begin
  if FCanvas<>nil then
    FCanvas.CheckHelper (self);
end;

procedure TFPCanvasHelper.CheckAllocated (ValueNeeded:boolean);

  procedure RaiseErrAllocation;
  begin
    Raise TFPFontException.CreateFmt (ErrAllocation,
                                      [EFont, ErrAlloc[ValueNeeded]]);
  end;

begin
  if (Allocated <> ValueNeeded) then
    RaiseErrAllocation;
end;

procedure TFPCanvasHelper.SetFPColor(const AValue:TFPColor);
begin
  FFPColor := AValue;
end;

procedure TFPCanvasHelper.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TFPCanvasHelper.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFPCanvasHelper.Lock;
begin

end;

procedure TFPCanvasHelper.UnLock;
begin

end;

procedure TFPCanvasHelper.SetFlags (index:integer; AValue:boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl index)
  else
    FFlags := FFlags and not (1 shl index);
end;

function TFPCanvasHelper.GetFlags (index:integer) : boolean;
begin
  result := (FFlags and (1 shl index)) <> 0;
end;

function TFPCanvasHelper.GetAllocated : boolean;
begin
  if FFixedCanvas then
    result := assigned(FCanvas)
  else
    result := FAllocated;
end;

procedure TFPCanvasHelper.AllocateResources (ACanvas : TFPCustomCanvas;
  CanDelay: boolean);
begin
  if FFixedCanvas and FAllocated then
    DeallocateResources;
  FCanvas := ACanvas;
  if DelayAllocate and CanDelay then exit;
  try
    DoAllocateResources;
    FAllocated := True;
  except
    FCanvas := nil;
    FAllocated := False;
  end;
end;

procedure TFPCanvasHelper.DeallocateResources;
begin
  if FAllocated then
    try
      DoDeallocateResources;
    finally
      FAllocated := false;
      NotifyCanvas;
      FCanvas := nil;
    end;
end;

procedure TFPCanvasHelper.DoCopyProps (From:TFPCanvasHelper);
begin
  FPColor := from.FPColor;
end;

procedure TFPCanvasHelper.DoAllocateResources;
begin
end;

procedure TFPCanvasHelper.DoDeallocateResources;
begin
end;


procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  FillRectangleColor (Canv, x1,y1, x2,y2, Canv.Brush.FPColor);
end;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var x,y : integer;
begin
  SortRect (x1,y1, x2,y2);
  with Canv do
    begin
    for x := x1 to x2 do
      for y := y1 to y2 do
        colors[x,y] := color;
    end;
end;



procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  DrawSolidLine (Canv, x1,y1, x2,y2, Canv.Pen.FPColor);
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  with canv.pen do
    case mode of
      pmMerge : PutPixelProc := @PutPixelAnd;
      pmMask : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;


procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
begin
  DrawPatternLine (Canv, x1,y1, x2,y2, pattern, Canv.Pen.FPColor);
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
// Is copy of DrawSolidLine with paterns added. Not the same procedure for faster solid lines
var LinePoints : TLinePoints;
    PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        if LinePoints[x mod PatternBitCount] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        if LinePoints[y mod PatternBitCount] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      if LinePoints[r mod PatternBitCount] then
        PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  PatternToPoints (pattern, @LinePoints);
  with canv.pen do
    case mode of
      pmMask : PutPixelProc := @PutPixelAnd;
      pmMerge : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashHorizontal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var y : integer;
begin
  with rect do
    begin
    y := Width + top;
    while y <= bottom do
      begin
      DrawSolidLine (Canv, left,y, right,y, c);
      inc (y,Width);
      end
    end;
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashVertical (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var x : integer;
begin
  with rect do
    begin
    x := Width + left;
    while x <= right do
      begin
      DrawSolidLine (Canv, x,top, x,bottom, c);
      inc (x, Width);
      end;
    end;
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashDiagonal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start + current - max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := top + Width;
    rx := left + Width;
    while (rx < right) and (ry < bottom) do
      begin
      DrawSolidLine (Canv, left,ry, rx,top, c);
      inc (rx, Width);
      inc (ry, Width);
      end;
    // check which turn need to be taken: left-bottom, right-top, or both
    if (rx >= right) then
      begin
      if (ry >= bottom) then
        begin // Both corners reached
        r := CheckCorner (rx, right, top);
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, top);
        while (ry < bottom) do
          begin
          DrawSolidLine (Canv, left,ry, right,r, c);
          inc (r, Width);
          inc (ry, Width);
          end;
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      end
    else
      if (ry >= bottom) then
        begin  // fill horizontal
        r := checkCorner (ry, bottom, left);
        while (rx <= right) do
          begin
          DrawSolidLine (Canv, r,bottom, rx,top, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, top);
        rx := r;
        end;
    while (rx < right) do  // fill lower right corner
      begin
      DrawSolidLine (Canv, rx,bottom, right,ry, c);
      inc (rx, Width);
      inc (ry, Width);
      end;
    end;
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashBackDiagonal (Canv, rect, width, Canv.Brush.FPColor);
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
  function CheckInversCorner (Current, min, start : integer) : integer;
  begin
    if Current < min then
      result := Start - current + min
    else
      result := Start;
  end;
  function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start - current + max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := bottom - Width;
    rx := left + Width;
    while (rx < right) and (ry > top) do
      begin
      DrawSolidLine (Canv, left,ry, rx,bottom, c);
      inc (rx, Width);
      dec (ry, Width);
      end;
    // check which turn need to be taken: left-top, right-bottom, or both
    if (rx >= right) then
      begin
      if (ry <= top) then
        begin // Both corners reached
        r := CheckCorner (rx, right, bottom);
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, bottom);
        while (ry > top) do
          begin
          DrawSolidLine (Canv, left,ry, right,r, c);
          dec (r, Width);
          dec (ry, Width);
          end;
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      end
    else
      if (ry <= top) then
        begin  // fill horizontal
        r := checkInversCorner (ry, top, left);
        while (rx < right) do
          begin
          DrawSolidLine (Canv, r,top, rx,bottom, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, bottom);
        rx := r;
        end;
    while (rx < right) do  // fill upper right corner
      begin
      DrawSolidLine (Canv, rx,top, right,ry, c);
      inc (rx, Width);
      dec (ry, Width);
      end;
    end;
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
begin
  FillRectanglePattern (Canv, x1,y1, x2,y2, pattern, Canv.Brush.FPColor);
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
var r : integer;
begin
  for r := y1 to y2 do
    DrawPatternLine (Canv, x1,r, x2,r, pattern[r mod PatternBitCount], color);
end;

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.colors[x,y] := colors[x mod width, y mod height];
end;

procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.colors[x,y] := colors[(x-x1) mod width, (y-y1) mod height];
end;


procedure CheckFloodFillColor (x,top,bottom,Direction:integer; data:PFloodFillData);

  procedure CheckRange;
  var r,t,b : integer;
  begin
    t := top;
    b := top -1;
    for r := top to bottom do
      with data^ do
        begin
        if canv.colors[x,r] = ReplColor then
          begin
          b := r;
          SetColor(Canv,x,r,ExtraData);
          end
        else
          begin
          if t < r then
            CheckFloodFillColor (x+Direction, t, r-1, Direction, data);
          t := r + 1;
          end;
        end;
    if t <= b then
      CheckFloodFillColor (x+Direction, t, b, Direction, data);
  end;

procedure CheckFloodFill (x,top,bottom,Direction:integer; data:PFloodFillData);
var beforetop, ontop, chain, myrec : PDoneRec;
    doneindex : integer;

  procedure CheckRange;
  var r,t,b : integer;
      n : PDoneRec;
  begin
    ontop := nil;
    beforetop := nil;
    n := chain;
    while (n <> nil) and (n^.min <= top) do
      begin
      beforetop := ontop;
      ontop := n;
      n := n^.next;
      end;
    if assigned(ontop) and (ontop^.max < top) then
      begin
      beforetop := ontop;
      ontop := nil;
      end;
    // ontop is: nil OR rec before top OR rec containing top
    if assigned(ontop) then
      begin
      t := ontop^.max + 1;
      myrec := ontop;
      end
    else
      begin
      t := top;
      new(myrec);
      myrec^.x := x;
      myrec^.min := top;
      myrec^.max := top;
      myrec^.Next := n;
      if assigned(beforetop) then
        beforetop^.next := myrec
      else
        begin
        with data^.DoneList do
          if DoneIndex < Count then
            Items[DoneIndex] := myrec
          else
            Add (myrec);
        chain := myrec;
        end;
      end;
    ontop := myrec;
    // ontop is rec containing the top
    b := t-1;
    r := t;
    while (r <= bottom) do
      begin
      with data^ do
        begin
        if canv.colors[x,r] = ReplColor then
          begin
          b := r;
          SetColor(Canv,x,r,ExtraData);
          end
        else
          begin
          if t < r then
            begin
            myrec^.max := r;
            CheckFloodFill (x+Direction, t, r-1, Direction, data);
            end;
          t := r + 1;
          end;
        inc (r);
        end;
      if assigned(n) and (r >= n^.min) then
        begin
        if t < r then
          begin
          myrec^.max := n^.min-1;
          CheckFloodFill (x+Direction, t, r-1, Direction, data);
          end;
        while assigned(n) and (r >= n^.min) do
          begin
          myrec := n;
          r := myrec^.max + 1;
          n := n^.next;
          end;
        t := r;
        end;
      end;
    myrec^.max := r - 1;
    if t <= b then
      CheckFloodFill (x+Direction, t, b, Direction, data);
  end;

  procedure CheckAboveRange (highest:integer);
  var t,b : integer;
  begin
    with data^ do
      begin
      t := top - 1;
      while (t >= highest) and (Canv.colors[x,t]=ReplColor) do
        begin
        SetColor(Canv, x,t, ExtraData);
        dec (t);
        end;
      t := t + 1;
      b := top - 1;
      if t <= b then
        begin
        ontop^.min := t - 1;
        CheckFloodFill (x-1, t, b, -1, data);
        CheckFloodFill (x+1, t, b, 1, data);
        end;
      end;
  end;

  procedure CheckBelowRange (lowest : integer);
  var t,b : integer;
  begin
    with data^ do
      begin
      b := bottom + 1;
      t := b;
      while (b <= lowest) and (Canv.colors[x,b]=ReplColor) do
        begin
        SetColor (Canv,x,b,ExtraData);
        inc (b);
        end;
      b := b - 1;
      if t <= b then
        begin
        myrec^.max := b+1;
        CheckFloodFill (x-1, t, b, -1, data);
        CheckFloodFill (x+1, t, b, 1, data);
        end;
      end;
  end;

var DoAbove, DoBelow : boolean;
    m : integer;
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
  if FindDoneIndex (data, x, DoneIndex) then
    begin
    chain := PDoneRec(data^.DoneList[DoneIndex]);
    myrec := chain;
    while assigned(myrec) do
      with myrec^ do
        myrec := next;
    end
  else
    chain := nil;
  CheckRange;
  // ontop: rec containing top
  // myrec: rec containing bottom
  if DoAbove and (ontop^.min = top) then
    begin
    if assigned (beforetop) then
      m := beforetop^.max + 1
    else
      m := 0;
    CheckAboveRange (m);
    end;
  if DoBelow and (myrec^.max = bottom) then
    begin
    if assigned (myrec^.next) then
      m := myrec^.next^.min - 1
    else
      m := data^.Canv.Height - 1;
    CheckBelowRange (m);
    end;
end;

procedure SetFloodColor (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
begin
  Canv.colors[x,y] := PFPColor(data)^;
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
var d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodColor;
  d.ExtraData := @color;
  CheckFloodFillColor (x, y, y, 1, @d);
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
begin
  FillFloodColor (Canv, x, y, Canv.Brush.FPColor);
end;

type
  TBoolPlane = array[0..PatternBitCount-1] of TLinePoints;
  TFloodPatternRec = record
    plane : TBoolPlane;
    color : TFPColor;
  end;
  PFloodPatternRec = ^TFloodPatternRec;

procedure SetFloodPattern (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var p : PFloodPatternRec;
begin
  p := PFloodPatternRec(data);
  if p^.plane[x mod PatternBitCount, y mod PatternBitCount] then
    Canv.colors[x,y] := p^.color;
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
var rec : TFloodPatternRec;
    d : TFloodFillData;

  procedure FillPattern;
  var r : integer;
  begin
    for r := 0 to PatternBitCount-1 do
      PatternToPoints (pattern[r], @rec.plane[r]);
  end;

begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodPattern;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  try
    FillPattern;
    rec.color := Color;
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
begin
  FillFloodPattern (Canv, x, y, pattern, Canv.Brush.FPColor);
end;

type
  TFloodHashRec = record
    color : TFPColor;
    width : integer;
  end;
  PFloodHashRec = ^TFloodHashRec;

procedure SetFloodHashHor(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (y mod r^.width) = 0 then
    Canv.colors[x,y] := r^.color;
end;

procedure SetFloodHashVer(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (x mod r^.width) = 0 then
    Canv.colors[x,y] := r^.color;
end;

procedure SetFloodHashDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) + (y mod w)) = (w - 1) then
    Canv.colors[x,y] := r^.color;
end;

procedure SetFloodHashBDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if (x mod w) = (y mod w) then
    Canv.colors[x,y] := r^.color;
end;

procedure SetFloodHashCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) = 0) or ((y mod w) = 0) then
    Canv.colors[x,y] := r^.color;
end;

procedure SetFloodHashDiagCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : 0..PatternBitCount-1;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ( (x mod w) = (y mod w) ) or
     ( ((x mod w) + (y mod w)) = (w - 1) ) then
    Canv.colors[x,y] := r^.color;
end;

procedure FillFloodHash (Canv:TFPCustomCanvas; x,y:integer; width:integer; SetHashColor:TFuncSetColor; const c:TFPColor);
var rec : TFloodHashRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := SetHashColor;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  rec.color := c;
  rec.width := Width;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashHor, c);
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashHorizontal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashVer, c);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashVertical (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiag, c);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashBDiag, c);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashBackDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;


procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiagCross, c);
end;

procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashCross, c);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

type
  TFloodImageRec = record
    xo,yo : integer;
    image : TFPCustomImage;
  end;
  PFloodImageRec = ^TFloodImageRec;

procedure SetFloodImage (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
begin
  r := PFloodImageRec(data);
  with r^.image do
    Canv.colors[x,y] := colors[x mod width, y mod height];
end;

procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodImage;
  d.ExtraData := @rec;
  d.DoneList := Tlist.Create;
  rec.image := image;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

procedure SetFloodImageRel (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
    xi, yi : integer;
begin
  r := PFloodImageRec(data);
  with r^, image do
    begin
    xi := (x - xo) mod width;
    if xi < 0 then
      xi := width - xi;
    yi := (y - yo) mod height;
    if yi < 0 then
      yi := height - yi;
    Canv.colors[x,y] := colors[xi,yi];
    end;
end;

procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  d.ReplColor := Canv.colors[x,y];
  d.SetColor := @SetFloodImageRel;
  d.ExtraData := @rec;
  d.DoneList := TList.Create;
  rec.image := image;
  rec.xo := x;
  rec.yo := y;
  try
    CheckFloodFill (x, y, y, 1, @d);
  finally
    FreeDoneList (d);
  end;
end;

{TPostScriptCanvas implementation}
{ ---------------------------------------------------------------------
  This code is heavily based on Tony Maro's initial TPostScriptCanvas
  implementation in the LCL, but was adapted to work with the custom
  canvas code and to work with streams instead of strings.
  ---------------------------------------------------------------------}


{ TPostScriptCanvas }

Procedure TPostScriptCanvas.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScriptCanvas.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

{ Y coords in postscript are backwards... }
function TPostScriptCanvas.TranslateY(Ycoord: Integer): Integer;
begin
  Result:=Height-Ycoord;
end;

{ Adds a fill finishing line to any path we desire to fill }
procedure TPostScriptCanvas.AddFill;
begin
  WritePs('gsave '+(Brush as TPSBrush).AsString+' fill grestore');
end;

{ Return to last moveto location }
procedure TPostScriptCanvas.ResetPos;
begin
  WritePS(inttostr(LastX)+' '+inttostr(TranslateY(LastY))+' moveto');
end;

constructor TPostScriptCanvas.Create(AStream : TStream);

begin
  inherited create;
  FStream:=AStream;
  Height := 792; // length of page in points at 72 ppi
  { // Choose a standard font in case the user doesn't
  FFontFace := 'AvantGarde-Book';
  SetFontSize(10);
    FLineSpacing := MPostScript.LineSpacing;
  end;
  FPen := TPSPen.Create;
  FPen.Width := 1;
  FPen.FPColor := 0;
  FPen.OnChange := @PenChanged;

  FBrush := TPSPen.Create;
  FBrush.Width := 1;
  FBrush.FPColor := -1;
  // don't notify us that the brush changed...
  }
end;

destructor TPostScriptCanvas.Destroy;
begin
{
  FPostScript.Free;
  FPen.Free;
  FBrush.Free;
}
  inherited Destroy;
end;

procedure TPostScriptCanvas.SetWidth (AValue : integer);

begin
  FWidth:=AValue;
end;

function  TPostScriptCanvas.GetWidth : integer;

begin
  Result:=FWidth;
end;

procedure TPostScriptCanvas.SetHeight (AValue : integer);

begin
  FHeight:=AValue;
end;

function  TPostScriptCanvas.GetHeight : integer;

begin
  Result:=FHeight;
end;


{ Move draw location }
procedure TPostScriptCanvas.DoMoveTo(X1, Y1: Integer);

var
  Y: Integer;

begin
  Y := TranslateY(Y1);
  WritePS(inttostr(X1)+' '+inttostr(Y)+' moveto');
  LastX := X1;
  LastY := Y1;
end;

{ Draw a line from current location to these coords }
procedure TPostScriptCanvas.DoLineTo(X1, Y1: Integer);

var
   Y: Integer;

begin
  Y := TranslateY(Y1);
  WritePS(inttostr(X1)+' '+inttostr(Y)+' lineto');
  LastX := X1;
  LastY := Y1;
end;

procedure TPostScriptCanvas.DoLine(X1, Y1, X2, Y2: Integer);
var
  Y12, Y22: Integer;

begin
  Y12 := TranslateY(Y1);
  Y22 := TranslateY(Y2);
  WritePS('newpath '+inttostr(X1)+' '+inttostr(Y12)+' moveto '+
          inttostr(X2)+' '+inttostr(Y22)+' lineto closepath stroke');
  // go back to last moveto position
  ResetPos;
end;

{ Draw a rectangle }

procedure TPostScriptCanvas.DoRectangleFill(const Bounds: TRect);

begin
  DrawRectangle(Bounds,true)
end;

procedure TPostScriptCanvas.DoRectangle(const Bounds: TRect);

begin
  DrawRectangle(Bounds,False);
end;

procedure TPostScriptCanvas.DrawRectangle(const Bounds: TRect; DoFill : Boolean);

var
   Y12, Y22: Integer;

begin
  Y12 := TranslateY(Bounds.Top);
  Y22 := TranslateY(Bounds.Bottom);
  WritePS('stroke newpath');
  With Bounds do
    begin
    WritePS(inttostr(Left)+' '+inttostr(Y12)+' moveto');
    WritePS(inttostr(Right)+' '+inttostr(Y12)+' lineto');
    WritePS(inttostr(Right)+' '+inttostr(Y22)+' lineto');
    WritePS(inttostr(Left)+' '+inttostr(Y22)+' lineto');
    end;
  WritePS('closepath');
  If DoFill and (Brush.Style<>bsClear) then
    AddFill;
  WritePS('stroke');
  ResetPos;
end;

{ Draw a series of lines }
procedure TPostScriptCanvas.DoPolyline(Const Points: Array of TPoint);
var
  i : Longint;
begin
  MoveTo(Points[0].X, Points[0].Y);
  For i := 1 to High(Points) do
    LineTo(Points[i].X, Points[i].Y);
  ResetPos;
end;

{ This was a pain to figure out... }

procedure TPostScriptCanvas.DoEllipse(Const Bounds : TRect);

begin
  DrawEllipse(Bounds,False);
end;

procedure TPostScriptCanvas.DoEllipseFill(Const Bounds : TRect);

begin
  DrawEllipse(Bounds,true);
end;

procedure TPostScriptCanvas.DrawEllipse(Const Bounds : TRect; DoFill : Boolean);

var
  radius: Integer;
  YRatio: Real;
  centerX, centerY: Integer;

begin
  // set radius to half the width
  With Bounds do
    begin
    radius := (Right-Left) div 2;
    if radius <1 then
      exit;
    YRatio := (Bottom - Top) / (Right-Left);
    // find center
    CenterX := (Right+Left) div 2;
    CenterY := (Top+Bottom) div 2;
    end;
  WritePS('newpath '+inttostr(CenterX)+' '+inttostr(TranslateY(CenterY))+' translate');
  // move to edge
  WritePS(inttostr(radius)+' 0 moveto');
  // now draw it
  WritePS('gsave 1 '+format('%.3f',[YRatio])+' scale');
  WritePS('0 0 '+inttostr(radius)+' 0 360 arc');
  if DoFill and (Brush.Style<>bsClear) then
    AddFill;
  // reset scale for drawing line thickness so it doesn't warp
  YRatio := 1 / YRatio;
  WritePS('1 '+format('%.2f',[YRatio])+' scale stroke grestore');
  // move origin back
  WritePS(inttostr(-CenterX)+' '+inttostr(-TranslateY(CenterY))+' translate closepath stroke');
  ResetPos;
end;

procedure TPostScriptCanvas.DoPie(x, y, AWidth, AHeight, angle1, angle2: Integer);
begin
  // set zero at center
  WritePS('newpath '+inttostr(X)+' '+inttostr(TranslateY(Y))+' translate');
  // scale it
  WritePS('gsave '+inttostr(AWidth)+' '+inttostr(Aheight)+' scale');
  //WritePS('gsave 1 1 scale');
  // draw line to edge
  WritePS('0 0 moveto');
  WritePS('0 0 1 '+inttostr(angle1)+' '+inttostr(angle2)+' arc closepath');
  if Brush.Style<>bsClear then
    AddFill;
  // reset scale so we don't change the line thickness
  // adding 0.01 to compensate for scaling error - there may be a deeper problem here...
  WritePS(format('%.6f',[(real(1) / X)+0.01])+' '+format('%.6f',[(real(1) / Y)+0.01])+' scale stroke grestore');
  // close out and return origin
  WritePS(inttostr(-X)+' '+inttostr(-TranslateY(Y))+' translate closepath stroke');
  resetpos;
end;

{ Writes text with a carriage return }
procedure TPostScriptCanvas.Writeln(AString: String);
begin
  TextOut(LastX, LastY, AString);
  LastY := LastY+Font.Size+FLineSpacing;
  MoveTo(LastX, LastY);
end;


{ Output text, restoring draw location }
procedure TPostScriptCanvas.TextOut(X, Y: Integer; const Text: String);
var
   Y1: Integer;
begin
  Y1 := TranslateY(Y);
  WritePS(inttostr(X)+' '+inttostr(Y1)+' moveto');
  WritePS('('+Text+') show');
  ResetPos; // move back to last moveto location
end;

function TPostScriptCanvas.DoCreateDefaultFont : TFPCustomFont;

begin
  Result:=TPSFont.Create;
end;


function TPostScriptCanvas.DoCreateDefaultPen : TFPCustomPen;

begin
  Result:=TPSPen.Create;
end;

function TPostScriptCanvas.DoCreateDefaultBrush : TFPCustomBrush;

begin
  Result:=TPSBrush.Create;
end;



{ TPostScript }

procedure TPostScript.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  UpdateBoundingBox;
  // filter down to the canvas height property
  if assigned(FCanvas) then
    FCanvas.Height := FHeight;
end;

procedure TPostScript.SetLineSpacing(const AValue: Integer);
begin
  if FLineSpacing=AValue then exit;
  FLineSpacing:=AValue;
  // filter down to the canvas
  if assigned(FCanvas) then FCanvas.LineSpacing := AValue;
end;

procedure TPostScript.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
    FWidth:=AValue;
  UpdateBoundingBox;
end;

{ Take our sizes and change the boundingbox line }
procedure TPostScript.UpdateBoundingBox;
begin
{

     // need to not hard-link this to line 1
     FDocument[1] := '%%BoundingBox: 0 0 '+inttostr(FWidth)+' '+inttostr(FHeight);
}
end;

{ Pattern changed so update the postscript code }
procedure TPostScript.PatternChanged(Sender: TObject);
begin
     // called anytime a pattern changes.  Update the postscript code.
     // look for and delete the current postscript code for this pattern
     // then paste the pattern back into the code before the first page
     InsertPattern(Sender As TPSPattern);
end;

{ Places a pattern definition into the bottom of the header in postscript }
procedure TPostScript.InsertPattern(APattern: TPSPattern);
var
   I, J: Integer;
   MyStrings: TStringList;
begin
{     I := 0;
     if FDocument.Count < 1 then begin
        // added pattern when no postscript exists - this shouldn't happen
        raise exception.create('Pattern inserted with no postscript existing');
        exit;
     end;

     for I := 0 to FDocument.count - 1 do begin
         if (FDocument[I] = '%%Page: 1 1') then begin
            // found it!
            // insert into just before that
            MyStrings := APattern.GetPS;
            for J := 0 to MyStrings.Count - 1 do begin
                FDocument.Insert(I-1+J, MyStrings[j]);
            end;
            exit;
         end;
     end;
}
end;

constructor TPostScript.Create(AOwner : TComponent);
begin
  inherited create(AOwner);
  // Set some defaults
  FHeight := 792; // 11 inches at 72 dpi
  FWidth := 612; // 8 1/2 inches at 72 dpi
end;

Procedure TPostScript.WritePS(const Cmd : String);
var
  ss : shortstring;
begin
  If length(Cmd)>0 then
    FStream.Write(Cmd[1],Length(Cmd));
  ss:=LineEnding;
  FStream.Write(ss[1],Length(ss));
end;

Procedure TPostScript.WritePS(Const Fmt : String; Args : Array of Const);

begin
  WritePS(Format(Fmt,Args));
end;

Procedure TPostScript.WriteDocumentHeader;

begin
  WritePS('%!PS-Adobe-3.0');
  WritePS('%%BoundingBox: 0 0 612 792');
  WritePS('%%Creator: '+Creator);
  WritePS('%%Title: '+FTitle);
  WritePS('%%Pages: (atend)');
  WritePS('%%PageOrder: Ascend');
  WriteStandardFont;
end;

Procedure TPostScript.WriteStandardFont;

begin
  // Choose a standard font in case the user doesn't
  WritePS('/AvantGarde-Book findfont');
  WritePS('10 scalefont');
  WritePS('setfont');
end;

Procedure TPostScript.FreePatterns;

Var
  i : Integer;

begin
  If Assigned(FPatterns) then
    begin
    For I:=0 to FPatterns.Count-1 do
      TObject(FPatterns[i]).Free;
    FreeAndNil(FPatterns);
    end;
end;

destructor TPostScript.Destroy;

begin
  Stream:=Nil;
  FreePatterns;
  inherited Destroy;
end;

{ add a pattern to the array }
procedure TPostScript.AddPattern(APSPattern: TPSPattern);
begin
  If Not Assigned(FPatterns) then
    FPatterns:=Tlist.Create;
  FPatterns.Add(APSPattern);
end;

{ Find a pattern object by it's name }

function TPostScript.FindPattern(AName: String): TPSPattern;

var
   I: Integer;

begin
  Result := nil;
  If Assigned(FPatterns) then
    begin
    I:=Fpatterns.Count-1;
    While (Result=Nil) and (I>=0) do
      if TPSPattern(FPatterns[I]).Name = AName then
        result := TPSPattern(FPatterns[i])
      else
        Dec(i)
   end;
end;

function TPostScript.DelPattern(AName: String): Boolean;
begin
  // can't do that yet...
  Result:=false;
end;


{ Create a new pattern and inserts it into the array for safe keeping }
function TPostScript.NewPattern(AName: String): TPSPattern;
var
   MyPattern: TPSPattern;
begin
  MyPattern := TPSPattern.Create;
  AddPattern(MyPattern);
  MyPattern.Name := AName;
  MyPattern.OnChange := @PatternChanged;
  MyPattern.OldName := '';
  // add this to the postscript now...
  InsertPattern(MyPattern);
  result := MyPattern;
end;

{ Start a new document }
procedure TPostScript.BeginDoc;

var
   I: Integer;

begin
  CheckStream;
  If FDocStarted then
    Raise Exception.Create(SErrDocumentAlreadyStarted);
  FCanvas:=TPostScriptCanvas.Create(FStream);
  FCanvas.Height:=Self.Height;
  FCanvas.Width:=Self.width;
  FreePatterns;
  WriteDocumentHeader;
  // start our first page
  FPageNumber := 1;
  WritePage;
  UpdateBoundingBox;
end;

Procedure TPostScript.WritePage;

begin
  WritePS('%%Page: '+inttostr(FPageNumber)+' '+inttostr(FPageNumber));
  WritePS('newpath');
end;

{ Copy current page into the postscript and start a new one }
procedure TPostScript.NewPage;
begin
  // dump the current page into our postscript first
  // put end page definition...
  WritePS('stroke');
  WritePS('showpage');
  FPageNumber := FPageNumber+1;
  WritePage;
end;

{ Finish off the document }
procedure TPostScript.EndDoc;
begin
  // Start printing the document after closing out the pages
  WritePS('stroke');
  WritePS('showpage');
  WritePS('%%Pages: '+inttostr(FPageNumber));
  // okay, the postscript is all ready, so dump it to the text file
  // or to the printer
  FDocStarted:=False;
  FreeAndNil(FCanvas);
end;

Function TPostScript.GetCreator : String;

begin
  If (FCreator='') then
    Result:=ClassName
  else
    Result:=FCreator;
end;


Procedure TPostScript.SetStream (Value : TStream);

begin
  if (FStream<>Value) then
    begin
    If (FStream<>Nil) and FDocStarted then
      EndDoc;
    FStream:=Value;
    FDocStarted:=False;
    end;
end;

Procedure TPostScript.CheckStream;

begin
  If Not Assigned(FStream) then
    Raise Exception.Create(SErrNoStreamAssigned);
end;

{ TPSPen }

procedure TPSPen.SetPattern(const AValue: TPSPattern);
begin
  if FPattern<>AValue then
    begin
    FPattern:=AValue;
    // NotifyCanvas;
    end;
end;


destructor TPSPen.Destroy;
begin
  // Do NOT free the pattern object from here...
  inherited Destroy;
end;


{ Return the pen definition as a postscript string }
function TPSPen.AsString: String;

begin
  Result:='';
  if FPattern <> nil then
    begin
    if FPattern.PaintType = ptColored then
      Result:='/Pattern setcolorspace '+FPattern.Name+' setcolor '
    else
      begin
      Result:='[/Pattern /DeviceRGB] setcolorspace '+inttostr(FPColor.Red)+' '+inttostr(FPColor.Green)+' '+
       inttostr(FPColor.Blue)+' '+FPattern.Name+' setcolor ';
      end;
    end
  else // no pattern do this:
    Result:=inttostr(FPColor.Red)+' '+inttostr(FPColor.Green)+' '+
           inttostr(FPColor.Blue)+' setrgbcolor ';
  Result := Result + format('%f',[Width])+' setlinewidth ';
end;

{ TPSPattern }

{ Returns the pattern definition as postscript }
function TPSPattern.GetpostScript: TStringList;

var
   I: Integer;
   S : String;

begin
  // If nothing in the canvas, error
  if FStream.Size=0 then
    raise exception.create('Empty pattern');
  FPostScript.Clear;
  With FPostScript do
    begin
    add('%% PATTERN '+FName);
    add('/'+FName+'proto 12 dict def '+FName+'proto begin');
    add('/PatternType 1 def');
    add(Format('/PaintType %d def',[ord(FPaintType)+1]));
    add(Format('/TilingType %d def',[ord(FTilingType)+1]));
    add('/BBox ['+inttostr(FBBox.Left)+' '+inttostr(FBBox.Top)+' '+inttostr(FBBox.Right)+' '+inttostr(FBBox.Bottom)+'] def');
    add('/XStep '+format('%f',[FXStep])+' def');
    add('/YStep '+format('%f',[FYstep])+' def');
    add('/PaintProc { begin');
    // insert the canvas
    SetLength(S,FStream.Size);
    FStream.Seek(0,soFromBeginning);
    FStream.Read(S[1],FStream.Size);
    Add(S);
    // add support for custom matrix later
    add('end } def end '+FName+'proto [1 0 0 1 0 0] makepattern /'+FName+' exch def');
    add('%% END PATTERN '+FName);
    end;
  Result := FPostScript;
end;

procedure TPSPattern.SetBBox(const AValue: TRect);
begin
{  if FBBox<>AValue then
    begin
    FBBox:=AValue;
    FPatternCanvas.Height := FBBox.Bottom - FBBox.Top;
//    NotifyCanvas;
    end;
}
end;

procedure TPSPattern.SetName(const AValue: String);
begin
  FOldName := FName;
  if (FName<>AValue) then
    begin
    FName:=AValue;
    // NotifyCanvas;
    end;
end;

procedure TPSPattern.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPSPattern.SetPaintType(const AValue: TPSPaintType);
begin
  if FPaintType=AValue then exit;
  FPaintType:=AValue;
  changed;
end;

procedure TPSPattern.SetTilingType(const AValue: TPSTileType);
begin
  if FTilingType=AValue then exit;
  FTilingType:=AValue;
  changed;
end;

procedure TPSPattern.SetXStep(const AValue: Real);
begin
  if FXStep=AValue then exit;
  FXStep:=AValue;
  changed;
end;

procedure TPSPattern.SetYStep(const AValue: Real);
begin
  if FYStep=AValue then exit;
  FYStep:=AValue;
  changed;
end;

constructor TPSPattern.Create;
begin
  FPostScript := TStringList.Create;
  FPaintType := ptColored;
  FTilingType := ttConstant;
  FStream:=TmemoryStream.Create;
  FPatternCanvas := TPostScriptCanvas.Create(FStream);
  FName := 'Pattern1';
end;

destructor TPSPattern.Destroy;
begin
  FPostScript.Free;
  FPatternCanvas.Free;
  FStream.Free;
  inherited Destroy;
end;

{ ---------------------------------------------------------------------
    TPSBrush
  ---------------------------------------------------------------------}


Function TPSBrush.GetAsString : String;

begin
  Result:='';
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


procedure HLine16(x, x2, y: SmallInt);
var
  xtmp: SmallInt;
  ScrOfs, HLength: Word;
  LMask, RMask: Byte;
begin
  { must we swap the values? }
  if x > x2 then
  begin
    xtmp := x2;
    x2 := x;
    x:= xtmp;
  end;
  { First convert to global coordinates }
  Inc(x, StartXViewPort);
  Inc(x2, StartXViewPort);
  Inc(y, StartYViewPort);
  if ClipPixels and LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
    StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
    exit;

  ScrOfs := y * ScrWidth + x div 8;
  HLength := x2 div 8 - x div 8;
  LMask := $ff shr (x and 7);
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
  RMask:=$ff shl (7 - (x2 and 7));
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
  if HLength=0 then
    LMask:=LMask and RMask;
  WritePortB($3ce, 0);
  if CurrentWriteMode <> NotPut Then
    WritePortB($3cf, CurrentColor)
  else
    WritePortB($3cf, not CurrentColor);
  WritePortW($3ce, $0f01);
  WritePortB($3ce, 3);
  case CurrentWriteMode of
    XORPut:
      WritePortB($3cf, 3 shl 3);
    ANDPut:
      WritePortB($3cf, 1 shl 3);
    ORPut:
      WritePortB($3cf, 2 shl 3);
    NormalPut, NotPut:
      WritePortB($3cf, 0)
    else
      WritePortB($3cf, 0)
  end;

  WritePortB($3ce, 8);
  WritePortB($3cf, LMask);
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
  VidMem^[ScrOfs] := VidMem^[ScrOfs] + 1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
  if HLength>0 then
  begin
    Dec(HLength);
    Inc(ScrOfs);
    if HLength>0 then
    begin
      WritePortW($3ce, $ff08);
      bytemove(VidMem^[ScrOfs], VidMem^[ScrOfs], HLength);
      Inc(ScrOfs, HLength);
    end else
      WritePortB($3ce, 8);
    WritePortB($3cf, RMask);
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
    VidMem^[ScrOfs] := VidMem^[ScrOfs] + 1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
  end;
end;


procedure VLine16(x,y,y2: SmallInt);
var
  ytmp: SmallInt;
  ScrOfs,i: longint;
  BitMask: byte;

begin
  { must we swap the values? }
  if y > y2 then
  begin
    ytmp := y2;
    y2 := y;
    y:= ytmp;
  end;
  { First convert to global coordinates }
  Inc(x, StartXViewPort);
  Inc(y, StartYViewPort);
  Inc(y2, StartYViewPort);
  if ClipPixels and LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
    StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
    exit;
  ScrOfs:=y*ScrWidth+x div 8;
  BitMask:=$80 shr (x and 7);
  WritePortB($3ce, 0);
  if CurrentWriteMode <> NotPut then
    WritePortB($3cf, CurrentColor)
  else
    WritePortB($3cf, not CurrentColor);
  WritePortW($3ce, $0f01);
  WritePortB($3ce, 8);
  WritePortB($3cf, BitMask);
  WritePortB($3ce, 3);
  case CurrentWriteMode of
    XORPut:
      WritePortB($3cf, 3 shl 3);
    ANDPut:
      WritePortB($3cf, 1 shl 3);
    ORPut:
      WritePortB($3cf, 2 shl 3);
    NormalPut, NotPut:
      WritePortB($3cf, 0)
    else
      WritePortB($3cf, 0)
  end;
  for i:=y to y2 do
  begin
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
    VidMem^[ScrOfs]:=VidMem^[ScrOfs]+1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
    Inc(ScrOfs, ScrWidth);
  end;
end;


  procedure PatternLineDefault(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
  {********************************************************}
  { Draws a horizontal patterned line according to the     }
  { current Fill Settings.                                 }
  {********************************************************}
  { Important notes:                                       }
  {  - CurrentColor must be set correctly before entering  }
  {    this routine.                                       }
  {********************************************************}
   var
    NrIterations: smallint;
    i           : smallint;
    j           : smallint;
    TmpFillPattern : byte;
    OldWriteMode : word;
    OldCurrentColor : word;
   begin
     { convert to global coordinates ... }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y  := y + StartYViewPort;
     { if line was fully clipped then exit...}
     if LineClipped(x1,y,x2,y,StartXViewPort,StartYViewPort,
        StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
         exit;

     OldWriteMode := CurrentWriteMode;
     CurrentWriteMode := NormalPut;


     { Get the current pattern }
     TmpFillPattern := FillPatternTable
       [FillSettings.Pattern][(y and $7)+1];

     Case TmpFillPattern Of
       0:
         begin
           OldCurrentColor := CurrentColor;
           CurrentColor := CurrentBkColor;
  { hline converts the coordinates to global ones, but that has been done }
  { already here!!! Convert them back to local ones... (JM)                }
           HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
           CurrentColor := OldCurrentColor;
         end;
       $ff:
         begin
           HLine(x1-StartXViewPort,x2-StartXViewPort,y-StartYViewPort);
         end;
       else
         begin
           { number of times to go throuh the 8x8 pattern }
           NrIterations := abs(x2 - x1+8) div 8;
           For i:= 0 to NrIterations do
             Begin
               for j:=0 to 7 do
                    Begin
                            { x1 mod 8 }
                    if RevBitArray[x1 and 7] and TmpFillPattern <> 0 then
                       DirectPutpixel(x1,y)
                    else
                      begin
                            { According to the TP graph manual, we overwrite everything }
                            { which is filled up - checked against VGA and CGA drivers  }
                            { of TP.                                                    }
                            OldCurrentColor := CurrentColor;
                            CurrentColor := CurrentBkColor;
                            DirectPutPixel(x1,y);
                            CurrentColor := OldCurrentColor;
                      end;
                    Inc(x1);
                    if x1 > x2 then
                     begin
                           CurrentWriteMode := OldWriteMode;
                           exit;
                     end;
                   end;
             end;
          end;
     End;
     CurrentWriteMode := OldWriteMode;
   end;


  Procedure InternalEllipseDefault(X,Y: smallint;XRadius: word;
    YRadius:word; stAngle,EndAngle: word; pl: PatternLineProc); {$ifndef fpc}far;{$endif fpc}
   Const ConvFac = Pi/180.0;

   var
    j, Delta, DeltaEnd: graph_float;
    NumOfPixels: longint;
    TempTerm: graph_float;
    xtemp, ytemp, xp, yp, xm, ym, xnext, ynext,
      plxpyp, plxmyp, plxpym, plxmym: smallint;
    BackupColor, TmpAngle, OldLineWidth: word;
  Begin
   If LineInfo.ThickNess = ThickWidth Then
    { first draw the two outer ellipses using normwidth and no filling (JM) }
     Begin
       OldLineWidth := LineInfo.Thickness;
       LineInfo.Thickness := NormWidth;
       InternalEllipseDefault(x,y,XRadius,YRadius,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       InternalEllipseDefault(x,y,XRadius+1,YRadius+1,StAngle,EndAngle,
                              {$ifdef fpc}@{$endif fpc}DummyPatternLine);
       If (XRadius > 0) and (YRadius > 0) Then
         { draw the smallest ellipse last, since that one will use the }
         { original pl, so it could possibly draw patternlines (JM)    }
         Begin
           Dec(XRadius);
           Dec(YRadius);
         End
       Else Exit;
       { restore line thickness }
       LineInfo.Thickness := OldLineWidth;
     End;
   If xradius = 0 then inc(xradius);
   if yradius = 0 then inc(yradius);
   { check for an ellipse with negligable x and y radius }
   If (xradius <= 1) and (yradius <= 1) then
     begin
       putpixel(x,y,CurrentColor);
       ArcCall.X := X;
       ArcCall.Y := Y;
       ArcCall.XStart := X;
       ArcCall.YStart := Y;
       ArcCall.XEnd := X;
       ArcCall.YEnd := Y;
       exit;
     end;
   { check if valid angles }
   stangle := stAngle mod 361;
   EndAngle := EndAngle mod 361;
   { if impossible angles then swap them! }
   if Endangle < StAngle then
     Begin
       TmpAngle:=EndAngle;
       EndAngle:=StAngle;
       Stangle:=TmpAngle;
     end;
   { approximate the number of pixels required by using the circumference }
   { equation of an ellipse.                                              }
   { Changed this formula a it (trial and error), but the net result is that }
   { less pixels have to be calculated now                                   }
   NumOfPixels:=Round(Sqrt(3)*sqrt(sqr(XRadius)+sqr(YRadius)));
   { Calculate the angle precision required }
   Delta := 90.0 / NumOfPixels;
   { for restoring after PatternLine }
   BackupColor := CurrentColor;
   { removed from inner loop to make faster }
   { store some arccall info }
   ArcCall.X := X;
   ArcCall.Y := Y;
   TempTerm := (StAngle)*ConvFac;
   ArcCall.XStart := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YStart := round(YRadius*Sin(TempTerm+Pi)) + Y;
   TempTerm := (EndAngle)*ConvFac;
   ArcCall.XEnd := round(XRadius*Cos(TempTerm)) + X;
   ArcCall.YEnd := round(YRadius*Sin(TempTerm+Pi)) + Y;
   { Always just go over the first 90 degrees. Could be optimized a   }
   { bit if StAngle and EndAngle lie in the same quadrant, left as an }
   { exercise for the reader :) (JM)                                  }
   j := 0;
   { calculate stop position, go 1 further than 90 because otherwise }
   { 1 pixel is sometimes not drawn (JM)                             }
   DeltaEnd := 91;
   { Calculate points }
   xnext := XRadius;
   ynext := 0;
   Repeat
     xtemp := xnext;
     ytemp := ynext;
     { this is used by both sin and cos }
     TempTerm := (j+Delta)*ConvFac;
     { Calculate points }
     xnext := round(XRadius*Cos(TempTerm));
     ynext := round(YRadius*Sin(TempTerm+Pi));

     xp := x + xtemp;
     xm := x - xtemp;
     yp := y + ytemp;
     ym := y - ytemp;
     plxpyp := maxsmallint;
     plxmyp := -maxsmallint-1;
     plxpym := maxsmallint;
     plxmym := -maxsmallint-1;
     If (j >= StAngle) and (j <= EndAngle) then
       begin
         plxpyp := xp;
         PutPixel(xp,yp,CurrentColor);
       end;
     If ((180-j) >= StAngle) and ((180-j) <= EndAngle) then
       begin
         plxmyp := xm;
         PutPixel(xm,yp,CurrentColor);
       end;
     If ((j+180) >= StAngle) and ((j+180) <= EndAngle) then
       begin
         plxmym := xm;
         PutPixel(xm,ym,CurrentColor);
       end;
     If ((360-j) >= StAngle) and ((360-j) <= EndAngle) then
       begin
         plxpym := xp;
         PutPixel(xp,ym,CurrentColor);
       end;
     If (ynext <> ytemp) and
        (xp - xm >= 1) then
       begin
         CurrentColor := FillSettings.Color;
         pl(plxmyp+1,plxpyp-1,yp);
         pl(plxmym+1,plxpym-1,ym);
         CurrentColor := BackupColor;
       end;
     j:=j+Delta;
   Until j > (DeltaEnd);
  end;


   procedure SetFillStyle(Pattern : word; Color: word);

   begin
     { on invalid input, the current fill setting will be }
     { unchanged.                                         }
     if (Pattern > UserFill) or (Color > GetMaxColor) then
      begin

           exit;
      end;
     FillSettings.Color := Color;
     FillSettings.Pattern := Pattern;
   end;


  procedure SetFillPattern(Pattern: FillPatternType; Color: word);
  {********************************************************}
  { Changes the Current FillPattern to a user defined      }
  { pattern and changes also the current fill color.       }
  { The FillPattern is saved in the FillPattern array so   }
  { it can still be used with SetFillStyle(UserFill,Color) }
  {********************************************************}
   var
    i: smallint;

   begin
     if Color > GetMaxColor then
       begin

          
            exit;
       end;

     FillSettings.Color := Color;
     FillSettings.Pattern := UserFill;

     { Save the pattern in the buffer }
     For i:=1 to 8 do
       FillPatternTable[UserFill][i] := Pattern[i];

   end;


PROCEDURE _SuspendViewPort;
{buffer viewport and render it invalid}
BEGIN
  GetDrawingMode(DrawModeBuffer);
  GetViewSettings(ViewPortBuffer);
{select the area specific to text mode!}
  UnClipGraphics;
END; {_SuspendViewPort}


PROCEDURE _ContinueViewPort;
{continue with the buffer viewport (not checked!)}
BEGIN
  SetViewSettings(ViewPortBuffer);
  SetDrawingMode(DrawModeBuffer);
END; {_ContinueViewPort}


PROCEDURE SetPixelMode(Color,BitBlit:BYTE);
{set all the things needed for pixel drawing, optimized!}
BEGIN
  DrawMode    := PixelDrawing;
  _SetColorAndBitBlit(Color,BitBlit);
END; {SetPixelMode}


PROCEDURE _SetColorAndBitBlit(Color,BitBlit:BYTE); ASSEMBLER;
{only set the color and blit operation}
ASM
  MOV AL, Color         {get color}
  MOV DrawFGColor, AL   {into drawfgcolor}
  MOV AL, BitBlit       {get bitblit}
  MOV DrawBitBlit, AL   {into drawbitblit}
  MOV BL, AL            {make copy in BL}
  AND BL, $0B           {keep 3 bits}
  MOV DrawLogic, BL     {store in drawlogic}
  AND AL, $04           {extract inverse bit}
  SHR AL, 2             {move it down}
  MOV DrawInverse, AL   {store in drawinverse}
END; {_SetColorAndBlit}

begin

   ArcCall.X := 0;
    ArcCall.Y := 0;
    ArcCall.XStart := 0;
    ArcCall.YStart := 0;
    ArcCall.XEnd := 0;
    ArcCall.YEnd := 0;
 
procedure GetArcCoords(out arccoords:ArcCoordsType);
begin
  arccoords:=lastArcCoords;
end;


procedure GetArcCoords(out arccoords:ArcCoordsType);
procedure GetLineSettings(out lineinfo:LineSettingsType);
procedure SetLineStyle(linestyle,pattern,thickness:word);

  PFloodLine = ^TFloodLine;
  TFloodLine = record
    next: PFloodLine;
    x1 : smallint;
    x2 : smallint;
    y  : smallint;
  end;

	FillSettingsType = record
		pattern: word;
        	color  : longword;
        end;
        FillPatternType = array [1..8] of byte;

  ArcCoordsType = record
             x,y : smallint;
             xstart,ystart : smallint;
             xend,yend : smallint;
       end;

       FillSettingsType = record
             pattern : word;
             color : word;
       end;

       FillPatternType = array[1..8] of byte;

       LineSettingsType = record
             linestyle : word;
             pattern : word;
             thickness : word;
       end;

       { Line styles for GetLineStyle/SetLineStyle }
       SolidLn = 0;
       DottedLn = 1;
       CenterLn = 2;
       DashedLn = 3;
       UserBitLn = 4;

       NormWidth = 1;
       ThickWidth = 3;

       HorizDir = 0;
       VertDir = 1;

    LineSettingsType = record
    	linestyle,pattern,thickness: word;
    end;
    ArcCoordsType = record
    	x,y,xstart,ystart,xend,yend: smallint;
    end;



  const
       fillpatternTable : array[0..12] of FillPatternType = (
           ($00,$00,$00,$00,$00,$00,$00,$00),     { background color  }
           ($ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff),     { foreground color  }
           ($ff,$ff,$00,$00,$ff,$ff,$00,$00),     { horizontal lines  }
           ($01,$02,$04,$08,$10,$20,$40,$80),     { slashes           }
           ($07,$0e,$1c,$38,$70,$e0,$c1,$83),     { thick slashes     }
           ($07,$83,$c1,$e0,$70,$38,$1c,$0e),     { thick backslashes }
           ($5a,$2d,$96,$4b,$a5,$d2,$69,$b4),     { backslashes       }
           ($ff,$88,$88,$88,$ff,$88,$88,$88),     { small boxes       }
           ($18,$24,$42,$81,$81,$42,$24,$18),     { rhombus           }
           ($cc,$33,$cc,$33,$cc,$33,$cc,$33),     { wall pattern      }
           ($80,$00,$08,$00,$80,$00,$08,$00),     { wide points       }
           ($88,$00,$22,$00,$88,$00,$22,$00),     { dense points      }
           (0,0,0,0,0,0,0,0)                      { user defined line style }
          );

defAspectRatio              : boolean;
    grWindow,grMemory,grTemp    : HDC;
    grPalette,old_Palette       : HPALETTE;
    grPen,old_Pen               : HPEN;
    grBrush,old_Brush           : HBRUSH;
    grFont,old_Font             : HFONT;
    grPattern,old_Bitmap        : HBITMAP;
    
 
    lineSettings                : LineSettingsType;
    fillSettings                : FillSettingsType;
    textSettings                : TextSettingsType;
    fillPattern                 : FillPatternType;
    viewPort                    : ViewPortType;


 floodMode                   : UINT;
   

{drawing style for lines}
const      
	SolidLn = word(0);
        DottedLn = word(1);
        DashDotLn = word(2);
        DashedLn = word(3);
        DashDotDotLn = word(4);
        UserBitLn = word(5);
        NullLn = word(6);
{thick constants for lines}
        NormWidth = word(1);
        DoubleWidth = word(2);
        TripleWidth = word(3);
        QuadWidth = word(4);
        ThickWidth = TripleWidth;

	EmptyFill = word(0);
        SolidFill = word(1);
         LineFill = word(2);
          ColFill = word(3);
        HatchFill = word(4);
        SlashFill = word(5);
      BkSlashFill = word(6);
       XHatchFill = word(7);
         UserFill = word(8);
           NoFill = word(9);


end.

