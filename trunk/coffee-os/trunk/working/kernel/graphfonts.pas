procedure swap_header(var h: theader);
(*
      THeader = packed record
        Signature:  char;     { signature byte                        }
        Nr_chars:   smallint;  { number of characters in file          }
        Reserved:   byte;
        First_char: byte;     { first character in file               }
        cdefs :     smallint;  { offset to character definitions       }
        scan_flag:  byte;     { TRUE if char is scanable              }
        org_to_cap: shortint;     { Height from origin to top of capitol  }
        org_to_base:shortint;     { Height from origin to baseline        }
        org_to_dec: shortint;     { Height from origin to bot of decender }
        _reserved: array[1..4] of char;
        Unused: byte;
      end;
*)
begin
  with h do
    begin
      nr_chars := swap(nr_chars);
      cdefs := swap(cdefs);
    end;
end;


procedure swap_offsets(var t: toffsettable; start, len: longint);
(*
      TOffsetTable =array[0..MaxChars] of smallint;
*)
var
  i: longint;
begin
  for i := start to start+len-1 do
    t[i]:=Swap(t[i]);
end;
{$endif FPC_BIG_ENDIAN}

{$ifdef FPC_BIG_ENDIAN}
procedure swap_fheader(var h: tfheader);
(*
      TFHeader = packed record
         header_size: word;    {* Version 2.0 Header Format        *}
         font_name: array[1..4] of char;
         font_size: word;      {* Size in byte of file        *}
         font_major: byte;     {* Driver Version Information    *}
         font_minor: byte;
         min_major: byte;      {* BGI Revision Information         *}
         min_minor: byte;
      end;
*)
begin
  with h do
    begin
      header_size := swap(header_size);
      font_size := swap(font_size);
    end;
end;



      { Font record information }
{      PHeader = ^THeader;}
      THeader = packed record
        Signature:  char;     { signature byte                        }
        Nr_chars:   smallint;  { number of characters in file          }
        Reserved:   byte;
        First_char: byte;     { first character in file               }
        cdefs :     smallint;  { offset to character definitions       }
        scan_flag:  byte;     { TRUE if char is scanable              }
        org_to_cap: shortint;     { Height from origin to top of capitol  }
        org_to_base:shortint;     { Height from origin to baseline        }
        org_to_dec: shortint;     { Height from origin to bot of decender }
        _reserved: array[1..4] of char;
        Unused: byte;
      end;


      TOffsetTable =array[0..MaxChars] of smallint;
      TWidthTable  =array[0..MaxChars] of byte;

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

      opcodes = (_END_OF_CHAR, _DO_SCAN, _DRAW := 253, _MOVE := 254 );


    var
       fonts : array[1..maxfonts] of tfontrec;
       Strokes: TStrokes; {* Stroke Data Base           *}
{       Stroke_count: Array[0..MaxChars] of smallint;} {* Stroke Count Table *}


{***************************************************************************
                             Text output routines                          
***************************************************************************

    const
       maxfonts    = 16;    maximum possible fonts              }
       MaxChars    = 255;  { Maximum nr. of characters in a file }
       Prefix_Size = $80;  { prefix size to skip                 }
       SIGNATURE   = '+';  { Signature of CHR file               }

    type
      { Prefix header of Font file }
{      PFHeader = ^TFHeader;}
      TFHeader = packed record
         header_size: word;    {* Version 2.0 Header Format        *}
         font_name: array[1..4] of char;
         font_size: word;      {* Size in byte of file        *}
         font_major: byte;     {* Driver Version Information    *}
         font_minor: byte;
         min_major: byte;      {* BGI Revision Information         *}
         min_minor: byte;
      end;


function TFPCustomFont.GetTextHeight (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextHeight (text)
  else
    result := FCanvas.GetTextHeight (text);
end;

function TFPCustomFont.GetTextWidth (text:string) : integer;
begin
  if inheritsFrom (TFPCustomDrawFont) then
    result := TFPCustomDrawFont(self).DoGetTextWidth (text)
  else
    result := FCanvas.GetTextWidth (text);
end;


procedure TFPCustomFont.DoCopyProps (From:TFPCanvasHelper);
begin
  with from as TFPCustomFont do
    begin
    self.FName := FName;
    self.FSize := FSize;
    self.FFPColor := FFPColor;
    self.FFlags := FFlags;
    end;
end;

function TFPCustomFont.CopyFont : TFPCustomFont;
begin
  result := TFPCustomFont(self.ClassType.Create);
  result.DoCopyProps (self);
end;

procedure TFPCustomFont.GetTextSize (text:string; var w,h:integer);
begin
  if inheritsFrom (TFPCustomDrawFont) then
    TFPCustomDrawFont(self).DoGetTextSize (text,w,h)
  else
    FCanvas.DoGetTextSize (text, w,h);
end;


{   Implementation of TFPCustomFont}


procedure TFPCustomFont.SetName (AValue:string);
begin
  FName := AValue;
end;

procedure TFPCustomFont.SetSize (AValue:integer);
begin
  FSize := AValue;
end;

procedure TFreeTypeFont.DrawCharBW (x,y:integer; data:PByteArray; pitch, width, height:integer);
var rb : byte;
    rx,ry,b,l : integer;
begin
  b := 0;
  for ry := 0 to height-1 do
    begin
    l := 0;
    for rx := 0 to width-1 do
      begin
      rb := rx mod 8;
      if (data^[b+l] and bits[rb]) <> 0 then
        canvas.colors[x+rx,y+ry] := FPColor;
      if rb = 7 then
        inc (l);
      end;
    inc (b, pitch);
    end;
end;


procedure TFreeTypeFont.DrawChar (x,y:integer; data:PByteArray; pitch, width, height:integer);

  procedure Combine (canv:TFPCustomCanvas; x,y:integer; c : TFPColor; t:longword);
  var a,r,g,b:longword;
  begin
    if t = 255 then
      canv.colors[x,y] := c
    else if t <> 0 then
      begin
      with canv.colors[x,y] do
        begin
        a := 255-t;
        r := ((red * a) + (c.red * t)) div 255;
        g := ((green * a) + (c.green * t)) div 255;
        b := ((blue * a) + (c.blue * t)) div 255;
        end;
      canv.colors[x,y] := FPImage.FPColor(r,g,b,alphaOpaque);
      end;
  end;

var b,rx,ry : integer;
begin
  b := 0;
  for ry := 0 to height-1 do
    begin
    for rx := 0 to width-1 do
      combine (canvas, x+rx, y+ry, FPColor, data^[b+rx]);
    inc (b, pitch);
    end;
end;


procedure TFreeTypeFont.GetText (aText:string);
var b : boolean;
begin
  if assigned (FLastText) then
    begin
    if CompareStr(FLastText.Text,aText) <> 0 then
      begin
      FLastText.Free;
      b := true;
      end
    else
      begin
      if FAntiAliased then
        b := (FLastText.mode <> bt256Gray)
      else
        b := (FLastText.mode <> btBlackWhite);
      if b then
        FLastText.Free;
      end;
    end
  else
    b := true;
  if b then
    begin
    FontMgr.Resolution := FResolution;
    if FAntiAliased then
      FLastText := FontMgr.GetStringGray (FFontId, aText, Size, Angle)
    else
      FLastText := FontMgr.GetString (FFontId, aText, Size, Angle);
    end;
end;

procedure TFreeTypeFont.DoDrawText (atX,atY:integer; atext:string);
var r,i : integer;
    f : longint;
begin
  GetText (atext);
  with FLastText do
    for r := 0 to count-1 do
      with Bitmaps[r]^ do
        begin
        if mode = btBlackWhite then
          DrawCharBW (atX+x, atY+y, data, pitch, width, height)
        else
          DrawChar (atX+x, atY+y, data, pitch, width, height);
        end;
end;


procedure TFreeTypeFont.GetFace;
begin
  if not assigned(FFace) then
    FFace := FontMgr.GetFreeTypeFont (FFontID);
end;

function TFreeTypeFont.GetFlags (index:integer) : boolean;
begin
  if index = 5 then        //bold
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_BOLD) <> 0;
    end
  else if index = 6 then    //italic
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_ITALIC) <> 0;
    end
  else
    result := inherited GetFlags (index);
end;

function TFreeTypeFont.DoGetTextHeight (text:string) : integer;
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    result := top - bottom;
end;

function TFreeTypeFont.DoGetTextWidth (text:string) : integer;
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    result := right - left;
end;

procedure TFreeTypeFont.SetFlags (index:integer; AValue:boolean);
begin
  if not (index in [5,6]) then   // bold,italic
    inherited SetFlags (index, AValue);
end;


destructor TFreeTypeFont.Destroy;
begin
  ClearLastText;
  inherited Destroy;
end;

procedure TFreeTypeFont.DoCopyProps (From:TFPCanvasHelper);
var f : TFreeTypeFont;
begin
  inherited;
  if from is TFreeTypeFont then
    begin
    f := TFreeTypeFont(from);
    FIndex := F.Findex;
    FAntiAliased := f.FAntiAliased;
    FResolution := f.FResolution;
    FAngle := f.FAngle;
    end;
end;

procedure TFreeTypeFont.SetName (AValue:string);
begin
  inherited;
  ClearLastText;
  if allocated then
    FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.SetIndex (AValue : integer);
begin
  FIndex := AValue;
  ClearLastText;
  if allocated then
    FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.SetSize (AValue : integer);
begin
  ClearLastText;
  inherited;
end;

procedure TFreeTypeFont.ClearLastText;
begin
  if assigned(FLastText) then
    begin
    FLastText.Free;
    FlastText := nil;
    end;
end;

procedure TFreeTypeFont.DoAllocateResources;
begin
  InitEngine;
  FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.DoDeAllocateResources;
begin
end;

procedure TFreeTypeFont.DoGetTextSize (text:string; var w,h:integer);
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    begin
    w := right - left;
    h := top - bottom;
    end;
end;


procedure InitEngine;

begin
  if not assigned (FontMgr) then
    FontMgr := TFontManager.create;
end;

procedure DoneEngine;
begin
  if assigned (FontMgr) then
    FontMgr.Free;
end;

constructor TFreeTypeFont.Create;
begin
  inherited;
  FFontID := -1;
  FAntiAliased := True;
  FResolution := DefaultResolution;
end;


{ Procedures without angle have own implementation to have better speed }

function TFontManager.GetString (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.GetStringGray (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
// Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size);
end;

function TFontManager.RequestFont (afilename:string) : integer;
begin
  result := RequestFont (afilename,0);
end;

function TFontManager.RequestFont (afilename:string; anindex:integer) : integer;
var s : string;
begin
  if afilename = '' then
    result := -1
  else
    begin
    s := SearchFont (afilename);
    result := GetFontID (s,anindex);
    if result < 0 then
      result := CreateFont (s,anindex);
    end;
end;

function TFontManager.GetFreeTypeFont (aFontID:integer) : PFT_Face;
begin
  result := GetFont(aFontID).font;
end;


function TFontManager.MakeString (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
var g : PMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    e, prevIndex, prevx, c, r, rx : integer;
    pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TStringBitmaps.Create(c);
  if (CurRenderMode = FT_RENDER_MODE_MONO) then
    result.FMode := btBlackWhite
  else
    result.FMode := bt256Gray;
  prevIndex := 0;
  prevx := 0;
  pos.x := 0;
  pos.y := 0;
  for r := 0 to c-1 do
    begin
    // retrieve loaded glyph
    g := GetGlyph (Text[r+1]);
    // check kerning
    if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
      begin
      prevx := pos.x;
      e := FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern);
      if e <> 0 then
        FTError (sErrKerning, e);
      pos.x := pos.x + kern.x;
      end;
    // render the glyph
    FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
    FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, @pos, true),sErrMakingString4);
    // Copy what is needed to record
    bm := PFT_BitmapGlyph(gl);
    with result.Bitmaps[r]^ do
      begin
      with gl^.advance do
        begin
        advanceX := x shr 6;
        advanceY := y shr 6;
        end;
      with bm^ do
        begin
        height := bitmap.rows;
        width := bitmap.width;
        x := (pos.x shr 6) + left;   // transformed bitmap has correct x,y
        y := (pos.y shr 6) - top;    // not transformed has only a relative correction
        buf := PByteArray(bitmap.buffer);
        reverse := (bitmap.pitch < 0);
        if reverse then
          begin
          pitch := -bitmap.pitch;
          getmem (data, pitch*height);
          for rx := height-1 downto 0 do
            move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
          end
        else
          begin
          pitch := bitmap.pitch;
          rx := pitch*height;
          getmem (data, rx);
          move (buf^[0], data^[0], rx);
          end;
        end;
      end;
    // place position for next glyph
    pos.x := pos.x + (gl^.advance.x shr 10);
    // pos.y := pos.y + (gl^.advance.y shr 6); // for angled texts also
    if prevx > pos.x then
      pos.x := prevx;
    // finish rendered glyph
    FT_Done_Glyph (gl);
    end;
  result.FText := Text;
  result.CalculateGlobals;
end;

function TFontManager.GetString (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
// Black and white
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size, angle);
end;

function TFontManager.GetStringGray (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
// Anti Aliased gray scale
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size, angle);
end;


function TFontManager.GetGlyph (c : char) : PMgrGlyph;
var r : integer;
begin
  With CurSize^ do
    begin
    r := Glyphs.Count;
    repeat
      dec (r)
    until (r < 0) or (PMgrGlyph(Glyphs[r])^.character = c);
    if r < 0 then
      result := CreateGlyph (c)
    else
      result := PMgrGlyph(Glyphs[r]);
    end;
end;

procedure TFontManager.InitMakeString (FontID, Size:integer);
begin
  GetSize (size,Resolution);
  UseKerning := ((Curfont.font^.face_flags and FT_FACE_FLAG_KERNING) <> 0);
end;

function TFontManager.MakeString (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
var g : PMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    e, prevIndex, prevx, c, r, rx : integer;
    pre, adv, pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
    trans : FT_Matrix;
begin
  CurFont := GetFont(FontID);
  if  (Angle = 0) or   // no angle asked, or can't work with angles (not scalable)
      ((CurFont.Font^.face_flags and FT_FACE_FLAG_SCALABLE)=0) then
    result := MakeString (FontID, Text, Size)
  else
    begin
    InitMakeString (FontID, Size);
    c := length(text);
    result := TStringBitmaps.Create(c);
    if (CurRenderMode = FT_RENDER_MODE_MONO) then
      result.FMode := btBlackWhite
    else
      result.FMode := bt256Gray;
    MakeTransformation (angle, trans);
    prevIndex := 0;
    prevx := 0;
    pos.x := 0;
    pos.y := 0;
    pre.x := 0;
    pre.y := 0;
    for r := 0 to c-1 do
      begin
      // retrieve loaded glyph
      g := GetGlyph (Text[r+1]);
      // check kerning
      if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
        begin
        prevx := pre.x;
        FTCheck(FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern),sErrKerning);
        pre.x := pre.x + kern.x;
        end;
      // render the glyph
      Gl:=Nil;
      FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
      //    placing the glyph
      FTCheck(FT_Glyph_Transform (gl, nil, @pre),sErrMakingString2);
      adv := gl^.advance;
      //    rotating the glyph
      FTCheck(FT_Glyph_Transform (gl, @trans, nil),sErrMakingString3);
      //    rendering the glyph
      FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, nil, true),sErrMakingString4);
      // Copy what is needed to record
      bm := PFT_BitmapGlyph(gl);
      with result.Bitmaps[r]^ do
        begin
        with gl^.advance do
          begin
          advanceX := x div 64;
          advanceY := y div 64;
          end;
        with bm^ do
          begin
          height := bitmap.rows;
          width := bitmap.width;
          x := {(pos.x div 64)} + left;  // transformed bitmap has correct x,y
          y := {(pos.y div 64)} - top;   // not transformed has only a relative correction
          buf := PByteArray(bitmap.buffer);
          reverse := (bitmap.pitch < 0);
          if reverse then
            begin
            pitch := -bitmap.pitch;
            getmem (data, pitch*height);
            for rx := height-1 downto 0 do
              move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
            end
          else
            begin
            pitch := bitmap.pitch;
            rx := pitch*height;
            getmem (data, rx);
            move (buf^[0], data^[0], rx);
            end;
          end;
        end;
      // place position for next glyph
      with gl^.advance do
        begin
        pos.x := pos.x + (x div 1024);
        pos.y := pos.y + (y div 1024);
        end;
      with adv do
        pre.x := pre.x + (x div 1024);
      if prevx > pre.x then
        pre.x := prevx;
      // finish rendered glyph
      FT_Done_Glyph (gl);
      end;
    result.FText := Text;
    result.CalculateGlobals;
    end;
end;


procedure TFontManager.SetPixelSize (aSize, aResolution : integer);

  procedure CheckSize;
  var r : integer;
  begin
    with Curfont.Font^ do
      begin
      r := Num_fixed_sizes;
      repeat
        dec (r);
      until (r < 0) or
         ( (available_sizes^[r].height=asize) and
           (available_sizes^[r].width=asize) );
      if r >= 0 then
        raise FreeTypeException.CreateFmt ('Size %d not available for %s %s',
                  [aSize, style_name, family_name]);
      end;
  end;

var s : longint;
    Err : integer;

begin
  with Curfont, Font^ do
    if (face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then
      begin
      CheckSize;
      Err := FT_Set_pixel_sizes (Font, aSize, aSize);
      if Err <> 0 then
        FTError (format(sErrSetPixelSize,[aSize,aResolution]), Err);
      end
    else
      begin
      s := aSize shl 6;
      Err := FT_Set_char_size (Font, s, s, aResolution, aResolution);
      if Err <> 0 then
        FTError (format(sErrSetCharSize,[aSize,aResolution]), Err);
      end;
end;

procedure TFontManager.MakeTransformation (angle:real; var Transformation:FT_Matrix);
begin
  with Transformation do
    begin
    xx := round( cos(angle)*$10000);
    xy := round(-sin(angle)*$10000);
    yx := round( sin(angle)*$10000);
    yy := round( cos(angle)*$10000);
    end;
end;

function TFontManager.CreateGlyph (c : char) : PMgrGlyph;
var e : integer;
begin
  new (result);
  result^.character := c;
  result^.GlyphIndex := FT_Get_Char_Index (CurFont.font, ord(c));
  e := FT_Load_Glyph (CurFont.font, result^.GlyphIndex, FT_Load_Default);
  if e <> 0 then
    begin
    FTError (sErrLoadingGlyph, e);
    end;
  e := FT_Get_Glyph (Curfont.font^.glyph, result^.glyph);
  if e <> 0 then
    begin
    FTError (sErrLoadingGlyph, e);
    end;
  CurSize^.Glyphs.Add (result);
end;


function TFontManager.CreateFont (afilename:string; anindex:integer) : integer;
var f : TMgrFont;
begin
//  writeln ('creating font ',afilename,' (',anindex,')');
  f := TMgrFont.Create (self, afilename, anindex);
  result := FList.Count;
  Flist.Add (f);
end;

function TFontManager.GetFont (FontID:integer) : TMgrFont;
begin
  result := TMgrFont(FList[FontID]);
  if result <> CurFont then  // set last used size of the font as current size
    begin
    CurSize := result.LastSize;
    end;
end;

procedure TFontManager.GetSize (aSize, aResolution : integer);
var r : integer;
begin
  if not ( assigned(CurSize) and
          (CurSize^.Size = aSize) and (CurSize^.resolution = aResolution)) then
    begin
    r := CurFont.FSizes.count;
    repeat
      dec (r)
    until (r < 0) or ( (PMgrSize(CurFont.FSizes[r])^.size = aSize) and
                       (PMgrSize(CurFont.FSizes[r])^.resolution = FResolution) );
    if r < 0 then
      CurSize := CreateSize (aSize,aResolution)
    else
      CurSize := PMgrSize(CurFont.FSizes[r]);
    CurFont.LastSize := CurSize;
    end;
end;

function TFontManager.CreateSize (aSize, aResolution : integer) : PMgrSize;
begin
  new (result);
  result^.Size := aSize;
  result^.Resolution := aResolution;
  result^.Glyphs := Tlist.Create;
  SetPixelSize (aSize,aResolution);
  CurFont.FSizes.Add (result);
end;


procedure TFontManager.SetSearchPath (AValue : string);
  procedure AddPath (apath : string);
  begin
    FPaths.Add (IncludeTrailingBackslash(Apath));
  end;
var p : integer;
begin
  while (AValue <> '') do
    begin
    p := pos (';', AValue);
    if p = 0 then
      begin
      AddPath (AValue);
      AValue := '';
      end
    else
      begin
      AddPath (copy(AValue,1,p-1));
      delete (AVAlue,1,p);
      end;
    end;
end;

procedure TFontManager.SetExtention (AValue : string);
begin
  if AValue <> '' then
    if AValue[1] <> '.' then
      FExtention := '.' + AValue
    else
      FExtention := AValue
  else
    AValue := '';
end;

function TFontManager.SearchFont (afilename:string) : string;
// returns full filename of font, taking SearchPath in account
var p,fn : string;
    r : integer;
begin
  if (pos('.', afilename)=0) and (DefaultFontExtention<>'') then
    fn := afilename + DefaultFontExtention
  else
    fn := aFilename;
  if FileExists(fn) then
    result := ExpandFilename(fn)
  else
    begin
    p := ExtractFilepath(fn);
    if p = '' then
      begin  // no path given, look in SearchPaths
      r := FPaths.Count;
      repeat
        dec (r);
      until (r < 0) or FileExists(FPaths[r]+fn);
      if r < 0 then
        raise FreeTypeException.CreateFmt (sErrFontFileNotFound, [fn])
      else
        result := FPaths[r]+fn;
      end
    else
      raise FreeTypeException.CreateFmt (sErrFontFileNotFound, [afilename]);
    end;
end;

function TFontManager.GetFontId (afilename:string; anindex:integer) : integer;
begin
  result := FList.count-1;
  while (result >= 0) and
        ( ({$ifdef CaseSense}CompareText{$else}CompareStr{$endif}
              (TMgrFont(FList[anIndex]).Filename, afilename) <> 0) or
          (anIndex <> TMgrFont(FList[anIndex]).font^.face_index)
        ) do
    dec (result);
end;


{ TFontManager }

constructor TFontManager.Create;
var r : integer;
begin
  inherited create;
  FList := Tlist.Create;
  FPaths := TStringList.Create;
  r := FT_Init_FreeType(FTLib);
  if r <> 0  then
    begin
    FTLib := nil;
    FTError (sErrInitializing, r);
    end;
  SearchPath := DefaultSearchPath;
  DefaultExtention := DefaultFontExtention;
  Resolution := DefaultResolution;
end;

destructor TFontManager.Destroy;
  procedure FreeFontObjects;
  var r : integer;
  begin
    for r := FList.Count-1 downto 0 do
      begin
      GetFont(r).Free;
      end;
  end;
  procedure FreeLibrary;
  var r : integer;
  begin
    r := FT_Done_FreeType (FTlib);
    if r <> 0 then
      FTError (sErrDestroying, r);
  end;
begin
  FreeFontObjects;
  FList.Free;
  FPaths.Free;
  try
    if assigned(FTLib) then
      FreeLibrary;
  finally
    inherited Destroy;
  end;
end;

function TFontManager.GetSearchPath : string;
var r : integer;
begin
  if FPaths.count > 0 then
    begin
    result := FPaths[0];
    for r := 1 to FPaths.count-1 do
      result := result + ';' + FPaths[r];
    end
  else
    result := '';
end;


{ TMgrFont }

constructor TMgrFont.Create (aMgr:TFontManager; afilename:string; anindex:integer);

begin
  inherited create;
  Filename := afilename;
  Mgr := aMgr;
  FSizes := TList.create;
  LastSize := nil;
  Try
    FTCheck(FT_New_Face (aMgr.FTLib, pchar(afilename), anindex, font),format (sErrLoadFont,[anindex,afilename]));
  except
    Font:=Nil;
    Raise;
  end;
end;

destructor TMgrFont.destroy;
begin
  try
    FreeGlyphs;
  finally
    FSizes.Free;
    inherited Destroy;
  end;
end;

procedure TMgrFont.FreeGlyphs;
var r,t : integer;
    S : PMgrSize;
    G : PMgrGlyph;
begin
  for r := FSizes.count-1 downto 0 do
    begin
    with PMgrSize(FSizes[r])^ do
      begin
      for t := Glyphs.count-1 downto 0 do
        begin
        with PMgrGlyph(Glyphs[t])^ do
          FT_Done_Glyph (Glyph);
        G := PMgrGlyph(Glyphs[t]);
        dispose (G);
        end;
      Glyphs.Free;
      end;
    S := PMgrSize(FSizes[r]);
    dispose (S);
    end;
end;


procedure FTError (Event:string; Err:integer);
begin
  raise FreeTypeException.CreateFmt (sErrFreeType, [Err,Event]);
end;

Function FTCheck (Res: Integer; Msg:string) : Integer;

begin
  Result:=Res;
  If (Result<>0) then
    FTError(Msg,Result);
end;



    procedure SetTextStyle(font,direction : word;charsize : word);

      var
         f : file;
         Prefix: array[0..Prefix_Size-1] of char; {* File Prefix Holder         *}
         Length, Current: longint;
         FontData: Pchar;
         hp  : pchar;
         i   : longint;
      begin
         if font>installedfonts then
           begin
              _graphresult:=grInvalidFontNum;
              exit;
           end;

         Currenttextinfo.font:=font;
         if (direction<>HorizDir) and (direction<>VertDir) then
           direction:=HorizDir;
         Currenttextinfo.direction:=direction;
         { According to the Turbo Pascal programmer's reference }
         { maximum charsize for bitmapped font is 10            }
         if (CurrentTextInfo.Font = DefaultFont) and (Charsize > 10) then
            Currenttextinfo.charsize:=10
         else if charsize<1 then
            Currenttextinfo.charsize:=1
         else
            Currenttextinfo.charsize:=charsize;

         { This is only valid for stroked fonts }

         if (charsize <> usercharsize) then
           Case CharSize of
             1: Begin
                  CurrentXRatio := 0.55;
                  CurrentYRatio := 0.55;
                End;
             2: Begin
                  CurrentXRatio := 0.65;
                  CurrentYRatio := 0.65;
                End;
             3: Begin
                  CurrentXRatio := 0.75;
                  CurrentYRatio := 0.75;
                End;
             4: Begin
                  CurrentXRatio := 1.0;
                  CurrentYRatio := 1.0;
                End;
             5: Begin
                  CurrentXRatio := 1.3;
                  CurrentYRatio := 1.3;
                End;
             6: Begin
                  CurrentXRatio := 1.65;
                  CurrentYRatio := 1.65
                End;
             7: Begin
                  CurrentXRatio := 2.0;
                  CurrentYRatio := 2.0;
                End;
             8: Begin
                  CurrentXRatio := 2.5;
                  CurrentYRatio := 2.5;
                End;
             9: Begin
                  CurrentXRatio := 3.0;
                  CurrentYRatio := 3.0;
                End;
             10: Begin
                   CurrentXRatio := 4.0;
                   CurrentYRatio := 4.0;
                 End
           End;
         { if this is a stroked font then load it if not already loaded }
         { into memory...                                               }
         if (font>DefaultFont) and not assigned(fonts[font].instr) then
           begin
              assign(f,bgipath+fonts[font].name+'.CHR');
{$ifopt I+}
{$define IOCHECK_WAS_ON}
{$i-}
{$endif}
              reset(f,1);
{$ifdef IOCHECK_WAS_ON}
{$i+}
{$endif}
              if ioresult<>0 then
                begin
                   _graphresult:=grFontNotFound;
                   Currenttextinfo.font:=DefaultFont;
                   exit;
                end;
              {* Read in the file prefix        *}
              BlockRead(F, Prefix, Prefix_Size);
              hp:=Prefix;
              i:=0;
              while (hp[i] <> chr($1a)) do Inc(i);
              System.move(hp[i+1],fonts[font].PHeader,sizeof(TFHeader));
              (* Read in the Header file  *)
              BlockRead(F,fonts[font].Header,Sizeof(THeader));
{$ifdef FPC_BIG_ENDIAN}
              swap_fheader(fonts[font].PHeader);
              swap_header(fonts[font].Header);
{$endif FPC_BIG_ENDIAN}
              BlockRead(F,Fonts[font].Offsets[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(smallint));
{$ifdef FPC_BIG_ENDIAN}
              swap_offsets(Fonts[font].Offsets,Fonts[font].Header.First_Char,Fonts[font].Header.Nr_chars);
{$endif FPC_BIG_ENDIAN}
              {*        Load the character width table into memory.                     *}
              BlockRead(F,Fonts[font].Widths[Fonts[font].Header.First_Char],Fonts[font].Header.Nr_chars*sizeof(byte));
              {*        Determine the length of the stroke database.                    *}
              current := FilePos( f );          {* Current file location        *}
              Seek( f, FileSize(F));            {* Go to the end of the file    *}
              length := FilePos( f );           {* Get the file length          *}
              Seek( f, current);        {* Restore old file location    *}
              {*        Load the stroke database.                                       *}
              { also allocate space for Null character   }
              Getmem(FontData, Length+1);          {* Create space for font data        *}

              BlockRead(F, FontData^, length-current);        {* Load the stroke data   *}
              FontData[length-current+1] := #0;

             if fonts[font].header.Signature<> SIGNATURE then
             begin
                _graphResult:=grInvalidFont;
                Currenttextinfo.font:=DefaultFont;
                System.Freemem(FontData, Length+1);
                exit;
             end;
             fonts[font].instr:=FontData;
             fonts[font].instrLength:=Length+1;


              if not testfont(Prefix) then
                begin
                   _graphresult:=grInvalidFont;
                   Currenttextinfo.font:=DefaultFont;
                   System.Freemem(FontData, Length+1);
                end;
              close(f);
           end;
      end;


PROCEDURE LoadRAMFont(FontNr:BYTE);
{load a new font type; the [bitmaps.fnt] file contains
 the seperate sets of font bitmaps at each 5126 bytes}
BEGIN
{protect}
  IF FontNr < 1  THEN FontNr := 1;
  IF FontNr > 61 THEN FontNr := 61;
{make sure we don't load the same twice!}
  IF FontNr = CurrentFontNr THEN EXIT;
{forget previous font}
  StopRAMFonts;
{load it!}
  _LoadRAMFont(FontNr);
{allocate heap space for font bitmap}
  GetMem(FontBitMap,5200);
  FillChar(FontBitMap^,5200,0);
{say it is on}
  RAMFontsOn := TRUE;
{set font nr}
  CurrentFontNr := FontNr;
END; {LoadRAMFont}



PROCEDURE StopRAMFonts;
{simply free the memory and reset number}
BEGIN
{protect}
  IF RAMFontsOn THEN
  BEGIN
    {free heap space for font data}
    ForgetMem(FontDataPtr,FontHeight SHL 8);
    {free heap space for font bitmap}
    ForgetMem(FontBitMap,5200);
    {reset number}
    CurrentFontNr := 0;
    {say so}
    RAMFontsOn    := FALSE;
  END;
END; {StopRAMFonts}


PROCEDURE _LoadRAMFont(FontNr:BYTE);
{load a new font type; the [bitmaps.fnt] file contains
 the seperate sets of font bitmaps at each 5126 bytes}
VAR FilePtr:FILE;
    Header :ARRAY[1..6] OF BYTE;
BEGIN
{open font file; no checking is build-in create one yourself!}
  Assign(FilePtr,'BITMAPS.FNT');
  Reset(FilePtr,1);
{goto position where the bitmap is stored}
  Seek(FilePtr,LONGINT(5126)*(FontNr-1));
{read header, no checking!}
  BlockRead(FilePtr,Header,6);
{set new height}
  FontHeight := Header[5];
{allocate heap space for font data}
  GetMem(FontDataPtr,FontHeight SHL 8);
{read font data}
  BlockRead(FilePtr,FontDataPtr^,FontHeight SHL 8);
{close font file}
  Close(FilePtr);
END; {_LoadRAMFont}


PROCEDURE _AllocFontCopy(VAR DestPtr,SourcePtr:POINTER;Height:WORD);
{allocate a copy of the given source}
BEGIN
{allocate heap space for font data}
  GetMem(DestPtr,Height SHL 8);
{copy source into destination}
  Move(SourcePtr^,DestPtr^,Height SHL 8);
END; {_AllocFontCopy}


  FreeTypeFontException = class (TFPFontException);

  TFreeTypeFont = class (TFPCustomDrawFont)
  private
    FResolution : longword;
    FAntiAliased : boolean;
    FLastText : TStringBitmaps;
    FIndex, FFontID : integer;
    FFace : PFT_Face;
    FAngle : real;
    procedure DrawChar (x,y:integer; data:PByteArray; pitch, width, height:integer);
    procedure DrawCharBW (x,y:integer; data:PByteArray; pitch, width, height:integer);
    procedure ClearLastText;
  protected
    procedure SetName (AValue:string); override;
    procedure SetIndex (AValue : integer);
    procedure SetSize (AValue : integer); override;
    function GetFlags (index:integer) : boolean; override;
    procedure SetFlags (index:integer; AValue:boolean); override;
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure DoDrawText (atx,aty:integer; atext:string); override;
    procedure DoGetTextSize (text:string; var w,h:integer); override;
    function DoGetTextHeight (text:string) : integer; override;
    function DoGetTextWidth (text:string) : integer; override;
    procedure GetText (aText:string);
    procedure GetFace;
  public
    constructor create; override;
    destructor Destroy; override;
    property FontIndex : integer read FIndex write SetIndex;
    property Resolution : longword read FResolution write FResolution;
    property AntiAliased : boolean read FAntiAliased write FAntiAliased;
    property Angle : real read FAngle write FAngle;
  end;

  TFontManager = class
    private
      FTLib : PFT_Library;
      FList : TList;
      FPaths : TStringList;
      FExtention : string;
      FResolution : integer;
      CurFont : TMgrFont;
      CurSize : PMgrSize;
      CurRenderMode : FT_Render_Mode;
      CurTransform : FT_Matrix;
      UseKerning : boolean;
      function GetSearchPath : string;
      procedure SetSearchPath (AValue : string);
      procedure SetExtention (AValue : string);
    protected
      function GetFontId (afilename:string; anindex:integer) : integer;
      function CreateFont (afilename:string; anindex:integer) : integer;
      function SearchFont (afilename:string) : string;
      function GetFont (FontID:integer) : TMgrFont;
      procedure GetSize (aSize, aResolution : integer);
      function CreateSize (aSize, aResolution : integer) : PMgrSize;
      procedure SetPixelSize (aSize, aResolution : integer);
      function GetGlyph (c : char) : PMgrGlyph;
      function CreateGlyph (c : char) : PMgrGlyph;
      procedure MakeTransformation (angle:real; var Transformation:FT_Matrix);
      procedure InitMakeString (FontID, Size:integer);
      function MakeString (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
      function MakeString (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
    public
      constructor Create;
      destructor destroy; override;
      function RequestFont (afilename:string) : integer;
      function RequestFont (afilename:string; anindex:integer) : integer;
      function GetFreeTypeFont (aFontID:integer) : PFT_Face;
      function GetString (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
      // Black and white
      function GetStringGray (FontId:integer; Text:string; size:integer; angle:real) : TStringBitmaps;
      // Anti Aliased gray scale
      function GetString (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
      // Black and white, following the direction of the font (left to right, top to bottom, ...)
      function GetStringGray (FontId:integer; Text:string; Size:integer) : TStringBitmaps;
      // Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
      property SearchPath : string read GetSearchPath write SetSearchPath;
      property DefaultExtention : string read FExtention write SetExtention;
      property Resolution : integer read Fresolution write FResolution;
  end;


  TFontManager = class;

  PMgrGlyph = ^TMgrGlyph;
  TMgrGlyph = record
    Character : char;
    GlyphIndex : FT_UInt;
    Glyph : PFT_Glyph;
  end;

  PMgrSize = ^TMgrSize;
  TMgrSize = record
    Resolution, Size : integer;
    Glyphs : TList;
  end;

  TMgrFont = class
    private
      Mgr : TFontManager;
      Font : PFT_Face;
      FSizes : TList;
      Filename : string;
      LastSize : PMgrSize;
      procedure FreeGlyphs;
    public
      constructor Create (aMgr:TFontManager; afilename:string; anindex:integer);
      destructor destroy; override;
  end;


  FreeTypeException = class (exception);

  TFPCustomFont = class (TFPCanvasHelper)
  private
    FName : string;
    FSize : integer;
  protected
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetName (AValue:string); virtual;
    procedure SetSize (AValue:integer); virtual;
  public
    function CopyFont : TFPCustomFont;
    // Creates a copy of the font with all properties the same, but not allocated
    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;
    property Name : string read FName write SetName;
    property Size : integer read FSize write SetSize;
    property Bold : boolean index 5 read GetFlags write SetFlags;
    property Italic : boolean index 6 read GetFlags write SetFlags;
    property Underline : boolean index 7 read GetFlags write SetFlags;
    property StrikeTrough : boolean index 8 read GetFlags write SetFlags;
  end;
  TFPCustomFontClass = class of TFPCustomFont;

     { Prefix header of Font file }
{      PFHeader = ^TFHeader;}
      TFHeader = packed record
         header_size: word;    {* Version 2.0 Header Format        *}
         font_name: array[1..4] of char;
         font_size: word;      {* Size in byte of file        *}
         font_major: byte;     {* Driver Version Information    *}
         font_minor: byte;
         min_major: byte;      {* BGI Revision Information         *}
         min_minor: byte;
      end;


      { Font record information }
{      PHeader = ^THeader;}
      THeader = packed record
        Signature:  char;     { signature byte                        }
        Nr_chars:   smallint;  { number of characters in file          }
        Reserved:   byte;
        First_char: byte;     { first character in file               }
        cdefs :     smallint;  { offset to character definitions       }
        scan_flag:  byte;     { TRUE if char is scanable              }
        org_to_cap: shortint;     { Height from origin to top of capitol  }
        org_to_base:shortint;     { Height from origin to baseline        }
        org_to_dec: shortint;     { Height from origin to bot of decender }
        _reserved: array[1..4] of char;
        Unused: byte;
      end;

  { Text }

  WRITEMODE_OVERWRITE = 0;
  WRITEMODE_MASKED    = 1;
  FONT_EXPANDED       = 0;
  FONT_COMPRESSED     = 2;

       { Set/GetTextStyle Konstanten: }
       DefaultFont = 0;
       TriplexFont = 1;
       SmallFont = 2;
       SansSerifFont = 3;
       GothicFont = 4;
       ScriptFont = 5;
       SimpleFont = 6;
       TSCRFont = 7;
       LCOMFont = 8;
       EuroFont = 9;
       BoldFont = 10;



FONTS (GLYPHS[Times, Arial,etc...]) are used for True/FreeType FONTS that you normally see these days.


procedure draw_char(screen, where:intger; font_char:char);


begin
        l:=0;
   	repeat
 	
 		for (i = 8; i > 0; i--)
 		begin
                        inc(j);
 			if ((font_char[l] & (1<<i)))
 			begin
 				
 				put_pixel(j, h, c);
                        end;
 		end;
 		inc(h);
 		j := x;
		inc(l);
 	until (l>8);
end;


procedure draw_string(screen,where:integer; input:string);
var
	x:integer;

 begin
   x:=1
   with input do begin //should be repeat until eol(string);
     draw_char(screen,where,font_data[input[x]]);
     where:=where+char_width;
     inc(x);     
   end;
 end;

// holding what you need for every character of the set
font_char:pointer; 
font_data:array[1..MAXCHARS] of font_char;
 
 // rendering one of the character, given its font_data
 draw_char(screen, where, font_char);

type


       TextSettingsType = record
             font : word;
             direction : word;
             charsize : word;
             horiz : word;
             vert : word;
       end;

	TextSettingsType = record
        	font,direction,charsize,horiz,vert: word;
        end;


