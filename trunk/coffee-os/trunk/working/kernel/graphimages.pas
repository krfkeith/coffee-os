
initialization
  ImageHandlers := TImageHandlersManager.Create;
  GrayConvMatrix := GCM_JPEG;

  ImageHandlers.RegisterImageReader ('BMP Format', 'bmp', TFPReaderBMP);
  ImageHandlers.RegisterImageReader ('PNM Format', 'PNM;PGM;PBM', TFPReaderPNM);
  ImageHandlers.RegisterImageWriter ('BMP Format', 'bmp', TFPWriterBMP);
  ImageHandlers.RegisterImageWriter ('PBM Format', 'pbm', TFPWriterPNM);
  ImageHandlers.RegisterImageWriter ('XPM Format', 'xpm', TFPWriterXPM);
  MakeCRCtable;
  ImageHandlers.RegisterImageReader('PCX Format', 'pcx', TFPReaderPCX);
  ImageHandlers.RegisterImageReader ('TARGA Format', 'tga', TFPReaderTarga);

begin
  InternalCheck:=True;
end;

finalization
  ImageHandlers.Free;
  DoneEngine;

end.


procedure TStringBitmaps.CalculateGlobals;
var r : integer;
begin
  if count = 0 then
    Exit;
  // check first 2 bitmaps for left side
  // check last 2 bitmaps for right side
  with BitMaps[0]^ do
    begin
    FBounds.left := x;
    FBounds.top := y + height;
    FBounds.bottom := y;
    end;
  with Bitmaps[count-1]^ do
    FBounds.right := x + width;
  if count > 1 then
    begin
    with Bitmaps[1]^ do
      r := x;
    if r < FBounds.left then
      FBounds.left := r;
    with Bitmaps[count-2]^ do
      r := x + width;
    if r > FBounds.right then
      FBounds.right := r;
    end;
  // check top/bottom of other bitmaps
  for r := 1 to count-1 do
    with Bitmaps[r]^ do
      begin
      if FBounds.top < y + height then
        FBounds.top := y + height;
      if FBounds.bottom > y then
        FBounds.bottom := y;
      end;
end;

procedure TStringBitmaps.GetBoundRect (var aRect : TRect);
begin
  aRect := FBounds;
end;


{ TStringBitmaps }

function TStringBitmaps.GetCount : integer;
begin
  result := FList.Count;
end;

function TStringBitmaps.GetBitmap (index:integer) : PFontBitmap;
begin
  result := PFontBitmap(FList[index]);
end;

constructor TStringBitmaps.Create (ACount : integer);
var r : integer;
    bm : PFontBitmap;
begin
  inherited create;
  FList := Tlist.Create;
  FList.Capacity := ACount;
  for r := 0 to ACount-1 do
    begin
    new (bm);
    FList.Add (bm);
    end;
end;

destructor TStringBitmaps.destroy;
var r : integer;
    bm : PFontBitmap;
begin
  for r := 0 to Flist.count-1 do
    begin
    bm := PFontBitmap(FList[r]);
    freemem (bm^.data);
    dispose (bm);
    end;
  FList.Free;
  inherited;
end;


Procedure TFPReaderTarga.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Col : Integer;
  C   : TFPColor;
  W   : Word;
  P   : PByte;

begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Case Header.ImgType of
    TARGA_INDEXED_IMAGE
      : for Col:=0 to Img.width-1 do
         Img.Colors[Col,Row]:=FPalette[P[Col]];
    TARGA_TRUECOLOR_IMAGE
      : for Col:=0 to Img.Width-1 do
          begin
          // Fill C depending on number of pixels.
          case BytesPerPixel of
          8,16 : begin
                 W:=P[0];
                 inc(P);
                 W:=W or (P[0] shl 8);
                 With C do
                   begin
                   Red:=((W)shr 10) shl 11;
                   Green:=((w)shr 5) shl 11;
                   Blue:=((w)) shl 11;
                   end;
                end;
          24,32 : With C do
                  begin
                  Blue:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Green:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Red:=P[0] or (P[0] shl 8);
                  If bytesPerPixel=32 then
                    begin
                    Inc(P);
                    Alpha:=AlphaOpaque;
                    if alphaBits = 8 then
                      if (P[0] and $80) = 0 then
                        Alpha:=alphaTransparent;
                    end;
                  end;
          end; // Case BytesPerPixel;
          Img[Col,Row]:=C;
          Inc(P);
          end;
    TARGA_GRAY_IMAGE
      :  case BytesPerPixel of
           8 : for Col:=0 to Img.width-1 do
                 Img.Colors[Col,Row]:=FPalette[P[Col]];
          16 : for Col:=0 to Img.width-1 do
               begin
                 With C do
                 begin
                   Blue:=FPalette[P^].blue;
                   Green:=FPalette[P^].green;
                   Red:=FPalette[P^].red;
                   Inc(P);
                   Alpha:=AlphaOpaque;
                   if alphaBits = 8 then
                    if (P[0] and $80) = 0 then
                        Alpha:=alphaTransparent;
                   Inc(P);
                 end;
               Img[Col,Row]:=C;
               end;
         end;
  end;
end;

function  TFPReaderTarga.InternalCheck (Stream:TStream) : boolean;

begin
  Result:=True;
end;


Procedure TFPReaderTarga.ReadPalette(Stream : TStream);

Var
  BGREntry : TBGREntry;
  BGRAEntry : TBGRAEntry;
  I : Integer;

begin
  Case Header.MapEntrySize Of
     16, 24:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGREntry, SizeOf(BGREntry));
          With FPalette[I] do
            begin
            Red:=BGREntry.Red shl 8;
            Green:=BGREntry.Green shl 8;
            Blue:=BGREntry.Blue shl 8;
            Alpha:=alphaOpaque;
            end;
        end;
     32:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGRAEntry,SizeOf(BGRAEntry));
          With FPalette[I] do
            begin
            Red:=BGRAEntry.Red shl 8;
            Green:=BGRAEntry.Green shl 8;
            Blue:=BGRAEntry.Blue shl 8;
            if alphaBits = 8 then
               if (BGRAEntry.Alpha and $80) <> 0 then
                 Alpha:=alphaTransparent
               else
                 Alpha:=AlphaOpaque;
            end;
        end;
    end;
end;


Procedure TFPReaderTarga.InternalRead  (Stream:TStream; Img:TFPCustomImage);

var
  H,Row : Integer;

begin
  Stream.Read(Header,SizeOf(Header));
  AnalyzeHeader(Img);
  If Header.IdLen>0 then
    begin
    SetLength(Identification,Header.IDLen);
    Stream.Read(Identification[1],Header.Idlen);
    If Length(Identification)<>0 then
      Img.Extra[KeyIdentification]:=Identification;
    end;

  If Header.MapType<>0 then
    ReadPalette(Stream);
  if Header.ImgType = TARGA_GRAY_IMAGE then
    CreateGrayPalette;

  H:=Img.height;
  If BottomUp then
    For Row:=0 to H-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end
  else
    For Row:=H-1 downto 0 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
end;

Procedure TFPReaderTarga.ReadScanLine(Row : Integer; Stream : TStream);

Var
  P : PByte;
  B : Byte;
  I,J : Integer;

begin
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
    P:=FScanLine;
    For I:=0 to ToWord(Header.Width)-1 do
      begin
      If (FPixelCount>0) then
        Dec(FPixelCount)
      else
        begin
        Dec(FBlockCount);
        If (FBlockCount<0) then
          begin
          Stream.ReadBuffer(B,1);
          If (B and $80)<>0 then
            begin
            FPixelCount:=B and $7F;
            FblockCount:=0;
            end
          else
            FBlockCount:=B and $7F
          end;
        Stream.ReadBuffer(FlastPixel,BytesPerPixel shr 3);
        end;
      For J:=0 to (BytesPerPixel shr 3)-1 do
        begin
        P[0]:=FLastPixel[j];
        Inc(P);
        end;
      end;
    end;
end;


procedure TFPReaderPCX.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  H, Row:   integer;
  continue: boolean;
  Rect:     TRect;
begin
  TotalWrite  := 0;
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psStarting, 0, False, Rect, '', continue);
  Stream.Read(Header, SizeOf(Header));
  AnalyzeHeader(Img);
  case BytesPerPixel of
    1: CreateBWPalette(Img);
    4: CreatePalette16(Img);
    8: ReadPalette(stream, Img);
    else
      if (Header.PaletteType = 2) then
        CreateGrayPalette(Img);
  end;
  H := Img.Height;
  TotalWrite := Img.Height * Img.Width;
  for Row := 0 to H - 1 do
  begin
    ReadScanLine(Row, Stream);
    WriteScanLine(Row, Img);
  end;
  Progress(psEnding, 100, False, Rect, '', continue);
  freemem(FScanLine);
end;

procedure TFPReaderPCX.WriteScanLine(Row: integer; Img: TFPCustomImage);
var
  Col:   integer;
  C:     TFPColor;
  P, P1, P2, P3: PByte;
  Z2:    word;
  color: byte;
begin
  C.Alpha := AlphaOpaque;
  P  := FScanLine;
  Z2 := Header.BytesPerLine;
  begin
    case BytesPerPixel of
      1:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Img.Colors[Col, Row] := Img.Palette[1]
          else
            Img.Colors[Col, Row] := Img.Palette[0];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      4:
      begin
        P1 := P;
        Inc(P1, Z2);
        P2 := P;
        Inc(P2, Z2 * 2);
        P3 := P;
        Inc(P3, Z2 * 3);
        for Col := 0 to Img.Width - 1 do
        begin
          color := 0;
          if (P[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1);
          if (P1[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 1);
          if (P2[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 2);
          if (P3[col div 8] and (128 shr (col mod 8))) <> 0 then
            Inc(color, 1 shl 3);
          Img.Colors[Col, Row] := Img.Palette[color];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      8:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          Img.Colors[Col, Row] := Img.Palette[P[Col]];
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
      24:
      begin
        for Col := 0 to Img.Width - 1 do
        begin
          with C do
          begin
            Red   := P[col] or (P[col] shl 8);
            Blue  := P[col + Z2 * 2] or (P[col + Z2 * 2] shl 8);
            Green := P[col + Z2] or (P[col + Z2] shl 8);
            Alpha := alphaOpaque;
          end;
          Img[col, row] := C;
          UpdateProgress(trunc(100.0 * (Row * Col / TotalWrite)));
        end;
      end;
    end;
  end;
end;

function TFPReaderPCX.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

Constructor TFPReaderTarga.Create;

begin
end;

Destructor TFPReaderTarga.Destroy;

begin
  FreeBuffers;
  Inherited;
end;

Procedure TFPReaderTarga.FreeBuffers;

begin
  If (FScanLine<>Nil) then
    begin
    FreeMem(FScanLine);
    FScanLine:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FScanLine:=Nil;
    end;
end;

Procedure TFPReaderTarga.AnalyzeHeader(Img : TFPCustomImage);

begin
  With Header do
    begin
    if not (ImgType in [1, 2, 3, 9, 10, 11]) and
       not (PixelSize in [8, 16, 24, 32]) then
      Raise Exception.Create('Unknown/Unsupported Targa image type');
    BottomUp:=(Flags and $20) <>0;
    AlphaBits := Flags and $0F;
    BytesPerPixel:=PixelSize;
    Compressed:=ImgType>8;
    If Compressed then
      ImgType:=ImgType-8;
    FLineSize:=(BytesPerPixel div 8)*ToWord(Width);
    GetMem(FScanLine,FLineSize);

    if ImgType = TARGA_GRAY_IMAGE then
      FPaletteSize:=SizeOf(TFPColor)*255
    else
      FPaletteSize:=SizeOf(TFPColor)*ToWord(MapLength);
    GetMem(FPalette,FPaletteSize);
    Img.Width:=ToWord(Width);
    Img.Height:=ToWord(Height);
    end;
end;

Procedure TFPReaderTarga.CreateGrayPalette;

Var
  I : Integer;

Begin
  For I:=0 To 255 Do
  Begin
    With FPalette[I] do
      begin
      Red:=I*255;
      Green:=I*255;
      Blue:=I*255;
      Alpha:=AlphaOpaque;
      end;
  end;
End;



procedure TFPReaderPCX.CreatePalette16(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 15 do
  begin
    with c, header do
    begin
      Red   := ColorMap[I].red shl 8;
      Green := ColorMap[I].Green shl 8;
      Blue  := ColorMap[I].Blue shl 8;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateGrayPalette(Img: TFPCustomImage);
var
  I: integer;
  c: TFPColor;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  for I := 0 to 255 do
  begin
    with c do
    begin
      Red   := I * 255;
      Green := I * 255;
      Blue  := I * 255;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(c);
  end;
end;

procedure TFPReaderPCX.CreateBWPalette(Img: TFPCustomImage);
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  Img.Palette.Add(colBlack);
  Img.Palette.Add(colWhite);
end;

procedure TFPReaderPCX.ReadPalette(Stream: TStream; Img: TFPCustomImage);
var
  RGBEntry: TRGB;
  I:      integer;
  c:      TFPColor;
  OldPos: integer;
begin
  Img.UsePalette := True;
  Img.Palette.Clear;
  OldPos := Stream.Position;
  Stream.Position := Stream.Size - 768;
  for I := 0 to 255 do
  begin
    Stream.Read(RGBEntry, SizeOf(RGBEntry));
    with c do
    begin
      Red   := RGBEntry.Red shl 8;
      Green := RGBEntry.Green shl 8;
      Blue  := RGBEntry.Blue shl 8;
      Alpha := alphaOpaque;
    end;
    Img.Palette.Add(C);
  end;
  Stream.Position := OldPos;
end;

procedure TFPReaderPCX.AnalyzeHeader(Img: TFPCustomImage);
begin
  with Header do
  begin
    if not ((FileID in [$0A, $0C]) and (ColorPlanes in [1, 3, 4]) and
      (Version in [0, 2, 3, 5]) and (PaletteType in [1, 2])) then
      raise Exception.Create('Unknown/Unsupported PCX image type');
    BytesPerPixel := BitsPerPixel * ColorPlanes;
    FCompressed   := Encoding = 1;
    Img.Width     := XMax - XMin + 1;
    Img.Height    := YMax - YMin + 1;
    FLineSize     := (BytesPerLine * ColorPlanes);
    GetMem(FScanLine, FLineSize);
  end;
end;

procedure TFPReaderPCX.ReadScanLine(Row: integer; Stream: TStream);
var
  P: PByte;
  B: byte;
  bytes, Count: integer;
begin
  P     := FScanLine;
  bytes := FLineSize;
  Count := 0;
  if Compressed then
  begin
    while bytes > 0 do
    begin
      if (Count = 0) then
      begin
        Stream.ReadBuffer(B, 1);
        if (B < $c0) then
          Count := 1
        else
        begin
          Count := B - $c0;
          Stream.ReadBuffer(B, 1);
        end;
      end;
      Dec(Count);
      P[0] := B;
      Inc(P);
      Dec(bytes);
    end;
  end
  else
    Stream.ReadBuffer(FScanLine^, FLineSize);
end;

procedure TFPReaderPCX.UpdateProgress(percent: longint);
var
  continue: boolean;
  Rect:     TRect;
begin
  Rect.Left   := 0;
  Rect.Top    := 0;
  Rect.Right  := 0;
  Rect.Bottom := 0;
  continue    := True;
  Progress(psRunning, 0, False, Rect, '', continue);
end;


procedure FPImgError (Fmt:TErrorTextIndices; data : array of const);
begin
  raise FPImageException.CreateFmt (ErrorText[Fmt],data);
end;

procedure FPImgError (Fmt:TErrorTextIndices);
begin
  raise FPImageException.Create (ErrorText[Fmt]);
end;

function FPColor (r,g,b:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := alphaOpaque;
    end;
end;

function FPColor (r,g,b,a:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := a;
    end;
end;

operator = (const c,d:TFPColor) : boolean;
begin
  result := (c.Red = d.Red) and
            (c.Green = d.Green) and
            (c.Blue = d.Blue) and
            (c.Alpha = d.Alpha);
end;

function GetFullColorData (color:TFPColor) : TColorData;
begin
  result := PColorData(@color)^;
end;

function SetFullColorData (color:TColorData) : TFPColor;
begin
  result := PFPColor (@color)^;
end;

operator or (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) OR GetFullColorData(d));
end;

operator and (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) AND GetFullColorData(d));
end;

operator xor (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) XOR GetFullColorData(d));
end;



{ TFPMemoryImage }

constructor TFPMemoryImage.Create (AWidth,AHeight:integer);
begin
  Fdata := nil;
  inherited create (AWidth,AHeight);
{Default behavior is to use palette as suggested by Michael}
  SetUsePalette(True);
end;

destructor TFPMemoryImage.Destroy;
begin
  // MG: missing if
  if FData<>nil then
    FreeMem (FData);
  inherited Destroy;
end;

function TFPMemoryImage.GetInternalColor(x,y:integer):TFPColor;
  begin
    if Assigned(FPalette)
    then
      Result:=inherited GetInternalColor(x,y)
    else
      Result:=PFPColorArray(FData)^[y*FWidth+x];
  end;

function TFPMemoryImage.GetInternalPixel (x,y:integer) : integer;
begin
  result := FData^[y*FWidth+x];
end;

procedure TFPMemoryImage.SetInternalColor (x,y:integer; const Value:TFPColor);
  begin
    if Assigned(FPalette)
    then
      inherited SetInternalColor(x,y,Value)
    else
      PFPColorArray(FData)^[y*FWidth+x]:=Value;
  end;

procedure TFPMemoryImage.SetInternalPixel (x,y:integer; Value:integer);
begin
  FData^[y*FWidth+x] := Value;
end;

function Lowest (a,b : integer) : integer;
begin
  if a <= b then
    result := a
  else
    result := b;
end;

procedure TFPMemoryImage.SetSize (AWidth, AHeight : integer);
var w, h, r, old : integer;
    NewData : PFPIntegerArray;
begin
  if (AWidth <> Width) or (AHeight <> Height) then
    begin
    old := Height * Width;
    r:=AWidth*AHeight;
    if Assigned(FPalette)
    then
      r:=SizeOf(integer)*r
    else
      r:=SizeOf(TFPColor)*r;
    if r = 0 then
      NewData := nil
    else
      begin
      GetMem (NewData, r);
      FillWord (Newdata^[0], r div sizeof(word), 0);
      end;
    // MG: missing "and (NewData<>nil)"
    if (old <> 0) and assigned(FData) and (NewData<>nil) then
      begin
      if r <> 0 then
        begin
        w := Lowest(Width, AWidth);
        h := Lowest(Height, AHeight);
        for r := 0 to h-1 do
          move (FData^[r*Width], NewData^[r*AWidth], w);
        end;
      end;
    if Assigned(FData) then FreeMem(FData);
    FData := NewData;
    inherited;
    end;
end;

procedure TFPMemoryImage.SetUsePalette(Value:boolean);
var
  OldColors:PFPColorArray;
  OldPixels:PFPIntegerArray;
  r,c:Integer;
begin
  if Value<>assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette:=TFPPalette.Create(0);
        //FPalette.Add(colTransparent);
        if assigned(FData) then
          begin
          OldColors:=PFPColorArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(Integer));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=OldColors^[r*FWidth+c];
          FreeMem(OldColors);
          end;
      end
    else
      begin
        if Assigned(FData) then
          begin
          OldPixels:=PFPIntegerArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(TFPColor));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=FPalette.Color[OldPixels^[r*FWidth+c]];
          FreeMem(OldPixels);
          end;
        FPalette.Free;
        FPalette:=nil;
      end;
end;


function TFPCustomImage.GetColor (x,y:integer) : TFPColor;
begin
  CheckIndex (x,y);
  result := GetInternalColor(x,y);
end;

procedure TFPCustomImage.SetInternalColor (x,y:integer; const Value:TFPColor);
var i : integer;
begin
  i := FPalette.IndexOf (Value);
  SetInternalPixel (x,y,i);
end;

function TFPCustomImage.GetInternalColor (x,y:integer) : TFPColor;
begin
  result := FPalette.Color[GetInternalPixel(x,y)];
end;

function TFPCustomImage.GetUsePalette : boolean;
begin
  result := assigned(FPalette);
end;

procedure TFPCustomImage.SetUsePalette(Value:boolean);
begin
  if Value <> assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette := TFPPalette.Create (0);
        // FPalette.Add (colTransparent);
      end
    else
      begin
        FPalette.Free;
        FPalette := nil;
      end;
end;

procedure TFPCustomImage.CheckPaletteIndex (PalIndex:integer);
begin
  if UsePalette then
    begin
    if (PalIndex < -1) or (PalIndex >= FPalette.Count) then
      FPImgError (StrInvalidIndex,[ErrorText[StrPalette],PalIndex]);
    end
  else
    FPImgError (StrNoPaletteAvailable);
end;

procedure TFPCustomImage.CheckIndex (x,y:integer);
begin
  if (x < 0) or (x >= FWidth) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= FHeight) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageY],y]);
end;

Procedure TFPCustomImage.Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);
begin
  If Assigned(FOnProgress) then
    FonProgress(Sender,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;

Procedure TFPCustomImage.Assign(Source: TPersistent);

Var
  Src : TFPCustomImage;
  X,Y : Integer;

begin
  If Source is TFPCustomImage then
    begin
    Src:=TFPCustomImage(Source);
    // Copy extra info
    FExtra.Assign(Src.Fextra);
    // Copy palette if needed.
    SetSize(0,0); { avoid side-effects in descendant classes }
    UsePalette:=Src.UsePalette;
    If UsePalette then
      begin
      Palette.Count:=0;
      Palette.Merge(Src.Palette);
      end;
    // Copy image.
    SetSize(Src.Width,Src.height);
    If UsePalette then
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          pixels[X,Y]:=src.pixels[X,Y]
    else
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          self[X,Y]:=src[X,Y];
    end
  else
    Inherited Assign(Source);
end;


function TFPCustomImage.GetExtraKey (index:integer) : string;
begin
  result := FExtra.Names[index];
end;

procedure TFPCustomImage.SetExtra (const key:String; const AValue:string);
begin
  FExtra.values[key] := AValue;
end;

function TFPCustomImage.GetExtra (const key:String) : string;
begin
  result := FExtra.values[key];
end;

function  TFPCustomImage.ExtraCount : integer;
begin
  result := FExtra.count;
end;

procedure TFPCustomImage.RemoveExtra (const key:string);
var p : integer;
begin
  p := FExtra.IndexOfName(key);
  if p >= 0 then
    FExtra.Delete (p);
end;

procedure TFPCustomImage.SetPixel (x,y:integer; Value:integer);
begin
  CheckPaletteIndex (Value);
  CheckIndex (x,y);
  SetInternalPixel (x,y,Value);
end;

function TFPCustomImage.GetPixel (x,y:integer) : integer;
begin
  CheckIndex (x,y);
  result := GetInternalPixel(x,y);
end;

procedure TFPCustomImage.SetColor (x,y:integer; const Value:TFPColor);
begin
  CheckIndex (x,y);
  SetInternalColor (x,y,Value);
end;


procedure TFPCustomImage.LoadFromFile (const filename:String);
var e,s : string;
    r : integer;
    f : TFileStream;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    d : TIHData;
    Msg : string;
begin
  e := lowercase (ExtractFileExt(filename));
  if (e <> '') and (e[1] = '.') then
    delete (e,1,1);
  with ImageHandlers do
    begin
      r := count-1;
      s := e + ';';
      while (r >= 0) do
        begin
        d := GetData(r);
        if (pos(s,d.Fextention+';') <> 0) then
          try
            h := d.FReader;
            if assigned (h) then
              begin
              reader := h.Create;
              try
                loadfromfile (filename, reader);
              finally
                Reader.Free;
              end;
              break;
              end;
          except
            on e : exception do
              Msg := e.message;
          end;
        dec (r);
        end
    end;
  if Msg = '' then
    begin
    if r < 0 then
      begin
      f := TFileStream.Create (filename, fmOpenRead);
      try
        LoadFromStream (f);
      finally
        f.Free;
      end;
      end;
    end
  else
    FPImgError (StrReadWithError, [Msg]);
end;

procedure TFPCustomImage.SetHeight (Value : integer);
begin
  if Value <> FHeight then
    SetSize (FWidth, Value);
end;

procedure TFPCustomImage.SetWidth (Value : integer);
begin
  if Value <> FWidth then
    SetSize (Value, FHeight);
end;

procedure TFPCustomImage.SetSize (AWidth, AHeight : integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TFPCustomImage.SetExtraValue (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    FExtra[index] := copy(s, 1, p) + AValue
  else
    FPImgError (StrInvalidIndex,[ErrorText[StrImageExtra],index]);
end;

function TFPCustomImage.GetExtraValue (index:integer) : string;
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    result := copy(s, p+1, maxint)
  else
    result := '';
end;

procedure TFPCustomImage.SetExtraKey (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos('=',s);
  if p > 0 then
    s := AValue + copy(s,p,maxint)
  else
    s := AValue;
  FExtra[index] := s;
end;


{ TFPCustomImage }

constructor TFPCustomImage.create (AWidth,AHeight:integer);
begin
  inherited create;
  FExtra := TStringList.Create;
  FWidth := 0;
  FHeight := 0;
  FPalette := nil;
  SetSize (AWidth,AHeight);
end;

destructor TFPCustomImage.destroy;
begin
  FExtra.Free;
  if assigned (FPalette) then
    FPalette.Free;
  inherited;
end;

procedure TFPCustomImage.LoadFromStream (Str:TStream; Handler:TFPCustomImagereader);
begin
  Handler.ImageRead (Str, self);
end;

procedure TFPCustomImage.LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
var
  fs : TStream;
begin
  if FileExists (filename) then
    begin
    fs := TFileStream.Create (filename, fmOpenRead);
    try
      LoadFromStream (fs, handler);
    finally
      fs.Free;
    end;
    end
  else
    FPImgError (StrNoFile, [filename]);
end;

procedure TFPCustomImage.SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
begin
  Handler.ImageWrite (Str, Self);
end;

procedure TFPCustomImage.SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
var
  fs : TStream;
begin
  fs := TFileStream.Create (filename, fmCreate);
  try
    SaveToStream (fs, handler);
  finally
    fs.Free;
  end
end;

procedure TFPCustomImage.SaveToFile (const filename:String);

var e,s : string;
    r : integer;
    f : TFileStream;
    h : TFPCustomImageWriterClass;
    Writer : TFPCustomImageWriter;
    d : TIHData;
    Msg : string;

begin
  e := lowercase (ExtractFileExt(filename));
  if (e <> '') and (e[1] = '.') then
    delete (e,1,1);
  with ImageHandlers do
    begin
    r := count-1;
    s := e + ';';
    while (r >= 0) do
      begin
      d := GetData(r);
      if (pos(s,d.Fextention+';') <> 0) then
        try
          h := d.FWriter;
          if assigned (h) then
            begin
            Writer := h.Create;
            try
              SaveTofile (filename, Writer);
            finally
              Writer.Free;
            end;
            break;
            end;
        except
          on e : exception do
            Msg := e.message;
        end;
      dec (r);
      end
    end;
  if (Msg<>'') then
    FPImgError (StrWriteWithError, [Msg]);
end;


procedure TFPCustomImage.LoadFromStream (Str:TStream);
var r : integer;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    msg : string;
    d : TIHData;
begin
  with ImageHandlers do
    try
      r := count-1;
      while (r >= 0) do
        begin
        d := GetData(r);
        if assigned (d) then
          h := d.FReader;
        if assigned (h) then
          begin
          reader := h.Create;
          with reader do
            try
              if CheckContents (str) then
                try
                  FStream := str;
                  FImage := self;
                  InternalRead (str, self);
                  break;
                except
                  on e : exception do
                    msg := e.message;
                end;
            finally
              Free;
              str.seek (soFromBeginning, 0);
            end;
          end;
        dec (r);
        end;
    except
      on e : exception do
        FPImgError (StrCantDetermineType, [e.message]);
    end;
  if r < 0 then
    if msg = '' then
      FPImgError (StrNoCorrectReaderFound)
    else
      FPImgError (StrReadWithError, [Msg]);
end;


procedure TFPWriterBMP.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize:Integer;
  PadCount : byte;
  aLine: PByte;
  i : Integer;
  tmppos : int64;
  continue : boolean;
  percent : byte;
  percentinterval : longword;
  percentacc : longword;
  Rect : TRect;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  percent:=0;
  percentinterval:=(Img.Height*4) div 100;
  if percentinterval=0 then percentinterval:=$FFFFFFFF;
  percentacc:=0;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;
  if (FRLECompress and (not (FBpp in [4,8]))) then
    raise FPImageException.Create('Can''t use RLE compression with '+IntToStr(FBpp)+' bits per pixel');
  if FRLECompress and (FBpp=4) then BFI.Compression:=BI_RLE4
  else if FRLECompress and (FBpp=8) then BFI.Compression:=BI_RLE8
  else BFI.Compression:=BI_RGB;
  BFI.ClrUsed:=0;
  try
    if FBpp<=8 then FillColorMap(Img); { sets colormap and ClrUsed}
    if FBpp=16 then Setup16bpp; { sets colormap with masks and Compression }
    RowSize:=0; { just to keep the compiler quiet. }
    case FBpp of
      1 : begin
            RowSize:=Img.Width div 8;
            if (Img.Width mod 8)<>0 then
              inc(RowSize);
          end;
      4 : begin
            RowSize:=Img.Width div 2;
            if (Img.Width mod 2)<>0 then
              inc(RowSize);
          end;
      8 : RowSize:=Img.Width;
     15 : RowSize:=Img.Width*2;
     16 : RowSize:=Img.Width*2;
     24 : RowSize:=Img.Width*3;
     32 : RowSize:=Img.Width*4;
    end;
    PadCount:=(4-(RowSize mod 4)) mod 4; { every row must end on 4 byte boundary }
    inc(RowSize,PadCount);
    BFI.SizeImage:=RowSize*Img.Height;

    SaveHeader(Stream,Img); { write the headers }
    for i:=0 to length(ColInfo)-1 do { write the palette (or the masks in 16bpp case) }
      Stream.Write(ColInfo[i],sizeof(TColorRGBA));

    GetMem(aLine,RowSize);
    try
      for Row:=Img.Height-1 downto 0 do
      begin
        i:=0; Col:=0;
        case FBpp of
          1 : while(Col<img.Width) do
              begin
                PByte(aline)[i]:=Pack1bpp(img,Col,Row); { increases Col by 8 each time }
                inc(i);
              end;
          4 : while(Col<img.Width) do
              begin
                PByte(aline)[i]:=Pack4bpp(img,Col,Row); { increases Col by 2 each time }
                inc(i);
              end;
          8 : for Col:=0 to img.Width-1 do
                PByte(aline)[Col]:=img.Pixels[Col,Row];
         15 : for Col:=0 to img.Width-1 do
                PWord(aline)[Col]:=PackWord555(img.colors[Col,Row]);
         16 : for Col:=0 to img.Width-1 do
                PWord(aline)[Col]:=PackWord565(img.colors[Col,Row]);
         24 : for Col:=0 to img.Width-1 do
                PColorRGB(aLine)[Col]:=FPColorToRGB(img.colors[Col,Row]);
         32 : for Col:=0 to img.Width-1 do
                PColorRGBA(aLine)[Col]:=FPColorToRGBA(img.colors[Col,Row]);
        end;
        { pad the scanline with zeros }
        for i:=RowSize-PadCount to RowSize-1 do
          Pbyte(aline)[i]:=0;

        if BFI.Compression=BI_RLE8 then CompressScanLineRLE8(aLine,Row,img.Width,Stream)
        else if BFI.Compression=BI_RLE4 then CompressScanLineRLE4(aLine,Row,img.Width,Stream)
        else Stream.Write(aLine[0],RowSize);

        inc(percentacc,4);
        if percentacc>=percentinterval then
        begin
          percent:=percent+(percentacc div percentinterval);
          percentacc:=percentacc mod percentinterval;
          Progress(psRunning,percent,false,Rect,'',continue);
          if not continue then exit;
        end;
      end;
      { If image is compressed we must fix the headers since we now know the size of the image }
      if BFI.Compression in [BI_RLE4,BI_RLE8] then 
      begin
        tmppos:=Stream.Position-StartPosition-BFH.bfOffset;
        BFI.SizeImage:=tmppos;          { set size of the image }
        tmppos:=Stream.Position;        { remember where we are }
        Stream.Position:=StartPosition; { rewind to the beginning }
        SaveHeader(Stream,Img);         { rewrite headers (this will update BFH.Size too) }
        Stream.Position:=tmppos;        { restore our position }
      end;
      Progress(psEnding,100,false,Rect,'',continue);
    finally
      FreeMem(aLine);
    end;
  finally
    setlength(ColInfo,0);
  end;
end;

constructor TFPWriterPNM.Create(aBitMapType:Integer);
  begin
    inherited Create;
    BitMapType:=aBitMapType;
  end;
procedure TFPWriterPNM.InternalWrite(Stream:TStream;Img:TFPCustomImage);
  function SaveHeader(stream:TStream):boolean;
    const
      MagicWords:Array[1..6]OF String[2]=('P1','P2','P3','P4','P5','P6');
    var
      PNMInfo:String;
      strWidth,StrHeight:String[15];
    begin
      SaveHeader:=false;
      with Img do
        begin
          Str(Img.Width,StrWidth);
          Str(Img.Height,StrHeight);
        end;
      PNMInfo:=Concat(MagicWords[BitMapType],#10,StrWidth,#32,StrHeight,#10);
      if BitMapType in [2,3,5,6]
      then
        PNMInfo:=Concat(PNMInfo,'255'#10);
      stream.seek(0,soFromBeginning);
      stream.Write(PNMInfo[1],Length(PNMInfo));
      SaveHeader := true;
    end;
  var
    Row,Coulumn,nBpLine,i:Integer;
    aColor:TFPColor;
    aLine:PByte;
    strCol:String[3];
  begin
    SaveHeader(Stream);
    case BitMapType of
      1:nBpLine:=Img.Width*2;{p p p}
      2:nBpLine:=Img.Width*4;{lll lll lll}
      3:nBpLine:=Img.Width*3*4;{rrr ggg bbb rrr ggg bbb}
      4:begin
          nBpLine:=Img.Width SHR 3;
          if(Img.Width AND $0F)<>0
          then
            Inc(nBpLine);
        end;
      5:nBpLine:=Img.Width;
      6:nBpLine:=Img.Width*3;
    end;
    GetMem(aLine,nBpLine);//3 extra byte for BMP 4Bytes alignement.
    for Row:=0 to img.Height-1 do
      begin
        FillChar(aLine^,nBpLine,0);
        for Coulumn:=0 to img.Width-1 do
          begin
            aColor:=img.Colors[Coulumn,Row];
            with aColor do
              case BitMapType of
                1:begin
                    if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                    then
                      aLine[2*Coulumn]:=Ord('1')
                    else
                      aLine[2*Coulumn]:=Ord('0');
                    aLine[2*Coulumn+1]:=32;
                  end;
                2:begin
                    Str(Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114))),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*Coulumn+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*Coulumn+i]:=32;
                  end;
                3:begin
                    Str(Hi(Red),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn)+i]:=32;
                    Str(Hi(Green),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+1)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+1)+i]:=32;
                    Str(Hi(Blue),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+2)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+2)+i]:=32;
                  end;
                4:if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                  then
                    aLine[Coulumn shr 3]:=aLine[Coulumn shr 3] or ($80 shr (Coulumn and $07));
                5:aLine[Coulumn]:=Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114)));
                6:begin
                    aLine[3*Coulumn]:=Hi(Red);
                    aLine[3*Coulumn+1]:=Hi(Green);
                    aLine[3*Coulumn+2]:=Hi(Blue);
                  end;
            end;
          end;
        Stream.Write(aLine^,nBpLine);
      end;
    FreeMem(aLine,nBpLine);
  end;

constructor TFPWriterXPM.create;
begin
  inherited create;
  PalChars := DefPalChars;
  FColorSize := 4;
end;

procedure TFPWriterXPM.SetColorSize (AValue : byte);
begin
  if AValue > 3 then
    FColorSize := 4
  else if AValue = 0 then
    FColorSize := 1
  else
    FColorSize := AValue;
end;

function TFPWriterXPM.ColorToHex (c:TFPColor) : string;
var r,g,b : word;
begin
  with c do
    begin
    r := red shr FColorShift;
    g := green shr FColorShift;
    b := blue shr FColorShift;
    end;
  result := format(FColorFormat,[r,g,b]);
end;

procedure TFPWriterXPM.InternalWrite (Str:TStream; Img:TFPCustomImage);
var p, l : TStringList;
    c, len, r, t : integer;
    TmpPalette, Palette: TFPPalette;
  procedure BuildPaletteStrings;
  var r,c,e : integer;
    procedure MakeCodes (const head:string; charplace:integer);
    var r : integer;
    begin
      r := 1;
      dec (charplace);
      while (r <= e) and (c >= 0) do
        begin
        if Charplace > 0 then
          MakeCodes (head+PalChars[r],charplace)
        else begin
          p.Add (head+PalChars[r]);
          dec(c);
        end;
        inc (r);
        end;
    end;
  begin
    // Calculate length of codes
    len := 1;
    e := length(PalChars);
    r := e;
    c := Palette.count;
    while (r <= c) do
      begin
      inc (len);
      r := r * e;
      end;
    MakeCodes ('',len);
  end;
  procedure InitConsts;
  var fmt : string;
  begin
    fmt := inttostr(FColorSize);
    fmt := '%'+fmt+'.'+fmt+'x';
    FColorFormat := fmt+fmt+fmt;
    case FColorSize of
      1 : FColorShift := 12;
      2 : FColorShift := 8;
      3 : FColorShift := 4;
      else FColorShift := 0;
    end;
  end;
var s : string;
begin
  l := TStringList.Create;
  p := TStringList.Create;
  TmpPalette := nil;
  try
    l.Add ('/* XPM */');
    l.Add ('static char *graphic[] = {');
    Palette := img.palette;
    if not Assigned(Palette) then begin
      TmpPalette := TFPPalette.Create(0);
      TmpPalette.Build(Img);
      Palette := TmpPalette;
    end;
    c := Palette.count;
    BuildPaletteStrings;
    l.add (format('"%d %d %d %d",',[img.width,img.height,c,len]));
    InitConsts;
    for r := 0 to c-1 do
      begin
      if Palette[r] <> colTransparent then
        l.Add (format('"%s c #%s",',[p[r],ColorToHex(Palette.color[r])]))
      else
        l.Add (format('"%s c None",',[p[r]]));
      end;
    for r := 0 to img.Height-1 do
      begin
      s := '';
      for t := 0 to img.Width-1 do
        if Assigned(TmpPalette) then
          s := s + p[TmpPalette.IndexOf(img.Colors[t,r])]
        else
          s := s + p[img.pixels[t,r]];
      s := '"'+s+'"';
      if r < img.Height-1 then
        s := s + ',';
      l.Add (s);
      end;
    l.Add ('};');
  finally
    TmpPalette.Free;
    l.SaveToStream (Str);
    p.Free;
    l.Free;
  end;
end;


function TFPWriterBMP.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  Result:=False;
  with BFI do
    begin
    Size:=sizeof(TBitMapInfoHeader);
    Width:=Img.Width;
    Height:=Img.Height;
    Planes:=1;
    if FBpp=15 then BitCount:=16
    else BitCount:=FBpp;
    XPelsPerMeter:=100;
    YPelsPerMeter:=100;
    ClrImportant:=0;
    end;
  with BFH do
    begin
    bfType:=BMmagic;//'BM'
    bfOffset:=sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader)+length(ColInfo)*4;
    bfReserved:=0;
    bfSize:=bfOffset+BFI.SizeImage;
    end;
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(BFH);
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  StartPosition:=Stream.Position;
  Stream.Write(bfh,sizeof(TBitMapFileHeader));
  Stream.Write(bfi,sizeof(TBitMapInfoHeader));
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(BFH);
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  Result:=true;
end;

{ This code is rather ugly and difficult to read, but compresses better than gimp.
  Brief explanation:
  A repetition is good if it's made of 3 elements at least: we have 2 bytes instead of 1. Let's call this a 
  "repetition" or "true repetition".
  So we start finding the first repetition from current position.
  Once found, we must decide how to handle elements between current position (i) and the repetition position (j)
  if j-i = 0 we are on the repetition, so we encode it
  if j-i = 1 there is only one pixel. We can't do anything but encode it as a repetition of 1 element.
  if j-i = 2 we have two pixels. These can be a couple (a repetition of 2 elements) or 2 singles
             (2 repetitions of 1 element)
  if j-i > 2 we have two choices. In fact, we must consider that absolute mode is 2 bytes + length of chunk.
             A repetition is always 2 bytes, so for 1 element we leak 1 byte, while for 2 elements we don't leak
             any byte.
             So if we have at most 1 single this means that everything else is made up of couples: it's best to
             use repetitions so that we leak 0 to 1 byte.
             If we have 2 singles or more it's better to use absolute mode, since we leak 2 bytes always,
             without regard to the size of chunk. }

procedure TFPWriterBMP.CompressScanLineRLE8(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles : integer;
    prev,tmp : byte;
begin
  i:=0;
  while (i<Width) do
  begin
    { let's see how bytes are disposed, so that we can choose the best way to compress }
    couples:=0; singles:=1;
    prev:=Aline[i];
    j:=i+1;
    while ((j<Width) and ((j-i)<255)) do
    begin
      if Aline[j]=prev then { this is a couple at least }
      begin
        dec(singles); { so the previous one wasn't a single }
        if (((j+1)<Width) and (Aline[j+1]=prev)) then { at least three equal items, it's a repetition }
        begin
          dec(j); { repetition starts at j-1, since j is the middle pixel and j+1 is the third pixel }
          break;
        end
        else inc(couples) { ok it's a couple }
      end
      else inc(singles); { this is a single if next isn't a couple }
      prev:=Aline[j];
      inc(j);
    end;

    { ok, now that we know more about byte disposition we write data }
    case (j-i) of
      0 : begin { there is a repetition with count>=3 }
            prev:=Aline[i];
            j:=i+1;
            while ((j<Width) and ((j-i)<255)) do
            begin
              if Aline[j]<>prev then break;
              inc(j);
            end;
            tmp:=j-i;
            Stream.Write(tmp,1);
            Stream.Write(prev,1);
          end;
      1 : begin { single value: we write a repetition of 1 }
            tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline[i],1);
          end;
      2 : begin
            if couples=1 then { a couple: we write a repetition of 2 }
            begin
              tmp:=2;
              Stream.Write(tmp,1);
              Stream.Write(Aline[i],1);
            end
            else { two singles: we write two repetitions of 1 each }
            begin
              tmp:=1;
              Stream.Write(tmp,1);
              Stream.Write(Aline[i],1);
              Stream.Write(tmp,1);
              Stream.Write(Aline[i+1],1);
            end;
          end;
      else { here we have two choices }
      begin
        if singles>1 then { it's cheaper to use absolute mode }
        begin
          tmp:=0; Stream.Write(tmp,1);   { escape }
          tmp:=j-i; Stream.Write(tmp,1); { number of pixels in absolute mode }
          Stream.Write(Aline[i],j-i);    { write these pixels... }
          if ((tmp mod 2)<>0) then       { we must end on a 2-byte boundary }
          begin
            tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
          end;
        end
        else { they're nearly all couples, don't use absolute mode }
        begin
          k:=i;
          while (k<j) do
          begin
            if ((k+1<j) and (Aline[k]=Aline[k+1])) then
            begin
              tmp:=2;
              inc(k);
            end
            else tmp:=1;
            Stream.Write(tmp,1);
            Stream.Write(Aline[k],1);
            inc(k);
          end;
        end;
      end;
    end;
    i:=j;
  end;
  tmp:=0; Stream.Write(tmp,1); { escape }
  if Row=0 then { last line, end of file }
    tmp:=1;
  Stream.Write(tmp,1);
end;

{ Ok, this is even uglier than the RLE8 version above, and this time gimp compresses better :\
  Differences with RLE8: repetition count is pixel-relative, not byte-relative, but repetition data is made
  of 2 pixels. So you have a repetition when you have pixels repeated in an alternate way, even if you can do
  something like:
  01E0 => E
  0316 => 161.
  A repetition is good if it's made of five elements at least (2 bytes instead of 3).
  In rle4 we consider "single" either a single nibble or 2 (a byte), while a couple is a repetition of 3 or 4
  elements. }

procedure TFPWriterBMP.CompressScanLineRLE4(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
var i, j, k, couples, singles, lastsingle : integer;
    prev1, prev2, prev : word;
    tmp : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  getmem(nibline,width);
  try
    k:=(Width div 2) + (Width mod 2);
    i:=0;
    while (i<k) do
    begin
      nibline[i*2]:=aline[i] shr 4;
      nibline[i*2+1]:=aline[i] and $F;
      inc(i);
    end;
    i:=0;
    while (i<Width) do
    begin
      { let's see how nibbles are disposed, so that we can choose the best way to compress }
      couples:=0; singles:=1; lastsingle:=-10;
      prev1:=nibline[i];
      prev2:=nibline[i+1];
      j:=i+2;
      while ((j<Width) and ((j-i)<255)) do
      begin
        if nibline[j]=prev1 then { this is a half-couple at least (repetition of 3) }
        begin
          dec(singles); { so the previous one wasn't a single }
          if (((j+1)<Width) and (nibline[j+1]=prev2)) then { at least a couple (repetition of 4) }
          begin
            if (((j+2)<Width) and (nibline[j+2]=prev1)) then { at least a repetition of 5, good }
            begin
              dec(j,2); { repetition starts at j-2: prev1 prev2 prev1* prev2 prev1, we are here * }
              break;
            end
            else
            begin { ok it's a couple }
              inc(couples);
              if (j-i)=254 then { in this rare case, j-i becomes 256. So, force a half-couple and exit }
              begin
                inc(j);
                break;
              end;
              prev1:=256; { this is a couple, don't consider these positions in further scanning }
              prev2:=256;
              inc(j,2);
              continue;
            end
          end
          else
            begin { ok it's a half-couple }
            inc(couples);
            prev:=256; //this is a half-couple, don't consider this position in further scanning.
          end;
        end
        else
        begin
          if lastsingle<>(j-1) then
          begin
            inc(singles); { this is a single if next isn't a couple }
            lastsingle:=j;
          end;
          prev:=nibline[j];
        end;
        prev1:=prev2;
        prev2:=prev;
        even:=not even;
        inc(j);
      end;
      if j>Width then j:=Width; { if j was Width-1 loop was skipped and j is Width+1, so we fix it }

      { ok, now that we know more about byte disposition we write data }
      case (j-i) of
        0 : begin { there is a repetition with count>=5 }
              even:=true;
              prev1:=nibline[i];
              prev2:=nibline[i+1];
              j:=i+2;
              while ((j<Width) and ((j-i)<255)) do
              begin
                if even then if nibline[j]<>prev1 then break;
                if not even then if nibline[j]<>prev2 then break;
                even:=not even;
                inc(j);
              end;
              tmp:=j-i;
              Stream.Write(tmp,1);
              prev:=(prev1 shl 4) + (prev2 and $F);
              tmp:=prev;
              Stream.Write(tmp,1);
            end;
        1 : begin { single value: we write a repetition of 1 }
              tmp:=1;
              Stream.Write(tmp,1);
              tmp:=nibline[i] shl 4;
              Stream.Write(tmp,1);
            end;
        2 : begin { 2 singles in the same byte: we write a repetition of 2 }
              tmp:=2;
              Stream.Write(tmp,1);
              tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
              Stream.Write(tmp,1);
            end;
        3 : begin
              if couples=1 then { a couple: we write a repetition of 3 }
              begin
                tmp:=3;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 and 1 respectively }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=1;
                Stream.Write(tmp,1);
                tmp:=nibline[i+2] shl 4;
                Stream.Write(tmp,1);
              end;
            end;
        4 : begin
              if singles=0 then { a couple: we write a repetition of 4 }
              begin
                tmp:=4;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
              end
              else
              begin { 2 singles, 2 repetitions of 2 each }
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i] shl 4) + (nibline[i+1] and $F);
                Stream.Write(tmp,1);
                tmp:=2;
                Stream.Write(tmp,1);
                tmp:=(nibline[i+2] shl 4) + (nibline[i+3] and $F);
                Stream.Write(tmp,1);
              end;
            end;
        else { here we have two choices }
        begin
          if singles>1 then { it's cheaper to use absolute mode }
          begin
            tmp:=0; Stream.Write(tmp,1);    { escape }
            tmp:=j-i; Stream.Write(tmp,1);  { number of pixels in absolute mode }
            k:=i;
            while (k<j) do                  { write these pixels... }
            begin
              tmp:=nibline[k] shl 4;
              inc(k);
              if k<j then
              begin
                tmp:=tmp+(nibline[k] and $F);
                inc(k);
              end;
              Stream.Write(tmp,1);
            end;
            k:=j-i;
            k:=k+(k mod 2);
            if (k mod 4)<>0 then            { we must end on a 2-byte boundary }
            begin
              tmp:=0; Stream.Write(tmp,1); { so pad with an additional zero }
            end;
          end
          else { they're nearly all couples, don't use absolute mode }
          begin
            k:=i;
            while (k<j) do
            begin
              if ((k+2<j) and (nibline[k]=nibline[k+2])) then
              begin
                if ((k+3<j) and (nibline[k+1]=nibline[k+3])) then tmp:=4
                else tmp:=3;
              end
              else
              begin
                if (k+1>=j) then tmp:=1
                else if ((k+3<j) and (nibline[k+1]=nibline[k+3])) then tmp:=1
                else tmp:=2;
              end;
              Stream.Write(tmp,1);
              prev:=tmp;
              tmp:=nibline[k] shl 4;
              if tmp<>1 then tmp:=tmp+(nibline[k+1] and $F);
              Stream.Write(tmp,1);
              inc(k,prev);
            end;
          end;
        end;
      end;
      i:=j;
    end;
    tmp:=0; Stream.Write(tmp,1); { escape }
    if Row=0 then { last line, end of file }
      tmp:=1;
    Stream.Write(tmp,1);
  finally
    FreeMem(nibline);
  end;
end;


function TFPWriterBMP.PackWord565(const col : TFPColor) : word;
var tmpcol : TColorRGB;
    tmpr, tmpg, tmpb : word;
begin
  tmpcol:=FPColorToRGB(col);
  tmpb:=tmpcol.b shr 3;
  tmpg:=tmpcol.g and $FC; tmpg:= tmpg shl 3;
  tmpr:=tmpcol.r and $F8; tmpr:= tmpr shl 8;
  tmpb:= tmpr or tmpg or tmpb;
  {$IFDEF ENDIAN_BIG}
  tmpb:=swap(tmpb);
  {$ENDIF}
  Result:=tmpb;
end;

{ First pixel in the most significant nibble, second one in LSN. If we are at the end of the line,
  pad with zero }
function TFPWriterBMP.Pack4bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
var b : byte;
begin
  b:=(img.Pixels[Col,Row] and $F) shl 4;
  if Col<img.Width-1 then
  begin
    inc(Col);
    b:=b + (img.Pixels[Col,Row] and $F);
  end;
  Result:=b;
  inc(col);
end;

{ First pixel in the most significant bit, last one in LSN. If we are at the end of the line,
  pad with zero }
function TFPWriterBMP.Pack1bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
var b : byte;
    sh : shortint;
begin
  b:=0;
  sh:=7;
  while ((Col<Img.Width) and (sh>=0)) do
  begin
    if img.Pixels[Col,Row]<>0 then { set this bit }
      b:=b+(1 shl sh);
    dec(sh);
    inc(Col);
  end;
  Result:=b;
end;


{ True 16 bit color is 5 bits red, 6 bits green and 5 bits blue.
  Compression must be set to BI_BITFIELDS and we must specify masks for red, green and blue.
  16 bit without compression and masks is 5 bits per channel, so it's 15 bit even if in the header we
  must write 16.
  It's possible to provide custom masks but this is not compatible with windows9x, so we use 555 for 15 bit
  and 565 for 16 bit.
  Masks are longwords stored in the palette instead of palette entries (which are 4 bytes long too, with
  components stored in following order: B G R A. Since we must write a low-endian longword, B is LSB and A
  is the MSB).
  We must write first red mask, then green and then blue.

  This sounds terribly confusing, if you don't understand take a look at
  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/bitmaps_1rw2.asp
   }
procedure TFPWriterBMP.Setup16bpp;
var col : TColorRGBA;
begin
  BFI.Compression:=BI_BITFIELDS;
  setlength(ColInfo,3);
  {      A R G B
  r := $0000F800
  g := $000007E0
  b := $0000001F
  }
  col.A:=0; Col.R:=0; { These are 0 for all the three masks}
  { Red Mask }
  Col.G:=$F8; Col.B:=0;
  ColInfo[0]:=Col;
  { Green Mask }
  Col.G:=$07; Col.B:=$E0;
  ColInfo[1]:=Col;
  { Blue Mask }
  Col.G:=$00; Col.B:=$1F;
  ColInfo[2]:=Col;
end;

{ 16 bit bpp with 555 packing (that is, 15 bit color)
  This is bit dislocation:
  0RRR RRGG GGGB BBBB  }

function TFPWriterBMP.PackWord555(const col : TFPColor) : word;
var tmpcol : TColorRGB;
    tmpr, tmpg, tmpb : word;
begin
  tmpcol:=FPColorToRGB(col);
  tmpb:=tmpcol.b shr 3;
  tmpg:=tmpcol.g and $F8; tmpg:= tmpg shl 2;
  tmpr:=tmpcol.r and $F8; tmpr:= tmpr shl 7;
  tmpb:= tmpr or tmpg or tmpb;
  {$IFDEF ENDIAN_BIG}
  tmpb:=swap(tmpb);
  {$ENDIF}
  Result:=tmpb;
end;

{ 16 bit bpp with 565 packing )
  This is bit dislocation:
  RRRR RGGG GGGB BBBB  }


constructor TFPWriterBMP.create;
begin
  inherited create;
  FBpp:=24;
  FRleCompress:=false;
end;


procedure TFPWriterBMP.SetColorSize (AValue : byte);
begin
  SetBpp(AValue*8);
end;

function TFPWriterBMP.GetColorSize : byte;
begin
  if FBpp<>15 then Result:=FBpp div 8
  else Result:=2;
end;

procedure TFPWriterBMP.SetBpp (const abpp : byte);
begin
  if not (abpp in [1,4,8,15,16,24,32]) then
    raise FPImageException.Create('Invalid color depth');
  FBpp:=abpp;
end;

procedure TFPWriterBMP.FillColorMap(Img : TFPCustomImage);
var BadPalette : boolean;
    i : integer;
begin
  BadPalette:=false;
  if not Img.UsePalette then BadPalette:=true
  else if Img.Palette.Count>(1 shl FBpp) then BadPalette:=true;
  if BadPalette then 
    raise FPImageException.Create('Image palette is too big or absent');
  setlength(ColInfo,Img.Palette.Count);
  BFI.ClrUsed:=Img.Palette.Count;
  for i:=0 to BFI.ClrUsed-1 do
  begin
    ColInfo[i]:=FPColorToRGBA(Img.Palette.Color[i]);
    ColInfo[i].A:=0;
  end;
end;


procedure TFPReaderPNM.ReadHeader(Stream : TStream);

Var
  C : Char;

begin
  Stream.ReadBuffer(C,1);
  If (C<>'P') then
    Raise Exception.Create('Not a valid PNM image.');
  Stream.ReadBuffer(C,1);
  FBitmapType:=Ord(C)-Ord('0');
  If Not (FBitmapType in [1..6]) then
    Raise Exception.CreateFmt('Unknown PNM subtype : %s',[C]);
  FWidth:=ReadInteger(Stream);
  FHeight:=ReadInteger(Stream);
  if FBitMapType in [1,4]
  then
    FMaxVal:=1
  else
    FMaxVal:=ReadInteger(Stream);
  If (FWidth<=0) or (FHeight<=0) or (FMaxVal<=0) then
    Raise Exception.Create('Invalid PNM header data');
  case FBitMapType of
    1: FBitPP := SizeOf(Word);
    2: FBitPP := 8 * SizeOf(Word);   // Grayscale (text)
    3: FBitPP := 8 * SizeOf(Word)*3; // RGB (text)
    4: FBitPP := 1; // 1bit PP (row)
    5: If (FMaxval>255) then   // Grayscale (raw);
         FBitPP:= 8 * 2
       else
         FBitPP:= 8;
    6: if (FMaxVal>255) then    // RGB (raw)
         FBitPP:= 8 * 6
       else
         FBitPP:= 8 * 3
  end;
//  Writeln(FWidth,'x',Fheight,' Maxval: ',FMaxVal,' BitPP: ',FBitPP);
end;

procedure TFPReaderPNM.InternalRead(Stream:TStream;Img:TFPCustomImage);

var
  Row:Integer;

begin
  ReadHeader(Stream);
  Img.SetSize(FWidth,FHeight);
  FScanLineSize:=FBitPP*((FWidth+7)shr 3);
  GetMem(FScanLine,FScanLineSize);
  try
    for Row:=0 to img.Height-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
  finally
    FreeMem(FScanLine);
  end;
end;

procedure TFPReaderPNM.ReadScanLine(Row : Integer; Stream:TStream);

Var
  P : PWord;
  I,j : Integer;

begin
  Case FBitmapType of
    1 : begin
        P:=PWord(FScanLine);
        For I:=0 to ((FWidth+7)shr 3)-1 do
          begin
            P^:=0;
            for j:=0 to 7 do
              P^:=(P^ shr 1)or ReadInteger(Stream);
            Inc(P);
          end;
        end;
    2 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream);
          Inc(P);
          end;
        end;
    3 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream); // Red
          Inc(P);
          P^:=ReadInteger(Stream); // Green
          Inc(P);
          P^:=ReadInteger(Stream); // Blue;
          Inc(P)
          end;
        end;
    4,5,6 : Stream.ReadBuffer(FScanLine^,FScanLineSize);
    end;
end;


procedure TFPReaderPNM.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  C : TFPColor;
  L : Cardinal;
  Scale: Cardinal;

  function ScaleByte(B: Byte):Word;
  begin
    if FMaxVal = 255 then
      Result := (B shl 8) or B { As used for reading .BMP files }
    else { Mimic the above with multiplications }
      Result := (B*(FMaxVal+1) + B) * 65535 div Scale;
  end;

  function ScaleWord(W: Word):Word;
  begin
    if FMaxVal = 65535 then
      Result := W
    else { Mimic the above with multiplications }
      Result := Int64(W*(FMaxVal+1) + W) * 65535 div Scale;
  end;

  Procedure ByteBnWScanLine;

  Var
    P : PByte;
    I,j,x : Integer;

  begin
    P:=PByte(FScanLine);
    x:=7;
    For I:=0 to ((FWidth+7)shr 3)-1 do
      begin
      L:=P^;
      for j:=0 to 7 do
        begin
          if odd(L)
          then
            Img.Colors[x,Row]:=colBlack
          else
            Img.Colors[x,Row]:=colWhite;
          L:=L shr 1;
          dec(x);
        end;
      Inc(P);
      Inc(x,16);
      end;
  end;

  Procedure WordGrayScanLine;

  Var
    P : PWord;
    I : Integer;

  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleWord(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure WordRGBScanLine;

  Var
    P : PWord;
    I : Integer;

  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleWord(P^);
      Inc(P);
      C.Green:=ScaleWord(P^);
      Inc(P);
      C.Blue:=ScaleWord(P^);
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure ByteGrayScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleByte(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure ByteRGBScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleByte(P^);
      Inc(P);
      C.Green:=ScaleByte(P^);
      Inc(P);
      C.Blue:=ScaleByte(P^);
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

begin
  C.Alpha:=AlphaOpaque;
  Scale := FMaxVal*(FMaxVal+1) + FMaxVal;
  Case FBitmapType of
    1 : ;
    2 : WordGrayScanline;
    3 : WordRGBScanline;
    4 : ByteBnWScanLine;
    5 : If FBitPP=8 then
          ByteGrayScanLine
        else
          WordGrayScanLine;
    6 : If FBitPP=24 then
          ByteRGBScanLine
        else
          WordRGBScanLine;
    end;
end;


procedure TFPReaderBMP.InternalRead(Stream:TStream; Img:TFPCustomImage);

Var
  Row, i, pallen : Integer;
  BadCompression : boolean;
begin
  Rect.Left:=0; Rect.Top:=0; Rect.Right:=0; Rect.Bottom:=0;
  continue:=true;
  Progress(psStarting,0,false,Rect,'',continue);
  if not continue then exit;
  Stream.Read(BFI,SizeOf(BFI));
  {$IFDEF ENDIAN_BIG}
  SwapBMPInfoHeader(BFI);
  {$ENDIF}
  { This will move past any junk after the BFI header }
  Stream.Position:=Stream.Position-SizeOf(BFI)+BFI.Size;
  with BFI do
  begin
    BadCompression:=false;
    if ((Compression=BI_RLE4) and (BitCount<>4)) then BadCompression:=true;
    if ((Compression=BI_RLE8) and (BitCount<>8)) then BadCompression:=true;
    if ((Compression=BI_BITFIELDS) and (not (BitCount in [16,32]))) then BadCompression:=true;
    if not (Compression in [BI_RGB..BI_BITFIELDS]) then BadCompression:=true;
    if BadCompression then
      raise FPImageException.Create('Bad BMP compression mode');
    TopDown:=(Height<0);
    Height:=abs(Height);
    if (TopDown and (not (Compression in [BI_RGB,BI_BITFIELDS]))) then
      raise FPImageException.Create('Top-down bitmaps cannot be compressed');
    Img.SetSize(0,0);
    if BitCount<=8 then
    begin
      Img.UsePalette:=true;
      Img.Palette.Clear;
    end
    else Img.UsePalette:=false;
    Case BFI.BitCount of
      1 : { Monochrome }
        SetupRead(2,Width,Stream);
      4 :
        SetupRead(16,Width*4,Stream);
      8 :
        SetupRead(256,Width*8,Stream);
      16 :
        SetupRead(0,Width*8*2,Stream);
      24:
        SetupRead(0,Width*8*3,Stream);
      32:
        SetupRead(0,Width*8*4,Stream);
    end;
  end;
  Try
    { Note: it would be better to Fill the image palette in setupread instead of creating FPalette.
      FPalette is indeed useless but we cannot remove it since it's not private :\ }
    pallen:=0;
    if BFI.BitCount<=8 then
      if BFI.ClrUsed>0 then pallen:=BFI.ClrUsed
      else pallen:=(1 shl BFI.BitCount);
    if pallen>0 then
    begin
      Img.Palette.Count:=pallen;
      for i:=0 to pallen-1 do
        Img.Palette.Color[i]:=FPalette[i];
    end;
    Img.SetSize(BFI.Width,BFI.Height);

    percent:=0;
    percentinterval:=(Img.Height*4) div 100;
    if percentinterval=0 then percentinterval:=$FFFFFFFF;
    percentacc:=0;

    DeltaX:=-1; DeltaY:=-1;
      if TopDown then
        for Row:=0 to Img.Height-1 do { A rare case of top-down bitmap! }
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end
      else
        for Row:=Img.Height-1 downto 0 do
        begin
          ReadScanLine(Row,Stream); // Scanline in LineBuf with Size ReadSize.
          WriteScanLine(Row,Img);
          if not continue then exit;
        end;
    Progress(psEnding,100,false,Rect,'',continue);
  finally
    FreeBufs;
  end;
end;

procedure TFPReaderBMP.ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
var i,j : integer;
    b0, b1 : byte;
begin
  i:=0;
  while true do
  begin
    { let's see if we must skip pixels because of delta... }
    if DeltaY<>-1 then
    begin
      if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
      else j:=ReadSize;            { else skip up to the end of this line }
      while (i<j) do
        begin
          LineBuf[i]:=0;
          inc(i);
        end;

      if Row=DeltaY then { we don't need delta anymore }
        DeltaY:=-1
      else break; { skipping must continue on the next line, we are finished here }
    end;

    Stream.Read(b0,1); Stream.Read(b1,1);
    if b0<>0 then { number of repetitions }
    begin
      if b0+i>ReadSize then
        raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
      j:=i+b0;
      while (i<j) do
      begin
        LineBuf[i]:=b1;
        inc(i);
      end;
    end
    else
      case b1 of 
        0: break; { end of line }
        1: break; { end of file }
        2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
             Stream.Read(b0,1); Stream.Read(b1,1);
             DeltaX:=i+b0; DeltaY:=Row+b1;
           end
        else begin { absolute mode }
               if b1+i>ReadSize then
                 raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
               Stream.Read(LineBuf[i],b1);
               inc(i,b1);
               { aligned on 2 bytes boundary: every group starts on a 2 bytes boundary, but absolute group
                 could end on odd address if there is a odd number of elements, so we pad it  }
               if (b1 mod 2)<>0 then Stream.Seek(1,soFromCurrent); 
             end;
      end;
  end;
end;

procedure TFPReaderBMP.ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
var i,j,tmpsize : integer;
    b0, b1 : byte;
    nibline : pbyte; { temporary array of nibbles }
    even : boolean;
begin
  tmpsize:=ReadSize*2; { ReadSize is in bytes, while nibline is made of nibbles, so it's 2*readsize long }
  getmem(nibline,tmpsize);
  if nibline=nil then
    raise FPImageException.Create('Out of memory');
  try
    i:=0;
    while true do
    begin
      { let's see if we must skip pixels because of delta... }
      if DeltaY<>-1 then
      begin
        if Row=DeltaY then j:=DeltaX { If we are on the same line, skip till DeltaX }
        else j:=tmpsize;            { else skip up to the end of this line }
        while (i<j) do
          begin
            NibLine[i]:=0;
            inc(i);
          end;

        if Row=DeltaY then { we don't need delta anymore }
          DeltaY:=-1
        else break; { skipping must continue on the next line, we are finished here }
      end;

      Stream.Read(b0,1); Stream.Read(b1,1);
      if b0<>0 then { number of repetitions }
      begin
        if b0+i>tmpsize then
          raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
        even:=true;
        j:=i+b0;
        while (i<j) do
        begin
          if even then NibLine[i]:=(b1 and $F0) shr 4
          else NibLine[i]:=b1 and $0F;
          inc(i);
          even:=not even;
        end;
      end
      else
        case b1 of 
          0: break; { end of line }
          1: break; { end of file }
          2: begin  { Next pixel position. Skipped pixels should be left untouched, but we set them to zero }
               Stream.Read(b0,1); Stream.Read(b1,1);
               DeltaX:=i+b0; DeltaY:=Row+b1;
             end
          else begin { absolute mode }
                 if b1+i>tmpsize then
                   raise FPImageException.Create('Bad BMP RLE chunk at row '+inttostr(row)+', col '+inttostr(i)+', file offset $'+inttohex(Stream.Position,16) );
                 j:=i+b1;
                 even:=true;
                 while (i<j) do
                 begin
                   if even then
                   begin
                     Stream.Read(b0,1);
                     NibLine[i]:=(b0 and $F0) shr 4;
                   end
                   else NibLine[i]:=b0 and $0F;
                   inc(i);
                   even:=not even;
                 end;
               { aligned on 2 bytes boundary: see rle8 for details  }
                 b1:=b1+(b1 mod 2);
                 if (b1 mod 4)<>0 then Stream.Seek(1,soFromCurrent);
               end;
        end;
    end;
    { pack the nibline into the linebuf }
    for i:=0 to ReadSize-1 do
      LineBuf[i]:=(NibLine[i*2] shl 4) or NibLine[i*2+1];
  finally
    FreeMem(nibline)
  end;
end;

procedure TFPReaderBMP.ReadScanLine(Row : Integer; Stream : TStream);
begin
  if BFI.Compression=BI_RLE8 then ExpandRLE8ScanLine(Row,Stream)
  else if BFI.Compression=BI_RLE4 then ExpandRLE4ScanLine(Row,Stream)
  else Stream.Read(LineBuf[0],ReadSize);
end;

procedure TFPReaderBMP.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  Column : Integer;

begin
  Case BFI.BitCount of
   1 :
     for Column:=0 to Img.Width-1 do
       if ((LineBuf[Column div 8] shr (7-(Column and 7)) ) and 1) <> 0 then
         img.Pixels[Column,Row]:=1
       else
         img.Pixels[Column,Row]:=0;
   4 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=(LineBuf[Column div 2] shr (((Column+1) and 1)*4)) and $0f;
   8 :
      for Column:=0 to img.Width-1 do
        img.Pixels[Column,Row]:=LineBuf[Column];
   16 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=ExpandColor(PWord(LineBuf)[Column]);
   24 :
      for Column:=0 to img.Width-1 do
        img.colors[Column,Row]:=RGBToFPColor(PColorRGB(LineBuf)[Column]);
   32 :
      for Column:=0 to img.Width-1 do
        if BFI.Compression=BI_BITFIELDS then
          img.colors[Column,Row]:=ExpandColor(PLongWord(LineBuf)[Column])
        else
          img.colors[Column,Row]:=RGBAToFPColor(PColorRGBA(LineBuf)[Column]);
    end;

    inc(percentacc,4);
    if percentacc>=percentinterval then
    begin
      percent:=percent+(percentacc div percentinterval);
      percentacc:=percentacc mod percentinterval;
      Progress(psRunning,percent,false,Rect,'',continue);
    end;
end;

function  TFPReaderBMP.InternalCheck (Stream:TStream) : boolean;

var
  BFH:TBitMapFileHeader;
begin
  stream.Read(BFH,SizeOf(BFH));
  {$IFDEF ENDIAN_BIG}
  SwapBMPFileHeader(BFH);
  {$ENDIF}
  With BFH do
    Result:=(bfType=BMmagic); // Just check magic number
end;


Constructor TFPReaderBMP.create;

begin
  inherited create;
end;

Destructor TFPReaderBMP.Destroy;

begin
  FreeBufs;
  inherited destroy;
end;

Procedure TFPReaderBMP.FreeBufs;

begin
  If (LineBuf<>Nil) then
    begin
    FreeMem(LineBuf);
    LineBuf:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FPalette:=Nil;
    end;
end;

{ Counts how many bits are set }
function TFPReaderBMP.CountBits(Value : byte) : shortint;
var i,bits : shortint;
begin
  bits:=0;
  for i:=0 to 7 do
  begin
    if (value mod 2)<>0 then inc(bits);
    value:=value shr 1;
  end;
  Result:=bits;
end;

{ If compression is bi_bitfields, there could be arbitrary masks for colors.
  Although this is not compatible with windows9x it's better to know how to read these bitmaps
  We must determine how to switch the value once masked
  Example: 0000 0111 1110 0000, if we shr 5 we have 00XX XXXX for the color, but these bits must be the
  highest in the color, so we must shr (5-(8-6))=3, and we have XXXX XX00.
  A negative value means "shift left"  }
function TFPReaderBMP.ShiftCount(Mask : longword) : shortint;
var tmp : shortint;
begin
  tmp:=0;
  if Mask=0 then
  begin
    Result:=0;
    exit;
  end;

  while (Mask mod 2)=0 do { rightmost bit is 0 }
  begin
    inc(tmp);
    Mask:= Mask shr 1;
  end;
  tmp:=tmp-(8-CountBits(Mask and $FF));
  Result:=tmp;
end;

function TFPReaderBMP.ExpandColor(value : longword) : TFPColor;
var tmpr, tmpg, tmpb : longword;
    col : TColorRGB;
begin
  {$IFDEF ENDIAN_BIG}
  value:=swap(value);
  {$ENDIF}
  tmpr:=value and RedMask;
  tmpg:=value and GreenMask;
  tmpb:=value and BlueMask;
  if RedShift < 0 then col.R:=byte(tmpr shl (-RedShift))
  else col.R:=byte(tmpr shr RedShift);
  if GreenShift < 0 then col.G:=byte(tmpg shl (-GreenShift))
  else col.G:=byte(tmpg shr GreenShift);
  if BlueShift < 0 then col.B:=byte(tmpb shl (-BlueShift))
  else col.B:=byte(tmpb shr BlueShift);
  Result:=RGBToFPColor(col);
end;

procedure TFPReaderBMP.SetupRead(nPalette, nRowBits: Integer; Stream : TStream);

var
  ColInfo: ARRAY OF TColorRGBA;
  i: Integer;

begin
  if ((BFI.Compression=BI_RGB) and (BFI.BitCount=16)) then { 5 bits per channel, fixed mask }
  begin
    RedMask:=$7C00; RedShift:=7;
    GreenMask:=$03E0; GreenShift:=2;
    BlueMask:=$001F; BlueShift:=-3;
  end
  else if ((BFI.Compression=BI_BITFIELDS) and (BFI.BitCount in [16,32])) then { arbitrary mask }
  begin
    Stream.Read(RedMask,4);
    Stream.Read(GreenMask,4);
    Stream.Read(BlueMask,4);
    {$IFDEF ENDIAN_BIG}
    RedMask:=swap(RedMask);
    GreenMask:=swap(GreenMask);
    BlueMask:=swap(BlueMask);
    {$ENDIF}
    RedShift:=ShiftCount(RedMask);
    GreenShift:=ShiftCount(GreenMask);
    BlueShift:=ShiftCount(BlueMask);
  end
  else if nPalette>0 then
    begin
    GetMem(FPalette, nPalette*SizeOf(TFPColor));
    SetLength(ColInfo, nPalette);
    if BFI.ClrUsed>0 then
      Stream.Read(ColInfo[0],BFI.ClrUsed*SizeOf(TColorRGBA))
    else // Seems to me that this is dangerous.
      Stream.Read(ColInfo[0],nPalette*SizeOf(TColorRGBA));
    for i := 0 to High(ColInfo) do
      FPalette[i] := RGBAToFPColor(ColInfo[i]);
    end
  else if BFI.ClrUsed>0 then { Skip palette }
    Stream.Position := Stream.Position + BFI.ClrUsed*SizeOf(TColorRGBA);
  ReadSize:=((nRowBits + 31) div 32) shl 2;
  GetMem(LineBuf,ReadSize);
end;


{ TFPPalette implementation}
{ TFPPalette }

constructor TFPPalette.create (ACount : integer);
begin
  inherited create;
  if aCount > 0 then
    getmem (FData, sizeof(TFPColor)*ACount)
  else
    FData := nil;
  FCapacity := ACount;
  SetCount (0);
end;

destructor TFPPalette.destroy;
begin
  if FCapacity > 0 then
    freemem (FData);
  inherited;
end;

procedure TFPPalette.Build (Img : TFPCustomImage);
var x,y : integer;
begin
  if (Img.Palette <> self) then
    begin
    Count := 0;
    for x := 0 to img.width-1 do
      for y := 0 to img.height-1 do
        IndexOf(img[x,y]);
    end;
end;

procedure TFPPalette.Copy(APalette: TFPPalette);
var
  x: integer;
begin
  if (APalette <> Self) then
  begin
    Self.Clear;
    for x := 0 to APalette.Count - 1 do
        Add(APalette.Color[x])
  end;
end;

procedure TFPPalette.Merge (pal : TFPPalette);
var r : integer;
begin
  for r := 0 to pal.count-1 do
    IndexOf (pal[r]);
end;

procedure TFPPalette.CheckIndex (index:integer);
begin
  if (index >= FCount) or (index < 0) then
    FPImgError (StrInvalidIndex,[ErrorText[StrPalette],index]);
end;

function TFPPalette.Add (const Value:TFPColor) : integer;
begin
  result := FCount;
  inc (FCount);
  if FCount > FCapacity then
    EnlargeData;
  FData^[result] := Value;
end;

procedure TFPPalette.SetColor (index:integer; const Value:TFPColor);
begin
  if index = FCount then
    Add (Value)
  else
    begin
    CheckIndex (index);
    FData^[index] := Value;
    end;
end;

function TFPPalette.GetColor (index:integer) : TFPColor;
begin
  CheckIndex (index);
  result := FData^[index];
end;

function TFPPalette.GetCount : integer;
begin
  result := FCount;
end;

procedure TFPPalette.EnlargeData;
var old : integer;
    NewData : PFPColorArray;
begin
  old := FCapacity;
  if FCapacity <= 16 then
    FCapacity := 32
  else if FCapacity <= 128 then
    FCapacity := 256
  else
    // MG: changed to exponential growth
    inc (FCapacity, FCapacity);
  GetMem (NewData, sizeof(TFPColor)*FCapacity);
  if old > 0 then
    begin
    move (FData^[0], NewData^[0], sizeof(TFPColor)*FCount);
    FreeMem (FData);
    end;
  FData := NewData;
end;

procedure TFPPalette.SetCount (Value:integer);
var NewData : PFPColorArray;
    O : integer;
begin
  if Value <> FCount then
    begin
    if Value > FCapacity then
      begin
      O := FCapacity;
      FCapacity := Value + 8;
      if FCapacity > 0 then
        GetMem (NewData, sizeof(TFPColor)*FCapacity)
      else
        FData := nil;
      move (FData^, NewData^, sizeof(TFPColor)*FCount);
      if O > 0 then
        FreeMem (FData);
      FData := NewData;
      end;
    for o := FCount to Value-1 do
      FData^[o] := colBlack;
    FCount := Value;
    end;
end;

function TFPPalette.IndexOf (const AColor:TFPColor) : integer;
begin
  result := FCount;
  repeat
    dec (result);
  until (result < 0) or (FData^[result]=AColor);
  if result < 0 then
    result := Add (AColor);
end;

procedure TFPPalette.Clear;
begin
  SetCount (0);
end;


procedure TFPBaseInterpolation.Horizontal (width : integer);
var x,y,r : integer;
  start, stop, maxcontribs : integer;
  center, re,gr,bl, density : double;
  contributions : array[0..10] of TInterpolationContribution;
  dif, w, gamma, a : double;
  c : TFPColor;
begin
  for x := 0 to width-1 do
    begin
    center := x * xfactor;
    start := round (center-xsupport);
    if start < 0 then
      start := 0;
    stop := round(center+xsupport);
    if stop >= image.Width then
      stop := image.Width-1;
    density := 0.0;
    maxcontribs := -1;
    for r := start to stop do
      begin
      dif := r - center;
      w := Filter (dif);
      if w > 0.0 then
        begin
        inc (maxcontribs);
        with contributions[maxcontribs] do
          begin
          weight := w;
          density := density + w;
          place := r;
          end;
        end;
      end;
    if (density <> 0.0) and (density <> 1.0) then
      begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
      end;
    for y := 0 to image.height-1 do
      begin
      gamma := 0.0;
      re := 0.0;
      gr := 0.0;
      bl := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
          with image.colors[place,y] do
            begin
            a := weight * alpha / $FFFF;
            re := re + a * image.colors[place,y].red;
            gr := gr + a * image.colors[place,y].green;
            bl := bl + a * image.colors[place,y].blue;
            gamma := gamma + a;
            end;
      with c do
        begin
        red := ColorRound (re);
        green := ColorRound (gr);
        blue := ColorRound (bl);
        alpha := ColorRound (gamma * $FFFF) ;
        end;
      tempimage.colors[x,y] := c;
      end;
    end;
end;

procedure TFPBaseInterpolation.vertical(dx,dy,width,height: integer);
var x,y,r : integer;
  start, stop, maxcontribs : integer;
  center, re,gr,bl, density : double;
  contributions : array[0..10] of TInterpolationContribution;
  dif, w, gamma, a : double;
  c : TFPColor;
begin
  for y := 0 to height-1 do
    begin
    center := y * yfactor;
    start := round (center-ysupport);
    if start < 0 then
      start := 0;
    stop := round(center+ysupport);
    if stop >= tempimage.height then
      stop := tempimage.height-1;
    density := 0.0;
    maxcontribs := -1;
    for r := start to stop do
      begin
      dif := r - center;
      w := Filter (dif);
      if w > 0.0 then
        begin
        inc (maxcontribs);
        with contributions[maxcontribs] do
          begin
          weight := w;
          density := density + w;
          place := r;
          end;
        end;
      end;
    if (density <> 0.0) and (density <> 1.0) then
      begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
      end;
    for x := 0 to width-1 do
      begin
      gamma := 0.0;
      re := 0.0;
      gr := 0.0;
      bl := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
          with tempimage.colors[x,place] do
            begin
            a := weight * alpha / $FFFF;
            re := re + a * red;
            gr := gr + a * green;
            bl := bl + a * blue;
            gamma := gamma + a;
            end;
      with c do
        begin
        red := ColorRound (re);
        green := ColorRound (gr);
        blue := ColorRound (bl);
        alpha := ColorRound (gamma * $FFFF);
        end;
      canvas.colors[x+dx,y+dy] := c;
      end;
    end;
end;

procedure TFPBaseInterpolation.Execute(x, y, w, h: integer);
var maxy : integer;
    rx,ry : integer;
begin
  tempimage := TFPMemoryImage.Create (w,image.height);
  tempimage.UsePalette := false;
  xfactor := image.Width / w;
  yfactor := image.Height / h;
  if xfactor > 1.0 then
    xsupport := MaxSupport
  else
    xsupport := xfactor * MaxSupport;
  if yfactor > 1.0 then
    ysupport := MaxSupport
  else
    ysupport := yfactor * MaxSupport;
  Horizontal (w);
  Vertical (x,y,w,h);
end;

{ TMitchelInterpolation }

function TMitchelInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TMitchelInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;


Procedure TFPCustomImage.Assign(Source: TPersistent);

Var
  Src : TFPCustomImage;
  X,Y : Integer;

begin
  If Source is TFPCustomImage then
    begin
    Src:=TFPCustomImage(Source);
    // Copy extra info
    FExtra.Assign(Src.Fextra);
    // Copy palette if needed.
    SetSize(0,0); { avoid side-effects in descendant classes }
    UsePalette:=Src.UsePalette;
    If UsePalette then
      begin
      Palette.Count:=0;
      Palette.Merge(Src.Palette);
      end;
    // Copy image.
    SetSize(Src.Width,Src.height);
    If UsePalette then
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          pixels[X,Y]:=src.pixels[X,Y]
    else
      For x:=0 to Src.Width-1 do
        For y:=0 to src.Height-1 do
          self[X,Y]:=src[X,Y];
    end
  else
    Inherited Assign(Source);
end;

{ TFPMemoryImage }

constructor TFPMemoryImage.Create (AWidth,AHeight:integer);
begin
  Fdata := nil;
  inherited create (AWidth,AHeight);
{Default behavior is to use palette as suggested by Michael}
  SetUsePalette(True);
end;

destructor TFPMemoryImage.Destroy;
begin
  // MG: missing if
  if FData<>nil then
    FreeMem (FData);
  inherited Destroy;
end;

function TFPMemoryImage.GetInternalColor(x,y:integer):TFPColor;
  begin
    if Assigned(FPalette)
    then
      Result:=inherited GetInternalColor(x,y)
    else
      Result:=PFPColorArray(FData)^[y*FWidth+x];
  end;

function TFPMemoryImage.GetInternalPixel (x,y:integer) : integer;
begin
  result := FData^[y*FWidth+x];
end;

procedure TFPMemoryImage.SetInternalColor (x,y:integer; const Value:TFPColor);
  begin
    if Assigned(FPalette)
    then
      inherited SetInternalColor(x,y,Value)
    else
      PFPColorArray(FData)^[y*FWidth+x]:=Value;
  end;

procedure TFPMemoryImage.SetInternalPixel (x,y:integer; Value:integer);
begin
  FData^[y*FWidth+x] := Value;
end;

function Lowest (a,b : integer) : integer;
begin
  if a <= b then
    result := a
  else
    result := b;
end;

procedure TFPMemoryImage.SetSize (AWidth, AHeight : integer);
var w, h, r, old : integer;
    NewData : PFPIntegerArray;
begin
  if (AWidth <> Width) or (AHeight <> Height) then
    begin
    old := Height * Width;
    r:=AWidth*AHeight;
    if Assigned(FPalette)
    then
      r:=SizeOf(integer)*r
    else
      r:=SizeOf(TFPColor)*r;
    if r = 0 then
      NewData := nil
    else
      begin
      GetMem (NewData, r);
      FillWord (Newdata^[0], r div sizeof(word), 0);
      end;
    // MG: missing "and (NewData<>nil)"
    if (old <> 0) and assigned(FData) and (NewData<>nil) then
      begin
      if r <> 0 then
        begin
        w := Lowest(Width, AWidth);
        h := Lowest(Height, AHeight);
        for r := 0 to h-1 do
          move (FData^[r*Width], NewData^[r*AWidth], w);
        end;
      end;
    if Assigned(FData) then FreeMem(FData);
    FData := NewData;
    inherited;
    end;
end;

procedure TFPMemoryImage.SetUsePalette(Value:boolean);
var
  OldColors:PFPColorArray;
  OldPixels:PFPIntegerArray;
  r,c:Integer;
begin
  if Value<>assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette:=TFPPalette.Create(0);
        //FPalette.Add(colTransparent);
        if assigned(FData) then
          begin
          OldColors:=PFPColorArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(Integer));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=OldColors^[r*FWidth+c];
          FreeMem(OldColors);
          end;
      end
    else
      begin
        if Assigned(FData) then
          begin
          OldPixels:=PFPIntegerArray(FData);
          GetMem(FData,FWidth*FHeight*SizeOf(TFPColor));
          for r:=0 to FHeight-1 do
            for c:=0 to FWidth-1 do
              Colors[c,r]:=FPalette.Color[OldPixels^[r*FWidth+c]];
          FreeMem(OldPixels);
          end;
        FPalette.Free;
        FPalette:=nil;
      end;
end;

procedure FPImgError (Fmt:TErrorTextIndices; data : array of const);
begin
  raise FPImageException.CreateFmt (ErrorText[Fmt],data);
end;

procedure FPImgError (Fmt:TErrorTextIndices);
begin
  raise FPImageException.Create (ErrorText[Fmt]);
end;

function FPColor (r,g,b:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := alphaOpaque;
    end;
end;

function FPColor (r,g,b,a:word) : TFPColor;
begin
  with result do
    begin
    red := r;
    green := g;
    blue := b;
    alpha := a;
    end;
end;

operator = (const c,d:TFPColor) : boolean;
begin
  result := (c.Red = d.Red) and
            (c.Green = d.Green) and
            (c.Blue = d.Blue) and
            (c.Alpha = d.Alpha);
end;

function GetFullColorData (color:TFPColor) : TColorData;
begin
  result := PColorData(@color)^;
end;

function SetFullColorData (color:TColorData) : TFPColor;
begin
  result := PFPColor (@color)^;
end;

operator or (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) OR GetFullColorData(d));
end;

operator and (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) AND GetFullColorData(d));
end;

operator xor (const c,d:TFPColor) : TFPColor;
begin
  result := SetFullColorData(GetFullColorData(c) XOR GetFullColorData(d));
end;

{ TFPCustomInterpolation }

procedure TFPCustomInterpolation.Initialize(aimage: TFPCustomImage; acanvas: TFPCustomCanvas);
begin
  fimage := aimage;
  fcanvas := acanvas;
end;

{ TFPBaseInterpolation }

type

  TInterpolationContribution = record
    weight : double;
    place : integer;
  end;


{ TFPFloydSteinbergDitherer }

const FSNullPixel : TFPPixelReal = (a : 0.0; r : 0.0; g : 0.0; b : 0.0);

constructor TFPFloydSteinbergDitherer.Create(ThePalette : TFPPalette);
begin
  inherited Create(ThePalette);
  Lines:=nil;
end;

function TFPFloydSteinbergDitherer.GetError(const c1, c2 : TFPColor) : TFPPixelReal;
var temp : TFPPixelReal;
begin
  if FUseAlpha then
    temp.a:=((c1.Alpha and $FF00) shr 8) - ((c2.Alpha and $FF00) shr 8);
  temp.r:=((c1.Red and $FF00) shr 8) - ((c2.Red and $FF00) shr 8);
  temp.g:=((c1.Green and $FF00) shr 8) - ((c2.Green and $FF00) shr 8);
  temp.b:=((c1.Blue and $FF00) shr 8) - ((c2.Blue and $FF00) shr 8);
  Result:=temp;
end;

function TFPFloydSteinbergDitherer.Color2Real(const c : TFPColor) : TFPPixelReal;
var temp : TFPPixelReal;
begin
  if FUseAlpha then
    temp.a:=((c.Alpha and $FF00) shr 8);
  temp.r:=((c.Red and $FF00) shr 8);
  temp.g:=((c.Green and $FF00) shr 8);
  temp.b:=((c.Blue and $FF00) shr 8);
  Result:=temp;
end;

function TFPFloydSteinbergDitherer.Real2Color(r : TFPPixelReal) : TFPColor;
var temp : TFPColor;
begin
  { adjust overflows and underflows }
  if r.r> 255 then r.r:=255; if r.r<0 then r.r:=0;
  if r.g> 255 then r.g:=255; if r.g<0 then r.g:=0;
  if r.b> 255 then r.b:=255; if r.b<0 then r.b:=0;
  if FUseAlpha then
  begin
    if r.a> 255 then r.a:=255; if r.a<0 then r.a:=0;
  end;

  temp.Red:=round(r.r);
  temp.Red:=(temp.Red shl 8) + temp.Red;
  temp.Green:=round(r.g);
  temp.Green:=(temp.Green shl 8) + temp.Green;
  temp.Blue:=round(r.b);
  temp.Blue:=(temp.Blue shl 8) + temp.Blue;
  if FUseAlpha then
  begin
    temp.Alpha:=round(r.a);
    temp.Alpha:=(temp.Alpha shl 8) + temp.Alpha;
  end
  else
    temp.Alpha:=AlphaOpaque;
  Result:=temp;
end;

procedure TFPFloydSteinbergDitherer.CreatePixelLine(var line : PFSPixelLine; const row : integer);
var i : integer;
begin
  line:=GetMem(sizeof(TFSPixelLine));
  if line=nil then
    raise FPDithererException.Create('Out of memory');
  line^.next:=nil;
  { two extra pixels so we don't have to check if the pixel is on start or end of line  }
  getmem(line^.pixels,sizeof(TFPPixelReal)*(FImage.Width+2));
  if line^.pixels=nil then
    raise FPDithererException.Create('Out of memory');
  if row<FImage.Height-1 then
  begin
    line^.pixels[0]:=FSNullPixel;
    line^.pixels[FImage.Width+1]:=FSNullPixel;
    for i:=0 to FImage.Width-1 do
      line^.pixels[i+1]:=Color2Real(FImage[i,row]);
  end
  else
    for i:=0 to FImage.Width+1 do
      line^.pixels[i]:=FSNullPixel;
end;

const e716 = 0.4375;
      e516 = 0.3125;
      e316 = 0.1875;
      e116 = 0.0625;

procedure TFPFloydSteinbergDitherer.DistributeErrors(var line : PFSPixelLine; const row : integer; Img : TFPCustomImage);
var i, width : integer;
    palindex : integer;
    OldColor : TFPColor;
    dir : shortint;
    nextline : PFSPixelLine;
begin
  width:=FImage.Width;
  if (row mod 2)=0 then
  begin
    dir:=1;
    i:=1;
  end
  else
  begin
    dir:=-1;
    i:=width;
  end;
  if width<1 then exit;

  repeat
    OldColor:=Real2Color(line^.pixels[i]);
    FindBestColor(OldColor, palindex);
    Img.Pixels[i-1,row]:=palindex; { we use this color for this pixel... }
    line^.pixels[i]:=GetError(OldColor,Palette[palindex]);
    { now distribute this error to the other pixels, in this way: }
    { note: for odd lines this is mirrored and we start from right}
    {    0      0      0  }
    {    0      X    7/16 }
    {  3/16   5/16   1/16 }
    line^.pixels[i+dir].r:=line^.pixels[i+dir].r+(line^.pixels[i].r*e716);
    line^.pixels[i+dir].g:=line^.pixels[i+dir].g+(line^.pixels[i].g*e716);
    line^.pixels[i+dir].b:=line^.pixels[i+dir].b+(line^.pixels[i].b*e716);
    if FUseAlpha then
      line^.pixels[i+dir].a:=line^.pixels[i+dir].a+(line^.pixels[i].a*e716);
    nextline:=line^.next;

    nextline^.pixels[i].r:=nextline^.pixels[i].r+(line^.pixels[i].r*e516);
    nextline^.pixels[i].g:=nextline^.pixels[i].g+(line^.pixels[i].g*e516);
    nextline^.pixels[i].b:=nextline^.pixels[i].b+(line^.pixels[i].b*e516);
    if FUseAlpha then
      nextline^.pixels[i].a:=nextline^.pixels[i].a+(line^.pixels[i].a*e516);

    nextline^.pixels[i+dir].r:=nextline^.pixels[i+dir].r+(line^.pixels[i].r*e116);
    nextline^.pixels[i+dir].g:=nextline^.pixels[i+dir].g+(line^.pixels[i].g*e116);
    nextline^.pixels[i+dir].b:=nextline^.pixels[i+dir].b+(line^.pixels[i].b*e116);
    if FUseAlpha then
      nextline^.pixels[i+dir].a:=nextline^.pixels[i+dir].a+(line^.pixels[i].a*e116);

    nextline^.pixels[i-dir].r:=nextline^.pixels[i-dir].r+(line^.pixels[i].r*e316);
    nextline^.pixels[i-dir].g:=nextline^.pixels[i-dir].g+(line^.pixels[i].g*e316);
    nextline^.pixels[i-dir].b:=nextline^.pixels[i-dir].b+(line^.pixels[i].b*e316);
    if FUseAlpha then
      nextline^.pixels[i-dir].a:=nextline^.pixels[i-dir].a+(line^.pixels[i].a*e316);

    i:=i+dir;
  until ((i<1) or (i>width));
end;

procedure TFPFloydSteinbergDitherer.DeleteAllPixelLines(var line : PFSPixelLine);
var tmp : PFSPixelLine;
begin
  while line<>nil do
  begin
    tmp:=line^.next;
    FreeMem(line^.pixels);
    FreeMem(line);
    line:=tmp;
  end;
end;

procedure TFPFloydSteinbergDitherer.InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage);
var i : integer;
    tmpline : PFSPixelLine;
    percent : byte;
    percentinterval : longword;
    percentacc : longword;
    FContinue : boolean;
begin
  FImage:=Source;
  if FImage.Height=0 then exit;
  Dest.SetSize(0,0);
  try
    Dest.UsePalette:=true;
    Dest.Palette.Clear;
    Dest.Palette.Merge(FPalette);
    Dest.SetSize(FImage.Width,FImage.Height);
    percent:=0;
    percentinterval:=(FImage.Height*4) div 100;
    if percentinterval=0 then percentinterval:=$FFFFFFFF;
    percentacc:=0;
    FContinue:=true;
    Progress (self,psStarting,0,'',FContinue);
    if not FContinue then exit;
    CreatePixelLine(Lines,0);
    CreatePixelLine(Lines^.next,1);

    for i:=0 to FImage.Height-1 do
    begin
      DistributeErrors(Lines, i, Dest);
      tmpline:=Lines;
      Lines:=Lines^.next;
      FreeMem(tmpline^.pixels);
      FreeMem(tmpline);
      CreatePixelLine(Lines^.next,i+2);
      inc(percentacc,4);
      if percentacc>=percentinterval then
      begin
        percent:=percent+(percentacc div percentinterval);
        percentacc:=percentacc mod percentinterval;
        Progress (self,psRunning,percent,'',FContinue);
        if not FContinue then exit;
      end;
    end;
    Progress (self,psEnding,100,'',FContinue);
  finally
    DeleteAllPixelLines(lines);
  end;
end;

{ TFPCustomImage }

constructor TFPCustomImage.create (AWidth,AHeight:integer);
begin
  inherited create;
  FExtra := TStringList.Create;
  FWidth := 0;
  FHeight := 0;
  FPalette := nil;
  SetSize (AWidth,AHeight);
end;

destructor TFPCustomImage.destroy;
begin
  FExtra.Free;
  if assigned (FPalette) then
    FPalette.Free;
  inherited;
end;

procedure TFPCustomImage.LoadFromStream (Str:TStream; Handler:TFPCustomImagereader);
begin
  Handler.ImageRead (Str, self);
end;

procedure TFPCustomImage.LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
var
  fs : TStream;
begin
  if FileExists (filename) then
    begin
    fs := TFileStream.Create (filename, fmOpenRead);
    try
      LoadFromStream (fs, handler);
    finally
      fs.Free;
    end;
    end
  else
    FPImgError (StrNoFile, [filename]);
end;

procedure TFPCustomImage.SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
begin
  Handler.ImageWrite (Str, Self);
end;

procedure TFPCustomImage.SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
var
  fs : TStream;
begin
  fs := TFileStream.Create (filename, fmCreate);
  try
    SaveToStream (fs, handler);
  finally
    fs.Free;
  end
end;

procedure TFPCustomImage.SaveToFile (const filename:String);

var e,s : string;
    r : integer;
    f : TFileStream;
    h : TFPCustomImageWriterClass;
    Writer : TFPCustomImageWriter;
    d : TIHData;
    Msg : string;

begin
  e := lowercase (ExtractFileExt(filename));
  if (e <> '') and (e[1] = '.') then
    delete (e,1,1);
  with ImageHandlers do
    begin
    r := count-1;
    s := e + ';';
    while (r >= 0) do
      begin
      d := GetData(r);
      if (pos(s,d.Fextention+';') <> 0) then
        try
          h := d.FWriter;
          if assigned (h) then
            begin
            Writer := h.Create;
            try
              SaveTofile (filename, Writer);
            finally
              Writer.Free;
            end;
            break;
            end;
        except
          on e : exception do
            Msg := e.message;
        end;
      dec (r);
      end
    end;
  if (Msg<>'') then
    FPImgError (StrWriteWithError, [Msg]);
end;


procedure TFPCustomImage.LoadFromStream (Str:TStream);
var r : integer;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    msg : string;
    d : TIHData;
begin
  with ImageHandlers do
    try
      r := count-1;
      while (r >= 0) do
        begin
        d := GetData(r);
        if assigned (d) then
          h := d.FReader;
        if assigned (h) then
          begin
          reader := h.Create;
          with reader do
            try
              if CheckContents (str) then
                try
                  FStream := str;
                  FImage := self;
                  InternalRead (str, self);
                  break;
                except
                  on e : exception do
                    msg := e.message;
                end;
            finally
              Free;
              str.seek (soFromBeginning, 0);
            end;
          end;
        dec (r);
        end;
    except
      on e : exception do
        FPImgError (StrCantDetermineType, [e.message]);
    end;
  if r < 0 then
    if msg = '' then
      FPImgError (StrNoCorrectReaderFound)
    else
      FPImgError (StrReadWithError, [Msg]);
end;

procedure TFPCustomImage.LoadFromFile (const filename:String);
var e,s : string;
    r : integer;
    f : TFileStream;
    h : TFPCustomImageReaderClass;
    reader : TFPCustomImageReader;
    d : TIHData;
    Msg : string;
begin
  e := lowercase (ExtractFileExt(filename));
  if (e <> '') and (e[1] = '.') then
    delete (e,1,1);
  with ImageHandlers do
    begin
      r := count-1;
      s := e + ';';
      while (r >= 0) do
        begin
        d := GetData(r);
        if (pos(s,d.Fextention+';') <> 0) then
          try
            h := d.FReader;
            if assigned (h) then
              begin
              reader := h.Create;
              try
                loadfromfile (filename, reader);
              finally
                Reader.Free;
              end;
              break;
              end;
          except
            on e : exception do
              Msg := e.message;
          end;
        dec (r);
        end
    end;
  if Msg = '' then
    begin
    if r < 0 then
      begin
      f := TFileStream.Create (filename, fmOpenRead);
      try
        LoadFromStream (f);
      finally
        f.Free;
      end;
      end;
    end
  else
    FPImgError (StrReadWithError, [Msg]);
end;

procedure TFPCustomImage.SetHeight (Value : integer);
begin
  if Value <> FHeight then
    SetSize (FWidth, Value);
end;

procedure TFPCustomImage.SetWidth (Value : integer);
begin
  if Value <> FWidth then
    SetSize (Value, FHeight);
end;

procedure TFPCustomImage.SetSize (AWidth, AHeight : integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TFPCustomImage.SetExtraValue (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    FExtra[index] := copy(s, 1, p) + AValue
  else
    FPImgError (StrInvalidIndex,[ErrorText[StrImageExtra],index]);
end;

function TFPCustomImage.GetExtraValue (index:integer) : string;
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos ('=', s);
  if p > 0 then
    result := copy(s, p+1, maxint)
  else
    result := '';
end;

procedure TFPCustomImage.SetExtraKey (index:integer; const AValue:string);
var s : string;
    p : integer;
begin
  s := FExtra[index];
  p := pos('=',s);
  if p > 0 then
    s := AValue + copy(s,p,maxint)
  else
    s := AValue;
  FExtra[index] := s;
end;

function TFPCustomImage.GetExtraKey (index:integer) : string;
begin
  result := FExtra.Names[index];
end;

procedure TFPCustomImage.SetExtra (const key:String; const AValue:string);
begin
  FExtra.values[key] := AValue;
end;

function TFPCustomImage.GetExtra (const key:String) : string;
begin
  result := FExtra.values[key];
end;

function  TFPCustomImage.ExtraCount : integer;
begin
  result := FExtra.count;
end;

procedure TFPCustomImage.RemoveExtra (const key:string);
var p : integer;
begin
  p := FExtra.IndexOfName(key);
  if p >= 0 then
    FExtra.Delete (p);
end;

procedure TFPCustomImage.SetPixel (x,y:integer; Value:integer);
begin
  CheckPaletteIndex (Value);
  CheckIndex (x,y);
  SetInternalPixel (x,y,Value);
end;

function TFPCustomImage.GetPixel (x,y:integer) : integer;
begin
  CheckIndex (x,y);
  result := GetInternalPixel(x,y);
end;

procedure TFPCustomImage.SetColor (x,y:integer; const Value:TFPColor);
begin
  CheckIndex (x,y);
  SetInternalColor (x,y,Value);
end;

function TFPCustomImage.GetColor (x,y:integer) : TFPColor;
begin
  CheckIndex (x,y);
  result := GetInternalColor(x,y);
end;

procedure TFPCustomImage.SetInternalColor (x,y:integer; const Value:TFPColor);
var i : integer;
begin
  i := FPalette.IndexOf (Value);
  SetInternalPixel (x,y,i);
end;

function TFPCustomImage.GetInternalColor (x,y:integer) : TFPColor;
begin
  result := FPalette.Color[GetInternalPixel(x,y)];
end;

function TFPCustomImage.GetUsePalette : boolean;
begin
  result := assigned(FPalette);
end;

procedure TFPCustomImage.SetUsePalette(Value:boolean);
begin
  if Value <> assigned(FPalette)
  then
    if Value
    then
      begin
        FPalette := TFPPalette.Create (0);
        // FPalette.Add (colTransparent);
      end
    else
      begin
        FPalette.Free;
        FPalette := nil;
      end;
end;

procedure TFPCustomImage.CheckPaletteIndex (PalIndex:integer);
begin
  if UsePalette then
    begin
    if (PalIndex < -1) or (PalIndex >= FPalette.Count) then
      FPImgError (StrInvalidIndex,[ErrorText[StrPalette],PalIndex]);
    end
  else
    FPImgError (StrNoPaletteAvailable);
end;

procedure TFPCustomImage.CheckIndex (x,y:integer);
begin
  if (x < 0) or (x >= FWidth) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= FHeight) then
    FPImgError (StrInvalidIndex,[ErrorText[StrImageY],y]);
end;

Procedure TFPCustomImage.Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean);
begin
  If Assigned(FOnProgress) then
    FonProgress(Sender,Stage,PercentDone,RedrawNow,R,Msg,Continue);
end;


Function DefaultImageSize(X1,Y1,X2,Y2: smallint): longint; {$ifndef fpc}far;{$endif fpc}
Begin
  { each pixel uses two bytes, to enable modes with colors up to 64K }
  { to work.                                                         }
  DefaultImageSize := 12 + (((X2-X1+1)*(Y2-Y1+1))*2);
end;

Procedure DefaultPutImage(X,Y: smallint; var Bitmap; BitBlt: Word); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  k: longint;
  oldCurrentColor: word;
  oldCurrentWriteMode, i, j, y1, x1, deltaX, deltaX1, deltaY: smallint;
Begin
{$ifdef logging}
  LogLn('putImage at ('+strf(x)+','+strf(y)+') with width '+strf(ptw(Bitmap)[0])+
    ' and height '+strf(ptw(Bitmap)[1]));
  deltaY := 0;
{$endif logging}
  inc(x,startXViewPort);
  inc(y,startYViewPort);
  { width/height are 1-based, coordinates are zero based }
  x1 := ptw(Bitmap)[0]+x-1; { get width and adjust end coordinate accordingly }
  y1 := ptw(Bitmap)[1]+y-1; { get height and adjust end coordinate accordingly }

  deltaX := 0;
  deltaX1 := 0;
  k := 3 * sizeOf(Longint) div sizeOf(Word); { Three reserved longs at start of bitmap }
 { check which part of the image is in the viewport }
  if clipPixels then
    begin
      if y < startYViewPort then
        begin
          deltaY := startYViewPort - y;
          inc(k,(x1-x+1)*deltaY);
          y := startYViewPort;
         end;
      if y1 > startYViewPort+viewHeight then
        y1 := startYViewPort+viewHeight;
      if x < startXViewPort then
        begin
          deltaX := startXViewPort-x;
          x := startXViewPort;
        end;
      if x1 > startXViewPort + viewWidth then
        begin
          deltaX1 := x1 - (startXViewPort + viewWidth);
          x1 := startXViewPort + viewWidth;
        end;
    end;
{$ifdef logging}
  LogLn('deltax: '+strf(deltax)+', deltax1: '+strf(deltax1)+',deltay: '+strf(deltay));
{$endif logging}
  oldCurrentColor := currentColor;
  oldCurrentWriteMode := currentWriteMode;
  currentWriteMode := bitBlt;
  for j:=Y to Y1 do
   Begin
     inc(k,deltaX);
     for i:=X to X1 do
      begin
        currentColor := pt(bitmap)[k];
        directPutPixel(i,j);
        inc(k);
     end;
     inc(k,deltaX1);
   end;
  currentWriteMode := oldCurrentWriteMode;
  currentColor := oldCurrentColor;
end;

Procedure DefaultGetImage(X1,Y1,X2,Y2: smallint; Var Bitmap); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  i,j: smallint;
  k: longint;
Begin
  k:= 3 * Sizeof(longint) div sizeof(word); { Three reserved longs at start of bitmap }
  i := x2 - x1 + 1;
  for j:=Y1 to Y2 do
   Begin
     GetScanLine(x1,x2,j,pt(Bitmap)[k]);
     inc(k,i);
   end;
   ptw(Bitmap)[0] := X2-X1+1;   { First longint  is width  }
   ptw(Bitmap)[1] := Y2-Y1+1;   { Second longint is height }
   ptw(bitmap)[2] := 0;       { Third longint is reserved}
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


    function Decode(byte1,byte2: char; var x,y: smallint): smallint;
    { This routines decoes a signle word in a font opcode section  }
    { to a stroke record.                                          }
      var
       b1,b2: shortint;
     Begin
       b1:=shortint(byte1);
       b2:=shortint(byte2);
       { Decode the CHR OPCODE }
       Decode:=byte((shortint(b1 and $80) shr 6)+(shortint(b2 and $80) shr 7));
       { Now get the X,Y coordinates        }
       { bit 0..7 only which are considered }
       { signed values.                     }
{ disable range check mode }
{$ifopt R+}
{$define OPT_R_WAS_ON}
{$R-}
{$endif}
       b1:=b1 and $7f;
       b2:=b2 and $7f;
       { Now if the MSB of these values are set }
       { then the value is signed, therefore we }
       { sign extend it...                      }
       if (b1 and $40)<>0 then b1:=b1 or $80;
       if (b2 and $40)<>0 then b2:=b2 or $80;
       x:=smallint(b1);
       y:=smallint(b2);
{ restore previous range check mode }
{$ifdef OPT_R_WAS_ON}
{$R+}
{$endif}
     end;


    function unpack(buf: pchar; index: smallint; var Stroke: TStrokes): smallint;

     var
      po: TStrokes;
      num_ops: smallint;
      opcode, i, opc: word;
      counter: smallint;
      lindex: smallint;
      jx, jy: smallint;
     begin
       num_ops := 0;
       counter := index;
       lindex :=0;


       while TRUE do    {* For each byte in buffer      *}
         Begin
           Inc(num_ops);  {* Count the operation                *}
           opcode := decode( buf[counter], buf[counter+1] ,jx, jy );
           Inc(counter,2);
           if( opcode = ord(_END_OF_CHAR) ) then break; {* Exit loop at end of char     *}
         end;

       counter:=index;

       for i:=0 to num_ops-1 do    {    /* For each opcode in buffer    */ }
         Begin
           opc := decode(buf[counter], buf[counter+1], po[lindex].x, po[lindex].y);  {* Decode the data field   *}
           inc(counter,2);
           po[lindex].opcode := opc;      {* Save the opcode            *}
           Inc(lindex);
         end;
       Stroke:=po;
       unpack := num_ops;       {* return OPS count             *}
     end;


procedure SwapBMPFileHeader(var BFH : TBitMapFileHeader);
begin
  with BFH do
  begin
    bfType:=swap(bfType);
    bfSize:=swap(bfSize);
    bfReserved:=swap(bfReserved);
    bfOffset:=swap(bfOffset);
  end;
end;

procedure SwapBMPInfoHeader(var BFI : TBitMapInfoHeader);
begin
  with BFI do
  begin
    Size:=swap(Size);
    Width:=swap(Width);
    Height:=swap(Height);
    Planes:=swap(Planes);
    BitCount:=swap(BitCount);
    Compression:=swap(Compression);
    SizeImage:=swap(SizeImage);
    XPelsPerMeter:=swap(XPelsPerMeter);
    YPelsPerMeter:=swap(YPelsPerMeter);
    ClrUsed:=swap(ClrUsed);
    ClrImportant:=swap(ClrImportant);
  end;
end;


function FPColor2Packed(Col : TFPColor) : TFPPackedColor;
begin
  Result.R:=(Col.Red and $FF00) shr 8;
  Result.G:=(Col.Green and $FF00) shr 8;
  Result.B:=(Col.Blue and $FF00) shr 8;
  Result.A:=(Col.Alpha and $FF00) shr 8;
end;

function Packed2FPColor(Col : TFPPackedColor) : TFPColor;
begin
  Result.Red:=(Col.R shl 8) + Col.R;
  Result.Green:=(Col.G shl 8) + Col.G;
  Result.Blue:=(Col.B shl 8) + Col.B;
  Result.Alpha:=(Col.A shl 8) + Col.A;
end;

constructor TFPColorHashTable.Create;
begin
  Fcount:=0;
  AllIntegers:=true;
  Root:=nil;
end;

destructor TFPColorHashTable.Destroy;
begin
  FreeAllData;
  inherited Destroy;
end;

procedure TFPColorHashTable.CalculateIndexes(Col : TFPPackedColor; var ahi, alo, ri, gi, bi, partial, sub : byte);
var tmp : longword;
begin
  ahi := (Col.A and $F0) shr 4;
  alo := (Col.A and $F);
  ri := (Col.R and $F);
  gi := (Col.G and $F);
  bi := (Col.B and $F);
  tmp:=((Col.R and $F0) shl 4) or (Col.G and $F0) or ((Col.B and $F0) shr 4);
  partial:=tmp div 256;
  sub:=tmp mod 256;
end;

function TFPColorHashTable.CalculateColor(const ahi, alo, ri, gi, bi, partial, sub : byte) : TFPPackedColor;
var tmp : longword;
    col : TFPPackedColor;
begin
  tmp:=(partial shl 8) + sub; //partial*256 + sub;
  col.A:=(ahi shl 4) or alo;
  col.R:=((tmp and $F00) shr 4) + ri;
  col.G:=(tmp and $0F0) + gi;
  col.B:=((tmp and $00F) shl 4) + bi;
  Result:=col;
end;

procedure TFPColorHashTable.FreeAllData;
begin
  DeallocateMainNode(Root,0);
  Root:=nil;
  FCount:=0;
  AllIntegers:=true;
end;

function TFPColorHashTable.AllocateMainNode : PColHashMainNode;
var tmp : PColHashMainNode;
    i : byte;
begin
  Result:=nil;
  tmp:=getmem(sizeof(TColHashMainNode));
  if tmp=nil then raise TFPColorHashException.Create('Out of memory');
  for i:=0 to high(tmp^.childs) do
    tmp^.childs[i]:=nil;
  Result:=tmp;
end;

function TFPColorHashTable.AllocateSubNode : PColHashSubNode;
var tmp : PColHashSubNode;
begin
  Result:=nil;
  tmp:=getmem(sizeof(TColHashSubNode));
  if tmp=nil then raise TFPColorHashException.Create('Out of memory');
  tmp^.index:=0;
  tmp^.data:=nil;
  tmp^.next:=nil;
  inc(FCount);
  Result:=tmp;
end;

procedure TFPColorHashTable.DeallocateLinkedList(node : PColHashSubNode);
var tmp : PColHashSubNode;
begin
  while (node<>nil) do
  begin
    tmp:=node^.next;
    if node^.data<>nil then
      FreeMem(node^.data);
    FreeMem(node);
    node:=tmp;
  end;
end;

procedure TFPColorHashTable.DeallocateMainNode(node : PColHashMainNode; level : byte);
var i : byte;
begin
  if node=nil then exit;
  if level=5 then
  begin
    for i:=0 to high(node^.childs) do
      DeallocateLinkedList(node^.childs[i]);
  end
  else
    for i:=0 to high(node^.childs) do
      DeallocateMainNode(node^.childs[i],level+1);
  FreeMem(node);
end;

function TFPColorHashTable.SearchSubNode(start : PColHashSubNode; const index : byte ) : PColHashSubNode;
var cur : PColHashSubNode;
begin
  Result:=nil;
  cur:=start;
  while cur<>nil do
  begin
    if cur^.index=index then break
    else if cur^.index>index then exit; { exit and returns nil}
    cur:=cur^.next;
  end;
  Result:=cur;
end;

function TFPColorHashTable.SearchSubNodeAllocate(var start : PColHashSubNode; const index : byte ) : PColHashSubNode;
var tmp, cur, prev : PColHashSubNode;
begin
  Result:=nil;
  prev:=nil;
  cur:=start;
  while cur<>nil do
  begin
    if cur^.index=index then break
    else if cur^.index>index then {whoops, we must insert the new node before this one}
    begin
      tmp:=AllocateSubNode;
      tmp^.index:=index;
      tmp^.next:=cur;
      if prev<>nil then prev^.next:=tmp
      else start:=tmp;
      cur:=tmp;
      break;
    end;
    prev:=cur;
    cur:=cur^.next;
  end;
  if cur=nil then { not found! append to the end }
  begin
    cur:=AllocateSubNode;
    cur^.index:=index;
    prev^.next:=cur  { start is always <> nil}
  end;
  Result:=cur;
end;

function TFPColorHashTable.Search(const Col : TFPPackedColor) : PColHashSubNode;
var ahi, alo, ri, gi, bi, partial, sub : byte;
    tmpmain : PColHashMainNode;
begin
  Result:=nil;
  CalculateIndexes(Col, ahi, alo, ri, gi, bi, partial, sub);
  if Root=nil then exit;
  if Root^.childs[ahi]=nil then exit;
  tmpmain:=Root^.childs[ahi];
  if tmpmain^.childs[alo]=nil then exit;
  tmpmain:=tmpmain^.childs[alo];
  if tmpmain^.childs[ri]=nil then exit;
  tmpmain:=tmpmain^.childs[ri];
  if tmpmain^.childs[gi]=nil then exit;
  tmpmain:=tmpmain^.childs[gi];
  if tmpmain^.childs[bi]=nil then exit;
  tmpmain:=tmpmain^.childs[bi];

  if tmpmain^.childs[partial]=nil then exit;
  Result:=SearchSubNode(tmpmain^.childs[partial],sub);
end;

{ get the node; if there isn't, build the part of the tree }
function TFPColorHashTable.SearchAllocate(const Col : TFPPackedColor) : PColHashSubNode;
var ahi, alo, ri, gi, bi, partial, sub : byte;
   tmpmain : PColHashMainNode;
begin
  Result:=nil;
  CalculateIndexes(Col, ahi, alo, ri, gi, bi, partial, sub);
  if Root=nil then Root:=AllocateMainNode;
  if Root^.childs[ahi]=nil then Root^.childs[ahi]:=AllocateMainNode;
  tmpmain:=Root^.childs[ahi];
  if tmpmain^.childs[alo]=nil then tmpmain^.childs[alo]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[alo];
  if tmpmain^.childs[ri]=nil then tmpmain^.childs[ri]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[ri];
  if tmpmain^.childs[gi]=nil then tmpmain^.childs[gi]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[gi];
  if tmpmain^.childs[bi]=nil then tmpmain^.childs[bi]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[bi];

  if tmpmain^.childs[partial]=nil then  { newly-created linked list. }
  begin
    tmpmain^.childs[partial]:=AllocateSubNode;
    Result:=tmpmain^.childs[partial];
    Result^.index:=sub;
    exit;
  end;
  Result:=SearchSubNodeAllocate(tmpmain^.childs[partial],sub)
end;

procedure TFPColorHashTable.Insert(const Col : TFPColor; const Value : integer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  node^.data:=getmem(sizeof(Value));
  integer(node^.data^):=value;
end;

procedure TFPColorHashTable.Insert(const Col : TFPColor; const Value : pointer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  node^.data:=Value;
  AllIntegers:=false;
end;

procedure TFPColorHashTable.Add(const Col : TFPColor; const Value : integer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  if node^.data=nil then
  begin
    node^.data:=getmem(sizeof(Value));
    integer(node^.data^):=0;
  end;
  inc(integer(node^.data^),value);
end;

function TFPColorHashTable.Get(const Col : TFPColor) : pointer;
var node : PColHashSubNode;
begin
  node:=Search(FPColor2Packed(col));
  if node<>nil then
    Result:=node^.data
  else
    Result:=nil;
end;

procedure TFPColorHashTable.Clear;
begin
  FreeAllData;
end;

function TFPColorHashTable.GetArray : TFPColorWeightArray;
var ahi, alo, ri, gi, bi, partial : byte;
    node : PColHashSubNode;
    i : longword;
    cw : PFPColorWeight;
    tmp1,tmp2,tmp3,tmp4,tmp5 : PColHashMainNode;
begin
  if not AllIntegers then
    raise TFPColorHashException.Create('Hashtable data is not made by integers.');
  SetLength(Result,FCount);
  if Root=nil then exit;
  i:=0;
  for ahi:=0 to 15 do
  begin
    if Root^.childs[ahi]=nil then continue;
    tmp1:=Root^.childs[ahi];
    for alo:=0 to 15 do
    begin
      if tmp1^.childs[alo]=nil then continue;
      tmp2:=tmp1^.childs[alo];
      for ri:=0 to 15 do
      begin
        if tmp2^.childs[ri]=nil then continue;
        tmp3:=tmp2^.childs[ri];
        for gi:=0 to 15 do
        begin
          if tmp3^.childs[gi]=nil then continue;
          tmp4:=tmp3^.childs[gi];
          for bi:=0 to 15 do
          begin
            if tmp4^.childs[bi]=nil then continue;
            tmp5:=tmp4^.childs[bi];
            for partial:=0 to 15 do
            begin
              node:=tmp5^.childs[partial];
              while (node<>nil) do
              begin
                getmem(cw,sizeof(TFPColorWeight));
                if cw=nil then
                  raise TFPColorHashException.Create('Out of memory');
                cw^.Col:=CalculateColor(ahi,alo,ri,gi,bi,partial,node^.index);
                cw^.Num:=integer(node^.data^);
                Result[i]:=cw;
                inc(i);
                node:=node^.next;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;


{ TFPBaseDitherer }

procedure TFPBaseDitherer.Dither(const Source : TFPCustomImage; Dest : TFPCustomImage);
begin
  if FPalette.Count=0 then
    raise FPDithererException.Create('Palette is empty');
  if Source=Dest then
    raise FPDithererException.Create('Source and Destination images must be different');
  InternalDither(Source,Dest);
  if FUseHash then
    FHashMap.Clear;
end;

constructor TFPBaseDitherer.Create(ThePalette : TFPPalette);
begin
  FSorted:=false;
  FUseAlpha:=false;
  FImage:=nil;
  FPalette:=ThePalette;
  FUseHash:=true;
  FHashMap:=TFPColorHashTable.Create;
end;

destructor TFPBaseDitherer.Destroy;
begin
  if Assigned(FHashMap) then
    FHashMap.Free;
end;

procedure TFPBaseDitherer.SetUseHash(Value : boolean);
begin
  if Value=FUseHash then exit;
  if Value then
    FHashMap:=TFPColorHashTable.Create
  else
  begin
    FHashMap.Free;
    FHashMap:=nil;
  end;
  FUseHash:=Value;
end;

procedure TFPBaseDitherer.SetSorted(Value : boolean);
begin
  FSorted:=Value;
end;

procedure TFPBaseDitherer.Progress(Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender,Stage,PercentDone,Msg,Continue);
end;

{ rgb triplets are considered like a number having msb in msb(r) and lsb in lsb(b) }

function TFPBaseDitherer.SubtractColorInt(const c1, c2 : TFPColor) : int64;
var whole1, whole2 : int64;
begin
  whole1:= ((c1.Red and $FF00) shl 8) or (c1.Green and $FF00) or ((c1.Blue and $FF00) shr 8);
  whole2:= ((c2.Red and $FF00) shl 8) or (c2.Green and $FF00) or ((c2.Blue and $FF00) shr 8);
  if FUseAlpha then
  begin
    whole1:=whole1 or ((c1.Alpha and $FF00) shl 16);
    whole2:=whole2 or ((c2.Alpha and $FF00) shl 16);
  end;
  Result:= whole1 - whole2;
end;

{ this is more efficient than calling subtractcolorint and then extracting r g b values }
function TFPBaseDitherer.GetColorDinst(const c1, c2 : TFPColor) : integer;
var dinst : integer;
begin
  dinst:=abs(((c1.Red and $FF00) shr 8) - ((c2.Red and $FF00) shr 8));
  dinst:=dinst+abs(((c1.Green and $FF00) shr 8) - ((c2.Green and $FF00) shr 8));
  dinst:=dinst+abs(((c1.Blue and $FF00) shr 8) - ((c2.Blue and $FF00) shr 8));
  if FUseAlpha then
    dinst:=dinst+abs(((c1.Alpha and $FF00) shr 8) - ((c2.Alpha and $FF00) shr 8));
  Result:= dinst;
end;

function TFPBaseDitherer.SubtractColor(const c1, c2 : TFPColor) : TFPColor;
var whole : int64;
begin
  whole:=abs(SubtractColorInt(c1,c2));
  if FUseALpha then
    Result.Alpha:=(whole and $FF000000) shr 16
  else
    Result.Alpha:=AlphaOpaque;
  Result.Red:=(whole and $00FF0000) shr 8;
  Result.Green:=(whole and $0000FF00);
  Result.Blue:=(whole and $000000FF) shl 8;
end;

function TFPBaseDitherer.ColorCompare(const c1, c2 : TFPColor) : shortint;
var whole : int64;
begin
  whole:=SubtractColorInt(c1,c2);
  if whole>0 then Result:=1
  else if whole<0 then Result:=-1
  else Result:=0;
end;

procedure TFPBaseDitherer.QuickSort(const l, r : integer);
var i, j : integer;
    pivot, temp : TFPColor;
begin
  if l<r then
  begin
    pivot:=FPalette[l];
    i:=l+1;
    j:=r;
    repeat
      while ((i<=r) and (ColorCompare(FPalette[i],pivot)<=0)) do
        inc(i);
      while (ColorCompare(FPalette[j],pivot)=1) do
        dec(j);
      if i<j then
      begin
        temp:=FPalette[i];
        FPalette[i]:=FPalette[j];
        FPalette[j]:=temp;
      end;
    until i > j;
    { don't swap if they are equal }
    if ColorCompare(FPalette[j],pivot)<>0 then
    begin
      Fpalette[l]:=Fpalette[j];
      Fpalette[j]:=pivot;
    end;
    Quicksort(l,j-1);
    Quicksort(i,r);
  end;
end;

procedure TFPBaseDitherer.SortPalette;
begin
  QuickSort(0,FPalette.Count-1);
  FSorted:=true;
end;

type
  PBestColorData = ^TBestColorData;
  TBestColorData = record
    palindex, dinst : integer;
  end;

function TFPBaseDitherer.FindBestColor(OrigColor : TFPColor; var PalIndex : integer) : integer;
var i, curr, dinst, tmpdinst, top, bottom : integer;
    hashval : PBestColorData;
begin
  dinst:=$7FFFFFFF;
  curr:=0;

  if FUseHash then { use the hashmap to improve speed }
  begin
    hashval:=FHashMap.Get(OrigColor);
    if hashval<>nil then
    begin
      PalIndex:=hashval^.palindex;
      Result:=hashval^.dinst;
      exit;
    end;
  end;

  { with a sorted palette, proceed by binary search. this is more efficient with large images or large palettes }
  if FSorted then 
  begin
    top:=0;
    bottom:=FPalette.Count-1;
    while top<=bottom do
    begin
      i:=(bottom+top) div 2;
      tmpdinst:=ColorCompare(OrigColor,Fpalette[i]);
      if tmpdinst<0 then bottom:=i-1
      else if tmpdinst>0 then top:=i+1
      else break; { we found it }
    end;
    curr:=i;
    dinst:=GetColorDinst(OrigColor,Fpalette[i]);
  end
  else
    for i:=0 to FPalette.Count-1 do
    begin
      tmpdinst:=GetColorDinst(OrigColor,FPalette[i]);
      if tmpdinst<dinst then
      begin
        dinst:=tmpdinst;
        curr:=i;
      end;
      if tmpdinst=0 then break; { There can't be anything better, stop searching }
    end;

  if FUseHash then { if we are using a hashmap, remember this value}
  begin
    hashval:=GetMem(sizeof(TBestColorData));
    if hashval=nil then
      raise FPDithererException.Create('Out of memory');
    hashval^.PalIndex:=curr;
    hashval^.dinst:=dinst;
    FHashMap.Insert(OrigColor,hashval);
  end;
  PalIndex:=curr;
  Result:=dinst;
end;

procedure TFPBaseDitherer.InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage);
var i,j, palindex : integer;
    percent : byte;
    percentinterval : longword;
    percentacc : longword;
    FContinue : boolean;
begin
  FImage:=Source;
  percent:=0;
  percentinterval:=(FImage.Width*FImage.Height*4) div 100;
  if percentinterval=0 then percentinterval:=$FFFFFFFF;
  percentacc:=0;
  FContinue:=true;
  Progress (self,psStarting,0,'',FContinue);
  Dest.SetSize(0,0);
  Dest.UsePalette:=true;
  Dest.Palette.Clear;
  Dest.Palette.Merge(FPalette);
  Dest.SetSize(FImage.Width,FImage.Height);
  for j:=0 to FImage.Height-1 do
    for i:=0 to FImage.Width-1 do
    begin
      FindBestColor(FImage[i,j], palindex);
      Dest.Pixels[i,j]:=palindex;
      inc(percentacc,4);
      if percentacc>=percentinterval then
      begin
        percent:=percent+(percentacc div percentinterval);
        percentacc:=percentacc mod percentinterval;
        Progress (self,psRunning,percent,'',FContinue);
        if not fcontinue then exit;
      end;
    end;
  Progress (self,psEnding,100,'',FContinue);
end;


PROCEDURE ExtractFromImage(DestBitMap,SourceBitMap:POINTER;
                           OffsetX,OffsetY,W,H:INTEGER);
{complex routine to extract an image from another,
 make sure that there an overlap between the two images
 and that there is enough memory space in destination!}
VAR DestIndex,SourceIndex,Y:WORD;
BEGIN
{correct for offsets in source!}
  WITH BitMapPtr(SourceBitMap)^ DO
  BEGIN
    Dec(OffsetX,B_OffsetX);
    Dec(OffsetY,B_OffsetY);
  END;
{correct for negative offsets}
  WITH BitMapPtr(DestBitMap)^ DO
  BEGIN
    IF OffsetX < 0 THEN
    BEGIN
      B_OffsetX := -OffsetX;
      OffsetX   := 0;
      Dec(W,B_OffsetX);
    END;
    IF OffsetY < 0 THEN
    BEGIN
      B_OffsetY := -OffsetY;
      OffsetY   := 0;
      Dec(H,B_OffsetY);
    END;
  END;
{correct for oversizing}
  WITH BitMapPtr(SourceBitMap)^ DO
  BEGIN
    IF OffsetX+W > B_W THEN W := B_W-OffsetX;
    IF OffsetY+H > B_H THEN H := B_H-OffsetY;
  END;
{start extraction}
  WITH BitMapPtr(DestBitMap)^ DO
  BEGIN
  {set new width and heigth}
    B_W := W;
    B_H := H;
  {now make sure we have something to extract!}
    IF (W <= 0) OR (H <= 0) THEN EXIT;
  {now copy that what needs to be copied}
    DestIndex := 0;
    FOR Y := 0 TO H-1 DO
    BEGIN
      SourceIndex := WORD(Y+OffsetY)*BitMapPtr(SourceBitMap)^.B_W+OffsetX;
      Move(BitMapPtr(SourceBitMap)^.B_Data[SourceIndex],
           B_Data[DestIndex],W);
      Inc(DestIndex,W);
    END;
  END;
END; {ExtractFromImage}



PROCEDURE StampInImage(DestBitMap,SourceBitMap:POINTER;
                       OffsetX,OffsetY:INTEGER;BitBlit:BYTE);
{complex routine to stamp one image into another...,
 source and dest can have internal offsets!}
VAR DestIndex,SourceIndex,
    SourceOfsX,SourceOfsY:WORD;
    X,Y,W,H              :INTEGER;
    Data                 :BYTE;
BEGIN
{first of all we correct for bitmap offsets and
 try to find the part that can actually be stamped}
  WITH BitMapPtr(SourceBitMap)^ DO
  BEGIN
  {correct for offsets in source!}
    Inc(OffsetX,B_OffsetX);
    Inc(OffsetY,B_OffsetY);
  {get size}
    W := B_W;
    H := B_H;
  END;
{then we stamp it}
  WITH BitMapPtr(DestBitMap)^ DO
  BEGIN
  {correct for offsets in destination!}
    Dec(OffsetX,B_OffsetX);
    Dec(OffsetY,B_OffsetY);
  {correct for negative offsets}
    IF OffsetX < 0 THEN
    BEGIN
      SourceOfsX := -OffsetX;
      Dec(W,SourceOfsX);
      OffsetX    := 0;
    END
    ELSE SourceOfsX := 0;
    IF OffsetY < 0 THEN
    BEGIN
      SourceOfsY := -OffsetY;
      Dec(H,SourceOfsY);
      OffsetY    := 0;
    END
    ELSE SourceOfsY := 0;
  {correct for oversizing}
    IF OffsetX+W > B_W THEN W := B_W-OffsetX;
    IF OffsetY+H > B_H THEN H := B_H-OffsetY;
  {check size}
    IF (W <= 0) OR (H <= 0) THEN EXIT;
  {then we do the actual stamping}
    FOR Y := 0 TO H-1 DO
    BEGIN
      SourceIndex := WORD(SourceOfsY+Y)*BitMapPtr(SourceBitMap)^.B_W+SourceOfsX;
      DestIndex   := WORD(OffsetY+Y)*B_W+OffsetX;
      FOR X := 1 TO W DO
      BEGIN
        Data := BitMapPtr(SourceBitMap)^.B_Data[SourceIndex];
        CASE BitBlit OF
          SpriteBlit : IF Data <> 0 THEN B_Data[DestIndex] := Data;
          NormalBlit,
          CopyBlit   : B_Data[DestIndex] := Data;
          XORBlit    : B_Data[DestIndex] := B_Data[DestIndex] XOR Data;
          OrBlit     : B_Data[DestIndex] := B_Data[DestIndex] OR Data;
          AndBlit    : B_Data[DestIndex] := B_Data[DestIndex] AND Data;
          NotBlit    : B_Data[DestIndex] := NOT Data;
          XNORBlit   : B_Data[DestIndex] := B_Data[DestIndex] XOR NOT Data;
          NOrBlit    : B_Data[DestIndex] := B_Data[DestIndex] OR NOT Data;
          NAndBlit   : B_Data[DestIndex] := B_Data[DestIndex] AND NOT Data;
        END;
        Inc(DestIndex);
        Inc(SourceIndex);
      END;
    END;
  END;
END; {StampInImage}

{
//needs file access
procedure load_icon(xx,yy :integer;iconname :string);

var
  r,rr :byte;
  f    :text;

begin
  x :=xx;y :=yy;
  assign(f,iconname +'.ico');
  //$I- reset(f); //$I+
  if ioresult =0 then begin
    for p :=1 to 766 do begin
      read(f,ch);q :=ord(ch);
      if (p >126) and (p <639) then begin
        r :=q shr 4;rr :=q-r div 16;
        putpixel(x,y,r);putpixel(x+1,y,rr);
        inc(x,2);
        if x =xx+32 then begin
          x :=xx;dec(y);
        end;
      end;
    end;
    close(f);
  end;
end;

}


{ TARGA common definitions}

  TWordRec = Packed Record
    Lo,Hi : byte;
  end;

  TTargaHeader = packed record
    IDLen        : Byte;
    MapType      : Byte;
    ImgType      : Byte;
    MapStart     : TWordRec;
    MapLength    : TWordRec;
    MapEntrySize : Byte;
    OriginX      : TWordrec;
    OriginY      : TWordRec;
    Width        : TWordRec;
    Height       : TWordRec;
    PixelSize    : Byte;
    Flags        : Byte;
  end;

  TBGREntry = packed record
    Blue, Green, Red : Byte;
  end;

  TBGRAEntry = packed record
    Blue, Green, Red, Alpha : Byte;
  end;

{ PNG reader/writer common code.}
type

  PNGImageException = class (FPImageException);

  TChunkTypes = (
    ctIHDR,  ctcHRM,  ctgAMA,  ctsBIT,
    ctPLTE,  ctbKGD,  cthIST,  cttRNS,
    ctoFFs,  ctpHYs,  ctIDAT,  cttIME,
    ctsCAL,  cttEXt,  ctzTXt,  ctIEND,
    ctsRGB,  ctiCCP,  ctiTXt,  ctsPLT,
    ctUnknown
    );

  EightLong = array[0..7] of longword;
  TChunkCode = array[0..3] of char;

  TChunk = record
    acapacity, alength, CRC : longword;
    ReadType : TChunkCode;
    data : PByteArray;
    aType : TChunkTypes;
  end;

  TChunkHeader = record
    CLength : longword;
    CType : TChunkCode;
  end;

  THeaderChunk = record
    Width, height : longword;
    BitDepth, ColorType, Compression, Filter, Interlace : byte;
  end;


  TRGB = packed record
    Red, Green, Blue: byte;
  end;

  TPCXHeader = record
    FileID:   byte;                      // signature $0A  PCX files, $CD SCR files
    Version:  byte;                     // 0: version 2.5
    // 2: 2.8 avec palette
    // 3: 2.8 sans palette
    // 5: version 3
    Encoding: byte;                    // 0: non compressed
    // 1: encodage RLE
    BitsPerPixel: byte;                // bits per pixel 1, 4, 8, 24
    XMin,                              
    YMin,                              
    XMax,                              
    YMax,                              
    HRes,                              
    VRes:     word;                        
    ColorMap: array[0..15] of TRGB;    // Palette
    Reserved,                          
    ColorPlanes: byte;                 
    BytesPerLine,                      
    PaletteType: word;                 
    //      1: color or B/W
    //      2: grey gradient
    Fill:     array[0..57] of byte;        
  end;

  TBitmapType = (btBlackWhite, bt256Gray);
  TFontBitmap = record
    height, width, pitch,
    x,y, advanceX, advanceY : integer;
    data : PByteArray;
  end;
  PFontBitmap = ^TFontBitmap;


  TStringBitMaps = class
    private
      FList : TList;
      FBounds : TRect;
      FText : string;
      FMode : TBitmapType;
      function GetCount : integer;
      function GetBitmap (index:integer) : PFontBitmap;
      procedure CalculateGlobals;
    public
      constructor Create (ACount : integer);
      destructor destroy; override;
      procedure GetBoundRect (var aRect : TRect);
      property Text : string read FText;
      property Mode : TBitmapType read FMode;
      property Count : integer read GetCount;
      property Bitmaps[index:integer] : PFontBitmap read GetBitmap;
  end;

{BMP writer implementation}

{ - 22/11/2007 Modified by Laurent Jacques for support all format }

  { TFPReaderTarga }

  TFPReaderTarga = class (TFPCustomImageReader)
  Private
    Procedure FreeBuffers;       // Free (and nil) buffers.
  protected
    Header         : TTargaHeader;
    AlphaBits      : Byte;
    Identification : ShortString;
    Compressed,
    BottomUp       : Boolean;
    BytesPerPixel  : Byte;
    FPalette       : PFPColor;
    FScanLine      : PByte;
    FLineSize      : Integer;
    FPaletteSize   : Integer;
    FBlockCount    : Integer;
    FPixelCount    : Integer;
    FLastPixel     : Packed Array[0..3] of byte;
    // AnalyzeHeader will allocate the needed buffers.
    Procedure AnalyzeHeader(Img : TFPCustomImage);
    procedure CreateGrayPalette;
    Procedure ReadPalette(Stream : TStream);
    procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
    procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
    // required by TFPCustomImageReader
    procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Stream:TStream) : boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


  { TFPReaderPCX }

  TFPReaderPCX = class(TFPCustomImageReader)
  private
    FCompressed: boolean;
  protected
    Header:     TPCXHeader;
    BytesPerPixel: byte;
    FScanLine:  PByte;
    FLineSize:  integer;
    TotalWrite: longint;
    procedure CreateGrayPalette(Img: TFPCustomImage);
    procedure CreateBWPalette(Img: TFPCustomImage);
    procedure CreatePalette16(Img: TFPCustomImage);
    procedure ReadPalette(Stream: TStream; Img: TFPCustomImage);
    procedure AnalyzeHeader(Img: TFPCustomImage);
    function InternalCheck(Stream: TStream): boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    procedure ReadScanLine(Row: integer; Stream: TStream); virtual;
    procedure UpdateProgress(percent: longint);
    procedure WriteScanLine(Row: integer; Img: TFPCustomImage); virtual;
  public
    property Compressed: boolean Read FCompressed;
  end;


  TImageHandlersManager = class
    private
      FData : TList;
      function GetReader (const TypeName:string) : TFPCustomImageReaderClass;
      function GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
      function GetExt (const TypeName:string) : string;
      function GetDefExt (const TypeName:string) : string;
      function GetTypeName (index:integer) : string;
      function GetData (const ATypeName:string) : TIHData;
      function GetData (index : integer) : TIHData;
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterImageHandlers (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
      procedure RegisterImageReader (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass);
      procedure RegisterImageWriter (const ATypeName,TheExtentions:string;
                   AWriter:TFPCustomImageWriterClass);
      property Count : integer read GetCount;
      property ImageReader [const TypeName:string] : TFPCustomImageReaderClass read GetReader;
      property ImageWriter [const TypeName:string] : TFPCustomImageWriterClass read GetWriter;
      property Extentions [const TypeName:string] : string read GetExt;
      property DefaultExtention [const TypeName:string] : string read GetDefExt;
      property TypeNames [index:integer] : string read GetTypeName;
    end;

  TErrorTextIndices = (
    StrInvalidIndex,
    StrNoImageToWrite,
    StrNoFile,
    StrNoStream,
    StrPalette,
    StrImageX,
    StrImageY,
    StrImageExtra,
    StrTypeAlreadyExist,
    StrTypeReaderAlreadyExist,
    StrTypeWriterAlreadyExist,
    StrCantDetermineType,
    StrNoCorrectReaderFound,
    StrReadWithError,
    StrWriteWithError,
    StrNoPaletteAvailable
    );

  TGrayConvMatrix = record
    red, green, blue : single;
  end;


  TFPCustomImageReader = class (TFPCustomImageHandler)
    private
      FDefImageClass:TFPCustomImageClass;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); virtual; abstract;
      function  InternalCheck (Str:TStream) : boolean; virtual; abstract;
    public
      constructor Create; override;
      function ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
      // reads image
      function CheckContents (Str:TStream) : boolean;
      // Gives True if contents is readable
      property DefaultImageClass : TFPCustomImageClass read FDefImageClass write FDefImageClass;
      // Image Class to create when no img is given for reading
  end;
  TFPCustomImageReaderClass = class of TFPCustomImageReader;

  TFPCustomImageWriter = class (TFPCustomImageHandler)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); virtual; abstract;
    public
      procedure ImageWrite (Str:TStream; Img:TFPCustomImage);
      // writes given image to stream
  end;
  TFPCustomImageWriterClass = class of TFPCustomImageWriter;


  TFPMemoryImage = class (TFPCustomImage)
    private
      FData : PFPIntegerArray;
      function GetInternalColor(x,y:integer):TFPColor;override;
      procedure SetInternalColor (x,y:integer; const Value:TFPColor);override;
      procedure SetUsePalette (Value:boolean);override;
    protected
      procedure SetInternalPixel (x,y:integer; Value:integer); override;
      function GetInternalPixel (x,y:integer) : integer; override;
    public
      constructor create (AWidth,AHeight:integer); override;
      destructor destroy; override;
      procedure SetSize (AWidth, AHeight : integer); override;
  end;

  TFPCustomImageHandler = class
    private
      FOnProgress : TFPImgProgressEvent;
      FStream : TStream;
      FImage : TFPCustomImage;
    protected
      procedure Progress(Stage: TProgressStage; PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
      property TheStream : TStream read FStream;
      property TheImage : TFPCustomImage read FImage;
    public
      constructor Create; virtual;
      Property OnProgress : TFPImgProgressEvent Read FOnProgress Write FOnProgress;
  end;

  TIHData = class
    private
      FExtention, FTypeName, FDefaultExt : string;
      FReader : TFPCustomImageReaderClass;
      FWriter : TFPCustomImageWriterClass;
  end;

  TFPCustomImage = class(TPersistent)
    private
      FOnProgress : TFPImgProgressEvent;
      FExtra : TStringlist;
      FPalette : TFPPalette;
      FHeight, FWidth : integer;
      procedure SetHeight (Value : integer);
      procedure SetWidth (Value : integer);
      procedure SetExtra (const key:String; const AValue:string);
      function GetExtra (const key:String) : string;
      procedure SetExtraValue (index:integer; const AValue:string);
      function GetExtraValue (index:integer) : string;
      procedure SetExtraKey (index:integer; const AValue:string);
      function GetExtraKey (index:integer) : string;
      procedure CheckIndex (x,y:integer);
      procedure CheckPaletteIndex (PalIndex:integer);
      procedure SetColor (x,y:integer; const Value:TFPColor);
      function GetColor (x,y:integer) : TFPColor;
      procedure SetPixel (x,y:integer; Value:integer);
      function GetPixel (x,y:integer) : integer;
      function GetUsePalette : boolean;
    protected
      // Procedures to store the data. Implemented in descendants
      procedure SetInternalColor (x,y:integer; const Value:TFPColor); virtual;
      function GetInternalColor (x,y:integer) : TFPColor; virtual;
      procedure SetInternalPixel (x,y:integer; Value:integer); virtual; abstract;
      function GetInternalPixel (x,y:integer) : integer; virtual; abstract;
      procedure SetUsePalette (Value:boolean);virtual;
      procedure Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
    public
      constructor create (AWidth,AHeight:integer); virtual;
      destructor destroy; override;
      procedure Assign(Source: TPersistent); override;
      // Saving and loading
      procedure LoadFromStream (Str:TStream; Handler:TFPCustomImageReader);
      procedure LoadFromStream (Str:TStream);
      procedure LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
      procedure LoadFromFile (const filename:String);
      procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String);
      // Size and data
      procedure SetSize (AWidth, AHeight : integer); virtual;
      property  Height : integer read FHeight write SetHeight;
      property  Width : integer read FWidth write SetWidth;
      property  Colors [x,y:integer] : TFPColor read GetColor write SetColor; default;
      // Use of palette for colors
      property  UsePalette : boolean read GetUsePalette write SetUsePalette;
      property  Palette : TFPPalette read FPalette;
      property  Pixels [x,y:integer] : integer read GetPixel write SetPixel;
      // Info unrelated with the image representation
      property  Extra [const key:string] : string read GetExtra write SetExtra;
      property  ExtraValue [index:integer] : string read GetExtraValue write SetExtraValue;
      property  ExtraKey [index:integer] : string read GetExtraKey write SetExtraKey;
      procedure RemoveExtra (const key:string);
      function  ExtraCount : integer;
      property OnProgress: TFPImgProgressEvent read FOnProgress write FOnProgress;
  end;
  TFPCustomImageClass = class of TFPCustomImage;
  TFPImgProgressStage = (psStarting, psRunning, psEnding);
  TFPImgProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean) of object;

{    fpImage base definitions}
  TFPCustomImageReader = class;
  TFPCustomImageWriter = class;
  TFPCustomImage = class;

  FPImageException = class (exception);

  TFPColor = record
    red,green,blue,alpha : word;
  end;
  PFPColor = ^TFPColor;

  TColorFormat = (cfMono,cfGray2,cfGray4,cfGray8,cfGray16,cfGray24,
                  cfGrayA8,cfGrayA16,cfGrayA32,
                  cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
                  cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
                  cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48,
                  cfABGR8,cfABGR16,cfABGR32,cfABGR64);
  TColorData = qword;
  PColorData = ^TColorData;

  TDeviceColor = record
    Fmt : TColorFormat;
    Data : TColorData;
  end;

  TFPColorArray = array [0..(maxint-1) div sizeof(TFPColor)-1] of TFPColor;
  PFPColorArray = ^TFPColorArray;

  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  TFPPalette = class
    protected
      FData : PFPColorArray;
      FCount, FCapacity : integer;
      procedure SetCount (Value:integer); virtual;
      function GetCount : integer;
      procedure SetColor (index:integer; const Value:TFPColor); virtual;
      function GetColor (index:integer) : TFPColor;
      procedure CheckIndex (index:integer); virtual;
      procedure EnlargeData; virtual;
    public
      constructor Create (ACount : integer);
      destructor Destroy; override;
      procedure Build (Img : TFPCustomImage); virtual;
      procedure Copy (APalette: TFPPalette); virtual;
      procedure Merge (pal : TFPPalette); virtual;
      function IndexOf (const AColor: TFPColor) : integer; virtual;
      function Add (const Value: TFPColor) : integer; virtual;
      procedure Clear; virtual;
      property Color [Index : integer] : TFPColor read GetColor write SetColor; default;
      property Count : integer read GetCount write SetCount;
  end;


  TFPIntegerArray = array [0..(maxint-1) div sizeof(integer)-1] of integer;
  PFPIntegerArray = ^TFPIntegerArray;
  TFPWriterPNM = class(TFPCustomImageWriter)
    private
      BitMapType:Integer;
    protected
      procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
    public
      constructor Create(aBitMapType:Integer);
  end;

{   XPM writer implementation.}
  TFPWriterXPM = class (TFPCustomImageWriter)
    private
      FPalChars : string;
      FColorFormat : string;
      FColorShift : word;
      FColorSize : byte;
      procedure SetColorSize (AValue : byte);
      function ColorToHex (c:TFPColor) : string;
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
    public
      constructor Create; override;
      property PalChars : string read FPalChars write FPalChars;
      property ColorCharSize : byte read FColorSize write SetColorSize;
      // number of characters to use for 1 colorcomponent
  end;

{   PNM writer implementation.}
{Support for writing PNM (Portable aNyMap) formats added :
    * PBM (P1,P4) : Portable BitMap format : 1 bit per pixel
    * PGM (P2,P5) : Portable GrayMap format : 8 bits per pixel
    * PPM (P5,P6) : Portable PixelMap foramt : 24 bits per pixel}



{   BMP writer implementation.}
{ 08/2005 by Giulio Bernardi:
   - Removed FBytesPerPixel, BytesPerPixel property is now deprecated, use BitsPerPixel instead.
   - Rewritten a large part of the file, so we can handle all bmp color depths
   - Support for RLE4 and RLE8 encoding
}

  TFPWriterBMP = class (TFPCustomImageWriter)
  private
    StartPosition : int64; { save start of bitmap in the stream, if we must go back and fix something }
    FBpp : byte;
    FRLECompress : boolean;
    BFH : TBitMapFileHeader;
    BFI : TBitMapInfoHeader;
    Colinfo : array of TColorRGBA;
    procedure SetColorSize (AValue : Byte);
    function GetColorSize : byte;
    procedure SetBpp (const abpp : byte);
    procedure FillColorMap(Img : TFPCustomImage);
    procedure Setup16bpp;
    function PackWord555(const col : TFPColor) : word;
    function PackWord565(const col : TFPColor) : word;
    function Pack4bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
    function Pack1bpp(const img : TFPCustomImage; var Col : integer; const Row : integer) : byte;
    procedure CompressScanLineRLE8(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
    procedure CompressScanLineRLE4(ALine : pbyte; const Row, Width : Integer; Stream : TStream);
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property BitsPerPixel : byte read FBpp write SetBpp;
    property RLECompress : boolean read FRleCompress write FRleCompress;
    Property BytesPerPixel : Byte Read GetColorSize Write SetColorSize; deprecated;
  end;


{
The PNM (Portable aNyMaps) is a generic name for :
  PBM : Portable BitMaps,
  PGM : Portable GrayMaps,
  PPM : Portable PixMaps.
There is no file format associated  with PNM itself.}

  TFPReaderPNM=class (TFPCustomImageReader)
    private
      FBitMapType : Integer;
      FWidth      : Integer;
      FHeight     : Integer;
    protected
      FMaxVal     : Cardinal;
      FBitPP        : Byte;
      FScanLineSize : Integer;
      FScanLine   : PByte;
      procedure ReadHeader(Stream : TStream);
      function  InternalCheck (Stream:TStream):boolean;override;
      procedure InternalRead(Stream:TStream;Img:TFPCustomImage);override;
      procedure ReadScanLine(Row : Integer; Stream:TStream);
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage);
  end;


{   BMP reader implementation.}

{ 08/2005 by Giulio Bernardi:
   - Added support for 16 and 15 bpp bitmaps.
   - If we have bpp <= 8 make an indexed image instead of converting it to RGB
   - Support for RLE4 and RLE8 decoding
   - Support for top-down bitmaps
}

  TFPReaderBMP = class (TFPCustomImageReader)
    Private
      Procedure FreeBufs;       // Free (and nil) buffers.
      DeltaX, DeltaY : integer; // Used for the never-used delta option in RLE
      TopDown : boolean;        // If set, bitmap is stored top down instead of bottom up
      continue : boolean;       // needed for onprogress event
      percent : byte;
      percentinterval : longword;
      percentacc : longword;
      Rect : TRect;
    protected
      ReadSize : Integer;       // Size (in bytes) of 1 scanline.
      BFI : TBitMapInfoHeader;  // The header as read from the stream.
      FPalette : PFPcolor;      // Buffer with Palette entries. (useless now)
      LineBuf : PByte;          // Buffer for 1 scanline. Can be Byte, Word, TColorRGB or TColorRGBA
      RedMask, GreenMask, BlueMask : longword; //Used if Compression=bi_bitfields
      RedShift, GreenShift, BlueShift : shortint;
      // SetupRead will allocate the needed buffers, and read the colormap if needed.
      procedure SetupRead(nPalette, nRowBits: Integer; Stream : TStream); virtual;
      function CountBits(Value : byte) : shortint;
      function ShiftCount(Mask : longword) : shortint;
      function ExpandColor(value : longword) : TFPColor;
      procedure ExpandRLE8ScanLine(Row : Integer; Stream : TStream);
      procedure ExpandRLE4ScanLine(Row : Integer; Stream : TStream);
      procedure ReadScanLine(Row : Integer; Stream : TStream); virtual;
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage); virtual;
      // required by TFPCustomImageReader
      procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Stream:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

{   fpImage base definitions.}


  TFPCustomImageReader = class;
  TFPCustomImageWriter = class;
  TFPCustomImage = class;

  FPImageException = class (exception);

  TFPColor = record
    red,green,blue,alpha : word;
  end;
  PFPColor = ^TFPColor;

  TColorFormat = (cfMono,cfGray2,cfGray4,cfGray8,cfGray16,cfGray24,
                  cfGrayA8,cfGrayA16,cfGrayA32,
                  cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
                  cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
                  cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48,
                  cfABGR8,cfABGR16,cfABGR32,cfABGR64);
  TColorData = qword;
  PColorData = ^TColorData;

  TDeviceColor = record
    Fmt : TColorFormat;
    Data : TColorData;
  end;

  TFPColorArray = array [0..(maxint-1) div sizeof(TFPColor)-1] of TFPColor;
  PFPColorArray = ^TFPColorArray;

  TFPImgProgressStage = (psStarting, psRunning, psEnding);
  TFPImgProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean) of object;
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

  TFPPalette = class
    protected
      FData : PFPColorArray;
      FCount, FCapacity : integer;
      procedure SetCount (Value:integer); virtual;
      function GetCount : integer;
      procedure SetColor (index:integer; const Value:TFPColor); virtual;
      function GetColor (index:integer) : TFPColor;
      procedure CheckIndex (index:integer); virtual;
      procedure EnlargeData; virtual;
    public
      constructor Create (ACount : integer);
      destructor Destroy; override;
      procedure Build (Img : TFPCustomImage); virtual;
      procedure Copy (APalette: TFPPalette); virtual;
      procedure Merge (pal : TFPPalette); virtual;
      function IndexOf (const AColor: TFPColor) : integer; virtual;
      function Add (const Value: TFPColor) : integer; virtual;
      procedure Clear; virtual;
      property Color [Index : integer] : TFPColor read GetColor write SetColor; default;
      property Count : integer read GetCount write SetCount;
  end;

  TFPCustomImage = class(TPersistent)
    private
      FOnProgress : TFPImgProgressEvent;
      FExtra : TStringlist;
      FPalette : TFPPalette;
      FHeight, FWidth : integer;
      procedure SetHeight (Value : integer);
      procedure SetWidth (Value : integer);
      procedure SetExtra (const key:String; const AValue:string);
      function GetExtra (const key:String) : string;
      procedure SetExtraValue (index:integer; const AValue:string);
      function GetExtraValue (index:integer) : string;
      procedure SetExtraKey (index:integer; const AValue:string);
      function GetExtraKey (index:integer) : string;
      procedure CheckIndex (x,y:integer);
      procedure CheckPaletteIndex (PalIndex:integer);
      procedure SetColor (x,y:integer; const Value:TFPColor);
      function GetColor (x,y:integer) : TFPColor;
      procedure SetPixel (x,y:integer; Value:integer);
      function GetPixel (x,y:integer) : integer;
      function GetUsePalette : boolean;
    protected
      // Procedures to store the data. Implemented in descendants
      procedure SetInternalColor (x,y:integer; const Value:TFPColor); virtual;
      function GetInternalColor (x,y:integer) : TFPColor; virtual;
      procedure SetInternalPixel (x,y:integer; Value:integer); virtual; abstract;
      function GetInternalPixel (x,y:integer) : integer; virtual; abstract;
      procedure SetUsePalette (Value:boolean);virtual;
      procedure Progress(Sender: TObject; Stage: TProgressStage;
                         PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
    public
      constructor create (AWidth,AHeight:integer); virtual;
      destructor destroy; override;
      procedure Assign(Source: TPersistent); override;
      // Saving and loading
      procedure LoadFromStream (Str:TStream; Handler:TFPCustomImageReader);
      procedure LoadFromStream (Str:TStream);
      procedure LoadFromFile (const filename:String; Handler:TFPCustomImageReader);
      procedure LoadFromFile (const filename:String);
      procedure SaveToStream (Str:TStream; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String; Handler:TFPCustomImageWriter);
      procedure SaveToFile (const filename:String);
      // Size and data
      procedure SetSize (AWidth, AHeight : integer); virtual;
      property  Height : integer read FHeight write SetHeight;
      property  Width : integer read FWidth write SetWidth;
      property  Colors [x,y:integer] : TFPColor read GetColor write SetColor; default;
      // Use of palette for colors
      property  UsePalette : boolean read GetUsePalette write SetUsePalette;
      property  Palette : TFPPalette read FPalette;
      property  Pixels [x,y:integer] : integer read GetPixel write SetPixel;
      // Info unrelated with the image representation
      property  Extra [const key:string] : string read GetExtra write SetExtra;
      property  ExtraValue [index:integer] : string read GetExtraValue write SetExtraValue;
      property  ExtraKey [index:integer] : string read GetExtraKey write SetExtraKey;
      procedure RemoveExtra (const key:string);
      function  ExtraCount : integer;
      property OnProgress: TFPImgProgressEvent read FOnProgress write FOnProgress;
  end;
  TFPCustomImageClass = class of TFPCustomImage;

  TFPIntegerArray = array [0..(maxint-1) div sizeof(integer)-1] of integer;
  PFPIntegerArray = ^TFPIntegerArray;

  TFPMemoryImage = class (TFPCustomImage)
    private
      FData : PFPIntegerArray;
      function GetInternalColor(x,y:integer):TFPColor;override;
      procedure SetInternalColor (x,y:integer; const Value:TFPColor);override;
      procedure SetUsePalette (Value:boolean);override;
    protected
      procedure SetInternalPixel (x,y:integer; Value:integer); override;
      function GetInternalPixel (x,y:integer) : integer; override;
    public
      constructor create (AWidth,AHeight:integer); override;
      destructor destroy; override;
      procedure SetSize (AWidth, AHeight : integer); override;
  end;

  TFPCustomImageHandler = class
    private
      FOnProgress : TFPImgProgressEvent;
      FStream : TStream;
      FImage : TFPCustomImage;
    protected
      procedure Progress(Stage: TProgressStage; PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                         const Msg: AnsiString; var Continue: Boolean); Virtual;
      property TheStream : TStream read FStream;
      property TheImage : TFPCustomImage read FImage;
    public
      constructor Create; virtual;
      Property OnProgress : TFPImgProgressEvent Read FOnProgress Write FOnProgress;
  end;

  TFPCustomImageReader = class (TFPCustomImageHandler)
    private
      FDefImageClass:TFPCustomImageClass;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); virtual; abstract;
      function  InternalCheck (Str:TStream) : boolean; virtual; abstract;
    public
      constructor Create; override;
      function ImageRead (Str:TStream; Img:TFPCustomImage) : TFPCustomImage;
      // reads image
      function CheckContents (Str:TStream) : boolean;
      // Gives True if contents is readable
      property DefaultImageClass : TFPCustomImageClass read FDefImageClass write FDefImageClass;
      // Image Class to create when no img is given for reading
  end;
  TFPCustomImageReaderClass = class of TFPCustomImageReader;

  TFPCustomImageWriter = class (TFPCustomImageHandler)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); virtual; abstract;
    public
      procedure ImageWrite (Str:TStream; Img:TFPCustomImage);
      // writes given image to stream
  end;
  TFPCustomImageWriterClass = class of TFPCustomImageWriter;

  TIHData = class
    private
      FExtention, FTypeName, FDefaultExt : string;
      FReader : TFPCustomImageReaderClass;
      FWriter : TFPCustomImageWriterClass;
  end;

  TImageHandlersManager = class
    private
      FData : TList;
      function GetReader (const TypeName:string) : TFPCustomImageReaderClass;
      function GetWriter (const TypeName:string) : TFPCustomImageWriterClass;
      function GetExt (const TypeName:string) : string;
      function GetDefExt (const TypeName:string) : string;
      function GetTypeName (index:integer) : string;
      function GetData (const ATypeName:string) : TIHData;
      function GetData (index : integer) : TIHData;
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure RegisterImageHandlers (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass; AWriter:TFPCustomImageWriterClass);
      procedure RegisterImageReader (const ATypeName,TheExtentions:string;
                   AReader:TFPCustomImageReaderClass);
      procedure RegisterImageWriter (const ATypeName,TheExtentions:string;
                   AWriter:TFPCustomImageWriterClass);
      property Count : integer read GetCount;
      property ImageReader [const TypeName:string] : TFPCustomImageReaderClass read GetReader;
      property ImageWriter [const TypeName:string] : TFPCustomImageWriterClass read GetWriter;
      property Extentions [const TypeName:string] : string read GetExt;
      property DefaultExtention [const TypeName:string] : string read GetDefExt;
      property TypeNames [index:integer] : string read GetTypeName;
    end;

operator = (const c,d:TFPColor) : boolean;
operator or (const c,d:TFPColor) : TFPColor;
operator and (const c,d:TFPColor) : TFPColor;
operator xor (const c,d:TFPColor) : TFPColor;



  TErrorTextIndices = (
    StrInvalidIndex,
    StrNoImageToWrite,
    StrNoFile,
    StrNoStream,
    StrPalette,
    StrImageX,
    StrImageY,
    StrImageExtra,
    StrTypeAlreadyExist,
    StrTypeReaderAlreadyExist,
    StrTypeWriterAlreadyExist,
    StrCantDetermineType,
    StrNoCorrectReaderFound,
    StrReadWithError,
    StrWriteWithError,
    StrNoPaletteAvailable
    );

  TGrayConvMatrix = record
    red, green, blue : single;
  end;

  { TFPCustomInterpolation }

  TFPCustomInterpolation = class
  private
    fcanvas: TFPCustomCanvas;
    fimage: TFPCustomImage;
  protected
    procedure Initialize (aimage:TFPCustomImage; acanvas:TFPCustomCanvas); virtual;
    procedure Execute (x,y,w,h:integer); virtual; abstract;
  public
    property Canvas : TFPCustomCanvas read fcanvas;
    property Image : TFPCustomImage read fimage;
  end;

  { TFPBaseInterpolation }

  TFPBaseInterpolation = class (TFPCustomInterpolation)
  private
    xfactor, yfactor : double;
    xsupport,ysupport : double;
    tempimage : TFPCustomImage;
    procedure Horizontal (width : integer);
    procedure vertical (dx,dy,width,height: integer);
  protected
    procedure Execute (x,y,w,h:integer); override;
    function Filter (x : double) : double; virtual; abstract;
    function MaxSupport : double; virtual; abstract;
  end;

  { TMitchelInterpolation }

  TMitchelInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

 
{color hash table.}

  TFPColorHashException = class(Exception);
  PColHashSubNode = ^TColHashSubNode;
  TColHashSubNode = packed record
    index : byte;
    data : pointer;
    next : PColHashSubNode;
  end;
  PColHashMainNode = ^TColHashMainNode;
  TColHashMainNode = packed record
    childs : array[0..16] of pointer; { can be either another MainNode or a SubNode }
  end;

{
  HashMap configuration:
  childs[MSN(A)]                                                   level 0
    |_childs[LSN(A)]                                               level 1
       |_childs[LSN(R)]                                            level 2
          |_childs[LSN(G)]                                         level 3
             |_childs[LSN(B)]                                      level 4
                |_childs[(MSN(R) MSN(G) MSN (B)) div 256]          level 5
                   |_element [(MSN(R) MSN(G) MSN (B)) mod 256]
  Very low accesses to reach an element, not much memory occupation if alpha is rarely used, even with
  images with 500,000 colors.
  For extremely colorful images (near 2^24 colors used) using only 5 bits per channel keeps the map
  small and efficient

}


  TFPPackedColor = record
    R, G, B, A : byte;
  end;

  TFPColorWeight = record
    Col : TFPPackedColor;
    Num : integer;
  end;
  PFPColorWeight = ^TFPColorWeight;
  TFPColorWeightArray = array of PFPColorWeight;

  TFPColorHashTable = class
  private
    Root : PColHashMainNode;
    AllIntegers : boolean;
    procedure FreeAllData;
    FCount : longword;
    function AllocateMainNode : PColHashMainNode;
    function AllocateSubNode : PColHashSubNode;
    procedure DeallocateLinkedList(node : PColHashSubNode);
    procedure DeallocateMainNode(node : PColHashMainNode; level : byte);
    procedure CalculateIndexes(Col : TFPPackedColor; var ahi, alo, ri, gi, bi, partial, sub : byte);
    function CalculateColor(const ahi, alo, ri, gi, bi, partial, sub : byte) : TFPPackedColor;
    function SearchSubNode(start : PColHashSubNode; const index : byte ) : PColHashSubNode;
    function SearchSubNodeAllocate(var start : PColHashSubNode; const index : byte ) : PColHashSubNode;
    function Search(const Col : TFPPackedColor) : PColHashSubNode;
    function SearchAllocate(const Col : TFPPackedColor) : PColHashSubNode;
  protected
  public
    procedure Insert(const Col : TFPColor; const Value : integer);
    procedure Insert(const Col : TFPColor; const Value : pointer);
    procedure Add(const Col : TFPColor; const Value : integer);
    function Get(const Col : TFPColor) : pointer;
    procedure Clear;
    function GetArray : TFPColorWeightArray;
    property Count : longword read FCount;
    constructor Create;
    destructor Destroy; override;
  end;


{ dithering of images}

  FPDithererException = class (exception);
  TFPDithererProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte;
                                         const Msg: AnsiString; var Continue : Boolean) of object;
  TFPBaseDitherer = class
    private
      FPalette : TFPPalette;
      FOnProgress : TFPDithererProgressEvent;
      procedure QuickSort(const l, r : integer);
    protected
      FImage : TFPCustomImage;
      FHashMap : TFPColorHashTable;
      FSorted : boolean;
      FUseHash : boolean;
      FUseAlpha : boolean;
      function ColorCompare(const c1, c2 : TFPColor) : shortint;
      function GetColorDinst(const c1, c2 : TFPColor) : integer;
      function SubtractColorInt(const c1, c2 : TFPColor) : int64;
      function SubtractColor(const c1, c2 : TFPColor) : TFPColor;
      procedure InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage); virtual;
      function FindBestColor(OrigColor : TFPColor; var PalIndex : integer) : integer; virtual;
      procedure Progress (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean); virtual;
      procedure SetUseHash(Value : boolean); virtual;
      procedure SetSorted(Value : boolean); virtual;
    public
      property OnProgress : TFPDithererProgressEvent read FOnProgress write FOnProgress;
      property Palette : TFPPalette read FPalette;
      property PaletteSorted : boolean read FSorted write SetSorted;
      property UseHashMap : boolean read FUseHash write SetUseHash;
      property UseAlpha : boolean read FUseAlpha write FUseAlpha;
      procedure Dither(const Source : TFPCustomImage; Dest : TFPCustomImage);
      procedure SortPalette; virtual;
      constructor Create(ThePalette : TFPPalette); virtual;
      destructor Destroy; override;
  end;

  PFPPixelReal = ^TFPPixelReal;
  TFPPixelReal = record   { pixel in real form }
    a, r, g, b : real;
  end;

  PFSPixelLine = ^TFSPixelLine;
  TFSPixelLine = record
    pixels : PFPPixelReal;             { a line of pixels }
    Next : PFSPixelLine;               { next line of pixels }
  end;

  TFPFloydSteinbergDitherer = class(TFPBaseDitherer)
    private
      Lines : PFSPixelLine;
      function Color2Real(const c : TFPColor) : TFPPixelReal;
      function Real2Color(r : TFPPixelReal) : TFPColor;
      procedure CreatePixelLine(var line : PFSPixelLine; const row : integer);
      function GetError(const c1, c2 : TFPColor) : TFPPixelReal;
      procedure DistributeErrors(var line : PFSPixelLine; const row : integer; Img : TFPCustomImage);
      procedure DeleteAllPixelLines(var line : PFSPixelLine);
    protected
      procedure InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage); override;
    public
      constructor Create(ThePalette : TFPPalette); override;
  end;

 TBitMapChar = array[0..7,0..7] of byte;

   { BMP reader/writer common code.}

   TBitMapFileHeader = packed record
{00+02 :File type}
      bfType:word;
{02+04 :File size in bytes}
      bfSize:longint;
{06+04 : Reserved}
      bfReserved:longint;
{10+04 : Offset of image data : size if the file hieder + the info header + palette}
      bfOffset:longint;
   end;
   PBitMapFileHeader = ^TBitMapFileHeader;


   TBitMapInfoHeader = packed record
{14+04 : Size of the bitmap info header : sould be 40=$28}
      Size:longint;
{18+04 : Image width in pixels}
      Width:longint;
{22+04 : Image height in pixels}
      Height:longint;
{26+02 : Number of image planes : should be 1 always}
      Planes:word;
{28+02 : Color resolution : Number of bits per pixel (1,4,8,16,24,32)}
      BitCount:word;
{30+04 : Compression Type}
      Compression:longint;
{34+04 : Size of image data (not headers nor palette): can be 0 if no compression}
      SizeImage:longint;
{38+04 : Horizontal resolution in pixel/meter}
      XPelsPerMeter:Longint;
{42+04 : Vertical resolution in pixel/meter}
      YPelsPerMeter:Longint;
{46+04 : Number of colors used}
      ClrUsed:longint;
{50+04 : Number of imprtant colors used : usefull for displaying on VGA256}
      ClrImportant:longint;
   end;
   PBitMapInfoHeader = ^TBitMapInfoHeader;


{ Bitmap utilities }
type
  PBitmap = ^TBitmap;
  TBitmap = record
            Width, Height: smallint;
            Data: record end;
            end;

Procedure DefaultGetImage(X1,Y1,X2,Y2: smallint; Var Bitmap); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  i,j: smallint;
  k: longint;
Begin
  k:= 3 * Sizeof(longint) div sizeof(word); { Three reserved longs at start of bitmap }
  i := x2 - x1 + 1;
  for j:=Y1 to Y2 do
   Begin
     GetScanLine(x1,x2,j,pt(Bitmap)[k]);
     inc(k,i);
   end;
   ptw(Bitmap)[0] := X2-X1+1;   { First longint  is width  }
   ptw(Bitmap)[1] := Y2-Y1+1;   { Second longint is height }
   ptw(bitmap)[2] := 0;       { Third longint is reserved}
end;

Procedure DefaultPutImage(X,Y: smallint; var Bitmap; BitBlt: Word); {$ifndef fpc}far;{$endif fpc}
type
  pt = array[0..$fffffff] of word;
  ptw = array[0..2] of longint;
var
  k: longint;
  oldCurrentColor: word;
  oldCurrentWriteMode, i, j, y1, x1, deltaX, deltaX1, deltaY: smallint;
Begin
{$ifdef logging}
  LogLn('putImage at ('+strf(x)+','+strf(y)+') with width '+strf(ptw(Bitmap)[0])+
    ' and height '+strf(ptw(Bitmap)[1]));
  deltaY := 0;
{$endif logging}
  inc(x,startXViewPort);
  inc(y,startYViewPort);
  { width/height are 1-based, coordinates are zero based }
  x1 := ptw(Bitmap)[0]+x-1; { get width and adjust end coordinate accordingly }
  y1 := ptw(Bitmap)[1]+y-1; { get height and adjust end coordinate accordingly }

  deltaX := 0;
  deltaX1 := 0;
  k := 3 * sizeOf(Longint) div sizeOf(Word); { Three reserved longs at start of bitmap }
 { check which part of the image is in the viewport }
  if clipPixels then
    begin
      if y < startYViewPort then
        begin
          deltaY := startYViewPort - y;
          inc(k,(x1-x+1)*deltaY);
          y := startYViewPort;
         end;
      if y1 > startYViewPort+viewHeight then
        y1 := startYViewPort+viewHeight;
      if x < startXViewPort then
        begin
          deltaX := startXViewPort-x;
          x := startXViewPort;
        end;
      if x1 > startXViewPort + viewWidth then
        begin
          deltaX1 := x1 - (startXViewPort + viewWidth);
          x1 := startXViewPort + viewWidth;
        end;
    end;
  oldCurrentColor := currentColor;
  oldCurrentWriteMode := currentWriteMode;
  currentWriteMode := bitBlt;
  for j:=Y to Y1 do
   Begin
     inc(k,deltaX);
     for i:=X to X1 do
      begin
        currentColor := pt(bitmap)[k];
        directPutPixel(i,j);
        inc(k);
     end;
     inc(k,deltaX1);
   end;
  currentWriteMode := oldCurrentWriteMode;
  currentColor := oldCurrentColor;
end;

Function DefaultImageSize(X1,Y1,X2,Y2: smallint): longint; {$ifndef fpc}far;{$endif fpc}
Begin
  { each pixel uses four bytes, to enable modes with colors up to TrueColor }
  { to work.                                                         }
  DefaultImageSize := 12 + (((X2-X1+1)*(Y2-Y1+1))*4);
end;


