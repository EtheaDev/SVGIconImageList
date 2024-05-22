unit Img32.Fmt.BMP;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.4                                                             *
* Date      :  8 May 2024                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  BMP file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows,{$ENDIF} SysUtils, Classes, Math, Img32;

type

  //TImage32Fmt_BMP.LoadFromFile() loads correctly all 'good' BMP images
  //in Jason Summers' test suite - see http://entropymine.com/jason/bmpsuite/
  //For notes on RLE bitmap compression, see ...
  //https://docs.microsoft.com/en-us/windows/desktop/gdi/bitmap-compression

  TImageFormat_BMP = class(TImageFormat)
  private
    fUseClipboardFormat: Boolean;
    fIncludeFileHeaderInSaveStream: Boolean;
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; override;
    // SaveToFile: compressionQuality parameter is ignored here
    function SaveToFile(const filename: string;
      img32: TImage32; compressionQuality: integer = 0): Boolean; override;
    // SaveToStream: the compressionQuality parameter is ignored here
    procedure SaveToStream(stream: TStream;
      img32: TImage32; compressionQuality: integer = 0); override;
{$IFDEF MSWINDOWS}
    class function CanCopyToClipboard: Boolean; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
{$ENDIF}
    property IncludeFileHeaderInSaveStream: Boolean read
      fIncludeFileHeaderInSaveStream write fIncludeFileHeaderInSaveStream;
  end;

{$IFDEF MSWINDOWS}
  function LoadFromHBITMAP(img32: TImage32; bm: HBITMAP; pal: HPALETTE = 0): Boolean;
{$ENDIF}

implementation

resourcestring
  s_cf_dib_error = 'TImage32 - clipboard CF_DIB format error';

type
  PTriColor32 = ^TTriColor32;
  TTriColor32 = array [0..2] of TColor32;
  TArrayOfByte = array of Byte;

  TBitmapFileHeader = packed record
    bfType: Word;
    bfSize: Cardinal;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Cardinal;
  end;

  TBitmapInfoHeader = packed record
    biSize: Cardinal;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Cardinal;
    biSizeImage: Cardinal;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: Cardinal;
    biClrImportant: Cardinal;
  end;

  PBitmapCoreHeader = ^TBitmapCoreHeader;
  TBitmapCoreHeader = packed record
    bcSize: Cardinal;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;

  TCIEXYZ = record
    ciexyzX: Longint;
    ciexyzY: Longint;
    ciexyzZ: Longint;
  end;

  TCIEXYZTriple = record
    ciexyzRed: TCIEXYZ;
    ciexyzGreen: TCIEXYZ;
    ciexyzBlue: TCIEXYZ;
  end;

  TBitmapV4Header = packed record
    bV4Size: Cardinal;
    bV4Width: Longint;
    bV4Height: Longint;
    bV4Planes: Word;
    bV4BitCount: Word;
    bV4V4Compression: Cardinal;
    bV4SizeImage: Cardinal;
    bV4XPelsPerMeter: Longint;
    bV4YPelsPerMeter: Longint;
    bV4ClrUsed: Cardinal;
    bV4ClrImportant: Cardinal;
    bV4RedMask: Cardinal;
    bV4GreenMask: Cardinal;
    bV4BlueMask: Cardinal;
    bV4AlphaMask: Cardinal;
    bV4CSType: Cardinal;
    bV4Endpoints: TCIEXYZTriple;
    bV4GammaRed: Cardinal;
    bV4GammaGreen: Cardinal;
    bV4GammaBlue: Cardinal;
  end;

const
  BI_RGB = 0;
  BI_RLE24 = 4;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

//------------------------------------------------------------------------------
// Loading (reading) BMP images from file ...
//------------------------------------------------------------------------------

function StreamReadPalette(stream: TStream;
  count, size: integer): TArrayOfColor32;
var
  i: integer;
  c: TARGB;
begin
  setLength(Result, count);
  for i := 0 to count -1 do
  begin
    stream.Read(c, size);
    with c do result[i] := $FF000000 +  R shl 16 + G shl 8 + B;
  end;
end;
//------------------------------------------------------------------------------

function StreamReadImageWithBitfields(stream: TStream; width, height,
  bpp: integer; bitfields: TTriColor32): TArrayOfColor32;
var
  i,j,bytesPerRow, bytesPerPix: integer;
  shift, size: array[0..2] of byte;
  buffer: PByte;
  dstPixel: PARGB;
  b: PCardinal;
begin
  Result := nil;

  //from the 3 bitfields, get each bit mask offset (shift) and bit mask size
  for i := 0 to 2 do
  begin
    size[i] := 0;
    shift[i] := 0;
    for j := 0 to 31 do
      if (size[i] > 0) then
      begin
        if bitfields[i] and (1 shl j) > 0 then inc(size[i])
        else break;
      end
      else if bitfields[i] and (1 shl j) > 0 then
      begin
        shift[i] := j;
        size[i] := 1;
      end;
  end;

  for i := 0 to 2 do
  begin
    //bitfields larger than 8 aren't supported
    if size[i] > 8 then Exit;
    //colorXBit.R = (buffer^ and bitfields[0]) shr shift[0]
    //and the largest possible value for colorXBit.R = (1 shl size[i]) - 1
    //so convert size[x] to the maximum possible value for colorXBit.R ...
    size[i] := (1 shl size[i]) - 1;
  end;

  bytesPerPix := bpp div 8;
  bytesPerRow := ((31 + bpp * width) div 32) * 4;
  setLength(Result, width * height);
  GetMem(buffer, bytesPerRow);
  try
    for i := 0 to height -1 do
    begin
      stream.Read(buffer^, bytesPerRow);
      b := PCardinal(buffer);
      dstPixel := @result[i * width];
      for j := 0 to width -1 do
      begin
        dstPixel.A := 255;
        //convert colorXBit.R to color32bit.R ...
        //dstPixel.R = colorXBit.R * 255 div size[0]
        dstPixel.R := DivTable[(b^ and bitfields[0]) shr shift[0], size[0]];
        dstPixel.G := DivTable[(b^ and bitfields[1]) shr shift[1], size[1]];
        dstPixel.B := DivTable[(b^ and bitfields[2]) shr shift[2], size[2]];
        inc(dstPixel);
        inc(PByte(b), bytesPerPix);
      end;
    end;
  finally
    FreeMem(buffer);
  end;
end;
//------------------------------------------------------------------------------

{$RANGECHECKS OFF}
function StreamReadImageWithPalette(stream: TStream;
  width, height, bpp: integer;
  const palette: TArrayOfColor32): TArrayOfColor32;
var
  i,j, bytesPerRow, palHigh, pxCnt: integer;
  buffer: TArrayOfByte;
  b: PByte;
  dstPixel: PColor32;
  c, shift: byte;
begin
  shift := 8 - bpp;
  bytesPerRow := ((31 + bpp * width) div 32) * 4;
  setLength(Result, width * height);
  palHigh := High(palette);
  SetLength(buffer, bytesPerRow);
  for i := 0 to height -1 do
  begin
    stream.Read(buffer[0], bytesPerRow);
    b := @buffer[0];
    dstPixel := @result[i * width];
    pxCnt := 0;
    for j := 0 to width -1 do
    begin
      pxCnt := (pxCnt + bpp) mod 8;
      c := Ord(b^) shr shift;
      if c > palHigh then dstPixel^ := clNone32
      else dstPixel^ := palette[c];
      if  pxCnt = 0 then inc(b)
      else Byte(b^) := Ord(b^) shl bpp;
      inc(dstPixel);
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetByte(stream: TStream): Byte;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  stream.Read(Result, 1);
end;
//------------------------------------------------------------------------------

function GetNibble(stream: TStream; var bitsOffset: integer): Byte;
begin
  stream.Read(Result, 1);
  if bitsOffset = 4 then
  begin
    result := result and $F;
    bitsOffset := 0;
  end else
  begin
    Stream.Position := Stream.Position -1;
    result := result shr 4;
    bitsOffset := 4;
  end;
end;
//------------------------------------------------------------------------------

function ReadRLE4orRLE8Compression(stream: TStream;
  width, height, bpp: integer;
  const palette: TArrayOfColor32): TArrayOfColor32;
var
  i,j,k, cnt, idx: integer;
  w, delta, bitOffset: integer;
  dst: PColor32;
  byte1, byte2: byte;
const
  COMMAND_BYTE = 0;
  DELTA_MODE = 2;
begin
  setLength(Result, width * height);
  for i := 0 to height -1 do
  begin
    dst := @result[i * width];
    w := 0; idx := 0;
    while w < width do
    begin
      byte1 := GetByte(stream);
      byte2 := GetByte(stream);
      if byte1 = COMMAND_BYTE then
      begin
        if byte2 < 2 then Exit          //error
        else if byte2 = DELTA_MODE then
        begin
          cnt := GetByte(stream);
          delta := GetByte(stream);
          if delta > 0 then Exit;       //Y-delta never seen & not supported
          for k := 1 to cnt do
          begin
            dst^ := palette[idx];
            inc(w);
            inc(dst);
          end;
        end
        else                            //'absolute mode'
        begin
          cnt := byte2;
          bitOffset := 0;
          for k := 1 to cnt do
          begin
            if bpp = 4 then
              idx := GetNibble(stream, bitOffset) else
              idx := GetByte(stream);
            dst^ := palette[idx];
            inc(w);
            if w = width then break;
            inc(dst);
          end;
          if bitOffset > 0 then GetByte(stream);
          if Odd(stream.Position) then
            GetByte(stream);            //ie must be WORD aligned
        end;
      end else                          //'encoded mode'
      begin
        cnt := byte1;
        if bpp = 4 then
        begin
          for j := 1 to cnt do
          begin
            if Odd(j) then
              idx := byte2 shr 4 else
              idx := byte2 and $F;
            dst^ := palette[idx];
            inc(w);
            if w = width then break;
            inc(dst);
          end;
        end else
        begin
          idx := byte2;
          for j := 1 to cnt do
          begin
            dst^ := palette[idx];
            inc(w);
            inc(dst);
          end;
        end;
      end;
    end;
    byte1 := GetByte(stream);
    byte2 := GetByte(stream);
    if (byte1 <> 0) or (byte2 <> 0) then Exit;
  end;
end;
//------------------------------------------------------------------------------

function IsValidBitFields(const bitFields: TTriColor32): boolean;
begin
  //make sure each color channel has a mask and that they don't overlap ...
  result := (bitFields[0] <> 0) and (bitFields[1] <> 0) and
    (bitFields[2] <> 0) and (bitFields[0] and bitFields[1] = 0) and
    (bitFields[0] and bitFields[2] = 0) and (bitFields[1] and bitFields[2] = 0);
end;
//------------------------------------------------------------------------------

function AlphaChannelAllZero(img32: TImage32): Boolean;
var
  i: integer;
  pc: PARGB;
begin
  result := false;
  pc := PARGB(img32.PixelBase);
  for i := 0 to img32.Width * img32.Height -1 do
  begin
    if (pc.A > 0) then Exit;
    inc(pc);
  end;
  result := true;
end;
//------------------------------------------------------------------------------

procedure ResetAlphaChannel(img32: TImage32);
var
  i: integer;
  pc: PARGB;
begin
  pc := PARGB(img32.PixelBase);
  for i := 0 to img32.Width * img32.Height -1 do
  begin
    pc.A := 255;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_BMP.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  flag: Cardinal;
const
  SizeOfBitmapInfoHeader = 40;
  SizeOfBitmapV4Header = 108;
  SizeOfBitmapV5Header = 124;
begin
  Result := false;
  savedPos := stream.position;
  if stream.size - savedPos <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  stream.Position := savedPos;
  Result := ((flag and $FFFF) = $4D42) or
    (flag = SizeOfBitmapInfoHeader) or
    (flag = SizeOfBitmapV4Header) or
    (flag = SizeOfBitmapV5Header);
end;
//------------------------------------------------------------------------------

function TImageFormat_BMP.LoadFromStream(stream: TStream;
  img32: TImage32; imgIndex: integer): Boolean;
var
  palEntrySize: integer;
  bihStart: cardinal;
  bfh: TBitmapFileHeader;
  bih: TBitmapInfoHeader;
  tmp, pal: TArrayOfColor32;
  bitfields: TTriColor32;
  isTopDown, hasValidBitFields: boolean;
  pb: PByteArray;
begin
  result := false;
  with stream do
  begin
    if Size < sizeof(bih) then Exit;
    bihStart := stream.Position;

    //some streams (eg resource streams) omit the file header ...
    Read(bfh, SizeOf(bfh));
    if bfh.bfType = $4D42 then
    begin
      inc(bihStart, SizeOf(bfh))
    end else
    begin
      bfh.bfOffBits := 0;
      stream.Position := bihStart;
    end;

    Read(bih, sizeof(bih));

    if bih.biSize < sizeof(bih) then //accommodate dodgy TBitmapInfoHeader's
    begin
      if bih.biSize <sizeof(TBitmapCoreHeader) then Exit;
      pb := @bih.biSize;
      FillChar(pb[bih.biSize], sizeof(bih) - bih.biSize, 0);
    end;

    palEntrySize := 4;
    if bih.biSize = sizeof(TBitmapCoreHeader) then
    begin
      bih.biBitCount     := PBitmapCoreHeader(@bih).bcBitCount;
      bih.biHeight       := PBitmapCoreHeader(@bih).bcHeight;
      bih.biWidth        := PBitmapCoreHeader(@bih).bcWidth;
      bih.biPlanes       := 1;
      bih.biCompression  := 0;
      bih.biClrUsed      := 0;
      palEntrySize       := 3;
    end;

    //make sure this is a valid BMP stream
    //(nb: embedded JPEG and PNG formats not supported)
    if (bih.biBitCount > 32) or
      (bih.biWidth > $3FFF) or (bih.biHeight > $3FFF) or //16,383
      (bih.biCompression > BI_BITFIELDS) then Exit;

    isTopDown := bih.biHeight < 0;
    bih.biHeight := abs(bih.biHeight);

    if //(bih.biBitCount < 32) and
      ((bih.biCompression and BI_BITFIELDS) = BI_BITFIELDS) then
    begin
      stream.Position := bihStart + 40;
      stream.Read(bitfields[0], Sizeof(TTriColor32));
      hasValidBitFields := IsValidBitFields(bitfields);
      if stream.Position < bihStart + bih.biSize then
        stream.Position := bihStart + bih.biSize;
    end else
    begin
      hasValidBitFields := false;
      stream.Position := bihStart + bih.biSize;
    end;

    if not hasValidBitFields then
    begin
      if bih.biBitCount = 24 then
      begin
        bitfields[0] := $FF shl 16;
        bitfields[1] := $FF shl 8;
        bitfields[2] := $FF;
        hasValidBitFields := true;
      end
      else if bih.biBitCount = 16 then
      begin
        bitfields[0] := $1F shl 10;
        bitfields[1] := $1F shl 5;
        bitfields[2] := $1F;
        hasValidBitFields := true;
      end;
    end;

    if bih.biClrUsed > 256 then bih.biClrUsed := 0;

    if (bih.biClrUsed = 0) and (bih.biBitCount < 16) then
      bih.biClrUsed := Trunc(Power(2, bih.biBitCount));

    if bih.biClrUsed > 0 then
      pal := StreamReadPalette(stream, bih.biClrUsed, palEntrySize);

    tmp := nil;
    result := true;
    img32.BeginUpdate;
    try
      img32.SetSize(bih.biWidth, bih.biHeight);

      //read pixels ....
      if stream.Position < bfh.bfOffBits then stream.Position := bfh.bfOffBits;

      if hasValidBitFields then
        tmp := StreamReadImageWithBitfields(
          stream, img32.Width, img32.Height, bih.biBitCount, bitfields)
      else if (bih.biBitCount = 32) then
      begin
        Read(img32.Pixels[0], bih.biWidth * bih.biHeight * sizeof(TColor32));
        if AlphaChannelAllZero(img32) then ResetAlphaChannel(img32);
      end
      else if (bih.biCompression = BI_RLE8) or (bih.biCompression = BI_RLE4) then
        tmp := ReadRLE4orRLE8Compression(
          stream, img32.Width, img32.Height, bih.biBitCount, pal)

      else tmp := StreamReadImageWithPalette(
        stream, img32.Width, img32.Height, bih.biBitCount, pal);

      if assigned(tmp) and (length(tmp) = img32.Width * img32.Height) then
        move(tmp[0], img32.Pixels[0], length(tmp) * sizeof(TColor32));

      if not isTopDown then img32.FlipVertical;
    finally
      img32.EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) BMP images to file ...
//------------------------------------------------------------------------------

function GetFileHeaderFromInfoHeader(stream: TStream;
  BitmapInfoHeaderOffset: integer): TBitmapFileHeader;
var
  bih: TBitmapInfoHeader;
begin
  FillChar(Result, sizeof(Result), #0);
  Result.bfType := $4D42;
  stream.Position := BitmapInfoHeaderOffset;
  stream.Read(bih, sizeof(bih));
  if (bih.biWidth = 0) or (bih.biHeight = 0) then Exit;
  Result.bfSize := bih.biSizeImage;
  Result.bfOffBits := (stream.Size - bih.biSizeImage);
end;
//------------------------------------------------------------------------------

function MakeBitfields: TTriColor32;
begin
  result[0] := $FF0000;
  result[1] := $00FF00;
  result[2] := $0000FF;
end;
//------------------------------------------------------------------------------

function GetRowSize(bitCount, imageWidth: integer): integer;
begin
  result := ((31 + BitCount * imageWidth) div 32) * 4;
end;
//------------------------------------------------------------------------------

function Find(color: TColor32; const colors: TArrayOfColor32;
  colorsCnt: integer; out idx: integer): Boolean;
var
  i,l,r: integer;
begin
  //binary search a sorted list ...
  Result := False;
  l := 0; r := colorsCnt -1;
  while l <= r do
  begin
    idx := (l + r) shr 1;

    i := integer(colors[idx]) - integer(color);
    if i < 0 then l := idx +1
    else
    begin
      r := idx -1;
      if i = 0 then
      begin
        result := true;
        l := idx;
      end;
    end;
  end;
  idx := l;
end;
//------------------------------------------------------------------------------

function IndexOf(color: TColor32; const colors: TArrayOfColor32): integer;
var
  i,l,r: integer;
begin
  //binary search a sorted list ...
  l := 0; r := Length(colors) -1;
  while l <= r do
  begin
    result := (l + r) shr 1;
    i := integer(colors[result]) - integer(color);
    if i < 0 then l := result +1
    else
    begin
      r := result -1;
      if i = 0 then l := result;
    end;
  end;
  result := l;
end;
//------------------------------------------------------------------------------

procedure Insert256(color: TColor32;
  var colors256: TArrayOfColor32; cnt, idx: integer);
begin
  if idx < cnt then
    move(colors256[idx], colors256[idx +1], (cnt - idx) * SizeOf(TColor32));
  colors256[idx] := color;
end;
//------------------------------------------------------------------------------

function GetPaletteColors(img32: TImage32): TArrayOfColor32;
var
  i, idx, palLen: integer;
  c: TColor32;
  pc: PColor32;
begin
  Result := nil;
  if img32.IsEmpty then Exit;
  SetLength(Result, 256);
  palLen := 0;
  pc := PColor32(img32.PixelBase);
  for i := 0 to img32.Width * img32.Height -1 do
  begin
    c := pc^ and $FFFFFF;
    if not Find(c, result, palLen, idx) then
    begin
      if palLen = 256 then
      begin
        result := nil; //too many colors for a palette
        Exit;
      end;
      Insert256(c, result, palLen, idx);
      inc(palLen);
    end;
    inc(pc);
  end;
  SetLength(Result, palLen);
end;
//------------------------------------------------------------------------------

procedure StreamWriteLoBitImage(img32: TImage32; const pals: TArrayOfColor32;
  BitCount: integer; stream: TStream);
var
  i, j, k, pxlPerByte, rowSize, delta, shiftSize, totalBytes: integer;
  buffer: TArrayOfByte;
  pSrc: PColor32;
  pDst: PByte;
begin
  pxlPerByte := 8 div BitCount;
  rowSize := GetRowSize(BitCount, img32.Width);
  delta := rowSize - img32.Width div pxlPerByte;
  //delphi doesn't handle modulo of negatives as expected so ...
  shiftSize := img32.Width mod pxlPerByte;
  if shiftSize > 0 then shiftSize := pxlPerByte - shiftSize;
  totalBytes := rowSize * img32.Height;
  SetLength(buffer, totalBytes);
  fillChar(buffer[0], totalBytes, 0);
  pSrc := img32.PixelBase;
  pDst := @buffer[0];
  for i := 1 to img32.Height do
  begin
    k := 0;
    for j := 1 to img32.Width do
    begin
      k := k shl BitCount + IndexOf(pSrc^ and $FFFFFF, pals);
      if (j mod pxlPerByte = 0) then
      begin
        Byte(pDst^) := k;
        inc(pDst);
        k := 0;
      end;
      inc(pSrc);
    end;
    if shiftSize > 0 then Byte(pDst^) := k shl shiftSize;
    inc(pDst, delta);
  end;
  stream.Write(buffer[0], totalBytes);
end;
//------------------------------------------------------------------------------

procedure StreamWrite24BitImage(img32: TImage32; stream: TStream);
var
  i,j, delta, rowSize, totalBytes: integer;
  buffer: TArrayOfByte;
  pc: PColor32;
  pb: PByte;
begin
  //rowSize = img32.Width *3 then rounded up to a multiple of 4
  rowSize := GetRowSize(24, img32.Width);
  delta := rowSize - (img32.Width *3);
  totalBytes := rowSize * img32.Height;
  setLength(buffer, totalBytes);
  fillChar(buffer[0], totalBytes, 0);
  pb := @buffer[0];
  pc := img32.PixelBase;
  for i := 0 to img32.Height -1 do
  begin
    for j := 0 to img32.Width -1 do
    begin
      Move(pc^, pb^, 3); //ie skipping the alpha byte
      inc(pc); inc(pb, 3);
    end;
    inc(pb, delta);
  end;
  stream.Write(buffer[0], totalBytes); //much faster to do this once
end;
//------------------------------------------------------------------------------

procedure TImageFormat_BMP.SaveToStream(stream: TStream;
  img32: TImage32; compressionQuality: integer = 0);
var
  bfh: TBitmapFileHeader;
  bih: TBitmapV4Header;
  palCnt, BitCount, rowSize, infoHeaderOffset: integer;
  UsesAlpha: Boolean;
  pals: TArrayOfColor32;
  tmp: TImage32;
begin
  //write everything except a BMP file header because some streams
  //(eg resource streams) don't need a file header

  if fUseClipboardFormat then
    UsesAlpha := false else
    UsesAlpha := img32.HasTransparency;

  if fUseClipboardFormat or UsesAlpha then
  begin
    BitCount := 32;
    palCnt := 0;
  end else
  begin
    pals := GetPaletteColors(img32);
    palCnt := Length(pals);
    if palCnt = 0 then BitCount := 24
    else if palCnt > 16 then BitCount := 8
    else if palCnt > 2 then BitCount := 4
    else BitCount := 1;
  end;

  if fIncludeFileHeaderInSaveStream then
  begin
    //Write empty BitmapFileHeader ...
    FillChar(bfh, sizeof(bfh), #0);
    stream.Write(bfh, sizeOf(bfh));
  end;
  infoHeaderOffset := stream.Position;

  FillChar(bih, sizeof(bih), #0);
  rowSize := GetRowSize(BitCount, img32.Width);
  bih.bV4Width := img32.Width;
  bih.bV4Height := img32.Height;
  bih.bV4Planes := 1;
  bih.bV4BitCount := BitCount;
  bih.bV4ClrUsed := palCnt;
  bih.bV4SizeImage := rowSize * img32.Height;
  if UsesAlpha then
  begin
    bih.bV4Size := sizeof(TBitmapV4Header);
    bih.bV4V4Compression := BI_BITFIELDS;
    bih.bV4RedMask       := $FF shl 16;
    bih.bV4GreenMask     := $FF shl 8;
    bih.bV4BlueMask      := $FF;
    bih.bV4AlphaMask     := Cardinal($FF) shl 24;
  end else
  begin
    bih.bV4Size := sizeof(TBitmapInfoHeader); //Version2 header
    bih.bV4V4Compression := BI_RGB;
  end;

  tmp := TImage32.Create(img32);
  try
    tmp.FlipVertical;

    case BitCount of
    1,4,8:
      begin
        stream.Write(bih, bih.bV4Size);
        SetLength(pals, palCnt);
        stream.Write(pals[0], palCnt * 4);
        StreamWriteLoBitImage(tmp, pals, BitCount, stream);
      end;
    24:
      begin
        stream.Write(bih, bih.bV4Size);
        // nb: BI_BITFIELDS only used in 16bpp and 32bpp formats
        // See BITMAPINFOHEADER structure
        StreamWrite24BitImage(tmp, stream);
      end
    else
      begin
        stream.Write(bih, bih.bV4Size);
        stream.Write(tmp.Pixels[0], tmp.Width * tmp.Height * sizeof(TColor32));
      end;
    end;
  finally
    tmp.Free;
  end;

  if fIncludeFileHeaderInSaveStream then
  begin
    //finally update BitmapFileHeader ...
    bfh := GetFileHeaderFromInfoHeader(stream, infoHeaderOffset);
    stream.Position := infoHeaderOffset - sizeOf(bfh);
    stream.Write(bfh, sizeOf(bfh));
  end;

end;
//------------------------------------------------------------------------------

function TImageFormat_BMP.SaveToFile(const filename: string;
  img32: TImage32; compressionQuality: integer = 0): Boolean;
var
  SaveStateIncludeFileHeader: Boolean;
  stream: TFilestream;
begin
  result := not img32.IsEmpty;
  if not result then Exit;
  SaveStateIncludeFileHeader := fIncludeFileHeaderInSaveStream;
  stream := TFileStream.Create(filename, fmCreate);
  try
    fIncludeFileHeaderInSaveStream := true;
    fUseClipboardFormat := false;
    SaveToStream(stream, img32);
  finally
    stream.Free;
    fIncludeFileHeaderInSaveStream := SaveStateIncludeFileHeader;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

class function TImageFormat_BMP.CanCopyToClipboard: Boolean;
begin
  Result := true;
end;
//------------------------------------------------------------------------------

class function TImageFormat_BMP.CopyToClipboard(img32: TImage32): Boolean;
var
  dataHdl: THandle;
  dataPtr: pointer;
  ms: TMemoryStream;
begin
  result := OpenClipboard(0);
  if not Result then Exit;
  ms := TMemoryStream.Create;
  try
    with TImageFormat_BMP.Create do
    try
      fUseClipboardFormat := true;
      SaveToStream(ms, img32);
    finally
      free;
    end;

    dataHdl := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ms.Size);
    try
      dataPtr := GlobalLock(dataHdl);
      try
        Move(ms.Memory^, dataPtr^, ms.Size);
      finally
        GlobalUnlock(dataHdl);
      end;
      if SetClipboardData(CF_DIB, dataHdl) = 0 then
        raise Exception.Create(s_cf_dib_error);
    except
      GlobalFree(dataHdl);
      raise;
    end;

  finally
    ms.free;
    CloseClipboard;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_BMP.CanPasteFromClipboard: Boolean;
begin
  result := IsClipboardFormatAvailable(CF_DIB) or
    IsClipboardFormatAvailable(CF_BITMAP);
end;
//------------------------------------------------------------------------------

class function TImageFormat_BMP.PasteFromClipboard(img32: TImage32): Boolean;
var
  dataHdl: THandle;
  bitmapHdl: HBITMAP;
  paletteHdl: HPALETTE;
  dataPtr: pointer;
  ms: TMemoryStream;
begin
  result := OpenClipboard(0);
  if not Result then Exit;

  try
    if IsClipboardFormatAvailable(CF_DIB) then
    begin
      ms := TMemoryStream.Create;
      try
        dataHdl := GetClipboardData(CF_DIB);
        result := dataHdl > 0;
        if not result then Exit;

        ms.SetSize(GlobalSize(dataHdl));
        dataPtr := GlobalLock(dataHdl);
        try
          Move(dataPtr^, ms.Memory^, ms.Size);
        finally
          GlobalUnlock(dataHdl);
        end;

        ms.Position := 0;
        with TImageFormat_BMP.Create do
        try
          LoadFromStream(ms, img32);
        finally
          Free;
        end;
      finally
        ms.free;
      end;

    end
    else if IsClipboardFormatAvailable(CF_BITMAP) then
    begin
      bitmapHdl := GetClipboardData(CF_BITMAP);
      if IsClipboardFormatAvailable(CF_PALETTE) then
        paletteHdl := GetClipboardData(CF_PALETTE) else
        paletteHdl := 0;
      result := bitmapHdl > 0;
      if result then
        LoadFromHBITMAP(img32, bitmapHdl, paletteHdl);
    end;

  finally
    CloseClipboard;
  end;
end;

//------------------------------------------------------------------------------

function LoadFromHBITMAP(img32: TImage32; bm: HBITMAP; pal: HPALETTE): Boolean;
var
  w, h: integer;
  memDc: HDC;
  oldBitmap, oldPalette: HGDIOBJ;
  bi: BITMAPINFO;
begin
  result := false;
  memDC := CreateCompatibleDC(0);
  try
    oldBitmap := SelectObject(memDC, bm);
    if (pal > 0) then
    begin
      oldPalette := SelectPalette(memDC, pal, FALSE);
      RealizePalette(memDc);
    end else
      oldPalette := 0;

    FillChar(bi, SizeOf(bi), 0);
    bi.bmiHeader.biSize := SizeOf(bi);
    if GetDIBits(memDc, bm, 0, 0, nil, bi, DIB_RGB_COLORS) = 0 then Exit;
    h := abs(bi.bmiHeader.biHeight);
    bi.bmiHeader.biHeight := h;
    w := bi.bmiHeader.biWidth;
    bi.bmiHeader.biBitCount := 32;
    bi.bmiHeader.biCompression := BI_RGB;

    img32.BeginUpdate;
    try
      img32.SetSize(w, h);
      if GetDIBits(memDc, bm, 0, h,
        PByte(img32.PixelBase), bi, DIB_RGB_COLORS) = 0 then Exit;
      img32.FlipVertical;
    finally
      img32.EndUpdate;
    end;

    SelectObject(memDC, oldBitmap);
    if (oldPalette > 0) then
      SelectObject(memDC, oldPalette);
    Result := true;
  finally
    DeleteDC(memDC);
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

initialization
  TImage32.RegisterImageFormatClass('BMP', TImageFormat_BMP, cpLow);

end.
