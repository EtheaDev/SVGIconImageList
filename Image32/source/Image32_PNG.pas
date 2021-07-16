unit Image32_PNG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.27                                                            *
* Date      :  15 July 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  PNG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}
{$IFDEF DELPHI_PNG}

uses
  SysUtils, Classes, Windows,
  {$IFDEF FPC} Graphics {$ELSE} Math, PngImage {$ENDIF}, Image32;

type

  TImageFormat_PNG = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CanCopyToClipboard: Boolean; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

var
  CF_PNG: Cardinal = 0;     //Windows Clipboard
  CF_IMAGEPNG: Cardinal = 0;

{$ENDIF}
implementation

{$IFDEF DELPHI_PNG}
resourcestring
  s_cf_png_error      = 'TImage32 - PNG clipboard format error';
  s_cf_imagepng_error = 'TImage32 - image/png clipboard format error';

//------------------------------------------------------------------------------
// Loading (reading) PNG images from file ...
//------------------------------------------------------------------------------

class function TImageFormat_PNG.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  flag: Cardinal;
begin
  Result := false;
  savedPos := stream.position;
  if stream.size - savedPos <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  stream.Position := savedPos;
  result := flag = $474E5089;
end;
//------------------------------------------------------------------------------


{$IFDEF FPC}
function TImageFormat_PNG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.LoadFromStream(stream);
    result := (png.Width * png.Height > 0) and
      (png.RawImage.Description.BitsPerPixel = 32);
    if not Result then Exit;

    img32.BeginUpdate;
    try
      img32.SetSize(png.Width, png.Height);
      Move(png.ScanLine[0]^, img32.PixelBase^, png.Width * png.Height *4);
    finally
      img32.EndUpdate;
    end;
  finally
    png.Free;
  end;
end;
//------------------------------------------------------------------------------

{$ELSE}

function TImageFormat_PNG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  i,j: integer;
  png: TPngImage;
  BitDepth: integer;
  pAlpha, pColor: PByte;
  pDst: PARGB;
  transColor: ColorRef;
  palleteChunk: TChunkPLTE;
begin
  result := false;

  img32.BeginUpdate;
  png := TPngImage.Create;
  try
    png.LoadFromStream(stream);
    if png.Transparent then
      transColor := png.TransparentColor else
      transColor := DWord(-1);
    if png.Empty then Exit;

    img32.SetSize(png.Header.Width, png.Header.Height);

    BitDepth := png.Header.BitDepth;
    if BitDepth <> 8 then Exit;
    case png.Header.ColorType of

      COLOR_GRAYSCALE:
      begin
        for i := 0 to img32.Height -1 do
        begin
          pColor := png.Scanline[i];
          pDst := PARGB(img32.PixelRow[i]);
          for j := 0 to img32.Width -1 do
          begin
            pDst.B := pColor^;
            pDst.G := pColor^;
            pDst.R := pColor^;
            if (pDst.Color = transColor) then
              pDst.A := 0 else
              pDst.A := 255;
            inc(pColor); inc(pDst);
          end;
        end;
      end;

      COLOR_PALETTE:
      begin
         palleteChunk := TChunkPLTE(png.Chunks.FindChunk(TChunkPLTE));
         if palleteChunk = nil then Exit;
        for i := 0 to img32.Height -1 do
        begin
          pColor := png.Scanline[i];
          pDst := PARGB(img32.PixelRow[i]);
          for j := 0 to img32.Width -1 do
          begin
            pDst.Color := TColor32(palleteChunk.Item[pColor^]);
            if (pDst.Color = transColor) then
              pDst.A := 0 else
              pDst.A := 255;
            inc(pColor); inc(pDst);
          end;
        end;
      end;

      COLOR_GRAYSCALEALPHA:
      begin
        for i := 0 to img32.Height -1 do
        begin
          pColor := png.Scanline[i];
          pDst := PARGB(img32.PixelRow[i]);
          pAlpha := @png.AlphaScanline[i][0];
          for j := 0 to img32.Width -1 do
          begin
            pDst.A := pAlpha^; inc(pAlpha);
            pDst.B := pColor^;
            pDst.G := pColor^;
            pDst.R := pColor^;  inc(pColor);
            inc(pDst); inc(pAlpha);
          end;
        end;
      end;

      COLOR_RGB:
      begin
        for i := 0 to img32.Height -1 do
        begin
          pColor := png.Scanline[i];
          pDst := PARGB(img32.PixelRow[i]);
          for j := 0 to img32.Width -1 do
          begin
            pDst.B := pColor^;  inc(pColor);
            pDst.G := pColor^;  inc(pColor);
            pDst.R := pColor^;  inc(pColor);
            if (pDst.Color = transColor) then
              pDst.A := 0 else
              pDst.A := 255;
            inc(pDst);
          end;
        end;
      end;

      COLOR_RGBALPHA:
      begin
        for i := 0 to img32.Height -1 do
        begin
          pAlpha := @png.AlphaScanline[i][0];
          pColor := PByte(png.Scanline[i]);
          pDst   := PARGB(img32.PixelRow[i]);
          for j := 0 to img32.Width -1 do
          begin
            pDst.A := pAlpha^; inc(pAlpha);
            pDst.B := pColor^;  inc(pColor);
            pDst.G := pColor^;  inc(pColor);
            pDst.R := pColor^;  inc(pColor);
            inc(pDst);
          end;
        end;
      end;

      else Exit; //error
    end;

    result := true;
  finally
    png.Free;
    img32.EndUpdate;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Saving (writing) PNG images to file ...
//------------------------------------------------------------------------------

{$IFDEF FPC}
procedure TImageFormat_PNG.SaveToStream(stream: TStream; img32: TImage32);
var
  png: TPortableNetworkGraphic;
begin
  if (img32.Width * img32.Height = 0) then Exit;

  img32.BeginUpdate;
  png := TPortableNetworkGraphic.Create;
  try
    png.SetSize(img32.Width, img32.Height);
    png.PixelFormat := pf32bit;
    Move(img32.PixelBase^, png.ScanLine[0]^, img32.Width * img32.Height *4);
    png.SaveToStream(stream);
  finally
    png.Free;
    img32.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------
{$ELSE}
procedure TImageFormat_PNG.SaveToStream(stream: TStream; img32: TImage32);
var
  i,j: integer;
  png: TPngImage;
  srcColor: PARGB;
  pAlpha, pCol: PByte;
begin
  png := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, img32.Width, img32.Height);
  try
    png.CreateAlpha;
    for i := 0 to img32.Height -1 do
    begin
      srcColor := PARGB(img32.PixelRow[i]);
      pCol     := png.Scanline[i];
      pAlpha   := @png.AlphaScanline[i][0];

      for j := 0 to img32.Width -1 do
      begin
        pAlpha^ :=  srcColor.A;
        if srcColor.A > 0 then
        begin
          pCol^ := srcColor.B; inc(pCol);
          pCol^ := srcColor.G; inc(pCol);
          pCol^ := srcColor.R; inc(pCol);
        end else
        begin
          pCol^ := 255; inc(pCol);
          pCol^ := 255; inc(pCol);
          pCol^ := 255; inc(pCol);
        end;
        inc(srcColor); inc(pAlpha);
      end;
    end;
    png.SaveToStream(stream);
  finally
    png.Free;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

class function TImageFormat_PNG.CanCopyToClipboard: Boolean;
begin
  Result := true;
end;
//------------------------------------------------------------------------------

class function TImageFormat_PNG.CopyToClipboard(img32: TImage32): Boolean;
var
  dataHdl: THandle;
  dataPtr: pointer;
  ms: TMemoryStream;
begin
  result := ((CF_PNG > 0) or (CF_IMAGEPNG > 0)) and OpenClipboard(0);
  if not result then Exit;

  ms := TMemoryStream.Create;
  try
    with TImageFormat_PNG.Create do
    try
      SaveToStream(ms, img32);
    finally
      free;
    end;

    //nb: clipboard should already be open
    dataHdl := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, ms.Size);
    try
      dataPtr := GlobalLock(dataHdl);
      try
        Move(ms.Memory^, dataPtr^, ms.Size);
      finally
        GlobalUnlock(dataHdl);
      end;
      if CF_PNG > 0 then
      begin
        if SetClipboardData(CF_PNG, dataHdl) = 0 then
          raise Exception.Create(s_cf_png_error);
      end
      else if SetClipboardData(CF_IMAGEPNG, dataHdl) = 0 then
          raise Exception.Create(s_cf_imagepng_error);
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

class function TImageFormat_PNG.CanPasteFromClipboard: Boolean;
begin
  result := IsClipboardFormatAvailable(CF_PNG) or
    IsClipboardFormatAvailable(CF_IMAGEPNG);
end;
//------------------------------------------------------------------------------

class function TImageFormat_PNG.PasteFromClipboard(img32: TImage32): Boolean;
var
  dataHdl: THandle;
  dataPtr: pointer;
  ms: TMemoryStream;
begin
  Result := ((CF_PNG > 0) or (CF_IMAGEPNG > 0)) and OpenClipboard(0);
  if not Result then Exit;

  ms := TMemoryStream.Create;
  try
    dataHdl := 0;
    if (CF_PNG > 0) and IsClipboardFormatAvailable(CF_PNG) then
      dataHdl := GetClipboardData(CF_PNG);
    if (dataHdl = 0) and IsClipboardFormatAvailable(CF_IMAGEPNG) then
      dataHdl := GetClipboardData(CF_IMAGEPNG);

    if dataHdl = 0 then
    begin
      result := false;
      Exit;
    end;

    ms.SetSize(GlobalSize(dataHdl));
    dataPtr := GlobalLock(dataHdl);
    try
      Move(dataPtr^, ms.Memory^, ms.Size);
    finally
      GlobalUnlock(dataHdl);
    end;

    ms.Position := 0;
    with TImageFormat_PNG.Create do
    try
      LoadFromStream(ms, img32);
    finally
      free;
    end;

  finally
    ms.free;
    CloseClipboard;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('PNG', TImageFormat_PNG, cpHigh);
  CF_PNG     := RegisterClipboardFormat('PNG');
  CF_IMAGEPNG := RegisterClipboardFormat('image/png');
{$ENDIF}

end.
