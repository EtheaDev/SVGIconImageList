unit Img32.Fmt.PNG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.0                                                             *
* Date      :  10 January 2022                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2022                                         *
* Purpose   :  PNG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

{$IFDEF DELPHI_PNG}
uses
  SysUtils, Classes, Windows,
  {$IFDEF FPC} Graphics {$ELSE} Math, PngImage {$ENDIF}, Img32;

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
{$ENDIF} //DELPHI_PNG - undefined in old versions

implementation

{$IFDEF DELPHI_PNG}
resourcestring
  s_cf_png_error      = 'TImage32 - PNG clipboard format error';
  s_cf_imagepng_error = 'TImage32 - image/png clipboard format error';

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

function TImageFormat_PNG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  i,j       : integer;
  png       : TPngImage;
  dst       : PARGB;
  srcAlpha  : PByte;
  srcColor  : PByte;
begin
  img32.BeginUpdate;
  png := TPngImage.Create;
  try
    png.LoadFromStream(stream);
    img32.SetSize(png.Width, png.Height);
    for i := 0 to img32.Height -1 do
    begin
      dst      := PARGB(img32.PixelRow[i]);
      srcColor := png.Scanline[i];
      if png.Transparent then
      begin
        srcAlpha := PByte(png.AlphaScanline[i]);
        for j := 0 to img32.Width -1 do
        begin
          dst.A := srcAlpha^; inc(srcAlpha);
          dst.B := srcColor^; inc(srcColor);
          dst.G := srcColor^; inc(srcColor);
          dst.R := srcColor^; inc(srcColor);
          inc(dst);
        end
      end else
        for j := 0 to img32.Width -1 do
        begin
          dst.A := 255;
          dst.B := srcColor^; inc(srcColor);
          dst.G := srcColor^; inc(srcColor);
          dst.R := srcColor^; inc(srcColor);
          inc(dst);
        end;
    end;
  finally
    png.Free;
    img32.EndUpdate;
  end;
  result := true;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_PNG.SaveToStream(stream: TStream; img32: TImage32);
var
  i,j: integer;
  png: TPngImage;
  srcColor: PARGB;
  dstAlpha, dstColor: PByte;
begin
  png := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, img32.Width, img32.Height);
  try
    png.CreateAlpha;
    for i := 0 to img32.Height -1 do
    begin
      srcColor := PARGB(img32.PixelRow[i]);
      dstColor := png.Scanline[i];
      dstAlpha := PByte(png.AlphaScanline[i]);
      for j := 0 to img32.Width -1 do
      begin
        dstAlpha^ :=  srcColor.A;
        if srcColor.A > 0 then
        begin
          dstColor^ := srcColor.B; inc(dstColor);
          dstColor^ := srcColor.G; inc(dstColor);
          dstColor^ := srcColor.R; inc(dstColor);
        end else
        begin
          dstColor^ := 255; inc(dstColor);
          dstColor^ := 255; inc(dstColor);
          dstColor^ := 255; inc(dstColor);
        end;
        inc(srcColor); inc(dstAlpha);
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
