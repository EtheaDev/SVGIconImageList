unit Img32.Fmt.GIF;
(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  GIF file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface
{$I Img32.inc}
{$IF COMPILERVERSION = 15}
implementation
{$ELSE}

uses
  SysUtils, Classes, Windows, Math, Img32,
  {$IFDEF USES_NAMESPACES} Vcl.Graphics, {$ELSE}Graphics, {$ENDIF}
  {$IFDEF DELPHI_GIF} GifImg {$ELSE} GifImage {$ENDIF};

type

  TImageFormat_GIF = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; override;
    procedure SaveToStream(stream: TStream;
      img32: TImage32; quality: integer = 0); override;
    class function GetImageCount(stream: TStream): integer; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;

    class function GetDelays(stream: TStream): TArrayOfInteger;
  end;

implementation

uses
  Img32.Extra;

//------------------------------------------------------------------------------
// Loading (reading) GIF images from file ...
//------------------------------------------------------------------------------

class function TImageFormat_GIF.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  flag: Cardinal;
begin
  Result := false;
  savedPos := stream.position;
  if stream.size - savedPos <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  stream.Position := savedPos;
  result := flag = $38464947;
end;
//------------------------------------------------------------------------------

function TImageFormat_GIF.LoadFromStream(stream: TStream;
  img32: TImage32; imgIndex: integer = 0): Boolean;
var
  gif: TGIFImage;
  i: integer;
  pb: PByte;
  pc: PARGB;
  palentries : array[0..255] of TPaletteEntry;
  img: TImage32;
  rec: TRect;
begin
  result := false;
  img := TImage32.Create;
  gif := TGIFImage.Create;
  try
    gif.LoadFromStream(stream);
    if gif.Empty then Exit;
    if (imgIndex < 0) or (imgIndex >= gif.Images.Count) then
      imgIndex := 0;

    if (imgIndex = 0) or (img32.Width <> gif.Width) or
      (img32.Height <> gif.Height) then
        img32.SetSize(gif.Width, gif.Height);

    with gif.Images[imgIndex] do
    begin
      if DataSize < (Width * Height) then Exit;
      GetPaletteEntries(Palette, 0, 256, palentries);
      FixPalette(@palentries[0], 256);

      if Transparent and Assigned(GraphicControlExtension) then
        with GraphicControlExtension do
          Cardinal(palentries[TransparentColorIndex]) := 0;

      img.SetSize(Width, Height);
      rec := Rect(Left, Top, Left+ Width, Top +Height);
      pb := Data;
      pc := PARGB(img.PixelBase);
      for i := 1 to Width * Height do
      begin
        pc.Color := TColor32(palentries[pb^]);
        inc(pb); inc(pc);
      end;
    end;
    if imgIndex = 0 then
      img32.Assign(img) else
      img32.Copy(img, img.Bounds, rec);
  finally
    gif.Free;
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.GetImageCount(stream: TStream): integer;
var
  gif: TGIFImage;
begin
  gif := TGIFImage.Create;
  try
    gif.LoadFromStream(stream);
    Result := gif.Images.Count;
  finally
    gif.Free;
  end;
end;
//------------------------------------------------------------------------------

type
  THackedGIFFrame = class(TGIFFrame);

class function TImageFormat_GIF.GetDelays(stream: TStream): TArrayOfInteger;
var
  i, cnt: Integer;
  gif: TGIFImage;
begin
  gif := TGIFImage.Create;
  try
    gif.LoadFromStream(stream);
    cnt := gif.Images.Count;
    SetLength(result, cnt);
    for i := 0 to cnt -1 do
      if Assigned(THackedGIFFrame(gif.Images[i])) then
        result[i] := THackedGIFFrame(gif.Images[i]).GCE.Delay;
  finally
    gif.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) gif images to file ...
//------------------------------------------------------------------------------

procedure TImageFormat_GIF.SaveToStream(stream: TStream;
  img32: TImage32; quality: integer = 0);
var
  gif: TGIFImage;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  gif := TGIFImage.Create;
  try
    //copy to the new TBitmap
    bmp.PixelFormat := pf32bit;
    bmp.SetSize(img32.Width, img32.Height);
    bmp.AlphaFormat := afDefined;
    SetBitmapBits(bmp.Handle,
      img32.Width * img32.Height * 4, img32.PixelBase);
    //next copy from the bitmap to the new TGifImage
    gif.Add(bmp);
    //and now save
    gif.SaveToStream(stream);
  finally
    gif.Free;
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.CopyToClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;

//------------------------------------------------------------------------------
class function TImageFormat_GIF.CanPasteFromClipboard: Boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.PasteFromClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('GIF', TImageFormat_GIF, cpLow);

{$IFEND}
end.


