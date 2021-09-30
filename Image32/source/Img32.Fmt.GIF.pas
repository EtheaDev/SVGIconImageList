unit Img32.Fmt.GIF;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.0                                                             *
* Date      :  20 July 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  GIF file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

{$IF COMPILERVERSION = 15}
implementation
{$ELSE}

uses
  SysUtils, Classes, Windows, Math, Img32, Graphics,
  {$IFDEF DELPHI_GIF} GifImg {$ELSE} GifImage {$ENDIF};

type

  TImageFormat_GIF = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

implementation

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

function TImageFormat_GIF.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  gif: TGIFImage;
  i, w,h: integer;
  bmp, bmpT: TBitmap;
  pcT, pc: PARGB;
begin
  result := false;
  bmp := TBitmap.Create;
  bmpT := TBitmap.Create;
  gif := TGIFImage.Create;
  try
    gif.LoadFromStream(stream);
    if gif.Empty then Exit;
    w := gif.Width; h := gif.Height;
    bmp.SetSize(w, h);
    bmp.PixelFormat := pf32bit;
    //get color pixels
    gif.Images[0].Draw(bmp.Canvas, bmp.Canvas.ClipRect, false, false);
    if gif.Transparent then
    begin
      bmpT.SetSize(w, h);
      bmpT.PixelFormat := pf32bit;
      //get transparent pixels
      gif.Images[0].Draw(bmpT.Canvas, bmpT.Canvas.ClipRect, true, false);
      //set the alpha in bmp for each pixel in bmpT that's not transparent
      pcT := bmpT.ScanLine[h-1];
      pc := bmp.ScanLine[h-1];
      for I := 0 to w * h -1 do
      begin
        if pcT.A = 0 then pc.A := 255;
        inc(pcT); inc(pc);
      end;
    end;
    img32.BeginUpdate;
    try
      //copy bmp to img32
      img32.SetSize(w, h);
      for I := 0 to h -1 do
        Move(bmp.ScanLine[i]^, img32.PixelRow[i]^, w * 4);
    finally
      img32.EndUpdate;
    end;
    result := true;
  finally
    gif.Free;
    bmp.Free;
    bmpT.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) gif images to file ...
//------------------------------------------------------------------------------

procedure TImageFormat_GIF.SaveToStream(stream: TStream; img32: TImage32);
var
  gif: TGIFImage;
begin
  gif := TGIFImage.Create;
  try
    gif.Bitmap.Width := img32.Width;
    gif.Bitmap.Height := img32.Height;
    img32.CopyToDc(gif.Bitmap.Canvas.Handle,0,0, false);
    gif.SaveToStream(stream);
  finally
    gif.Free;
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





