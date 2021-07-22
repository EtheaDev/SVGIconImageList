unit Img32.Fmt.JPG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.0                                                             *
* Date      :  20 July 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  JPG/JPEG file format extension for TImage32                     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$IFNDEF FPC}
{$I Img32.inc}
uses
  SysUtils, Classes, Windows, Math, Img32, Graphics, JPEG;

type

  TImageFormat_JPG = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

var
  CF_JPG: Cardinal = 0;     //Windows Clipboard
  CF_IMAGEJPG: Cardinal = 0;

{$ENDIF}
implementation
{$IFNDEF FPC}

//------------------------------------------------------------------------------
// Loading (reading) Jpeg images from file ...
//------------------------------------------------------------------------------

type
  TJpegImageHack = class(TJpegImage);

class function TImageFormat_JPG.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  flag: Cardinal;
begin
  Result := false;
  savedPos := stream.position;
  if stream.size - savedPos <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  stream.Position := savedPos;
  result := flag and $FFFFFF = $FFD8FF;
end;
//------------------------------------------------------------------------------

function TImageFormat_JPG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  jpeg: TJpegImage;
begin
  result := false;
  jpeg := TJpegImage.Create;
  try
    jpeg.LoadFromStream(stream);
    if jpeg.Empty then Exit;
    with TJpegImageHack(jpeg).Bitmap do
      img32.CopyFromDC(Canvas.Handle, Rect(0,0, Width, Height));
    result := true;
  finally
    jpeg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) Jpeg images to file ...
//------------------------------------------------------------------------------

procedure TImageFormat_JPG.SaveToStream(stream: TStream; img32: TImage32);
var
  Jpeg: TJpegImage;
begin
  Jpeg := TJpegImage.Create;
  try
    TJpegImageHack(jpeg).NewImage;
    TJpegImageHack(jpeg).NewBitmap;
    TJpegImageHack(jpeg).Bitmap.Width := img32.Width;
    TJpegImageHack(jpeg).Bitmap.Height := img32.Height;
    img32.CopyToDc(TJpegImageHack(jpeg).Bitmap.Canvas.Handle,0,0, false);
    Jpeg.SaveToStream(stream);
  finally
    Jpeg.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_JPG.CopyToClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;
//------------------------------------------------------------------------------

class function TImageFormat_JPG.CanPasteFromClipboard: Boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_JPG.PasteFromClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('JPG', TImageFormat_JPG, cpLow);
  TImage32.RegisterImageFormatClass('JPEG', TImageFormat_JPG, cpLow);
  //don't bother with clipboard formats as PNG and BMP formats are preferred

{$ENDIF}
end.

