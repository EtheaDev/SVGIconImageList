unit Img32.Fmt.JPG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  JPG/JPEG file format extension for TImage32                     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$IFNDEF FPC}
{$I Img32.inc}
uses
  SysUtils, Classes, Windows, Math, Img32, JPEG;

type

  TImageFormat_JPG = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; override;
    //SaveToStream: compressionQuality (range: 0-100%)
    procedure SaveToStream(stream: TStream;
      img32: TImage32; compressionQlty: integer = defaultCompression); override;
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

function TImageFormat_JPG.LoadFromStream(stream: TStream;
  img32: TImage32; imgIndex: integer = 0): Boolean;
var
  jpeg: TJpegImage;
begin
  result := false;
  jpeg := TJpegImage.Create;
  try
    jpeg.LoadFromStream(stream);
    if jpeg.Empty then Exit;

    with TJpegImageHack(jpeg).Bitmap do
    try
      if GetCurrentThreadId <> MainThreadID then Canvas.Lock;
      img32.CopyFromDC(Canvas.Handle, Rect(0,0, Width, Height));
    finally
      if GetCurrentThreadId <> MainThreadID then Canvas.Unlock;
    end;

    result := true;
  finally
    jpeg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) Jpeg images to file ...
//------------------------------------------------------------------------------

procedure TImageFormat_JPG.SaveToStream(stream: TStream;
  img32: TImage32; compressionQlty: integer);
var
  Jpeg: TJpegImage;
begin
  Jpeg := TJpegImage.Create;
  with TJpegImageHack(jpeg) do
  try
    if compressionQuality = defaultCompression then
      jpeg.CompressionQuality := 75 else
      jpeg.CompressionQuality := Max(0, Min(100, compressionQuality));
    NewImage;
    NewBitmap;
    Bitmap.Width := img32.Width;
    Bitmap.Height := img32.Height;

    if GetCurrentThreadId <> MainThreadID then Bitmap.Canvas.Lock;
    try
      img32.CopyToDc(Bitmap.Canvas.Handle, 0, 0, false);
    finally
      if GetCurrentThreadId <> MainThreadID then Bitmap.Canvas.Unlock;
    end;
    SaveToStream(stream);
  finally
    Free;
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

