unit Img32.Fmt.SVG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.1                                                             *
* Date      :  15 August 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  SVG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Img32, Img32.Vector, Img32.SVG.Reader;

type
  TImageFormat_SVG = class(TImageFormat)
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
  defaultSvgWidth: integer = 800;
  defaultSvgHeight: integer = 600;

implementation

//------------------------------------------------------------------------------
// Loading (reading) PNG images from file ...
//------------------------------------------------------------------------------

function TImageFormat_SVG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  r: TRectWH;
  w,h, sx,sy: double;
begin
  with TSvgReader.Create do
  try
    Result := LoadFromStream(stream);
    if not Result then Exit;
    r := GetViewbox(img32.Width, img32.Height);

    //if the current image's dimensions are larger than the
    //SVG's viewbox, then scale the SVG image up to fit
    if not r.IsEmpty then
    begin
      w := r.Width;
      h := r.Height;
      sx := img32.Width / w;
      sy := img32.Height / h;
      if sy < sx then sx := sy;
      if sx > 1 then
      begin
        w := w * sx;
        h := h * sx;
      end;
      img32.SetSize(Round(w), Round(h));
    end
    else if img32.IsEmpty then
      img32.SetSize(defaultSvgWidth, defaultSvgHeight);
    //draw the SVG image to fit inside the canvas
    DrawImage(img32, True);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) SVG images to file (not currently implemented) ...
//------------------------------------------------------------------------------

class function TImageFormat_SVG.IsValidImageStream(stream: TStream): Boolean;
var
  i, savedPos, len: integer;
  buff: array [1..256] of AnsiChar;
begin
  Result := false;
  savedPos := stream.Position;
  len := Min(256, stream.Size - savedPos);
  stream.Read(buff[1], len);
  stream.Position := savedPos;
  for i := 1 to len -4 do
  begin
    if buff[i] < #9 then Exit
    else if (buff[i] = '<') and
      (buff[i +1] = 's') and
      (buff[i +2] = 'v') and
      (buff[i +3] = 'g') then
    begin
      Result := true;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_SVG.SaveToStream(stream: TStream; img32: TImage32);
begin
  //not enabled
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanCopyToClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CopyToClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanPasteFromClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.PasteFromClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('SVG', TImageFormat_SVG, cpLow);

end.
