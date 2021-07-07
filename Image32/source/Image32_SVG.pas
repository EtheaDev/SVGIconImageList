unit Image32_SVG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.1                                                             *
* Date      :  12 March 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  SVG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Image32, Image32_SVG_Reader;

type
  TImageFormat_SVG = class(TImageFormat)
  public
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
  r: TRect;
  w,h, sx,sy: double;
begin
  with TSvgReader.Create do
  try
    Result := LoadFromStream(stream);
    if not Result then Exit;
    r := RootElement.GetViewbox;

    //if the current image's dimensions are larger than the
    //SVG's viewbox, then scale the SVG image up to fit
    if not IsEmptyRect(r) then
    begin
      w := RectWidth(r);
      h := RectHeight(r);
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

procedure TImageFormat_SVG.SaveToStream(stream: TStream; img32: TImage32);
begin
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
