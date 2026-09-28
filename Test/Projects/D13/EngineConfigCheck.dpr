/// <summary>
///  Build check for the PreferNativeSvgSupport configuration of
///  SVGIconImageList.inc: the project defines it (see the .dproj), so the fact
///  that it compiles at all is half of the check. The other half: at run time
///  the global factory must exist (Direct2D, or the fallback engine where
///  Windows has no SVG support) and must render.
///  Exit code: 0 = OK, 1 = no factory or wrong rendering.
/// </summary>
program EngineConfigCheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Types,
  Winapi.Windows,
  Vcl.Graphics,
  SVGInterfaces;

const
  SVG_RED_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="0" y="0" width="100" height="100" fill="#FF0000"/></svg>';

var
  LSVG: ISVG;
  LBitmap: TBitmap;
  LPixel: TColor;
begin
  try
    Writeln('Engine: ', GetGlobalSVGFactoryDesc);
    if GlobalSVGFactory = nil then
    begin
      Writeln('[FAIL] GlobalSVGFactory is nil');
      Halt(1);
    end;
    LSVG := GlobalSVGFactory.NewSvg;
    LSVG.Source := SVG_RED_SQUARE;
    LBitmap := TBitmap.Create;
    try
      LBitmap.PixelFormat := pf32bit;
      LBitmap.SetSize(20, 20);
      LBitmap.Canvas.Brush.Color := clWhite;
      LBitmap.Canvas.FillRect(Rect(0, 0, 20, 20));
      LSVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, 20, 20));
      LPixel := ColorToRGB(LBitmap.Canvas.Pixels[10, 10]);
    finally
      LBitmap.Free;
    end;
    if (GetRValue(LPixel) < 200) or (GetGValue(LPixel) > 60) or (GetBValue(LPixel) > 60) then
    begin
      Writeln(Format('[FAIL] center pixel is %.6x, expected red', [LPixel]));
      Halt(1);
    end;
    Writeln('[OK] PreferNativeSvgSupport configuration builds and renders');
  except
    on E: Exception do
    begin
      Writeln('[FAIL] ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
