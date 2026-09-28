{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

/// <summary>
///  Helpers shared by the test suite: the SVG documents the tests render, the
///  factory of every engine by name, and the pixel probes that turn "it looks
///  right" into something a test can assert.
/// </summary>
unit SVGTestUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Rtti,
  Winapi.Windows,
  Vcl.Graphics,
  DUnitX.TestFramework,
  SVGInterfaces;

const
  /// <summary>The engines every contract test runs against.</summary>
  ENGINE_IMAGE32 = 'Image32';
  ENGINE_SVGMAGIC = 'SVGMagic';
  ENGINE_SKIA = 'Skia';
  ENGINE_D2D = 'D2D';

  /// <summary>A 100x100 square filled red, edge to edge.</summary>
  SVG_RED_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="0" y="0" width="100" height="100" fill="#FF0000"/></svg>';

  /// <summary>A 100x100 square filled blue, edge to edge.</summary>
  SVG_BLUE_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="0" y="0" width="100" height="100" fill="#0000FF"/></svg>';

  /// <summary>
  ///  An outline icon, shaped like the Lucide/Feather/Tabler sets: the root says
  ///  fill="none" and the shape only has a stroke. The inside of the square is
  ///  meant to stay transparent whatever colour is applied.
  /// </summary>
  SVG_OUTLINE_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" ' +
    'fill="none" stroke="#0000FF" stroke-width="20">' +
    '<rect x="10" y="10" width="80" height="80"/></svg>';

  /// <summary>
  ///  The same outline, drawn with currentColor as the Lucide set does: the
  ///  colour a "root only" FixedColor has to change is the one the root passes
  ///  down, while fill="none" has to stay none.
  /// </summary>
  SVG_OUTLINE_CURRENTCOLOR =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" ' +
    'fill="none" stroke="currentColor" stroke-width="20">' +
    '<rect x="10" y="10" width="80" height="80"/></svg>';

  /// <summary>The colour of the shape comes from the root fill (inherited).</summary>
  SVG_ROOT_FILL_GREEN =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" fill="#00FF00">' +
    '<rect x="0" y="0" width="100" height="100"/></svg>';

  /// <summary>Explicit width/height, different from each other.</summary>
  SVG_SIZED_50x20 =
    '<svg xmlns="http://www.w3.org/2000/svg" width="50" height="20" viewBox="0 0 50 20">' +
    '<rect x="0" y="0" width="50" height="20" fill="#FF0000"/></svg>';

  /// <summary>No width/height attributes: the size comes from the viewBox.</summary>
  SVG_VIEWBOX_ONLY_10x10 =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10 10">' +
    '<rect x="0" y="0" width="10" height="10" fill="#FF0000"/></svg>';

  /// <summary>Colour set through a CSS class in a style block.</summary>
  SVG_STYLE_CLASS_RED =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<style>.a{fill:#FF0000}</style>' +
    '<rect class="a" x="0" y="0" width="100" height="100"/></svg>';

  /// <summary>
  ///  A class attribute BEFORE the style block (on the root) and an element
  ///  that already has the attribute the class sets: CSS wins over the
  ///  presentation attribute, so the square is red.
  /// </summary>
  SVG_STYLE_CLASS_TRICKY =
    '<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 100 100">' +
    '<style type="text/css">.icon{stroke-width:1.5}.a{fill:#FF0000}</style>' +
    '<rect class="a" fill="#000000" x="0" y="0" width="100" height="100"/></svg>';

  /// <summary>
  ///  Ids NESTED in groups (not direct children of the root), referenced by
  ///  &lt;use&gt;: the structure of the W3C SVG logo (svg-logo-v.svg). Red
  ///  bar on the left half, drawn once directly and once through the use
  ///  chain, so the square is red on the left and white on the right.
  /// </summary>
  SVG_NESTED_IDS_WITH_USE =
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 100 100">' +
    '<defs><g id="shapes"><rect id="bar" x="0" y="0" width="50" height="100" fill="#FF0000"/></g></defs>' +
    '<g><g id="star"><use xlink:href="#bar"/></g></g>' +
    '<use xlink:href="#star"/></svg>';

  /// <summary>Well-formed SVG without any drawable child.</summary>
  SVG_EMPTY_DOCUMENT = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';

  /// <summary>Not an SVG at all.</summary>
  NOT_AN_SVG = 'this is not an svg document';

  clPureRed   = TColor($0000FF);
  clPureGreen = TColor($00FF00);
  clPureBlue  = TColor($FF0000);
  clPureWhite = TColor($FFFFFF);
  clPureBlack = TColor($000000);
  clPureYellow = TColor($00FFFF);

type
  /// <summary>
  ///  Runs the decorated test once per engine: the test method takes the engine
  ///  name (one of the ENGINE_* constants) as its only argument.
  /// </summary>
  AllEnginesAttribute = class(CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray: TestCaseInfoArray; override;
  end;

  TSVGTestUtils = class
  public
    /// <summary>The factory of the engine called AEngine (see ENGINE_*).</summary>
    class function Factory(const AEngine: string): ISVGFactory; static;
    /// <summary>A new, empty ISVG of the engine called AEngine.</summary>
    class function NewSvg(const AEngine: string): ISVG; static;
    /// <summary>A new ISVG of the engine called AEngine, holding ASource.</summary>
    class function SvgOf(const AEngine, ASource: string): ISVG; static;
    /// <summary>
    ///  A 32-bit bitmap of AWidth x AHeight filled with ABackground, with ASVG
    ///  painted over the whole of it. The caller frees it.
    /// </summary>
    class function Render(const ASVG: ISVG; const AWidth, AHeight: Integer;
      const ABackground: TColor = clPureWhite;
      const AKeepAspectRatio: Boolean = True): TBitmap; static;
    /// <summary>The RGB colour of the pixel (X, Y), alpha dropped.</summary>
    class function PixelAt(const ABitmap: TBitmap; const X, Y: Integer): TColor; static;
    /// <summary>
    ///  True when every channel of AActual is within ATolerance of AExpected;
    ///  anti-aliasing and colour management are allowed that much slack.
    /// </summary>
    class function SameColor(const AActual, AExpected: TColor;
      const ATolerance: Byte = 40): Boolean; static;
    /// <summary>Colour as #RRGGBB, for the failure messages.</summary>
    class function ColorText(const AColor: TColor): string; static;
    /// <summary>
    ///  Fails the running test unless pixel (X, Y) of ABitmap is AExpected;
    ///  AWhat names the probe in the failure message.
    /// </summary>
    class procedure AssertPixel(const ABitmap: TBitmap; const X, Y: Integer;
      const AExpected: TColor; const AWhat: string); static;
    /// <summary>Renders ASVG and checks one pixel of the result.</summary>
    class procedure AssertRenderedPixel(const ASVG: ISVG; const X, Y: Integer;
      const AExpected: TColor; const AWhat: string;
      const ASize: Integer = 100; const ABackground: TColor = clPureWhite); static;
    /// <summary>A file name in the temporary folder, unique to this run.</summary>
    class function TempFileName(const AExtension: string): string; static;
  end;

implementation

uses
  Image32SVGFactory,
  SVGMagicFactory,
  SkiaSVGFactory,
  D2DSVGFactory;

{ AllEnginesAttribute }

function AllEnginesAttribute.GetCaseInfoArray: TestCaseInfoArray;
const
  ENGINES: array[0..3] of string = (ENGINE_IMAGE32, ENGINE_SVGMAGIC, ENGINE_SKIA, ENGINE_D2D);
var
  I: Integer;
begin
  SetLength(Result, Length(ENGINES));
  for I := 0 to High(ENGINES) do
  begin
    Result[I].Name := ENGINES[I];
    SetLength(Result[I].Values, 1);
    Result[I].Values[0] := TValue.From<string>(ENGINES[I]);
  end;
end;

{ TSVGTestUtils }

class function TSVGTestUtils.Factory(const AEngine: string): ISVGFactory;
begin
  if SameText(AEngine, ENGINE_IMAGE32) then
    Result := GetImage32SVGFactory
  else if SameText(AEngine, ENGINE_SVGMAGIC) then
    Result := GetSVGMagicFactory
  else if SameText(AEngine, ENGINE_SKIA) then
    Result := GetSkiaSVGFactory
  else if SameText(AEngine, ENGINE_D2D) then
  begin
    if not WinSvgSupported then
      Assert.Pass('Direct2D SVG support is not available on this Windows');
    Result := GetD2DSVGFactory;
  end
  else
    raise EArgumentException.CreateFmt('Unknown SVG engine "%s"', [AEngine]);
end;

class function TSVGTestUtils.NewSvg(const AEngine: string): ISVG;
begin
  Result := Factory(AEngine).NewSvg;
end;

class function TSVGTestUtils.SvgOf(const AEngine, ASource: string): ISVG;
begin
  Result := NewSvg(AEngine);
  Result.Source := ASource;
end;

class function TSVGTestUtils.Render(const ASVG: ISVG; const AWidth,
  AHeight: Integer; const ABackground: TColor;
  const AKeepAspectRatio: Boolean): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.SetSize(AWidth, AHeight);
    Result.Canvas.Brush.Color := ABackground;
    Result.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
    ASVG.PaintTo(Result.Canvas.Handle, TRectF.Create(0, 0, AWidth, AHeight),
      AKeepAspectRatio);
  except
    Result.Free;
    raise;
  end;
end;

class function TSVGTestUtils.PixelAt(const ABitmap: TBitmap; const X,
  Y: Integer): TColor;
var
  LRow: PRGBQuad;
begin
  Assert.IsTrue((X >= 0) and (X < ABitmap.Width) and (Y >= 0) and (Y < ABitmap.Height),
    Format('Probe (%d,%d) outside a %dx%d bitmap', [X, Y, ABitmap.Width, ABitmap.Height]));
  LRow := ABitmap.ScanLine[Y];
  Inc(LRow, X);
  Result := RGB(LRow.rgbRed, LRow.rgbGreen, LRow.rgbBlue);
end;

class function TSVGTestUtils.SameColor(const AActual, AExpected: TColor;
  const ATolerance: Byte): Boolean;
begin
  Result :=
    (Abs(GetRValue(AActual) - GetRValue(AExpected)) <= ATolerance) and
    (Abs(GetGValue(AActual) - GetGValue(AExpected)) <= ATolerance) and
    (Abs(GetBValue(AActual) - GetBValue(AExpected)) <= ATolerance);
end;

class function TSVGTestUtils.ColorText(const AColor: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x',
    [GetRValue(AColor), GetGValue(AColor), GetBValue(AColor)]);
end;

class procedure TSVGTestUtils.AssertPixel(const ABitmap: TBitmap; const X,
  Y: Integer; const AExpected: TColor; const AWhat: string);
var
  LActual: TColor;
begin
  LActual := PixelAt(ABitmap, X, Y);
  Assert.IsTrue(SameColor(LActual, AExpected),
    Format('%s: pixel (%d,%d) is %s, expected %s',
      [AWhat, X, Y, ColorText(LActual), ColorText(AExpected)]));
end;

class procedure TSVGTestUtils.AssertRenderedPixel(const ASVG: ISVG; const X,
  Y: Integer; const AExpected: TColor; const AWhat: string;
  const ASize: Integer; const ABackground: TColor);
var
  LBitmap: TBitmap;
begin
  LBitmap := Render(ASVG, ASize, ASize, ABackground);
  try
    AssertPixel(LBitmap, X, Y, AExpected, AWhat);
  finally
    LBitmap.Free;
  end;
end;

class function TSVGTestUtils.TempFileName(const AExtension: string): string;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'SVGIconImageListTests_' + GUIDToString(LGuid).Replace('{', '').Replace('}', '') +
    AExtension;
end;

end.
