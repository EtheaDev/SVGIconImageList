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
///  The ISVG contract, checked against every engine: the components only ever
///  talk to ISVG, so an engine that answers differently from the others shows
///  up as a component bug that happens "only with Skia" or "only with D2D".
/// </summary>
unit SVGEngineTests;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Vcl.Graphics,
  DUnitX.TestFramework,
  SVGInterfaces,
  SVGTestUtils;

type
  [TestFixture]
  TSVGEngineRenderTests = class
  public
    /// <summary>Sanity: a filled square renders its own colour.</summary>
    [Test]
    [AllEngines]
    procedure Paint_FilledSquare_RendersItsColor(const AEngine: string);

    /// <summary>
    ///  Regression (Skia). With no FixedColor the root fill was overridden with
    ///  ColorToAlphaColor(clDefault) = opaque black, so an outline icon
    ///  (root fill="none") came out as a filled black square.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_NoFixedColor_OutlineInteriorStaysTransparent(const AEngine: string);

    /// <summary>
    ///  Regression (Skia). Same cause as above: a colour inherited from the root
    ///  fill turned black.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_NoFixedColor_RootFillIsKept(const AEngine: string);

    /// <summary>Sanity: FixedColor recolours a filled shape.</summary>
    [Test]
    [AllEngines]
    procedure Paint_FixedColor_RecolorsFill(const AEngine: string);

    /// <summary>
    ///  Guard (SVGMagic keeps the recolored document between paints): a new
    ///  FixedColor, or a new source, must not paint the previous one.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_FixedColorOrSourceChanged_UsesTheNewOne(const AEngine: string);

    /// <summary>
    ///  Regression (SVGMagic, D2D). FixedColor overwrote fill="none" with the
    ///  colour, so outline icons were painted as solid shapes.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_FixedColor_OutlineInteriorStaysTransparent(const AEngine: string);

    /// <summary>
    ///  Regression (all engines). With ApplyFixedColorToRootOnly the root
    ///  fill="none" was replaced by the colour, which every child inherits.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_FixedColorRootOnly_OutlineInteriorStaysTransparent(const AEngine: string);

    /// <summary>
    ///  Regression (Image32, D2D). clNone is "no colour": TColor(clNone) is
    ///  $1FFFFFFF, whose RGB bytes are white, and the icon was painted white.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_FixedColorNone_KeepsOriginalColors(const AEngine: string);

    /// <summary>Sanity: GrayScale renders a red square as gray.</summary>
    [Test]
    [AllEngines]
    procedure Paint_GrayScale_RendersGray(const AEngine: string);

    /// <summary>
    ///  Regression (D2D). SetGrayScale assigned the new value before testing it
    ///  to decide whether to reload the document, so switching GrayScale off
    ///  (the sequence ApplyAttributesToInterface uses when an image list goes
    ///  from the disabled image to the normal one) left the icon gray.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_GrayScaleOff_RestoresColors(const AEngine: string);

    /// <summary>Sanity: half opacity on white gives a light red.</summary>
    [Test]
    [AllEngines]
    procedure Paint_HalfOpacity_BlendsWithBackground(const AEngine: string);

    /// <summary>
    ///  Regression (SVGMagic). An empty document filled the target rectangle
    ///  with a white brush instead of painting nothing.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_EmptyDocument_LeavesBackgroundUntouched(const AEngine: string);

    /// <summary>
    ///  Regression (Image32, Skia). An opacity outside 0..1 overflowed the
    ///  Byte it is converted to: ERangeError with range checking on,
    ///  wrap-around without.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Paint_OpacityOutOfRange_IsClamped(const AEngine: string);
  end;

  [TestFixture]
  TSVGEngineDocumentTests = class
  public
    /// <summary>Sanity: the size comes from width/height.</summary>
    [Test]
    [AllEngines]
    procedure Size_AfterSource_IsDocumentSize(const AEngine: string);

    /// <summary>
    ///  Regression (Skia). LoadFromStream never computed the size: Width and
    ///  Height stayed 0, which is what TSVGGraphic (TPicture) reports.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Size_AfterLoadFromStream_IsDocumentSize(const AEngine: string);

    /// <summary>
    ///  Regression (Skia). PaintTo stored the size of the paint rectangle in
    ///  the fields Width/Height read, so after a paint the "document size" was
    ///  the size of the last paint.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Size_AfterPaint_IsStillDocumentSize(const AEngine: string);

    /// <summary>
    ///  Regression (D2D). The size of the previous document was not reset, so
    ///  a document with only a viewBox inherited the width/height of the one
    ///  loaded before it.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Size_ReloadWithViewBoxOnly_IsNotInherited(const AEngine: string);

    /// <summary>
    ///  Regression (Skia). Source returned the text rewritten by InlineSvgStyle
    ///  (style block removed, classes inlined): an icon edited in the IDE was
    ///  saved to the DFM in the rewritten form.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Source_WithStyleBlock_IsReturnedUnchanged(const AEngine: string);

    /// <summary>Regression (Skia). Same as above for SaveToStream.</summary>
    [Test]
    [AllEngines]
    procedure SaveToStream_WithStyleBlock_WritesOriginalText(const AEngine: string);

    /// <summary>
    ///  Regression (Image32: access violation, Skia: silently accepted, D2D:
    ///  EOSError). The contract says ESVGException.
    /// </summary>
    [Test]
    [AllEngines]
    procedure LoadFromStream_NotAnSvg_RaisesESVGException(const AEngine: string);

    /// <summary>
    ///  Regression (SVGMagic library, TWSVGParser). Clear unregistered from
    ///  the defines table only the ids of the direct children of the root:
    ///  the nested ones kept pointing to the freed elements of the previous
    ///  document, and on the next load a &lt;use&gt; of them resolved to freed
    ///  memory (stack overflow or random access violation). It is what
    ///  SvgViewer does with svg-logo-v.svg (LoadFromFile before every paint).
    /// </summary>
    [Test]
    [AllEngines]
    procedure LoadFromStream_Twice_NestedIdsWithUse_Renders(const AEngine: string);

    /// <summary>Regression (Skia: accepted, D2D: plain Exception).</summary>
    [Test]
    [AllEngines]
    procedure Source_NotAnSvg_RaisesESVGException(const AEngine: string);

    /// <summary>
    ///  Regression (Image32, Skia). Clear loaded a stub document, so IsEmpty
    ///  was False and Source returned the stub instead of ''.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Clear_IsEmptyAndSourceIsEmpty(const AEngine: string);

    /// <summary>
    ///  Regression (Image32). Source := '' did not touch the reader, which
    ///  kept, and painted, the previous document.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Source_SetToEmpty_ForgetsPreviousDocument(const AEngine: string);

    /// <summary>
    ///  Regression (D2D). The Opacity getter read the document, not the stored
    ///  value: with no document it always answered 1.
    /// </summary>
    [Test]
    [AllEngines]
    procedure Opacity_Getter_ReturnsAssignedValue(const AEngine: string);
  end;

  /// <summary>
  ///  InlineSvgStyle rewrites the CSS a Skia DOM does not understand into
  ///  presentation attributes; what it writes must still be an SVG.
  /// </summary>
  [TestFixture]
  TSkiaInlineStyleTests = class
  public
    /// <summary>Sanity: a class rule becomes the matching attribute.</summary>
    [Test]
    procedure Inline_SimpleClass_BecomesAttribute;

    /// <summary>
    ///  Regression. A class attribute before the style block shifted the
    ///  positions computed up front, and the final Delete cut the wrong range;
    ///  "type" on the style tag hid the block; the class replacing an existing
    ///  fill gave a duplicate attribute (not well formed XML); the "." of
    ///  "1.5" was read as a selector.
    /// </summary>
    [Test]
    procedure Inline_ClassBeforeStyleAndDuplicateAttribute_StaysWellFormed;

    /// <summary>Regression. class="a b" was dropped instead of merged.</summary>
    [Test]
    procedure Inline_MultipleClasses_AreMerged;

    /// <summary>End to end: the tricky document renders red with Skia.</summary>
    [Test]
    procedure Skia_TrickyStyleDocument_RendersRed;
  end;

  /// <summary>
  ///  Known issues of the third-party engines, kept here so that they are
  ///  documented and can be enabled once the library is fixed.
  /// </summary>
  [TestFixture]
  TSVGEngineKnownIssuesTests = class
  public
    /// <summary>
    ///  SVGMagic library (UTWRenderer_GDIPlus): the stroke of a &lt;rect&gt; is
    ///  drawn inside the rectangle (x 10..30 for x="10" stroke-width="20")
    ///  instead of centered on its edge (0..20) as SVG requires; &lt;path&gt;
    ///  and &lt;polygon&gt; are right.
    /// </summary>
    [Test]
    [Ignore('SVGMagic library: the stroke of <rect> is drawn inset, not centered on the edge')]
    procedure SVGMagic_RectStroke_IsCenteredOnTheEdge;
  end;

implementation

uses
  System.Math,
  Xml.XMLIntf,
  Xml.XMLDoc,
  Winapi.ActiveX,
  SkiaSVGUtils;

{ TSVGEngineRenderTests }

procedure TSVGEngineRenderTests.Paint_FilledSquare_RendersItsColor(
  const AEngine: string);
begin
  TSVGTestUtils.AssertRenderedPixel(
    TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE), 50, 50, clPureRed, 'center');
end;

procedure TSVGEngineRenderTests.Paint_NoFixedColor_OutlineInteriorStaysTransparent(
  const AEngine: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TSVGTestUtils.Render(TSVGTestUtils.SvgOf(AEngine, SVG_OUTLINE_SQUARE), 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 50, 50, clPureWhite, 'inside the outline');
    TSVGTestUtils.AssertPixel(LBitmap, 15, 50, clPureBlue, 'on the stroke');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGEngineRenderTests.Paint_NoFixedColor_RootFillIsKept(
  const AEngine: string);
begin
  TSVGTestUtils.AssertRenderedPixel(
    TSVGTestUtils.SvgOf(AEngine, SVG_ROOT_FILL_GREEN), 50, 50, clPureGreen, 'center');
end;

procedure TSVGEngineRenderTests.Paint_FixedColor_RecolorsFill(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.FixedColor := clPureBlue;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureBlue, 'center');
end;

procedure TSVGEngineRenderTests.Paint_FixedColorOrSourceChanged_UsesTheNewOne(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.FixedColor := clPureBlue;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureBlue, 'first color');
  LSVG.FixedColor := clPureGreen;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureGreen, 'second color');
  LSVG.ApplyFixedColorToRootOnly := True;
  LSVG.Source := SVG_ROOT_FILL_GREEN;
  LSVG.FixedColor := clPureRed;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureRed, 'new source, root only');
end;

procedure TSVGEngineRenderTests.Paint_FixedColor_OutlineInteriorStaysTransparent(
  const AEngine: string);
var
  LSVG: ISVG;
  LBitmap: TBitmap;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_OUTLINE_SQUARE);
  LSVG.FixedColor := clPureRed;
  LBitmap := TSVGTestUtils.Render(LSVG, 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 50, 50, clPureWhite, 'inside the outline');
    TSVGTestUtils.AssertPixel(LBitmap, 15, 50, clPureRed, 'on the stroke');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGEngineRenderTests.Paint_FixedColorRootOnly_OutlineInteriorStaysTransparent(
  const AEngine: string);
var
  LSVG: ISVG;
  LBitmap: TBitmap;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_OUTLINE_CURRENTCOLOR);
  LSVG.FixedColor := clPureRed;
  LSVG.ApplyFixedColorToRootOnly := True;
  LBitmap := TSVGTestUtils.Render(LSVG, 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 50, 50, clPureWhite, 'inside the outline');
    TSVGTestUtils.AssertPixel(LBitmap, 15, 50, clPureRed, 'on the stroke');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGEngineRenderTests.Paint_FixedColorNone_KeepsOriginalColors(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.FixedColor := SVG_NONE_COLOR;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureRed, 'center');
end;

procedure TSVGEngineRenderTests.Paint_GrayScale_RendersGray(
  const AEngine: string);
var
  LSVG: ISVG;
  LBitmap: TBitmap;
  LColor: TColor;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.GrayScale := True;
  LBitmap := TSVGTestUtils.Render(LSVG, 100, 100);
  try
    LColor := TSVGTestUtils.PixelAt(LBitmap, 50, 50);
    Assert.IsTrue(
      (Abs(GetRValue(LColor) - GetGValue(LColor)) <= 8) and
      (Abs(GetGValue(LColor) - GetBValue(LColor)) <= 8),
      'center is ' + TSVGTestUtils.ColorText(LColor) + ', expected a gray');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGEngineRenderTests.Paint_GrayScaleOff_RestoresColors(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  // The disabled image first...
  LSVG.FixedColor := SVG_INHERIT_COLOR;
  LSVG.GrayScale := True;
  TSVGTestUtils.Render(LSVG, 100, 100).Free;
  // ...then the normal one, in the order TSVGIconItem.ApplyAttributesToInterface
  // assigns the attributes.
  LSVG.FixedColor := SVG_INHERIT_COLOR;
  LSVG.ApplyFixedColorToRootOnly := False;
  LSVG.GrayScale := False;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureRed, 'center');
end;

procedure TSVGEngineRenderTests.Paint_HalfOpacity_BlendsWithBackground(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.Opacity := 0.5;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, RGB(255, 128, 128), 'center');
end;

procedure TSVGEngineRenderTests.Paint_EmptyDocument_LeavesBackgroundUntouched(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.Clear;
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureYellow, 'center',
    100, clPureYellow);
end;

procedure TSVGEngineRenderTests.Paint_OpacityOutOfRange_IsClamped(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.Opacity := 1.5;
  Assert.AreEqual(Single(1), LSVG.Opacity, 0.001, 'Opacity above 1');
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureRed, 'opacity above 1');
  LSVG.Opacity := -0.5;
  Assert.AreEqual(Single(0), LSVG.Opacity, 0.001, 'Opacity below 0');
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureWhite, 'opacity below 0');
end;

{ TSVGEngineDocumentTests }

procedure TSVGEngineDocumentTests.Size_AfterSource_IsDocumentSize(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_SIZED_50x20);
  Assert.AreEqual(Single(50), LSVG.Width, 0.5, 'Width');
  Assert.AreEqual(Single(20), LSVG.Height, 0.5, 'Height');
end;

procedure TSVGEngineDocumentTests.Size_AfterLoadFromStream_IsDocumentSize(
  const AEngine: string);
var
  LSVG: ISVG;
  LStream: TStringStream;
begin
  LSVG := TSVGTestUtils.NewSvg(AEngine);
  LStream := TStringStream.Create(SVG_SIZED_50x20, TEncoding.UTF8);
  try
    LSVG.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
  Assert.AreEqual(Single(50), LSVG.Width, 0.5, 'Width');
  Assert.AreEqual(Single(20), LSVG.Height, 0.5, 'Height');
end;

procedure TSVGEngineDocumentTests.Size_AfterPaint_IsStillDocumentSize(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_SIZED_50x20);
  TSVGTestUtils.Render(LSVG, 100, 100).Free;
  Assert.AreEqual(Single(50), LSVG.Width, 0.5, 'Width');
  Assert.AreEqual(Single(20), LSVG.Height, 0.5, 'Height');
end;

procedure TSVGEngineDocumentTests.Size_ReloadWithViewBoxOnly_IsNotInherited(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_SIZED_50x20);
  LSVG.Source := SVG_VIEWBOX_ONLY_10x10;
  Assert.AreEqual(Single(10), LSVG.Width, 0.5, 'Width');
  Assert.AreEqual(Single(10), LSVG.Height, 0.5, 'Height');
end;

procedure TSVGEngineDocumentTests.Source_WithStyleBlock_IsReturnedUnchanged(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  if SameText(AEngine, ENGINE_D2D) then
    Assert.Pass('Direct2D does not support <style>');
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_STYLE_CLASS_RED);
  Assert.AreEqual(SVG_STYLE_CLASS_RED, LSVG.Source);
end;

procedure TSVGEngineDocumentTests.SaveToStream_WithStyleBlock_WritesOriginalText(
  const AEngine: string);
var
  LSVG: ISVG;
  LStream: TStringStream;
begin
  if SameText(AEngine, ENGINE_D2D) then
    Assert.Pass('Direct2D does not support <style>');
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_STYLE_CLASS_RED);
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LSVG.SaveToStream(LStream);
    Assert.AreEqual(SVG_STYLE_CLASS_RED, LStream.DataString);
  finally
    LStream.Free;
  end;
end;

procedure TSVGEngineDocumentTests.LoadFromStream_NotAnSvg_RaisesESVGException(
  const AEngine: string);
var
  LSVG: ISVG;
  LStream: TStringStream;
begin
  LSVG := TSVGTestUtils.NewSvg(AEngine);
  LStream := TStringStream.Create(NOT_AN_SVG, TEncoding.UTF8);
  try
    Assert.WillRaise(
      procedure
      begin
        LSVG.LoadFromStream(LStream);
      end, ESVGException);
  finally
    LStream.Free;
  end;
end;

procedure TSVGEngineDocumentTests.LoadFromStream_Twice_NestedIdsWithUse_Renders(
  const AEngine: string);
var
  LSVG: ISVG;
  LStream: TStringStream;
  I: Integer;
  LBitmap: TBitmap;
begin
  LSVG := TSVGTestUtils.NewSvg(AEngine);
  for I := 1 to 3 do
  begin
    LStream := TStringStream.Create(SVG_NESTED_IDS_WITH_USE, TEncoding.UTF8);
    try
      LSVG.LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
    LBitmap := TSVGTestUtils.Render(LSVG, 100, 100);
    try
      TSVGTestUtils.AssertPixel(LBitmap, 25, 50, clPureRed, Format('load %d, left half', [I]));
      TSVGTestUtils.AssertPixel(LBitmap, 75, 50, clPureWhite, Format('load %d, right half', [I]));
    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TSVGEngineDocumentTests.Source_NotAnSvg_RaisesESVGException(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.NewSvg(AEngine);
  Assert.WillRaise(
    procedure
    begin
      LSVG.Source := NOT_AN_SVG;
    end, ESVGException);
end;

procedure TSVGEngineDocumentTests.Clear_IsEmptyAndSourceIsEmpty(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  Assert.IsFalse(LSVG.IsEmpty, 'IsEmpty before Clear');
  LSVG.Clear;
  Assert.IsTrue(LSVG.IsEmpty, 'IsEmpty after Clear');
  Assert.AreEqual('', LSVG.Source, 'Source after Clear');
  Assert.AreEqual(Single(0), LSVG.Width, 0.001, 'Width after Clear');
end;

procedure TSVGEngineDocumentTests.Source_SetToEmpty_ForgetsPreviousDocument(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.SvgOf(AEngine, SVG_RED_SQUARE);
  LSVG.Source := '';
  Assert.IsTrue(LSVG.IsEmpty, 'IsEmpty');
  TSVGTestUtils.AssertRenderedPixel(LSVG, 50, 50, clPureWhite, 'center');
end;

procedure TSVGEngineDocumentTests.Opacity_Getter_ReturnsAssignedValue(
  const AEngine: string);
var
  LSVG: ISVG;
begin
  LSVG := TSVGTestUtils.NewSvg(AEngine);
  LSVG.Opacity := 0.5;
  Assert.AreEqual(Single(0.5), LSVG.Opacity, 0.001, 'before loading');
  LSVG.Source := SVG_RED_SQUARE;
  LSVG.Opacity := 0.25;
  Assert.AreEqual(Single(0.25), LSVG.Opacity, 0.001, 'after loading');
end;

{ TSkiaInlineStyleTests }

/// <summary>Parses ASvg as XML; fails the test if it is not well formed.</summary>
procedure AssertWellFormed(const ASvg: string);
var
  LDoc: IXMLDocument;
begin
  try
    LDoc := LoadXMLData(ASvg);
  except
    on E: Exception do
      Assert.Fail('Not well formed XML (' + E.Message + '): ' + ASvg);
  end;
end;

procedure TSkiaInlineStyleTests.Inline_SimpleClass_BecomesAttribute;
var
  LResult: string;
begin
  LResult := InlineSvgStyle(SVG_STYLE_CLASS_RED);
  AssertWellFormed(LResult);
  Assert.IsFalse(LResult.Contains('<style'), 'style block left: ' + LResult);
  Assert.IsFalse(LResult.Contains('class='), 'class left: ' + LResult);
  Assert.IsTrue(LResult.Contains('fill="#FF0000"'), 'fill not inlined: ' + LResult);
end;

procedure TSkiaInlineStyleTests.Inline_ClassBeforeStyleAndDuplicateAttribute_StaysWellFormed;
var
  LResult: string;
begin
  LResult := InlineSvgStyle(SVG_STYLE_CLASS_TRICKY);
  AssertWellFormed(LResult);
  Assert.IsFalse(LResult.Contains('<style'), 'style block left: ' + LResult);
  Assert.IsFalse(LResult.Contains('fill="#000000"'),
    'the class rule must win over the presentation attribute: ' + LResult);
  Assert.IsTrue(LResult.Contains('fill="#FF0000"'), 'fill not inlined: ' + LResult);
  Assert.IsTrue(LResult.Contains('stroke-width="1.5"'), 'stroke-width not inlined: ' + LResult);
end;

procedure TSkiaInlineStyleTests.Inline_MultipleClasses_AreMerged;
const
  SVG_MULTI =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<style>.f{fill:#FF0000}.s{stroke:#0000FF}</style>' +
    '<rect class="f s" width="100" height="100"/></svg>';
var
  LResult: string;
begin
  LResult := InlineSvgStyle(SVG_MULTI);
  AssertWellFormed(LResult);
  Assert.IsTrue(LResult.Contains('fill="#FF0000"'), 'first class lost: ' + LResult);
  Assert.IsTrue(LResult.Contains('stroke="#0000FF"'), 'second class lost: ' + LResult);
end;

procedure TSkiaInlineStyleTests.Skia_TrickyStyleDocument_RendersRed;
begin
  TSVGTestUtils.AssertRenderedPixel(
    TSVGTestUtils.SvgOf(ENGINE_SKIA, SVG_STYLE_CLASS_TRICKY), 50, 50, clPureRed, 'center');
end;

{ TSVGEngineKnownIssuesTests }

procedure TSVGEngineKnownIssuesTests.SVGMagic_RectStroke_IsCenteredOnTheEdge;
var
  LBitmap: TBitmap;
begin
  LBitmap := TSVGTestUtils.Render(
    TSVGTestUtils.SvgOf(ENGINE_SVGMAGIC, SVG_OUTLINE_SQUARE), 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 5, 50, clPureBlue, 'outer half of the stroke');
    TSVGTestUtils.AssertPixel(LBitmap, 25, 50, clPureWhite, 'past the inner half of the stroke');
  finally
    LBitmap.Free;
  end;
end;

initialization
  CoInitialize(nil);
  TDUnitX.RegisterTestFixture(TSVGEngineRenderTests);
  TDUnitX.RegisterTestFixture(TSVGEngineDocumentTests);
  TDUnitX.RegisterTestFixture(TSkiaInlineStyleTests);
  TDUnitX.RegisterTestFixture(TSVGEngineKnownIssuesTests);

end.
