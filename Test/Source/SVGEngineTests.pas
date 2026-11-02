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
  ///  Defects of the bundled SVGMagic library fixed in this fork, each pinned by a
  ///  regression test (until 2026-11-03 the first was an ignored known issue).
  /// </summary>
  [TestFixture]
  TSVGMagicLibraryFixTests = class
  public
    /// <summary>
    ///  Regression.  The rasterizer drew &lt;rect&gt; through TWRenderer.DrawRect, which
    ///  is made for controls and insets the outline: x 10..30 for x="10"
    ///  stroke-width="20", where SVG centres it on the edge (0..20).
    /// </summary>
    [Test]
    procedure SVGMagic_RectStroke_IsCenteredOnTheEdge;

    /// <summary>
    ///  Regression.  The fill of a stroked &lt;rect&gt; was inset with the outline: it
    ///  must reach the geometric edge, under the inner half of the stroke, so the
    ///  stroke ends at x 20 and the fill shows from there.
    /// </summary>
    [Test]
    procedure SVGMagic_StrokedRectFill_ReachesTheEdge;

    /// <summary>
    ///  Regression.  A rounded &lt;rect&gt; (rx, ry) took the same inset path: its stroke
    ///  must be centred on the edge too, and the corner must stay rounded (the
    ///  outer corner pixel unpainted).
    /// </summary>
    [Test]
    procedure SVGMagic_RoundedRectStroke_IsCenteredOnTheEdge;

    /// <summary>
    ///  Regression.  A stroked and filled &lt;rect&gt; must render pixel for pixel like
    ///  the &lt;path&gt; of the same geometry, which never went through DrawRect: before
    ///  the fix the two differed along the whole outline and the edge of the fill.
    /// </summary>
    [Test]
    procedure SVGMagic_StrokedRect_RendersAsTheSamePath;

    /// <summary>
    ///  Regression.  A rounded &lt;rect&gt; must render like the &lt;path&gt; of four arcs of the
    ///  same radii: a wrong arc size or a square corner shows as differing pixels.
    /// </summary>
    [Test]
    procedure SVGMagic_RoundedRect_RendersAsTheSamePath;

    /// <summary>
    ///  Regression.  Radii larger than half the side are clamped to it, as SVG requires:
    ///  rx="100" ry="100" on 112 x 96 is the ellipse of radii 56 and 48.
    /// </summary>
    [Test]
    procedure SVGMagic_OversizedRadii_AreClampedToHalfTheSide;

    /// <summary>
    ///  Regression.  A dashed &lt;rect&gt; must dash like the same &lt;path&gt;: the old route
    ///  added the pattern twice and divided it by the stroke width twice.
    /// </summary>
    [Test]
    procedure SVGMagic_DashedRect_RendersAsTheSamePath;

    /// <summary>
    ///  A &lt;rect&gt; inside a nested &lt;svg&gt; with preserveAspectRatio must be placed and
    ///  scaled like the same &lt;path&gt;: the rect now takes the path's transform route.
    /// </summary>
    [Test]
    procedure SVGMagic_RectInAspectRatioViewport_RendersAsTheSamePath;
  private
    /// <summary>
    ///  Renders two SVG documents with SVGMagic at 100 x 100 and counts the pixels whose
    ///  channels differ by more than 64 (arcs are approximated by Bezier curves, so a few
    ///  levels of anti-aliasing differ at rounded edges and do not count).
    /// </summary>
    /// <param name="ASvgA">The first document.</param>
    /// <param name="ASvgB">The second document.</param>
    /// <returns>The number of clearly differing pixels.</returns>
    function DifferingPixels(const ASvgA, ASvgB: string): Integer;
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

{ TSVGMagicLibraryFixTests }

procedure TSVGMagicLibraryFixTests.SVGMagic_RectStroke_IsCenteredOnTheEdge;
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

procedure TSVGMagicLibraryFixTests.SVGMagic_StrokedRectFill_ReachesTheEdge;
const
  SVG_FILLED_OUTLINE_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="10" y="10" width="80" height="80" fill="#FF0000" stroke="#0000FF" stroke-width="20"/></svg>';
var
  LBitmap: TBitmap;
begin
  LBitmap := TSVGTestUtils.Render(
    TSVGTestUtils.SvgOf(ENGINE_SVGMAGIC, SVG_FILLED_OUTLINE_SQUARE), 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 5, 50, clPureBlue, 'outer half of the stroke');
    TSVGTestUtils.AssertPixel(LBitmap, 25, 50, clPureRed, 'fill, past the inner half of the stroke');
    TSVGTestUtils.AssertPixel(LBitmap, 50, 50, clPureRed, 'fill, centre');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_RoundedRectStroke_IsCenteredOnTheEdge;
const
  SVG_ROUNDED_OUTLINE_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="10" y="10" width="80" height="80" rx="20" fill="none" stroke="#0000FF" stroke-width="20"/></svg>';
var
  LBitmap: TBitmap;
begin
  LBitmap := TSVGTestUtils.Render(
    TSVGTestUtils.SvgOf(ENGINE_SVGMAGIC, SVG_ROUNDED_OUTLINE_SQUARE), 100, 100);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 5, 50, clPureBlue, 'outer half of the stroke');
    TSVGTestUtils.AssertPixel(LBitmap, 25, 50, clPureWhite, 'past the inner half of the stroke');
    TSVGTestUtils.AssertPixel(LBitmap, 1, 1, clPureWhite, 'outside the rounded corner');
  finally
    LBitmap.Free;
  end;
end;

const
  SVG_PAIR_HEAD = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 192 192">';
  SVG_PAIR_STYLE = ' fill="#4F4F4F" stroke="#1060C0" stroke-width="8"/>';

function TSVGMagicLibraryFixTests.DifferingPixels(const ASvgA, ASvgB: string): Integer;
var
  LA, LB: TBitmap;
  LColorA, LColorB: TColor;
  X, Y: Integer;
begin
  Result := 0;
  LA := TSVGTestUtils.Render(TSVGTestUtils.SvgOf(ENGINE_SVGMAGIC, ASvgA), 100, 100);
  try
    LB := TSVGTestUtils.Render(TSVGTestUtils.SvgOf(ENGINE_SVGMAGIC, ASvgB), 100, 100);
    try
      for Y := 0 to 99 do
        for X := 0 to 99 do
        begin
          LColorA := TSVGTestUtils.PixelAt(LA, X, Y);
          LColorB := TSVGTestUtils.PixelAt(LB, X, Y);
          if (Abs(GetRValue(LColorA) - GetRValue(LColorB)) > 64) or (Abs(GetGValue(LColorA) - GetGValue(LColorB)) > 64)
            or (Abs(GetBValue(LColorA) - GetBValue(LColorB)) > 64) then
            Inc(Result);
        end;
    finally
      LB.Free;
    end;
  finally
    LA.Free;
  end;
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_StrokedRect_RendersAsTheSamePath;
begin
  Assert.AreEqual(0, DifferingPixels(
    SVG_PAIR_HEAD + '<rect x="28" y="64" width="112" height="96"' + SVG_PAIR_STYLE + '</svg>',
    SVG_PAIR_HEAD + '<path d="M28 64H140V160H28Z"' + SVG_PAIR_STYLE + '</svg>'),
    'pixels where the rect and the path differ');
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_RoundedRect_RendersAsTheSamePath;
begin
  Assert.AreEqual(0, DifferingPixels(
    SVG_PAIR_HEAD + '<rect x="28" y="64" width="112" height="96" rx="24"' + SVG_PAIR_STYLE + '</svg>',
    SVG_PAIR_HEAD + '<path d="M52 64H116A24 24 0 0 1 140 88V136A24 24 0 0 1 116 160H52A24 24 0 0 1 28 136V88' +
      'A24 24 0 0 1 52 64Z"' + SVG_PAIR_STYLE + '</svg>'),
    'pixels where the rounded rect and the path differ');
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_OversizedRadii_AreClampedToHalfTheSide;
begin
  Assert.AreEqual(0, DifferingPixels(
    SVG_PAIR_HEAD + '<rect x="28" y="64" width="112" height="96" rx="100" ry="100"' + SVG_PAIR_STYLE + '</svg>',
    SVG_PAIR_HEAD + '<path d="M84 64A56 48 0 0 1 140 112A56 48 0 0 1 84 160A56 48 0 0 1 28 112A56 48 0 0 1 84 64Z"' +
      SVG_PAIR_STYLE + '</svg>'),
    'pixels where the clamped rect and the ellipse differ');
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_DashedRect_RendersAsTheSamePath;
const
  DASHED = ' fill="none" stroke="#1060C0" stroke-width="8" stroke-dasharray="24 12"/>';
begin
  Assert.AreEqual(0, DifferingPixels(
    SVG_PAIR_HEAD + '<rect x="28" y="64" width="112" height="96"' + DASHED + '</svg>',
    SVG_PAIR_HEAD + '<path d="M28 64H140V160H28Z"' + DASHED + '</svg>'),
    'pixels where the dashed rect and the dashed path differ');
end;

procedure TSVGMagicLibraryFixTests.SVGMagic_RectInAspectRatioViewport_RendersAsTheSamePath;
const
  VIEWPORT = '<svg x="0" y="40" width="192" height="96" viewBox="0 0 100 100" preserveAspectRatio="xMidYMid meet">';
begin
  Assert.AreEqual(0, DifferingPixels(
    SVG_PAIR_HEAD + VIEWPORT + '<rect x="10" y="20" width="80" height="60"' + SVG_PAIR_STYLE + '</svg></svg>',
    SVG_PAIR_HEAD + VIEWPORT + '<path d="M10 20H90V80H10Z"' + SVG_PAIR_STYLE + '</svg></svg>'),
    'pixels where the rect and the path in the viewport differ');
end;

initialization
  CoInitialize(nil);
  TDUnitX.RegisterTestFixture(TSVGEngineRenderTests);
  TDUnitX.RegisterTestFixture(TSVGEngineDocumentTests);
  TDUnitX.RegisterTestFixture(TSkiaInlineStyleTests);
  TDUnitX.RegisterTestFixture(TSVGMagicLibraryFixTests);

end.
