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
///  The FMX components: TSVGIconImageList (source items, destinations,
///  streaming), the FMX engine wrapper and TSVGIconImage. They run on the FMX
///  engine selected by SVGIconImageList.inc (Image32 by default).
/// </summary>
unit SVGFMXTests;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Types,
  FMX.Graphics,
  FMX.ImgList,
  DUnitX.TestFramework,
  FMX.ImageSVG,
  FMX.SVGIconImageList,
  FMX.SVGIconImage;

type
  [TestFixture]
  TFMXSVGIconImageListTests = class
  strict private
    FList: TSVGIconImageList;
    function SourceItem(const AIndex: Integer): TSVGIconSourceItem;
    /// <summary>The bitmap the source item renders (caller does not free it).</summary>
    function IconBitmap(const AIndex: Integer): TBitmap;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>Sanity: an icon renders its own colour at the list size.</summary>
    [Test]
    procedure AddIcon_RendersAtListSize;

    /// <summary>
    ///  Regression. InsertIcon gave the item the de-duplicated name ("home1")
    ///  but the destination layer the requested one ("home"): the second icon
    ///  showed the first one.
    /// </summary>
    [Test]
    procedure AddIcon_DuplicateName_LayerUsesTheItemName;

    /// <summary>
    ///  Regression. Assigning SVGText inside InsertIcon updated (and, on
    ///  append, added) a destination before InsertIcon inserted its own: one
    ///  destination more than the icons.
    /// </summary>
    [Test]
    procedure AddIcon_DestinationsMatchIcons;

    /// <summary>
    ///  Regression. With the layer named after the requested name, DeleteIcon
    ///  did not find the destination of the deleted icon and left it there.
    /// </summary>
    [Test]
    procedure DeleteIcon_AfterDuplicateName_RemovesItsDestination;

    /// <summary>
    ///  Regression. The published getters answer the list value when the item
    ///  inherits, so the writer stored it on every item, and on reading it
    ///  became the item's own: a later change of the list had no effect.
    /// </summary>
    [Test]
    procedure Streaming_InheritedAttributes_StayInherited;

    /// <summary>
    ///  Regression. DrawSVGIcon never passed ApplyFixedColorToRootOnly to the
    ///  engine: the whole icon was recoloured.
    /// </summary>
    [Test]
    procedure ApplyFixedColorToRootOnly_KeepsChildColors;

    /// <summary>
    ///  Regression. With Zoom below 100 the engine resized the bitmap to the
    ///  zoomed size instead of drawing a smaller icon centered in it.
    /// </summary>
    [Test]
    procedure Zoom50_BitmapKeepsItsSizeAndIconIsCentered;

    /// <summary>Regression. Assign did not copy ApplyFixedColorToRootOnly.</summary>
    [Test]
    procedure Assign_CopiesApplyFixedColorToRootOnly;

    /// <summary>
    ///  Regression. LoadFromFile ran outside the per-file try/except: one bad
    ///  file stopped the whole batch instead of being reported with the others.
    /// </summary>
    [Test]
    procedure LoadFromFiles_BadFile_LoadsTheOthers;
  end;

  [TestFixture]
  TFMXImageSVGTests = class
  public
    /// <summary>
    ///  Regression. SetApplyFixedColorToRootOnly kept the TAlphaColor in a
    ///  TColor (signed): with range checking every opaque colour raised
    ///  ERangeError.
    /// </summary>
    [Test]
    procedure ApplyFixedColorToRootOnly_WithOpaqueColor_DoesNotRaise;
  end;

  [TestFixture]
  TFMXSVGIconImageTests = class
  public
    /// <summary>
    ///  Regression. The Opacity of the bitmap item was stored and published
    ///  but never passed to the engine.
    /// </summary>
    [Test]
    procedure Opacity_IsApplied;
  end;

implementation

uses
  System.IOUtils,
  System.Math,
  FMX.MultiResBitmap;

const
  SVG_RED_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="0" y="0" width="100" height="100" fill="#FF0000"/></svg>';
  SVG_BLUE_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<rect x="0" y="0" width="100" height="100" fill="#0000FF"/></svg>';
  SVG_ROOT_AND_CHILD =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" fill="#00FF00">' +
    '<rect x="0" y="0" width="50" height="100"/>' +
    '<rect x="50" y="0" width="50" height="100" fill="#0000FF"/></svg>';

/// <summary>Pixel (X, Y) of ABitmap (premultiplied, as FMX stores it).</summary>
function PixelOf(const ABitmap: TBitmap; const X, Y: Integer): TAlphaColor;
var
  LData: TBitmapData;
begin
  Assert.IsTrue((X < ABitmap.Width) and (Y < ABitmap.Height),
    Format('Probe (%d,%d) outside a %dx%d bitmap', [X, Y, ABitmap.Width, ABitmap.Height]));
  if not ABitmap.Map(TMapAccess.Read, LData) then
    Assert.Fail('Cannot map the bitmap');
  try
    Result := LData.GetPixel(X, Y);
  finally
    ABitmap.Unmap(LData);
  end;
end;

procedure AssertPixel(const ABitmap: TBitmap; const X, Y: Integer;
  const AExpected: TAlphaColor; const AWhat: string; const ATolerance: Integer = 40);
var
  LActual: TAlphaColorRec;
  LExpected: TAlphaColorRec;
begin
  LActual.Color := PixelOf(ABitmap, X, Y);
  LExpected.Color := AExpected;
  Assert.IsTrue(
    (Abs(LActual.R - LExpected.R) <= ATolerance) and
    (Abs(LActual.G - LExpected.G) <= ATolerance) and
    (Abs(LActual.B - LExpected.B) <= ATolerance) and
    (Abs(LActual.A - LExpected.A) <= ATolerance),
    Format('%s: pixel (%d,%d) is %.8x, expected %.8x',
      [AWhat, X, Y, LActual.Color, LExpected.Color]));
end;

{ TFMXSVGIconImageListTests }

procedure TFMXSVGIconImageListTests.Setup;
begin
  FList := TSVGIconImageList.Create(nil);
  FList.Size := 32;
end;

procedure TFMXSVGIconImageListTests.TearDown;
begin
  FreeAndNil(FList);
end;

function TFMXSVGIconImageListTests.SourceItem(
  const AIndex: Integer): TSVGIconSourceItem;
begin
  Result := FList.Source.Items[AIndex] as TSVGIconSourceItem;
end;

function TFMXSVGIconImageListTests.IconBitmap(const AIndex: Integer): TBitmap;
begin
  Result := (SourceItem(AIndex).MultiResBitmap[0] as TSVGIconBitmapItem).Bitmap;
end;

procedure TFMXSVGIconImageListTests.AddIcon_RendersAtListSize;
var
  LBitmap: TBitmap;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'red');
  LBitmap := IconBitmap(0);
  Assert.AreEqual(32, LBitmap.Width, 'Width');
  Assert.AreEqual(32, LBitmap.Height, 'Height');
  AssertPixel(LBitmap, 16, 16, TAlphaColors.Red, 'center');
end;

procedure TFMXSVGIconImageListTests.AddIcon_DuplicateName_LayerUsesTheItemName;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'home');
  FList.AddIcon(SVG_BLUE_SQUARE, 'home');
  Assert.AreEqual('home1', SourceItem(1).IconName, 'item name');
  Assert.AreEqual('home1', FList.Destination[1].Layers[0].Name, 'layer name');
end;

procedure TFMXSVGIconImageListTests.AddIcon_DestinationsMatchIcons;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'red');
  FList.AddIcon(SVG_BLUE_SQUARE, 'blue');
  Assert.AreEqual(2, FList.Destination.Count, 'destinations');
  Assert.AreEqual('red', FList.Destination[0].Layers[0].Name, 'layer 0');
  Assert.AreEqual('blue', FList.Destination[1].Layers[0].Name, 'layer 1');
end;

procedure TFMXSVGIconImageListTests.DeleteIcon_AfterDuplicateName_RemovesItsDestination;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'home');
  FList.AddIcon(SVG_BLUE_SQUARE, 'home');
  FList.DeleteIcon(1);
  Assert.AreEqual(1, FList.Source.Count, 'icons');
  Assert.AreEqual(1, FList.Destination.Count, 'destinations');
end;

procedure TFMXSVGIconImageListTests.Streaming_InheritedAttributes_StayInherited;
var
  LStream: TMemoryStream;
  LCopy: TSVGIconImageList;
  LItem: TSVGIconSourceItem;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'red');
  FList.FixedColor := TAlphaColors.Red;
  FList.Opacity := 0.5;
  LStream := TMemoryStream.Create;
  try
    LStream.WriteComponent(FList);
    LStream.Position := 0;
    LCopy := TSVGIconImageList.Create(nil);
    try
      LStream.ReadComponent(LCopy);
      Assert.AreEqual(1, LCopy.Source.Count, 'icons read');
      LItem := LCopy.Source.Items[0] as TSVGIconSourceItem;
      LCopy.FixedColor := TAlphaColors.Blue;
      LCopy.Opacity := 0.8;
      LCopy.GrayScale := True;
      Assert.AreEqual(TAlphaColors.Blue, LItem.FixedColor, 'FixedColor follows the list');
      Assert.AreEqual(Single(0.8), LItem.Opacity, 0.001, 'Opacity follows the list');
      Assert.IsTrue(LItem.GrayScale, 'GrayScale follows the list');
    finally
      LCopy.Free;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TFMXSVGIconImageListTests.ApplyFixedColorToRootOnly_KeepsChildColors;
var
  LBitmap: TBitmap;
begin
  FList.AddIcon(SVG_ROOT_AND_CHILD, 'icon');
  FList.FixedColor := TAlphaColors.Red;
  FList.ApplyFixedColorToRootOnly := True;
  LBitmap := IconBitmap(0);
  AssertPixel(LBitmap, 8, 16, TAlphaColors.Red, 'inherited half');
  AssertPixel(LBitmap, 24, 16, TAlphaColors.Blue, 'explicit half');
end;

procedure TFMXSVGIconImageListTests.Zoom50_BitmapKeepsItsSizeAndIconIsCentered;
var
  LBitmap: TBitmap;
begin
  FList.AddIcon(SVG_RED_SQUARE, 'red');
  FList.Zoom := 50;
  LBitmap := IconBitmap(0);
  Assert.AreEqual(32, LBitmap.Width, 'Width');
  Assert.AreEqual(32, LBitmap.Height, 'Height');
  AssertPixel(LBitmap, 16, 16, TAlphaColors.Red, 'center');
  AssertPixel(LBitmap, 2, 2, TAlphaColors.Null, 'corner, outside the zoomed icon');
end;

procedure TFMXSVGIconImageListTests.Assign_CopiesApplyFixedColorToRootOnly;
var
  LCopy: TSVGIconImageList;
begin
  FList.ApplyFixedColorToRootOnly := True;
  LCopy := TSVGIconImageList.Create(nil);
  try
    LCopy.Assign(FList);
    Assert.IsTrue(LCopy.ApplyFixedColorToRootOnly);
  finally
    LCopy.Free;
  end;
end;

procedure TFMXSVGIconImageListTests.LoadFromFiles_BadFile_LoadsTheOthers;
var
  LFiles: TStringList;
  LFolder: string;
  LRaised: Boolean;
begin
  LFolder := TPath.Combine(TPath.GetTempPath, 'SVGFMXTests_' + TGUID.NewGuid.ToString);
  ForceDirectories(LFolder);
  LFiles := TStringList.Create;
  try
    TFile.WriteAllText(TPath.Combine(LFolder, 'one.svg'), SVG_RED_SQUARE);
    TFile.WriteAllText(TPath.Combine(LFolder, 'two.svg'), SVG_BLUE_SQUARE);
    LFiles.Add(TPath.Combine(LFolder, 'one.svg'));
    LFiles.Add(TPath.Combine(LFolder, 'missing.svg'));
    LFiles.Add(TPath.Combine(LFolder, 'two.svg'));
    LRaised := False;
    try
      FList.LoadFromFiles(LFiles);
    except
      on E: Exception do
        LRaised := E.Message.Contains('missing.svg');
    end;
    Assert.IsTrue(LRaised, 'the bad file is reported');
    Assert.AreEqual(2, FList.Source.Count, 'the good files are loaded');
  finally
    LFiles.Free;
    TDirectory.Delete(LFolder, True);
  end;
end;

{ TFMXImageSVGTests }

procedure TFMXImageSVGTests.ApplyFixedColorToRootOnly_WithOpaqueColor_DoesNotRaise;
var
  LList: TSVGIconImageList;
  LSVG: TFmxImageSVG;
begin
  LList := TSVGIconImageList.Create(nil);
  try
    LList.AddIcon(SVG_RED_SQUARE, 'red');
    LSVG := LList.ExtractSVG(0);
    try
      LSVG.FixedColor := TAlphaColors.Blue;
      // Fails the test with ERangeError, if raised.
      LSVG.ApplyFixedColorToRootOnly := True;
      Assert.AreEqual(TAlphaColors.Blue, LSVG.FixedColor, 'FixedColor kept');
    finally
      LSVG.Free;
    end;
  finally
    LList.Free;
  end;
end;

{ TFMXSVGIconImageTests }

procedure TFMXSVGIconImageTests.Opacity_IsApplied;
var
  LImage: TSVGIconImage;
  LItem: TSVGIconFixedBitmapItem;
begin
  LImage := TSVGIconImage.Create(nil);
  try
    LImage.SetBounds(0, 0, 32, 32);
    LImage.SVGText := SVG_RED_SQUARE;
    LItem := LImage.GetFixedBitmap;
    LItem.Opacity := 0.5;
    // Premultiplied: half red is $80800000.
    AssertPixel(LItem.Bitmap, 16, 16, $80800000, 'center');
  finally
    LImage.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFMXSVGIconImageListTests);
  TDUnitX.RegisterTestFixture(TFMXImageSVGTests);
  TDUnitX.RegisterTestFixture(TFMXSVGIconImageTests);

end.
