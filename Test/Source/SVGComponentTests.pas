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
///  The VCL components: TSVGIconImageList, TSVGIconImageCollection,
///  TSVGIconVirtualImageList, TSVGIconImage/TSVGGraphic and the SVGIconUtils
///  helpers. They run on the global factory (Image32 unless the .inc says
///  otherwise): what they check is the component logic, not the engine.
/// </summary>
unit SVGComponentTests;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ComCtrls,
  DUnitX.TestFramework,
  SVGInterfaces,
  SVGIconItems,
  SVGIconImageListBase,
  SVGIconImageList,
  SVGIconImageCollection,
  SVGIconVirtualImageList,
  SVGIconImage,
  SVGTestUtils;

type
  [TestFixture]
  TSVGIconImageListTests = class
  strict private
    FList: TSVGIconImageList;
    FChangeCount: Integer;
    procedure ListChanged(Sender: TObject);
    function AddIcon(const ASource, AName: string): Integer;
    /// <summary>Draws icon AIndex through the image list on white.</summary>
    function DrawIcon(const AIndex: Integer): TBitmap;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>Sanity: the category argument ends up in IconName.</summary>
    [Test]
    procedure Add_WithCategoryArgument_BuildsIconName;

    /// <summary>
    ///  Regression. The two-argument Add passed AIconCategory = '' down, and
    ///  "Category := ''" rebuilt IconName without the category the caller had
    ///  put in the name: "Arrows\Left" was stored as "Left".
    /// </summary>
    [Test]
    procedure Add_NameWithCategory_KeepsCategory;

    /// <summary>
    ///  Regression. Inside "with AStrip do", Width and Height were the strip's,
    ///  not the image list's: every icon but the first was drawn outside the
    ///  bitmap.
    /// </summary>
    [Test]
    procedure SaveToFile_SeveralIcons_EachIconInItsCell;

    /// <summary>Regression. With no icons CalcDimensions divided 0 by 0.</summary>
    [Test]
    procedure SaveToFile_NoIcons_DoesNotRaise;

    /// <summary>
    ///  Regression. ApplyFixedColorToRootOnly was not published: the value set
    ///  in the component editor was never written to the DFM.
    /// </summary>
    [Test]
    procedure ApplyFixedColorToRootOnly_IsStreamed;

    /// <summary>
    ///  Regression. PaintTo (hence Draw) set FixedColor but not
    ///  ApplyFixedColorToRootOnly, so Draw recoloured the whole icon.
    /// </summary>
    [Test]
    procedure Draw_ApplyFixedColorToRootOnly_KeepsChildColors;

    /// <summary>
    ///  Regression. SetAntiAliasColor did not call Change: the bitmaps kept the
    ///  old background until some other property changed.
    /// </summary>
    [Test]
    procedure AntiAliasColor_Set_NotifiesChange;
  end;

  [TestFixture]
  TSVGIconImageCollectionTests = class
  strict private
    FCollection: TSVGIconImageCollection;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>
    ///  Regression. IndexOf (and Remove, which uses it) compared names with
    ///  "=", while GetIconByName and GetIndexByName ignore case.
    /// </summary>
    [Test]
    procedure IndexOf_And_Remove_IgnoreCase;

    /// <summary>
    ///  Regression. ForceDirectories('') raises EInOutError: a file name with
    ///  no folder could not be saved.
    /// </summary>
    [Test]
    procedure SaveToFile_FileNameWithoutFolder_Saves;

    /// <summary>Regression. Assign did not copy AntiAliasColor.</summary>
    [Test]
    procedure Assign_CopiesAntiAliasColor;
  end;

  [TestFixture]
  TSVGIconVirtualImageListTests = class
  strict private
    FCollection: TSVGIconImageCollection;
    FList: TSVGIconVirtualImageList;
    function DrawIcon(const AIndex: Integer; const AEnabled: Boolean = True): TBitmap;
    procedure AssertIconColor(const AIndex: Integer; const AExpected: TColor;
      const AWhat: string; const AEnabled: Boolean = True);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>Sanity: the list renders with its own FixedColor.</summary>
    [Test]
    procedure Draw_UsesOwnFixedColor;

    /// <summary>
    ///  Regression. With AutoFill a change of the collection went through
    ///  DoAutoFill, which renders outside the render override: every icon went
    ///  back to the collection colours.
    /// </summary>
    [Test]
    procedure AutoFill_CollectionChanged_KeepsOwnFixedColor;

    /// <summary>
    ///  Regression. A change to one item goes through
    ///  TVirtualImageListItem.Update, also outside the override.
    /// </summary>
    [Test]
    procedure ItemChanged_KeepsOwnFixedColor;

    /// <summary>
    ///  Regression. The disabled bitmap is created on first use, from DoDraw,
    ///  outside the override.
    /// </summary>
    [Test]
    procedure DrawDisabled_UsesOwnFixedColor;

    /// <summary>
    ///  Regression. Changing DisabledOpacity (or DisabledGrayscale) rebuilds
    ///  the disabled bitmaps already created, again outside the override.
    /// </summary>
    [Test]
    procedure DrawDisabled_AfterDisabledOpacityChange_UsesOwnFixedColor;

    /// <summary>
    ///  Regression, the reported issue "FixedColor is ignored when creating
    ///  disabled SVG icons with TSVGIconVirtualImageList": fill="currentColor",
    ///  collection FixedColor clDefault, list FixedColor clWhite, GrayScale
    ///  False, DisabledGrayScale False, DisabledOpacity 125. Expected a
    ///  semi-transparent white icon (on black: gray 125).
    /// </summary>
    [Test]
    procedure Issue_DisabledCurrentColorIcon_UsesListFixedColor;

    /// <summary>
    ///  Regression. Assign fell through to TVirtualImageList.Assign, which
    ///  knows nothing of FixedColor, GrayScale, Opacity and the rest.
    /// </summary>
    [Test]
    procedure Assign_CopiesOwnAttributes;
  end;

  [TestFixture]
  TSVGIconImageTests = class
  strict private
    FImage: TSVGIconImage;
    FList: TSVGIconImageList;
    procedure PaintImage;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>
    ///  Regression. UsingSVGText only compared ImageIndex with Count: linked to
    ///  a plain TImageList, SVGIconItem is nil and Paint dereferenced it.
    /// </summary>
    [Test]
    procedure Paint_LinkedToPlainImageList_DoesNotRaise;

    /// <summary>
    ///  Regression. IsImageIndexAvail used "<=" Count: once the icon shown was
    ///  the last one and got deleted, Paint read SVGIconItems[Count].
    /// </summary>
    [Test]
    procedure Paint_ImageIndexEqualToCount_DoesNotRaise;

    /// <summary>
    ///  Regression. Clear cleared SVG, which, with a linked list, is the icon
    ///  inside the list: the icon disappeared for every other user of the list.
    /// </summary>
    [Test]
    procedure Clear_WithLinkedImageList_LeavesListIconAlone;

    /// <summary>
    ///  Regression. Assign shared the source's ISVG: changing the copy changed
    ///  the original.
    /// </summary>
    [Test]
    procedure Assign_CopiesTheSvg;

    /// <summary>Regression. Same for TSVGGraphic.Assign.</summary>
    [Test]
    procedure Graphic_Assign_CopiesTheSvg;

    /// <summary>
    ///  Regression. An empty TSVGGraphic is written with Size = 0, and
    ///  CopyFrom(Stream, 0) copies the WHOLE stream: reading it back fed the
    ///  surrounding bytes to the SVG parser.
    /// </summary>
    [Test]
    procedure Graphic_ReadEmptyData_StaysEmpty;
  end;

  [TestFixture]
  TSVGIconItemTests = class
  public
    /// <summary>
    ///  Regression. SVG := nil was accepted, and every later access (SVGText,
    ///  Assign, GetBitmap) was an access violation.
    /// </summary>
    [Test]
    procedure SetSVG_Nil_LeavesAnEmptySvg;
  end;

  [TestFixture]
  TSVGIconUtilsTests = class
  strict private
    FForm: TForm;
    FListView: TListView;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    /// <summary>
    ///  Regression. The TVirtualImageList branches were under {$IFDEF D10_3},
    ///  which is never defined (the .inc defines D10_3+): a list view fed by
    ///  a TSVGIconVirtualImageList stayed empty.
    /// </summary>
    [Test]
    procedure UpdateListView_VirtualImageList_ListsIcons;

    /// <summary>
    ///  Regression. With a category filter the function returned the number of
    ///  icons, not the number of list items it added.
    /// </summary>
    [Test]
    procedure UpdateListView_CategoryFilter_ReturnsItemsAdded;

    /// <summary>
    ///  Regression. The captions helper walked ImageList.Count and indexed the
    ///  list items with the icon index: with a filter it went past the end of
    ///  the list view, and without one it could label the wrong row.
    /// </summary>
    [Test]
    procedure UpdateCaptions_CategoryFilter_LabelsEachRowWithItsIcon;

    /// <summary>
    ///  Regression. The export filled the bitmap with clNone, which GDI paints
    ///  as white with alpha 0: un-premultiplying the blended pixels gave
    ///  channels above 255, wrapped around by {$R-}.
    /// </summary>
    [Test]
    procedure ExportToPng_HalfTransparentIcon_KeepsItsColor;
  end;

implementation

uses
  System.IOUtils,
  Winapi.Messages,
  Vcl.Imaging.pngimage,
  Vcl.VirtualImageList,
  SVGIconUtils;

const
  /// <summary>
  ///  Root fill green inherited by the left half, explicit blue on the right
  ///  half: "root only" recolours the left half and leaves the right one.
  /// </summary>
  SVG_ROOT_AND_CHILD =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" fill="#00FF00">' +
    '<rect x="0" y="0" width="50" height="100"/>' +
    '<rect x="50" y="0" width="50" height="100" fill="#0000FF"/></svg>';

type
  TSVGGraphicAccess = class(TSVGGraphic);

function NewSvgOf(const ASource: string): ISVG;
begin
  Result := GlobalSVGFactory.NewSvg;
  Result.Source := ASource;
end;

function WhiteBitmap(const AWidth, AHeight: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AWidth, AHeight);
  Result.Canvas.Brush.Color := clPureWhite;
  Result.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));
end;

{ TSVGIconImageListTests }

procedure TSVGIconImageListTests.Setup;
begin
  FList := TSVGIconImageList.Create(nil);
  FList.Size := 16;
  FChangeCount := 0;
end;

procedure TSVGIconImageListTests.TearDown;
begin
  FreeAndNil(FList);
end;

procedure TSVGIconImageListTests.ListChanged(Sender: TObject);
begin
  Inc(FChangeCount);
end;

function TSVGIconImageListTests.AddIcon(const ASource, AName: string): Integer;
begin
  Result := FList.Add(NewSvgOf(ASource), AName);
end;

function TSVGIconImageListTests.DrawIcon(const AIndex: Integer): TBitmap;
begin
  Result := WhiteBitmap(FList.Width, FList.Height);
  FList.Draw(Result.Canvas, 0, 0, AIndex);
end;

procedure TSVGIconImageListTests.Add_WithCategoryArgument_BuildsIconName;
begin
  FList.Add(NewSvgOf(SVG_RED_SQUARE), 'Left', 'Arrows');
  Assert.AreEqual('Arrows\Left', FList.SVGIconItems[0].IconName);
end;

procedure TSVGIconImageListTests.Add_NameWithCategory_KeepsCategory;
begin
  AddIcon(SVG_RED_SQUARE, 'Arrows\Left');
  Assert.AreEqual('Arrows\Left', FList.SVGIconItems[0].IconName, 'IconName');
  Assert.AreEqual('Arrows', FList.SVGIconItems[0].Category, 'Category');
  Assert.AreEqual(0, Integer(FList.GetIndexByName('Arrows\Left')), 'GetIndexByName');
end;

procedure TSVGIconImageListTests.SaveToFile_SeveralIcons_EachIconInItsCell;
var
  LFileName: string;
  LStrip: TBitmap;
begin
  AddIcon(SVG_RED_SQUARE, 'red');
  AddIcon(SVG_BLUE_SQUARE, 'blue');
  AddIcon(SVG_BLUE_SQUARE, 'blue2');
  AddIcon(SVG_RED_SQUARE, 'red2');
  LFileName := TSVGTestUtils.TempFileName('.bmp');
  try
    FList.SaveToFile(LFileName);
    LStrip := TBitmap.Create;
    try
      LStrip.LoadFromFile(LFileName);
      LStrip.PixelFormat := pf32bit;
      Assert.AreEqual(32, LStrip.Width, 'strip width (2 cells of 16)');
      Assert.AreEqual(32, LStrip.Height, 'strip height (2 cells of 16)');
      TSVGTestUtils.AssertPixel(LStrip, 8, 8, clPureRed, 'cell 0');
      TSVGTestUtils.AssertPixel(LStrip, 24, 8, clPureBlue, 'cell 1');
      TSVGTestUtils.AssertPixel(LStrip, 8, 24, clPureBlue, 'cell 2');
      TSVGTestUtils.AssertPixel(LStrip, 24, 24, clPureRed, 'cell 3');
    finally
      LStrip.Free;
    end;
  finally
    System.SysUtils.DeleteFile(LFileName);
  end;
end;

procedure TSVGIconImageListTests.SaveToFile_NoIcons_DoesNotRaise;
var
  LFileName: string;
begin
  LFileName := TSVGTestUtils.TempFileName('.bmp');
  try
    // Fails the test with the exception, if any.
    FList.SaveToFile(LFileName);
  finally
    System.SysUtils.DeleteFile(LFileName);
  end;
end;

procedure TSVGIconImageListTests.ApplyFixedColorToRootOnly_IsStreamed;
var
  LStream: TMemoryStream;
  LCopy: TSVGIconImageList;
begin
  FList.FixedColor := clPureRed;
  FList.ApplyFixedColorToRootOnly := True;
  LStream := TMemoryStream.Create;
  try
    LStream.WriteComponent(FList);
    LStream.Position := 0;
    LCopy := TSVGIconImageList.Create(nil);
    try
      LStream.ReadComponent(LCopy);
      Assert.IsTrue(LCopy.ApplyFixedColorToRootOnly);
    finally
      LCopy.Free;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TSVGIconImageListTests.Draw_ApplyFixedColorToRootOnly_KeepsChildColors;
var
  LBitmap: TBitmap;
begin
  AddIcon(SVG_ROOT_AND_CHILD, 'icon');
  FList.FixedColor := clPureRed;
  FList.ApplyFixedColorToRootOnly := True;
  LBitmap := DrawIcon(0);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 3, 8, clPureRed, 'inherited half');
    TSVGTestUtils.AssertPixel(LBitmap, 12, 8, clPureBlue, 'explicit half');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGIconImageListTests.AntiAliasColor_Set_NotifiesChange;
begin
  AddIcon(SVG_RED_SQUARE, 'red');
  FList.OnChange := ListChanged;
  FList.AntiAliasColor := clPureYellow;
  Assert.IsTrue(FChangeCount > 0, 'OnChange not fired');
end;

{ TSVGIconImageCollectionTests }

procedure TSVGIconImageCollectionTests.Setup;
begin
  FCollection := TSVGIconImageCollection.Create(nil);
  FCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'Home');
end;

procedure TSVGIconImageCollectionTests.TearDown;
begin
  FreeAndNil(FCollection);
end;

procedure TSVGIconImageCollectionTests.IndexOf_And_Remove_IgnoreCase;
begin
  Assert.AreEqual(0, FCollection.IndexOf('home'), 'IndexOf');
  FCollection.Remove('HOME');
  Assert.AreEqual(0, FCollection.SVGIconItems.Count, 'Count after Remove');
end;

procedure TSVGIconImageCollectionTests.SaveToFile_FileNameWithoutFolder_Saves;
var
  LOldDir, LFileName: string;
begin
  LOldDir := GetCurrentDir;
  LFileName := ExtractFileName(TSVGTestUtils.TempFileName('.svg'));
  SetCurrentDir(GetEnvironmentVariable('TEMP'));
  try
    Assert.IsTrue(FCollection.SaveToFile(LFileName, 'Home'), 'SaveToFile');
    Assert.IsTrue(FileExists(LFileName), 'file written');
  finally
    System.SysUtils.DeleteFile(LFileName);
    SetCurrentDir(LOldDir);
  end;
end;

procedure TSVGIconImageCollectionTests.Assign_CopiesAntiAliasColor;
var
  LCopy: TSVGIconImageCollection;
begin
  FCollection.AntiAliasColor := clPureYellow;
  LCopy := TSVGIconImageCollection.Create(nil);
  try
    LCopy.Assign(FCollection);
    Assert.AreEqual(Integer(clPureYellow), Integer(LCopy.AntiAliasColor));
  finally
    LCopy.Free;
  end;
end;

{ TSVGIconVirtualImageListTests }

procedure TSVGIconVirtualImageListTests.Setup;
begin
  FCollection := TSVGIconImageCollection.Create(nil);
  FCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'red');
  FList := TSVGIconVirtualImageList.Create(nil);
  FList.Size := 16;
  FList.FixedColor := clPureBlue;
  FList.ImageCollection := FCollection;
  FList.AutoFill := True;
end;

procedure TSVGIconVirtualImageListTests.TearDown;
begin
  FreeAndNil(FList);
  FreeAndNil(FCollection);
end;

function TSVGIconVirtualImageListTests.DrawIcon(const AIndex: Integer;
  const AEnabled: Boolean): TBitmap;
begin
  Result := WhiteBitmap(FList.Width, FList.Height);
  FList.Draw(Result.Canvas, 0, 0, AIndex, AEnabled);
end;

procedure TSVGIconVirtualImageListTests.AssertIconColor(const AIndex: Integer;
  const AExpected: TColor; const AWhat: string; const AEnabled: Boolean);
var
  LBitmap: TBitmap;
begin
  LBitmap := DrawIcon(AIndex, AEnabled);
  try
    TSVGTestUtils.AssertPixel(LBitmap, 8, 8, AExpected, AWhat);
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGIconVirtualImageListTests.Draw_UsesOwnFixedColor;
begin
  AssertIconColor(0, clPureBlue, 'icon 0');
end;

procedure TSVGIconVirtualImageListTests.AutoFill_CollectionChanged_KeepsOwnFixedColor;
begin
  FCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'red2');
  Assert.AreEqual(2, FList.Count, 'Count');
  AssertIconColor(0, clPureBlue, 'icon 0');
  AssertIconColor(1, clPureBlue, 'icon 1');
end;

procedure TSVGIconVirtualImageListTests.ItemChanged_KeepsOwnFixedColor;
begin
  FList.AutoFill := False;
  // A different red square, so that the item really changes.
  FCollection.SVGIconItems[0].SVGText := StringReplace(SVG_RED_SQUARE,
    'x="0"', 'x="0.0"', []);
  AssertIconColor(0, clPureBlue, 'icon 0');
end;

procedure TSVGIconVirtualImageListTests.DrawDisabled_UsesOwnFixedColor;
begin
  FList.DisabledGrayscale := False;
  FList.DisabledOpacity := 255;
  AssertIconColor(0, clPureBlue, 'disabled icon 0', False);
end;

procedure TSVGIconVirtualImageListTests.DrawDisabled_AfterDisabledOpacityChange_UsesOwnFixedColor;
begin
  FList.DisabledGrayscale := False;
  FList.DisabledOpacity := 255;
  // Creates the disabled bitmap...
  DrawIcon(0, False).Free;
  // ...which this rebuilds.
  FList.DisabledOpacity := 254;
  AssertIconColor(0, clPureBlue, 'disabled icon 0', False);
end;

procedure TSVGIconVirtualImageListTests.Issue_DisabledCurrentColorIcon_UsesListFixedColor;
const
  SVG_CURRENTCOLOR_SQUARE =
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">' +
    '<path fill="currentColor" d="M0 0 H100 V100 H0 Z"/></svg>';
var
  LBitmap: TBitmap;
begin
  FCollection.SVGIconItems[0].SVGText := SVG_CURRENTCOLOR_SQUARE;
  Assert.AreEqual(Integer(SVG_INHERIT_COLOR), Integer(FCollection.FixedColor), 'collection FixedColor');
  FList.FixedColor := clPureWhite;
  FList.GrayScale := False;
  FList.DisabledGrayscale := False;
  FList.DisabledOpacity := 125;
  // Enabled: white
  LBitmap := TSVGTestUtils.Render(GlobalSVGFactory.NewSvg, FList.Width, FList.Height, clPureBlack);
  try
    FList.Draw(LBitmap.Canvas, 0, 0, 0, True);
    TSVGTestUtils.AssertPixel(LBitmap, 8, 8, clPureWhite, 'enabled');
  finally
    LBitmap.Free;
  end;
  // Disabled (first request: the disabled bitmap is created now): white at
  // DisabledOpacity, not the collection color (black) at DisabledOpacity
  LBitmap := TSVGTestUtils.Render(GlobalSVGFactory.NewSvg, FList.Width, FList.Height, clPureBlack);
  try
    FList.Draw(LBitmap.Canvas, 0, 0, 0, False);
    TSVGTestUtils.AssertPixel(LBitmap, 8, 8, RGB(125, 125, 125), 'disabled');
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGIconVirtualImageListTests.Assign_CopiesOwnAttributes;
var
  LCopy: TSVGIconVirtualImageList;
begin
  FList.GrayScale := True;
  FList.Opacity := 100;
  FList.ApplyFixedColorToRootOnly := True;
  FList.AntiAliasColor := clPureYellow;
  LCopy := TSVGIconVirtualImageList.Create(nil);
  try
    LCopy.Assign(FList);
    Assert.AreEqual(Integer(FList.FixedColor), Integer(LCopy.FixedColor), 'FixedColor');
    Assert.IsTrue(LCopy.GrayScale, 'GrayScale');
    Assert.AreEqual(100, Integer(LCopy.Opacity), 'Opacity');
    Assert.IsTrue(LCopy.ApplyFixedColorToRootOnly, 'ApplyFixedColorToRootOnly');
    Assert.AreEqual(Integer(clPureYellow), Integer(LCopy.AntiAliasColor), 'AntiAliasColor');
  finally
    LCopy.Free;
  end;
end;

{ TSVGIconImageTests }

procedure TSVGIconImageTests.Setup;
begin
  FImage := TSVGIconImage.Create(nil);
  FImage.SetBounds(0, 0, 32, 32);
  FList := TSVGIconImageList.Create(nil);
  FList.Size := 32;
  FList.Add(NewSvgOf(SVG_RED_SQUARE), 'red');
  FList.Add(NewSvgOf(SVG_BLUE_SQUARE), 'blue');
end;

procedure TSVGIconImageTests.TearDown;
begin
  FreeAndNil(FImage);
  FreeAndNil(FList);
end;

procedure TSVGIconImageTests.PaintImage;
var
  LBitmap: TBitmap;
begin
  // A TGraphicControl paints on the DC a WM_PAINT carries, parent or not.
  LBitmap := WhiteBitmap(FImage.Width, FImage.Height);
  try
    FImage.Perform(WM_PAINT, WPARAM(LBitmap.Canvas.Handle), 0);
  finally
    LBitmap.Free;
  end;
end;

procedure TSVGIconImageTests.Paint_LinkedToPlainImageList_DoesNotRaise;
var
  LPlainList: TImageList;
  LBitmap: TBitmap;
begin
  LPlainList := TImageList.Create(nil);
  try
    LBitmap := WhiteBitmap(16, 16);
    try
      LPlainList.Add(LBitmap, nil);
    finally
      LBitmap.Free;
    end;
    FImage.ImageList := LPlainList;
    FImage.ImageIndex := 0;
    Assert.WillNotRaiseAny(PaintImage);
    FImage.ImageList := nil;
  finally
    LPlainList.Free;
  end;
end;

procedure TSVGIconImageTests.Paint_ImageIndexEqualToCount_DoesNotRaise;
begin
  FImage.ImageList := FList;
  FImage.ImageIndex := 1;
  PaintImage;
  FList.Delete(1);
  Assert.WillNotRaiseAny(PaintImage);
end;

procedure TSVGIconImageTests.Clear_WithLinkedImageList_LeavesListIconAlone;
begin
  FImage.ImageList := FList;
  FImage.ImageIndex := 0;
  FImage.Clear;
  Assert.IsFalse(FList.SVGIconItems[0].SVG.IsEmpty, 'the list icon was emptied');
end;

procedure TSVGIconImageTests.Assign_CopiesTheSvg;
var
  LCopy: TSVGIconImage;
begin
  FImage.SVGText := SVG_RED_SQUARE;
  LCopy := TSVGIconImage.Create(nil);
  try
    LCopy.Assign(FImage);
    Assert.AreEqual(SVG_RED_SQUARE, LCopy.SVGText, 'copy');
    LCopy.SVGText := SVG_BLUE_SQUARE;
    Assert.AreEqual(SVG_RED_SQUARE, FImage.SVGText, 'original after changing the copy');
  finally
    LCopy.Free;
  end;
end;

procedure TSVGIconImageTests.Graphic_Assign_CopiesTheSvg;

  function TextOf(const AGraphic: TSVGGraphic): string;
  var
    LStream: TStringStream;
  begin
    LStream := TStringStream.Create('', TEncoding.UTF8);
    try
      AGraphic.SaveToStream(LStream);
      Result := LStream.DataString;
    finally
      LStream.Free;
    end;
  end;

var
  LGraphic, LCopy: TSVGGraphic;
  LStream: TStringStream;
begin
  LGraphic := TSVGGraphic.Create;
  LCopy := TSVGGraphic.Create;
  try
    LGraphic.AssignSVG(NewSvgOf(SVG_RED_SQUARE));
    LGraphic.Opacity := 100;
    LCopy.Assign(LGraphic);
    Assert.AreEqual(SVG_RED_SQUARE, TextOf(LCopy), 'copy');
    Assert.AreEqual(100, Integer(LCopy.Opacity), 'Opacity');
    LStream := TStringStream.Create(SVG_BLUE_SQUARE, TEncoding.UTF8);
    try
      LCopy.LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
    Assert.AreEqual(SVG_RED_SQUARE, TextOf(LGraphic), 'original after changing the copy');
  finally
    LCopy.Free;
    LGraphic.Free;
  end;
end;

procedure TSVGIconImageTests.Graphic_ReadEmptyData_StaysEmpty;
const
  PREFIX: AnsiString = 'bytes that belong to someone else';
var
  LEmpty, LRead: TSVGGraphic;
  LStream: TMemoryStream;
begin
  LEmpty := TSVGGraphic.Create;
  LRead := TSVGGraphic.Create;
  LStream := TMemoryStream.Create;
  try
    LStream.WriteBuffer(PAnsiChar(PREFIX)^, Length(PREFIX));
    TSVGGraphicAccess(LEmpty).WriteData(LStream);
    LStream.Position := Length(PREFIX);
    // Fails the test with the exception, if any.
    TSVGGraphicAccess(LRead).ReadData(LStream);
    Assert.IsTrue(LRead.Empty, 'Empty');
    Assert.AreEqual(LStream.Size, LStream.Position, 'stream left at the end of the data');
  finally
    LStream.Free;
    LRead.Free;
    LEmpty.Free;
  end;
end;

{ TSVGIconItemTests }

procedure TSVGIconItemTests.SetSVG_Nil_LeavesAnEmptySvg;
var
  LList: TSVGIconImageList;
  LItem: TSVGIconItem;
begin
  LList := TSVGIconImageList.Create(nil);
  try
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'red');
    LItem := LList.SVGIconItems[0];
    LItem.SVG := nil;
    Assert.IsNotNull(LItem.SVG, 'SVG');
    Assert.AreEqual('', LItem.SVGText, 'SVGText');
  finally
    LList.Free;
  end;
end;

{ TSVGIconUtilsTests }

procedure TSVGIconUtilsTests.Setup;
begin
  FForm := TForm.CreateNew(nil);
  FListView := TListView.Create(FForm);
  FListView.Parent := FForm;
  FListView.ViewStyle := vsIcon;
end;

procedure TSVGIconUtilsTests.TearDown;
begin
  FreeAndNil(FForm);
end;

procedure TSVGIconUtilsTests.UpdateListView_VirtualImageList_ListsIcons;
var
  LCollection: TSVGIconImageCollection;
  LList: TSVGIconVirtualImageList;
begin
  LCollection := TSVGIconImageCollection.Create(nil);
  LList := TSVGIconVirtualImageList.Create(nil);
  try
    LCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'one');
    LCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'two');
    LCollection.Add(NewSvgOf(SVG_RED_SQUARE), 'three');
    LList.ImageCollection := LCollection;
    LList.AutoFill := True;
    FListView.LargeImages := LList;
    Assert.AreEqual(3, UpdateSVGIconListView(FListView), 'result');
    Assert.AreEqual(3, FListView.Items.Count, 'list items');
    FListView.LargeImages := nil;
  finally
    LList.Free;
    LCollection.Free;
  end;
end;

procedure TSVGIconUtilsTests.UpdateListView_CategoryFilter_ReturnsItemsAdded;
var
  LList: TSVGIconImageList;
begin
  LList := TSVGIconImageList.Create(nil);
  try
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'one', 'A');
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'two', 'B');
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'three', 'A');
    FListView.LargeImages := LList;
    Assert.AreEqual(2, UpdateSVGIconListView(FListView, 'A'), 'result');
    Assert.AreEqual(2, FListView.Items.Count, 'list items');
    FListView.LargeImages := nil;
  finally
    LList.Free;
  end;
end;

procedure TSVGIconUtilsTests.UpdateCaptions_CategoryFilter_LabelsEachRowWithItsIcon;
var
  LList: TSVGIconImageList;
begin
  LList := TSVGIconImageList.Create(nil);
  try
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'one', 'A');
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'two', 'B');
    LList.Add(NewSvgOf(SVG_RED_SQUARE), 'three', 'A');
    FListView.LargeImages := LList;
    UpdateSVGIconListView(FListView, 'A');
    // Fails the test with the exception, if any.
    UpdateSVGIconListViewCaptions(FListView);
    Assert.AreEqual('0.A\one', FListView.Items[0].Caption, 'row 0');
    Assert.AreEqual('2.A\three', FListView.Items[1].Caption, 'row 1');
    FListView.LargeImages := nil;
  finally
    LList.Free;
  end;
end;

procedure TSVGIconUtilsTests.ExportToPng_HalfTransparentIcon_KeepsItsColor;
var
  LSVG: ISVG;
  LFolder, LFileName: string;
  LPng: TPngImage;
  LRGB: PByte;
begin
  LSVG := NewSvgOf(SVG_RED_SQUARE);
  LSVG.Opacity := 0.5;
  LFolder := GetEnvironmentVariable('TEMP');
  LFileName := ExtractFileName(TSVGTestUtils.TempFileName('.png'));
  SVGExportToPng(16, 16, LSVG, LFolder, LFileName);
  LFileName := IncludeTrailingPathDelimiter(LFolder) + LFileName;
  LPng := TPngImage.Create;
  try
    LPng.LoadFromFile(LFileName);
    Assert.AreEqual(Integer(128), Integer(LPng.AlphaScanline[8][8]), 3, 'alpha');
    // PNG scan lines are BGR.
    LRGB := PByte(LPng.Scanline[8]);
    Inc(LRGB, 8 * 3);
    Assert.AreEqual(0, Integer(LRGB[0]), 3, 'blue');
    Assert.AreEqual(0, Integer(LRGB[1]), 3, 'green');
    Assert.AreEqual(255, Integer(LRGB[2]), 3, 'red');
  finally
    LPng.Free;
    System.SysUtils.DeleteFile(LFileName);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSVGIconImageListTests);
  TDUnitX.RegisterTestFixture(TSVGIconImageCollectionTests);
  TDUnitX.RegisterTestFixture(TSVGIconVirtualImageListTests);
  TDUnitX.RegisterTestFixture(TSVGIconImageTests);
  TDUnitX.RegisterTestFixture(TSVGIconItemTests);
  TDUnitX.RegisterTestFixture(TSVGIconUtilsTests);

end.
