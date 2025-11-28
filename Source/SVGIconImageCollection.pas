{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Vincent Parrett                                                }
{       Contributors: Carlo Barazzetta, Kiriakos Vlahos                        }
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
///   Centralized SVG icon collection component for sharing icons across
///   multiple VirtualImageLists. Recommended for Delphi 10.3+ applications.
/// </summary>
unit SVGIconImageCollection;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Types
  , System.UITypes
  , System.Classes
  , WinApi.Windows
  , Vcl.Graphics
{$IFDEF D10_3+}
  , Vcl.BaseImageCollection
{$ENDIF}
  , SvgInterfaces
  , SVGIconItems
  ;

type
  /// <summary>
  ///   Alias for TSVGIconItem from SVGIconItems unit.
  /// </summary>
  TSVGIconItem = SVGIconItems.TSVGIconItem;

  /// <summary>
  ///   Alias for TSVGIconItems from SVGIconItems unit.
  /// </summary>
  TSVGIconItems = SVGIconItems.TSVGIconItems;

type
  /// <summary>
  ///   A centralized collection of SVG icons that can be shared across multiple
  ///   TSVGIconVirtualImageList components.
  /// </summary>
  /// <remarks>
  ///   <para>TSVGIconImageCollection is the recommended approach for Delphi 10.3+
  ///   applications. Benefits include:</para>
  ///   <list type="bullet">
  ///     <item>Single source of icons shared by multiple image lists</item>
  ///     <item>Reduced memory usage when the same icons appear in multiple forms</item>
  ///     <item>Centralized icon management</item>
  ///     <item>Integration with TVirtualImageList (inherited from TCustomImageCollection)</item>
  ///   </list>
  ///   <para>Use TSVGIconVirtualImageList to connect to this collection and
  ///   display icons with custom sizes and rendering attributes.</para>
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Create collection and virtual image lists
  ///   SVGIconImageCollection1.LoadFromFiles(IconFileList);
  ///   SVGIconVirtualImageList1.ImageCollection := SVGIconImageCollection1;
  ///   SVGIconVirtualImageList1.Size := 24;  // 24x24 icons
  ///   SVGIconVirtualImageList2.ImageCollection := SVGIconImageCollection1;
  ///   SVGIconVirtualImageList2.Size := 32;  // 32x32 icons from same collection
  ///   </code>
  /// </example>
  {$IFDEF D10_3+}
  TSVGIconImageCollection = class(TCustomImageCollection)
  {$ELSE}
  TSVGIconImageCollection = class(TComponent)
  {$ENDIF}
  private
    FSVGItems: TSVGIconItems;
    FFixedColor: TColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FAntiAliasColor: TColor;
    FOpacity: Byte;
    procedure SetSVGIconItems(const Value: TSVGIconItems);
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetAntiAliasColor(const Value: TColor);
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
    procedure SetOpacity(const Value: Byte);

  protected
    {$IFDEF D10_3+}
    function GetCount: Integer; override;
    {$ENDIF}

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    procedure DefineProperties(Filer: TFiler); override;

  public
    /// <summary>
    ///   Sets multiple color-related properties in a single call.
    /// </summary>
    /// <param name="AFixedColor">
    ///   The fixed color to apply. Default is SVG_INHERIT_COLOR.
    /// </param>
    /// <param name="AApplyToRootOnly">
    ///   When True, applies color only to root element. Default is False.
    /// </param>
    /// <param name="AAntiAliasColor">
    ///   Background color for anti-aliasing. Default is clBtnFace.
    /// </param>
    procedure SetColors(const AFixedColor: TColor = SVG_INHERIT_COLOR;
      const AApplyToRootOnly: Boolean = False;
      const AAntiAliasColor: TColor = clBtnFace);

    {$IFDEF D10_3+}
    /// <summary>
    ///   Checks if the specified index is valid.
    /// </summary>
    /// <param name="AIndex">
    ///   The index to check.
    /// </param>
    /// <returns>
    ///   True if the index is valid; False otherwise.
    /// </returns>
    function IsIndexAvailable(AIndex: Integer): Boolean; override;

    /// <summary>
    ///   Gets the index of an icon by its name.
    /// </summary>
    /// <param name="AName">
    ///   The icon name to search for (case-insensitive).
    /// </param>
    /// <returns>
    ///   The zero-based index, or -1 if not found.
    /// </returns>
    function GetIndexByName(const AName: String): Integer; override;

    /// <summary>
    ///   Gets the name of an icon by its index.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the icon.
    /// </param>
    /// <returns>
    ///   The icon name, or empty string if index is invalid.
    /// </returns>
    function GetNameByIndex(AIndex: Integer): String; override;

    /// <summary>
    ///   Renders an icon to a bitmap at the specified size.
    /// </summary>
    /// <param name="AIndex">
    ///   The index of the icon to render.
    /// </param>
    /// <param name="AWidth">
    ///   The width of the output bitmap.
    /// </param>
    /// <param name="AHeight">
    ///   The height of the output bitmap.
    /// </param>
    /// <returns>
    ///   A new TBitmap containing the rendered icon. Caller must free.
    ///   Returns nil if index is invalid.
    /// </returns>
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; override;

    /// <summary>
    ///   Draws an icon to a canvas.
    /// </summary>
    /// <param name="ACanvas">
    ///   The canvas to draw to.
    /// </param>
    /// <param name="ARect">
    ///   The destination rectangle.
    /// </param>
    /// <param name="AIndex">
    ///   The index of the icon to draw.
    /// </param>
    /// <param name="AProportional">
    ///   When True, maintains aspect ratio. Default is False.
    /// </param>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); override;

    /// <summary>
    ///   Updates all rendering attributes at once.
    /// </summary>
    /// <param name="AFixedColor">
    ///   The fixed color to apply.
    /// </param>
    /// <param name="AApplyFixedColorToRootOnly">
    ///   Whether to apply color only to root element.
    /// </param>
    /// <param name="AGrayScale">
    ///   Whether to render in grayscale.
    /// </param>
    /// <param name="AAntiAliasColor">
    ///   Background color for anti-aliasing.
    /// </param>
    /// <param name="AOpacity">
    ///   Opacity value from 0 to 255.
    /// </param>
    procedure UpdateAttributes(AFixedColor: TColor;
      AApplyFixedColorToRootOnly: Boolean;
      AGrayScale: Boolean;
      AAntiAliasColor: TColor;
      AOpacity: Byte);
    {$ELSE}
    /// <summary>
    ///   Triggers a change notification for pre-10.3 compatibility.
    /// </summary>
    procedure Change;

    /// <summary>
    ///   Draws an icon to a canvas.
    /// </summary>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False);
    {$ENDIF}

    /// <summary>
    ///   Creates a new SVG icon image collection.
    /// </summary>
    /// <param name="AOwner">
    ///   The component that owns this collection.
    /// </param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Destroys the collection and releases all resources.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Copies properties and icons from another collection or items.
    /// </summary>
    /// <param name="Source">
    ///   The source to copy from (TSVGIconImageCollection or TSVGIconItems).
    /// </param>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Loads multiple SVG files into the collection.
    /// </summary>
    /// <param name="AFileNames">
    ///   A TStrings containing the full paths to SVG files.
    /// </param>
    /// <param name="AAppend">
    ///   When True, appends to existing icons. When False, clears first.
    ///   Default is True.
    /// </param>
    /// <returns>
    ///   The number of icons successfully loaded.
    /// </returns>
    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;

    /// <summary>
    ///   Loads a single SVG file into the collection.
    /// </summary>
    /// <param name="AFileName">
    ///   The full path to the SVG file.
    /// </param>
    /// <param name="AImageName">
    ///   Output parameter receiving the assigned icon name.
    /// </param>
    /// <returns>
    ///   The newly created TSVGIconItem, or nil if file doesn't exist.
    /// </returns>
    function LoadFromFile(const AFileName: string;
      out AImageName: string): TSVGIconItem;

    /// <summary>
    ///   Saves an icon to an SVG file.
    /// </summary>
    /// <param name="AFileName">
    ///   The full path of the file to create.
    /// </param>
    /// <param name="AImageName">
    ///   The name of the icon to save.
    /// </param>
    /// <returns>
    ///   True if successful; False if icon not found.
    /// </returns>
    function SaveToFile(const AFileName: string;
      const AImageName: string): Boolean;

    /// <summary>
    ///   Adds an SVG icon to the collection.
    /// </summary>
    /// <param name="ASVG">
    ///   The ISVG interface containing the SVG.
    /// </param>
    /// <param name="AIconName">
    ///   The name to assign to this icon.
    /// </param>
    /// <param name="AGrayScale">
    ///   Whether to render in grayscale. Default is False.
    /// </param>
    /// <param name="AFixedColor">
    ///   Fixed color for this icon. Default is SVG_INHERIT_COLOR.
    /// </param>
    /// <param name="AApplyToRootOnly">
    ///   Apply color only to root element. Default is False.
    /// </param>
    /// <returns>
    ///   The index of the newly added icon.
    /// </returns>
    function Add(const ASVG: ISVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR;
       const AApplyToRootOnly: Boolean = False): Integer;

    /// <summary>
    ///   Deletes an icon by index.
    /// </summary>
    /// <param name="Index">
    ///   The zero-based index of the icon to delete.
    /// </param>
    procedure Delete(const Index: Integer); overload;

    /// <summary>
    ///   Deletes icons by category and index range.
    /// </summary>
    /// <param name="ACategory">
    ///   The category to filter by (empty string for all).
    /// </param>
    /// <param name="AStartIndex">
    ///   The starting index for deletion.
    /// </param>
    /// <param name="AEndIndex">
    ///   The ending index for deletion (-1 for end of list).
    /// </param>
    procedure Delete(const ACategory: String; AStartIndex, AEndIndex: Integer); overload;

    /// <summary>
    ///   Removes an icon by name.
    /// </summary>
    /// <param name="Name">
    ///   The name of the icon to remove.
    /// </param>
    procedure Remove(const Name: string);

    /// <summary>
    ///   Finds the index of an icon by name.
    /// </summary>
    /// <param name="Name">
    ///   The icon name to search for.
    /// </param>
    /// <returns>
    ///   The zero-based index, or -1 if not found.
    /// </returns>
    function IndexOf(const Name: string): Integer;

    /// <summary>
    ///   Removes all icons from the collection.
    /// </summary>
    procedure ClearIcons;

    /// <summary>
    ///   Loads an SVG from a resource embedded in the application.
    /// </summary>
    /// <param name="hInstance">
    ///   The module handle containing the resource.
    /// </param>
    /// <param name="ResourceName">
    ///   The name of the RCDATA resource.
    /// </param>
    /// <param name="IconName">
    ///   The name to assign to the loaded icon.
    /// </param>
    /// <returns>
    ///   The index of the newly added icon.
    /// </returns>
    function LoadFromResource(const hInstance : THandle; const ResourceName : string; const IconName : string) : integer;

    /// <summary>
    ///   Loads an SVG from a string containing SVG XML source.
    /// </summary>
    /// <param name="Source">
    ///   The SVG XML source string.
    /// </param>
    /// <param name="IconName">
    ///   The name to assign to the loaded icon.
    /// </param>
    /// <returns>
    ///   The index of the newly added icon.
    /// </returns>
    function LoadFromString(const Source : string; const IconName : string) : integer;

  published
    /// <summary>
    ///   The collection of SVG icon items.
    /// </summary>
    property SVGIconItems: TSVGIconItems read FSVGItems write SetSVGIconItems;

    /// <summary>
    ///   Default fixed color applied to all icons.
    /// </summary>
    /// <value>
    ///   Default is SVG_INHERIT_COLOR (use original colors).
    /// </value>
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;

    /// <summary>
    ///   When True, applies FixedColor only to root SVG elements.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default False;

    /// <summary>
    ///   Background color for anti-aliasing.
    /// </summary>
    /// <value>
    ///   Default is clBtnFace.
    /// </value>
    property AntiAliasColor: TColor read FAntiAliasColor write SetAntiAliasColor default clBtnFace;

    /// <summary>
    ///   Renders all icons in grayscale when True.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;

    /// <summary>
    ///   Default opacity applied to all icons (0-255).
    /// </summary>
    /// <value>
    ///   Default is 255 (fully opaque).
    /// </value>
    property Opacity: Byte read FOpacity write SetOpacity default 255;
  end;

implementation

uses
  System.SysUtils
  , VCL.Controls
  , {$IFDEF DXE4+}System.Messaging{$ELSE}SVGMessaging{$ENDIF};

{ TSVGIconImageCollection }

function TSVGIconImageCollection.Add(
  const ASVG: ISVG; const AIconName: string;
  const AGrayScale: Boolean = False;
  const AFixedColor: TColor = SVG_INHERIT_COLOR;
  const AApplyToRootOnly: Boolean = False): Integer;
var
  Item: TSVGIconItem;
begin
  FSVGItems.BeginUpdate;
  try
    Item := FSVGItems.Add;
    Item.SVG := ASVG;
    Item.IconName := AIconName;
    Item.FixedColor := AFixedColor;
    Item.ApplyFixedColorToRootOnly := AApplyToRootOnly;
    Item.GrayScale := AGrayScale;
  finally
    FSVGItems.EndUpdate;
  end;
  Result := FSVGItems.Count - 1;
end;

procedure TSVGIconImageCollection.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageCollection then
  begin
    FFixedColor := TSVGIconImageCollection(Source).FFixedColor;
    FApplyFixedColorToRootOnly := TSVGIconImageCollection(Source).FApplyFixedColorToRootOnly;
    FGrayScale := TSVGIconImageCollection(Source).FGrayScale;
    FOpacity := TSVGIconImageCollection(Source).FOpacity;
    FSVGItems.Assign(TSVGIconImageCollection(Source).SVGIconItems)
  end
  else if Source is TSVGIconItems then
    FSVGItems.Assign(Source)
  else
    inherited;
end;

procedure TSVGIconImageCollection.ClearIcons;
begin
  FSVGItems.BeginUpdate;
  try
    FSVGItems.Clear;
  finally
    FSVGItems.EndUpdate;
  end;
end;

constructor TSVGIconImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
  FFixedColor := SVG_INHERIT_COLOR;
  FApplyFixedColorToRootOnly := False;
  FAntiAliasColor := clBtnFace;
  FGrayScale := False;
  FOpacity := 255;
end;

procedure TSVGIconImageCollection.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Info: Longint;
begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then
    Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft, LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop, LongRec(DesignInfo).Hi <> LongRec(Info).Hi);
end;

procedure TSVGIconImageCollection.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
  begin
    FSVGItems.Delete(Index);
  end;
end;

procedure TSVGIconImageCollection.Delete(const ACategory: String; AStartIndex, AEndIndex: Integer);
var
  I: Integer;
begin
  if FSVGItems.Count = 0 then
    Exit;

  if (ACategory = '') and (AStartIndex <= 0) and ((AEndIndex < 0) or (AEndIndex >= FSVGItems.Count - 1)) then
  begin
    FSVGItems.Clear;
    Exit;
  end;

  if AStartIndex < 0 then
    AStartIndex := 0;
  if (AEndIndex < 0) or (AEndIndex > FSVGItems.Count - 1) then
    AEndIndex := FSVGItems.Count - 1;

  FSVGItems.BeginUpdate;
  try
    for I := AEndIndex downto AStartIndex do
      if (ACategory = '') or SameText(ACategory, FSVGItems[I].Category) then
      begin
          FSVGItems.Delete(I);
      end;
  finally
    FSVGItems.EndUpdate;
  end;
end;

destructor TSVGIconImageCollection.Destroy;
begin
  FSVGItems.Free;
  inherited;
end;

function TSVGIconImageCollection.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to FSVGItems.Count - 1 do
    if FSVGItems[Result].IconName = Name then
      Exit;
  Result := -1;
end;

function TSVGIconImageCollection.LoadFromFile(const AFileName: string;
  out AImageName: string): TSVGIconItem;
begin
  SVGIconItems.BeginUpdate;
  try
    Result := SVGIconItems.LoadFromFile(AFileName, AImageName);
  finally
    SVGIconItems.EndUpdate;
  end;
end;

function TSVGIconImageCollection.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean): Integer;
begin
  SVGIconItems.BeginUpdate;
  try
    Result := SVGIconItems.LoadFromFiles(AFileNames, AAppend);
  finally
    SVGIconItems.EndUpdate;
  end;
end;

function TSVGIconImageCollection.LoadFromResource(const hInstance: THandle; const ResourceName, IconName: string) : integer;
var
  ResStream: TResourceStream;
  LSvg: ISVG;
begin
  resStream := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
  try
    LSvg := GlobalSVGFactory.NewSvg;
    LSvg.LoadFromStream(ResStream);
    result := Add(LSvg, IconName);
  finally
    ResStream.Free;
  end;
end;

function TSVGIconImageCollection.LoadFromString(const Source,  IconName: string): integer;
var
  LSvg: ISVG;
begin
  LSvg := GlobalSVGFactory.NewSvg;
  LSvg.Source := Source;
  result := Add(LSvg, IconName);
end;

procedure TSVGIconImageCollection.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

function TSVGIconImageCollection.SaveToFile(const AFileName,
  AImageName: string): Boolean;
var
  LOutDir: string;
  LItem: TSVGIconItem;
begin
  Result := False;
  LItem := SVGIconItems.GetIconByName(AImageName);
  if Assigned(LItem) then
  begin
    LOutDir := ExtractFilePath(AFileName);
    System.SysUtils.ForceDirectories(LOutDir);
    LItem.SVG.SaveToFile(AFileName);
    Result := True;
  end;
end;

procedure TSVGIconImageCollection.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FAntiAliasColor := Value;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetColors(
  const AFixedColor: TColor = SVG_INHERIT_COLOR;
  const AApplyToRootOnly: Boolean = False;
  const AAntiAliasColor: TColor = clBtnFace);
begin
  FSVGItems.BeginUpdate;
  try
    FFixedColor := AFixedColor;
    FApplyFixedColorToRootOnly := AApplyToRootOnly;
    FAntiAliasColor := AAntiAliasColor;
  finally
    FSVGItems.EndUpdate;
  end;
end;

procedure TSVGIconImageCollection.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FFixedColor := Value;
      if FFixedColor <> SVG_INHERIT_COLOR then
        FGrayScale := False;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetApplyFixedColorToRootOnly(
  const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FApplyFixedColorToRootOnly := Value;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;


procedure TSVGIconImageCollection.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FGrayScale := Value;
      if FGrayScale then
        FixedColor := SVG_INHERIT_COLOR;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FSVGItems.BeginUpdate;
    try
      FOpacity := Value;
    finally
      FSVGItems.EndUpdate;
    end;
  end;
end;

procedure TSVGIconImageCollection.SetSVGIconItems(const Value: TSVGIconItems);
begin
  FSVGItems.Assign(Value);
end;

procedure TSVGIconImageCollection.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageCollection.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{$IFDEF D10_3+}
function TSVGIconImageCollection.GetCount: Integer;
begin
  if Assigned(FSVGItems) then
    Result := FSVGItems.Count
  else
    Result := 0;
end;

function TSVGIconImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := FSVGItems[AIndex].IconName
  else
    result := '';
end;

function TSVGIconImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
  S: String;
begin
  Result := -1;
  S := LowerCase(AName);
  for I := 0 to FSVGItems.Count - 1 do
    if LowerCase(FSVGItems[I].IconName) = S then
      Exit(I);
end;

function TSVGIconImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count);
end;

function TSVGIconImageCollection.GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap;
begin
  if (AIndex >= 0) and (AIndex < FSVGItems.Count ) then
    Result := FSVGItems[AIndex].GetBitmap(AWidth, AHeight, FFixedColor,
      FApplyFixedColorToRootOnly, FOpacity, FGrayScale, FAntiAliasColor)
  else
    Result := nil;
end;

procedure TSVGIconImageCollection.UpdateAttributes(
  AFixedColor: TColor;
  AApplyFixedColorToRootOnly: Boolean;
  AGrayScale: Boolean;
  AAntiAliasColor: TColor;
  AOpacity: Byte);
begin
  if (AFixedColor <> FFixedColor) or
    (AApplyFixedColorToRootOnly <> FApplyFixedColorToRootOnly) or
    (AGrayScale <> FGrayScale) or
    (AAntiAliasColor <> FAntiAliasColor) or
    (AOpacity <> FOpacity) then
  FSVGItems.BeginUpdate;
  try
    FFixedColor := AFixedColor;
    FApplyFixedColorToRootOnly := AApplyFixedColorToRootOnly;
    FGrayScale := AGrayScale;
    FAntiAliasColor := AAntiAliasColor;
    FOpacity := AOpacity;
  finally
    FSVGItems.EndUpdate;
  end;
end;
{$ELSE}
procedure TSVGIconImageCollection.Change;
begin
  FSVGItems.BeginUpdate;
  FSVGItems.EndUpdate;
end;
{$ENDIF}

procedure TSVGIconImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
  AProportional: Boolean = False);
var
  LItem: TSVGIconItem;
  LSVG: ISVG;
begin
  LItem := FSVGItems.Items[AIndex];
  LSVG := LItem.SVG;
  if LItem.FixedColor <> SVG_INHERIT_COLOR then
  begin
    LSVG.ApplyFixedColorToRootOnly := LItem.ApplyFixedColorToRootOnly;
    LSVG.FixedColor := LItem.FixedColor;
  end
  else
  begin
    LSVG.ApplyFixedColorToRootOnly := FApplyFixedColorToRootOnly;
    LSVG.FixedColor := FFixedColor;
  end;
  if LItem.GrayScale or FGrayScale then
    LSVG.Grayscale := True
  else
    LSVG.Grayscale := False;
  LSVG.Opacity := 1;

  LSVG.PaintTo(ACanvas.Handle, TRectF.Create(ARect), AProportional);
end;

initialization

{$IF NOT DEFINED(CLR)}
  StartClassGroup(VCL.Controls.TControl);
  ActivateClassGroup(VCL.Controls.TControl);
  GroupDescendentsWith(TSVGIconImageCollection, VCL.Controls.TControl);
{$IFEND}


end.
