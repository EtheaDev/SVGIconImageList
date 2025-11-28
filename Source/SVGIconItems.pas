{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
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
///   Collection classes for managing SVG icon items.
///   Contains TSVGIconItem (individual icon) and TSVGIconItems (collection).
/// </summary>
unit SVGIconItems;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Types,
  System.UITypes,
  System.Classes,
  {$IFDEF DXE4+}System.Messaging,{$ELSE}SVGMessaging,{$ENDIF}
  Winapi.Windows,
  Vcl.Graphics,
  SVGInterfaces;

type
  TSVGIconItems = class;

  /// <summary>
  ///   Message sent when SVG icon items are updated.
  ///   Used internally to notify image lists of changes to the icon collection.
  /// </summary>
  TSVGItemsUpdateMessage = class({$IFDEF DXE4+}System.Messaging.{$ELSE}SVGMessaging.{$ENDIF}TMessage)
  end;

  /// <summary>
  ///   Represents a single SVG icon item within a TSVGIconItems collection.
  ///   Each item contains the SVG source and rendering attributes.
  /// </summary>
  /// <remarks>
  ///   <para>TSVGIconItem stores:</para>
  ///   <list type="bullet">
  ///     <item>The SVG source (via SVG interface or SVGText property)</item>
  ///     <item>Icon name with optional category (format: "Category\Name")</item>
  ///     <item>Per-item rendering attributes (FixedColor, GrayScale, etc.)</item>
  ///   </list>
  ///   <para>Per-item attributes override the parent image list's attributes
  ///   when they are set to non-default values.</para>
  /// </remarks>
  TSVGIconItem = class(TCollectionItem)
  private
    FIconName: string;
    FSVG: ISVG;
    FFixedColor: TColor;
    FApplyFixedColorToRootOnly: Boolean;
    FAntiAliasColor: TColor;
    FGrayScale: Boolean;
    procedure SetIconName(const Value: string);
    procedure SetSVG(const Value: ISVG);
    procedure SetSVGText(const Value: string);
    function GetSVGText: string;
    procedure SetFixedColor(const Value: TColor);
    procedure SetAntiAliasColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    function GetCategory: string;
    function GetName: string;
    procedure SetCategory(const Value: string);
    procedure SetName(const Value: string);
    function ExtractCategory(const S: String): String;
    function ExtractName(const S: String): String;
    procedure BuildIconName(const ACategory, AName: String);
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
  public
    /// <summary>
    ///   Applies rendering attributes to the internal ISVG interface.
    /// </summary>
    /// <param name="AFixedColor">
    ///   The fixed color to apply. Overridden by item's FixedColor if set.
    /// </param>
    /// <param name="AApplyToRootOnly">
    ///   Whether to apply color only to root element.
    /// </param>
    /// <param name="AOpacity">
    ///   Opacity value from 0 to 255.
    /// </param>
    /// <param name="AGrayScale">
    ///   Whether to render in grayscale.
    /// </param>
    procedure ApplyAttributesToInterface(const AFixedColor: TColor;
      const AApplyToRootOnly: Boolean; const AOpacity: Byte; const AGrayScale: Boolean);

    /// <summary>
    ///   Copies all properties from another TSVGIconItem.
    /// </summary>
    /// <param name="Source">
    ///   The source item to copy from.
    /// </param>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Returns the display name for this item in the collection editor.
    /// </summary>
    /// <returns>
    ///   The IconName if set, otherwise "SVGIconItem".
    /// </returns>
    function GetDisplayName: string; override;

    /// <summary>
    ///   Renders the SVG to a bitmap with the specified attributes.
    /// </summary>
    /// <param name="AWidth">
    ///   The width of the resulting bitmap in pixels.
    /// </param>
    /// <param name="AHeight">
    ///   The height of the resulting bitmap in pixels.
    /// </param>
    /// <param name="AFixedColor">
    ///   The fixed color to apply. Item's FixedColor takes precedence if set.
    /// </param>
    /// <param name="AApplyToRootOnly">
    ///   Whether to apply color only to root element.
    /// </param>
    /// <param name="AOpacity">
    ///   Opacity value from 0 to 255.
    /// </param>
    /// <param name="AGrayScale">
    ///   Whether to render in grayscale.
    /// </param>
    /// <param name="AAntiAliasColor">
    ///   Background color for anti-aliasing. Default is clBtnFace.
    /// </param>
    /// <returns>
    ///   A new TBitmap containing the rendered SVG. Caller must free this bitmap.
    /// </returns>
    function GetBitmap(const AWidth, AHeight: Integer;
      const AFixedColor: TColor; const AApplyToRootOnly: Boolean; const AOpacity: Byte;
      const AGrayScale: Boolean; const AAntiAliasColor: TColor = clBtnFace): TBitmap;

    /// <summary>
    ///   Creates a new SVG icon item.
    /// </summary>
    /// <param name="Collection">
    ///   The collection that owns this item.
    /// </param>
    constructor Create(Collection: TCollection); override;

    /// <summary>
    ///   Direct access to the ISVG interface for this icon.
    /// </summary>
    property SVG: ISVG read FSVG write SetSVG;

    /// <summary>
    ///   The name part of the icon (without category prefix).
    /// </summary>
    /// <remarks>
    ///   For IconName "MyCategory\MyIcon", Name returns "MyIcon".
    /// </remarks>
    property Name: string read GetName write SetName;

    /// <summary>
    ///   The category part of the icon name.
    /// </summary>
    /// <remarks>
    ///   For IconName "MyCategory\MyIcon", Category returns "MyCategory".
    ///   Returns empty string if no category is set.
    /// </remarks>
    property Category: string read GetCategory write SetCategory;
  published
    /// <summary>
    ///   The full icon name including optional category prefix.
    /// </summary>
    /// <remarks>
    ///   Format: "Category\Name" or just "Name" if no category.
    ///   The backslash (\) character separates category from name.
    /// </remarks>
    property IconName: string read FIconName write SetIconName;

    /// <summary>
    ///   The SVG source code as a string.
    /// </summary>
    /// <remarks>
    ///   Setting this property parses and loads the SVG content.
    ///   Getting returns the current SVG source.
    /// </remarks>
    property SVGText: string read GetSVGText write SetSVGText;

    /// <summary>
    ///   A fixed color specific to this icon item.
    /// </summary>
    /// <value>
    ///   Set to SVG_INHERIT_COLOR to inherit from parent image list.
    ///   Default is SVG_INHERIT_COLOR.
    /// </value>
    /// <remarks>
    ///   When set to a color other than SVG_INHERIT_COLOR, this overrides
    ///   the parent image list's FixedColor property for this specific icon.
    /// </remarks>
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;

    /// <summary>
    ///   When True, applies FixedColor only to the root SVG element.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default false;

    /// <summary>
    ///   Background color for anti-aliasing specific to this icon item.
    /// </summary>
    /// <value>
    ///   Default is clBtnFace.
    /// </value>
    property AntiAliasColor: TColor read FAntiAliasColor write SetAntiAliasColor default clBtnFace;

    /// <summary>
    ///   Renders this specific icon in grayscale when True.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

  /// <summary>
  ///   A collection of SVG icon items (TSVGIconItem).
  ///   Used by TSVGIconImageList and TSVGIconImageCollection to store SVG icons.
  /// </summary>
  /// <remarks>
  ///   TSVGIconItems extends TOwnedCollection to provide SVG-specific functionality
  ///   including loading from files, searching by name, and notifying parent
  ///   components of changes.
  /// </remarks>
  TSVGIconItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSVGIconItem;
    procedure SetItem(Index: Integer; const Value: TSVGIconItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>
    ///   Creates a new SVG icon items collection.
    /// </summary>
    /// <param name="AOwner">
    ///   The component that owns this collection.
    /// </param>
    constructor Create(AOwner: TComponent);

    /// <summary>
    ///   Adds a new empty SVG icon item to the collection.
    /// </summary>
    /// <returns>
    ///   The newly created TSVGIconItem.
    /// </returns>
    function Add: TSVGIconItem;

    /// <summary>
    ///   Copies all items from another TSVGIconItems collection.
    /// </summary>
    /// <param name="Source">
    ///   The source collection to copy from.
    /// </param>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Finds an icon item by its name.
    /// </summary>
    /// <param name="AIconName">
    ///   The icon name to search for (case-insensitive).
    /// </param>
    /// <returns>
    ///   The matching TSVGIconItem, or nil if not found.
    /// </returns>
    function GetIconByName(const AIconName: string): TSVGIconItem;

    /// <summary>
    ///   Loads an SVG file and adds it to the collection.
    /// </summary>
    /// <param name="AFileName">
    ///   The full path to the SVG file.
    /// </param>
    /// <param name="AImageName">
    ///   Output parameter receiving the name assigned to the icon
    ///   (filename without extension).
    /// </param>
    /// <returns>
    ///   The newly created TSVGIconItem, or nil if file doesn't exist.
    /// </returns>
    function LoadFromFile(const AFileName: string;
      out AImageName: string): TSVGIconItem;

    /// <summary>
    ///   Loads multiple SVG files and adds them to the collection.
    /// </summary>
    /// <param name="AFileNames">
    ///   A TStrings containing the full paths to SVG files.
    /// </param>
    /// <param name="AAppend">
    ///   When True, appends to existing items. When False, clears collection first.
    ///   Default is True.
    /// </param>
    /// <returns>
    ///   The number of icons successfully loaded.
    /// </returns>
    /// <exception cref="Exception">
    ///   Raised if any files fail to load, with details of all errors.
    /// </exception>
    function LoadFromFiles(const AFileNames: TStrings; const AAppend: Boolean = True): Integer;

    /// <summary>
    ///   Provides indexed access to items in the collection.
    /// </summary>
    /// <param name="Index">
    ///   The zero-based index of the item.
    /// </param>
    property Items[Index: Integer]: TSVGIconItem read GetItem write SetItem; default;
  end;

implementation

uses
  {$IFDEF IgnoreAntiAliasedColor}
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  {$ENDIF}
  SVGIconUtils,
  System.SysUtils,
  VCL.Controls,
  VCL.Themes,
{$IFDEF D10_3+}
  Vcl.BaseImageCollection,
{$ENDIF}
  SVGIconImageList,
  SVGIconImageCollection;

const
  CATEGORY_SEP = '\';

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

{ TSVGIconItem }

procedure TSVGIconItem.Assign(Source: TPersistent);
begin
  if Source is TSVGIconItem then
  begin
    FIconName := TSVGIconItem(Source).FIconName;
    FFixedColor := TSVGIconItem(Source).FFixedColor;
    FApplyFixedColorToRootOnly := TSVGIconItem(Source).FApplyFixedColorToRootOnly;
    FAntiAliasColor := TSVGIconItem(Source).FAntiAliasColor;
    FGrayScale := TSVGIconItem(Source).FGrayScale;
    FSVG.Source :=  TSVGIconItem(Source).FSVG.Source;
  end else
    inherited;
end;

constructor TSVGIconItem.Create(Collection: TCollection);
begin
  FSVG := GlobalSVGFactory.NewSvg;
  inherited Create(Collection);
  FFixedColor := SVG_INHERIT_COLOR;
  FApplyFixedColorToRootOnly := False;
  FAntiAliasColor := clBtnFace;
end;

function TSVGIconItem.GetDisplayName: string;
begin
  if IconName <> '' then
    Result := Format('%s',[IconName])
  else
    Result := 'SVGIconItem';
end;

function TSVGIconItem.GetName: string;
begin
  Result := ExtractName(FIconName);
end;

procedure TSVGIconItem.ApplyAttributesToInterface(const AFixedColor: TColor;
  const AApplyToRootOnly: Boolean; const AOpacity: Byte; const AGrayScale: Boolean);
begin
  if FFixedColor <> SVG_INHERIT_COLOR then
  begin
    FSVG.FixedColor := FFixedColor;
    FSVG.ApplyFixedColorToRootOnly := FApplyFixedColorToRootOnly;
  end
  else
  begin
    FSVG.FixedColor := AFixedColor;
    FSVG.ApplyFixedColorToRootOnly := AApplyToRootOnly;
  end;

  if FGrayScale or AGrayScale then
    FSVG.Grayscale := True
  else
    FSVG.Grayscale := False;
  FSVG.Opacity := AOpacity / 255;
end;

function TSVGIconItem.GetBitmap(const AWidth, AHeight: Integer;
  const AFixedColor: TColor; const AApplyToRootOnly: Boolean; const AOpacity: Byte;
  const AGrayScale: Boolean; const AAntiAliasColor: TColor = clBtnFace): TBitmap;
var
  LAntiAliasColor: TColor;
begin
  ApplyAttributesToInterface(AFixedColor, AApplyToRootOnly,
    AOpacity, AGrayScale);

  if FAntiAliasColor <> clBtnFace then
    LAntiAliasColor := FAntiAliasColor
  else
    LAntiAliasColor := AAntiAliasColor;

  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  if TStyleManager.IsCustomStyleActive then
    Result.Canvas.Brush.Color := ColorToRGB(StyleServices.GetSystemColor(LAntiAliasColor))
  else
    Result.Canvas.Brush.Color := ColorToRGB(LAntiAliasColor);
  Result.SetSize(AWidth, AHeight);
  FSVG.PaintTo(Result.Canvas.Handle, TRectF.Create(0, 0, AWidth, AHeight));
end;

function TSVGIconItem.GetCategory: string;
begin
  Result := ExtractCategory(FIconName);
end;

function TSVGIconItem.GetSVGText: string;
begin
  Result := SVG.Source;
end;

procedure TSVGIconItem.SetCategory(const Value: string);
begin
  BuildIconName(Value, Name);
end;

procedure TSVGIconItem.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> SVG_INHERIT_COLOR then
      FGrayScale := False;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FAntiAliasColor := Value;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetApplyFixedColorToRootOnly(const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := SVG_INHERIT_COLOR;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetIconName(const Value: string);
begin
  if FIconName <> Value then
  begin
    FIconName := Value;
    Changed(False);
  end;
end;

function TSVGIconItem.ExtractName(const S: String): String;
begin
  Result := S.SubString(S.IndexOf(CATEGORY_SEP) + 1);
end;

function TSVGIconItem.ExtractCategory(const S: String): String;
begin
  Result := S.Substring(0, S.IndexOf(CATEGORY_SEP));
end;

procedure TSVGIconItem.BuildIconName(const ACategory, AName: String);
begin
  if ACategory <> '' then
    IconName := ACategory + CATEGORY_SEP + AName
  else
    IconName := AName;
end;

procedure TSVGIconItem.SetName(const Value: string);
begin
  BuildIconName(Category, Value);
end;

procedure TSVGIconItem.SetSVG(const Value: ISVG);
begin
  if FSVG <> Value then
  begin
    FSVG := Value;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetSVGText(const Value: string);
begin
  if FSVG.Source <> Value then
  begin
    FSVG.Source := Value;
    Changed(False);
  end;
end;

{ TSVGIconItems }

function TSVGIconItems.Add: TSVGIconItem;
begin
  Result := TSVGIconItem(inherited Add);
end;

procedure TSVGIconItems.Assign(Source: TPersistent);
var
  C: Integer;
  Item: TSVGIconItem;
begin
  if (Source is TSVGIconItems) and (Owner <> nil) then
  begin
    BeginUpdate;
    try
      Clear;
      for C := 0 to TSVGIconItems(Source).Count - 1 do
      begin
        Item := Add;
        Item.Assign(TSVGIconItems(Source)[C]);
      end;
    finally
      EndUpdate;
    end;
  end else
    inherited;
end;

constructor TSVGIconItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TSVGIconItem);
end;

function TSVGIconItems.GetIconByName(const AIconName: string): TSVGIconItem;
var
  I: Integer;
  LSVGIconItem: TSVGIconItem;
begin
  Result := nil;
  for I := 0 to Count -1 do
  begin
    LSVGIconItem := Items[I];
    if SameText(LSVGIconItem.IconName, AIconName) then
    begin
      Result := LSVGIconItem;
      Break;
    end;
  end;
end;

function TSVGIconItems.GetItem(
  Index: Integer): TSVGIconItem;
begin
  Result := TSVGIconItem(inherited GetItem(Index));
end;

function TSVGIconItems.LoadFromFile(const AFileName: string;
  out AImageName: string): TSVGIconItem;
var
  LSVG: ISVG;
begin
  Result := nil;
  if FileExists(AFileName) then
  begin
    LSVG := GlobalSVGFactory.NewSvg;
    LSVG.LoadFromFile(AFileName);
    Result := Add;
    Result.IconName := ChangeFileExt(ExtractFileName(AFileName), '');
    Result.SVG := LSVG;
    AImageName := Result.Name;
  end;
end;

function TSVGIconItems.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean): Integer;
var
  LIndex: Integer;
  LSVG: ISVG;
  LFileName: string;
  LItem: TSVGIconItem;
  LErrors: string;
begin
  Result := 0;
  BeginUpdate;
  try
    LErrors := '';
    if not AAppend then
      Clear;
    for LIndex := 0 to AFileNames.Count - 1 do
    begin
      LFileName := AFileNames[LIndex];
      try
        LSVG := GlobalSVGFactory.NewSvg;
        LSVG.LoadFromFile(LFileName);
        LItem := Add;
        LItem.IconName := ChangeFileExt(ExtractFileName(LFileName), '');
        LItem.SVG := LSVG;
        Inc(Result);
      except
        on E: Exception do
          LErrors := LErrors + Format('%s (%s)',[E.Message, LFileName]) + sLineBreak;
      end;
    end;
    if LErrors <> '' then
      raise Exception.Create(ERROR_LOADING_FILES+sLineBreak+LErrors);
  finally
    EndUpdate;
  end;
end;

procedure TSVGIconItems.SetItem(Index: Integer;
  const Value: TSVGIconItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSVGIconItems.Update(Item: TCollectionItem);
begin
  inherited;
  {$IFDEF DXE4+}System.Messaging{$ELSE}SVGMessaging{$ENDIF}.TMessageManager.DefaultManager.SendMessage(Self,
    TSVGItemsUpdateMessage.Create);

  {$IFDEF D10_3+}
  if Owner is TSVGIconImageCollection then
  begin
    if Item = nil then
      TSVGIconImageCollection(Owner).Change
    else
      System.Messaging.TMessageManager.DefaultManager.SendMessage(nil,
        TImageCollectionChangedMessage.Create(TSVGIconImageCollection(Owner),
          Item.Index, TSVGIconItem(Item).IconName
          {$IFDEF D11+}, TSVGIconItem(Item).IconName{$ENDIF}));
  end;
  {$ENDIF}
end;

end.
