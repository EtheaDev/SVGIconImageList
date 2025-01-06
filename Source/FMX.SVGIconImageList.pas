{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/FMX                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
unit FMX.SVGIconImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes
  , System.UITypes
  , System.Rtti
  , System.Messaging
  , System.ImageList
  , System.Types
  , FMX.Controls
  , FMX.ImgList
  , FMX.MultiResBitmap
  , FMX.Types
  , FMX.Graphics
  , FMX.Objects
  , FMX.ImageSVG
  ;

const
  SVGIconImageListVersion = '4.4.1';
  DEFAULT_SIZE = 32;
  ZOOM_DEFAULT = 100;
  SVG_INHERIT_COLOR = TAlphaColors.Null;
  SVG_NONE_COLOR = TAlphaColors.Null;

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

type
  TSVGIconMultiResBitmap = class;
  TSVGIconImageList = class;
  TSVGIconSourceItem = class;

  TSVGIconBitmapItem = class(TCustomBitmapItem)
  private
    FWidth, FHeight, FZoom: Integer;
    FOwnerMultiResBitmap: TSVGIconMultiResBitmap;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetSize(const AValue: Integer);
    procedure DrawSVGIcon;
    function GetSVG: TFmxImageSVG;
    function GetGrayScale: Boolean;
    function GetFixedColor: TAlphaColor;
    function GetOpacity: single;
    function GetSize: Integer;
    procedure SetIconSize(const AWidth, AHeight, AZoom: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;
    procedure SetZoom(const AValue: Integer);
    function GetApplyFixedColorToRootOnly: Boolean;
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    property SVG: TFmxImageSVG read GetSVG;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Scale;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Zoom: Integer read FZoom write SetZoom default ZOOM_DEFAULT;
    //Readonly properties from Source Item
    property FixedColor: TAlphaColor read GetFixedColor stored false;
    property ApplyFixedColorToRootOnly: Boolean read GetApplyFixedColorToRootOnly stored false;
    property GrayScale: Boolean read GetGrayScale stored false;
    property Opacity: single read GetOpacity stored false;
  end;

  TSVGIconBitmapItemClass = class of TSVGIconBitmapItem;

  TSVGIconMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TSVGIconSourceItem;
    procedure UpdateImageSize(const AWidth, AHeight, AZoom: Integer);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TSVGIconBitmapItemClass); overload;
  public
  end;

  {TSVGIconSourceItem}
  TSVGIconSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TSVGIconImageList;
    FSVG: TFmxImageSVG;
    FOpacity: single;
    FFixedColor: TAlphaColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    procedure RefreshAllIcons;
    function GetSVGText: string;
    procedure SetFixedColor(const Value: TAlphaColor);
    procedure SetGrayScale(const Value: Boolean);
    function GetFixedColor: TAlphaColor;
    function GetGrayScale: Boolean;
    procedure SetSVG(const Value: TFmxImageSVG);
    procedure SetSVGText(const Value: string);
    procedure SetOpacity(const AValue: single);
    procedure AutoSizeBitmap(const AWidth, AHeight, AZoom: Integer);
    function GetIconName: string;
    procedure SetIconName(const Value: string);
    function GetOpacity: single;
    function GetDestinationItem: TCustomDestinationItem;
    procedure UpdateIconAttributes(const AFixedColor: TAlphaColor;
      const ApplyToRootOnly: Boolean;
      const AReplaceFixedColor: Boolean = False);
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    function CreateMultiResBitmap: TMultiResBitmap; override;
    function StoreOpacity: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property SVG: TFmxImageSVG read FSVG write SetSVG;
  published
    property MultiResBitmap;
    property IconName: string read GetIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
    property FixedColor: TAlphaColor read GetFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default false;
    property GrayScale: Boolean read GetGrayScale write SetGrayScale default False;
    property Opacity: single read GetOpacity write SetOpacity stored StoreOpacity;
  end;

  {TSVGIconImageList}
  {$IF CompilerVersion > 34}
  [ComponentPlatforms(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or pidiOSSimulator32 or pidiOSDevice32 or pidiOSDevice64 or pidAndroidArm32 or pidAndroidArm64)]
  {$ELSE}
  [ComponentPlatforms(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator32 or pidiOSDevice32 or pidAndroid32Arm)]
  {$ENDIF}
  TSVGIconImageList = class(TCustomImageList)
  private
    FWidth, FHeight: Integer;
    FAutoSizeBitmaps: Boolean;
    FFixedColor: TAlphaColor;
    FGrayScale: Boolean;
    FOpacity: single;
    FZoom: Integer;
    FApplyFixedColorToRootOnly: Boolean;
    function StoreOpacity: Boolean;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
    procedure SetFixedColor(const Value: TAlphaColor);
    procedure UpdateDestination(ASize: TSize; const Index: Integer);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetOpacity(const Value: single);
    procedure SetIconSize(const AWidth, AHeight: Integer);
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure SetZoom(const AValue: Integer);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
  protected
    procedure Loaded; override;
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DeleteIcon(const AIndex: Integer);
    function AddIcon(const ASVGText: string;
      const AIconName: string = ''): TSVGIconSourceItem;
    function InsertIcon(const AIndex: Integer;
      const ASVGText: string; const AIconName: string = ''): TSVGIconSourceItem;
    function CloneIcon(const AIndex: Integer; const AInsertIndex: Integer = -1): TSVGIconSourceItem;
    function GetIcon(const AIndex: Integer): TSVGIconSourceItem;
    function GetIconByName(const AName: string): TSVGIconSourceItem;
    function ExtractSVG(const AIndex: Integer): TFmxImageSVG;
    function ExtractSVGByName(const AName: string): TFmxImageSVG;
    //Multiple icons methods
    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;
    procedure ClearIcons; virtual;
    procedure RefreshAllIcons;
    procedure UpdateIconAttributes(const ASize: Integer; const AOpacity: Single); overload;
  published
    //Publishing properties of standard ImageList
    property Source;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Zoom: Integer read FZoom write SetZoom default ZOOM_DEFAULT;
    property Destination;
    property OnChange;
    property OnChanged;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default True;
    property FixedColor: TAlphaColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default false;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property Opacity: single read FOpacity write SetOpacity stored StoreOpacity;
  end;

procedure PaintToBitmap(const ABitmap: TBitmap; const ASVG: TFmxImageSVG;
  const AZoom: Integer = 100; const AKeepAspectRatio: Boolean = True);

implementation

uses
  System.Math
  , System.RTLConsts
  , System.SysUtils
  , FMX.Forms
  , FMX.Consts
  {$IFDEF Image32_SVGEngine}
  , FMX.Image32SVG
  {$ENDIF}
  {$IFDEF Skia_SVGEngine}
  , FMX.ImageSkiaSVG
  {$ENDIF}
  ;

procedure PaintToBitmap(const ABitmap: TBitmap; const ASVG: TFmxImageSVG;
  const AZoom: Integer = 100; const AKeepAspectRatio: Boolean = True);
var
  LRect: TRectF;
  LWidth, LHeight: Integer;
begin
  LWidth := ABitmap.Canvas.Width;
  LHeight := ABitmap.Canvas.Height;
  LRect := TRect.Create(0, 0, LWidth, LHeight);
  ASVG.PaintToBitmap(ABitmap, AZoom, AKeepAspectRatio);
end;

{ TSVGIconBitmapItem }

function TSVGIconBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TSVGIconBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  FWidth := DEFAULT_SIZE;
  FHeight := DEFAULT_SIZE;
  FZoom := ZOOM_DEFAULT;
  if Collection is TSVGIconMultiResBitmap then
    FOwnerMultiResBitmap := Collection as TSVGIconMultiResBitmap;
end;

procedure TSVGIconBitmapItem.DrawSVGIcon;
var
  LBitmap: TBitmap;
  LBitmapWidth, LBitmapHeight: Integer;
  LSVG: TFmxImageSVG;
begin
  LBitmap := inherited Bitmap;
  LBitmapWidth := Round(FWidth * Scale);
  LBitmapHeight := Round(FHeight * Scale);
  LBitmap.SetSize(LBitmapWidth, LBitmapHeight);
  LSVG := SVG;
  LSVG.Opacity := Opacity;
  LSVG.FixedColor := FixedColor;
  LSVG.Grayscale := GrayScale;
  PaintToBitmap(LBitmap, LSVG, FZoom);
end;

function TSVGIconBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawSVGIcon;
  Result := inherited Bitmap;
end;

function TSVGIconBitmapItem.GetDisplayName: string;
begin
  Result := Format('%s - %dx%d - Scale: %s',
    [FOwnerMultiResBitmap.FOwnerSourceItem.Name,
     Size, Size, FloatToStr(Scale)]);
end;

function TSVGIconBitmapItem.GetGrayScale: Boolean;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.GrayScale;
end;

function TSVGIconBitmapItem.GetFixedColor: TAlphaColor;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FixedColor;
end;

function TSVGIconBitmapItem.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.ApplyFixedColorToRootOnly;
end;

function TSVGIconBitmapItem.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconBitmapItem.GetOpacity: single;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Opacity;
end;

function TSVGIconBitmapItem.GetSize: Integer;
begin
  Result := Max(FWidth, FHeight);
  if Result = 0 then
    Result := DEFAULT_SIZE;
end;

function TSVGIconBitmapItem.GetSVG: TFmxImageSVG;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.SVG;
end;

function TSVGIconBitmapItem.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TSVGIconBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TSVGIconBitmapItem.SetHeight(const AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    DrawSVGIcon;
  end;
end;

procedure TSVGIconBitmapItem.SetWidth(const AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    DrawSVGIcon;
  end;
end;

procedure TSVGIconBitmapItem.SetZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
  begin
    FZoom := AValue;
    DrawSVGIcon;
  end;
end;

procedure TSVGIconBitmapItem.SetIconSize(const AWidth, AHeight, AZoom: Integer);
begin
  if (AWidth <> 0) and (AHeight <> 0) and
    ((AWidth <> FWidth) or (AHeight <> FHeight) or (AZoom <> FZoom)) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    FZoom := AZoom;
    DrawSVGIcon;
  end;
end;

procedure TSVGIconBitmapItem.SetSize(const AValue: Integer);
begin
  if ((AValue <> FHeight) or (AValue <> FWidth)) then
    SetIconSize(AValue, AValue, FZoom);
end;

function TSVGIconBitmapItem.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TSVGIconBitmapItem.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconBitmapItem.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
end;

{ TSVGIconMultiResBitmap }

constructor TSVGIconMultiResBitmap.Create(AOwner: TPersistent;
  ItemClass: TSVGIconBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if (AOwner is TSVGIconSourceItem) then
    FOwnerSourceItem := TSVGIconSourceItem(AOwner)
  else
    FOwnerSourceItem := nil;
end;

procedure TSVGIconMultiResBitmap.UpdateImageSize(const AWidth, AHeight, AZoom: Integer);
var
  I, J: Integer;
  LItem: TSVGIconBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J] as TSVGIconBitmapItem;
      if (LItem.FWidth <> AWidth) or (LItem.FHeight <> AHeight) then
      begin
        LItem.FWidth := AWidth;
        LItem.FHeight := AHeight;
        LItem.Zoom := AZoom;
        LItem.DrawSVGIcon;
      end;
    end;
  end;
end;

{ TSVGIconSourceItem }

procedure TSVGIconSourceItem.Assign(Source: TPersistent);
begin
  if Source is TSVGIconSourceItem then
  begin
    FOpacity := TSVGIconSourceItem(Source).FOpacity;
    FFixedColor := TSVGIconSourceItem(Source).FFixedColor;
    FGrayScale := TSVGIconSourceItem(Source).FGrayScale;
    FSVG.LoadFromText(TSVGIconSourceItem(Source).SVG.Source);
  end;
  inherited;
end;

procedure TSVGIconSourceItem.AutoSizeBitmap(const AWidth, AHeight, AZoom: Integer);
begin
  //If present, delete multiple items
  while MultiResBitmap.Count > 1 do
    MultiResBitmap.Delete(MultiResBitmap.Count-1);
  //Add only one item
  if MultiResBitmap.Count = 0 then
    MultiResBitmap.Add;
  (MultiResBitmap as TSVGIconMultiResBitmap).UpdateImageSize(AWidth, AHeight, AZoom);
end;

constructor TSVGIconSourceItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  {$IFDEF Image32_SVGEngine}
  FSVG := TFmxImage32SVG.Create;
  {$ENDIF}
  {$IFDEF Skia_SVGEngine}
  FSVG := TFmxImageSKIASVG.Create;
  {$ENDIF}
  FOpacity := -1;
  FixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
  RefreshAllIcons;
end;

function TSVGIconSourceItem.CreateMultiResBitmap: TMultiResBitmap;
begin
  Result := TSVGIconMultiResBitmap.Create(self, TSVGIconBitmapItem);
  FOwnerImageList := Result.ImageList as TSVGIconImageList;
end;

destructor TSVGIconSourceItem.Destroy;
begin
  FSVG.Free;
  inherited;
end;

function TSVGIconSourceItem.GetDisplayName: string;
begin
  Result := Format('%d.%s', [Index, Name])
end;

function TSVGIconSourceItem.GetFixedColor: TAlphaColor;
begin
  if FFixedColor = SVG_INHERIT_COLOR then
    Result := FOwnerImageList.FixedColor
  else
    Result := FFixedColor;
end;

function TSVGIconSourceItem.GetGrayScale: Boolean;
begin
  if not FGrayScale then
    Result := FOwnerImageList.FGrayScale
  else
    Result := FGrayScale;
end;

function TSVGIconSourceItem.GetIconName: string;
begin
  Result := inherited Name;
end;

function TSVGIconSourceItem.GetOpacity: single;
begin
  if FOpacity = -1 then
    Result := FOwnerImageList.FOpacity
  else
    Result := FOpacity;
end;

function TSVGIconSourceItem.GetSVGText: string;
begin
  Result := SVG.Source;
end;

function TSVGIconSourceItem.GetDestinationItem: TCustomDestinationItem;
var
  LDest: TCustomDestinationItem;
begin
  Result := nil;
  if FOwnerImageList.Destination.Count > Index then
  begin
    LDest := FOwnerImageList.Destination.Items[Index];
    if (LDest.LayersCount > 0) and
      SameText(LDest.Layers[0].Name, IconName) then
      Result := LDest;
  end;
end;

procedure TSVGIconSourceItem.SetApplyFixedColorToRootOnly(const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconSourceItem.SetFixedColor(const Value: TAlphaColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconSourceItem.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconSourceItem.SetIconName(const Value: string);
var
  LDest: TCustomDestinationItem;
begin
  if Value <> Name then
  begin
    LDest := GetDestinationItem;
    inherited Name := Value;
    if Assigned(LDest) then
      LDest.Layers[0].Name := Value;
  end;
end;

procedure TSVGIconSourceItem.SetOpacity(const AValue: single);
begin
  if Assigned(FOwnerImageList) and (AValue = FOwnerImageList.Opacity) then
  begin
    FOpacity := -1;
  end
  else
    FOpacity := AValue;
  RefreshAllIcons;
end;

procedure TSVGIconSourceItem.SetSVG(const Value: TFmxImageSVG);
begin
  if not SameText(FSVG.Source, Value.Source) then
  begin
    FSVG.LoadFromText(Value.Source);
    RefreshAllIcons;
  end;
end;

procedure TSVGIconSourceItem.SetSVGText(const Value: string);
begin
  FSVG.LoadFromText(Value);
  RefreshAllIcons;
end;

function TSVGIconSourceItem.StoreOpacity: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FOpacity <> FOwnerImageList.FOpacity);
end;

procedure TSVGIconSourceItem.UpdateIconAttributes(
  const AFixedColor: TAlphaColor; const ApplyToRootOnly: Boolean;
  const AReplaceFixedColor: Boolean = False);
begin
  //If AReplaceFontColor is false then the color of single icon is preserved
  if AReplaceFixedColor and (FFixedColor <> TAlphaColors.Null) then
    FFixedColor := AFixedColor;
  FApplyFixedColorToRootOnly := ApplyToRootOnly;
end;

procedure TSVGIconSourceItem.RefreshAllIcons;
var
  I: Integer;
  LItem: TSVGIconBitmapItem;
  LSize: TSize;
begin
  //Update all Source and Destination Items/Icons
  for I := 0 to MultiResBitmap.Count -1 do
  begin
    LItem := MultiResBitmap.Items[I] as TSVGIconBitmapItem;
    Litem.DrawSVGIcon;
    if (I=0) and (FOwnerImageList <> nil) then
    begin
      LItem.SetIconSize(FOwnerImageList.Width, FOwnerImageList.Height, FOwnerImageList.Zoom);
      LSize.cx := LItem.Width;
      LSize.cy := LItem.Height;
      FOwnerImageList.UpdateDestination(LSize, Index);
    end;
  end;
end;

{ TSVGIconImageList }

function TSVGIconImageList.InsertIcon(const AIndex: Integer;
  const ASVGText: string; const AIconName: string = ''): TSVGIconSourceItem;
var
  LItem: TSVGIconSourceItem;
  LDest: TCustomDestinationItem;
begin
  LItem := Self.Source.Insert(AIndex) as TSVGIconSourceItem;
  Result := LItem;
  LItem.MultiResBitmap.Add;
  LItem.SVGText := ASVGText;
  LDest := Self.Destination.Insert(AIndex);
  if AIconName <> '' then
  begin
    LItem.Name := AIconName;
    with LDest.Layers.Add do
      Name := AIconName;
  end;
end;

function TSVGIconImageList.CloneIcon(const AIndex: Integer; const AInsertIndex: Integer = -1): TSVGIconSourceItem;
var
  LItem: TSVGIconSourceItem;
  LNewIndex: Integer;
begin
  LItem := Self.GetIcon(AIndex);

  if AInsertIndex >= 0 then LNewIndex := AInsertIndex
  else LNewIndex := AIndex;

  Result := InsertIcon(LNewIndex, LItem.SVGText);
  Result.Opacity := LItem.Opacity;
  Result.FixedColor := LItem.FixedColor;
  Result.GrayScale := LItem.GrayScale;
  Result.SVG.LoadFromText(LItem.SVG.Source);
  RefreshAllIcons;
end;

function TSVGIconImageList.GetIcon(const AIndex: Integer): TSVGIconSourceItem;
begin
  Result := Self.Source.Items[AIndex] as TSVGIconSourceItem;
end;

function TSVGIconImageList.GetIconByName(const AName: string): TSVGIconSourceItem;
var
  LItemIndex: Integer;
begin
  LItemIndex := Self.Source.IndexOf(AName);
  if LItemIndex >= 0 then
    Result := Self.GetIcon(LItemIndex)
  else
    Result := nil;
end;

function TSVGIconImageList.ExtractSVG(const AIndex: Integer): TFmxImageSVG;
var
  LItem: TSVGIconSourceItem;
begin
  LItem := Self.GetIcon(AIndex);

  if Assigned(LItem) then
  begin
    {$IFDEF Image32_SVGEngine}
    Result := TFmxImage32SVG.Create;
    {$ENDIF}
    {$IFDEF Skia_SVGEngine}
    Result := TFmxImageSKIASVG.Create;
    {$ENDIF}

    Result.LoadFromText(LItem.SVG.Source);
    Result.Opacity := LItem.Opacity;
    Result.FixedColor := LItem.FixedColor;
    Result.GrayScale := LItem.GrayScale;
  end
  else
    Result := nil;
end;

function TSVGIconImageList.ExtractSVGByName(const AName: string): TFmxImageSVG;
var
  LItemIndex: Integer;
begin
  LItemIndex := Self.Source.IndexOf(AName);
  if LItemIndex >= 0 then
    Result := Self.ExtractSVG(LItemIndex)
  else
    Result := nil;
end;

function TSVGIconImageList.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean = True): Integer;
var
  LIndex: Integer;
  LSVG: TFmxImageSVG;
  LIconName, LFileName: string;
  LItem: TSVGIconSourceItem;
  LErrors: string;
begin
  Result := 0;
  {$IFDEF Image32_SVGEngine}
  LSVG := TFmxImage32SVG.Create;
  {$ENDIF}
  {$IFDEF Skia_SVGEngine}
  LSVG := TFmxImageSKIASVG.Create;
  {$ENDIF}
  try
    if not AAppend then
      ClearIcons;
    for LIndex := 0 to AFileNames.Count - 1 do
    begin
      LFileName := AFileNames[LIndex];
      LSVG.LoadFromFile(LFileName);
      LIconName := ChangeFileExt(ExtractFileName(LFileName), '');
      try
        LItem := InsertIcon(Source.Count, LSVG.Source, LIconName);
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
    LSVG.Free;
  end;
end;

function TSVGIconImageList.AddIcon(const ASVGText,
  AIconName: string): TSVGIconSourceItem;
begin
  if Count = 0 then
    Result := InsertIcon(0, ASVGText, AIconName)
  else
    Result := InsertIcon(Source.Count, ASVGText, AIconName);
end;

procedure TSVGIconImageList.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageList then
  begin
    Opacity := TSVGIconImageList(Source).Opacity;
    FFixedColor := TSVGIconImageList(Source).FFixedColor;
    FGrayScale := TSVGIconImageList(Source).FGrayScale;
    FAutoSizeBitmaps := TSVGIconImageList(Source).FAutoSizeBitmaps;
    Zoom := TSVGIconImageList(Source).FZoom;
    SetIconSize(TSVGIconImageList(Source).FWidth,
      TSVGIconImageList(Source).FHeight);
  end;
  inherited;
end;

procedure TSVGIconImageList.ClearIcons;
begin
  Source.Clear;
  Destination.Clear;
end;

constructor TSVGIconImageList.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizeBitmaps := True;
  FixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
  FOpacity := 1;
  FWidth := DEFAULT_SIZE;
  FHeight := DEFAULT_SIZE;
  FZoom := ZOOM_DEFAULT;
end;

function TSVGIconImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TSVGIconSourceItem);
end;

procedure TSVGIconImageList.UpdateDestination(ASize: TSize;
  const Index: Integer);
var
  LDestItem: TDestinationItem;
  LSourceItem: TSVGIconSourceItem;
  LIndex: Integer;
  LWidth, LHeight: Integer;
begin
  while Index > Destination.Count-1 do
    Destination.Add;
  LDestItem := Destination.Items[Index] as TDestinationItem;
  if LDestItem.Layers.Count = 0 then
  begin
    LSourceItem := Source.Items[Index] as TSVGIconSourceItem;
    with LDestItem.Layers.Add do
      Name := LSourceItem.IconName;
  end;
  if LDestItem.Layers.Count > 0 then
  begin
    LIndex := Source.indexOf(LDestItem.Layers[0].Name);
    if LIndex >= 0 then
    begin
      LSourceItem := Source.Items[LIndex] as TSVGIconSourceItem;
      if Assigned(LSourceItem) then
      begin
        if FAutoSizeBitmaps then
        begin
          if FWidth = FHeight then
          begin
            LWidth := Min(ASize.cy, ASize.cx);
            LHeight := LWidth;
          end
          else if FWidth > FHeight then
          begin
            LWidth := Min(ASize.cy, ASize.cx);
            LHeight := Round((FHeight / FWidth) * ASize.cy);
          end
          else
          begin
            LHeight := ASize.cy;
            LWidth := Round((FWidth / FHeight) * ASize.cx);
          end;
          LSourceItem.AutoSizeBitmap(LWidth, LHeight, FZoom);
        end
        else
        begin
          LWidth := LSourceItem.FOwnerImageList.FWidth;
          LHeight := LSourceItem.FOwnerImageList.FHeight;
        end;
        LDestItem.Layers[0].SourceRect.Top := 0;
        LDestItem.Layers[0].SourceRect.Left := 0;
        LDestItem.Layers[0].SourceRect.Right := LWidth;
        LDestItem.Layers[0].SourceRect.Bottom := LHeight;
      end;
    end;
  end;
end;

procedure TSVGIconImageList.UpdateIconAttributes(const ASize: Integer;
  const AOpacity: Single);
var
  I: Integer;
  LSVGIconItem: TSVGIconSourceItem;
begin
  Self.Size := ASize;
  for I := 0 to Source.Count -1 do
  begin
    LSVGIconItem := Source.Items[I] as TSVGIconSourceItem;
    LSVGIconItem.UpdateIconAttributes(FixedColor, ApplyFixedColorToRootOnly);
  end;
end;

procedure TSVGIconImageList.DeleteIcon(const AIndex: Integer);
var
  LDest: TCustomDestinationItem;
  LSourceItem: TSVGIconSourceItem;
begin
  LSourceItem := Source.Items[AIndex] as TSVGIconSourceItem;
  if Assigned(LSourceItem) then
  begin
    LDest := LSourceItem.GetDestinationItem;
    Source.Delete(AIndex);
    if Assigned(LDest) then
      Destination.Delete(AIndex);
  end;
end;

function TSVGIconImageList.DoBitmap(Size: TSize;
  const Index: Integer): TBitmap;
begin
  UpdateDestination(Size, Index);
  Result := inherited DoBitmap(Size, Index);
end;

function TSVGIconImageList.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconImageList.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconImageList.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TSVGIconImageList.GetSize: Integer;
begin
  Result := Max(FWidth, FHeight);
end;

function TSVGIconImageList.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TSVGIconImageList.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TSVGIconImageList.Loaded;
begin
  inherited;
  RefreshAllIcons;
end;

procedure TSVGIconImageList.SetAutoSizeBitmaps(const Value: Boolean);
begin
  FAutoSizeBitmaps := Value;
  if (Count > 0) then
    RefreshAllIcons;
end;

procedure TSVGIconImageList.RefreshAllIcons;
var
  I: Integer;
  LSourceItem: TSVGIconSourceItem;
begin
  //Delete destination items more than source items
  while Destination.Count > Source.Count do
    Destination.Delete(Destination.Count-1);
  //Update all Source and Destination Items/Icons
  for I := 0 to Source.Count -1 do
  begin
    LSourceItem := Source[I] as TSVGIconSourceItem;
    if LSourceItem.FOpacity = -1 then
      LSourceItem.Opacity := FOpacity;
    if not LSourceItem.GrayScale then
      LSourceItem.GrayScale := FGrayScale;
    if LSourceItem.FixedColor = SVG_INHERIT_COLOR then
      LSourceItem.FixedColor := FFixedColor;
    LSourceItem.RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetFixedColor(const Value: TAlphaColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetApplyFixedColorToRootOnly(const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetHeight(const AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetIconSize(const AWidth, AHeight: Integer);
begin
  if (AWidth <> 0) and (AHeight <> 0) and
    ((AWidth <> FWidth) or (AHeight <> FHeight)) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RefreshAllIcons;
  end;
end;

function TSVGIconImageList.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

procedure TSVGIconImageList.SetSize(const AValue: Integer);
begin
  if ((AValue <> FHeight) or (AValue <> FWidth)) then
    SetIconSize(AValue, AValue);
end;

procedure TSVGIconImageList.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    RefreshAllIcons;
  end;
end;

procedure TSVGIconImageList.SetZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
  begin
    FZoom := AValue;
    RefreshAllIcons;
  end;
end;

initialization
  RegisterFmxClasses([TSVGIconImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.SVGIconImageList.TSVGIconImageList, TFmxObject);

end.
