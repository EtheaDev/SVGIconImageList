{******************************************************************************}
{                                                                              }
{       SVG Image in TPicture: useful to show a Scalable Vector Graphic        }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{       Original version (c) 2005, 2008 Martin Walter with license:            }
{       Use of this file is permitted for commercial and non-commercial        }
{       use, as long as the author is credited.                                }
{       home page: http://www.mwcs.de                                          }
{       email    : martin.walter@mwcs.de                                       }
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
unit FMX.SVGIconImage;

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
  , SVG
  ;

const
  DEFAULT_SIZE = 32;
  ZOOM_DEFAULT = 100;

type
  TSVGIconFixedMultiResBitmap = class;

  TSVGIconFixedBitmapItem = class(TFixedBitmapItem)
  private
    FWidth, FHeight: Single;
    FZoom: Integer;
    FOpacity: Single;
    FOwnerCollection: TSVGIconFixedMultiResBitmap;
    FIconName: string;
    FSVG: TSVG;
    function StoreOpacity: Boolean;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetIconSize(const AWidth, AHeight: Single;
      const AZoom: Integer);
    procedure DrawSVGIcon;
    procedure SetOpacity(const AValue: Single);
    procedure SetIconName(const AValue: string);
    function GetSVGText: string;
    procedure SetSVGText(const Value: string);
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    property SVG: TSVG read FSVG;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Opacity: Single read FOpacity write SetOpacity stored StoreOpacity;
    property IconName: string read FIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

  TSVGIconFixedBitmapItemClass = class of TSVGIconFixedBitmapItem;
  TSVGIconImage = class;

  TSVGIconFixedMultiResBitmap = class(TFixedMultiResBitmap)
  private
    FOwnerImage: TSVGIconImage;
    procedure OnDrawImage(Sender: TObject);
    procedure UpdateImageSize(const AWidth, AHeight: Single;
      const AZoom: Integer);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TSVGIconFixedBitmapItemClass); overload;
    constructor Create(AOwner: TPersistent); overload;
  end;

  TSVGIconImage = class(TImage)
  private
    FZoom: Integer;
    FSVGIconMultiResBitmap: TSVGIconFixedMultiResBitmap;
    procedure SetBitmapZoom(const AValue: Integer);
    procedure SetIconSize(const AWidth, AHeight: Single;
      const AZoom: Integer);
  protected
    function CreateMultiResBitmap: TFixedMultiResBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  published
    property BitmapZoom: Integer read FZoom write SetBitmapZoom default ZOOM_DEFAULT;
  end;

implementation

uses
  System.Math
  , FMX.SVGIconImageList
  , System.RTLConsts
  , System.SysUtils
  , System.Character
  , FMX.Forms
  , FMX.Consts
  , GDIPOBJ2;

{ TSVGIconFixedMultiResBitmap }

constructor TSVGIconFixedMultiResBitmap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSVGIconFixedBitmapItem);
  if AOwner is TSVGIconImage then
    FOwnerImage := TSVGIconImage(AOwner);
end;

procedure TSVGIconFixedMultiResBitmap.OnDrawImage(Sender: TObject);
begin
  if Assigned(FOwnerImage) then
    FOwnerImage.Repaint;
end;

constructor TSVGIconFixedMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TSVGIconFixedBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if AOwner is TSVGIconImage then
    FOwnerImage := TSVGIconImage(AOwner);
end;

procedure TSVGIconFixedMultiResBitmap.UpdateImageSize(const AWidth, AHeight: Single;
  const AZoom: Integer);
var
  I, J: Integer;
  LItem: TFixedBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J];
      if LItem is TSVGIconFixedBitmapItem then
        TSVGIconFixedBitmapItem(LItem).SetIconSize(AWidth, AHeight, AZoom);
    end;
  end;
end;

{ TSVGIconFixedBitmapItem }

procedure TSVGIconFixedBitmapItem.Assign(Source: TPersistent);
begin
  if Source is TSVGIconSourceItem then
  begin
    Opacity := TSVGIconSourceItem(Source).Opacity;
    IconName := TSVGIconSourceItem(Source).IconName;
    SVGText := TSVGIconSourceItem(Source).SVGText;
  end
  else
    inherited;
end;

function TSVGIconFixedBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TSVGIconFixedBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TSVGIconFixedMultiResBitmap then
    FOwnerCollection := Collection as TSVGIconFixedMultiResBitmap;
  FZoom := ZOOM_DEFAULT;
  FOpacity := 1;
  FSVG := TSVG.Create;
end;

destructor TSVGIconFixedBitmapItem.Destroy;
begin
  FSVG.DisposeOf;
  inherited;
end;

procedure TSVGIconFixedBitmapItem.DrawSVGIcon;
var
  LBitmap: TBitmap;
  LBitmapWidth, LBitmapHeight: Integer;
begin
  if (FWidth <= 0) or (FHeight <= 0) or (FZoom <= 0) or (FZoom >= 100) then
    Exit;
  LBitmap := inherited Bitmap;
  LBitmapWidth := Round(FWidth * Scale);
  LBitmapHeight := Round(FHeight * Scale);
  LBitmap.Width  := LBitmapWidth;
  LBitmap.Height := LBitmapHeight;
  LBitmap.Canvas.BeginScene;
  try
    LBitmap.Clear(talphacolors.Null);
    PaintToBitmap(LBitmap, FSVG, FZoom);
  finally
    LBitmap.Canvas.EndScene;
  if Assigned(FOwnerCollection) then
    FOwnerCollection.OnDrawImage(Self);
  end;
end;

function TSVGIconFixedBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawSVGIcon;
  Result := inherited Bitmap;
end;

function TSVGIconFixedBitmapItem.GetDisplayName: string;
begin
  Result := FIconName;
end;

function TSVGIconFixedBitmapItem.GetSVGText: string;
begin
  Result := FSVG.Source;
end;

procedure TSVGIconFixedBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TSVGIconFixedBitmapItem.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;


procedure TSVGIconFixedBitmapItem.SetSVGText(const Value: string);
begin
  FSVG.LoadFromText(Value);
end;

procedure TSVGIconFixedBitmapItem.SetIconSize(const AWidth, AHeight:Single;
  const AZoom: Integer);
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

procedure TSVGIconFixedBitmapItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  DrawSVGIcon;
end;

function TSVGIconFixedBitmapItem.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

{ TSVGIconImage }

constructor TSVGIconImage.Create(AOwner: TComponent);
begin
  inherited;
  DisableInterpolation := True;
  FSVGIconMultiResBitmap := MultiResBitmap as TSVGIconFixedMultiResBitmap;
  FZoom := ZOOM_DEFAULT;
end;

function TSVGIconImage.CreateMultiResBitmap: TFixedMultiResBitmap;
begin
  Result := TSVGIconFixedMultiResBitmap.Create(Self, TSVGIconFixedBitmapItem);
end;

destructor TSVGIconImage.Destroy;
begin
  inherited;
  FSVGIconMultiResBitmap := nil;
end;

procedure TSVGIconImage.SetIconSize(const AWidth, AHeight: Single;
  const AZoom: Integer);
begin
  inherited Width := AWidth;
  inherited height := AHeight;
  FZoom := AZoom;
  FSVGIconMultiResBitmap.UpdateImageSize(AWidth, AHeight, AZoom);
end;

procedure TSVGIconImage.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  SetIconSize(AWidth, AHeight, FZoom);
end;

procedure TSVGIconImage.SetBitmapZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
    SetIconSize(Width, Height, AValue);
end;

initialization
  RegisterFmxClasses([TSVGIconImage]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.SVGIconImage.TSVGIconImage, TFmxObject);

end.
