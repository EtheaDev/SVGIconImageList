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
unit SVGIconVirtualImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  WinApi.Windows,
  Winapi.CommCtrl,
  System.Classes,
  {$IFDEF DXE4+}System.Messaging,{$ELSE}SVGMessaging,{$ENDIF}
  Vcl.Controls,
  Vcl.Graphics,
{$IFDEF D10_3+}
  Vcl.VirtualImageList,
{$ENDIF}
  SVGInterfaces,
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  {$IFDEF D10_3+}
  TSVGIconVirtualImageList = class(TVirtualImageList)
  {$ELSE}
  TSVGIconVirtualImageList = class(TSVGIconImageListBase)
  {$ENDIF}
  private
    {$IFDEF D10_3+}
    FFixedColor: TColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FAntiAliasColor: TColor;
    FOpacity: Byte;
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetAntiAliasColor(const Value: TColor);
    procedure SetApplyFixedColorToRootOnly(const Value: Boolean);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function StoreSize: Boolean;
    procedure UpdateImageCollection;
    {$ELSE}
    FImageCollection: TSVGIconImageCollection;
    {$ENDIF}
  protected
    {$IFNDEF D10_3+}
    // override abstract methods
    function GetSVGIconItems: TSVGIconItems; {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure RecreateBitmaps; {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure DoAssign(const source : TPersistent); {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure SetImageCollection(const value: TSVGIconImageCollection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCount: Integer; override;
    {$ELSE}
    procedure DoChange; override;
    {$ENDIF}

  public
    {$IFNDEF D10_3+}
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;
    {$ELSE}
    constructor Create(AOwner: TComponent); override;
    {$ENDIF}
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    {$IFDEF D10_3+}
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property ApplyFixedColorToRootOnly: Boolean read FApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly default False;
    property AntiAliasColor: TColor read FAntiAliasColor write SetAntiAliasColor default clBtnFace;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property ImageCollection;
    {$ELSE}
    property Opacity;
    property Size;
    property FixedColor;
    property AntiAliasColor;
    property GrayScale;
    property ApplyFixedColorToRootOnly;
    property ImageCollection : TSVGIconImageCollection read FImageCollection write SetImageCollection;
    {$ENDIF}
    property Width;
    property Height;
    property DisabledGrayScale;
    property DisabledOpacity;
    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.Types,
  System.UITypes,
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ImgList,
  SVGIconImageList;

{ TSVGIconVirtualImageList }

{$IFNDEF D10_3+}
procedure TSVGIconVirtualImageList.DoAssign(const source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    if FImageCollection <> nil then
      FImageCollection.SVGIconItems.Assign(TSVGIconImageList(Source).SVGIconItems);
  end
  else if Source is TSVGIconVirtualImageList then
    SetImageCollection(TSVGIconVirtualImageList(Source).FImageCollection);
end;

function TSVGIconVirtualImageList.GetCount: Integer;
begin
  if FImageCollection <> nil then
    result := FImageCollection.SVGIconItems.Count
  else
    result := 0;
end;

function TSVGIconVirtualImageList.GetSVGIconItems: TSVGIconItems;
begin
  if Assigned(FImageCollection) then
    Result := FImageCollection.SVGIconItems
  else
    Result := nil;
end;

procedure TSVGIconVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageCollection) then
  begin
    BeginUpdate;
    try
      FImageCollection := nil;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.PaintTo(const ACanvas: TCanvas;
  const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  LSVG: ISVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (FImageCollection <> nil) and (AIndex >= 0) and (AIndex < FImageCollection.SVGIconItems.Count) then
  begin
    LItem := FImageCollection.SVGIconItems[AIndex];
    LSVG := LItem.SVG;
    if LItem.FixedColor <> SVG_INHERIT_COLOR then
      LSVG.FixedColor := LItem.FixedColor
    else
      LSVG.FixedColor := FixedColor;
    LOpacity := Opacity;
    if AEnabled then
    begin
      if LItem.GrayScale or GrayScale then
        LSVG.Grayscale := True
      else
        LSVG.Grayscale := False;
    end
    else
    begin
      if DisabledGrayScale then
        LSVG.Grayscale := True
      else
        LOpacity := DisabledOpacity;
    end;
    LSVG.Opacity := LOpacity / 255;
    LSVG.PaintTo(ACanvas.Handle, TRectF.Create(TPointF.Create(X, Y), AWidth, AHeight));
    LSVG.Opacity := 1;
  end;
end;

procedure TSVGIconVirtualImageList.RecreateBitmaps;
var
  C: Integer;
  LItem: TSVGIconItem;
  BitMap: TBitmap;
  LFixedColor, LAntiAliasColor: TColor;
  LApplyToRootOnly: Boolean;
  LGrayScale: Boolean;
begin
  if not Assigned(FImageCollection) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    if FImageCollection.FixedColor <> SVG_INHERIT_COLOR then
    begin
      LFixedColor := FImageCollection.FixedColor;
      LApplyToRootOnly := FImageCollection.ApplyFixedColorToRootOnly;
    end
    else
    begin
      LFixedColor := FixedColor;
      LApplyToRootOnly := ApplyFixedColorToRootOnly;
    end;
    if FImageCollection.AntiAliasColor <> clBtnFace then
      LAntiAliasColor := FImageCollection.AntiAliasColor
    else
      LAntiAliasColor := AntiAliasColor;
    if GrayScale or FImageCollection.GrayScale then
      LGrayscale := True
    else
      LGrayscale := False;
    for C := 0 to FImageCollection.SVGIconItems.Count - 1 do
    begin
      LItem := FImageCollection.SVGIconItems[C];
      Bitmap := LItem.GetBitmap(Width, Height, LFixedColor, LApplyToRootOnly,
        Opacity, LGrayScale, LAntiAliasColor);
      try
        ImageList_Add(Handle, Bitmap.Handle, 0);
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.SetImageCollection(const value: TSVGIconImageCollection);
begin
  if FImageCollection <> Value then
  begin
    if FImageCollection <> nil then
      FImageCollection.RemoveFreeNotification(Self);
    FImageCollection := Value;
    if FImageCollection <> nil then
      FImageCollection.FreeNotification(Self);
    Change;
  end;
end;
{$ENDIF}

{$IFDEF D10_3+}
procedure TSVGIconVirtualImageList.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    UpdateImageCollection;
  end;
end;

procedure TSVGIconVirtualImageList.SetApplyFixedColorToRootOnly(
  const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    UpdateImageCollection;
  end;
end;

procedure TSVGIconVirtualImageList.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    UpdateImageCollection;
  end;
end;

procedure TSVGIconVirtualImageList.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateImageCollection;
  end;
end;

procedure TSVGIconVirtualImageList.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FAntiAliasColor := Value;
    UpdateImageCollection;
  end;
end;

procedure TSVGIconVirtualImageList.UpdateImageCollection;
begin
  if ImageCollection is TSVGIconImageCollection then
  begin
    TSVGIconImageCollection(ImageCollection).UpdateAttributes(
      FFixedColor,
      FApplyFixedColorToRootOnly,
      FGrayScale,
      FAntiAliasColor,
      FOpacity);
  end;
end;

procedure TSVGIconVirtualImageList.DoChange;
begin
  UpdateImageCollection;
  inherited;
end;

function TSVGIconVirtualImageList.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

procedure TSVGIconVirtualImageList.SetSize(const Value: Integer);
begin
  if (Height <> Value) or (Width <> Value) then
  begin
    BeginUpdate;
    try
      Width := Value;
      Height := Value;
    finally
      EndUpdate;
    end;
  end;
end;

function TSVGIconVirtualImageList.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

constructor TSVGIconVirtualImageList.Create(AOwner: TComponent);
begin
  FFixedColor := SVG_INHERIT_COLOR;
  FApplyFixedColorToRootOnly := False;
  FAntiAliasColor := clBtnFace;
  FGrayScale := False;
  FOpacity := 255;
  inherited;
end;
{$ENDIF}

end.
