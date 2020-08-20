{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
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
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  Vcl.Graphics,
  SVG,
  SVGColor,
{$IFDEF D10_3+}
  Vcl.VirtualImageList,
{$ENDIF}
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  TSVGIconVirtualImageList = class(TSVGIconImageListBase)
  private
    FCollection : TSVGIconImageCollection;
  protected
    // override abstract methods
    function GetSVGIconItems: TSVGIconItems; override;
    procedure RecreateBitmaps; override;
    procedure SetCollection(const value: TSVGIconImageCollection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoAssign(const source : TPersistent); override;
    function GetCount: Integer; override;

  public
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;

  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property Opacity;
    property Width;
    property Height;
    property Size;
    property FixedColor;
    property GrayScale;
    property DisabledGrayScale;
    property DisabledOpacity;
    property Collection : TSVGIconImageCollection read FCollection write SetCollection;

    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.UITypes,
  System.Math,
  System.SysUtils,
  WinApi.Windows,
  Winapi.CommCtrl,
  Winapi.GDIPAPI,
  Vcl.Forms,
  Vcl.ImgList,
  SVGTypes,
  SVGCommon,
  SVGIconImageList;

{ TSVGIconVirtualImageList }

procedure TSVGIconVirtualImageList.DoAssign(const source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    if FCollection <> nil then
    begin
      FCollection.SVGIconItems.Assign(TSVGIconImageList(Source).SVGIconItems);
      FCollection.StoreAsText := TSVGIconImageList(Source).StoreAsText;
    end;
  end
  else if Source is TSVGIconVirtualImageList then
    SetCollection(TSVGIconVirtualImageList(Source).FCollection);
end;

function TSVGIconVirtualImageList.GetCount: Integer;
begin
  if FCollection <> nil then
    result := FCollection.SVGIconItems.Count
  else
    result := 0;
end;

function TSVGIconVirtualImageList.GetSVGIconItems: TSVGIconItems;
begin
  if Assigned(FCollection) then
    Result := FCollection.SVGIconItems
  else
    Result := nil;
end;

procedure TSVGIconVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCollection) then
  begin
    BeginUpdate;
    try
      FCollection := nil;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.PaintTo(const ACanvas: TCanvas;
  const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  R: TGPRectF;
  LSVG: TSVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (FCollection <> nil) and (AIndex >= 0) and (AIndex < FCollection.SVGIconItems.Count) then
  begin
    LItem := FCollection.SVGIconItems[AIndex];
    LSVG := LItem.SVG;
    if LItem.FixedColor <> SVG_INHERIT_COLOR then
      LSVG.FixedColor := LItem.FixedColor
    else
      LSVG.FixedColor := FFixedColor;
    LOpacity := FOpacity;
    if AEnabled then
    begin
      if LItem.GrayScale or FGrayScale then
        LSVG.Grayscale := True
      else
        LSVG.Grayscale := False;
    end
    else
    begin
      if FDisabledGrayScale then
        LSVG.Grayscale := True
      else
        LOpacity := FDisabledOpacity;
    end;
    LSVG.SVGOpacity := LOpacity / 255;
    R := FittedRect(MakeRect(X, Y, AWidth, AHeight), LSVG.Width, LSVG.Height);
    LSVG.PaintTo(ACanvas.Handle, R, nil, 0);
    LSVG.SVGOpacity := 1;
  end;
end;

procedure TSVGIconVirtualImageList.RecreateBitmaps;
var
  C: Integer;
  LItem: TSVGIconItem;
  LIcon: HICON;
  LFixedColor: TColor;
  LGrayScale: Boolean;
begin
  if not Assigned(FCollection) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    if FCollection.FixedColor <> SVG_INHERIT_COLOR then
      LFixedColor := FCollection.FixedColor
    else
      LFixedColor := FFixedColor;
    if FGrayScale or FCollection.GrayScale then
      LGrayscale := True
    else
      LGrayscale := False;
    for C := 0 to FCollection.SVGIconItems.Count - 1 do
    begin
      LItem := FCollection.SVGIconItems[C];
      LIcon := LItem.GetIcon(Width, Height, LFixedColor, FOpacity, LGrayScale);
      ImageList_AddIcon(Handle, LIcon);
      DestroyIcon(LIcon);
    end;
  end;
end;

procedure TSVGIconVirtualImageList.SetCollection(const value: TSVGIconImageCollection);
begin
  if FCollection <> Value then
  begin
    if FCollection <> nil then
      FCollection.RemoveFreeNotification(Self);
    FCollection := Value;
    if FCollection <> nil then
      FCollection.FreeNotification(Self);
    Change;
  end;
end;

end.
