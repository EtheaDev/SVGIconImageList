{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2026 (Ethea S.r.l.)                                 }
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
///   Virtual image list component that connects to a TSVGIconImageCollection
///   for displaying SVG icons with custom rendering attributes.
/// </summary>
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
  Vcl.BaseImageCollection,
{$ENDIF}
  SVGInterfaces,
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  /// <summary>
  ///   A virtual image list that displays icons from a TSVGIconImageCollection
  ///   with its own rendering attributes.
  /// </summary>
  /// <remarks>
  ///   <para>TSVGIconVirtualImageList allows multiple image lists to share the
  ///   same icon collection while displaying them with different properties:</para>
  ///   <list type="bullet">
  ///     <item>Different sizes for toolbars vs menus</item>
  ///     <item>Different colors for different themes</item>
  ///     <item>Different opacity for different states</item>
  ///   </list>
  ///   <para>In Delphi 10.3+, inherits from TVirtualImageList for full
  ///   integration with the VCL virtual image list system.</para>
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Create multiple virtual lists from one collection
  ///   SVGIconVirtualImageList1.ImageCollection := SVGIconImageCollection1;
  ///   SVGIconVirtualImageList1.Size := 16;  // Small icons
  ///   SVGIconVirtualImageList1.FixedColor := clNavy;
  ///
  ///   SVGIconVirtualImageList2.ImageCollection := SVGIconImageCollection1;
  ///   SVGIconVirtualImageList2.Size := 32;  // Large icons
  ///   SVGIconVirtualImageList2.GrayScale := True;
  ///   </code>
  /// </example>
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
    function GetSVGImageCollection: TSVGIconImageCollection;
    function GetImageCollection: TCustomImageCollection;
    procedure SetImageCollection(const Value: TCustomImageCollection);
    {$ELSE}
    FImageCollection: TSVGIconImageCollection;
    {$ENDIF}
  protected
    {$IFNDEF D10_3+}
    function GetSVGIconItems: TSVGIconItems; {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure RecreateBitmaps; {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure DoAssign(const source : TPersistent); {$IFDEF D10_3+}virtual;{$ELSE}override;{$ENDIF}
    procedure SetImageCollection(const value: TSVGIconImageCollection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCount: Integer; override;
    {$ELSE}
    procedure DoChange; override;
    procedure Loaded; override;
    {$ENDIF}

  public
    {$IFNDEF D10_3+}
    /// <summary>
    ///   Paints an icon to a canvas at the specified location and size.
    /// </summary>
    /// <param name="ACanvas">
    ///   The canvas to paint to.
    /// </param>
    /// <param name="AIndex">
    ///   The index of the icon to paint.
    /// </param>
    /// <param name="X">
    ///   The horizontal position.
    /// </param>
    /// <param name="Y">
    ///   The vertical position.
    /// </param>
    /// <param name="AWidth">
    ///   The width to render the icon.
    /// </param>
    /// <param name="AHeight">
    ///   The height to render the icon.
    /// </param>
    /// <param name="AEnabled">
    ///   When True, renders normally. When False, applies disabled styling.
    /// </param>
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;
    {$ELSE}
    /// <summary>
    ///   Creates a new virtual image list.
    /// </summary>
    /// <param name="AOwner">
    ///   The component that owns this image list.
    /// </param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Copies another virtual image list, its own rendering attributes
    ///   (FixedColor, GrayScale, Opacity...) included.
    /// </summary>
    procedure Assign(Source: TPersistent); override;

    /// <summary>
    ///   Draws an icon; a disabled one is rendered (on first use) with this
    ///   list attributes too.
    /// </summary>
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    {$ENDIF}
  published
    /// <summary>
    ///   Event triggered when the image list contents change.
    /// </summary>
    property OnChange;

    {$IFDEF D10_3+}
    /// <summary>
    ///   Fixed color to apply to icons from this virtual list.
    /// </summary>
    /// <value>
    ///   Default is SVG_INHERIT_COLOR.
    /// </value>
    /// <remarks>
    ///   This property overrides the collection's FixedColor when rendering
    ///   icons through this virtual image list.
    /// </remarks>
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
    ///   Renders icons in grayscale when True.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;

    /// <summary>
    ///   Opacity applied to icons (0-255).
    /// </summary>
    /// <value>
    ///   Default is 255 (fully opaque).
    /// </value>
    property Opacity: Byte read FOpacity write SetOpacity default 255;

    /// <summary>
    ///   Sets both Width and Height to the same value for square icons.
    /// </summary>
    /// <value>
    ///   Default is 16 pixels.
    /// </value>
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;

    /// <summary>
    ///   The TSVGIconImageCollection providing the icons.
    /// </summary>
    property ImageCollection: TCustomImageCollection read GetImageCollection write SetImageCollection;
    {$ELSE}
    property Opacity;
    property Size;
    property FixedColor;
    property AntiAliasColor;
    property GrayScale;
    property ApplyFixedColorToRootOnly;

    /// <summary>
    ///   The TSVGIconImageCollection providing the icons (pre-10.3 version).
    /// </summary>
    property ImageCollection : TSVGIconImageCollection read FImageCollection write SetImageCollection;
    {$ENDIF}

    /// <summary>
    ///   The width of icons in pixels.
    /// </summary>
    property Width;

    /// <summary>
    ///   The height of icons in pixels.
    /// </summary>
    property Height;

    /// <summary>
    ///   Renders disabled icons in grayscale when True.
    /// </summary>
    property DisabledGrayScale;

    /// <summary>
    ///   Opacity applied to disabled icons (0-255).
    /// </summary>
    property DisabledOpacity;

    {$IFDEF HiDPISupport}
    /// <summary>
    ///   Enables automatic DPI scaling.
    /// </summary>
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
        LSVG.Grayscale := False;
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
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

procedure TSVGIconVirtualImageList.SetApplyFixedColorToRootOnly(
  const Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

procedure TSVGIconVirtualImageList.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

procedure TSVGIconVirtualImageList.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

procedure TSVGIconVirtualImageList.SetAntiAliasColor(const Value: TColor);
begin
  if FAntiAliasColor <> Value then
  begin
    FAntiAliasColor := Value;
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

function TSVGIconVirtualImageList.GetSVGImageCollection: TSVGIconImageCollection;
begin
  if inherited ImageCollection is TSVGIconImageCollection then
    Result := TSVGIconImageCollection(inherited ImageCollection)
  else
    Result := nil;
end;

function TSVGIconVirtualImageList.GetImageCollection: TCustomImageCollection;
begin
  Result := inherited ImageCollection;
end;

procedure TSVGIconVirtualImageList.SetImageCollection(
  const Value: TCustomImageCollection);
begin
  inherited ImageCollection := Value;
  //Re-render with this VirtualImageList own attributes: the base class
  //setter already rebuilt the list using the collection defaults only.
  if not (csLoading in ComponentState) then
    Change;
end;

procedure TSVGIconVirtualImageList.DoChange;
var
  LCollection: TSVGIconImageCollection;
  LRendered: Integer;
begin
  //Each VirtualImageList bakes its own native image list using its own
  //attributes, without mutating the shared collection. This allows multiple
  //VirtualImageLists bound to the same collection to use different FixedColor,
  //GrayScale, Opacity, ApplyFixedColorToRootOnly and AntiAliasColor.
  LCollection := GetSVGImageCollection;
  if LCollection <> nil then
  begin
    LCollection.BeginRenderOverride(FFixedColor, FApplyFixedColorToRootOnly,
      FGrayScale, FAntiAliasColor, FOpacity);
    try
      LRendered := LCollection.OverrideRenderCount;
      inherited;
      //TVirtualImageList renders some bitmaps before calling Change, and then
      //skips the rebuild in DoChange (FImageListUpdating): AutoFill, Add,
      //a single item changed by the collection, DisabledOpacity and
      //DisabledGrayscale. Those bitmaps were rendered without this list
      //attributes: when inherited rendered nothing, rebuild here.
      if (LCollection.OverrideRenderCount = LRendered) and (Images.Count > 0) and
        ([csLoading, csDestroying] * ComponentState = []) then
        UpdateImageList;
    finally
      LCollection.EndRenderOverride;
    end;
  end
  else
    inherited;
end;

procedure TSVGIconVirtualImageList.DoDraw(Index: Integer; Canvas: TCanvas;
  X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  LCollection: TSVGIconImageCollection;
begin
  //The disabled bitmap is created on first use, here
  LCollection := GetSVGImageCollection;
  if not Enabled and (LCollection <> nil) then
  begin
    LCollection.BeginRenderOverride(FFixedColor, FApplyFixedColorToRootOnly,
      FGrayScale, FAntiAliasColor, FOpacity);
    try
      inherited;
    finally
      LCollection.EndRenderOverride;
    end;
  end
  else
    inherited;
end;

procedure TSVGIconVirtualImageList.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconVirtualImageList then
  begin
    FFixedColor := TSVGIconVirtualImageList(Source).FFixedColor;
    FApplyFixedColorToRootOnly := TSVGIconVirtualImageList(Source).FApplyFixedColorToRootOnly;
    FGrayScale := TSVGIconVirtualImageList(Source).FGrayScale;
    FAntiAliasColor := TSVGIconVirtualImageList(Source).FAntiAliasColor;
    FOpacity := TSVGIconVirtualImageList(Source).FOpacity;
    if not (csLoading in ComponentState) then
      Change;
  end;
end;

procedure TSVGIconVirtualImageList.Loaded;
var
  LCollection: TSVGIconImageCollection;
begin
  //TVirtualImageList.Loaded rebuilds the image list directly (bypassing
  //DoChange), so the render override must be applied here too.
  LCollection := GetSVGImageCollection;
  if LCollection <> nil then
  begin
    LCollection.BeginRenderOverride(FFixedColor, FApplyFixedColorToRootOnly,
      FGrayScale, FAntiAliasColor, FOpacity);
    try
      inherited;
    finally
      LCollection.EndRenderOverride;
    end;
  end
  else
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
