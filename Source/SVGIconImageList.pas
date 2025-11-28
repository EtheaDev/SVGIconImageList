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
///   Main VCL component for displaying SVG icons in an ImageList.
///   TSVGIconImageList provides a self-contained SVG icon collection
///   with support for rendering attributes and High-DPI scaling.
/// </summary>
unit SVGIconImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes
  , System.SysUtils
  , Vcl.ImgList
  , WinApi.Windows
  , Vcl.Graphics
{$IFDEF HiDPISupport}
  , System.Messaging
{$ENDIF}
  , Vcl.Forms
  , SVGInterfaces
  , SVGIconItems
  , SVGIconImageListBase
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

  /// <summary>
  ///   An extended ImageList component for VCL applications that displays SVG icons.
  ///   Provides a self-contained collection of SVG icons with support for
  ///   resizing, opacity, fixed colors, grayscale, and High-DPI scaling.
  /// </summary>
  /// <remarks>
  ///   <para>TSVGIconImageList is a drop-in replacement for TImageList that uses
  ///   SVG vector graphics instead of raster images. Benefits include:</para>
  ///   <list type="bullet">
  ///     <item>Resolution independence - icons scale perfectly at any size</item>
  ///     <item>Smaller file size compared to multiple bitmap resolutions</item>
  ///     <item>Runtime color and opacity modifications</item>
  ///     <item>Automatic High-DPI scaling (Delphi 10.3+)</item>
  ///   </list>
  ///   <para>For Delphi 10.3+, consider using TSVGIconImageCollection with
  ///   TSVGIconVirtualImageList for better performance when sharing icons
  ///   across multiple forms or controls.</para>
  /// </remarks>
  /// <example>
  ///   <code>
  ///   // Load SVG icons programmatically
  ///   var
  ///     SVG: ISVG;
  ///   begin
  ///     SVG := GlobalSVGFactory.NewSvg;
  ///     SVG.LoadFromFile('icon.svg');
  ///     SVGIconImageList1.Add(SVG, 'MyIcon');
  ///   end;
  ///   </code>
  /// </example>
  TSVGIconImageList = class(TSVGIconImageListBase)
  private
    FSVGItems: TSVGIconItems;
  protected
    function GetSVGIconItems: TSVGIconItems; override;
    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(const Source: TPersistent); override;
  public
    /// <summary>
    ///   Recreates the internal bitmap cache from the SVG sources.
    ///   Called automatically when properties change.
    /// </summary>
    procedure RecreateBitmaps; override;

    /// <summary>
    ///   Creates a new SVG icon image list.
    /// </summary>
    /// <param name="AOwner">
    ///   The component that owns this image list.
    /// </param>
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    ///   Destroys the SVG icon image list and releases all resources.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Adds an SVG icon to the image list.
    /// </summary>
    /// <param name="ASVG">
    ///   The ISVG interface containing the SVG to add.
    /// </param>
    /// <param name="AIconName">
    ///   The name to assign to this icon.
    /// </param>
    /// <param name="AGrayScale">
    ///   When True, renders this icon in grayscale. Default is False.
    /// </param>
    /// <param name="AFixedColor">
    ///   Fixed color for this icon. Default is SVG_INHERIT_COLOR.
    /// </param>
    /// <param name="AAntiAliasColor">
    ///   Background color for anti-aliasing. Default is clBtnFace.
    /// </param>
    /// <returns>
    ///   The index of the newly added icon.
    /// </returns>
    function Add(const ASVG: ISVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR;
       const AAntiAliasColor: TColor = clBtnFace): Integer; overload;

    /// <summary>
    ///   Adds an SVG icon to the image list with a category.
    /// </summary>
    /// <param name="ASVG">
    ///   The ISVG interface containing the SVG to add.
    /// </param>
    /// <param name="AIconName">
    ///   The name to assign to this icon.
    /// </param>
    /// <param name="AIconCategory">
    ///   The category for this icon (used for organization).
    /// </param>
    /// <param name="AGrayScale">
    ///   When True, renders this icon in grayscale. Default is False.
    /// </param>
    /// <param name="AFixedColor">
    ///   Fixed color for this icon. Default is SVG_INHERIT_COLOR.
    /// </param>
    /// <param name="AAntiAliasColor">
    ///   Background color for anti-aliasing. Default is clBtnFace.
    /// </param>
    /// <returns>
    ///   The index of the newly added icon.
    /// </returns>
    function Add(const ASVG: ISVG; const AIconName, AIconCategory: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR;
       const AAntiAliasColor: TColor = clBtnFace): Integer; overload;

    /// <summary>
    ///   Deletes an icon from the image list by index.
    /// </summary>
    /// <param name="Index">
    ///   The zero-based index of the icon to delete.
    /// </param>
    procedure Delete(const Index: Integer);

    /// <summary>
    ///   Removes an icon from the image list by name.
    /// </summary>
    /// <param name="Name">
    ///   The name of the icon to remove.
    /// </param>
    procedure Remove(const Name: string);

    /// <summary>
    ///   Removes all icons from the image list.
    /// </summary>
    procedure ClearIcons; override;

    /// <summary>
    ///   Saves all icons as a bitmap strip to a file.
    /// </summary>
    /// <param name="AFileName">
    ///   The full path of the bitmap file to create.
    /// </param>
    /// <remarks>
    ///   The icons are arranged in a grid pattern to create a square-ish strip.
    ///   This is useful for exporting icons to bitmap format.
    /// </remarks>
    procedure SaveToFile(const AFileName: string);

    /// <summary>
    ///   Paints a specific icon to a canvas at the specified location and size.
    /// </summary>
    /// <param name="ACanvas">
    ///   The canvas to paint to.
    /// </param>
    /// <param name="AIndex">
    ///   The zero-based index of the icon to paint.
    /// </param>
    /// <param name="X">
    ///   The horizontal position in pixels.
    /// </param>
    /// <param name="Y">
    ///   The vertical position in pixels.
    /// </param>
    /// <param name="AWidth">
    ///   The width to render the icon.
    /// </param>
    /// <param name="AHeight">
    ///   The height to render the icon.
    /// </param>
    /// <param name="AEnabled">
    ///   When True, renders normally. When False, applies disabled styling.
    ///   Default is True.
    /// </param>
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;
  published
    /// <summary>
    ///   The width of icons in pixels.
    /// </summary>
    property Width;

    /// <summary>
    ///   The height of icons in pixels.
    /// </summary>
    property Height;

    /// <summary>
    ///   Sets both Width and Height to the same value for square icons.
    /// </summary>
    property Size;

    /// <summary>
    ///   Event triggered when the image list contents change.
    /// </summary>
    property OnChange;

    /// <summary>
    ///   The collection of SVG icon items.
    /// </summary>
    property SVGIconItems;

    /// <summary>
    ///   Global opacity applied to all icons (0-255).
    /// </summary>
    property Opacity;

    /// <summary>
    ///   Fixed color applied to all icons.
    /// </summary>
    property FixedColor;

    /// <summary>
    ///   Background color for anti-aliasing.
    /// </summary>
    property AntiAliasColor;

    /// <summary>
    ///   Renders all icons in grayscale when True.
    /// </summary>
    property GrayScale;

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
    ///   Enables automatic scaling when form DPI changes.
    /// </summary>
    /// <value>
    ///   Default is True.
    /// </value>
    /// <remarks>
    ///   When True, icons automatically scale when the application moves
    ///   between monitors with different DPI settings.
    /// </remarks>
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.Types
  , WinApi.CommCtrl
  , System.Math
  , Vcl.ComCtrls
  , SVGIconVirtualImageList;


{ TSVGIconImageList }

function TSVGIconImageList.Add(const ASVG: ISVG;
  const AIconName: string; const AGrayScale: Boolean = False;
  const AFixedColor: TColor = SVG_INHERIT_COLOR;
  const AAntiAliasColor: TColor = clBtnFace): Integer;
begin
  Result := Add(ASVG, AIconName, '', AGrayScale, AFixedColor, AAntiAliasColor);
end;

function TSVGIconImageList.Add(const ASVG: ISVG;
  const AIconName, AIconCategory: string; const AGrayScale: Boolean = False;
  const AFixedColor: TColor = SVG_INHERIT_COLOR;
  const AAntiAliasColor: TColor = clBtnFace): Integer;
var
  Item: TSVGIconItem;
begin
  FSVGItems.BeginUpdate;
  try
    Item := FSVGItems.Add;
    Item.SVG := ASVG;
    Item.IconName := AIconName;
    Item.Category := AIconCategory;
    Item.FixedColor := AFixedColor;
    Item.AntiAliasColor := AAntiAliasColor;
    Item.GrayScale := AGrayScale;
  finally
    FSVGItems.EndUpdate;
  end;
  Result := FSVGItems.Count - 1;
end;


procedure TSVGIconImageList.ClearIcons;
begin
  BeginUpdate;
  try
    FSVGItems.Clear;
    inherited Clear;
  finally
    EndUpdate;
  end;
end;

constructor TSVGIconImageList.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
end;

procedure TSVGIconImageList.Delete(const Index: Integer);
begin
  //Don't call inherited method of ImageList, to avoid errors
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems.Delete(Index);
end;

destructor TSVGIconImageList.Destroy;
begin
  FreeAndNil(FSVGItems);
  inherited;
end;

procedure TSVGIconImageList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, False);
end;

procedure TSVGIconImageList.DoAssign(const Source: TPersistent);
{$IFNDEF D10_3+}
var
  LVirtualList : TSVGIconVirtualImageList;
{$ENDIF}
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    FSVGItems.Assign(TSVGIconImageList(Source).FSVGItems);
  end
  {$IFNDEF D10_3+}
  else if Source is TSVGIconVirtualImageList then
  begin
    LVirtualList := TSVGIconVirtualImageList(Source);
    if LVirtualList.ImageCollection <> nil then
    begin
      FSVGItems.Assign(LVirtualList.ImageCollection.SVGIconItems);
    end;
  end;
  {$ENDIF}
end;

procedure TSVGIconImageList.PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
  const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True);
var
  LSVG: ISVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (AIndex >= 0) and (AIndex < FSVGItems.Count) then
  begin
    LItem := FSVGItems[AIndex];
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

procedure TSVGIconImageList.ReadImageData(Stream: TStream);
var
  LStream: TMemoryStream;
  LCount, LSize: Integer;
  LSVG: ISVG;
  C: Integer;
  LPos: Int64;
  LIconName: string;
  LTag: TBytes;
  LFixedColorStr: AnsiString;
  LGrayScale: Boolean;
  LFixedColor: TColor;
begin
  //Only for backward compatibility: load images stored in old format
  LStream := nil;
  LSVG := nil;
  BeginUpdate;
  try
    LStream := TMemoryStream.Create;
    //Read Count of Images
    if Stream.Read(LCount, SizeOf(Integer)) > 0 then
    begin
      for C := 0 to LCount - 1 do
      begin
        LSVG := GlobalSVGFactory.NewSvg;
        try
          //Read IconName
          Stream.Read(LSize, SizeOf(Integer));
          SetLength(LIconName, LSize);
          Stream.Read(PChar(LIconName)^, LSize * SizeOf(Char));
          //Read SVG Stream Size
          Stream.Read(LSize, SizeOf(Integer));
          LStream.CopyFrom(Stream, LSize);
          //Read SVG Stream data
          try
            LSVG.LoadFromStream(LStream);
          except
            on E: Exception do
              raise Exception.CreateFmt('Detected "old" binary image stream of %s! '+
                'You must disable Direct2D (removing $DEFINE PreferNativeSvgSupport from SVGIconImageList.inc), '+
                'recompile SVGIconImageList packages, and try again',
                [Owner.Name+'.'+Self.Name]);
          end;
          //Check for FixedColor attribute
          LPos := Stream.Position;
          LFixedColor := SVG_INHERIT_COLOR;
          SetLength(LTag, 10);
          Stream.Read(Pointer(LTag)^, 10);
          SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 10);
          if LFixedColorStr = 'FixedColor' then
            //Read Fixed Color value
            Stream.Read(LFixedColor, SizeOf(Integer))
          else
            Stream.Position := LPos;

          //Check for GrayScale attribute
          LPos := Stream.Position;
          LGrayScale := False;
          SetLength(LTag, 9);
          Stream.Read(Pointer(LTag)^, 9);
          SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 9);
          if LFixedColorStr = 'GrayScale' then
            LGrayScale := True
          else
            Stream.Position := LPos;

          Add(LSVG, LIconName, LGrayScale, LFixedColor);
          LStream.Clear;
        finally
          LSVG := nil;
        end;
      end;
    end;
  finally
    LStream.Free;
    EndUpdate;
  end;
end;

procedure TSVGIconImageList.RecreateBitmaps;
var
  C: Integer;
  LBitmap: TBitmap;
  LItem: TSVGIconItem;
begin
  if not Assigned(FSVGItems) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    for C := 0 to FSVGItems.Count - 1 do
    begin
      LItem := FSVGItems[C];
      LBitmap := LItem.GetBitmap(Width, Height, FixedColor,
        ApplyFixedColorToRootOnly, Opacity, GrayScale, AntiAliasColor);
      try
        ImageList_Add(Handle, LBitmap.Handle, 0);
      finally
        LBitmap.Free;
      end;
    end;
  end;
end;

procedure TSVGIconImageList.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGIconImageList.SaveToFile(const AFileName: string);
var
  LImageStrip: TBitmap;
  LImageCount: Integer;
  LStripWidth, LStripHeight: Integer;

  procedure CreateLImageStrip(var AStrip: TBitmap);
  var
    I, J, K: Integer;
  begin
    with AStrip do
    begin
      Canvas.Brush.Color := clNone;
      Canvas.FillRect(Rect(0, 0, AStrip.Width, AStrip.Height));
      J := 0;
      K := 0;
      for I := 0 to Self.Count - 1 do
      begin
        Draw(Canvas, J * Width, K * Height, I, dsTransparent, itImage);
        Inc(J);
        if J >= LStripWidth then
        begin
          J := 0;
          Inc(K);
        end;
      end;
    end;
  end;

  procedure CalcDimensions(ACount: Integer; var AWidth, AHeight: Integer);
  var
    X: Single;
  begin
    X := Sqrt(ACount);
    AWidth := Trunc(X);
    if Frac(X) > 0 then
      Inc(AWidth);
    X := ACount / AWidth;
    AHeight := Trunc(X);
    if Frac(X) > 0 then
      Inc(AHeight);
  end;

begin
  LImageStrip := TBitmap.Create;
  try
    LImageCount := Count;
    CalcDimensions(LImageCount, LStripWidth, LStripHeight);
    LImageStrip.Width := LStripWidth * Width;
    LImageStrip.Height := LStripHeight * Height;
    CreateLImageStrip(LImageStrip);
    LImageStrip.SaveToFile(AFileName);
  finally
    LImageStrip.Free;
  end;
end;

function TSVGIconImageList.GetSVGIconItems: TSVGIconItems;
begin
  Result := FSVGItems;
end;

procedure TSVGIconImageList.WriteImageData(Stream: TStream);
begin
  Exit;
end;


end.
