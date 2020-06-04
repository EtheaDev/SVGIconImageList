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
unit SVGIconImage;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Windows
  , SysUtils
{$IFDEF D10_4+}
  , System.UITypes
{$ENDIF}
  , GDIPOBJ
  , Classes
  , Controls
  , Graphics
  , SVG
  , SVGIconImageList;

type
  TSVGIconImage = class(TGraphicControl)
  strict private
    FSVG: TSVG;
    FStream: TMemoryStream;

    FCenter: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FAutoSize: Boolean;
    FScale: Double;
    FOpacity: Byte;
    FFileName: TFileName;
    FImageList: TSVGIconImageList;
    FImageIndex: Integer;
    procedure SetCenter(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
    //procedure ReadData(Stream: TStream);
    //procedure WriteData(Stream: TStream);
    procedure SetImageIndex(const Value: Integer);
    procedure SetStretch(const Value: Boolean);
    procedure SetScale(const Value: Double);
    procedure SetAutoSizeImage(const Value: Boolean);
  private
    function GetSVGText: string;
    procedure SetSVGText(const AValue: string);
    function UsingSVGText: Boolean;
    procedure SetImageList(const Value: TSVGIconImageList);
  protected
    //procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckAutoSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Empty: Boolean;
    procedure Paint; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure Assign(Source: TPersistent); override;
    property SVG: TSVG read FSVG;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSizeImage;
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch;
    property Opacity: Byte read FOpacity write SetOpacity;
    property Scale: Double read FScale write SetScale;
    property FileName: TFileName read FFileName write SetFileName;
    property ImageList: TSVGIconImageList read FImageList write SetImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property SVGText: string read GetSVGText write SetSVGText stored UsingSVGText;
    property Enabled;
    property Visible;
    property Constraints;
    property Anchors;
    property Align;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  TSVGGraphic = class(TGraphic)
  strict private
    FSVG: TSVG;
    FStream: TMemoryStream;

    FOpacity: Byte;
    FFileName: TFileName;

    procedure SetOpacity(Value: Byte);
    procedure SetFileName(const Value: TFileName);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function GetEmpty: Boolean; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;

    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure AssignSVG(SVG: TSVG);

    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;

    procedure SaveToStream(Stream: TStream); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;

    property Opacity: Byte read FOpacity write SetOpacity;
    property FileName: TFileName read FFileName write SetFileName;
  end;

function TGPImageToBitmap(Image: TGPImage): TBitmap;

implementation

uses
  Vcl.Dialogs,
  Winapi.GDIPAPI;

function TGPImageToBitmap(Image: TGPImage): TBitmap;
var
  Graphics: TGPGraphics;
  Bitmap: TBitmap;
  P: Pointer;
  W, H: Cardinal;
begin
  Bitmap := nil;
  if Assigned(Image) then
  begin
    W := Image.GetWidth;
    H := Image.GetHeight;
    if (W > 0) and (H > 0) then
    begin
      Bitmap := TBitmap.Create;
      Bitmap.PixelFormat := pf32Bit;
      Bitmap.Width := W;
      Bitmap.Height := H;
      P := Bitmap.ScanLine[H - 1];
      FillChar(P^, (W * H) shl 2, 0);
      Graphics := TGPGraphics.Create(Bitmap.Canvas.Handle);
      try
        Graphics.DrawImage(Image, 0, 0);
      finally
        Graphics.Free;
      end;
    end;
  end;
  Result := Bitmap;
end;

constructor TSVGIconImage.Create(AOwner: TComponent);
begin
  inherited;
  FSVG := TSVG.Create;
  FProportional := False;
  FCenter := True;
  FStretch := True;
  FOpacity := 255;
  FScale := 1;
  FImageIndex := -1;
  FStream := TMemoryStream.Create;
end;

destructor TSVGIconImage.Destroy;
begin
  FSVG.Free;
  FStream.Free;
  inherited;
end;

procedure TSVGIconImage.CheckAutoSize;
begin
  if FAutoSize and (FSVG.Width > 0) and (FSVG.Height > 0) then
  begin
    SetBounds(Left, Top,  Round(FSVG.Width), Round(FSVG.Height));
  end;
end;

procedure TSVGIconImage.Clear;
begin
  FSVG.Clear;
  FFileName := '';
  Repaint;
end;

function TSVGIconImage.Empty: Boolean;
begin
  Empty := FSVG.Count = 0;
end;

function TSVGIconImage.GetSVGText: string;
begin
  Result := FSVG.Source;
end;

function TSVGIconImage.UsingSVGText: Boolean;
begin
  Result := not (Assigned(FImageList) and (FImageIndex >= 0) and
     (FImageIndex < FImagelist.Count));
end;

procedure TSVGIconImage.Paint;
var
  Bounds: TGPRectF;

  procedure CalcWidth(const ImageWidth, ImageHeight: Double);
  var
    R: Double;
  begin
    Bounds.Width := ImageWidth * FScale;
    Bounds.Height := ImageHeight * FScale;

    if FProportional then
    begin
      if ImageHeight > 0 then
        R :=  ImageWidth / ImageHeight
      else
        R := 1;

      if Width / Height > R then
      begin
        Bounds.Width := Height * R;
        Bounds.Height := Height;
      end else
      begin
        Bounds.Width := Width;
        Bounds.Height := Width / R;
      end;
      Exit;
    end;

    if FStretch then
    begin
      Bounds := MakeRect(0.0, 0, Width, Height);
      Exit;
    end;
  end;

  procedure CalcOffset;
  begin
    Bounds.X := 0;
    Bounds.Y := 0;
    if FCenter then
    begin
      Bounds.X := (Width - Bounds.Width) / 2;
      Bounds.Y := (Height - Bounds.Height) / 2;
    end;
  end;

var
  SVG: TSVG;
begin
  if not UsingSVGText then
    SVG := FImageList.Images[FImageIndex]
  else
    SVG := FSVG;

  if SVG.Count > 0 then
  begin
    CalcWidth(SVG.Width, SVG.Height);
    CalcOffset;

    SVG.SVGOpacity := FOpacity / 255;
    SVG.PaintTo(Canvas.Handle, Bounds, nil, 0);
    SVG.SVGOpacity := 1;
  end;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TSVGIconImage.LoadFromFile(const FileName: string);
begin
  if csLoading in ComponentState then
    Exit;
  try
    FStream.Clear;
    FStream.LoadFromFile(FileName);
    FSVG.LoadFromStream(FStream);
    FFileName := FileName;
  except
    Clear;
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.LoadFromStream(Stream: TStream);
begin
  try
    FFileName := '';
    FStream.Clear;
    FStream.LoadFromStream(Stream);
    FSVG.LoadFromStream(FStream);
  except
  end;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;
end;

procedure TSVGIconImage.Assign(Source: TPersistent);
var
  SVG: TSVG;
begin
  if (Source is TSVGIconImage) then
  begin
    SVG := (Source as TSVGIconImage).FSVG;
    FSVG.LoadFromText(SVG.Source);
    FImageIndex := -1;
    CheckAutoSize;
  end;

  if (Source.ClassType = TSVG) then
  begin
    SVG := TSVG(Source);
    FSVG.LoadFromText(SVG.Source);
    FImageIndex := -1;
  end;

  Repaint;
end;

procedure TSVGIconImage.SaveToFile(const FileName: string);
begin
  FSVG.SaveToFile(FileName);
end;

procedure TSVGIconImage.SetAutoSizeImage(const Value: Boolean);
begin
  if (Value = FAutoSize) then
    Exit;
  FAutoSize := Value;

  CheckAutoSize;
end;

procedure TSVGIconImage.SetCenter(Value: Boolean);
begin
  if Value = FCenter then
    Exit;

  FCenter := Value;
  Repaint;
end;

procedure TSVGIconImage.SetProportional(Value: Boolean);
begin
  if Value = FProportional then
    Exit;

  FProportional := Value;
  Repaint;
end;

procedure TSVGIconImage.SetScale(const Value: Double);
begin
  if Value = FScale then
    Exit;
  FScale := Value;
  FAutoSize := False;
  Repaint;
end;

procedure TSVGIconImage.SetStretch(const Value: Boolean);
begin
  if Value = FStretch then
    Exit;

  FStretch := Value;
  if FStretch then
    FAutoSize := False;
  Repaint;
end;

procedure TSVGIconImage.SetSVGText(const AValue: string);
begin
  FSVG.LoadFromText(AValue);
  Repaint;
end;

procedure TSVGIconImage.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Repaint;
end;

procedure TSVGIconImage.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;
  LoadFromFile(Value);
end;

(*
procedure TSVGIconImage.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGIconImage.ReadData(Stream: TStream);
var
  Size: LongInt;
begin
  if UsingSVGText then
    Exit;
  Stream.Read(Size, SizeOf(Size));
  FStream.Clear;
  if Size > 0 then
  begin
    FStream.CopyFrom(Stream, Size);
    FSVG.LoadFromStream(FStream);
  end else
    FSVG.Clear;
end;

procedure TSVGIconImage.WriteData(Stream: TStream);
var
  Size: LongInt;
begin
  if UsingSVGText then
    Exit;
  FSVG.SaveToStream(Stream);
end;
*)

constructor TSVGGraphic.Create;
begin
  inherited;
  FSVG := TSVG.Create;
  FOpacity := 255;
  FStream := TMemoryStream.Create;
end;

destructor TSVGGraphic.Destroy;
begin
  FSVG.Free;
  FStream.Free;
  inherited;
end;

procedure TSVGGraphic.Clear;
begin
  FSVG.Clear;
  FFileName := '';
  Changed(Self);
end;

procedure TSVGGraphic.Assign(Source: TPersistent);
begin
  if (Source is TSVGGraphic) then
  begin
    try
      FSVG.Free;
      FSVG := TSVG(TSVGGraphic(Source).FSVG.Clone(nil));
      FStream.Clear;
      FStream.LoadFromStream(TSVGGraphic(Source).FStream);
    except
    end;
    Changed(Self);
  end;
end;

procedure TSVGGraphic.AssignSVG(SVG: TSVG);
begin
  FSVG.LoadFromText(SVG.Source);
  Changed(Self);
end;

procedure TSVGGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGGraphic then
    TSVGGraphic(Dest).Assign(Self);
end;

procedure TSVGGraphic.SetOpacity(Value: Byte);
begin
  if Value = FOpacity then
    Exit;

  FOpacity := Value;
  Changed(Self);
end;

procedure TSVGGraphic.SetWidth(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.SetFileName(const Value: TFileName);
begin
  if Value = FFileName then
    Exit;

  LoadFromFile(Value);
end;

procedure TSVGGraphic.SetHeight(Value: Integer);
begin
  inherited;

end;

procedure TSVGGraphic.ReadData(Stream: TStream);
var
  Size: LongInt;
begin
  Stream.Read(Size, SizeOf(Size));
  FStream.Clear;
  FStream.CopyFrom(Stream, Size);
  FSVG.LoadFromStream(FStream);
end;

procedure TSVGGraphic.WriteData(Stream: TStream);
var
  Size: LongInt;
begin
  Size := FStream.Size;
  Stream.Write(Size, SizeOf(Size));
  FStream.Position := 0;
  FStream.SaveToStream(Stream);
end;

procedure TSVGGraphic.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TSVGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Bounds: TGPRectF;
begin
  if Empty then
    Exit;

  Bounds := MakeRect(Rect.Left + 0.0, Rect.Top,
    Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);

  FSVG.SVGOpacity := FOpacity / 255;
  FSVG.PaintTo(ACanvas.Handle, Bounds, nil, 0);
end;


function TSVGGraphic.GetEmpty: Boolean;
begin
  Result := FSVG.Count = 0;
end;

function TSVGGraphic.GetWidth: Integer;
begin
  Result := Round(FSVG.Width);
end;

function TSVGGraphic.GetHeight: Integer;
begin
  Result := Round(FSVG.Height);
end;

procedure TSVGGraphic.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.LoadFromFile(const Filename: String);
begin
  FStream.Clear;
  FStream.LoadFromFile(FileName);
  FSVG.LoadFromStream(FStream);
  Changed(Self);
end;

procedure TSVGGraphic.LoadFromStream(Stream: TStream);
begin
  try
    FFileName := '';
    FStream.LoadFromStream(Stream);
    FSVG.LoadFromStream(FStream);
  except
  end;
  Changed(Self);
end;

procedure TSVGGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  inherited;

end;

procedure TSVGGraphic.SaveToStream(Stream: TStream);
begin
  FStream.Position := 0;
  FStream.SaveToStream(Stream);
end;


procedure TSVGIconImage.SetImageIndex(const Value: Integer);
begin
  if FImageIndex = Value then
    Exit;
  FImageIndex := Value;
  CheckAutoSize;
  Repaint;
end;

procedure TSVGIconImage.SetImageList(const Value: TSVGIconImageList);
begin
  FImageList := Value;
  SVGText := '';
end;

initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TSVGGraphic);
end.
