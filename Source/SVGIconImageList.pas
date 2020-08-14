{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
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
unit SVGIconImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Classes
  , SysUtils
  , ImgList
  , Windows
  , Graphics
{$IFDEF D10_4+}
  , System.UITypes
{$ENDIF}
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
  , Forms
  , SVG
  , SVGColor
  , SVGIconItems
  , SVGIconImageListBase;

type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;

  TSVGIconImageList = class;


  {TSVGIconImageList}
  TSVGIconImageList = class(TSVGIconImageListBase)
  private
    FSVGItems: TSVGIconItems;
    FStoreAsText: boolean;
  protected
    function GetSVGIconItems: TSVGIconItems; override;
    //procedure SetSVGIconItems(const Value: TSVGIconItems); override;
    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(const Source: TPersistent); override;
  public
    procedure RecreateBitmaps;override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ASVG: TSVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TSVGColor = inherit_color): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    procedure ClearIcons;override;
    procedure SaveToFile(const AFileName: string);
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property SVGIconItems stored FStoreAsText;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width;
    property Height;
    property Size;
    property StoreAsText: boolean read FStoreAsText write FStoreAsText default False;
    property FixedColor: TSVGColor read FFixedColor write SetFixedColor default TSVGColor.inherit_color;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;
    /// <summary>
    /// Enable and disable scaling with form
    /// </summary>
    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  CommCtrl
  , Math
  , Winapi.GDIPAPI
  , ComCtrls
  , SVGTypes
  , SVGCommon
  , SVGIconVirtualImageList;


{ TSVGIconImageList }

function TSVGIconImageList.Add(const ASVG: TSVG;
  const AIconName: string; const AGrayScale: Boolean = False;
  const AFixedColor: TSVGColor = inherit_color): Integer;
var
  Item: TSVGIconItem;
begin
  FSVGItems.BeginUpdate;
  try
    Item := FSVGItems.Add;
    Item.SVG := ASVG;
    Item.IconName := AIconName;
    Item.FixedColor := AFixedColor;
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
  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, True);
end;


procedure TSVGIconImageList.DoAssign(const Source: TPersistent);
var
  virtualList : TSVGIconVirtualImageList;
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    FSVGItems.Assign(TSVGIconImageList(Source).FSVGItems);
    FStoreAsText := TSVGIconImageList(Source).FStoreAsText;
  end
  else if Source is TSVGIconVirtualImageList then
  begin
    virtualList := TSVGIconVirtualImageList(Source);
    if virtualList.Collection <> nil then
    begin
      FSVGItems.Assign(virtualList.Collection.SVGIconItems);
      FStoreAsText := virtualList.Collection.StoreAsText;
    end;
  end;
end;

procedure TSVGIconImageList.PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True);
var
  R: TGPRectF;
  SVG: TSVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (AIndex >= 0) and (AIndex < FSVGItems.Count) then
  begin
    LItem := FSVGItems[AIndex];
    SVG := LItem.SVG;
    if LItem.FixedColor <> inherit_color then
      SVG.FixedColor := LItem.FixedColor
    else
      SVG.FixedColor := FFixedColor;
    LOpacity := FOpacity;
    if AEnabled then
    begin
      if LItem.GrayScale or FGrayScale then
        SVG.Grayscale := True
      else
        SVG.Grayscale := False;
    end
    else
    begin
      if FDisabledGrayScale then
        SVG.Grayscale := True
      else
        LOpacity := FDisabledOpacity;
    end;
    SVG.SVGOpacity := LOpacity / 255;
    R := FittedRect(MakeRect(X, Y, AWidth, AHeight), SVG.Width, SVG.Height);
    SVG.PaintTo(ACanvas.Handle, R, nil, 0);
    SVG.SVGOpacity := 1;
  end;
end;

procedure TSVGIconImageList.ReadImageData(Stream: TStream);
var
  LStream: TMemoryStream;
  LCount, LSize: Integer;
  LSVG: TSVG;
  C: Integer;
  LPos: Int64;
  LIconName: string;
  LTag: TBytes;
  LFixedColorStr: AnsiString;
  LGrayScale: Boolean;
  LFixedColor: TSVGColor;
begin
  if FStoreAsText then
    Exit;

  BeginUpdate;
  try
    LStream := TMemoryStream.Create;
    //Read Count of Images
    Stream.Read(LCount, SizeOf(Integer));
    LSVG := TSVG.Create(nil);
    for C := 0 to LCount - 1 do
    begin
      //Read IconName
      Stream.Read(LSize, SizeOf(Integer));
      SetLength(LIconName, LSize);
      Stream.Read(PChar(LIconName)^, LSize * SizeOf(Char));
      //Read SVG Stream Size
      Stream.Read(LSize, SizeOf(Integer));
      LStream.CopyFrom(Stream, LSize);
      //Read SVG Stream data
      LSVG.LoadFromStream(LStream);

      //Check for FixedColor attribute
      LPos := Stream.Position;
      LFixedColor := SVGColor.inherit_color;
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
    end;
    LStream.Free;
    LSVG.Free;
  finally
    EndUpdate;
  end;
end;

procedure TSVGIconImageList.RecreateBitmaps;
var
  C: Integer;
  SVG: TSVG;
  Icon: HIcon;
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
      SVG := LItem.SVG;
      if Assigned(SVG) then
      begin
        if LItem.FixedColor <> inherit_color then
          SVG.FixedColor := LItem.FixedColor
        else
          SVG.FixedColor := FFixedColor;
        if LItem.GrayScale or FGrayScale then
          SVG.Grayscale := True
        else
          SVG.Grayscale := False;
        Icon := SVGToIcon(SVG);
        ImageList_AddIcon(Handle, Icon);
        DestroyIcon(Icon);
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
  Result := fSVGItems;
end;

procedure TSVGIconImageList.WriteImageData(Stream: TStream);
var
  Count, Size: Integer;
  SVG: TSVG;
  LIconName: string;
  LTag: AnsiString;
  LItem: TSVGIconItem;
  C: Integer;
  SVGStream: TMemoryStream;
begin
  if FStoreAsText then
    Exit;
  Count := FSVGItems.Count;
  //Store count
  Stream.Write(Count, SizeOf(Integer));

  SVGStream := TMemoryStream.Create;
  for C := 0 to Count - 1 do
  begin
    LItem := FSVGItems[C];
    //Store IconName
    LIconName := LItem.IconName;
    SVG := FSVGItems[C].SVG;
    Size := Length(LIconName);
    Stream.Write(Size, SizeOf(Integer));
    Stream.WriteBuffer(PChar(LIconName)^, Size * SizeOf(Char));
    //Store SVG Stream Size
    SVG.SaveToStream(SVGStream);
    Size := SVGStream.Size;
    Stream.Write(Size, SizeOf(Integer));
    SVGStream.Position := 0;
    //Store SVG Data
    Stream.CopyFrom(SVGStream, Size);
    //Store FixedColor (optionally)
    if LItem.FixedColor <> TSVGColor.inherit_color then
    begin
      LTag := 'FixedColor';
      Stream.WriteBuffer(PAnsiChar(LTag)^, 10);
      Size := Ord(LItem.FixedColor);
      Stream.Write(Size, SizeOf(Integer));
    end;
    //Store GrayScale (optionally)
    if LItem.GrayScale then
    begin
      LTag := 'GrayScale';
      Stream.WriteBuffer(PAnsiChar(LTag)^, 9);
    end;
    SVGStream.Clear;
  end;
  SVGStream.Free;
end;


end.
