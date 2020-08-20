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
unit SVGIconImageCollection;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Types
  , System.UITypes
  , System.Classes
  , SVGTypes
  , SVG
  , SVGColor
  , WinApi.Windows
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
{$IFDEF D10_3+}
  , Graphics
  , BaseImageCollection
{$ENDIF}
  , SVGIconItems
  ;

//alias to make usage simpler (less uses clause entries).
type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;

type
  {$IFDEF D10_3+}
  TSVGIconImageCollection = class(TCustomImageCollection)
  {$ELSE}
  TSVGIconImageCollection = class(TComponent)
  {$ENDIF}
  private
    FSVGItems: TSVGIconItems;
    FStoreAsText : boolean;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    procedure SetSVGIconItems(const Value: TSVGIconItems);
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);

  protected
    {$IFDEF D10_3+}
    function GetCount: Integer; override;
    {$ENDIF}

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);

    procedure DefineProperties(Filer: TFiler); override;

  public
    {$IFDEF D10_3+}
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; override;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); override;
    {$ENDIF}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(const ASVG: TSVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TColor = SVG_INHERIT_COLOR): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): Integer;
    procedure ClearIcons;

    procedure LoadFromResource(const hInstance : THandle; const resourceName : string; const iconName : string);

  published
    property SVGIconItems: TSVGIconItems read FSVGItems write SetSVGIconItems stored FStoreAsText;
    property StoreAsText: boolean read FStoreAsText write FStoreAsText default False;
    property FixedColor: TColor read FFixedColor write SetFixedColor default SVG_INHERIT_COLOR;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

implementation

uses
  SVGCommon,
  System.SysUtils;

{ TSVGIconImageCollection }

function TSVGIconImageCollection.Add(const ASVG: TSVG; const AIconName: string;
  const AGrayScale: Boolean; const AFixedColor: TColor): Integer;
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

procedure TSVGIconImageCollection.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageCollection then
  begin
    FStoreAsText := TSVGIconImageCollection(Source).FStoreAsText;
    FFixedColor := TSVGIconImageCollection(Source).FFixedColor;
    FGrayScale := TSVGIconImageCollection(Source).FGrayScale;
    FSVGItems.Assign(TSVGIconImageCollection(Source).SVGIconItems)
  end
  else if Source is TSVGIconItems then
    FSVGItems.Assign(Source)
  else
    inherited;
end;

procedure TSVGIconImageCollection.ClearIcons;
begin
  FSVGItems.Clear;
end;

constructor TSVGIconImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
  FStoreAsText := false;
  FFixedColor := SVG_INHERIT_COLOR;
  FGrayScale := False;
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

  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, True);

end;

procedure TSVGIconImageCollection.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems.Delete(Index);
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

procedure TSVGIconImageCollection.LoadFromResource(const hInstance: THandle; const resourceName, iconName: string);
var
  resStream: TResourceStream;
  svg : TSVG;
begin
  resStream := TResourceStream.Create(hInstance, resourceName, RT_RCDATA);
  try
    svg := TSVG.Create;
    try
      svg.LoadFromStream(resStream);
      Add(svg, iconName);
    except
      svg.Free;
      raise;
    end;
  finally
    resStream.Free;
  end;
end;

procedure TSVGIconImageCollection.ReadImageData(Stream: TStream);
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
  LFixedColor: TColor;
begin
  if FStoreAsText then
    Exit;

  FSVGItems.BeginUpdate;
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
    end;
    LStream.Free;
    LSVG.Free;
  finally
    FSVGItems.EndUpdate;
  end;
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

procedure TSVGIconImageCollection.SetSVGIconItems(const Value: TSVGIconItems);
begin
  //shouldn't this use assign?
  //FSVGItems := Value;

  if FSVGItems <> Value then
  begin
    FSVGItems.Assign(Value);
  end;
end;

procedure TSVGIconImageCollection.WriteImageData(Stream: TStream);
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
    if LItem.FixedColor <> SVG_INHERIT_COLOR then
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

procedure TSVGIconImageCollection.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageCollection.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{$IFDEF D10_3+}
function UpdateRectForProportionalSize(ARect: TRect; AWidth, AHeight: Integer; AStretch: Boolean): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  Result := ARect;
  if AWidth * AHeight = 0 then
    Exit;

  w := AWidth;
  h := AHeight;
  cw := ARect.Width;
  ch := ARect.Height;

  if AStretch or ((w > cw) or (h > ch)) then
  begin
    xyaspect := w / h;
    if w > h then
    begin
      w := cw;
      h := Trunc(cw / xyaspect);
      if h > ch then
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
      end;
     end
     else
     begin
       h := ch;
       w := Trunc(ch * xyaspect);
       if w > cw then
       begin
         w := cw;
         h := Trunc(cw / xyaspect);
       end;
     end;
  end;

  Result := Rect(0, 0, w, h);
  OffsetRect(Result, ARect.Left + (cw - w) div 2, ARect.Top + (ch - h) div 2);
end;

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
    Result := FSVGItems[AIndex].IconName;
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
    Result := FSVGItems[AIndex].GetBitmap(AWidth, AHeight, FFixedColor, 255, FGrayScale)
  else
    Result := nil;
end;

procedure TSVGIconImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
  AProportional: Boolean = False);
var
  LItem: TSVGIconItem;
  LSVG: TSVG;
begin
  LItem := FSVGItems.Items[AIndex];
  LSVG := LItem.SVG;
  if LItem.FixedColor <> SVG_INHERIT_COLOR then
    LSVG.FixedColor := LItem.FixedColor
  else
    LSVG.FixedColor := FFixedColor;
  if LItem.GrayScale or FGrayScale then
    LSVG.Grayscale := True
  else
    LSVG.Grayscale := False;
  LSVG.SVGOpacity := 1;
  if AProportional then
    ARect := UpdateRectForProportionalSize(ARect, ARect.Width, ARect.Height, True);
  LSVG.PaintTo(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;
{$ENDIF}

end.
