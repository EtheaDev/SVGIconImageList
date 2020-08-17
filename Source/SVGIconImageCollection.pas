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
  System.Types,
  System.UITypes,
  System.Classes,
  SVGTypes,
  SVG,
  SVGColor,
  SVGIconItems;

//alias to make usage simpler (less uses clause entries).
type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;

type
  TSVGIconImageCollection = class;

  TSVGIconImageCollection = class(TComponent)
  private
    FSVGItems: TSVGIconItems;
    FStoreAsText : boolean;
    procedure SetSVGIconItems(const Value: TSVGIconItems);

  protected
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);

    procedure DefineProperties(Filer: TFiler); override;

  public
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

  end;

implementation

uses
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
    FStoreAsText := TSVGIconImageCollection(Source).StoreAsText;
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

end.
