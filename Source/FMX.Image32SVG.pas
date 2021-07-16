{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/FMX                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
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
unit FMX.Image32SVG;

interface

Uses
  System.Types
  , System.UIConsts
  , System.UITypes
  , System.SysUtils
  , System.Classes
  , System.Rtti
  , System.Messaging
  , FMX.MultiResBitmap
  , FMX.Types
  , FMX.Graphics
  , FMX.Objects
  , Image32             //Warning: from version 2.3 the default rendering engine is Image32
  , Image32_SVG_Core    //because is the best engine available with SVGIconImageList.
  , Image32_SVG_Reader  //If you don't want to use it change SVGIconImageList.inc
  , Image32_SVG_Writer  //Otherwise you must add two search path:
  , Image32_Ttf         //- SVGIconImageList\Images32\Source
  ;                     //- SVGIconImageList\Images32\Source\Image32_SVG

resourcestring
  SVG_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  SVG_ERROR_TAG_SVG = 'Error: Tag "svg" not found.';

type
  TFmxImage32SVG = class(TObject)
  private
    fSvgReader: TSvgReader;
    FSource: String;
    FWidth: Single;
    FHeight: Single;
    FFixedColor: TColor32;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FOpacity: Single;
    FImage32: TImage32;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TAlphaColor;
    procedure SetFixedColor(const AAlphaColor: TAlphaColor);
    function GetApplyFixedColorToRootOnly: Boolean;
    procedure SetApplyFixedColorToRootOnly(Value:Boolean);
    function GetSource: string;
    procedure SetSource(const ASource: string);
    // procedures and functions
    procedure Clear;
    procedure LoadFromSource;
    procedure SourceFromStream(Stream: TStream);
    procedure SetHeight(const Value: Single);
    procedure SetWidth(const Value: Single);
    {$IFDEF CheckForUnsupportedSvg}
    procedure CheckForUnsupportedSvg;
    {$ENDIF}
  public
    function IsEmpty: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromText(const ASVGText: string);
    procedure PaintToBitmap(ABitmap: TBitmap;
      const KeepAspectRatio: Boolean);
    property Opacity: Single read GetOpacity write SetOpacity;
    property FixedColor: TAlphaColor read GetFixedColor write SetFixedColor;
    property GrayScale: Boolean read GetGrayScale write SetGrayScale;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Source: string read GetSource;
    property ApplyFixedColorToRootOnly: Boolean read GetApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly;
  end;

procedure CopyImage32ToFmxBitmap(AImage32: TImage32; ABitmap: TBitmap);

implementation

function AlphaColor32(AlphaColor: TAlphaColor): TColor32;
var
  R: TAlphaColorRec;
  res: TARGB absolute Result;
begin
  R.Color := AlphaColor;
  res.A := R.A;
  res.R := R.R;
  res.G := R.G;
  res.B := R.B;
end;

procedure CopyImage32ToFmxBitmap(AImage32: TImage32; ABitmap: TBitmap);
var
  LSource, LDest: TBitmapData;
begin
  LSource := TBitMapData.Create(AImage32.Width, AImage32.Height, TPixelFormat.BGRA);
  LSource.Data := AImage32.PixelBase;
  LSource.Pitch := AImage32.Width * 4;
  if not Assigned(AImage32) or not Assigned(ABitmap) then Exit;
  ABitmap.SetSize(AImage32.Width, AImage32.Height);
  if ABitmap.Map(TMapAccess.Write, LDest) then
  try
    LDest.Copy(LSource);
  finally
    ABitmap.Unmap(LDest);
  end;
end;

{ TFmxImage32SVG }
procedure TFmxImage32SVG.Clear;
Const
  EmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
begin
  SetSource(EmptySvg);
end;

constructor TFmxImage32SVG.Create;
begin
  inherited;
  fSvgReader := TSvgReader.Create;
  FImage32 := TImage32.Create;
  FImage32.Resampler := rBicubicResampler;
  FFixedColor := clNone32;
  FOpacity := 1.0;
end;

destructor TFmxImage32SVG.Destroy;
begin
  fSvgReader.Free;
  FImage32.Free;
  inherited;
end;

procedure TFmxImage32SVG.LoadFromFile(const FileName: string);
Var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TFmxImage32SVG.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FApplyFixedColorToRootOnly;
end;

function TFmxImage32SVG.GetFixedColor: TAlphaColor;
begin
  Result := FFixedColor;
end;

function TFmxImage32SVG.GetGrayScale: Boolean;
begin
  Result := FGrayScale;
end;

function TFmxImage32SVG.GetHeight: Single;
begin
  Result := FHeight;
end;

function TFmxImage32SVG.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TFmxImage32SVG.GetSource: string;
begin
  Result := FSource;
end;

function TFmxImage32SVG.GetWidth: Single;
begin
  Result := FWidth;
end;

function TFmxImage32SVG.IsEmpty: Boolean;
begin
  Result := fSvgReader.IsEmpty;
end;

procedure TFmxImage32SVG.LoadFromSource;
begin
  fSvgReader.LoadFromString(FSource);
end;

procedure TFmxImage32SVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
begin
  // read and save the Source
  OldPos := Stream.Position;
  SourceFromStream(Stream);
  // Restore Position
  Stream.Position := OldPos;
  // Now create the SVG
  FImage32.LoadFromStream(Stream);
end;

procedure TFmxImage32SVG.LoadFromText(const ASVGText: string);
var
  LOldText: string;
begin
  LOldText := FSource;
  Clear;
  if ASVGText = '' then Exit;
  try
    FSource := ASVGText;
    LoadFromSource;
  except
    on E: Exception do
    begin
      FSource := LOldText;
      raise Exception.CreateFmt(SVG_ERROR_PARSING_SVG_TEXT, [E.Message]);
    end;
  end;
end;

procedure TFmxImage32SVG.PaintToBitmap(ABitmap: TBitmap;
  const KeepAspectRatio: Boolean);
begin
  Assert(Assigned(FImage32));
  Assert(Assigned(ABitmap));

  //Define Image32 output size
  FImage32.SetSize(ABitmap.Width, ABitmap.Height);

  //Draw SVG image to Image32
  FsvgReader.DrawImage(FImage32, True);

  //GrayScale and FixedColor applyed to Image32
  if FGrayScale then
    FImage32.Grayscale
  else if (FFixedColor <> clNone32) then
  begin
    if FApplyFixedColorToRootOnly then
    begin
      fSvgReader.RootElement.SetFillColor(FFixedColor);
      with fSvgReader.RootElement.DrawData do
        if (strokeColor <> clInvalid) and (strokeColor <> clNone32) then
          fSvgReader.RootElement.SetStrokeColor(FFixedColor);
    end
    else
      FImage32.SetRGB(FFixedColor);
  end;

  //Opacity applyed to Image32
  if FOpacity <> 1.0 then
    FImage32.ReduceOpacity(Round(FOpacity * 255));

  //Copy Image to Bitmap
  CopyImage32ToFmxBitmap(FImage32, ABitmap);
end;

procedure TFmxImage32SVG.SaveToFile(const FileName: string);
Var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TFmxImage32SVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

procedure TFmxImage32SVG.SetApplyFixedColorToRootOnly(Value: Boolean);
var
  Color: TColor;
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    if FFixedColor <> clNone32 then
    begin
       Color := FFixedColor;
       FFixedColor := clNone32;
       LoadFromSource;
       SetFixedColor(Color);
    end;
  end;
end;

procedure TFmxImage32SVG.SetFixedColor(const AAlphaColor: TAlphaColor);
var
  LColor: TColor32;
begin
  LColor := AlphaColor32(AAlphaColor);
  if LColor = FFixedColor then Exit;
  if (FGrayScale and (LColor <> clNone32)) or
    ((FFixedColor <> clNone32) and (LColor = clNone32))
  then
    LoadFromSource;
  FFixedColor := LColor;
  FGrayScale := False;
end;

procedure TFmxImage32SVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = FGrayScale then Exit;
  if FGrayScale or (FFixedColor <> clNone32) then
    LoadFromSource;
  FGrayScale := IsGrayScale;
  FFixedColor := clNone32;
end;

procedure TFmxImage32SVG.SetHeight(const Value: Single);
begin
  FHeight := Value;
end;

procedure TFmxImage32SVG.SetOpacity(const Opacity: Single);
begin
  FOpacity := Opacity;
end;

procedure TFmxImage32SVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

procedure TFmxImage32SVG.SetWidth(const Value: Single);
begin
  FWidth := Value;
end;

procedure TFmxImage32SVG.SourceFromStream(Stream: TStream);
var
  LStream: TStringStream;
begin
  fSvgReader.LoadFromStream(Stream);
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    FSource := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

initialization
  FontManager.Load('Arial');
  FontManager.Load('Times New Roman');

end.
