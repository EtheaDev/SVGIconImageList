{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/FMX                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
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
unit FMX.ImageSVG;

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
  , FMX.Objects;

resourcestring
  SVG_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  SVG_ERROR_TAG_SVG = 'Error: Tag "svg" not found.';

type
  TFmxImageSVG = class(TObject)
  private
    FSource: String;
    FFixedColor: TAlphaColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FOpacity: Single;
    // property access methods
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TAlphaColor;
    procedure SetFixedColor(const AColor: TAlphaColor);
    function GetApplyFixedColorToRootOnly: Boolean;
    procedure SetApplyFixedColorToRootOnly(Value:Boolean);
    function GetSource: string;
    // procedures and functions
    procedure Clear;
    {$IFDEF CheckForUnsupportedSvg}
    procedure CheckForUnsupportedSvg;
    {$ENDIF}
  protected
    procedure SetSource(const ASource: string);
    //Abstract methods
    procedure LoadFromSource; virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
  public
    //Abstract methods
    function IsEmpty: Boolean; virtual; abstract;
    procedure PaintToBitmap(ABitmap: TBitmap;
      const AZoom: Integer = 100; const KeepAspectRatio: Boolean = True); virtual; abstract;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromText(const ASVGText: string);
    property Opacity: Single read GetOpacity write SetOpacity;
    property FixedColor: TAlphaColor read GetFixedColor write SetFixedColor;
    property GrayScale: Boolean read GetGrayScale write SetGrayScale;
    property Source: string read GetSource;
    property ApplyFixedColorToRootOnly: Boolean read GetApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly;
  end;

implementation

{ TFmxImageSVG }
procedure TFmxImageSVG.Clear;
Const
  EmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
begin
  SetSource(EmptySvg);
end;

constructor TFmxImageSVG.Create;
begin
  inherited;
  FFixedColor := TAlphaColorRec.Null;
  FOpacity := 1.0;
end;

destructor TFmxImageSVG.Destroy;
begin
  inherited;
end;

procedure TFmxImageSVG.LoadFromFile(const FileName: string);
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

function TFmxImageSVG.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FApplyFixedColorToRootOnly;
end;

function TFmxImageSVG.GetFixedColor: TAlphaColor;
begin
  Result := FFixedColor;
end;

function TFmxImageSVG.GetGrayScale: Boolean;
begin
  Result := FGrayScale;
end;
function TFmxImageSVG.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TFmxImageSVG.GetSource: string;
begin
  Result := FSource;
end;
procedure TFmxImageSVG.LoadFromText(const ASVGText: string);
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

procedure TFmxImageSVG.SaveToFile(const FileName: string);
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

procedure TFmxImageSVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

procedure TFmxImageSVG.SetApplyFixedColorToRootOnly(Value: Boolean);
var
  Color: TColor;
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    if FFixedColor <> TAlphaColorRec.Null then
    begin
       Color := FFixedColor;
       FFixedColor := TAlphaColorRec.Null;
       LoadFromSource;
       SetFixedColor(Color);
    end;
  end;
end;

procedure TFmxImageSVG.SetFixedColor(const AColor: TAlphaColor);
begin
  if AColor = FFixedColor then Exit;
  if (FGrayScale and (AColor <> TAlphaColorRec.Null)) or
    ((FFixedColor <> TAlphaColorRec.Null) and (AColor = TAlphaColorRec.Null))
  then
    LoadFromSource;
  FFixedColor := AColor;
  FGrayScale := False;
end;

procedure TFmxImageSVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = FGrayScale then Exit;
  if FGrayScale or (FFixedColor <> TAlphaColorRec.Null) then
    LoadFromSource;
  FGrayScale := IsGrayScale;
  FFixedColor := TAlphaColorRec.Null;
end;
procedure TFmxImageSVG.SetOpacity(const Opacity: Single);
begin
  FOpacity := Opacity;
end;

procedure TFmxImageSVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

end.
