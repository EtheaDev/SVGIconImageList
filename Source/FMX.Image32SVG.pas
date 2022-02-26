{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/FMX                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2022 (Ethea S.r.l.)                                 }
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
  , FMX.ImageSVG
  , Img32             //Warning: from version 2.3 the default rendering engine is Image32
  , Img32.SVG.Core    //because is the best engine available with SVGIconImageList.
  , Img32.SVG.Reader
  , Img32.FMX
  , Img32.Text        //You must add this search path:
  , Img32.Vector;     //- SVGIconImageList\Image32\Source

type
  TFmxImage32SVG = class(TFmxImageSVG)
  private
    fSvgReader: TSvgReader;
    FImage32: TImage32;
  protected
    //Abstract methods
    procedure LoadFromSource; override;
    procedure LoadFromStream(Stream: TStream); override;
  public
    //Abstract methods
    function IsEmpty: Boolean; override;
    procedure PaintToBitmap(ABitmap: TBitmap;
      const AZoom: Integer = 100; const KeepAspectRatio: Boolean = True); override;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

function AlphaToColor32(AlphaColor: TAlphaColor): TColor32;
var
  res: TARGB;
begin
  res.A := TAlphaColorRec(AlphaColor).A;
  res.R := TAlphaColorRec(AlphaColor).R;
  res.G := TAlphaColorRec(AlphaColor).G;
  res.B := TAlphaColorRec(AlphaColor).B;
  Result := res.Color;
{$IF Defined(ANDROID) or Defined(MACOS) or Defined(MACOSX)}
  Result := SwapRedBlue(Result);
{$IFEND}
end;

{ TFmxImage32SVG }
constructor TFmxImage32SVG.Create;
begin
  inherited Create;
  fSvgReader := TSvgReader.Create;
  FImage32 := TImage32.Create;
end;

destructor TFmxImage32SVG.Destroy;
begin
  fSvgReader.Free;
  FImage32.Free;
  inherited;
end;

function TFmxImage32SVG.IsEmpty: Boolean;
begin
  Result := fSvgReader.IsEmpty;
end;

procedure TFmxImage32SVG.LoadFromSource;
begin
  fSvgReader.LoadFromString(Source);
end;

procedure TFmxImage32SVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
  LStream: TStringStream;
begin
  // read and save the Source
  OldPos := Stream.Position;
  fSvgReader.LoadFromStream(Stream);
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    SetSource(LStream.DataString);
  finally
    LStream.Free;
  end;
  // Restore Position
  Stream.Position := OldPos;
  // Now create the SVG
  FImage32.LoadFromStream(Stream);
end;

procedure TFmxImage32SVG.PaintToBitmap(ABitmap: TBitmap;
  const AZoom: Integer = 100; const KeepAspectRatio: Boolean = True);
var
  LColor: TColor32;
  LWidth, LHeight: Integer;

  procedure Image32ToFmxBitmap(const ASource: TImage32; const ATarget: FMX.Graphics.TBitmap);
  var
    LStream: TMemoryStream;
  begin
    LStream := TMemoryStream.Create;
    try
        ASource.SaveToStream(LStream, 'BMP');
        LStream.Position := 0;
        ATarget.LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;

begin
  Assert(Assigned(FImage32));
  Assert(Assigned(ABitmap));

  LWidth := Round(ABitmap.Width * AZoom / 100);
  LHeight := Round(ABitmap.Height * AZoom / 100);

  //Define Image32 output size
  FImage32.SetSize(LWidth, LHeight);

  //Update FsvgReader BEFORE calling FsvgReader.DrawImage
  if ApplyFixedColorToRootOnly and not GrayScale and
    (AlphaToColor32(FixedColor) <> clNone32) then
      LColor := AlphaToColor32(FixedColor)
  else
    LColor := clNone32;

  fSvgReader.SetOverrideFillColor(LColor);
  fSvgReader.SetOverrideStrokeColor(LColor);

  FsvgReader.KeepAspectRatio := KeepAspectRatio;

  //Draw SVG image to FImage32
  FsvgReader.DrawImage(FImage32, True);

  //Apply GrayScale and FixedColor to Image32
  if GrayScale then
    FImage32.Grayscale
  else if (AlphaToColor32(FixedColor) <> clNone32) and
    not ApplyFixedColorToRootOnly then
      FImage32.SetRGB(AlphaToColor32(FixedColor));

  //Opacity applyed to Image32
  if Opacity <> 1.0 then
    FImage32.ReduceOpacity(Round(Opacity * 255));

  //Copy Image32 to Bitmap
  Image32ToFmxBitmap(FImage32, ABitmap);

  //AssignImage32ToFmxBitmap(FImage32, ABitmap);
end;

initialization
{$IFDEF MSWINDOWS}
  FontManager.Load('Arial');
  FontManager.Load('Times New Roman');
{$ENDIF}

end.
