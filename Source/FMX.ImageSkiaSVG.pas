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
unit FMX.ImageSkiaSVG;

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
  , System.Skia       
  , FMX.Skia;

type
  TFmxImageSkiaSVG = class(TFmxImageSVG)
  private
    FSvg: TSkSvgBrush;
    procedure DeleteBuffers;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
      const AOpacity: Single);
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

{ TFmxImageSkiaSVG }
constructor TFmxImageSkiaSVG.Create;
begin
  inherited Create;
  FSvg := TSkSvgBrush.Create;
end;

procedure TFmxImageSkiaSVG.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  inherited;
  //GrayScale and FixedColor
  FSvg.GrayScale := GrayScale;
  if not GrayScale and (FixedColor <> TAlphaColorRec.Null) and
    (FixedColor <> TAlphaColorRec.Null) then
    FSvg.OverrideColor := FixedColor
  else
    FSvg.OverrideColor := Default(TAlphaColor);

  FSvg.Render(ACanvas, ADest, AOpacity);
end;

procedure TFmxImageSkiaSVG.DeleteBuffers;
begin
  ;
end;

destructor TFmxImageSkiaSVG.Destroy;
begin
  FSvg.Free;
  inherited;
end;

function TFmxImageSkiaSVG.IsEmpty: Boolean;
begin
  Result := (FSvg.Source = '') or (FSvg.DOM = nil);
end;

procedure TFmxImageSkiaSVG.LoadFromSource;
begin
  FSvg.Source := Source;
end;

procedure TFmxImageSkiaSVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
  LStream: TStringStream;
begin
  // read and save the Source
  OldPos := Stream.Position;
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    DeleteBuffers;
    LStream.Position := 0;
    FSvg.Source := LStream.DataString;
  finally
    LStream.Free;
    // Restore Position
    Stream.Position := OldPos;
    DeleteBuffers;
  end;
end;

procedure TFmxImageSkiaSVG.PaintToBitmap(ABitmap: TBitmap;
  const AZoom: Integer = 100; const KeepAspectRatio: Boolean = True);
var
  LWidth, LHeight: Integer;
begin
  Assert(Assigned(FSvg));
  Assert(Assigned(ABitmap));

  LWidth := Round(ABitmap.Width * AZoom / 100);
  LHeight := Round(ABitmap.Height * AZoom / 100);
  ABitmap.SetSize(LWidth, LHeight);
  ABitmap.SkiaDraw(
    procedure(const ACanvas: ISkCanvas)
    var
      LDestRect: TRectF;
    begin
      LDestRect := RectF(0, 0, LWidth, LHeight);
      Draw(ACanvas, LDestRect, Opacity);
    end);
end;

end.
