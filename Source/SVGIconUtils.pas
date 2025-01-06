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
unit SVGIconUtils;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes
  , Vcl.ImgList
  , SVGInterfaces
  , SVGIconImageListBase
  , SVGIconImageList
  , WinApi.Windows
  , Vcl.Graphics
  , Vcl.Imaging.pngimage
  , Vcl.ComCtrls
  ;

Type
  TPngExportSize = (es16, es32, es48, es64, es96, es128, es192, es256, esCustom);
  TPngExportSizes = Set of TPngExportSize;

  TExportToPngEvent = procedure (const ASizes: TPngExportSizes; const SVGText: string;
    const AFolder, AFormat: string; ACustomSize: Integer) of Object;

const
  AllPngExportSizes = [es16, es32, es48, es64, es96, es128, es192, es256, esCustom];

function UpdateSVGIconListView(const AListView: TListView;
  const ACategory: string = '';
  const AIncludeIndex: Boolean = True): Integer;
function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
procedure SVGExportToPng(const AWidth, AHeight: Integer;
  FSVG: ISVG; const AOutFolder: string;
  const AFileName: string = '');
function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
procedure SVGCopyToClipboardAsPng(const AWidth, AHeight: Integer; FSVG: ISVG);

implementation

uses
  System.SysUtils
  , System.Types
  , Vcl.Themes
  , SVGIconImageCollection
  , Vcl.Clipbrd
  {$IFDEF D10_3}
  , Vcl.VirtualImageList
  {$ENDIF}
  ;

// Source: http://www.entwickler-ecke.de/topic_Bitmap+pf32bit+mit+Alpha+afPremultipied+zu+PNG+speichern_103159,0.html
type
  TRGB = packed record B, G, R: byte end;
  TRGBA = packed record B, G, R, A: byte end;
  TRGBAArray = array[0..0] of TRGBA;

{$R-}
function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
var
  X, Y: integer;
  BmpRGBA: ^TRGBAArray;
  PngRGB: ^TRGB;
begin
  //201011 Thomas Wassermann
  Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

  Result.CreateAlpha;
  Result.Canvas.CopyMode:= cmSrcCopy;
  Result.Canvas.Draw(0, 0, aBitmap);

  for Y := 0 to Pred(aBitmap.Height) do
  begin
    BmpRGBA := aBitmap.ScanLine[Y];
    PngRGB:= Result.Scanline[Y];

    for X := 0 to Pred(aBitmap.width) do
    begin
      Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
      if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
      begin
        if BmpRGBA[X].A <> 0 then
        begin
          PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
          PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
          PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
        end else begin
          PngRGB^.B := Round(BmpRGBA[X].B * 255);
          PngRGB^.R := Round(BmpRGBA[X].R * 255);
          PngRGB^.G := Round(BmpRGBA[X].G * 255);
        end;
      end;
      Inc(PngRGB);
    end;
  end;
end;

procedure SVGCopyToClipboardAsPng(const AWidth, AHeight: Integer;
  FSVG: ISVG);
var
  LImagePng: TPngImage;
  LBitmap: TBitmap;
  iFormat: Word;
  iData: THandle;
  iPalette: HPALETTE;
begin
  LBitmap := nil;
  LImagePng := nil;
  try
    LBitmap := TBitmap.Create;
    LBitmap.PixelFormat := TPixelFormat.pf32bit;   // 32bit bitmap
    LBitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel

    LBitmap.SetSize(AWidth, AHeight);

    // Fill background with transparent
    LBitmap.Canvas.Brush.Color := clNone;
    LBitmap.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));

    FSVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, AWidth, AHeight));

    LImagePng := PNG4TransparentBitMap(LBitmap);

    iFormat := RegisterClipboardFormat('PNG');
    LImagePng.SaveToClipBoardFormat(iFormat, iData, iPalette);
    ClipBoard.SetAsHandle(iFormat, iData);
  finally
    LBitmap.Free;
    LImagePng.Free;
  end;
end;

procedure SVGExportToPng(const AWidth, AHeight: Integer;
  FSVG: ISVG; const AOutFolder: string;
  const AFileName: string = '');
var
  LImagePng: TPngImage;
  LBitmap: TBitmap;
  LFileName: string;
begin
  LBitmap := nil;
  LImagePng := nil;
  try
    LBitmap := TBitmap.Create;
    LBitmap.PixelFormat := TPixelFormat.pf32bit;   // 32bit bitmap
    LBitmap.AlphaFormat := TAlphaFormat.afDefined; // Enable alpha channel

    LBitmap.SetSize(AWidth, AHeight);

    // Fill background with transparent
    LBitmap.Canvas.Brush.Color := clNone;
    LBitmap.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));

    FSVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, AWidth, AHeight));

    LImagePng := PNG4TransparentBitMap(LBitmap);
    LFileName := IncludeTrailingPathDelimiter(AOutFolder)+
      StringReplace(AFileName, '\', '_',[rfReplaceAll]);
    ChangeFileExt(LFileName,'.png');
    LImagePng.SaveToFile(LFileName);
  finally
    LBitmap.Free;
    LImagePng.Free;
  end;
end;

function UpdateSVGIconListView(const AListView: TListView;
  const ACategory: string = '';
  const AIncludeIndex: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  LListItem: TListItem;
  LImageList: TCustomImageList;
  LIconItems: TSVGIconItems;

  function GetItemCaption: string;
  begin
    if AIncludeIndex then
      Result := Format('%d.%s', [LItem.Index, LItem.Name])
    else
      Result := Format('%s', [LItem.Name]);
  end;

begin
  LImageList := AListView.LargeImages as TCustomImageList;
  AListView.Items.BeginUpdate;
  try
    AListView.Clear;
    Result := 0;
    if (LImageList is TSVGIconImageListBase) then
      LIconItems := TSVGIconImageListBase(LImageList).SVGIconItems
    {$IFDEF D10_3}
    else if (LImageList is TVirtualImageList) and
      (TVirtualImageList(LImageList).ImageCollection is TSVGIconImageCollection) then
      LIconItems := TSVGIconImageCollection(TVirtualImageList(LImageList).ImageCollection).SVGIconItems
    {$ENDIF}
    else
      Exit;
    Result := LIconItems.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconItems[I];
      if (ACategory = '') or
       (LowerCase(ACategory) = LowerCase(LItem.Category)) then
      begin
        LListItem := AListView.Items.Add;
        LListItem.Caption := GetItemCaption;
        LListItem.ImageIndex := I;
      end;
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  {$IFDEF D10_3}
  LVirtualItem: TVirtualImageListItem;
  {$ENDIF}
  LListItem: TListItem;
  LImageList: TCustomImageList;
begin
  LImageList := AListView.LargeImages as TCustomImageList;
  AListView.Items.BeginUpdate;
  try
    Result := LImageList.Count;
    for I := 0 to Result -1 do
    begin
      if (LImageList is TSVGIconImageListBase) then
      begin
        LItem := TSVGIconImageListBase(LImageList).SVGIconItems[I];
        LListItem := AListView.Items[I];
        if AShowCaption then
        begin
          LListItem.Caption := Format('%d.%s',
            [LItem.Index, LItem.IconName]);
        end
        else
          LListItem.Caption := '';
      end;
      {$IFDEF D10_3}
      if (LImageList is TVirtualImageList) then
      begin
        LVirtualItem := TVirtualImageList(LImageList).Images.Items[I];
        LListItem := AListView.Items[I];
        if AShowCaption then
        begin
          LListItem.Caption := Format('%d.%s',
            [LVirtualItem.Index, LVirtualItem.Name]);
        end
        else
          LListItem.Caption := '';
      end;
      {$ENDIF}
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;


end.
