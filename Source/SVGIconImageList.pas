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
  , SVG;

const
  SVGIconImageListVersion = '1.1.0';
  DEFAULT_SIZE = 16;

type
  TSVGIconItem = class(TCollectionItem)
  private
    FIconName: string;
    FSVG: TSVG;
    procedure SetIconName(const Value: string);
    procedure SetSVG(const Value: TSVG);
    procedure SetSVGText(const Value: string);
    function GetSVGText: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property SVG: TSVG read FSVG write SetSVG;
  published
    property IconName: string read FIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

  {TSVGIconItems}
  TSVGIconItems = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSVGIconItem;
    procedure SetItem(Index: Integer; const Value: TSVGIconItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSVGIconItem;
    procedure Assign(Source: TPersistent); override;
    function GetIconByName(const AIconName: string): TSVGIconItem;
    property Items[index: Integer]: TSVGIconItem read GetItem write SetItem; default;
  end;

  {TSVGIconImageList}
  TSVGIconImageList = class(TCustomImageList)
  private
    FStopDrawing: Integer;
    FSVGItems: TSVGIconItems;
    FOpacity: Byte;
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    function GetImages(Index: Integer): TSVG;
    function GetNames(Index: Integer): string;
    procedure SetImages(Index: Integer; const Value: TSVG);
    procedure SetNames(Index: Integer; const Value: string);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function SVGToIcon(const SVG: TSVG): HICON;
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);
  private
    FStoreAsText: boolean;
    procedure SetSVGIconItems(const Value: TSVGIconItems);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: Messaging.TMessage);
    {$ENDIF}
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    function GetCount: Integer; override;
    procedure Loaded; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure StopDrawing(const AStop: Boolean);
    procedure Change; override;
    procedure RecreateBitmaps;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const SVG: TSVG; const Name: string): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): Integer;
    procedure ClearIcons;
    procedure PaintTo(const DC: HDC; const Index: Integer;
      const X, Y, Width, Height: Double); overload;
    procedure PaintTo(const DC: HDC; const Name: string;
      const X, Y, Width, Height: Double); overload;
    function LoadFromFiles(const AFileNames: TStrings;
      const AAppend: Boolean = True): Integer;
    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function IsScaled: Boolean; override;
    function GetIndexByName(const AName: TImageName): TImageIndex; override;
    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}
    property Images[Index: Integer]: TSVG read GetImages write SetImages;
    property Names[Index: Integer]: string read GetNames write SetNames;
    property Count: Integer read GetCount;
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property SVGIconItems: TSVGIconItems read FSVGItems write SetSVGIconItems stored FStoreAsText;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width: Integer read GetWidth write SetWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight default DEFAULT_SIZE;
    property Size: Integer read GetSize write SetSize default DEFAULT_SIZE;
    property StoreAsText: boolean read FStoreAsText write FStoreAsText default False;
    /// <summary>
    /// Enable and disable scaling with form
    /// </summary>
    {$IFDEF HiDPISupport}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
  end;

implementation

uses
  CommCtrl
  , Math
  , GDIPAPI
  , GDIPOBJ
  , ComCtrls
  , GDIPUtils
  , SVGTypes;

{ TSVGIconItem }

procedure TSVGIconItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconItem then
  begin
    FIconName := TSVGIconItem(Source).FIconName;
    FSVG.LoadFromText(TSVGIconItem(Source).FSVG.Source);
  end;
end;

procedure TSVGIconItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGIconItem then
  begin
    TSVGIconItem(Dest).FIconName := FIconName;
    TSVGIconItem(Dest).FSVG.LoadFromText(FSVG.Source);
  end;
end;

constructor TSVGIconItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSVG := TSVG.Create;
end;

destructor TSVGIconItem.Destroy;
begin
  FreeAndNil(FSVG);
  inherited;
end;

function TSVGIconItem.GetDisplayName: string;
begin
  if IconName <> '' then
    Result := Format('%s',[IconName])
  else
    Result := 'SVGIconItem';
end;

function TSVGIconItem.GetSVGText: string;
begin
  Result := SVG.Source;
end;

procedure TSVGIconItem.SetIconName(const Value: string);
begin
  FIconName := Value;
  TSVGIconItems(Collection).Update(Self);
end;

procedure TSVGIconItem.SetSVG(const Value: TSVG);
begin
  FSVG.LoadFromText(Value.Source);
  TSVGIconItems(Collection).Update(Self);
end;

procedure TSVGIconItem.SetSVGText(const Value: string);
begin
  FSVG.LoadFromText(Value);
  TSVGIconItems(Collection).Update(Self);
end;

{ TSVGIconItems }

function TSVGIconItems.Add: TSVGIconItem;
begin
  Result := TSVGIconItem(inherited Add);
end;

procedure TSVGIconItems.Assign(Source: TPersistent);
var
  C: Integer;
  Item: TSVGIconItem;
begin
  inherited;

  if Source is TSVGIconItems then
  try
    BeginUpdate;
    Clear;
    for C := 0 to TSVGIconItems(Source).Count - 1 do
    begin
      Item := Add;
      Item.Assign(TSVGIconItems(Source)[C]);
    end;
  finally
    EndUpdate;
    Update(nil);
  end;
end;

constructor TSVGIconItems.Create(AOwner: TPersistent);
begin
  inherited Create(TSVGIconItem);
  FOwner := AOwner;
end;

function TSVGIconItems.GetIconByName(const AIconName: string): TSVGIconItem;
var
  I: Integer;
  LSVGIconItem: TSVGIconItem;
begin
  Result := nil;
  for I := 0 to Count -1 do
  begin
    LSVGIconItem := Items[I];
    if SameText(LSVGIconItem.IconName, AIconName) then
    begin
      Result := LSVGIconItem;
      Break;
    end;
  end;
end;


function TSVGIconItems.GetItem(
  Index: Integer): TSVGIconItem;
begin
  Result := TSVGIconItem(inherited GetItem(Index));
end;

function TSVGIconItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSVGIconItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if FOwner is TSVGIconImageList then
    TSVGIconImageList(FOwner).RecreateBitmaps;
end;

procedure TSVGIconItems.SetItem(Index: Integer;
  const Value: TSVGIconItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSVGIconItems.Update(Item: TCollectionItem);
begin
  inherited;
  if FOwner is TSVGIconImageList then
    TSVGIconImageList(FOwner).RecreateBitmaps;
end;

{ TSVGIconImageList }

function TSVGIconImageList.Add(const SVG: TSVG;
  const Name: string): Integer;
var
  Item: TSVGIconItem;
begin
  try
    Item := FSVGItems.Add;
    Item.SVG := SVG;
    Item.IconName := Name;
  finally
    RecreateBitmaps;
  end;
  Result := FSVGItems.Count - 1;
end;

procedure TSVGIconImageList.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    StopDrawing(True);
    try
      Width := TSVGIconImageList(Source).Width;
      Height := TSVGIconImageList(Source).Height;
      FOpacity := TSVGIconImageList(Source).FOpacity;
      FStoreAsText := TSVGIconImageList(Source).FStoreAsText;
      FSVGItems.Assign(TSVGIconImageList(Source).FSVGItems);
    finally
      StopDrawing(False);
    end;
    if not (csLoading in ComponentState) then
      RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.AssignTo(Dest: TPersistent);
begin
  ClearIcons;
  inherited;
  if Dest is TSVGIconImageList then
  begin
    TSVGIconImageList(Dest).FOpacity := FOpacity;
    TSVGIconImageList(Dest).Width := Width;
    TSVGIconImageList(Dest).Height := Height;
    FSVGItems.AssignTo(TSVGIconImageList(Dest).FSVGItems);
  end;
end;

procedure TSVGIconImageList.ClearIcons;
begin
  StopDrawing(True);
  try
    inherited Clear;
    FSVGItems.Clear;
  finally
    StopDrawing(False);
    RecreateBitmaps;
  end;
end;

constructor TSVGIconImageList.Create(AOwner: TComponent);
begin
  inherited;
  Width := DEFAULT_SIZE;
  Height := DEFAULT_SIZE;
  FSVGItems := TSVGIconItems.Create(Self);
  FOpacity := 255;
  {$IFDEF HiDPISupport}
  FScaled := True;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF}
end;

procedure TSVGIconImageList.Delete(const Index: Integer);
begin
  //Don't call inherited method of ImageList, to avoid errors
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems.Delete(Index);
end;

destructor TSVGIconImageList.Destroy;
begin
  {$IFDEF HiDPISupport}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
  {$ENDIF}
  FreeAndNil(FSVGItems);
  inherited;
end;

procedure TSVGIconImageList.Change;
begin
  //Optimization: Do not notify to components during redrawing of icons
  if FStopDrawing = 0 then
    inherited;
end;

procedure TSVGIconImageList.DefineProperties(Filer: TFiler);
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

{$IFDEF HiDPISupport}
procedure TSVGIconImageList.DPIChangedMessageHandler(const Sender: TObject;
  const Msg: Messaging.TMessage);
var
  LWidthScaled, LHeightScaled: Integer;
begin
  if FScaled and (TChangeScaleMessage(Msg).Sender = Owner) then
  begin
    LWidthScaled := MulDiv(Width, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    LHeightScaled := MulDiv(Height, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    FScaling := True;
    try
      if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
      begin
        StopDrawing(True);
        try
          Width := LWidthScaled;
          Height := LHeightScaled;
        finally
          StopDrawing(False);
        end;
      end;
    finally
      FScaling := False;
    end;
  end;
end;
{$ENDIF}

procedure TSVGIconImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas.Handle, Index, X, Y, Width, Height);
end;

function TSVGIconImageList.GetCount: Integer;
begin
  Result := FSVGItems.Count;
end;

function TSVGIconImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconImageList.GetImages(Index: Integer): TSVG;
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
    Result := FSVGItems[Index].SVG
  else
    Result := nil;
end;

function TSVGIconImageList.GetNames(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
    Result := FSVGItems[Index].IconName
  else
    Result := '';
end;

function TSVGIconImageList.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TSVGIconImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TSVGIconImageList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to FSVGItems.Count - 1 do
    if FSVGItems[Result].IconName = Name then
      Exit;
  Result := -1;
end;

procedure TSVGIconImageList.Loaded;
begin
  inherited;
  RecreateBitmaps;
end;

function TSVGIconImageList.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean): Integer;
var
  LIndex: Integer;
  LSVG: TSVG;
  LFileName: string;
  LItem: TSVGIconItem;
begin
  Result := 0;
  StopDrawing(True);
  try
    LSVG := TSVG.Create;
    try
      if not AAppend then
        ClearIcons;
      for LIndex := 0 to AFileNames.Count - 1 do
      begin
        LFileName := AFileNames[LIndex];
        LSVG.LoadFromFile(LFileName);
        LItem := SVGIconItems.Add;
        LItem.IconName := ChangeFileExt(ExtractFileName(LFileName), '');
        LItem.SVG := LSVG;
        Inc(Result);
      end;
    finally
      LSVG.Free;
    end;
  finally
    StopDrawing(False);
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.PaintTo(const DC: HDC; const Index: Integer;
  const X, Y, Width, Height: Double);
var
  R: TGPRectF;
  SVG: TSVG;
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
  begin
    SVG := FSVGItems[Index].SVG;
    SVG.SVGOpacity := FOpacity / 255;
    R := CalcRect(MakeRect(X, Y, Width, Height), SVG.Width, SVG.Height, baCenterCenter);
    SVG.PaintTo(DC, R, nil, 0);
    SVG.SVGOpacity := 1;
  end;
end;

procedure TSVGIconImageList.PaintTo(const DC: HDC; const Name: string;
  const X, Y, Width, Height: Double);
var
  Index: Integer;
begin
  Index := IndexOf(Name);
  PaintTo(DC, Index, X, Y, Width, Height);
end;


procedure TSVGIconImageList.ReadImageData(Stream: TStream);
var
  FStream: TMemoryStream;
  Count, Size: Integer;
  SVG: TSVG;
  Name: string;
  C: Integer;
begin
  if FStoreAsText then
    Exit;
  try
    StopDrawing(True);
    FStream := TMemoryStream.Create;
    Stream.Read(Count, SizeOf(Integer));
    SVG := TSVG.Create(nil);
    for C := 0 to Count - 1 do
    begin
      Stream.Read(Size, SizeOf(Integer));
      SetLength(Name, Size);
      Stream.Read(PChar(Name)^, Size * SizeOf(Char));

      Stream.Read(Size, SizeOf(Integer));
      FStream.CopyFrom(Stream, Size);
      SVG.LoadFromStream(FStream);
      FStream.Clear;
      Add(SVG, Name);
    end;
    FStream.Free;
    SVG.Free;
  finally
    StopDrawing(False);
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageList.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageList.RecreateBitmaps;
var
  C: Integer;
  SVG: TSVG;
  Icon: HIcon;
begin
  if  (FStopDrawing = 0) and
    not (csLoading in ComponentState) and
    not (csDestroying in ComponentState) then
  begin
    ImageList_Remove(Handle, -1);
    if (Width > 0) and (Height > 0) then
    begin
      Handle := ImageList_Create(Width, Height,
        ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);

      for C := 0 to FSVGItems.Count - 1 do
      begin
        SVG := FSVGItems[C].SVG;
        if Assigned(SVG) then
        begin
          Icon := SVGToIcon(SVG);
          ImageList_AddIcon(Handle, Icon);
          DestroyIcon(Icon);
        end;
      end;
    end;
  end;
end;

procedure TSVGIconImageList.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGIconImageList.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    RecreateBitmaps;
  end;
end;

{$IFDEF D10_4+}
function TSVGIconImageList.GetIndexByName(
  const AName: TImageName): TImageIndex;
var
  LIconFontItem: TSVGIconItem;
begin
  LIconFontItem := SVGIconItems.GetIconByName(AName);
  if Assigned(LIconFontItem) then
    Result := LIconFontItem.Index
  else
    Result := -1;
end;

function TSVGIconImageList.GetNameByIndex(AIndex: TImageIndex): TImageName;
begin
  Result := SVGIconItems.Items[AIndex].IconName;
end;

function TSVGIconImageList.IsImageNameAvailable: Boolean;
begin
  Result := True;
end;

function TSVGIconImageList.IsScaled: Boolean;
begin
  Result := FScaled;
end;
{$ENDIF}

procedure TSVGIconImageList.SetImages(Index: Integer; const Value: TSVG);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
  begin
    if FSVGItems[Index].SVG <> Value then
      FSVGItems[Index].SVG := Value;
  end;
end;

procedure TSVGIconImageList.SetNames(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems[Index].IconName := Value;
end;

procedure TSVGIconImageList.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.SetSize(const Value: Integer);
begin
  if (Height <> Value) or (Width <> Value) then
  begin
    StopDrawing(True);
    try
      Width := Value;
      Height := Value;
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.SetSVGIconItems(const Value: TSVGIconItems);
begin
  FSVGItems := Value;
end;

procedure TSVGIconImageList.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageList.StopDrawing(const AStop: Boolean);
begin
  if AStop then
    Inc(FStopDrawing)
  else
    Dec(FStopDrawing);
end;

procedure PaintToBitmap(SVG: TSVG; Bitmap: TBitmap; Bounds: TGPRectF;
  Rects: PRectArray; RectCount: Integer);
var
  Graphics: TGPGraphics;
begin
  Graphics := TGPGraphics.Create(Bitmap.Canvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    SVG.PaintTo(Graphics, Bounds, Rects, RectCount);
  finally
    Graphics.Free;
  end;
end;

function TSVGIconImageList.SVGToIcon(const SVG: TSVG): HICON;
var
  R: TGPRectF;

  function SVGToIcon24(SVG: TSVG): HIcon;
  var
    ColorBitmap, MaskBitmap: TBitmap;
    X: Integer;
    Y: Integer;
    Bits: PRGBQuad;
    IconInfo: TIconInfo;
    TransparentBitmap: TBitmap;
    BF: TBlendFunction;
    DC: THandle;
  begin
    ColorBitmap := TBitmap.Create;
    MaskBitmap := TBitmap.Create;
    TransparentBitmap := TBitmap.Create;
    try
      TransparentBitmap.PixelFormat := pf32bit;
      TransparentBitmap.Width := Width;
      TransparentBitmap.Height := Height;
      FillChar(TransparentBitmap.Scanline[Height - 1]^, Width * Height * 4, 0);

      PaintToBitmap(SVG, TransparentBitmap, R, nil, 0);


      ColorBitmap.PixelFormat := pf32bit;
      ColorBitmap.Width := Width;
      ColorBitmap.Height := Height;
      MaskBitmap.PixelFormat := pf32bit;
      MaskBitmap.Width := Width;
      MaskBitmap.Height := Height;


      ColorBitmap.Canvas.Brush.Color := BkColor;
      ColorBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

      BF.BlendOp := AC_SRC_OVER;
      BF.BlendFlags := 0;
      BF.SourceConstantAlpha := 255;
      BF.AlphaFormat := AC_SRC_ALPHA;
      AlphaBlend(ColorBitmap.Canvas.Handle, 0, 0, Width, Height,
        TransparentBitmap.Canvas.Handle, 0, 0, Width, Height, BF);

      DC := MaskBitmap.Canvas.Handle;
      for Y := 0 to Height - 1 do
      begin
        Bits := TransparentBitmap.ScanLine[Y];
        for X := 0 to Width - 1 do
        begin
          if Bits.rgbReserved = 0 then
            SetPixelV(DC, X, Y, clWhite)
          else
            SetPixelV(DC, X, Y, clBlack);
          Inc(Bits);
        end;
      end;

      IconInfo.fIcon := True;
      IconInfo.hbmColor := ColorBitmap.Handle;
      IconInfo.hbmMask := MaskBitmap.Handle;
      Result := CreateIconIndirect(IconInfo);
    finally
      TransparentBitmap.Free;
      ColorBitmap.Free;
      MaskBitmap.Free;
    end;
  end;

  function SVGToIcon32(SVG: TSVG): HICON;
  var
    Bitmap: TGPBitmap;
    Graphics: TGPGraphics;
  begin
    Bitmap := TGPBitmap.Create(Width, Height);
    Graphics := TGPGraphics.Create(Bitmap);
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    SVG.PaintTo(Graphics, R, nil, 0);
    Graphics.Free;

    Bitmap.GetHICON(Result);
    Bitmap.Free;
  end;

begin
  SVG.SVGOpacity := FOpacity / 255;
  R := CalcRect(MakeRect(0.0, 0, Width, Height), SVG.Width, SVG.Height, baCenterCenter);

  if GetFileVersion(comctl32) >= ComCtlVersionIE6 then
    Result := SVGToIcon32(SVG)
  else
    Result := SVGToIcon24(SVG);
    
  SVG.SVGOpacity := 1;
end;

procedure TSVGIconImageList.WriteImageData(Stream: TStream);
var
  Count, Size: Integer;
  SVG: TSVG;
  Name: string;
  C: Integer;
  SVGStream: TMemoryStream;
begin
  if FStoreAsText then
    Exit;
  Count := FSVGItems.Count;
  Stream.Write(Count, SizeOf(Integer));

  SVGStream := TMemoryStream.Create;
  for C := 0 to Count - 1 do
  begin
    Name := FSVGItems[C].IconName;
    SVG := FSVGItems[C].SVG;
    Size := Length(Name);
    Stream.Write(Size, SizeOf(Integer));
    Stream.WriteBuffer(PChar(Name)^, Size * SizeOf(Char));

    SVG.SaveToStream(SVGStream);
    Size := SVGStream.Size;
    Stream.Write(Size, SizeOf(Integer));
    SVGStream.Position := 0;
    Stream.CopyFrom(SVGStream, Size);
    SVGStream.Clear;
  end;
  SVGStream.Free;
end;

procedure TSVGIconImageList.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageList.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

end.
