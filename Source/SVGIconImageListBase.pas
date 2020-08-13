unit SVGIconImageListBase;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  WinApi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ImgList,
  System.UITypes,   // after ImgList to avoid deprecation warnings
  SVG,
  SVGColor,
  SVGIconItems;

const
  SVGIconImageListVersion = '1.7.0';
  DEFAULT_SIZE = 16;

type
  TSVGIconImageListBase = class(TDragImageList)
  protected
    {$IFDEF HiDPISupport}
    {$IFNDEF D10_4+}
    FScaled: Boolean;
    {$ENDIF}
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    FSVGItemsUpdateMessageID: Integer;
    FOpacity: Byte;
    FFixedColor: TSVGColor;
    FGrayScale: Boolean;
    FDisabledGrayScale: Boolean;
    FDisabledOpacity: Byte;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;

    function GetSVGIconItems: TSVGIconItems; virtual; abstract;
    procedure SetSVGIconItems(const Value: TSVGIconItems); virtual;

    function GetImages(Index: Integer): TSVG; virtual;
    function GetNames(Index: Integer): string; virtual;

    procedure SetImages(Index: Integer; const Value: TSVG); virtual;
    procedure SetNames(Index: Integer; const Value: string); virtual;
    procedure SetSize(const Value: Integer);
    procedure SetFixedColor(const Value: TSVGColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetDisabledGrayScale(const Value: Boolean);
    procedure SetDisabledOpacity(const Value: Byte);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    function IndexOf(const Name: string): Integer;virtual;

    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); overload; virtual; abstract;
    procedure PaintTo(const ACanvas: TCanvas; const AName: string; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); overload;


    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure Loaded; override;
    function GetCount: Integer;    {$IF CompilerVersion > 29} override; {$ELSE}  virtual;abstract; {$ENDIF}

    procedure RecreateBitmaps;virtual;abstract;
    procedure DoChange; override;
    procedure ClearIcons;virtual;

    function SVGToIcon(const SVG: TSVG): HICON;

  {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  {$ENDIF}
    procedure SVGItemsUpdateMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoAssign(const Source: TPersistent); virtual;
    procedure DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer); virtual;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure Assign(Source: TPersistent); override;

    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function GetIndexByName(const AName: TImageName): TImageIndex; override;
    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}

    property SVGIconItems: TSVGIconItems read GetSVGIconItems write SetSVGIconItems;
    property Count: Integer read GetCount;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property FixedColor: TSVGColor read FFixedColor write SetFixedColor default TSVGColor.inherit_color;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;

    {$IFDEF HiDPISupport}
    {$IFNDEF D10_4+}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
    {$ENDIF}
    property Images[Index: Integer]: TSVG read GetImages write SetImages;
    property Names[Index: Integer]: string read GetNames write SetNames;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  Vcl.ComCtrls,
  Vcl.Forms,
  GDIPUtils,
  SVGTypes;

{ TSVGIconImageListBase }



procedure TSVGIconImageListBase.Assign(Source: TPersistent);
begin
  if Source is TSVGIconImageListBase then
  begin
    BeginUpdate;
    try
      Width := TSVGIconImageListBase(Source).Width;
      Height := TSVGIconImageListBase(Source).Height;
      FOpacity := TSVGIconImageListBase(Source).FOpacity;
      FFixedColor := TSVGIconImageListBase(Source).FFixedColor;
      FGrayScale := TSVGIconImageListBase(Source).FGrayScale;
      DoAssign(Source);
    finally
     EndUpdate;
    end;
  end else if Source is TSVGIconItems then begin
    if Assigned(SVGIconItems) then
      SVGIconItems.Assign(Source);
  end else
    inherited;
end;

procedure TSVGIconImageListBase.AssignTo(Dest: TPersistent);
begin
  if (Dest is TSVGIconItems) then begin
    if Assigned(SVGIconItems) then
      Dest.Assign(SVGIconItems)
  end else
    inherited;
end;

procedure TSVGIconImageListBase.ClearIcons;
begin
  //do nothing
end;

constructor TSVGIconImageListBase.Create(AOwner: TComponent);
begin
  inherited;
  ColorDepth := cd32Bit;
  Width := DEFAULT_SIZE;
  Height := DEFAULT_SIZE;
  FOpacity := 255;
  FFixedColor := inherit_color;
  FGrayScale := False;
  {$IFDEF HiDPISupport}
  FScaled := True;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF}
  FSVGItemsUpdateMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TSVGItemsUpdateMessage, SVGItemsUpdateMessageHandler);
  FDisabledGrayScale := True;
  FDisabledOpacity := 125;

end;

procedure TSVGIconImageListBase.DefineProperties(Filer: TFiler);
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
end;

destructor TSVGIconImageListBase.Destroy;
begin
  {$IFDEF HiDPISupport}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
  {$ENDIF}
  TMessageManager.DefaultManager.Unsubscribe(TSVGItemsUpdateMessage, FSVGItemsUpdateMessageID);
  inherited;
end;

procedure TSVGIconImageListBase.DoAssign(const Source: TPersistent);
begin
  //do nothing.. TSVGIconImageList will override;
end;

procedure TSVGIconImageListBase.DoChange;
begin
  inherited;
  RecreateBitmaps;
end;

procedure TSVGIconImageListBase.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas, Index, X, Y, Width, Height, Enabled);
end;

procedure TSVGIconImageListBase.DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer);
var
  LSizeScaled: Integer;
  LWidthScaled, LHeightScaled: Integer;
begin
  if Width = Height then
  begin
    LSizeScaled := MulDiv(Size, NewDPI, OldDPI);
    {$IFDEF D10_3+}
    FScaling := True;
    try
      SetSize(LSizeScaled);
    finally
      FScaling := False;
    end;
    {$ELSE}
      SetSize(LSizeScaled);
    {$ENDIF}
  end
  else
  begin
    LWidthScaled := MulDiv(Width, NewDPI, OldDPI);
    LHeightScaled := MulDiv(Height, NewDPI, OldDPI);
    {$IFDEF D10_3+}
    FScaling := True;
    try
      if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
      begin
        Width := LWidthScaled;
        Height := LHeightScaled;
      end;
    finally
      FScaling := False;
    end;
    {$ELSE}
       if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
       begin
         Width := LWidthScaled;
         Height := LHeightScaled;
       end;
    {$ENDIF}
  end;
end;

{$IF CompilerVersion > 29}
function TSVGIconImageListBase.GetCount: Integer;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) then
    Result := Items.Count
  else
    Result := 0;
end;
{$ENDIF}

function TSVGIconImageListBase.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconImageListBase.GetImages(Index: Integer): TSVG;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].SVG
  else
    Result := nil;
end;

function TSVGIconImageListBase.GetNames(Index: Integer): string;
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].IconName
  else
    Result := '';
end;

function TSVGIconImageListBase.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TSVGIconImageListBase.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TSVGIconImageListBase.IndexOf(const Name: string): Integer;
Var
  Items: TSVGIconItems;
  Item: TSVGIconItem;
begin
  Items := SVGIconItems;
  if not Assigned(Items) then Exit(-1);

  Item := Items.GetIconByName(Name);
  if Assigned(Item) then
    Result := Item.Index
  else
    Result := -1;
end;

{$IFDEF D10_4+}
function TSVGIconImageListBase.IsImageNameAvailable: Boolean;
begin
  Result := true;
end;

function TSVGIconImageListBase.GetIndexByName(const AName: TImageName): TImageIndex;
begin
  Result := IndexOf(AName);
end;

function TSVGIconImageListBase.GetNameByIndex(AIndex: TImageIndex): TImageName;
begin
  Result := GetNames(AIndex);
end;
{$ENDIF}

procedure TSVGIconImageListBase.Loaded;
begin
  inherited;
  Change;
end;

procedure TSVGIconImageListBase.PaintTo(const ACanvas: TCanvas; const AName: string; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);
  PaintTo(ACanvas, LIndex, X, Y, AWidth, AHeight, AEnabled);
end;

procedure TSVGIconImageListBase.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageListBase.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;


procedure TSVGIconImageListBase.SetDisabledGrayScale(const Value: Boolean);
begin
  if FDisabledGrayScale <> Value then
  begin
    FDisabledGrayScale := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetDisabledOpacity(const Value: Byte);
begin
  if FDisabledOpacity <> Value then
  begin
    FDisabledOpacity := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetFixedColor(const Value: TSVGColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> inherit_color then
      FGrayScale := False;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := inherit_color;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetImages(Index: Integer; const Value: TSVG);
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
  begin
    if Items[Index].SVG <> Value then
      Items[Index].SVG := Value;
  end;
end;

procedure TSVGIconImageListBase.SetNames(Index: Integer; const Value: string);
Var
  Items: TSVGIconItems;
begin
  Items := SVGIconItems;
  if Assigned(Items) and (Index >= 0) and (Index < Items.Count) then
    Items[Index].IconName := Value;
end;

procedure TSVGIconImageListBase.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetSize(const Value: Integer);
begin
  if (Height <> Value) or (Width <> Value) then
  begin
    BeginUpdate;
    try
      Width := Value;
      Height := Value;
    finally
      EndUpdate;
    end;
    Change;
  end;
end;

procedure TSVGIconImageListBase.SetSVGIconItems(const Value: TSVGIconItems);
begin
  if Assigned(SvgIconItems) then
    SvgIconItems.Assign(Value);
end;

procedure TSVGIconImageListBase.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    Change;
  end;
end;

function TSVGIconImageListBase.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TSVGIconImageListBase.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconImageListBase.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
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


function TSVGIconImageListBase.SVGToIcon(const SVG: TSVG): HICON;
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

procedure TSVGIconImageListBase.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageListBase.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{$IFDEF HiDPISupport}
procedure TSVGIconImageListBase.DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
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
        BeginUpdate;
        try
          Width := LWidthScaled;
          Height := LHeightScaled;
        finally
          EndUpdate;
        end;
        Change;
      end;
    finally
      FScaling := False;
    end;
  end;
end;
{$ENDIF}

procedure TSVGIconImageListBase.SVGItemsUpdateMessageHandler(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
var
  items : TSVGIconItems;
begin
  items := SVGIconItems;
  if TObject(items) = Sender then
    Change;
end;


end.
