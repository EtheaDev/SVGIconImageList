unit SVGIconImageListBase;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  WinApi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  SVG,
  SVGColor;

const
  SVGIconImageListVersion = '1.6.0';
  DEFAULT_SIZE = 16;

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

type
  TSVGIconImageListBase = class(TDragImageList)
  private
    FStopDrawing: Integer;
    FOpacity: Byte;
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    FFixedColor: TSVGColor;
    FGrayScale: Boolean;
    FDisabledGrayScale: Boolean;
    FDisabledOpacity: Byte;
  protected
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;

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

    function IndexOf(const Name: string): Integer;virtual;abstract;

    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Double; AEnabled: Boolean = True); overload; virtual; abstract;
    procedure PaintTo(const ACanvas: TCanvas; const AName: string; const X, Y, AWidth, AHeight: Double; AEnabled: Boolean = True); overload;


    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure Loaded; override;
    function GetCount: Integer;    {$IF CompilerVersion > 29} override; {$ELSE}  virtual;abstract; {$ENDIF}

    procedure StopDrawing(const AStop: Boolean);
    procedure Change; override;
    procedure RecreateBitmaps;virtual;abstract;
    procedure ClearIcons;virtual;

    function SVGToIcon(const SVG: TSVG): HICON;

    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function IsScaled: Boolean; override;
//    function GetIndexByName(const AName: TImageName): TImageIndex; override;
//    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}


    procedure AssignTo(Dest: TPersistent); override;
    procedure DoAssign(const Source: TPersistent); virtual;

  public
    procedure Assign(Source: TPersistent); override;


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
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  ComCtrls,
  GDIPUtils,
  SVGTypes;

{ TSVGIconImageListBase }


procedure TSVGIconImageListBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageListBase then
  begin
    StopDrawing(True);
    try
      Width := TSVGIconImageListBase(Source).Width;
      Height := TSVGIconImageListBase(Source).Height;
      FOpacity := TSVGIconImageListBase(Source).FOpacity;
      FFixedColor := TSVGIconImageListBase(Source).FFixedColor;
      FGrayScale := TSVGIconImageListBase(Source).FGrayScale;
      DoAssign(Source);
      //TODO : move to TSVGIconImageList
      //FSVGItems.Assign(TSVGIconImageList(Source).FSVGItems);
      //FStoreAsText := TSVGIconImageList(Source).FStoreAsText;
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.AssignTo(Dest: TPersistent);
begin
  ClearIcons;
  inherited;
  if Dest is TSVGIconImageListBase then
  begin
    TSVGIconImageListBase(Dest).FOpacity := FOpacity;
    TSVGIconImageListBase(Dest).Width := Width;
    TSVGIconImageListBase(Dest).Height := Height;
//    FSVGItems.AssignTo(TSVGIconImageList(Dest).FSVGItems);
  end;

end;

procedure TSVGIconImageListBase.Change;
begin
  //Optimization: Do not notify to components during redrawing of icons
  if FStopDrawing = 0 then
    inherited;

end;

procedure TSVGIconImageListBase.ClearIcons;
begin
  //do nothing
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

procedure TSVGIconImageListBase.DoAssign(const Source: TPersistent);
begin
  //do nothing.. TSVGIconImageList will override;

end;

procedure TSVGIconImageListBase.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas, Index, X, Y, Width, Height, Enabled);
end;


{$IF CompilerVersion > 29}
function TSVGIconImageListBase.GetCount: Integer;
begin
  raise ENotImplemented.Create('needed to be overridden.');
end;
{$ENDIF}

function TSVGIconImageListBase.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconImageListBase.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TSVGIconImageListBase.GetWidth: Integer;
begin
  Result := inherited Width;

end;

{$IFDEF D10_4+}
function TSVGIconImageListBase.IsImageNameAvailable: Boolean;
begin
  result := true;
end;

function TSVGIconImageListBase.IsScaled: Boolean;
begin
  result := FScaled;
end;
{$ENDIF}

procedure TSVGIconImageListBase.Loaded;
begin
  inherited;
  RecreateBitmaps;

end;

procedure TSVGIconImageListBase.PaintTo(const ACanvas: TCanvas; const AName: string; const X, Y, AWidth, AHeight: Double; AEnabled: Boolean);
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
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetDisabledOpacity(const Value: Byte);
begin
  if FDisabledOpacity <> Value then
  begin
    FDisabledOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetFixedColor(const Value: TSVGColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> inherit_color then
      FGrayScale := False;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := inherit_color;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.SetSize(const Value: Integer);
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

procedure TSVGIconImageListBase.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageListBase.StopDrawing(const AStop: Boolean);
begin
  if AStop then
    Inc(FStopDrawing)
  else
    Dec(FStopDrawing);
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

end.
