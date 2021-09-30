{-----------------------------------------------------------------------------
 Unit Name: SkiaSVGFactory
 Author:    Carlo Barazzxetta
 Purpose:   High-level encapsulation of Skia Svg functionality using
            Google Skia libraries
-----------------------------------------------------------------------------}
unit SkiaSVGFactory;

interface

Uses
  SVGInterfaces;

// Factory Methods
function GetSkiaSVGFactory: ISVGFactory;

resourcestring
  SKIA_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text with Skia';

implementation

Uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.GDIPAPI,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  //Skia engine
  Skia;

type
  TSkiaSVG = class(TInterfacedObject, ISVG)
  private
    FDOM: ISKSVGDOM;
    FDrawCached: Boolean;
    FDrawBuffer: HBITMAP;
    FDrawBufferData: Pointer;
    FDrawBufferStride: Integer;
    FDrawCacheEnabled: Boolean;
    FSource: String;
    FWidth: Integer;
    FHeight: Integer;
    FFixedColor: TColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FOpacity: Single;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const Color: TColor);
    function GetApplyFixedColorToRootOnly: Boolean;
    procedure SetApplyFixedColorToRootOnly(Value:Boolean);
    function GetSource: string;
    procedure SetSource(const ASource: string);
    // procedures and functions
    function IsEmpty: Boolean;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);
    procedure LoadFromSource;
    procedure SourceFromStream(Stream: TStream);
    procedure UpdateSizeInfo(defaultWidth, defaultHeight: integer);
    procedure DeleteBuffers;
    procedure CreateBuffer(const AWidth, AHeight: Integer;
      const AMemDC: HDC; out ABuffer: HBITMAP;
      out AData: Pointer; out AStride: Integer);
    procedure DrawWithDefaultColor(const ACanvas: ISKCanvas;
      const ADOM: ISKSVGDOM; const ADestRect: TRectF);
    procedure DrawWithFixedColor(const ACanvas: ISKCanvas;
      const ADOM: ISKSVGDOM; const ADestRect: TRectF;
      const AColor: TAlphaColor);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSkiaSVGFactory = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
  end;

{ TSkiaSVG }
procedure TSkiaSVG.Clear;
Const
  EmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
begin
  SetSource(EmptySvg);
end;

constructor TSkiaSVG.Create;
begin
  inherited;
  FFixedColor := TColors.SysDefault; // clDefault
  FOpacity := 1.0;
end;

destructor TSkiaSVG.Destroy;
begin
  inherited;
  DeleteBuffers;
end;

procedure TSkiaSVG.CreateBuffer(
  const AWidth, AHeight: Integer;
  const AMemDC: HDC; out ABuffer: HBITMAP;
  out AData: Pointer; out AStride: Integer);
const
  ColorMasks: array[0..2] of DWORD = ($00FF0000, $0000FF00, $000000FF);
var
  LBitmapInfo: PBitmapInfo;
  function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
  begin
    Dec(Alignment);
    Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
    Result := Result div 8;
  end;

begin
  AStride := BytesPerScanline(AWidth, 32, 32);
  GetMem(LBitmapInfo, SizeOf(TBitmapInfoHeader) + SizeOf(ColorMasks));
  try
    LBitmapInfo.bmiHeader := Default(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth       := AWidth;
    LBitmapInfo.bmiHeader.biHeight      := -AHeight;
    LBitmapInfo.bmiHeader.biPlanes      := 1;
    LBitmapInfo.bmiHeader.biBitCount    := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_BITFIELDS;
    LBitmapInfo.bmiHeader.biSizeImage   := AStride * AHeight;
    Move(ColorMasks[0], LBitmapInfo.bmiColors[0], SizeOf(ColorMasks));
    ABuffer := CreateDIBSection(AMemDC, LBitmapInfo^, DIB_RGB_COLORS, AData, 0, 0);
    if ABuffer <> 0 then
      GdiFlush;
  finally
    FreeMem(LBitmapInfo);
  end;
end;

procedure TSkiaSVG.DeleteBuffers;
begin
  if FDrawBuffer <> 0 then
  begin
    FDrawCached := False;
    DeleteObject(FDrawBuffer);
    FDrawBuffer := 0;
  end;
end;

procedure TSkiaSVG.LoadFromFile(const FileName: string);
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

function TSkiaSVG.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FApplyFixedColorToRootOnly;
end;

function TSkiaSVG.GetFixedColor: TColor;
begin
  Result := FFixedColor;
end;

function TSkiaSVG.GetGrayScale: Boolean;
begin
  Result := FGrayScale;
end;

function TSkiaSVG.GetHeight: Single;
begin
  Result := FHeight;
end;

function TSkiaSVG.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TSkiaSVG.GetSource: string;
begin
  Result := FSource;
end;

function TSkiaSVG.GetWidth: Single;
begin
  Result := FWidth;
end;

function TSkiaSVG.IsEmpty: Boolean;
begin
  Result := FSource = '';
end;

procedure TSkiaSVG.UpdateSizeInfo(defaultWidth, defaultHeight: integer);
var
  LSize: TSizeF;
begin
  if Assigned(FDOM) and FDOM.ContainerSize.IsZero then
  begin
    LSize.cx := defaultWidth;
    LSize.cy := defaultHeight;
    FDOM.ContainerSize := LSize;
  end;
end;

procedure TSkiaSVG.LoadFromSource;
var
  LStream: TStringStream;
begin
  if (FSource <> '') then
  begin
    LStream := TStringStream.Create(FSource, TEncoding.UTF8);
    try
      LStream.Position := 0;
      DeleteBuffers;
      FDOM := TSkSVGDOM.Make(LStream);
    finally
      LStream.Free;
    end;
    if not Assigned(FDOM) then
      raise ESVGException.Create(SKIA_ERROR_PARSING_SVG_TEXT);
  end;
end;

procedure TSkiaSVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
begin
  // read and save the Source
  OldPos := Stream.Position;
  SourceFromStream(Stream);
  // Restore Position
  Stream.Position := OldPos;
  DeleteBuffers;
end;

procedure TSkiaSVG.DrawWithDefaultColor(const ACanvas: ISKCanvas; const ADOM: ISKSVGDOM;
  const ADestRect: TRectF);
var
  LSvgRect: TRectF;
begin
  if ADestRect.IsEmpty or not Assigned(ADOM) then
    Exit;
  LSvgRect := TRectF.Create(TPointF.Create(0, 0), ADOM.ContainerSize);
  if LSvgRect.IsEmpty then
    Exit;
  ACanvas.Translate(ADestRect.Left, ADestRect.Top);
  ACanvas.Scale(ADestRect.Width / LSvgRect.Width, ADestRect.Height / LSvgRect.Height);
  ADOM.Render(ACanvas);
end;

procedure TSkiaSVG.DrawWithFixedColor(const ACanvas: ISKCanvas; const ADOM: ISKSVGDOM;
  const ADestRect: TRectF; const AColor: TAlphaColor);
var
  LSurface: ISKSurface;
  LImage: ISKImage;
  LPaint: ISKPaint;
  LSvgRect: TRectF;
begin
  if ADestRect.IsEmpty or not Assigned(ADOM) then
    Exit;
  LSvgRect := TRectF.Create(TPointF.Create(0, 0), ADOM.ContainerSize);
  if LSvgRect.IsEmpty then
    Exit;
  LSurface := TSKSurface.MakeRaster(Round(ADestRect.Width), Round(ADestRect.Height));
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSurface.Canvas.Scale(ADestRect.Width / LSvgRect.Width, ADestRect.Height / LSvgRect.Height);
  ADOM.Render(LSurface.Canvas);
  LImage := LSurface.MakeImageSnapshot;
  LPaint := TSKPaint.Create;
  //if FFillKind = TSvgBrushKind.Solid then
    LPaint.ColorFilter := TSKColorFilter.MakeBlend(AColor, TSkBlendMode.SrcIn);
  LPaint.Style := TSKPaintStyle.Fill;
  ACanvas.DrawImage(LImage, ADestRect.Left, ADestRect.Top, LPaint);
end;

procedure TSkiaSVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
const
  BlendFunction: TBlendFunction = (BlendOp: AC_SRC_OVER; BlendFlags: 0; SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA);
var
  LOldObj: HGDIOBJ;
  LDrawBufferDC: HDC;
  LBlendFunction: TBlendFunction;
  LScaleFactor: Single;

  function ColorToAlphaColor(Value: TColor): TAlphaColor;
  var
    CRec: TColorRec;
    ARec: TAlphaColorRec;
  begin
    CRec.Color := Value;
    ARec.A := CRec.A;
    ARec.B := CRec.B;
    ARec.G := CRec.G;
    ARec.R := CRec.R;
    Result := ARec.Color;
  end;

  procedure InternalDraw;
  var
    LSurface: ISkSurface;
    LDestRect: TRectF;
  begin
    LSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(FWidth, FHeight),
      FDrawBufferData, FDrawBufferStride);
    LSurface.Canvas.Clear(TAlphaColors.Null);
    LScaleFactor := 1;
    LSurface.Canvas.Concat(TMatrix.CreateScaling(LScaleFactor, LScaleFactor));
    LDestRect := TRectF.Create(0, 0, FWidth / LScaleFactor, FHeight / LScaleFactor);

    if not FGrayScale and
      (FFixedColor <> TColors.SysDefault) and
      (FFixedColor <> TColors.SysNone) then
      DrawWithFixedColor(LSurface.Canvas, FDOM, R, ColorToAlphaColor(FFixedColor))
    else
      DrawWithDefaultColor(LSurface.Canvas, FDOM, R);
    FDrawCached := True;
  end;

begin
  if (Round(R.Width) <> FWidth) or (Round(R.Height) <> FHeight) then
  begin
    FWidth := Round(R.Width);
    FHeight := Round(R.Height);
    DeleteBuffers;
  end;
  UpdateSizeInfo(FWidth, FHeight);

  LDrawBufferDC := CreateCompatibleDC(0);
  if LDrawBufferDC <> 0 then
    try
      if FDrawBuffer = 0 then
        CreateBuffer(FWidth, FHeight, LDrawBufferDC, FDrawBuffer, FDrawBufferData, FDrawBufferStride);
      if FDrawBuffer <> 0 then
      begin
        LOldObj := SelectObject(LDrawBufferDC, FDrawBuffer);
        try
          if (not FDrawCacheEnabled) or (not FDrawCached) then
            InternalDraw;
          LBlendFunction := BlendFunction;
          LBlendFunction.SourceConstantAlpha := Round(FOpacity * 255);
          AlphaBlend(DC, 0, 0, FWidth, FHeight, LDrawBufferDC, 0, 0, FWidth, FHeight, LBlendFunction);
        finally
          if LOldObj <> 0 then
            SelectObject(LDrawBufferDC, LOldObj);
        end;
      end;
    finally
      DeleteDC(LDrawBufferDC);
    end;
end;

procedure TSkiaSVG.SaveToFile(const FileName: string);
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

procedure TSkiaSVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

procedure TSkiaSVG.SetApplyFixedColorToRootOnly(Value: Boolean);
var
  Color: TColor;
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
    if FFixedColor <> TColors.SysDefault then
    begin
       Color := FFixedColor;
       FFixedColor := TColors.SysDefault;
       LoadFromSource;
       SetFixedColor(Color);
    end;
  end;
end;

procedure TSkiaSVG.SetFixedColor(const Color: TColor);
begin
  if Color = FFixedColor then Exit;
  if (FGrayScale and (Color <> TColors.SysDefault)) or
    ((FFixedColor <> TColors.SysDefault) and (Color = TColors.SysDefault))
  then
    LoadFromSource;
  if Color < 0  then
    FFixedColor := GetSysColor(Color and $000000FF)
  else
    FFixedColor := Color;
  FGrayScale := False;
end;

procedure TSkiaSVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = FGrayScale then Exit;
  if FGrayScale or (FFixedColor <> TColors.SysDefault) then
    LoadFromSource;
  FGrayScale := IsGrayScale;
  FFixedColor := TColors.SysDefault;
end;

procedure TSkiaSVG.SetOpacity(const Opacity: Single);
begin
  FOpacity := Opacity;
end;

procedure TSkiaSVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

procedure TSkiaSVG.SourceFromStream(Stream: TStream);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    DeleteBuffers;
    FDOM := TSkSVGDOM.Make(LStream);
    FSource := LStream.DataString;
  finally
    LStream.Free;
  end;
  if not Assigned(FDOM) then
    raise ESVGException.Create(SKIA_ERROR_PARSING_SVG_TEXT);
end;

{ TSkiaSVGFactory }
function TSkiaSVGFactory.NewSvg: ISVG;
begin
  Result := TSkiaSVG.Create;
end;

// Factory methods
function GetSkiaSVGFactory: ISVGFactory;
begin
  Result := TSkiaSVGFactory.Create;
end;

end.
