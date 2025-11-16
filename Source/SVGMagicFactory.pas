{-----------------------------------------------------------------------------
 Unit Name: SVGMagicFactory
 Author: Carlo Barazzetta
 Contributor: Claude Code
 Purpose: High-level encapsuation of Svg functionality for SVGMagic Library
-----------------------------------------------------------------------------}
unit SVGMagicFactory;

interface

Uses
  SVGInterfaces;

resourcestring
  SVGMAGIC_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG with SVGMagic Library';

// Factory Methods
function GetSVGMagicFactory: ISVGFactory;

implementation

Uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  UTWSVG,
  UTWSVGGraphic,
  UTWSVGRasterizer,
  UTWSVGGDIPlusRasterizer,
  UTWControlRenderer;

type
  TSVGMagic = class(TInterfacedObject, ISVG)
  private
    fSVG: TWSVG;
    fSVGGraphic: TWSVGGraphic;
    fRasterizer: TWSVGGDIPlusRasterizer;
    FSource: String;
    FWidth: Single;
    FHeight: Single;
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
    procedure UpdateSizeInfo;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSVGMagicFactory = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
  end;

{ TSVGMagic }

constructor TSVGMagic.Create;
begin
  inherited;
  fSVG := TWSVG.Create;
  fSVGGraphic := TWSVGGraphic.Create;
  fRasterizer := TWSVGGDIPlusRasterizer.Create(TWControlRenderer.GetGDIPlusToken);
  FFixedColor := TColors.SysDefault; // clDefault
  FOpacity := 1.0;
  FGrayScale := False;
  FApplyFixedColorToRootOnly := False;
end;

destructor TSVGMagic.Destroy;
begin
  fRasterizer.Free;
  fSVGGraphic.Free;
  fSVG.Free;
  inherited;
end;

procedure TSVGMagic.Clear;
begin
  fSVG.Clear;
  FSource := '';
  FWidth := 0;
  FHeight := 0;
end;

function TSVGMagic.IsEmpty: Boolean;
begin
  Result := fSVG.IsEmpty;
end;

procedure TSVGMagic.UpdateSizeInfo;
var
  LSize: TSize;
begin
  // Get original SVG dimensions from the rasterizer
  if not IsEmpty then
  begin
    LSize := fRasterizer.GetSize(fSVG);
    FWidth := LSize.cx;
    FHeight := LSize.cy;
  end
  else
  begin
    FWidth := 0;
    FHeight := 0;
  end;
end;

procedure TSVGMagic.LoadFromSource;
var
  StringStream: TStringStream;
begin
  // Clear previous SVG data
  fSVG.Clear;

  if FSource <> '' then
  begin
    StringStream := TStringStream.Create(FSource, TEncoding.UTF8);
    try
      StringStream.Position := 0;
      if not fSVG.LoadFromStream(StringStream) then
        raise ESVGException.Create(SVGMAGIC_ERROR_PARSING_SVG_TEXT);
      UpdateSizeInfo;
    finally
      StringStream.Free;
    end;
  end
  else
  begin
    FWidth := 0;
    FHeight := 0;
  end;
end;

procedure TSVGMagic.SourceFromStream(Stream: TStream);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    FSource := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

procedure TSVGMagic.LoadFromFile(const FileName: string);
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

procedure TSVGMagic.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
begin
  // Clear previous SVG data
  fSVG.Clear;

  // read and save the Source
  OldPos := Stream.Position;
  SourceFromStream(Stream);
  // Restore Position
  Stream.Position := OldPos;
  // Now create the SVG
  if not fSVG.LoadFromStream(Stream) then
    raise ESVGException.Create(SVGMAGIC_ERROR_PARSING_SVG_TEXT);
  UpdateSizeInfo;
end;

procedure TSVGMagic.SaveToFile(const FileName: string);
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

procedure TSVGMagic.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

procedure TSVGMagic.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  Bitmap: TBitmap;
  DestRect: TRect;
  Animation: TWSVGRasterizer.IAnimation;
  X, Y: Integer;
  Row: PRGBQuad;
  Gray: Byte;
  FixedR, FixedG, FixedB: Byte;
  BlendFunc: TBlendFunction;
begin
  if IsEmpty then
  begin
    // Clear the destination DC if empty
    FillRect(DC, Rect(Round(R.Left), Round(R.Top), Round(R.Right), Round(R.Bottom)), GetStockObject(WHITE_BRUSH));
    Exit;
  end;

  // Create a temporary bitmap for rendering
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(Round(R.Width), Round(R.Height));

    // Don't clear the bitmap - leave it transparent (like Image32 when fBkgndColor = clNone32)
    // TBitmap with pf32bit has transparent pixels by default (alpha = 0)

    // Setup destination rect
    DestRect := TRect.Create(0, 0, Bitmap.Width, Bitmap.Height);

    // Setup animation (not used for static rendering)
    Animation.m_Position := 0.0;
    Animation.m_pCustomData := nil;

    // Render SVG to bitmap using the rasterizer
    fRasterizer.Draw(fSVG, DestRect, KeepAspectRatio, True, Animation, Bitmap.Canvas);

    // Apply grayscale if needed (similar to Image32.Grayscale)
    if FGrayScale then
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Row := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          // Only process non-transparent pixels
          if Row^.rgbReserved > 0 then
          begin
            // Convert to grayscale using standard formula
            Gray := Round(Row^.rgbRed * 0.299 + Row^.rgbGreen * 0.587 + Row^.rgbBlue * 0.114);
            Row^.rgbRed := Gray;
            Row^.rgbGreen := Gray;
            Row^.rgbBlue := Gray;
          end;
          Inc(Row);
        end;
      end;
    end;

    // Apply fixed color if needed (similar to Image32.SetRGB)
    // NOTE: SVGMagic doesn't provide easy access to modify element colors before rendering,
    // so ApplyToRootOnly is not fully supported - all SVG elements get the fixed color
    if (FFixedColor <> TColors.SysDefault) and (FFixedColor <> TColors.SysNone) and not FGrayScale then
    begin
      FixedR := GetRValue(FFixedColor);
      FixedG := GetGValue(FFixedColor);
      FixedB := GetBValue(FFixedColor);

      for Y := 0 to Bitmap.Height - 1 do
      begin
        Row := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          // Only process non-transparent pixels
          if Row^.rgbReserved > 0 then
          begin
            // Replace RGB with FixedColor (like Image32 SetRGB)
            Row^.rgbRed := FixedR;
            Row^.rgbGreen := FixedG;
            Row^.rgbBlue := FixedB;
          end;
          Inc(Row);
        end;
      end;
    end;

    // Apply opacity (similar to Image32.ReduceOpacity)
    if FOpacity < 1.0 then
    begin
      for Y := 0 to Bitmap.Height - 1 do
      begin
        Row := Bitmap.ScanLine[Y];
        for X := 0 to Bitmap.Width - 1 do
        begin
          // Reduce opacity by multiplying alpha channel
          Row^.rgbReserved := Round(Row^.rgbReserved * FOpacity);
          Inc(Row);
        end;
      end;
    end;

    // Premultiply alpha before AlphaBlend (required by AC_SRC_ALPHA like Image32 does)
    for Y := 0 to Bitmap.Height - 1 do
    begin
      Row := Bitmap.ScanLine[Y];
      for X := 0 to Bitmap.Width - 1 do
      begin
        if Row^.rgbReserved > 0 then
        begin
          // Premultiply: RGB = RGB * Alpha / 255
          Row^.rgbRed := (Row^.rgbRed * Row^.rgbReserved) div 255;
          Row^.rgbGreen := (Row^.rgbGreen * Row^.rgbReserved) div 255;
          Row^.rgbBlue := (Row^.rgbBlue * Row^.rgbReserved) div 255;
        end;
        Inc(Row);
      end;
    end;

    // Copy bitmap to DC with transparency support (like Image32.CopyToDc)
    BlendFunc.BlendOp := AC_SRC_OVER;
    BlendFunc.BlendFlags := 0;
    BlendFunc.SourceConstantAlpha := 255; // Use per-pixel alpha
    BlendFunc.AlphaFormat := AC_SRC_ALPHA; // Per-pixel alpha

    WinApi.Windows.AlphaBlend(DC, Round(R.Left), Round(R.Top), Bitmap.Width, Bitmap.Height,
                       Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
                       BlendFunc);
  finally
    Bitmap.Free;
  end;
end;

function TSVGMagic.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FApplyFixedColorToRootOnly;
end;

function TSVGMagic.GetFixedColor: TColor;
begin
  Result := FFixedColor;
end;

function TSVGMagic.GetGrayScale: Boolean;
begin
  Result := FGrayScale;
end;

function TSVGMagic.GetHeight: Single;
begin
  Result := FHeight;
end;

function TSVGMagic.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TSVGMagic.GetSource: string;
begin
  Result := FSource;
end;

function TSVGMagic.GetWidth: Single;
begin
  Result := FWidth;
end;

procedure TSVGMagic.SetApplyFixedColorToRootOnly(Value: Boolean);
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

procedure TSVGMagic.SetFixedColor(const Color: TColor);
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

procedure TSVGMagic.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = FGrayScale then Exit;
  if FGrayScale or (FFixedColor <> TColors.SysDefault) then
    LoadFromSource;
  FGrayScale := IsGrayScale;
  FFixedColor := TColors.SysDefault;
end;

procedure TSVGMagic.SetOpacity(const Opacity: Single);
begin
  FOpacity := Opacity;
end;

procedure TSVGMagic.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

{ TSVGMagicFactory }

function TSVGMagicFactory.NewSvg: ISVG;
begin
  Result := TSVGMagic.Create;
end;

// Factory methods
function GetSVGMagicFactory: ISVGFactory;
begin
  Result := TSVGMagicFactory.Create;
end;

end.
