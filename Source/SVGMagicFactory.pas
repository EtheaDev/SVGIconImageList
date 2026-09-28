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
  System.Math,
  Vcl.Graphics,
  Xml.VerySimple,
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
    FOriginalSource: String; // Original SVG source without color modifications
    //The SVG recolored with FixedColor, kept until color, root-only or source
    //change: a new TWSVG per paint re-parsed the XML every time and added a
    //new entry (new UUID) to the rasterizer cache at every paint
    FColoredSVG: TWSVG;
    FColoredKey: string;
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
    function ApplyFixedColorToSVGSource(const SVGSource: string): string;
    function GetColoredSVG: TWSVG;
    procedure ResetColoredSVG;
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
  ResetColoredSVG;
  fRasterizer.Free;
  fSVGGraphic.Free;
  fSVG.Free;
  inherited;
end;

procedure TSVGMagic.ResetColoredSVG;
begin
  FreeAndNil(FColoredSVG);
  FColoredKey := '';
end;

function TSVGMagic.GetColoredSVG: TWSVG;
var
  LKey: string;
  LStream: TStringStream;
begin
  LKey := Format('%d|%d', [FFixedColor, Ord(FApplyFixedColorToRootOnly)]);
  if Assigned(FColoredSVG) and (LKey = FColoredKey) then
    Exit(FColoredSVG);
  ResetColoredSVG;
  Result := nil;
  try
    FColoredSVG := TWSVG.Create;
    LStream := TStringStream.Create(ApplyFixedColorToSVGSource(FOriginalSource), TEncoding.UTF8);
    try
      LStream.Position := 0;
      if not FColoredSVG.LoadFromStream(LStream) then
      begin
        // If loading fails, fall back to original
        ResetColoredSVG;
        Exit;
      end;
    finally
      LStream.Free;
    end;
    FColoredKey := LKey;
    Result := FColoredSVG;
  except
    // If recoloring fails, fall back to original
    ResetColoredSVG;
  end;
end;

procedure TSVGMagic.Clear;
begin
  ResetColoredSVG;
  fSVG.Clear;
  FSource := '';
  FOriginalSource := '';
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
  ResetColoredSVG;
  fSVG.Clear;

  if FOriginalSource <> '' then
  begin
    StringStream := TStringStream.Create(FOriginalSource, TEncoding.UTF8);
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
    FOriginalSource := LStream.DataString;
    FSource := FOriginalSource; // Initially same as original
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
  ResetColoredSVG;
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
  // Save the original source (not the modified one with fixed colors)
  Buffer := TEncoding.UTF8.GetBytes(FOriginalSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

function TSVGMagic.ApplyFixedColorToSVGSource(const SVGSource: string): string;
var
  ColorStr: string;
  RgbColor: TColor;
  R, G, B: Byte;
  XmlDoc: TXmlVerySimple;
  StringStream: TStringStream;
  OutputStringStream: TStringStream;

  // none and url(...) (gradients, patterns) are not colors to replace
  function IsPaintColor(const Value: string): Boolean;
  var
    LValue: string;
  begin
    LValue := LowerCase(Trim(Value));
    Result := (LValue <> 'none') and not LValue.StartsWith('url');
  end;

  procedure ProcessNode(Node: TXMLNode; IsRoot: Boolean);
  var
    I: Integer;
    NodeName: string;
  begin
    if Node = nil then
      Exit;

    NodeName := LowerCase(Node.NodeName);

    // Check if this is an SVG element that can have fill/stroke
    if (NodeName = 'svg') or (NodeName = 'g') or (NodeName = 'path') or
       (NodeName = 'rect') or (NodeName = 'circle') or (NodeName = 'ellipse') or
       (NodeName = 'line') or (NodeName = 'polyline') or (NodeName = 'polygon') or
       (NodeName = 'text') or (NodeName = 'tspan') then
    begin
      // Only process if ApplyToRootOnly is false OR this is the root element
      if (not FApplyFixedColorToRootOnly) or IsRoot then
      begin
        // fill: an explicit color is replaced, fill="none" (outline icons)
        // is kept; the root gets the color when it has no fill, since that
        // default is what the children without a fill inherit
        if Node.HasAttribute('fill') then
        begin
          if IsPaintColor(Node.Attributes['fill']) then
            Node.SetAttribute('fill', ColorStr);
        end
        else if IsRoot then
          Node.SetAttribute('fill', ColorStr);

        // stroke: only where the element has one
        if Node.HasAttribute('stroke') and IsPaintColor(Node.Attributes['stroke']) then
          Node.SetAttribute('stroke', ColorStr);

        // "currentColor" is resolved against the color attribute
        if IsRoot then
          Node.SetAttribute('color', ColorStr);
      end;
    end;

    // Recursively process child nodes
    for I := 0 to Node.ChildNodes.Count - 1 do
      ProcessNode(Node.ChildNodes[I], False);
  end;

begin
  Result := SVGSource;

  if (FFixedColor = TColors.SysDefault) or (FFixedColor = TColors.SysNone) then
    Exit;

  // Convert TColor to RGB
  RgbColor := ColorToRGB(FFixedColor);
  R := GetRValue(RgbColor);
  G := GetGValue(RgbColor);
  B := GetBValue(RgbColor);
  ColorStr := Format('rgb(%d,%d,%d)', [R, G, B]);

  XmlDoc := TXmlVerySimple.Create;
  try
    // Parse the SVG source
    StringStream := TStringStream.Create(SVGSource, TEncoding.UTF8);
    try
      XmlDoc.LoadFromStream(StringStream);
    finally
      StringStream.Free;
    end;

    // Process the root node (DocumentElement is the public property)
    if XmlDoc.DocumentElement <> nil then
      ProcessNode(XmlDoc.DocumentElement, True);

    // Convert back to string
    OutputStringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      XmlDoc.SaveToStream(OutputStringStream);
      Result := OutputStringStream.DataString;
    finally
      OutputStringStream.Free;
    end;
  finally
    XmlDoc.Free;
  end;
end;

procedure TSVGMagic.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  Bitmap: TBitmap;
  DestRect: TRect;
  Animation: TWSVGRasterizer.IAnimation;
  X, Y: Integer;
  Row: PRGBQuad;
  Gray: Byte;
  BlendFunc: TBlendFunction;
  TempSVG: TWSVG;
  NeedTempSVG: Boolean;
begin
  // An empty document paints nothing (as the other engines do)
  if IsEmpty then
    Exit;

  // Check if we need to apply fixed color by modifying the source
  NeedTempSVG := (FFixedColor <> TColors.SysDefault) and (FFixedColor <> TColors.SysNone) and not FGrayScale;

  // If we need to apply fixed color, use the SVG with modified source
  if NeedTempSVG then
  begin
    TempSVG := GetColoredSVG;
    NeedTempSVG := Assigned(TempSVG);
  end
  else
    TempSVG := nil;

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
    // Use TempSVG if we applied fixed color, otherwise use fSVG
    if NeedTempSVG and Assigned(TempSVG) then
      fRasterizer.Draw(TempSVG, DestRect, KeepAspectRatio, True, Animation, Bitmap.Canvas)
    else
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
  // Always return the original source (not the modified one with fixed colors)
  Result := FOriginalSource;
end;

function TSVGMagic.GetWidth: Single;
begin
  Result := FWidth;
end;

procedure TSVGMagic.SetApplyFixedColorToRootOnly(Value: Boolean);
begin
  if FApplyFixedColorToRootOnly <> Value then
  begin
    FApplyFixedColorToRootOnly := Value;
  end;
end;

procedure TSVGMagic.SetFixedColor(const Color: TColor);
begin
  if Color = FFixedColor then Exit;

  // Convert system color to RGB
  if Color < 0 then
    FFixedColor := GetSysColor(Color and $000000FF)
  else
    FFixedColor := Color;

  FGrayScale := False;
end;

procedure TSVGMagic.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = FGrayScale then Exit;
  FGrayScale := IsGrayScale;
  FFixedColor := TColors.SysDefault;
end;

procedure TSVGMagic.SetOpacity(const Opacity: Single);
begin
  FOpacity := EnsureRange(Opacity, 0, 1);
end;

procedure TSVGMagic.SetSource(const ASource: string);
begin
  if FOriginalSource <> ASource then
  begin
    FOriginalSource := ASource;
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
