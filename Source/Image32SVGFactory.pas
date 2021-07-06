{-----------------------------------------------------------------------------
 Unit Name: Image32SVGFactory
 Author:    Carlo Barazzxetta
 Purpose:   High-level encapsuation of Svg functionality for Image32 Library
 History:
-----------------------------------------------------------------------------}
unit Image32SVGFactory;
interface
Uses
  Winapi.D2D1,
  SVGInterfaces;
// Factory Methods
function GetImage32SVGFactory: ISVGFactory;
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
  Image32,
  Image32_SVG_Core,
  Image32_SVG_Reader,
  Image32_SVG_Writer
;
resourcestring
  D2D_ERROR_NOT_AVAILABLE    = 'Windows SVG support is not available';
  D2D_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  D2D_ERROR_UNSUPPORTED_SVG  = '<style> or <text> elements and class="" attributes are not supported by Windows SVG';

type
  TImage32SVG = class(TInterfacedObject, ISVG)
  private
    fSvgReader: TSvgReader;
    fSource: String;
    fWidth: Single;
    fHeight: Single;
    fFixedColor: TColor;
    fApplyFixedColorToRootOnly: Boolean;
    fGrayScale: Boolean;
    fImage: TImage32;
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
    {$IFDEF CheckForUnsupportedSvg}
    procedure CheckForUnsupportedSvg;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TImage32SVGFactory = class(TInterfacedObject, ISVGFactory)
    function NewSvg: ISVG;
  end;

{ TImage32SVG }
procedure TImage32SVG.Clear;
Const
  EmptySvg = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';
begin
  SetSource(EmptySvg);
end;

constructor TImage32SVG.Create;
begin
  inherited;
  fSvgReader := TSvgReader.Create;
  fSvgReader.AddFont('Arial');
  fSvgReader.AddFont('Arial Bold');
  fSvgReader.AddFont('Arial Italic');
  fSvgReader.AddFont('Arial Bold Italic');
  fSvgReader.AddFont('Times New Roman');
  fSvgReader.AddFont('Times New Roman Bold');
  fSvgReader.AddFont('Times New Roman Italic');
  fSvgReader.AddFont('Times New Roman Bold Italic');
  fSvgReader.AddFont('DejaVu Sans Mono');

  fImage := TImage32.Create;
  fImage.Resampler := rBicubicResampler;
  fFixedColor := TColors.SysDefault; // clDefault
end;

destructor TImage32SVG.Destroy;
begin
  fSvgReader.Free;
  fImage.Free;
  inherited;
end;

procedure TImage32SVG.LoadFromFile(const FileName: string);
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

function TImage32SVG.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := fApplyFixedColorToRootOnly;
end;

function TImage32SVG.GetFixedColor: TColor;
begin
  Result := fFixedColor;
end;

function TImage32SVG.GetGrayScale: Boolean;
begin
  Result := fGrayScale;
end;

function TImage32SVG.GetHeight: Single;
begin
  Result := fHeight;
end;

function TImage32SVG.GetOpacity: Single;
begin
  Result := 1;
  //TODO: Opacity from fImage
end;

function TImage32SVG.GetSource: string;
begin
  Result := FSource;
end;

function TImage32SVG.GetWidth: Single;
begin
  Result := fWidth;
end;

function TImage32SVG.IsEmpty: Boolean;
begin
  if fImage = nil then Exit(True);
  Result := fImage.IsEmpty;
end;

procedure TImage32SVG.LoadFromSource;
begin
  fSvgReader.LoadFromString(FSource);
end;

procedure TImage32SVG.LoadFromStream(Stream: TStream);
Var
  OldPos : Int64;
begin
  // read and save the Source
  OldPos := Stream.Position;
  SourceFromStream(Stream);
  // Restore Position
  Stream.Position := OldPos;
  // Now create the SVG
  fImage.LoadFromStream(Stream);
end;

procedure TImage32SVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
begin
  fImage.SetSize(Round(R.Width), Round(R.Height));
  FsvgReader.DrawImage(fImage, true);
  fImage.SaveToFile('C:\temp\temp.svg');
  fImage.CopyToDc(DC, 0, 0, True);
end;

procedure TImage32SVG.SaveToFile(const FileName: string);
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

procedure TImage32SVG.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  Buffer := TEncoding.UTF8.GetBytes(FSource);
  Stream.WriteBuffer(Buffer, Length(Buffer))
end;

procedure TImage32SVG.SetApplyFixedColorToRootOnly(Value: Boolean);
var
  Color: TColor;
begin
  if fApplyFixedColorToRootOnly <> Value then
  begin
    fApplyFixedColorToRootOnly := Value;
    if fFixedColor <> TColors.SysDefault then
    begin
       Color := fFixedColor;
       fFixedColor := TColors.SysDefault;
       LoadFromSource;
       SetFixedColor(Color);
    end;
  end;
end;

procedure TImage32SVG.SetFixedColor(const Color: TColor);
begin
  if Color = fFixedColor then Exit;
  if (fGrayScale and (Color <> TColors.SysDefault)) or
    ((fFixedColor <> TColors.SysDefault) and (Color = TColors.SysDefault))
  then
    LoadFromSource;
  if Color < 0  then
    fFixedColor := GetSysColor(Color and $000000FF)
  else
    fFixedColor := Color;
  fGrayScale := False;
  (* TODO apply fixedcolor to SVG
  if (FFixedColor <> TColors.SysDefault) and Assigned(fImage) then
  begin
    fImage.GetRoot(Root);
    with TColors(fFixedColor) do
      NewColor :=  D2D1ColorF(r/255, g/255, b/255, 1);
    Root.SetAttributeValue('fill', D2D1_SVG_ATTRIBUTE_POD_TYPE_COLOR,
              @NewColor, SizeOf(NewColor));
    if not fApplyFixedColorToRootOnly then
      RecolorSubtree(Root, NewColor)
    else
      RecolorAttribute(Root, 'stroke', NewColor);
  end;
  *)
end;

// Converts any color to grayscale
function GrayScaleColor(Color : TD2D1ColorF) : TD2D1ColorF;
var
  LGray : Single;
begin
  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := 0.299 * Color.R + 0.587 * Color.G + 0.114 * Color.B;
  // set the result to the new grayscale color including the alpha info
  Result := D2D1ColorF(LGray, LGray, LGray, Color.A);
end;

procedure TImage32SVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  if IsGrayScale = fGrayScale then Exit;
  if fGrayScale or (fFixedColor <> TColors.SysDefault) then
    LoadFromSource;
  fGrayScale := IsGrayScale;
  fFixedColor := TColors.SysDefault;
  (* TODO: apply grayscale to SVG
  if fGrayScale then
  begin
    fImage.GetRoot(Root);
    GrayScaleSubtree(Root);
  end;
  *)
end;

procedure TImage32SVG.SetOpacity(const Opacity: Single);
begin
  if Assigned(fImage) then
  begin
    (* TODO: apply opacity to SVG
    fImage.GetRoot(Root);
    if Assigned(Root) then
      Root.SetAttributeValue('opacity', D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT,
        @Opacity, SizeOf(Opacity));
    *)
  end;
end;
procedure TImage32SVG.SetSource(const ASource: string);
begin
  if FSource <> ASource then
  begin
    FSource := ASource;
    LoadFromSource;
  end;
end;

procedure TImage32SVG.SourceFromStream(Stream: TStream);
begin
  fSvgReader.LoadFromStream(Stream);
  fSource := fSvgReader.ToString;
end;

{ TImage32SVGFactory }
function TImage32SVGFactory.NewSvg: ISVG;
begin
  Result := TImage32SVG.Create;
end;

// Factory methods
function GetImage32SVGFactory: ISVGFactory;
begin
  Result := TImage32SVGFactory.Create;
end;

end.
