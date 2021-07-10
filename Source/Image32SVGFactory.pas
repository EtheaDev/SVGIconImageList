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
  Image32_SVG_Writer,
  Image32_Ttf
  ;

resourcestring
  D2D_ERROR_NOT_AVAILABLE    = 'Windows SVG support is not available';
  D2D_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG Text: %s';
  D2D_ERROR_UNSUPPORTED_SVG  = '<style> or <text> elements and class="" attributes are not supported by Windows SVG';

type
  TImage32SVG = class(TInterfacedObject, ISVG)
  private
    fSvgReader: TSvgReader;
    FSource: String;
    FWidth: Single;
    FHeight: Single;
    FFixedColor: TColor;
    FApplyFixedColorToRootOnly: Boolean;
    FGrayScale: Boolean;
    FOpacity: Single;
    FImage32: TImage32;
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
  FImage32 := TImage32.Create;
  FImage32.Resampler := rBicubicResampler;
  FFixedColor := TColors.SysDefault; // clDefault
  FOpacity := 1.0;
end;

destructor TImage32SVG.Destroy;
begin
  fSvgReader.Free;
  FImage32.Free;
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
  Result := FApplyFixedColorToRootOnly;
end;

function TImage32SVG.GetFixedColor: TColor;
begin
  Result := FFixedColor;
end;

function TImage32SVG.GetGrayScale: Boolean;
begin
  Result := FGrayScale;
end;

function TImage32SVG.GetHeight: Single;
begin
  Result := FHeight;
end;

function TImage32SVG.GetOpacity: Single;
begin
  Result := FOpacity;
end;

function TImage32SVG.GetSource: string;
begin
  Result := FSource;
end;

function TImage32SVG.GetWidth: Single;
begin
  Result := FWidth;
end;

function TImage32SVG.IsEmpty: Boolean;
begin
  Result := fSvgReader.IsEmpty;
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
  FImage32.LoadFromStream(Stream);
end;

procedure TImage32SVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  LSvgRect: TRectF;
  LSourceRect, LDestRect: TRect;
begin
  //Define Image32 output size
  FImage32.SetSize(Round(R.Width), Round(R.Height));

  //Draw SVG image to Image32
  FsvgReader.DrawImage(FImage32, True);

  //GrayScale and FixedColor applyed to Image32
  if FGrayScale then
    FImage32.Grayscale
  else if (FFixedColor <> TColors.SysDefault) then
  begin
    if FApplyFixedColorToRootOnly then
    begin
      fSvgReader.RootElement.SetFillColor(Color32(FFixedColor));
      if fSvgReader.RootElement.HasStroke then
        fSvgReader.RootElement.SetStrokeColor(Color32(FFixedColor));
    end
    else
      FImage32.SetRGB(Color32(FFixedColor));
  end;

  //Opacity applyed to Image32
  if FOpacity <> 1.0 then
    FImage32.ReduceOpacity(Round(FOpacity * 255));

  //Centering Image to the output Rect if KeepAspectRatio is requested
  if KeepAspectRatio then
  begin
    LSvgRect := TRectF.Create(0, 0, FImage32.Width, FImage32.Height);
    LSvgRect := RectCenter(LSvgRect, R);
  end
  else
    LSvgRect := R;

  LSourceRect := TRect.Create(0, 0, FImage32.Width, FImage32.Height);
  LDestRect := TRect.Create(Round(LSvgRect.Left), Round(LSvgRect.Top), Round(LSvgRect.Right), Round(LSvgRect.Bottom));
  FImage32.CopyToDc(LSourceRect, LDestRect, DC, True);
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

procedure TImage32SVG.SetFixedColor(const Color: TColor);
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
  if IsGrayScale = FGrayScale then Exit;
  if FGrayScale or (FFixedColor <> TColors.SysDefault) then
    LoadFromSource;
  FGrayScale := IsGrayScale;
  FFixedColor := TColors.SysDefault;
end;

procedure TImage32SVG.SetOpacity(const Opacity: Single);
begin
  FOpacity := Opacity;
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
var
  LStream: TStringStream;
begin
  fSvgReader.LoadFromStream(Stream);
  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.Position := 0;
    LStream.LoadFromStream(Stream);
    FSource := LStream.DataString;
  finally
    LStream.Free;
  end;
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

initialization
  FontLibrary.Add('Arial');
  FontLibrary.Add('Times New Roman');

end.
