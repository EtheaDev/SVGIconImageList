{-----------------------------------------------------------------------------
 Unit Name: Image32SVGFactory
 Author:    Carlo Barazzxetta
 Purpose:   High-level encapsuation of Svg functionality for Image32 Library
 History:
-----------------------------------------------------------------------------}
unit Image32SVGFactory;

interface

Uses
  SVGInterfaces;

resourcestring
  IMAGE32_ERROR_PARSING_SVG_TEXT = 'Error parsing SVG with Image32 Library';

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
  Img32,             //Warning: from version 2.3 the default rendering engine is Image32
  Img32.SVG.Core,    //because is the best engine available with SVGIconImageList.
  Img32.SVG.Reader,  //If you don't want to use it change SVGIconImageList.inc
  Img32.Text,        //Otherwise you must add this search path:
  Img32.Vector;      //- SVGIconImageList\Image32\Source

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
    procedure UpdateSizeInfo(defaultWidth, defaultHeight: integer);
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

procedure TImage32SVG.UpdateSizeInfo(defaultWidth, defaultHeight: integer);
var
  vbox: TRectWH;
begin
  //nb: default widths should be the target image's dimensions
  //since these values will be used for SVG images that simply
  //specify their widths and heights as percentages
  vbox := fSvgReader.RootElement.GetViewbox;
  FWidth := vbox.Width;
  FHeight := vbox.Height;
end;

procedure TImage32SVG.LoadFromSource;
begin
  if FSource <> '' then
  begin
    if not fSvgReader.LoadFromString(FSource) then
      raise ESVGException.Create(IMAGE32_ERROR_PARSING_SVG_TEXT);
    UpdateSizeInfo(100, 100);
  end;
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
  fSvgReader.LoadFromStream(Stream);
  UpdateSizeInfo(100, 100);
end;

procedure TImage32SVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  dx,dy: double;
  LFixedColor: TColor32;
  dd: TDrawData;
begin
  //Define Image32 output size
  FImage32.SetSize(Round(R.Width), Round(R.Height));

  //Update FsvgReader BEFORE calling FsvgReader.DrawImage
  //to apply fixed color to root only
  if FApplyFixedColorToRootOnly and not FGrayScale and
    (FFixedColor <> TColors.SysDefault) and
    (FFixedColor <> TColors.SysNone) then
  begin
    LFixedColor := Color32(FFixedColor);
    dd := fSvgReader.RootElement.DrawData;
    dd.FillColor := LFixedColor;
    if dd.strokeColor <> clInvalid then
      dd.strokeColor := LFixedColor;
    fSvgReader.RootElement.DrawData := dd;
  end;

  FsvgReader.KeepAspectRatio := KeepAspectRatio;

  //Draw SVG image to FImage32
  FsvgReader.DrawImage(FImage32, True);

  //assuming KeepAspectRatio = true, prepare to center
  dx := (R.Width - FImage32.Width) *0.5;
  dy := (R.Height - FImage32.Height) *0.5;

  //Apply GrayScale and FixedColor to Image32
  if FGrayScale then
    FImage32.Grayscale
  else if (FFixedColor <> TColors.SysDefault) and
    not FApplyFixedColorToRootOnly then
      FImage32.SetRGB(Color32(FFixedColor));

  //Opacity applyed to Image32
  if FOpacity <> 1.0 then
    FImage32.ReduceOpacity(Round(FOpacity * 255));

  FImage32.CopyToDc(DC, Round(R.Left + dx), Round(R.Top + dy), True);
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
  FontManager.Load('Arial');
  FontManager.Load('Times New Roman');

end.
