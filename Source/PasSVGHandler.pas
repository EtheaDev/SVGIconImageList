{-----------------------------------------------------------------------------
 Unit Name: D2DSVGHandler
 Author:    PyScripter
 Purpose:   High-level encapsuation of Direct2D Svg functionality
 History:
-----------------------------------------------------------------------------}
unit PasSVGHandler;

interface
Uses
  Winapi.D2D1,
  SVGInterfaces;

// Factory Methods
function GetPasSVGHandler: ISVGHandler;

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
  Svg,
  SvgTypes;

type

  TPasSVG = class(TInterfacedObject, ISVG)
  private
    fSvgDoc: TSVG;
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const Color: TColor);
    function GetSource: string;
    procedure SetSource(const ASource: string);
    // procedures
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPasSVGHandler = class(TInterfacedObject, ISvgHandler)
    function NewSvg: ISVG;
  end;

{ TPasSVG }

constructor TPasSVG.Create;
begin
  inherited;
  fSvgDoc := TSVG.Create;
end;

procedure TPasSVG.LoadFromFile(const FileName: string);
begin
  fSvgDoc.LoadFromFile(FileName);
end;

destructor TPasSVG.Destroy;
begin
  fSvgDoc.Free;
  inherited;
end;

function TPasSVG.GetFixedColor: TColor;
begin
  Result := fSvgDoc.FixedColor;
end;

function TPasSVG.GetGrayScale: Boolean;
begin
  Result := fSvgDoc.GrayScale;
end;

function TPasSVG.GetHeight: Single;
begin
  Result := fSvgDoc.Height;
end;

function TPasSVG.GetOpacity: Single;
// ReadOnly property
begin
  Result := 1;
end;

function TPasSVG.GetSource: string;
begin
  Result := fSvgDoc.Source;
end;

function TPasSVG.GetWidth: Single;
begin
  Result := fSvgDoc.Width;
end;

procedure TPasSVG.LoadFromStream(Stream: TStream);
begin
  fSvgDoc.LoadFromStream(Stream);
end;

procedure TPasSVG.PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean);
var
  SvgRect : TRectF;
  GPRectF: TGPRectF;
begin
  SvgRect:= R;
  if (fSvgDoc.Width > 0) and (fSvgDoc.Height > 0) and KeepAspectRatio then
  begin
      SvgRect := TRectF.Create(0, 0, fSvgDoc.Width, fSvgDoc.Height);
      SvgRect := SvgRect.FitInto(R);
  end;
  GPRectF := Winapi.GDIPAPI.MakeRect(SvgRect.Left, SvgRect.Top, SvgRect.Width, SvgRect.Height);
  fSvgDoc.PaintTo(DC, GPRectF, nil, 0);
end;

procedure TPasSVG.SaveToFile(const FileName: string);
begin
  fSvgDoc.SaveToFile(FileName);
end;

procedure TPasSVG.SaveToStream(Stream: TStream);
begin
  fSvgDoc.SaveToStream(Stream);
end;


procedure TPasSVG.SetFixedColor(const Color: TColor);
begin
  if Color < 0  then
    fSvgDoc.FixedColor := GetSysColor(Color and $000000FF)
  else
    fSvgDoc.FixedColor := Color;
end;


procedure TPasSVG.SetGrayScale(const IsGrayScale: Boolean);
begin
  fSvgDoc.GrayScale := IsGrayScale;
end;

procedure TPasSVG.SetOpacity(const Opacity: Single);
begin
  fSvgDoc.SVGOpacity := Opacity;
end;

procedure TPasSVG.SetSource(const ASource: string);
begin
  fSvgDoc.LoadFromText(ASource);
end;

{ TPasSVGHandler }

function TPasSVGHandler.NewSvg: ISVG;
begin
  Result := TPasSVG.Create;
end;

// Factory methods
function GetPasSVGHandler: ISVGHandler;
begin
  Result := TPasSVGHandler.Create;
end;

end.
