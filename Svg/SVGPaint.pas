{******************************************************************}
{ SVG fill classes                                                 }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-04-2008                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (deprecated xlink:href to href)                  }
{ Kiriakos Vlahos (fixed gradient transform)                       }
{ Kiriakos Vlahos (fixed LinearGradient and RadialGradient)        }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGPaint;

interface

uses
  Winapi.Windows, Winapi.GDIPOBJ, Winapi.GDIPAPI,
  System.UITypes, System.Classes,
  Xml.XmlIntf,
  SVGTypes, SVG;

type
  TColors = record
    Colors: packed array of ARGB;
    Positions: packed array of Single;
    Count: Integer;
  end;

  TSVGStop = class(TSVGObject)
  strict private
    FStop: TFloat;
    FStopColor: TColor;
    FOpacity: TFloat;

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadIn(const Node: IXMLNode); override;
    procedure PaintToGraphics(Graphics: TGPGraphics); override;
    procedure PaintToPath(Path: TGPGraphicsPath); override;

    property Stop: TFloat read FStop write FStop;
    property StopColor: TColor read FStopColor write FStopColor;

    property Opacity: TFloat read FOpacity write FOpacity;
  end;

  TSVGFiller = class(TSVGMatrix)
  public
    procedure ReadIn(const Node: IXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TGPBrush; virtual; abstract;
    procedure PaintToGraphics(Graphics: TGPGraphics); override;
    procedure PaintToPath(Path: TGPGraphicsPath); override;
  end;

  TSVGGradient = class(TSVGFiller)
  {
    SpreadMethod is not implemented
    Assumed to be repeat for LinearGradient and pad for RadialGradient
  }
  private
    FURI: string;
    FGradientUnits: TGradientUnits;
  protected
    function GetColors(Alpha: Byte): TColors; virtual;
  public
    procedure ReadIn(const Node: IXMLNode); override;
  end;

  TSVGLinearGradient = class(TSVGGradient)
  private
    FX1: TFloat;
    FY1: TFloat;
    FX2: TFloat;
    FY2: TFloat;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadIn(const Node: IXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TGPBrush; override;
    procedure Clear; override;

    property X1: TFloat read FX1 write FX1;
    property Y1: TFloat read FY1 write FY1;
    property X2: TFloat read FX2 write FX2;
    property Y2: TFloat read FY2 write FY2;
  end;

  TSVGRadialGradient = class(TSVGGradient)
  private
    FCX: TFloat;
    FCY: TFloat;
    FR: TFloat;
    FFX: TFloat;
    FFY: TFloat;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear; override;
    procedure ReadIn(const Node: IXMLNode); override;
    function GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TGPBrush; override;

    property CX: TFloat read FCX write FCX;
    property CY: TFloat read FCY write FCY;
    property R: TFloat read FR write FR;
    property FX: TFloat read FFX write FFX;
    property FY: TFloat read FFY write FFY;
  end;

implementation

uses
  System.Types, System.SysUtils, System.Math.Vectors,
  SVGParse, SVGStyle, SVGProperties, SVGColor,
  GDIPUtils;

// TSVGStop

procedure TSVGStop.PaintToPath(Path: TGPGraphicsPath);
begin
end;

procedure TSVGStop.ReadIn(const Node: IXMLNode);
var
  S: string;
begin
  inherited;
  LoadPercent(Node, 'offset', FStop);

  LoadString(Node, 'stop-color', S);
  if GetRoot.Grayscale then
    FStopColor := GetSVGGrayscale(GetSVGColor(S))
   else
    FStopColor := GetSVGColor(S);

  if FStopColor = INHERIT then
  begin
    S := Style['stop-color'];
    if GetRoot.Grayscale then
      FStopColor := GetSVGGrayscale(GetSVGColor(S))
    else
      FStopColor := GetSVGColor(S);
  end;

  if (GetRoot.FixedColor  <> TSVGColor.inherit_color) and
     (integer(FStopColor) <> INHERIT) and
     (integer(FStopColor) <> SVG_NONE_COLOR) then
    FStopColor := SVGColorToColor(GetRoot.FixedColor);

  S := Style['stop-opacity'];
  if (S <> '') then
    FOpacity := ParsePercent(S)
  else
    FOpacity := 1;

  if (FOpacity < 0) then
    FOpacity := 0;

  if (FOpacity > 1) then
    FOpacity := 1;
end;

procedure TSVGStop.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGStop then
  begin
    TSVGStop(Dest).FStop := FStop;
    TSVGStop(Dest).FStopColor := FStopColor;
  end;
end;

procedure TSVGStop.PaintToGraphics(Graphics: TGPGraphics);
begin
end;

// TSVGFiller

procedure TSVGFiller.PaintToPath(Path: TGPGraphicsPath);
begin
end;           

procedure TSVGFiller.ReadIn(const Node: IXMLNode);
begin
  inherited;
  Display := 0;
end;

procedure TSVGFiller.PaintToGraphics(Graphics: TGPGraphics);
begin
end;

// TSVGGradient

procedure TSVGGradient.ReadIn(const Node: IXMLNode);
var
  C: Integer;
  Stop: TSVGStop;
  Matrix: TMatrix;
begin
  inherited;

  LoadGradientUnits(Node, FGradientUnits);

  for C := 0 to Node.childNodes.count - 1 do
   if Node.childNodes[C].nodeName = 'stop' then
   begin
     Stop := TSVGStop.Create(Self);
     Stop.ReadIn(Node.childNodes[C]);
   end;

  FURI := Style['xlink:href'];
  if FURI = '' then
    FURI := Style['href'];
  if FURI = '' then
    LoadString(Node, 'xlink:href', FURI);
  if FURI = '' then
    LoadString(Node, 'href', FURI);


  if FURI <> '' then
  begin
    FURI := Trim(FURI);
    if (FURI <> '') and (FURI[1] = '#') then
      FURI := Copy(FURI, 2, MaxInt);
  end;

  FillChar(Matrix, SizeOf(Matrix), 0);
  LoadTransform(Node, 'gradientTransform', Matrix);
  PureMatrix := Matrix;
end;

// TSVGLinearGradient

procedure TSVGLinearGradient.ReadIn(const Node: IXMLNode);
begin
  inherited;
  LoadLength(Node, 'x1', FX1);
  LoadLength(Node, 'y1', FY1);
  LoadLength(Node, 'x2', FX2);
  LoadLength(Node, 'y2', FY2);
end;

procedure TSVGLinearGradient.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGLinearGradient then
  begin
    TSVGLinearGradient(Dest).FX1 := FX1;
    TSVGLinearGradient(Dest).FY1 := FY1;
    TSVGLinearGradient(Dest).FX2 := FX2;
    TSVGLinearGradient(Dest).FY2 := FY2;
  end;
end;

procedure TSVGLinearGradient.Clear;
begin
  inherited;
  FX1 := INHERIT;
  FX2 := INHERIT;
  FY1 := INHERIT;
  FY2 := INHERIT;
end;

function TSVGLinearGradient.GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TGPBrush;
var
  Brush: TGPLinearGradientBrush;
  TGP: TGPMatrix;
  Colors: TColors;
  BoundsRect: TRectF;
  MX1, MX2, MY1, MY2: TFloat;
begin
  BoundsRect :=  DestObject.ObjectBounds;
  if FX1 = INHERIT then MX1 := BoundsRect.Left else MX1 := FX1;
  if FX2 = INHERIT then MX2 := BoundsRect.Right else MX2 := FX2;
  if FY1 = INHERIT then MY1 := BoundsRect.Top else MY1 := FY1;
  if FY2 = INHERIT then MY2 := BoundsRect.Top else MY2 := FY2;
  if FGradientUnits = guObjectBoundingBox then begin
    // X1, X2, Y1, Y2 are relative to the Object Bounding Rect
    if FX1 <> INHERIT then MX1 := BoundsRect.Left + FX1 * BoundsRect.Width;
    if FX2 <> INHERIT then MX2 := BoundsRect.Left + FX2 * BoundsRect.Width;
    if FY1 <> INHERIT then MY1 := BoundsRect.Top + FY1 * BoundsRect.Height;
    if FY2 <> INHERIT then MY2 := BoundsRect.Top + FY2 * BoundsRect.Height;
  end;

  Brush := TGPLinearGradientBrush.Create(MakePoint(MX1, MY1), MakePoint(MX2, MY2), 0, 0);

  Colors := GetColors(Alpha);

  Brush.SetInterpolationColors(PGPColor(Colors.Colors),
    PSingle(Colors.Positions), Colors.Count);

  if PureMatrix.m33 = 1 then
  begin
    TGP := GetGPMatrix(PureMatrix);
    Brush.SetTransform(TGP);
    TGP.Free;
  end;

  Result := Brush;
end;

// TSVGRadialGradient

procedure TSVGRadialGradient.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TSVGRadialGradient then
  begin
    TSVGRadialGradient(Dest).FCX := FCX;
    TSVGRadialGradient(Dest).FCY := FCY;
    TSVGRadialGradient(Dest).FFX := FFX;
    TSVGRadialGradient(Dest).FFY := FFY;
    TSVGRadialGradient(Dest).FR := FR;
  end;
end;

procedure TSVGRadialGradient.Clear;
begin
  inherited;
  FCX := INHERIT;
  FCY := INHERIT;
  FR := INHERIT;
  FFX := FCX;
  FFY := FCY;
end;

procedure TSVGRadialGradient.ReadIn(const Node: IXMLNode);
begin
  inherited;
  LoadLength(Node, 'cx', FCX);
  LoadLength(Node, 'cy', FCY);
  LoadLength(Node, 'r', FR);
  LoadLength(Node, 'fx', FFX);
  LoadLength(Node, 'fy', FFY);
  if FFX = INHERIT then
    FFX := FCX;
  if FFY = INHERIT then
    FFY := FCY;
end;

function TSVGRadialGradient.GetBrush(Alpha: Byte; const DestObject: TSVGBasic): TGPBrush;
var
  Brush: TGPPathGradientBrush;
  Path: TGPGraphicsPath;
  TGP: TGPMatrix;
  Colors: TColors;
  RevColors: TColors;
  BoundsRect: TRectF;
  MCX, MCY, MR, MFX, MFY: TFloat;
  i: integer;
begin
  BoundsRect :=  DestObject.ObjectBounds;
  if FCX = INHERIT then MCX := BoundsRect.Left + 0.5 * BoundsRect.Width else MCX := FCX;
  if FFX = INHERIT then MFX := MCX else MFX := FFX;
  if FCY = INHERIT then MCY := BoundsRect.Top + 0.5 * BoundsRect.Height else MCY := FCY;
  if FFY = INHERIT then MFY := MCY else MFY := FFY;
  if FR = INHERIT then
    MR := 0.5 * Sqrt(Sqr(BoundsRect.Width) + Sqr(BoundsRect.Height))/Sqrt(2)
  else
    MR := FR;
  if FGradientUnits = guObjectBoundingBox then begin
    // CX, CY, R, FX, FY are relative to the Object Bounding Rect
    if FCX <> INHERIT then MCX := BoundsRect.Left + FCX * BoundsRect.Width;
    if FFX <> INHERIT then MFX := BoundsRect.Left + FFX * BoundsRect.Width;
    if FCY <> INHERIT then MCY := BoundsRect.Top + FCY * BoundsRect.Height;
    if FFY <> INHERIT then MFY := BoundsRect.Top + FFY * BoundsRect.Height;
    if FR <> INHERIT then
      MR := FR * Sqrt(Sqr(BoundsRect.Width) + Sqr(BoundsRect.Height))/Sqrt(2);
  end;

  Path := TGPGraphicsPath.Create;
  Path.AddEllipse(MCX - MR, MCY - MR, 2 * MR, 2 * MR);

  Brush := TGPPathGradientBrush.Create(Path);
  Path.Free;

  Colors := GetColors(Alpha);

  SetLength(RevColors.Colors, Colors.Count);
  SetLength(RevColors.Positions, Colors.Count);
  // Reverse colors! TGPPathGradientBrush uses colors from outside to inside unlike svg
  for i := 0 to Colors.Count - 1 do
  begin
    RevColors.Colors[i] := Colors.Colors[Colors.Count - 1 - i];
    RevColors.Positions[i] := 1 - Colors.Positions[Colors.Count - 1 - i];
  end;
  if Colors.Count > 0 then
    // Temporarily store last color. Used in TSVGBasic.BeforePaint
    DestObject.FillColor := Integer(RevColors.Colors[0]);

  Brush.SetInterpolationColors(PARGB(RevColors.Colors), PSingle(RevColors.Positions), Colors.Count);

  if (FFX <> INHERIT) and (FFY <> INHERIT) then
    Brush.SetCenterPoint(MakePoint(MFX, MFY));

  if PureMatrix.m33 = 1 then
  begin
    TGP := GetGPMatrix(PureMatrix);
    Brush.SetTransform(TGP);
    TGP.Free;
  end;

  Result := Brush;
end;

function TSVGGradient.GetColors(Alpha: Byte): TColors;
var
  C, Start, ColorCount: Integer;
  Stop: TSVGStop;
  Item: TSVGGradient;
begin
  Result.Count := 0;
  if FURI = '' then
    Item := Self
  else
  begin
    Item := TSVGGradient(GetRoot.FindByID(FURI));
    if not (Item is TSVGGradient) then
      Exit;
  end;

  Start := 0;
  ColorCount := Item.Count;

  if Item.Count = 0 then
    Exit;

  if TSVGStop(Item.Items[ColorCount - 1]).Stop < 1 then
    Inc(ColorCount);

  if TSVGStop(Item.Items[0]).Stop > 0 then
  begin
    Inc(ColorCount);
    Inc(Start);
  end;

  SetLength(Result.Colors, ColorCount);
  SetLength(Result.Positions, ColorCount);

  if Start > 0 then
  begin
    Stop := TSVGStop(Item.Items[0]);
    Result.Colors[0] := ConvertColor(Stop.StopColor, Round(Alpha * Stop.Opacity));
    Result.Positions[0] := 0;
  end;

  for C := 0 to Item.Count - 1 do
  begin
    Stop := TSVGStop(Item.Items[C]);
    Result.Colors[C + Start] := ConvertColor(Stop.StopColor, Round(Alpha * Stop.Opacity));
    Result.Positions[C + Start] := Stop.Stop;
  end;

  if (ColorCount - Start) > Item.Count then
  begin
    Stop := TSVGStop(Item.Items[Item.Count - 1]);
    Result.Colors[ColorCount - 1] := ConvertColor(Stop.StopColor, Round(Alpha * Stop.Opacity));
    Result.Positions[ColorCount - 1] := 1;
  end;

  Result.Count := ColorCount;
end;

end.
