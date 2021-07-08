unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.26                                                            *
* Date      :  8 July 2021                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Read SVG 2.0 files                                              *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Image32, Image32_SVG_Core, Image32_Vector, Image32_Draw,
  Image32_Transform, Image32_Ttf;

type
  TElement          = class;

  TDrawInfo = record
    currentColor  : TColor32;
    fillColor     : TColor32;
    fillRule      : TFillRule;
    fillEl        : AnsiString;
    strokeColor   : TColor32;
    strokeWidth   : TValue;
    strokeCap     : TEndStyle;
    strokeJoin    : TJoinStyle;
    strokeMitLim  : double;
    strokeEl      : AnsiString;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    fontInfo      : TSVGFontInfo;
    markerStart   : AnsiString;
    markerMiddle  : AnsiString;
    markerEnd     : AnsiString;
    filterEl      : AnsiString;
    maskEl        : AnsiString;
    clipPathEl    : AnsiString;
    opacity       : integer;
    matrix        : TMatrixD;
    visible       : Boolean;
    InUse         : Boolean; //avoids <USE> recursion
    bounds        : TRectD;
  end;

  TSvgReader = class;

  TElementClass = class of TElement;
  TElement = class
  private
    fParent         : TElement;
    fParserEl       : TSvgTreeEl;
    fReader         : TSvgReader;
{$IFDEF XPLAT_GENERICS}
    fChilds         : TList<TElement>;
{$ELSE}
    fChilds         : TList;
{$ENDIF}
    function  FindRefElement(refname: AnsiString): TElement;
  protected
    fDrawInfo : TDrawInfo;      //currently both static and dynamic vars
    elRectWH  : TValueRecWH;    //multifunction variable
    function  IsFirstChild: Boolean;
    procedure LoadAttributes;
    procedure LoadAttribute(attrib: PSvgAttrib);
    function  LoadContent: Boolean; virtual;
    //GetRelFracLimit: ie when to assume untyped vals are relative vals
    function  GetRelFracLimit: double; virtual;
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); virtual;
    procedure DrawChildren(image: TImage32; drawInfo: TDrawInfo); virtual;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); virtual;
    destructor  Destroy; override;
  end;

  TSvgElement = class(TElement)
  protected
    viewboxWH : TRectWH;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
    function GetViewbox: TRect;
  end;

  TSvgReader = class
  private
    fSvgParser        : TSvgParser;
    fBkgndColor       : TColor32;
    fTempImage        : TImage32;
    fBlurQuality      : integer;
    fIdList           : TStringList;
    fClassStyles      : TClassStylesList;
    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;
    fImgRenderer      : TImageRenderer;
    fRootElement      : TSvgElement;
    fFontCache        : TGlyphCache;
    function  LoadInternal: Boolean;
    function  GetIsEmpty: Boolean;
    procedure SetBlurQuality(quality: integer);
  protected
    userSpaceBounds : TRectD;
    currentColor    : TColor32;
    procedure GetBestFontForFontCache(const svgFontInfo: TSVGFontInfo);
    property  RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property  LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
    property  ImageRenderer  : TImageRenderer read fImgRenderer;
    property  TempImage      : TImage32 read fTempImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromFile(const filename: string): Boolean;
    function  LoadFromString(const str: string): Boolean;
    property  BackgroundColor : TColor32 read fBkgndColor write fBkgndColor;
    property  BlurQuality     : integer read fBlurQuality write SetBlurQuality;
    property  IsEmpty         : Boolean read GetIsEmpty;
    property  RootElement     : TSvgElement read fRootElement;
  end;

implementation

uses
  Image32_Extra, StrUtils;

type
  TDefsElement = class(TElement)
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  //-------------------------------------

  TShapeElement = class(TElement)
  protected
    hasPaths    : Boolean;
    drawPathsO  : TPathsD; //open only
    drawPathsC  : TPathsD; //closed only
    drawPathsF  : TPathsD; //both open and closed (for filling)
    function  GetBounds: TRectD; virtual;
    function  HasMarkers: Boolean;
    procedure GetPaths(const drawInfo: TDrawInfo); virtual;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; virtual;
    procedure DrawFilled(img: TImage32; drawInfo: TDrawInfo);
    procedure DrawStroke(img: TImage32; drawInfo: TDrawInfo; isClosed: Boolean);
    procedure DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TGroupElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  end;

  TSwitchElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  end;

  TUseElement = class(TShapeElement)
  protected
    refEl: AnsiString;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  end;

  TMaskElement = class(TShapeElement)
  protected
    maskRec: TRect;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    procedure ApplyMask(img: TImage32; const drawInfo: TDrawInfo);
  end;

  TSymbolElement = class(TShapeElement)
  protected
    viewboxWH: TRectWH;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  //-------------------------------------

  TPathElement = class(TShapeElement)
  private
    fSvgPaths : TSvgPaths;
    procedure Flatten(index: integer; scalePending: double;
      out path: TPathD; out isClosed: Boolean);
  protected
    function  GetBounds: TRectD; override;
    procedure ParseDAttrib(const value: AnsiString);
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path      : TPathD;
    function  GetBounds: TRectD; override;
    procedure ParsePoints(const value: AnsiString);
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path      : TPathD;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValue;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  //TTextElement: although this is a TShapeElement descendant, it's really
  //only a container for 'tspan' and 'subtext' elements. (See Draw method.)
  TTextElement = class(TShapeElement)
  protected
    offset    : TValuePt;
    startX    : double;
    currentPt : TPointD;
    function  GetTopTextElement: TTextElement;
    procedure DoOffsetX(dx: double);
    procedure ResetTmpPt;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  LoadContent: Boolean; override;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TTSpanElement = class(TTextElement)
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TSubtextElement = class(TShapeElement)
  protected
    text      : AnsiString;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  //-------------------------------------

  TTextPathElement = class(TSubtextElement)
  protected
    pathEl: AnsiString;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  end;

  TMarkerElement = class(TShapeElement)
  private
    fPoints     : TPathD;
  protected
    refPt       : TValuePt;
    angle       : double;
    angle2      : double;
    markerBoxWH : TRectWH;
    autoStartReverse  : Boolean;
    procedure SetEndPoint(const pt: TPointD; angle: double);
    function SetMiddlePoints(const points: TPathD): Boolean;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TSvgColorStop = record
    offset    : double;
    color     : TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TFillElement = class(TElement)
  protected
    refEl : AnsiString;
    units : Cardinal;
    function  GetRelFracLimit: double; override;
  end;

  TPatternElement = class(TFillElement)
  protected
    pattBoxWH : TRectWH;
    function PrepareRenderer(renderer: TImageRenderer;
      drawInfo: TDrawInfo): Boolean; virtual;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  //nb: gradients with objectBoundingBox should not be applied to
  //elements without width and height.
  TGradientElement = class(TFillElement)
  protected
    stops         : TSvgColorStops;
    spreadMethod  : TGradientFillStyle;
    function LoadContent: Boolean; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(other: TElement);  virtual;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; virtual;
  end;

  TRadGradElement = class(TGradientElement)
  protected
    radius: TValuePt;
    F, C: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TGradStopElement = class(TElement)
  protected
    offset: double;
    color: TColor32;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TFilterElement = class(TElement)
  private
    fSrcImg       : TImage32;
    fLastImg      : TImage32;
    fScale        : double;
    fFilterBounds : TRect;
    fObjectBounds : TRect;
    fImages       : array of TImage32;
    fNames        : array of AnsiString;
  protected
    procedure Clear;
    function GetRelFracLimit: double; override;
    function GetAdjustedBounds(const bounds: TRectD): TRectD;
    function FindNamedImage(const name: AnsiString): TImage32;
    function AddNamedImage(const name: AnsiString): TImage32;
    function GetNamedImage(const name: AnsiString): TImage32;
    procedure Apply(img: TImage32;
      const filterBounds: TRect; const matrix: TMatrixD);
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
    destructor Destroy; override;
  end;

  TFeBaseElement = class(TElement)
  private
    function GetParentAsFilterEl: TFilterElement;
  protected
    in1: AnsiString;
    in2: AnsiString;
    res: AnsiString;
    srcImg, dstImg: TImage32;
    srcRec, dstRec: TRect;
    function GetSrcAndDst: Boolean;
    function GetBounds(img: TImage32): TRect;
    procedure Apply; virtual; abstract;
    property ParentFilterEl: TFilterElement read GetParentAsFilterEl;
  end;

  TFeBlendElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TCompositeOp = (coOver, coIn, coOut, coAtop, coXOR, coArithmetic);

  TFeCompositeElement  = class(TFeBaseElement)
  protected
    ks: array [0..3] of double; //arithmetic constants
    compositeOp: TCompositeOp;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TFeColorMatrixElement  = class(TFeBaseElement)
  protected
    values: TArrayOfDouble;
    procedure Apply; override;
  end;

  TFeDefuseLightElement = class(TFeBaseElement)
  protected
    color         : TColor32;
    surfaceScale  : double;
    diffuseConst  : double;
    kernelSize    : integer;
    procedure Apply; override;
  end;

  TFeDropShadowElement = class(TFeBaseElement)
  protected
    stdDev      : double;
    offset        : TValuePt;
    floodColor  : TColor32;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TFeFloodElement  = class(TFeBaseElement)
  protected
    floodColor  : TColor32;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TFeGaussElement  = class(TFeBaseElement)
  protected
    stdDev: double;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;

  TFeMergeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeMergeNodeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeOffsetElement = class(TFeBaseElement)
  protected
    offset        : TValuePt;
    procedure Apply; override;
  end;

  TFePointLightElement = class(TFeBaseElement)
  protected
    z             : double;
  end;

  TFeSpecLightElement = class(TFeBaseElement)
  protected
    exponent      : double;
    color         : TColor32;
    procedure Apply; override;
  end;

  TClipPathElement = class(TShapeElement)
  protected
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; svgEl: TSvgTreeEl); override;
  end;
  //-------------------------------------


const
  buffSize    = 32;
  clAlphaSet  = $00010101;
  SourceImage   : AnsiString = 'SourceGraphic';
  SourceAlpha   : AnsiString = 'SourceAlpha';
  tmpFilterImg  : AnsiString = 'tmp';

  //https://www.w3.org/TR/css-fonts-3/#font-family-prop
  emptyDrawInfo: TDrawInfo =
    (currentColor: clInvalid; fillColor: clInvalid; fillRule: frNonZero;
    fillEl: ''; strokeColor: clInvalid;
    strokeWidth: (rawVal: InvalidD; unitType: utNumber);
    strokeCap: esButt; strokeJoin: jsMiter; strokeMitLim: 0.0;
    dashArray:nil; dashOffset:0; fontInfo: (family: ttfUnknown; size: 0;
    spacing: 0.0; textLength: 0; italic: sfsUndefined; weight: -1;
    align: staUndefined; decoration: fdUndefined;
    baseShift: (rawVal: InvalidD; unitType: utNumber));
    markerStart: ''; markerMiddle: ''; markerEnd: ''; filterEl: ''; maskEl: '';
    clipPathEl: ''; opacity: MaxInt; matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1));
    visible: true; InUse: false; bounds: (Left:0; Top:0; Right:0; Bottom:0));

var
  //defaultFontHeight: this size will be used to retrieve all glyph contours
  //(and later scaled as necessary). This relatively large default ensures
  //that contours will have adequate detail.
  defaultFontHeight: double = 20.0;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function HashToElementClass(hash: Cardinal): TElementClass;
begin
  case hash of
    hClippath       : Result := TClipPathElement;
    hCircle         : Result := TCircleElement;
    hDefs           : Result := TDefsElement;
    hEllipse        : Result := TEllipseElement;
    hFilter         : Result := TFilterElement;
    hfeBlend        : Result := TFeBlendElement;
    hfeColorMatrix  : Result := TFeColorMatrixElement;
    hfeComposite    : Result := TFeCompositeElement;
    hfeDefuseLighting : Result := TFeDefuseLightElement;
    hfeDropShadow   : Result := TFeDropShadowElement;
    hfeFlood        : Result := TFeFloodElement;
    hFeGaussianBlur : Result := TFeGaussElement;
    hfeMerge        : Result := TFeMergeElement;
    hfeMergeNode    : Result := TFeMergeNodeElement;
    hfeOffset       : Result := TFeOffsetElement;
    hfePointLight   : Result := TFePointLightElement;
    hfeSpecularLighting : Result := TFeSpecLightElement;
    hG              : Result := TGroupElement;
    hLine           : Result := TLineElement;
    hLineargradient : Result := TLinGradElement;
    hMarker         : Result := TMarkerElement;
    hMask           : Result := TMaskElement;
    hPath           : Result := TPathElement;
    hPattern        : Result := TPatternElement;
    hPolyline       : Result := TPolyElement;
    hPolygon        : Result := TPolyElement;
    hRadialgradient : Result := TRadGradElement;
    hRect           : Result := TRectElement;
    hStop           : Result := TGradStopElement;
    hSvg            : Result := TSvgElement;
    hSwitch         : Result := TSwitchElement;
    hSymbol         : Result := TSymbolElement;
    hText           : Result := TTextElement;
    hTextPath       : Result := TTextPathElement;
    hTSpan          : Result := TTSpanElement;
    hUse            : Result := TUseElement;
    else              Result := TElement; //use generic class
  end;
end;
//------------------------------------------------------------------------------

procedure UpdateDrawInfo(var drawInfo: TDrawInfo; thisElement: TElement);
begin
  with thisElement.fDrawInfo do
  begin
    if currentColor <> clInvalid then
      thisElement.fReader.currentColor := currentColor;
    drawInfo.fillRule := fillRule;
    if (fillColor = clCurrent) then
      drawInfo.fillColor := thisElement.fReader.currentColor
    else if (fillColor <> clInvalid) then
      drawInfo.fillColor := fillColor;
    if (fillEl <> '') then
      drawInfo.fillEl := fillEl;
    if (clipPathEl <> '') then
      drawInfo.clipPathEl := clipPathEl;
    if (strokeColor <> clInvalid) then
    begin
      if (strokeColor = clCurrent) then
        drawInfo.strokeColor := thisElement.fReader.currentColor else
        drawInfo.strokeColor := strokeColor;
      if not drawInfo.strokeWidth.IsValid then
        drawInfo.strokeWidth.SetValue(1);
    end;
    if strokeWidth.IsValid then
      drawInfo.strokeWidth := strokeWidth;
    if strokeMitLim > 0 then
      drawInfo.strokeMitLim := strokeMitLim;
    if Assigned(dashArray) then
      drawInfo.dashArray := Copy(dashArray, 0, Length(dashArray));
    if dashOffset <> 0 then
      drawInfo.dashOffset := dashOffset;
    if (strokeEl <> '') then
      drawInfo.strokeEl := strokeEl;
    if opacity < MaxInt then
      drawInfo.opacity := opacity;
    if (filterEl <> '') then
      drawInfo.filterEl := filterEl;
    if (maskEl <> '') then
      drawInfo.maskEl := maskEl;

    if fontInfo.family <> ttfUnknown then
      drawInfo.fontInfo.family := fontInfo.family;
    if fontInfo.size > 0 then
      drawInfo.fontInfo.size := fontInfo.size;
    if fontInfo.spacing <> 0 then
      drawInfo.fontInfo.spacing := fontInfo.spacing;
    if fontInfo.textLength > 0 then
      drawInfo.fontInfo.textLength := fontInfo.textLength;

    if (fontInfo.italic <> sfsUndefined) then
      drawInfo.fontInfo.italic := fontInfo.italic;
    if (fontInfo.weight <> -1) then
      drawInfo.fontInfo.weight := fontInfo.weight;

    if fontInfo.align <> staUndefined then
      drawInfo.fontInfo.align := fontInfo.align;

    if (thisElement is TTextElement) or
      (fontInfo.decoration <> fdUndefined) then
      drawInfo.fontInfo.decoration := fontInfo.decoration;
    if fontInfo.baseShift.IsValid then
      drawInfo.fontInfo.baseShift := fontInfo.baseShift;
    if not IsIdentityMatrix(matrix) then
      drawInfo.matrix := MatrixMultiply(drawInfo.matrix, matrix);
  end;
end;
//------------------------------------------------------------------------------

function IsFilled(const drawInfo: TDrawInfo): Boolean;
begin
  Result :=
    (drawInfo.fillColor = clInvalid) or (drawInfo.fillColor = clCurrent) or
    (drawInfo.fillEl <> '') or (TARGB(drawInfo.fillColor).A > 0);
end;
//------------------------------------------------------------------------------

function IsStroked(const drawInfo: TDrawInfo): Boolean;
begin
  with drawInfo do
    if (strokeColor = clInvalid) or
      (strokeColor = clCurrent) or
      (strokeEl <> '') then
        Result := (strokeWidth.rawVal > 0)
    else
        Result := (TARGB(strokeColor).A > 0) and
          ((strokeWidth.rawVal = InvalidD) or (strokeWidth.rawVal > 0));
end;
//------------------------------------------------------------------------------

function AnsiStringToFloat(const ansiValue: AnsiString;
  var value: double): Boolean;
var
  c: PAnsiChar;
begin
  c := PAnsiChar(ansiValue);
  Result := ParseNextNum(c, c + Length(ansiValue), false, value);
end;
//------------------------------------------------------------------------------

function AnsiStringToFloatEx(const ansiValue: AnsiString;
  var value: double; out measureUnit: TUnitType): Boolean;
var
  c: PAnsiChar;
begin
  c := PAnsiChar(ansiValue);
  Result := ParseNextNumEx(c, c + Length(ansiValue), false, value, measureUnit);
end;
//------------------------------------------------------------------------------

procedure AnsiStringToOpacity(const ansiValue: AnsiString; var color: TColor32);
var
  opacity: double;
begin
  if color = clNone32 then
  begin
    color := clAlphaSet;
    Exit;
  end;

  if color = clInvalid then color := clNone32;
  if not AnsiStringToFloat(ansiValue, opacity) then Exit;
  with TARGB(color) do
    if (opacity <= 0) then
    begin
      if Color = clNone32 then Color := clAlphaSet
      else A := 0;
    end
    else if (opacity >= 1) then A := 255
    else A := Round(255 * opacity);
end;
//------------------------------------------------------------------------------

function MatrixApply(const paths: TPathsD; const matrix: TMatrixD): TPathsD; overload;
var
  i,j,len,len2: integer;
  pp,rr: PPointD;
begin
  if not Assigned(paths) then
    Result := nil
  else if IsIdentityMatrix(matrix) then
    Result := CopyPaths(paths)
  else
  begin
    len := Length(paths);
    SetLength(Result, len);
    for i := 0 to len -1 do
    begin
      len2 := Length(paths[i]);
      SetLength(Result[i], len2);
      if len2 = 0 then Continue;
      pp := @paths[i][0];
      rr := @Result[i][0];
      for j := 0 to High(paths[i]) do
      begin
        rr.X := pp.X * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
        rr.Y := pp.X * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
        inc(pp); inc(rr);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDefsElement
//------------------------------------------------------------------------------

constructor TDefsElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.visible := false;
end;

//------------------------------------------------------------------------------
// TGroupElement
//------------------------------------------------------------------------------

procedure TGroupElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  clipEl    : TElement;
  maskEl    : TElement;
  tmpImg    : TImage32;
  clipPaths : TPathsD;
  clipRec   : TRect;
begin
  if fChilds.Count = 0 then Exit;

  UpdateDrawInfo(drawInfo, self);

  maskEl := FindRefElement(drawInfo.maskEl);
  clipEl := FindRefElement(drawInfo.clipPathEl);
  if Assigned(clipEl) then
  begin
    drawInfo.clipPathEl := ''; //avoids propagation
    with TClipPathElement(clipEl) do
    begin
      GetPaths(drawInfo);
      clipPaths := CopyPaths(drawPathsF);

      MatrixApply(drawInfo.matrix, clipPaths);
      clipRec := Image32_Vector.GetBounds(clipPaths);
    end;
    if IsEmptyRect(clipRec) then Exit;

    tmpImg := fReader.TempImage;
    tmpImg.Clear(clipRec);
    DrawChildren(tmpImg, drawInfo);

    with TClipPathElement(clipEl) do
      EraseOutsidePaths(tmpImg, clipPaths, fDrawInfo.fillRule, clipRec);
    image.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);
  end
  else if Assigned(maskEl) then
  begin
    drawInfo.maskEl := '';    //prevent propagation
    with TMaskElement(maskEl) do
    begin
      GetPaths(drawInfo);
      clipRec := maskRec;
    end;
    tmpImg := TImage32.Create(image.Width, image.Height);
    try
      DrawChildren(tmpImg, drawInfo);
      TMaskElement(maskEl).ApplyMask(tmpImg, DrawInfo);
      image.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);
    finally
      tmpImg.Free;
    end;
  end else
    DrawChildren(image, drawInfo);
end;

//------------------------------------------------------------------------------
// TSwitchElement
//------------------------------------------------------------------------------

procedure TSwitchElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
        if fDrawInfo.visible then
        begin
          Draw(image, drawInfo);
          break; //break after the first successful drawing
        end;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

procedure TUseElement.GetPaths(const drawInfo: TDrawInfo);
var
  el: TElement;
  dx, dy: double;
begin
  if Assigned(drawPathsF) or (refEl = '') then Exit;

  el := FindRefElement(refEl);
  if not Assigned(el) or not (el is TShapeElement) then Exit;
  with TShapeElement(el) do
  begin
    GetPaths(drawInfo);
    self.drawPathsC := CopyPaths(drawPathsC);
    self.drawPathsO := CopyPaths(drawPathsO);
  end;

  if elRectWH.left.IsValid then
    dx := elRectWH.left.rawVal else
    dx := 0;
  if elRectWH.top.IsValid  then
    dy := elRectWH.top.rawVal else
    dy := 0;

  if (dx <> 0) or (dy <> 0) then
  begin
    drawPathsC := OffsetPath(drawPathsC, dx, dy);
    drawPathsO := OffsetPath(drawPathsO, dx, dy);
  end;

  drawPathsF := CopyPaths(drawPathsC);
  AppendPath(drawPathsF, drawPathsO);
end;
//------------------------------------------------------------------------------

procedure TUseElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  el: TElement;
  s, dx, dy: double;
  scale, scale2: TSizeD;
  mat: TMatrixD;
begin
  el := FindRefElement(refEl);
  if not Assigned(el) or drawInfo.InUse then Exit;

  UpdateDrawInfo(drawInfo, el);
  UpdateDrawInfo(drawInfo, self); //nb: <use> attribs override el's.
  scale := ExtractScaleFromMatrix(drawInfo.matrix);

  if elRectWH.left.IsValid then dx := elRectWH.left.rawVal else dx := 0;
  if elRectWH.top.IsValid  then dy := elRectWH.top.rawVal  else dy := 0;

  mat := IdentityMatrix;
  MatrixTranslate(mat, dx, dy);
  drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);

  if el is TSymbolElement then
  begin
    with TSymbolElement(el) do
    begin
      if not viewboxWH.IsEmpty then
      begin
        //scale the symbol according to its width and height attributes
        if elRectWH.width.IsValid and elRectWH.height.IsValid then
        begin
          scale2.sx := elRectWH.width.rawVal / viewboxWH.Width;
          scale2.sy := elRectWH.height.rawVal / viewboxWH.Height;
          if scale2.sy < scale2.sx then s := scale2.sy else s := scale2.sx;
          //the following 3 lines will scale without translating
          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);
          drawInfo.bounds := RectD(0,0,viewboxWH.Width, viewboxWH.Height);
        end;

        if self.elRectWH.width.IsValid and
          self.elRectWH.height.IsValid then
        begin
          //scale <symbol> proportionally to fill the <use> element
          scale2.sx := self.elRectWH.width.rawVal / viewboxWH.Width;
          scale2.sy := self.elRectWH.height.rawVal / viewboxWH.Height;
          if scale2.sy < scale2.sx then s := scale2.sy else s := scale2.sx;

          //again, scale without translating
          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);

          //now center after scaling
          if scale2.sx > scale2.sy then
          begin
            if scale2.sx > 1 then
            begin
              s := (self.elRectWH.width.rawVal - viewboxWH.Width) * 0.5;
              MatrixTranslate(drawInfo.matrix, s * scale.sx, 0);
            end;
          end else if scale2.sy > 1 then
          begin
            s := (self.elRectWH.height.rawVal - viewboxWH.Height) * 0.5;
            MatrixTranslate(drawInfo.matrix, 0, s * scale.sy);
          end;

        end;
      end;
      DrawChildren(img, drawInfo);
    end;
  end
  else if el is TShapeElement then
  begin
    drawInfo.InUse := true;              //flag <use> precedence
    el.Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TMaskElement
//------------------------------------------------------------------------------

procedure TMaskElement.GetPaths(const drawInfo: TDrawInfo);
var
  i   : integer;
  di,di2 : TDrawInfo;
  el  : TShapeElement;
begin
  di := drawInfo;
  UpdateDrawInfo(di, self);

  maskRec := NullRect;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
    begin
      el := TShapeElement(fChilds[i]);
      di2 := di;
      UpdateDrawInfo(di2, el);
      el.GetPaths(di2);
      maskRec := Image32_Vector.UnionRect(maskRec,
        Image32_Vector.GetBounds(el.drawPathsF));
    end;
  MatrixApply(di.matrix, maskRec);
end;
//------------------------------------------------------------------------------

procedure TMaskElement.ApplyMask(img: TImage32; const drawInfo: TDrawInfo);
var
  tmpImg: TImage32;
begin
  tmpImg := TImage32.Create(img.Width, img.Height);
  try
    DrawChildren(tmpImg, drawInfo);
    img.CopyBlend(tmpImg, maskRec, maskRec, BlendBlueChannel);
  finally
    tmpImg.Free;
  end;
end;

//------------------------------------------------------------------------------
// TSymbolElement
//------------------------------------------------------------------------------

constructor TSymbolElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.visible := false;
end;

//------------------------------------------------------------------------------
// TGradElement
//------------------------------------------------------------------------------

function TGradientElement.LoadContent: Boolean;
var
  i: integer;
begin
  Result := inherited LoadContent;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TGradStopElement then
      with TGradStopElement(fChilds[i]) do
        AddStop(color, offset);
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddStop(color: TColor32; offset: double);
var
  len: integer;
begin
  //if a stop is less than previous stops, it is set equal to the largest stop.
  //If two stops are equal the last stop controls the color from that point.
  len := Length(stops);
  if (len > 0) and (stops[len-1].offset > offset) then
    offset := stops[len-1].offset;
  setLength(stops, len+1);
  stops[len].offset := Min(1,Max(0, offset));
  stops[len].color := color;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AssignTo(other: TElement);
var
  i, len: integer;
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;

  with TGradientElement(other) do
  begin
    if units = 0 then
      units := Self.units;

    if Length(stops) = 0 then
    begin
      len := Length(self.stops);
      SetLength(stops, len);
      for i := 0 to len -1 do
        stops[i] := Self.stops[i];
    end;

    if IsIdentityMatrix(fDrawInfo.matrix) then
      fDrawInfo.matrix := self.fDrawInfo.matrix;
  end;
end;
//------------------------------------------------------------------------------

function TGradientElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; drawInfo: TDrawInfo): Boolean;
var
  el: TElement;
begin
  if (refEl <> '') then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TGradientElement) then
      TGradientElement(el).AssignTo(self);
  end;
  Result := Length(stops) > 0;
end;

//------------------------------------------------------------------------------
// TRadGradElement
//------------------------------------------------------------------------------

constructor TRadGradElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  radius.Init;
  F.Init;
  C.Init;
end;
//------------------------------------------------------------------------------

procedure TRadGradElement.AssignTo(other: TElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TRadGradElement then
    with TRadGradElement(other) do
    begin
      if not radius.IsValid then radius := self.radius;
      if not C.IsValid then C := self.C;
      if not F.IsValid then F := self.F;
    end;
end;
//------------------------------------------------------------------------------

function TRadGradElement.PrepareRenderer(renderer: TCustomGradientRenderer;
  drawInfo: TDrawInfo): Boolean;
var
  i, hiStops: integer;
  cp, fp, r: TPointD;
  scale, scale2: TSizeD;
  rec2, rec3: TRectD;
begin
  inherited PrepareRenderer(renderer, drawInfo);
  hiStops := High(stops);
  Result := hiStops >= 0;
  if not Result then Exit;

  if units = hUserSpaceOnUse then
    rec2 := fReader.userSpaceBounds else
    rec2 := drawInfo.bounds;

  if radius.IsValid then
  begin
    if radius.X.HasFontUnits then
      r := radius.GetPoint(drawInfo.fontInfo.size, GetRelFracLimit) else
      r := radius.GetPoint(rec2, GetRelFracLimit);
  end else
  begin
    r.X := rec2.Width * 0.5;
    r.Y := rec2.Height * 0.5;
  end;
  scale := ExtractScaleFromMatrix(drawInfo.matrix);
  scale2 := ExtractScaleFromMatrix(fDrawInfo.matrix);
  r := ScalePoint(r, scale.sx * scale2.sx, scale.sy * scale2.sy);

  if C.IsValid then
  begin
    if C.X.HasFontUnits then
      cp := C.GetPoint(drawInfo.fontInfo.size, GetRelFracLimit) else
      cp := C.GetPoint(rec2, GetRelFracLimit);
    cp := OffsetPoint(cp, rec2.Left, rec2.Top);
  end else
    cp := rec2.MidPoint;
  MatrixApply(fDrawInfo.matrix, cp);
  MatrixApply(drawInfo.matrix, cp);

  rec3 := RectD(cp.X-r.X, cp.Y-r.Y, cp.X+r.X, cp.Y+r.Y);

  if F.IsValid then
  begin
    if F.X.HasFontUnits then
      fp := F.GetPoint(drawInfo.fontInfo.size, GetRelFracLimit) else
      fp := F.GetPoint(rec2, GetRelFracLimit);
    fp := OffsetPoint(fp, rec2.Left, rec2.Top);
    MatrixApply(fDrawInfo.matrix, fp);
    MatrixApply(drawInfo.matrix, fp);
  end else
    fp := MidPoint(rec3);

  with renderer as TSvgRadialGradientRenderer do
  begin
    SetParameters(Rect(rec3), Point(fp),
      stops[0].color, stops[hiStops].color, spreadMethod);
    for i := 1 to hiStops -1 do
      with stops[i] do
        renderer.InsertColorStop(offset, color);
  end;
end;

//------------------------------------------------------------------------------
// TLinGradElement
//------------------------------------------------------------------------------

constructor TLinGradElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  startPt.Init;
  endPt.Init;
end;
//------------------------------------------------------------------------------

procedure TLinGradElement.AssignTo(other: TElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TLinGradElement then
    with TLinGradElement(other) do
    begin
      if not startPt.IsValid then startPt := self.startPt;
      if not endPt.IsValid then endPt := self.endPt;
    end;
end;
//------------------------------------------------------------------------------

function TLinGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; drawInfo: TDrawInfo): Boolean;
var
  pt1, pt2: TPointD;
  i, hiStops: integer;
  rec2: TRectD;
begin
  inherited PrepareRenderer(renderer, drawInfo);
  hiStops := High(stops);
  Result := (hiStops >= 0);
  if not Result then Exit;

  //w3c-coords-units-01-b.svg

  //if gradientUnits=objectBoundingBox (default) then all values must be
  //percentages. Also... when the object's bounding box is not square, the
  //gradient may render non-perpendicular relative to the gradient vector
  //unless the gradient vector is vertical or horizontal.
  //https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits

  if units = hUserSpaceOnUse then
    rec2 := fReader.userSpaceBounds else
    rec2 := drawInfo.bounds;

  with TLinearGradientRenderer(renderer) do
  begin
    if startPt.X.HasFontUnits then
      pt1 := startPt.GetPoint(drawInfo.fontInfo.size, GetRelFracLimit) else
      pt1 := startPt.GetPoint(rec2, GetRelFracLimit);

    if (startPt.X.unitType <> utPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.X := pt1.X + rec2.Left;

    if (startPt.Y.unitType <> utPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.Y := pt1.Y + rec2.Top;

    MatrixApply(fDrawInfo.matrix, pt1);
    MatrixApply(drawInfo.matrix, pt1);


    if not endPt.X.IsValid then
      pt2.X := rec2.Width else
      pt2.X := endPt.X.GetValue(rec2.Width, GetRelFracLimit);
    pt2.Y := endPt.Y.GetValue(rec2.Height, GetRelFracLimit);
    pt2 := OffsetPoint(pt2, rec2.Left, rec2.Top);

    MatrixApply(fDrawInfo.matrix, pt2);
    MatrixApply(drawInfo.matrix, pt2);

    if (units <> hUserSpaceOnUse) and
      ((pt2.X <> pt1.X) or (pt2.Y <> pt1.Y)) then
    begin
      //skew the gradient
    end;

    SetParameters(pt1, pt2, stops[0].color,
      stops[hiStops].color, spreadMethod);
    for i := 1 to hiStops -1 do
      with stops[i] do
        renderer.InsertColorStop(offset, color);
  end;
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  color := clBlack32;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.visible := false;
  elRectWH.Init;
end;
//------------------------------------------------------------------------------

destructor TFilterElement.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Clear;
var
  i: integer;
begin
  for i := 0 to High(fImages) do
    fImages[i].Free;
  fImages := nil;
  fNames := nil;
  fLastImg := nil;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetRelFracLimit: double;
begin
  //always assume fractional values below 2.5 are relative
  Result := 2.5;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetAdjustedBounds(const bounds: TRectD): TRectD;
var
  recWH: TRectWH;
begin
  fObjectBounds := Rect(bounds);
  if elRectWH.IsValid then
  begin
    recWH := elRectWH.GetRectWH(bounds, GetRelFracLimit);
    Result.Left := bounds.Left + recWH.Left;
    Result.Top := bounds.Top + recWH.Top;
    Result.Right := Result.Left + recWH.Width;
    Result.Bottom := Result.Top + recWH.Height;
  end else
    //default: inflate by 15%
    Result := InflateRect(bounds, bounds.Width * 0.15, bounds.Height * 0.15);
end;
//------------------------------------------------------------------------------

function TFilterElement.FindNamedImage(const name: AnsiString): TImage32;
var
  i, len: integer;
begin
  Result := nil;
  len := Length(fNames);
  for i := 0 to len -1 do
    if name = fNames[i] then
    begin
      Result := fImages[i];
      Break;
    end;
end;
//------------------------------------------------------------------------------

function TFilterElement.AddNamedImage(const name: AnsiString): TImage32;
var
  len: integer;
begin
  len := Length(fNames);
  SetLength(fNames, len+1);
  SetLength(fImages, len+1);
  Result :=
    TImage32.Create(RectWidth(fFilterBounds), RectHeight(fFilterBounds));
  fImages[len] := Result;
  fNames[len] := name;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetNamedImage(const name: AnsiString): TImage32;
var
  i, len: integer;
  hash: Cardinal;
begin
  hash := GetHash(name);
  case hash of
    hSourceGraphic, hSourceAlpha:
      begin
        Result := FindNamedImage(name);
        if not Assigned(Result) then
        begin
          Result := AddNamedImage(name);
          Result.Copy(fSrcImg, fFilterBounds, Result.Bounds);
          if hash = hSourceAlpha then
            Result.SetRGB(clNone32, Result.Bounds);
        end;
        Exit;
      end;
  end;

  len := Length(fNames);
  for i := 0 to len -1 do
    if name = fNames[i] then
    begin
      Result := fImages[i];
      Exit;
    end;
  Result := AddNamedImage(name);
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Apply(img: TImage32;
  const filterBounds: TRect; const matrix: TMatrixD);
var
  i: integer;
begin
  fScale := ExtractAvgScaleFromMatrix(matrix);
  fFilterBounds := filterBounds;
  fObjectBounds := Image32_Vector.IntersectRect(img.Bounds, fObjectBounds);
  fSrcImg := img;
  try
    for i := 0 to fChilds.Count -1 do
    begin
      case TElement(fChilds[i]).fParserEl.hash of
        hfeBlend        : TFeBlendElement(fChilds[i]).Apply;
        hfeColorMatrix  : TFeColorMatrixElement(fChilds[i]).Apply;
        hfeComposite    : TFeCompositeElement(fChilds[i]).Apply;
        hfeDefuseLighting : TFeDefuseLightElement(fChilds[i]).Apply;
        hfeDropShadow   : TFeDropShadowElement(fChilds[i]).Apply;
        hfeFlood        : TFeFloodElement(fChilds[i]).Apply;
        hFeGaussianBlur : TFeGaussElement(fChilds[i]).Apply;
        hfeMerge        : TFeMergeElement(fChilds[i]).Apply;
        hfeOffset       : TFeOffsetElement(fChilds[i]).Apply;
        hfeSpecularLighting : TFeSpecLightElement(fChilds[i]).Apply;
      end;
    end;
    if fLastImg <> fSrcImg then
      fSrcImg.Copy(fLastImg, fLastImg.Bounds, fFilterBounds);
  finally
    Clear;
  end;
end;

//------------------------------------------------------------------------------
// TFeBaseElement
//------------------------------------------------------------------------------

function TFeBaseElement.GetParentAsFilterEl: TFilterElement;
var
  el: TElement;
begin
  el := fParent;
  while Assigned(el) and not (el is TFilterElement) do
    el := el.fParent;
  if not Assigned(el) then
    Result := nil else
    Result := TFilterElement(el);
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetBounds(img: TImage32): TRect;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if img = pfe.fSrcImg then
    Result := pfe.fFilterBounds else
    Result := img.Bounds;
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetSrcAndDst: Boolean;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if (in1 <> '') then
    srcImg := pfe.GetNamedImage(in1)
  else if Assigned(pfe.fLastImg) then
    srcImg := pfe.fLastImg
  else
    srcImg := pfe.GetNamedImage(SourceImage);

  if (res <> '') then
    dstImg := pfe.GetNamedImage(res) else
    dstImg := pfe.fSrcImg;

  Result := Assigned(srcImg) and Assigned(dstImg);
  if not Result then Exit;
  pfe.fLastImg := dstImg;
  srcRec := GetBounds(srcImg);
  dstRec := GetBounds(dstImg);
end;

//------------------------------------------------------------------------------
// TFeBlendElement
//------------------------------------------------------------------------------

procedure TFeBlendElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if (in2 = '') then Exit;
  if dstImg = srcImg then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2);

  srcImg2 := pfe.GetNamedImage(in2);
  srcRec2 := GetBounds(srcImg2);
  dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlpha);
  dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlpha);
  if dstImg = srcImg then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeCompositeElement
//------------------------------------------------------------------------------

constructor TFeCompositeElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  ks[0] := InvalidD; ks[1] := InvalidD; ks[2] := InvalidD; ks[3] := InvalidD;
end;
//------------------------------------------------------------------------------

procedure Arithmetic(p1, p2, r: PColor32; const ks: array of byte);
var
  c1  : PARGB absolute p1;
  c2  : PARGB absolute p2;
  res : PARGB absolute r;
begin
  res.A := ClampByte(MulBytes(ks[0], MulBytes(c1.A, c2.A)) +
    MulBytes(ks[1], c1.A) + MulBytes(ks[2], c2.A) + ks[3]);
  res.R := ClampByte(MulBytes(ks[0], MulBytes(c1.R, c2.R)) +
    MulBytes(ks[1], c1.R) + MulBytes(ks[2], c2.R) + ks[3]);
  res.G := ClampByte(MulBytes(ks[0], MulBytes(c1.G, c2.G)) +
    MulBytes(ks[1], c1.G) + MulBytes(ks[2], c2.G) + ks[3]);
  res.B := ClampByte(MulBytes(ks[0], MulBytes(c1.B, c2.B)) +
    MulBytes(ks[1], c1.B) + MulBytes(ks[2], c2.B) + ks[3]);
end;
//------------------------------------------------------------------------------

procedure ArithmeticBlend(src1, src2, dst: TImage32;
  const recS1, recS2, recDst: TRect; const ks: array of double);
var
  kk: array[0..3] of byte;
  w,h,i,j: integer;
  p1,p2,r: PColor32;
begin
  w := RectWidth(recS1);
  h := RectHeight(recS1);
  if (RectWidth(recS2) <> w) or (RectWidth(recDst) <> w) or
    (RectHeight(recS2) <> h) or (RectHeight(recDst) <> h) or
    (ks[0] = InvalidD) or (ks[1] = InvalidD) or
    (ks[2] = InvalidD) or (ks[3] = InvalidD) then Exit;

  for i := 0 to 3 do
    kk[i] := ClampByte(ks[i]*255);

  for i := 0 to h -1 do
  begin
    p1 := @src1.Pixels[(recS1.Top + i) * src1.Width + recS1.Left];
    p2 := @src2.Pixels[(recS2.Top + i) * src2.Width + recS2.Left];
    r  := @dst.Pixels[(recDst.Top + i) * dst.Width + recDst.Left];
    for j := 0 to w -1 do
    begin
      Arithmetic(p1, p2, r, kk);
      inc(p1); inc(p2); inc(r);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TFeCompositeElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if (in2 = '') then Exit;
  if dstImg = srcImg then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2);

  srcImg2 := pfe.GetNamedImage(in2);
  srcRec2 := GetBounds(srcImg2);

  case compositeOp of
    coIn:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendMask);
      end;
    coOut:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendInvertedMask);
      end;
    coArithmetic:
      begin
        ArithmeticBlend(srcImg, srcImg2, dstImg2,
          srcRec, srcRec2, dstRec2, ks);
      end;
    else     //coOver
      begin
        dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlpha);
        dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlpha);
      end;
  end;
  if dstImg = srcImg then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeColorMatrixElement
//------------------------------------------------------------------------------

type
  TColorMatrix = array[0..19] of Byte;

function ApplyColorMatrix(color: TColor32; const mat: TColorMatrix): TColor32;
var
  clrIn : TARGB absolute color;
  clrOut: TARGB absolute Result;
begin
  clrOut.R := ClampByte(MulBytes(mat[0],clrIn.R) + MulBytes(mat[1],clrIn.G) +
    MulBytes(mat[2],clrIn.B) + MulBytes(mat[3],clrIn.A) + mat[4]);
  clrOut.G := ClampByte(MulBytes(mat[5],clrIn.R) + MulBytes(mat[6],clrIn.G) +
    MulBytes(mat[7],clrIn.B) + MulBytes(mat[8],clrIn.A) + mat[9]);
  clrOut.B := ClampByte(MulBytes(mat[10],clrIn.R) + MulBytes(mat[11],clrIn.G) +
    MulBytes(mat[12],clrIn.B) + MulBytes(mat[13],clrIn.A) + mat[14]);
  clrOut.A := ClampByte(MulBytes(mat[15],clrIn.R) + MulBytes(mat[16],clrIn.G) +
    MulBytes(mat[17],clrIn.B) + MulBytes(mat[18],clrIn.A) + mat[19]);
end;
//------------------------------------------------------------------------------

procedure TFeColorMatrixElement.Apply;
var
  i,j, dx1,dx2: integer;
  colorMatrix: TColorMatrix;
  p1, p2: PColor32;
begin
  if not GetSrcAndDst or not Assigned(values) then Exit;
  for i := 0 to 19 do
    colorMatrix[i] := ClampByte(Round(values[i]*255));

  dx1 := srcImg.Width - RectWidth(srcRec);
  dx2 := dstImg.Width - RectWidth(dstRec);
  p1 := @srcImg.Pixels[srcRec.Top * srcImg.Width + srcRec.Left];
  p2 := @dstImg.Pixels[dstRec.Top * dstImg.Width + dstRec.Left];
  for i := srcRec.Top to srcRec.Bottom -1 do
  begin
    for j := srcRec.Left to srcRec.Right -1 do
    begin
      p2^ := ApplyColorMatrix(p1^, colorMatrix);
      inc(p1); inc(p2);
    end;
    inc(p1, dx1); inc(p2, dx2);
  end;
end;

//------------------------------------------------------------------------------
// TFeDefuseLightElement
//------------------------------------------------------------------------------

procedure TFeDefuseLightElement.Apply;
begin
  //not implemented
  if not GetSrcAndDst then Exit;
  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);
end;

//------------------------------------------------------------------------------
// TFeDropShadowElement
//------------------------------------------------------------------------------

constructor TFeDropShadowElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  stdDev := InvalidD;
  floodColor := clInvalid;
  offset.X.SetValue(0);
  offset.Y.SetValue(0);
end;
//------------------------------------------------------------------------------

procedure TFeDropShadowElement.Apply;
var
  alpha: Byte;
  off: TPointD;
  dstOffRec: TRect;
  pfe: TFilterElement;
  dropShadImg: TImage32;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  dropShadImg := pfe.GetNamedImage(tmpFilterImg);
  dropShadImg.Copy(srcImg, srcRec, dropShadImg.Bounds);

  off := offset.GetPoint(RectD(pfe.fObjectBounds), GetRelFracLimit);
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do Image32_Vector.OffsetRect(dstOffRec, X, Y);
  dstImg.Copy(srcImg, srcRec, dstOffRec);
  dstImg.SetRGB(floodColor);
  alpha := floodColor shr 24;
  if (alpha > 0) and (alpha < 255) then
    dstImg.ReduceOpacity(alpha);
  if stdDev > 0 then
    FastGaussianBlur(dstImg, dstRec,
      Ceil(stdDev *0.75 * ParentFilterEl.fScale) , 0);
  dstImg.CopyBlend(dropShadImg, dropShadImg.Bounds, dstRec, BlendToAlpha);
end;

//------------------------------------------------------------------------------
// TFeFloodElement
//------------------------------------------------------------------------------

constructor TFeFloodElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  floodColor := clInvalid;
end;
//------------------------------------------------------------------------------

procedure TFeFloodElement.Apply;
var
  rec: TRect;
begin
  if not GetSrcAndDst then Exit;
  if elRectWH.IsValid then
    rec := Rect(elRectWH.GetRectD(RectD(srcRec), GetRelFracLimit)) else
    rec := srcRec;
  dstImg.FillRect(rec, floodColor);
end;

//------------------------------------------------------------------------------
// TFeGaussElement
//------------------------------------------------------------------------------

constructor TFeGaussElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  stdDev := InvalidD;
end;
//------------------------------------------------------------------------------

procedure TFeGaussElement.Apply;
begin
  if not GetSrcAndDst then Exit;

  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);

  ////True GaussianBlur is visually optimal, but it's also *extremely* slow.
  //GaussianBlur(dstImg, dstRec, Ceil(stdDev *PI * ParentFilterEl.fScale));

  //FastGaussianBlur is a very good approximation and also very much faster.
  //Empirically stdDev * PI/4 more closely emulates other renderers.
  FastGaussianBlur(dstImg, dstRec,
    Ceil(stdDev * PI/4 * ParentFilterEl.fScale), fReader.fBlurQuality);
end;

//------------------------------------------------------------------------------
// TFeMergeElement
//------------------------------------------------------------------------------

procedure TFeMergeElement.Apply;
var
  i: integer;
  tmpImg: TImage32;
  tmpRec: TRect;
  pfe: TFilterElement;
begin
  tmpImg := nil; tmpRec := NullRect;
  if not GetSrcAndDst then Exit;

  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TFeMergeNodeElement then
      with TFeMergeNodeElement(fChilds[i]) do
      begin
        if not GetSrcAndDst then Continue;
        if Assigned(tmpImg) then
        begin
          tmpImg.CopyBlend(srcImg, srcRec, tmpRec, BlendToAlpha);
        end else
        begin
          tmpImg := srcImg;
          tmpRec := srcRec;
        end;
      end;

  dstImg.Copy(tmpImg, tmpRec, dstRec);
  pfe := ParentFilterEl;
  pfe.fLastImg := dstImg;
end;

//------------------------------------------------------------------------------
// TFeMergeNodeElement
//------------------------------------------------------------------------------

procedure TFeMergeNodeElement.Apply;
begin
  //should never get here ;)
end;

//------------------------------------------------------------------------------
// TFeOffsetElement
//------------------------------------------------------------------------------

procedure TFeOffsetElement.Apply;
var
  off: TPointD;
  dstOffRec: TRect;
  tmpImg: TImage32;
  pfe: TFilterElement;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  off := offset.GetPoint(RectD(pfe.fObjectBounds), GetRelFracLimit);
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do Image32_Vector.OffsetRect(dstOffRec, X, Y);

  if srcImg = dstImg then
  begin
    tmpImg := pfe.GetNamedImage(tmpFilterImg);
    tmpImg.Copy(srcImg, srcRec, tmpImg.Bounds);
    dstImg.Clear(dstRec);
    dstImg.Copy(tmpImg, tmpImg.Bounds, dstOffRec);
  end else
  begin
    dstImg.Clear(dstRec);
    dstImg.Copy(srcImg, srcRec, dstOffRec);
  end;
end;

//------------------------------------------------------------------------------
// TFeSpecLightElement
//------------------------------------------------------------------------------

procedure TFeSpecLightElement.Apply;
begin
  //not implemented
  if not GetSrcAndDst then Exit;
  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  i: integer;
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then Exit;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetPaths(drawInfo);
        AppendPath(self.drawPathsO, drawPathsO);
        AppendPath(self.drawPathsC, drawPathsC);
      end;
  drawPathsF := CopyPaths(drawPathsC);
  AppendPath(drawPathsF, drawPathsO);
end;

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  elRectWH.Init;
  hasPaths := true;
  fDrawInfo.visible := true;
  if fParserEl.name.text = nil then Exit;
end;
//------------------------------------------------------------------------------

function  TShapeElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := NullRectD;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
       Result := UnionRect(Result, TShapeElement(fChilds[i]).GetBounds);
end;
//------------------------------------------------------------------------------

function TShapeElement.HasMarkers: Boolean;
begin
  Result := IsStroked(fDrawInfo) and ((fDrawInfo.markerStart <> '') or
    (fDrawInfo.markerMiddle <> '') or (fDrawInfo.markerEnd <> ''));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  d           : double;
  img         : TImage32;
  stroked     : Boolean;
  filled      : Boolean;
  clipRec     : TRectD;
  clipRec2    : TRect;
  clipPathEl  : TElement;
  filterEl    : TElement;
  maskEl      : TElement;
  clipPaths   : TPathsD;
  di          : TDrawInfo;
  usingTempImage: Boolean;
begin
  UpdateDrawInfo(drawInfo, self);

  filled := IsFilled(drawInfo);
  stroked := IsStroked(drawInfo);
  GetPaths(drawInfo);

  if not (filled or stroked) or not hasPaths then Exit;
  drawInfo.bounds := GetBoundsD(drawPathsF);

  img := image;
  clipRec2 := NullRect;
  clipPathEl := nil; filterEl := nil; maskEl := nil;
  if (drawInfo.clipPathEl <> '') then
    clipPathEl := FindRefElement(drawInfo.clipPathEl);
  if (drawInfo.filterEl <> '') then
    filterEl := FindRefElement(drawInfo.filterEl);
  if (drawInfo.maskEl <> '') then
    maskEl := FindRefElement(drawInfo.maskEl);
  if (drawInfo.fillEl <> '') then
    with TARGB(drawInfo.fillColor) do
      if (A > 0) and (A < 255) then DrawInfo.opacity := A;

  usingTempImage := Assigned(clipPathEl) or
    Assigned(filterEl) or Assigned(maskEl) or (DrawInfo.opacity < 255);

  if usingTempImage then
  begin
    img := fReader.TempImage;

    //get special effects bounds
    if Assigned(clipPathEl) then
    begin
      di := drawInfo;
      with TClipPathElement(clipPathEl) do
      begin
        GetPaths(di);
        clipPaths := MatrixApply(drawPathsF, di.matrix);
        clipRec := GetBoundsD(clipPaths);
      end;
    end
    else if Assigned(maskEl) then
    begin
      drawInfo.maskEl := '';
      with TMaskElement(maskEl) do
      begin
        GetPaths(drawInfo);
        clipRec := RectD(maskRec);
      end;
    end else
    begin
      clipRec := GetBoundsD(drawPathsF);
      if stroked and drawInfo.strokeWidth.IsValid then
      begin
        with drawInfo.strokeWidth do
          if HasFontUnits then
            d := GetValue(drawInfo.fontInfo.size, GetRelFracLimit) else
            d := GetValueXY(clipRec, GetRelFracLimit);
        clipRec := InflateRect(clipRec, d * 0.5, d * 0.5);
      end;
      if Assigned(filterEl) then
        with TFilterElement(filterEl) do
          clipRec := GetAdjustedBounds(clipRec);
      MatrixApply(drawInfo.matrix, clipRec);
    end;
    clipRec2 := Rect(clipRec);
    clipRec2 := Image32_Vector.IntersectRect(clipRec2, img.Bounds);
    if IsEmptyRect(clipRec2) then Exit;
    if image <> fReader.TempImage then
      img.Clear(clipRec2);
  end;

  if not IsValidMatrix(drawInfo.matrix) then
    raise Exception.Create('Invalid matrix found when drawing element');

  if filled then
    DrawFilled(img, drawInfo);

  if stroked then
  begin
    if Assigned(drawPathsC) then
      DrawStroke(img, drawInfo, true);
    if stroked and Assigned(drawPathsO) then
      DrawStroke(img, drawInfo, false);
  end;

  if Assigned(filterEl) then
    with TFilterElement(filterEl) do
      Apply(img, clipRec2, drawInfo.matrix);

  if DrawInfo.opacity < 255 then
    img.ReduceOpacity(DrawInfo.opacity, clipRec2);

  if Assigned(maskEl) then
    TMaskElement(maskEl).ApplyMask(img, DrawInfo)
  else if Assigned(clipPathEl) then
    with TClipPathElement(clipPathEl) do
      EraseOutsidePaths(img, clipPaths, fDrawInfo.fillRule, clipRec2);

  if usingTempImage and (img <> image) then
    image.CopyBlend(img, clipRec2, clipRec2, BlendToAlpha);

  //todo: enable "paint-order" to change filled/stroked/marker paint order
  if HasMarkers then DrawMarkers(img, drawInfo);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  sw: double;
  markerEl: TElement;
  markerPaths: TPathsD;
  pt1, pt2: TPointD;
  di: TDrawInfo;
begin
  markerPaths := GetUncurvedPath(drawInfo);
  markerPaths := StripNearDuplicates(markerPaths, 0.01, false);

  if not Assigned(markerPaths) then Exit;
  MatrixApply(drawInfo.matrix, markerPaths);

  di := emptyDrawInfo;

  //prepare to scale the markers by the stroke width
  with fDrawInfo.strokeWidth do
    if not IsValid then sw := 1
    else if HasFontUnits then
      sw := GetValue(drawInfo.fontInfo.size, GetRelFracLimit)
    else sw := GetValueXY(drawInfo.bounds, GetRelFracLimit);

  MatrixScale(di.matrix, sw * ExtractAvgScaleFromMatrix(drawInfo.matrix));

  if (fDrawInfo.markerStart <> '') then
  begin
    markerEl := FindRefElement(fDrawInfo.markerStart);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          if Length(markerPaths[i]) < 2 then Continue;
          pt1 := markerPaths[i][0];
          pt2 := markerPaths[i][1];
          if autoStartReverse then
            SetEndPoint(pt1, GetAngle(pt2, pt1)) else
            SetEndPoint(pt1, GetAngle(pt1, pt2));
          Draw(img, di);
        end;
      end;
  end;

  if (fDrawInfo.markerMiddle <> '') then
  begin
    markerEl := FindRefElement(fDrawInfo.markerMiddle);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
        for i := 0 to High(markerPaths) do
          if SetMiddlePoints(markerPaths[i]) then
            Draw(img, di);
  end;

  if (fDrawInfo.markerEnd <> '') then
  begin
    markerEl := FindRefElement(fDrawInfo.markerEnd);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          j := High(markerPaths[i]);
          if j < 1 then Continue;
          pt1 := markerPaths[i][j];
          pt2 := markerPaths[i][j-1];
          SetEndPoint(pt1, GetAngle(pt2, pt1));
          Draw(img, di);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.GetPaths(const drawInfo: TDrawInfo);
begin
  drawPathsO := nil; drawPathsC := nil; drawPathsF := nil;
end;
//------------------------------------------------------------------------------

function  TShapeElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32; drawInfo: TDrawInfo);
var
  refEl: TElement;
  fillPaths: TPathsD;
begin
  if not assigned(drawPathsF) then Exit;

  fillPaths := MatrixApply(drawPathsF, drawInfo.matrix);
  if (drawInfo.fillEl <> '') then
  begin
    refEl := FindRefElement(drawInfo.fillEl);
    if Assigned(refEl) and (refEl is TFillElement) then
    begin
      if refEl is TRadGradElement then
      begin
        with TRadGradElement(refEl), fReader do
          if PrepareRenderer(RadGradRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, RadGradRenderer);
      end
      else if refEl is TLinGradElement then
      begin
        with TLinGradElement(refEl), fReader do
          if PrepareRenderer(LinGradRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, LinGradRenderer);
      end
      else if refEl is TPatternElement then
      begin
        with TPatternElement(refEl), fReader do
          if PrepareRenderer(ImageRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, ImageRenderer);
      end;
    end;
  end
  else if drawInfo.fillColor = clInvalid then
    DrawPolygon(img, fillPaths, drawInfo.fillRule, clBlack32)
  else
    DrawPolygon(img, fillPaths, drawInfo.fillRule, drawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawStroke(img: TImage32;
  drawInfo: TDrawInfo; isClosed: Boolean);
var
  dashOffset, scaledStrokeWidth, roundingScale: double;
  dashArray: TArrayOfInteger;
  scale: Double;
  strokePaths: TPathsD;
  refEl: TElement;
  endStyle: TEndStyle;
  joinStyle: TJoinStyle;
  bounds: TRectD;
begin
  if isClosed then
  begin
    strokePaths := MatrixApply(drawPathsC, drawInfo.matrix);
    endStyle := esPolygon;
  end else
  begin
    strokePaths := MatrixApply(drawPathsO, drawInfo.matrix);
    endStyle := fDrawInfo.strokeCap;
  end;
  if not Assigned(strokePaths) then Exit;
  joinStyle := fDrawInfo.strokeJoin;

  scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  bounds := fReader.userSpaceBounds;
  with drawInfo.strokeWidth do
    if HasFontUnits then
      scaledStrokeWidth :=
        GetValue(drawInfo.fontInfo.size, GetRelFracLimit) * scale
    else
      scaledStrokeWidth := GetValueXY(bounds, 0) * scale;
  roundingScale := scale;

  if Length(drawInfo.dashArray) > 0 then
    dashArray := MakeDashArray(drawInfo.dashArray, scale) else
    dashArray := nil;

  if Assigned(dashArray) then
  begin
    dashOffset := drawInfo.dashOffset * scale;
    DrawDashedLine(img, strokePaths, dashArray, @dashOffset,
      scaledStrokeWidth, drawInfo.strokeColor, endStyle)
  end
  else if (drawInfo.strokeEl <> '') then
  begin
    refEl := FindRefElement(drawInfo.strokeEl);
    if not Assigned(refEl) then Exit;

    if refEl is TRadGradElement then
    begin
      with TRadGradElement(refEl) do
        PrepareRenderer(fReader.RadGradRenderer, drawInfo);
      DrawLine(img, strokePaths, scaledStrokeWidth,
        fReader.RadGradRenderer, endStyle, joinStyle, roundingScale);
    end
    else if refEl is TLinGradElement then
    begin
      with TLinGradElement(refEl) do
        PrepareRenderer(fReader.LinGradRenderer, drawInfo);
      DrawLine(img, strokePaths, scaledStrokeWidth,
        fReader.LinGradRenderer, endStyle, joinStyle, roundingScale);
    end
    else if refEl is TPatternElement then
    begin
      with TPatternElement(refEl) do
        PrepareRenderer(fReader.ImageRenderer, drawInfo);
      DrawLine(img, strokePaths,  scaledStrokeWidth,
        fReader.ImageRenderer, endStyle, joinStyle, roundingScale);
    end;
  end
  else if (joinStyle = jsMiter) then
    DrawLine(img, strokePaths, scaledStrokeWidth,
      drawInfo.strokeColor, endStyle, joinStyle, drawInfo.strokeMitLim)
  else
    DrawLine(img, strokePaths,scaledStrokeWidth,
      drawInfo.strokeColor, endStyle, joinStyle, roundingScale);
end;


//------------------------------------------------------------------------------
// TPathElement
//------------------------------------------------------------------------------

function TPathElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := NullRectD;
  for i := 0 to High(fSvgPaths) do
    Result := UnionRect(Result, fSvgPaths[i].GetBounds);
end;
//------------------------------------------------------------------------------

procedure TPathElement.ParseDAttrib(const value: AnsiString);
begin
  fSvgPaths := Image32_SVG_Core.ParseSvgPath(value);
end;
//------------------------------------------------------------------------------

procedure TPathElement.Flatten(index: integer; scalePending: double;
  out path: TPathD; out isClosed: Boolean);
begin
  isClosed := fSvgPaths[index].isClosed;
  path := fSvgPaths[index].GetFlattenedPath(scalePending);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  i: integer;
  scalePending: double;
  isClosed: Boolean;
  path: TPathD;
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then inherited;
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  for i := 0 to High(fSvgPaths) do
  begin
    Flatten(i, scalePending, path, isClosed);
    if not Assigned(path) then Continue;

    if isClosed then
      AppendPath(drawPathsC, path) else
      AppendPath(drawPathsO, path);
  end;
  AppendPath(drawPathsF, drawPathsO);
  AppendPath(drawPathsF, drawPathsC);
end;
//------------------------------------------------------------------------------

function TPathElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
var
  i, len: integer;
begin
  len := Length(fSvgPaths);
  SetLength(Result, len);
  for i := 0 to High(fSvgPaths) do
    Result[i] := fSvgPaths[i].GetSimplePath;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

function TPolyElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.GetPaths(const drawInfo: TDrawInfo);
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then Exit;
  if not Assigned(path) then Exit;
  if (fParserEl.hash = hPolygon) then
  begin
    AppendPath(drawPathsC, path);                    //hPolygon
    drawPathsF := drawPathsC;
  end else
  begin
    AppendPath(drawPathsO, path);                   //hPolyline
    drawPathsF := drawPathsO;
  end;
end;
//------------------------------------------------------------------------------

function TPolyElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.ParsePoints(const value: AnsiString);
var
  currCnt, currCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + buffSize;
      SetLength(path, currCap);
    end;
    path[currCnt] := pt;
    inc(currCnt);
  end;

var
  pt: TPointD;
  c, endC: PAnsiChar;
begin
  currCnt     := 0;
  currCap     := buffSize;
  c := PAnsiChar(value);
  endC := c + Length(value);
  SetLength(path, currCap);
  while IsNumPending(c, endC, true) and
    ParseNextNum(c, endC, true, pt.X) and
    ParseNextNum(c, endC, true, pt.Y) do
      AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

constructor TLineElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  SetLength(path, 2);
  path[0] := NullPointD; path[1] := NullPointD;
end;
//------------------------------------------------------------------------------

function TLineElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TLineElement.GetPaths(const drawInfo: TDrawInfo);
begin
  if Assigned(drawPathsO) then Exit;
  AppendPath(drawPathsO, path);
  drawPathsF := drawPathsO;
end;
//------------------------------------------------------------------------------

function TLineElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

constructor TCircleElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  centerPt.Init;
  radius.Init;
end;
//------------------------------------------------------------------------------

function TCircleElement.GetBounds: TRectD;
var
  cp  : TPointD;
  r   : double;
begin
  Result := NullRectD;
  if not radius.IsValid then Exit;
  r := radius.GetValue(1, GetRelFracLimit);
  cp := centerPt.GetPoint(NullRectD, GetRelFracLimit);
  Result := RectD(cp.X -r, cp.Y -r, cp.X +r, cp.Y +r);
end;
//------------------------------------------------------------------------------

procedure TCircleElement.GetPaths(const drawInfo: TDrawInfo);
var
  scalePending : double;
  rec   : TRectD;
  pt    : TPointD;
  path  : TPathD;
  r: double;
begin
  if Assigned(drawPathsC) then inherited;
  if not radius.IsValid then Exit;
  r := radius.GetValueXY(drawInfo.bounds, GetRelFracLimit);
  pt := centerPt.GetPoint(drawInfo.bounds, GetRelFracLimit);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  rec := RectD(pt.X -r, pt.Y -r, pt.X +r, pt.Y +r);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

constructor TEllipseElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  centerPt.Init;
  radius.Init;
end;
//------------------------------------------------------------------------------

function TEllipseElement.GetBounds: TRectD;
var
  cp  : TPointD;
  r   : TPointD;
begin
  Result := NullRectD;
  if not radius.IsValid then Exit;
  r := radius.GetPoint(NullRectD, GetRelFracLimit);
  cp := centerPt.GetPoint(NullRectD, GetRelFracLimit);
  Result := RectD(cp.X -r.X, cp.Y -r.Y, cp.X +r.X, cp.Y +r.X);
end;
//------------------------------------------------------------------------------

procedure TEllipseElement.GetPaths(const drawInfo: TDrawInfo);
var
  scalePending  : double;
  rec       : TRectD;
  path      : TPathD;
  rad       : TPointD;
  centPt    : TPointD;
begin
  if Assigned(drawPathsC) then inherited;
  rad := radius.GetPoint(drawInfo.bounds, GetRelFracLimit);
  centPt := centerPt.GetPoint(drawInfo.bounds, GetRelFracLimit);
  with centPt do
    rec := RectD(X -rad.X, Y -rad.Y, X +rad.X, Y +rad.Y);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

constructor TRectElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  radius.Init;
  elRectWH.width.SetValue(100, utPercent);
  elRectWH.height.SetValue(100, utPercent);
end;
//------------------------------------------------------------------------------

function  TRectElement.GetBounds: TRectD;
begin
  Result := elRectWH.GetRectD(NullRectD, GetRelFracLimit);
end;
//------------------------------------------------------------------------------

procedure TRectElement.GetPaths(const drawInfo: TDrawInfo);
var
  radXY : TPointD;
  bounds: TRectD;
  path  : TPathD;
begin
  if Assigned(drawPathsC) then Exit;
  if elRectWH.width.HasFontUnits then
    bounds := elRectWH.GetRectD(drawInfo.fontInfo.size, GetRelFracLimit) else
    bounds := elRectWH.GetRectD(drawInfo.bounds, GetRelFracLimit);
  if bounds.IsEmpty then Exit;

  radXY := radius.GetPoint(bounds, GetRelFracLimit);
  if (radXY.X > 0) or (radXY.Y > 0) then
  begin
    if (radXY.X <= 0) then radXY.X := radXY.Y
    else if (radXY.Y <= 0) then radXY.Y := radXY.X;
    path := RoundRect(bounds, radXY);
  end else
    path := Rectangle(bounds);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;
//------------------------------------------------------------------------------

function TRectElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
var
  rec: TRectD;
begin
  Result := nil;
  rec := elRectWH.GetRectD(drawInfo.bounds, GetRelFracLimit);
  if not rec.IsEmpty then
    AppendPath(Result, Rectangle(rec));
end;

//------------------------------------------------------------------------------
// TTextElement
//------------------------------------------------------------------------------

constructor TTextElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  offset.Init;
  hasPaths := false;
end;
//------------------------------------------------------------------------------

function  TTextElement.LoadContent: Boolean;
var
  i       : integer;
  svgEl   : TSvgTreeEl;
  elClass : TElementClass;
  el      : TElement;
begin
  Result := false;
  for i := 0 to fParserEl.childs.Count -1 do
  begin
    svgEl := TSvgTreeEl(fParserEl.childs[i]);
    if svgEl.hash = 0 then
    begin
      el := TSubtextElement.Create(self, svgEl);
      Self.fChilds.Add(el);
      if assigned(svgEl.text.text) then
        TSubtextElement(el).text := svgEl.text.AsAnsiString;
    end else
    begin
      elClass := HashToElementClass(svgEl.hash);
      el := elClass.Create(self, svgEl);
      Self.fChilds.Add(el);
      el.LoadAttributes;
      if not el.LoadContent then Exit; //error
    end;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function TTextElement.GetTopTextElement: TTextElement;
var
  el: TElement;
begin
  el := self;
  while Assigned(el.fParent) and (el.fParent is TTextElement) do
    el := el.fParent;
  Result := TTextElement(el);
end;
//------------------------------------------------------------------------------

procedure TTextElement.DoOffsetX(dx: double);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TTextElement then
      TTextElement(fChilds[i]).DoOffsetX(dx)
    else if TElement(fChilds[i]) is TSubTextElement then
      with TSubTextElement(fChilds[i]) do
      begin
        drawPathsC := OffsetPath(drawPathsC, dx, 0);
        drawPathsO := OffsetPath(drawPathsO, dx, 0);
        drawPathsF := OffsetPath(drawPathsF, dx, 0);
      end;
end;
//------------------------------------------------------------------------------

procedure TTextElement.GetPaths(const drawInfo: TDrawInfo);
var
  i         : integer;
  el        : TElement;
  di        : TDrawInfo;
  topTextEl : TTextElement;
begin
  di := drawInfo;
  if self <> GetTopTextElement then
    UpdateDrawInfo(di, self);

  if Self is TTSpanElement then
  begin
    el := fParent;
    while (el is TTSpanElement) do
      el := el.fParent;
    if not (el is TTextElement) then Exit; //ie error (eg <textarea>)
    topTextEl := TTextElement(el);

    if elRectWH.left.IsValid then
      currentPt.X := elRectWH.left.rawVal else
      currentPt.X := topTextEl.currentPt.X;
    if elRectWH.top.IsValid then
      currentPt.Y := elRectWH.top.rawVal else
      currentPt.Y := topTextEl.currentPt.Y;

    if offset.X.IsValid then
      currentPt.X := currentPt.X + offset.X.GetValue(0, 0);
    if offset.Y.IsValid then
      currentPt.Y := currentPt.Y + offset.Y.GetValue(0, 0);

    topTextEl.currentPt := currentPt;
  end else
  begin
    if elRectWH.left.IsValid then
      currentPt.X := elRectWH.left.rawVal else
      currentPt.X := 0;
    if elRectWH.top.IsValid then
      currentPt.Y := elRectWH.top.rawVal else
      currentPt.Y := 0;
    startX := currentPt.X;
  end;

  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      TShapeElement(fChilds[i]).GetPaths(di);
end;
//------------------------------------------------------------------------------

procedure TTextElement.ResetTmpPt;
begin
  startX    := 0;
  currentPt := InvalidPointD;
end;
//------------------------------------------------------------------------------

procedure TTextElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  dx        : double;
begin
  if self = GetTopTextElement then
  begin
    UpdateDrawInfo(drawInfo, self);
    //get child paths
    GetPaths(drawInfo);

    case drawInfo.FontInfo.align of
      staCenter:
        begin
          dx := (currentPt.X - startX) * 0.5;
          DoOffsetX(-dx);
        end;
      staRight:
        begin
          dx := (currentPt.X - startX);
          DoOffsetX(-dx);
        end;
    end;
  end
  else if (currentPt.X = InvalidD) or
    (currentPt.Y = InvalidD) then
      Exit; //probably a <textarea> element

  DrawChildren(img, drawInfo);
end;

//------------------------------------------------------------------------------
// TSubtextElement
//------------------------------------------------------------------------------

constructor TSubtextElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  hasPaths := true;
  fDrawInfo := fParent.fDrawInfo;
  fDrawInfo.matrix := IdentityMatrix;
end;
//------------------------------------------------------------------------------

function FixSpaces(const text: string): string;
var
  i,j, len: integer;
begin
  //changes \r\n\t chars to spaces
  //and trims consecutive spaces

  len  := Length(text);
  SetLength(Result, len);
  if len = 0 then Exit;

  if text[1] <= #32 then
    Result[1] := #32 else
    Result[1] := text[1];

  j := 1;
  for i := 2 to len do
  begin
    if text[i] <= #32 then
    begin
      if Result[j] = #32 then Continue
      else Result[j+1] := #32;
    end
    else
      Result[j+1] := text[i];
    inc(j);
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

procedure TSubtextElement.GetPaths(const drawInfo: TDrawInfo);
var
  el : TElement;
  topTextEl : TTextElement;
  s: string;
  tmpX, offsetX, scale, fontSize, bs: double;
  mat: TMatrixD;
begin
  if Assigned(drawPathsC) then Exit;
  fReader.GetBestFontForFontCache(drawInfo.FontInfo);
  if drawInfo.FontInfo.size = 0 then
    fontSize := 16 else
    fontSize := drawInfo.FontInfo.size;
  if (Length(text) = 0) or (fontSize < 2) or
    not Assigned(fReader.fFontCache) then Exit;

  el := self;
  while (el.fParent is TTextElement) do
    el := el.fParent;
  if not (el is TTextElement) then Exit;
  topTextEl := TTextElement(el);

  if (topTextEl.currentPt.X = InvalidD) or
    (topTextEl.currentPt.Y = InvalidD) then Exit;

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(text));
  {$ELSE}
  s := Utf8Decode(HtmlDecode(text));
  {$ENDIF}
  s := FixSpaces(s);

  drawPathsC := fReader.fFontCache.GetTextGlyphs(0, 0, s, tmpX);
  //by not changing the fontCache.FontHeight, the quality of
  //small font render improves very significantly (though of course
  //this requires additional glyph scaling and offsetting).
  scale := fontSize /fReader.fFontCache.FontHeight;

  with topTextEl.currentPt do
  begin
    offsetX := X;
    X := X + tmpX * scale;
  end;

  with drawInfo.fontInfo do
    if baseShift.rawVal = 0 then
      bs := 0 else
      bs := baseShift.GetValue(size, GetRelFracLimit);

  mat := IdentityMatrix;
  MatrixScale(mat, scale);
  MatrixTranslate(mat, offsetX, topTextEl.currentPt.Y - bs);
  MatrixApply(mat, drawPathsC);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

constructor TTSpanElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.FontInfo.decoration := fdUndefined;
  fDrawInfo.FontInfo.baseShift.SetValue(0);

  elRectWH.Init;
  currentPt := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TTextPathElement
//------------------------------------------------------------------------------

procedure TTextPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  parentTextEl, topTextEl: TTextElement;
  el: TElement;
  isFirst: Boolean;
  s: string;
  i, len, charsThatFit: integer;
  d, fontScale, spacing: double;
  utf8String: AnsiString;
  mat: TMatrixD;
  tmpPath: TPathD;
  isClosed: Boolean;
begin
  if Assigned(drawPathsC) then Exit;
  fReader.GetBestFontForFontCache(drawInfo.FontInfo);
  if (drawInfo.FontInfo.size < 2) or
    not Assigned(fReader.fFontCache) then Exit;

  parentTextEl := TTextElement(fParent);
  topTextEl := parentTextEl;
  isFirst := IsFirstChild;
  while topTextEl.fParserEl.hash <> hText do
  begin
    isFirst := isFirst and topTextEl.IsFirstChild;
    topTextEl := TTextElement(topTextEl.fParent);
  end;

  //if first subtext then reset X offset
  if not isFirst then Exit;
  topTextEl.ResetTmpPt;
  utf8String := '';

  //nb: only exit AFTER setting parentTextEl.tmpPt.
  if (fParserEl.text.len = 0) then
  begin
    if (fChilds.Count = 0) or
      not (TElement(fChilds[0]) is TTSpanElement) then
        Exit;
    el := TElement(fChilds[0]);
    if (el.fChilds.Count = 0) or
      not (TElement(el.fChilds[0]) is TSubtextElement) then
        Exit;
    with TSubtextElement(el.fChilds[0]) do
    begin
      utf8String := text;
      spacing := fDrawInfo.FontInfo.spacing;
    end;
  end else
  begin
    utf8String := fParserEl.text.AsAnsiString;
    spacing := drawInfo.FontInfo.spacing;
  end;

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(utf8String));
  {$ELSE}
  s := Utf8Decode(HtmlDecode(utf8String));
  {$ENDIF}
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;

  i := PosEx(#32#32, s);
  while i > 0 do
  begin
    Delete(s, i, 1);
    i := PosEx(#32#32, s, i);
  end;

  el := FindRefElement(pathEl);
  if not (el is TPathElement) then Exit;
  fontScale := drawInfo.FontInfo.size/fReader.fFontCache.FontHeight;
  spacing := spacing /fontScale;

  //adjust glyph spacing when fFontInfo.textLength is assigned.
  len := Length(s);
  if (len > 1) and (drawInfo.FontInfo.textLength > 0) then
  begin
    d := fReader.fFontCache.GetTextWidth(s);
    spacing := (drawInfo.FontInfo.textLength/fontScale) - d;
    spacing := spacing / (len -1);
  end;

  with TPathElement(el) do
  begin
    mat := fDrawInfo.matrix;
    MatrixScale(mat, 1/fontScale);
    for i := 0 to High(fSvgPaths) do
    begin
      Flatten(i, fontScale, tmpPath, isClosed);
      //'path' is temporarily scaled to accommodate fReader.fFontCache's
      //static fontheight. The returned glyphs will be de-scaled later.
      MatrixApply(mat, tmpPath);
      AppendPath(self.drawPathsC,
        GetTextGlyphsOnPath(s, tmpPath, fReader.fFontCache,
          taLeft, 0, spacing, charsThatFit));
      if charsThatFit = Length(s) then Break;
      Delete(s, 1, charsThatFit);
    end;
  end;
  drawPathsC := ScalePath(drawPathsC, fontScale);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TMarkerElement
//------------------------------------------------------------------------------

constructor TMarkerElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i, len: integer;
  l,t,w,h,scale, a, a2: double;
  mat: TMatrixD;
  angles: TArrayOfDouble;
begin
  UpdateDrawInfo(drawInfo, self);
  mat := drawInfo.matrix;

  if elRectWH.width.IsValid and elRectWH.height.IsValid and
    not markerBoxWH.IsEmpty then
  begin
    w := elRectWH.width.rawVal;
    h := elRectWH.height.rawVal;
    //currently assume preserve aspect ratio
    scale := Min(w/markerBoxWH.Width, h/markerBoxWH.Height);
    MatrixScale(mat, scale, scale);
  end;

  if refPt.X.IsValid and refPt.Y.IsValid then
  begin
    l := refPt.X.rawVal;
    t := refPt.Y.rawVal;
    scale := ExtractAvgScaleFromMatrix(mat);
    MatrixTranslate(mat, -l * scale, -t * scale);
  end;

  len := Length(fPoints);
  if len = 0 then Exit;
  SetLength(angles, len);
  angles[0] := angle;
  a := angle;
  for i := 0 to len -2 do
  begin
    a2 := GetAngle(fPoints[i], fPoints[i+1]);
    angles[i] := Average(a, a2);
    a := a2;
  end;
  if len > 1 then
    angles[len -1] := Average(a, angle2);

  //for each 'point' draw the marker
  for i := 0 to len -1 do
  begin
    drawInfo.matrix := mat;
    MatrixRotate(drawInfo.matrix, NullPointD, angles[i]);
    MatrixTranslate(drawInfo.matrix, fPoints[i].X, fPoints[i].Y);
    DrawChildren(img, drawInfo);
  end;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.SetEndPoint(const pt: TPointD; angle: double);
begin
  SetLength(fPoints, 1);
  fPoints[0] := pt;
  self.angle := angle;
end;
//------------------------------------------------------------------------------

function TMarkerElement.SetMiddlePoints(const points: TPathD): Boolean;
var
  len: integer;
begin
  len := Length(points);
  Result := len > 2;
  if Result then
  begin
    angle := GetAngle(Points[0],Points[1]);
    angle2 := GetAngle(Points[len-2],Points[len-1]);
    Self.fPoints := Copy(points, 1, len -2);
  end;
end;

//------------------------------------------------------------------------------
// TFillElement
//------------------------------------------------------------------------------

function TFillElement.GetRelFracLimit: double;
begin
  //always assume fractional values below 1 are relative
  Result := 1.0;
end;

//------------------------------------------------------------------------------
// TPatternElement
//------------------------------------------------------------------------------

constructor TPatternElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited;
  elRectWH.Init;
  pattBoxWH.Width   := InvalidD;
  pattBoxWH.Height  := InvalidD;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

function TPatternElement.PrepareRenderer(renderer: TImageRenderer;
  drawInfo: TDrawInfo): Boolean;
var
  i     : integer;
  recWH : TRectWH;
  el    : TElement;
  rec   : TRectD;
  mat   : TMatrixD;
  sx,sy : double;
  scale: TSizeD;
  closedPaths, openPaths: TPathsD;
begin
  Result := false;

  scale := ExtractScaleFromMatrix(drawInfo.matrix);
  if units = hUserSpaceOnUse then
    rec := fReader.userSpaceBounds else
    rec := drawInfo.bounds;

  //todo: implement patternUnits & patternContentUnits too

  sx := 1; sy := 1;
  if elRectWH.Width.IsValid and elRectWH.Height.IsValid then
  begin
    recWH := elRectWH.GetRectWH(rec, GetRelFracLimit);

    //also scale if necessary
    if not pattBoxWH.IsEmpty then
    begin
      sx := recWH.Width/pattBoxWH.Width;
      sy := recWH.Height/pattBoxWH.Height;
    end;

  end
  else if not pattBoxWH.IsEmpty then
  begin
    recWH.Width   := pattBoxWH.Width;
    recWH.Height  := pattBoxWH.Width;
  end else
    Exit;

  renderer.Image.SetSize(
    Round(recWH.Width * scale.sx),
    Round(recWH.Height * scale.sy));

  Result := true;
  closedPaths := nil;
  openPaths   := nil;

  mat := IdentityMatrix;
  MatrixScale(mat, scale.sx * sx, scale.sy * sy);

  //recWH.Left := 0; recWH.Top := 0;
  if (refEl <> '') then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TShapeElement) then
      with TShapeElement(el) do
      begin
        drawInfo := fDrawInfo;
        drawInfo.matrix := mat;
        drawInfo.bounds := recWH.RectD;
        Draw(renderer.Image, drawInfo);
      end;
  end;

  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        drawInfo := fDrawInfo;
        drawInfo.matrix := mat;
        drawInfo.bounds := rec;
        Draw(renderer.Image, drawInfo);
      end;
end;

//------------------------------------------------------------------------------
// TSvgElement
//------------------------------------------------------------------------------

constructor TSvgElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
  inherited Create(parent, svgEl);
end;
//------------------------------------------------------------------------------

function TSvgElement.GetViewbox: TRect;
var
  w,h, sx,sy: double;
begin
  Result := NullRect;
  Result.Left := Round(elRectWH.left.GetValue(0, 0));
  Result.Top := Round(elRectWH.top.GetValue(0, 0));

  w := elRectWH.width.GetValue(0, 0);
  h := elRectWH.height.GetValue(0, 0);

  if viewboxWH.IsEmpty then
  begin
    Result := Rect(0,0, Round(w), Round(h));
  end
  else if (w > 0) and (h > 0) then
  begin
    sx := w/viewboxWH.Width;
    sy := h/viewboxWH.Height;
    //preserveAspectRatio
    sx := (sx + sy) * 0.5; sy := sx;
    Result.Right := Result.Left + Round(viewboxWH.Width * sx);
    Result.Bottom := Result.Top + Round(viewboxWH.Height * sy);
  end;

end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; svgEl: TSvgTreeEl);
begin
{$IFDEF XPLAT_GENERICS}
  fChilds         := TList<TElement>.create;
{$ELSE}
  fChilds         := TList.Create;
{$ENDIF}
  fParserEl          := svgEl;
  self.fParent    := parent;
  if not Assigned(parent) then Exit;

  fReader         := parent.fReader;
  fDrawInfo       := emptyDrawInfo;
  elRectWH.Init;
end;
//------------------------------------------------------------------------------

destructor  TElement.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TElement(fChilds[i]).Free;
  fChilds.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function  TElement.IsFirstChild: Boolean;
begin
  Result := not Assigned(fParent) or (self = fParent.fChilds[0]);
end;
//------------------------------------------------------------------------------

procedure TElement.Draw(image: TImage32; drawInfo: TDrawInfo);
begin
  DrawChildren(image, drawInfo);
end;
//------------------------------------------------------------------------------

procedure TElement.DrawChildren(image: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    with TElement(fChilds[i]) do
      if fDrawInfo.visible then Draw(image, drawInfo);
end;
//------------------------------------------------------------------------------

function TElement.FindRefElement(refname: AnsiString): TElement;
var
  i, len: integer;
  c, endC: PAnsiChar;
  ref: AnsiString;
begin
  result := nil;
  len := Length(refname);
  if len = 0 then Exit;
  c := PAnsiChar(refname);
  endC := c + len;
  if Match(c, 'url(') then
  begin
    inc(c, 4);
    dec(endC); //removes trailing ')'
  end;
  if c^ = '#' then inc(c);
  PAnsiCharToAnsiString(c, endC, ref);
  i := fReader.fIdList.IndexOf(string(ref));
  if i >= 0 then
    Result := TElement(fReader.fIdList.Objects[i]) else
    Result := nil;
end;

//------------------------------------------------------------------------------
// dozens of function to process various element attributes
//------------------------------------------------------------------------------

procedure Id_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  aOwnerEl.fReader.fIdList.AddObject(string(value), aOwnerEl);
end;
//------------------------------------------------------------------------------

procedure In_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TFeBaseElement then
    TFeBaseElement(aOwnerEl).in1 := value;
end;
//------------------------------------------------------------------------------

procedure In2_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TFeBaseElement then
    TFeBaseElement(aOwnerEl).in2 := value;
end;
//------------------------------------------------------------------------------

procedure LetterSpacing_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with TTextElement(aOwnerEl) do
    AnsiStringToFloat(value, fDrawInfo.FontInfo.spacing);
end;
//------------------------------------------------------------------------------

procedure Href_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  el: TElement;
begin
  el := aOwnerEl;
  case el.fParserEl.Hash of
    hUse:
      TUseElement(el).refEl := ExtractRef(value);
    hTextPath:
      TTextPathElement(el).pathEl := ExtractRef(value);
    else if el is TFillElement then
      TFillElement(el).refEl := ExtractRef(value);
  end;
end;
//------------------------------------------------------------------------------

procedure BaselineShift_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  ParseNextWord(c, endC, word);
  with aOwnerEl.fDrawInfo.FontInfo do
    case GetHash(word) of
      hSuper: baseShift.SetValue(50, utPercent);
      hSub: baseShift.SetValue(-50, utPercent);
      hBaseline: baseShift.SetValue(0, utPixel);
      else
      begin
        AnsiStringToFloatEx(value, val, mu);
        baseShift.SetValue(val, mu);
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure Color_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  color: TColor32;
begin
  color := clInvalid;
  AnsiStringToColor32(value, color);
  //for setting currentcolor when drawing (eg drawing shapes)
  aOwnerEl.fDrawInfo.currentColor := color;
  //for setting currentcolor during element creation (eg gradient colors)
  aOwnerEl.fReader.currentColor := color;
end;
//------------------------------------------------------------------------------

procedure LightingColor_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  color: TColor32;
begin
  color := clInvalid;
  AnsiStringToColor32(value, color);
  if (aOwnerEl is TFeSpecLightElement) then
    TFeSpecLightElement(aOwnerEl).color := color
  else if (aOwnerEl is TFeDefuseLightElement) then
    TFeDefuseLightElement(aOwnerEl).color := color
end;
//------------------------------------------------------------------------------

procedure ClipPath_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  aOwnerEl.fDrawInfo.clipPathEl := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure D_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TPathElement then
    TPathElement(aOwnerEl).ParseDAttrib(value);
end;
//------------------------------------------------------------------------------

procedure Fill_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  case aOwnerEl.fParserEl.Hash of
    hfeDropShadow:
      AnsiStringToColor32(value, TFeDropShadowElement(aOwnerEl).floodColor);
    hfeFlood:
      AnsiStringToColor32(value, TFeFloodElement(aOwnerEl).floodColor);
    else
    begin
      if Match(PAnsiChar(value), 'url(') then
        aOwnerEl.fDrawInfo.fillEl := ExtractRef(value)
      else
        AnsiStringToColor32(value, aOwnerEl.fDrawInfo.fillColor);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FillOpacity_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  case aOwnerEl.fParserEl.Hash of
    hfeDropShadow:
      AnsiStringToOpacity(value, TFeDropShadowElement(aOwnerEl).floodColor);
    hfeFlood:
      AnsiStringToOpacity(value, TFeFloodElement(aOwnerEl).floodColor);
    else
      AnsiStringToOpacity(value, aOwnerEl.fDrawInfo.fillColor);
  end;
end;
//------------------------------------------------------------------------------

procedure DashArray_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  c, endC: PAnsiChar;
  val: double;
  len: integer;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawInfo do
  begin
    len := Length(dashArray);
    while ParseNextNum(c, endC, true, val) do
    begin
      SetLength(dashArray, len +1);
      dashArray[len] := val;
      inc(len);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DashOffset_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawInfo do
    ParseNextNum(c, endC, true, dashOffset);
end;
//------------------------------------------------------------------------------

procedure Display_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if GetHash(value) = hNone then
    aOwnerEl.fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure Font_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  GetSvgFontInfo(value, aOwnerEl.fDrawInfo.FontInfo);
end;
//------------------------------------------------------------------------------

procedure FontFamily_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  with aOwnerEl.fDrawInfo.FontInfo do
  begin
    family := ttfUnknown;
    c := PAnsiChar(value);
    endC := c + Length(value);
    while ParseNextWordEx(c, endC, word) do
    begin
      case GetHash(word) of
        hSans_045_Serif, hArial  : family := ttfSansSerif;
        hSerif, hTimes: family := ttfSerif;
        hMonospace: family := ttfMonospace;
        else Continue;
      end;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FontSize_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  num: double;
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value); endC := c + Length(value);
  if not ParseNextNum(c, endC, false, num) then Exit;
  aOwnerEl.fDrawInfo.FontInfo.size := num;
end;
//------------------------------------------------------------------------------

procedure FontStyle_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl.fDrawInfo.FontInfo do
    if GetHash(value) = hItalic then
      italic := sfsItalic else
      italic := sfsNone;
end;
//------------------------------------------------------------------------------

procedure FontWeight_Attrib(aOwnerEl: TElement; const value: AnsiString);

var
  num: double;
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawInfo.FontInfo do
  begin
    if IsNumPending(c, endC, false) and
      ParseNextNum(c, endC, false, num) then
        weight := Round(num)
    else if ParseNextWord(c, endC, word) then
      case GetHash(word) of
        hBold   : weight := 600;
        hNormal : weight := 400;
        hBolder : if weight >= 0 then weight := Min(900, weight + 200)
                  else weight := 600;
        hLighter: if weight >= 0 then weight := Max(0, weight - 200)
                  else weight := 200;

      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Fx_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TRadGradElement) then
    with TRadGradElement(aOwnerEl) do
    begin
      AnsiStringToFloatEx(value, F.X.rawVal, F.X.unitType);
    end;
end;
//------------------------------------------------------------------------------

procedure Fy_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TRadGradElement) then
    with TRadGradElement(aOwnerEl) do
    begin
      AnsiStringToFloatEx(value, F.Y.rawVal, F.Y.unitType);
    end;
end;
//------------------------------------------------------------------------------

procedure TextAlign_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl.fDrawInfo.FontInfo do
    case GetHash(value) of
      hMiddle : align := staCenter;
      hEnd    : align := staRight;
      else align := staLeft;
    end;
end;
//------------------------------------------------------------------------------

procedure TextDecoration_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl.fDrawInfo.FontInfo do
    case GetHash(value) of
      hUnderline        : decoration := fdUnderline;
      hline_045_through : decoration := fdStrikeThrough;
      else                decoration := fdNone;
    end;
end;
//------------------------------------------------------------------------------

procedure TextLength_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  AnsiStringToFloat(value, aOwnerEl.fDrawInfo.FontInfo.textLength);
end;
//------------------------------------------------------------------------------


procedure MarkerStart_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawInfo.markerStart := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure MarkerMiddle_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawInfo.markerMiddle := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure MarkerEnd_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawInfo.markerEnd := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Filter_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TShapeElement) then
    aOwnerEl.fDrawInfo.filterEl := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Mask_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TShapeElement) then
    aOwnerEl.fDrawInfo.maskEl := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Offset_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: TValue;
begin
  if (aOwnerEl is TGradStopElement) then
    with TGradStopElement(aOwnerEl) do
    begin
      val.Init;
      AnsiStringToFloatEx(value, val.rawVal, val.unitType);
      offset := val.GetValue(1, GetRelFracLimit);
    end
end;
//------------------------------------------------------------------------------

procedure Opacity_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  opacity: double;
begin
  if not AnsiStringToFloat(value, opacity) then Exit;
  if opacity < 0 then opacity := 0
  else if opacity > 1 then opacity := 1;
  aOwnerEl.fDrawInfo.opacity := Round(opacity * 255);
end;
//------------------------------------------------------------------------------

procedure Operator_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TFeCompositeElement) then
    with TFeCompositeElement(aOwnerEl) do
      case GetHash(value) of
        hAtop       : compositeOp := coAtop;
        hIn         : compositeOp := coIn;
        hOut        : compositeOp := coOut;
        hOver       : compositeOp := coOver;
        hXor        : compositeOp := coXor;
        hArithmetic : compositeOp := coArithmetic;
      end;
end;
//------------------------------------------------------------------------------

procedure Orient_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TMarkerElement) and
    (GetHash(value) = hauto_045_start_045_reverse) then
        TMarkerElement(aOwnerEl).autoStartReverse := true;
end;
//------------------------------------------------------------------------------

procedure StopColor_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  acolor: TColor32;
begin
  if aOwnerEl is TGradStopElement then
  begin
    acolor := clInvalid;
    AnsiStringToColor32(value, acolor);
    with TGradStopElement(aOwnerEl) do
      if acolor = clCurrent then
        color := aOwnerEl.fReader.currentColor else
        color := acolor;
  end;
end;
//------------------------------------------------------------------------------

procedure StopOpacity_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TGradStopElement then
  AnsiStringToOpacity(value, TGradStopElement(aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure Points_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TPolyElement then
    TPolyElement(aOwnerEl).ParsePoints(value);
end;
//------------------------------------------------------------------------------

procedure Stroke_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if Match(PAnsiChar(value), 'url(') then
    aOwnerEl.fDrawInfo.strokeEl := ExtractRef(value)
  else
    AnsiStringToColor32(value, aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeLineCap_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  ParseNextWord(c, endC, word);
  with aOwnerEl.fDrawInfo do
    case GetHash(word) of
      hButt   : strokeCap := esButt;
      hRound  : strokeCap := esRound;
      hSquare : strokeCap := esSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeLineJoin_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  ParseNextWord(c, endC, word);
  with aOwnerEl.fDrawInfo do
    case GetHash(word) of
      hMiter  : strokeJoin := jsMiter;
      hRound  : strokeJoin := jsRound;
      hBevel  : strokeJoin := jsSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeMiterLimit_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  AnsiStringToFloat(value, aOwnerEl.fDrawInfo.strokeMitLim);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  AnsiStringToOpacity(value, aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl do
  begin
    AnsiStringToFloatEx(value, fDrawInfo.strokewidth.rawVal,
      fDrawInfo.strokewidth.unitType);
  end;
end;
//------------------------------------------------------------------------------

procedure FillRule_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if LowerCaseTable[value[1]] = 'e' then
    aOwnerEl.fDrawInfo.fillRule := frEvenOdd else
    aOwnerEl.fDrawInfo.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Style_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl do
    case fParserEl.Hash of
      hFlowRegion, hFlowRoot: fDrawInfo.fillColor := clNone32;
      //else fStyleAttribIdx := High(fAttribs);
    end;
end;
//------------------------------------------------------------------------------

procedure Transform_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  with aOwnerEl.fDrawInfo do
    matrix := MatrixMultiply(matrix, ParseTransform(value));
end;
//------------------------------------------------------------------------------

procedure Values_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  cnt: integer;
  c, endC: PAnsiChar;
begin
  if aOwnerEl is TFeColorMatrixElement then
    with TFeColorMatrixElement(aOwnerEl) do
    begin
      SetLength(values, 20);
      c := PAnsiChar(value);
      endC := c + Length(value);
      cnt := 0;
      while (cnt < 20) and ParseNextNum(c, endC, true, values[cnt]) do
        inc(cnt);
      if cnt < 20 then values := nil;
    end;
end;
//------------------------------------------------------------------------------

procedure GradientTransform_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mat: TMatrixD;
begin
  if not (aOwnerEl is TGradientElement) then Exit;
  mat := ParseTransform(value);
  with aOwnerEl.fDrawInfo do
    matrix := MatrixMultiply(matrix, mat);
end;
//------------------------------------------------------------------------------

procedure GradientUnits_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if aOwnerEl is TFillElement then
    with TFillElement(aOwnerEl) do
      units := GetHash(value);
end;
//------------------------------------------------------------------------------

procedure Viewbox_Attrib(aOwnerEl: TElement; const value: AnsiString);

  function LoadViewbox: TRectWH;
  var
    c, endC: PAnsiChar;
  begin
    c := PAnsiChar(value);
    endC := c + Length(value);
    with Result do
      if not ParseNextNum(c, endC, false, Left) or
        not ParseNextNum(c, endC, true, Top) or
        not ParseNextNum(c, endC, true, Width) or
        not ParseNextNum(c, endC, true, Height) then
          Result := RectWH(0,0,0,0);
  end;

begin
  case aOwnerEl.fParserEl.Hash of
    hSvg    : TSvgElement(aOwnerEl).viewboxWH := LoadViewbox;
    hMarker : TMarkerElement(aOwnerEl).markerBoxWH := LoadViewbox;
    hSymbol : TSymbolElement(aOwnerEl).viewboxWH := LoadViewbox;
    else if aOwnerEl is TPatternElement then
      TPatternElement(aOwnerEl).pattBoxWH := LoadViewbox;
  end;
end;
//------------------------------------------------------------------------------

procedure Height_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  with aOwnerEl do
  begin
    elRectWH.height.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  with aOwnerEl do
  begin
    elRectWH.width.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Cx_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hCircle:
      with TCircleElement(aOwnerEl) do centerPt.X.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(aOwnerEl) do centerPt.X.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        C.X.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Cy_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hCircle:
      with TCircleElement(aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        C.Y.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Dx_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).offset.X.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(aOwnerEl).offset.X.SetValue(val, mu);
    hText, hTSpan:
      TTextElement(aOwnerEl).offset.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Dy_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hText, hTSpan:
      TTextElement(aOwnerEl).offset.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Result_Attrib(aOwnerEl: TElement; const value: AnsiString);
begin
  if (aOwnerEl is TFeBaseElement) then
    TFeBaseElement(aOwnerEl).res := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Rx_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hRect:
      with TRectElement(aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hCircle:
      with TCircleElement(aOwnerEl) do
      begin
        radius.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        radius.X. SetValue(val, mu);
        radius.Y. SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(aOwnerEl) do
        refPt.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Ry_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hRect:
      with TRectElement(aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(aOwnerEl) do refPt.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure SpreadMethod_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  word: AnsiString;
  c, endC: PAnsiChar;
begin
  if not (aOwnerEl is TGradientElement) then Exit;
  c := PAnsiChar(value);
  endC := c + Length(value);
  ParseNextWord(c, endC, word);
  with TGradientElement(aOwnerEl) do
    case GetHash(word) of
      hPad      : spreadMethod := gfsClamp;
      hReflect  : spreadMethod := gfsMirror;
      hRepeat   : spreadMethod := gfsRepeat;
    end;
end;
//------------------------------------------------------------------------------

procedure SpectacularExponent(aOwnerEl: TElement; const value: AnsiString);
var
  se: double;
begin
  if not (aOwnerEl is TFeSpecLightElement) then Exit;
  AnsiStringToFloat(value, se);
  if (se > 0) and (se < 100) then
    TFeSpecLightElement(aOwnerEl).exponent := se;
end;
//------------------------------------------------------------------------------

procedure StdDev_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  sd: double;
begin
  AnsiStringToFloat(value, sd);
  if (sd < 0) and (sd > 100) then Exit;
  case aOwnerEl.fParserEl.Hash of
    hfeGaussianBlur:
      TFeGaussElement(aOwnerEl).stdDev := sd;
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).stdDev := sd;
  end;
end;
//------------------------------------------------------------------------------

procedure K1_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: double;
begin
  AnsiStringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).ks[0] := val;
end;
//------------------------------------------------------------------------------

procedure K2_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: double;
begin
  AnsiStringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).ks[1] := val;
end;
//------------------------------------------------------------------------------

procedure K3_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: double;
begin
  AnsiStringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).ks[2] := val;
end;
//------------------------------------------------------------------------------

procedure K4_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: double;
begin
  AnsiStringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).ks[3] := val;
end;
//------------------------------------------------------------------------------

procedure X1_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[0].X := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        startPt.X.SetValue(val, mu);
      end;
    hFilter:
      with aOwnerEl do
      begin
        elRectWH.left.SetValue(val, mu);
      end;
    else
      aOwnerEl.elRectWH.left.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure X2_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[1].X := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        endPt.X.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Y1_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[0].Y := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        startPt.Y.SetValue(val, mu);
      end;
    hFilter:
      with aOwnerEl do
      begin
        elRectWH.top.SetValue(val, mu);
      end;
    else
      aOwnerEl.elRectWH.top.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Y2_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  mu: TUnitType;
  val: double;
begin
  AnsiStringToFloatEx(value, val, mu);
  case aOwnerEl.fParserEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[1].Y := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        endPt.Y.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Z_Attrib(aOwnerEl: TElement; const value: AnsiString);
var
  val: double;
begin
  AnsiStringToFloat(value, val);
  if aOwnerEl is TFePointLightElement then
    TFePointLightElement(aOwnerEl).z := val;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TElement.LoadAttribute(attrib: PSvgAttrib);
begin
  with attrib^ do
  case hash of
    hbaseline_045_shift:    BaselineShift_Attrib(self, value);
    hColor:                 Color_Attrib(self, value);
    hClip_045_path:         ClipPath_Attrib(self, value);
    hCx:                    Cx_Attrib(self, value);
    hCy:                    Cy_Attrib(self, value);
    hD:                     D_Attrib(self, value);
    hDisplay:               Display_Attrib(self, value);
    hDx:                    Dx_Attrib(self, value);
    hDy:                    Dy_Attrib(self, value);
    hStroke_045_DashArray:  DashArray_Attrib(self, value);
    hStroke_045_DashOffset: DashOffset_Attrib(self, value);
    hFill:                  Fill_Attrib(self, value);
    hFill_045_Opacity:      FillOpacity_Attrib(self, value);
    hFill_045_Rule:         FillRule_Attrib(self, value);
    hFilter:                Filter_Attrib(self, value);
    hflood_045_color:       Fill_Attrib(self, value);
    hflood_045_opacity:     FillOpacity_Attrib(self, value);
    hFont:                  Font_Attrib(self, value);
    hFont_045_Family:       FontFamily_Attrib(self, value);
    hFont_045_Size:         FontSize_Attrib(self, value);
    hFont_045_Style:        FontStyle_Attrib(self, value);
    hFont_045_Weight:       FontWeight_Attrib(self, value);
    hFx:                    Fx_Attrib(self, value);
    hFy:                    Fy_Attrib(self, value);
    hGradientTransform:     GradientTransform_Attrib(self, value);
    hGradientUnits:         GradientUnits_Attrib(self, value);
    hHeight:                Height_Attrib(self, value);
    hHref:                  Href_Attrib(self, value);
    hId:                    Id_Attrib(self, value);
    hIn:                    In_Attrib(self, value);
    hIn2:                   In2_Attrib(self, value);
    hk1:                    K1_Attrib(self, value);
    hk2:                    K2_Attrib(self, value);
    hk3:                    K3_Attrib(self, value);
    hk4:                    K4_Attrib(self, value);
    hletter_045_spacing:    LetterSpacing_Attrib(self, value);
    hlighting_045_color:    LightingColor_Attrib(self, value);
    hMarker_045_End:        MarkerEnd_Attrib(self, value);
    hMarkerHeight:          Height_Attrib(self, value);
    hMarker_045_Mid:        MarkerMiddle_Attrib(self, value);
    hMarker_045_Start:      MarkerStart_Attrib(self, value);
    hMarkerWidth:           Width_Attrib(self, value);
    hMask:                  Mask_Attrib(self, value);
    hOffset:                Offset_Attrib(self, value);
    hOpacity:               Opacity_Attrib(self, value);
    hOperator:              Operator_Attrib(self, value);
    hOrient:                Orient_Attrib(self, value);
    hPatternUnits:          GradientUnits_Attrib(self, value);
    hPatternTransform:      Transform_Attrib(self, value);
    hPoints:                Points_Attrib(self, value);
    hR:                     Rx_Attrib(self, value);
    hRefX:                  Rx_Attrib(self, value);
    hRefY:                  Ry_Attrib(self, value);
    hResult:                Result_Attrib(self, value);
    hRx:                    Rx_Attrib(self, value);
    hRy:                    Ry_Attrib(self, value);
    hspecularExponent:      SpectacularExponent(self, value);
    hSpreadMethod:          SpreadMethod_Attrib(self, value);
    hstdDeviation:          StdDev_Attrib(self, value);
    hStop_045_Color:        StopColor_Attrib(self, value);
    hStop_045_Opacity:      StopOpacity_Attrib(self, value);
    hStroke:                Stroke_Attrib(self, value);
    hstroke_045_linecap:    StrokeLineCap_Attrib(self, value);
    hstroke_045_linejoin:   StrokeLineJoin_Attrib(self, value);
    hstroke_045_miterlimit: StrokeMiterLimit_Attrib(self, value);
    hStroke_045_Opacity:    StrokeOpacity_Attrib(self, value);
    hStroke_045_Width:      StrokeWidth_Attrib(self, value);
    hText_045_Anchor:       TextAlign_Attrib(self, value);
    hText_045_Decoration:   TextDecoration_Attrib(self, value);
    hTextLength:            TextLength_Attrib(self, value);
    hTransform:             Transform_Attrib(self, value);
    hValues:                Values_Attrib(self, value);
    hViewbox:               Viewbox_Attrib(self, value);
    hWidth:                 Width_Attrib(self, value);
    hX:                     X1_Attrib(self, value);
    hX1:                    X1_Attrib(self, value);
    hX2:                    X2_Attrib(self, value);
    hXlink_058_Href:        Href_Attrib(self, value);
    hY:                     Y1_Attrib(self, value);
    hY1:                    Y1_Attrib(self, value);
    hY2:                    Y2_Attrib(self, value);
    hZ:                     Z_Attrib(self, value);
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.LoadAttributes;
var
  i: integer;
begin
  for i := 0 to fParserEl.attribs.Count -1 do
    LoadAttribute(PSvgAttrib(fParserEl.attribs[i]));
end;
//------------------------------------------------------------------------------

function PreferRelativeFraction(val: TValue): TTriState;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if (val.rawVal = InvalidD) or (val.unitType = utUnknown) then
    Result := tsUnknown
  else if val.unitType = utPercent then Result := tsYes
  else if val.unitType <> utNumber then Result := tsNo
  else if (Abs(val.rawVal) < 1) then Result := tsYes
  else Result := tsNo;
end;
//------------------------------------------------------------------------------

function TElement.GetRelFracLimit: double;
begin
  //the default behaviour here is to assume untyped fractional values
  //below 1.0 are values relative (to the bounding size) BUT ONLY WHEN
  //the parent element's width or height are relative (ie percentages).
  if Assigned(fParent) and (fParent.fParserEl.hash <> hSvg) then
  begin
    case PreferRelativeFraction(fParent.elRectWH.width) of
      tsYes: begin Result := 1.0; Exit; end;
      tsNo : begin Result := 0; Exit; end;
    end;
    case PreferRelativeFraction(fParent.elRectWH.height) of
      tsYes: begin Result := 1.0; Exit; end;
      tsNo : begin Result := 0; Exit; end;
    end;
  end;
  Result := 0;
end;
//------------------------------------------------------------------------------

function TElement.LoadContent: Boolean;
var
  i       : integer;
  svgEl   : TSvgTreeEl;
  elClass : TElementClass;
  el      : TElement;
begin
  Result := false;
  for i := 0 to fParserEl.childs.Count -1 do
  begin
    svgEl := TSvgTreeEl(fParserEl.childs[i]);
    if svgEl.hash = 0 then
      Continue;
    elClass := HashToElementClass(svgEl.hash);
    el := elClass.Create(self, svgEl);
    Self.fChilds.Add(el);
    el.LoadAttributes;
    if not el.LoadContent then //ie recursive
      Exit; //error
  end;
  Result := true;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fSvgParser        := TSvgParser.Create;
  fIdList             := TStringList.Create;
  fIdList.Duplicates  := dupIgnore;
  fIdList.CaseSensitive := false;
  fIdList.Sorted      := True;
  fBlurQuality        := 1; //0: draft (faster); 1: good; 2: excellent (slow)
  currentColor        := clBlack32;
  fClassStyles        := TClassStylesList.Create;

  fLinGradRenderer  := TLinearGradientRenderer.Create;
  fRadGradRenderer  := TSvgRadialGradientRenderer.Create;
  fImgRenderer      := TImageRenderer.Create;
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fSvgParser.Free;
  fIdList.Free;
  fClassStyles.Free;

  fLinGradRenderer.Free;
  fRadGradRenderer.Free;
  fImgRenderer.Free;
  FreeAndNil(fFontCache);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fSvgParser.Clear;
  fIdList.Clear;
  fClassStyles.Clear;
  fLinGradRenderer.Clear;
  fRadGradRenderer.Clear;
  fImgRenderer.Image.Clear;
  currentColor := clBlack32;
  userSpaceBounds := NullRectD;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  rawScale, sx, sy, w,h: double;
  di: TDrawInfo;
begin
  if not Assigned(fRootElement) then Exit;

  with fRootElement do
  begin
    di := emptyDrawInfo;
    MatrixTranslate(di.matrix, -viewboxWH.Left, -viewboxWH.Top);

    w := elRectWH.width.GetValue(RectWidth(img.Bounds), 0);
    h := elRectWH.height.GetValue(RectHeight(img.Bounds), 0);

    if viewboxWH.IsEmpty then
    begin
      viewboxWH := RectWH(0, 0, w, h);
      if viewboxWH.IsEmpty then
        viewboxWH := RectWH(0, 0, img.Width, img.Height);
      fDrawInfo.bounds := viewboxWH.RectD;
      userSpaceBounds  := viewboxWH.RectD;
    end
    else if (w > 0) or (h > 0) then
    begin
      fDrawInfo.bounds := viewboxWH.RectD;
      userSpaceBounds  := viewboxWH.RectD;

      if (w > 0) then
        sx := w/viewboxWH.Width else
        sx := h/viewboxWH.Height;
      if (h > 0) then
        sy := h/viewboxWH.Height else
        sy := sx;
      //assume default preserveAspectRatio - ie xMidYMid.
      sx := (sx + sy) * 0.5; sy := sx;
      MatrixScale(di.matrix, sx, sy);
      viewboxWH.Width := viewboxWH.Width * sx;
      viewboxWH.Height := viewboxWH.Height * sy;
    end else
    begin
      fDrawInfo.bounds := viewboxWH.RectD;
      userSpaceBounds  := viewboxWH.RectD;
    end;
    di.bounds := fDrawInfo.bounds;

    //the rootElement's matrix may have been modified by
    //the svg element's height/width and viewbox settings
    if scaleToImage and not img.IsEmpty then
    begin
      rawScale := img.width / (viewboxWH.Width);
      sy := img.height / (viewboxWH.Height);
      if sy < rawScale then rawScale := sy;
      MatrixScale(di.matrix, rawScale);
      img.SetSize(
        Round(viewboxWH.Width * rawScale),
        Round(viewboxWH.Height * rawScale));
    end else
      img.SetSize(Round(viewboxWH.Width), Round(viewboxWH.Height));
  end;

  if fBkgndColor <> clNone32 then
    img.Clear(fBkgndColor);

  fTempImage := TImage32.Create(img.Width, img.Height);
  try
    fRootElement.Draw(img, di);
  finally
    fTempImage.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadInternal: Boolean;
begin
  Result := false;
  if not Assigned(fSvgParser.svgTree) or
    (fSvgParser.svgTree.hash <> hSvg) then Exit;
  fRootElement := TSvgElement.Create(nil, fSvgParser.svgTree);
  fRootElement.fReader := self;
  fRootElement.LoadAttributes;
  Result := fRootElement.LoadContent;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromFile(filename);
  if Result then LoadInternal;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromStream(stream: TStream): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromStream(stream);
  if Result then LoadInternal;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromString(const str: string): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromString(str);
  if Result then LoadInternal;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetBestFontForFontCache(const svgFontInfo: TSVGFontInfo);
var
  bestFontReader: TFontReader;
  fi: TFontInfo;
begin
  fi.fontFamily := svgFontInfo.family;
  fi.faceName := ''; //just match to a family here, not to a specific facename
  fi.macStyles := [];
  if svgFontInfo.italic = sfsItalic then
    Include(fi.macStyles, msItalic);
  if svgFontInfo.weight >= 600 then
    Include(fi.macStyles, msBold);

  bestFontReader := FontLibrary.GetBestMatchedFont(fi);
  if not Assigned(bestFontReader) then Exit;

  if Assigned(fFontCache) then
    fFontCache.FontReader := bestFontReader else
    fFontCache := TGlyphCache.Create(bestFontReader, defaultFontHeight);

  fFontCache.Underlined := False;
  fFontCache.StrikeOut := False;
  case svgFontInfo.decoration of
    fdUnderline     : fFontCache.Underlined := true;
    fdStrikeThrough : fFontCache.StrikeOut := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.SetBlurQuality(quality: integer);
begin
  fBlurQuality := Max(0, Min(2, quality));
end;
//------------------------------------------------------------------------------

function TSvgReader.GetIsEmpty: Boolean;
begin
  Result := not Assigned(fRootElement);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
