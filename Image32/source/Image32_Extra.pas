unit Image32_Extra;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.25                                                            *
* Date      :  28 June 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Miscellaneous routines for TImage32 that                        *
*           :  don't obviously belong in other modules.                        *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Types,
  Image32, Image32_Draw, Image32_Vector;

type
  TButtonShape = (bsRound, bsSquare, bsDiamond);
  TButtonAttribute = (baShadow, ba3D);
  TButtonAttributes = set of TButtonAttribute;

procedure DrawShadow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; depth: double; angleRads: double = angle45;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;
procedure DrawShadow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; depth: double; angleRads: double = angle45;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;

procedure DrawGlow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;
procedure DrawGlow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;

//FloodFill: If no CompareFunc is provided, FloodFill will fill whereever
//adjoining pixels exactly match the starting pixel - Point(x,y).
procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  compareFunc: TCompareFunction = nil; tolerance: Integer = 0);

procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDev: integer; repeats: integer = 2);
procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);

//Emboss: A smaller radius is sharper. Increasing depth increases contrast.
//Luminance changes grayscale balance (unless preserveColor = true)
procedure Emboss(img: TImage32; radius: Integer = 1; depth: Integer = 10;
  luminance: Integer = 75; preserveColor: Boolean = false);

//Sharpen: Radius range is 1 - 10; amount range is 1 - 50.<br>
//see https://en.wikipedia.org/wiki/Unsharp_masking
procedure Sharpen(img: TImage32; radius: Integer = 2; amount: Integer = 10);

//HatchBackground: Assumes the current image is semi-transparent.
procedure HatchBackground(img: TImage32; color1: TColor32 = clWhite32;
  color2: TColor32= $FFE8E8E8; hatchSize: Integer = 10);

procedure GridBackground(img: TImage32; majorInterval, minorInterval: integer;
  fillColor: TColor32 = clWhite32;
  majColor: TColor32 = $30000000; minColor: TColor32 = $20000000);

procedure ReplaceColor(img: TImage32; oldColor, newColor: TColor32);

//EraseColor: Removes the specified color from the image, even from
//pixels that are a blend of colors including the specified color.<br>
//see https://stackoverflow.com/questions/9280902/
procedure EraseColor(img: TImage32; color: TColor32);

//RedEyeRemove: Removes 'red eye' from flash photo images.
procedure RedEyeRemove(img: TImage32; const rect: TRect);

procedure PencilEffect(img: TImage32; intensity: integer = 0);
procedure TraceContours(img: TImage32; intensity: integer);

procedure EraseInsidePath(img: TImage32;
  const path: TPathD; fillRule: TFillRule);
procedure EraseInsidePaths(img: TImage32;
  const paths: TPathsD; fillRule: TFillRule);

procedure EraseOutsidePath(img: TImage32; const path: TPathD;
  fillRule: TFillRule; const outsideBounds: TRect);
procedure EraseOutsidePaths(img: TImage32; const paths: TPathsD;
  fillRule: TFillRule; const outsideBounds: TRect);

procedure Draw3D(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle45); overload;
procedure Draw3D(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle45); overload;

function RainbowColor(fraction: double): TColor32;
function GradientColor(color1, color2: TColor32; frac: single): TColor32;
function MakeDarker(color: TColor32; percent: cardinal): TColor32;
function MakeLighter(color: TColor32; percent: cardinal): TColor32;

function DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32 = clNone32;
  buttonShape: TButtonShape = bsRound;
  buttonAttributes: TButtonAttributes = [baShadow, ba3D]): TPathD;

//Vectorize: convert an image into polygon vectors
function Vectorize(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; colorTolerance: Integer;
  roundingTolerance: integer = 2): TPathsD;

function VectorizeMask(const mask: TArrayOfByte;
  maskWidth: integer): TPathsD;

//RamerDouglasPeucker: simplifies paths, recursively removing vertices where
//they deviate no more than 'epsilon' from their adjacent vertices.
function RamerDouglasPeucker(const path: TPathD;
  epsilon: double): TPathD; overload;
function RamerDouglasPeucker(const paths: TPathsD;
  epsilon: double): TPathsD; overload;

//SmoothToBezier: this function is based on -
//"An Algorithm for Automatically Fitting Digitized Curves"
//by Philip J. Schneider in "Graphics Gems", Academic Press, 1990
function SmoothToBezier(const path: TPathD; closed: Boolean;
  tolerance: double; minSegLength: double = 2): TPathD; overload;
function SmoothToBezier(const paths: TPathsD; closed: Boolean;
  tolerance: double; minSegLength: double = 2): TPathsD; overload;

function GetFloodFillMask(img: TImage32; x, y: Integer;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;

procedure SymmetricCropTransparent(img: TImage32);

//3 additional blend functions (see TImage32.CopyBlend)
function BlendAverage(bgColor, fgColor: TColor32): TColor32;
function BlendLinearBurn(bgColor, fgColor: TColor32): TColor32;
function BlendColorDodge(bgColor, fgColor: TColor32): TColor32;

implementation

uses
  {$IFNDEF MSWINDOWS}
  Image32_FMX,
  {$ENDIF}
  Image32_Transform;

const
  FloodFillDefaultRGBTolerance: byte = 20;
  FloodFillDefaultHueTolerance: byte = 1;
  MaxBlur = 100;

type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;

  PWeightedColorArray = ^TWeightedColorArray;
  TWeightedColorArray = array [0.. $FFFFFF] of TWeightedColor;


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetSymmetricCropTransparentRect(img: TImage32): TRect;
var
  w,h, x,y, x1,y1: Integer;
  p1,p2: PARGB;
  opaquePxlFound: Boolean;
begin
  Result := img.Bounds;
  w := img.Width;
  y1 := 0;
  opaquePxlFound := false;
  for y := 0 to (img.Height div 2) -1 do
  begin
    p1 := PARGB(img.PixelRow[y]);
    p2 := PARGB(img.PixelRow[img.Height - y -1]);
    for x := 0 to w -1 do
    begin
      if (p1.A > 0) or (p2.A > 0) then
      begin
        y1 := y;
        opaquePxlFound := true;
        break;
      end;
      inc(p1); inc(p2);
    end;
    if opaquePxlFound then break;
  end;

  //probably safeset not to resize empty images
  if not opaquePxlFound then Exit;

  if y1 > 0 then
  begin
    inc(Result.Top, y1);
    dec(Result.Bottom, y1);
  end;

  x1 := 0;
  h := RectHeight(Result);
  opaquePxlFound := false;
  for x := 0 to (w div 2) -1 do
  begin
    p1 := PARGB(@img.Pixels[Result.Top * w + x]);
    p2 := PARGB(@img.Pixels[Result.Top * w + (w -1) - x]);
    for y := 0 to h -1 do
    begin
      if (p1.A > 0) or (p2.A > 0) then
      begin
        x1 := x;
        opaquePxlFound := true;
        break;
      end;
      inc(p1, w); inc(p2, w);
    end;
    if opaquePxlFound then break;
  end;

  if not opaquePxlFound then Exit;
  inc(Result.Left, x1);
  dec(Result.Right, x1);
end;
//------------------------------------------------------------------------------

//SymmetricCropTransparent: after cropping, the image's midpoint
//will be the same pixel as before cropping. (Important for rotating.)
procedure SymmetricCropTransparent(img: TImage32);
var
  rec: TRect;
begin
  rec := GetSymmetricCropTransparentRect(img);
  if (rec.Top > 0) or (rec.Left > 0) then img.Crop(rec);
end;
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  polygons: TPathsD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawShadow(img, polygons, fillRule, depth,
    angleRads, color, cutoutInsideShadow);
end;
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  x, y: double;
  blurSize: integer;
  rec: TRect;
  polys, shadowPolys: TPathsD;
  shadowImg: TImage32;
begin
  rec := GetBounds(polygons);
  if IsEmptyRect(rec) or (depth < 1) then Exit;
  if not ClockwiseRotationIsAnglePositive then angleRads := -angleRads;
  GetSinCos(angleRads, y, x);
  x := depth * x;
  y := depth * y;
  blurSize := Max(1,Round(depth / 4));
  rec := Image32_Vector.InflateRect(rec, Ceil(depth*2), Ceil(depth*2));
  polys := OffsetPath(polygons, -rec.Left, -rec.Top);
  shadowPolys := OffsetPath(polys, x, y);
  shadowImg := TImage32.Create(RectWidth(rec), RectHeight(rec));
  try
    DrawPolygon(shadowImg, shadowPolys, fillRule, color);
    FastGaussianBlur(shadowImg, shadowImg.Bounds, blurSize, 1);
    if cutoutInsideShadow then EraseInsidePaths(shadowImg, polys, fillRule);
    img.CopyBlend(shadowImg, shadowImg.Bounds, rec, BlendToAlpha);
  finally
    shadowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  polygons: TPathsD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawGlow(img, polygons, fillRule, color, blurRadius);
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  rec: TRect;
  glowPolys: TPathsD;
  glowImg: TImage32;
begin
  rec := GetBounds(polygons);
  glowPolys := OffsetPath(polygons,
    blurRadius -rec.Left +1, blurRadius -rec.Top +1);
  rec := Image32_Vector.InflateRect(rec, blurRadius +1, blurRadius +1);
  glowImg := TImage32.Create(RectWidth(rec), RectHeight(rec));
  try
    DrawPolygon(glowImg, glowPolys, fillRule, color);
    FastGaussianBlur(glowImg, glowImg.Bounds, blurRadius);
    glowImg.ScaleAlpha(4);
    img.CopyBlend(glowImg, glowImg.Bounds, rec, BlendToAlpha);
  finally
    glowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Sharpen(img: TImage32; radius: Integer; amount: Integer);
var
  i: Integer;
  amt: double;
  weightAmount: array [-255 .. 255] of Integer;
  bmpBlur: TImage32;
  pColor, pBlur: PARGB;
begin
  if radius = 0 then Exit;
  amt := ClampRange(amount/10, 0.1, 5);
  radius := ClampRange(radius, 1, 10);
  for i := -255 to 255 do
    weightAmount[i] := Round(amt * i);

  bmpBlur := TImage32.Create(img); //clone self
  try
    pColor := PARGB(img.pixelBase);
    FastGaussianBlur(bmpBlur, bmpBlur.Bounds, radius);
    pBlur := PARGB(bmpBlur.pixelBase);
    for i := 1 to img.Width * img.Height do
    begin
      if (pColor.A > 0) then
      begin
        pColor.R := ClampByte(pColor.R  + weightAmount[pColor.R - pBlur.R]);
        pColor.G := ClampByte(pColor.G  + weightAmount[pColor.G - pBlur.G]);
        pColor.B := ClampByte(pColor.B  + weightAmount[pColor.B - pBlur.B]);
      end;
      Inc(pColor); Inc(pBlur);
    end;
  finally
    bmpBlur.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure HatchBackground(img: TImage32;
  color1: TColor32; color2: TColor32; hatchSize: Integer);
var
  i,j: Integer;
  pc: PColor32;
  colors: array[boolean] of TColor32;
  hatch: Boolean;
begin
  colors[false] := color1;
  colors[true] := color2;
  img.BeginUpdate;
  try
    pc := img.Pixelbase;
    for i := 0 to img.Height -1 do
    begin
      hatch := Odd(i div hatchSize);
      for j := 0 to img.Width -1 do
      begin
        if (j + 1) mod hatchSize = 0 then hatch := not hatch;
        pc^ := BlendToOpaque(colors[hatch], pc^);
       inc(pc);
      end;
    end;
  finally
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure GridBackground(img: TImage32; majorInterval, minorInterval: integer;
  fillColor: TColor32; majColor: TColor32; minColor: TColor32);
var
  i, x,y, w,h: integer;
  path: TPathD;
begin
  img.Clear(fillColor);
  w := img.Width; h := img.Height;
  SetLength(path, 2);
  if minorInterval > 0 then
  begin
    x := minorInterval;
    path[0] := PointD(x, 0); path[1] := PointD(x, h);;
    for i := 1 to (w div minorInterval) do
    begin
      Image32_Draw.DrawLine(img, path, 1, minColor, esSquare);
      path[0].X := path[0].X + minorInterval;
      path[1].X := path[1].X + minorInterval;
    end;
    y := minorInterval;
    path[0] := PointD(0, y); path[1] := PointD(w, y);
    for i := 1 to (h div minorInterval) do
    begin
      Image32_Draw.DrawLine(img, path, 1, minColor, esSquare);
      path[0].Y := path[0].Y + minorInterval;
      path[1].Y := path[1].Y + minorInterval;
    end;
  end;
  if majorInterval > minorInterval then
  begin
    x := majorInterval;
    path[0] := PointD(x, 0); path[1] := PointD(x, h);;
    for i := 1 to (w div majorInterval) do
    begin
      Image32_Draw.DrawLine(img, path, 1, majColor, esSquare);
      path[0].X := path[0].X + majorInterval;
      path[1].X := path[1].X + majorInterval;
    end;
    y := majorInterval;
    path[0] := PointD(0, y); path[1] := PointD(w, y);
    for i := 1 to (h div majorInterval) do
    begin
      Image32_Draw.DrawLine(img, path, 1, majColor, esSquare);
      path[0].Y := path[0].Y + majorInterval;
      path[1].Y := path[1].Y + majorInterval;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ColorDifference(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := Abs(c1.R - c2.R) + Abs(c1.G - c2.G) + Abs(c1.B - c2.B);
end;
//------------------------------------------------------------------------------

procedure ReplaceColor(img: TImage32; oldColor, newColor: TColor32);
var
  color: PColor32;
  i: Integer;
begin
  color := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if color^ = oldColor then color^ := newColor;
    inc(color);
  end;
end;
//------------------------------------------------------------------------------

procedure EraseColor(img: TImage32; color: TColor32);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: Integer;
  Q: byte;
begin
  if fg.A = 0 then Exit;
  bg := PARGB(img.PixelBase);

  for i := 0 to img.Width * img.Height -1 do
  begin
    if bg.A > 0 then
    begin
      if (bg.R > fg.R) then Q := DivTable[bg.R - fg.R, not fg.R]
      else if (bg.R < fg.R) then Q := DivTable[fg.R - bg.R, fg.R]
      else Q := 0;
      if (bg.G > fg.G) then Q := Max(Q, DivTable[bg.G - fg.G, not fg.G])
      else if (bg.G < fg.G) then Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);
      if (bg.B > fg.B) then Q := Max(Q, DivTable[bg.B - fg.B, not fg.B])
      else if (bg.B < fg.B) then Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);
      if (Q > 0) then
      begin
        bg.A := MulTable[bg.A, Q];
        bg.R := DivTable[bg.R - MulTable[fg.R, not Q], Q];
        bg.G := DivTable[bg.G - MulTable[fg.G, not Q], Q];
        bg.B := DivTable[bg.B - MulTable[fg.B, not Q], Q];
      end else
        bg.Color := clNone32;
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure RedEyeRemove(img: TImage32; const rect: TRect);
var
  k: integer;
  cutout, mask: TImage32;
  path: TPathD;
  cutoutRec, rect3: TRect;
  radGrad: TRadialGradientRenderer;
begin
  k := RectWidth(rect) * RectHeight(rect);
  if k < 120 then k := 2
  else if k < 230 then k := 3
  else k := 4;
  cutoutRec := rect;
  Image32_Vector.InflateRect(cutoutRec, k, k);

  cutout  := TImage32.Create(img, cutoutRec);
  mask    := TImage32.Create(cutout.Width, cutout.Height);
  radGrad := TRadialGradientRenderer.Create;
  try
    //fill behind the cutout with black also
    //blurring the fill to soften its edges
    rect3 := cutout.Bounds;
    Image32_Vector.InflateRect(rect3, -k, -k);
    path := Ellipse(rect3);
    DrawPolygon(mask, path, frNonZero, clBlack32);
    //given the very small area and small radius of the blur, the
    //speed improvement of BoxBlur over GaussianBlur is inconsequential.
    GaussianBlur(mask, mask.Bounds, k);
    img.CopyBlend(mask, mask.Bounds, cutoutRec, BlendToOpaque);

    //gradient fill to clNone32 a mask to soften cutout's edges
    path := Ellipse(cutoutRec);
    radGrad.SetParameters(rect3, clBlack32, clNone32);
    DrawPolygon(mask, path, frNonZero, radGrad);
    cutout.CopyBlend(mask, mask.Bounds, cutout.Bounds, BlendMask);
    //now remove red from the cutout
    EraseColor(cutout, clRed32);
    //finally replace the cutout ...
    img.CopyBlend(cutout, cutout.Bounds, cutoutRec, BlendToOpaque);
  finally
    mask.Free;
    cutout.Free;
    radGrad.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure EraseInsidePath(img: TImage32; const path: TPathD; fillRule: TFillRule);
begin
  if assigned(path) then
    ErasePolygon(img, path, fillRule);
end;
//------------------------------------------------------------------------------

procedure EraseInsidePaths(img: TImage32; const paths: TPathsD; fillRule: TFillRule);
begin
  if assigned(paths) then
    ErasePolygon(img, paths, fillRule);
end;
//------------------------------------------------------------------------------

procedure EraseOutsidePath(img: TImage32; const path: TPathD;
  fillRule: TFillRule; const outsideBounds: TRect);
var
  mask: TImage32;
  p: TPathD;
begin
  if not assigned(path) then Exit;
  mask := TImage32.Create(RectWidth(outsideBounds), RectHeight(outsideBounds));
  try
    p := OffsetPath(path, -outsideBounds.Left, -outsideBounds.top);
    DrawPolygon(mask, p, fillRule, clBlack32);
    img.CopyBlend(mask, mask.Bounds, outsideBounds, BlendMask);
  finally
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure EraseOutsidePaths(img: TImage32; const paths: TPathsD;
  fillRule: TFillRule; const outsideBounds: TRect);
var
  mask: TImage32;
  pp: TPathsD;
begin
  if not assigned(paths) then Exit;
  mask := TImage32.Create(RectWidth(outsideBounds), RectHeight(outsideBounds));
  try
    pp := OffsetPath(paths, -outsideBounds.Left, -outsideBounds.top);
    DrawPolygon(mask, pp, fillRule, clBlack32);
    img.CopyBlend(mask, mask.Bounds, outsideBounds, BlendMask);
  finally
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  polygons: TPathsD;
begin
  setLength(polygons, 1);
  polygons[0] := polygon;
  Draw3D(img, polygons, fillRule, height, blurRadius, colorLt, colorDk, angleRads);
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  tmp: TImage32;
  recI: TRect;
  recD: TRectD;
  paths, paths2: TPathsD;
  x,y: double;
begin
  if not ClockwiseRotationIsAnglePositive then angleRads := -angleRads;
  GetSinCos(angleRads, y, x);
  recD := GetBoundsD(polygons);
  if recD.IsEmpty then Exit;
  recI := Rect(recD);
  paths := OffsetPath(polygons, -recI.Left, -recI.Top);
  tmp := TImage32.Create(rectWidth(recI), rectHeight(recI));
  try
    if colorLt shr 24 > 0 then
    begin
      tmp.Clear(colorLt);
      paths2 := OffsetPath(paths, height*x, height*y);
      EraseInsidePaths(tmp, paths2, fillRule);
      FastGaussianBlur(tmp, tmp.Bounds, Round(blurRadius), 0);
      EraseOutsidePaths(tmp, paths, fillRule, tmp.Bounds);
      img.CopyBlend(tmp, tmp.Bounds, recI, BlendToAlpha);
    end;

    if colorDk shr 24 > 0 then
    begin
      tmp.Clear(colorDk);
      paths2 := OffsetPath(paths, -height*x, -height*y);
      EraseInsidePaths(tmp, paths2, fillRule);
      FastGaussianBlur(tmp, tmp.Bounds, Round(blurRadius), 0);
      EraseOutsidePaths(tmp, paths, fillRule, tmp.Bounds);
      img.CopyBlend(tmp, tmp.Bounds, recI, BlendToAlpha);
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

function RainbowColor(fraction: double): TColor32;
begin
  if (fraction >= 1) or (fraction <= 0) then
    result := clRed32
  else
  begin
    fraction := fraction * 6;
    case trunc(fraction) of
      0: result := GradientColor(clRed32, clYellow32, frac(fraction));
      1: result := GradientColor(clYellow32, clLime32, frac(fraction));
      2: result := GradientColor(clLime32, clAqua32, frac(fraction));
      3: result := GradientColor(clAqua32, clBlue32, frac(fraction));
      4: result := GradientColor(clBlue32, clFuchsia32, frac(fraction));
      else result := GradientColor(clFuchsia32, clRed32, frac(fraction));
    end;
  end;
end;
//------------------------------------------------------------------------------

function GradientColor(color1, color2: TColor32; frac: single): TColor32;
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
  r:  TARGB absolute Result;
begin
  if frac >= 1 then
    result := color2
  else if frac <= 0 then
    result := color1
  else
  begin
    r.B := trunc(c1.B*(1-frac) + c2.B*frac);
    r.G := trunc(c1.G*(1-frac) + c2.G*frac);
    r.R := trunc(c1.R*(1-frac) + c2.R*frac);
    r.A := trunc(c1.A*(1-frac) + c2.A*frac);
  end;
end;
//------------------------------------------------------------------------------

function MakeDarker(color: TColor32; percent: cardinal): TColor32;
begin
  result := GradientColor(color, $FF000000, percent/100);
end;
//------------------------------------------------------------------------------

function MakeLighter(color: TColor32; percent: cardinal): TColor32;
begin
  result := GradientColor(color, $FFFFFFFF, percent/100);
end;
//------------------------------------------------------------------------------

function DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32; buttonShape: TButtonShape;
  buttonAttributes: TButtonAttributes): TPathD;
var
  i: integer;
  radius: double;
  rec: TRectD;
  shadowSize, shadowAngle: double;
begin
  img.Clear;
  if (size < 5) then Exit;
  radius := size * 0.5;
  shadowSize := radius * 0.25;

  rec := RectD(pt.X -radius, pt.Y -radius, pt.X +radius, pt.Y +radius);

  case buttonShape of
    bsDiamond:
      begin
        SetLength(Result, 4);
        for i := 0 to 3 do Result[i] := pt;
        Result[0].X := Result[0].X -radius;
        Result[1].Y := Result[1].Y -radius;
        Result[2].X := Result[2].X +radius;
        Result[3].Y := Result[3].Y +radius;
      end;
    bsSquare:
      begin
        rec := InflateRect(rec, -1,-1);
        Result := Rectangle(rec);
      end;
    else
      Result := Ellipse(rec);
  end;
  shadowAngle := angle45;

  img.BeginUpdate;
  try

    //nb: only need to cutout the inside shadow if
    //the pending color fill is semi-transparent
    if baShadow in buttonAttributes then
      DrawShadow(img, Result, frNonZero, shadowSize,
        shadowAngle, $AA000000, color shr 24 < 254);

    if color shr 24 > 2 then
      DrawPolygon(img, Result, frNonZero, color);

    if ba3D in buttonAttributes then
      Draw3D(img, Result, frNonZero, shadowSize*2,
        Ceil(shadowSize), $CCFFFFFF, $AA000000, shadowAngle);
    DrawLine(img, Result, DpiAwareI, clBlack32, esPolygon);
  finally
    img.EndUpdate;
  end;

end;
//------------------------------------------------------------------------------

function AlphaAverage(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := (c1.A + c2.A) shr 1;
end;
//------------------------------------------------------------------------------

function BlendAverage(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := (fg.A + bg.A) shr 1;
  res.R := (fg.R + bg.R) shr 1;
  res.G := (fg.G + bg.G) shr 1;
  res.B := (fg.B + bg.B) shr 1;
end;
//------------------------------------------------------------------------------

function BlendLinearBurn(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := Max(0, bg.R + fg.R - 255);
  res.G := Max(0, bg.G + fg.G - 255);
  res.B := Max(0, bg.B + fg.B - 255);
end;
//------------------------------------------------------------------------------

function BlendColorDodge(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := DivTable[bg.R, not fg.R];
  res.G := DivTable[bg.G, not fg.G];
  res.B := DivTable[bg.B, not fg.B];
end;
//------------------------------------------------------------------------------

procedure PencilEffect(img: TImage32; intensity: integer);
var
  w,h: integer;
  img2: TImage32;
begin
  w := img.Width; h := img.Height;
  if w * h = 0 then Exit;

  intensity := max(1, min(10, intensity));
  img.Grayscale;
  img2 := TImage32.Create(img);
  try
    img2.InvertColors;
    FastGaussianBlur(img2, img2.Bounds, intensity);
    img.CopyBlend(img2, img2.Bounds, img.Bounds, BlendColorDodge);
  finally
    img2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TraceContours(img: TImage32; intensity: integer);
var
  i,j, w,h: integer;
  tmp, tmp2: TArrayOfColor32;
  s: PColor32;
  d: PARGB;
begin
  w := img.Width; h := img.Height;
  if w * h = 0 then Exit;
  SetLength(tmp, w * h);
  SetLength(tmp2, w * h);
  s := img.PixelRow[0]; d := @tmp[0];
  for j := 0 to h-1 do
  begin
    for i := 0 to w-2 do
    begin
      d.A := Min($FF, ColorDifference(s^, IncPColor32(s, 1)^));
      inc(s); inc(d);
    end;
    inc(s); inc(d);
  end;

  for j := 0 to w-1 do
  begin
    s := @tmp[j]; d := @tmp2[j];
    for i := 0 to h-2 do
    begin
      d.A := Min($FF, AlphaAverage(s^, IncPColor32(s, w)^));
      inc(s, w); inc(d, w);
    end;
  end;
  Move(tmp2[0], img.PixelBase^, w * h * sizeOf(TColor32));

  if intensity < 1 then Exit;
  if intensity > 10 then
    intensity := 10; //range = 1-10
  img.ScaleAlpha(intensity);
end;

//------------------------------------------------------------------------------
// FloodFill - and support functions
//------------------------------------------------------------------------------

type
  PFloodFillRec = ^TFloodFillRec;
  TFloodFillRec = record
    xLeft     : Integer;
    xRight    : Integer;
    y         : Integer;
    dirY      : Integer;
    next      : PFloodFillRec;
  end;

  TFloodFillStack = class
    first     : PFloodFillRec;
    maxY      : Integer;
    constructor Create(maxY: Integer);
    destructor Destroy; override;
    procedure Push(xLeft, xRight,y, direction: Integer);
    procedure Pop(out xLeft, xRight,y, direction: Integer);
    function IsEmpty: Boolean;
  end;

  TFloodFillMask = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    mask         : TArrayOfByte;
    width        : Integer;
    height       : Integer;
    tolerance    : Integer;
    initialColor : TColor32;
    colorsBase   : PColor32Array;
    colorsRow    : PColor32Array;
    maskRow      : PByteArray;
    compareFunc  : TCompareFunction;
    procedure Reset(w, h, x, y: Integer; pixelBase: PColor32;
      compFunc: TCompareFunction; aTolerance: Integer = 0);
    procedure SetCurrentY(y: Integer);
    function IsMatch(x: Integer): Boolean;
  end;

  //----------------------------
  // TFloodFillStack methods
  //----------------------------

constructor TFloodFillStack.Create(maxY: Integer);
begin
  self.maxY := maxY;
end;
//------------------------------------------------------------------------------

destructor TFloodFillStack.Destroy;
var
  ffr: PFloodFillRec;
begin
  while assigned(first) do
  begin
    ffr := first;
    first := first.next;
    dispose(ffr);
  end;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Push(xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  if ((y = 0) and (direction = -1)) or
    ((y = maxY) and (direction = 1)) then Exit;
  new(ffr);
  ffr.xLeft  := xLeft;
  ffr.xRight := xRight;
  ffr.y      := y;
  ffr.dirY   := direction;
  ffr.next   := first;
  first      := ffr;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Pop(out xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  xLeft     := first.xLeft;
  xRight    := first.xRight;
  direction := first.dirY;
  y         := first.y + direction;
  ffr := first;
  first := first.next;
  dispose(ffr);
end;
//------------------------------------------------------------------------------

function TFloodFillStack.IsEmpty: Boolean;
begin
  result := not assigned(first);
end;

  //----------------------------
  // TFloodFillMask methods
  //----------------------------

procedure TFloodFillMask.Reset(w, h, x, y: Integer;
  pixelBase: PColor32; compFunc: TCompareFunction; aTolerance: Integer);
begin
   mask := nil; //clear a existing mask

   //create a mask the size of the image
   setLength(mask, w * h);
   Self.width := w;
   Self.height := h;
   colorsBase := PColor32Array(pixelBase);
   Self.initialColor := colorsBase[x + y * w];
   Self.compareFunc := compFunc;
   Self.tolerance := aTolerance;
   //Self.colorsRow and Self.maskRow are left undefined here
end;
//------------------------------------------------------------------------------

procedure TFloodFillMask.SetCurrentY(y: Integer);
begin
  colorsRow := @colorsBase[y * width];
  maskRow := @mask[y * width];
end;
//------------------------------------------------------------------------------

function TFloodFillMask.IsMatch(x: Integer): Boolean;
begin
  result := (maskRow[x] = 0) and
    compareFunc(initialColor, colorsRow[x], tolerance);
  if result then maskRow[x] := 1;
end;
//------------------------------------------------------------------------------

function GetFloodFillMask(img: TImage32; x, y: Integer;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;
var
  xl, xr, xr2, dirY: Integer;
  maxX, maxY: Integer;
  ffs: TFloodFillStack;
  ffm: TFloodFillMask;
begin
  result := nil;
  if (x < 0) or (x >= img.Width) or (y < 0) or (y >= img.Height) then
    Exit;
  maxX := img.Width -1;
  maxY := img.Height -1;

  if not Assigned(compareFunc) then compareFunc := CompareRGB;

  ffs := TFloodFillStack.create(maxY);
  try
    xl := x; xr := x;
    ffm.Reset(img.Width, img.Height, x, y,
      img.PixelBase, compareFunc, tolerance);
    ffm.SetCurrentY(y);
    ffm.IsMatch(x);

    while (xl > 0) and ffm.IsMatch(xl -1) do dec(xl);
    while (xr < maxX) and ffm.IsMatch(xr +1) do inc(xr);
    ffs.Push(xl, xr, y, -1); //down
    ffs.Push(xl, xr, y, 1);  //up
    while not ffs.IsEmpty do
    begin
      ffs.Pop(xl, xr, y, dirY);
      ffm.SetCurrentY(y);
      xr2 := xl;
      //check left ...
      if ffm.IsMatch(xl) then
      begin
        while (xl > 0) and ffm.IsMatch(xl-1) do dec(xl);
        if xl <= xr2 -2 then
          ffs.Push(xl, xr2-2, y, -dirY);
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, dirY);
        if xr2 >= xr +2 then
          ffs.Push(xr+2, xr2, y, -dirY);
        xl := xr2 +2;
      end;
      //check right ...
      while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      while (xl <= xr) do
      begin
        xr2 := xl;
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, dirY);
        if xr2 >= xr +2 then
        begin
          ffs.Push(xr+2, xr2, y, -dirY);
          break;
        end;
        inc(xl, 2);
        while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      end;
    end;
    result := ffm.mask;
  finally
    ffs.Free;
  end;
end;
//------------------------------------------------------------------------------

function GetFloodFillBounds(img: TImage32; x,y: Integer;
  const byteArray: TArrayOfByte): TRect;

  function RowHasFill(i: Integer): Boolean;
  var
    pb, pEnd: PByte;
  begin
    result := true;
    pb := @byteArray[i * img.Width];
    pEnd := pb + img.Width;
    while (pb < pEnd) do
      if Ord(pb^) = 1 then Exit
      else inc(pb);
    result := false;
  end;

  function ColHasFill(i: Integer): Boolean;
  var
    pb, pEnd: PByte;
  begin
    result := true;
    pb := @byteArray[i];
    pEnd := @byteArray[length(byteArray)-1];
    while (pb < pEnd) do
      if Ord(pb^) > 0 then Exit
      else inc(pb, img.Width);
    result := false;
  end;

begin
  Result := Types.Rect(x,y, x,y);
  while (Result.Top > 0) and RowHasFill(Result.Top -1) do
    dec(Result.Top);
  while (Result.Bottom < img.Height -1) and RowHasFill(Result.Bottom) do
    inc(Result.Bottom);
  while (Result.Left > 0) and ColHasFill(Result.Left -1) do
    dec(Result.Left);
  while (Result.Right < img.Width -1) and ColHasFill(Result.Right) do
    inc(Result.Right);
end;
//------------------------------------------------------------------------------

procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer);
var
  i: Integer;
  ba: TArrayOfByte;
  pb: PByte;
  pc: PColor32;
begin
  if not assigned(compareFunc) then
  begin
    compareFunc := CompareRGB;
    tolerance := FloodFillDefaultRGBTolerance;
  end;

  if (tolerance < 0) then
  begin
    if Addr(compareFunc) = Addr(CompareRGB) then
      tolerance := FloodFillDefaultRGBTolerance
    else if Addr(compareFunc) = Addr(CompareHue) then
      tolerance := FloodFillDefaultHueTolerance;
  end;

  ba := GetFloodFillMask(img, x, y, compareFunc, tolerance);
  if ba = nil then Exit;
  pb := @ba[0];
  pc := img.PixelBase;
  for i := 0 to High(ba) do
  begin
    if Ord(pb^) > 0 then pc^ := newColor;
    inc(pb); inc(pc);
  end;
end;

//------------------------------------------------------------------------------
// Emboss - and support functions
//------------------------------------------------------------------------------

function IncPWeightColor(pwc: PWeightedColor; cnt: Integer): PWeightedColor;
begin
  result := PWeightedColor(PByte(pwc) + cnt * SizeOf(TWeightedColor));
end;
//------------------------------------------------------------------------------

function Intensity(color: TColor32): byte;
var
  c: TARGB absolute color;
begin
  Result := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
end;
//------------------------------------------------------------------------------

function Gray(color: TColor32): TColor32;
var
  c: TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.A := c.A;
  res.R := Intensity(color);
  res.G := res.R;
  res.B := res.R;
end;
//------------------------------------------------------------------------------

procedure Emboss(img: TImage32; radius: Integer;
  depth: Integer; luminance: Integer; preserveColor: Boolean);
var
  yy,xx, x,y, w,h: Integer;
  b: byte;
  kernel: array [0 .. MaxBlur, 0 .. MaxBlur] of Integer;
  wca: TArrayOfWeightedColor;
  pc0, pcf, pcb: PColor32; //pointers to pixels (forward & backward in kernel)
  pw0, pw: PWeightedColor; //pointers to weight
  customGray: TColor32;
  pc: PColor32;
const
  maxDepth = 50;
begin
  //grayscale luminance as percent where 0% is black and 100% is white
  //(luminance is ignored when preserveColor = true)
  luminance := ClampRange(luminance, 0, 100);
  b := luminance *255 div 100;
  customGray := $FF000000 + b shl 16 + b shl 8 + b;

  ClampRange(radius, 1, 5);
  inc(depth);
  ClampRange(depth, 2, maxDepth);

  kernel[0][0] := 1;
  for y := 1 to radius do
    for x := 1 to radius do
      kernel[y][x] := depth;

  w := img.Width; h := img.Height;
  //nb: dynamic arrays are zero-initialized (unless they're a function result)
  SetLength(wca, w * h);

  pc0 := IncPColor32(img.PixelBase, radius * w);
  pw0 := @wca[radius * w];
  for y := radius to h -1 - radius do
  begin
    for x := radius to w -1 - radius do
    begin
      pw := IncPWeightColor(pw0, x);
      pcb := IncPColor32(pc0, x - 1);
      if preserveColor then
      begin
        pcf := IncPColor32(pc0, x);
        pw^.Add(pcf^, kernel[0,0]);
        inc(pcf);
      end else
      begin
        pw^.Add(customGray, kernel[0,0]);
        pcf := IncPColor32(pc0, x + 1);
      end;

      //parse the kernel ...
      for yy := 1 to radius do
      begin
        for xx := 1 to radius do
        begin
          pw^.Subtract(Gray(pcf^), kernel[yy,xx]);
          pw^.Add(Gray(pcb^), kernel[yy,xx]);
          dec(pcb); inc(pcf);
        end;
        dec(pcb, img.Width - radius);
        inc(pcf, img.Width - radius);
      end;
    end;
    inc(pc0, img.Width);
    inc(pw0, img.Width);
  end;

  pc := @img.Pixels[0]; pw := @wca[0];
  for x := 0 to img.width * img.Height - 1 do
  begin
    pc^ := pw.Color or $FF000000;
    inc(pc); inc(pw);
  end;
end;

//------------------------------------------------------------------------------
// Structure and functions used by the Vectorize routine
//------------------------------------------------------------------------------

type
  TPt2Container = class;

  TPt2 = class
    pt         : TPointD;
    owner      : TPt2Container;
    isStart    : Boolean;
    isHole     : Boolean;
    nextInPath : TPt2;
    prevInPath : TPt2;
    nextInRow  : TPt2;
    prevInRow  : TPt2;
    destructor Destroy; override;
    procedure Update(x, y: double);
    function GetCount: integer;
    function GetPoints: TPathD;
    property IsAscending: Boolean read isStart;
  end;

  TPt2Container = class
    prevRight: integer;
    leftMostPt, rightMost: TPt2;
    solution: TPathsD;
    procedure AddToSolution(const path: TPathD);
    function StartNewPath(insertBefore: TPt2;
      xLeft, xRight, y: integer; isHole: Boolean): TPt2;
    procedure AddRange(var current: TPt2; xLeft, xRight, y: integer);
    function JoinAscDesc(path1, path2: TPt2): TPt2;
    function JoinDescAsc(path1, path2: TPt2): TPt2;
    procedure CheckRowEnds(pt2Left, pt2Right: TPt2);
  end;

//------------------------------------------------------------------------------

destructor TPt2.Destroy;
var
  startPt, endPt, pt: TPt2;
begin
  if not isStart then Exit;
  startPt := self;
  endPt := startPt.prevInPath;

  //remove 'endPt' from double linked list
  if endPt = owner.rightMost then
    owner.rightMost := endPt.prevInRow
  else if assigned(endPt.nextInRow) then
    endPt.nextInRow.prevInRow := endPt.prevInRow;
  if endPt = owner.leftMostPt then
    owner.leftMostPt := endPt.nextInRow
  else if assigned(endPt.prevInRow) then
    endPt.prevInRow.nextInRow := endPt.nextInRow;

  //remove 'startPt' from double linked list
  if startPt = owner.leftMostPt then
    owner.leftMostPt := startPt.nextInRow
  else if assigned(startPt.prevInRow) then
    startPt.prevInRow.nextInRow := startPt.nextInRow;
  if assigned(startPt.nextInRow) then
    startPt.nextInRow.prevInRow := startPt.prevInRow;

  owner.AddToSolution(GetPoints);

  //now Free the entire path (except self)
  pt := startPt.nextInPath;
  while pt <> startPt do
  begin
    endPt := pt;
    pt := pt.nextInPath;
    endPt.Free;
  end;
end;
//------------------------------------------------------------------------------

function IsColinear(const pt1, pt2, pt3: TPoint): Boolean; overload;
begin
  //cross product = 0
  result := (pt1.X - pt2.X)*(pt2.Y - pt3.Y) = (pt2.X - pt3.X)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function IsColinear(const pt1, pt2, pt3, pt4: TPoint): Boolean; overload;
begin
  result := (pt1.X - pt2.X)*(pt3.Y - pt4.Y) = (pt3.X - pt4.X)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function CreatePt2After(pt: TPt2; const p: TPointD): TPt2;
begin
  Result := TPt2.Create;
  Result.pt := p;
  Result.nextInPath := pt.nextInPath;
  Result.prevInPath := pt;
  pt.nextInPath.prevInPath := Result;
  pt.nextInPath := Result;
end;
//------------------------------------------------------------------------------

procedure TPt2.Update(x, y: double);
var
  newPt2: TPt2;
begin

  if isStart then
  begin
    //just update self.pt when colinear
    if (x = pt.X) and (pt.X = nextInPath.pt.X) then
    begin
      pt := PointD(x,y);
      Exit;
    end;

    //self -> 2 -> 1 -> nip
    CreatePt2After(self, pt);
    if (x <> pt.X) or (x <> nextInPath.pt.X) then
    begin
      //add a pixel either below or beside
      if IsAscending then
        CreatePt2After(self, PointD(pt.X, y)) else
        CreatePt2After(self, PointD(x, pt.Y));
    end;
    pt := PointD(x,y);

  end else
  begin
    //just update self.pt when colinear
    if (x = pt.X) and (pt.X = prevInPath.pt.X) then
    begin
      pt := PointD(x,y);
      Exit;
    end;

    //self <- 2 <- 1 <- pip
    newPt2 := CreatePt2After(prevInPath, pt);
    if (x <> pt.X) or (x <> prevInPath.pt.X) then
    begin
      //add a pixel either below or beside
      if IsAscending then
        CreatePt2After(newPt2, PointD(x, pt.Y)) else
        CreatePt2After(newPt2, PointD(pt.X, y));
    end;
    pt := PointD(x,y);
  end;
end;
//------------------------------------------------------------------------------

function TPt2.GetCount: integer;
var
  pt2: TPt2;
begin
  result := 1;
  pt2 := nextInPath;
  while pt2 <> self do
  begin
    inc(Result);
    pt2 := pt2.nextInPath;
  end;
end;
//------------------------------------------------------------------------------

function TPt2.GetPoints: TPathD;
var
  i, count: integer;
  pt2: TPt2;
begin
  Update(pt.X, pt.Y+1);
  with prevInPath do Update(pt.X, pt.Y+1); //path 'end'

  count := GetCount;
  SetLength(Result, count);
  pt2 := self;
  for i := 0 to count -1 do
  begin
    Result[i] := pt2.pt;
    pt2 := pt2.nextInPath;
  end;
end;
//------------------------------------------------------------------------------

procedure TPt2Container.AddToSolution(const path: TPathD);
var
  len: integer;
begin
  if Length(path) < 2 then Exit;
  len := Length(solution);
  SetLength(solution, len + 1);
  solution[len] := path;
end;
//------------------------------------------------------------------------------

function TPt2Container.StartNewPath(insertBefore: TPt2;
  xLeft, xRight, y: integer; isHole: Boolean): TPt2;
var
  pt2Left, pt2Right: TPt2;
begin
  inc(xRight);
  pt2Left := TPt2.Create;
  pt2Left.owner := self;
  pt2Left.isStart := not isHole;
  pt2Left.isHole := isHole;
  pt2Left.pt := PointD(xLeft, y);

  pt2Right := TPt2.Create;
  pt2Right.owner := self;
  pt2Right.isStart := isHole;
  pt2Right.isHole := isHole;
  pt2Right.pt := PointD(xRight, y);

  pt2Left.nextInPath := pt2Right;
  pt2Left.prevInPath := pt2Right;
  pt2Right.nextInPath := pt2Left;
  pt2Right.prevInPath := pt2Left;

  pt2Left.nextInRow := pt2Right;
  pt2Right.prevInRow := pt2Left;
  if not Assigned(insertBefore) then
  begin
    //must be a new rightMost path
    pt2Left.prevInRow := rightMost;
    if Assigned(rightMost) then rightMost.nextInRow := pt2Left;
    pt2Right.nextInRow := nil;
    rightMost := pt2Right;
    if not Assigned(leftMostPt) then leftMostPt := pt2Left;
  end else
  begin
    pt2Right.nextInRow := insertBefore;
    if leftMostPt = insertBefore then
    begin
      //must be a new leftMostPt path
      leftMostPt := pt2Left;
      pt2Left.prevInRow := nil;
    end else
    begin
      pt2Left.prevInRow := insertBefore.prevInRow;
      insertBefore.prevInRow.nextInRow := pt2Left;
    end;
    insertBefore.prevInRow := pt2Right;
  end;
  result := pt2Right.nextInRow;
end;
//------------------------------------------------------------------------------

procedure TPt2Container.CheckRowEnds(pt2Left, pt2Right: TPt2);
begin
  if pt2Left = leftMostPt then leftMostPt := pt2Right.nextInRow;
  if pt2Right = rightMost then rightMost := pt2Left.prevInRow;
end;
//------------------------------------------------------------------------------

function TPt2Container.JoinAscDesc(path1, path2: TPt2): TPt2;
begin
  result := path2.nextInRow;
  CheckRowEnds(path1, path2);
  if path2 = path1.prevInPath then
  begin
    path1.Free;
    Exit;
  end;

  with path1 do Update(pt.X, pt.Y+1);
  with path2 do Update(pt.X, pt.Y+1);
  path1.isStart := false;
  //remove path1 from double linked list
  if assigned(path1.nextInRow) then
    path1.nextInRow.prevInRow := path1.prevInRow;
  if assigned(path1.prevInRow) then
    path1.prevInRow.nextInRow := path1.nextInRow;
  //remove path2 from double linked list
  if assigned(path2.nextInRow) then
    path2.nextInRow.prevInRow := path2.prevInRow;
  if assigned(path2.prevInRow) then
    path2.prevInRow.nextInRow := path2.nextInRow;

  path1.prevInPath.nextInPath := path2.nextInPath;
  path2.nextInPath.prevInPath := path1.prevInPath;
  path2.nextInPath := path1;
  path1.prevInPath := path2;
end;
//------------------------------------------------------------------------------

function TPt2Container.JoinDescAsc(path1, path2: TPt2): TPt2;
begin
  result := path2.nextInRow;
  CheckRowEnds(path1, path2);
  if path1 = path2.prevInPath then
  begin
    path2.Free;
    Exit;
  end;

  with path1 do Update(pt.X, pt.Y+1);
  with path2 do Update(pt.X, pt.Y+1);
  path2.isStart := false;
  //remove path1 'end' from double linked list
  if assigned(path1.nextInRow) then
    path1.nextInRow.prevInRow := path1.prevInRow;
  if assigned(path1.prevInRow) then
    path1.prevInRow.nextInRow := path1.nextInRow;
  //remove path2 'start' from double linked list
  if assigned(path2.nextInRow) then
    path2.nextInRow.prevInRow := path2.prevInRow;
  if assigned(path2.prevInRow) then
    path2.prevInRow.nextInRow := path2.nextInRow;

  path1.nextInPath.prevInPath := path2.prevInPath;
  path2.prevInPath.nextInPath := path1.nextInPath;
  path1.nextInPath := path2;
  path2.prevInPath := path1;
end;
//------------------------------------------------------------------------------

function IsHeadingLeft(current: TPt2; r: integer): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := r <= current.pt.X;
end;
//------------------------------------------------------------------------------

procedure TPt2Container.AddRange(var current: TPt2;
  xLeft, xRight, y: integer);
begin
  if (prevRight > 0) then
  begin
    //nb: prevRight always ends a range (whether a hole or an outer)

    //check if we're about to start a hole
    if xLeft < current.pt.X then
    begin
      //'current' must be descending and hence prevRight->xLeft a hole
      current := StartNewPath(current, prevRight, xLeft -1, y, true);
      prevRight := xRight;
      Exit; //nb: it's possible for multiple holes
    end;

    //check if we're passing under a pending join
    while assigned(current) and assigned(current.nextInRow) and
      (prevRight > current.nextInRow.pt.X) do
    begin
      //Assert(not current.IsAscending, 'oops!');
      //Assert(current.nextInRow.IsAscending, 'oops!');
      current := JoinDescAsc(current, current.nextInRow);
    end;

    //check again for a new hole
    if (xLeft < current.pt.X) then
    begin
      current := StartNewPath(current, prevRight, xLeft -1, y, true);
      prevRight := xRight;
      Exit;
    end;

    current.Update(prevRight, y);
    current := current.nextInRow;
    prevRight := 0;
  end;

  //check if we're passing under a pending join
  while assigned(current) and assigned(current.nextInRow) and
    (xLeft > current.nextInRow.pt.X) do
      current := JoinAscDesc(current, current.nextInRow);

  if not assigned(current) or (xRight < current.pt.X) then
  begin
    StartNewPath(current, xLeft, xRight -1, y, false);
    //nb: current remains unchanged
  end else
  begin
    //'range' must somewhat overlap one or more paths above
    if IsHeadingLeft(current, xRight) then
    begin
      if current.isHole then
      begin
        current.Update(xLeft, y);
        current := current.nextInRow;
      end;

      current.Update(xRight, y);
      current.Update(xLeft, y);
      if current.IsAscending then
        prevRight := xRight else
        prevRight := 0;
      current := current.nextInRow;
    end else
    begin
      current.Update(xLeft, y);
      current := current.nextInRow;
      prevRight := xRight;
    end;
  end
end;
//------------------------------------------------------------------------------

function VectorizeMask(const mask: TArrayOfByte;
  maskWidth: integer): TPathsD;
var
  i,j, len, height, blockStart: integer;
  current: TPt2;
  ba: PByteArray;
  pt2Container: TPt2Container;
begin
  Result := nil;
  len := Length(mask);
  if (len = 0) or (maskWidth = 0) or (len mod maskWidth <> 0) then Exit;
  height := len div maskWidth;

  pt2Container := TPt2Container.Create;
  try
    for i := 0 to height -1 do
    begin
      ba := @mask[maskWidth * i];
      blockStart := -2;
      current := pt2Container.leftMostPt;
      for j := 0 to maskWidth -1 do
      begin
        if (ba[j] > 0) = (blockStart >= 0) then Continue;
        if blockStart >= 0 then
        begin
          pt2Container.AddRange(current, blockStart, j, i);
          blockStart := -1;
        end else
          blockStart := j;
      end;

      if blockStart >= 0 then
        pt2Container.AddRange(current, blockStart, maskWidth, i);

      if (pt2Container.prevRight > 0) then
      begin
        while Assigned(current.nextInRow) and
          (pt2Container.prevRight >= current.nextInRow.pt.X) do
        begin
          if current.isStart then
            current := pt2Container.JoinAscDesc(current, current.nextInRow)
          else
            current := pt2Container.JoinDescAsc(current, current.nextInRow);
        end;
        current.Update(pt2Container.prevRight, i);
        current := current.nextInRow;
        pt2Container.prevRight := 0;
      end;

      while assigned(current) do
      begin
        if current.isStart then
          current := pt2Container.JoinAscDesc(current, current.nextInRow) else
          current := pt2Container.JoinDescAsc(current, current.nextInRow);
      end
    end;

    with pt2Container do
      while Assigned(leftMostPt) do
        if leftMostPt.isStart then
          JoinAscDesc(leftMostPt, leftMostPt.nextInRow) else
          JoinDescAsc(leftMostPt, leftMostPt.nextInRow);

    Result := pt2Container.solution;
  finally
    pt2Container.Free;
  end;
end;
//------------------------------------------------------------------------------

function Tidy(const poly: TPathD; tolerance: integer): TPathD;
var
  i,j, highI: integer;
  prev: TPointD;
  tolSqrd: double;
begin
  highI := High(poly);
  while  (HighI >= 0) and PointsEqual(poly[highI], poly[0]) do dec(highI);
  if highI < 1 then
  begin
    Result := nil;
    Exit;
  end;
  tolSqrd := Sqr(Max(2.02, Min(16.1, tolerance + 0.01)));
  SetLength(Result, highI +1);
  prev := poly[highI];
  Result[0] := prev;
  Result[1] := poly[0];
  j := 1;
  for i := 1 to highI -1 do
  begin
    if ((DistanceSqrd(prev, Result[j]) > tolSqrd) and
        (DistanceSqrd(Result[j], poly[i]) > tolSqrd)) or
      (TurnsRight(prev, result[j], poly[i]) or
        TurnsLeft(result[j], poly[i], poly[i+1])) then
    begin
      prev := result[j];
      inc(j);
    end;
    result[j] := poly[i];
  end;
  if ((DistanceSqrd(prev, Result[j]) > tolSqrd) and
    (DistanceSqrd(Result[j], Result[0]) > tolSqrd)) or
    TurnsRight(prev, result[j], Result[0]) or
    TurnsLeft(result[j], Result[0], Result[1]) then
      SetLength(Result, j +1) else
      SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function Vectorize(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; colorTolerance: Integer;
  roundingTolerance: integer): TPathsD;
var
  i: integer;
  mask: TArrayOfByte;
begin
  mask := GetBoolMask(img, compareColor, compareFunc, colorTolerance);
  Result := VectorizeMask(mask, img.Width);
  for i := 0 to high(Result) do
    Result[i] := Tidy(Result[i], roundingTolerance);
end;

//------------------------------------------------------------------------------
// RamerDouglasPeucker - and support functions
//------------------------------------------------------------------------------

function PerpendicularDistSqrd(const pt, line1, line2: TPointD): double;
var
  a,b,c,d: double;
begin
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  if (c = 0) and (d = 0) then
    result := 0 else
    result := Sqr(a * d - c * b) / (c * c + d * d);
end;
//------------------------------------------------------------------------------

procedure RDP(const path: TPathD; startIdx, endIdx: integer;
  epsilonSqrd: double; const flags: TArrayOfInteger);
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
  for i := startIdx +1 to endIdx -1 do
  begin
    //PerpendicularDistSqrd - avoids expensive Sqrt()
    d := PerpendicularDistSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  flags[idx] := 1;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, flags);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, flags);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPathD;
  epsilon: double): TPathD;
var
  i,j, len: integer;
  buffer: TArrayOfInteger;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(buffer, len); //buffer is zero initialized

  buffer[0] := 1;
  buffer[len -1] := 1;
  RDP(path, 0, len -1, Sqr(epsilon), buffer);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if buffer[i] = 1 then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPathsD;
  epsilon: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := length(paths);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := RamerDouglasPeucker(paths[i], epsilon);
    if Result[i] <> nil then inc(j);
  end;
  setLength(Result, j);
end;

//------------------------------------------------------------------------------
// SmoothToBezier() support structures and functions
//------------------------------------------------------------------------------

type
  PPt = ^TPt;
  TPt = record
    pt   : TPointD;
    vec  : TPointD;
    len  : double;
    next : PPt;
    prev : PPt;
  end;

  TFitCurveContainer = class
  private
    ppts      : PPt;
    solution  : TPathD;
    tolSqrd   : double;
    function Count(first, last: PPt): integer;
    function AddPt(const pt: TPointD): PPt;
    procedure Clear;
    function ComputeLeftTangent(p: PPt): TPointD;
    function ComputeRightTangent(p: PPt): TPointD;
    function ComputeCenterTangent(p: PPt): TPointD;
    function ChordLengthParameterize(
      first: PPt; cnt: integer): TArrayOfDouble;
    function GenerateBezier(first, last: PPt; cnt: integer;
      const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
    function Reparameterize(first: PPt; cnt: integer;
      const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
    function NewtonRaphsonRootFind(const q: TPathD;
      const pt: TPointD; u: double): double;
    function ComputeMaxErrorSqrd(first, last: PPt;
      const bezier: TPathD; const u: TArrayOfDouble;
      out SplitPoint: PPt): double;
    function FitCubic(first, last: PPt;
      firstTan, lastTan: TPointD): Boolean;
    procedure AppendSolution(const bezier: TPathD);
  public
    function FitCurve(const path: TPathD; closed: Boolean;
      tolerance: double; minSegLength: double): TPathD;
  end;

//------------------------------------------------------------------------------

function Scale(const vec: TPointD; newLen: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * newLen;
  Result.Y := vec.Y * newLen;
end;
//------------------------------------------------------------------------------

function Mul(const vec: TPointD; val: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * val;
  Result.Y := vec.Y * val;
end;
//------------------------------------------------------------------------------

function AddVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X + vec2.X;
  Result.Y := vec1.Y + vec2.Y;
end;
//------------------------------------------------------------------------------

function SubVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X - vec2.X;
  Result.Y := vec1.Y - vec2.Y;
end;
//------------------------------------------------------------------------------

function DotProdVecs(const vec1, vec2: TPointD): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
end;
//---------------------------------------------------------------------------

function NormalizeVec(const vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  len: double;
begin
  len := Sqrt(vec.X * vec.X + vec.Y * vec.Y);
  if len <> 0 then
  begin
    Result.X := vec.X / len;
    Result.Y := vec.Y / len;
  end else
    result := vec;
end;
//------------------------------------------------------------------------------

function NormalizeTPt(const pt: PPt): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  with pt^ do
    if len <> 0 then
    begin
      Result.X := vec.X / len;
      Result.Y := vec.Y / len;
    end else
      result := vec;
end;
//------------------------------------------------------------------------------

function NegateVec(vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := -vec.X;
  Result.Y := -vec.Y;
end;
//------------------------------------------------------------------------------

function B0(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := tmp * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B1(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := 3 * u * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B2(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := 3 * u * u * (1.0 - u);
end;
//------------------------------------------------------------------------------

function B3(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := u * u * u;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.AddPt(const pt: TPointD): PPt;
begin
  new(Result);
  Result.pt := pt;
  if not assigned(ppts) then
  begin
    Result.prev := Result;
    Result.next := Result;
    ppts := Result;
  end else
  begin
    Result.prev := ppts.prev;
    ppts.prev.next := Result;
    ppts.prev := Result;
    Result.next := ppts;
  end;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.Clear;
var
  p: PPt;
begin
  solution := nil;
  ppts.prev.next := nil; //break loop
  while assigned(ppts) do
  begin
    p := ppts;
    ppts := ppts.next;
    Dispose(p);
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Count(first, last: PPt): integer;
begin
  if first = last then
    result := 0 else
    result := 1;
  repeat
    inc(Result);
    first := first.next;
  until (first = last);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeLeftTangent(p: PPt): TPointD;
begin
  Result := NormalizeTPt(p);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeRightTangent(p: PPt): TPointD;
begin
  Result := NegateVec(NormalizeTPt(p.prev));
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeCenterTangent(p: PPt): TPointD;
var
  v1, v2: TPointD;
begin
  v1 := SubVecs(p.pt, p.prev.pt);
  v2 := SubVecs(p.next.pt, p.pt);
  Result := AddVecs(v1, v2);
  Result := NormalizeVec(Result);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ChordLengthParameterize(
  first: PPt; cnt: integer): TArrayOfDouble;
var
  d: double;
  i: integer;
begin
  SetLength(Result, cnt);
  Result[0] := 0;
  d := 0;
  for i := 1 to cnt -1 do
  begin
    d := d + first.len;
    Result[i] := d;
    first := first.next;
  end;
  for i := 1 to cnt -1 do
    Result[i] := Result[i] / d;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.GenerateBezier(first, last: PPt; cnt: integer;
  const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
var
  i: integer;
  p: PPt;
  dist, epsilon: double;
  v1,v2, tmp: TPointD;
  a0, a1: TPathD;
  c: array [0..1, 0..1] of double;
  x: array [0..1] of double;
  det_c0_c1, det_c0_x, det_x_c1, alphaL, alphaR: double;
begin
  SetLength(a0, cnt);
  SetLength(a1, cnt);
  dist := Distance(first.pt, last.pt);

  for i := 0 to cnt -1 do
  begin
		v1 := Scale(firstTan, B1(u[i]));
		v2 := Scale(lastTan, B2(u[i]));
		a0[i] := v1;
		a1[i] := v2;
  end;

  FillChar(c[0][0], 4 * SizeOf(double), 0);
  FillChar(x[0], 2 * SizeOf(double), 0);

  p := first;
  for i := 0 to cnt -1 do
  begin
		c[0][0] := c[0][0] + DotProdVecs(a0[i], (a0[i]));
		c[0][1] := c[0][1] + DotProdVecs(a0[i], (a1[i]));
		c[1][0] := c[0][1];
		c[1][1] := c[1][1] + DotProdVecs(a1[i], (a1[i]));

    tmp := SubVecs(p.pt,
      AddVecs(Mul(first.pt, B0(u[i])),
      AddVecs(Mul(first.pt, B1(u[i])),
      AddVecs(Mul(last.pt, B2(u[i])),
      Mul(last.pt, B3(u[i]))))));

    x[0] := x[0] + DotProdVecs(a0[i], tmp);
    x[1] := x[1] + DotProdVecs(a1[i], tmp);
    p := p.next;
  end;

  det_c0_c1 := c[0][0] * c[1][1] - c[1][0] * c[0][1];
	det_c0_x := c[0][0] * x[1] - c[1][0] * x[0];
	det_x_c1 := x[0] * c[1][1] - x[1] * c[0][1];

  if det_c0_c1 = 0 then
    alphaL := 0 else
    alphaL := det_x_c1 / det_c0_c1;

  if det_c0_c1 = 0 then
    alphaR := 0 else
    alphaR := det_c0_x / det_c0_c1;

  //check for unlikely fit
  if (alphaL > dist * 2) then alphaL := 0
  else if (alphaR > dist * 2) then alphaR := 0;
  epsilon := 1.0e-6 * dist;

  SetLength(Result, 4);
  Result[0] := first.pt;
  Result[3] := last.pt;
  if (alphaL < epsilon) or (alphaR < epsilon) then
  begin
    dist := dist / 3;
    Result[1] := AddVecs(Result[0], Scale(firstTan, dist));
    Result[2] := AddVecs(Result[3], Scale(lastTan, dist));
  end else
  begin
    Result[1] := AddVecs(Result[0], Scale(firstTan, alphaL));
    Result[2] := AddVecs(Result[3], Scale(lastTan, alphaR));
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Reparameterize(first: PPt; cnt: integer;
  const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
var
  i: integer;
begin
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
  begin
    Result[i] := NewtonRaphsonRootFind(bezier, first.pt, u[i]);
    first := first.next;
  end;
end;
//------------------------------------------------------------------------------

function BezierII(degree: integer; const v: array of TPointD; t: double): TPointD;
var
  i,j: integer;
  tmp: array[0..3] of TPointD;
begin
  Move(v[0], tmp[0], (degree +1) * sizeOf(TPointD));
  for i := 1 to degree do
    for j := 0 to degree - i do
    begin
      tmp[j].x := (1.0 - t) * tmp[j].x + t * tmp[j+1].x;
      tmp[j].y := (1.0 - t) * tmp[j].y + t * tmp[j+1].y;
    end;
  Result := tmp[0];
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeMaxErrorSqrd(first, last: PPt;
	const bezier: TPathD; const u: TArrayOfDouble;
  out SplitPoint: PPt): double;
var
  i: integer;
  distSqrd: double;
  pt: TPointD;
  p: PPt;
begin
	Result := 0;
  i := 1;
  SplitPoint := first.next;
  p := first.next;
	while p <> last do
	begin
		pt := BezierII(3, bezier, u[i]);
		distSqrd := DistanceSqrd(pt, p.pt);
		if (distSqrd >= Result) then
    begin
      Result := distSqrd;
      SplitPoint := p;
    end;
    inc(i);
    p := p.next;
	end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.NewtonRaphsonRootFind(const q: TPathD;
  const pt: TPointD; u: double): double;
var
  numerator, denominator: double;
  qu, q1u, q2u: TPointD;
  q1: array[0..2] of TPointD;
  q2: array[0..1] of TPointD;
begin

  q1[0].x := (q[1].x - q[0].x) * 3.0;
  q1[0].y := (q[1].y - q[0].y) * 3.0;
  q1[1].x := (q[2].x - q[1].x) * 3.0;
  q1[1].y := (q[2].y - q[1].y) * 3.0;
  q1[2].x := (q[3].x - q[2].x) * 3.0;
  q1[2].y := (q[3].y - q[2].y) * 3.0;

  q2[0].x := (q1[1].x - q1[0].x) * 2.0;
  q2[0].y := (q1[1].y - q1[0].y) * 2.0;
  q2[1].x := (q1[2].x - q1[1].x) * 2.0;
  q2[1].y := (q1[2].y - q1[1].y) * 2.0;

  qu  := BezierII(3, q, u);
  q1u := BezierII(2, q1, u);
  q2u := BezierII(1, q2, u);

  numerator := (qu.x - pt.x) * (q1u.x) + (qu.y - pt.y) * (q1u.y);
  denominator := (q1u.x) * (q1u.x) + (q1u.y) * (q1u.y) +
    (qu.x - pt.x) * (q2u.x) + (qu.y - pt.y) * (q2u.y);

  if (denominator = 0) then
    Result := u else
    Result := u - (numerator / denominator);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCubic(first, last: PPt;
  firstTan, lastTan: TPointD): Boolean;
var
  i, cnt: integer;
  splitPoint: PPt;
  centerTan: TPointD;
  bezier: TPathD;
  clps, uPrime: TArrayOfDouble;
  maxErrorSqrd: double;
const
  maxRetries = 4;
begin
  Result := true;
  cnt := Count(first, last);
  if cnt = 2 then
  begin
    SetLength(bezier, 4);
    bezier[0] := first.pt;
    bezier[3] := last.pt;
    bezier[1] := bezier[0];
    bezier[2] := bezier[3];
    AppendSolution(bezier);
    Exit;
  end
  else if cnt = 3 then
  begin
    if TurnsLeft(first.prev.pt, first.pt, first.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        firstTan := ComputeCenterTangent(first);
    if TurnsLeft(last.prev.pt, last.pt, last.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        lastTan := NegateVec(ComputeCenterTangent(last));
  end;


  clps := ChordLengthParameterize(first, cnt);
  bezier := GenerateBezier(first, last, cnt, clps, firstTan, lastTan);
  maxErrorSqrd := ComputeMaxErrorSqrd(first, last, bezier, clps, splitPoint);
  if (maxErrorSqrd < tolSqrd) then
  begin
    AppendSolution(bezier);
    Exit;
  end;

  if (maxErrorSqrd < tolSqrd * 4) then //close enough to try again
  begin
		for i := 1 to maxRetries do
    begin
      uPrime := Reparameterize(first, cnt, clps, bezier);
      bezier := GenerateBezier(first, last, cnt, uPrime, firstTan, lastTan);
      maxErrorSqrd :=
        ComputeMaxErrorSqrd(first, last, bezier, uPrime, splitPoint);
			if (maxErrorSqrd < tolSqrd) then
      begin
        AppendSolution(bezier);
        Exit;
			end;
			clps := uPrime;
		end;
	end;

  //We need to break the curve because it's too complex for a single Bezier.
  //If we're changing direction then make this a 'hard' break (see below).
  if TurnsLeft(splitPoint.prev.prev.pt, splitPoint.prev.pt, splitPoint.pt) <>
    TurnsLeft(splitPoint.prev.pt, splitPoint.pt, splitPoint.next.pt) then
  begin
    centerTan := ComputeRightTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, centerTan);
    centerTan := ComputeLeftTangent(splitPoint);
    FitCubic(splitPoint, last, centerTan, lastTan);
  end else
  begin
    centerTan := ComputeCenterTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, NegateVec(centerTan));
    FitCubic(splitPoint, last, centerTan, lastTan);
  end;
end;
//------------------------------------------------------------------------------

function HardBreakCheck(ppt: PPt; compareLen: double): Boolean;
var
  q: double;
const
  longLen = 15;
begin
  //A 'break' means starting a new Bezier. A 'hard' break avoids smoothing
  //whereas a 'soft' break will still be smoothed. There is as much art as
  //science in determining where to smooth and where not to. For example,
  //long edges should generally remain straight but how long does an edge
  //have to be to be considered a 'long' edge?

  if (ppt.prev.len * 4 < ppt.len) or (ppt.len * 4 < ppt.prev.len) then
  begin
    //We'll hard break whenever there's significant asymmetry between
    //segment lengths because GenerateBezier() will perform poorly.
    result := true;
  end
  else if ((ppt.prev.len > longLen) and (ppt.len > longLen)) then
  begin
    //hard break long segments only when turning by more than ~45 degrees
    q := (Sqr(ppt.prev.len) + Sqr(ppt.len) - DistanceSqrd(ppt.prev.pt, ppt.next.pt)) /
      (2 * ppt.prev.len * ppt.len); //Cosine Rule.
    result := (1 - abs(q)) > 0.3;
  end
  else if ((TurnsLeft(ppt.prev.prev.pt, ppt.prev.pt, ppt.pt) =
    TurnsRight(ppt.prev.pt, ppt.pt, ppt.next.pt)) and
    (ppt.prev.len > compareLen) and (ppt.len > compareLen)) then
  begin
    //we'll also hard break whenever there's a significant inflection point
    result := true;
  end else
  begin
    //Finally, we'll also force a 'hard' break when there's a significant bend.
    //Again uses the Cosine Rule.
    q :=(Sqr(ppt.prev.len) + Sqr(ppt.len) -
      DistanceSqrd(ppt.prev.pt, ppt.next.pt)) / (2 * ppt.prev.len * ppt.len);
    Result := (q > -0.2); //ie more than 90%
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCurve(const path: TPathD;
  closed: Boolean; tolerance: double; minSegLength: double): TPathD;
var
  i, highI: integer;
  d: double;
  p, p2, pEnd: PPt;
begin
  //tolerance: specifies the maximum allowed variance between the existing
  //vertices and the new Bezier curves. More tolerance will produce
  //fewer Beziers and simpler paths, but at the cost of less precison.
  tolSqrd := Sqr(Max(1, Min(10, tolerance))); //range 1..10

  //minSegLength: Typically when vectorizing raster images, the produced
  //vector paths will have many series of axis aligned segments that trace
  //pixel boundaries. These paths will also contain many 1 unit segments at
  //right angles to adjacent segments. Importantly, these very short segments
  //will cause artifacts in the solution unless they are trimmed.
  highI     := High(path);
  if closed then
    while (highI > 0) and (Distance(path[highI], path[0]) < minSegLength) do
      dec(highI);

  p := AddPt(path[0]);
  for i := 1 to highI do
  begin
    d := Distance(p.pt, path[i]);
    //skip line segments with lengths less than 'minSegLength'
    if d < minSegLength then Continue;
    p := AddPt(path[i]);
    p.prev.len := d;
    p.prev.vec := SubVecs(p.pt, p.prev.pt);
  end;
  p.len := Distance(ppts.pt, p.pt);
  p.vec := SubVecs(p.next.pt, p.pt);
  p := ppts;

  if (p.next = p) or (closed and (p.next = p.prev)) then
  begin
    Clear;
    result := nil;
    Exit;
  end;

  //for closed paths, find a good starting point
  if closed then
  begin
    repeat
      if HardBreakCheck(p, tolerance) then break;
      p := p.next;
    until p = ppts;
    pEnd := p;
  end else
    pEnd := ppts.prev;

  p2 := p.next;
  repeat
    if HardBreakCheck(p2, tolerance) then
    begin
      FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));
      p := p2;
    end;
    p2 := p2.next;
  until (p2 = pEnd);
  FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));

  Result := solution;
  Clear;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.AppendSolution(const bezier: TPathD);
var
  i, len: integer;
begin
  len := Length(solution);
  if len > 0 then
  begin
    SetLength(solution, len + 3);
    for i := 0 to 2 do
      solution[len +i] := bezier[i +1];
  end else
    solution := bezier;
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const path: TPathD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathD;
var
  paths, solution: TPathsD;
begin
  SetLength(paths, 1);
  paths[0] := path;
  solution := SmoothToBezier(paths, closed, tolerance, minSegLength);
  if solution <> nil then
    Result := solution[0];
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const paths: TPathsD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := Length(paths);
  SetLength(Result, len);
  with TFitCurveContainer.Create do
  try
    for i := 0 to len -1 do
      if (paths[i] <> nil) and (Abs(Area(paths[i])) > Sqr(tolerance)) then
      begin
        Result[j] := FitCurve(paths[i], closed, tolerance, minSegLength);
        inc(j);
      end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;

//------------------------------------------------------------------------------
// GaussianBlur
//------------------------------------------------------------------------------

procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);
var
  i, x,y,yy,z: Integer;
  gaussTable: array [-MaxBlur .. MaxBlur] of Cardinal;
  wc: TWeightedColor;
  wca: TArrayOfWeightedColor;
  row: PColor32Array;
  wcRow: PWeightedColorArray;
begin
  rec := Image32_Vector.IntersectRect(rec, img.Bounds);
  if IsEmptyRect(rec) or (radius < 1) then Exit
  else if radius > MaxBlur then radius := MaxBlur;

  for i := 0 to radius do
  begin
    gaussTable[i] := Sqr(Radius - i +1);
    gaussTable[-i] := gaussTable[i];
  end;

  setLength(wca, RectWidth(rec) * RectHeight(rec));

  for y := 0 to RectHeight(rec) -1 do
  begin
    row := PColor32Array(@img.Pixels[(y + rec.Top) * img.Width + rec.Left]);
    wcRow := PWeightedColorArray(@wca[y * RectWidth(rec)]);
    for x := 0 to RectWidth(rec) -1 do
      for z := max(0, x - radius) to min(img.Width -1, x + radius) do
        wcRow[x].Add(row[z], gaussTable[x-z]);
  end;

  for x := 0 to RectWidth(rec) -1 do
  begin
    for y := 0 to RectHeight(rec) -1 do
    begin
      wc.Reset;
      yy := max(0, y - radius) * RectWidth(rec);
      for z := max(0, y - radius) to min(RectHeight(rec) -1, y + radius) do
      begin
        wc.Add(wca[x + yy].Color, gaussTable[y-z]);
        inc(yy, RectWidth(rec));
      end;
      img.Pixels[x + rec.Left + (y + rec.Top) * img.Width] := wc.Color;
    end;
  end;
end;

//------------------------------------------------------------------------------
// FastGaussian blur - and support functions
//------------------------------------------------------------------------------

//http://blog.ivank.net/fastest-gaussian-blur.html
//https://www.peterkovesi.com/papers/FastGaussianSmoothing.pdf

function BoxesForGauss(stdDev, boxCnt, maxVal: integer): TArrayOfInteger;
var
  i, wl, wu, m: integer;
  wIdeal, mIdeal: double;
begin
  wIdeal := Sqrt((12*stdDev*stdDev/boxCnt)+1);
  wl := Floor(wIdeal);
  if (wl mod 2 = 0) then dec(wl);
  wu := wl+2;

  mIdeal := (12 *stdDev*stdDev
    -boxCnt *wl*wl - 4*boxCnt *wl - 3 *boxCnt) / (-4 *wl - 4);
  m := Round(mIdeal);
  SetLength(Result, boxCnt);
  wl := Min(maxVal, wl);
  wu := Min(maxVal, wu);

  for i := 0 to boxCnt -1 do
    if i < m then
      Result[i] := (wl -1) div 2 else
      Result[i] := (wu -1) div 2;
end;
//------------------------------------------------------------------------------

procedure BoxBlurH(var src, dst: TArrayOfColor32; w,h, stdDev: integer);
var
  i,j, ti, li, ri: integer;
  fv, lv, val: TWeightedColor;
begin
  for i := 0 to h -1 do
  begin
    ti := i * w;
    li := ti;
    ri := ti +stdDev;

    fv.Reset;
    lv.Reset;
    val.Reset;

    fv.Add(src[ti], 1);
    lv.Add(src[ti +w -1], 1);
    val.Add(fv.Color, stdDev +1);

    for j := 0 to stdDev -1 do
      val.Add(src[ti + j]);
    for j := 0 to stdDev do
    begin
      val.Add(src[ri]); inc(ri);
      val.Subtract(fv);
      dst[ti] := val.Color; inc(ti);
    end;
    for j := stdDev +1 to w - stdDev -1 do
    begin
      val.Add(src[ri]); inc(ri);
      val.Subtract(src[li]); inc(li);
      dst[ti] := val.Color; inc(ti);
    end;
    for j := w - stdDev to w -1 do
    begin
      val.Add(lv);
      val.Subtract(src[li]); inc(li);
      dst[ti] := val.Color; inc(ti);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure BoxBlurT(var src, dst: TArrayOfColor32; w, h, stdDev: integer);
var
  i,j, ti, li, ri: integer;
  fv, lv, val: TWeightedColor;
begin
  for i := 0 to w -1 do
  begin
    ti := i;
    li := ti;
    ri := ti + stdDev * w;

    fv.Reset;
    lv.Reset;
    val.Reset;

    fv.Add(src[ti]);
    lv.Add(src[ti +w *(h-1)], 1);
    val.Add(fv.Color, stdDev +1);

    for j := 0 to stdDev -1 do
      val.Add(src[ti + j *w]);
    for j := 0 to stdDev do
    begin
      val.Add(src[ri]); inc(ri, w);
      val.Subtract(fv);
      dst[ti] := val.Color; inc(ti, w);

    end;
    for j := stdDev +1 to h - stdDev -1 do
    begin
      val.Add(src[ri]); inc(ri, w);
      val.Subtract(src[li]); inc(li, w);
      dst[ti] := val.Color; inc(ti, w);
    end;
    for j := h - stdDev to h -1 do
    begin
      val.Add(lv);
      val.Subtract(src[li]); inc(li, w);
      dst[ti] := val.Color; inc(ti, w);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDev: integer; repeats: integer);
var
  i,j,len, w,h, maxStdDev: integer;
  rec2: TRect;
  boxes: TArrayOfInteger;
  src, dst: TArrayOfColor32;
  blurFullImage: Boolean;
  p: PColor32;
begin
  if not Assigned(img) then Exit;
  rec2 := Image32_Vector.IntersectRect(rec, img.Bounds);
  if IsEmptyRect(rec2) then Exit;
  blurFullImage := RectsEqual(rec2, img.Bounds);

  w := RectWidth(rec2);
  h := RectHeight(rec2);
  len := w * h;
  SetLength(src, len);
  SetLength(dst, len);

  //safety check to avoid overflows
  maxStdDev := Min(w, h) div 2 -1;
  if stdDev > maxStdDev then
    stdDev := maxStdDev;
  if stdDev < 1 then Exit;

  if blurFullImage then
  begin
    //copy image rect into  dst array
    Move(img.PixelBase^, dst[0], len * SizeOf(TColor32));
  end else
  begin
    //copy just a rectangular region into  dst array
    p := @dst[0];
    for i := rec2.Top to rec2.Bottom -1 do
    begin
      Move(img.Pixels[i * img.Width + rec2.Left],
        p^, w * SizeOf(TColor32));
      inc(p, w);
    end;
  end;

  //do the blur
  inc(repeats);
  boxes := BoxesForGauss(stdDev, repeats, maxStdDev);
  for j := 0 to repeats -1 do
    begin
      BoxBlurH(dst, src, w, h, boxes[j]);
      BoxBlurT(src, dst, w, h, boxes[j]);
    end;

  //copy dst array back to image rect
  img.BeginUpdate;
  try
    if blurFullImage then
    begin
      Move(dst[0], img.PixelBase^, len * SizeOf(TColor32));
    end else
    begin
      p := @dst[0];
      for i := rec2.Top to rec2.Bottom -1 do
      begin
        Move(p^, img.Pixels[i * img.Width + rec2.Left], w * SizeOf(TColor32));
        inc(p, w);
      end;
    end;
  finally
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

end.
