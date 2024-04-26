unit Img32.Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.3                                                             *
* Date      :  17 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Img32;

//BoxDownSampling: As the name implies, this routine is only intended for
//image down-sampling (ie when shrinking images) where it generally performs
//better than other resamplers which tend to lose too much detail. However,
//because this routine is inferior to other resamplers when performing other
//transformations (ie when enlarging, rotating, and skewing images), it's not
//intended as a general purpose resampler.
procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);

(* The following functions are registered in the initialization section below
function NearestResampler(img: TImage32; x, y: double): TColor32;
function BilinearResample(img: TImage32; x, y: double): TColor32;
function BicubicResample (img: TImage32; x, y: double): TColor32;
*)

implementation

uses
  Img32.Transform;

//------------------------------------------------------------------------------
// NearestNeighbor resampler
//------------------------------------------------------------------------------

function NearestResampler(img: TImage32; x, y: double): TColor32;
var
  iw, ih, xx, yy: integer;
begin
  iw := img.Width;
  ih := img.Height;
  if (x < -0.5) or (x -0.5 >= iw) or
     (y < -0.5) or (y -0.5 >= ih) then
  begin
    Result := clNone32;
    Exit;
  end;

  // scale the image fractionally so as to avoid the pixels along the
  // right and bottom edges effectively duplicating their adjacent pixels
  if (x > 0) and (x < iw) then x := x - x/(iw+0.25);
  if (y > 0) and (y < ih) then y := y - y/(ih+0.25);

  xx := Min(Max(0, Round(x)), iw -1);
  yy := Min(Max(0, Round(y)), ih -1);
  Result := img.Pixels[xx + yy * img.Width];
end;

//------------------------------------------------------------------------------
// BiLinear resampler
//------------------------------------------------------------------------------

function BilinearResample(img: TImage32; x, y: double): TColor32;
var
  iw, ih: integer;
  xx, yy, xR, yB: integer;
  weight: Cardinal;
  pixels: TArrayOfColor32;
  weightedColor: TWeightedColor;
  xf, yf: double;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  if (x < -1) or (x >= iw + 1) or
     (y < -1) or (y >= ih + 1) then
  begin
    result := clNone32;
    Exit;
  end;

  // scale the image fractionally so as to avoid the pixels along the
  // right and bottom edges effectively duplicating their adjacent pixels
  if (x > 0) and (x < iw) then x := x - x/(iw+0.25);
  if (y > 0) and (y < ih) then y := y - y/(ih+0.25);

  if x < 0 then
    xf := frac(1+x) else
    xf := frac(x);
  if y < 0 then
    yf := frac(1+y) else
    yf := frac(y);

  xx := Floor(x);
  yy := Floor(y);
  xR := xx +1;
  yB := yy +1;

  if xx >= iw -1 then
  begin
    xx := iw -1;
    xR := xx;
  end;
  if yy >= ih -1 then
  begin
    yy := ih -1;
    yB := yy;
  end;

  weightedColor.Reset;

  weight := Round((1-xf) * (1-yf) * 256);      //top-left
  if weight > 0 then
  begin
    if (x < 0) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yy * iw], weight);
  end;

  weight := Round(xf * (1-yf) * 256);         //top-right
  if weight > 0 then
  begin
    if (x > iw) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yy * iw], weight);
  end;

  weight := Round((1-xf) * yf * 256);          //bottom-left
  if weight > 0 then
  begin
    if (x < 0) or (y > ih) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yB * iw], weight);
  end;

  weight := Round(xf * yf * 256);              //bottom-right
  if weight > 0 then
  begin
    if (x > iw) or (y > ih) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yB * iw], weight);
  end;
  Result := weightedColor.Color;
end;

//------------------------------------------------------------------------------
// BiCubic resampler
//------------------------------------------------------------------------------

type
  TBiCubicEdgeAdjust = (eaNormal, eaOnePixel,
    eaPreStart, eaStart, eaEnd, eaPostEnd);

var
  byteFrac: array [0..255] of double;
  byteFracSq: array [0..255] of double;
  byteFracCubed: array [0..255] of double;

//------------------------------------------------------------------------------

function CubicHermite(aclr: PColor32; t: Byte; bce: TBiCubicEdgeAdjust): TColor32;
var
  a,b,c,d: PARGB;
  q: TARGB;
	aa, bb, cc: integer;
  t1, t2, t3: double;
  res: TARGB absolute Result;
const
  clTrans: TColor32 = clNone32;
  clDebug: TColor32 = clBlack32;
begin
  case bce of
    eaPreStart:
      begin
        Inc(aclr);
        a := @clTrans;
        b := @clTrans;
        c := PARGB(aclr);
        d := c;
      end;
    eaStart:
      begin
        a := PARGB(aclr);
        b := a;
        Inc(aclr);
        c := PARGB(aclr);
        d := c;
      end;
    eaEnd:
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        c := b;
        d := b;
      end;
    eaPostEnd:
      begin
        a := PARGB(aclr);
        b := a;
        c := @clTrans;
        d := @clTrans;
      end;
    else
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        Inc(aclr);
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
  end;

  if (b.A = 0) and (c.A = 0) then
  begin
    result := clNone32;
    Exit;
  end
  else if (b.Color = c.Color) then
  begin
    result := b.Color;
    Exit;
  end
  else if b.A = 0 then
  begin
    q := c^;
    q.A := 0;
    b := @q;
  end;
  if c.A = 0 then
  begin
    q := b^;
    q.A := 0;
    c := @q;
  end;

  t1 := byteFrac[t];
  t2 := byteFracSq[t];
  t3 := byteFracCubed[t];

	aa := Integer(-a.A + 3*b.A - 3*c.A + d.A) div 2;
	bb := Integer(2*a.A - 5*b.A + 4*c.A - d.A) div 2;
	cc := Integer(-a.A + c.A) div 2;
  Res.A := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.A);

	aa := Integer(-a.R + 3*b.R - 3*c.R + d.R) div 2;
	bb := Integer(2*a.R - 5*b.R + 4*c.R - d.R) div 2;
	cc := Integer(-a.R + c.R) div 2;
  Res.R := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.R);

	aa := Integer(-a.G + 3*b.G - 3*c.G + d.G) div 2;
	bb := Integer(2*a.G - 5*b.G + 4*c.G - d.G) div 2;
	cc := Integer(-a.G + c.G) div 2;
  Res.G := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.G);

	aa := Integer(-a.B + 3*b.B - 3*c.B + d.B) div 2;
	bb := Integer(2*a.B - 5*b.B + 4*c.B - d.B) div 2;
	cc := Integer(-a.B + c.B) div 2;
  Res.B := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x, y: double): TColor32;
var
  i, pxIdx, iw, ih, dy, last: integer;
  xFrac,yFrac: Byte;
  c: array[0..3] of TColor32;
  bceX, bceY: TBiCubicEdgeAdjust;
begin

  iw := img.Width;
  ih := img.Height;
  last := iw * ih -1;

  Result := clNone32;
  if (x <= -1) or (x >= iw +1) or
    (y <= -1) or (y >= ih +1) then Exit;

  // scale the image fractionally so as to avoid the pixels along the
  // right and bottom edges effectively duplicating their adjacent pixels
  if (x > 0) and (x <= iw) then x := x - x/(iw+0.25);
  if (y > 0) and (y <= ih) then y := y - y/(ih+0.25);

  if x < 0 then bceX := eaPreStart
  else if x > iw then bceX := eaPostEnd
  else if iw = 1 then bceX := eaOnePixel
  else if x < 1 then bceX := eaStart
  else if x >= iw -1 then bceX := eaEnd
  else bceX := eaNormal;

  if y < 0 then bceY := eaPreStart
  else if y > ih then bceY := eaPostEnd
  else if ih = 1 then bceY := eaOnePixel
  else if y < 1 then bceY := eaStart
  else if y >= ih -1 then bceY := eaEnd
  else bceY := eaNormal;

  if x < 0 then
    xFrac := Round(frac(1+x) *255) else
    xFrac := Round(frac(x) *255);
  if y < 0 then
    yFrac := Round(frac(1+y) *255) else
    yFrac := Round(frac(y) *255);

  if (x < 0) then x := 0
  else if (x >= 1) then x := x - 1;
  if (y < 0) then y := 0
  else if (y >= 1) then y := y - 1;

  if bceY = eaPostEnd then dy := 1
  else if bceY = eaNormal then dy := 4
  else dy := 2;

  pxIdx := Floor(y) * iw + Floor(x);

  if bceY = eaOnePixel then
  begin
    if bceX = eaOnePixel then
      Result := img.Pixels[0] else
      Result := CubicHermite(@img.Pixels[pxIdx], xFrac, bceX);
  end
  else if bceX = eaOnePixel then
  begin
    for i := 0 to dy-1 do
    begin
      c[i] := img.Pixels[pxIdx];
      inc(pxIdx, iw);
    end;
    Result := CubicHermite(@c[0], yFrac, bceY);
  end else
  begin
    for i := 0 to dy-1 do
    begin
      c[i] := CubicHermite(@img.Pixels[pxIdx], xFrac, bceX);
      inc(pxIdx, iw);
      if pxIdx >= last then break;
    end;
    Result := CubicHermite(@c[0], yFrac, bceY);
  end;
end;

//------------------------------------------------------------------------------
// BoxDownSampling and related functions
//------------------------------------------------------------------------------

function GetWeightedColor(const srcBits: TArrayOfColor32;
  x256, y256, xx256, yy256, maxX: Integer): TColor32;
var
  i, j, xi, yi, xxi, yyi, weight: Integer;
  xf, yf, xxf, yyf: cardinal;
  color: TWeightedColor;
begin
  //This function performs 'box sampling' and differs from GetWeightedPixel
  //(bilinear resampling) in one important aspect - it accommodates weighting
  //any number of pixels (rather than just adjacent pixels) and this produces
  //better image quality when significantly downsizing.

  //Note: there's no range checking here, so the precondition is that the
  //supplied boundary values are within the bounds of the srcBits array.

  color.Reset;

  xi := x256 shr 8; xf := x256 and $FF;
  yi := y256 shr 8; yf := y256 and $FF;
  xxi := xx256 shr 8; xxf := xx256 and $FF;
  yyi := yy256 shr 8; yyf := yy256 and $FF;

  //1. average the corners ...
  weight := (($100 - xf) * ($100 - yf)) shr 8;
  color.Add(srcBits[xi + yi * maxX], weight);
  weight := (xxf * ($100 - yf)) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yi * maxX], weight);
  weight := (($100 - xf) * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xi + yyi * maxX], weight);
  weight := (xxf * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yyi * maxX], weight);

  //2. average the edges
  if (yi +1 < yyi) then
  begin
    xf := $100 - xf;
    for i := yi + 1 to yyi - 1 do
      color.Add(srcBits[xi + i * maxX], xf);
    if (xxf <> 0) then
      for i := yi + 1 to yyi - 1 do
        color.Add(srcBits[xxi + i * maxX], xxf);
  end;
  if (xi + 1 < xxi) then
  begin
    yf := $100 - yf;
    for i := xi + 1 to xxi - 1 do
      color.Add(srcBits[i + yi * maxX], yf);
    if (yyf <> 0) then
      for i := xi + 1 to xxi - 1 do
        color.Add(srcBits[i + yyi * maxX], yyf);
  end;

  //3. average the non-fractional pixel 'internals' ...
  for i := xi + 1 to xxi - 1 do
    for j := yi + 1 to yyi - 1 do
      color.Add(srcBits[i + j * maxX], $100);

  //4. finally get the weighted color ...
  if color.AddCount = 0 then
    Result := srcBits[xi + yi * maxX] else
    Result := color.Color;
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);
var
  x,y, x256,y256,xx256,yy256: Integer;
  sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: array of Integer;
begin
  sx := Image.Width/newWidth * 256;
  sy := Image.Height/newHeight * 256;
  SetLength(tmp, newWidth * newHeight);

  SetLength(scaledX, newWidth +1); //+1 for fractional overrun
  for x := 0 to newWidth -1 do
    scaledX[x] := Round((x+1) * sx);

  y256 := 0;
  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    x256 := 0;
    yy256 := Round((y+1) * sy);
    for x := 0 to newWidth - 1 do
    begin
      xx256 := scaledX[x];
      pc^ := GetWeightedColor(Image.Pixels,
        x256, y256, xx256, yy256, Image.Width);
      x256 := xx256;
      inc(pc);
    end;
    y256 := yy256;
  end;

  Image.BeginUpdate;
  Image.SetSize(newWidth, newHeight);
  Move(tmp[0], Image.Pixels[0], newWidth * newHeight * SizeOf(TColor32));
  Image.EndUpdate;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure InitByteExponents;
var
  i: integer;
const
  inv255     : double = 1/255;
  inv255sqrd : double = 1/(255*255);
  inv255cubed: double = 1/(255*255*255);
begin
  for i := 0 to 255 do
  begin
    byteFrac[i]  := i     *inv255;
    byteFracSq[i]  := i*i   *inv255sqrd;
    byteFracCubed[i] := i*i*i *inv255cubed;
  end;
end;
//------------------------------------------------------------------------------

initialization
  InitByteExponents;

  rNearestResampler  := RegisterResampler(NearestResampler, 'NearestNeighbor');
  rBilinearResampler := RegisterResampler(BilinearResample, 'Bilinear');
  rBicubicResampler  := RegisterResampler(BicubicResample, 'HermiteBicubic');
  DefaultResampler   := rBilinearResampler;

end.
