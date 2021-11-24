unit Img32.Controls;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.5                                                             *
* Date      :  31 October 2021                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Routines to draw control objects                                *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
  Img32, Img32.Text, Img32.Extra, Img32.Draw, Img32.Vector;

type

  TTabCtrlMetrics = record
    bounds: TRect;
    offsets: TArrayOfInteger;
  end;

  TCtrlState = (csNormal, csPressed, csChecked = 1, csDisabled, csHot);

procedure DrawBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache;
  bevelHeight: double; state: TCtrlState = csNormal;
  color: TColor32 = clBtnFace32; textColor: TColor32 = clBlack32);

procedure DrawBevelledCtrl(Image: TImage32; const path: TPathD;
  bevelHeight: double; bevelWidth: integer; color: TColor32 = clNone32);

function DrawRoundedBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache;
  bevelHeight: double; state: TCtrlState = csNormal;
  color: TColor32 = clBtnFace32; textColor: TColor32 = clBlack32): TPathD;

function DrawEllipseBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache; bevelHeight: double;
  state: TCtrlState = csNormal;
  color: TColor32 = clBtnFace32; textColor: TColor32 = clBlack32): TPathD;

procedure DrawEditCtrl(Image: TImage32; const rect: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32);

procedure DrawCheckboxCtrl(Image: TImage32; const rec: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32;
  state: TCtrlState = csNormal; triState: TTriState = tsNo);

function DrawRadioCtrl(Image: TImage32; const rec: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32;
  state: TCtrlState = csNormal; triState: TTriState = tsNo): TPathD;

function DrawTabCtrl(Image: TImage32;
  const captions: array of string; fontCache: TGlyphCache;
  position: TPoint; bevelHeight: double; selectedIdx: integer;
  tabWidth: integer; tabHeight: integer;
  color: TColor32; textColor: TColor32;
  selColor: TColor32; selTextColor: TColor32): TTabCtrlMetrics;

function DrawPageCtrl(Image: TImage32;
  const captions: array of string; fontCache: TGlyphCache;
  const pageRect: TRect; selectedIdx: integer;
  bevelHeight: double;  tabWidth: integer = 0; tabHeight: integer = 0;
  color: TColor32 = clBtnFace32; textColor: TColor32 = clBlack32;
  selColor: TColor32 = clBtnFace32;
  selTextColor: TColor32 = clMaroon32): TTabCtrlMetrics;

function GetSegIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;

const
  csLimeGreen32 = TColor32($FF32CD32);
  csPaleGray32  = TColor32($FFE0E0E0);

implementation

function CrossProduct(const vector1, vector2: TPointD): double;
begin
  result := vector1.X * vector2.Y - vector2.X * vector1.Y;
end;
//------------------------------------------------------------------------------

function DotProduct(const vector1, vector2: TPointD): double;
begin
  result := vector1.X * vector2.X + vector1.Y * vector2.Y;
end;
//------------------------------------------------------------------------------

function GetNormal(const pt, norm: TPointD; delta: double): TPointD;
begin
  result := PointD(pt.x + norm.x * delta, pt.y + norm.y * delta);
end;
//------------------------------------------------------------------------------

function GetSegIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;
var
  pqd,r,s : TPointD; //scalar vectors;
  rs, t   : double;
begin
  //https://stackoverflow.com/a/565282/359538
  Result := InvalidPointD;
  r := PointD(ln1b.X - ln1a.X, ln1b.Y - ln1a.Y);
  s := PointD(ln2b.X - ln2a.X, ln2b.Y - ln2a.Y);
  rs := CrossProduct(r,s);
  if Abs(rs) < 1 then Exit;
  pqd.X := ln2a.X - ln1a.X;
  pqd.y := ln2a.Y - ln1a.Y;
  t := CrossProduct(pqd, s) / rs;
  if (t < -0.025) or (t > 1.025) then Exit;
  Result.X := ln1a.X + t * r.X;
  Result.Y := ln1a.Y + t * r.Y;
//  pqd.X := -pqd.X; pqd.Y := -pqd.Y;
//  u := CrossProduct(pqd, r) / rs;
//  if (u < -0.05) or (u > 1.05) then Exit;
end;
//------------------------------------------------------------------------------

function SimpleGrow(const path, normals: TPathD; delta: double): TPathD;
var
  resCnt, resCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resCnt >= resCap then
    begin
      inc(resCap, 64);
      setLength(result, resCap);
    end;
    result[resCnt] := pt;
    inc(resCnt);
  end;

var
  i,j   : cardinal;
  len   : cardinal;
  prevI : cardinal;
  norms : TPathD;
  ip    : TPointD;
  p     : TPathD;
  sinA  : double;
  cosA  : double;
  a,d   : double;
  steps : integer;
  stepSin     : double;
  stepCos     : double;
  stepsPerRad : double;
begin
  //nb: assumes clockwise orientation such that right-turn == concave

  Result := nil;
  if not Assigned(path) then exit;

  len := Length(path);
  if len < 3 then Exit;

  norms := normals;
  if not assigned(norms) then
    norms := GetNormals(path);

  prevI := len -1;
  SetLength(p, len *2);
  for i := 0 to len -1 do
  begin
    p[i*2]     := GetNormal(path[i], norms[prevI], delta);
    p[i*2 +1]  := GetNormal(path[i], norms[i], delta);
    prevI := i;
  end;

  prevI := len -1;
  d := Abs(delta);
  a := 4 * Max(1, Sqrt(d));
  stepsPerRad := a / (Pi *2);

  if delta < 0 then a := -a;
  GetSinCos(Pi*2/a, stepSin, stepCos);
  resCnt := 0; resCap := 0;

  for i := 0 to len -1 do
  begin
    //check for intersections at vertices and truncate
    //adjacent edges when and where they intersect.
    if i = len -1 then
      ip := GetSegIntersectPt(p[prevI*2+1], p[i*2], p[i*2+1], p[0]) else
      ip := GetSegIntersectPt(p[prevI*2+1], p[i*2], p[i*2+1], p[i*2+2]);

    if (ip.X = InvalidD) then
    begin
      sinA := CrossProduct(norms[prevI], norms[i]);
      cosA := DotProduct(norms[prevI], norms[i]);
      a := ArcTan2(sinA, cosA);
      steps := Round(stepsPerRad * Abs(a));
      ip := PointD(norms[prevI].X * delta, norms[prevI].Y * delta);
      AddPoint(PointD(path[i].X + ip.X, path[i].Y + ip.Y));
      for j := 1 to steps do
      begin
        ip := PointD(ip.X * stepCos - stepSin * ip.Y,
          ip.X * stepSin + ip.Y * stepCos);
        AddPoint(PointD(path[i].X + ip.X, path[i].Y + ip.Y));
      end;
    end else
    begin
      AddPoint(ip);
    end;
    prevI := i;
  end;
  SetLength(Result, resCnt);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DrawBtnInternal(Image: TImage32;
  const rec: TRect; const p: TPathD;
  const caption: string; fontCache: TGlyphCache;
  bevelHeight: double; state: TCtrlState = csNormal;
  color: TColor32 = clNone32; textColor: TColor32 = clBlack32;
  textOffX: integer = 0; textOffY: integer = 0; closePath: Boolean = true);
var
  dx      : double;
  pp      : TPathsD;
  p2      : TPathD;
begin
  if Length(p) = 0 then
    p2 := Rectangle(rec) else
    p2 := p;

  if color shr 24 > 2 then
    DrawPolygon(Image, p2, frNonZero, color);

  if state = csDisabled then
  begin
    bevelHeight := bevelHeight * 2/3;
    textColor   := clGray32;
  end;

  if state = csPressed then
  begin
    bevelHeight := bevelHeight * 2/3;
    DrawEdge(Image, p2, clSilver32, clWhite32, bevelHeight, closePath);
  end else
    DrawEdge(Image, p2, clWhite32, clSilver32, bevelHeight, closePath);

  if (textColor shr 24 < 3) or (Trim(caption) = '') then Exit;
  pp := fontCache.GetTextGlyphs(rec, caption, taCenter, tvaMiddle);
  if state = csPressed then pp := OffsetPath(pp, bevelHeight, bevelHeight);
  pp := OffsetPath(pp, textOffX, textOffY);

  dx := fontCache.LineHeight/24;
  pp := OffsetPath(pp, -dx, -dx);
  DrawPolygon(Image, pp, frNonZero, clWhite32);
  pp := OffsetPath(pp, dx, dx);
  DrawPolygon(Image, pp, frNonZero, textColor);

  if state = csHot then
  begin
    p2 := SimpleGrow(p2, nil, bevelHeight/2);
    DrawLine(Image, p2, DpiAwareOne*5/4, $80000000, esPolygon);
  end;
end;
//------------------------------------------------------------------------------

procedure DrawBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache;
  bevelHeight: double; state: TCtrlState;
  color: TColor32; textColor: TColor32);
begin
  DrawBtnInternal(Image, rec, nil, caption, fontCache,
    bevelHeight, state, color, textColor);
end;
//------------------------------------------------------------------------------

procedure DrawBevelledCtrl(Image: TImage32; const path: TPathD;
  bevelHeight: double; bevelWidth: integer; color: TColor32);
var
  outer, inner: TPathD;
  bhDiv2: integer;
begin
  bhDiv2 := Ceil(bevelHeight/2);
  outer := SimpleGrow(path, nil, -bhDiv2);
  inner := SimpleGrow(path, nil, -bevelWidth -bhDiv2);
  if color shr 24 > 2 then
    DrawPolygon(Image, outer, frNonZero, color);

  DrawEdge(Image, outer, clWhite32, clSilver32, bevelHeight);
  DrawEdge(Image, inner, clSilver32, clWhite32, bevelHeight);
end;
//------------------------------------------------------------------------------

function DrawRoundedBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache; bevelHeight: double;
  state: TCtrlState; color: TColor32; textColor: TColor32): TPathD;
var
  i,w,h : integer;
begin
  RectWidthHeight(rec, w, h);
  i := Average(w, h);
  Result := RoundRect(rec, i div 3);
  DrawBtnInternal(Image, rec, Result, caption, fontCache,
    bevelHeight, state, color, textColor);
end;
//------------------------------------------------------------------------------

function DrawEllipseBtnCtrl(Image: TImage32; const rec: TRect;
  const caption: string; fontCache: TGlyphCache; bevelHeight: double;
  state: TCtrlState; color: TColor32; textColor: TColor32): TPathD;
begin
  DrawBtnInternal(Image, rec, Ellipse(rec), caption, fontCache,
    bevelHeight, state, color, textColor);
end;
//------------------------------------------------------------------------------

function GetTabOutLine(const rec: TRect; radius: double): TPathD;
var
  p2 : TPathD;
  rec2  : TRectD;
begin
  radius := radius *2;
  with rec do
    rec2 := RectD(Left, Top, Left + radius, Top + radius);
  Result := MakePathD([rec.Left, rec.Bottom]);
  AppendPath(Result, arc(rec2, angle180, angle270));
  OffsetRect(rec2, rec.Width - radius, 0);
  p2 := arc(rec2, angle270, angle0);
  AppendPath(Result, p2);
  AppendPoint(Result, PointD(rec.BottomRight));
end;
//------------------------------------------------------------------------------

function DrawTabCtrl(Image: TImage32;
  const captions: array of string; fontCache: TGlyphCache;
  position: TPoint; bevelHeight: double; selectedIdx: integer;
  tabWidth: integer; tabHeight: integer;
  color: TColor32; textColor: TColor32;
  selColor: TColor32; selTextColor: TColor32): TTabCtrlMetrics;
var
  i, len  : integer;
  bh      : integer;
  bhDiv2  : integer;
  padding : integer;
  totalW  : integer;
  r       : double;
  rec     : TRect;
  img     : TImage32;
  p       : TPathD;
begin
  Result.bounds := NullRect;
  Result.offsets := nil;

  len := Length(captions);
  if len = 0 then Exit;
  if selectedIdx < 0 then selectedIdx := 0
  else if selectedIdx >= len then selectedIdx := len-1;
  bhDiv2 := Ceil(bevelHeight/2);
  bh := bhDiv2 * 2;
  r := fontCache.LineHeight/2;

  SetLength(Result.offsets, len +1);
  Result.offsets[0] := position.X;
  if (tabWidth <= 0) then
  begin
    padding := Round(fontCache.GetTextWidth(' ') *6);
    for i := 1 to len do
      Result.offsets[i] := Result.offsets[i-1] +
        Round(fontCache.GetTextWidth(captions[i-1])) + padding;
  end else
    for i := 1 to len do
      Result.offsets[i] := Result.offsets[i-1] + tabWidth;
  totalW := Result.offsets[len];

  if tabHeight <= 0 then
    tabHeight := Ceil(fontCache.LineHeight * 1.33);

  Result.bounds := Rect(position.X, position.Y,
    position.X + totalW +bhDiv2*2, position.Y + tabHeight +bhDiv2 + bh);

  img := TImage32.Create(totalW +bhDiv2*2, tabHeight +bhDiv2*2 +bh);
  try
    if selectedIdx > 0 then
    begin
      for i := selectedIdx-1 downto 0 do
      begin
        rec := Rect(bhDiv2 + Result.offsets[i] - position.X, bhDiv2 + bh,
          bhDiv2 + Result.offsets[i+1] - position.X, bhDiv2 + bh + tabHeight);
        p := GetTabOutLine(rec, r);
        DrawBtnInternal(img, rec, p, captions[i], fontCache,
          bevelHeight, csNormal, color, textColor, 0, 0, false);
      end;
    end;

    for i := len -1 downto selectedIdx+1 do
    begin
      rec := Rect(bhDiv2 + Result.offsets[i] - position.X, bhDiv2 + bh,
        bhDiv2 + Result.offsets[i+1] - position.X, bhDiv2 + bh + tabHeight);
      p := GetTabOutLine(rec, r);
      DrawBtnInternal(img, rec, p, captions[i], fontCache,
        bevelHeight, csNormal, color, textColor, 0, 0, false);
    end;

    rec := Rect(bhDiv2 + Result.offsets[selectedIdx] - position.X, bhDiv2 + bh,
      bhDiv2 + Result.offsets[selectedIdx+1] - position.X, bhDiv2 + bh + tabHeight);
    img32.Vector.InflateRect(rec, 0, bh);
    p := GetTabOutLine(rec, r);

    DrawBtnInternal(img, rec, p, captions[selectedIdx], fontCache,
      bevelHeight, csNormal, selColor, selTextColor, 0, -bhDiv2, false);

    rec.Left := position.X;
    rec.Top := position.Y;
    rec.Right := rec.Left + img.Width;
    rec.Bottom := rec.Top + img.Height;
    Image.CopyBlend(img, img.Bounds, rec, BlendToAlpha);
  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawEditCtrl(Image: TImage32; const rect: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32);
begin
  Image.FillRect(rect, color);
  DrawEdge(Image, rect, clSilver32, clWhite32, bevelHeight);
end;
//------------------------------------------------------------------------------

procedure DrawCheckboxCtrl(Image: TImage32; const rec: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32;
  state: TCtrlState = csNormal; triState: TTriState = tsNo);
var
  i,w,h : integer;
  rec2  : TRect;
  pw    : double;
  c     : TColor32;
begin
  RectWidthHeight(rec, w, h);
  i := min(w, h);
  with rec do
    rec2 := Rect(Left, Top, Left + i, Top + i);
  Image.FillRect(rec2, color);
  DrawEdge(Image, rec2, clSilver32, clWhite32, bevelHeight);

  if state = csDisabled then triState := tsUnknown;

  case triState of
    tsUnknown : c := csPaleGray32;
    tsYes     : if state = csHot then c := csLimeGreen32 else c := clGreen32;
    else Exit;
  end;

  pw := i/5;
  i := Ceil(i/4);
  Img32.Vector.InflateRect(rec2, -i, -i);
  with rec2 do
  begin
    DrawLine(Image, PointD(TopLeft), PointD(BottomRight), pw, c);
    DrawLine(Image, PointD(Left, Bottom), PointD(Right, Top), pw, c);
  end;
end;
//------------------------------------------------------------------------------

function DrawRadioCtrl(Image: TImage32; const rec: TRect;
  bevelHeight: double;  color: TColor32 = clWhite32;
  state: TCtrlState = csNormal; triState: TTriState = tsNo): TPathD;
var
  i,w,h : integer;
  rec2  : TRect;
  c     : TColor32;
  p     : TPathD;
begin
  RectWidthHeight(rec, w, h);
  i := min(w, h);
  with rec do
    rec2 := Rect(Left, Top, Left + i, Top + i);
  Result := Ellipse(rec2);
  DrawPolygon(Image, Result, frNonZero, color);
  DrawEdge(Image, Result, clSilver32, clWhite32, bevelHeight);

  if state = csDisabled then triState := tsUnknown;

  case triState of
    tsUnknown : c := csPaleGray32;
    tsYes     : if state = csHot then c := csLimeGreen32 else c := clGreen32;
    else Exit;
  end;

  i := Ceil(i/4);
  Img32.Vector.InflateRect(rec2, -i, -i);
  p := Ellipse(rec2);
  DrawPolygon(Image, p, frNonZero, c);
end;
//------------------------------------------------------------------------------

function DrawPageCtrl(Image: TImage32;
  const captions: array of string; fontCache: TGlyphCache;
  const pageRect: TRect; selectedIdx: integer;
  bevelHeight: double;  tabWidth: integer = 0; tabHeight: integer = 0;
  color: TColor32 = clBtnFace32; textColor: TColor32 = clBlack32;
  selColor: TColor32 = clBtnFace32;
  selTextColor: TColor32 = clMaroon32): TTabCtrlMetrics;
var
  bh  : integer;
  p   : TPathD;
  pt1 : TPoint;
  pt2 : TPoint;
  tcm : TTabCtrlMetrics;
begin
  bh := Ceil(bevelHeight/2);
  tcm := DrawTabCtrl(Image, captions, fontCache,
    Types.Point(pageRect.Left + bh*2, pageRect.Top),
    bevelHeight, selectedIdx, tabWidth, tabHeight,
    color, textColor, selColor, selTextColor);

  pt1.X := tcm.offsets[selectedIdx];
  pt1.Y := tcm.bounds.Bottom;

  pt2.X := tcm.offsets[selectedIdx +1];
  pt2.Y := tcm.bounds.Bottom;

  with tcm.bounds do
  begin
    DrawLine(Image, PointD(Left, Bottom), PointD(Right, Bottom), bh, selColor);
    Image.Clear(Rect(Left, Bottom, pageRect.Right, pageRect.Bottom), selColor);
  end;

  SetLength(p, 6);
  p[0] := PointD(pt2);
  p[1] := PointD(pageRect.Right -bh, tcm.bounds.Bottom);
  p[2] := PointD(pageRect.Right -bh, pageRect.Bottom -bh);
  p[3] := PointD(pageRect.Left +bh, pageRect.Bottom -bh);
  p[4] := PointD(pageRect.Left +bh, tcm.bounds.Bottom);
  p[5] := PointD(pt1);
  DrawEdge(Image, p, clWhite32, clSilver32, bevelHeight, false);
end;
//-------------------------------- ----------------------------------------------

end.
