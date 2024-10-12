unit Img32.Vectorizer;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.6                                                             *
* Date      :  18 September 2024                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2023                                         *
* Purpose   :  Converts raster images to vector paths                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Img32;

function Vectorize(img: TImage32;
  compareColor: TColor32; compareFunc: TCompareFunction;
  colorTolerance: Integer; simplifyTolerance: double = 0.25): TPathsD;

implementation

uses
  SysUtils, Classes, Math, Types, Img32.Extra, Img32.Vector;

type
  PPt = ^TPt;
  TPt = record
    X         : integer;
    Y         : integer;
    next     : PPt;
    prev     : PPt;
  end;

  PActive = ^TActive;
  TActive = record
    pt: PPt;
    isAscending: Boolean;
    next: PActive;
    prev: PActive;
  end;

  TVectorizer = class
  private
    pendingX: integer;
    actives: PActive;
    currActive: PActive;
    paths: TPathsD;
    procedure AddToCurrent(x,y: integer);
    procedure InsertBeforeCurrent(left, right, y: integer);
    procedure InsertAtEnd(left, right, y: integer);
    procedure JoinAndPopCurrent(left, right: PActive);
    procedure AddRange(left, right, y: integer);
  public
    function Execute(const ba: TArrayOfByte; w,h: integer): TPathsD;
  end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

procedure DeletePath(pt: PPt);
var
  tmp: PPt;
begin
  pt.prev.next := nil;
  while Assigned(pt) do
  begin
    tmp := pt;
    pt := pt.next;
    Dispose(tmp);
  end;
end;
//------------------------------------------------------------------------------

function GetVertexCount(pt: PPt): integer;
var
  pt2: PPt;
begin
  Result := 1;
  pt2 := pt.next;
  while (pt2 <> pt) do
  begin
    inc(Result);
    pt2 := pt2.next;
  end;
end;
//------------------------------------------------------------------------------

function ConvertToPathD(pt: PPt): TPathD;
var
  i, len: integer;
begin
  len := GetVertexCount(pt);
  NewPointDArray(Result, len, True);
  for i := 0 to len -1 do
  begin
    Result[i] := PointD(pt.X, pt.Y);
    pt := pt.next;
  end;
end;
//------------------------------------------------------------------------------

function NewPoint(x, y: integer): PPt;
begin
  new(Result);
  Result.X := x;
  Result.Y := y;
  Result.next := Result;
  Result.prev := Result;
end;

//------------------------------------------------------------------------------
// TVectorizer class
//------------------------------------------------------------------------------

procedure TVectorizer.JoinAndPopCurrent(left, right: PActive);
var
  ResultP, ResultN: PActive;
  pt, pt2, ascendPt, descendPt, apPrev, dpNext: PPt;
  path: TPathD;
begin
  ResultN := right.next;
  ResultP := left.prev;
  if Assigned(ResultN) then
    ResultN.prev := ResultP;
  if Assigned(ResultP) then
    ResultP.next := ResultN;

  if ResultP = nil then
    actives := ResultN;

  currActive := ResultN;

  // remove active records while
  // still retaining PPt records
  if left.isAscending then
  begin
    ascendPt := left.pt;
    descendPt := right.pt;
  end else
  begin
    ascendPt := right.pt;
    descendPt := left.pt;
  end;
  Dispose(left);
  Dispose(right);

  pt := NewPoint(ascendPt.X, ascendPt.Y +1);
  ascendPt.prev.next := pt;
  pt.prev := ascendPt.prev;
  pt.next := ascendPt;
  ascendPt.prev := pt;
  ascendPt := pt;

  pt2 := NewPoint(descendPt.X, descendPt.Y +1);
  descendPt.next.prev := pt2;
  pt2.next := descendPt.next;
  pt2.prev := descendPt;
  descendPt.next := pt2;
  descendPt := pt2;

  if (descendPt.next = ascendPt) then
  begin
    // nothing to join since path is now complete
    path := ConvertToPathD(ascendPt);
    AppendPath(paths, path);
    DeletePath(ascendPt);
  end else
  begin
    // joining, though path will remain incomplete
    dpNext := descendPt.next;
    apPrev := ascendPt.prev;

    dpNext.prev := apPrev;
    apPrev.next := dpNext;
    descendPt.next := ascendPt;
    ascendPt.prev := descendPt;
  end;
end;
//------------------------------------------------------------------------------

procedure TVectorizer.InsertBeforeCurrent(left, right, y: integer);
var
  pt1, pt2: PPt;
  a1, a2, prev: PActive;
begin
  new(a1);
  new(a2);
  a1.next := a2;
  a2.prev := a1;
  if assigned(currActive) then
  begin
    prev := currActive.prev;
    if assigned(prev) then
      prev.next := a1 else
      actives := a1;
    a1.prev := prev;
    currActive.prev := a2;
    a1.isAscending := currActive.isAscending;
    a2.isAscending := not currActive.isAscending;
  end else
  begin
    actives := a1;
    a1.prev := nil;
    a1.isAscending := true;
    a2.isAscending := false;
  end;
  a2.next := currActive;
  pt1 := NewPoint(left, y);
  a1.pt := pt1;
  pt2 := NewPoint(right, y);
  a2.pt := pt2;
  pt1.next := pt2;
  pt2.next := pt1;
  pt1.prev := pt2;
  pt2.prev := pt1;
end;
//------------------------------------------------------------------------------

procedure TVectorizer.InsertAtEnd(left, right, y: integer);
var
  pt1, pt2: PPt;
  a1, a2, prev: PActive;
begin
  if not Assigned(actives) then
  begin
    InsertBeforeCurrent(left, right, y);
    Exit;
  end;

  new(a1);
  new(a2);
  a1.next := a2;
  a2.prev := a1;
  prev := actives;
  while Assigned(prev.next) do prev := prev.next;

  prev.next := a1;
  a1.prev := prev;
  a1.isAscending := not prev.isAscending;
  a2.isAscending := prev.isAscending;
  a2.next := nil;

  pt1 := NewPoint(left, y);
  a1.pt := pt1;
  pt2 := NewPoint(right, y);
  a2.pt := pt2;
  pt1.next := pt2;
  pt2.next := pt1;
  pt1.prev := pt2;
  pt2.prev := pt1;
end;
//------------------------------------------------------------------------------

procedure TVectorizer.AddToCurrent(x,y: integer);
var
  prev, next, ap, pt: PPt;
begin
  ap := currActive.pt;
  prev := ap.prev;
  next := ap.next;
  if currActive.isAscending then
  begin
    dec(x);
    if (x = ap.X) and (x = next.X) and (next <> ap) then
    begin
      ap.Y := y; // collinear so just move currActive.pt
      Exit;
    end
    else if (x = ap.X) and (Abs(x - next.X) = 1) then
    begin
      ap.X := x; // 1px corner so just cut the corner
      ap.Y := y;
      Exit;
    end;
    pt := NewPoint(x,y);
    prev.next := pt;
    pt.prev := prev;
    ap.prev := pt;
    pt.next := ap;
  end else
  begin
    if (x = ap.X) and (x = prev.X) and (prev <> ap) then
    begin
      ap.Y := y;
      Exit;
    end;
    pt := NewPoint(x,y);
    next.prev := pt;
    pt.next := next;
    ap.next := pt;
    pt.prev := ap;
  end;
  currActive.pt := pt;
end;
//------------------------------------------------------------------------------

procedure TVectorizer.AddRange(left, right, y: integer);
begin
  // nb: currActive is always the beginning of a range UNLESS
  // pendingX > 0. And then it's always the end of a range.

  if (pendingX > 0) then
  begin
    if (left <= currActive.pt.X)  then
    begin
      // the new range starts before currActive
      // so pendingX must be the start of a hole and
      // 'left' must be the end of that hole.
      InsertBeforeCurrent(pendingX, left, y);
      // and there may now be a series of holes
      while assigned(currActive.next) and
        (right > currActive.next.pt.X) do
          JoinAndPopCurrent(currActive, currActive.next);
      pendingX := right;
      Exit;
    end;

    AddToCurrent(pendingX, y);
    pendingX := 0;
    currActive := currActive.next;
  end;

  if not assigned(currActive) then
  begin
    InsertAtEnd(left, right, y);
    Exit;
  end
  else if (right < currActive.pt.X) then
  begin
    InsertBeforeCurrent(left, right, y);
    Exit;
  end;

  //Assert(currActive.isAscending);

  while assigned(currActive) and (left > currActive.next.pt.X) do
    JoinAndPopCurrent(currActive, currActive.next);

  if assigned(currActive) then
  begin
    if currActive.pt.X <= right then
    begin
      AddToCurrent(left, y);
      currActive := currActive.next;
      while assigned(currActive.next) and
        (right > currActive.next.pt.X) do
          JoinAndPopCurrent(currActive, currActive.next);
      pendingX := right;
    end else
      InsertBeforeCurrent(left, right, y)
  end else
    InsertAtEnd(left, right, y);
end;
//------------------------------------------------------------------------------

function TVectorizer.Execute(const ba: TArrayOfByte; w,h: integer): TPathsD;
var
  i,j, len: integer;
  left: integer;
  pb: PByte;
  isInRange: Boolean;
begin
  Result := nil;
  len := Length(ba);
  if w * h <> len then Exit;
  pb := @ba[0];
  left := 0;
  dec(pb);
  for i := 0 to h -1 do
  begin
    currActive := actives;
    isInRange := false;
    pendingX := 0;
    for j := 0 to w -1 do
    begin
      inc(pb);
      if (pb^ = 0) <> isInRange then Continue;
      isInRange := not isInRange;
      if isInRange then left := j
      else AddRange(left, j, i);
    end;

    if isInRange then AddRange(left, w-1, i);
    if (pendingX > 0) then
    begin
      AddToCurrent(pendingX, i);
      currActive := currActive.next;
    end;
    while Assigned(currActive) do
      JoinAndPopCurrent(currActive, currActive.next);
  end;
  currActive := actives;
  while Assigned(currActive) do
    JoinAndPopCurrent(currActive, currActive.next);
  Result := paths;
end;
//------------------------------------------------------------------------------

function Vectorize(img: TImage32;
  compareColor: TColor32; compareFunc: TCompareFunction;
  colorTolerance: Integer; simplifyTolerance: double = 0.25): TPathsD;
var
  ba: TArrayOfByte;
begin
  ba := GetBoolMask(img, compareColor, compareFunc, colorTolerance);
  with TVectorizer.Create do
  try
    Result := Execute(ba, img.Width, img.Height);
  finally
    free;
  end;
  Result := SimplifyPathsEx(Result, simplifyTolerance);
end;
//------------------------------------------------------------------------------

end.
