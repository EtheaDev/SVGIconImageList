unit Img32.Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  26 June 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Wrapper module for the Clipper library                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Clipper, Clipper.Core, Clipper.Engine, Clipper.Offset,
  Img32, Img32.Draw, Img32.Vector;

//nb: InflatePath assumes that there's consistent winding where
//outer paths wind in one direction and inner paths in the other

function InflatePath(const path: TPathD; delta: Double;
  joinStyle: TJoinStyle = jsAuto; endStyle: TEndStyle = esPolygon;
  miterLimit: double = 2.0; arcTolerance: double = 0.0;
  minEdgeLength: double = 0.25): TPathsD;

function InflatePaths(const paths: TPathsD; delta: Double;
  joinStyle: TJoinStyle = jsAuto; endStyle: TEndStyle = esPolygon;
  miterLimit: double = 2.0; arcTolerance: double = 0.0;
  minEdgeLength: double = 0): TPathsD;

//UnionPolygon: removes self-intersections
function UnionPolygon(const polygon: TPathD;
  fillRule: TFillRule): TPathsD;

function UnionPolygons(const polygons: TPathsD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygon1, polygon2: TPathD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD; overload;

function IntersectPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

function DifferencePolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePath(const path: TPathD;
  delta: Double; joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double; arcTolerance: double; minEdgeLength: double): TPathsD;
var
  paths: TPathsD;
begin
  setLength(paths, 1);
  paths[0] := path;
  Result := InflatePaths(paths, delta, joinStyle, endStyle,
    miterLimit, arcTolerance, minEdgeLength);
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPathsD;
  delta: Double; joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double; arcTolerance: double; minEdgeLength: double): TPathsD;
var
  jt: Clipper.Offset.TJoinType;
  et: TEndType;
begin
  case joinStyle of
    jsSquare: jt := jtSquare;
    jsMiter:  jt :=  jtMiter;
    jsRound:  jt := jtRound;
    else if endStyle = esRound then jt := jtRound
    else jt := jtSquare;
  end;
  case endStyle of
    esButt: et := etButt;
    esSquare: et := etSquare;
    esRound: et := etRound;
    else et := etPolygon;
  end;
  Result := Img32.TPathsD(Clipper.InflatePaths(
    Clipper.Core.TPathsD(paths), delta, jt, et));
end;
//------------------------------------------------------------------------------

function UnionPolygon(const polygon: TPathD; fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathD(polygon));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygon1, polygon2: TPathD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathD(polygon1));
    AddClip(Clipper.Core.TPathD(polygon2));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function IntersectPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctIntersection,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function DifferencePolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctDifference,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

end.
