{**
 @abstract(@name provides a geometrical line.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWLine;

interface

uses System.SysUtils,
     System.Math,
     UTWVector;

type
    {**
     2D line. A line is a straight set of points that extend in opposite directions without ending
    }
    TWLine2 = record
        private
            m_Point:  TWVector2; // known point on line
            m_Dir:    TWVector2; // line direction
            m_InvDir: TWVector2; // inverted direction (i.e. 1 / dir)

        public
            {**
             Constructor
             @param(point Any point on line)
             @param(dir Line direction)
            }
            constructor Create(const point, dir: TWVector2);

            {**
             Get known point on line
             @returns(Known point on line)
            }
            function GetPoint: TWVector2; inline;

            {**
             Set known point on line
             @param(point Known point on the line)
            }
            procedure SetPoint(const point: TWVector2); inline;

            {**
             Get line direction
             @returns(Line direction)
            }
            function GetDir: TWVector2; inline;

            {**
             Set line direction
             @param(dir Line direction)
            }
            procedure SetDir(const dir: TWVector2); inline;

            {**
             Get line inverted direction
             @returns(Line inverted direction)
            }
            function GetInvDir: TWVector2; inline;

            {**
             Calculate the distance to the line
             @param(point Point from which the distance must be calculated)
             @returns(The distance to line)
            }
            function DistanceTo(const point: TWVector2): Single; inline;

            {**
             Check if the line intersects with another line
             @param(other Other line to check)
             @param(intersection @bold([out]) The intersection point)
             @returns(@true if lines intersect, otherwise @false)
            }
            function Intersect(const other: TWLine2; out intersection: TWVector2): Boolean; inline;

        public
            property Point:  TWVector2 read GetPoint write SetPoint;
            property Dir:    TWVector2 read GetDir   write SetDir;
            property InvDir: TWVector2 read GetInvDir;
    end;

implementation
//---------------------------------------------------------------------------
// TWLine2
//---------------------------------------------------------------------------
constructor TWLine2.Create(const point, dir: TWVector2);
var
    invX, invY: Single;
begin
    // calculate the inverted x dir value
    if (dir.X = 0.0) then
        invX := infinity
    else
        invX := 1.0 / dir.X;

    // calculate the inverted y dir value
    if (dir.Y = 0.0) then
        invY := infinity
    else
        invY := 1.0 / dir.Y;

    m_Point  := point;
    m_Dir    := dir;
    m_InvDir := TWVector2.Create(invX, invY);
end;
//---------------------------------------------------------------------------
function TWLine2.GetPoint: TWVector2;
begin
    Result := m_Point;
end;
//---------------------------------------------------------------------------
procedure TWLine2.SetPoint(const point: TWVector2);
begin
    m_Point := point;
end;
//---------------------------------------------------------------------------
function TWLine2.GetDir: TWVector2;
begin
    Result := m_Dir;
end;
//---------------------------------------------------------------------------
procedure TWLine2.SetDir(const dir: TWVector2);
var
    invX, invY: Single;
begin
    // calculate the inverted x dir value
    if (dir.X = 0.0) then
        invX := infinity
    else
        invX := 1.0 / dir.X;

    // calculate the inverted y dir value
    if (dir.Y = 0.0) then
        invY := infinity
    else
        invY := 1.0 / dir.Y;

    m_Dir    := dir;
    m_InvDir := TWVector2.Create(invX, invY);
end;
//---------------------------------------------------------------------------
function TWLine2.GetInvDir: TWVector2;
begin
    Result := m_InvDir;
end;
//---------------------------------------------------------------------------
function TWLine2.DistanceTo(const point: TWVector2): Single;
var
    delta, lineClosest, closest: TWVector2;
    lineParam, sqrDistance:      Single;
begin
    delta       := point.Sub(m_Point);
    lineParam   := m_Dir.Dot(delta);

    if (lineParam > 0.0) then
        lineClosest := m_Point.Add(m_Dir.Mul(lineParam))
    else
        lineClosest := m_Point;

    closest     := point.Sub(lineClosest);
    sqrDistance := closest.Dot(closest);
    Result      := Sqrt(sqrDistance);
end;
//---------------------------------------------------------------------------
function TWLine2.Intersect(const other: TWLine2; out intersection: TWVector2): Boolean;
var
    delta, xDir, t: TWVector2;
begin
    delta := other.m_Point.Sub(m_Point);
    xDir  := m_Dir.Cross(other.m_Dir);

    // if the dir cross result is 0, then the two lines are either collinear or parallel
    if (xDir.IsZero) then
    begin
        // are lines collinear?
        if (DistanceTo(other.m_Point) = 0.0) then
        begin
            // lines are collinear, there is an infinity of intersections
            intersection := TWVector2.Create(infinity, infinity);
            Exit(True);
        end;

        // lines are parallel, no possible intersection
        Exit(False);
    end;

    // calculate the intersection point
    t            := delta.Cross(other.m_Dir).Divide(xDir);
    intersection := m_Point.Add(t.Mul(m_Dir));
    Result       := True;
end;
//---------------------------------------------------------------------------

end.
