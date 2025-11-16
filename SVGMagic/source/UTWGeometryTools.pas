{**
 @abstract(@name provides several geometrical tools.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWGeometryTools;

interface

uses System.Types,
     System.SysUtils,
     System.Math,
     UTWHelpers,
     UTWVector;

type
    {**
     Geometry tools
    }
    TWGeometryTools = record
        public type
            {**
             Hit edge enumeration
             @value(IE_Top Indicates that top edge was hit)
             @value(IE_Left Indicates that left edge was hit)
             @value(IE_Bottom Indicates that bottom edge was hit)
             @value(IE_Right Indicates that right edge was hit)
             @value(IE_Center Indicates that center was hit)
            }
            IEEdge =
            (
                IE_Top,
                IE_Left,
                IE_Bottom,
                IE_Right,
                IE_Center
            );

            IEEdges = set of IEEdge;

            {**
             Invalid size result enumeration
             @value(IE_IS_None Indicates that rect contains no invalid size)
             @value(IE_IS_Width Indicates that rect width is invalid)
             @value(IE_IS_Height Indicates that rect height is invalid)
             @value(IE_IS_Both Indicates that both rect width and height are invalid)
            }
            IEInvalidSize =
            (
                IE_IS_None,
                IE_IS_Width,
                IE_IS_Height,
                IE_IS_Both
            );

            {**
             Use VCL rectangle structure as margins container
            }
            IMargins = TRect;

            IPoints = array of TWVector2;

        private
            {**
             Calculate angle between 2 vectors
             @param(ux First vector x position)
             @param(uy First vector y position)
             @param(vx Second vector x position)
             @param(vy Second vector y position)
             @returns(Angle between vectors)
            }
            class function CalcVectorAngle(const ux, uy, vx, vy: Double): Double; static;

        public
            {**
             Get hit edges
             @param(area Area to check)
             @param(margins Edge margins)
             @param(x x coordinate)
             @param(y y coordinate)
             @returns(Hit edges, IE_None if no edge has been hit)
             @br @bold(NOTE) Area and x/y coordinates should be in the same coordinate system
             @br @bold(NOTE) Result can be a combination of many flags, e.g. IE_Top | IE_Right
            }
            class function GetHitEdges(const area: TRect; const margins: IMargins;
                    x, y: Integer): IEEdges; inline; static;

            {**
             Get x axis hit edge
             @param(area Area to check)
             @param(margins Edge margins)
             @param(x x coordinate)
             @returns(Hit edge, IE_None if no edge has been hit)
             @br @bold(NOTE) Area and x coordinate should be in the same coordinate system
            }
            class function GetHitEdgeX(const area: TRect; const margins: IMargins;
                    x: Integer): IEEdges; static;

            {**
             Get y axis hit edge
             @param(area Area to check)
             @param(margins Edge margins)
             @param(y y coordinate)
             @returns(Hit edge, IE_None if no edge has been hit)
             @br @bold(NOTE) Area and y coordinate should be in the same coordinate system
            }
            class function GetHitEdgeY(const area: TRect; const margins: IMargins;
                    y: Integer): IEEdges; static;

            {**
             Check if rect coordinates are valid (i.e. left is before right and top is before bottom)
             @param(rect Rect to validate)
             @returns(IE_None on success, invalid size otherwise)
            }
            class function Validate(const rect: TRect): IEInvalidSize; inline; static;

            {**
             Get rect center point
             @param(rect Rectangle)
             @returns(Center point)
            }
            class function GetCenter(const rect: TRect): TPoint; inline; static;

            {**
             Check if a point is inside a rectangle
             @param(x Point x coordinate)
             @param(y Point y coordinate)
             @param(rect Rectangle)
             @returns(@true if point is inside the rectangle, otherwise @false)
             @br @bold(NOTE) Unlike the TRect::PtInRect() or TRect::Contains() functions, the point
                             is considered to be inside the rect if located on the right and/or
                             bottom edges
            }
            class function PointInRect(x, y: Integer; const rect: TRect): Boolean; inline; static;

            {**
             Check if 2 rectangles overlap
             @param(r1 First rectangle to check)
             @param(r2 Second rectangle to check)
             @returns(@true if rectangles overlap, otherwise @false)
             @br @bold(NOTE) Unlike the TRect::Intersect() or TRect::IntersectsWith() functions, the
                             point is considered to be inside the rect if located on the right and/or
                             bottom edges
            }
            class function Intersect(const r1, r2: TRect): Boolean; inline; static;

            {**
             Check if first rectangle is completely inside the second
             @param(r1 First rectangle to check)
             @param(r2 Second rectangle to check with)
             @returns(@true if first rectangle is completely inside the second, otherwise @false)
             @br @bold(NOTE) Be careful, the rects order is important. In theory, r1 should always
                             be smaller than r2, and that's not because r1 may be fully contained in
                             r2 that the opposite is true
             @br @bold(NOTE) Unlike the TRect::Contains() function, the first rect is considered to
                             be inside the second if both left top and right bottom edges match
            }
            class function Inside(const r1, r2: TRect): Boolean; inline; static;

            {**
             Check if a point is inside an ellipse
             @param(x Point x coordinate)
             @param(y Point y coordinate)
             @param(h Ellipse center on x coordinate)
             @param(k Ellipse center on y coordinate)
             @param(rx Ellipse radius on x coordinate)
             @param(ry Ellipse radius on y coordinate)
             @param(onBoundary @bold([out]) If true, the point is exactly on the boundary of the ellipse)
             @returns(@true if point is inside the ellipse or on its boundary, otherwise @false)
            }
            class function PointInEllipse(x, y, h, k, rx, ry: Single; out onBoundary: Boolean): Boolean; inline; static;

            {**
             Convert degrees to radians
             @param(angle Angle in degrees)
             @returns(Angle in radians)
            }
            class function DegToRad(const angle: Double): Double; inline; static;

            {**
             Convert radians to degrees
             @param(angle Angle in radians)
             @returns(Angle in degrees)
            }
            class function RadToDeg(const angle: Double): Double; inline; static;

            {**
             Convert gradians to radians
             @param(angle Angle in gradians)
             @returns(Angle in radians)
             @br @bold(NOTE) A gradian represents one hundredth of the right angle. One full circle equals 400grad
            }
            class function GradToRad(const angle: Double): Double; inline; static;

            {**
             Convert radians to gradians
             @param(angle Angle in radians)
             @returns(Angle in gradians)
             @br @bold(NOTE) A gradian represents one hundredth of the right angle. One full circle equals 400grad
            }
            class function RadToGrad(const angle: Double): Double; inline; static;

            {**
             Convert turns to radians
             @param(angle Angle in turns)
             @returns(Angle in radians)
             @br @bold(NOTE) A turn represents an angle in a number of turns. One full circle is 1 turn
            }
            class function TurnToRad(const angle: Double): Double; inline; static;

            {**
             Convert radians to turns
             @param(angle Angle in radians)
             @returns(Angle in turns)
             @br @bold(NOTE) A turn represents an angle in a number of turns. One full circle is 1 turn
            }
            class function RadToTurn(const angle: Double): Double; inline; static;

            {**
             Calculate intersection point between 2 lines
             @param(p1 Line 1 start position)
             @param(p2 Line 1 end position)
             @param(p3 Line 2 start position)
             @param(p4 Line 2 end position)
             @param(intersection @bold([out]) Intersection point)
             @returns(@true if lines intersect somewhere, @false if lines are parallel and never intersect)
            }
            class function GetLineIntersectPoint(const p1, p2, p3, p4: TWVector2;
                    out intersection: TWVector2): Boolean; static;

            {**
             Calculate a point on a line
             @param(lineStart Line start coordinates)
             @param(lineEnd Line end coordinates)
             @param(position Position of the point to find in percent (between 0.0f and 1.0f))
             @returns(Point coordinates on the line)
            }
            class function GetPointOnLine(const lineStart, lineEnd: TWVector2;
                    position: Single): TWVector2; inline; static;

            {**
             Calculate a point on a quadratic Bezier curve
             @param(curveStart Bezier curve start coordinate)
             @param(curveEnd Bezier curve end coordinate)
             @param(control Bezier curve control point coordinate)
             @param(position Position of the point to find in percent (between 0.0f and 1.0f))
             @returns(Quadratic Bezier point coordinate matching with position)
            }
            class function GetQuadraticBezierPoint(const curveStart, curveEnd, control: TWVector2;
                    position: Single): TWVector2; static;

            {**
             Calculate a point on a cubic Bezier curve
             @param(start Bezier curve start coordinate)
             @param(end Bezier curve end coordinate)
             @param(control1 First Bezier curve control point coordinate)
             @param(control2 Second Bezier curve control point coordinate)
             @param(position Position of the point to find in percent (between 0.0f and 1.0f))
             @returns(Cubic Bezier point coordinate matching with position)
            }
            class function GetCubicBezierPoint(const curveStart, curveEnd, control1, control2: TWVector2;
                    position: Single): TWVector2; static;

            {**
             Get points composing an arc
             @param(pt1 Start point (on the arc))
             @param(pt2 End point (on the arc))
             @param(angle Inclination angle of the ellipse at which the arc belongs in radians)
             @param(rx Ellipse x radius at which the arc belongs)
             @param(ry Ellipse y radius at which the arc belongs)
             @param(sweep If @true, sweep is clockwise, otherwise sweep is counter clockwise)
             @param(largeArc If @true, the arc is large, i.e. the opposite of the sweep angle will be used)
             @param(pointCount Number of wished point tht will compose the arc (if possible))
             @param(points @bold([out]) Points composing the arc)
             @param(isLine @bold([out]) If @true, the arc is a simple line (e.g. if rx or ry is equal to 0))
            }
            class procedure GetArc(const pt1, pt2: TWVector2; angle, rx, ry: Double;
                    sweep, largeArc: Boolean; pointCount: NativeUInt; out points: IPoints;
                    out isLine: Boolean); static;
    end;

implementation
//---------------------------------------------------------------------------
class function TWGeometryTools.CalcVectorAngle(const ux, uy, vx, vy: Double): Double;
var
    ta, tb: Double;
begin
    ta := ArcTan2(uy, ux);
    tb := ArcTan2(vy, vx);

    if (tb >= ta) then
        Result := tb - ta
    else
        Result := 2.0 * PI - (ta - tb);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetHitEdges(const area: TRect; const margins: IMargins;
        x, y: Integer): IEEdges;
begin
    // get horizontal and vertical hit edge
    Result := GetHitEdgeX(area, margins, x) + GetHitEdgeY(area, margins, y);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetHitEdgeX(const area: TRect; const margins: IMargins;
        x: Integer): IEEdges;
var
    y:        Integer;
    edgeRect: TRect;
begin
    // check if x coordinate is out of area
    if ((x < area.Left) or (x > area.Right)) then
        Exit;

    // calculate y coordinate, this value is only needed for PointInRect function
    y := area.Top + (area.Height div 2);

    // is there left edge to check?
    if (margins.Left > 0) then
    begin
        // calculate left edge area
        edgeRect       := area;
        edgeRect.Right := area.Left + margins.Left;

        // is x coordinate inside left edge?
        if (PointInRect(x, y, edgeRect)) then
            Exit([IE_Left]);
    end;

    // is there right edge to check?
    if (margins.Right > 0) then
    begin
        // calculate right edge area
        edgeRect      := area;
        edgeRect.Left := area.Right - margins.Right;

        // is x coordinate inside right edge?
        if (PointInRect(x, y, edgeRect)) then
            Exit([IE_Right]);
    end;

    // x coordinate is inside center area
    Result := [IE_Center];
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetHitEdgeY(const area: TRect; const margins: IMargins;
        y: Integer): IEEdges;
var
    x:        Integer;
    edgeRect: TRect;
begin
    // check if y coordinate is out of area
    if ((y < area.Top) or (y > area.Bottom)) then
        Exit;

    // calculate x coordinate, this value is only needed for PointInRect function
    x := area.Left + (area.Width div 2);

    // is there top edge to check?
    if (margins.Top > 0) then
    begin
        // calculate top edge area
        edgeRect        := area;
        edgeRect.Bottom := area.Top + margins.Top;

        // is y coordinate inside top edge?
        if (PointInRect(x, y, edgeRect)) then
            Exit([IE_Top]);
    end;

    // is there bottom edge to check?
    if (margins.Bottom > 0) then
    begin
        // calculate bottom edge area
        edgeRect     := area;
        edgeRect.Top := area.Bottom - margins.Bottom;

        // is y coordinate inside bottom edge?
        if (PointInRect(x, y, edgeRect)) then
            Exit([IE_Bottom]);
    end;

    // y coordinate is inside center area
    Result := [IE_Center];
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.Validate(const rect: TRect): IEInvalidSize;
begin
    // is right position before left or bottom position before top?
    if (rect.Right < rect.Left) then
    begin
        if (rect.Bottom < rect.Top) then
            Exit(IE_IS_Both);

        Exit(IE_IS_Width);
    end;

    if (rect.Bottom < rect.Top) then
        Result := IE_IS_Height
    else
        Result := IE_IS_None;
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetCenter(const rect: TRect): TPoint;
begin
    Result := TPoint.Create(((rect.Left + rect.Right) div 2), ((rect.Top + rect.Bottom) div 2));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.PointInRect(x, y: Integer; const rect: TRect): Boolean;
begin
    Result := ((x >= rect.Left) and (y >= rect.Top) and (x <= (rect.Left + rect.Width))
            and (y <= (rect.Top + rect.Height)));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.Intersect(const r1, r2: TRect): Boolean;
begin
    Result := not((r1.Left > r2.Right) or (r1.Right < r2.Left) or (r1.Top > r2.Bottom)
            or (r1.Bottom < r2.Top));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.Inside(const r1, r2: TRect): Boolean;
begin
    Result := (PointInRect(r1.Left, r1.Top, r2) and PointInRect(r1.Right, r1.Bottom, r2));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.PointInEllipse(x, y, h, k, rx, ry: Single; out onBoundary: Boolean): Boolean;
var
    r: Single;
begin
    r          := (Power(x - h, 2.0) / Power(rx, 2.0)) + (Power(y - k, 2.0) / Power(ry, 2.0));
    onBoundary := (r = 1.0);
    Result     := (r <= 1.0);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.DegToRad(const angle: Double): Double;
begin
    Result := ((angle * PI) / 180.0);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.RadToDeg(const angle: Double): Double;
begin
    Result := ((angle * 180.0) / PI);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GradToRad(const angle: Double): Double;
begin
    Result := ((angle * PI) / 200.0);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.RadToGrad(const angle: Double): Double;
begin
    Result := ((angle * 200.0) / PI);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.TurnToRad(const angle: Double): Double;
begin
    Result := (angle * PI * 2.0);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.RadToTurn(const angle: Double): Double;
begin
    Result := (angle / (PI * 2.0));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetLineIntersectPoint(const p1, p2, p3, p4: TWVector2;
        out intersection: TWVector2): Boolean;
var
    r, s, t, p3p1, rxs: TWVector2;
begin
    // calculate the direction for each line and the cross products between lines
    r    := p2.Sub(p1);
    s    := p4.Sub(p3);
    p3p1 := p3.Sub(p1);
    rxs  := r.Cross(s);

    // if the cross result of r and s is 0, then the two lines are either collinear or parallel
    if (rxs.IsZero()) then
        Exit(False);

    // calculate the intersection point
    t            := p3p1.Cross(s).Divide(rxs);
    intersection := p1.Add(t.Mul(r));

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetPointOnLine(const lineStart, lineEnd: TWVector2;
        position: Single): TWVector2;
begin
    Result := lineStart.Add(lineEnd.Sub(lineStart).Mul(position));
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetQuadraticBezierPoint(const curveStart, curveEnd, control: TWVector2;
        position: Single): TWVector2;
var
    p0p1, p1p2: TWVector2;
begin
    // point p0p1 is the point on the line formed by the start position and the control point, and
    // point p1p2 is the point on the line formed by the control point and the line end
    p0p1 := GetPointOnLine(curveStart, control,  position);
    p1p2 := GetPointOnLine(control,    curveEnd, position);

    // the resulting point is the point found on the intermediate segment (p0p1 to p1p2)
    Result := GetPointOnLine(p0p1, p1p2, position);
end;
//---------------------------------------------------------------------------
class function TWGeometryTools.GetCubicBezierPoint(const curveStart, curveEnd, control1, control2: TWVector2;
        position: Single): TWVector2;
var
    p0p1, p1p2, p2p3: TWVector2;
begin
    // point p0p1 is the point on the line formed by the start position and the first control point,
    // point p1p2 is the point on the line formed by the first control point and the second control
    // point, and point p2p3 is the point on the line formed by the second control point and the
    // line end
    p0p1 := GetPointOnLine(curveStart, control1, position);
    p1p2 := GetPointOnLine(control1,   control2, position);
    p2p3 := GetPointOnLine(control2,   curveEnd, position);

    // the resulting point is the quadratic bezier point found on the intermediate segments (p0p1 to
    // p1p2 and p1p2 to p2p3)
    Result := GetQuadraticBezierPoint(p0p1, p2p3, p1p2, position);
end;
//---------------------------------------------------------------------------
class procedure TWGeometryTools.GetArc(const pt1, pt2: TWVector2; angle, rx, ry: Double;
        sweep, largeArc: Boolean; pointCount: NativeUInt; out points: IPoints;
        out isLine: Boolean);
var
    sinPhi,
    cosPhi,
    x1Dash,
    y1Dash,
    numerator,
    root,
    s,
    cxDash,
    cyDash,
    offset,
    theta,
    dtheta,
    delta,
    nextTheta,
    nextCosTheta,
    nextSinTheta: Double;
    i:            NativeUInt;
    center:       TWVector2;
begin
    isLine := False;

    // too few points to calculate an ellipse?
    if (pointCount < 2) then
        Exit;

    // if endpoints are identical the elliptical arc segment should be omitted entirely
    if (pt1.IsEqual(pt2)) then
        Exit;

    // if rx = 0 or ry = 0 then arc should be treated as a straight line segment joining the endpoints
    if ((rx = 0.0) or (ry = 0.0)) then
    begin
        isLine := True;

        SetLength(points, 2);
        points[0] := pt1;
        points[1] := pt2;
        Exit;
    end;

    // in accordance to http://www.w3.org/TR/SVG/implnote.html#ArcOutOfRangeParameters
    rx := Abs(rx);
    ry := Abs(ry);

    // limit angle between 0 and 360°
    angle := TWMathHelper.ExtMod(angle, PI * 2.0);

    sinPhi    :=  Sin(angle);
    cosPhi    :=  Cos(angle);
    x1Dash    :=  cosPhi * (pt1.X - pt2.X) / 2.0 + sinPhi * (pt1.Y - pt2.Y) / 2.0;
    y1Dash    := -sinPhi * (pt1.X - pt2.X) / 2.0 + cosPhi * (pt1.Y - pt2.Y) / 2.0;
    numerator :=  (rx * rx * ry * ry) - (rx * rx * y1Dash * y1Dash) - (ry * ry * x1Dash * x1Dash);

    // no valid solution to calculate arc? (i.e. the ellipse is not big enough to reach the end
    // position from start, and should be scaled up uniformly until find a valid solution)
    if (numerator < 0.0) then
    begin
        // find factor s, such that numerator with rx' = s * rx and ry' = s * ry becomes 0
        s := Sqrt(1.0 - numerator / (rx * rx * ry * ry));

        rx   := rx * s;
        ry   := ry * s;
        root := 0.0;
    end
    else
    begin
        if (largeArc = sweep) then
            offset := -1.0
        else
            offset :=  1.0;

        root := offset * Sqrt(numerator / ((rx * rx * y1Dash * y1Dash) + (ry * ry * x1Dash * x1Dash)));
    end;

    cxDash :=  root * rx * y1Dash / ry;
    cyDash := -root * ry * x1Dash / rx;

    // calculate ellipse center
    center := TWVector2.Create(cosPhi * cxDash - sinPhi * cyDash + (pt1.X + pt2.X) / 2.0,
                               sinPhi * cxDash + cosPhi * cyDash + (pt1.Y + pt2.Y) / 2.0);

    theta  := CalcVectorAngle(1.0, 0.0, (x1Dash - cxDash) / rx, (y1Dash - cyDash) / ry);
    dTheta := CalcVectorAngle((x1Dash - cxDash) / rx, (y1Dash - cyDash) / ry,
            (-x1Dash - cxDash) / rx, (-y1Dash - cyDash) / ry);

    if ((not sweep) and (dTheta > 0.0)) then
        dTheta := dTheta - (2.0 * PI)
    else
    if (sweep and (dTheta < 0.0)) then
        dTheta := dTheta + (2.0 * PI);

    delta := dTheta / (pointCount - 1);

    SetLength(points, pointCount);

    points[0] := pt1;

    // iterate through points to create
    for i := 1 to pointCount - 2 do
    begin
        nextTheta    := theta + delta;
        nextCosTheta := Cos(nextTheta);
        nextSinTheta := Sin(nextTheta);

        // calculate next point
        points[i] := TWVector2.Create((cosPhi * rx * nextCosTheta) - (sinPhi * ry * nextSinTheta) + center.X,
                                      (sinPhi * rx * nextCosTheta) + (cosPhi * ry * nextSinTheta) + center.Y);

        // do next segment
        theta := nextTheta;
    end;

    points[pointCount - 1] := pt2;
end;
//---------------------------------------------------------------------------

end.
