{**
 @abstract(@name provides several tools to manage gradient and generate GDI+ brushes.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWGDIPlusGradient;

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     System.Math,
     System.UITypes,
     System.Generics.Collections,
     Winapi.Windows,
     Winapi.GDIPAPI,
     Winapi.GDIPObj,
     UTWSmartPointer,
     UTWColor,
     UTWFillAndStroke,
     UTWSize,
     UTWPoint,
     UTWLine,
     UTWRect,
     UTWVector,
     UTWMatrix,
     UTWGeometryTools,
     UTWHelpers,
     UTWRendererCommon;

type
    {**
     GDI+ gradient class
     @author(JMR)
    }
    TWGDIPlusGradient = class
        private type
            {**
             Struct containing 2 points and the distance separating them
            }
            IPointsDist = record
                m_Start:  TWVector2;
                m_End:    TWVector2;
                m_Length: Single;
            end;

        public type
            {**
             Gradient stop
            }
            IStop = class
                private
                    m_Color:  TWColor;
                    m_Offset: Single;

                protected
                    {**
                     Get color
                     @returns(Color)
                    }
                    function GetColor: PWColor; virtual;

                public
                    constructor Create; overload; virtual;

                    {**
                     Constructor
                     @param(color Stop color)
                     @param(offset Stop offset, in percent (between 0.0f and 1.0f))
                    }
                    constructor Create(const color: TWColor; offset: Single); overload; virtual;

                    {**
                     Check if gradient stop content is equal to another gradient stop
                     @param(pOther Other gradient stop to compare with)
                     @returns(@true if gradient stops are equals, otherwise @false)
                    }
                    function IsEqual(const pOther: IStop): Boolean; inline;

                    {**
                     Check if gradient stop content differs from another gradient stop
                     @param(pOther Other gradient stop to compare with)
                     @returns(@true if gradient stops differ, otherwise @false)
                    }
                    function Differs(const pOther: IStop): Boolean; inline;

                    {**
                     Set color
                     @param(pColor Color to set)
                    }
                    procedure SetColor(const pColor: PWColor); virtual;

                public
                    property Color:  PWColor read GetColor;
                    property Offset: Single  read m_Offset write m_Offset;
            end;

        private type
            IStops    = TObjectList<IStop>;
            IVectors  = array of TWVector2;
            IGpColors = array of TGpColor;
            IOffsets  = array of Single;

        private
            m_StartColor:   TWColor;
            m_EndColor:     TWColor;
            m_StartPoint:   TWPointF;
            m_EndPoint:     TWPointF;
            m_WrapMode:     EWrapMode;
            m_pStops:       IStops;
            m_RadialMatrix: TWMatrix3x3;

            {**
             Set linear gradient brush
             @param(pBrush Source brush)
            }
            procedure SetLinearGradient(const pBrush: TWLinearGradientBrush);

            {**
             Set radial gradient brush
             @param(pBrush Source brush)
            }
            procedure SetRadialGradient(const pBrush: TWRadialGradientBrush);

            {**
             Calculate linear gradient stop offset
             @param(startPos Linear gradient start position)
             @param(endPos Linear gradient end position)
             @param(pos Gradient stop position)
             @returns(Gradient stop offset in percent (between 0.0 and 1.0))
            }
            function CalculateLinearGradientStopOffset(const startPos, endPos, pos: TWPointF): Single;

            {**
             Create linear gradient brush supporting clamping mode
             @param(gradientVector Gradient vector)
             @param(start Gradient start position (in pixels, between rect left top and right bottom))
             @param(end Gradient end position (in pixels, between rect left top and right bottom))
             @param(startColor Gradient start color)
             @param(endColor Gradient end color)
             @returns(Brush, @nil if could not be created)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            function CreateLinearClampBrush(const pGradientVector: TWLinearGradientVector;
                    const startPoint, endPoint: TGpPointF; const startColor, endColor: TGpColor): TGpBrush;

            {**
             Get the most distant points in a point list
             @param(points Points in which the most distant should be found)
             @return(A structure containing the most distant points and the length between them)
             @br @bold(NOTE) The point list should contain at least 2 points
            }
            function GetMostDistantPoints(const points: IVectors): IPointsDist;

            {**
             Populate stop arrays
             @param(colors @bold([in, out]) Color array to populate)
             @param(offsets @bold([in, out]) Offset array to populate)
             @param(start Start offset)
             @param(count Stop count)
             @param(invert If @true, the arrays will be populated in the reverse order, from last
                           item to first. Offsets will also be inverted)
            }
            procedure PopulateStopArrays(var colors: IGpColors; var offsets: IOffsets; start,
                    count: Cardinal; invert: Boolean);

            {**
             Delete an item from a vector list
             @param(vectors Vector list from which item should be removed)
             @param(index Item index to remove)
            }
            procedure DeleteItem(var vectors: IVectors; index: NativeUInt);

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
             @param(pStartColor Gradient start color)
             @param(pEndColor Gradient end color)
             @param(wrapMode Wrap mode)
            }
            constructor Create(const startColor, endColor: TWColor; wrapMode: EWrapMode); overload; virtual;

            {**
             Constructor
             @param(pStartColor Gradient start color)
             @param(pEndColor Gradient end color)
             @param(startPoint Gradient start point, in pixels, relatively to target rect left top corner)
             @param(endPoint Gradient end point, in pixels, relatively to target rect left top corner)
             @param(wrapMode Wrap mode)
             @br @bold(NOTE) This constructor should be used to build a linear gradient
            }
            constructor Create(const startColor, endColor: TWColor; const startPoint, endPoint: TWPointF;
                    wrapMode: EWrapMode); overload; virtual;

            {**
             Constructor
             @param(startColor Gradient start color)
             @param(endColor Gradient end color)
             @param(centerPoint Center point, in pixels, relatively to target rect left top corner)
             @param(focusPoint Focus point, in pixels, relatively to target rect left top corner)
             @br @bold(NOTE) This constructor should be used to build a radius gradient
            }
            constructor Create(const startColor, endColor: TWColor; const centerPoint,
                    focusPoint: TWPointF); overload; virtual;

            {**
             Constructor
             @param(startColor Gradient start color)
             @param(endColor Gradient end color)
             @param(focusPoint Focus point, in pixels, relatively to target rect left top corner)
             @br @bold(NOTE) This constructor should be used to build a path gradient
            }
            constructor Create(const startColor, endColor: TWColor; const focusPoint: TWPointF); overload; virtual;

            {**
             Constructor
             @param (pBrush Brush)
            }
            constructor Create(const pBrush: TWBrush); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Create the gradient from a brush
             @param(pBrush Brush to create from)
            }
            procedure FromBrush(const pBrush: TWBrush); overload; virtual;
            procedure FromBrush(const pBrush: TWLinearGradientBrush); overload; virtual;
            procedure FromBrush(const pBrush: TWRadialGradientBrush); overload; virtual;

            {**
             Get gradient start color (or center color for path gradient)
             @returns(Color)
            }
            function GetStartColor: PWColor; virtual;

            {**
             Set gradient start color (or center color for path gradient)
             @param(pColor Color)
            }
            procedure SetStartColor(const pColor: PWColor); virtual;

            {**
             Get gradient end color (or surround color for path gradient)
             @returns(Color)
            }
            function GetEndColor: PWColor; virtual;

            {**
             Set gradient end color (or surround color for path gradient)
             @param(pColor Color)
            }
            procedure SetEndColor(const pColor: PWColor); virtual;

            {**
             Get gradient start point (for linear gradients)
             @returns(Point, in pixels, relatively to target rect left top corner)
            }
            function GetStartPoint: TWPointF; virtual;

            {**
             Set gradient start point (for linear gradients)
             @param(point Point, in pixels, relatively to target rect left top corner)
            }
            procedure SetStartPoint(const point: TWPointF); virtual;

            {**
             Get gradient end point (for linear gradients)
             @returns(Point, in pixels, relatively to target rect left top corner)
            }
            function GetEndPoint: TWPointF; virtual;

            {**
             Set gradient end point (for linear gradients)
             @param(point Point, in pixels, relatively to target rect left top corner)
            }
            procedure SetEndPoint(const point: TWPointF); virtual;

            {**
             Get gradient center point (for radial or path gradients)
             @returns(Point, in pixels, relatively to target rect left top corner)
            }
            function GetCenterPoint: TWPointF; virtual;

            {**
             Set gradient center point (for radial or path gradients)
             @param(point Point, in pixels, relatively to target rect left top corner)
            }
            procedure SetCenterPoint(const point: TWPointF); virtual;

            {**
             Get gradient focus point (for radial or path gradients)
             @returns(Point, in pixels, relatively to target rect left top corner)
            }
            function GetFocusPoint: TWPointF; virtual;

            {**
             Set gradient focus point (for radial or path gradients)
             @param(point Point, in pixels, relatively to target rect left top corner)
            }
            procedure SetFocusPoint(const point: TWPointF); virtual;

            {**
             Add gradient stop
             @param(pStop Gradient stop to add)
            }
            procedure AddStop(const pStop: IStop); overload; virtual;

            {**
             Add gradient stop
             @param(pColor Gradient stop color)
             @param(offset Offset where gradient stop is located, in percent (between 0.0 and 1.0))
            }
            procedure AddStop(const pColor: PWColor; offset: Single); overload; virtual;

            {**
             Insert gradient stop
             @param(index Index where stop will be inserted)
             @param(stop Gradient stop to insert)
            }
            procedure InsertStop(index: NativeInt; const pStop: IStop); overload; virtual;

            {**
             Insert gradient stop
             @param(index Index where stop will be inserted)
             @param(pColor Gradient stop color)
             @param(offset Offset where gradient stop is located, in percent (between 0.0 and 1.0))
            }
            procedure InsertStop(index: NativeInt; const pColor: PWColor; offset: Single); overload; virtual;

            {**
             Delete gradient stop at index
             @param(index Index)
            }
            procedure DeleteStop(index: NativeInt); virtual;

            {**
             Get gradient stop at index
             @param(index Index)
             @returns(Gradient stop, @nil if not found)
            }
            function GetStop(index: NativeUInt): IStop; virtual;

            {**
             Get gradient stop count
             @returns(Count)
            }
            function GetStopCount: NativeUInt; virtual;

            {**
             Check if gradient is visible
             @returns(@true if gradient is visible, otherwise @false)
            }
            function IsVisible: Boolean; virtual;

            {**
             Check if gradient is opaque
             @returns(@true if opaque, otherwise @false)
            }
            function IsOpaque: Boolean; virtual;

            {**
             Check if all colors composing gradient are the same
             @param(color @bold([out]) United color if all colors are the same)
             @returns(@true if all colors are the same, otherwise @false)
            }
            function CheckUnited(out color: TWColor): Boolean; virtual;

            {**
             Check if a radial gradient should be wrapped
             @param(boundingRect The rect that bounds the area to fill with the gradient)
             @param(radius Radius)
             @returns(@true if radial gradient should be wrapped, otherwise @false)
            }
            function DoWrap(const boundingRect: TWRectF; const radius: TWSizeF): Boolean; overload; virtual;

            {**
             Get the path used to build the radial gradient
             @param(radius Radius)
             @param(pPath Path)
            }
            procedure GetRadialPath(const radius: TWSizeF; pPath: TGpGraphicsPath); virtual;

            {**
             Get linear gradient brush
             @param(gradientVector Gradient vector, not used if wrap mode is not IE_Clamp)
             @returns(Linear gradient brush, @nil on error)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            function GetLinear(const pGradientVector: TWLinearGradientVector = nil): TGpBrush; virtual;

            {**
             Get radial gradient brush
             @param(radius Radius)
             @returns(Radial gradient brush, @nil on error)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            function GetRadial(const radius: TWSizeF): TGpBrush; virtual;

            {**
             Get path gradient brush
             @param(path Path from which gradient should be generated)
             @returns(Path gradient brush, @nil on error)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            function GetPath(pPath: TGpGraphicsPath): TGpBrush; virtual;

        { Properties }
        public
            {**
             Get the gradient start color
            }
            property StartColor: PWColor read GetStartColor;

            {**
             Get the gradient end color
            }
            property EndColor: PWColor read GetEndColor;

            {**
             Get the gradient start point
            }
            property StartPoint: TWPointF read GetStartPoint write SetStartPoint;

            {**
             Get the gradient end point
            }
            property EndPoint: TWPointF read GetEndPoint write SetEndPoint;

            {**
             Get the gradient center point
            }
            property CenterPoint: TWPointF read GetCenterPoint write SetCenterPoint;

            {**
             Get the gradient focus point
            }
            property FocusPoint: TWPointF read GetFocusPoint write SetFocusPoint;

            {**
             Get or set the gradient wrap mode
            }
            property WrapMode: EWrapMode read m_WrapMode write m_WrapMode;
    end;

implementation
//---------------------------------------------------------------------------
// TWGDIPlusGradient.IStop
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.IStop.Create;
begin
    inherited Create;

    m_Color  := TWColor.GetDefault;
    m_Offset := 0.0;
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.IStop.Create(const color: TWColor; offset: Single);
begin
    inherited Create;

    m_Color.Assign(color);

    // check if offset is out of bounds, correct value if needed
    if (offset > 1.0) then
        m_Offset := 1.0
    else
    if (offset < 0.0) then
        m_Offset := 0.0
    else
        m_Offset := offset;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.IStop.IsEqual(const pOther: IStop): Boolean;
begin
    Result := (m_Offset = pOther.m_Offset) and m_Color.IsEqual(pOther.m_Color);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.IStop.Differs(const pOther: IStop): Boolean;
begin
    Result := (m_Offset <> pOther.m_Offset) or m_Color.Differs(pOther.m_Color);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.IStop.GetColor: PWColor;
begin
    Result := @m_Color;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.IStop.SetColor(const pColor: PWColor);
begin
    if (not Assigned(pColor)) then
        raise Exception.Create('Color is undefined');

    m_Color := TWColor.Create(pColor);
end;
//---------------------------------------------------------------------------
// TWGDIPlusGradient
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create;
begin
    inherited Create;

    m_pStops       := IStops.Create;
    m_StartColor   := TWColor.GetDefault;
    m_EndColor     := TWColor.GetDefault;
    m_StartPoint   := Default(TWPointF);
    m_EndPoint     := Default(TWPointF);
    m_RadialMatrix := TWMatrix3x3.GetDefault;
    m_WrapMode     := E_WM_Clamp;
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create(const startColor, endColor: TWColor; wrapMode: EWrapMode);
begin
    inherited Create;

    m_pStops       := IStops.Create;
    m_StartPoint   := Default(TWPointF);
    m_EndPoint     := Default(TWPointF);
    m_RadialMatrix := TWMatrix3x3.GetDefault;
    m_WrapMode     := wrapMode;

    m_StartColor.Assign(startColor);
    m_EndColor.Assign(endColor);
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create(const startColor, endColor: TWColor; const startPoint,
        endPoint: TWPointF; wrapMode: EWrapMode);
begin
    inherited Create;

    m_pStops       := IStops.Create;
    m_StartPoint   := startPoint;
    m_EndPoint     := endPoint;
    m_RadialMatrix := TWMatrix3x3.GetDefault;
    m_WrapMode     := wrapMode;

    m_StartColor.Assign(startColor);
    m_EndColor.Assign(endColor);
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create(const startColor, endColor: TWColor;
        const centerPoint, focusPoint: TWPointF);
begin
    inherited Create;

    m_pStops       := IStops.Create;
    m_StartPoint   := focusPoint;
    m_EndPoint     := centerPoint;
    m_RadialMatrix := TWMatrix3x3.GetDefault;
    m_WrapMode     := E_WM_Clamp;

    m_StartColor.Assign(startColor);
    m_EndColor.Assign(endColor);
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create(const startColor, endColor: TWColor; const focusPoint: TWPointF);
begin
    inherited Create;

    m_pStops       := IStops.Create;
    m_StartPoint   := focusPoint;
    m_RadialMatrix := TWMatrix3x3.GetDefault;
    m_WrapMode     := E_WM_Clamp;

    m_StartColor.Assign(startColor);
    m_EndColor.Assign(endColor);
end;
//---------------------------------------------------------------------------
constructor TWGDIPlusGradient.Create(const pBrush: TWBrush);
begin
    inherited Create;

    FromBrush(pBrush);
end;
//---------------------------------------------------------------------------
destructor TWGDIPlusGradient.Destroy;
begin
    m_pStops.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetLinearGradient(const pBrush: TWLinearGradientBrush);
var
    gradientStopCount, i:  NativeUInt;
    startPos, endPos, pos: TWPointF;
    pStop:                 IStop;
begin
    // get gradient stop count
    gradientStopCount := pBrush.Stops.Count;

    if (gradientStopCount = 0) then
        Exit;

    startPos := pBrush.Stops[0].Position^;
    endPos   := pBrush.Stops[gradientStopCount - 1].Position^;

    // set gradient start and end points
    SetStartColor(pBrush.Stops[0].Color);
    SetStartPoint(startPos);
    SetEndColor(pBrush.Stops[gradientStopCount - 1].Color);
    SetEndPoint(endPos);
    m_WrapMode := pBrush.WrapMode;

    // iterate through gradient stops
    if (gradientStopCount > 2) then
        for i := 1 to gradientStopCount - 2 do
        begin
            pStop := nil;

            try
                // create and populate new gradient stop
                pStop         := IStop.Create;;
                pStop.m_Color := pBrush.Stops[i].Color^;

                if ((pBrush.Stops[i].Percent >= 0.0) and (pBrush.Stops[i].Percent <= 1.0)) then
                    pStop.m_Offset := pBrush.Stops[i].Percent
                else
                begin
                    pos            := pBrush.Stops[i].Position^;
                    pStop.m_Offset := CalculateLinearGradientStopOffset(startPos, endPos, pos);
                end;

                // add gradient stop
                AddStop(pStop);
            finally
                pStop.Free;
            end;
        end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetRadialGradient(const pBrush: TWRadialGradientBrush);
var
    gradientStopCount, i:  NativeUInt;
    pStop:                 IStop;
begin
    // get gradient stop count
    gradientStopCount := pBrush.Stops.Count;

    if (gradientStopCount = 0) then
        Exit;

    // set gradient center and focus points
    SetStartColor(pBrush.Stops[0].Color);
    SetCenterPoint(pBrush.Center);
    SetEndColor(pBrush.Stops[gradientStopCount - 1].Color);
    SetFocusPoint(pBrush.Focus);

    // despite of the wrap mode, the gradient brush should always be clamp, otherwise the brush
    // pattern will be repeated instead of wrapping the colors. Unfortunately there is no way to
    // wrap the gradient brush simply in one operation, instead the drawing should be repeated with
    // higher values and a mask until the correct effect is reached
    m_WrapMode := E_WM_Clamp;

    // iterate through gradient stops
    if (gradientStopCount > 2) then
        for i := 1 to gradientStopCount - 2 do
        begin
            pStop := nil;

            try
                // create and populate new gradient stop
                pStop          := IStop.Create;;
                pStop.m_Color  := pBrush.Stops[i].Color^;
                pStop.m_Offset := pBrush.Stops[i].Percent;

                // add gradient stop
                AddStop(pStop);
            finally
                pStop.Free;
            end;
        end;

    // copy the radial matrix (will be used later to calculate the gradient path and to transform
    // the radial gradient values)
    m_RadialMatrix.Assign(pBrush.Matrix^);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.CalculateLinearGradientStopOffset(const startPos, endPos, pos: TWPointF): Single;
var
    startVector, endVector, posVector:      TWVector2;
    startEndLength, startPosLength, offset: Single;
begin
    // calculate total gradient length
    startVector    := TWVector2.Create(startPos.X, startPos.Y);
    endVector      := TWVector2.Create(endPos.X, endPos.Y);
    startEndLength := (endVector.Sub(startVector)).Length;

    // no length?
    if (startEndLength = 0.0) then
        Exit(0.0);

    // calculate length from gradient start position
    posVector      := TWVector2.Create(pos.X, pos.Y);
    startPosLength := (posVector.Sub(startVector)).Length;

    // calculate offset in percent
    offset := (startPosLength / startEndLength);

    // clamp offset value to certify that it's not out of bounds
    Result := TWMathHelper.Clamp(offset, 0.0, 1.0);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.CreateLinearClampBrush(const pGradientVector: TWLinearGradientVector;
        const startPoint, endPoint: TGpPointF; const startColor, endColor: TGpColor): TGpBrush;
var
    startVec,
    endVec,
    v1,
    v2,
    v3,
    v4,
    rotDir,
    refDir,
    gradientStartPoint,
    gradientEndPoint:     TWVector2;
    gradLine,
    v1ToGradLine,
    v2ToGradLine,
    v3ToGradLine,
    v4ToGradLine:         TWLine2;
    rotateMat:            TWMatrix3x3;
    intersectionPoints,
    gradientPoints,
    sortedGradientPoints: IVectors;
    bestResult:           IPointsDist;
    pBrush:               TGpLinearGradientBrush;
    colors:               IGpColors;
    offsets:              IOffsets;
    gradientCount,
    i,
    j,
    index,
    sortedIndex,
    stopCount:            NativeUInt;
    angle,
    refAngle,
    lenGradient,
    startPos,
    endPos:               Single;
    v1Intersect,
    v2Intersect,
    v3Intersect,
    v4Intersect,
    sorted:               Boolean;
begin
    // nothing to fill?
    if (not Assigned(pGradientVector) or (pGradientVector.Width = 0) or (pGradientVector.Height = 0)) then
        Exit (m_EndColor.GetGDIPlusSolidBrush);

    startVec := TWVector2.Create(startPoint);
    endVec   := TWVector2.Create(endPoint);

    // get the grandient line
    gradLine := TWLine2.Create(startVec, endVec.Sub(startVec).Normalize);

    // get rectangle vertices
    v1 := TWVector2.Create(pGradientVector.GradientStart.X, pGradientVector.GradientStart.Y);
    v2 := TWVector2.Create(pGradientVector.GradientEnd.X,   pGradientVector.GradientStart.Y);
    v3 := TWVector2.Create(pGradientVector.GradientEnd.X,   pGradientVector.GradientEnd.Y);
    v4 := TWVector2.Create(pGradientVector.GradientStart.X, pGradientVector.GradientEnd.Y);

    // get a matrix to calculate the rotation around the gradient line
    rotateMat.SetIdentity;
    rotateMat.RotateCenter(PI * 0.5, Default(TWVector2));

    // get the direction perpendicular to the line
    rotDir := rotateMat.Transform(gradLine.Dir);

    // calculate the lines in the direction of the gradient line and passing through each rect vertex
    v1ToGradLine := TWLine2.Create(v1, rotDir);
    v2ToGradLine := TWLine2.Create(v2, rotDir);
    v3ToGradLine := TWLine2.Create(v3, rotDir);
    v4ToGradLine := TWLine2.Create(v4, rotDir);

    SetLength(intersectionPoints, 4);

    // calculate each intersection where the vertices lines cross the gradient line
    v1Intersect := v1ToGradLine.Intersect(gradLine, intersectionPoints[0]);
    v2Intersect := v2ToGradLine.Intersect(gradLine, intersectionPoints[1]);
    v3Intersect := v3ToGradLine.Intersect(gradLine, intersectionPoints[2]);
    v4Intersect := v4ToGradLine.Intersect(gradLine, intersectionPoints[3]);

    // no intersection point?
    if ((intersectionPoints[0].X = infinity) or (intersectionPoints[1].X = infinity)
            or (intersectionPoints[2].X = infinity) or (intersectionPoints[3].X = infinity))
    then
    begin
        // todo -cBug -oJean: For now log an error, but if this case really happen, fix it
        TWLogHelper.LogToCompiler('Calculate clamping gradient brush - FAILED - no intersection point can be found'
                + ' - gradient start - x - ' + FloatToStr(startPoint.X)       + ' - y - '  + FloatToStr(startPoint.Y)
                + ' - gradient end - x - '   + FloatToStr(endPoint.X)         + ' - y - '  + FloatToStr(endPoint.Y)
                + ' - vector start - x - '   + FloatToStr(pGradientVector.GradientStart.X)
                + ' - y - '                  + FloatToStr(pGradientVector.GradientStart.Y)
                + ' - vector end - x - '     + FloatToStr(pGradientVector.GradientEnd.X)
                + ' - y - '                  + FloatToStr(pGradientVector.GradientEnd.Y)
                + ' - test result - v1 - '   + IntToStr(Integer(v1Intersect)) + ' - v2 - ' + IntToStr(Integer(v2Intersect))
                + ' - v3 - '                 + IntToStr(Integer(v3Intersect)) + ' - v4 - ' + IntToStr(Integer(v4Intersect)));

        Exit(nil);
    end;

    refDir   := TWVector2.Create(0.0, 1.0);
    refAngle := gradLine.Dir.Dot(refDir);

    // normally it should not be possible that the lines not cross, because they cannot be parallel.
    // So check if it's the case and log an error if happen
    if ((not v1Intersect) or (not v2Intersect) or (not v3Intersect) or (not v4Intersect)) then
    begin
        TWLogHelper.LogToCompiler('Calculate clamping gradient brush - FAILED - unexpected geometrical configuration'
                + ' - gradient start - x - ' + FloatToStr(startPoint.X)       + ' - y - '  + FloatToStr(startPoint.Y)
                + ' - gradient end - x - '   + FloatToStr(endPoint.X)         + ' - y - '  + FloatToStr(endPoint.Y)
                + ' - vector start - x - '   + FloatToStr(pGradientVector.GradientStart.X)
                + ' - y - '                  + FloatToStr(pGradientVector.GradientStart.Y)
                + ' - vector end - x - '     + FloatToStr(pGradientVector.GradientEnd.X)
                + ' - y - '                  + FloatToStr(pGradientVector.GradientEnd.Y)
                + ' - test result - v1 - '   + IntToStr(Integer(v1Intersect)) + ' - v2 - ' + IntToStr(Integer(v2Intersect))
                + ' - v3 - '                 + IntToStr(Integer(v3Intersect)) + ' - v4 - ' + IntToStr(Integer(v4Intersect)));

        // build a rescue config
        gradientStartPoint := startVec;
        gradientEndPoint   := endVec;
        lenGradient        := gradientEndPoint.Sub(gradientStartPoint).Length;
    end
    else
    begin
        // find the most distant points between the 4 points projected on the gradient line
        bestResult := GetMostDistantPoints(intersectionPoints);

        // calculate the angle between the line formed by the best result points and the gradient line
        angle := bestResult.m_End.Sub(bestResult.m_Start).Normalize.Dot(refDir);

        // is the line direction inverted?
        if (((angle > 0.0) and (refAngle < 0.0)) or ((angle < 0.0) and (refAngle > 0.0))) then
        begin
            gradientStartPoint := bestResult.m_End;
            gradientEndPoint   := bestResult.m_Start;
        end
        else
        begin
            gradientStartPoint := bestResult.m_Start;
            gradientEndPoint   := bestResult.m_End;
        end;

        lenGradient := bestResult.m_Length;
    end;

    SetLength(gradientPoints, 4);
    gradientPoints[0] := gradientStartPoint;
    gradientPoints[1] := startVec;
    gradientPoints[2] := endVec;
    gradientPoints[3] := gradientEndPoint;
    gradientCount     := Length(gradientPoints);

    SetLength(sortedGradientPoints, 4);

    sortedIndex := 0;

    while (gradientCount > 0) do
    begin
        index := 0;

        // iterate through points and search for the longest distance
        for i := 0 to gradientCount - 1 do
        begin
            sorted := True;

            for j := 0 to gradientCount - 1 do
            begin
                if (j = i) then
                    continue;

                angle := gradientPoints[j].Sub(gradientPoints[i]).Normalize.Dot(refDir);

                // is the line direction inverted?
                if (((angle > 0.0) and (refAngle < 0.0)) or ((angle < 0.0) and (refAngle > 0.0))) then
                begin
                    sorted := false;
                    break;
                end;
            end;

            if (sorted) then
            begin
                index := i;
                break;
            end;
        end;

        sortedGradientPoints[sortedIndex] := gradientPoints[index];
        DeleteItem(gradientPoints, index);

        gradientCount := Length(gradientPoints);
        Inc(sortedIndex);
    end;

    // calculate the gradient start and end positions
    startPos := sortedGradientPoints[1].Sub(sortedGradientPoints[0]).Length / lenGradient;
    endPos   := sortedGradientPoints[2].Sub(sortedGradientPoints[0]).Length / lenGradient;

    pBrush := nil;

    try
        // create linear gradient brush
        pBrush := TGpLinearGradientBrush.Create(sortedGradientPoints[0].ToGpPointF,
                sortedGradientPoints[3].ToGpPointF, startColor, endColor);

        pBrush.SetWrapMode(WrapModeClamp);

        // get stop count
        stopCount := m_pStops.Count;

        // create gradient stop color and offset arrays
        SetLength(colors,  stopCount + 4);
        SetLength(offsets, stopCount + 4);

        colors [0] := startColor;
        offsets[0] := 0.0;
        colors [1] := startColor;
        offsets[1] := Max(startPos, 0.0);

        // populate stop arrays content
        PopulateStopArrays(colors, offsets, 2, stopCount, False);

        colors [stopCount + 2] := endColor;
        offsets[stopCount + 2] := Min(endPos, 1.0);
        colors [stopCount + 3] := endColor;
        offsets[stopCount + 3] := 1.0;

        // set gradient brush stops
        pBrush.SetInterpolationColors(PGpColor(colors), PSingle(offsets), stopCount + 4);

        Result := pBrush;
        pBrush := nil;
    finally
        pBrush.Free;
    end
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetMostDistantPoints(const points: IVectors): IPointsDist;
var
    count, i, j: NativeUInt;
    len:         Single;
begin
    Result := Default(IPointsDist);

    // get the point count to iterate
    count := Length(points);

    // not enough points to find a distance?
    if (count < 2) then
        Exit;

    // iterate through points and search for the longest distance
    for i := 0 to count - 1 do
        for j := i + 1 to count - 1 do
        begin
            // calculate the length between 2 points
            len := points[j].Sub(points[i]).Length;

            // found a best result?
            if (len > Result.m_Length) then
            begin
                // keep it
                Result.m_Start  := points[i];
                Result.m_End    := points[j];
                Result.m_Length := len;
            end;
        end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.PopulateStopArrays(var colors: IGpColors; var offsets: IOffsets;
        start, count: Cardinal; invert: Boolean);
var
    i, index: NativeUInt;
begin
    if ((Length(colors) = 0) or (Length(offsets) = 0)) then
        Exit;

    if (count = 0) then
        Exit;

    // iterate through gradient stops to add
    for i := 0 to count - 1 do
    begin
        // calculate array index
        if (invert) then
            index := (count - 1) - i
        else
            index := i;

        // add gradient stop to arrays
        colors [start + index] := m_pStops[index].m_Color.GetGDIPlusColor;

        if (invert) then
            offsets[start + index] := 1.0 - m_pStops[index].m_Offset
        else
            offsets[start + index] := m_pStops[index].m_Offset;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.DeleteItem(var vectors: IVectors; index: NativeUInt);
var
    vectorCount:  NativeUInt;
    tailElements: NativeUInt;
begin
    vectorCount := Length(vectors);

    // is index out of bounds?
    if ((vectorCount = 0) or (index >= vectorCount)) then
        Exit;

    TailElements := vectorCount - index;

    if (TailElements > 0) then
        Move(vectors[index + 1], vectors[index], SizeOf(TWVector2) * TailElements);

    SetLength(vectors, vectorCount - 1);
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.FromBrush(const pBrush: TWBrush);
begin
    if (pBrush is TWLinearGradientBrush) then
    begin
        SetLinearGradient(pBrush as TWLinearGradientBrush);
        Exit;
    end;

    if (pBrush is TWRadialGradientBrush) then
    begin
        SetRadialGradient(pBrush as TWRadialGradientBrush);
        Exit;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.FromBrush(const pBrush: TWLinearGradientBrush);
begin
    if (not Assigned(pBrush)) then
        Exit;

    SetLinearGradient(pBrush);
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.FromBrush(const pBrush: TWRadialGradientBrush);
begin
    if (not Assigned(pBrush)) then
        Exit;

    SetRadialGradient(pBrush);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetStartColor: PWColor;
begin
    Result := @m_StartColor;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetStartColor(const pColor: PWColor);
begin
    m_StartColor.Assign(pColor^);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetEndColor: PWColor;
begin
    Result := @m_EndColor;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetEndColor(const pColor: PWColor);
begin
    m_EndColor.Assign(pColor^);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetStartPoint: TWPointF;
begin
    Result := m_StartPoint;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetStartPoint(const point: TWPointF);
begin
    m_StartPoint := point;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetEndPoint: TWPointF;
begin
    Result := m_EndPoint;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetEndPoint(const point: TWPointF);
begin
    m_EndPoint := point;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetCenterPoint: TWPointF;
begin
    Result := m_EndPoint;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetCenterPoint(const point: TWPointF);
begin
    m_EndPoint := point;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetFocusPoint: TWPointF;
begin
    Result := m_StartPoint;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.SetFocusPoint(const point: TWPointF);
begin
    m_StartPoint := point;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.AddStop(const pStop: IStop);
var
    pNewStop: IStop;
begin
    pNewStop := nil;

    try
        // create and populate new gradient stop
        pNewStop          := IStop.Create;
        pNewStop.m_Color  := pStop.m_Color;
        pNewStop.m_Offset := pStop.m_Offset;

        // add newly created stop to list
        m_pStops.Add(pNewStop);
        pNewStop := nil;
    finally
        pNewStop.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.AddStop(const pColor: PWColor; offset: Single);
var
    pNewStop: IStop;
begin
    Assert(Assigned(pColor));

    pNewStop := nil;

    try
        // create and populate new gradient stop
        pNewStop          := IStop.Create;
        pNewStop.m_Color  := pColor^;
        pNewStop.m_Offset := offset;

        // add newly created stop to list
        m_pStops.Add(pNewStop);
        pNewStop := nil;
    finally
        pNewStop.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.InsertStop(index: NativeInt; const pStop: IStop);
var
    pNewStop: IStop;
begin
    pNewStop := nil;

    try
        // create and populate new gradient stop
        pNewStop          := IStop.Create;
        pNewStop.m_Color  := pStop.m_Color;
        pNewStop.m_Offset := pStop.m_Offset;

        // is index out of bounds
        if (index >= m_pStops.Count) then
            // add newly created stop at the list end
            m_pStops.Add(pNewStop)
        else
            // insert stop at index
            m_pStops.Insert(index, pNewStop);

        pNewStop := nil;
    finally
        pNewStop.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.InsertStop(index: NativeInt; const pColor: PWColor; offset: Single);
var
    pNewStop: IStop;
begin
    pNewStop := nil;

    try
        // create and populate new gradient stop
        pNewStop          := IStop.Create;
        pNewStop.m_Color  := pColor^;
        pNewStop.m_Offset := offset;

        // is index out of bounds
        if (index >= m_pStops.Count) then
            // add newly created stop at the list end
            m_pStops.Add(pNewStop)
        else
            // insert stop at index
            m_pStops.Insert(index, pNewStop);

        pNewStop := nil;
    finally
        pNewStop.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.DeleteStop(index: NativeInt);
begin
    // is index out of bounds?
    if (index >= m_pStops.Count) then
        Exit;

    // delete gradient stop and remove from list
    m_pStops[index].Free;
    m_pStops.Delete(index);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetStop(index: NativeUInt): IStop;
begin
    // is index out of bounds?
    if (NativeInt(index) >= m_pStops.Count) then
        Exit(nil);

    Result := m_pStops[index];
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetStopCount: NativeUInt;
begin
    Result := m_pStops.Count;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.IsVisible: Boolean;
var
    pStop: IStop;
begin
    // iterate through gradient stops
    for pStop in m_pStops do
        // any stop is visible?
        if (pStop.m_Color.GetAlpha <> 0) then
            Exit(True);

    // any base color is visible?
    Result := ((m_StartColor.GetAlpha <> 0) or (m_EndColor.GetAlpha <> 0));
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.IsOpaque: Boolean;
var
    pStop: IStop;
begin
    // iterate through gradient stops
    for pStop in m_pStops do
        // any stop is transparent?
        if (pStop.m_Color.GetAlpha <> 255) then
            Exit(False);

    // base color are opaque?
    Result := ((m_StartColor.GetAlpha = 255) and (m_EndColor.GetAlpha = 255));
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.CheckUnited(out color: TWColor): Boolean;
var
    pStop: IStop;
begin
    // are start and end colors the same?
    if (m_StartColor.Differs(m_EndColor)) then
        Exit(False);

    // iterate through gradient stops
    for pStop in m_pStops do
        // any gradient stop differs from base colors?
        if (pStop.m_Color.Differs(m_StartColor) or pStop.m_Color.Differs(m_EndColor)) then
            Exit(False);

    // get united color (can use any of the gradient stops because all colors are the same)
    color  := m_StartColor;
    Result := True;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.DoWrap(const boundingRect: TWRectF; const radius: TWSizeF): Boolean;
var
    onBoundary: Boolean;
begin
    if ((radius.Width = 0) or (radius.Height = 0)) then
        Exit(False);

    // is bounding rect left top edge inside the ellipse?
    if (not TWGeometryTools.PointInEllipse(boundingRect.Left, boundingRect.Top, m_EndPoint.X,
            m_EndPoint.Y, radius.Width * 0.5, radius.Height * 0.5, onBoundary))
    then
        Exit(True);

    // is bounding rect right top edge inside the ellipse?
    if (not TWGeometryTools.PointInEllipse(boundingRect.Right, boundingRect.Top, m_EndPoint.X,
            m_EndPoint.Y, radius.Width * 0.5, radius.Height * 0.5, onBoundary))
    then
        Exit(True);

    // is bounding rect right bottom edge inside the ellipse?
    if (not TWGeometryTools.PointInEllipse(boundingRect.Right, boundingRect.Bottom, m_EndPoint.X,
            m_EndPoint.Y, radius.Width * 0.5, radius.Height * 0.5, onBoundary))
    then
        Exit(True);

    // is bounding rect left bottom edge inside the ellipse?
    if (not TWGeometryTools.PointInEllipse(boundingRect.Left, boundingRect.Bottom, m_EndPoint.X,
            m_EndPoint.Y, radius.Width * 0.5, radius.Height * 0.5, onBoundary))
    then
        Exit(True);

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWGDIPlusGradient.GetRadialPath(const radius: TWSizeF; pPath: TGpGraphicsPath);
var
    surroundRect: TGpRectF;
    pGpMatrix:    IWSmartPointer<TGpMatrix>;
begin
    // calculate the surrounding rect
    surroundRect.X      := m_EndPoint.X - (radius.Width  * 0.5);
    surroundRect.Y      := m_EndPoint.Y - (radius.Height * 0.5);
    surroundRect.Width  := radius.Width;
    surroundRect.Height := radius.Height;

    // create a graphic path containing the gradient circle
    pPath.AddEllipse(surroundRect);

    // apply the radial matrix to the ellipsis. Do it to the path, and not to the bounding rect,
    // otherwise the ellipse may not be drawn in the right direction
    if (not m_RadialMatrix.IsIdentity) then
    begin
        pGpMatrix := TWSmartPointer<TGpMatrix>.Create();
        m_RadialMatrix.ToGpMatrix(pGpMatrix);
        pPath.Transform(pGpMatrix);
    end;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetLinear(const pGradientVector: TWLinearGradientVector): TGpBrush;
var
    startPoint, endPoint: TGpPointF;
    wrapMode:             GpWrapMode;
    stopCount:            NativeUInt;
    colors:               IGpColors;
    offsets:              IOffsets;
    pBrush:               TGpLinearGradientBrush;
begin
    // if start and end points are equals to 0.0, then a solid brush using the last stop color
    // should be used instead
    if (m_StartPoint = m_EndPoint) then
        Exit(m_EndColor.GetGDIPlusSolidBrush);

    // convert standard GDI values to GDI+ values
    startPoint   := Default(TGpPointF);
    startPoint.X := m_StartPoint.x;
    startPoint.Y := m_StartPoint.y;
    endPoint     := Default(TGpPointF);
    endPoint.X   := m_EndPoint.x;
    endPoint.Y   := m_EndPoint.y;

    // search for wrap mode
    case (m_WrapMode) of
        E_WM_Tile:       wrapMode := WrapModeTile;
        E_WM_TileFlipX:  wrapMode := WrapModeTileFlipX;
        E_WM_TileFlipY:  wrapMode := WrapModeTileFlipY;
        E_WM_TileFlipXY: wrapMode := WrapModeTileFlipXY;

        E_WM_Clamp:
            // create clamped linear gradient brush
            Exit(CreateLinearClampBrush(pGradientVector, startPoint, endPoint,
                    m_StartColor.GetGDIPlusColor, m_EndColor.GetGDIPlusColor));
    else
        Exit(nil);
    end;

    pBrush := nil;

    try
        // create linear gradient brush
        pBrush := TGpLinearGradientBrush.Create(startPoint, endPoint, m_StartColor.GetGDIPlusColor,
                m_EndColor.GetGDIPlusColor);

        // set gradient brush wrap mode
        pBrush.SetWrapMode(wrapMode);

        // get gradient stop count
        stopCount := m_pStops.Count;

        if (stopCount > 0) then
        begin
            // create gradient stop color and offset arrays
            SetLength(colors,  stopCount + 2);
            SetLength(offsets, stopCount + 2);

            colors [0] := m_StartColor.GetGDIPlusColor;
            offsets[0] := 0.0;

            // populate stop arrays content
            PopulateStopArrays(colors, offsets, 1, stopCount, False);

            colors [stopCount + 1] := m_EndColor.GetGDIPlusColor;
            offsets[stopCount + 1] := 1.0;

            // set gradient brush stops
            pBrush.SetInterpolationColors(PGpColor(colors), PSingle(offsets), stopCount + 2);
        end;

        Result := pBrush;
        pBrush := nil;
    finally
        pBrush.Free;
    end;
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetRadial(const radius: TWSizeF): TGpBrush;
var
    pEllipsePath: IWSmartPointer<TGpGraphicsPath>;
begin
    // if r is equals to 0.0, then a solid brush using the last stop color should be used instead
    if ((radius.Width = 0.0) or (radius.Height = 0.0)) then
        Exit(m_EndColor.GetGDIPlusSolidBrush);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pEllipsePath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // create a graphic path containing the gradient circle
    GetRadialPath(radius, pEllipsePath);

    Result := GetPath(pEllipsePath);
end;
//---------------------------------------------------------------------------
function TWGDIPlusGradient.GetPath(pPath: TGpGraphicsPath): TGpBrush;
var
    wrapMode:               GpWrapMode;
    focusPoint:             TGpPointF;
    boundingBox:            TGpRectF;
    focus:                  TWVector2;
    surroundCount:          Integer;
    stopCount:              NativeUInt;
    colors, surroundColors: IGpColors;
    offsets:                IOffsets;
    pBrush:                 TGpPathGradientBrush;
begin
    // no source path?
    if (not Assigned(pPath)) then
        Exit(nil);

    // search for wrap mode
    case (m_WrapMode) of
        E_WM_Tile:       wrapMode := WrapModeTile;
        E_WM_TileFlipX:  wrapMode := WrapModeTileFlipX;
        E_WM_TileFlipY:  wrapMode := WrapModeTileFlipY;
        E_WM_TileFlipXY: wrapMode := WrapModeTileFlipXY;
        E_WM_Clamp:      wrapMode := WrapModeClamp;
    else
        Exit(nil);
    end;

    // no focus point defined?
    if (m_StartPoint.IsZero) then
    begin
        // get the path bounding box
        pPath.GetBounds(boundingBox);

        // calculate the center of the path. By default, this will be used as focus point
        focusPoint.X := boundingBox.X + (boundingBox.Width  * 0.5);
        focusPoint.Y := boundingBox.Y + (boundingBox.Height * 0.5);
    end
    else
    begin
        // get the focus point, transformed to be on the right location
        focus.X      := m_StartPoint.X;
        focus.Y      := m_StartPoint.Y;
        focus        := m_RadialMatrix.Transform(focus);
        focusPoint.X := focus.X;
        focusPoint.Y := focus.Y;
    end;

    pBrush := nil;

    try
        // create linear gradient brush
        pBrush := TGpPathGradientBrush.Create(pPath);

        // set center point and color
        pBrush.SetCenterPoint(focusPoint);
        pBrush.SetCenterColor(m_StartColor.GetGDIPlusColor);

        surroundCount := 1;

        SetLength(surroundColors, surroundCount);
        surroundColors[0] := m_EndColor.GetGDIPlusColor;

        // set surround color
        pBrush.SetSurroundColors(PARGB(surroundColors), surroundCount);

        // set gradient brush wrap mode
        pBrush.SetWrapMode(wrapMode);

        // get gradient stop count
        stopCount := m_pStops.Count;

        if (stopCount > 0) then
        begin
            // create gradient stop color and offset arrays
            SetLength(colors,  stopCount + 2);
            SetLength(offsets, stopCount + 2);

            colors [0] := m_EndColor.GetGDIPlusColor;
            offsets[0] := 0.0;

            // populate stop arrays content
            PopulateStopArrays(colors, offsets, 1, stopCount, True);

            colors [stopCount + 1] := m_StartColor.GetGDIPlusColor;
            offsets[stopCount + 1] := 1.0;

            // set gradient brush stops
            pBrush.SetInterpolationColors(PARGB(colors), PSingle(offsets), stopCount + 2);
        end;

        Result := pBrush;
        pBrush := nil;
    finally
        pBrush.Free;
    end;
end;
//---------------------------------------------------------------------------

end.
