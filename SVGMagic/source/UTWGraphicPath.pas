{**
 @abstract(@name provides the classes to convert a generic graphic path to a graphic path belonging
           to a graphic library, like e.g. GDI, GDI+, ...)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWGraphicPath;

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     System.Generics.Collections,
     Vcl.Graphics,
     Winapi.GDIPAPI,
     Winapi.GDIPObj,
     Winapi.Windows,
     UTWPoint,
     UTWVector,
     UTWRect,
     UTWGeometryTools,
     UTWHelpers;

type
    {**
     Path command (or instruction). Each of them notify the converter about the next action to do,
     for example go to position [x, y] or draw a line from current position to [x, y]
    }
    TWPathCmd = class
        public type
            {**
             Available commands
            }
            IEType =
            (
                IE_IT_Unknown,
                IE_IT_MoveTo,
                IE_IT_LineTo,
                IE_IT_Horiz_LineTo,
                IE_IT_Vert_LineTo,
                IE_IT_CurveTo,
                IE_IT_Smooth_CurveTo,
                IE_IT_Quadratic_Bezier_CurveTo,
                IE_IT_Smooth_Quadratic_Bezier_CurveTo,
                IE_IT_Elliptical_Arc,
                IE_IT_ClosePath
            );

            {**
             List of points making up the command, its content depends of the command itself. For
             example, a IE_IT_MoveTo command will contain 2 points, where points[0] is the x
             coordinate and points[1] the y coordinate to go to
            }
            IPoints = array of Single;

        private
            m_Type:     IEType;
            m_Relative: Boolean;

        protected
            m_Points: IPoints;

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Add a point to the list
            }
            procedure AddPoint(point: Single); virtual;

            {**
             Get the point at index
             @returns(The point)
            }
            function GetPoint(index: Integer): Single; virtual;

            {**
             Get the point count
             @returns(The point count)
            }
            function GetPointCount: Integer; virtual;

            {**
             Copy content from another path command
             @param(other Other path command to copy from)
            }
            procedure Assign(const other: TWPathCmd); virtual;

            {**
             Clear path command content
            }
            procedure Clear; virtual;

            {**
             Convert type to string
             @param(type Type)
             @returns(Converted string)
            }
            class function TypeToStr(cmdType: IEType): UnicodeString; static;

        public
            property Command:                IEType  read m_Type     write m_Type;
            property Relative:               Boolean read m_Relative write m_Relative;
            property Points[index: Integer]: Single  read GetPoint;
            property PointCount:             Integer read GetPointCount;
    end;

    {**
     Path command (or instruction) list
    }
    TWPathCmds = TObjectList<TWPathCmd>;

    {**
     Graphic path converter, used to convert a generic path to an output, like e.g. GDI+
    }
    TWGraphicPathConverter = class
        protected type
            {**
             Convert path to output
            }
            IConverter = class
                private
                    m_Type:     TWPathCmd.IEType;
                    m_Rounding: Boolean;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; overload; virtual;

                    {**
                     Called when a path curve instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert)
                     @param(relative If @true, point is relative)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @param(curveStart @bold([in, out]) Calculated curve start)
                     @param(curveEnd @bold([in, out]) Calculated curve end)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF; var curveStart: TWPointF;
                            var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean; overload; virtual;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Enable or disable rounding before adding value to output
                     @param(value If @true, values will be rounded before added to output)
                    }
                    procedure EnableRounding(value: Boolean); virtual;

                    {**
                     Convert path instruction to output and update coordinate
                     @param(instruction Instruction to convert)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(coordinate @bold([in, out]) current coordinate, new coordinate after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function Convert(const instruction: TWPathCmd; x, y: Integer;
                            var coordinate: TWPointF): Boolean; overload; virtual;

                    {**
                     Convert path instruction to output and update coordinate
                     @param(instruction Instruction to convert)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function Convert(const instruction: TWPathCmd; x, y: Integer;
                            lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var lastCurveEnd: TWPointF): Boolean; overload; virtual;
            end;

        protected
            {**
             Convert path to output
             @param(rect Rect bounding path)
             @param(x Start x position, in pixels)
             @param(y Start y position, in pixels)
             @param(path Path to convert)
             @returns(@true on success, otherwise @false)
            }
            function ConvertPath(const rect: TWRectF; x, y: Integer;
                    const path: TWPathCmds): Boolean; virtual; abstract;

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Process the conversion from the generic path to the output
             @param(rect Rect bounding path)
             @param(path Path to convert)
             @returns(@true on success, otherwise @false)
            }
            function Process(const rect: TWRectF; const path: TWPathCmds): Boolean; virtual;
    end;

    {**
     Graphic path converter, used to convert a generic path to a GDI+ path
    }
    TWGraphicPathConverter_GDIPlus = class(TWGraphicPathConverter)
        private type
            {**
             Convert "move to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                IMoveToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                IMoveToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput:    TGpGraphicsPath;
                    m_StartPoint: TWPointF;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Get the start point (i.e. the point reached by the first moveTo, before implicit
                     lineTo were also applied
                     @returns(start point)
                    }
                    function GetStartPoint: TWPointF; virtual;
            end;

            {**
             Convert "line to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                ILineToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                ILineToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "horizontal line to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                IHorzLineToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                IHorzLineToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "vertical line to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                IVertLineToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                IVertLineToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "curve to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                ICurveToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                ICurveToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path curve instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert)
                     @param(relative If @true, point is relative)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @param(curveStart @bold([in, out]) Calculated curve start)
                     @param(curveEnd @bold([in, out]) Calculated curve end)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF; var curveStart: TWPointF;
                            var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "smooth curve to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                ISmoothCurveToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                ISmoothCurveToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path curve instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert)
                     @param(relative If @true, point is relative)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @param(curveStart @bold([in, out]) Calculated curve start)
                     @param(curveEnd @bold([in, out]) Calculated curve end)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF; var curveStart: TWPointF;
                            var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "quadratic bezier curve to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                IQuadraticBezierCurveToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                IQuadraticBezierCurveToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path curve instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert)
                     @param(relative If @true, point is relative)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @param(curveStart @bold([in, out]) Calculated curve start)
                     @param(curveEnd @bold([in, out]) Calculated curve end)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF; var curveStart: TWPointF;
                            var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "smooth quadratic bezier curve to" path instruction to output
            }
            {$if CompilerVersion <= 24}
                ISmoothQuadraticBezierCurveToConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                ISmoothQuadraticBezierCurveToConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput: TGpGraphicsPath;

                protected
                    {**
                     Called when a path curve instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert)
                     @param(relative If @true, point is relative)
                     @param(lastOp Last executed operation)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @param(curveStart @bold([in, out]) Calculated curve start)
                     @param(curveEnd @bold([in, out]) Calculated curve end)
                     @param(lastCurveEnd @bold([in, out]) Last curve end, current curve end after conversion)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF; var curveStart: TWPointF;
                            var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Convert "elliptical arc" path instruction to output
            }
            {$if CompilerVersion <= 24}
                IEllipticalArcConverter = class(TWGraphicPathConverter.IConverter)
            {$else}
                IEllipticalArcConverter = class(IConverter)
            {$ifend}
                private
                    m_pOutput:  TGpGraphicsPath;
                    m_Radius:   TWPointF;
                    m_Angle:    Single;
                    m_LargeArc: Boolean;
                    m_Sweep:    Boolean;

                protected
                    {**
                     Called when a path instruction must be converted to output
                     @param(index Point index)
                     @param(x Start x position in pixels)
                     @param(y Start y position in pixels)
                     @param(point Point to convert (it's a part of the instruction))
                     @param(relative If @true, point is relative)
                     @param(coordinate @bold([in, out]) Current coordinate, new coordinate after conversion)
                     @param(nextCoordinate @bold([in, out]) Next calculated coordinate)
                     @returns(@true on success, otherwise @false)
                    }
                    function OnConvert(index: NativeUInt; x, y: Integer; point: Single;
                            relative: Boolean; var coordinate: TWPointF;
                            var nextCoordinate: TWPointF): Boolean; override;

                public
                    {**
                     Constructor
                     @param(pOutput Output GDI+ graphics path to convert to)
                    }
                    constructor Create(pOutput: TGpGraphicsPath); reintroduce; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear the converter content
                    }
                    procedure Clear; virtual;
            end;

        private
            m_pGraphicsPath:                 TGpGraphicsPath;
            m_pMoveTo:                       IMoveToConverter;
            m_pLineTo:                       ILineToConverter;
            m_pHorzLineTo:                   IHorzLineToConverter;
            m_pVertLineTo:                   IVertLineToConverter;
            m_pCurveTo:                      ICurveToConverter;
            m_pSmoothCurveTo:                ISmoothCurveToConverter;
            m_pQuadraticBezierCurveTo:       IQuadraticBezierCurveToConverter;
            m_pSmoothQuadraticBezierCurveTo: ISmoothQuadraticBezierCurveToConverter;
            m_pIEllipticalArc:               IEllipticalArcConverter;

        protected
            {**
             Convert path to GDI+ path
             @param(rect Rect bounding path)
             @param(x Start x position, in pixels)
             @param(y Start y position, in pixels)
             @param(path Path to convert)
             @returns(@true on success, otherwise @false)
            }
            function ConvertPath(const rect: TWRectF; x, y: Integer; const path: TWPathCmds): Boolean; override;

        public
            {**
             Constructor
             @param(pOutput Output GDI+ graphics path to convert to)
            }
            constructor Create(pGraphicsPath: TGpGraphicsPath); reintroduce; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;
    end;

implementation
//---------------------------------------------------------------------------
// TWPathCmd
//---------------------------------------------------------------------------
constructor TWPathCmd.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWPathCmd.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWPathCmd.AddPoint(point: Single);
var
    offset: NativeUInt;
begin
    // get offset of the last point in the list
    offset := Length(m_Points);

    // increase point list size
    SetLength(m_Points, offset + 1);

    // set new point in the list
    m_Points[offset] := point;
end;
//---------------------------------------------------------------------------
function TWPathCmd.GetPoint(index: Integer): Single;
begin
    if (index >= Length(m_Points)) then
        raise Exception.CreateFmt('Index is out of bounds - %d', [index]);

    Result := m_Points[index];
end;
//---------------------------------------------------------------------------
function TWPathCmd.GetPointCount: Integer;
begin
    Result := Length(m_Points);
end;
//---------------------------------------------------------------------------
procedure TWPathCmd.Assign(const other: TWPathCmd);
var
    count, i: NativeInt;
begin
    // copy values
    m_Type     := other.m_Type;
    m_Relative := other.m_Relative;

    // get point count for source
    count := Length(other.m_Points);

    // resize local point list
    SetLength(m_Points, count);

    // copy points from source
    for i := 0 to count - 1 do
        m_Points[i] := other.m_Points[i];
end;
//---------------------------------------------------------------------------
procedure TWPathCmd.Clear;
begin
    m_Type     := IE_IT_Unknown;
    m_Relative := False;
    SetLength(m_Points, 0);
end;
//---------------------------------------------------------------------------
class function TWPathCmd.TypeToStr(cmdType: IEType): UnicodeString;
begin
    case (cmdType) of
        IE_IT_Unknown:                         Result := 'IE_IT_Unknown';
        IE_IT_MoveTo:                          Result := 'IE_IT_MoveTo';
        IE_IT_LineTo:                          Result := 'IE_IT_LineTo';
        IE_IT_Horiz_LineTo:                    Result := 'IE_IT_Horiz_LineTo';
        IE_IT_Vert_LineTo:                     Result := 'IE_IT_Vert_LineTo';
        IE_IT_CurveTo:                         Result := 'IE_IT_CurveTo';
        IE_IT_Smooth_CurveTo:                  Result := 'IE_IT_Smooth_CurveTo';
        IE_IT_Quadratic_Bezier_CurveTo:        Result := 'IE_IT_Quadratic_Bezier_CurveTo';
        IE_IT_Smooth_Quadratic_Bezier_CurveTo: Result := 'IE_IT_Smooth_Quadratic_Bezier_CurveTo';
        IE_IT_Elliptical_Arc:                  Result := 'IE_IT_Elliptical_Arc';
        IE_IT_ClosePath:                       Result := 'IE_IT_ClosePath';
    else
        raise Exception.CreateFmt('Unknown type', [Integer(cmdType)]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter.IConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter.IConverter.Create;
begin
    inherited Create;

    m_Type     := TWPathCmd.IEType.IE_IT_Unknown;
    m_Rounding := False;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter.IConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter.IConverter.OnConvert(index: NativeUInt; x, y: Integer; point: Single;
        relative: Boolean; var coordinate: TWPointF; var nextCoordinate: TWPointF): Boolean;
begin
    // a basic implementation of this function is required, because the inherited children require
    // the implementation of only one of the two functions, depending of the conversion work to do
    Result := False;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter.IConverter.OnConvert(index: NativeUInt; x, y: Integer; point: Single;
        relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
        var nextCoordinate: TWPointF; var curveStart: TWPointF;
        var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean;
begin
    // a basic implementation of this function is required, because the inherited children require
    // the implementation of only one of the two functions, depending of the conversion work to do
    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWGraphicPathConverter.IConverter.EnableRounding(value: Boolean);
begin
    m_Rounding := value;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter.IConverter.Convert(const instruction: TWPathCmd; x, y: Integer;
        var coordinate: TWPointF): Boolean;
var
    nextCoordinate:       TWPointF;
    index, pointCount, i: NativeUInt;
    point:                Single;
begin
    // is instruction valid?
    if (instruction.Command <> m_Type) then
    begin
        TWLogHelper.LogToCompiler('Convert - FAILED - instruction does not match - expected - '
                + TWPathCmd.TypeToStr(m_Type) + ' - received - '
                + TWPathCmd.TypeToStr(instruction.Command));
        Exit(False);
    end;

    index      := 0;
    pointCount := instruction.PointCount;

    // iterate through instruction points
    if (pointCount > 0) then
        for i := 0 to pointCount - 1 do
        begin
            point := instruction.Points[i];

            // convert to point
            if (not OnConvert(index, x, y, point, instruction.Relative, coordinate, nextCoordinate))
            then
                Exit(False);

            Inc(index);
        end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter.IConverter.Convert(const instruction: TWPathCmd; x, y: Integer;
        lastOp: TWPathCmd.IEType; var coordinate: TWPointF; var lastCurveEnd: TWPointF): Boolean;
var
    nextCoordinate, curveStart, curveEnd: TWPointF;
    index, pointCount, i:                 NativeUInt;
    point:                                Single;
begin
    // is instruction valid?
    if (instruction.Command <> m_Type) then
    begin
        TWLogHelper.LogToCompiler('Convert - FAILED - instruction does not match - expected - '
                + TWPathCmd.TypeToStr(m_Type) + ' - received - '
                + TWPathCmd.TypeToStr(instruction.Command));
        Exit(False);
    end;

    index      := 0;
    pointCount := instruction.PointCount;

    // iterate through instruction points
    if (pointCount > 0) then
        for i := 0 to pointCount - 1 do
        begin
            point := instruction.Points[i];

            // convert to curve point
            if (not OnConvert(index, x, y, point, instruction.Relative, lastOp, coordinate,
                    nextCoordinate, curveStart, curveEnd, lastCurveEnd))
            then
                Exit(False);

            Inc(index);
        end;

    Result := True;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.IMoveToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.IMoveToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_MoveTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.IMoveToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IMoveToConverter.OnConvert(index: NativeUInt; x, y: Integer;
        point: Single; relative: Boolean; var coordinate: TWPointF; var nextCoordinate: TWPointF): Boolean;
var
    startPoint, endPoint: TGpPointF;
begin
    // search for current value to update
    case (index) of
        0:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // update coordinate
            coordinate := nextCoordinate;

            // save start point (coordinate may be modified later by implicit lineTo)
            m_StartPoint := coordinate;

            Exit(True);
        end;
    else
        // a "move to" instruction may contain more than 2 points. In this case, the remaining
        // points should be considered as subsequent "line to" instructions (see
        // http://www.w3.org/TR/SVG/paths.html#PathDataMovetoCommands)
        if ((index mod 2) = 0) then
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;
        end
        else
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // add start x and y to final coordinates
            startPoint.X := x + coordinate.X;
            startPoint.Y := y + coordinate.Y;
            endPoint.X   := x + nextCoordinate.X;
            endPoint.Y   := y + nextCoordinate.Y;

            // add line to path
            m_pOutput.AddLine(startPoint, endPoint);

            // update coordinate
            coordinate := nextCoordinate;
        end;

        Exit(True);
    end;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IMoveToConverter.GetStartPoint: TWPointF;
begin
    Result := m_StartPoint;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.ILineToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.ILineToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_LineTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.ILineToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.ILineToConverter.OnConvert(index: NativeUInt; x, y: Integer;
        point: Single; relative: Boolean; var coordinate: TWPointF; var nextCoordinate: TWPointF): Boolean;
var
    startPoint, endPoint: TGpPointF;
begin
    // search for current value to update. NOTE a "line to" instruction may contain more than 2
    // points. In this case, the remaining points should be considered as subsequent "polyline to"
    // instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataLinetoCommands)
    case (index mod 2) of
        0:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // add start x and y to final coordinates
            startPoint.X := x + coordinate.X;
            startPoint.Y := y + coordinate.Y;
            endPoint.X   := x + nextCoordinate.X;
            endPoint.Y   := y + nextCoordinate.Y;

            // add line to path
            m_pOutput.AddLine(startPoint, endPoint);

            // update coordinate
            coordinate := nextCoordinate;
            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.IHorzLineToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.IHorzLineToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_Horiz_LineTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.IHorzLineToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IHorzLineToConverter.OnConvert(index: NativeUInt; x, y: Integer;
        point: Single; relative: Boolean; var coordinate: TWPointF; var nextCoordinate: TWPointF): Boolean;
var
    startPoint, endPoint: TGpPointF;
begin
    // is relative?
    if (relative) then
        // calculate next x coordinate relatively to current
        nextCoordinate.X := coordinate.X + point
    else
        // get next x coordinate
        nextCoordinate.X := point;

    // get y coordinate
    nextCoordinate.Y := coordinate.Y;

    // add start x and y to final coordinates
    startPoint.X := x + coordinate.X;
    startPoint.Y := y + coordinate.Y;
    endPoint.X   := x + nextCoordinate.X;
    endPoint.Y   := y + nextCoordinate.Y;

    // add line to path
    m_pOutput.AddLine(startPoint, endPoint);

    // update coordinate
    coordinate := nextCoordinate;
    Result     := True;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.IVertLineToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.IVertLineToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_Vert_LineTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.IVertLineToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IVertLineToConverter.OnConvert(index: NativeUInt; x, y: Integer;
        point: Single; relative: Boolean; var coordinate: TWPointF; var nextCoordinate: TWPointF): Boolean;
var
    startPoint, endPoint: TGpPointF;
begin
    // get x coordinate
    nextCoordinate.X := coordinate.X;

    // is relative?
    if (relative) then
        // calculate next y coordinate relatively to current
        nextCoordinate.Y := coordinate.Y + point
    else
        // get next y coordinate
        nextCoordinate.Y := point;

    // add start x and y to final coordinates
    startPoint.X := x + coordinate.X;
    startPoint.Y := y + coordinate.Y;
    endPoint.X   := x + nextCoordinate.X;
    endPoint.Y   := y + nextCoordinate.Y;

    // add line to path
    m_pOutput.AddLine(startPoint, endPoint);

    // update coordinate
    coordinate := nextCoordinate;
    Result     := True;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.ICurveToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.ICurveToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_CurveTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.ICurveToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.ICurveToConverter.OnConvert(index: NativeUInt; x, y: Integer;
        point: Single; relative: Boolean; lastOp: TWPathCmd.IEType; var coordinate: TWPointF;
        var nextCoordinate: TWPointF; var curveStart: TWPointF;
        var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean;
var
    startPoint, curveControl1, curveControl2, endPoint: TGpPointF;
begin
    // search for current value to update. NOTE a "curve to" instruction may contain more than 6
    // points. In this case, the remaining points should be considered as subsequent "polycurve to"
    // instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands)
    case (index mod 6) of
        0:
        begin
            // is relative?
            if (relative) then
                // calculate next curve start x coordinate relatively to current coordinate
                curveStart.X := coordinate.X + point
            else
                // get next curve start x coordinate
                curveStart.X := point;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
                // calculate next curve start y coordinate relatively to current coordinate
                curveStart.Y := coordinate.Y + point
            else
                // get next curve start y coordinate
                curveStart.Y := point;

            Exit(True);
        end;

        2:
        begin
            // is relative?
            if (relative) then
                // calculate next curve end x coordinate relatively to current coordinate
                curveEnd.X := coordinate.X + point
            else
                // get next curve end x coordinate
                curveEnd.X := point;

            Exit(True);
        end;

        3:
        begin
            // is relative?
            if (relative) then
                // calculate next curve end y coordinate relatively to current coordinate
                curveEnd.Y := coordinate.Y + point
            else
                // get next curve end y coordinate
                curveEnd.Y := point;

            Exit(True);
        end;

        4:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        5:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // add start x and y to final coordinates
            startPoint.X    := x + coordinate.X;
            startPoint.Y    := y + coordinate.Y;
            curveControl1.X := x + curveStart.X;
            curveControl1.Y := y + curveStart.Y;
            curveControl2.X := x + curveEnd.X;
            curveControl2.Y := y + curveEnd.Y;
            endPoint.X      := x + nextCoordinate.X;
            endPoint.Y      := y + nextCoordinate.Y;

            // add cubic Bézier curve
            m_pOutput.AddBezier(startPoint, curveControl1, curveControl2, endPoint);

            // update coordinates
            coordinate   := nextCoordinate;
            lastCurveEnd := curveEnd;

            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.ISmoothCurveToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.ISmoothCurveToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_Smooth_CurveTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.ISmoothCurveToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.ISmoothCurveToConverter.OnConvert(index: NativeUInt;
        x, y: Integer; point: Single; relative: Boolean; lastOp: TWPathCmd.IEType;
        var coordinate: TWPointF; var nextCoordinate: TWPointF; var curveStart: TWPointF;
        var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean;
var
    startPoint, curveControl1, curveControl2, endPoint: TGpPointF;
begin
    // search for current value to update. NOTE a "smooth curve to" instruction may contain more
    // than 4 points. In this case, the remaining points should be considered as subsequent
    // "polycurve to" instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands)
    case (index mod 4) of
        0:
        begin
            // is relative?
            if (relative) then
                // calculate next curve end x coordinate relatively to current coordinate
                curveEnd.X := coordinate.X + point
            else
                // get next curve end x coordinate
                curveEnd.X := point;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
                // calculate next curve end y coordinate relatively to current coordinate
                curveEnd.Y := coordinate.Y + point
            else
                // get next curve end y coordinate
                curveEnd.Y := point;

            Exit(True);
        end;

        2:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        3:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // last operation was a curve or other smooth curve?
            if ((lastOp <> TWPathCmd.IEType.IE_IT_CurveTo)
                    and (lastOp <> TWPathCmd.IEType.IE_IT_Smooth_CurveTo))
            then
                // set current coordinate as start curve control point
                curveStart := coordinate
            else
            begin
                // calculate curve start (it's the mirror of last curve end)
                curveStart.X := coordinate.X + (coordinate.X - lastCurveEnd.X);
                curveStart.Y := coordinate.Y + (coordinate.Y - lastCurveEnd.Y);
            end;

            // add start x and y to final coordinates
            startPoint.X    := x + coordinate.X;
            startPoint.Y    := y + coordinate.Y;
            curveControl1.X := x + curveStart.X;
            curveControl1.Y := y + curveStart.Y;
            curveControl2.X := x + curveEnd.X;
            curveControl2.Y := y + curveEnd.Y;
            endPoint.X      := x + nextCoordinate.X;
            endPoint.Y      := y + nextCoordinate.Y;

            // add cubic Bézier curve
            m_pOutput.AddBezier(startPoint, curveControl1, curveControl2, endPoint);

            // update coordinates
            coordinate   := nextCoordinate;
            lastCurveEnd := curveEnd;

            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.IQuadraticBezierCurveToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.IQuadraticBezierCurveToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_Quadratic_Bezier_CurveTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.IQuadraticBezierCurveToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IQuadraticBezierCurveToConverter.OnConvert(index: NativeUInt;
        x, y: Integer; point: Single; relative: Boolean; lastOp: TWPathCmd.IEType;
        var coordinate: TWPointF; var nextCoordinate: TWPointF; var curveStart: TWPointF;
        var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean;
var
    startPoint, curveControl1, curveControl2, endPoint: TGpPointF;
begin
    // search for current value to update. NOTE a "quadratic curve to" instruction may contain more
    // than 4 points. In this case, the remaining points should be considered as subsequent
    // "quadratic polycurve to" instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands)
    case (index mod 4) of
        0:
        begin
            // is relative?
            if (relative) then
            begin
                // calculate next curve start and end x coordinate relatively to current coordinate
                curveStart.X := coordinate.X + point;
                curveEnd.X   := curveStart.X;
            end
            else
            begin
                // get next curve start and end x coordinate
                curveStart.X := point;
                curveEnd.X   := point;
            end;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
            begin
                // calculate next curve start and end y coordinate relatively to current coordinate
                curveStart.Y := coordinate.Y + point;
                curveEnd.Y   := coordinate.Y + point;
            end
            else
            begin
                // get next curve start and end y coordinate
                curveStart.Y := point;
                curveEnd.Y   := point;
            end;

            Exit(True);
        end;

        2:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        3:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // add start x and y to final coordinates
            startPoint.X    := x + coordinate.X;
            startPoint.Y    := y + coordinate.Y;
            curveControl1.X := x + curveStart.X;
            curveControl1.Y := y + curveStart.Y;
            curveControl2.X := x + curveEnd.X;
            curveControl2.Y := y + curveEnd.Y;
            endPoint.X      := x + nextCoordinate.X;
            endPoint.Y      := y + nextCoordinate.Y;

            // add cubic Bézier curve
            m_pOutput.AddBezier(startPoint, curveControl1, curveControl2, endPoint);

            // update coordinates
            coordinate   := nextCoordinate;
            lastCurveEnd := curveEnd;

            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.ISmoothQuadraticBezierCurveToConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.ISmoothQuadraticBezierCurveToConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type    := TWPathCmd.IEType.IE_IT_Smooth_Quadratic_Bezier_CurveTo;
    m_pOutput := pOutput;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.ISmoothQuadraticBezierCurveToConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.ISmoothQuadraticBezierCurveToConverter.OnConvert(index: NativeUInt;
        x, y: Integer; point: Single; relative: Boolean; lastOp: TWPathCmd.IEType;
        var coordinate: TWPointF; var nextCoordinate: TWPointF; var curveStart: TWPointF;
        var curveEnd: TWPointF; var lastCurveEnd: TWPointF): Boolean;
var
    startPoint, curveControl1, curveControl2, endPoint: TGpPointF;
begin
    // search for current value to update. NOTE a "smooth quadratic curve to" instruction may contain
    // more than 2 points. In this case, the remaining points should be considered as subsequent
    // "smooth quadratic polycurve to" instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands)
    case (index mod 2) of
        0:
        begin
            // is relative?
            if (relative) then
                // calculate next x coordinate relatively to current
                nextCoordinate.X := coordinate.X + point
            else
                // get next x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        1:
        begin
            // is relative?
            if (relative) then
                // calculate next y coordinate relatively to current
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next y coordinate
                nextCoordinate.Y := point;

            // last operation was a curve or other smooth curve?
            if ((lastOp <> TWPathCmd.IEType.IE_IT_Quadratic_Bezier_CurveTo)
                    and (lastOp <> TWPathCmd.IEType.IE_IT_Smooth_Quadratic_Bezier_CurveTo))
            then
            begin
                // set current coordinate as curve control point
                curveStart := coordinate;
                curveEnd   := coordinate;
            end
            else
            begin
                // calculate curve control point (it's the mirror of last curve control point)
                curveStart.X := coordinate.X + (coordinate.X - lastCurveEnd.X);
                curveEnd.X   := curveStart.X;
                curveStart.Y := coordinate.Y + (coordinate.Y - lastCurveEnd.Y);
                curveEnd.Y   := curveStart.Y;
            end;

            // add start x and y to final coordinates
            startPoint.X    := x + coordinate.X;
            startPoint.Y    := y + coordinate.Y;
            curveControl1.X := x + curveStart.X;
            curveControl1.Y := y + curveStart.Y;
            curveControl2.X := x + curveEnd.X;
            curveControl2.Y := y + curveEnd.Y;
            endPoint.X      := x + nextCoordinate.X;
            endPoint.Y      := y + nextCoordinate.Y;

            // add cubic Bézier curve
            m_pOutput.AddBezier(startPoint, curveControl1, curveControl2, endPoint);

            // update coordinates
            coordinate   := nextCoordinate;
            lastCurveEnd := curveEnd;

            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus.IEllipticalArcConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.IEllipticalArcConverter.Create(pOutput: TGpGraphicsPath);
begin
    inherited Create;

    m_Type     := TWPathCmd.IEType.IE_IT_Elliptical_Arc;
    m_pOutput  := pOutput;
    m_Radius   := Default(TWPointF);
    m_Angle    := 0.0;
    m_LargeArc := False;
    m_Sweep    := False;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.IEllipticalArcConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.IEllipticalArcConverter.OnConvert(index: NativeUInt; x, y:
        Integer; point: Single; relative: Boolean; var coordinate: TWPointF;
        var nextCoordinate: TWPointF): Boolean;
var
    startPoint, endPoint: TWVector2;
    startLine, endLine:   TGpPointF;
    wPoints:              TWGeometryTools.IPoints;
    pointCount, i:        NativeInt;
    isLine:               Boolean;
    points:               array of TGpPointF;
begin
    // search for current value to update. NOTE a "elliptical arc" instruction may contain more than
    // 7 points. In this case, the remaining points should be considered as subsequent "elliptical
    // arc" instructions (see https://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands)
    case (index mod 7) of
        0:
        begin
            // get arc x radius
            m_Radius.X := point;
            Exit(True);
        end;

        1:
        begin
            // get arc y radius
            m_Radius.Y := point;
            Exit(True);
        end;

        2:
        begin
            // get rotation angle (always on x axis)
            m_Angle := point;
            Exit(True);
        end;

        3:
        begin
            // is a large arc?
            m_LargeArc := (point <> 0.0);
            Exit(True);
        end;

        4:
        begin
            // is sweep?
            m_Sweep := (point <> 0.0);
            Exit(True);
        end;

        // x end
        5:
        begin
            // is relative?
            if (relative) then
                // calculate next arc end x coordinate relatively to current coordinate
                nextCoordinate.X := coordinate.X + point
            else
                // get next arc end x coordinate
                nextCoordinate.X := point;

            Exit(True);
        end;

        // y end
        6:
        begin
            // is relative?
            if (relative) then
                // calculate next arc end y coordinate relatively to current coordinate
                nextCoordinate.Y := coordinate.Y + point
            else
                // get next arc end y coordinate
                nextCoordinate.Y := point;

            // add start x and y to final coordinates
            startPoint.X := x + coordinate.X;
            startPoint.Y := y + coordinate.Y;
            endPoint.X   := x + nextCoordinate.X;
            endPoint.Y   := y + nextCoordinate.Y;

            // calculate arc points
            TWGeometryTools.GetArc(startPoint, endPoint, TWGeometryTools.DegToRad(m_Angle),
                    m_Radius.X, m_Radius.Y, m_Sweep, m_LargeArc, 25, wPoints, isLine);

            // is arc a line?
            if (isLine) then
            begin
                startLine.X := wPoints[0].X;
                startLine.Y := wPoints[0].Y;
                endLine.X   := wPoints[1].X;
                endLine.Y   := wPoints[1].Y;

                // draw line
                m_pOutput.AddLine(startLine, endLine);
            end
            else
            begin
                // get point to draw count
                pointCount := Length(wPoints);

                if (pointCount > 0) then
                begin
                    SetLength(points, pointCount);

                    // convert arc points to GDI+
                    for i := 0 to pointCount - 1 do
                    begin
                        points[i].X := wPoints[i].X;
                        points[i].Y := wPoints[i].Y;
                    end;

                    // draw arc
                    m_pOutput.AddCurve(PGPPointF(points), pointCount);
                end;
            end;

            // update coordinate
            coordinate := nextCoordinate;

            Exit(True);
        end;
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
procedure TWGraphicPathConverter_GDIPlus.IEllipticalArcConverter.Clear;
begin
    m_Radius   := Default(TWPointF);
    m_Angle    := 0.0;
    m_LargeArc := False;
    m_Sweep    := False;
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter.Process(const rect: TWRectF; const path: TWPathCmds): Boolean;
begin
    Result := ConvertPath(rect, Round(rect.Left), Round(rect.Top), path);
end;
//---------------------------------------------------------------------------
// TWGraphicPathConverter_GDIPlus
//---------------------------------------------------------------------------
constructor TWGraphicPathConverter_GDIPlus.Create(pGraphicsPath: TGpGraphicsPath);
begin
    inherited Create;

    // no GDI+ path?
    if (not Assigned(pGraphicsPath)) then
        raise Exception.Create('Output GDI+ graphics path is missing');

    m_pGraphicsPath                 := pGraphicsPath;
    m_pMoveTo                       := IMoveToConverter.Create(pGraphicsPath);
    m_pLineTo                       := ILineToConverter.Create(pGraphicsPath);
    m_pHorzLineTo                   := IHorzLineToConverter.Create(pGraphicsPath);
    m_pVertLineTo                   := IVertLineToConverter.Create(pGraphicsPath);
    m_pCurveTo                      := ICurveToConverter.Create(pGraphicsPath);
    m_pSmoothCurveTo                := ISmoothCurveToConverter.Create(pGraphicsPath);
    m_pQuadraticBezierCurveTo       := IQuadraticBezierCurveToConverter.Create(pGraphicsPath);
    m_pSmoothQuadraticBezierCurveTo := ISmoothQuadraticBezierCurveToConverter.Create(pGraphicsPath);
    m_pIEllipticalArc               := IEllipticalArcConverter.Create(pGraphicsPath);

    // configure converters
    m_pMoveTo.EnableRounding(True);
    m_pLineTo.EnableRounding(True);
    m_pHorzLineTo.EnableRounding(True);
    m_pVertLineTo.EnableRounding(True);
    m_pCurveTo.EnableRounding(True);
    m_pSmoothCurveTo.EnableRounding(True);
    m_pQuadraticBezierCurveTo.EnableRounding(True);
    m_pSmoothQuadraticBezierCurveTo.EnableRounding(True);
    m_pIEllipticalArc.EnableRounding(True);
end;
//---------------------------------------------------------------------------
destructor TWGraphicPathConverter_GDIPlus.Destroy;
begin
    m_pMoveTo.Free;
    m_pLineTo.Free;
    m_pHorzLineTo.Free;
    m_pVertLineTo.Free;
    m_pCurveTo.Free;
    m_pSmoothCurveTo.Free;
    m_pQuadraticBezierCurveTo.Free;
    m_pSmoothQuadraticBezierCurveTo.Free;
    m_pIEllipticalArc.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGraphicPathConverter_GDIPlus.ConvertPath(const rect: TWRectF; x, y: Integer;
        const path: TWPathCmds): Boolean;
var
    pPathCmd:                             TWPathCmd;
    startCoord, coordinate, lastCurveEnd: TWPointF;
    lastOp:                               TWPathCmd.IEType;
    pathCount:                            NativeInt;
    isFirst:                              Boolean;
begin
    pathCount := path.Count;

    // no GDI+ path?
    if (pathCount = 0) then
        Exit(False);

    lastOp := IE_IT_Unknown;

    // update position to consider bounding rect
    Dec(x, Trunc(rect.Left));
    Dec(y, Trunc(rect.Top));

    isFirst := True;

    // iterate through path instructions to execute
    for pPathCmd in path do
    begin
        // search for instruction type
        case (pPathCmd.m_Type) of
            TWPathCmd.IEType.IE_IT_MoveTo:
            begin
                // first sub-path?
                if (lastOp <> TWPathCmd.IEType.IE_IT_ClosePath) then
                    // start a new sub-path but don't close the current one
                    m_pGraphicsPath.StartFigure;

                // first instruction and relative "move to"?
                if (isFirst and pPathCmd.m_Relative) then
                    // force to be an absolute coordinate (see
                    // https://www.w3.org/TR/SVG/paths.html#PathDataMovetoCommands)
                    coordinate := TWPointF.Create(x, y);

                isFirst := False;

                // convert "move to" instruction from path to GDI+
                if (not m_pMoveTo.Convert(pPathCmd, x, y, coordinate)) then
                    Exit(False);

                // get the figure starting point. It is required to reset the coordinate system to
                // the correct location (i.e. starting point) after the figure was closed
                startCoord := m_pMoveTo.GetStartPoint;

                // update last executed operation. Also consider subsequent lineTo, in case
                // instruction contained one
                if (pPathCmd.PointCount > 2) then
                    lastOp := TWPathCmd.IEType.IE_IT_LineTo
                else
                    lastOp := TWPathCmd.IEType.IE_IT_MoveTo;
            end;

            TWPathCmd.IEType.IE_IT_LineTo:
            begin
                // convert "line to" instruction from path to GDI+
                if (not m_pLineTo.Convert(pPathCmd, x, y, coordinate)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_LineTo;
            end;

            TWPathCmd.IEType.IE_IT_Horiz_LineTo:
            begin
                // convert "horizontal line to" instruction from path to GDI+
                if (not m_pHorzLineTo.Convert(pPathCmd, x, y, coordinate)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Horiz_LineTo;
            end;

            TWPathCmd.IEType.IE_IT_Vert_LineTo:
            begin
                // convert "vertical line to" instruction from path to GDI+
                if (not m_pVertLineTo.Convert(pPathCmd, x, y, coordinate)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Vert_LineTo;
            end;

            TWPathCmd.IEType.IE_IT_CurveTo:
            begin
                // convert "curve to" instruction from path to GDI+
                if (not m_pCurveTo.Convert(pPathCmd, x, y, lastOp, coordinate, lastCurveEnd)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_CurveTo;
            end;

            TWPathCmd.IEType.IE_IT_Smooth_CurveTo:
            begin
                // convert "smooth curve to" instruction from path to GDI+
                if (not m_pSmoothCurveTo.Convert(pPathCmd, x, y, lastOp, coordinate, lastCurveEnd)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Smooth_CurveTo;
            end;

            TWPathCmd.IEType.IE_IT_Quadratic_Bezier_CurveTo:
            begin
                // convert "quadratic bezier curve to" instruction from path to GDI+
                if (not m_pQuadraticBezierCurveTo.Convert(pPathCmd, x, y, lastOp, coordinate,
                        lastCurveEnd))
                then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Quadratic_Bezier_CurveTo;
            end;

            TWPathCmd.IEType.IE_IT_Smooth_Quadratic_Bezier_CurveTo:
            begin
                // convert "smooth quadratic bezier curve to" instruction from path to GDI+
                if (not m_pSmoothQuadraticBezierCurveTo.Convert(pPathCmd, x, y, lastOp, coordinate,
                        lastCurveEnd))
                then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Smooth_Quadratic_Bezier_CurveTo;
            end;

            TWPathCmd.IEType.IE_IT_Elliptical_Arc:
            begin
                // convert "elliptical arc" instruction from path to GDI+
                if (not m_pIEllipticalArc.Convert(pPathCmd, x, y, coordinate)) then
                    Exit(False);

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_Elliptical_Arc;
            end;

            // close all figures
            TWPathCmd.IEType.IE_IT_ClosePath:
            begin
                // close the current figure. This will also implicitly begin a new one
                m_pGraphicsPath.CloseFigure;

                // reset the current coordinate to the starting point, because the figure was closed
                // and GDI+ implicitly drawn the final closing line
                coordinate := startCoord;

                // update last executed operation
                lastOp := TWPathCmd.IEType.IE_IT_ClosePath;
            end;
        else
            TWLogHelper.LogToCompiler('Convert path - FAILED - unknown instruction - '
                    + IntToStr(Integer(pPathCmd.m_Type)));
            Exit(False);
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------

end.
