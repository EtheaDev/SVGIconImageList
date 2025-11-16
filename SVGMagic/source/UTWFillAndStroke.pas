{**
 @abstract(@name provides generic brush and pen classes, that can be used for fill and stroke
           operations in a generic way between several libraries, as e.g. GDI, GDI+, Direct2D, ...)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWFillAndStroke;

interface

uses System.SysUtils,
     System.Generics.Defaults,
     System.Generics.Collections,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     UTWColor,
     UTWPoint,
     UTWSize,
     UTWVector,
     UTWMatrix;

type
    {**
     Brush type, determine what kind of color the brush contains
     @value(E_BT_Solid The brush contains a solid color)
     @value(E_BT_Linear The brush contains a linear gradient)
     @value(E_BT_Radial The brush contains a radial gradient)
    }
    EBrushType =
    (
        E_BT_Solid,
        E_BT_Linear,
        E_BT_Radial
    );

    {**
     Gradient unit enumeration
     @value(E_GU_ObjectBoundingBox The unit coordinates system is based on the local bounding box
                                   surrounding the object)
     @value(E_GU_UserSpaceOnUse The unit coordinates system is based on the user space (e.g. a page)
                                where the entire rendering is processed)
    }
    EGradientUnit =
    (
        E_GU_ObjectBoundingBox,
        E_GU_UserSpaceOnUse
    );

    {**
     Wrapping mode enumeration
     @value(E_WM_Tile The pattern is tiled)
     @value(E_WM_TileFlipX The pattern is flipped on the x axis and tiled)
     @value(E_WM_TileFlipY The pattern is flipped on the y axis and tiled)
     @value(E_WM_TileFlipXY The pattern is flipped on the both x and y axis and tiled)
     @value(E_WM_Clamp The pattern is clamped)
    }
    EWrapMode =
    (
        E_WM_Tile,
        E_WM_TileFlipX,
        E_WM_TileFlipY,
        E_WM_TileFlipXY,
        E_WM_Clamp
    );

    {**
     Linecap enumeration
     @value(E_LC_Default The currently defined value is used)
     @value(E_LC_Flat The line ends at the last point, the end is squared off)
     @value(E_LC_Square Square cap, the center of the square is the last point in the line, the
                        height and width of the square are the line width)
     @value(E_LC_Round Circular cap, the center of the circle is the last point in the line, the
                       diameter of the circle is the line width)
     @value(E_LC_Triangle Triangular cap, the base of the triangle is the last point in the line,
                          the base of the triangle is the line width)
     @value(E_LC_NoAnchor The line ends are not anchored)
     @value(E_LC_SquareAnchor The line ends are anchored with a square, the center of the square is
                              the last point in the line, the height and width of the square are the
                              line width)
     @value(E_LC_RoundAnchor The line ends are anchored with a circle, the center of the circle is
                             at the last point in the line, the circle is wider than the line)
     @value(E_LC_DiamondAnchor The line ends are anchored with a diamond (a square turned at 45
                               degrees), the center of the diamond is at the last point in the line,
                               the diamond is wider than the line)
     @value(E_LC_ArrowAnchor The line ends are anchored with arrowheads, the arrowhead point is
                             located at the last point in the line, the arrowhead is wider than the
                             line)
     @value(E_LC_Custom The line ends are made from a custom linecap)
    }
    ELineCap =
    (
        E_LC_Default,
        E_LC_Flat,
        E_LC_Square,
        E_LC_Round,
        E_LC_Triangle,
        E_LC_NoAnchor,
        E_LC_SquareAnchor,
        E_LC_RoundAnchor,
        E_LC_DiamondAnchor,
        E_LC_ArrowAnchor,
        E_LC_Custom
    );

    {**
     Dashcap enumeration
     @value(E_DC_Default The currently defined value is used)
     @value(E_DC_Flat Square cap that squares off both ends of each dash)
     @value(E_DC_Round Circular cap that rounds off both ends of each dash)
     @value(E_DC_Triangle Triangular cap that points both ends of each dash)
    }
    EDashCap =
    (
        E_DC_Default,
        E_DC_Flat,
        E_DC_Round,
        E_DC_Triangle
    );

    {**
     Line join enumeration
     @value(E_LJ_Default The currently defined value is used)
     @value(E_LJ_Round Circular join. This produces a smooth, circular arc between the lines)
     @value(E_LJ_Bevel Beveled join. This produces a diagonal corner)
     @value(E_LJ_Miter Mitered join. This produces a sharp corner or a clipped corner, depending on
                       whether the length of the miter exceeds the miter limit)
     @value(E_LJ_MiterClipped Mitered join. This produces a sharp corner or a beveled corner,
                              depending on whether the length of the miter exceeds the miter limit)
    }
    ELineJoin =
    (
        E_LJ_Default,
        E_LJ_Round,
        E_LJ_Bevel,
        E_LJ_Miter,
        E_LJ_MiterClipped
    );

    TWDashPattern = TList<Double>;

    {**
     Gradient stop, it's a color with a position in the gradient, in pixels or in percent
     @br @bold(NOTE) If percent position is set to -1.0f, the normal position will be used instead
    }
    TWGradientStop = class
        private
            m_Color:    TWColor;
            m_Position: TWPointF;
            m_Percent:  Double;

        protected
            {**
             Get color
             @returns(Color)
            }
            function GetColor: PWColor; virtual;

            {**
             Get position
             @returns(Position)
            }
            function GetPosition: PWPointF; virtual;

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
             Check if gradient stop content is equal to another gradient stop
             @param(pOther Other gradient stop to compare with)
             @returns(@true if gradient stops are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWGradientStop): Boolean; virtual;

            {**
             Check if gradient stop content differs from another gradient stop
             @param(pOther Other gradient stop to compare with)
             @returns(@true if gradient stops differ, otherwise @false)
            }
            function Differs(const pOther: TWGradientStop): Boolean; virtual;

            {**
             Assign (i.e. copy) the gradient stop
             @param(pOther Other gradient stop to copy from)
            }
            procedure Assign(const pOther: TWGradientStop); virtual;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; reintroduce; virtual;

            {**
             Set color
             @param(pColor Color to set)
            }
            procedure SetColor(const pColor: PWColor); virtual;

            {**
             Set position
             @param(pPosition Position to set)
            }
            procedure SetPosition(const pPosition: PWPointF); virtual;

        { Properties }
        public
            {**
             Get the gradient stop color
            }
            property Color: PWColor read GetColor;

            {**
             Get the gradient stop position
            }
            property Position: PWPointF read GetPosition;

            {**
             Get or set the gradient stop position in percent (between 0.0 and 1.0)
            }
            property Percent: Double read m_Percent write m_Percent;
    end;

    TWGradientStops = TObjectList<TWGradientStop>;

    {**
     Linear gradient vector
    }
    TWLinearGradientVector = class
        private
            m_Start: TWVector2;
            m_End:   TWVector2;

        protected
            {**
             Get the vector width
             @returns(The vector width)
            }
            function GetWidth: Single; virtual;

            {**
             Get the vector height
             @returns(The vector height)
            }
            function GetHeight: Single; virtual;

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
             Check if gradient vector content is equal to another gradient vector
             @param(pOther Other gradient vector to compare with)
             @returns(@true if gradient vectors are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWLinearGradientVector): Boolean; virtual;

            {**
             Check if gradient vector content differs from another gradient vector
             @param(pOther Other gradient vector to compare with)
             @returns(@true if gradient vectors differ, otherwise @false)
            }
            function Differs(const pOther: TWLinearGradientVector): Boolean; virtual;

            {**
             Assign (i.e. copy) the gradient vector
             @param(pOther Other gradient vector to copy from)
            }
            procedure Assign(const pOther: TWLinearGradientVector); virtual;

            {**
             Clear the vector content
            }
            procedure Clear; virtual;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; reintroduce; virtual;

            {**
             Check if linear gradient vector is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if gradient vector is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

        { Properties }
        public
            {**
             Get or set the gradient start position
            }
            property GradientStart: TWVector2 read m_Start write m_Start;

            {**
             Get or set the gradient end position
            }
            property GradientEnd: TWVector2 read m_End write m_End;

            {**
             Get the vector width
            }
            property Width: Single read GetWidth;

            {**
             Get the vector height
            }
            property Height: Single read GetHeight;
    end;

    {**
     Basic brush
    }
    TWBrush = class
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
             Check if brush content is equal to another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWBrush): Boolean; virtual; abstract;

            {**
             Check if brush content differs from another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes differ, otherwise @false)
            }
            function Differs(const pOther: TWBrush): Boolean; virtual; abstract;

            {**
             Clear the brush content and reset it to default
            }
            procedure Clear; virtual; abstract;

            {**
             Assign brush
             @param(pOther Other brush to copy from)
            }
            procedure Assign(const pOther: TWBrush); virtual; abstract;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; reintroduce; virtual; abstract;

            {**
             Check if brush is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if brush is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual; abstract;
    end;

    {**
     Solid brush
    }
    TWSolidBrush = class(TWBrush)
        private
            m_Color: TWColor;

        protected
            {**
             Get color
             @returns(Color)
            }
            function GetColor: PWColor; virtual;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if brush content is equal to another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWBrush): Boolean; override;

            {**
             Check if brush content differs from another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes differ, otherwise @false)
            }
            function Differs(const pOther: TWBrush): Boolean; override;

            {**
             Clear the brush content and reset it to default
            }
            procedure Clear; override;

            {**
             Assign brush
             @param(pOther Other brush to copy from)
            }
            procedure Assign(const pOther: TWBrush); override;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; override;

            {**
             Set color
             @param(pColor Color to set)
            }
            procedure SetColor(const pColor: PWColor); virtual;

            {**
             Check if brush is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if brush is empty, otherwise @false)
            }
            function IsEmpty: Boolean; override;

        { Properties }
        public
            {**
             Get or set the solid color
            }
            property Color: PWColor read GetColor write SetColor;
    end;

    {**
     Basic gradient brush
    }
    TWGradientBrush = class(TWBrush)
        private
            m_pStops:   TWGradientStops;
            m_Matrix:   TWMatrix3x3;
            m_Unit:     EGradientUnit;
            m_WrapMode: EWrapMode;

        protected
            {**
             Get the transformation matrix
             @returns(Transformation matrix)
            }
            function GetMatrix: PWMatrix3x3; virtual;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if brush content is equal to another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWBrush): Boolean; override;

            {**
             Check if brush content differs from another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes differ, otherwise @false)
            }
            function Differs(const pOther: TWBrush): Boolean; override;

            {**
             Clear the brush content and reset it to default
            }
            procedure Clear; override;

            {**
             Assign brush
             @param(pOther Other brush to copy from)
            }
            procedure Assign(const pOther: TWBrush); override;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; override;

            {**
             Check if brush is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if brush is empty, otherwise @false)
            }
            function IsEmpty: Boolean; override;

            {**
             Set the transformation matrix
             @param(pMatrix Transformation matrix to set)
            }
            procedure SetMatrix(const pMatrix: PWMatrix3x3); virtual;

        { Properties }
        public
            {**
             Get or set the gradient stops
            }
            property Stops: TWGradientStops read m_pStops;

            {**
             Get the transformation matrix
            }
            property Matrix: PWMatrix3x3 read GetMatrix;

            {**
             Get or set the gradient unit
            }
            property GradientUnit: EGradientUnit read m_Unit write m_Unit default E_GU_ObjectBoundingBox;

            {**
             Get or set the gradient wrap mode
            }
            property WrapMode: EWrapMode read m_WrapMode write m_WrapMode default E_WM_Clamp;
    end;

    {**
     Linear gradient brush
    }
    TWLinearGradientBrush = class(TWGradientBrush)
        private
            m_pVector: TWLinearGradientVector;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if brush content is equal to another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWBrush): Boolean; override;

            {**
             Check if brush content differs from another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes differ, otherwise @false)
            }
            function Differs(const pOther: TWBrush): Boolean; override;

            {**
             Clear the brush content and reset it to default
            }
            procedure Clear; override;

            {**
             Assign brush
             @param(pOther Other brush to copy from)
            }
            procedure Assign(const pOther: TWBrush); override;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; override;

            {**
             Check if brush is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if brush is empty, otherwise @false)
            }
            function IsEmpty: Boolean; override;

        { Properties }
        public
            {**
             Get or set the gradient vector
            }
            property Vector: TWLinearGradientVector read m_pVector;
    end;

    {**
     Radial gradient brush
    }
    TWRadialGradientBrush = class(TWGradientBrush)
        private
            m_Center:      TWPointF;
            m_Focus:       TWPointF;
            m_Radius:      TWSizeF;
            m_ScaleFactor: TWSizeF; // indicative but must always be ignored while hash key is computed

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if brush content is equal to another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWBrush): Boolean; override;

            {**
             Check if brush content differs from another brush
             @param(pOther Other brush to compare with)
             @returns(@true if brushes differ, otherwise @false)
            }
            function Differs(const pOther: TWBrush): Boolean; override;

            {**
             Clear the brush content and reset it to default
            }
            procedure Clear; override;

            {**
             Assign brush
             @param(pOther Other brush to copy from)
            }
            procedure Assign(const pOther: TWBrush); override;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; override;

            {**
             Check if brush is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if brush is empty, otherwise @false)
            }
            function IsEmpty: Boolean; override;

        { Properties }
        public
            {**
             Get or set the gradient center
            }
            property Center: TWPointF read m_Center write m_Center;

            {**
             Get or set the gradient focus
            }
            property Focus: TWPointF read m_Focus write m_Focus;

            {**
             Get or set the gradient radius
            }
            property Radius: TWSizeF read m_Radius write m_Radius;

            {**
             Get or set the gradient scale factor
            }
            property ScaleFactor: TWSizeF read m_ScaleFactor write m_ScaleFactor;
    end;

    {**
     Fill properties, contains the style to apply to fill a shape with a graphical library like e.g. GDI
    }
    TWFill = class
        private
            m_Type:   EBrushType;
            m_pBrush: TWBrush;

            {**
             Clear memory
            }
            procedure ClearMemory;

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
             @param(pOther Other fill to create from)
            }
            constructor Create(const pOther: TWFill); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if fill content is equal to another fill
             @param(pOther Other fill to compare with)
             @returns(@true if fills are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWFill): Boolean; virtual;

            {**
             Check if fill content differs from another fill
             @param(pOther Other fill to compare with)
             @returns(@true if fills differ, otherwise @false)
            }
            function Differs(const pOther: TWFill): Boolean; virtual;

            {**
             Clear the fill content and reset it to default
            }
            procedure Clear; virtual;

            {**
             Assign fill
             @param(pOther Other fill to copy from)
            }
            procedure Assign(const pOther: TWFill); virtual;

            {**
             Check if fill is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if fill is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; reintroduce; virtual;

            {**
             Set the brush
             @param(brush Brush to set)
            }
            procedure SetBrush(const pBrush: TWBrush); virtual;

        { Properties }
        public
            {**
             Get or set the brush
            }
            property Brush: TWBrush read m_pBrush write SetBrush;

            {**
             Get the brush type
            }
            property BrushType: EBrushType read m_Type;
    end;

    {**
     Stroke properties, contains the style to apply to outline a shape with a graphical library like e.g. GDI
    }
    TWStroke = class
        private
            m_LineCap:      ELineCap;
            m_DashCap:      EDashCap;
            m_LineJoin:     ELineJoin;
            m_pDashPattern: TWDashPattern;
            m_DashOffset:   Double;
            m_Width:        Double;
            m_Matrix:       TWMatrix3x3;
            m_Type:         EBrushType;
            m_pBrush:       TWBrush;

            {**
             Clear memory
            }
            procedure ClearMemory;

        protected
            {**
             Get the transformation matrix
             @returns(Transformation matrix)
            }
            function GetMatrix: PWMatrix3x3; virtual;

            {**
             Get the dash pattern count
            }
            function GetDashPatternCount: NativeUInt; virtual;

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
             @param(pOther Other stroke to create from)
            }
            constructor Create(const pOther: TWStroke); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Check if stroke content is equal to another stroke
             @param(pOther Other stroke to compare with)
             @returns(@true if strokes are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWStroke): Boolean; virtual;

            {**
             Check if stroke content differs from another stroke
             @param(pOther Other stroke to compare with)
             @returns(@true if strokes differ, otherwise @false)
            }
            function Differs(const pOther: TWStroke): Boolean; virtual;

            {**
             Clear the stroke content and reset it to default
            }
            procedure Clear; virtual;

            {**
             Assign stroke
             @param(pOther Other stroke to copy from)
            }
            procedure Assign(const pOther: TWStroke); virtual;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; reintroduce; virtual;

            {**
             Check if stroke is empty (i.e. if its content has no effect on the drawing)
             @returns(@true if stroke is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

            {**
             Set the transformation matrix
             @param(pMatrix Transformation matrix to set)
            }
            procedure SetMatrix(const pMatrix: PWMatrix3x3); virtual;

            {**
             Set the base brush
             @param(brush Brush to set)
            }
            procedure SetBrush(const pBrush: TWBrush); virtual;

        { Properties }
        public
            {**
             Get or set the linecap
            }
            property LineCap: ELineCap read m_LineCap write m_LineCap;

            {**
             Get or set the dashcap
            }
            property DashCap: EDashCap read m_DashCap write m_DashCap;

            {**
             Get or set the line join
            }
            property LineJoin: ELineJoin read m_LineJoin write m_LineJoin;

            {**
             Get the dash pattern
            }
            property DashPattern: TWDashPattern read m_pDashPattern;

            {**
             Get the dash pattern count
            }
            property DashPatternCount: NativeUInt read GetDashPatternCount;

            {**
             Get or set the dash offset
            }
            property DashOffset: Double read m_DashOffset write m_DashOffset;

            {**
             Get or set the width
            }
            property Width: Double read m_Width write m_Width;

            {**
             Get the transformation matrix
            }
            property Matrix: PWMatrix3x3 read GetMatrix;

            {**
             Get or set the brush
            }
            property Brush: TWBrush read m_pBrush write SetBrush;

            {**
             Get the base brush type
            }
            property BrushType: EBrushType read m_Type;
    end;

implementation
//---------------------------------------------------------------------------
// TWGradientStop
//---------------------------------------------------------------------------
constructor TWGradientStop.Create;
begin
    inherited Create;

    m_Color    :=  TWColor.GetDefault;
    m_Position :=  Default(TWPointF);
    m_Percent  := -1.0;
end;
//---------------------------------------------------------------------------
destructor TWGradientStop.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGradientStop.GetColor: PWColor;
begin
    Result := @m_Color;
end;
//---------------------------------------------------------------------------
function TWGradientStop.GetPosition: PWPointF;
begin
    Result := @m_Position;
end;
//---------------------------------------------------------------------------
function TWGradientStop.IsEqual(const pOther: TWGradientStop): Boolean;
begin
    Result := (m_Color.IsEqual(pOther.m_Color) and (m_Percent = pOther.m_Percent)
            and (m_Position.X = pOther.m_Position.X) and (m_Position.Y = pOther.m_Position.Y));
end;
//---------------------------------------------------------------------------
function TWGradientStop.Differs(const pOther: TWGradientStop): Boolean;
begin
    Result := (m_Color.Differs(pOther.m_Color) or (m_Percent <> pOther.m_Percent)
            or (m_Position.X <> pOther.m_Position.X) or (m_Position.Y <> pOther.m_Position.Y));
end;
//---------------------------------------------------------------------------
procedure TWGradientStop.Assign(const pOther: TWGradientStop);
begin
    if (not Assigned(pOther)) then
    begin
        m_Color    :=  TWColor.GetDefault;
        m_Position :=  Default(TWPointF);
        m_Percent  := -1.0;
        Exit;
    end;

    // copy values
    m_Position := pOther.m_Position;
    m_Percent  := pOther.m_Percent;
    m_Color.Assign(pOther.m_Color);
end;
//---------------------------------------------------------------------------
function TWGradientStop.GetHashCode(initValue: Integer): Integer;
begin
    Result := m_Color.GetHashCode(initValue);
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Percent, SizeOf(Single), Result);
    {$else}
        Result := BobJenkinsHash(m_Percent, SizeOf(Single), Result);
    {$ifend}
    Result := m_Position.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
procedure TWGradientStop.SetColor(const pColor: PWColor);
begin
    if (not Assigned(pColor)) then
        raise Exception.Create('Color is undefined');

    m_Color.Assign(pColor^);
end;
//---------------------------------------------------------------------------
procedure TWGradientStop.SetPosition(const pPosition: PWPointF);
begin
    m_Position.X := pPosition.X;
    m_Position.Y := pPosition.Y;
end;
//---------------------------------------------------------------------------
// TWLinearGradientVector
//---------------------------------------------------------------------------
constructor TWLinearGradientVector.Create;
begin
    inherited Create;

    m_Start := Default(TWVector2);
    m_End   := Default(TWVector2);
end;
//---------------------------------------------------------------------------
destructor TWLinearGradientVector.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.GetWidth: Single;
begin
    Result := m_End.X - m_Start.X;
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.GetHeight: Single;
begin
    Result := m_End.Y - m_Start.Y;
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.IsEqual(const pOther: TWLinearGradientVector): Boolean;
begin
    Result := m_Start.IsEqual(pOther.m_Start) and m_End.IsEqual(pOther.m_End);
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.Differs(const pOther: TWLinearGradientVector): Boolean;
begin
    Result := m_Start.Differs(pOther.m_Start) or m_End.Differs(pOther.m_End);
end;
//---------------------------------------------------------------------------
procedure TWLinearGradientVector.Assign(const pOther: TWLinearGradientVector);
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // copy values
    m_Start.Assign(pOther.m_Start);
    m_End.Assign(pOther.m_End);
end;
//---------------------------------------------------------------------------
procedure TWLinearGradientVector.Clear;
begin
    m_Start := Default(TWVector2);
    m_End   := Default(TWVector2);
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.GetHashCode(initValue: Integer): Integer;
begin
    Result := m_Start.GetHashCode(initValue);
    Result := m_End.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
function TWLinearGradientVector.IsEmpty: Boolean;
begin
    // gradient vector has no effect?
    Result := m_Start.IsEqual(m_End);
end;
//---------------------------------------------------------------------------
// TWBrush
//---------------------------------------------------------------------------
constructor TWBrush.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWBrush.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSolidBrush
//---------------------------------------------------------------------------
constructor TWSolidBrush.Create;
begin
    inherited Create;

    m_Color := TWColor.GetDefault;
end;
//---------------------------------------------------------------------------
destructor TWSolidBrush.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSolidBrush.GetColor: PWColor;
begin
    Result := @m_Color;
end;
//---------------------------------------------------------------------------
function TWSolidBrush.IsEqual(const pOther: TWBrush): Boolean;
begin
    if (not Assigned(pOther)) then
        Exit(False);

    // brushes cannot be equal if the other brush isn't a solid brush
    if (not(pOther is TWSolidBrush)) then
        Exit(False);

    Result := (m_Color = (pOther as TWSolidBrush).m_Color);
end;
//---------------------------------------------------------------------------
function TWSolidBrush.Differs(const pOther: TWBrush): Boolean;
begin
    if (not Assigned(pOther)) then
        Exit(True);

    // brushes always differ if the other brush isn't a solid brush
    if (not(pOther is TWSolidBrush)) then
        Exit(True);

    Result := (m_Color <> (pOther as TWSolidBrush).m_Color);
end;
//---------------------------------------------------------------------------
procedure TWSolidBrush.Clear;
begin
    m_Color := TWColor.GetDefault;
end;
//---------------------------------------------------------------------------
procedure TWSolidBrush.Assign(const pOther: TWBrush);
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // cannot assign the content if the other brush isn't a solid brush. In this case reset the values
    if (not(pOther is TWSolidBrush)) then
    begin
        Clear;
        Exit;
    end;

    m_Color.Assign((pOther as TWSolidBrush).m_Color);
end;
//---------------------------------------------------------------------------
function TWSolidBrush.GetHashCode(initValue: Integer): Integer;
begin
    Result := m_Color.GetHashCode(initValue);
end;
//---------------------------------------------------------------------------
procedure TWSolidBrush.SetColor(const pColor: PWColor);
begin
    if (not Assigned(pColor)) then
        raise Exception.Create('Color is undefined');

    m_Color.Assign(pColor^);
end;
//---------------------------------------------------------------------------
function TWSolidBrush.IsEmpty: Boolean;
begin
    Result := m_Color.IsEmpty;
end;
//---------------------------------------------------------------------------
// TWGradientBrush
//---------------------------------------------------------------------------
constructor TWGradientBrush.Create;
begin
    inherited Create;

    m_pStops   := TWGradientStops.Create;
    m_Matrix   := TWMatrix3x3.GetDefault;
    m_Unit     := E_GU_ObjectBoundingBox;
    m_WrapMode := E_WM_Clamp;
end;
//---------------------------------------------------------------------------
destructor TWGradientBrush.Destroy;
begin
    m_pStops.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGradientBrush.IsEqual(const pOther: TWBrush): Boolean;
var
    pSource: TWGradientBrush;
    i:       NativeUInt;
begin
    if (not Assigned(pOther)) then
        Exit(False);

    // brushes cannot be equal if the other brush isn't a gradient brush
    if (not(pOther is TWGradientBrush)) then
        Exit(False);

    pSource := pOther as TWGradientBrush;

    if (m_pStops.Count <> pSource.m_pStops.Count) then
        Exit(False);

    // compare the gradient stops
    if (m_pStops.Count > 0) then
        for i := 0 to m_pStops.Count - 1 do
            if (m_pStops[i].GetHashCode(0) <> pSource.m_pStops[i].GetHashCode(0)) then
                Exit(False);

    Result := (m_Unit = pSource.m_Unit) and (m_WrapMode = pSource.m_WrapMode)
            and m_Matrix.IsEqual(pSource.m_Matrix);
end;
//---------------------------------------------------------------------------
function TWGradientBrush.Differs(const pOther: TWBrush): Boolean;
begin
    Result := not IsEqual(pOther);
end;
//---------------------------------------------------------------------------
function TWGradientBrush.GetMatrix: PWMatrix3x3;
begin
    Result := @m_Matrix;
end;
//---------------------------------------------------------------------------
procedure TWGradientBrush.Clear;
begin
    m_Matrix   := TWMatrix3x3.GetDefault;
    m_Unit     := E_GU_ObjectBoundingBox;
    m_WrapMode := E_WM_Clamp;

    m_pStops.Clear;
end;
//---------------------------------------------------------------------------
procedure TWGradientBrush.Assign(const pOther: TWBrush);
var
    pItem, pStop: TWGradientStop;
    pSource:      TWGradientBrush;
begin
    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // cannot assign the content if the other brush isn't a gradient brush. In this case reset the
    // values
    if (not(pOther is TWGradientBrush)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as TWGradientBrush;

    // copy values
    m_Unit     := pSource.m_Unit;
    m_WrapMode := pSource.m_WrapMode;
    m_Matrix.Assign(pSource.m_Matrix);

    m_pStops.Clear;

    // copy gradient stops
    if (pSource.m_pStops.Count > 0) then
        for pItem in pSource.m_pStops do
        begin
            pStop := nil;

            try
                pStop := TWGradientStop.Create;
                pStop.Assign(pItem);
                m_pStops.Add(pStop);
                pStop := nil;
            finally
                pStop.Free;
            end;
        end;
end;
//---------------------------------------------------------------------------
function TWGradientBrush.GetHashCode(initValue: Integer): Integer;
var
    pStop: TWGradientStop;
begin
    // hash values
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Unit,     SizeOf(EGradientUnit), initValue);
        Result := THashBobJenkins.GetHashValue(m_WrapMode, SizeOf(EWrapMode),     Result);
    {$else}
        Result := BobJenkinsHash(m_Unit,     SizeOf(EGradientUnit), initValue);
        Result := BobJenkinsHash(m_WrapMode, SizeOf(EWrapMode),     Result);
    {$ifend}
    Result := m_Matrix.GetHashCode(Result);

    // hash gradient stops
    for pStop in m_pStops do
        Result := pStop.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
function TWGradientBrush.IsEmpty: Boolean;
begin
    Result := (m_pStops.Count = 0)
end;
//---------------------------------------------------------------------------
procedure TWGradientBrush.SetMatrix(const pMatrix: PWMatrix3x3);
begin
    if (not Assigned(pMatrix)) then
        raise Exception.Create('Matrix is undefined');

    m_Matrix := TWMatrix3x3.Create(pMatrix^);
end;
//---------------------------------------------------------------------------
// TWLinearGradientBrush
//---------------------------------------------------------------------------
constructor TWLinearGradientBrush.Create;
begin
    inherited Create;

    m_pVector := TWLinearGradientVector.Create;
end;
//---------------------------------------------------------------------------
destructor TWLinearGradientBrush.Destroy;
begin
    m_pVector.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWLinearGradientBrush.IsEqual(const pOther: TWBrush): Boolean;
var
    pSource: TWLinearGradientBrush;
begin
    if (not Assigned(pOther)) then
        Exit(False);

    // brushes cannot be equal if the other brush isn't a linear gradient brush
    if (not(pOther is TWLinearGradientBrush)) then
        Exit(False);

    pSource := pOther as TWLinearGradientBrush;

    Result := m_pVector.IsEqual(pSource.m_pVector) and (inherited IsEqual(pSource));
end;
//---------------------------------------------------------------------------
function TWLinearGradientBrush.Differs(const pOther: TWBrush): Boolean;
begin
    Result := not IsEqual(pOther);
end;
//---------------------------------------------------------------------------
procedure TWLinearGradientBrush.Clear;
begin
    inherited Clear;
    m_pVector.Clear;
end;
//---------------------------------------------------------------------------
procedure TWLinearGradientBrush.Assign(const pOther: TWBrush);
var
    pSource: TWLinearGradientBrush;
begin
    inherited Assign(pOther);

    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // cannot assign the content if the other brush isn't a linear gradient brush. In this case
    // reset the values
    if (not(pOther is TWLinearGradientBrush)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as TWLinearGradientBrush;

    // copy values
    m_pVector.Assign(pSource.m_pVector);
end;
//---------------------------------------------------------------------------
function TWLinearGradientBrush.GetHashCode(initValue: Integer): Integer;
begin
    Result := inherited GetHashCode(initValue);
    Result := m_pVector.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
function TWLinearGradientBrush.IsEmpty: Boolean;
begin
    Result := m_pVector.IsEmpty and (inherited IsEmpty);
end;
//---------------------------------------------------------------------------
// TWRadialGradientBrush
//---------------------------------------------------------------------------
constructor TWRadialGradientBrush.Create;
begin
    inherited Create;

    m_Center      := Default(TWPointF);
    m_Focus       := Default(TWPointF);
    m_Radius      := Default(TWSizeF);
    m_ScaleFactor := Default(TWSizeF);
end;
//---------------------------------------------------------------------------
destructor TWRadialGradientBrush.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWRadialGradientBrush.IsEqual(const pOther: TWBrush): Boolean;
var
    pSource: TWRadialGradientBrush;
begin
    if (not Assigned(pOther)) then
        Exit(False);

    // brushes cannot be equal if the other brush isn't a radial gradient brush
    if (not(pOther is TWRadialGradientBrush)) then
        Exit(False);

    pSource := pOther as TWRadialGradientBrush;

    Result := m_Center.IsEqual(pSource.m_Center) and m_Focus.IsEqual(pSource.m_Focus)
            and m_Radius.IsEqual(pSource.m_Radius) and m_ScaleFactor.IsEqual(pSource.m_ScaleFactor)
            and (inherited IsEqual(pSource));
end;
//---------------------------------------------------------------------------
function TWRadialGradientBrush.Differs(const pOther: TWBrush): Boolean;
begin
    Result := not IsEqual(pOther);
end;
//---------------------------------------------------------------------------
procedure TWRadialGradientBrush.Clear;
begin
    inherited Clear;

    m_Center      := Default(TWPointF);
    m_Focus       := Default(TWPointF);
    m_Radius      := Default(TWSizeF);
    m_ScaleFactor := Default(TWSizeF);
end;
//---------------------------------------------------------------------------
procedure TWRadialGradientBrush.Assign(const pOther: TWBrush);
var
    pSource: TWRadialGradientBrush;
begin
    inherited Assign(pOther);

    if (not Assigned(pOther)) then
    begin
        Clear;
        Exit;
    end;

    // cannot assign the content if the other brush isn't a linear gradient brush. In this case
    // reset the values
    if (not(pOther is TWRadialGradientBrush)) then
    begin
        Clear;
        Exit;
    end;

    pSource := pOther as TWRadialGradientBrush;

    m_Center      := pSource.m_Center;
    m_Focus       := pSource.m_Focus;
    m_Radius      := pSource.m_Radius;
    m_ScaleFactor := pSource.m_ScaleFactor;
end;
//---------------------------------------------------------------------------
function TWRadialGradientBrush.GetHashCode(initValue: Integer): Integer;
begin
    Result := inherited GetHashCode(initValue);
    Result := m_Center.GetHashCode(Result);
    Result := m_Focus.GetHashCode(Result);
    Result := m_Radius.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
function TWRadialGradientBrush.IsEmpty: Boolean;
begin
    Result := m_Radius.IsZero and (inherited IsEmpty);
end;
//---------------------------------------------------------------------------
// TWFill
//---------------------------------------------------------------------------
constructor TWFill.Create;
begin
    inherited Create;

    m_Type   := E_BT_Solid;
    m_pBrush := TWSolidBrush.Create;
end;
//---------------------------------------------------------------------------
constructor TWFill.Create(const pOther: TWFill);
begin
    inherited Create;

    Assign(pOther);
end;
//---------------------------------------------------------------------------
destructor TWFill.Destroy;
begin
    ClearMemory;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWFill.ClearMemory;
begin
    FreeAndNil(m_pBrush);
end;
//---------------------------------------------------------------------------
function TWFill.IsEqual(const pOther: TWFill): Boolean;
begin
    if (not Assigned(m_pBrush)) then
        Exit(False);

    if (m_Type <> pOther.m_Type) then
        Exit(False);

    Result := m_pBrush.IsEqual(pOther.m_pBrush);
end;
//---------------------------------------------------------------------------
function TWFill.Differs(const pOther: TWFill): Boolean;
begin
    if (not Assigned(m_pBrush)) then
        Exit(True);

    if (m_Type <> pOther.m_Type) then
        Exit(True);

    Result := m_pBrush.Differs(pOther.m_pBrush);
end;
//---------------------------------------------------------------------------
procedure TWFill.Clear;
begin
    ClearMemory;

    m_Type   := E_BT_Solid;
    m_pBrush := TWSolidBrush.Create;
end;
//---------------------------------------------------------------------------
procedure TWFill.Assign(const pOther: TWFill);
begin
    // nothing to copy, reset the brush
    if (not(Assigned(pOther))) then
    begin
        Clear;
        Exit;
    end;

    // copy the brush
    SetBrush(pOther.m_pBrush);
end;
//---------------------------------------------------------------------------
function TWFill.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Type, SizeOf(EBrushType), initValue);
    {$else}
        Result := BobJenkinsHash(m_Type, SizeOf(EBrushType), initValue);
    {$ifend}
    Result := m_pBrush.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
procedure TWFill.SetBrush(const pBrush: TWBrush);
begin
    ClearMemory;

    // do set a solid brush?
    if (pBrush is TWSolidBrush) then
    begin
        // create a solid brush
        m_Type   := E_BT_Solid;
        m_pBrush := TWSolidBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // do set a linear gradient brush?
    if (pBrush is TWLinearGradientBrush) then
    begin
        // create a linear gradient brush
        m_Type   := E_BT_Linear;
        m_pBrush := TWLinearGradientBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // do set a radial gradient brush?
    if (pBrush is TWRadialGradientBrush) then
    begin
        // create a radial gradient brush
        m_Type   := E_BT_Radial;
        m_pBrush := TWRadialGradientBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // create a default brush
    m_Type   := E_BT_Solid;
    m_pBrush := TWSolidBrush.Create;
end;
//---------------------------------------------------------------------------
function TWFill.IsEmpty: Boolean;
begin
    if (not(Assigned(m_pBrush))) then
        Exit(True);

    Result := m_pBrush.IsEmpty;
end;
//---------------------------------------------------------------------------
// TWStroke
//---------------------------------------------------------------------------
constructor TWStroke.Create;
begin
    inherited Create;

    m_LineCap      := E_LC_Default;
    m_DashCap      := E_DC_Default;
    m_LineJoin     := E_LJ_Default;
    m_pDashPattern := TList<Double>.Create();
    m_DashOffset   := 0.0;
    m_Width        := 0.0;
    m_Matrix       := TWMatrix3x3.GetDefault;
    m_Type         := E_BT_Solid;
    m_pBrush       := TWSolidBrush.Create;
end;
//---------------------------------------------------------------------------
constructor TWStroke.Create(const pOther: TWStroke);
begin
    inherited Create;

    m_pDashPattern := TList<Double>.Create();

    Assign(pOther);
end;
//---------------------------------------------------------------------------
destructor TWStroke.Destroy;
begin
    ClearMemory;

    m_pDashPattern.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWStroke.ClearMemory;
begin
    FreeAndNil(m_pBrush);
end;
//---------------------------------------------------------------------------
function TWStroke.GetMatrix: PWMatrix3x3;
begin
    Result := @m_Matrix;
end;
//---------------------------------------------------------------------------
function TWStroke.GetDashPatternCount: NativeUInt;
begin
    Result := m_pDashPattern.Count;
end;
//---------------------------------------------------------------------------
function TWStroke.IsEqual(const pOther: TWStroke): Boolean;
var
    dpCount, otherDpCount, i: NativeUInt;
begin
    if (m_Width <> pOther.m_Width) then
        Exit(False);

    if (m_LineCap <> pOther.m_LineCap) then
        Exit(False);

    if (m_DashCap <> pOther.m_DashCap) then
        Exit(False);

    if (m_LineJoin <> pOther.m_LineJoin) then
        Exit(False);

    if (m_DashOffset <> pOther.m_DashOffset) then
        Exit(False);

    // get dash pattern values count
    dpCount      := DashPatternCount;
    otherDpCount := pOther.DashPatternCount;

    //dash pattern values count are identical?
    if (dpCount <> otherDpCount) then
        Exit(False);

    // compare each dash pattern values
    if (dpCount > 0) then
        for i := 0 to dpCount - 1 do
            if (m_pDashPattern[i] <> pOther.m_pDashPattern[i]) then
                Exit(False);

    // matrices are identical?
    if (m_Matrix <> pOther.m_Matrix) then
        Exit(False);

    if (m_Type <> pOther.m_Type) then
        Exit(False);

    if (not Assigned(m_pBrush)) then
        Exit(False);

    Result := m_pBrush.IsEqual(pOther.m_pBrush);
end;
//---------------------------------------------------------------------------
function TWStroke.Differs(const pOther: TWStroke): Boolean;
begin
    Result := not IsEqual(pOther);
end;
//---------------------------------------------------------------------------
procedure TWStroke.Clear;
begin
    ClearMemory;

    m_LineCap    := E_LC_Default;
    m_DashCap    := E_DC_Default;
    m_LineJoin   := E_LJ_Default;
    m_DashOffset := 0.0;
    m_Width      := 0.0;
    m_Matrix     := TWMatrix3x3.GetDefault;
    m_Type       := E_BT_Solid;
    m_pBrush     := TWSolidBrush.Create;
    m_pDashPattern.Clear;
end;
//---------------------------------------------------------------------------
procedure TWStroke.Assign(const pOther: TWStroke);
var
    item: Double;
begin
    // nothing to copy, reset the brush
    if (not(Assigned(pOther))) then
    begin
        Clear;
        Exit;
    end;

    // copy the basic values
    m_LineCap    := pOther.m_LineCap;
    m_DashCap    := pOther.m_DashCap;
    m_LineJoin   := pOther.m_LineJoin;
    m_DashOffset := pOther.m_DashOffset;
    m_Width      := pOther.m_Width;
    m_Matrix     := pOther.m_Matrix;

    m_pDashPattern.Clear;

    // copy the dash pattern content
    for item in pOther.m_pDashPattern do
        m_pDashPattern.Add(item);

    // copy the brush
    SetBrush(pOther.m_pBrush);
end;
//---------------------------------------------------------------------------
function TWStroke.GetHashCode(initValue: Integer): Integer;
var
    i, count: NativeUInt;
    value:    Double;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_LineCap,    SizeOf(ELineCap),   initValue);
        Result := THashBobJenkins.GetHashValue(m_DashCap,    SizeOf(EDashCap),   Result);
        Result := THashBobJenkins.GetHashValue(m_LineJoin,   SizeOf(ELineJoin),  Result);
        Result := THashBobJenkins.GetHashValue(m_DashOffset, SizeOf(Double),     Result);
        Result := THashBobJenkins.GetHashValue(m_Width,      SizeOf(Double),     Result);
        Result := THashBobJenkins.GetHashValue(m_Type,       SizeOf(EBrushType), Result);
    {$else}
        Result := BobJenkinsHash(m_LineCap,    SizeOf(ELineCap),   initValue);
        Result := BobJenkinsHash(m_DashCap,    SizeOf(EDashCap),   Result);
        Result := BobJenkinsHash(m_LineJoin,   SizeOf(ELineJoin),  Result);
        Result := BobJenkinsHash(m_DashOffset, SizeOf(Double),     Result);
        Result := BobJenkinsHash(m_Width,      SizeOf(Double),     Result);
        Result := BobJenkinsHash(m_Type,       SizeOf(EBrushType), Result);
    {$ifend}
    Result := m_Matrix.GetHashCode(Result);
    Result := m_pBrush.GetHashCode(Result);

    count := m_pDashPattern.Count;

    if (count > 0) then
        for i := 0 to count - 1 do
        begin
            value := m_pDashPattern[i];
            {$if CompilerVersion >= 29}
                Result := THashBobJenkins.GetHashValue(value, SizeOf(Double), Result);
            {$else}
                Result := BobJenkinsHash(value, SizeOf(Double), Result);
            {$ifend}
        end;
end;
//---------------------------------------------------------------------------
procedure TWStroke.SetMatrix(const pMatrix: PWMatrix3x3);
begin
    if (not Assigned(pMatrix)) then
        raise Exception.Create('Matrix is undefined');

    m_Matrix := TWMatrix3x3.Create(pMatrix^);
end;
//---------------------------------------------------------------------------
procedure TWStroke.SetBrush(const pBrush: TWBrush);
begin
    ClearMemory;

    // do set a solid brush?
    if (pBrush is TWSolidBrush) then
    begin
        // create a solid brush
        m_Type   := E_BT_Solid;
        m_pBrush := TWSolidBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // do set a linear gradient brush?
    if (pBrush is TWLinearGradientBrush) then
    begin
        // create a linear gradient brush
        m_Type   := E_BT_Linear;
        m_pBrush := TWLinearGradientBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // do set a radial gradient brush?
    if (pBrush is TWRadialGradientBrush) then
    begin
        // create a radial gradient brush
        m_Type   := E_BT_Radial;
        m_pBrush := TWRadialGradientBrush.Create;

        // copy the brush content
        m_pBrush.Assign(pBrush);
        Exit;
    end;

    // create a default brush
    m_Type   := E_BT_Solid;
    m_pBrush := TWSolidBrush.Create;
end;
//---------------------------------------------------------------------------
function TWStroke.IsEmpty: Boolean;
begin
    if (m_Width = 0.0) then
        Exit(True);

    if (not Assigned(m_pBrush)) then
        Exit(True);

    Result := m_pBrush.IsEmpty;
end;
//---------------------------------------------------------------------------

end.
