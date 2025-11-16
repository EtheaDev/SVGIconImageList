{**
 @abstract(@name provides generic rectangle structure.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWRect;
{$I SVGMagic.inc}

interface

uses System.Rtti,
     System.Types,
     System.TypInfo,
     System.SysUtils,
     System.Math,
     System.Generics.Defaults,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     {$ifdef USE_VCL}
        Vcl.Graphics,
        Winapi.Windows,
        Winapi.GDIPAPI,
     {$endif}
     UTWGenericNumber,
     UTWPoint,
     UTWSize;

type
    {**
     Platform independent rectangle structure
    }
    TWRect<T> = record
        private
            m_Left:   TWGenericNumber<T>;
            m_Top:    TWGenericNumber<T>;
            m_Right:  TWGenericNumber<T>;
            m_Bottom: TWGenericNumber<T>;

            {**
             Get left value
             @returns(Left value)
            }
            function GetLeft: T;

            {**
             Set left value
             @param(value Left value)
            }
            procedure SetLeft(value: T);

            {**
             Get top value
             @returns(Top value)
            }
            function GetTop: T;

            {**
             Set top value
             @param(value Top value)
            }
            procedure SetTop(value: T);

            {**
             Get right value
             @returns(Right value)
            }
            function GetRight: T;

            {**
             Set right value
             @param(value Right value)
            }
            procedure SetRight(value: T);

            {**
             Get bottom value
             @returns(Bottom value)
            }
            function GetBottom: T;

            {**
             Set bottom value
             @param(value Bottom value)
            }
            procedure SetBottom(value: T);

        public
            {**
             Constructor
             @param(left Left edge)
             @param(top Top edge)
             @param(right Right edge)
             @param(bottom Bottom edge)
            }
            constructor Create(const left, top, right, bottom: T); overload;

            {**
             Constructor
             @param(min Left top corner position)
             @param(max Right bottom corner position)
            }
            constructor Create(const min, max: TWPoint<T>); overload;

            {**
             Constructor
             @param(x X position)
             @param(y Y position)
             @param(size Size)
            }
            constructor Create(const x, y: T; const size: TWSize<T>); overload;

            {**
             Constructor
             @param(pos Position)
             @param size Size)
            }
            constructor Create(const pos: TWPoint<T>; const size: TWSize<T>); overload;

            {**
             Constructor
             @param(rect VCL rect)
            }
            {$ifdef USE_VCL}
                constructor Create(const rect: TRect); overload;
            {$endif}

            {**
             Constructor
             @param(rect VCL floating rect)
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const rect: TRectF; doRound: Boolean); overload;
            {$endif}

            {**
             Constructor
             @param(rect GDI+ rect)
            }
            {$ifdef USE_VCL}
                constructor Create(const rect: TGpRect); overload;
            {$endif}

            {**
             Constructor
             @param(rect GDI+ floating rect)
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const rect: TGpRectF; doRound: Boolean); overload;
            {$endif}

            {**
             Addition operator
             @param(a First rect to add)
             @param(b Second rect to add)
             @returns(Rect containing the result)
            }
            class operator Add(a, b: TWRect<T>): TWRect<T>; overload; inline;

            {**
             Addition operator
             @param(a rect to add value to)
             @param(b Value to add to)
             @returns(Rect containing the result)
            }
            class operator Add(a: TWRect<T>; b: T): TWRect<T>; overload; inline;

            {**
             Subtraction operator
             @param(a First rect to subtract)
             @param(b Second rect to subtract)
             @returns(Rect containing the result)
            }
            class operator Subtract(a, b: TWRect<T>): TWRect<T>; overload; inline;

            {**
             Subtraction operator
             @param(a rect to subtract value from)
             @param(b Value to subtract to)
             @returns(Rect containing the result)
            }
            class operator Subtract(a: TWRect<T>; b: T): TWRect<T>; overload; inline;

            {**
             Multiplication operator
             @param(a First rect to multiply)
             @param(b Second rect to multiply)
             @returns(Rect containing the result)
            }
            class operator Multiply(a, b: TWRect<T>): TWRect<T>; overload; inline;

            {**
             Multiplication operator
             @param(a rect to multiply value with)
             @param(b Value to multiply with)
             @returns(Rect containing the result)
            }
            class operator Multiply(a: TWRect<T>; b: T): TWRect<T>; overload; inline;

            {**
             Division operator
             @param(a First rect to divide)
             @param(b Second rect to divide)
             @returns(Rect containing the result)
            }
            class operator Divide(a, b: TWRect<T>): TWRect<T>; overload; inline;

            {**
             Division operator
             @param(a rect to divide value by)
             @param(b Value to divide by)
             @returns(Rect containing the result)
            }
            class operator Divide(a: TWRect<T>; b: T): TWRect<T>; overload; inline;

            {**
             Integer division operator
             @param(a First rect to divide)
             @param(b Second rect to divide)
             @returns(Rect containing the result)
            }
            class operator IntDivide(a, b: TWRect<T>): TWRect<T>; overload; inline;

            {**
             Integer division operator
             @param(a rect to divide value by)
             @param(b Value to divide by)
             @returns(Rect containing the result)
            }
            class operator IntDivide(a: TWRect<T>; b: T): TWRect<T>; overload; inline;

            {**
             Inversion operator
             @param(a rect to invert)
             @returns(Rect containing the result)
            }
            class operator Negative(a: TWRect<T>): TWRect<T>; inline;

            {**
             Equality operator
             @param(a First rect to compare)
             @param(b Second rect to compare)
             @returns(@true if rects are equal, otherwise @false)
            }
            class operator Equal(a, b: TWRect<T>): Boolean; inline;

            {**
             Not equality operator
             @param(a First rect to compare)
             @param(b Second rect to compare)
             @returns(@true if rects are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWRect<T>): Boolean; inline;

            {**
             Check if rect content is equal to another rect
             @param(other Other rect to compare with)
             @returns(@true if rects are equal, otherwise @false)
            }
            function IsEqual(const other: TWRect<T>): Boolean; inline;

            {**
             Check if rect content differs from another rect
             @param(other Other rect to compare with)
             @returns(@true if rects differ, otherwise @false)
            }
            function Differs(const other: TWRect<T>): Boolean; inline;

            {**
             Invert the rect (e.g. a rect with coordinates [-3, 4, 5, -6] will become [3, -4, -5, 6])
             @returns(Inverted size)
            }
            function Invert: TWRect<T>; inline;

            {**
             Get the top and left point
             @returns(Top and left point)
            }
            function TopLeft: TWPoint<T>; inline;

            {**
             Get the bottom and right point
             @returns(Bottom and right point)
            }
            function BottomRight: TWPoint<T>; inline;

            {**
             Get rect width
             @returns(Rect width)
            }
            function Width: T; inline;

            {**
             Get rect height
             @returns(Rect height)
            }
            function Height: T; inline;

            {**
             Get diagonal length
             @returns(Diagonal length)
            }
            function Diagonal: T; inline;

            {**
             Make sure top > left and bottom > top
            }
            procedure Normalize; inline;

            {**
             Get center point
             @returns(Center point)
            }
            function GetCenter: TWPoint<T>; inline;

            {**
             Offset rect
             @param(x Value to move on x axis (value can be negative))
             @param(y Value to move on y axis (value can be negative))
            }
            procedure Offset(const x, y: T); inline;

            {**
             Inflate rect
             @param(x Value to inflate on x axis (deflate if value is negative))
             @param(y Value to inflate on y axis (deflate if value is negative))
            }
            procedure Inflate(const x, y: T); overload; inline;

            {**
             Inflate rect
             @param(left Value to inflate on the left (deflate if value is negative))
             @param(top Value to inflate on the top (deflate if value is negative))
             @param(right Value to inflate on the right (deflate if value is negative))
             @param(bottom Value to inflate on the bottom (deflate if value is negative))
            }
            procedure Inflate(const left, top, right, bottom: T); overload; inline;

            {**
             Check if rect is empty
             @returns(@true if rect is empty, otherwise @false)
            }
            function IsEmpty: Boolean; inline;

            {**
             Check if a point is inside the rect
             @param(point Point to check)
             @param(includeRightBottom If @true, a point located on the right or bottom edges will
                                       be considered as inside the rect (the VCL and the Win API
                                       exclude it))
             @returns(@true if the point is inside the rect, otherwise @false)
            }
            function PtInRect(const point: TWPoint<T>; includeRightBottom: Boolean): Boolean; inline;

            {**
             Check if a rect is inside this rect
             @param(rect Rect to check)
             @param(includeRightBottom If @true, the points located on the right or bottom edges
                                       will be considered as inside the rect (the VCL and the Win
                                       API exclude them))
             @returns(@true if the rect is inside this rect, otherwise @false)
            }
            function Inside(const rect: TWRect<T>; includeRightBottom: Boolean): Boolean; inline;

            {**
             Check if a rect intersects this rect
             @param(rect Rect to check)
             @param(includeRightBottom If @true, the points located on the right or bottom edges
                                       will be considered as inside the rect (the VCL and the Win
                                       API exclude them))
             @returns(@true if the rect is intersects this rect, otherwise @false)
            }
            function Intersect(const rect: TWRect<T>; includeRightBottom: Boolean): Boolean; inline;

            {**
             Convert rect to VCL TRect
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Rect as VCL TRect)
            }
            {$ifdef USE_VCL}
                function ToTRect(doRound: Boolean): TRect; inline;
            {$endif}

            {**
             Convert rect to VCL TRectF
             @returns(Rect as VCL TRectF)
            }
            {$ifdef USE_VCL}
                function ToTRectF: TRectF; inline;
            {$endif}

            {**
             Convert rect to GDI+ rect
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Rect as GDI+ rect)
            }
            {$ifdef USE_VCL}
                function ToGpRect(doRound: Boolean): TGpRect; inline;
            {$endif}

            {**
             Convert rect to GDI+ RectF
             @returns(Rect as GDI+ RectF)
            }
            {$ifdef USE_VCL}
                function ToGpRectF: TGpRectF; inline;
            {$endif}

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; inline;

        public
            {**
             Get or set the rect left value
            }
            property Left: T read GetLeft write SetLeft;

            {**
             Get or set the rect top value
            }
            property Top: T read GetTop write SetTop;

            {**
             Get or set the rect right value
            }
            property Right: T read GetRight write SetRight;

            {**
             Get or set the rect bottom value
            }
            property Bottom: T read GetBottom write SetBottom;
    end;

    TWRectI = TWRect<Integer>;
    TWRectF = TWRect<Single>;

    PWRectI = ^TWRectI;
    PWRectF = ^TWRectF;

implementation
//---------------------------------------------------------------------------
// TWRect
//---------------------------------------------------------------------------
constructor TWRect<T>.Create(const left, top, right, bottom: T);
begin
    m_Left.Value   := left;
    m_Top.Value    := top;
    m_Right.Value  := right;
    m_Bottom.Value := bottom;
end;
//---------------------------------------------------------------------------
constructor TWRect<T>.Create(const min, max: TWPoint<T>);
begin
    m_Left.Value   := min.X;
    m_Top.Value    := min.Y;
    m_Right.Value  := max.X;
    m_Bottom.Value := max.Y;
end;
//---------------------------------------------------------------------------
constructor TWRect<T>.Create(const x, y: T; const size: TWSize<T>);
var
    w, h: TWGenericNumber<T>;
begin
    w.Value := size.Width;
    h.Value := size.Height;

    m_Left.Value := x;
    m_Top.Value  := y;
    m_Right      := m_Left + w;
    m_Bottom     := m_Top  + h;
end;
//---------------------------------------------------------------------------
constructor TWRect<T>.Create(const pos: TWPoint<T>; const size: TWSize<T>);
var
    x, y, w, h: TWGenericNumber<T>;
begin
    x.Value := pos.X;
    y.Value := pos.Y;
    w.Value := size.Width;
    h.Value := size.Height;

    m_Left.Value := pos.X;
    m_Top.Value  := pos.Y;
    m_Right      := x + w;
    m_Bottom     := y + h;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWRect<T>.Create(const rect: TRect);
    begin
        m_Left   := rect.Left;
        m_Top    := rect.Top;
        m_Right  := rect.Right;
        m_Bottom := rect.Bottom;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWRect<T>.Create(const rect: TRectF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            left, top, right, bottom: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                left   := Round(rect.Left);
                top    := Round(rect.Top);
                right  := Round(rect.Right);
                bottom := Round(rect.Bottom);

                m_Left.Value   := left.AsType<T>;
                m_Top.Value    := top.AsType<T>;
                m_Right.Value  := right.AsType<T>;
                m_Bottom.Value := bottom.AsType<T>;
            {$else}
                m_Left   := Round(rect.Left);
                m_Top    := Round(rect.Top);
                m_Right  := Round(rect.Right);
                m_Bottom := Round(rect.Bottom);
            {$endif}
            Exit;
        end;

        m_Left   := rect.Left;
        m_Top    := rect.Top;
        m_Right  := rect.Right;
        m_Bottom := rect.Bottom;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWRect<T>.Create(const rect: TGpRect);
    var
        w, h: TWGenericNumber<T>;
    begin
        w := rect.Width;
        h := rect.Height;

        m_Left   := rect.X;
        m_Top    := rect.Y;
        m_Right  := m_Left + w;
        m_Bottom := m_Top  + h;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWRect<T>.Create(const rect: TGpRectF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            left, top, right, bottom: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                left   := Round(rect.X);
                top    := Round(rect.y);
                right  := Round(rect.X + rect.Width);
                bottom := Round(rect.y + rect.Height);

                m_Left.Value   := left.AsType<T>;
                m_Top.Value    := top.AsType<T>;
                m_Right.Value  := right.AsType<T>;
                m_Bottom.Value := bottom.AsType<T>;
            {$else}
                m_Left   := Round(rect.X);
                m_Top    := Round(rect.y);
                m_Right  := Round(rect.X + rect.Width);
                m_Bottom := Round(rect.y + rect.Height);
            {$endif}
            Exit;
        end;

        m_Left   := rect.X;
        m_Top    := rect.y;
        m_Right  := rect.X + rect.Width;
        m_Bottom := rect.y + rect.Height;
    end;
{$endif}
//---------------------------------------------------------------------------
class operator TWRect<T>.Add(a, b: TWRect<T>): TWRect<T>;
begin
    Result.Left   := (a.m_Left   + b.m_Left).Value;
    Result.Top    := (a.m_Top    + b.m_Top).Value;
    Result.Right  := (a.m_Right  + b.m_Right).Value;
    Result.Bottom := (a.m_Bottom + b.m_Bottom).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Add(a: TWRect<T>; b: T): TWRect<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Left   := (a.m_Left   + value).Value;
    Result.Top    := (a.m_Top    + value).Value;
    Result.Right  := (a.m_Right  + value).Value;
    Result.Bottom := (a.m_Bottom + value).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Subtract(a, b: TWRect<T>): TWRect<T>;
begin
    Result.Left   := (a.m_Left   - b.m_Left).Value;
    Result.Top    := (a.m_Top    - b.m_Top).Value;
    Result.Right  := (a.m_Right  - b.m_Right).Value;
    Result.Bottom := (a.m_Bottom - b.m_Bottom).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Subtract(a: TWRect<T>; b: T): TWRect<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Left   := (a.m_Left   - value).Value;
    Result.Top    := (a.m_Top    - value).Value;
    Result.Right  := (a.m_Right  - value).Value;
    Result.Bottom := (a.m_Bottom - value).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Multiply(a, b: TWRect<T>): TWRect<T>;
begin
    Result.Left   := (a.m_Left   * b.m_Left).Value;
    Result.Top    := (a.m_Top    * b.m_Top).Value;
    Result.Right  := (a.m_Right  * b.m_Right).Value;
    Result.Bottom := (a.m_Bottom * b.m_Bottom).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Multiply(a: TWRect<T>; b: T): TWRect<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Left   := (a.m_Left   * value).Value;
    Result.Top    := (a.m_Top    * value).Value;
    Result.Right  := (a.m_Right  * value).Value;
    Result.Bottom := (a.m_Bottom * value).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Divide(a, b: TWRect<T>): TWRect<T>;
begin
    Result.Left   := (a.m_Left   / b.m_Left).Value;
    Result.Top    := (a.m_Top    / b.m_Top).Value;
    Result.Right  := (a.m_Right  / b.m_Right).Value;
    Result.Bottom := (a.m_Bottom / b.m_Bottom).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Divide(a: TWRect<T>; b: T): TWRect<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Left   := (a.m_Left   / value).Value;
    Result.Top    := (a.m_Top    / value).Value;
    Result.Right  := (a.m_Right  / value).Value;
    Result.Bottom := (a.m_Bottom / value).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.IntDivide(a, b: TWRect<T>): TWRect<T>;
begin
    Result.Left   := (a.m_Left   div b.m_Left).Value;
    Result.Top    := (a.m_Top    div b.m_Top).Value;
    Result.Right  := (a.m_Right  div b.m_Right).Value;
    Result.Bottom := (a.m_Bottom div b.m_Bottom).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.IntDivide(a: TWRect<T>; b: T): TWRect<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Left   := (a.m_Left   div value).Value;
    Result.Top    := (a.m_Top    div value).Value;
    Result.Right  := (a.m_Right  div value).Value;
    Result.Bottom := (a.m_Bottom div value).Value;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Negative(a: TWRect<T>): TWRect<T>;
begin
    Result := a.Invert;
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.Equal(a, b: TWRect<T>): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWRect<T>.NotEqual(a, b: TWRect<T>): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
function TWRect<T>.GetLeft: T;
begin
    Result := m_Left.Value;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.SetLeft(value: T);
begin
    m_Left.Value := value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.GetTop: T;
begin
    Result := m_Top.Value;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.SetTop(value: T);
begin
    m_Top.Value := value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.GetRight: T;
begin
    Result := m_Right.Value;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.SetRight(value: T);
begin
    m_Right.Value := value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.GetBottom: T;
begin
    Result := m_Bottom.Value;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.SetBottom(value: T);
begin
    m_Bottom.Value := value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.Width: T;
begin
    Result := (m_Right - m_Left).Value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.Height: T;
begin
    Result := (m_Bottom - m_Top).Value;
end;
//---------------------------------------------------------------------------
function TWRect<T>.Diagonal: T;
var
    w, h, res: TValue;
begin
    w := TValue.From<T>(Width);
    h := TValue.From<T>(Height);

    // normally this should always be true, as w and h are of the same type
    Assert(w.Kind = h.Kind);

    // search for w and h types, and process the diagonal calculation
    case (w.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((w.TypeInfo.Name = 'Cardinal') or (w.TypeInfo.Name = 'NativeUInt')) then
                    res := Sqrt(Power(w.AsUInt64, 2) + Power(h.AsUInt64, 2))
                else
            {$ifend}
                res := Sqrt(Power(w.AsInt64, 2) + Power(h.AsInt64, 2));

        tkFloat:
            res := Sqrt(Power(w.AsExtended, 2.0) + Power(h.AsExtended, 2.0));
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(h.Kind)]);
    end;

    // convert to generic type again
    Result := res.AsType<T>;
end;
//---------------------------------------------------------------------------
function TWRect<T>.IsEqual(const other: TWRect<T>): Boolean;
begin
    Result := ((m_Left = other.m_Left) and (m_Top = other.m_Top) and (m_Right = other.m_Right)
            and (m_Bottom = other.m_Bottom));
end;
//---------------------------------------------------------------------------
function TWRect<T>.Differs(const other: TWRect<T>): Boolean;
begin
    Result := ((m_Left <> other.m_Left) or (m_Top <> other.m_Top) or (m_Right <> other.m_Right)
            or (m_Bottom <> other.m_Bottom));
end;
//---------------------------------------------------------------------------
function TWRect<T>.Invert: TWRect<T>;
begin
    {$if CompilerVersion <= 24}
        Result.m_Left   := TWGenericNumber<T>(0.0) - m_Left;
        Result.m_Top    := TWGenericNumber<T>(0.0) - m_Top;
        Result.m_Right  := TWGenericNumber<T>(0.0) - m_Right;
        Result.m_Bottom := TWGenericNumber<T>(0.0) - m_Bottom;
    {$else}
        Result.m_Left   := -m_Left;
        Result.m_Top    := -m_Top;
        Result.m_Right  := -m_Right;
        Result.m_Bottom := -m_Bottom;
    {$ifend}
end;
//---------------------------------------------------------------------------
function TWRect<T>.TopLeft: TWPoint<T>;
begin
    Result := TWPoint<T>.Create(m_Left.Value, m_Top.Value);
end;
//---------------------------------------------------------------------------
function TWRect<T>.BottomRight: TWPoint<T>;
begin
    Result := TWPoint<T>.Create(m_Right.Value, m_Bottom.Value);
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.Normalize;
var
    temp: TWGenericNumber<T>;
begin
    if (m_Left > m_Right) then
    begin
        // swap the left and right values
        temp    := m_Left;
        m_Left  := m_Right;
        m_Right := temp;
    end;

    if (m_Top > m_Bottom) then
    begin
        // swap the top and bottom values
        temp     := m_Top;
        m_Top    := m_Bottom;
        m_Bottom := temp;
    end;
end;
//---------------------------------------------------------------------------
function TWRect<T>.GetCenter: TWPoint<T>;
begin
    {$if CompilerVersion <= 24}
        Result := TWPoint<T>.Create(((m_Left + m_Right) / TWGenericNumber<T>(2.0)).Value,
                ((m_Top + m_Bottom) / TWGenericNumber<T>(2.0)).Value);
    {$else}
        Result := TWPoint<T>.Create(((m_Left + m_Right) / 2.0).Value, ((m_Top + m_Bottom) / 2.0).Value);
    {$ifend}
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.Offset(const x, y: T);
var
    deltaX, deltaY: TWGenericNumber<T>;
begin
    deltaX.Value := x;
    deltaY.Value := y;

    m_Left   := m_Left   + deltaX;
    m_Top    := m_Top    + deltaY;
    m_Right  := m_Right  + deltaX;
    m_Bottom := m_Bottom + deltaY;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.Inflate(const x, y: T);
var
    deltaX, deltaY: TWGenericNumber<T>;
begin
    deltaX.Value := x;
    deltaY.Value := y;

    m_Left   := m_Left   - deltaX;
    m_Top    := m_Top    - deltaY;
    m_Right  := m_Right  + deltaX;
    m_Bottom := m_Bottom + deltaY;
end;
//---------------------------------------------------------------------------
procedure TWRect<T>.Inflate(const left, top, right, bottom: T);
var
    deltaLeft, deltaTop, deltaRight, deltaBottom: TWGenericNumber<T>;
begin
    deltaLeft.Value   := left;
    deltaTop.Value    := top;
    deltaRight.Value  := right;
    deltaBottom.Value := bottom;

    m_Left   := m_Left   - deltaLeft;
    m_Top    := m_Top    - deltaTop;
    m_Right  := m_Right  + deltaRight;
    m_Bottom := m_Bottom + deltaBottom;
end;
//---------------------------------------------------------------------------
function TWRect<T>.IsEmpty: Boolean;
begin
    Result := ((m_Right = m_Left) or (m_Bottom = m_Top));
end;
//---------------------------------------------------------------------------
function TWRect<T>.PtInRect(const point: TWPoint<T>; includeRightBottom: Boolean): Boolean;
var
    px, py: TWGenericNumber<T>;
begin
    px.Value := point.X;
    py.Value := point.Y;

    if (includeRightBottom) then
        Result := ((px >= m_Left) and (py >= m_Top) and (px <= m_Right) and (py <= m_Bottom))
    else
        Result := ((px >= m_Left) and (py >= m_Top) and (px < m_Right) and (py < m_Bottom));
end;
//---------------------------------------------------------------------------
function TWRect<T>.Inside(const rect: TWRect<T>; includeRightBottom: Boolean): Boolean;
begin
    Result := (PtInRect(rect.TopLeft, includeRightBottom) and PtInRect(rect.BottomRight,
            includeRightBottom));
end;
//---------------------------------------------------------------------------
function TWRect<T>.Intersect(const rect: TWRect<T>; includeRightBottom: Boolean): Boolean;
begin
    if (includeRightBottom) then
        Result := not((m_Left > rect.m_Right) or (m_Right < rect.m_Left) or (m_Top > rect.m_Bottom)
                or (m_Bottom < rect.m_Top))
    else
        Result := not((rect.m_Right < m_Left) or (m_Right < rect.m_Left) or (rect.m_Bottom < m_Top)
                or (m_Bottom < rect.m_Top));
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWRect<T>.ToTRect(doRound: Boolean): TRect;
    var
        left, top, right, bottom: TValue;
    begin
        left   := TValue.From<T>(m_Left.Value);
        top    := TValue.From<T>(m_Top.Value);
        right  := TValue.From<T>(m_Right.Value);
        bottom := TValue.From<T>(m_Bottom.Value);

        if (doRound) then
        begin
            Result.Left   := Round(left.AsType<Single>);
            Result.Top    := Round(top.AsType<Single>);
            Result.Right  := Round(right.AsType<Single>);
            Result.Bottom := Round(bottom.AsType<Single>);
            Exit;
        end;

        Result.Left   := left.AsType<Int64>;
        Result.Top    := top.AsType<Int64>;
        Result.Right  := right.AsType<Int64>;
        Result.Bottom := bottom.AsType<Int64>;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWRect<T>.ToTRectF: TRectF;
    var
        left, top, right, bottom: TValue;
    begin
        left   := TValue.From<T>(m_Left.Value);
        top    := TValue.From<T>(m_Top.Value);
        right  := TValue.From<T>(m_Right.Value);
        bottom := TValue.From<T>(m_Bottom.Value);

        Result.Left   := left.AsType<Single>;
        Result.Top    := top.AsType<Single>;
        Result.Right  := right.AsType<Single>;
        Result.Bottom := bottom.AsType<Single>;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWRect<T>.ToGpRect(doRound: Boolean): TGpRect;
    var
        x, y, w, h: TValue;
    begin
        x := TValue.From<T>(m_Left.Value);
        y := TValue.From<T>(m_Top.Value);
        w := TValue.From<T>(Width);
        h := TValue.From<T>(Height);

        if (doRound) then
        begin
            Result.X      := Round(x.AsType<Single>);
            Result.Y      := Round(y.AsType<Single>);
            Result.Width  := Round(w.AsType<Single>);
            Result.Height := Round(h.AsType<Single>);
            Exit;
        end;

        Result.X      := x.AsType<Int64>;
        Result.Y      := y.AsType<Int64>;
        Result.Width  := w.AsType<Int64>;
        Result.Height := h.AsType<Int64>;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWRect<T>.ToGpRectF: TGpRectF;
    var
        x, y, w, h: TValue;
    begin
        x := TValue.From<T>(m_Left.Value);
        y := TValue.From<T>(m_Top.Value);
        w := TValue.From<T>(Width);
        h := TValue.From<T>(Height);

        Result.X      := x.AsType<Single>;
        Result.Y      := y.AsType<Single>;
        Result.Width  := w.AsType<Single>;
        Result.Height := h.AsType<Single>;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWRect<T>.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Left,   SizeOf(T), initValue);
        Result := THashBobJenkins.GetHashValue(m_Top,    SizeOf(T), Result);
        Result := THashBobJenkins.GetHashValue(m_Right,  SizeOf(T), Result);
        Result := THashBobJenkins.GetHashValue(m_Bottom, SizeOf(T), Result);
    {$else}
        Result := BobJenkinsHash(m_Left,   SizeOf(T), initValue);
        Result := BobJenkinsHash(m_Top,    SizeOf(T), Result);
        Result := BobJenkinsHash(m_Right,  SizeOf(T), Result);
        Result := BobJenkinsHash(m_Bottom, SizeOf(T), Result);
    {$ifend}
end;
//---------------------------------------------------------------------------

end.
