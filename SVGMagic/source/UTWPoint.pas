{**
 @abstract(@name provides a generic point structure.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWPoint;
{$I SVGMagic.inc}

interface

uses System.Rtti,
     System.Types,
     System.Generics.Defaults,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     {$ifdef USE_VCL}
        Winapi.Windows,
        Winapi.GDIPAPI,
     {$endif}
     UTWGenericNumber;

type
    {**
     Platform independent point structure
    }
    TWPoint<T> = record
        private
            m_X: TWGenericNumber<T>;
            m_Y: TWGenericNumber<T>;

            {**
             Get x value
             @returns(X value)
            }
            function GetX: T; inline;

            {**
             Set x value
             @param(value X value)
            }
            procedure SetX(value: T); inline;

            {**
             Get y value
             @returns(Y value)
            }
            function GetY: T; inline;

            {**
             Set y value
             @param(value Y value)
            }
            procedure SetY(value: T); inline;

        public
            {**
             Constructor
             @param(x x coordinate)
             @param(y y coordinate)
            }
            constructor Create(const x, y: T); overload;

            {**
             Constructor
             @param(point VCL point)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TPoint); overload;
            {$endif}

            {**
             Constructor
             @param(point VCL floating point)
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TPointF; doRound: Boolean); overload;
            {$endif}

            {**
             Constructor
             @param(point GDI+ point)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TGpPoint); overload;
            {$endif}

            {**
             Constructor
             @param(point GDI+ floating point)
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TGpPointF; doRound: Boolean); overload;
            {$endif}

            {**
             Addition operator
             @param(a First point to add)
             @param(b Second point to add)
             @returns(Point containing the result)
            }
            class operator Add(a, b: TWPoint<T>): TWPoint<T>; overload; inline;

            {**
             Addition operator
             @param(a Point to add value to)
             @param(b Value to add to)
             @returns(Point containing the result)
            }
            class operator Add(a: TWPoint<T>; b: T): TWPoint<T>; overload; inline;

            {**
             Subtraction operator
             @param(a First point to subtract)
             @param(b Second point to subtract)
             @returns(Point containing the result)
            }
            class operator Subtract(a, b: TWPoint<T>): TWPoint<T>; overload; inline;

            {**
             Subtraction operator
             @param(a Point to subtract value from)
             @param(b Value to subtract to)
             @returns(Point containing the result)
            }
            class operator Subtract(a: TWPoint<T>; b: T): TWPoint<T>; overload; inline;

            {**
             Multiplication operator
             @param(a First point to multiply)
             @param(b Second point to multiply)
             @returns(Point containing the result)
            }
            class operator Multiply(a, b: TWPoint<T>): TWPoint<T>; overload; inline;

            {**
             Multiplication operator
             @param(a Point to multiply value with)
             @param(b Value to multiply with)
             @returns(Point containing the result)
            }
            class operator Multiply(a: TWPoint<T>; b: T): TWPoint<T>; overload; inline;

            {**
             Division operator
             @param(a First point to divide)
             @param(b Second point to divide)
             @returns(Point containing the result)
            }
            class operator Divide(a, b: TWPoint<T>): TWPoint<T>; overload; inline;

            {**
             Division operator
             @param(a Point to divide value by)
             @param(b Value to divide by)
             @returns(Point containing the result)
            }
            class operator Divide(a: TWPoint<T>; b: T): TWPoint<T>; overload; inline;

            {**
             Integer division operator
             @param(a First point to divide)
             @param(b Second point to divide)
             @returns(Point containing the result)
            }
            class operator IntDivide(a, b: TWPoint<T>): TWPoint<T>; overload; inline;

            {**
             Integer division operator
             @param(a Point to divide value by)
             @param(b Value to divide by)
             @returns(Point containing the result)
            }
            class operator IntDivide(a: TWPoint<T>; b: T): TWPoint<T>; overload; inline;

            {**
             Inversion operator
             @param(a Point to invert)
             @returns(Point containing the result)
            }
            class operator Negative(a: TWPoint<T>): TWPoint<T>; inline;

            {**
             Equality operator
             @param(a First point to compare)
             @param(b Second point to compare)
             @returns(@true if points are equal, otherwise @false)
            }
            class operator Equal(a, b: TWPoint<T>): Boolean; inline;

            {**
             Not equality operator
             @param(a First point to compare)
             @param(b Second point to compare)
             @returns(@true if points are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWPoint<T>): Boolean; inline;

            {**
             Check if point content is equal to another point
             @param(other Other point to compare with)
             @returns(@true if point are equals, otherwise @false)
            }
            function IsEqual(const other: TWPoint<T>): Boolean; inline;

            {**
             Check if point content differs from another point
             @param(other Other point to compare with)
             @returns(@true if points differ, otherwise @false)
            }
            function Differs(const other: TWPoint<T>): Boolean; inline;

            {**
             Invert the point (e.g. a point with coordinates [-3, 4] will become [3, -4])
             @returns(Inverted point)
            }
            function Invert: TWPoint<T>; inline;

            {**
             Check if point is set to [0, 0] coordinate
             @returns(@true if point is set to [0, 0] coordinate, otherwise @false)
            }
            function IsZero: Boolean; inline;

            {**
             Convert point to VCL TPoint
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Point as VCL TPoint
            }
            {$ifdef USE_VCL}
                function ToTPoint(doRound: Boolean): TPoint; inline;
            {$endif}

            {**
             Convert point to VCL TPointF
             @returns(Point as VCL TPointF)
            }
            {$ifdef USE_VCL}
                function ToTPointF: TPointF; inline;
            {$endif}

            {**
             Convert point to GDI+ point
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Point as GDI+ point)
            }
            {$ifdef USE_VCL}
                function ToGpPoint(doRound: Boolean): TGpPoint; inline;
            {$endif}

            {**
             Convert point to GDI+ PointF
             @returns(Point as GDI+ PointF)
            }
            {$ifdef USE_VCL}
                function ToGpPointF: TGpPointF; inline;
            {$endif}

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; inline;

        public
            {**
             Gets or sets the point x coordinate
            }
            property X: T read GetX write SetX;

            {**
             Gets or sets the point y coordinate
            }
            property Y: T read GetY write SetY;
    end;

    TWPointI = TWPoint<Integer>;
    TWPointF = TWPoint<Single>;

    PWPointI = ^TWPointI;
    PWPointF = ^TWPointF;

implementation
//---------------------------------------------------------------------------
// TWPoint
//---------------------------------------------------------------------------
constructor TWPoint<T>.Create(const x, y: T);
begin
    m_X.Value := x;
    m_Y.Value := y;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWPoint<T>.Create(const point: TPoint);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWPoint<T>.Create(const point: TPointF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            x, y: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                x := Round(point.X);
                y := Round(point.Y);

                m_X.Value := x.AsType<T>;
                m_Y.Value := y.AsType<T>;
            {$else}
                m_X := Round(point.X);
                m_Y := Round(point.Y);
            {$endif}
            Exit;
        end;

        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWPoint<T>.Create(const point: TGpPoint);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWPoint<T>.Create(const point: TGpPointF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            x, y: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                x := Round(point.X);
                y := Round(point.Y);

                m_X.Value := x.AsType<T>;
                m_Y.Value := y.AsType<T>;
            {$else}
                m_X := Round(point.X);
                m_Y := Round(point.Y);
            {$endif}
            Exit;
        end;

        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
class operator TWPoint<T>.Add(a, b: TWPoint<T>): TWPoint<T>;
begin
    Result.X := (a.m_X + b.m_X).Value;
    Result.Y := (a.m_Y + b.m_Y).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Add(a: TWPoint<T>; b: T): TWPoint<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.X := (a.m_X + value).Value;
    Result.Y := (a.m_Y + value).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Subtract(a, b: TWPoint<T>): TWPoint<T>;
begin
    Result.X := (a.m_X - b.m_X).Value;
    Result.Y := (a.m_Y - b.m_Y).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Subtract(a: TWPoint<T>; b: T): TWPoint<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.X := (a.m_X - value).Value;
    Result.Y := (a.m_Y - value).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Multiply(a, b: TWPoint<T>): TWPoint<T>;
begin
    Result.X := (a.m_X * b.m_X).Value;
    Result.Y := (a.m_Y * b.m_Y).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Multiply(a: TWPoint<T>; b: T): TWPoint<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.X := (a.m_X * value).Value;
    Result.Y := (a.m_Y * value).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Divide(a, b: TWPoint<T>): TWPoint<T>;
begin
    Result.X := (a.m_X / b.m_X).Value;
    Result.Y := (a.m_Y / b.m_Y).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Divide(a: TWPoint<T>; b: T): TWPoint<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.X := (a.m_X / value).Value;
    Result.Y := (a.m_Y / value).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.IntDivide(a, b: TWPoint<T>): TWPoint<T>;
begin
    Result.X := (a.m_X div b.m_X).Value;
    Result.Y := (a.m_Y div b.m_Y).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.IntDivide(a: TWPoint<T>; b: T): TWPoint<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.X := (a.m_X div value).Value;
    Result.Y := (a.m_Y div value).Value;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Negative(a: TWPoint<T>): TWPoint<T>;
begin
    Result := a.Invert;
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.Equal(a, b: TWPoint<T>): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWPoint<T>.NotEqual(a, b: TWPoint<T>): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
function TWPoint<T>.GetX: T;
begin
    Result := m_X.Value;
end;
//---------------------------------------------------------------------------
procedure TWPoint<T>.SetX(value: T);
begin
    m_X.Value := value;
end;
//---------------------------------------------------------------------------
function TWPoint<T>.GetY: T;
begin
    Result := m_Y.Value;
end;
//---------------------------------------------------------------------------
procedure TWPoint<T>.SetY(value: T);
begin
    m_Y.Value := value;
end;
//---------------------------------------------------------------------------
function TWPoint<T>.IsEqual(const other: TWPoint<T>): Boolean;
begin
    Result := ((m_X = other.m_X) and (m_Y = other.m_Y));
end;
//---------------------------------------------------------------------------
function TWPoint<T>.Differs(const other: TWPoint<T>): Boolean;
begin
    Result := ((m_X <> other.m_X) or (m_Y <> other.m_Y));
end;
//---------------------------------------------------------------------------
function TWPoint<T>.Invert: TWPoint<T>;
begin
    {$if CompilerVersion <= 24}
        Result.m_X := TWGenericNumber<T>(0.0) - m_X;
        Result.m_Y := TWGenericNumber<T>(0.0) - m_Y;
    {$else}
        Result.m_X := -m_X;
        Result.m_Y := -m_Y;
    {$endif}
end;
//---------------------------------------------------------------------------
function TWPoint<T>.IsZero: Boolean;
begin
    {$if CompilerVersion <= 24}
        Result := ((m_X = TWGenericNumber<T>(0.0)) and (m_Y = TWGenericNumber<T>(0.0)));
    {$else}
        Result := ((m_X = 0.0) and (m_Y = 0.0));
    {$endif}
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWPoint<T>.ToTPoint(doRound: Boolean): TPoint;
    var
        x, y: TValue;
    begin
        x := TValue.From<T>(m_X.Value);
        y := TValue.From<T>(m_Y.Value);

        if (doRound) then
        begin
            Result.X := Round(x.AsExtended);
            Result.Y := Round(y.AsExtended);
            Exit;
        end;

        Result.X := x.AsInt64;
        Result.Y := y.AsInt64;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWPoint<T>.ToTPointF: TPointF;
    var
        x, y: TValue;
    begin
        x := TValue.From<T>(m_X.Value);
        y := TValue.From<T>(m_Y.Value);

        Result.X := x.AsExtended;
        Result.Y := y.AsExtended;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWPoint<T>.ToGpPoint(doRound: Boolean): TGpPoint;
    var
        x, y: TValue;
    begin
        x := TValue.From<T>(m_X.Value);
        y := TValue.From<T>(m_Y.Value);

        if (doRound) then
        begin
            Result.X := Round(x.AsExtended);
            Result.Y := Round(y.AsExtended);
            Exit;
        end;

        Result.X := x.AsInt64;
        Result.Y := y.AsInt64;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWPoint<T>.ToGpPointF: TGpPointF;
    var
        x, y: TValue;
    begin
        x := TValue.From<T>(m_X.Value);
        y := TValue.From<T>(m_Y.Value);

        Result.X := x.AsExtended;
        Result.Y := y.AsExtended;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWPoint<T>.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_X, SizeOf(T), initValue);
        Result := THashBobJenkins.GetHashValue(m_Y, SizeOf(T), Result);
    {$else}
        Result := BobJenkinsHash(m_X, SizeOf(T), initValue);
        Result := BobJenkinsHash(m_Y, SizeOf(T), Result);
    {$endif}
end;
//---------------------------------------------------------------------------

end.

