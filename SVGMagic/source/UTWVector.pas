{**
 @abstract(@name provides a geometrical 2D vector.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWVector;
{$I SVGMagic.inc}

interface

uses System.Types,
     System.SysUtils,
     System.Generics.Defaults
     {$if CompilerVersion >= 29}
         ,
         System.Hash
     {$ifend}
     {$ifdef USE_VCL}
         ,
         Winapi.GDIPAPI,
         Winapi.Windows
     {$endif}
     ;

type
    {**
     2D vector
    }
    TWVector2 = record
        private
            m_X: Single;
            m_Y: Single;

        public
            {**
             Constructor
             @param(x x coordinate)
             @param(y y coordinate)
            }
            constructor Create(const x, y: Single); overload;

            {**
             Copy constructor
             @param(other Other vector to copy from)
            }
            {$ifndef CPUX64}
                constructor Create(const other: TWVector2); overload;
            {$endif}

            {**
             Constructor
             @param(point Point representing x and y coordinates)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TPoint); overload;
            {$endif}

            {**
             Constructor
             @param(point Point representing x and y coordinates)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TPointF); overload;
            {$endif}

            {**
             Constructor
             @param(point Point representing x and y coordinates)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TGpPoint); overload;
            {$endif}

            {**
             Constructor
             @param(point Point representing x and y coordinates)
            }
            {$ifdef USE_VCL}
                constructor Create(const point: TGpPointF); overload;
            {$endif}

            {**
             Equality operator (allows operations on vectors like a = b)
             @param(a First vector to compare)
             @param(b Second vector to compare)
             @returns(@true if vectors are equal, otherwise @false)
            }
            class operator Equal(a, b: TWVector2): Boolean;

            {**
             Not equality operator (allows operations on vectors like a <> b)
             @param(a First vector to compare)
             @param(b Second vector to compare)
             @returns(@true if vectors are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWVector2): Boolean;

            {**
             Assigns (i.e. copies) the content from another vector
             @param(other Other vector to copy from)
            }
            procedure Assign(const other: TWVector2); inline;

            {**
             Compares the content of 2 vectors and determines if they are equal
             @param(other Other vector to compare with)
             @returns(@true if vectors are equal, otherwise @false)
            }
            function IsEqual(const other: TWVector2): Boolean; inline;

            {**
             Compares the content of 2 vectors and determines if they are different
             @param(other Other vector to compare with)
             @returns(@true if vectors differ, otherwise @false)
            }
            function Differs(const other: TWVector2): Boolean; inline;

            {**
             Inverts the vector
             @returns(Inverted vector)
            }
            function Invert: TWVector2; overload; inline;

            {**
             Adds a value to this vector
             @param(value Value to add to vector)
             @returns(Added vector)
            }
            function Add(const value: Single): TWVector2; overload; inline;

            {**
             Adds the content of another vector to this vector
             @param(other Other vector to add to this vector)
             @returns(Added vector)
            }
            function Add(const other: TWVector2): TWVector2; overload; inline;

            {**
             Subtracts a value to this vector
             @param(value Value to subtract to this vector)
             @returns(Subtracted vector)
            }
            function Sub(const value: Single): TWVector2; overload; inline;

            {**
             Subtracts to this vector the contents of another vector
             @param(other Other vector to subtract to this vector)
             @returns(Subtracted vector)
            }
            function Sub(const other: TWVector2): TWVector2; overload; inline;

            {**
             Multiplies a value to this vector
             @param(value Value to multiply to this vector)
             @returns(Multiplied vector)
            }
            function Mul(const value: Single): TWVector2; overload; inline;

            {**
             Multiplies the content of another vector to this vector
             @param(other Other vector to multiply to this vector)
             @returns(Multiplied vector)
            }
            function Mul(const other: TWVector2): TWVector2; overload; inline;

            {**
             Divides a value to this vector
             @param(value Value to divide to this vector)
             @returns(Divided vector)
            }
            function Divide(const value: Single): TWVector2; overload; inline;

            {**
             Divides the content of another vector to this vector
             @param(other Other vector to divide to this vector)
             @returns(Divided vector)
            }
            function Divide(const other: TWVector2): TWVector2; overload; inline;

            {**
             Adds a value and assigns the result to this vector
             @param(value Value to add to vector)
             @returns(Added vector)
            }
            function AddAndAssign(const value: Single): TWVector2; overload; inline;

            {**
             Adds the content of another vector and assigns the result to this vector
             @param(other Other vector to add to this vector)
             @returns(Added vector)
            }
            function AddAndAssign(const other: TWVector2): TWVector2; overload; inline;

            {**
             Subtracts a value and assigns the result to this vector
             @param(value Value to subtract to vector)
             @returns(Subtracted vector)
            }
            function SubAndAssign(const value: Single): TWVector2; overload; inline;

            {**
             Subtracts the content of another vector and assigns the result to this vector
             @param(other Other vector to subtract to this vector)
             @returns(Subtracted vector)
            }
            function SubAndAssign(const other: TWVector2): TWVector2; overload; inline;

            {**
             Multilpies a value and assigns the result to this vector
             @param(value Value to multiply to vector)
             @returns(Multiplied vector)
            }
            function MulAndAssign(const value: Single): TWVector2; overload; inline;

            {**
             Multiplies the content of another vector and assigns the result to this vector
             @param(other Other vector to multiply to this vector)
             @returns(Multiplied vector)
            }
            function MulAndAssign(const other: TWVector2): TWVector2; overload; inline;

            {**
             Divides a value and assigns the result to this vector
             @param(value Value to divide to vector)
             @returns(Divided vector)
            }
            function DivAndAssign(const value: Single): TWVector2; overload; inline;

            {**
             Divides the content of another vector and assigns the result to this vector
             @param(other Other vector to divide to this vector)
             @returns(Divided vector)
            }
            function DivAndAssign(const other: TWVector2): TWVector2; overload; inline;

            {**
             Check if vector is empty (i.e. all values are equals to 0)
             @returns(@true if vector is empty, otherwise @false)
            }
            function IsZero: Boolean; inline;

            {**
             Calculate vector length
             @returns(Length)
            }
            function Length: Single; inline;

            {**
             Get normalized vector
             @returns(Normalized vector)
            }
            function Normalize: TWVector2; inline;

            {**
             Get cross product between 2 vectors
             @param(vector Other vector to calculate with)
             @returns(Cross product)
            }
            function Cross(const vector: TWVector2): TWVector2; inline;

            {**
             Get dot product between 2 vectors (i.e. angle between 2 vectors)
             @param(vector Other vector to calculate with)
             @returns(Dot product)
             @br @bold(NOTE) To find correct angle value, result must be divided by the product of
                             vector lengths (v1.Length * v2.Length) in case they weren't normalized
                             before
             @br @bold(NOTE) Resulting angle is exprimed in Radians
            }
            function Dot(const vector: TWVector2): Single; inline;

            {**
             Convert vector to VCL TPoint
             @param(doRound If @true, values will be rounded (useful e.g. to convert from float to int))
             @returns(Vector as VCL TPoint)
            }
            {$ifdef USE_VCL}
                function ToTPoint(doRound: Boolean): TPoint; inline;
            {$endif}

            {**
             Convert vector to VCL TPointF
             @returns(Vector as VCL TPointF)
            }
            {$ifdef USE_VCL}
                function ToTPointF: TPointF; inline;
            {$endif}

            {**
             Convert vector to GDI+ point
             @param(doRound If @true, values will be rounded (useful e.g. to convert from float to int))
             @returns(Vector as GDI+ point)
            }
            {$ifdef USE_VCL}
                function ToGpPoint(doRound: Boolean): TGpPoint; inline;
            {$endif}

            {**
             Convert vector to GDI+ PointF
             @returns(Vector as GDI+ PointF)
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
            property X: Single read m_X write m_X;
            property Y: Single read m_Y write m_Y;
    end;

    PWVector = ^TWVector2;

implementation
//---------------------------------------------------------------------------
constructor TWVector2.Create(const x, y: Single);
begin
    m_X := x;
    m_Y := y;
end;
//---------------------------------------------------------------------------
{$ifndef CPUX64}
    constructor TWVector2.Create(const other: TWVector2);
    begin
        Assign(other);
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWVector2.Create(const point: TPoint);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWVector2.Create(const point: TPointF);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWVector2.Create(const point: TGpPoint);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWVector2.Create(const point: TGpPointF);
    begin
        m_X := point.X;
        m_Y := point.Y;
    end;
{$endif}
//---------------------------------------------------------------------------
class operator TWVector2.Equal(a, b: TWVector2): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWVector2.NotEqual(a, b: TWVector2): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
procedure TWVector2.Assign(const other: TWVector2);
begin
    m_X := other.m_X;
    m_Y := other.m_Y;
end;
//---------------------------------------------------------------------------
function TWVector2.IsEqual(const other: TWVector2): Boolean;
begin
    Result := ((m_X = other.m_X) and (m_Y = other.m_Y));
end;
//---------------------------------------------------------------------------
function TWVector2.Differs(const other: TWVector2): Boolean;
begin
    Result := ((m_X <> other.m_X) or (m_Y <> other.m_Y));
end;
//---------------------------------------------------------------------------
function TWVector2.Invert: TWVector2;
begin
    Result := TWVector2.Create(-m_X, -m_Y);
end;
//---------------------------------------------------------------------------
function TWVector2.Add(const value: Single): TWVector2;
begin
    Result := TWVector2.Create(m_X + value, m_Y + value);
end;
//---------------------------------------------------------------------------
function TWVector2.Add(const other: TWVector2): TWVector2;
begin
    Result := TWVector2.Create(m_X + other.m_X, m_Y + other.m_Y);
end;
//---------------------------------------------------------------------------
function TWVector2.Sub(const value: Single): TWVector2;
begin
    Result := TWVector2.Create(m_X - value, m_Y - value);
end;
//---------------------------------------------------------------------------
function TWVector2.Sub(const other: TWVector2): TWVector2;
begin
    Result := TWVector2.Create(m_X - other.m_X, m_Y - other.m_Y);
end;
//---------------------------------------------------------------------------
function TWVector2.Mul(const value: Single): TWVector2;
begin
    Result := TWVector2.Create(m_X * value, m_Y * value);
end;
//---------------------------------------------------------------------------
function TWVector2.Mul(const other: TWVector2): TWVector2;
begin
    Result := TWVector2.Create(m_X * other.m_X, m_Y * other.m_Y);
end;
//---------------------------------------------------------------------------
function TWVector2.Divide(const value: Single): TWVector2;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    Result := TWVector2.Create(m_X / value, m_Y / value);
end;
//---------------------------------------------------------------------------
function TWVector2.Divide(const other: TWVector2): TWVector2;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    Result := TWVector2.Create(m_X / other.m_X, m_Y / other.m_Y);
end;
//---------------------------------------------------------------------------
function TWVector2.AddAndAssign(const value: Single): TWVector2;
begin
    m_X := m_X + value;
    m_Y := m_Y + value;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.AddAndAssign(const other: TWVector2): TWVector2;
begin
    m_X := m_X + other.m_X;
    m_Y := m_Y + other.m_Y;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.SubAndAssign(const value: Single): TWVector2;
begin
    m_X := m_X - value;
    m_Y := m_Y - value;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.SubAndAssign(const other: TWVector2): TWVector2;
begin
    m_X := m_X - other.m_X;
    m_Y := m_Y - other.m_Y;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.MulAndAssign(const value: Single): TWVector2;
begin
    m_X := m_X * value;
    m_Y := m_Y * value;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.MulAndAssign(const other: TWVector2): TWVector2;
begin
    m_X := m_X * other.m_X;
    m_Y := m_Y * other.m_Y;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.DivAndAssign(const value: Single): TWVector2;
begin
    if (value = 0.0) then
        raise Exception.Create('Division by 0 is prohibited');

    m_X := m_X / value;
    m_Y := m_Y / value;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.DivAndAssign(const other: TWVector2): TWVector2;
begin
    if (other.m_X = 0.0) then
        raise Exception.Create('Vector x axis - division by 0 is prohibited');

    if (other.m_Y = 0.0) then
        raise Exception.Create('Vector y axis - division by 0 is prohibited');

    m_X := m_X / other.m_X;
    m_Y := m_Y / other.m_Y;

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWVector2.IsZero: Boolean;
begin
    Result := ((m_X = 0.0) and (m_Y = 0.0));
end;
//---------------------------------------------------------------------------
function TWVector2.Length: Single;
begin
    Result := Sqrt((m_X * m_X) + (m_Y * m_Y));
end;
//---------------------------------------------------------------------------
function TWVector2.Normalize: TWVector2;
var
    len: Single;
begin
    len := Length;

    if (len = 0.0) then
    begin
        Result := Default(TWVector2);
        Exit;
    end;

    Result := TWVector2.Create((m_X / len), (m_Y / len));
end;
//---------------------------------------------------------------------------
function TWVector2.Cross(const vector: TWVector2): TWVector2;
begin
    Result := TWVector2.Create((m_Y * vector.m_X) - (vector.m_Y * m_X),
                               (m_X * vector.m_Y) - (vector.m_X * m_Y));
end;
//---------------------------------------------------------------------------
function TWVector2.Dot(const vector: TWVector2): Single;
begin
    Result := ((m_X * vector.m_X) + (m_Y * vector.m_Y));
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWVector2.ToTPoint(doRound: Boolean): TPoint;
    begin
        if (doRound) then
            Result := TPoint.Create(Round(m_X), Round(m_Y))
        else
            Result := TPoint.Create(Trunc(m_X), Trunc(m_Y));
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWVector2.ToTPointF: TPointF;
    begin
        Result := TPointF.Create(m_X, m_Y);
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWVector2.ToGpPoint(doRound: Boolean): TGpPoint;
    begin
        if (doRound) then
        begin
            Result.X := Round(m_X);
            Result.Y := Round(m_Y);
        end
        else
        begin
            Result.X := Trunc(m_X);
            Result.Y := Trunc(m_Y);
        end;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWVector2.ToGpPointF: TGpPointF;
    begin
        Result.X := m_X;
        Result.Y := m_Y;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWVector2.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_X, SizeOf(Single), initValue);
        Result := THashBobJenkins.GetHashValue(m_Y, SizeOf(Single), Result);
    {$else}
        Result := BobJenkinsHash(m_X, SizeOf(Single), initValue);
        Result := BobJenkinsHash(m_Y, SizeOf(Single), Result);
    {$ifend}
end;
//---------------------------------------------------------------------------

end.
