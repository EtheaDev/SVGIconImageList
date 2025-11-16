{**
 @abstract(@name provides a 3x3 matrix.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWMatrix;
{$I SVGMagic.inc}

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     System.Generics.Defaults,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     {$ifdef USE_VCL}
         Winapi.GDIPObj,
     {$endif}
     UTWVector;

type
    {**
     3x3 matrix
     @br @bold(NOTE) The matrix table can be read in both mathematical or in-memory order. From a
                     mathematical point of view, the m_Table content should be read as m_Table[x][y],
                     whereas it should be read as m_Table[y][x] from the in-memory point of view.
                     The both representation are equivalent and interchangeable, and may be used
                     anytime if the same logical is used constantly. However, this may be highly
                     confusing when you have to manipulate the source code, because the table items
                     may appear to be crossed. For that reason, be aware of what you are doing if
                     you have to modify it
    }
    TWMatrix3x3 = record
        private type
            IWTable = array [0..2, 0..2] of Double;

        private
            m_Table: IWTable;

            {**
             Get table item
             @param(x Horizontal index between 0 and 2)
             @param(y Vertical index between 0 and 2)
             @returns(The table item)
             @raises(Exception if x or y index is higher than 2)
            }
            function GetTableItem(x, y: NativeUInt): Double;

            {**
             Set table item
             @param(x Horizontal index between 0 and 2)
             @param(y Vertical index between 0 and 2)
             @param(value Table item value to set)
             @raises(Exception if x or y index is higher than 2)
            }
            procedure SetTableItem(x, y: NativeUInt; value: Double);

        public
            {**
             Constructor
             @param(_11 Matrix value at [0][0])
             @param(_12 Matrix value at [1][0])
             @param(_13 Matrix value at [2][0])
             @param(_21 Matrix value at [0][1])
             @param(_22 Matrix value at [1][1])
             @param(_23 Matrix value at [2][1])
             @param(_31 Matrix value at [0][2])
             @param(_32 Matrix value at [1][2])
             @param(_33 Matrix value at [2][2])
            }
            constructor Create(const _11, _12, _13,
                                     _21, _22, _23,
                                     _31, _32, _33: Double); overload;

            {**
             Constructor
             @param(matrix Source GDI+ matrix to build from)
            }
            {$ifdef USE_VCL}
                constructor Create(const matrix: TGpMatrix); overload;
            {$endif}

            {**
             Copy constructor
             @param(other Other matrix to copy from)
            }
            constructor Create(const other: TWMatrix3x3); overload;

            {**
             Equality operator (allows operations on matrices like a = b)
             @param(a First matrix to compare)
             @param(b Second matrix to compare)
             @returns(@true if matrices are equal, otherwise @false)
            }
            class operator Equal(a, b: TWMatrix3x3): Boolean;

            {**
             Not equality operator (allows operations on matrices like a <> b)
             @param(a First matrix to compare)
             @param(b Second color matrix to compare)
             @returns(@true if matrices are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWMatrix3x3): Boolean;

            {**
             Get the default matrix
             @returns(A new default identity matrix)
            }
            class function GetDefault: TWMatrix3x3; inline; static;

            {**
             Check if matrix content is equal to another matrix
             @param(other Other matrix to compare with)
             @returns(@true if matrices are equals, otherwise @false)
            }
            function IsEqual(const other: TWMatrix3x3): Boolean;

            {**
             Check if matrix content differs from another matrix
             @param(other Other matrix to compare with)
             @returns(@true if matrices differ, otherwise @false)
            }
            function Differs(const other: TWMatrix3x3): Boolean; inline;

            {**
             Assign (i.e copy) matrix
             @param(other Other matrix to copy from)
            }
            procedure Assign(const other: TWMatrix3x3); inline;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer;

            {**
             Set matrix as identity
            }
            procedure SetIdentity; inline;

            {**
             Check if matrix is the identity matrix (meaning thus this matrix is "empty")
             @returns(@true if matrix is the identity matrix, otherwise @false)
            }
            function IsIdentity: Boolean; inline;

            {**
             Multiply a matrix by another matrix
             @param(other Other matrix to multiply with)
             @returns(multiplied resulting matrix)
             @br @bold(NOTE) The other matrix is prepended to the current one, this means that, to
                             obtain the following result:
                             Mc = Ma * Mb
                             the Multiply function should be used as follow:
                             Mc := Mb.Multiply(Ma);
            }
            function Multiply(const other: TWMatrix3x3): TWMatrix3x3;

            {**
             Translate matrix
             @param(t Translation vector)
             @returns(Copy of translated matrix)
            }
            function Translate(const t: TWVector2): TWMatrix3x3; inline;

            {**
             Rotate matrix
             @param(angle Rotation angle in radians)
             @param(r Rotation direction (e.g. [0.0f, 1.0f] for a y-axis rotation))
             @returns(Copy of rotated matrix)
             @br @bold(NOTE) Rotation direction vector should be normalized before calling this
                             function
            }
            function Rotate(angle: Single; const r: TWVector2): TWMatrix3x3; inline;

            {**
             Rotate matrix based on a rotation center point
             @param(angle Rotation angle in radians)
             @param(center Rotation center point)
             @returns(Copy of rotated matrix)
            }
            function RotateCenter(angle: Single; const center: TWVector2): TWMatrix3x3; inline;

            {**
             Scale matrix
             @param(s Scale vector)
             @returns(Copy of scaled matrix)
            }
            function Scale(const s: TWVector2): TWMatrix3x3; inline;

            {**
             Shear matrix
             @param(s Shear vector)
             @returns(Copy of sheared matrix)
            }
            function Shear(const s: TWVector2): TWMatrix3x3; inline;

            {**
             Transform a vector using matrix
             @param(vector Vector to transform)
             @returns(transformed vector)
            }
            function Transform(const vector: TWVector2): TWVector2; inline;

            {**
             Convert matrix to GDI+ matrix
             @param(pMatrix GDI+ matrix to populate)
            }
            {$ifdef USE_VCL}
                procedure ToGpMatrix(pMatrix: TGpMatrix); inline;
            {$endif}

        public
            {**
             Gets or sets the matrix table item. Example: Table[2, 0] := 12.33f;
             @br @bold(NOTE) Values for x and y doesn't exceed 2, otherwise an exception will be raised
            }
            property Table[x, y: NativeUInt]: Double read GetTableItem write SetTableItem;
    end;

    PWMatrix3x3 = ^TWMatrix3x3;

implementation
//---------------------------------------------------------------------------
constructor TWMatrix3x3.Create(const _11, _12, _13,
                                     _21, _22, _23,
                                     _31, _32, _33: Double);

begin
    m_Table[0][0] := _11; m_Table[1][0] := _12; m_Table[2][0] := _13;
    m_Table[0][1] := _21; m_Table[1][1] := _22; m_Table[2][1] := _23;
    m_Table[0][2] := _31; m_Table[1][2] := _32; m_Table[2][2] := _33;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWMatrix3x3.Create(const matrix: TGpMatrix);
    var
        elements: TMatrixArray;
    begin
        matrix.GetElements(elements);

        m_Table[0][0] := elements[0]; m_Table[1][0] := elements[2]; m_Table[2][0] := elements[4];
        m_Table[0][1] := elements[1]; m_Table[1][1] := elements[3]; m_Table[2][1] := elements[5];
        m_Table[0][2] := 0.0;         m_Table[1][2] := 0.0;         m_Table[2][2] := 1.0;
    end;
{$endif}
//---------------------------------------------------------------------------
constructor TWMatrix3x3.Create(const other: TWMatrix3x3);
begin
    Assign(other);
end;
//---------------------------------------------------------------------------
function TWMatrix3x3.GetTableItem(x, y: NativeUInt): Double;
begin
    if ((x > 2) or (y > 2)) then
        raise Exception.CreateFmt('Index out of bounds - x - %d - y - %d', [x, y]);

    Result := m_Table[x][y];
end;
//---------------------------------------------------------------------------
procedure TWMatrix3x3.SetTableItem(x, y: NativeUInt; value: Double);
begin
    if ((x > 2) or (y > 2)) then
        raise Exception.CreateFmt('Index out of bounds - x - %d - y - %d', [x, y]);

    m_Table[x][y] := value;
end;
//---------------------------------------------------------------------------
class operator TWMatrix3x3.Equal(a, b: TWMatrix3x3): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWMatrix3x3.NotEqual(a, b: TWMatrix3x3): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
class function TWMatrix3x3.GetDefault: TWMatrix3x3;
begin
    Result := Default(TWMatrix3x3);
    Result.SetIdentity;
end;
//---------------------------------------------------------------------------
function TWMatrix3x3.IsEqual(const other: TWMatrix3x3): Boolean;
var
    i, j: Byte;
begin
    // compare each matrix element with other matrix. NOTE don't use CompareMem here because the
    // comparison will fail in the following cases:
    // 1. if 0.0 == -0.0, the equality operator will return true whereas the memory cmp will fail
    // 2. if NaN, the memory compare will always fail
    for i := 0 to 2 do
        for j := 0 to 2 do
            if (m_Table[i][j] <> other.m_Table[i][j]) then
                Exit(False);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWMatrix3x3.Differs(const other: TWMatrix3x3): Boolean;
begin
    Result := not IsEqual(other);
end;
//---------------------------------------------------------------------------
procedure TWMatrix3x3.Assign(const other: TWMatrix3x3);
begin
    Move(other.m_Table, m_Table, SizeOf(m_Table));
end;
//---------------------------------------------------------------------------
function TWMatrix3x3.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Table, SizeOf(m_Table), initValue);
    {$else}
        Result := BobJenkinsHash(m_Table, SizeOf(m_Table), initValue);
    {$ifend}
end;
//---------------------------------------------------------------------------
procedure TWMatrix3x3.SetIdentity;
begin
    m_Table[0][0] := 1.0; m_Table[1][0] := 0.0; m_Table[2][0] := 0.0;
    m_Table[0][1] := 0.0; m_Table[1][1] := 1.0; m_Table[2][1] := 0.0;
    m_Table[0][2] := 0.0; m_Table[1][2] := 0.0; m_Table[2][2] := 1.0;
end;
//---------------------------------------------------------------------------
function TWMatrix3x3.IsIdentity: Boolean;
begin
    Result := IsEqual(GetDefault);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Multiply(const other: TWMatrix3x3): TWMatrix3x3;
var
    i, j: Byte;
begin
    for i := 0 to 2 do
        for j := 0 to 2 do
            Result.m_Table[i][j] := m_Table[i][0] * other.m_Table[0][j] +
                                    m_Table[i][1] * other.m_Table[1][j] +
                                    m_Table[i][2] * other.m_Table[2][j];
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Translate(const t: TWVector2): TWMatrix3x3;
begin
    m_Table[2][0] := m_Table[2][0] + (m_Table[0][0] * t.X + m_Table[1][0] * t.Y);
    m_Table[2][1] := m_Table[2][1] + (m_Table[0][1] * t.X + m_Table[1][1] * t.Y);

    Result.Assign(Self);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Rotate(angle: Single; const r: TWVector2): TWMatrix3x3;
var
    c, s:   Single;
    matrix: TWMatrix3x3;
begin
    // calculate sinus and cosinus values
    c := Cos(angle);
    s := Sin(angle);

    // create rotation matrix
    matrix.SetIdentity;
    matrix.m_Table[0][0] := c * r.X; matrix.m_Table[1][0] := -s * r.Y;
    matrix.m_Table[0][1] := s * r.X; matrix.m_Table[1][1] :=  c * r.Y;

    // combine current matrix with rotation matrix
    Assign(matrix.Multiply(Self));

    Result.Assign(Self);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.RotateCenter(angle: Single; const center: TWVector2): TWMatrix3x3;
var
    c, s:   Single;
    matrix: TWMatrix3x3;
begin
    c := Cos(angle);
    s := Sin(angle);

    // create rotation matrix
    matrix.SetIdentity;

    // equivalent to translate(center) * rotate(angle) * translate(-center)
    matrix.m_Table[0][0] := c; matrix.m_Table[1][0] := -s; matrix.m_Table[2][0] := center.X - center.X * c + center.Y * s;
    matrix.m_Table[0][1] := s; matrix.m_Table[1][1] :=  c; matrix.m_Table[2][1] := center.Y - center.X * s - center.Y * c;

    // combine current matrix with rotation matrix
    Assign(matrix.Multiply(Self));

    Result.Assign(Self);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Scale(const s: TWVector2): TWMatrix3x3;
begin
    m_Table[0][0] := m_Table[0][0] * s.X; m_Table[1][0] := m_Table[1][0] * s.Y;
    m_Table[0][1] := m_Table[0][1] * s.X; m_Table[1][1] := m_Table[1][1] * s.Y;
    m_Table[0][2] := m_Table[0][2] * s.X; m_Table[1][2] := m_Table[1][2] * s.Y;

    Result.Assign(Self);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Shear(const s: TWVector2): TWMatrix3x3;
var
    matrix: TWMatrix3x3;
begin
    // create rotation matrix
    matrix.SetIdentity;

    matrix.m_Table[0][0] := 1.0; matrix.m_Table[1][0] := s.X;
    matrix.m_Table[0][1] := s.Y; matrix.m_Table[1][1] := 1.0;

    // combine current matrix with shear matrix
    Assign(matrix.Multiply(Self));

    Result.Assign(Self);
end;
//------------------------------------------------------------------------------
function TWMatrix3x3.Transform(const vector: TWVector2): TWVector2;
begin
    // calculates x and y coordinates and returns transformed vector
    Result := TWVector2.Create((vector.X * m_Table[0][0] + vector.Y * m_Table[1][0] + m_Table[2][0]),
                               (vector.X * m_Table[0][1] + vector.Y * m_Table[1][1] + m_Table[2][1]));
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWMatrix3x3.ToGpMatrix(pMatrix: TGpMatrix);
    begin
        pMatrix.SetElements(m_Table[0][0], m_Table[0][1],
                            m_Table[1][0], m_Table[1][1],
                            m_Table[2][0], m_Table[2][1]);
    end;
{$endif}
//---------------------------------------------------------------------------

end.
