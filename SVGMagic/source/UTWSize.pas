{**
 @abstract(@name provides a generic size structure.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSize;
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
     Platform independent size structure
    }
    TWSize<T> = record
        private
            m_Width:  TWGenericNumber<T>;
            m_Height: TWGenericNumber<T>;

            {**
             Get width value
             @returns(Width value)
            }
            function GetWidth: T; inline;

            {**
             Set width value
             @param(value Width value)
            }
            procedure SetWidth(value: T); inline;

            {**
             Get height value
             @returns(Height value)
            }
            function GetHeight: T; inline;

            {**
             Set height value
             @param(value Height value)
            }
            procedure SetHeight(value: T); inline;

        public
            {**
             Constructor
             @param(width Width)
             @param(height Height)
            }
            constructor Create(const width, height: T); overload;

            {**
             Constructor
             @param(size VCL size)
            }
            {$ifdef USE_VCL}
                constructor Create(const size: TSize); overload;
            {$endif}

            {**
             Constructor
             @param(size VCL floating size)
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const size: TSizeF; doRound: Boolean); overload;
            {$endif}

            {**
             Constructor
             @param(size GDI+ size)
            }
            {$ifdef USE_VCL}
                constructor Create(const size: TGpSize); overload;
            {$endif}

            {**
             Constructor
             @param(size GDI+ floating size)
             @param(round If @true, values will be rounded (useful e.g. to convert from Single to Integer))
            }
            {$ifdef USE_VCL}
                constructor Create(const size: TGpSizeF; doRound: Boolean); overload;
            {$endif}

            {**
             Addition operator
             @param(a First size to add)
             @param(b Second size to add)
             @returns(Size containing the result)
            }
            class operator Add(a, b: TWSize<T>): TWSize<T>; overload; inline;

            {**
             Addition operator
             @param(a Size to add value to)
             @param(b Value to add to)
             @returns(Size containing the result)
            }
            class operator Add(a: TWSize<T>; b: T): TWSize<T>; overload; inline;

            {**
             Subtraction operator
             @param(a First size to subtract)
             @param(b Second size to subtract)
             @returns(Size containing the result)
            }
            class operator Subtract(a, b: TWSize<T>): TWSize<T>; overload; inline;

            {**
             Subtraction operator
             @param(a Size to subtract value from)
             @param(b Value to subtract to)
             @returns(Size containing the result)
            }
            class operator Subtract(a: TWSize<T>; b: T): TWSize<T>; overload; inline;

            {**
             Multiplication operator
             @param(a First size to multiply)
             @param(b Second size to multiply)
             @returns(Size containing the result)
            }
            class operator Multiply(a, b: TWSize<T>): TWSize<T>; overload; inline;

            {**
             Multiplication operator
             @param(a Size to multiply value with)
             @param(b Value to multiply with)
             @returns(Size containing the result)
            }
            class operator Multiply(a: TWSize<T>; b: T): TWSize<T>; overload; inline;

            {**
             Division operator
             @param(a First size to divide)
             @param(b Second size to divide)
             @returns(Size containing the result)
            }
            class operator Divide(a, b: TWSize<T>): TWSize<T>; overload; inline;

            {**
             Division operator
             @param(a Size to divide value by)
             @param(b Value to divide by)
             @returns(Size containing the result)
            }
            class operator Divide(a: TWSize<T>; b: T): TWSize<T>; overload; inline;

            {**
             Integer division operator
             @param(a First size to divide)
             @param(b Second size to divide)
             @returns(Size containing the result)
            }
            class operator IntDivide(a, b: TWSize<T>): TWSize<T>; overload; inline;

            {**
             Integer division operator
             @param(a Size to divide value by)
             @param(b Value to divide by)
             @returns(Size containing the result)
            }
            class operator IntDivide(a: TWSize<T>; b: T): TWSize<T>; overload; inline;

            {**
             Inversion operator
             @param(a Size to invert)
             @returns(Size containing the result)
            }
            class operator Negative(a: TWSize<T>): TWSize<T>; inline;

            {**
             Equality operator
             @param(a First size to compare)
             @param(b Second size to compare)
             @returns(@true if sizes are equal, otherwise @false)
            }
            class operator Equal(a, b: TWSize<T>): Boolean; inline;

            {**
             Not equality operator
             @param(a First size to compare)
             @param(b Second size to compare)
             @returns(@true if sizes are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWSize<T>): Boolean; inline;

            {**
             Check if size content is equal to another size
             @param(other Other size to compare with)
             @returns(@true if sizes are equal, otherwise @false)
            }
            function IsEqual(const other: TWSize<T>): Boolean; inline;

            {**
             Check if size content differs from another size
             @param(other Other size to compare with)
             @returns(@true if sizes differ, otherwise @false)
            }
            function Differs(const other: TWSize<T>): Boolean; inline;

            {**
             Invert the size (e.g. a size with coordinates [-3, 4] will become [3, -4])
             @returns(Inverted size)
            }
            function Invert: TWSize<T>; inline;

            {**
             Check if size is set to [0, 0] coordinate
             @returns(@true if size is set to [0, 0] coordinate, otherwise @false)
            }
            function IsZero: Boolean; inline;

            {**
             Convert size to VCL TSize
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Size as VCL TSize)
            }
            {$ifdef USE_VCL}
                function ToTSize(doRound: Boolean): TSize; inline;
            {$endif}

            {**
             Convert size to VCL TSizeF
             @returns(Size as VCL TSizeF)
            }
            {$ifdef USE_VCL}
                function ToTSizeF: TSizeF; inline;
            {$endif}

            {**
             Convert size to GDI+ size
             @param(doRound If @true, values will be rounded (useful e.g. to convert from Single to Integer))
             @returns(Size as GDI+ size)
            }
            {$ifdef USE_VCL}
                function ToGpSize(doRound: Boolean): TGpSize; inline;
            {$endif}

            {**
            * Convert size to GDI+ SizeF
            *@returns size as GDI+ SizeF
            }
            {$ifdef USE_VCL}
                function ToGpSizeF: TGpSizeF; inline;
            {$endif}

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; inline;

        public
            {**
             Get or set the width
            }
            property Width: T read GetWidth write SetWidth;

            {**
             Get or set the height
            }
            property Height: T read GetHeight write SetHeight;
    end;

    TWSizeI = TWSize<Integer>;
    TWSizeF = TWSize<Single>;

    PWSizeI = ^TWSizeI;
    PWSizeF = ^TWSizeF;

implementation
//---------------------------------------------------------------------------
constructor TWSize<T>.Create(const width, height: T);
begin
    m_Width.Value  := width;
    m_Height.Value := height;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWSize<T>.Create(const size: TSize);
    begin
        m_Width  := size.Width;
        m_Height := size.Height;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWSize<T>.Create(const size: TSizeF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            width, height: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                width  := Round(size.Width);
                height := Round(size.Height);

                m_Width.Value  := width.AsType<T>;
                m_Height.Value := height.AsType<T>;
            {$else}
                m_Width  := Round(size.Width);
                m_Height := Round(size.Height);
            {$endif}
            Exit;
        end;

        m_Width  := size.Width;
        m_Height := size.Height;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWSize<T>.Create(const size: TGpSize);
    begin
        m_Width  := size.Width;
        m_Height := size.Height;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWSize<T>.Create(const size: TGpSizeF; doRound: Boolean);
    {$ifdef CPUX64}
        var
            width, height: TValue;
    {$endif}
    begin
        if (doRound) then
        begin
            {$ifdef CPUX64}
                width  := Round(size.Width);
                height := Round(size.Height);

                m_Width.Value  := width.AsType<T>;
                m_Height.Value := height.AsType<T>;
            {$else}
                m_Width  := Round(size.Width);
                m_Height := Round(size.Height);
            {$endif}
            Exit;
        end;

        m_Width  := size.Width;
        m_Height := size.Height;
    end;
{$endif}
//---------------------------------------------------------------------------
class operator TWSize<T>.Add(a, b: TWSize<T>): TWSize<T>;
begin
    Result.Width  := (a.m_Width  + b.m_Width).Value;
    Result.Height := (a.m_Height + b.m_Height).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Add(a: TWSize<T>; b: T): TWSize<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Width  := (a.m_Width  + value).Value;
    Result.Height := (a.m_Height + value).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Subtract(a, b: TWSize<T>): TWSize<T>;
begin
    Result.Width  := (a.m_Width  - b.m_Width).Value;
    Result.Height := (a.m_Height - b.m_Height).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Subtract(a: TWSize<T>; b: T): TWSize<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Width  := (a.m_Width  - value).Value;
    Result.Height := (a.m_Height - value).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Multiply(a, b: TWSize<T>): TWSize<T>;
begin
    Result.Width  := (a.m_Width  * b.m_Width).Value;
    Result.Height := (a.m_Height * b.m_Height).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Multiply(a: TWSize<T>; b: T): TWSize<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Width  := (a.m_Width  * value).Value;
    Result.Height := (a.m_Height * value).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Divide(a, b: TWSize<T>): TWSize<T>;
begin
    Result.Width  := (a.m_Width  / b.m_Width).Value;
    Result.Height := (a.m_Height / b.m_Height).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Divide(a: TWSize<T>; b: T): TWSize<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Width  := (a.m_Width  / value).Value;
    Result.Height := (a.m_Height / value).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.IntDivide(a, b: TWSize<T>): TWSize<T>;
begin
    Result.Width  := (a.m_Width  div b.m_Width).Value;
    Result.Height := (a.m_Height div b.m_Height).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.IntDivide(a: TWSize<T>; b: T): TWSize<T>;
var
    value: TWGenericNumber<T>;
begin
    value.Value := b;

    Result.Width  := (a.m_Width  div value).Value;
    Result.Height := (a.m_Height div value).Value;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Negative(a: TWSize<T>): TWSize<T>;
begin
    Result := a.Invert;
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.Equal(a, b: TWSize<T>): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWSize<T>.NotEqual(a, b: TWSize<T>): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
function TWSize<T>.GetWidth: T;
begin
    Result := m_Width.Value;
end;
//---------------------------------------------------------------------------
procedure TWSize<T>.SetWidth(value: T);
begin
    m_Width.Value := value;
end;
//---------------------------------------------------------------------------
function TWSize<T>.GetHeight: T;
begin
    Result := m_Height.Value;
end;
//---------------------------------------------------------------------------
procedure TWSize<T>.SetHeight(value: T);
begin
    m_Height.Value := value;
end;
//---------------------------------------------------------------------------
function TWSize<T>.IsEqual(const other: TWSize<T>): Boolean;
begin
    Result := ((m_Width = other.m_Width) and (m_Height = other.m_Height));
end;
//---------------------------------------------------------------------------
function TWSize<T>.Differs(const other: TWSize<T>): Boolean;
begin
    Result := ((m_Width <> other.m_Width) or (m_Height <> other.m_Height));
end;
//---------------------------------------------------------------------------
function TWSize<T>.Invert: TWSize<T>;
begin
    {$if CompilerVersion <= 24}
        Result.m_Width  := TWGenericNumber<T>(0.0) - m_Width;
        Result.m_Height := TWGenericNumber<T>(0.0) - m_Height;
    {$else}
        Result.m_Width  := -m_Width;
        Result.m_Height := -m_Height;
    {$ifend}
end;
//---------------------------------------------------------------------------
function TWSize<T>.IsZero: Boolean;
begin
    {$if CompilerVersion <= 24}
        Result := ((m_Width = TWGenericNumber<T>(0.0)) and (m_Height = TWGenericNumber<T>(0.0)));
    {$else}
        Result := ((m_Width = 0.0) and (m_Height = 0.0));
    {$ifend}
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWSize<T>.ToTSize(doRound: Boolean): TSize;
    var
        width, height: TValue;
    begin
        width  := TValue.From<T>(m_Width.Value);
        height := TValue.From<T>(m_Height.Value);

        if (doRound) then
        begin
            Result.Width  := Round(width.AsExtended);
            Result.Height := Round(height.AsExtended);
            Exit;
        end;

        Result.Width  := width.AsInt64;
        Result.Height := height.AsInt64;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWSize<T>.ToTSizeF: TSizeF;
    var
        width, height: TValue;
    begin
        width  := TValue.From<T>(m_Width.Value);
        height := TValue.From<T>(m_Height.Value);

        Result.Width  := width.AsExtended;
        Result.Height := height.AsExtended;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWSize<T>.ToGpSize(doRound: Boolean): TGpSize;
    var
        width, height: TValue;
    begin
        width  := TValue.From<T>(m_Width.Value);
        height := TValue.From<T>(m_Height.Value);

        if (doRound) then
        begin
            Result.Width  := Round(width.AsExtended);
            Result.Height := Round(height.AsExtended);
            Exit;
        end;

        Result.Width  := width.AsInt64;
        Result.Height := height.AsInt64;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWSize<T>.ToGpSizeF: TGpSizeF;
    var
        width, height: TValue;
    begin
        width  := TValue.From<T>(m_Width.Value);
        height := TValue.From<T>(m_Height.Value);

        Result.Width  := width.AsExtended;
        Result.Height := height.AsExtended;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWSize<T>.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Width,  SizeOf(T), initValue);
        Result := THashBobJenkins.GetHashValue(m_Height, SizeOf(T), Result);
    {$else}
        Result := BobJenkinsHash(m_Width,  SizeOf(T), initValue);
        Result := BobJenkinsHash(m_Height, SizeOf(T), Result);
    {$ifend}
end;
//---------------------------------------------------------------------------

end.
