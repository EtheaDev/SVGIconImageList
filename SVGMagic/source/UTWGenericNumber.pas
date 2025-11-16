{**
 @abstract(@name provides a class to work with numbers (real, integer, ...) in a generic way.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWGenericNumber;

interface

uses System.Rtti,
     {$if CompilerVersion >= 23}
        System.TypInfo,
     {$ifend}
     System.SysUtils,
     System.Math,
     UTWHelpers;

type
    {**
     Generic number
    }
    TWGenericNumber<T> = record
        private
            m_Value: T;

        public
            {**
             Addition operator (allows operations on generic numbers like r := a + b)
             @param(a First generic number to add)
             @param(b Second generic number to add)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be added. E.g. trying to add an
                             integer with a cardinal may cause a compiler error
            }
            class operator Add(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Subtraction operator (allows operations on generic numbers like r := a - b)
             @param(a First generic number to subtract)
             @param(b Second generic number to subtract)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be subtracted. E.g. trying to
                             subtract an integer with a cardinal may cause a compiler error
            }
            class operator Subtract(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Multiplication operator (allows operations on generic numbers like r := a * b)
             @param(a First generic number to multiply)
             @param(b Second generic number to multiply)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be multiplied. E.g. trying to
                             multiply an integer with a cardinal may cause a compiler error
            }
            class operator Multiply(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Division operator (allows operations on generic numbers like r := a / b)
             @param(a First generic number to divide)
             @param(b Second generic number to divide)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be divided. E.g. trying to divide an
                             integer with a cardinal may cause a compiler error
            }
            class operator Divide(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Integer division operator (allows operations on generic numbers like r := a div b)
             @param(a First generic number to divide)
             @param(b Second generic number to divide)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be divided. E.g. trying to divide an
                             integer with a cardinal may cause a compiler error
            }
            class operator IntDivide(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Inversion operator (allows operations on generic numbers like r := -a)
             @param(a Number to invert)
             @returns(Generic number containing the result)
            }
            class operator Negative(a: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Increment operator (allows operations on generic numbers like Inc(a))
             @param(a Generic number to increment)
             @returns(Generic number containing the result)
            }
            class operator Inc(a: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Decrement operator (allows operations on generic numbers like Dec(a))
             @param(a Generic number to decrement)
             @returns(Generic number containing the result)
            }
            class operator Dec(a: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Left shift operator (allows operations on generic numbers like r := a shl b)
             @param(a Generic number on which shifting will be applied)
             @param(b Shift value)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be processed. E.g. trying to apply a
                             shifting on integer with a cardinal may cause a compiler error
            }
            class operator LeftShift(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Right shift operator (allows operations on generic numbers like r := a shr b)
             @param(a Generic number on which shifting will be applied)
             @param(b Shift value)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be processed. E.g. trying to apply a
                             shifting on integer with a cardinal may cause a compiler error
            }
            class operator RightShift(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Bitwise xor operator (allows operations on generic numbers like r := a xor b)
             @param(a Generic number on which xor will be applied)
             @param(b Generic number containing xor to apply)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be processed. E.g. trying to apply a
                             xor on integer with a cardinal may cause a compiler error
            }
            class operator BitwiseXor(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Modulus operator (allows operations on generic numbers like r := a mod b)
             @param(a Generic number on which modulus will be applied)
             @param(b Modulus to apply)
             @returns(Generic number containing the result)
             @br @bold(NOTE) Only numbers of the same type may be processed. E.g. trying to apply a
                             modulus on integer with a cardinal may cause a compiler error
            }
            class operator Modulus(a, b: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Trunc operator (allows operations on generic numbers like r := Trunc(a))
             @param(a Generic number to trunc)
             @returns(Generic number containing the result)
            }
            class operator Trunc(a: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Round operator (allows operations on generic numbers like r := Trunc(a))
             @param(a Generic number to round)
             @returns(Generic number containing the result)
            }
            class operator Round(a: TWGenericNumber<T>): TWGenericNumber<T>;

            {**
             Equality operator (allows operations on generic numbers like a = b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if numbers are equal, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator Equal(a, b: TWGenericNumber<T>): Boolean;

            {**
             Not equality operator (allows operations on generic numbers like a <> b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if numbers are not equal, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator NotEqual(a, b: TWGenericNumber<T>): Boolean;

            {**
             Greater than operator (allows operations on generic numbers like a > b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if number a is greater than number b, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator GreaterThan(a, b: TWGenericNumber<T>): Boolean;

            {**
             Greater than or equal operator (allows operations on generic numbers like a >= b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if number a is greater than or equal to number b, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator GreaterThanOrEqual(a, b: TWGenericNumber<T>): Boolean;

            {**
             Less than operator (allows operations on generic numbers like a < b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if number a is less than number b, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator LessThan(a, b: TWGenericNumber<T>): Boolean;

            {**
             Less than or equal operator (allows operations on generic numbers like a <= b)
             @param(a First generic number to compare)
             @param(b Second generic number to compare)
             @returns(@true if number a is less than or equal to number b, otherwise @false)
             @br @bold(NOTE) Only numbers of the same type may be compared. E.g. trying to compare
                             an integer with a cardinal may cause a compiler error
            }
            class operator LessThanOrEqual(a, b: TWGenericNumber<T>): Boolean;

            {**
             Implicit conversion from Integer operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Integer): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from Cardinal operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Cardinal): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from Single operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Single): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from Double operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Double): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from Extended operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Extended): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from Int64 operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: Int64): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from UInt64 operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            class operator Implicit(value: UInt64): TWGenericNumber<T>; overload;

            {**
             Implicit conversion from NativeInt operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            {$if CompilerVersion <= 35}
                class operator Implicit(value: NativeInt): TWGenericNumber<T>; overload;
            {$ifend}

            {**
             Implicit conversion from NativeUInt operator (allows operations on generic numbers like r := value)
             @param(value Value to convert)
             @returns(Generic number assigned to value)
            }
            {$if CompilerVersion <= 35}
                class operator Implicit(value: NativeUInt): TWGenericNumber<T>; overload;
            {$ifend}

            // NOTE don't defined the Explicit operators, in reference to this thread:
            // http://stackoverflow.com/questions/9546433/overload-operator-and-conversion-type
            // @David Heffernan: If you are supplying an implicit cast operator then there's no need
            //                   to supply an explicit cast operator also. An explicit cast will
            //                   result in the implicit operator being called.

            {**
             Get the kind of value
             @returns(Kind of value)
            }
            function Kind: TTypeKind;

        public
            property Value: T read m_Value write m_Value;
    end;

implementation
//---------------------------------------------------------------------------
// TWGenericNumber
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Add(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the addition
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 + right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 + right.AsInt64;

        tkFloat:
            resVal := left.AsExtended + right.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set added value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Subtract(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the subtraction
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 - right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 - right.AsInt64;

        tkFloat:
            resVal := left.AsExtended - right.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set subtracted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Multiply(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the multiplication
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 * right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 * right.AsInt64;

        tkFloat:
            resVal := left.AsExtended * right.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set multiplied value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Divide(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the division
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 div right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 div right.AsInt64;

        tkFloat:
            resVal := left.AsExtended / right.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set divided value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.IntDivide(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the division
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 div right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 div right.AsInt64;

        tkFloat:
            resVal := left.AsExtended / right.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set divided value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Negative(a: TWGenericNumber<T>): TWGenericNumber<T>;
var
    val, resVal: TValue;
begin
    // convert value to generic
    val := TValue.From<T>(a.Value);

    // search for type, and process the inversion
    case (val.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((val.TypeInfo.Name = 'Cardinal') or (val.TypeInfo.Name = 'NativeUInt')) then
                    resVal := -val.AsUInt64
                else
            {$ifend}
                resVal := -val.AsInt64;

        tkFloat:
            resVal := -val.AsExtended;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(val.Kind)]);
    end;

    // set incremented value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Inc(a: TWGenericNumber<T>): TWGenericNumber<T>;
var
    val, resVal: TValue;
begin
    // convert value to generic
    val := TValue.From<T>(a.Value);

    // search for type, and process the addition
    case (val.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((val.TypeInfo.Name = 'Cardinal') or (val.TypeInfo.Name = 'NativeUInt')) then
                    resVal := val.AsUInt64 + 1
                else
            {$ifend}
                resVal := val.AsInt64 + 1;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(val.Kind)]);
    end;

    // set incremented value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Dec(a: TWGenericNumber<T>): TWGenericNumber<T>;
var
    val, resVal: TValue;
begin
    // convert value to generic
    val := TValue.From<T>(a.Value);

    // search for type, and process the subtraction
    case (val.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((val.TypeInfo.Name = 'Cardinal') or (val.TypeInfo.Name = 'NativeUInt')) then
                    resVal := val.AsUInt64 - 1
                else
            {$ifend}
                resVal := val.AsInt64 - 1;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(val.Kind)]);
    end;

    // set decremented value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.LeftShift(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the shifting
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 shl right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 shl right.AsInt64;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set shifted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.RightShift(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the shifting
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 shr right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 shr right.AsInt64;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set shifted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.BitwiseXor(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the xor
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 xor right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 xor right.AsInt64;
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set xored value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Modulus(a, b: TWGenericNumber<T>): TWGenericNumber<T>;
var
    left, right, resVal: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the modulus
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    resVal := left.AsUInt64 mod right.AsUInt64
                else
            {$ifend}
                resVal := left.AsInt64 mod right.AsInt64;

        tkFloat:
            resVal := TWMathHelper.ExtMod(left.AsExtended, right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;

    // set modulated value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Trunc(a: TWGenericNumber<T>): TWGenericNumber<T>;
var
    val, resVal: TValue;
begin
    // convert value to generic
    val := TValue.From<T>(a.Value);

    // search for type, and process the truncation
    case (val.Kind) of
        tkInteger,
        tkInt64:   Exit(a);
        tkFloat:   resVal := Trunc(val.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(val.Kind)]);
    end;

    // set truncated value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Round(a: TWGenericNumber<T>): TWGenericNumber<T>;
var
    val, resVal: TValue;
begin
    // convert value to generic
    val := TValue.From<T>(a.Value);

    // search for type, and process the round
    case (val.Kind) of
        tkInteger,
        tkInt64:   Exit(a);
        tkFloat:   resVal := Round(val.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(val.Kind)]);
    end;

    // set rounded value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Equal(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 = right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 = right.AsInt64);

        tkFloat:
            Result := (left.AsExtended = right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.NotEqual(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 <> right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 <> right.AsInt64);

        tkFloat:
            Result := (left.AsExtended <> right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.GreaterThan(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 > right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 > right.AsInt64);

        tkFloat:
            Result := (left.AsExtended > right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.GreaterThanOrEqual(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 >= right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 >= right.AsInt64);

        tkFloat:
            Result := (left.AsExtended >= right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.LessThan(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 < right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 < right.AsInt64);

        tkFloat:
            Result := (left.AsExtended < right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.LessThanOrEqual(a, b: TWGenericNumber<T>): Boolean;
var
    left, right: TValue;
begin
    // convert left and right value to generic values
    left  := TValue.From<T>(a.Value);
    right := TValue.From<T>(b.Value);

    // normally this should always be true, as a and b are of the same type
    Assert(left.Kind = right.Kind);

    // search for left and right types, and process the comparison
    case (left.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((left.TypeInfo.Name = 'Cardinal') or (left.TypeInfo.Name = 'NativeUInt')) then
                    Result := (left.AsUInt64 <= right.AsUInt64)
                else
            {$ifend}
                Result := (left.AsInt64 <= right.AsInt64);

        tkFloat:
            Result := (left.AsExtended <= right.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(left.Kind)]);
    end;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Integer): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Integer>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Cardinal): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Cardinal>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Single): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Single>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Double): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Double>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Extended): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Extended>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: Int64): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<Int64>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
class operator TWGenericNumber<T>.Implicit(value: UInt64): TWGenericNumber<T>;
var
    resVal: TValue;
begin
    // convert value to generic values
    resVal := TValue.From<UInt64>(value);

    // set converted value to result
    Result.Value := resVal.AsType<T>;
end;
//---------------------------------------------------------------------------
{$if CompilerVersion <= 35}
    class operator TWGenericNumber<T>.Implicit(value: NativeInt): TWGenericNumber<T>;
    var
        resVal: TValue;
    begin
        // convert value to generic values
        resVal := TValue.From<NativeInt>(value);

        // set converted value to result
        Result.Value := resVal.AsType<T>;
    end;
{$ifend}
//---------------------------------------------------------------------------
{$if CompilerVersion <= 35}
    class operator TWGenericNumber<T>.Implicit(value: NativeUInt): TWGenericNumber<T>;
    var
        resVal: TValue;
    begin
        // convert value to generic values
        resVal := TValue.From<NativeUInt>(value);

        // set converted value to result
        Result.Value := resVal.AsType<T>;
    end;
{$ifend}
//---------------------------------------------------------------------------
function TWGenericNumber<T>.Kind: TTypeKind;
var
    resVal: TValue;
begin
    // convert value to generic value and get kind
    resVal := TValue.From<T>(m_Value);
    Result := resVal.Kind;
end;
//---------------------------------------------------------------------------

end.
