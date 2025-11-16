{**
 @abstract(@name provides the platform independent generic types and pointers, to use when the type
           size should remain constant on any target platform, e.g. when a value should be read from
           a file)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWTypes;

interface

type
    TWInt8    = ShortInt;
    TWUInt8   = Byte;
    TWInt16   = SmallInt;
    TWUInt16  = Word;
    TWInt32   = LongInt;
    TWUInt32  = LongWord;
    TWInt64   = Int64;
    TWUInt64  = UInt64;
    TWFloat32 = Single;
    TWFloat64 = Double;

    PTWInt8    = ^TWInt8;
    PTWUInt8   = ^TWUInt8;
    PTWInt16   = ^TWInt16;
    PTWUInt16  = ^TWUInt16;
    PTWInt32   = ^TWInt32;
    PTWUInt32  = ^TWUInt32;
    PTWInt64   = ^TWInt64;
    PTWUInt64  = ^TWUInt64;
    PTWFloat32 = ^TWFloat32;
    PTWFloat64 = ^TWFloat64;

    // TBytes type isn't defined for XE2 and earlier, so defines it
    {$if CompilerVersion <= 24}
        TBytes = array of byte;
    {$ifend}

implementation

end.
