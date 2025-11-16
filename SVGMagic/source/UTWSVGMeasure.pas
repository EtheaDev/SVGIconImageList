{**
 @abstract(@name provides a generic class to read the Scalable Vector Graphics (SVG) value with unit,
                 as e.g. x="0px")
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGMeasure;

interface

uses System.TypInfo,
     System.RTTI,
     System.SysUtils,
     UTWMajorSettings,
     UTWGenericNumber,
     UTWHelpers,
     UTWSVGTags,
     UTWSVGItems;

type
    {**
     Scalable Vector Graphics (SVG) measure, it's a value with its unit, as e.g. x="0px"
    }

    {**
     Unit enumeration
     @value(IE_UN_Unknown Value for which unit is unknown)
     @value(IE_UN_None Value without unit)
     @value(IE_UN_CM Value expressed in centimeters)
     @value(IE_UN_FT Value expressed in feet)
     @value(IE_UN_IN Value expressed in inches)
     @value(IE_UN_M Value expressed in meters)
     @value(IE_UN_MM Value expressed in millimeters)
     @value(IE_UN_PC Value expressed in picas)
     @value(IE_UN_PT Value expressed in points)
     @value(IE_UN_PX Value expressed in pixels)
     @value(IE_UN_Percent Value expressed in percent)
    }
    IEUnit =
    (
        IE_UN_Unknown,
        IE_UN_None,
        IE_UN_CM,
        IE_UN_FT,
        IE_UN_IN,
        IE_UN_M,
        IE_UN_MM,
        IE_UN_PC,
        IE_UN_PT,
        IE_UN_PX,
        IE_UN_Percent
    );

    TWSVGMeasure<T> = class(TWSVGProperty)
        private
            m_Unit:          IEUnit;
            m_Value:         TWGenericNumber<T>;
            m_InkscapeStyle: Boolean;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
             @param(inkscapeStyle If @true, property is an inkscape style)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions;
                    inkscapeStyle: Boolean); reintroduce; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) content from another item
             @param(pOther Other item to copy from)
            }
            procedure Assign(const pOther: TWSVGItem); override;

            {**
             Clear
            }
            procedure Clear; override;

            {**
             Create new property instance
             @param(pParent Parent item, orphan or root if @nil)
             @returns(Property instance)
            }
            function CreateInstance(pParent: TWSVGItem): TWSVGProperty; override;

            {**
             Parse data
             @param(data Data to parse)
             @returns(@true on success, otherwise @false)
            }
            function Parse(const data: UnicodeString): Boolean; override;

            {**
             Parse data
             @param(data Data to parse)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) In this overload, the code clarity is favored on the execution speed.
                             The reason why this function was kept is that it can be used as strongly
                             tested alternative in case an unknown bug happen in the function used in
                             production
            }
            function Parse_Unoptimized(const data: UnicodeString): Boolean; virtual;

            {**
             Log content
             @param(margin Margin length in chars)
            }
            procedure Log(margin: Cardinal); override;

            {**
             Print content to string
             @param(margin Margin length in chars)
             @returns(Content)
            }
            function Print(margin: Cardinal): UnicodeString; override;

            {**
             Get xml formatted string
             @returns(String)
            }
            function ToXml: UnicodeString; override;

            {**
             Convert string to unit
             @param(unitStr Unit string to convert)
             @returns(Converted unit, IE_UN_Unknown if unknown)
            }
            class function StrToUnit(const unitStr: UnicodeString): IEUnit; static;

            {**
             Convert unit to string
             @param(measureUnit Unit)
             @param(defValue Default value to return if unit is unknown)
             @returns(Converted string, default value if unknown)
            }
            class function UnitToStr(measureUnit: IEUnit; const defValue: UnicodeString): UnicodeString; static;

        public
            {**
             Get or set the measure unit
            }
            property MeasureUnit: IEUnit read m_Unit  write m_Unit;

            {**
             Get or set the measure value
            }
            property Value: TWGenericNumber<T> read m_Value write m_Value;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGMeasure
//---------------------------------------------------------------------------
constructor TWSVGMeasure<T>.Create(pParent: TWSVGItem; pOptions: PWSVGOptions; inkscapeStyle: Boolean);
begin
    inherited Create(pParent, pOptions);

    m_Value         := Default(TWGenericNumber<T>);
    m_Unit          := IE_UN_Unknown;
    m_InkscapeStyle := inkscapeStyle;
end;
//---------------------------------------------------------------------------
destructor TWSVGMeasure<T>.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGMeasure<T>.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGMeasure<T>;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGMeasure<T>)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGMeasure<T>;

    // copy data from source
    m_Value         := pSource.m_Value;
    m_Unit          := pSource.m_Unit;
    m_InkscapeStyle := pSource.m_InkscapeStyle;
end;
//---------------------------------------------------------------------------
procedure TWSVGMeasure<T>.Clear;
begin
    inherited Clear;

    m_Value         := Default(TWGenericNumber<T>);
    m_Unit          := IE_UN_Unknown;
    m_InkscapeStyle := False;
end;
//---------------------------------------------------------------------------
function TWSVGMeasure<T>.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGMeasure<T>.Create(pParent, m_pOptions, m_InkscapeStyle);
end;
//---------------------------------------------------------------------------
function TWSVGMeasure<T>.Parse(const data: UnicodeString): Boolean;
var
    value:       Single;
    offset, len: NativeInt;
begin
    offset := 1;

    // convert value to float
    TWStringHelper.ReadFloat(data, offset, value);
    m_Value := value;

    len := Length(data);

    // although UnicodeString types are 1 based, the Substring() function is 0 based, so decrease
    // the offset of 1 to keep the correlation
    Dec(offset);

    // get the unit
    if (offset < len) then
        m_Unit := StrToUnit(TWStringHelper.Substr(data, offset, len - offset));

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGMeasure<T>.Parse_Unoptimized(const data: UnicodeString): Boolean;
var
    value, unitName: UnicodeString;
    c:               WideChar;
    readUnit:        Boolean;
    kind:            TTypeKind;
begin
    readUnit := False;

    // iterate through data to read
    for c in data do
    begin
        // is numeric value?
        if (TWStringHelper.IsNumeric(c, False)) then
        begin
            // unit is read?
            if (readUnit) then
                Exit(False);

            // add char to value
            value := value + c;
            continue;
        end;

        // begin to read unit
        readUnit := True;

        // add char to unit
        unitName := unitName + c;
    end;

    // is value empty?
    if (Length(value) = 0) then
        Exit(False);

    kind := PTypeInfo(TypeInfo(T))^.Kind;

    // get value
    case (kind) of
        tkInteger, tkInt64: m_Value := StrToInt(value);
        tkFloat:            m_Value := StrToFloat(value, g_InternationalFormatSettings);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(kind)]);
    end;

    // get unit
    m_Unit := StrToUnit(unitName);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGMeasure<T>.Log(margin: Cardinal);
var
    {$if CompilerVersion <= 24}
        unitType: Integer;
    {$ifend}
    value:        TValue;
    valStr:       UnicodeString;
begin
    value := TValue.From<T>(m_Value.Value);

    // search for value kind and convert to string
    case (value.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                    valStr := IntToStr(value.AsUInt64)
                else
            {$ifend}
                valStr := IntToStr(value.AsInt64);

        tkFloat:
            valStr := FloatToStr(value.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
    end;

    {$if CompilerVersion <= 24}
        // this code is stupid and useless, but required, because otherwise the hyper bugged RAD
        // Studio compiler will raise the very stupid, useless and unexpressive error:
        // [DCC Fatal Error] F2084 Internal Error: AV08B844DF-R0000000C-0
        // while UTWSVGStyle unit is compiled
        unitType := Integer(IE_UN_None);

        if (m_Unit = IEUnit(unitType)) then
    {$else}
        if (m_Unit = IE_UN_None) then
    {$ifend}
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + valStr)
    else
        TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + valStr
                + UnitToStr(m_Unit, '<Unknown>'));
end;
//---------------------------------------------------------------------------
function TWSVGMeasure<T>.Print(margin: Cardinal): UnicodeString;
var
    {$if CompilerVersion <= 24}
        unitType: Integer;
    {$ifend}
    value:        TValue;
    valStr:       UnicodeString;
begin
    value := TValue.From<T>(m_Value.Value);

    // search for value kind and convert to string
    case (value.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                    valStr := IntToStr(value.AsUInt64)
                else
            {$ifend}
                valStr := IntToStr(value.AsInt64);

        tkFloat:
            valStr := FloatToStr(value.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
    end;

    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + valStr;

    {$if CompilerVersion <= 24}
        // this code is stupid and useless, but required, because otherwise the hyper bugged RAD
        // Studio compiler will raise the very stupid, useless and unexpressive error:
        // [DCC Fatal Error] F2084 Internal Error: AV08B844DF-R0000000C-0
        // while UTWSVGStyle unit is compiled
        unitType := Integer(IE_UN_None);

        if (m_Unit <> IEUnit(unitType)) then
    {$else}
        if (m_Unit <> IE_UN_None) then
    {$ifend}
        Result := Result + UnitToStr(m_Unit, '<Unknown>');

    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGMeasure<T>.ToXml: UnicodeString;
var
    {$if CompilerVersion <= 24}
        unitType: Integer;
    {$ifend}
    value:        TValue;
    valStr:       UnicodeString;
begin
    // convert value to generic value
    value := TValue.From<T>(m_Value.Value);

    // search for value kind and convert to string
    case (value.Kind) of
        tkInteger,
        tkInt64:
            // search for signed or unsigned type
            {$if CompilerVersion > 24}
                if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                    valStr := IntToStr(value.AsUInt64)
                else
            {$ifend}
                valStr := IntToStr(value.AsInt64);

        tkFloat:
            valStr := FloatToStr(value.AsExtended);
    else
        raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
    end;

    Result := ItemName;

    {$if CompilerVersion <= 24}
        // this code is stupid and useless, but required, because otherwise the hyper bugged RAD
        // Studio compiler will raise the very stupid, useless and unexpressive error:
        // [DCC Fatal Error] F2084 Internal Error: AV08B844DF-R0000000C-0
        // while UTWSVGStyle unit is compiled
        unitType := Integer(IE_UN_None);
    {$ifend}

    // format string
    if (m_InkscapeStyle) then
        Result := Result + ':' + valStr
    else
    {$if CompilerVersion <= 24}
        if (m_Unit = IEUnit(unitType)) then
    {$else}
        if (m_Unit = IE_UN_None) then
    {$ifend}
        Result := Result + '=\"' + valStr + '\"'
    else
        Result := Result + '=\"' + valStr + UnitToStr(m_Unit, '') + '\"';
end;
//---------------------------------------------------------------------------
class function TWSVGMeasure<T>.StrToUnit(const unitStr: UnicodeString): IEUnit;
begin
    // search for matching unit
    if (Length(unitStr) = 0) then
        Exit(IE_UN_None)
    else
    if (unitStr = C_SVG_Value_CM) then
        Exit(IE_UN_CM)
    else
    if (unitStr = C_SVG_Value_FT) then
        Exit(IE_UN_FT)
    else
    if (unitStr = C_SVG_Value_IN) then
        Exit(IE_UN_IN)
    else
    if (unitStr = C_SVG_Value_M) then
        Exit(IE_UN_M)
    else
    if (unitStr = C_SVG_Value_MM) then
        Exit(IE_UN_MM)
    else
    if (unitStr = C_SVG_Value_PC) then
        Exit(IE_UN_PC)
    else
    if (unitStr = C_SVG_Value_PT) then
        Exit(IE_UN_PT)
    else
    if (unitStr = C_SVG_Value_PX) then
        Exit(IE_UN_PX)
    else
    if (unitStr = C_SVG_Value_Percent) then
        Exit(IE_UN_Percent)
    else
        Exit(IE_UN_Unknown);
end;
//---------------------------------------------------------------------------
class function TWSVGMeasure<T>.UnitToStr(measureUnit: IEUnit; const defValue: UnicodeString): UnicodeString;
begin
    // search for unit
    case (measureUnit) of
        IE_UN_None:    Exit('');
        IE_UN_CM:      Exit(C_SVG_Value_CM);
        IE_UN_FT:      Exit(C_SVG_Value_FT);
        IE_UN_IN:      Exit(C_SVG_Value_IN);
        IE_UN_M:       Exit(C_SVG_Value_M);
        IE_UN_MM:      Exit(C_SVG_Value_MM);
        IE_UN_PC:      Exit(C_SVG_Value_PC);
        IE_UN_PT:      Exit(C_SVG_Value_PT);
        IE_UN_PX:      Exit(C_SVG_Value_PX);
        IE_UN_Percent: Exit(C_SVG_Value_Percent);
    else
        Exit(defValue);
    end;
end;
//---------------------------------------------------------------------------

end.
