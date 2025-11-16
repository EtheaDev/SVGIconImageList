{**
 @abstract(@name provides a generic class to parse the Scalable Vector Graphics (SVG) attributes. A
           SVG attribute may ontain one value (e.g. to='30') or a value list (e.g. to='360 20 20'))
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGAttribute;

interface

uses System.TypInfo,
     System.RTTI,
     System.SysUtils,
     UTWMajorSettings,
     UTWHelpers,
     UTWSVGCommon,
     UTWSVGTags,
     UTWSVGItems;

type
    {**
     Scalable Vector Graphics (SVG) attribute, can contain one value (e.g. to="30") or a value list
     (e.g. to="360 20 20")
    }
    TWSVGAttribute<T> = class(TWSVGProperty)
        public type
            IValues = array of T;

        private
            m_Values:           IValues;
            m_MatrixSeparator:  WideChar;
            m_GroupCount:       NativeUInt;
            m_ValPerGroupCount: NativeUInt;

            {**
             Check if value list is a matrix animation value list
             @returns(@true if value list is a matrix animation value list, otherwise @false)
            }
            function IsMatrixAnimMode: Boolean;

            {**
             Count the number of groups and the number of values contained in a group
             @param(data Data to parse)
            }
            procedure CountGroupsAndValues(const data: UnicodeString);

        protected
            {**
             Get value at index
             @param(index Value index)
             @returns(Value)
             @raises(Exception if index is out of bounds)
            }
            function GetValue(index: Integer): T; virtual;

            {**
             Get value count
             @returns(Value count)
            }
            function GetValueCount: Integer; virtual;

        public
            {**
             Constructor
             @param(pParent Parent item, orphan or root if @nil)
             @param(pOptions SVG options)
            }
            constructor Create(pParent: TWSVGItem; pOptions: PWSVGOptions); override;

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

        public
            {**
             Get the value at index. Example: aValue := Values[0];
             @br @bold(NOTE) An exception will be raised if index is out of bounds
            }
            property Values[index: Integer]: T read GetValue;

            {**
             Get the values count
            }
            property Count: Integer read GetValueCount;

            {**
             Get the group count
            }
            property GroupCount: NativeUInt read m_GroupCount;

            {**
             Get the values per groups count
            }
            property ValuePerGroupCount: NativeUInt read m_ValPerGroupCount;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWSVGAttribute<T>.Create(pParent: TWSVGItem; pOptions: PWSVGOptions);
begin
    inherited Create(pParent, pOptions);

    m_MatrixSeparator  := ' ';
    m_GroupCount       := 0;
    m_ValPerGroupCount := 0;
end;
//---------------------------------------------------------------------------
destructor TWSVGAttribute<T>.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.IsMatrixAnimMode: Boolean;
begin
    if (ItemName <> C_SVG_Animation_Values) then
        Exit(False);

    Result := (Length(m_Values) = 6);
end;
//---------------------------------------------------------------------------
procedure TWSVGAttribute<T>.CountGroupsAndValues;
var
    i: NativeUInt;
begin
    m_GroupCount := 1;

    // ';' separators are normally used to delimit groups, so count them (may be important later
    // for some animations)
    for i := 1 to Length(data) do
        if (data[i] = ';') then
            Inc(m_GroupCount);

    // certify that the last separator isn't a ';', otherwise this may bias the group count
    for i := Length(data) downto 1 do
    begin
        if ((data[i] >= '0') and (data[i] <= '9')) then
            break;

        if (data[i] = ';') then
            Dec(m_GroupCount);
    end;

    // calculate the number of values per groups
    if (m_GroupCount > 1) then
        m_ValPerGroupCount := NativeUInt(Length(m_Values)) div m_GroupCount
    else
        m_ValPerGroupCount := Length(m_Values);
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.GetValue(index: Integer): T;
begin
    if (index >= Length(m_Values)) then
        raise Exception.CreateFmt('Index is out of bounds - %d', [index]);

    Result := m_Values[index];
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.GetValueCount: Integer;
begin
    Result := Length(m_Values);
end;
//---------------------------------------------------------------------------
procedure TWSVGAttribute<T>.Assign(const pOther: TWSVGItem);
var
    pSource: TWSVGAttribute<T>;
    value:   T;
    index:   NativeInt;
begin
    inherited Assign(pOther);

    // invalid item?
    if (not(pOther is TWSVGAttribute<T>)) then
    begin
        Clear;
        Exit;
    end;

    // get source object
    pSource := pOther as TWSVGAttribute<T>;

    // copy the values
    m_GroupCount       := pSource.m_GroupCount;
    m_ValPerGroupCount := pSource.m_ValPerGroupCount;

    // resize the value list
    SetLength(m_Values, Length(pSource.m_Values));

    index := 0;

    // iterate through value list and copy each value
    for value in pSource.m_Values do
    begin
        m_Values[index] := value;
        Inc(index);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGAttribute<T>.Clear;
begin
    inherited Clear;

    // clear the values
    m_GroupCount       := 0;
    m_ValPerGroupCount := 0;

    SetLength(m_Values, 0);
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.CreateInstance(pParent: TWSVGItem): TWSVGProperty;
begin
    Result := TWSVGAttribute<T>.Create(pParent, m_pOptions);
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.Parse(const data: UnicodeString): Boolean;
begin
    // no value? (none is sometimes used to indicate that a particular attribute is not used, e.g.
    // stroke-dasharray="none" means that stroke not use dash array at all)
    if (data = C_SVG_Value_None) then
        Exit(True);

    // get the used separator in case the attribute represents a matrix
    m_MatrixSeparator := TWSVGCommon.DetectSeparator(data, 1, Length(data));

    {$if CompilerVersion <= 24}
        Result := TWSVGCommon.ExtractValues<T>(data, TWSVGArray<T>(m_Values));
    {$else}
        Result := TWSVGCommon.ExtractValues<T>(data, m_Values);
    {$ifend}

    CountGroupsAndValues(data);
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.Parse_Unoptimized(const data: UnicodeString): Boolean;
var
    value:        UnicodeString;
    c:            WideChar;
    valToConvert: TValue;
    kind:         TTypeKind;
begin
    // no value? (none is sometimes used to indicate that a particular attribute is not used, e.g.
    // stroke-dasharray="none" means that stroke not use dash array at all)
    if (data = C_SVG_Value_None) then
        Exit(True);

    value := '';

    // iterate through data to read
    for c in data do
    begin
        // ignore line feed, carriage return and tabs
        if ((c = #10) or (c = #13) or (c = #09)) then
            continue;

        // is numeric value?
        if (((c >= '0') and (c <= '9')) or (c = '.') or (c = '-')) then
        begin
            // add char to value
            value := value + c;
            continue;
        end;

        // char isn't a separator? (can be a space, a semicolon or a comma)
        if ((c <> ' ') and (c <> ';') and (c <> ',')) then
            Exit(False);

        // found comma?
        if (c = ',') then
            m_MatrixSeparator := c;

        // is value empty? (can happen e.g. when a semicolon is followed by a space)
        if (Length(value) = 0) then
            continue;

        kind := PTypeInfo(TypeInfo(T))^.Kind;

        case (kind) of
            tkInteger, tkInt64: valToConvert := StrToInt(value);
            tkFloat:            valToConvert := StrToFloat(value, g_InternationalFormatSettings);
        else
            raise Exception.CreateFmt('Unsupported type - %d', [Integer(kind)]);
        end;

        // add newly read value to list
        SetLength(m_Values, Length(m_Values) + 1);
        m_Values[Length(m_Values) - 1] := valToConvert.AsType<T>;

        // clear previous value and read next
        value := '';
    end;

    // is value empty? (can happen e.g. when last char is a semicolon)
    if (Length(value) > 0) then
    begin
        kind := PTypeInfo(TypeInfo(T))^.Kind;

        case (kind) of
            tkInteger, tkInt64: valToConvert := StrToInt(value);
            tkFloat:            valToConvert := StrToFloat(value, g_InternationalFormatSettings);
        else
            raise Exception.CreateFmt('Unsupported type - %d', [Integer(kind)]);
        end;

        // add newly read value to list
        SetLength(m_Values, Length(m_Values) + 1);
        m_Values[Length(m_Values) - 1] := valToConvert.AsType<T>;
    end;

    CountGroupsAndValues(data);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGAttribute<T>.Log(margin: Cardinal);
var
    animMatrixMode: Boolean;
    count, i:       NativeInt;
    value:          TValue;
    str:            UnicodeString;
begin
    str := '';

    // is matrix animation list of values?
    animMatrixMode := IsMatrixAnimMode;
    count          := Length(m_Values);

    // iterate through value list and add each value to log string
    for i := 0 to count - 1 do
    begin
        // not the first value?
        if (i <> 0) then
            if (animMatrixMode and ((i mod 2) = 0)) then
                // add semicolon separator
                str := str + '; '
            else
            if (animMatrixMode) then
                // add comma separator or blank space in some situations
                str := str + m_MatrixSeparator
            else
                // add blank space
                str := str + ' ';

        // convert value to generic value
        value := TValue.From<T>(m_Values[i]);

        // search for value kind, convert to string and add to result
        case (value.Kind) of
            tkInteger,
            tkInt64:
                // search for signed or unsigned type
                {$if CompilerVersion > 24}
                    if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                        str := str + IntToStr(value.AsUInt64)
                    else
                {$ifend}
                    str := str + IntToStr(value.AsInt64);

            tkFloat:
                str := str + FloatToStr(value.AsExtended);
        else
            raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
        end;
    end;

    // log value list
    TWLogHelper.LogToCompiler(TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ' + str);
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.Print(margin: Cardinal): UnicodeString;
var
    animMatrixMode: Boolean;
    count, i:       NativeInt;
    value:          TValue;
begin
    Result := '';

    // is matrix animation list of values?
    animMatrixMode := IsMatrixAnimMode;

    // add name to string
    Result := TWStringHelper.FillStrRight(ItemName, margin, ' ') + ' - ';
    count  := Length(m_Values);

    // iterate through value list and add each value to result
    for i := 0 to count - 1 do
    begin
        // not the first value?
        if (i <> 0) then
            if (animMatrixMode and ((i mod 2) = 0)) then
                // add semicolon separator
                Result := Result + '; '
            else
            if (animMatrixMode) then
                // add comma separator or blank space in some situations
                Result := Result + m_MatrixSeparator
            else
                // add blank space
                Result := Result + ' ';

        // convert value to generic value
        value := TValue.From<T>(m_Values[i]);

        // search for value kind, convert to string and add to result
        case (value.Kind) of
            tkInteger,
            tkInt64:
                // search for signed or unsigned type
                {$if CompilerVersion > 24}
                    if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                        Result := Result + IntToStr(value.AsUInt64)
                    else
                {$ifend}
                    Result := Result + IntToStr(value.AsInt64);

            tkFloat:
                Result := Result + FloatToStr(value.AsExtended);
        else
            raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
        end;
    end;

    Result := Result + #13 + #10;
end;
//---------------------------------------------------------------------------
function TWSVGAttribute<T>.ToXml: UnicodeString;
var
    animMatrixMode: Boolean;
    count, i:       NativeInt;
    value:          TValue;
begin
    // is matrix animation list of values?
    animMatrixMode := IsMatrixAnimMode;

    // add name to string and open value area
    Result := ItemName + '=\"';
    count  := Length(m_Values);

    // iterate through value list and add each value to string
    for i := 0 to count - 1 do
    begin
        // not the first value?
        if (i <> 0) then
            if (animMatrixMode and ((i mod 2) = 0)) then
                // add semicolon separator
                Result := Result + '; '
            else
            if (animMatrixMode) then
                // add comma separator or blank space in some situations
                Result := Result + m_MatrixSeparator
            else
                // add blank space
                Result := Result + ' ';

        // convert value to generic value
        value := TValue.From<T>(m_Values[i]);

        // search for value kind, convert to string and add to result
        case (value.Kind) of
            tkInteger,
            tkInt64:
                // search for signed or unsigned type
                {$if CompilerVersion > 24}
                    if ((value.TypeInfo.Name = 'Cardinal') or (value.TypeInfo.Name = 'NativeUInt')) then
                        Result := Result + IntToStr(value.AsUInt64)
                    else
                {$ifend}
                    Result := Result + IntToStr(value.AsInt64);

            tkFloat:
                Result := Result + FloatToStr(value.AsExtended);
        else
            raise Exception.CreateFmt('Unsupported type - %d', [Integer(value.Kind)]);
        end;
    end;

    // close value area
    Result := Result + '\"';
end;
//---------------------------------------------------------------------------

end.
