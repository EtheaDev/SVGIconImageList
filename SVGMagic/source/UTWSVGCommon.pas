{**
 @abstract(@name provides the common classes and functions to work with the SVG data.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGCommon;
{$I SVGMagic.inc}

interface

uses System.Rtti,
     System.Classes,
     System.SysUtils,
     System.Variants,
     {$ifdef USE_VERYSIMPLEXML}
         Xml.VerySimple,
     {$else}
         Xml.XMLIntf,
     {$endif}
     UTWMajorSettings,
     UTWHelpers,
     UTWSVGTags;

type
    {**
     Scalable Vector Graphics (SVG) value list, may contain points, matrix table, ...
    }
    TWSVGArray_Unoptimized = array of Single;
    TWSVGArray<T>          = array of T;

    {**
     Scalable Vector Graphics (SVG) common functions and algorithms
    }
    TWSVGCommon = class
        public
            const m_Separators: UnicodeString = ' ,;:'#13#10#09;

        public type
            {**
             Value type enumeration
             @value(IE_VT_Unknown Unknown value type)
             @value(IE_VT_Value A value or list that contains real numbers)
             @value(IE_VT_Color A value or list that contains colors)
             @value(IE_VT_Rect A value or list that contains rectangles (x, y, width, height))
             @value(IE_VT_Matrix A value or list that contains matrices)
             @value(IE_VT_Text A value or list that contains texts)
             @value(IE_VT_Version A value or list that contains version numbers)
             @value(IE_VT_Enum A value or list based on an enumerator (the exact type depends of the context))
             @value(IE_VT_Mixed A value or list that contains several type of values (e.g. the enable-background property))
            }
            IEValueType =
            (
                IE_VT_Unknown,
                IE_VT_Value,
                IE_VT_Color,
                IE_VT_Rect,
                IE_VT_Matrix,
                IE_VT_Text,
                IE_VT_Version,
                IE_VT_Enum,
                IE_VT_Mixed
            );

            IValues = TStringList;

        public
            {**
             Get the value type associated with a property
             @param(name Property name)
             @return(Value type)
            }
            class function GetType(name: UnicodeString): IEValueType; static;

            {**
             Detect the separator used in a value list
             @param(str String containing the values for which the separator should be detected)
             @param(start Start pos in string from which the search will begin, in chars)
             @param(len Search length in chars)
             @returns(detected separator, '\0' if no separator)
            }
            class function DetectSeparator(const str: UnicodeString; start, len: NativeInt): WideChar; static;

            {**
             Prepare a string to be parsed (i.e. trim it and remove all CRLF and Tab chars)
             @param(str String to prepare)
             @returns(Prepared string)
            }
            class function PrepareStr(const str: UnicodeString): UnicodeString; static;

            {**
             Extract values from a value list (e.g. keyTimes="0;0.33;0.66;1" or values="none;inline;none;none")
             @param(data Data containing values to extract)
             @param(start Start position where data will begin to be read in the string)
             @param(len Data length to read in the string)
             @param(pValues Extracted values)
             @param(doTrim If @true, extracted values will be trimmed)
             @returns(Number of extracted values)
            }
            class function ExtractValues(const data: UnicodeString; start, len: NativeInt;
                    pValues: IValues; doTrim: Boolean): NativeUInt; overload; static;

            {**
             Extract numeric values from a value list (e.g. keyTimes="0;0.33;0.66;1")
             @param(data Source SVG value list)
             @param(values @bold([out]) List of converted values)
             @returns(@true on success, otherwise @false)
            }
            class function ExtractValues<T>(const data: UnicodeString; var values: TWSVGArray<T>): Boolean; overload; static;

            {**
             Extract numeric values from a value list (e.g. keyTimes="0;0.33;0.66;1")
             @param(data Source SVG value list)
             @param(start Start position where data will begin to be read in the string)
             @param(len Data length to read in the string)
             @param(values @bold([out]) List of converted values)
             @returns(@true on success, otherwise @false)
            }
            class function ExtractValues<T>(const data: UnicodeString; start, len: NativeInt;
                    var values: TWSVGArray<T>): Boolean; overload; static;

            {**
             Extract values from a value list (e.g. keyTimes="0;0.33;0.66;1" or values="none;inline;none;none")
             @param(data Data containing values to extract)
             @param(pValues Extracted values)
             @param(doTrim If @true, extracted values will be trimmed)
             @returns(Number of extracted values)
             @br @bold(NOTE) In this overload, the code clarity is favored on the execution speed.
                             The reason why this function was kept is that it can be used as strongly
                             tested alternative in case an unknown bug happen in the function used in
                             production
            }
            class function ExtractValues_Unoptimized(const data: UnicodeString; pValues: IValues;
                    doTrim: Boolean): NativeUInt; overload; static;

            {**
             Extract numeric values from a value list (e.g. keyTimes="0;0.33;0.66;1")
             @param(data Source SVG value list)
             @param(separator Separator used in the current SVG)
             @param(values @bold([out]) List of converted values)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) In this overload, the code clarity is favored on the execution speed.
                             The reason why this function was kept is that it can be used as strongly
                             tested alternative in case an unknown bug happen in the function used in
                             production
            }
            class function ExtractValues_Unoptimized(const data: UnicodeString; const separator: WideChar;
                    var values: TWSVGArray_Unoptimized): Boolean; overload; static;

            {**
             Check if the last value of a list should be ignored because empty
             @param(data List data to check)
             @param(start Start position where data will begin to be read in the string)
             @param(len Data length to read in the string)
             @param(lastValuePos The last valid position in the string, on the last value end, in chars)
             @returns(@true if last value should be ignored, otherwise @false)
             @br @bold(NOTE) This function is required to check if the last item in a list is really
                             a value, because several list may be terminated with a semicolon, like
                             e.g:
                             values="inline;inline;none;inline;"
                             In this case, there is no more value after the semicolon, but the SVG
                             parser believes that an empty value exists after the semicolon, and
                             wrongly process it
            }
            class function DoIgnoreLastValue(const data: UnicodeString; start, len: NativeInt;
                    out lastValuePos: NativeInt): Boolean; static;

            {**
             Get a SVG attribute
             @param(pNode Xml node containing the attribute to get)
             @param(name Attribute name to get)
             @param(defVal Default value to return in case the SVG attribute doesn't exist)
             @returns(attribute content, default value if attribute doesnt'exist)
            }
            {$ifdef USE_VERYSIMPLEXML}
                class function GetAttribute(const pNode: TXMLNode; const name,
                        defVal: UnicodeString): UnicodeString; static;
            {$else}
                class function GetAttribute(const pNode: IXMLNode; const name,
                        defVal: UnicodeString): UnicodeString; static;
            {$endif}

            {**
             Get a SVG value
             @param(pNode Xml node containing the value to get)
             @returns(value, empty string if node contains no value or on error)
            }
            {$ifdef USE_VERYSIMPLEXML}
                class function GetValue(const pNode: TXMLNode): UnicodeString; static;
            {$else}
                class function GetValue(const pNode: IXMLNode): UnicodeString; static;
            {$endif}

        private
            {**
             Safe convert string to float
             @param(str String to convert)
             @returns(converted float value)
            }
            class function SafeStrToFloat(const str: UnicodeString): Single; static;
    end;

implementation
//---------------------------------------------------------------------------
class function TWSVGCommon.GetType(name: UnicodeString): IEValueType;
begin
    // deduct the value type from the property name
    if (name = C_SVG_Prop_ID) then
        Exit (IE_VT_Text)
    else
    if (name = C_SVG_Prop_Version) then
        Exit (IE_VT_Version)
    else
    if (name = C_SVG_Prop_ViewBox) then
        Exit (IE_VT_Rect)
    else
    if (name = C_SVG_Prop_Enable_Background) then
        Exit (IE_VT_Mixed)
    else
    if (name = C_SVG_Prop_Page_Color) then
        Exit (IE_VT_Color)
    else
    if (name = C_SVG_Prop_Border_Color) then
        Exit (IE_VT_Color)
    else
    if (name = C_SVG_Prop_Border_Opacity) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Fill) then
        Exit (IE_VT_Color)
    else
    if (name = C_SVG_Prop_Stroke) then
        Exit (IE_VT_Color)
    else
    if (name = C_SVG_Prop_Stroke_Width) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Stroke_DashArray) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Stroke_DashOffset) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Stroke_LineCap) then
        Exit (IE_VT_Enum)
    else
    if (name = C_SVG_Prop_Stroke_LineJoin) then
        Exit (IE_VT_Enum)
    else
    if (name = C_SVG_Prop_Stroke_MiterLimit) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Opacity) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Fill_Rule) then
        Exit (IE_VT_Enum)
    else
    if (name = C_SVG_Prop_Fill_Opacity) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Stroke_Opacity) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Points) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Path) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_X) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Y) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_X1) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Y1) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_X2) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Y2) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_CX) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_CY) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Width) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Height) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_R) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_RX) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_RY) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_FX) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_FY) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Font_Family) then
        Exit (IE_VT_Text)
    else
    if (name = C_SVG_Prop_Font_Size) then
        Exit (IE_VT_Value)
    else
    if (name = C_SVG_Prop_Transform) then
        Exit (IE_VT_Matrix)
    else
    if (name = C_SVG_Prop_XLink_HRef) then
        Exit (IE_VT_Text)
    else
    if (name = C_SVG_Prop_HRef) then
        Exit (IE_VT_Text)
    else
    if (name = C_SVG_Prop_Display) then
        Exit (IE_VT_Enum)
    else
    if (name = C_SVG_Prop_Visibility) then
        Exit (IE_VT_Enum)
    else
    if (name = C_SVG_Prop_PreserveAspectRatio) then
        Exit(IE_VT_Enum);

    Exit (IE_VT_Unknown)
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.DetectSeparator(const str: UnicodeString; start, len: NativeInt): WideChar;
var
    i, count: NativeInt;
    trimming: Boolean;
begin
    count := (start + len) - 1;

    Assert(count <= Length(str));

    i        := start;
    trimming := True;
    Result   := #0;

    // iterate through chars
    while (i <= count) do
        case (str[i]) of
            ' ':
            begin
                // skip all following spaces, if any
                while ((i <= count) and (str[i] = ' ')) do
                    Inc(i);

                // next char is a valid separator?
                case (str[i]) of
                    ',',
                    ';': Exit(str[i]);

                    ':':
                        // colons are special separators, they generally separates a name from a value
                        // or value list. If the colon is the only separator of the string, then it
                        // should be returned, otherwise any other separator will override it
                        Result := str[i];
                else
                    if (not trimming and (i <= count)) then
                        // the following char isn't a space, and the space isn't part of a begin or end
                        // trimming, so the space itself is a separator
                        Exit(' ');
                end;

                Inc(i);
            end;

            ':':
            begin
                trimming := False;
                result   := str[i];
                Inc(i);
            end;

            ',',
            ';':
                Exit(str[i]);
        else
            trimming := False;
            Inc(i);
        end;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.PrepareStr(const str: UnicodeString): UnicodeString;
var
    index, npos, firstValidChar, lastValidChar, spacesCount, spacesCountAfterCommit: NativeUInt;
    c:                                                                               WideChar;
    nextStr:                                                                         UnicodeString;
begin
    // simulate the c++ npos value to respect the original algorithm written by Niki
    npos := Length(str) + 1;

    // don't forget: strings are 1 based in Delphi
    index                  := 0;
    firstValidChar         := npos;
    lastValidChar          := npos;
    spacesCount            := 0;
    spacesCountAfterCommit := 0;

    // iterate through source string, remove all tab and CRLF chars, and left trim source
    for c in str do
    begin
        case (c) of
            // skip new line and tabs
            #09,
            #10,
            #13:
            begin
                // copy accumulated string
                if (firstValidChar <> npos) then
                begin
                    // get the accumuated string
                    nextStr := TWStringHelper.Substr(str, firstValidChar, index - firstValidChar);

                    // if last digit of the dest string and first digit of the following substring
                    // are both a number, an extra space should be added to separate the both numbers,
                    // otherwise the numbers will be wrongly unified as if they were a single number
                    if ((Length(Result) > 0) and (Length(nextStr) > 0)
                            and TWStringHelper.IsNumeric(Result[Length(Result)], True)
                            and TWStringHelper.IsNumeric(nextStr[1], True))
                    then
                        Result := Result + ' ';

                    Result                 := Result + nextStr;
                    firstValidChar         := npos;
                    spacesCountAfterCommit := 0;
                end;
            end;

            ' ':
            begin
                // skip spaces in front of string (left trim)
                if ((Length(Result) = 0) and (firstValidChar >= lastValidChar)) then
                begin
                    Inc(index);
                    continue;
                end;

                // space as valid char in the middle string
                if (firstValidChar = npos) then
                    firstValidChar := index;

                // count spaces
                Inc(spacesCount);
                Inc(spacesCountAfterCommit);
            end;
        else
            // if first char, mark as such
            if (firstValidChar = npos) then
                firstValidChar := index;

            lastValidChar := index;

            // reset spaces count (to avoid right trimming them)
            spacesCount            := 0;
            spacesCountAfterCommit := 0;
        end;

        Inc(index);
    end;

    // commit last accumulated part
    if ((firstValidChar < npos) and (lastValidChar < npos) and (firstValidChar <= lastValidChar)) then
    begin
        // get the accumuated string
        nextStr := TWStringHelper.Substr(str, firstValidChar, (lastValidChar + 1) - firstValidChar);

        // if last digit of the dest string and first digit of the following substring
        // are both a number, an extra space should be added to separate the both numbers,
        // otherwise the numbers will be wrongly unified as if they were a single number
        if ((Length(Result) > 0) and (Length(nextStr) > 0)
                and TWStringHelper.IsNumeric(Result[Length(Result)], True)
                and TWStringHelper.IsNumeric(nextStr[1], True))
        then
            Result := Result + ' ';

        Result := Result + nextStr;
    end
    else
    // anything to trim right?
    if (spacesCount > 0) then
    begin
        // remove spaces after commit which are not in the final string
        Dec(spacesCount, spacesCountAfterCommit);

        // right trim?
        if (spacesCount > 0) then
            SetLength(Result, Length(Result) - NativeInt(spacesCount));
    end;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.ExtractValues(const data: UnicodeString; start, len: NativeInt;
        pValues: IValues; doTrim: Boolean): NativeUInt;
var
    trimStart, trimOffset, offset, lastValuePos, i, j: NativeInt;
    ignoreLastValue:                                   Boolean;
begin
    // check if last value should be ignored, keep the last value position if yes
    ignoreLastValue := DoIgnoreLastValue(data, start, len, lastValuePos);

    offset := start;

    // iterate through data
    for i := 0 to len - 1 do
        case (data[offset]) of
            ';':
            begin
                // do ignore the last value, and the last value is reached?
                if (ignoreLastValue and (start >= lastValuePos)) then
                    Exit(pValues.Count);

                // do trim the values?
                if (doTrim) then
                begin
                    trimStart := start;

                    // left trim the string
                    for j := 1 to offset - start do
                        case (data[j]) of
                            ' ',
                            #09,
                            #10,
                            #13: Inc(trimStart);
                        else
                            break;
                        end;

                    // no value exists after trimming has been applied?
                    if (trimStart = offset) then
                    begin
                        TWLogHelper.LogToCompiler('Extract values - FAILED - data contains incorrect values - data - '
                                + data + ' - start - ' + IntToStr(start) + ' - offset - ' + IntToStr(offset));
                        continue;
                    end;

                    trimOffset := offset;

                    // right trim the string
                    for j := offset - 1 downto trimStart do
                        case (data[j]) of
                            ' ',
                            #09,
                            #10,
                            #13: Dec(trimOffset);
                        else
                            break;
                        end;

                    // no value exists after trimming has been applied?
                    if (trimOffset = trimStart) then
                    begin
                        TWLogHelper.LogToCompiler('Extract values - FAILED - data contains incorrect values - data - '
                                + data + ' - start - ' + IntToStr(start) + ' - offset - ' + IntToStr(offset));
                        continue;
                    end;

                    // extract the value and add it to list. BE CAREFUL, the string in the Substr()
                    // function is ZERO BASED, whereas the entire UnicodeString is 1 based. I will
                    // not give my opinion about that!!!
                    pValues.Add(TWStringHelper.Substr(data, trimStart - 1, trimOffset - trimStart));
                end
                else
                    // extract the value and add it to list. BE CAREFUL, the string in the Substr()
                    // function is ZERO BASED, whereas the entire UnicodeString is 1 based. I will
                    // not give my opinion about that!!!
                    pValues.Add(TWStringHelper.Substr(data, start - 1, offset - start));

                // read the next value
                Inc(offset);
                start := offset;
                continue;
            end;
        else
            Inc(offset);
            continue;
        end;

    // extract last value, if any
    if (start < offset) then
        if (doTrim) then
        begin
            trimStart := start;

            // left trim the string
            for i := 1 to offset - start do
                case (data[i]) of
                    ' ',
                    #09,
                    #10,
                    #13: Inc(trimStart);
                else
                    break;
                end;

            // no value exists after trimming has been applied?
            if (trimStart = offset) then
            begin
                TWLogHelper.LogToCompiler('Extract values - FAILED - data contains incorrect values - data - '
                        + data + ' - start - ' + IntToStr(start) + ' - offset - ' + IntToStr(offset));
                Exit(pValues.Count);
            end;

            trimOffset := offset;

            // right trim the string
            for i := offset - 1 downto trimStart do
                case (data[i]) of
                    ' ',
                    #09,
                    #10,
                    #13: Dec(trimOffset);
                else
                    break;
                end;

            // no value exists after trimming has been applied?
            if (trimOffset = trimStart) then
            begin
                TWLogHelper.LogToCompiler('Extract values - FAILED - data contains incorrect values - data - '
                        + data + ' - start - ' + IntToStr(start) + ' - offset - ' + IntToStr(offset));
                Exit(pValues.Count);
            end;

            // extract the last value and add it to list. BE CAREFUL, the string in the Substr()
            // function is ZERO BASED, whereas the entire UnicodeString is 1 based. I will not give
            // my opinion about that!!!
            pValues.Add(TWStringHelper.Substr(data, trimStart - 1, trimOffset - trimStart));

        end
        else
            // extract the last value and add it to list. BE CAREFUL, the string in the Substr()
            // function is ZERO BASED, whereas the entire UnicodeString is 1 based. I will not give
            // my opinion about that!!!
            pValues.Add(TWStringHelper.Substr(data, start - 1, offset - start));

    Result := pValues.Count;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.ExtractValues<T>(const data: UnicodeString; var values: TWSVGArray<T>): Boolean;
begin
    Result := ExtractValues<T>(data, 1, Length(data), values);
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.ExtractValues<T>(const data: UnicodeString; start, len: NativeInt;
        var values: TWSVGArray<T>): Boolean;
var
    v:                      Single;
    resVal:                 TValue;
    i, prevI, index, count: NativeInt;
begin
    count := (start + len) - 1;

    Assert(count <= Length(data));

    i     := start;
    prevI := start;
    index := 0;

    SetLength(values, 0);

    // process every number
    while ((i <= count) and TWStringHelper.SkipChars(data, m_Separators, i)
            and TWStringHelper.ReadFloat(data, i, v))
    do
    begin
        // protection against infinite loops. Normally the loop should always progress
        if (i = prevI) then
            Exit(False);

        prevI := i;

        // convert read value to generic
        resVal := TValue.From<Single>(v);

        SetLength(values, index + 1);
        values[index] := resVal.AsType<T>;
        Inc(index);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.ExtractValues_Unoptimized(const data: UnicodeString; pValues: IValues;
        doTrim: Boolean): NativeUInt;
var
    lastValuePos, i: NativeInt;
begin
    if (not Assigned(pValues)) then
        raise Exception.Create('Resulting value list is not initialized');

    // split data in sub-values
    pValues.Clear;
    pValues.Delimiter       := ';';
    pValues.StrictDelimiter := True;
    pValues.DelimitedText   := data;

    // check if last value should be ignored
    if (DoIgnoreLastValue(data, 1, Length(data), lastValuePos)) then
        // erase the last value, as this value will always be empty
        pValues.Delete(pValues.Count - 1);

    // count values
    Result := pValues.Count;

    // do trim resulting values?
    if (not doTrim) then
        Exit;

    // iterate through each values and trim them
    for i := 0 to Result - 1 do
        pValues[i] := Trim(pValues[i]);
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.ExtractValues_Unoptimized(const data: UnicodeString; const separator: WideChar;
        var values: TWSVGArray_Unoptimized): Boolean;
var
    value:       UnicodeString;
    c:           WideChar;
    exponential: Boolean;
begin
    exponential := False;

    // iterate through data
    for c in data do
    begin
        // ignore line feed, carriage return and tabs
        if ((c = #10) or (c = #13) or (c = #09)) then
            continue;

        // check for separator char
        if ((c = separator) or (c = ' ')) then
        begin
            if (Length(value) = 0) then
                continue;

            // convert value and add to list
            SetLength(values, Length(values) + 1);
            values[Length(values) - 1] := SafeStrToFloat(value);

            exponential := False;

            // clear value
            value := '';
            continue;
        end;

        // found exponent?
        if ((c = 'e') or (c = 'E')) then
        begin
            exponential := True;
            value       := value + 'e';
            continue;
        end;

        // check for negation char as it has two meanings
        if (c = '-') then
        begin
            // current value starts with -
            if (Length(value) = 0) then
            begin
                value := value + '-';
                continue;
            end;

            // otherwise sign starts next value, so convert and save current value first
            if (exponential) then
                value := value + '-'
            else
            begin
                SetLength(values, Length(values) + 1);
                values[Length(values) - 1] := SafeStrToFloat(value);
                exponential                := False;

                // next value starts with -
                value := '-';
            end;

            continue;
        end;

        // check for dot char as it has two meanings
        if (c = '.') then
        begin
            // check if current value already has a dot
            if (System.Pos('.', value) = 0) then
            begin
                // dot is still part of present value
                value := value + '.';
                continue;
            end;

            // convert and save value. NOTE dot is only allowed once in value so if there is another
            // occurrence it means a new value is starting
            SetLength(values, Length(values) + 1);
            values[Length(values) - 1] := SafeStrToFloat(value);

            exponential := False;

            // next value starts with dot
            value := '.';
            continue;
        end;

        // check if char is valid
        if ((c < '0') or (c > '9')) then
        begin
            TWLogHelper.LogToCompiler('Parse SVG values - FAILED - invalid char - ' + c);
            Exit(False);
        end;

        // add current char to value
        value := value + c;
    end;

    // last value?
    if (Length(value) <> 0) then
    begin
        // convert value and add to list
        SetLength(values, Length(values) + 1);
        values[Length(values) - 1] := SafeStrToFloat(value);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.DoIgnoreLastValue(const data: UnicodeString; start, len: NativeInt;
        out lastValuePos: NativeInt): Boolean;
begin
    lastValuePos := (start + len) - 1;

    // reverse iterate string until last char different than blank space is found. In case this char
    // is a semicolon, then the last value does not exist and should be ignored
    while (lastValuePos >= start) do
        case (data[lastValuePos]) of
            ' ',
            #09,
            #10,
            #13: Dec(lastValuePos);
            ';': Exit(True);
        else
            break;
        end;

    Exit(False);
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    class function TWSVGCommon.GetAttribute(const pNode: TXMLNode; const name,
            defVal: UnicodeString): UnicodeString;
{$else}
    class function TWSVGCommon.GetAttribute(const pNode: IXMLNode; const name,
            defVal: UnicodeString): UnicodeString;
{$endif}
{$ifndef USE_VERYSIMPLEXML}
    var
        pAttribute: OleVariant;
{$endif}
begin
    // is node empty?
    if (not Assigned(pNode)) then
        exit(defVal);

    {$ifdef USE_VERYSIMPLEXML}
        if (not(pNode.HasAttribute(name))) then
            Exit(defVal);

        Result := pNode.Attributes[name];
    {$else}
        // get attribute from node
            pAttribute := pNode.Attributes[name];

        // found it?
        if (VarIsNull(pAttribute)) then
            exit(defVal);

        Result := pAttribute;
    {$endif}
end;
//---------------------------------------------------------------------------
{$ifdef USE_VERYSIMPLEXML}
    class function TWSVGCommon.GetValue(const pNode: TXMLNode): UnicodeString;
{$else}
    class function TWSVGCommon.GetValue(const pNode: IXMLNode): UnicodeString;
{$endif}
var
    pValue: OleVariant;
    i:      NativeInt;
begin
    // is node empty?
    if (not Assigned(pNode)) then
        Exit('');

    // node contains a value (i.e. a text inside the tag)?
    {$ifdef USE_VERYSIMPLEXML}
        if ((pNode.NodeType <> ntText) and (pNode.NodeType <> ntElement)) then
    {$else}
        if (pNode.NodeType <> ntText) then
    {$endif}
            Exit('');

    // get value from node
    pValue := pNode.NodeValue;

    // found it?
    if (VarIsNull(pValue) or (pValue = '')) then
    begin
        // sometimes texts formatted with a CDATA (e.g. <![CDATA[My text]]> may be contained in
        // children nodes
        for i := 0 to pNode.ChildNodes.Count - 1 do
        begin
            // get value from child node
            pValue := pNode.ChildNodes[i].NodeValue;

            // found it?
            if (not VarIsNull(pValue)) then
            begin
                // check if the variant type is a kind of string, get it if yes
                case (VarType(pValue) and VarTypeMask) of
                    varOleStr,
                    varString: Exit(UnicodeString(pValue));
                end;
            end;
        end;

        Exit('');
    end;

    Result := pValue;
end;
//---------------------------------------------------------------------------
class function TWSVGCommon.SafeStrToFloat(const str: UnicodeString): Single;
begin
    if (Length(str) = 0) then
        Exit(0.0);

    try
        Result := StrToFloat(str, g_InternationalFormatSettings);
    except
        on e: EConvertError do Exit(0.0);
    else
        raise Exception.CreateFmt('Unable to convert string to single - %s', [str]);
    end;
end;
//---------------------------------------------------------------------------

end.
