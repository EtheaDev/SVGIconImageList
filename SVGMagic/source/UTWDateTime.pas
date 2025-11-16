{**
 @abstract(@name provides the classes to manipulate date and time.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWDateTime;

interface

uses System.SysUtils,
     System.Math,
     UTWMajorSettings,
     UTWSmartPointer,
     UTWHelpers;

type
    {**
     Struct keeping time and containing methods for comparison and time arithmetics
     @author(Niki)
    }
    TWSimpleTime = class
        public type
            {**
             SMIL time unit enumeration
             @value(IE_SU_Unknown Unknown unit)
             @value(IE_SU_None No unit defined)
             @value(IE_SU_Hour Hours)
             @value(IE_SU_Min Minutes)
             @value(IE_SU_Sec Seconds)
             @value(IE_SU_MSec Milliseconds)
            }
            IESMILUnit =
            (
                IE_SU_Unknown,
                IE_SU_None,
                IE_SU_Hour,
                IE_SU_Min,
                IE_SU_Sec,
                IE_SU_MSec
            );

        private
            m_Hour: SmallInt;
            m_Min:  SmallInt;
            m_Sec:  SmallInt;
            m_MSec: SmallInt;

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
             @param(h Hour)
             @param(m Minutes)
             @param(s Seconds)
             @param(ms Milliseconds)
            }
            constructor Create(h, m, s: SmallInt; ms: SmallInt = 0); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Assign (i.e. copy) the content from another simple time
             @param(pOther Other simple time to copy from)
            }
            procedure Assign(const pOther: TWSimpleTime); virtual;

            {**
             Add the content of another simple time and assign the result to this simple time
             @param(pOther Other simple time to add to this simple time)
             @return(Added simple time)
            }
            function AddAndAssign(const pOther: TWSimpleTime): TWSimpleTime; virtual;

            {**
             Subtract a simple time and assign the result to this simple time
             @param(pOther Other simple time to subtract to this simple time)
             @return(Subtracted simple time)
            }
            function SubAndAssign(const pOther: TWSimpleTime): TWSimpleTime; overload; inline;

            {**
             Add seconds to this simple time
             @param(seconds Number of seconds to add to this simple time)
             @return(Added simple time)
            }
            function Add(seconds: Integer): TWSimpleTime; overload; inline;

            {**
             Add a simple time to this simple time
             @param(pOther Other simple time to add to this simple time)
             @return(Added simple time)
            }
            function Add(const pOther: TWSimpleTime): TWSimpleTime; overload; inline;

            {**
             Subtract seconds from this simple time
             @param(seconds Seconds to subtract from this simple time)
             @return(Subtracted simple time)
            }
            function Sub(seconds: Integer): TWSimpleTime; overload; inline;

            {**
             Subtract a simple time from this simple time
             @param(pOther Other simple time to subtract from this simple time)
             @return(Subtracted simple time)
            }
            function Sub(const pOther: TWSimpleTime): TWSimpleTime; overload; inline;

            {**
             Increments this simple time by one second
             @returns(Incremented simple time)
            }
            function Increment: TWSimpleTime; virtual;

            {**
             Decrements this simple time by one second
             @returns(Decremented simple time)
            }
            function Decrement: TWSimpleTime; virtual;

            {**
             Compare the content of 2 simple times and determine if they are equal
             @param(pOther Other simple time to compare with)
             @return(@true if simple times are equal, otherwise @false)
            }
            function IsEqual(const pOther: TWSimpleTime): Boolean; inline;

            {**
             Compares the content of 2 simple times and determines if they are different
             @param(pOther Other simple time to compare with)
             @return(@true if simple times differ, otherwise @false)
            }
            function Differs(const pOther: TWSimpleTime): Boolean; inline;

            {**
             Check if version is lower than another
             @param(pOther Other version to compare with)
             @returns(@true if version is lower than other, otherwise @false)
            }
            function IsLower(const pOther: TWSimpleTime): Boolean; virtual;

            {**
             Check if version is lower than or equals to another
             @param(pOther Other version to compare with)
             @returns(@true if version is lower than or equals to other, otherwise @false)
            }
            function IsLowerOrEqual(const pOther: TWSimpleTime): Boolean; virtual;

            {**
             Check if version is higher than another
             @param(pOther Other version to compare with)
             @returns(@true if version is higher than other, otherwise @false)
            }
            function IsHigher(const pOther: TWSimpleTime): Boolean; virtual;

            {**
             Check if version is higher than or equals to another
             @param(pOther Other version to compare with)
             @returns(@true if version is higher than or equals to other, otherwise @false)
            }
            function IsHigherOrEqual(const pOther: TWSimpleTime): Boolean; virtual;

            {**
             Clear time
            }
            procedure Clear; virtual;

            {**
             Check if time is empty
             @returns(@true if time is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

            {**
             Set time and fix it for over flows, returns overflow in days. Values can be nagtives, e.g.
             @unorderedList(
                     @item(23:13:60 becomes 23:14:00)
                     @item(25:60:121 becomes 02:02:01 and will return 1 (one extra day))
                 )
             @param(hour New hour)
             @param(min New minutes)
             @param(sec New seconds)
             @param(msec New milliseconds)
             @returns(Number days of over/under flows of hour (eg, 48:00:00 becomes 00:00:00 and returns 2))
            }
            function NormalizeAndSet(hour, min, sec: Integer; msec: Integer = 0): Integer; virtual;

            {**
             Get time as milliseconds
             @returns(Time in milliseconds)
             @author(JMR)
             @br @bold(NOTE) The max value that this function can return is 86400000 ms, so Cardinal
                             is enough
            }
            function ToMilliseconds: Cardinal; virtual;

            {**
             Convert time to string
             @returns(Time as string)
            }
            function ToStr: UnicodeString; virtual;

            {**
             Set time from SMIL standard formatted string (used e.g. in SVG files)
             @param(txt SMIL time string)
             @returns(@true on success, otherwise @false)
             @author(JMR)
             @br @bold(NOTE) SMIL (Synchronized Multimedia Integration Language) is a standard used
                             to describe multimedia presentations. It defines markup for timing,
                             layout, animations, visual transitions, and media embedding, among other
                             things. SMIL is used by the SVG standard, to animate one or many shape
                             contained inside. The SMIL time notation can be written as follow:
                             @unorderedList(
                                 @item(02:30:03    = 2 hours, 30 minutes and 3 seconds)
                                 @item(50:00:10.25 = 50 hours, 10 seconds and 250 milliseconds)
                                 @item(02:33       = 2 minutes and 33 seconds)
                                 @item(00:10.5     = 10.5 seconds = 10 seconds and 500 milliseconds)
                                 @item(3.2h        = 3.2 hours = 3 hours and 12 minutes)
                                 @item(45min       = 45 minutes)
                                 @item(30s         = 30 second)
                                 @item(5ms         = 5 milliseconds)
                                 @item(12.467      = 12 seconds and 467 milliseconds)
                             )
                             Fractional values are just (base 10) floating point definitions of
                             seconds. Thus:
                             @unorderedList(
                                 @item(00.5s       = 500 milliseconds)
                                 @item(00:00.005   = 5 milliseconds)
                             )
            }
            function FromSMIL(const txt: UnicodeString): Boolean; virtual;

            {**
             Get SMIL standard formatted string time (used e.g. in SVG files)
             @returns(SMIL standard formatted string)
             @author(JMR)
             @br @bold(NOTE) SMIL (Synchronized Multimedia Integration Language) is a standard used
                             to describe multimedia presentations. It defines markup for timing,
                             layout, animations, visual transitions, and media embedding, among other
                             things. SMIL is used by the SVG standard, to animate one or many shape
                             contained inside. The SMIL time notation can be written as follow:
                             @unorderedList(
                                 @item(02:30:03    = 2 hours, 30 minutes and 3 seconds)
                                 @item(50:00:10.25 = 50 hours, 10 seconds and 250 milliseconds)
                                 @item(02:33       = 2 minutes and 33 seconds)
                                 @item(00:10.5     = 10.5 seconds = 10 seconds and 500 milliseconds)
                                 @item(3.2h        = 3.2 hours = 3 hours and 12 minutes)
                                 @item(45min       = 45 minutes)
                                 @item(30s         = 30 second)
                                 @item(5ms         = 5 milliseconds)
                                 @item(12.467      = 12 seconds and 467 milliseconds)
                             )
                             Fractional values are just (base 10) floating point definitions of
                             seconds. Thus:
                             @unorderedList(
                                 @item(00.5s       = 500 milliseconds)
                                 @item(00:00.005   = 5 milliseconds)
                             )
            }
            function ToSMIL: UnicodeString; virtual;

            {**
             Convert unit as string to SMIL standard time unit
             @param(smilUnit Unit to convert)
             @returns(SMIL standard time unit)
            }
            class function UnitToSMIL(const smilUnit: UnicodeString): IESMILUnit; static;

            {**
             Convert SMIL standard time unit to unit as string
             @param(smilUnit SMIL standard time unit to convert)
             @returns(unit as string)
             @raises(Exception on unknown SMIL unit)
            }
            class function SMILToUnit(smilUnit: IESMILUnit): UnicodeString; static;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWSimpleTime.Create;
begin
    inherited Create;

    m_Hour := 0;
    m_Min  := 0;
    m_Sec  := 0;
    m_MSec := 0;
end;
//---------------------------------------------------------------------------
constructor TWSimpleTime.Create(h, m, s, ms: SmallInt);
begin
    inherited Create;

    m_Hour := h;
    m_Min  := m;
    m_Sec  := s;
    m_MSec := ms;
end;
//---------------------------------------------------------------------------
destructor TWSimpleTime.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSimpleTime.Assign(const pOther: TWSimpleTime);
begin
    m_Hour := pOther.m_Hour;
    m_Min  := pOther.m_Min;
    m_Sec  := pOther.m_Sec;
    m_MSec := pOther.m_MSec;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.AddAndAssign(const pOther: TWSimpleTime): TWSimpleTime;
begin
    // add, fix over/under flow and set
    NormalizeAndSet(m_Hour + pOther.m_Hour, m_Min + pOther.m_Min, m_Sec + pOther.m_Sec,
            m_MSec + pOther.m_MSec);

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.SubAndAssign(const pOther: TWSimpleTime): TWSimpleTime;
begin
    // subtract, fix over/under flow and set
    NormalizeAndSet(m_Hour - pOther.m_Hour, m_Min - pOther.m_Min, m_Sec - pOther.m_Sec,
            m_MSec - pOther.m_MSec);

    Result := Self;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Add(seconds: Integer): TWSimpleTime;
var
    pTime: IWSmartPointer<TWSimpleTime>;
begin
    if (seconds > 0) then
    begin
        pTime  := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, seconds));
        Result := Add(pTime);
    end
    else
    begin
        pTime  := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, -seconds));
        Result := Sub(pTime);
    end;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Add(const pOther: TWSimpleTime): TWSimpleTime;
begin
    // add time
    Result := TWSimpleTime.Create;
    Result.Assign(Self);
    Result.AddAndAssign(pOther);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Sub(seconds: Integer): TWSimpleTime;
var
    pTime: IWSmartPointer<TWSimpleTime>;
begin
    if (seconds > 0) then
    begin
        pTime  := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, seconds));
        Result := Sub(pTime);
    end
    else
    begin
        pTime  := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, -seconds));
        Result := Add(pTime);
    end;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Sub(const pOther: TWSimpleTime): TWSimpleTime;
begin
    // subtract time
    Result := TWSimpleTime.Create;
    Result.Assign(Self);
    Result.SubAndAssign(pOther);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Increment: TWSimpleTime;
var
    pTime: IWSmartPointer<TWSimpleTime>;
begin
    // create a simple time of one second
    pTime := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, 1));

    // increment the time by one second
    AddAndAssign(pTime);

    Result := TWSimpleTime.Create;
    Result.Assign(Self);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Decrement: TWSimpleTime;
var
    pTime: IWSmartPointer<TWSimpleTime>;
begin
    // create a simple time of one second
    pTime := TWSmartPointer<TWSimpleTime>.Create(TWSimpleTime.Create(0, 0, 1));

    // decrement the time by one second
    SubAndAssign(pTime);

    Result := TWSimpleTime.Create;
    Result.Assign(Self);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsEqual(const pOther: TWSimpleTime): Boolean;
begin
    // todo FIXME -cCheck -oNiki: Why milliseconds are not considered here? Normal?
    Result := ((m_Hour = pOther.m_Hour) and (m_Min = pOther.m_Min) and (m_Sec = pOther.m_Sec));
end;
//---------------------------------------------------------------------------
function TWSimpleTime.Differs(const pOther: TWSimpleTime): Boolean;
begin
    Result := not IsEqual(pOther);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsLower(const pOther: TWSimpleTime): Boolean;
begin
    // todo FIXME -cCheck -oNiki: Why milliseconds are not considered here? Normal?
    // can be optmized to be done with one or 2 instructions when knowning if on big or little endian
    Result := ((m_Hour < pOther.m_Hour)
           or ((m_Hour = pOther.m_Hour) and ((m_Min  < pOther.m_Min)
           or ((m_Min  = pOther.m_Min)  and ((m_Sec  < pOther.m_Sec)
           or ((m_Sec  = pOther.m_Sec)  and ((m_MSec < pOther.m_MSec))))))));
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsLowerOrEqual(const pOther: TWSimpleTime): Boolean;
begin
    Result := (IsLower(pOther) or IsEqual(pOther));
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsHigher(const pOther: TWSimpleTime): Boolean;
begin
    // todo FIXME -cCheck -oNiki: Why milliseconds are not considered here? Normal?
    // can be optmized to be done with one or 2 instructions when knowning if on big or little endian
    Result := ((m_Hour > pOther.m_Hour)
           or ((m_Hour = pOther.m_Hour) and ((m_Min  > pOther.m_Min)
           or ((m_Min  = pOther.m_Min)  and ((m_Sec  > pOther.m_Sec)
           or ((m_Sec  = pOther.m_Sec)  and ((m_MSec > pOther.m_MSec))))))));
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsHigherOrEqual(const pOther: TWSimpleTime): Boolean;
begin
    Result := (IsHigher(pOther) or IsEqual(pOther));
end;
//---------------------------------------------------------------------------
procedure TWSimpleTime.Clear;
begin
    m_Hour := 0;
    m_Min  := 0;
    m_Sec  := 0;
    m_MSec := 0;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.IsEmpty: Boolean;
begin
    Result := ((m_Hour = 0) and (m_Min = 0) and (m_Sec = 0) and (m_MSec = 0));
end;
//---------------------------------------------------------------------------
function TWSimpleTime.NormalizeAndSet(hour, min, sec, msec: Integer): Integer;
begin
    // normalize milliseconds
    m_MSec := TWMathHelper.Normalize(msec, 1000, Result);
    Inc(sec, Result);

    // normalize seconds
    m_Sec := TWMathHelper.Normalize(sec, 60, Result);
    Inc(min, Result);

    // normalize minutes
    m_Min := TWMathHelper.Normalize(min, 60, Result);
    Inc(hour, Result);

    // normalize hours
    m_Hour := TWMathHelper.Normalize(hour, 24, Result);

    // NOTE Result contains now the extra day (if any)
end;
//---------------------------------------------------------------------------
function TWSimpleTime.ToMilliseconds: Cardinal;
begin
    Result := ((m_Hour * 3600000) + (m_Min * 60000) + (m_Sec * 1000) + m_MSec);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.ToStr: UnicodeString;
begin
    //  delmiter as 13:00:00.xxx
    Result :=          Format('%.*d', [2, m_Hour]) + ':';
    Result := Result + Format('%.*d', [2, m_Min])  + ':';
    Result := Result + Format('%.*d', [2, m_Sec])  + '.';
    Result := Result + Format('%.*d', [3, m_MSec]);
end;
//---------------------------------------------------------------------------
function TWSimpleTime.FromSMIL(const txt: UnicodeString): Boolean;
var
    value, smilUnit:           UnicodeString;
    c:                         WideChar;
    values:                    array of Cardinal;
    isFractional, readingUnit: Boolean;
    fraction, hour, min, sec:  Single;
begin
    Clear;

    try
        // empty string is failure
        if (Length(txt) = 0) then
            Exit(False);

        isFractional := False;
        readingUnit  := False;

        // iterate through text containing date/time to read
        for c in txt do
            // is numeric value?
            if ((c >= '0') and (c <= '9')) then
            begin
                // unit was reading?
                if (readingUnit) then
                begin
                    Clear;
                    Exit(False);
                end;

                value := value + c;
            end
            else
            // found fractional separator?
            if (c = '.') then
            begin
                // unit was reading?
                if (readingUnit) then
                begin
                    Clear;
                    Exit(False);
                end;

                // previous fraction char was already read?
                if (isFractional) then
                begin
                    Clear;
                    Exit(False);
                end;

                // value is fractional
                isFractional := True;
                value        := value + c;
            end
            else
            // found separator?
            if (c = ':') then
            begin
                // unit was reading?
                if (readingUnit) then
                begin
                    Clear;
                    Exit(False);
                end;

                // fractional value can only be the last read value
                if (isFractional) then
                begin
                    Clear;
                    Exit(False);
                end;

                // convert and store value, and clear previously read value
                SetLength(values, Length(values) + 1);
                {$if CompilerVersion <= 24}
                    values[Length(values) - 1] := StrToInt64(value);
                {$else}
                    values[Length(values) - 1] := StrToUInt64(value);
                {$ifend}
                value                      := '';
            end
            else
            begin
                readingUnit := True;
                smilUnit    := smilUnit + c;
            end;

        // last value is a fraction?
        if (isFractional) then
        begin
            // search for value type
            case (Length(values)) of
                2:
                begin
                    // full or partial clock value should not have an unit, only timecount should have
                    if (Length(smilUnit) = 0) then
                    begin
                        Clear;
                        Exit(False);
                    end;

                    // HH:MM:Fraction(SS.MS)
                    m_Hour := values[0];
                    m_Min  := values[1];
                end;

                1:
                begin
                    // full or partial clock value should not have an unit, only timecount should have
                    if (Length(smilUnit) = 0) then
                    begin
                        Clear;
                        Exit(False);
                    end;

                    // MM:Fraction(SS.MS)
                    m_Min := values[0];
                end;
            else
                // fraction only, it's a timecount value
            end;

            // convert fractional value to float
            fraction := StrToFloat(value, g_InternationalFormatSettings);

            // search for unit
            case (UnitToSMIL(smilUnit)) of
                IE_SU_Hour:
                begin
                    m_Hour   := Floor(fraction);
                    hour     := m_Hour;
                    fraction := ((fraction - hour) * 60.0);
                    m_Min    := Floor(fraction);
                    min      := m_Min;
                    fraction := ((fraction - min) * 60.0);
                    m_Sec    := Floor(fraction);
                    sec      := m_Sec;
                    fraction := ((fraction - sec) * 1000.0);
                    m_MSec   := Round(fraction);
                end;

                IE_SU_Min:
                begin
                    m_Min    := Floor(fraction);
                    min      := m_Min;
                    fraction := ((fraction - min) * 60.0);
                    m_Sec    := Floor(fraction);
                    sec      := m_Sec;
                    fraction := ((fraction - sec) * 1000.0);
                    m_MSec   := Round(fraction);
                end;

                // IE_CT_None should be considered as seconds, and fractional part as milliseconds
                IE_SU_None,
                IE_SU_Sec:
                begin
                    m_Sec    := Floor(fraction);
                    sec      := m_Sec;
                    fraction := ((fraction - sec) * 1000.0);
                    m_MSec   := Round(fraction);
                end;

                IE_SU_MSec:
                    m_MSec := Round(fraction);
            else
                Clear;
                Exit(False);
            end;

            Exit(True);
        end;

        // convert and store last value
        SetLength(values, Length(values) + 1);
        {$if CompilerVersion <= 24}
            values[Length(values) - 1] := StrToInt64(value);
        {$else}
            values[Length(values) - 1] := StrToUInt64(value);
        {$ifend}

        // found unit?
        if (Length(smilUnit) > 0) then
        begin
            // values should contain only one value
            if (Length(values) <> 1) then
                raise Exception.Create('Too many values in SMIL unit');

            // search for unit
            case (UnitToSMIL(smilUnit)) of
                IE_SU_Hour: begin m_Hour := values[0]; Exit(True); end;
                IE_SU_Min:  begin m_Min  := values[0]; Exit(True); end;
                IE_SU_Sec:  begin m_Sec  := values[0]; Exit(True); end;
                IE_SU_MSec: begin m_MSec := values[0]; Exit(True); end;
            else
                Clear;
                Exit(False);
            end;
        end;

        // search for value type
        case (Length(values)) of
            3:
            begin
                // HH:MM:SS
                m_Hour := values[0];
                m_Min  := values[1];
                m_Sec  := values[2];
            end;

            2:
            begin
                // MM:SS
                m_Min := values[0];
                m_Sec := values[1];
            end;

            1:
            begin
                // SS
                m_Sec := values[0];
            end;
        else
            Clear;
            Exit(False);
        end;

        Result := True;
    except
        Clear;
        Exit(False);
    end;
end;
//---------------------------------------------------------------------------
function TWSimpleTime.ToSMIL: UnicodeString;
var
    fraction, sec, msec: Single;
begin
    // convert hour to string
    if (m_Hour <> 0) then
    begin
        // do convert hour only
        if ((m_Min = 0) and (m_Sec = 0) and (m_MSec = 0)) then
            Exit(IntToStr(m_Hour) + SMILToUnit(IE_SU_Hour));

        Result := IntToStr(m_Hour);
    end;

    // convert minutes to string (don't forget that time can be 1 hour and 0 minutes)
    if ((m_Min <> 0) or (m_Hour <> 0)) then
    begin
        // do convert minutes only
        if ((m_Hour = 0) and (m_Sec = 0) and (m_MSec = 0)) then
        begin
            Result := Result + IntToStr(m_Min) + SMILToUnit(IE_SU_Min);
            Exit;
        end;

        // is first value?
        if (Length(Result) <> 0) then
            Result := Result + ':';

        Result := Result + IntToStr(m_Min);
    end;

    // convert seconds to string (don't forget that time can be 1 hour or 1 minute and 0 seconds)
    if ((m_Sec <> 0) or (m_Min <> 0) or (m_Hour <> 0)) then
    begin
        // do convert seconds only
        if ((m_Hour = 0) and (m_Min = 0) and (m_MSec = 0)) then
        begin
            Result := Result + IntToStr(m_Sec) + SMILToUnit(IE_SU_Sec);
            Exit;
        end;

        // is first value?
        if (Length(Result) <> 0) then
            Result := Result + ':';

        // value contains milliseconds?
        if (m_MSec <> 0) then
        begin
            sec  := m_Sec;
            msec := m_MSec;

            fraction := sec + (msec / 1000.0);
            Result   := Result + FloatToStr(fraction);
        end
        else
            Result := Result + IntToStr(m_Sec);
    end;

    // convert milliseconds to string
    if (m_MSec <> 0) then
    begin
        Result := Result + IntToStr(m_MSec) + SMILToUnit(IE_SU_MSec);
        Exit;
    end;

    // by default value is 0s
    Result := Result + IntToStr(m_Sec) + SMILToUnit(IE_SU_Sec);
end;
//---------------------------------------------------------------------------
class function TWSimpleTime.UnitToSMIL(const smilUnit: UnicodeString): IESMILUnit;
begin
    // search for unit
    if (Length(smilUnit) = 0) then
        Result := IE_SU_None
    else
    if (smilUnit = 'h') then
        Result := IE_SU_Hour
    else
    if (smilUnit = 'min') then
        Result := IE_SU_Min
    else
    if (smilUnit = 's') then
        Result := IE_SU_Sec
    else
    if (smilUnit = 'ms') then
        Result := IE_SU_MSec
    else
        Result := IE_SU_Unknown;
end;
//---------------------------------------------------------------------------
class function TWSimpleTime.SMILToUnit(smilUnit: IESMILUnit): UnicodeString;
begin
    // search for clock time type
    case (smilUnit) of
        IE_SU_None: Result := '';
        IE_SU_Hour: Result := 'h';
        IE_SU_Min:  Result := 'min';
        IE_SU_Sec:  Result := 's';
        IE_SU_MSec: Result := 'ms';
    else
        raise Exception.CreateFmt('Unknown SMIL unit - %d', [Integer(smilUnit)]);
    end;
end;
//---------------------------------------------------------------------------

end.
