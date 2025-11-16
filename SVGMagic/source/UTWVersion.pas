{**
 @abstract(@name provides a class to manage version numbers.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWVersion;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows,
     UTWSmartPointer;

type
    {**
     Class to manage versions numbers
    }
    TWVersion = class
        private
            m_Items: array[0..4] of Cardinal;

        protected
            {**
             Get major version item
             @returns(Major version item)
            }
            function GetMajor: Cardinal; virtual;

            {**
             Set major version item
             @param(value Major version item value)
            }
            procedure SetMajor(value: Cardinal); virtual;

            {**
             Get minor version item
             @returns(Minor version item)
            }
            function GetMinor: Cardinal; virtual;

            {**
             Set minor version item
             @param(value Minor version item value)
            }
            procedure SetMinor(value: Cardinal); virtual;

            {**
             Get release version item
             @returns(Release version item)
            }
            function GetRelease: Cardinal; virtual;

            {**
             Set release version item
             @param(value Release version item value)
            }
            procedure SetRelease(value: Cardinal); virtual;

            {**
             Get build version item
             @returns(Build version item)
            }
            function GetBuild: Cardinal; virtual;

            {**
             Set build version item
             @param(value Build version item value)
            }
            procedure SetBuild(value: Cardinal); virtual;

            {**
             Get beta version item
             @returns(Beta version item)
            }
            function GetBeta: Cardinal; virtual;

            {**
             Set beta version item
             @param(value Beta version item value)
            }
            procedure SetBeta(value: Cardinal); virtual;

            {**
             Get version item at index
             @param(index Item index to get)
             @returns(Item at index)
             @raises(Exception if index is out of bounds)
            }
            function GetVersion(index: Cardinal): Cardinal; virtual;

            {**
             Set version item at index
             @param(index Item index)
             @param(value Item value)
             @raises(Exception if index is out of bounds)
            }
            procedure SetVersion(index, value: Cardinal); virtual;

        public
            {**
             Constructor
            }
            constructor Create; overload; virtual;

            {**
             Constructor
             @param(major Major version number)
             @param(minor Minor version number)
             @param(release Release version number)
             @param(build Build version number)
            }
            constructor Create(major: Cardinal; minor: Cardinal = 0; release: Cardinal = 0;
                    build: Cardinal = 0; beta: Cardinal = High(Cardinal)); overload; virtual;

            {**
             Constructor
             @param(major String representing major version number)
             @param(minor String representing minor version number)
             @param(release String representing release version number)
             @param(build String representing build version number)
            }
            constructor Create(const major, minor, release, build: UnicodeString); overload; virtual;

            {**
             Constructor
             @param(version String containing version)
             @raises(Exception on error)
             @br @bold(NOTE) Format can be xx, xx.yy, xx.yy.zz or xx.yy.zz.ww, values are automatically
                             swapped, i.e:
                             @unorderedList(
                                 @item(a string containing '8.0.3.0' will be converted to 8.0.3.0)
                                 @item(a string containing '9.1.5' will be converted to 9.1.5.0)
                                 @item(a string containing '8.25' will be converted to 8.25.0.0)
                             )
            }
            constructor Create(const version: UnicodeString); overload; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Clear all values
            }
            procedure Clear; virtual;

            {**
             Check if version is empty
             @returns(@true if version is empty, otherwise @false)
            }
            function IsEmpty: Boolean; virtual;

            {**
             Check if version is higher than another
             @param(pOther Other version to compare with)
             @returns(@true if version is higher than other, otherwise @false)
            }
            function IsHigher(const pOther: TWVersion): Boolean; virtual;

            {**
             Check if version is lower than another
             @param(pOther Other version to compare with)
             @returns(@true if version is lower than other, otherwise @false)
            }
            function IsLower(const pOther: TWVersion): Boolean; virtual;

            {**
             Check if version is higher than or equals to another
             @param(pOther Other version to compare with)
             @returns(@true if version is higher than or equals to other, otherwise @false)
            }
            function IsHigherOrEqual(const pOther: TWVersion): Boolean; virtual;

            {**
             Check if version is lower than or equals to another
             @param(pOther Other version to compare with)
             @returns(@true if version is lower than or equals to other, otherwise @false)
            }
            function IsLowerOrEqual(const pOther: TWVersion): Boolean; virtual;

            {**
             Check if version content is equal to another version
             @param(pOther Other version to compare with)
             @returns(@true if versions are equals, otherwise @false)
            }
            function IsEqual(const pOther: TWVersion): Boolean; virtual;

            {**
             Check if version content differs from another version
             @param(pOther Other version to compare with)
             @returns(@true if versions differ, otherwise @false)
            }
            function Differs(const pOther: TWVersion): Boolean; virtual;

            {**
             Assign version
             @param(pOther Other version to copy from)
            }
            procedure Assign(const pOther: TWVersion); virtual;

            {**
             Build version number
             @returns(Value representing version number (i.e if version is 1.6.1.5, returns 1615))
            }
            function ToInt: Integer; virtual;

            {**
             Version as string
             @param(all @true to include build number even if zero e.g. if @true 2.910, 2.91 otherwise)
             @returns(String representing version number (i.e if version is 1.6.1.5, returns '1.615'))
            }
            function ToStr(all: Boolean = False): UnicodeString; overload; virtual;

            {**
             Version as string
             @param(precision Precision (e.g '2.1' with precision = 2, '2.10' with precision = 3, ...))
             @returns(Version number as string (e.g. if version is 1.6.1.5, returns "1.615"))
             @br @bold(NOTE) If precision is bigger than max possible value, precision is reduced to
                             max (e.g. 1.2.3.4.56 with precision = 10 will return "1.23456"). If
                             precision = 0, empty string will be returned
            }
            function ToStr(precision: Integer):  UnicodeString; overload; virtual;
            function ToStr(precision: Cardinal): UnicodeString; overload; virtual;

            {**
             Version as string
             @param(all @true to include build number even if zero e.g. if @true 2.9.1.0, 2.9.1 otherwise)
             @returns(String version of version e.g. 'x.y.z.v')
             @author(Niki)
            }
            function ToStr2(all: Boolean = True): UnicodeString; overload; virtual;

            {**
             Version as string
             @param(precision Precision (e.g '2.1' with precision = 2, '2.1.0' with precision = 3, ...))
             @returns(Version number as string (e.g. if version is 1.6.1.5, returns '1.6.1.5'))
             @br @bold(NOTE)If precision is bigger than max possible value, precision is reduced to
                            max (e.g. 1.2.3.4.56 with precision = 10 will return '1.2.3.4.56'). If
                            precision = 0, empty string will be returned
            }
            function ToStr2(precision: Integer):  UnicodeString; overload; virtual;
            function ToStr2(precision: Cardinal): UnicodeString; overload; virtual;

            {**
             Version as string doted string stopping at first zero
             @returns(String version of version e.g. 2.3.0.0 = '2.3', 2.3.4.0 = '2.3.4')
             @author(Niki)
            }
            function ToStr3: UnicodeString; virtual;

            {**
             Convert value
             @param(major String representing major version number)
             @param(minor String representing minor version number)
             @param(release String representing release version number)
             @param(build String representing build version number)
            }
            procedure Convert(const major, minor, release, build: UnicodeString); overload; virtual;

            {**
             Convert value
             @param(version String containing version)
             @param(throwException If @true (default) will throw WException on conversion error,
                                   otherwise will return @false on error)
             @returns(@true on success, otherwise if throwException = @false, will return @false)
             @raises(Exception on error if throwException is @true)
             @br @bold(NOTE) Format can be xx, xx.yy, xx.yy.zz or xx.yy.zz.ww, values are automatically
                             swapped, i.e:
                             @unorderedList(
                                 @item(a string containing '8.0.3.0' will be converted to 8.0.3.0)
                                 @item(a string containing '9.1.5' will be converted to 9.1.5.0)
                                 @item(a string containing '8.25' will be converted to 8.25.0.0)
                             )
            }
            function Convert(const version: UnicodeString; throwException: Boolean = True): Boolean; overload; virtual;

        public
            {**
             Gets or sets the major version item
            }
            property Major: Cardinal read GetMajor write SetMajor;

            {**
             Gets or sets the minor version item
            }
            property Minor: Cardinal read GetMinor write SetMinor;

            {**
             Gets or sets the release version item
            }
            property Release: Cardinal read GetRelease write SetRelease;

            {**
             Gets or sets the build version item
            }
            property Build: Cardinal read GetBuild write SetBuild;

            {**
             Gets or sets the beta version item
            }
            property Beta: Cardinal read GetBeta write SetBeta;

            {**
             Gets or sets the version item at index
            }
            property Version[index: Cardinal]: Cardinal read GetVersion write SetVersion;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWVersion.Create;
begin
    inherited Create;

    Clear;
end;
//---------------------------------------------------------------------------
constructor TWVersion.Create(major, minor, release, build, beta: Cardinal);
begin
    inherited Create;

    m_Items[0] := major;
    m_Items[1] := minor;
    m_Items[2] := release;
    m_Items[3] := build;
    m_Items[4] := beta;
end;
//---------------------------------------------------------------------------
constructor TWVersion.Create(const major, minor, release, build: UnicodeString);
begin
    inherited Create;

    m_Items[4] := High(Cardinal);

    // convert to unsigned
    Convert(major, minor, release, build);
end;
//---------------------------------------------------------------------------
constructor TWVersion.Create(const version: UnicodeString);
begin
    inherited Create;

    m_Items[4] := High(Cardinal);

    // convert to unsigned
    Convert(version);
end;
//---------------------------------------------------------------------------
destructor TWVersion.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWVersion.GetMajor: Cardinal;
begin
    Result := m_Items[0];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetMajor(value: Cardinal);
begin
    m_Items[0] := value;
end;
//---------------------------------------------------------------------------
function TWVersion.GetMinor: Cardinal;
begin
    Result := m_Items[1];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetMinor(value: Cardinal);
begin
    m_Items[1] := value;
end;
//---------------------------------------------------------------------------
function TWVersion.GetRelease: Cardinal;
begin
    Result := m_Items[2];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetRelease(value: Cardinal);
begin
    m_Items[2] := value;
end;
//---------------------------------------------------------------------------
function TWVersion.GetBuild: Cardinal;
begin
    Result := m_Items[3];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetBuild(value: Cardinal);
begin
    m_Items[3] := value;
end;
//---------------------------------------------------------------------------
function TWVersion.GetBeta: Cardinal;
begin
    Result := m_Items[4];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetBeta(value: Cardinal);
begin
    m_Items[4] := value;
end;
//---------------------------------------------------------------------------
function TWVersion.GetVersion(index: Cardinal): Cardinal;
begin
    if (index > 4) then
        raise Exception.CreateFmt('Index out of bounds - %d', [index]);

    Result := m_Items[index];
end;
//---------------------------------------------------------------------------
procedure TWVersion.SetVersion(index, value: Cardinal);
begin
    if (index > 4) then
        raise Exception.CreateFmt('Index out of bounds - %d', [index]);

    m_Items[index] := value;
end;
//---------------------------------------------------------------------------
procedure TWVersion.Clear;
begin
    FillChar(m_Items, SizeOf(m_Items), $0);

    // set beta to max int value, it's normal
    m_Items[4] := High(Cardinal);
end;
//---------------------------------------------------------------------------
function TWVersion.IsEmpty: Boolean;
begin
    Result := ((m_Items[0] = 0) and (m_Items[1] = 0) and (m_Items[2] = 0) and (m_Items[3] = 0)
            and (m_Items[4] = High(Cardinal)));
end;
//---------------------------------------------------------------------------
function TWVersion.IsHigher(const pOther: TWVersion): Boolean;
var
    count, i: Cardinal;
begin
    count := Length(m_Items);

    if (count > 0) then
        for i := 0 to count - 1 do
            if (m_Items[i] > pOther.m_Items[i]) then
                Exit(True)
            else
            if (m_Items[i] < pOther.m_Items[i]) then
                Exit(False);

    // must be equal
    Result := False;
end;
//---------------------------------------------------------------------------
function TWVersion.IsLower(const pOther: TWVersion): Boolean;
var
    count, i: Cardinal;
begin
    count := Length(m_Items);

    if (count > 0) then
        for i := 0 to count - 1 do
            if (m_Items[i] < pOther.m_Items[i]) then
                Exit(True)
            else
            if (m_Items[i] > pOther.m_Items[i]) then
                Exit(False);

    // must be equal
    Result := False;
end;
//---------------------------------------------------------------------------
function TWVersion.IsHigherOrEqual(const pOther: TWVersion): Boolean;
var
    count, i: Cardinal;
begin
    count := Length(m_Items);

    if (count > 0) then
        for i := 0 to count - 1 do
            if (m_Items[i] > pOther.m_Items[i]) then
                Exit(True)
            else
            if (m_Items[i] < pOther.m_Items[i]) then
                Exit(False);

    // must be equal
    Result := True;
end;
//---------------------------------------------------------------------------
function TWVersion.IsLowerOrEqual(const pOther: TWVersion): Boolean;
var
    count, i: Cardinal;
begin
    count := Length(m_Items);

    if (count > 0) then
        for i := 0 to count - 1 do
            if (m_Items[i] < pOther.m_Items[i]) then
                Exit(True)
            else
            if (m_Items[i] > pOther.m_Items[i]) then
                Exit(False);

    // must be equal
    Result := True;
end;
//---------------------------------------------------------------------------
function TWVersion.IsEqual(const pOther: TWVersion): Boolean;
begin
    Result := CompareMem(@m_Items, @pOther.m_Items, SizeOf(m_Items));
end;
//---------------------------------------------------------------------------
function TWVersion.Differs(const pOther: TWVersion): Boolean;
begin
    Result := not CompareMem(@m_Items, @pOther.m_Items, SizeOf(m_Items));
end;
//---------------------------------------------------------------------------
procedure TWVersion.Assign(const pOther: TWVersion);
begin
    Move(pOther.m_Items, m_Items, SizeOf(m_Items));
end;
//---------------------------------------------------------------------------
function TWVersion.ToInt: Integer;
begin
    // calculate and return built value
    Result := (m_Items[0] * 1000) + (m_Items[1] * 100) + (m_Items[2] * 10) + m_Items[3];
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr(all: Boolean): UnicodeString;
begin
    Result := IntToStr(m_Items[0]) + '.' + IntToStr(m_Items[1]) + IntToStr(m_Items[2]);

    // check if build version exists
    if (all or (m_Items[3] <> 0)) then
        // if yes, include build version into final result
        Result := Result + IntToStr(m_Items[3]);
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr(precision: Integer):  UnicodeString;
begin
    Result := ToStr(Cardinal(precision));
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr(precision: Cardinal): UnicodeString;
var
    max, i: Cardinal;
begin
    // calculate max possible value for precision
    max := Length(m_Items);

    // is precision out of bounds?
    if (precision > max) then
        // reduce precision value to max value
        precision := max;

    Result := '';

    if (precision > 0) then
        for i := 0 to precision - 1 do
            if (i = 1) then
                Result := Result + '.' + IntToStr(m_Items[i])
            else
                Result := Result + IntToStr(m_Items[i]);
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr2(all: Boolean): UnicodeString;
begin
    Result := IntToStr(m_Items[0]) + '.' + IntToStr(m_Items[1]) + '.' + IntToStr(m_Items[2]);

    // check if build version exists
    if (all or (m_Items[3] <> 0)) then
        // if yes, include build version in final result
        Result := Result + '.' + IntToStr(m_Items[3]);
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr2(precision: Integer):  UnicodeString;
begin
    Result := ToStr2(Cardinal(precision));
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr2(precision: Cardinal): UnicodeString;
var
    max, i: Cardinal;
begin
    // calculate max possible value for precision
    max := Length(m_Items);

    // is precision out of bounds?
    if (precision > max) then
        // reduce precision value to max value
        precision := max;

    Result := '';

    if (precision > 0) then
        for i := 0 to precision - 1 do
            if (i = 0) then
                Result := Result + IntToStr(m_Items[i])
            else
                Result := Result + '.' + IntToStr(m_Items[i]);
end;
//---------------------------------------------------------------------------
function TWVersion.ToStr3: UnicodeString;
begin
    Result := IntToStr(m_Items[0]);

    if (m_Items[3] <> 0) then
        Result := Result + '.' + IntToStr(m_Items[1]) + '.' + IntToStr(m_Items[2]) + '.' + IntToStr(m_Items[3])
    else
    if (m_Items[2] <> 0) then
        Result := Result + '.' + IntToStr(m_Items[1]) + '.' + IntToStr(m_Items[2])
    else
    if (m_Items[1] <> 0) then
        Result := Result + '.' + IntToStr(m_Items[1]);
end;
//---------------------------------------------------------------------------
procedure TWVersion.Convert(const major, minor, release, build: UnicodeString);
begin
    // convert all parameters from string to unsigned
    m_Items[0] := StrToInt(major);
    m_Items[1] := StrToInt(minor);
    m_Items[2] := StrToInt(release);
    m_Items[3] := StrToInt(build);
end;
//---------------------------------------------------------------------------
function TWVersion.Convert(const version: UnicodeString; throwException: Boolean): Boolean;
var
    pSplittedVersion:             IWSmartPointer<TStringList>;
    major, minor, release, build: UnicodeString;
begin
    if (Length(version) = 0) then
    begin
        Clear;
        Exit(False);
    end;

    pSplittedVersion := TWSmartPointer<TStringList>.Create();

    // split input string
    ExtractStrings(['.'], [], PWideChar(version), pSplittedVersion);

    minor   := '0';
    release := '0';
    build   := '0';

    // extract major number
    if (pSplittedVersion.Count > 0) then
    begin
        major := pSplittedVersion[0];

        // extract minor number
        if (pSplittedVersion.Count > 1) then
        begin
            minor := pSplittedVersion[1];

            // extract release number
            if (pSplittedVersion.Count > 2) then
            begin
                release := pSplittedVersion[2];

                // extract build number
                if (pSplittedVersion.Count > 3) then
                    build := pSplittedVersion[3];
            end;
        end;
    end
    else
    if (throwException) then
        // at least major must be defined, if not, this is a wrong string format
        raise Exception.CreateFmt('Wrong string format - cannot initialize version - %s', [version])
    else
        Exit(False);

    // convert version from string to numbers
    Convert(major, minor, release, build);

    Result := True;
end;
//---------------------------------------------------------------------------

end.
