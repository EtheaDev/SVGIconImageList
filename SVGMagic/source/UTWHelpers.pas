{**
 @abstract(@name provides several helper classes to set up generic tasks for mathematics, file
           management, ..., that the VCL/RTL does not support.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWHelpers;

interface
    // do not include Winapi.GDIPObj in hpp, because it may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GDIPObj *)

    {$ifdef WTCONTROLS_DEBUG}
        // uncomment line below to enable cache logging (never in release)
        //{$define ENABLE_GDI_CACHE_LOGGING}
    {$endif}

uses System.TypInfo,
     System.Rtti,
     System.Classes,
     System.SysUtils,
     System.UITypes,
     System.Math,
     System.Generics.Collections,
     Vcl.Graphics,
     Vcl.Imaging.Jpeg,
     Vcl.Imaging.PngImage,
     Vcl.Imaging.GifImg,
     Vcl.Clipbrd,
     Vcl.Controls,
     Vcl.Forms,
     {$if CompilerVersion < 33}
        Winapi.MultiMon,
     {$ifend}
     {$if CompilerVersion >= 30}
         Winapi.ShellScaling,
     {$ifend}
     Winapi.GDIPAPI,
     Winapi.GDIPObj,
     Winapi.Messages,
     Winapi.Windows,
     Usp10,
     UTWTypes,
     UTWCacheHit,
     UTWColor,
     UTWVersion,
     UTWSmartPointer;

     // to avoid users to explicitly have to link usp10 in their projects
     {$hppemit '#pragma comment(lib, "usp10")'}

// multiple monitors API doesn't exists for RAD Studio versions before 10.0 Seattle, so add it if needed
{$if CompilerVersion < 30}
    const
        // redeclare the WM_DPICHANGED to keep compatibility with XE8 and earlier
        {$EXTERNALSYM WM_DPICHANGED}
        WM_DPICHANGED = $02E0;

    type
        PROCESS_DPI_AWARENESS =
        (
            PROCESS_DPI_UNAWARE           = 0,
            PROCESS_SYSTEM_DPI_AWARE      = 1,
            PROCESS_PER_MONITOR_DPI_AWARE = 2
        );

        TWfGetProcessDpiAwareness = function (hProcess: THandle; out value: PROCESS_DPI_AWARENESS): HRESULT; stdcall;

        MONITOR_DPI_TYPE =
        (
            MDT_EFFECTIVE_DPI = 0,
            MDT_ANGULAR_DPI   = 1,
            MDT_RAW_DPI       = 2,
            MDT_DEFAULT       = MDT_EFFECTIVE_DPI
        );

        TMonitorDpiType = MONITOR_DPI_TYPE;

        TWfGetDpiForMonitor = function(hMonitor: HMONITOR; dpiType: TMonitorDpiType; out dpiX: UINT;
                out dpiY: UINT): HRESULT; stdcall;
{$ifend}

const
    C_TWFileHelper_FSDrivePrefix: UnicodeString = '\\?\';
    C_TWFileHelper_FSUNCPrefix:   UnicodeString = '\\?\UNC';

type
    {**
     Array of bytes
    }
    TWByteArray = array of Byte;

    {**
     Pointer to array of bytes
    }
    PWByteArray = ^TWByteArray;

    {**
     Array of wide chars
    }
    TWWideCharArray = array of WideChar;

    {**
     Pointer to array of wide chars
    }
    PWWideCharArray = ^TWWideCharArray;

    {**
     Array of RGB pixels
    }
    TWRGBTripleArray = array [Byte] of TRGBTriple;

    {**
     Pointer to array of RGB pixels
    }
    PWRGBTripleArray = ^TWRGBTripleArray;

    {**
     Array of RGBA pixels
    }
    TWRGBQuadArray = array [Byte] of TRGBQuad;

    {**
     Pointer to array of RGBA pixels
    }
    PWRGBQuadArray = ^TWRGBQuadArray;

    {**
     Array of image codec info
    }
    TWImageCodecInfoArray = array [Byte] of TImageCodecInfo;

    {**
     Pointer to array of image codec info
    }
    PWImageCodecInfoArray = ^TWImageCodecInfoArray;

    {**
     Helper for string
    }
    TWStringHelper = record
        {**
         Negative exponent multiplier
         @br @bold(NOTE) Memory tables are used to quickly get exponent multiplier in several
                         "string to value" conversions (pow is very slow)
        }
        const m_NegExpMultip: array [0..17] of Double =
        (
            1, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.000001, 0.0000001, 0.00000001,
            0.000000001, 0.0000000001, 0.00000000001, 0.000000000001, 0.0000000000001,
            0.00000000000001, 0.000000000000001, 0.0000000000000001, 0.00000000000000001
        );

        {**
         Positive exponent multiplier
         @br @bold(NOTE) Memory tables are used to quickly get exponent multiplier in several
                         "string to value" conversions (pow is very slow)
        }
        const m_PosExpMultip: array [0..17] of Double =
        (
            1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
            100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
            10000000000000, 100000000000000, 1000000000000000, 10000000000000000, 100000000000000000
        );

        {**
         Empty string instance
        }
        const Empty = '';

        {**
         Gets a substring of a string
         @param(str String from which the substring should be extracted)
         @param(index Start index in the source string from which the substring should be copied)
         @param(length Substring length to copy in the source string)
         @returns(Substring)
        }
        class function Substr(const str: UnicodeString; index: Integer): UnicodeString; overload; inline; static;
        class function Substr(const str: UnicodeString; index, length: Integer): UnicodeString; overload; inline; static;

        {**
         Splits a string into sub-strings based on a delimiter
         @param(delimiter Delimiter to use to split string)
         @param(str String to split)
         @param(strings @bold([out]) list containing resulting splitted strings (must be created before calling this function))
        }
        class procedure Split(const delimiter: WideChar; const str: UnicodeString; strings: TStrings); inline; static;

        {**
         Converts a string to upper case
         @param(str Source string to convert)
         @returns(Converted string to uppercase)
        }
        class function ToUpper(const str: UnicodeString): UnicodeString; inline; static;

        {**
         Converts a string to lower case
         @param(str Source string to convert)
         @returns(Converted string to lowercase)
        }
        class function ToLower(const str: UnicodeString): UnicodeString; inline; static;

        {**
         Convert string to bool
         @param(str String to convert)
         @returns(@true if string is "-1", "1", "true" or "TRUE", otherwise @false)
        }
        class function StrToBool(const str: UnicodeString): Boolean; inline; static;

        {**
         Get first char index found in a string
         @param(str String to search in)
         @param(ch Char to find)
         @returns(Index of first char occurrence found in source string, -1 if not found)
        }
        class function IndexOf(const str: UnicodeString; ch: Char): Integer; inline; static;

        {**
         Check if a string is empty
         @param(str String to check)
         @returns(@true if string is empty, otherwise @false)
        }
        class function IsEmpty(const str: UnicodeString): Boolean; inline; static;

        {**
         Trim a string
         @param(str String to trim)
         @returns(Trimmed string)
        }
        class function Trim(const str: UnicodeString): UnicodeString; inline; static;

        {**
         Convert bool to string
         @param(value Value to convert)
         @param(asString Set to @true to return "TRUE" or "FALSE", otherwise "1" or "0")
         @param(compensate Compensate the string length difference between "TRUE" and "FALSE",
                           when it's important that the length always remains the same)
         @returns(Result)
        }
        class function BoolToStr(value, asString: Boolean; compensate: Boolean = False): UnicodeString; static;

        {**
         Check if a digit contains only chars that can be converted to number
         @param(digit Digit to check)
         @param(isStrict If @true, only chars from '0' to '9' will be accepted, see note)
         @returns(@true if digit contains only chars that can be converted to number, otherwise @false)
         @br @bold(NOTE) If strict mode is set to @false, math symbols as e.g. '.' or '-' will also
                         be accepted as valid numeric chars. This may be useful when IsNumeric() is
                         used e.g. to determine if a string can be converted to number
        }
        class function IsNumeric(digit: AnsiChar; isStrict: Boolean): Boolean; overload; static;

        {**
         Check if a digit contains only chars that can be converted to number
         @param(digit Digit to check)
         @param(isStrict If @true, only chars from '0' to '9' will be accepted, see note)
         @returns(@true if digit contains only chars that can be converted to number, otherwise @false)
         @br @bold(NOTE) If strict mode is set to @false, math symbols as e.g. '.' or '-' will also
                         be accepted as valid numeric chars. This may be useful when IsNumeric() is
                         used e.g. to determine if a string can be converted to number
        }
        class function IsNumeric(digit: WideChar; isStrict: Boolean): Boolean; overload; static;

        {**
         Check if digit is numeric or ASCII alphabetical letter (from 'a' to 'z' and from 'A' to 'Z')
         @param(digit Digit to check)
         @param(numOnly If @true, only numeric char can success)
         @returns(@true if digit contains only numeric or alphanumeric value, otherwise @false)
        }
        class function IsNumOrAscii(digit: WideChar; numOnly: Boolean): Boolean; static;

        {**
         Check if character can break a word
         @br @bold(NOTE) This a basic implementations considering that Chinese, Japanese and Korean
                         letters can break words, but all others not. It does not take account of
                         specific word wrap rules of these languages
         @br @bold(NOTE) For reference see: @unorderedList(
                         @item(http://en.wikipedia.org/wiki/Word_wrap)
                         @item(http://en.wikipedia.org/wiki/Line_breaking_rules_in_East_Asian_language)
                         @item(http://jrgraphix.net/research/unicode_blocks.php)
                         )
         @author(Niki, David)
        }
        class function CanWrap(const c: WideChar): Boolean; static;

        {**
         Delimit a string with a specified character
         E.g: ---- Hello World ----
         @param(str String to delimit)
         @param(sideLen Number of characters on each side)
         @param(delimiter Delimiter char)
         @returns(A delimited string)
         @br @bold(NOTE) The length doesn't count the two spaces before and after the string
        }
        class function DelimitStr(const str: UnicodeString; sideLen: NativeInt;
                delimiter: WideChar): UnicodeString; static;

        {**
         Delimit a string with a specified character, with as much character as
         needed to reach wanted string size
         E.g: if wanted length is 21 and delimiter '-'
            -----Hello World-----
            --------Hello--------
         @param(str String to delimit)
         @param(wantedLen Wanted final length of the string)
         @param(delimiter Delimiter char)
         @returns(A delimited string)
         @br @bold(NOTE) If str length >= wantedLen, result = str
        }
        class function DelimitFillStr(const str: UnicodeString; wantedLen: NativeInt;
                delimiter: WideChar): UnicodeString; static;

        {**
         Complete string length with selected character
         E.g. FillStrLeft("Hello", 10, '#') -> "#####Hello"
         @param(str String to complete)
         @param(fillLen Length of string to reach)
         @param(delimiter Character to use to fill the space)
         @returns(Completed string)
         @br @bold(NOTE) If string is already >= length, it won't be modified
        }
        class function FillStrLeft(const str: UnicodeString; fillLen: NativeInt;
                delimiter: WideChar): UnicodeString; static;

        {**
         Complete string length with selected character
         E.g. FillStrRight("Hello", 10, '#') -> "Hello#####"
         @param(str String to complete)
         @param(fillLen Length of string to reach)
         @param(delimiter Character to use to fill the space)
         @returns(Completed string)
         @br @bold(NOTE) If string is already >= length, it won't be modified
        }
        class function FillStrRight(const str: UnicodeString; fillLen: NativeInt;
                delimiter: WideChar): UnicodeString; static;

        {**
         Skip chars in a string, from the current pos until the next pos containing none of these chars
         @param(str String in which characters should be skipped)
         @param(chars Chars to skip)
         @param(pos @bold([in, out]) Start pos in string, pos after skipped chars on function ends)
         @returns(@false if string end is reached, otherwise @true)
        }
        class function SkipChars(const str, chars: UnicodeString; var pos: NativeInt): Boolean; static;

        {**
         Read and parse next int/int64 in string. Will skip spaces and new lines in front of the
         number and will stop at number end
         @param(str String to read from)
         @param(pos @bold([in, out]) Start/end position, end updated only on success)
         @param(resVal @bold([out]) Variable to receive parsed value)
         @returns(@true if int was read from string, @false if contained none)
        }
        class function ReadInt<TInt>(const str: UnicodeString; var pos: NativeInt; var resVal: TInt): Boolean; static;

        {**
         Read and parse next int/int64 in string. Will skip spaces and new lines in front of the
         number and will stop at number end
         @param(str String to read from)
         @param(pos @bold([in, out]) Start/end position, end updated only on success)
         @param(validNumPos @bold([out]) Will return position of last char used in number; e.g. if
                                         forFractalPart not all numbers maybe used in case of overflow,
                                         this will be set to last used number)
         @param(resVal @bold([out]) Variable to receive parsed value)
         @param(forFractalPart @true if integer in fractal part of floating point, in that case
                               result won't overlow and will read only whats possible, if false if
                               number > readed one, number can overflow)
         @returns(@true if int was read from string, @false if contained none)
        }
        class function ReadIntEx<TInt>(const str: UnicodeString; var pos, validNumPos: NativeInt;
                var resVal: TInt; forFractalPart: Boolean): Boolean; static;

        {**
         Read and parse next float/double in string. Will skip spaces and new lines in front of the
         number and will stop at number end
         @param(str String to read from)
         @param(pos @bold([in, out]) Start/end position, end updated only on success)
         @param(resVal @bold([out]) Variable to receive parsed value)
         @returns(@true if float/double was read from string, @false if contained none)
         @br @bold(NOTE) Function was implemented as performance alternative to string stream and
                         it 2x - 3x faster
         @br @bold(NOTE) This function should be only used with non localized strings (i.e for which
                         separators are compatible with EN language) because the separators defined
                         in the computer locale will be ignored, and only the EN version will be used
        }
        class function ReadFloat<TFlt, TInt>(const str: UnicodeString; var pos: NativeInt; var resVal: TFlt): Boolean; overload; static;
        class function ReadFloat(const str: UnicodeString; var pos: NativeInt; var resVal: Single): Boolean; overload; static;
        class function ReadFloat(const str: UnicodeString; var pos: NativeInt; var resVal: Double): Boolean; overload; static;
    end;

    {**
     Pointer to string helper
    }
    PWStringHelper = ^TWStringHelper;

    {**
     Helper for files
    }
    TWFileHelper = record
        public
            {**
             Return the properly prefixed unicode file name
             @param(path The path, plus eventual file name which to append the prefix)
             @returns(If the path is relative or has already a prefix, the path itself will be returned,
                      if it's an absolute path with drive letter, \\\\?\\ will be appended,
                      if it's a network path, \\\\?\\UNC will be added e.g. \\\\server\\drive = \\\\?\\UNC\\server\\drive)
             @br @bold(NOTE) Needed for Win32 very long file paths
            }
            class function GetFSPrefixedName(const path: UnicodeString): UnicodeString; static;

            {**
             Get the version of a file
             @param(fileName File name for which version should be get)
             @returns(File version)
            }
            class function GetFileVersion(const fileName: string): string; overload; static;

            {**
             Get the version of a file
             @param(fileName File name for which version should be get)
             @param(pFileVersion @bold([out]) File version)
             @param(pProductVersion @bold([out]) File product version)
             @returns(@true on success, otherwise @false)
            }
            class function GetFileVersion(const fileName: AnsiString;
                    pFileVersion, pProductVersion: TWVersion): Boolean; overload; static;
            class function GetFileVersion(const fileName: UnicodeString;
                    pFileVersion, pProductVersion: TWVersion): Boolean; overload; static;
    end;

    {**
     Pointer to system helper
    }
    PWFileHelper = ^TWFileHelper;

    {**
     Helper for memory
    }
    TWMemoryHelper = record
        {**
         Check if system on which program is executed is big endian
         @returns(@true if system on which program is executed is big endian, @false if little endian)
        }
        class function IsSystemBE: Boolean; static;

        {**
         Swap content of 2 variables with the same size
         @param(left @bold([in, out]) First variable to swap)
         @param(right @bold([in, out]) Second variable to swap)
        }
        class procedure Swap<T>(var left, right: T); static;
    end;

    {**
     Pointer to memory helper
    }
    PWMemoryHelper = ^TWMemoryHelper;

    {**
     Helper for math
    }
    TWMathHelper = record
        public
            {**
             Clamp a value between a min and max range (in a such manner that minVal >= value >= maxVal)
             @param(value Value to clamp)
             @param(minVal Min value)
             @param(maxVal Max value)
             @returns(Clamped value)
            }
            class function Clamp(value, minVal, maxVal: Integer):  Integer;  overload; inline; static;
            class function Clamp(value, minVal, maxVal: Int64):    Int64;    overload; inline; static;
            class function Clamp(value, minVal, maxVal: UInt64):   UInt64;   overload; inline; static;
            class function Clamp(value, minVal, maxVal: Single):   Single;   overload; inline; static;
            class function Clamp(value, minVal, maxVal: Double):   Double;   overload; inline; static;
            class function Clamp(value, minVal, maxVal: Extended): Extended; overload; inline; static;

            {**
             Perform a real mathematical modulo, that works also with negative numbers
             @param(num Number on which modulo should be performed)
             @param(by Number against which modulo should be performed)
             @returns(Modulo)
             @br @bold(NOTE) This function should be used instead of the Delphi mod operand whenever
                             the expected result should be equals to the mathematical modulo result.
                             For example, processing -6 mod 5 is expected to return 4, conformly to
                             a mathematical modulo result, however the Delphi modulo will return -1
                             instead
            }
            class function IntMod(const num, by: Integer): Integer; static;

            {**
             Perform a modulo on a real number
             @param(num Real number on which modulo should be performed)
             @param(by Real number against which modulo should be performed)
             @returns(Modulo)
            }
            class function ExtMod(const num, by: Extended): Extended; inline; static;

            {**
             Normalize number between 0 and limit, number of times its is contained in the limit
             (number of under or overflow), e.g.
             @unorderedList(
                 @item(Normalize(25, 24, loops) will return 1 and loops will be 1 (one overflow))
                 @item(Normalize(-1, 24, loops) will return 23 and loops will be -1 (one underflow))
                 @item(Normalize(60, 24, loops) will return 12 and loops will be 2 (two overflows)))
             @param(number Number to normalize)
             @param(limit Max allowed number value (exclusive, e.g. if limit 24 and number 24 will return 0))
             @param(loops @bold([out]) Fractional loops due to underflow/overflow)
             @returns(Normalized number)
            }
            class function Normalize(number, limit: Integer; out loops: Integer): Integer; inline; static;

            {**
             Check if the multiplier of a product can be found and get it if yes
             @param(multiplicand The multiplicand number)
             @param(product The multiplication product)
             @param(multiplier @bold([out]) the multiplier, if function success)
             @returns(@True if a multiplier can be found, otherwise @False)
            }
            class function CheckAndGetMuliplier(multiplicand, product: NativeUInt;
                    out multiplier: NativeUInt): Boolean; inline; static;
    end;

    {**
     Pointer to math helper
    }
    PWMathHelper = ^TWMathHelper;

    {**
     Helper for system
    }
    TWSystemHelper = record
        public
            {**
             Get the version of a DLL
             @param(hMod DLL module instance for which the version should be get)
             @returns(DLL version)
            }
            class function GetDllVersion(hMod: HMODULE): string; overload; static;
            class function GetDllVersion: string; overload; static;
    end;

    {**
     Pointer to system helper
    }
    PWSystemHelper = ^TWSystemHelper;

    NTSTATUS            =  Integer;
    PRTL_OSVERSIONINFOW = ^TOSVersionInfoW;

    {**
     RtlGetVersion function prototype
     @param(pOsvi Windows version info structure to populate, populated structure on function ends)
     @returns(STATUS_SUCCESS (i.e 0x0) on success, otherwise error code)
    }
    TWfRtlGetVersion = function(pOsvi: PRTL_OSVERSIONINFOW): NTSTATUS; stdcall;

    {**
     Result type
    }
    TWEResult =
    (
        TW_E_Error = -1,
        TW_E_False =  0,
        TW_E_True  =  1
    );

    {**
     Helper for Windows operating system
    }
    TWOSWinHelper = record
        private
            class var m_WinVersion: TOSVersionInfoExW;

            {**
             Check using RtlGetVersion() if version passed in parameter is not newer and updates it
             if so updated it
             @param(osvi @bold([in, out]) Windows version to check against and update)
             @returns(E_True if version is newer, E_False otherwise, E_Error if could not retrieve
                      version from kernel32.dll)
            }
            class function GetVersionFromNtDll(var osvi: TOSVersionInfoExW): TWEResult; static;

            {**
             Check from kernel32.dll if version passed in parameter is not newer and updates it if so
             @param(osvi @bold([in, out]) Windows version to check against and update)
             @returns(E_True if version is newer, E_False otherwise, E_Error if could not retrieve
                      version from kernel32.dll)
            }
            class function GetVersionFromKernel32(var osvi: TOSVersionInfoExW): TWEResult; static;

        public
            {**
             Get Windows version
             @param(useCache If @true the cached version will be used, otherwise it will be recomputed)
             @returns(Windows version)
            }
            class function GetWinVersion(useCache: Boolean = False): TOSVersionInfoExW; static;

            {**
             Check if the Windows version is equals or higher than a specified level
             @param(major Windows major version)
             @param(minor Windows minor version)
             @param(build Windows build version)
             @param(useCache If @true the cached version will be used, otherwise it will be recomputed)
             @returns(Windows version)
             @br @bold(NOTE) This function is guarantee to work correctly in all situations and should
                             be used instead of the broken VCL CheckWin32Version() function
            }
            class function CheckWinVersion(major: Integer; minor: Integer = 0; build: Integer = 0;
                    useCache: Boolean = True): Boolean; static;
    end;

    {**
     Pointer to Windows operating system helper
    }
    PWOSWinHelper = ^TWOSWinHelper;

    {**
     Controller that can enable or disable some specific GDI cache
    }
    TWGDICacheController = packed record
        m_DCs:                     Boolean;
        m_Bitmaps:                 Boolean;
        m_DIBBitmaps:              Boolean;
        m_UseDIBBitmapsToDrawText: Boolean;
        m_UseAlphaBlendToDrawText: Boolean;
        m_UseDIBBitmapsToDrawRect: Boolean;
        m_UseAlphaBlendToDrawRect: Boolean;
    end;

    {**
     Pointer to GDI cache controller
    }
    PWGDICacheController = ^TWGDICacheController;

    {**
     Optimized functions to draw with Windows GDI. These functions support alpha transparency
     @br @bold(NOTE) The code has been optimized to gain maximum possible speed, therefore, do not
                     clean it unless you are really aware of what you are doing
    }
    TWGDIHelper = class
        public type
            {**
             Horizontal alignment
             @value(IE_HA_None Not aligned)
             @value(IE_HA_Left Left aligned)
             @value(IE_HA_Center Centered)
             @value(IE_HA_Right Right aligned)
            }
            IEHAlign =
            (
                IE_HA_None,
                IE_HA_Left,
                IE_HA_Center,
                IE_HA_Right
            );

            {**
             Vertical alignment
             @value(IE_VA_None Not aligned)
             @value(IE_VA_Top Top aligned)
             @value(IE_VA_Center Centered)
             @value(IE_VA_Bottom Bottom aligned)
            }
            IEVAlign =
            (
                IE_VA_None,
                IE_VA_Top,
                IE_VA_Center,
                IE_VA_Bottom
            );

            {**
             Text trimming
             @value(IE_TT_None Text is not trimmed)
             @value(IE_TT_Character For compatibility with GDI+, should not be used here (same result as IE_TT_None))
             @value(IE_TT_Word For compatibility with GDI+, should not be used here (same result as IE_TT_None))
             @value(IE_TT_EllipsisCharacter Text is ellipsified on the closest char)
             @value(IE_TT_EllipsisWord Text is ellipsified on the closest word)
             @value(IE_TT_EllipsisPath Text is ellipsified on the middle, string begin and end are visible)
            }
            IETextTrimming =
            (
                IE_TT_None,
                IE_TT_Character,
                IE_TT_Word,
                IE_TT_EllipsisCharacter,
                IE_TT_EllipsisWord,
                IE_TT_EllipsisPath
            );

            {**
             Draw text function to use
             @value(IE_DF_GDI Native GDI draw text function is used)
             @value(IE_DF_WS_Optimized Advanced draw text function is used)
            }
            IEDrawTextFunc =
            (
                IE_DF_GDI,
                IE_DF_WS_Optimized
            );

            {**
             Rectangle options
            }
            IRectOptions = record
                m_Color:        TColor;
                m_Outline:      TColor;
                m_OutlineWidth: Cardinal;

                {**
                 Get the default rect options
                 @returns(A new rect options instance initialized with the default values)
                }
                class function GetDefault: IRectOptions; inline; static;
            end;

            {**
             Pointer to rectangle options
            }
            PRectOptions = ^IRectOptions;

            {**
             Text options
            }
            ITextOptions = record
                m_HAlign:                 IEHAlign;
                m_VAlign:                 IEVAlign;
                m_BgColor:                TColor;
                m_DrawBg:                 Boolean;
                m_RightToLeft:            Boolean;
                m_NoWrap:                 Boolean;
                m_NoClip:                 Boolean;
                m_ShowHotkeyPrefix:       Boolean;
                m_EditCtrlStyle:          Boolean;
                m_NoFullWidthCharBreak:   Boolean;
                m_ExpandTabs:             Boolean;
                m_IncludeExternalLeading: Boolean;
                m_LeftMargin:             Integer;
                m_RightMargin:            Integer;
                m_TabLength:              Integer;
                m_TextTrimming:           IETextTrimming;
                m_DrawTextFunc:           IEDrawTextFunc;

                {**
                 Get the default text options
                 @returns(A new text options instance initialized with the default values)
                }
                class function GetDefault: ITextOptions; inline; static;

                {**
                 Get the configuration flags to pass to DrawText() function
                 @returns(The configuration flags to pass to DrawText() function)
                }
                function GetDrawTextConfig: DWORD;
            end;

            {**
             Pointer to text options
            }
            PTextOptions = ^ITextOptions;

        protected type
            {**
             Cached bitmap keys enumeration
             @value(IE_BK_DrawRectMaskDIB Cached device independent bitmap used to draw rectangle mask)
             @value(IE_BK_DrawRectOverlayDIB Cached device independent bitmap used as overlay to draw rectangle)
             @value(IE_BK_DrawTextMaskDIB Cached device independent bitmap used to draw text mask)
             @value(IE_BK_DrawTextOverlayDIB Cached device independent bitmap used as overlay to draw text)
            }
            IEBitmapKey =
            (
                IE_BK_DrawRectMaskDIB,
                IE_BK_DrawRectOverlayDIB,
                IE_BK_DrawTextMaskDIB,
                IE_BK_DrawTextOverlayDIB
            );

        private type
            {**
             Bitmap info with a reserved fixed space
             @br @bold(NOTE) This record is equivalent to a C++ union of bmpInfo and pReserveSpace
            }
            IBitmapInfoUnion = record
                case Boolean of
                    True:  (bmpInfo:       TBitmapInfo);
                    False: (pReserveSpace: array [0..(sizeof(TBitmapInfo) + $FF * SizeOf(TRGBQuad))] of Byte);
            end;

            {**
             Pointer to bitmap info with a reserved fixed space
            }
            PBitmapInfoUnion = ^IBitmapInfoUnion;

            {**
             Cached device context, they are device contexts that are not expected to change often,
             but which are often used for generic tasks like e.g. create a compatible bitmap or draw
             a text in a DIB bitmap
            }
            ICachedDC = class
                private
                    m_pDCCount: TWCacheHit;
                    m_hDC:      THandle;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Get device context
                     @returns(Device context)
                    }
                    function GetDC: THandle; virtual;
            end;

            {**
             Device independent bitmap
            }
            IDIBBitmap = class
                private
                    m_hBitmap:     HBITMAP;
                    m_pBytes:      Pointer;
                    m_hDC:         THandle;
                    m_Width:       LONG;
                    m_Height:      LONG;
                    m_PixelFormat: WORD;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Bitmap cache
            }
            ICachedBitmaps = TObjectDictionary<IEBitmapKey, Vcl.Graphics.TBitmap>;

            {**
             Device indepednent bitmap cache
            }
            ICachedDIBBitmaps = TObjectDictionary<IEBitmapKey, IDIBBitmap>;

            {**
             Cached data structure
            }
            ICache = class
                private
                    m_pBitmaps:    ICachedBitmaps;
                    m_pDIBBitmaps: ICachedDIBBitmaps;
                    m_pMemoryDC:   ICachedDC;

                    {$ifdef ENABLE_GDI_CACHE_LOGGING}
                        m_pBitmapsCount:    TWCacheHit;
                        m_pDIBBitmapsCount: TWCacheHit;
                    {$endif}

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Clear cache
                    }
                    procedure Clear; virtual;
            end;

        private
            m_pCache: ICache;

            {**
             Configure GDI to draw text
             @param(hDC Device context)
             @param(drawBg If @true, opaque background will be drawn below text)
             @param(hFont Font to use to draw text)
             @param(bgColor Color to use to draw text background, ignored if bgColor is set to @false)
             @param(textColor Color to use to draw text)
            }
            procedure ConfigureGDIToDrawText(hDC: THandle; drawBg: Boolean; hFont: THandle;
                    bgColor, textColor: TColorRef);

            {**
             Draw a rectangle (internal function)
             @param(drawRect Rect bounding rectangle to draw)
             @param(radiusX Radius x value, can be 0)
             @param(radiusY Radius y value, can be 0)
             @param(options Rectangle options)
             @param(doDrawRoundRect If @true, a rounded rect will be drawn)
             @param(hDC Device context to draw to)
            }
            procedure DrawRect(const drawRect: TRect; radiusX, radiusY: Cardinal;
                    const options: IRectOptions; doDrawRoundRect: Boolean; hDC: Thandle); overload;

            {**
             Get text mask
             @param(text Text for which mask should be get)
             @param(textWidth Measured text width)
             @param(textHeight Measured text height)
             @param(textRect Measured text rect)
             @param(pFont Font to use to draw text)
             @param(config Text configuration flags)
             @param(params @bold([in, out]) Text drawing extra parameters)
             @param(textFunc Draw text function to use)
             @param(hDC Device context from which background should be copied)
             @returns(Bitmap containing mask, @nil on error)
            }
            function GetTextMask(const text: UnicodeString; textWidth, textHeight: Integer;
                    const textRect: TRect; pFont: TFont; config: DWORD; var params: TDrawTextParams;
                    drawTextFunc: IEDrawTextFunc; hDC: THandle): Vcl.Graphics.TBitmap;

            {**
             Prepare bitmap to be used with AlphaBlend functions
             @param(pBm1 First 24 bit bitmap source)
             @param(pBm2 Second 24 bit bitmap source)
             @param(alpha Alpha value to apply)
             @returns(Bitmap to use with AlphaBlend function, @nil on error)
             @br @bold(NOTE) Bitmap should be deleted when useless)
            }
            function PrepareBlending(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap;
                    alpha: Byte): Vcl.Graphics.TBitmap;

            {**
             Check if a rectangle fits a bitmap, calculate rect that never exceeds bitmap if not
             @param(rect Rectangle)
             @param(width Bitmap width)
             @param(height Bitmap height)
             @param(bitmapRect @bold([out]) Blend rectangle fitting in bitmap size)
             @returns(@true if blend rectangle could be calculated in the bitmap limits, otherwise @false)
            }
            class function CalculateBitmapRect(const rect: TRect; width, height: Integer;
                    out bitmapRect: TRect): Boolean; static;

        protected
            {**
             Get cached bitmap
             @param(key Bitmap key)
             @returns(Cached bitmap)
            }
            function GetCachedBitmap(key: IEBitmapKey): Vcl.Graphics.TBitmap; virtual;

            {**
             Get cached device independent bitmap
             @param(key Bitmap key)
             @param(hDC Device context to use, in case bitmap should be created)
             @param(width @bold([in, out]) Bitmap width, cached width on function ends)
             @param(height @bold([in, out]) Bitmap height, cached height on function ends)
             @param(pixelFormat Bitmap pixel format in case bitmap should be created)
             @param(pBits @bold([out]) Array of bits that contains the bitmap pixels if function succeeded)
             @returns(Cached bitmap)
             @br @bold(NOTE) Bitmap will be recreated in case its dc, width, height or pixel format
                             was modified
            }
            function GetCachedDIBBitmap(key: IEBitmapKey; hDC: THandle; var width, height: LONG;
                    pixelFormat: WORD; out pBits: Pointer): HBitmap; virtual;

            {**
             Update text rect position relatively to alignment and reference rect
             @param(refRect Reference rect)
             @param(rect @bold([in, out]) Rect to update)
             @param(options Text options)
            }
            procedure UpdateTextRectPos(const refRect: TRect; var rect: TRect;
                    const options: ITextOptions); virtual;

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Draw a rectangle
             @param(rect Rect bounding rectangle to draw)
             @param(radiusX Radius x value, can be 0)
             @param(radiusY Radius y value, can be 0)
             @param(options Rectangle options)
             @param(hDC Device context to draw to)
            }
            procedure DrawRect(const rect: TRect; radiusX, radiusY: Cardinal;
                    const options: IRectOptions; hDC: THandle); overload; virtual;

            {**
             Draw a transparent rectangle
             @param(rect Rect bounding rectangle to draw)
             @param(radiusX Radius x value, can be 0)
             @param(radiusY Radius y value, can be 0)
             @param(opacity Opacity level, between 0.0 (fully transparent) and 1.0 (fully opaque))
             @param(options Rectangle options)
             @param(hDC Device context to draw to)
            }
            procedure DrawRect(const rect: TRect; radiusX, radiusY: Cardinal; opacity: Single;
                    const options: IRectOptions; hDC: THandle); overload; virtual;

            {**
             Get the font fallback
             @param(hDC Device context)
             @param(hFont Proposed font)
             @param(text Text for which the font fallback should be found)
             @param(pFontFallback @bold([out]) If the function returns true, the found font fallback)
             @returns(@true on success, otherwise @false)
            }
            function GetFontFallback(hDC, hFont: THandle; const text: UnicodeString;
                    pFontFallback: PLogFont): Boolean; virtual;

            {**
             Get text size
             @param(text Text to measure)
             @param(rect Text bounding rectangle)
             @param(pFont Text font)
             @param(options Text options)
             @param(hDC Device context handle)
             @param(pCharacters @bold([out]) Pointer to variable to receive the number of characters
                                             that will be displayed in rect, if @nil will be ingored)
             @param(pLines @bold([out]) Pointer to variable to receive the number of lines that will
                                        be displayed in rect, if @nil will be ingored)
             @returns(Text size, empty size on error)
            }
            function GetTextSize(const text: UnicodeString; const rect: TRect; pFont: TFont;
                    const options: ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
                    pLines: PInteger = nil): TSize; virtual;

            {**
             Draw a text
             @param(text Text to draw)
             @param(rect Rect bounding text to draw)
             @param(pFont Text font to use)
             @param(options Text options)
             @param(hDC Device context to draw to)
            }
            procedure DrawText(const text: UnicodeString; const rect: TRect; pFont: TFont;
                    const options: ITextOptions; hDC: THandle); overload; virtual;

            {**
             Draw a transparent text
             @param(text Text to draw)
             @param(rect Rect bounding text to draw)
             @param(pFont Text font to use)
             @param(opacity Opacity level, between 0.0 (fully transparent) and 1.0 (fully opaque))
             @param(options Text options)
             @param(hDC Device context to draw to)
            }
            procedure DrawText(const text: UnicodeString; const rect: TRect; pFont: TFont;
                    opacity: Single; const options: ITextOptions; hDC: THandle); overload; virtual;

            {**
             Draw or measure a text and select the correct function to use
             @param(drawTextFunc Draw text function to use)
             @param(hDC Device context used to draw or measure text)
             @param(pStr @bold([in, out]) String to draw, can be modified depending on flags)
             @param(iCount String to draw char count (WARNING not modified if input string was modified))
             @param(textRect @bold([in, out]) Bounding rectangle, can be modified depending on flags)
             @param(flags Draw flags)
             @param(pDtp Draw extra parameters to use)
             @returns(Text height in logical units, 0 on error)
            }
            class function DrawText(drawTextFunc: IEDrawTextFunc; hDC: THandle; pStr: LPWSTR;
                    iCount: Integer; var textRect: TRect; flags: UINT; pDtp: PDrawTextParams): Integer; overload; static;

            {**
             Draw a line
             @param(hDC Device context on which line should be drawn)
             @param(startPos Line start position)
             @param(endPos Line end position)
             @param(color Line color)
             @param(width Line width, in logical units. If 0, will be a single pixel width regardless
                          of the current transformations)
            }
            class procedure DrawLine(hDC: THandle; const startPos, endPos: TPoint; color: TColor;
                    width: NativeUInt); static;

            {**
             Compare fonts
             @param(pFont1 First font to compare to)
             @param(pFont2 Second font to compare with)
             @returns(@true if font are identical, otherwise @false)
            }
            class function CompareFonts(pFont1, pFont2: TFont): Boolean; static;

            {**
             Compare font instances
             @param(pFont1 First font to compare to)
             @param(pFont2 Second font to compare with)
             @returns(@true if font instances are equals, otherwise @false)
            }
            class function CompareFontInstances(pFont1, pFont2: TFont): Boolean; static;

            {**
             Get a pixel on the bitmap at the specified location
             @param(pBitmap Bitmap on which pixel should be get)
             @param(x Pixel x coordinate)
             @param(y Pixel y coordinate)
             @param(defaultColor Default color to return in case pixel coordinates are out of bounds)
             @returns(Pixel color, default color if not found or on error)
            }
            class function GetPixel(pBitmap: Vcl.Graphics.TBitmap; x, y: Integer;
                    const defaultColor: TWColor): TWColor; static;

            {**
             Check if bitmap is a device format bitmap, i.e. a bitmap located on the device
             @param(hBitmap Bitmap handle to check)
             @param(info @bold([out]) Bitmap info)
             @returns(@true if bitmap is a device format bitmap, otherwise @false)
            }
            class function IsDFB(hBitmap: THandle; out info: BITMAP): Boolean; static;

            {**
             Check if bitmap is a device independent bitmap, i.e. a bitmap located in memory
             @param(hBitmap Bitmap handle to check)
             @param(info @bold([out]) Bitmap info)
             @returns(@true if bitmap is a device independent bitmap, otherwise @false)
            }
            class function IsDIB(hBitmap: THandle; out info: BITMAP): Boolean; static;

            {**
             Convert a device independent bitmap to a device format bitmap
             @param(hBitmap @bold([in, out]) Bitmap to convert, converted bitmap if function succeeded)
             @returns(@true if conversion succeeded, @false if conversion was unneeded or failed)
            }
            function ConvertToDFB(var hBitmap: THandle): Boolean; virtual;

            {**
             Convert a device format bitmap to a device independent bitmap
             @param(hBitmap @bold([in, out]) Bitmap to convert, converted bitmap if function succeeded)
             @returns(@true if conversion succeeded, @false if conversion was unneeded or failed)
            }
            function ConvertToDIB(var hBitmap: THandle): Boolean; virtual;

            {**
             Get device independent bitmap using GDI
             @param(hDC Device context to make bitmap compatible with, if 0 the screen dc will be used)
             @param(width Bitmap width, in pixels)
             @param(height Bitmap height, in pixels)
             @param(pixelFormat Pixel format)
             @param(pBits @bold([out]) Array of bits that contains the bitmap pixels if function succeeded)
             @returns(bitmap handle, @nil on error)
             @br @bold(NOTE) Bitmap should be deleted by calling ::DeleteObject() when useless
            }
            class function GetDIBBitmap(hDC: THandle; width, height: LONG; pixelFormat: WORD;
                    out pBits: Pointer): THandle; static;

            {**
             Get bitmap
             @param(width Bitmap width, ignored if 0)
             @param(height Bitmap height, ignored if 0)
             @param(bmpType Bitmap type, can be device dependent bitmap or device independent bitmap)
             @param(pixelFormat Pixel format)
             @param(alphaFormat Alpha format, ignored if pixel format is not set to pf32bit)
             @returns(Bitmap)
             @br @bold(NOTE) Bitmap should be deleted when useless
            }
            class function GetBitmap(width, height: Integer; bmpType: TBitmapHandleType;
                    pixelFormat: Vcl.Graphics.TPixelFormat; alphaFormat: TAlphaFormat = afIgnored):
                    Vcl.Graphics.TBitmap; static;

            {**
             Configure bitmap
             @param(pBitmap Bitmap to configure)
             @param(width Bitmap width)
             @param(height Bitmap height)
             @param(higherOnly If @true, bitmap size will be updated only if higher than actual size)
             @param(bmpType Bitmap type, can be device dependent bitmap or device independent bitmap)
             @param(pixelFormat Pixel format)
             @param(alphaFormat Alpha format, ignored if pixel format is not set to pf32bit)
            }
            class procedure ConfigBitmap(pBitmap: Vcl.Graphics.TBitmap; width, height: Integer;
                    higherOnly: Boolean; bmpType: TBitmapHandleType; pixelFormat: Vcl.Graphics.TPixelFormat;
                    alphaFormat: TAlphaFormat = afIgnored); static;

            {**
             Partially clear bitmap
             @param(pBitmap Bitmap to clear)
             @param(rect The rect area to clear)
             @br @bold(NOTE) 32 bit bitmap alpha channel will also be cleared
            }
            class procedure Clear(pBitmap: Vcl.Graphics.TBitmap; const rect: TRect); overload; static;

            {**
             Clear a bitmap
             @param(pBitmap Bitmap to clear)
             @br @bold(NOTE) 32 bit bitmap alpha channel will also be cleared
            }
            class procedure Clear(pBitmap: Vcl.Graphics.TBitmap); overload; static;

            {**
             Partially fill a canvas with a color
             @param(pCanvas Canvas to fill)
             @param(rect The rect area to fill with color)
             @param(color Color to fill with)
            }
            class procedure Fill(pCanvas: TCanvas; const rect: TRect; const color: TWColor); overload; static;

            {**
             Partially fill a bitmap with a color
             @param(pBitmap Bitmap to fill)
             @param(rect The rect area to fill with color)
             @param(color Color to fill with)
            }
            class procedure Fill(pBitmap: Vcl.Graphics.TBitmap; const rect: TRect;
                    const color: TWColor); overload; static;

            {**
             Fill a bitmap with a color
             @param(pBitmap Bitmap to fill)
             @param(color Color to fill with)
            }
            class procedure Fill(pBitmap: Vcl.Graphics.TBitmap; const color: TWColor); overload; static;

            {**
             Copy 32 bit bitmap to another bitmap
             @param(rect Rectangle representing surface to copy, entire bitmaps will be copied if empty)
             @param(pSrc Source bitmap to copy from)
             @param(pDst Destination bitmap to copy to)
            }
            class procedure Copy32(const rect: TRect; pSrc: Vcl.Graphics.TBitmap; pDst: Pointer); static;

            {**
             Calculate bitmap stride
             @param(width Bitmap width)
             @param(pixelFormat Bitmap pixel format)
             @returns(Bitmap stride)
            }
            class function CalculateStride(width, pixelFormat: NativeUInt): NativeUInt; inline; static;

            {**
             Convert a pixel format to a color depth value
             @param(pixelFormat Pixel format to convert)
             @returns(Color depth (1, 2, 4, 8, 16, 32, ...))
             @raises(Exception on unsupported (i.e. custom pixel format) or unknown pixel format)
             @br @bold(NOTE) If pixel format is device dependent, the returned value will be 0
            }
            class function PixelFormatToColorDepth(pixelFormat: Vcl.Graphics.TPixelFormat): Cardinal; static;

            {**
             Check if bitmap content are equals
             @param(rect Rectangle representing surface to compare, entire bitmaps will be compared if empty)
             @param(pBm1 First bitmap to compare)
             @param(pBm2 Second bitmap to compare with)
             @param(width Bitmap width)
             @param(height Bitmap height)
             @returns(@true if bitmap content are equals, otherwise @false)
             @br @bold(NOTE) It is assumed that bitmaps pixel formats and sizes are equals. An access
                             violation may occur if it's not the case
            }
            class function BitmapEquals(const rect: TRect; const pBm1, pBm2: Pointer;
                    width, height: LONG; pixelFormat: WORD): Boolean; static;

            {**
             Blend 24 bit bitmaps together
             @param(rect Rectangle representing surface to blend, entire bitmaps will be blended if empty)
             @param(pBm1 @bold([in, out]) First bitmap to blend, blended bitmap when function ends)
             @param(pBm2 Second bitmap to blend with)
             @param(opacity Opacity level, between 0.0 (fully transparent) and 1.0 (fully opaque))
             @br @bold(NOTE) It is assumed that bitmaps pixel formats are both 24 bit and sizes are
                             equals. An access violation may occur if it's not the case
            }
            class procedure Blend24(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap; opacity: Single); overload; static;

            {**
             Blend 24 bit bitmaps together
             @param(rect Rectangle representing surface to blend, entire bitmaps will be blended if empty)
             @param(pBm1 @bold([in, out]) First bitmap to blend, blended bitmap when function ends)
             @param(pBm2 Second bitmap to blend with)
             @param(width Bitmap width)
             @param(height Bitmap height)
             @param(opacity Opacity level, between 0.0 (fully transparent) and 1.0 (fully opaque))
             @br @bold(NOTE) It is assumed that bitmaps pixel formats are both 24 bit and sizes are
                             equals. An access violation may occur if it's not the case
            }
            class procedure Blend24(const rect: TRect; pBm1: Pointer; const pBm2: Pointer;
                    width, height: NativeUInt; opacity: Single); overload; static;

            {**
             Blend 32 bit bitmaps together
             @param(rect Rectangle representing surface to blend, entire bitmaps will be blended if empty)
             @param(pBm1 @bold([in, out]) First bitmap to blend, blended bitmap when function ends)
             @param(pBm2 Second bitmap to blend with)
             @param(position Blending level position in percent (0.0 = bitmap1, 1.0 = bitmap2))
             @br @bold(NOTE) It is assumed that bitmaps pixel formats are both 32 bit and sizes are
                             equals. An access violation may occur if it's not the case
            }
            class procedure Blend32(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap; position: Single); overload; static;

            {**
             Blend 32 bit bitmaps together
             @param(rect Rectangle representing surface to blend, entire bitmaps will be blended if empty)
             @param(pBm1 @bold([in, out]) First bitmap to blend, blended bitmap when function ends)
             @param(pBm2 Second bitmap to blend with)
             @param(width Bitmap width)
             @param(height Bitmap height)
             @param(position Blending level position in percent (0.0 = bitmap1, 1.0 = bitmap2))
             @br @bold(NOTE) It is assumed that bitmaps pixel formats are both 32 bit and sizes are
                             equals. An access violation may occur if it's not the case
            }
            class procedure Blend32(const rect: TRect; pBm1: Pointer; const pBm2: Pointer;
                    width, height: NativeUInt; position: Single); overload; static;

            {**
             Convert opacity value (exprimed in percent between 0.0 and 1.0) to alpha value
             @param(value Opacity value to convert)
             @returns(Converted alpha value)
            }
            class function OpacityToAlpha(opacity: Double): Byte; inline; static;

            {**
             Draw an image on a canvas applying a global opacity value
             @param(pGraphic Graphic containing the source image to draw)
             @param(pos Position where the image will be painted on the destination canvas)
             @param(opacity Opacity in percent (between 0.0 and 1.0))
             @param(pCanvas Destination canvas on which the image will be painted)
             @param(pOverlay Optional overlay to use for painting (may optimize the process speed))
             @returns(@true on success, otherwise @false)
            }
            class function DrawTransparentImage(pGraphic: TGraphic; const pos: TPoint; opacity: Double;
                    pCanvas: TCanvas; pOverlay: Vcl.Graphics.TBitmap): Boolean; static;
    end;

    {**
     Helper functions for GDI+
    }
    TWGDIPlusHelper = record
        public type
            {**
             GDI+ encoder types
             @value(IE_EC_BMP Use the bitmap encoder)
             @value(IE_EC_JPG Use the JPEG encoder)
             @value(IE_EC_PNG Use the PNG encoder)
             @value(IE_EC_TIFF Use the TIFF encoder)
             @value(IE_EC_GIF Use the GIF encoder)
            }
            IEEncoderType =
            (
                IE_EC_BMP,
                IE_EC_JPG,
                IE_EC_PNG,
                IE_EC_TIFF,
                IE_EC_GIF
            );

        public
            {**
             Clear bitmap completely
             @param(pBitmap Bitmap to clear)
             @param(pGraphics GDI+ graphics to use (should be built using canvas device context))
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) 32 bit bitmap alpha channel will also be cleared
            }
            class function Clear(pBitmap: Vcl.Graphics.TBitmap; pGraphics: TGpGraphics): Boolean; overload; static;

            {**
             Clear canvas completely
             @param(pCanvas Canvas to clear)
             @param(pGraphics GDI+ graphics to use (should be built using canvas device context))
             @returns(@true on success, otherwise @false)
            }
            class function Clear(pCanvas: TCanvas; pGraphics: TGpGraphics): Boolean; overload; static;

            {**
             Get encoder to use to convert GDI+ bitmap to another format
             @param(format Target format for which encoder should be get)
             @param(pClsid @bold([out]) Encoder GUID, empty GUID if not found or on error)
             @returns(@true on success, otherwise @false)
            }
            class function GetGdiplusEncoderClsid(const format: UnicodeString; out pClsid: TGuid): Boolean; static;

            {**
             Convert a bitmap to a graphic (e.g. JPG, PNG, ...) using GDI+. In case the destination
             format contains an alpha channel, it will be preserved during the conversion
             @param(pSrc Source bitmap)
             @param(encoderType Encoder type to use to generate the destination)
             @param(compressionLevel Compression level to use in case the destination graphic can be
                                     compressed. This value may be between 0 and 9 for PNG and
                                     between 0 and 100 for JPEG)
             @returns(Converted graphic, @nil if failed or on error)
            }
            class function ToGraphic(pSrc: Vcl.Graphics.TBitmap; encoderType: IEEncoderType;
                    compressionLevel: Cardinal): TGraphic; static;

            {**
             Create GDI+ bitmap from TBitmap
             @param(pBitmap Bitmap object to convert)
             @returns(@nil if pBitmap is @nil, otherwise newly created bitmap, caller is responsible to
                      free it)
            }
            class function ToGDIPlusBitmap(const pBitmap: Vcl.Graphics.TBitmap): TGpBitmap; static;
    end;

    {**
     Helper class for images
    }
    TWImageHelper = record
        {**
         Convert a bitmap to a PNG using GDI, taking care of preserving its alpha channel
         @param(pSrc Source bitmap)
         @returns(PNG image, @nil if failed or on error)
        }
        class function BmpToPng_GDI(pSrc: Vcl.Graphics.TBitmap): TPngImage; static;

        {**
         Convert a bitmap to any other image format, taking care of preserving the alpha channel
         @param(pBitmap Bitmap to convert)
         @param(pImage Image to convert to)
         @param(useGDIPlus If @true, GDI+ will be used to process the conversion, GDI otherwise)
         @returns(@true on success, otherwise @false)
         @br @bold(NOTE) GDI+ should be previously initialized if useGDIPlus is set to @true
        }
        class function BitmapTo(const pBitmap: Vcl.Graphics.TBitmap; pImage: TGraphic;
                useGDIPlus: Boolean): Boolean; static;

        {**
         Calculate and get the best image size keeping its proportions
         @param(wSrc The image width to resize (must be > 0))
         @param(hSrc The image height to resize (must be > 0))
         @param(wDestMax @bold([in, out]) As in, the max required width of the resized image. As out,
                                          actual computed best width. If zero, it will be computed
                                          using the source image aspect ratio and dest height)
         @param(hDestMax @bold([in, out]) As in, the max required height of the resized image. As out,
                                          actual computed best height. If zero, it will be computed
                                          using source image aspect ratio and dest width)
         @param(padded If @true, image will be padded, cropped otherwise)
        }
        class procedure GetProportionalSize(wSrc, hSrc: Integer; var wDestMax, hDestMax: Integer;
                padded: Boolean); static;
    end;

    {**
     Helper class for clipboards
    }
    TWClipboardHelper = record
        {**
         Get the PNG format handle
         @returns(PNG format handle)
        }
        class function GetPNGFormatHandle: THandle; static;

        {**
         Copy a stream content to the clipboard
         @param(hFormat Format handle that represent the stream data inside the clipboard)
         @param(pStream Stream to copy to clipboard)
         @param(length Length in stream to copy to clipboard)
         @returns(@true on success, otherwise @false)
         @br @bold(NOTE) The stream will be copied from its current position
        }
        class function StreamTo(hFormat: THandle; pStream: TStream; len: NativeUInt): Boolean; static;

        {**
         Copy a string to the clipboard
         @param(hFormat Format handle that represent the string inside the clipboard)
         @param(str String to copy to clipboard)
         @returns(@true on success, otherwise @false)
        }
        class function StringTo(hFormat: THandle; const str: UnicodeString): Boolean; static;

        {**
         Copy a text to the clipboard
         @param(text Text to copy to clipboard)
         @returns(@true on success, otherwise @false)
        }
        class function TextTo(const text: UnicodeString): Boolean; static;
    end;

    {**
     Available DPI awareness contexts
    }
    EDPIAwarenessContext =
    (
        E_AC_None,
        E_AC_Unaware,
        E_AC_System,
        E_AC_Per_Monitor,
        E_AC_Per_Monitor_V2
    );

    {**
     Helper class for VCL
    }
    TWVCLHelper = record
        {**
         Get form owning a control
         @param(pControl Control for which owning form should be get)
         @returns(Form owning control, nil if not found or on error)
        }
        class function GetParentForm(pControl: TControl): TCustomForm; static;

        {**
         Get closest parent owning a control
         @param(pControl Control for which owning parent should be get)
         @returns(classID Parent class identifier to find)
        }
        class function GetNextParent(pControl: TControl; classID: TClass): TObject; static;

        {**
         Get pixels per inch reference (i.e declared in design time)
         @param(pOwner Component owning this image list)
         @returns(Pixels per inch)
        }
        class function GetPixelsPerInchRef(pOwner: TComponent): Integer; static;

        {**
         Get the system pixels per inch
         @returns(The system pixels per inch)
        }
        class function GetSystemPixelsPerInch: Integer; static;

        {**
         Get pixels per inch on current running app monitor
         @param(pComponent Component located on monitor for which the current DPI should be get)
         @param(refDPI Pixels per inch reference, i.e DPI at which interface was designed)
         @param(fGetDpiForMonitor Get DPI for monitor callback to call)
         @returns(Pixels per inch)
        }
        {$if CompilerVersion < 30}
            class function GetMonitorPixelsPerInch(pComponent: TComponent;
                    refDPI: Integer; fGetDpiForMonitor: TWfGetDpiForMonitor): Integer; static;
        {$else}
            class function GetMonitorPixelsPerInch(pComponent: TComponent;
                    refDPI: Integer): Integer; static;
        {$ifend}

        {**
         Scale value based on DPI
         @param(value Value to scale)
         @param(dpi Current pixels per inch value)
         @param(refDPI Pixels per inch reference, i.e DPI at which interface was designed)
         @returns(Scaled value)
        }
        class function ScaleByDPI(value, dpi, refDPI: Integer): Integer; static;

        {**
         Get if application is DPI aware
         @param(hProcess Process for which DPI awareness should be tested)
         @param(fGetProcessDpiAwareness Get process DPI awareness callback to call)
         @returns(@true if system is DPI aware, otherwise @false)
        }
        {$if CompilerVersion < 30}
            class function IsDPIAware(hProcess: THandle; fGetProcessDpiAwareness: TWfGetProcessDpiAwareness): Boolean; static;
        {$else}
            class function IsDPIAware(hProcess: THandle): Boolean; static;
        {$ifend}

        {**
         Get DPI awareness context
         @param(fGetProcessDpiAwareness Get process DPI awareness callback to call)
         @param(pControl Control or form for which DPI awareness should be tested, can be @nil)
         @returns(DPI awareness context)
        }
        {$if CompilerVersion < 30}
            class function GetDPIAwarenessContext(fGetProcessDpiAwareness: TWfGetProcessDpiAwareness):
                    EDPIAwarenessContext; static;
        {$else}
            class function GetDPIAwarenessContext(pControl: TWinControl): EDPIAwarenessContext; static;
        {$ifend}
    end;

    {**
     Helper class for message logging
    }
    TWLogHelper = record
        {**
         Dump the content of a stream to the compiler console
         @param(pStream Stream containing the data to dump)
         @param(maxLimit Maximum size limit in bytes before which the log will be truncated, ignored if 0)
        }
        class procedure Dump(const pStream: TStream; maxLimit: NativeUInt); static;

        {**
         Convert Windows message to string
         @param(pOwner Component that owns the message)
         @param(message Windows message to convert)
         @return(Windows message as string)
        }
        class function WinMsgToStr(pOwner: TComponent; const message: TMessage): UnicodeString; static;

        {**
         Convert GDI+ status error to string
         @param(status Status to convert)
         @return(Status as string)
        }
        class function GdiPlusStatusToStr(status: TStatus): UnicodeString; static;

        {**
         Convert GDI+ matrix content to string
         @param(pMatrix Matrix to convert)
         @return(Matrix as string)
        }
        class function GdiPlusMatrixToStr(pMatrix: TGpMatrix): UnicodeString; static;

        {**
         Log a message to the compiler console
         @param(msg Message to log)
        }
        class procedure LogToCompiler(const msg: UnicodeString); static;

        {**
         Log a block title to the compiler console
         @param(title Title to log)
        }
        class procedure LogBlockToCompiler(const title: UnicodeString); static;
    end;

var
    g_GDICacheController: TWGDICacheController;

implementation
//---------------------------------------------------------------------------
// TWStringHelper
//---------------------------------------------------------------------------
class function TWStringHelper.Substr(const str: UnicodeString; index: Integer): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        // NOTE + 1 to compensate 1 based UnicodeString indexes
        Result := Copy(str, index + 1, Length(str));
    {$else}
        Result := str.Substring(index);
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.Substr(const str: UnicodeString; index, length: Integer): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        // NOTE + 1 to compensate 1 based UnicodeString indexes
        Result := Copy(str, index + 1, length);
    {$else}
        Result := str.Substring(index, length);
    {$ifend}
end;
//---------------------------------------------------------------------------
class procedure TWStringHelper.Split(const delimiter: WideChar; const str: UnicodeString; strings: TStrings);
begin
    if (not Assigned(strings)) then
        Exit;

   strings.Clear;
   strings.Delimiter       := delimiter;
   strings.StrictDelimiter := True;
   strings.DelimitedText   := str;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ToUpper(const str: UnicodeString): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        Result := WideUpperCase(str);
    {$else}
        Result := str.ToUpper;
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ToLower(const str: UnicodeString): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        Result := WideLowerCase(str);
    {$else}
        Result := str.ToLower;
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.IndexOf(const str: UnicodeString; ch: Char): Integer;
begin
    {$if CompilerVersion <= 24}
        // NOTE -1 to compensate 1 based UnicodeString indexes
        Result := System.Pos(ch, str) - 1;
    {$else}
        Result := str.IndexOf(ch);
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.IsEmpty(const str: UnicodeString): Boolean;
begin
    {$if CompilerVersion <= 24}
        Result := (str = Empty);
    {$else}
        Result := str.IsEmpty;
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.Trim(const str: UnicodeString): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        // NOTE full qualified name is required here to avoid to call TWStringHelper.Trim() infinitely
        Result := System.SysUtils.Trim(str);
    {$else}
        Result := str.Trim;
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWStringHelper.StrToBool(const str: UnicodeString): Boolean;
begin
    Result := ((str = '-1') or (str = '1') or (str = 'TRUE') or (str = 'True') or (str = 'true'));
end;
//---------------------------------------------------------------------------
class function TWStringHelper.BoolToStr(value, asString, compensate: Boolean): UnicodeString;
begin
    if (not asString) then
        if (value) then
            Exit('1')
        else
            Exit('0');

    if (compensate) then
    begin
        if (value) then
            Exit('TRUE ')
        else
            Exit('FALSE');
    end
    else
        if (value) then
            Exit('TRUE')
        else
            Exit('FALSE');
end;
//---------------------------------------------------------------------------
class function TWStringHelper.IsNumeric(digit: AnsiChar; isStrict: Boolean): Boolean;
begin
    Result := (((digit >= '0') and (digit <= '9')) or ((not isStrict) and ((digit = '-') or (digit = '.'))));
end;
//---------------------------------------------------------------------------
class function TWStringHelper.IsNumeric(digit: WideChar; isStrict: Boolean): Boolean;
begin
    Result := (((digit >= '0') and (digit <= '9')) or ((not isStrict) and ((digit = '-') or (digit = '.'))));
end;
//---------------------------------------------------------------------------
class function TWStringHelper.IsNumOrAscii(digit: WideChar; numOnly: Boolean): Boolean;
begin
    Result := (((digit >= '0') and (digit <= '9')) or ((not numOnly) and ((digit >= 'a') and (digit <= 'z'))
            or ((digit >= 'A') and (digit <= 'Z'))));
end;
//---------------------------------------------------------------------------
class function TWStringHelper.CanWrap(const c: WideChar): Boolean;
begin
    // if ((c >= WideChar($0020)) and (c <= WideChar($007F))) then Exit(True);    // Basic Latin
    // if ((c >= WideChar($00A0)) and (c <= WideChar($00FF))) then Exit(True);    // Latin-1 Supplement
    // if ((c >= WideChar($0100)) and (c <= WideChar($017F))) then Exit(True);    // Latin Extended-A
    // if ((c >= WideChar($0180)) and (c <= WideChar($024F))) then Exit(True);    // Latin Extended-B
    // if ((c >= WideChar($0250)) and (c <= WideChar($02AF))) then Exit(True);    // IPA Extensions
    // if ((c >= WideChar($02B0)) and (c <= WideChar($02FF))) then Exit(True);    // Spacing Modifier Letters
    // if ((c >= WideChar($0300)) and (c <= WideChar($036F))) then Exit(True);    // Combining Diacritical Marks
    // if ((c >= WideChar($0370)) and (c <= WideChar($03FF))) then Exit(True);    // Greek and Coptic
    // if ((c >= WideChar($0400)) and (c <= WideChar($04FF))) then Exit(True);    // Cyrillic
    // if ((c >= WideChar($0500)) and (c <= WideChar($052F))) then Exit(True);    // Cyrillic Supplementary
    // if ((c >= WideChar($0530)) and (c <= WideChar($058F))) then Exit(True);    // Armenian
    // if ((c >= WideChar($0590)) and (c <= WideChar($05FF))) then Exit(True);    // Hebrew
    // if ((c >= WideChar($0600)) and (c <= WideChar($06FF))) then Exit(True);    // Arabic
    // if ((c >= WideChar($0700)) and (c <= WideChar($074F))) then Exit(True);    // Syriac
    // if ((c >= WideChar($0780)) and (c <= WideChar($07BF))) then Exit(True);    // Thaana
    // if ((c >= WideChar($0900)) and (c <= WideChar($097F))) then Exit(True);    // Devanagari
    // if ((c >= WideChar($0980)) and (c <= WideChar($09FF))) then Exit(True);    // Bengali
    // if ((c >= WideChar($0A00)) and (c <= WideChar($0A7F))) then Exit(True);    // Gurmukhi
    // if ((c >= WideChar($0A80)) and (c <= WideChar($0AFF))) then Exit(True);    // Gujarati
    // if ((c >= WideChar($0B00)) and (c <= WideChar($0B7F))) then Exit(True);    // Oriya
    // if ((c >= WideChar($0B80)) and (c <= WideChar($0BFF))) then Exit(True);    // Tamil
    // if ((c >= WideChar($0C00)) and (c <= WideChar($0C7F))) then Exit(True);    // Telugu
    // if ((c >= WideChar($0C80)) and (c <= WideChar($0CFF))) then Exit(True);    // Kannada
    // if ((c >= WideChar($0D00)) and (c <= WideChar($0D7F))) then Exit(True);    // Malayalam
    // if ((c >= WideChar($0D80)) and (c <= WideChar($0DFF))) then Exit(True);    // Sinhala
    // if ((c >= WideChar($0E00)) and (c <= WideChar($0E7F))) then Exit(True);    // Thai
    // if ((c >= WideChar($0E80)) and (c <= WideChar($0EFF))) then Exit(True);    // Lao
    // if ((c >= WideChar($0F00)) and (c <= WideChar($0FFF))) then Exit(True);    // Tibetan
    // if ((c >= WideChar($1000)) and (c <= WideChar($109F))) then Exit(True);    // Myanmar
    // if ((c >= WideChar($10A0)) and (c <= WideChar($10FF))) then Exit(True);    // Georgian
    // if ((c >= WideChar($1100)) and (c <= WideChar($11FF))) then Exit(True);    // Hangul Jamo
    // if ((c >= WideChar($1200)) and (c <= WideChar($137F))) then Exit(True);    // Ethiopic
    // if ((c >= WideChar($13A0)) and (c <= WideChar($13FF))) then Exit(True);    // Cherokee
    // if ((c >= WideChar($1400)) and (c <= WideChar($167F))) then Exit(True);    // Unified Canadian Aboriginal Syllabics
    // if ((c >= WideChar($1680)) and (c <= WideChar($169F))) then Exit(True);    // Ogham
    // if ((c >= WideChar($16A0)) and (c <= WideChar($16FF))) then Exit(True);    // Runic
    // if ((c >= WideChar($1700)) and (c <= WideChar($171F))) then Exit(True);    // Tagalog
    // if ((c >= WideChar($1720)) and (c <= WideChar($173F))) then Exit(True);    // Hanunoo
    // if ((c >= WideChar($1740)) and (c <= WideChar($175F))) then Exit(True);    // Buhid
    // if ((c >= WideChar($1760)) and (c <= WideChar($177F))) then Exit(True);    // Tagbanwa
    // if ((c >= WideChar($1780)) and (c <= WideChar($17FF))) then Exit(True);    // Khmer
    // if ((c >= WideChar($1800)) and (c <= WideChar($18AF))) then Exit(True);    // Mongolian
    // if ((c >= WideChar($1900)) and (c <= WideChar($194F))) then Exit(True);    // Limbu
    // if ((c >= WideChar($1950)) and (c <= WideChar($197F))) then Exit(True);    // Tai Le
    // if ((c >= WideChar($19E0)) and (c <= WideChar($19FF))) then Exit(True);    // Khmer Symbols
    // if ((c >= WideChar($1D00)) and (c <= WideChar($1D7F))) then Exit(True);    // Phonetic Extensions
    // if ((c >= WideChar($1E00)) and (c <= WideChar($1EFF))) then Exit(True);    // Latin Extended Additional
    // if ((c >= WideChar($1F00)) and (c <= WideChar($1FFF))) then Exit(True);    // Greek Extended
    // if ((c >= WideChar($2000)) and (c <= WideChar($206F))) then Exit(True);    // General Punctuation
    // if ((c >= WideChar($2070)) and (c <= WideChar($209F))) then Exit(True);    // Superscripts and Subscripts
    // if ((c >= WideChar($20A0)) and (c <= WideChar($20CF))) then Exit(True);    // Currency Symbols
    // if ((c >= WideChar($20D0)) and (c <= WideChar($20FF))) then Exit(True);    // Combining Diacritical Marks for Symbols
    // if ((c >= WideChar($2100)) and (c <= WideChar($214F))) then Exit(True);    // Letterlike Symbols
    if ((c >= WideChar($2150)) and (c <= WideChar($218F))) then Exit(True);    // Number Forms
    if ((c >= WideChar($2190)) and (c <= WideChar($21FF))) then Exit(True);    // Arrows
    if ((c >= WideChar($2200)) and (c <= WideChar($22FF))) then Exit(True);    // Mathematical Operators
    if ((c >= WideChar($2300)) and (c <= WideChar($23FF))) then Exit(True);    // Miscellaneous Technical
    if ((c >= WideChar($2400)) and (c <= WideChar($243F))) then Exit(True);    // Control Pictures
    if ((c >= WideChar($2440)) and (c <= WideChar($245F))) then Exit(True);    // Optical Character Recognition
    if ((c >= WideChar($2460)) and (c <= WideChar($24FF))) then Exit(True);    // Enclosed Alphanumerics
    if ((c >= WideChar($2500)) and (c <= WideChar($257F))) then Exit(True);    // Box Drawing
    if ((c >= WideChar($2580)) and (c <= WideChar($259F))) then Exit(True);    // Block Elements
    if ((c >= WideChar($25A0)) and (c <= WideChar($25FF))) then Exit(True);    // Geometric Shapes
    if ((c >= WideChar($2600)) and (c <= WideChar($26FF))) then Exit(True);    // Miscellaneous Symbols
    if ((c >= WideChar($2700)) and (c <= WideChar($27BF))) then Exit(True);    // Dingbats
    if ((c >= WideChar($27C0)) and (c <= WideChar($27EF))) then Exit(True);    // Miscellaneous Mathematical Symbols-A
    // if ((c >= WideChar($27F0)) and (c <= WideChar($27FF))) then Exit(True);    // Supplemental Arrows-A
    // if ((c >= WideChar($2800)) and (c <= WideChar($28FF))) then Exit(True);    // Braille Patterns
    // if ((c >= WideChar($2900)) and (c <= WideChar($297F))) then Exit(True);    // Supplemental Arrows-B
    // if ((c >= WideChar($2980)) and (c <= WideChar($29FF))) then Exit(True);    // Miscellaneous Mathematical Symbols-B
    // if ((c >= WideChar($2A00)) and (c <= WideChar($2AFF))) then Exit(True);    // Supplemental Mathematical Operators
    // if ((c >= WideChar($2B00)) and (c <= WideChar($2BFF))) then Exit(True);    // Miscellaneous Symbols and Arrows
    if ((c >= WideChar($2E80)) and (c <= WideChar($2EFF))) then Exit(True);    // CJK Radicals Supplement
    if ((c >= WideChar($2F00)) and (c <= WideChar($2FDF))) then Exit(True);    // Kangxi Radicals
    if ((c >= WideChar($2FF0)) and (c <= WideChar($2FFF))) then Exit(True);    // Ideographic Description Characters
    // if ((c >= WideChar($3000)) and (c <= WideChar($303F))) then Exit(True);    // CJK Symbols and Punctuation
    if ((c >= WideChar($3040)) and (c <= WideChar($309F))) then Exit(True);    // Hiragana
    if ((c >= WideChar($30A0)) and (c <= WideChar($30FF))) then Exit(True);    // Katakana
    if ((c >= WideChar($3100)) and (c <= WideChar($312F))) then Exit(True);    // Bopomofo
    if ((c >= WideChar($3130)) and (c <= WideChar($318F))) then Exit(True);    // Hangul Compatibility Jamo
    if ((c >= WideChar($3190)) and (c <= WideChar($319F))) then Exit(True);    // Kanbun
    if ((c >= WideChar($31A0)) and (c <= WideChar($31BF))) then Exit(True);    // Bopomofo Extended
    if ((c >= WideChar($31F0)) and (c <= WideChar($31FF))) then Exit(True);    // Katakana Phonetic Extensions
    if ((c >= WideChar($3200)) and (c <= WideChar($32FF))) then Exit(True);    // Enclosed CJK Letters and Months
    if ((c >= WideChar($3300)) and (c <= WideChar($33FF))) then Exit(True);    // CJK Compatibility
    if ((c >= WideChar($3400)) and (c <= WideChar($4DBF))) then Exit(True);    // CJK Unified Ideographs Extension A
    if ((c >= WideChar($4DC0)) and (c <= WideChar($4DFF))) then Exit(True);    // Yijing Hexagram Symbols
    if ((c >= WideChar($4E00)) and (c <= WideChar($9FFF))) then Exit(True);    // CJK Unified Ideographs
    if ((c >= WideChar($A000)) and (c <= WideChar($A48F))) then Exit(True);    // Yi Syllables
    if ((c >= WideChar($A490)) and (c <= WideChar($A4CF))) then Exit(True);    // Yi Radicals
    if ((c >= WideChar($AC00)) and (c <= WideChar($D7AF))) then Exit(True);    // Hangul Syllables
    if ((c >= WideChar($D800)) and (c <= WideChar($DB7F))) then Exit(True);    // High Surrogates
    if ((c >= WideChar($DB80)) and (c <= WideChar($DBFF))) then Exit(True);    // High Private Use Surrogates
    if ((c >= WideChar($DC00)) and (c <= WideChar($DFFF))) then Exit(True);    // Low Surrogates
    // if ((c >= WideChar($E000)) and (c <= WideChar($F8FF))) then Exit(True);    // Private Use Area
    if ((c >= WideChar($F900)) and (c <= WideChar($FAFF))) then Exit(True);    // CJK Compatibility Ideographs
    // if ((c >= WideChar($FB00)) and (c <= WideChar($FB4F))) then Exit(True);    // Alphabetic Presentation Forms
    // if ((c >= WideChar($FB50)) and (c <= WideChar($FDFF))) then Exit(True);    // Arabic Presentation Forms-A
    // if ((c >= WideChar($FE00)) and (c <= WideChar($FE0F))) then Exit(True);    // Variation Selectors
    // if ((c >= WideChar($FE20)) and (c <= WideChar($FE2F))) then Exit(True);    // Combining Half Marks
    // if ((c >= WideChar($FE30)) and (c <= WideChar($FE4F))) then Exit(True);    // CJK Compatibility Forms
    // if ((c >= WideChar($FE50)) and (c <= WideChar($FE6F))) then Exit(True);    // Small Form Variants
    // if ((c >= WideChar($FE70)) and (c <= WideChar($FEFF))) then Exit(True);    // Arabic Presentation Forms-B
    // if ((c >= WideChar($FF00)) and (c <= WideChar($FFEF))) then Exit(True);    // Halfwidth and Fullwidth Forms
    // if ((c >= WideChar($FFF0)) and (c <= WideChar($FFFF))) then Exit(True);    // Specials
    // if ((c >= WideChar($10000)) and (c <= WideChar($1007F))) then Exit(True); // Linear B Syllabary
    // if ((c >= WideChar($10080)) and (c <= WideChar($100FF))) then Exit(True); // Linear B Ideograms
    // if ((c >= WideChar($10100)) and (c <= WideChar($1013F))) then Exit(True); // Aegean Numbers
    // if ((c >= WideChar($10300)) and (c <= WideChar($1032F))) then Exit(True); // Old Italic
    // if ((c >= WideChar($10330)) and (c <= WideChar($1034F))) then Exit(True); // Gothic
    // if ((c >= WideChar($10380)) and (c <= WideChar($1039F))) then Exit(True); // Ugaritic
    // if ((c >= WideChar($10400)) and (c <= WideChar($1044F))) then Exit(True); // Deseret
    // if ((c >= WideChar($10450)) and (c <= WideChar($1047F))) then Exit(True); // Shavian
    // if ((c >= WideChar($10480)) and (c <= WideChar($104AF))) then Exit(True); // Osmanya
    // if ((c >= WideChar($10800)) and (c <= WideChar($1083F))) then Exit(True); // Cypriot Syllabary
    // if ((c >= WideChar($1D000)) and (c <= WideChar($1D0FF))) then Exit(True); // Byzantine Musical Symbols
    // if ((c >= WideChar($1D100)) and (c <= WideChar($1D1FF))) then Exit(True); // Musical Symbols
    // if ((c >= WideChar($1D300)) and (c <= WideChar($1D35F))) then Exit(True); // Tai Xuan Jing Symbols
    // if ((c >= WideChar($1D400)) and (c <= WideChar($1D7FF))) then Exit(True); // Mathematical Alphanumeric Symbols
    { ONLY for unicode32
    if ((c >= WideChar($20000)) and (c <= WideChar($2A6DF))) then Exit(True); // CJK Unified Ideographs Extension B
    if ((c >= WideChar($2F800)) and (c <= WideChar($2FA1F))) then Exit(True); // CJK Compatibility Ideographs Supplement
    }
    // if ((c >= WideChar($E0000)) and (c <= WideChar($E007F))) then Exit(True); // Tags

    Result := False;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.DelimitStr(const str: UnicodeString; sideLen: NativeInt;
        delimiter: WideChar): UnicodeString;
begin
    // add delimiters
    Result := StringOfChar(delimiter, sideLen) + ' ' + str + ' ' + StringOfChar(delimiter, sideLen);

    // add an extra delimiter, if str is of odd length
    if ((Length(str) mod 2) <> 0) then
        Result := Result + delimiter;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.DelimitFillStr(const str: UnicodeString; wantedLen: NativeInt;
        delimiter: WideChar): UnicodeString;
var
    missing, count: NativeUInt;
begin
    // check if wanted len not bigger than the string
    if (Length(str) >= wantedLen) then
        Exit(Str);

    // number of delimiters on each side of the string
    missing := wantedLen - Length(str);
    count   := missing div 2;

    // add delimiters
    Result := StringOfChar(delimiter, count) + str + StringOfChar(delimiter, count);

    // add an extra delimiter, if odd length
    if ((missing mod 2) <> 0) then
        Result := Result + delimiter;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.FillStrLeft(const str: UnicodeString; fillLen: NativeInt;
        delimiter: WideChar): UnicodeString;
begin
    if (Length(str) >= fillLen) then
        Exit(str)
    else
        Result := StringOfChar(delimiter, fillLen - Length(str)) + str;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.FillStrRight(const str: UnicodeString; fillLen: NativeInt;
        delimiter: WideChar): UnicodeString;
begin
    if (Length(str) >= fillLen) then
        Exit(str)
    else
        Result := str + StringOfChar(delimiter, fillLen - Length(str));
end;
//---------------------------------------------------------------------------
class function TWStringHelper.SkipChars(const str, chars: UnicodeString; var pos: NativeInt): Boolean;
begin
    while ((pos <= Length(str)) and (IndexOf(chars, str[pos]) <> -1)) do
        Inc(pos);

    Result := pos <= Length(str);
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ReadInt<TInt>(const str: UnicodeString; var pos: NativeInt; var resVal: TInt): Boolean;
var
    validNumPos: NativeInt;
begin
    Result := ReadIntEx<TInt>(str, pos, validNumPos, resVal, False);
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ReadIntEx<TInt>(const str: UnicodeString; var pos, validNumPos: NativeInt;
        var resVal: TInt; forFractalPart: Boolean): Boolean;
var
    genericVal:               TValue;
    tmp, resultTmp, start, i: NativeInt;
    sign, zeroOffset:         Integer;
    ch:                       WideChar;
    skip, overflow:           Boolean;
begin
    if (Length(str) = 0) then
        Exit(False);

    resultTmp := 0;

    i := pos;

    // check sign
    if (str[i] = '-') then
    begin
        sign := -1;
        Inc(i);
    end
    else
        sign := 1;

    skip       := False;
    start      := i;
    zeroOffset := Ord('0');

    while (i <= Length(str)) do
    begin
        ch := str[i];

        if ((ch >= '0') and (ch <= '9')) then
        begin
            if (forFractalPart) then
            begin
                if (skip) then
                begin
                    Inc(i);
                    continue;
                end;

                tmp := resultTmp;

                // shift by 10 to left
                tmp := tmp * 10;

                overflow := False;

                // check for overflow
                if ((tmp div 10) <> resultTmp) then
                    overflow := True
                else
                    // convert char to num and add to result
                    Inc(tmp, (Ord(ch) - zeroOffset));

                // check if overflowed, in this case skip numbers
                if (not overflow and (tmp >= resultTmp)) then
                    resultTmp := tmp
                else
                begin
                    // skip overflow
                    skip        := True;
                    validNumPos := i;
                end;
            end
            else
            begin
                // shift by 10 to left
                resultTmp := resultTmp * 10;
                Inc(resultTmp, (Ord(ch) - zeroOffset));
            end;
        end
        else
            break;

        Inc(i);
    end;

    // processed anything meaningful?
    if (i <= start) then
        Exit(False);

    // apply sign and set final value
    genericVal := TValue.From<NativeInt>(sign * resultTmp);
    resVal     := genericVal.AsType<TInt>;
    pos        := i;

    if (not skip) then
        validNumPos := i;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ReadFloat<TFlt, TInt>(const str: UnicodeString; var pos: NativeInt;
        var resVal: TFlt): Boolean;
var
    decimalPoint, intPartReaded, fractReaded:   Boolean;
    exponent, intPart, fractPartTmp:            TInt;
    zeroVal, genVal:                            TValue;
    fracPos, intExp, start, fractStart, len, i: NativeInt;
    fractPart, sign, exp, number:               Double;
    ch:                                         WideChar;
begin
    // must not be empty
    if (Length(str) = 0) then
        Exit(False);

    decimalPoint  := False;
    intPartReaded := False;
    fractReaded   := False;
    zeroVal       := TValue.From<Integer>(0);
    exponent      := zeroVal.AsType<TInt>;
    intPart       := zeroVal.AsType<TInt>;
    fractPart     := 0.0;
    i             := pos;

    // check for sign
    if (str[i] = '-') then
    begin
        sign := -1.0;
        Inc(i);
    end
    else
        sign := 1.0;

    start := i;

    // iterate characters and read numbers
    while (i <= Length(str)) do
    begin
        ch := str[i];

        case (ch) of
            // decimal point?
            '.':
            begin
                if (decimalPoint) then
                    // second decimal point, we are done
                    break;

                decimalPoint := True;

                Inc(i);
                continue;
            end;

            // exponent?
            'e',
            'E':
            begin
                // must not start with it
                if (i <= start) then
                    break;

                Inc(i);

                // read exponent
                if (not ReadInt<TInt>(str, i, exponent)) then
                    Exit(False);

                // exponent marks end of number
                break;
            end;
        else
            // we are done if franctional part is read
            if (fractReaded) then
                break;

            // did we already read the decimal point?
            if (decimalPoint) then
            begin
                fractStart := i;

                // read fract part
                if (not ReadIntEx<TInt>(str, i, fracPos, fractPartTmp, True)) then
                    Exit(False);

                // get number of digits of fractional part
                len := (fracPos - fractStart);

                // not to big?
                if (Length(m_NegExpMultip) < len) then
                    Exit(False);

                // add decimal point
                genVal    := TValue.From<TInt>(fractPartTmp);
                fractPart := genVal.AsType<Double> * m_NegExpMultip[len];

                // done unless exponent follows
                fractReaded := True;
            end
            else
            // read integer part if not already read
            if (not intPartReaded) then
            begin
                if (not ReadInt<TInt>(str, i, intPart)) then
                    Exit(False);

                intPartReaded := True;
            end
            else
                break;

            continue;
        end;

        break;
    end;

    // processed anything meaningful?
    if (i <= start) then
        Exit(False);

    // add integer and fractional parts and apply sign
    genVal := TValue.From<TInt>(intPart);
    number := sign * (genVal.AsType<Double> + fractPart);

    genVal := TValue.From<TInt>(exponent);
    intExp := genVal.AsType<NativeInt>;

    // apply exponent if any
    if (intExp <> 0) then
    begin
        // if positive, use positive memory table to compute multplier
        if (intExp >= 0) then
        begin
            // not to big?
            if (Length(m_PosExpMultip) < intExp) then
                Exit(False);

            exp := m_PosExpMultip[intExp];
        end
        // negative exponent, use negative memory table to compute multplier
        else
        begin
            intExp := intExp * -1;

            // not to big?
            if (Length(m_NegExpMultip) < intExp) then
                Exit(False);

            exp := m_NegExpMultip[intExp];
        end;

        number := number * exp;
    end;

    // convert back final number to generic result
    genVal := TValue.From<Double>(number);
    resVal := genVal.AsType<TFlt>;
    pos    := i;
    Result := True;
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ReadFloat(const str: UnicodeString; var pos: NativeInt;
        var resVal: Single): Boolean;
begin
    Result := ReadFloat<Single, Integer>(str, pos, resVal);
end;
//---------------------------------------------------------------------------
class function TWStringHelper.ReadFloat(const str: UnicodeString; var pos: NativeInt;
        var resVal: Double): Boolean;
begin
    Result := ReadFloat<Double, NativeInt>(str, pos, resVal);
end;
//---------------------------------------------------------------------------
// TWFileHelper
//---------------------------------------------------------------------------
class function TWFileHelper.GetFSPrefixedName(const path: UnicodeString): UnicodeString;
{$ifdef Win32}
    var
        len: NativeUInt;
{$endif}
begin
    // todo -cspeed -oNiki: check the performance of this function, maybe return reference
    {$ifdef Win32}
        len := Length(path);

        // check if the file is relative
        if (len < 2) then
            Exit(path);

        if (path[1] = '\\') then
        begin
            if (path[2] = '\\') then
            begin
                if ((len > 2) and (path[3] = '?')) then
                    // there is already a prefix, do not modify
                    Exit(path);

                // a network path, append UNC prefix
                {$if CompilerVersion <= 24}
                    // NOTE + 1 to compensate 1 based UnicodeString indexes
                    Exit(C_TWFileHelper_FSUNCPrefix + Copy(path, 2, len));
                {$else}
                    Exit(C_TWFileHelper_FSUNCPrefix + path.Substring(1));
                {$ifend}
            end;

            // the path must be relative
            Exit(path);
        end
        else
        if ((path[2] = ':') and (len > 2)) then
            // assume it has a drive letter, append prefix
            Exit(C_TWFileHelper_FSDrivePrefix + path);

        // path must be relative ('mydir/file.ext') or just a drive letter ('c:')
        Result := path;
    {$else}
        Result := path;
    {$endif}
end;
//---------------------------------------------------------------------------
class function TWFileHelper.GetFileVersion(const fileName: string): string;
var
    infoSize:      DWORD;
    verBuf:        pointer;
    verSize, wnd:  UINT;
    FixedFileInfo: PVSFixedFileInfo;
begin
    infoSize := GetFileVersioninfoSize(PChar(fileName), wnd);

    Result := '';

    if (infoSize <> 0) then
    begin
        GetMem(verBuf, infoSize);

        try
            if (GetFileVersionInfo(PChar(fileName), wnd, infoSize, verBuf)) then
            begin
                VerQueryValue(verBuf, '\', Pointer(FixedFileInfo), verSize);

                Result := IntToStr(FixedFileInfo.dwFileVersionMS div $10000) + '.' +
                          IntToStr(FixedFileInfo.dwFileVersionMS and $0FFFF) + '.' +
                          IntToStr(FixedFileInfo.dwFileVersionLS div $10000) + '.' +
                          IntToStr(FixedFileInfo.dwFileVersionLS and $0FFFF);
            end;
        finally
            FreeMem(verBuf);
        end;
    end;
end;
//---------------------------------------------------------------------------
class function TWFileHelper.GetFileVersion(const fileName: AnsiString; pFileVersion, pProductVersion: TWVersion): Boolean;
var
    dwHandle, infoStructSize: DWORD;
    lpData:                   LPTSTR;
    pFileInfo:                PVSFixedFileInfo;
    buffSize:                 UINT;
begin
    if ((not Assigned(pFileVersion)) or (not Assigned(pProductVersion))) then
        Exit(False);

    pFileVersion.Clear;
    pProductVersion.Clear;

    // get file info structure size
    infoStructSize := GetFileVersionInfoSizeA(PAnsiChar(fileName), dwHandle);

    if (infoStructSize = 0) then
        Exit(False);

    // allocate memory for structure
    GetMem(lpData, infoStructSize);

    if (not Assigned(lpData)) then
        Exit(False);

    // get file version structure
    Result := GetFileVersionInfoA(PAnsiChar(fileName), 0, infoStructSize, lpData);

    if (not Result) then
    begin
        // free memory
        FreeMem(lpData);
        Exit(False);
    end;

    // get version values
    if (VerQueryValueA(lpData, '\\', Pointer(pFileInfo), buffSize) = FALSE) then
    begin
        // free memory
        FreeMem(lpData);
        Exit(False);
    end;

    // copy values
    pFileVersion.Major      := HIWORD(pFileInfo.dwFileVersionMS);
    pFileVersion.Minor      := LOWORD(pFileInfo.dwFileVersionMS);
    pFileVersion.Release    := HIWORD(pFileInfo.dwFileVersionLS);
    pFileVersion.Build      := LOWORD(pFileInfo.dwFileVersionLS);

    pProductVersion.Major   := HIWORD(pFileInfo.dwProductVersionMS);
    pProductVersion.Minor   := LOWORD(pFileInfo.dwProductVersionMS);
    pProductVersion.Release := HIWORD(pFileInfo.dwProductVersionLS);
    pProductVersion.Build   := LOWORD(pFileInfo.dwProductVersionLS);

    // free memory
    FreeMem(lpData);

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWFileHelper.GetFileVersion(const fileName: UnicodeString; pFileVersion, pProductVersion: TWVersion): Boolean;
var
    dwHandle, infoStructSize: DWORD;
    lpData:                   LPTSTR;
    pFileInfo:                PVSFixedFileInfo;
    buffSize:                 UINT;
    prefixedFileName:         UnicodeString;
begin
    if ((not Assigned(pFileVersion)) or (not Assigned(pProductVersion))) then
        Exit(False);

    pFileVersion.Clear;
    pProductVersion.Clear;

    prefixedFileName := GetFSPrefixedName(fileName);

    // get file info structure size
    infoStructSize := GetFileVersionInfoSizeW(PWChar(prefixedFileName), dwHandle);

    if (infoStructSize = 0) then
        Exit(False);

    // allocate memory for structure
    GetMem(lpData, infoStructSize);

    if (not Assigned(lpData)) then
        Exit(False);

    // get file version structure
    Result := GetFileVersionInfoW(PWChar(prefixedFileName), 0, infoStructSize, lpData);

    if (not Result) then
    begin
        // free memory
        FreeMem(lpData);
        Exit(False);
    end;

    // get version values
    if (VerQueryValueW(lpData, '\\', Pointer(pFileInfo), buffSize) = FALSE) then
    begin
        // free memory
        FreeMem(lpData);
        Exit(False);
    end;

    // copy values
    pFileVersion.Major      := HIWORD(pFileInfo.dwFileVersionMS);
    pFileVersion.Minor      := LOWORD(pFileInfo.dwFileVersionMS);
    pFileVersion.Release    := HIWORD(pFileInfo.dwFileVersionLS);
    pFileVersion.Build      := LOWORD(pFileInfo.dwFileVersionLS);

    pProductVersion.Major   := HIWORD(pFileInfo.dwProductVersionMS);
    pProductVersion.Minor   := LOWORD(pFileInfo.dwProductVersionMS);
    pProductVersion.Release := HIWORD(pFileInfo.dwProductVersionLS);
    pProductVersion.Build   := LOWORD(pFileInfo.dwProductVersionLS);

    // free memory
    FreeMem(lpData);

    Result := True;
end;
//---------------------------------------------------------------------------
// TWMemoryHelper
//---------------------------------------------------------------------------
{$HINTS OFF}
  class function TWMemoryHelper.IsSystemBE: Boolean;
  type
      // this record is more or less a transcription of an union type in c++
      IEndianness = record
          case Boolean of
              True:  (i: TWUInt32);
              False: (p: array[1..4] of TWUInt8);
      end;
  var
      bInt: ^IEndianness;
  begin
      Result := False;
      bInt   := nil;

      try
          // set a value of 5 inside the endianness structure
          New(bInt);
          bInt^.i := 5;

          // check whether value is on the first or last byte, and thus determine system endianness
          if (bInt^.p[1] = 5) then
              Result := False
          else
          if (bInt^.p[4] = 5) then
              Result := True
          else
              raise Exception.Create('Cannot determine system endianness');
      finally
          if (Assigned(bInt)) then
              Dispose(bInt);
      end;
  end;
{$HINTS ON}
//---------------------------------------------------------------------------
class procedure TWMemoryHelper.Swap<T>(var left, right: T);
var
    value: T;
begin
    value := left;
    left  := right;
    right := value;
end;
//---------------------------------------------------------------------------
// TWGDIHelper.IRectOptions
//---------------------------------------------------------------------------
class function TWGDIHelper.IRectOptions.GetDefault: IRectOptions;
begin
    Result           := Default(TWGDIHelper.IRectOptions);
    Result.m_Color   := clBlack;
    Result.m_Outline := clBlack;
end;
//---------------------------------------------------------------------------
// TWGDIHelper.ITextOptions
//---------------------------------------------------------------------------
class function TWGDIHelper.ITextOptions.GetDefault: ITextOptions;
begin
    Result                 := Default(TWGDIHelper.ITextOptions);
    Result.m_HAlign        := IE_HA_None;
    Result.m_VAlign        := IE_VA_None;
    Result.m_BgColor       := clWhite;
    Result.m_EditCtrlStyle := True;
    Result.m_LeftMargin    := 2;
    Result.m_RightMargin   := 2;
    Result.m_TabLength     := 4;
    Result.m_TextTrimming  := IE_TT_None;
    Result.m_DrawTextFunc  := IE_DF_GDI;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.ITextOptions.GetDrawTextConfig: DWORD;
begin
    Result := 0;

    // set horizontal alignment
    case (m_HAlign) of
        IE_HA_None:   ;
        IE_HA_Left:   Result := Result or DT_LEFT;
        IE_HA_Center: Result := Result or DT_CENTER;
        IE_HA_Right:  Result := Result or DT_RIGHT;
    else
        raise Exception.CreateFmt('Unknown horizontal alignment - %d', [Integer(m_HAlign)]);
    end;

    // set vertical alignment
    case (m_VAlign) of
        IE_VA_None:   ;
        IE_VA_Top:    Result := Result or DT_TOP;
        IE_VA_Center: Result := Result or DT_VCENTER;
        IE_VA_Bottom: Result := Result or DT_BOTTOM;
    else
        raise Exception.CreateFmt('Unknown vertical alignment - %d', [Integer(m_VAlign)]);
    end;

    // set read text layout
    if (m_RightToLeft) then
        Result := Result or DT_RTLREADING;

    // set clipping mode
    if (m_NoClip) then
        Result := Result or DT_NOCLIP;

    // set show hotkey prefix
    if (not m_ShowHotkeyPrefix) then
        Result := Result or DT_NOPREFIX;

    // set no full width char break
    if (m_NoFullWidthCharBreak) then
        Result := Result or DT_NOFULLWIDTHCHARBREAK;

    // set expand tabs
    if (m_ExpandTabs) then
        Result := Result or DT_EXPANDTABS;

    // set include external leading
    if (m_IncludeExternalLeading) then
        Result := Result or DT_EXTERNALLEADING;

    // do ignore word wrapping?
    if (m_NoWrap) then
    begin
        Result := Result or DT_SINGLELINE;

        // search for trimming
        case (m_TextTrimming) of
            IE_TT_None,
            IE_TT_Character,
            IE_TT_Word:              ;
            IE_TT_EllipsisCharacter: Result := Result or DT_END_ELLIPSIS;
            IE_TT_EllipsisWord:      Result := Result or DT_WORD_ELLIPSIS;
            IE_TT_EllipsisPath:      Result := Result or DT_PATH_ELLIPSIS;
        else
            raise Exception.CreateFmt('Unknown trimming - %d', [Integer(m_TextTrimming)]);
        end;
    end
    else
    begin
        Result := Result or DT_WORDBREAK;

        // set edit control style (i.e. line will be drawn as an multi-line edit control)
        if (m_EditCtrlStyle) then
            Result := Result or DT_EDITCONTROL;
    end;
end;
//---------------------------------------------------------------------------
// TWGDIHelper.ICachedDC
//---------------------------------------------------------------------------
constructor TWGDIHelper.ICachedDC.Create;
begin
    inherited Create;

    m_hDC      := 0;
    m_pDCCount := nil;

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pDCCount      := TWCacheHit.Create;
        m_pDCCount.Name := 'GDI device context';
    {$endif}
end;
//---------------------------------------------------------------------------
destructor TWGDIHelper.ICachedDC.Destroy;
begin
    if (m_hDC <> 0) then
        DeleteDC(m_hDC);

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pDCCount.Free;
    {$endif}

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.ICachedDC.GetDC: THandle;
begin
    // can use device context caching?
    if (not g_GDICacheController.m_DCs) then
        Exit(CreateCompatibleDC(0));

    // device context exists?
    if (m_hDC <> 0) then
    begin
        {$ifdef ENABLE_GDI_CACHE_LOGGING}
            m_pDCCount.Hit := m_pDCCount.Hit + 1;
        {$endif}

        Exit(m_hDC);
    end;

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pDCCount.Miss := m_pDCCount.Miss + 1;
    {$endif}

    if (m_hDC <> 0) then
        DeleteDC(m_hDC);

    // recreate device context
    m_hDC  := CreateCompatibleDC(0);
    Result := m_hDC;
end;
//---------------------------------------------------------------------------
// TWGDIHelper.IDIBBitmap
//---------------------------------------------------------------------------
constructor TWGDIHelper.IDIBBitmap.Create;
begin
    inherited Create;

    m_hBitmap     := 0;
    m_pBytes      := nil;
    m_hDC         := 0;
    m_Width       := 0;
    m_Height      := 0;
    m_PixelFormat := 0;
end;
//---------------------------------------------------------------------------
destructor TWGDIHelper.IDIBBitmap.Destroy;
begin
    if (m_hBitmap <> 0) then
        DeleteObject(m_hBitmap);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWGDIHelper.ICache
//---------------------------------------------------------------------------
constructor TWGDIHelper.ICache.Create;
begin
    inherited Create;

    m_pBitmaps    := ICachedBitmaps.Create([doOwnsValues]);
    m_pDIBBitmaps := ICachedDIBBitmaps.Create([doOwnsValues]);
    m_pMemoryDC   := ICachedDC.Create;

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pBitmapsCount    := TWCacheHit.Create;
        m_pDIBBitmapsCount := TWCacheHit.Create;

        m_pBitmapsCount.Name    := 'GDI TBitmaps';
        m_pDIBBitmapsCount.Name := 'GDI DIB bitmaps';
    {$endif}
end;
//---------------------------------------------------------------------------
destructor TWGDIHelper.ICache.Destroy;
begin
    Clear;

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pBitmapsCount.Free;
        m_pDIBBitmapsCount.Free;
    {$endif}

    m_pMemoryDC.Free;
    m_pDIBBitmaps.Free;
    m_pBitmaps.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.ICache.Clear;
begin
    m_pBitmaps.Clear;
    m_pDIBBitmaps.Clear;
end;
//---------------------------------------------------------------------------
// TWMathHelper
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: Integer): Integer;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: Int64): Int64;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: UInt64): UInt64;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: Single): Single;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: Double): Double;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Clamp(value, minVal, maxVal: Extended): Extended;
begin
    Result := Min(Max(value, minVal), maxVal);
end;
//---------------------------------------------------------------------------
class function TWMathHelper.IntMod(const num, by: Integer): Integer;
begin
    Result := (num mod by + by) mod by;
end;
//---------------------------------------------------------------------------
class function TWMathHelper.ExtMod(const num, by: Extended): Extended;
begin
    if (by = 0.0) then
        Exit(0.0);

    Result := num - Floor(num / by) * by;
end;
//---------------------------------------------------------------------------
class function TWMathHelper.Normalize(number, limit: Integer; out loops: Integer): Integer;
begin
    Result := number;

    if (Result < 0) then
    begin
        // limit to 0 and decrease extra on underflow
        if ((Result mod limit) <> 0) then
        begin
            // fractional loop e.g. for seconds limit == 60, if number == -1 => loop = -1
            // if number == -67 => loop = -2
            loops  := Result div limit - 1;
            Result := limit + (Result mod limit);
        end
        else
        begin
            // exact loop e.g. for seconds limit == 60, if number == -60 => loop = -1,
            // if number  == -120 => loop = -2,
            loops  := Result div limit;
            Result := Result mod limit;
        end;
    end
    else
    if (Result >= limit) then
    begin
        // limit and increase loops on overflow
        loops  := Result div limit;
        Result := Result mod limit;
    end;
end;
//---------------------------------------------------------------------------
class function TWMathHelper.CheckAndGetMuliplier(multiplicand, product: NativeUInt;
        out multiplier: NativeUInt): Boolean;
begin
    if (product = 0) then
    begin
        multiplier := 0;
        Exit(True);
    end;

    if (multiplicand = 0) then
        Exit(False);

    if ((product mod multiplicand) <> 0) then
        Exit(False);

    multiplier := product div multiplicand;
    Result     := True;
end;
//---------------------------------------------------------------------------
// TWSystemHelper
//---------------------------------------------------------------------------
class function TWSystemHelper.GetDllVersion(hMod: HMODULE): string;
var
    fileName: string;
begin
    SetLength(fileName, MAX_PATH);

    // get the module file name
    if (GetModuleFileName(hMod, PCHar(fileName), MAX_PATH) = 0) then
        Exit('');

    // succeeded?
    if (Length(fileName) = 0) then
        Exit('');

    // extract the version number from the module file name
    Result := TWFileHelper.GetFileVersion(fileName);
end;
//---------------------------------------------------------------------------
class function TWSystemHelper.GetDllVersion: string;
begin
    Result := GetDllVersion(HInstance);
end;
//---------------------------------------------------------------------------
// TWOSWinHelper
//---------------------------------------------------------------------------
class function TWOSWinHelper.GetVersionFromNtDll(var osvi: TOSVersionInfoExW): TWEResult;
var
    hMod:           HMODULE;
    tmp:            TOSVersionInfoExW;
    fRtlGetVersion: TWfRtlGetVersion;
begin
    // load the ntdll.dll module
    hMod := GetModuleHandleW('ntdll.dll');

    if (hMod = 0) then
    begin
        FillChar(osvi, SizeOf(osvi), $0);
        Exit(TW_E_Error);
    end;

    // get RtlGetVersion() function. This is the new function to use instead of deprecated GetVersionEx()
    fRtlGetVersion := GetProcAddress(hMod, 'RtlGetVersion');

    if (not(Assigned(fRtlGetVersion))) then
    begin
        FillChar(osvi, SizeOf(osvi), $0);
        Exit(TW_E_Error);
    end;

    FillChar(tmp, SizeOf(tmp), $0);
    tmp.dwOSVersionInfoSize := SizeOf(tmp);

    // execute the RtlGetVersion() function. NOTE the return value is 0 on success. Any other value
    // means that the function failed
    if (fRtlGetVersion(@tmp) <> 0) then
        Exit(TW_E_Error);

    // update versions if different
    if (tmp.dwMajorVersion > osvi.dwMajorVersion) then
    begin
        // update version info
        osvi := tmp;
        Exit(TW_E_True);
    end
    else
    if ((tmp.dwMajorVersion = osvi.dwMajorVersion) and (tmp.dwMinorVersion > osvi.dwMinorVersion)) then
    begin
        // update version
        osvi := tmp;
        Exit(TW_E_True);
    end;

    Result := TW_E_False;
end;
//---------------------------------------------------------------------------
class function TWOSWinHelper.GetVersionFromKernel32(var osvi: TOSVersionInfoExW): TWEResult;
var
    kernel32DLLPath:             array [0 .. MAX_PATH] of WCHAR;
    systemPath:                  UnicodeString;
    fileVersion, productVersion: TWVersion;
begin
    // get system32 path and append kernel32.dll to it
    if (GetSystemDirectoryW(@kernel32DLLPath, SizeOf(kernel32DLLPath)) = 0) then
        Exit(TW_E_Error);

    // append kernel32.dll to system32 path
    systemPath := IncludeTrailingPathDelimiter(kernel32DLLPath) + 'kernel32.dll';

    fileVersion    := nil;
    productVersion := nil;

    try
        fileVersion    := TWVersion.Create;
        productVersion := TWVersion.Create;

        // get file version
        if (not TWFileHelper.GetFileVersion(systemPath, fileVersion, productVersion)) then
            Exit(TW_E_Error);

        // update versions if different
        if (productVersion.Major > osvi.dwMajorVersion) then
        begin
            // update version
            osvi.dwMajorVersion := productVersion.Major;
            osvi.dwMinorVersion := productVersion.Minor;

            Exit(TW_E_True);
        end
        else
        if ((productVersion.Major = osvi.dwMajorVersion) and (productVersion.Minor > osvi.dwMinorVersion)) then
        begin
            // update version
            osvi.dwMinorVersion := productVersion.Minor;
            Exit(TW_E_True);
        end;
    finally
        if (Assigned(fileVersion)) then
            fileVersion.Free;

        if (Assigned(productVersion)) then
            productVersion.Free;
    end;

    Result := TW_E_False;
end;
//---------------------------------------------------------------------------
class function TWOSWinHelper.GetWinVersion(useCache: Boolean): TOSVersionInfoExW;
begin
    if (useCache) then
        Exit(m_WinVersion);

    FillChar(Result, SizeOf(Result), $0);
    Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfoExW);

    // first get version info from GetVersionExW(). NOTE be careful, this function is obsolete and
    // may not work well since Windows 8.1
    if (not(GetVersionExW(Result))) then
    begin
        Result.dwOSVersionInfoSize := SizeOf(TOSVersionInfoExW);

        if (not GetVersionExW(Result)) then
            Exit;
    end;

    // update from RtlGetVersion() and eventually kernel32.dll file because since Windows 8.1,
    // GetVersionEx() will always return 6.3 (i.e Win8.1) as version number if app doesn't contain a
    // manifest
    if (GetVersionFromNtDll(Result) <> TW_E_True) then
        GetVersionFromKernel32(Result);
end;
//---------------------------------------------------------------------------
class function TWOSWinHelper.CheckWinVersion(major, minor, build: Integer; useCache: Boolean): Boolean;
var
    winVersion: TOSVersionInfoExW;
begin
    winVersion := GetWinVersion(useCache);

    Result := ((Integer(winVersion.dwMajorVersion) > major) or ((Integer(winVersion.dwMajorVersion) = major)
            and (Integer(winVersion.dwMinorVersion) > minor)) or ((Integer(winVersion.dwMajorVersion) = major)
            and (Integer(winVersion.dwMinorVersion) = minor) and (Integer(winVersion.dwBuildNumber) >= build)));
end;
//---------------------------------------------------------------------------
// TWGDIHelper
//---------------------------------------------------------------------------
{**
 Callback to intercept font creation
 @param(hDC Device context)
 @param(pTable Handle table associated with the graphics objects (pens, brushes, and so on) in the
        metafile. The first entry contains the enhanced-metafile handle)
 @param(pRecord Pointer to one of the records in the metafile. This record should not be modified.
        (If modification is necessary, it should be performed on a copy of the record))
 @param(tableEntries The number of objects with associated handles in the handle table)
 @param(pFont @bold([out]) The found font fallback)
}
function MetaFileEnumProc(hDC: THandle; pTable: PHandleTable; const pRecord: PEnhMetaRecord;
        tableEntries: Integer; pFont: LPARAM): Integer; stdcall;
var
    pCreateFontRecord: PEMRExtCreateFontIndirect;
begin
    if (pRecord.iType = EMR_EXTCREATEFONTINDIRECTW) then
    begin
        pCreateFontRecord := PEMRExtCreateFontIndirect(pRecord);
        PLogFont(pFont)^  := pCreateFontRecord.elfw.elfLogFont;
    end;

    Result := 1;
end;
//---------------------------------------------------------------------------
constructor TWGDIHelper.Create;
begin
    inherited Create;

    m_pCache := ICache.Create;
end;
//---------------------------------------------------------------------------
destructor TWGDIHelper.Destroy;
begin
    m_pCache.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.ConfigureGDIToDrawText(hDC: THandle; drawBg: Boolean; hFont: THandle;
        bgColor, textColor: TColorRef);
begin
    // no device context?
    if (hDC = 0) then
        Exit;

    // select background color
    if (drawBg) then
    begin
        SetBkMode(hDC, OPAQUE);
        SetBkColor(hDC, bgColor);
    end
    else
        // background is transparent
        SetBkMode(hDC, TRANSPARENT);

    // select foreground color
    SetTextColor(hDC, textColor);

    // select font
    SelectObject(hDC, hFont);
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.DrawRect(const drawRect: TRect; radiusX, radiusY: Cardinal;
        const options: IRectOptions; doDrawRoundRect: Boolean; hDC: Thandle);
var
    hBrush, hBorderBrush, hPen: THandle;
    delta, endX, endY:          Integer;
begin
    // no device context?
    if (hDC = 0) then
        Exit;

    hBrush       := 0;
    hBorderBrush := 0;
    hPen         := 0;

    try
        // create brush and pen to use to draw rect
        hBrush := CreateSolidBrush(ColorToRGB(options.m_Color));
        hPen   := CreatePen(PS_SOLID, options.m_OutlineWidth, ColorToRGB(options.m_Outline));

        // select brush and pen to use
        SelectObject(hDC, hBrush);
        SelectObject(hDC, hPen);

        // do draw round rect?
        if (doDrawRoundRect) then
            // draw round rect
            RoundRect(hDC, drawRect.Left, drawRect.Top, drawRect.Right, drawRect.Bottom, radiusX,
                    radiusY)
        else
        if (options.m_OutlineWidth = 1) then
        begin
            // draw rect background
            FillRect(hDC, drawRect, hBrush);

            // create brush to use to draw border
            hBorderBrush := CreateSolidBrush(ColorToRGB(options.m_Outline));

            // draw rect frame
            FrameRect(hDC, drawRect, hBorderBrush);
        end
        else
        begin
            // draw rect background
            FillRect(hDC, drawRect, hBrush);

            // do draw outline?
            if (options.m_OutlineWidth <> 0) then
            begin
                if ((options.m_OutlineWidth mod 2) <> 0) then
                    delta := 1
                else
                    delta := 0;

                // calculate rect end position
                endX  := drawRect.Right  - delta;
                endY  := drawRect.Bottom - delta;

                // draw frame
                MoveToEx(hDC, 0,    0, nil);
                LineTo(hDC,   endX, 0);
                LineTo(hDC,   endX, endY);
                LineTo(hDC,   0,    endY);
                LineTo(hDC,   0,    0);
            end;
        end;
    finally
        if (hPen <> 0) then
            DeleteObject(hPen);

        if (hBorderBrush <> 0) then
            DeleteObject(hBorderBrush);

        if (hBrush <> 0) then
            DeleteObject(hBrush);
    end;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.GetTextMask(const text: UnicodeString; textWidth, textHeight: Integer;
        const textRect: TRect; pFont: TFont; config: DWORD; var params: TDrawTextParams;
        drawTextFunc: IEDrawTextFunc; hDC: THandle): Vcl.Graphics.TBitmap;
var
    drawRect: TRect;
    success:  Boolean;
begin
    if (not Assigned(pFont)) then
        Exit(nil);

    success := False;

    // configure mask to fill background (always a black background)
    Result := GetBitmap(textWidth, textHeight, bmDIB, pf24bit);

    try
        // configure mask to draw text (always a white text)
        Result.Canvas.Font.Assign(pFont);
        Result.Canvas.Brush.Style := bsClear;

        // copy background to overlay
        BitBlt(Result.Canvas.Handle, 0, 0, textWidth, textHeight, hDC, textRect.Left, textRect.Top,
                SRCCOPY);

        // calculate draw rect
        drawRect := textRect;
        drawRect.Offset(-drawRect.Left, -drawRect.Top);

        // draw mask text
        DrawText(drawTextFunc, Result.Canvas.Handle, PWideChar(text), Length(text), drawRect, config,
                @params);

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.PrepareBlending(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap;
        alpha: Byte): Vcl.Graphics.TBitmap;
var
    blendRect:                               TRect;
    pixelSize24, pixelSize32:                Cardinal;
    startX, startY, endX, endY, x, y, destX: Integer;
    success:                                 Boolean;
    pBm1Line, pBm2Line:                      PWRGBTripleArray;
    pOutputLine:                             PWRGBQuadArray;
begin
    if (not Assigned(pBm1)) then
        Exit(nil);

    if (not Assigned(pBm2)) then
        Exit(nil);

    // overlay and mask sizes should be identical
    if ((pBm1.Width <> pBm2.Width) or (pBm1.Height <> pBm2.Height)) then
        raise Exception.Create('Sizes does not match');

    // only 24 bit pixel formats are supported
    if ((pBm1.PixelFormat <> pf24bit) or (pBm2.PixelFormat <> pf24bit)) then
        raise Exception.Create('Incompatible pixel formats');

    // calculate blend rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, pBm1.Width, pBm1.Height, blendRect)) then
        Exit(nil);

    success := False;

    // configure text overlay
    Result := GetBitmap(rect.Width, rect.Height, bmDIB, pf32bit, afDefined);

    try
        // needed before ScanLine
        GdiFlush;

        // calculate 24 and 32 bit pixel sizes
        pixelSize24 := SizeOf(TRGBTriple);
        pixelSize32 := SizeOf(TRGBQuad);

        // get area to blend values
        startX := blendRect.Left;
        startY := blendRect.Top;
        endX   := blendRect.Right;
        endY   := blendRect.Bottom;

        // are values incoherent?
        if ((endX - 1 <= startX) or (endY - 1 <= startY)) then
            Exit;

        // iterate through overlay and mask lines
        for y := startY to endY - 1 do
        begin
            // get lines
            pBm1Line    := PWRGBTripleArray(pBm1.ScanLine[y]);
            pBm2Line    := PWRGBTripleArray(pBm2.ScanLine[y]);
            pOutputLine := PWRGBQuadArray(Result.ScanLine[y - startY]);

            // iterate through overlay and mask pixels
            for x := startX to endX - 1 do
            begin
                // calculate destination x offset, as destination buffer is 0 based
                destX := x - startX;

                // is pixel affected by text drawn?
                if (CompareMem(@pBm2Line[x], @pBm1Line[x], pixelSize24)) then
                begin
                    FillChar(pOutputLine[destX], $0, pixelSize32);
                    continue;
                end;

                // blend text with output. Tone cannot be higher than alpha channel, otherwise there
                // can be a glitch in the colors
                pOutputLine[destX].rgbRed      := Min(alpha, pBm2Line[x].rgbtRed);
                pOutputLine[destX].rgbGreen    := Min(alpha, pBm2Line[x].rgbtGreen);
                pOutputLine[destX].rgbBlue     := Min(alpha, pBm2Line[x].rgbtBlue);
                pOutputLine[destX].rgbReserved := alpha;
            end;
        end;

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.CalculateBitmapRect(const rect: TRect; width, height: Integer;
        out bitmapRect: TRect): Boolean;
var
    rectX, rectY, rectWidth, rectHeight: Integer;
begin
    // do use entire bitmap area?
    if (rect.IsEmpty) then
    begin
        bitmapRect.Left   := 0;
        bitmapRect.Top    := 0;
        bitmapRect.Right  := width;
        bitmapRect.Bottom := height;

        Exit(True);
    end;

    // calculate area to blend position and size
    rectX      := rect.Left;
    rectY      := rect.Top;
    rectWidth  := rect.Width;
    rectHeight := rect.Height;

    // do update x position?
    if (rectX < 0) then
    begin
        // update horizontal values. Adds rectX because adding a negative number is equivalent to
        // subtract its absolute value
        Inc(rectWidth, rectX);
        rectX := 0;
    end;

    // do update y position?
    if (rectY < 0) then
    begin
        // update vertical values. Adds rectY because adding a negative number is equivalent to
        // subtract its absolute value
        Inc(rectHeight, rectY);
        rectY := 0;
    end;

    // is rect area out of bounds?
    if ((rectWidth <= 0) or (rectHeight <= 0) or (rectX >= width) or (rectY >= height)) then
        Exit(False);

    // are x pos and size out of bounds?
    if ((rectX + rectWidth) > width) then
        rectWidth := width - rectX;

    // are y pos and size out of bounds?
    if ((rectY + rectHeight) > height) then
        rectHeight := height - rectY;

    // get final bend rectangle
    bitmapRect.Left   := rectX;
    bitmapRect.Top    := rectY;
    bitmapRect.Right  := rectWidth;
    bitmapRect.Bottom := rectHeight;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.GetCachedBitmap(key: IEBitmapKey): Vcl.Graphics.TBitmap;
var
    bitmapCached: Boolean;
begin
    Result := Vcl.Graphics.TBitmap.Create;

    // can use bitmaps cache?
    if (not g_GDICacheController.m_Bitmaps) then
        Exit;

    // get cached bitmap
    if (m_pCache.m_pBitmaps.TryGetValue(key, Result)) then
        Exit;

    bitmapCached := False;

    try
        // cache new bitmap
        m_pCache.m_pBitmaps.Add(key, Result);
        bitmapCached := True;
    finally
        if (not bitmapCached) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.GetCachedDIBBitmap(key: IEBitmapKey; hDC: THandle; var width, height: LONG;
        pixelFormat: WORD; out pBits: Pointer): HBitmap;
var
    pBitmap: IDIBBitmap;
begin
    // can use bitmaps cache?
    if (not g_GDICacheController.m_DIBBitmaps) then
        Exit(GetDIBBitmap(hDC, width, height, pixelFormat, pBits));

    // get cached DIB bitmap
    if (m_pCache.m_pDIBBitmaps.TryGetValue(key, pBitmap)) then
    begin
        // do recreate bitmap?
        if ((width > pBitmap.m_Width) or (height > pBitmap.m_Height)
                or (pixelFormat <> pBitmap.m_PixelFormat) or (hDC <> pBitmap.m_hDC))
        then
        begin
            {$ifdef ENABLE_GDI_CACHE_LOGGING}
                m_pCache.m_pDIBBitmapsCount.Miss := m_pCache.m_pDIBBitmapsCount.Miss + 1;
            {$endif}

            // delete previous bitmap
            if (pBitmap.m_hBitmap <> 0) then
                DeleteObject(pBitmap.m_hBitmap);

            // recreate bitmap
            pBitmap.m_pBytes      := nil;
            pBitmap.m_hBitmap     := GetDIBBitmap(hDC, width, height, pixelFormat, pBitmap.m_pBytes);
            pBitmap.m_hDC         := hDC;
            pBitmap.m_Width       := width;
            pBitmap.m_Height      := height;
            pBitmap.m_PixelFormat := pixelFormat;
        end
        else
        begin
            // set cached bitmap width and height to output
            width  := pBitmap.m_Width;
            height := pBitmap.m_Height;

            {$ifdef ENABLE_GDI_CACHE_LOGGING}
                m_pCache.m_pDIBBitmapsCount.Hit := m_pCache.m_pDIBBitmapsCount.Hit + 1;
            {$endif}
        end;

        // set bitmap bytes to output
        pBits  := pBitmap.m_pBytes;
        Result := pBitmap.m_hBitmap;
        Exit;
    end;

    {$ifdef ENABLE_GDI_CACHE_LOGGING}
        m_pCache.m_pDIBBitmapsCount.Miss := m_pCache.m_pDIBBitmapsCount.Miss + 1;
    {$endif}

    Result  := 0;
    pBitmap := IDIBBitmap.Create;

    try
        // cache new bitmap
        pBitmap.m_hBitmap         := GetDIBBitmap(hDC, width, height, pixelFormat, pBitmap.m_pBytes);
        pBitmap.m_hDC             := hDC;
        pBitmap.m_Width           := width;
        pBitmap.m_Height          := height;
        pBitmap.m_PixelFormat     := pixelFormat;
        m_pCache.m_pDIBBitmaps.Add(key, pBitmap);

        // get output values
        pBits  := pBitmap.m_pBytes;
        Result := pBitmap.m_hBitmap;
    finally
        if (Result = 0) then
            pBitmap.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.UpdateTextRectPos(const refRect: TRect; var rect: TRect;
        const options: ITextOptions);
var
    deltaX, deltaY: Integer;
begin
    deltaX := 0;
    deltaY := 0;

    // locate rect on the right top of the reference rect
    rect.Location := refRect.Location;

    // calculate delta X relatively to reference rect
    case (options.m_HAlign) of
        IE_HA_None,
        IE_HA_Left:   ;
        IE_HA_Center: deltaX := (refRect.Width - rect.Width) div 2;
        IE_HA_Right:  deltaX :=  refRect.Width - rect.Width;
    else
        raise Exception.CreateFmt('Unknown horizontal alignment - %d', [Integer(options.m_HAlign)]);
    end;

    // calculate delta Y relatively to reference rect
    case (options.m_VAlign) of
        IE_VA_None,
        IE_VA_Top:    ;
        IE_VA_Center: deltaY := (refRect.Height - rect.Height) div 2;
        IE_VA_Bottom: deltaY :=  refRect.Height - rect.Height;
    else
        raise Exception.CreateFmt('Unknown vertical alignment - %d', [Integer(options.m_VAlign)]);
    end;

    // update rect location
    rect.Offset(deltaX, deltaY);
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.DrawRect(const rect: TRect; radiusX, radiusY: Cardinal;
        const options: IRectOptions; hDC: THandle);
begin
    // no device context?
    if (hDC = 0) then
        Exit;

    // do draw x radius but y is equal to 0?
    if ((radiusX <> 0) and (radiusY = 0)) then
        radiusY := radiusX;

    // do draw y radius but x is equal to 0?
    if ((radiusY <> 0) and (radiusX = 0)) then
        radiusX := radiusY;

    // draw rect on overlay surface
    DrawRect(rect, radiusX, radiusY, options, ((radiusX <> 0) or (radiusY <> 0)), hDC);
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.DrawRect(const rect: TRect; radiusX, radiusY: Cardinal; opacity: Single;
        const options: IRectOptions; hDC: THandle);
var
    doDrawRoundRect:     Boolean;
    width,
    height:              Integer;
    paintRect:           TRect;
    hOffscreenDC,
    hUncachedOffscreenDC,
    hOverlay,
    hUncachedOverlay,
    hMask,
    hUncachedMask:       THandle;
    maskWidth,
    maskHeight,
    overlayWidth,
    overlayHeight:       LONG;
    pMaskBits,
    pOverlayBits:        Pointer;
    pOverlay,
    pMask,
    pOutput:             IWSmartPointer<Vcl.Graphics.TBitmap>;
    alpha:               Byte;
    bf:                  TBlendFunction;
begin
    // not a transparent rect?
    if (opacity >= 1.0) then
    begin
        DrawRect(rect, radiusX, radiusY, options, hDC);
        Exit;
    end
    else
    if (opacity <= 0.0) then
        // nothing to draw
        Exit;

    // no device context?
    if (hDC = 0) then
        Exit;

    // do draw x radius but y is equal to 0?
    if ((radiusX <> 0) and (radiusY = 0)) then
        radiusY := radiusX;

    // do draw y radius but x is equal to 0?
    if ((radiusY <> 0) and (radiusX = 0)) then
        radiusX := radiusY;

    // do draw round rect?
    doDrawRoundRect := ((radiusX <> 0) or (radiusY <> 0));

    // get rect width and height
    width  := rect.Width;
    height := rect.Height;

    // calculate paint rectangle
    paintRect := rect;
    paintRect.Offset(-paintRect.Left, -paintRect.Top);

    // can use in-memory (DIB) bitmaps?
    if (g_GDICacheController.m_UseDIBBitmapsToDrawRect) then
    begin
        hUncachedOffscreenDC := 0;
        hUncachedOverlay     := 0;
        hUncachedMask        := 0;

        try
            // create offscreen device context to use for in-memory GDI functions
            hOffscreenDC := m_pCache.m_pMemoryDC.GetDC;

            // succeeded?
            if (hOffscreenDC = 0) then
                Exit;

            // can use device context caching?
            if (not g_GDICacheController.m_DCs) then
                // set device context as disposable when useless
                hUncachedOffscreenDC := hOffscreenDC;

            maskWidth  := width;
            maskHeight := height;

            // get in-memory mask bitmap
            hMask := GetCachedDIBBitmap(IE_BK_DrawRectMaskDIB, hOffscreenDC, maskWidth, maskHeight,
                    24, pMaskBits);

            // succeeded?
            if (hMask = 0) then
                Exit;

            // can use DIB bitmaps caching?
            if (not g_GDICacheController.m_DIBBitmaps) then
                // set bitmap as disposable when useless
                hUncachedMask := hMask;

            // select mask bitmap
            SelectObject(hOffscreenDC, hMask);

            // draw mask background. It's the same as in overlay, thus it's possible to compare
            // which pixels were affected by text drawn, and thus blend only text pixels without
            // alter the background
            BitBlt(hOffscreenDC, 0, 0, width, height, hDC, rect.Left, rect.Top, SRCCOPY);

            // draw rect on mask
            DrawRect(paintRect, radiusX, radiusY, options, doDrawRoundRect, hOffscreenDC);

            overlayWidth  := width;
            overlayHeight := height;

            // get overlay
            hOverlay := GetCachedDIBBitmap(IE_BK_DrawRectOverlayDIB, hOffscreenDC, overlayWidth,
                    overlayHeight, 24, pOverlayBits);

            // succeeded?
            if (hOverlay = 0) then
                Exit;

            // can use DIB bitmaps caching?
            if (not g_GDICacheController.m_DIBBitmaps) then
                // set bitmap as disposable when useless
                hUncachedOverlay := hOverlay;

            // select overlay bitmap
            SelectObject(hOffscreenDC, hOverlay);

            // copy background to overlay
            BitBlt(hOffscreenDC, 0, 0, width, height, hDC, rect.Left, rect.Top, SRCCOPY);

            // mask and overlay bitmaps should always be of the same size
            if ((maskWidth <> overlayWidth) or (maskHeight <> overlayHeight)) then
                raise Exception.Create('Rect overlay and mask bitmaps doesn''t match');

            // apply blending
            Blend24(paintRect, pOverlayBits, pMaskBits, overlayWidth, overlayHeight, opacity);

            // copy final result to canvas
            BitBlt(hDC, rect.Left, rect.Top, width, height, hOffscreenDC, 0, 0, SRCCOPY);
        finally
            if (hUncachedMask <> 0) then
                DeleteObject(hUncachedMask);

            if (hUncachedOverlay <> 0) then
                DeleteObject(hUncachedOverlay);

            if (hUncachedOffscreenDC <> 0) then
                DeleteDC(hUncachedOffscreenDC);
        end;

        Exit;
    end;

    // configure overlay canvas to draw text
    pOverlay := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(GetBitmap(width, height, bmDIB, pf24bit));

    // copy background to overlay
    BitBlt(pOverlay.Canvas.Handle, 0, 0, width, height, hDC, rect.Left, rect.Top, SRCCOPY);

    // get mask bitmap
    pMask := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(GetBitmap(width, height, bmDIB, pf24bit));

    // copy background to mask
    BitBlt(pMask.Canvas.Handle, 0, 0, width, height, hDC, rect.Left, rect.Top, SRCCOPY);

    // draw rect on mask
    DrawRect(paintRect, radiusX, radiusY, options, doDrawRoundRect, pMask.Canvas.Handle);

    // do use native GDI alpha blending function to draw text?
    if (g_GDICacheController.m_UseAlphaBlendToDrawRect) then
    begin
        // needed before ScanLine
        GdiFlush;

        // calculate global alpha value to apply and 24 and 32 bit pixel sizes
        alpha := Trunc(255.0 * opacity);

        // prepare output image blending
        pOutput := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(PrepareBlending(paintRect, pOverlay,
                pMask, alpha));

        // populate blend function (all fields are constant except source constant alpha, that designates
        // transparency degree for the text)
        bf.BlendOp             := AC_SRC_OVER;
        bf.BlendFlags          := 0;
        bf.SourceConstantAlpha := alpha;
        bf.AlphaFormat         := AC_SRC_ALPHA;

        // alpha blend text above destination canvas
        AlphaBlend(hDC, rect.Left, rect.Top, width, height, pOutput.Canvas.Handle, 0, 0, width,
                height, bf);

        Exit;
    end;

    // apply blending
    Blend24(paintRect, pOverlay, pMask, opacity);

    // copy final result to canvas
    BitBlt(hDC, rect.Left, rect.Top, width, height, pOverlay.Canvas.Handle, 0, 0, SRCCOPY);
end;
//---------------------------------------------------------------------------
function TWGDIHelper.GetFontFallback(hDC, hFont: THandle; const text: UnicodeString;
        pFontFallback: PLogFont): Boolean;
var
    hMetaFileDC:     THandle;
    pScriptAnalysis: SCRIPT_STRING_ANALYSIS;
    hMetaFile:       HENHMETAFILE;
    logFont:         TLogFont;
    metaFileRect:    TRect;
    hRes:            HRESULT;
begin
    // use a meta file to intercept the fallback font chosen by Uniscribe
    hMetaFileDC := CreateEnhMetaFile(hDC, nil, nil, nil);

    if (hMetaFileDC = 0) then
        Exit(False);

    if (hFont <> 0) then
        SelectObject(hMetaFileDC, hFont);

    hRes := ScriptStringAnalyse(hMetaFileDC, PWideChar(text), Length(text), 0, -1,
            SSA_METAFILE or SSA_FALLBACK or SSA_GLYPHS or SSA_LINK, 0, nil, nil, nil, nil, nil,
            @pScriptAnalysis);

    if (SUCCEEDED(hRes)) then
    begin
        hRes := ScriptStringOut(pScriptAnalysis, 0, 0, 0, nil, 0, 0, False);
        ScriptStringFree(@pScriptAnalysis);
    end;

    Result := False;

    hMetaFile := 0;

    try
        hMetaFile := CloseEnhMetaFile(hMetaFileDC);

        if (SUCCEEDED(hRes)) then
        begin
            logFont.lfFaceName[0] := #0;
            metaFileRect          := Default(TRect);

            EnumEnhMetaFile(0, hMetaFile, @MetaFileEnumProc, @logFont, metaFileRect);

            if (logFont.lfFaceName[0] <> #0) then
            begin
                pFontFallback^ := logFont;
                Result         := True;
            end;
        end;
    finally
        if (hMetaFile <> 0) then
            DeleteEnhMetaFile(hMetaFile);
    end;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.GetTextSize(const text: UnicodeString; const rect: TRect; pFont: TFont;
        const options: ITextOptions; hDC: THandle; pCharacters: PInteger; pLines: PInteger): TSize;
var
    config:             DWORD;
    params:             TDrawTextParams;
    textMtc:            TEXTMETRIC;
    textRect:           TRect;
    height, fontHeight: Integer;
begin
    // no device context?
    if (hDC = 0) then
        Exit(Default(TSize));

    // is text empty?
    if (Length(text) = 0) then
        Exit(Default(TSize));

    // select font to use
    SelectObject(hDC, pFont.Handle);

    // get text draw configuration flags
    config := options.GetDrawTextConfig;

    // create and populate draw text parameters (margins, tab length, ...)
    params.cbSize        := SizeOf(params);
    params.iLeftMargin   := options.m_LeftMargin;
    params.iRightMargin  := options.m_RightMargin;
    params.iTabLength    := options.m_TabLength;
    params.uiLengthDrawn := 0;

    textRect := rect;

    // measure text
    height := DrawText(options.m_DrawTextFunc, hDC, PWideChar(text), Length(text), textRect,
            DT_CALCRECT or config, @params);

    // get drawn char count, if needed
    if (Assigned(pCharacters)) then
        pCharacters^ := params.uiLengthDrawn;

    // do get drawn line count?
    if (Assigned(pLines)) then
    begin
        // get text metrics for the current font
        GetTextMetrics(hDC, textMtc);

        // calculate font height
        fontHeight := textMtc.tmHeight + textMtc.tmExternalLeading;

        if (fontHeight = 0) then
            fontHeight := 1;

        // calculate and get drawn line count
        pLines^ := height div fontHeight;
    end;

    Result := TSize.Create(textRect.Width, textRect.Height);
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.DrawText(const text: UnicodeString; const rect: TRect; pFont: TFont;
        const options: ITextOptions; hDC: THandle);
var
    textColor, bgColor: TColorRef;
    config:             DWORD;
    params:             TDrawTextParams;
    textRect:           TRect;
begin
    // no device context?
    if (hDC = 0) then
        Exit;

    // is text empty?
    if (Length(text) = 0) then
        Exit;

    // get text color
    textColor := ColorToRGB(pFont.Color);

    // configure GDI to draw text
    if (options.m_DrawBg) then
    begin
        bgColor := ColorToRGB(options.m_BgColor);
        ConfigureGDIToDrawText(hDC, true, pFont.Handle, bgColor, textColor);
    end
    else
        ConfigureGDIToDrawText(hDC, False, pFont.Handle, 0, textColor);

    // get text draw configuration flags
    config := options.GetDrawTextConfig;

    // create and populate draw text parameters (margins, tab length, ...)
    params.cbSize        := SizeOf(params);
    params.iLeftMargin   := options.m_LeftMargin;
    params.iRightMargin  := options.m_RightMargin;
    params.iTabLength    := options.m_TabLength;
    params.uiLengthDrawn := 0;

    textRect := rect;

    // is text centered vertically or bottom aligned but is not on a single line?
    if (((config and (DT_VCENTER or DT_BOTTOM)) <> 0) and ((config and DT_SINGLELINE) = 0)) then
    begin
        // measure text. This is needed, because DrawText() ignores the DT_VCENTER and DT_BOTTOM
        // flags if DT_SINGLELINE is not set. In other words, the text is not aligned correctly by
        // the GDI if it contains multiple lines and text isn't top aligned. In this case, the only
        // way respect the text alignment is to measure it first, then calculate the final position
        // according to the parent rect
        DrawText(options.m_DrawTextFunc, hDC, PWideChar(text), Length(text), textRect,
                DT_CALCRECT or config, @params);

        // locate rect on the correct position, relatively to parent rect
        UpdateTextRectPos(rect, textRect, options);
    end;

    // draw text
    DrawText(options.m_DrawTextFunc, hDC, PWideChar(text), Length(text), textRect, config, @params);
end;
//---------------------------------------------------------------------------
procedure TWGDIHelper.DrawText(const text: UnicodeString; const rect: TRect; pFont: TFont;
        opacity: Single; const options: ITextOptions; hDC: THandle);
var
    config:              DWORD;
    params:              TDrawTextParams;
    bf:                  TBlendFunction;
    hOffscreenDC,
    hUncachedOffscreenDC,
    hOverlay,
    hUncachedOverlay,
    hMask,
    hUncachedMask:       THandle;
    textColor,
    bgColor:             TColorRef;
    textRect,
    paintRect:           TRect;
    textWidth,
    textHeight:          Integer;
    maskWidth,
    maskHeight,
    overlayWidth,
    overlayHeight:       LONG;
    pMaskBits,
    pOverlayBits:        Pointer;
    pOverlay,
    pMask,
    pOutput:             IWSmartPointer<Vcl.Graphics.TBitmap>;
    alpha:               Byte;
begin
    // no opacity?
    if (opacity >= 1.0) then
    begin
        DrawText(text, rect, pFont, options, hDC);
        Exit;
    end;

    // no device context?
    if (hDC = 0) then
        Exit;

    // is text empty?
    if (Length(text) = 0) then
        Exit;

    // nothing to draw?
    if (opacity <= 0.0) then
        Exit;

    // get text draw configuration flags
    config := options.GetDrawTextConfig;

    // create and populate draw text parameters (margins, tab length, ...)
    params.cbSize        := SizeOf(params);
    params.iLeftMargin   := options.m_LeftMargin;
    params.iRightMargin  := options.m_RightMargin;
    params.iTabLength    := options.m_TabLength;
    params.uiLengthDrawn := 0;

    // can use in-memory (DIB) bitmaps?
    if (g_GDICacheController.m_UseDIBBitmapsToDrawText) then
    begin
        hUncachedOffscreenDC := 0;
        hUncachedOverlay     := 0;
        hUncachedMask        := 0;

        try
            // create offscreen device context to use for in-memory GDI functions
            hOffscreenDC := m_pCache.m_pMemoryDC.GetDC;

            // succeeded?
            if (hOffscreenDC = 0) then
                Exit;

            // can use device context caching?
            if (g_GDICacheController.m_DCs) then
                // set device context as disposable when useless
                hUncachedOffscreenDC := hOffscreenDC;

            // get text color
            textColor := ColorToRGB(pFont.Color);

            // configure GDI to draw text
            if (options.m_DrawBg) then
            begin
                bgColor := ColorToRGB(options.m_BgColor);
                ConfigureGDIToDrawText(hOffscreenDC, true, pFont.Handle, bgColor, textColor);
            end
            else
                ConfigureGDIToDrawText(hOffscreenDC, False, pFont.Handle, 0, textColor);

            textRect := rect;

            // is text centered vertically or bottom aligned but is not on a single line?
            if (((config and (DT_VCENTER or DT_BOTTOM)) <> 0) and ((config and DT_SINGLELINE) = 0)) then
            begin
                // measure text. This is needed, because DrawText() ignores the DT_VCENTER and
                // DT_BOTTOM flags if DT_SINGLELINE is not set. In other words, the text is not
                // aligned correctly by the GDI if it contains multiple lines and text isn't top
                // aligned. In this case, the only way respect the text alignment is to measure it
                // first, then calculate the final position according to the parent rect
                DrawText(options.m_DrawTextFunc, hOffscreenDC, PWideChar(text), Length(text),
                        textRect, DT_CALCRECT or config, @params);

                // locate rect on the correct position, relatively to parent rect
                UpdateTextRectPos(rect, textRect, options);
            end;

            // get text width and height
            textWidth  := textRect.Width;
            textHeight := textRect.Height;

            // calculate draw rect
            paintRect := textRect;
            paintRect.Offset(-paintRect.Left, -paintRect.Top);

            maskWidth  := textWidth;
            maskHeight := textHeight;

            // get in-memory mask bitmap
            hMask := GetCachedDIBBitmap(IE_BK_DrawTextMaskDIB, hOffscreenDC, maskWidth, maskHeight,
                    24, pMaskBits);

            // succeeded?
            if (hMask = 0) then
                Exit;

            // can use DIB bitmaps caching?
            if (not g_GDICacheController.m_DIBBitmaps) then
                // set bitmap as disposable when useless
                hUncachedMask := hMask;

            // select mask bitmap
            SelectObject(hOffscreenDC, hMask);

            // draw mask background. It's the same as in overlay, thus it's possible to compare
            // which pixels were affected by text drawn, and thus blend only text pixels without
            // alter the background
            BitBlt(hOffscreenDC, 0, 0, textWidth, textHeight, hDC, textRect.Left, textRect.Top,
                    SRCCOPY);

            // draw mask text
            DrawText(options.m_DrawTextFunc, hOffscreenDC, PWideChar(text), Length(text), paintRect,
                    config, @params);

            overlayWidth  := textWidth;
            overlayHeight := textHeight;

            // get text overlay
            hOverlay := GetCachedDIBBitmap(IE_BK_DrawTextOverlayDIB, hOffscreenDC, overlayWidth,
                    overlayHeight, 24, pOverlayBits);

            // succeeded?
            if (hOverlay = 0) then
                Exit;

            // can use DIB bitmaps caching?
            if (not g_GDICacheController.m_DIBBitmaps) then
                // set bitmap as disposable when useless
                hUncachedOverlay := hOverlay;

            // select overlay bitmap
            SelectObject(hOffscreenDC, hOverlay);

            // copy background to overlay
            BitBlt(hOffscreenDC, 0, 0, textWidth, textHeight, hDC, textRect.Left, textRect.Top,
                    SRCCOPY);

            // mask and overlay bitmaps should always be of the same size
            if ((maskWidth <> overlayWidth) or (maskHeight <> overlayHeight)) then
                raise Exception.Create('Text overlay and mask bitmaps doesn''t match');

            // apply blending
            Blend24(paintRect, pOverlayBits, pMaskBits, overlayWidth, overlayHeight, opacity);

            // copy final result to canvas
            BitBlt(hDC, textRect.Left, textRect.Top, textWidth, textHeight, hOffscreenDC, 0, 0,
                    SRCCOPY);
        finally
            if (hUncachedMask = 0) then
                DeleteObject(hUncachedMask);

            if (hUncachedOverlay = 0) then
                DeleteObject(hUncachedOverlay);

            if (hUncachedOffscreenDC = 0) then
                DeleteDC(hUncachedOffscreenDC);
        end;

        Exit;
    end;

    // configure overlay canvas to draw text
    pOverlay := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(GetBitmap(0, 0, bmDIB, pf24bit));
    pOverlay.Canvas.Font.Assign(pFont);

    textRect := rect;

    // is text centered vertically or bottom aligned but is not on a single line?
    if (((config and (DT_VCENTER or DT_BOTTOM)) <> 0) and ((config and DT_SINGLELINE) = 0)) then
    begin
        // measure text. This is needed, because DrawText() ignores the DT_VCENTER and DT_BOTTOM
        // flags if DT_SINGLELINE is not set. In other words, the text is not aligned correctly by
        // the GDI if it contains multiple lines and text isn't top aligned. In this case, the only
        // way respect the text alignment is to measure it first, then calculate the final position
        // according to the parent rect
        DrawText(options.m_DrawTextFunc, pOverlay.Canvas.Handle, PWideChar(text), Length(text),
                textRect, config or DT_CALCRECT, @params);

        // locate rect on the correct position, relatively to parent rect
        UpdateTextRectPos(rect, textRect, options);
    end;

    // get text width and height
    textWidth  := textRect.Width;
    textHeight := textRect.Height;

    // calculate draw rect
    paintRect := textRect;
    paintRect.Offset(-paintRect.Left, -paintRect.Top);

    // set overlay size
    pOverlay.SetSize(textWidth, textHeight);

    // copy background to overlay
    BitBlt(pOverlay.Canvas.Handle, 0, 0, textWidth, textHeight, hDC, textRect.Left, textRect.Top,
            SRCCOPY);

    // get text mask
    pMask := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(GetTextMask(text, textWidth, textHeight,
            textRect, pFont, config, params, options.m_DrawTextFunc, hDC));

    // do use native GDI alpha blending function to draw text?
    if (g_GDICacheController.m_UseAlphaBlendToDrawText) then
    begin
        // needed before ScanLine
        GdiFlush;

        // calculate global alpha value to apply and 24 and 32 bit pixel sizes
        alpha := Trunc(255.0 * opacity);

        // prepare output image blending
        pOutput := TWSmartPointer<Vcl.Graphics.TBitmap>.Create(PrepareBlending(paintRect, pOverlay,
                pMask, alpha));

        // populate blend function (all fields are constant except source constant alpha, that designates
        // transparency degree for the text)
        bf.BlendOp             := AC_SRC_OVER;
        bf.BlendFlags          := 0;
        bf.SourceConstantAlpha := alpha;
        bf.AlphaFormat         := AC_SRC_ALPHA;

        // alpha blend text above destination canvas
        AlphaBlend(hDC, textRect.Left, textRect.Top, textWidth, textHeight, pOutput.Canvas.Handle,
                0, 0, textWidth, textHeight, bf);

        Exit;
    end;

    // apply blending
    Blend24(paintRect, pOverlay, pMask, opacity);

    // copy final result to canvas
    BitBlt(hDC, textRect.Left, textRect.Top, textWidth, textHeight, pOverlay.Canvas.Handle, 0, 0,
            SRCCOPY);
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.DrawText(drawTextFunc: IEDrawTextFunc; hDC: THandle; pStr: LPWSTR;
        iCount: Integer; var textRect: TRect; flags: UINT; pDtp: PDrawTextParams): Integer;
begin
    // select draw text function to use
    case (drawTextFunc) of
        IE_DF_GDI:          Result := DrawTextExW(hDC, pStr, iCount, textRect, flags, pDtp);
        // todo FIXME -cFeature -oJean: uncomment line below when WGDIDrawTextEx will be translated
        //                              in Delphi and available
        //IE_DF_WS_Optimized: Result := WGDIDrawTextEx::DrawTextExW(hDC, pStr, iCount, textRect, flags, pDtp);
    else
        raise Exception.CreateFmt('Unknown draw text function - %d', [Integer(drawTextFunc)]);
    end;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.DrawLine(hDC: THandle; const startPos, endPos: TPoint; color: TColor;
        width: NativeUInt);
var
    hPen, hOldPen: THandle;
begin
    hPen := 0;

    try
        // create pen to draw underscore
        hPen := CreatePen(PS_SOLID, width, ColorToRGB(color));

        // succeeded?
        if (hPen = 0) then
            Exit;

        // select pen to use to draw underscore
        hOldPen := SelectObject(hDC, hPen);

        // draw underscore
        MoveToEx(hDC, startPos.x, startPos.y, nil);
        LineTo(hDC, endPos.x, endPos.y);

        // restore previous pen
        SelectObject(hDC, hOldPen);
    finally
        if (hPen <> 0) then
            DeleteObject(hPen);
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.CompareFonts(pFont1, pFont2: TFont): Boolean;
begin
    if ((not Assigned(pFont1)) or (not Assigned(pFont2))) then
        Exit(False);

    // compares properties which may have an impact on the drawing size. This function exists in the
    // VCL, but of course is unaccessible
    Result := ((pFont1.Height      = pFont1.Height)      and
               (pFont1.Style       = pFont1.Style)       and
               (pFont1.Charset     = pFont1.Charset)     and
               (pFont1.Name        = pFont1.Name)        and
               (pFont1.Orientation = pFont1.Orientation) and
               (pFont1.Quality     = pFont1.Quality));
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.CompareFontInstances(pFont1, pFont2: TFont): Boolean;
begin
    if ((not Assigned(pFont1)) or (not Assigned(pFont2))) then
        Exit(False);

    Result := pFont1.Equals(pFont2);
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.GetPixel(pBitmap: Vcl.Graphics.TBitmap; x, y: Integer;
        const defaultColor: TWColor): TWColor;
var
    width, height: Integer;
    p24BitLine:    PWRGBTripleArray;
    p32BitLine:    PWRGBQuadArray;
begin
    if (not Assigned(pBitmap)) then
        Exit(defaultColor);

    // needed before ScanLine
    GdiFlush;

    width  := pBitmap.Width;
    height := pBitmap.Height;

    // is y coordinate out of bounds
    if ((y >= 0) and (y < height)) then
    begin
        // get lines
        p24BitLine := nil;
        p32BitLine := nil;

        case (pBitmap.PixelFormat) of
            pf24bit: p24BitLine := PWRGBTripleArray(pBitmap.ScanLine[y]);
            pf32bit: p32BitLine := PWRGBQuadArray(pBitmap.ScanLine[y]);
        else
            Exit(defaultColor);
        end;

        // is x coordinate out of bounds
        if ((x >= 0) and (x < width)) then
        begin
            if (Assigned(p24BitLine)) then
                Result := TWColor.Create(p24BitLine[x].rgbtRed, p24BitLine[x].rgbtGreen,
                        p24BitLine[x].rgbtBlue)
            else
            if (Assigned(p32BitLine)) then
                Result := TWColor.Create(p32BitLine[x].rgbRed, p32BitLine[x].rgbGreen,
                        p32BitLine[x].rgbBlue, p32BitLine[x].rgbReserved)
            else
                Result := defaultColor;

            Exit;
        end;
    end;

    Result := defaultColor;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.IsDFB(hBitmap: THandle; out info: BITMAP): Boolean;
begin
    Result := (GetObject(hBitmap, SizeOf(info), @info) <> 0) and (not Assigned(info.bmBits));
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.IsDIB(hBitmap: THandle; out info: BITMAP): Boolean;
begin
    Result := (GetObject(hBitmap, sizeof(info), @info) <> 0) and Assigned(info.bmBits);
end;
//---------------------------------------------------------------------------
function TWGDIHelper.ConvertToDFB(var hBitmap: THandle): Boolean;
var
    info:           BITMAP;
    hScreen,
    hDFB,
    hMemSrc,
    hUncachedMemSrc,
    hOldSrc,
    hMemDst,
    hUncachedMemDst,
    hOldDst:        THandle;
begin
    // is bitmap already a DFB?
    if (IsDFB(hBitmap, info)) then
        Exit(False);

    hScreen         := 0;
    hDFB            := 0;
    hMemSrc         := 0;
    hUncachedMemSrc := 0;
    hOldSrc         := 0;
    hMemDst         := 0;
    hUncachedMemDst := 0;
    hOldDst         := 0;

    try
        // get screen device context
        hScreen := GetDC(0);

        // found it?
        if (hScreen = 0) then
            Exit(False);

        // create DFB with same size, using screen pixel format (to bypass conversion every time the
        // bitmap will be painted)
        hDFB := CreateCompatibleBitmap(hScreen, info.bmWidth, info.bmHeight);

        // succeeded?
        if (hDFB = 0) then
            Exit(False);

        // newly created bitmap is really a DFB?
        if (not IsDFB(hDFB, info)) then
            Exit(False);

        // create in-memory source device context
        hMemSrc := m_pCache.m_pMemoryDC.GetDC;

        // succeeded?
        if (hMemSrc = 0) then
            Exit(False);

        // can use device context caching?
        if (not g_GDICacheController.m_DCs) then
            // set device context as disposable when useless
            hUncachedMemSrc := hMemSrc;

        // select DIB to copy from
        hOldSrc := SelectObject(hMemSrc, hBitmap);

        // succeeded?
        if (hOldSrc = 0) then
            Exit(False);

        // create in-memory destination device context
        hMemDst := m_pCache.m_pMemoryDC.GetDC;

        // succeeded?
        if (hMemDst = 0) then
            Exit(False);

        // can use device context caching?
        if (not g_GDICacheController.m_DCs) then
            // set device context as disposable when useless
            hUncachedMemDst := hMemDst;

        // select DFB to copy to
        hOldDst := SelectObject(hMemDst, hDFB);

        // succeeded?
        if (hOldDst = 0) then
            Exit(False);

        // convert bitmaps. It's probable that the DrvCopyBits() driver function will be called internally
        if (BitBlt(hMemDst, 0, 0, info.bmWidth, info.bmHeight, hMemSrc, 0, 0, SRCCOPY)) then
        begin
            // delete source bitmap and replace by converted bitmap
            DeleteObject(hBitmap);
            hBitmap := hDFB;
            hDFB    := 0;
        end;
    finally
        if (hOldDst <> 0) then
            SelectObject(hMemDst, hOldDst);

        if (hUncachedMemDst <> 0) then
            DeleteDC(hUncachedMemDst);

        if (hOldSrc <> 0) then
            SelectObject(hMemSrc, hOldSrc);

        if (hUncachedMemSrc <> 0) then
            DeleteDC(hUncachedMemSrc);

        if (hDFB <> 0) then
            DeleteObject(hDFB);

        if (hScreen <> 0) then
            ReleaseDC(0, hScreen);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWGDIHelper.ConvertToDIB(var hBitmap: THandle): Boolean;
var
    info:             BITMAP;
    hScreen,
    hDIB,
    hMemSrc,
    hUncachedMemSrc,
    hOldSrc,
    hMemDst,
    hUncachedMemDst,
    hOldDst,
    hPalette:         THandle;
    pBits:            Pointer;
    nEntries,
    nIndex:           UINT;
    paletteEntries:   array [0..$100] of PALETTEENTRY;
    pBmpInfoUnion:    PBitmapInfoUnion;
begin
    // is bitmap already a DIB?
    if (IsDIB(hBitmap, info)) then
        Exit(False);

    hScreen         := 0;
    hDIB            := 0;
    hMemSrc         := 0;
    hUncachedMemSrc := 0;
    hOldSrc         := 0;
    hMemDst         := 0;
    hUncachedMemDst := 0;
    hOldDst         := 0;
    pBmpInfoUnion   := nil;

    try
        // get screen device context
        hScreen := GetDC(0);

        // found it?
        if (hScreen = 0) then
            Exit(False);

        New(pBmpInfoUnion);

        // create DIB info structure with same size and pixel format as source bitmap
        ZeroMemory(@pBmpInfoUnion^.pReserveSpace, SizeOf(pBmpInfoUnion^.pReserveSpace));
        pBmpInfoUnion^.bmpInfo.bmiHeader.biSize        := SizeOf(pBmpInfoUnion^.bmpInfo.bmiHeader);
        pBmpInfoUnion^.bmpInfo.bmiHeader.biWidth       := info.bmWidth;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biHeight      := info.bmHeight;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biPlanes      := 1;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biBitCount    := info.bmBitsPixel;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biCompression := BI_RGB;

        // is souce image palletized?
        if (info.bmBitsPixel <= 8) then
            // synthetise pallette
            pBmpInfoUnion^.bmpInfo.bmiHeader.biClrUsed := (1 shl info.bmBitsPixel);

        pBmpInfoUnion^.bmpInfo.bmiHeader.biClrImportant := pBmpInfoUnion^.bmpInfo.bmiHeader.biClrUsed;

        // create DIB bitmap
        hDIB := CreateDIBSection(hScreen, pBmpInfoUnion^.bmpInfo, DIB_RGB_COLORS, pBits, 0, 0);

        // succeeded?
        if (hDIB = 0) then
            Exit(False);

        // create in-memory source device context
        hMemSrc := m_pCache.m_pMemoryDC.GetDC;

        // succeeded?
        if (hMemSrc = 0) then
            Exit(False);

        // can use device context caching?
        if (not g_GDICacheController.m_DCs) then
            // set device context as disposable when useless
            hUncachedMemSrc := hMemSrc;

        // select DFB to copy from
        hOldSrc := SelectObject(hMemSrc, hBitmap);

        // succeeded?
        if (hOldSrc = 0) then
            Exit(False);

        // create in-memory destination device context
        hMemDst := m_pCache.m_pMemoryDC.GetDC;

        // succeeded?
        if (hMemDst = 0) then
            Exit(False);

        // can use device context caching?
        if (not g_GDICacheController.m_DCs) then
            // set device context as disposable when useless
            hUncachedMemDst := hMemDst;

        // select DIB to copy to
        hOldDst := SelectObject(hMemDst, hDIB);

        // succeeded?
        if (hOldDst = 0) then
            Exit(False);

        // is source image palletized?
        if (info.bmBitsPixel <= 8) then
        begin
            // select source pallette
            hPalette := GetCurrentObject(hMemSrc, OBJ_PAL);

            if (hPalette = 0) then
                Exit(False);

            nEntries := GetPaletteEntries(hPalette, 0, pBmpInfoUnion^.bmpInfo.bmiHeader.biClrUsed,
                    paletteEntries);

            if (nEntries = 0) then
                Exit(False);

            if (nEntries > $100) then
                Exit(False);

            // initialize destination pallette
            if (nEntries > 0) then
                for nIndex := 0 to nEntries - 1 do
                    paletteEntries[nEntries].peFlags := 0;

            // copy palette
            if (SetDIBColorTable(hMemDst, 0, nEntries, paletteEntries) <> nEntries) then
                Exit(False);
        end;

        // convert bitmaps. It's probable that the DrvCopyBits() driver function will be called internally
        if (BitBlt(hMemDst, 0, 0, info.bmWidth, info.bmHeight, hMemSrc, 0, 0, SRCCOPY)) then
        begin
            // delete source bitmap and replace by converted bitmap
            DeleteObject(hBitmap);
            hBitmap := hDIB;
            hDIB    := 0;
        end;
    finally
        if (hOldDst <> 0) then
            SelectObject(hMemDst, hOldDst);

        if (hUncachedMemDst <> 0) then
            DeleteDC(hUncachedMemDst);

        if (hOldSrc <> 0) then
            SelectObject(hMemSrc, hOldSrc);

        if (hUncachedMemSrc <> 0) then
            DeleteDC(hUncachedMemSrc);

        if (hDIB <> 0) then
            DeleteObject(hDIB);

        if (hScreen <> 0) then
            ReleaseDC(0, hScreen);

        if (Assigned(pBmpInfoUnion)) then
            Dispose(pBmpInfoUnion);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.GetDIBBitmap(hDC: THandle; width, height: LONG; pixelFormat: WORD;
        out pBits: Pointer): THandle;
var
    hDefaultDC:    THandle;
    pBmpInfoUnion: PBitmapInfoUnion;
begin
    hDefaultDC    := 0;
    pBmpInfoUnion := nil;

    try
        // no device context?
        if (hDC = 0) then
        begin
            // use screen device context by default
            hDefaultDC := GetDC(0);
            hDC        := hDefaultDC;
        end;

        New(pBmpInfoUnion);

        // create and populate bitmap info structure. NOTE bitmap height is negative to specify that
        // bitmap origin is on the left top corner. A POSITIVE HEIGHT VALUE MEANS THAT BITMAP ORIGIN
        // IS ON THE LEFT BOTTOM CORNER
        ZeroMemory(@pBmpInfoUnion^.pReserveSpace, SizeOf(pBmpInfoUnion^.pReserveSpace));
        pBmpInfoUnion^.bmpInfo.bmiHeader.biSize        :=  SizeOf(pBmpInfoUnion^.bmpInfo.bmiHeader);
        pBmpInfoUnion^.bmpInfo.bmiHeader.biWidth       :=  width;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biHeight      := -height;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biPlanes      :=  1;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biBitCount    :=  pixelFormat;
        pBmpInfoUnion^.bmpInfo.bmiHeader.biCompression :=  BI_RGB;

        // do create a palletized bitmap?
        if (pixelFormat <= 8) then
            // synthetise pallette
            pBmpInfoUnion^.bmpInfo.bmiHeader.biClrUsed := (1 shl pixelFormat);

        pBmpInfoUnion^.bmpInfo.bmiHeader.biClrImportant := pBmpInfoUnion^.bmpInfo.bmiHeader.biClrUsed;

        // create DIB bitmap
        Result := CreateDIBSection(hDC, pBmpInfoUnion^.bmpInfo, DIB_RGB_COLORS, &pBits, 0, 0);
    finally
        if (hDefaultDC <> 0) then
            ReleaseDC(0, hDefaultDC);

        if (Assigned(pBmpInfoUnion)) then
            Dispose(pBmpInfoUnion);
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.GetBitmap(width, height: Integer; bmpType: TBitmapHandleType;
        pixelFormat: Vcl.Graphics.TPixelFormat; alphaFormat: TAlphaFormat = afIgnored): Vcl.Graphics.TBitmap;
var
    success: Boolean;
begin
    success := False;

    // todo -cFeature -oJean: implements caching if possible, but be careful, resizing an existing
    //                        bitmap is highly slower than set size on a newly created bitmap
    Result := Vcl.Graphics.TBitmap.Create;

    try
        Result.HandleType  := bmpType;
        Result.PixelFormat := pixelFormat;

        // set bitmap size
        if ((width <> 0) and (height <> 0)) then
            Result.SetSize(width, height)
        else
        if (width <> 0) then
            Result.Width := width
        else
        if (height <> 0) then
            Result.Height := height;

        // if pixel format is 32 bit, set alpha format
        if (pixelFormat = pf32bit) then
            Result.AlphaFormat := alphaFormat;

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.ConfigBitmap(pBitmap: Vcl.Graphics.TBitmap; width, height: Integer;
        higherOnly: Boolean; bmpType: TBitmapHandleType; pixelFormat: Vcl.Graphics.TPixelFormat;
        alphaFormat: TAlphaFormat);
begin
    if (not Assigned(pBitmap)) then
        Exit;

    // configure handle type if needed
    if (pBitmap.HandleType <> bmpType) then
        pBitmap.HandleType := bmpType;

    // configure pixel format if needed
    if (pBitmap.PixelFormat <> pixelFormat) then
        pBitmap.PixelFormat := pixelFormat;

    // if pixel format is 32 bit, configure alpha format if needed
    if ((pixelFormat = pf32bit) and (pBitmap.AlphaFormat <> alphaFormat)) then
        pBitmap.AlphaFormat := alphaFormat;

    // do change size only if new size is higher than current size?
    if (higherOnly) then
    begin
        // bitmap needs a higher size?
        if ((width > pBitmap.Width) or (height > pBitmap.Height)) then
            // set bitmap size
            pBitmap.SetSize(Max(width, pBitmap.Width), Max(height, pBitmap.Height));
    end
    else
        // bitmap size has changed?
        if ((width <> pBitmap.Width) or (height <> pBitmap.Height)) then
            // set bitmap size
            pBitmap.SetSize(width, height);
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Clear(pBitmap: Vcl.Graphics.TBitmap; const rect: TRect);
begin
    // validate bitmap
    if (not Assigned(pBitmap) or (pBitmap.Width <= 0) or (pBitmap.Height <= 0)) then
        Exit;

    // clear bitmap canvas
    pBitmap.Canvas.Brush.Color := Vcl.Graphics.clNone;
    pBitmap.Canvas.Brush.Style := bsSolid;
    pBitmap.Canvas.FillRect(rect);
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Clear(pBitmap: Vcl.Graphics.TBitmap);
begin
    // clear the entire bitmap
    Clear(pBitmap, TRect.Create(0, 0, pBitmap.Width, pBitmap.Height));
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Fill(pCanvas: TCanvas; const rect: TRect; const color: TWColor);
begin
    if (not Assigned(pCanvas)) then
        Exit;

    // fill canvas region with color
    pCanvas.Brush.Color := color.GetColor();
    pCanvas.Brush.Style := bsSolid;
    pCanvas.FillRect(rect);
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Fill(pBitmap: Vcl.Graphics.TBitmap; const rect: TRect; const color: TWColor);
var
    fillRect: TRect;
begin
    // validate bitmap
    if (not Assigned(pBitmap) or (pBitmap.Width <= 0) or (pBitmap.Height <= 0)) then
        Exit;

    // limit the rect to the bitmap surface
    fillRect := TRect.Intersect(TRect.Create(0, 0, pBitmap.Width, pBitmap.Height), rect);

    // fill bitmap region with color
    Fill(pBitmap.Canvas, fillRect, color);
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Fill(pBitmap: Vcl.Graphics.TBitmap; const color: TWColor);
begin
    // fill entire bitmap with color
    Fill(pBitmap, TRect.Create(0, 0, pBitmap.Width, pBitmap.Height), color);
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Copy32(const rect: TRect; pSrc: Vcl.Graphics.TBitmap; pDst: Pointer);
var
    width, height, startX, startY, endY, y:         Integer;
    pixelSize:                                      Cardinal;
    stride, lineStart, lineLength, offset, yOffset: NativeUInt;
    copyRect:                                       TRect;
    pDestBmp:                                       PByte;
    pLine:                                          PWRGBQuadArray;
begin
    if ((not Assigned(pSrc)) or (not Assigned(pDst))) then
        Exit;

    if (pSrc.PixelFormat <> pf32bit) then
        Exit;

    // get bitmap size
    width  := pSrc.Width;
    height := pSrc.Height;

    // calculate copy rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, width, height, copyRect)) then
        Exit;

    // needed before ScanLine
    GdiFlush;

    // get area to blend values
    startX := copyRect.Left;
    startY := copyRect.Top;
    endY   := copyRect.Bottom;

    // calculate pixel size
    pixelSize  := SizeOf(TRGBQuad);
    stride     := CalculateStride(width, 32);
    lineStart  := startX * Integer(pixelSize);
    lineLength := copyRect.Width * Integer(pixelSize);
    pDestBmp   := PByte(pDst);

    // iterate through overlay and mask lines
    for y := startY to endY - 1 do
    begin
        // get line
        pLine := PWRGBQuadArray(pSrc.ScanLine[y]);

        // calculate destination offset
        yOffset := y * Integer(stride);

        // calculate destination pixel start offset
        offset := yOffset + lineStart;

        // copy line
        CopyMemory(@pDestBmp[offset], @pLine[startX], lineLength);
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.CalculateStride(width, pixelFormat: NativeUInt): NativeUInt;
begin
    // NOTE stride is calculated using the formula provided in MS documentation here:
    // https://msdn.microsoft.com/en-us/library/windows/desktop/dd318229%28v=vs.85%29.aspx
    Result := ((((width * pixelFormat) + 31) and (not 31)) shr 3);
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.PixelFormatToColorDepth(pixelFormat: Vcl.Graphics.TPixelFormat): Cardinal;
begin
    case (pixelFormat) of
        pfDevice: Result := 0;
        pf1bit:   Result := 1;
        pf4bit:   Result := 4;
        pf8bit:   Result := 8;
        pf15bit:  Result := 15;
        pf16bit:  Result := 16;
        pf24bit:  Result := 24;
        pf32bit:  Result := 32;
        pfCustom: raise Exception.Create('Unsupported pixel format');
    else
        raise Exception.Create('Unknown pixel format');
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.BitmapEquals(const rect: TRect; const pBm1, pBm2: Pointer;
        width, height: LONG; pixelFormat: WORD): Boolean;
var
    bitmapRect:                                                           TRect;
    pixelSize, stride, startX, startY, endX, endY, x, y, offset, yOffset: NativeUInt;
    pBm1Bytes, pBm2Bytes:                                                 PByte;
begin
    // no both first and second bitmap?
    if ((not Assigned(pBm1)) and (not Assigned(pBm2))) then
        Exit(True);

    // no first bitmap?
    if (not Assigned(pBm1)) then
        Exit(False);

    // no second bitmap?
    if (not Assigned(pBm2)) then
        Exit(False);

    // get bitmap rectangle to iterate
    if (not CalculateBitmapRect(rect, width, height, bitmapRect)) then
        Exit(False);

    pixelSize := (pixelFormat shr 3);
    stride    := CalculateStride(width, pixelFormat);
    startX    := bitmapRect.Left * Integer(pixelSize);
    startY    := bitmapRect.Top;
    endX      := bitmapRect.Right * Integer(pixelSize);
    endY      := bitmapRect.Bottom;
    pBm1Bytes := PByte(pBm1);
    pBm2Bytes := PByte(pBm2);

    // check if end values can be used for the loop
    if ((endX = 0) or (endY = 0)) then
        Exit(False);

    // iterate through lines to blend
    for y := startY to endY - 1 do
    begin
        // calculate y offset
        yOffset := (y * stride);
        x       := startX;

        // iterate through pixels to blend
        while (x < endX) do
        begin
            // calculate pixel offset
            offset := yOffset + x;

            // are pixels equal?
            if (not CompareMem(@pBm1Bytes[offset], @pBm2Bytes[offset], pixelSize)) then
                Exit(False);

            Inc(x, pixelSize);
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Blend24(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap; opacity: Single);
var
    width, height, startX, startY, endX, endY, x, y: Integer;
    pixelSize:                                       Cardinal;
    blendRect:                                       TRect;
    pBm1Line, pBm2Line:                              PWRGBTripleArray;
    red, green, blue:                                Single;
begin
    if (not Assigned(pBm1)) then
        Exit;

    if (not Assigned(pBm2)) then
        Exit;

    // get bitmap width and height
    width  := pBm1.Width;
    height := pBm1.Height;

    // overlay and mask sizes should be identical
    if ((width <> pBm2.Width) or (height <> pBm2.Height)) then
        raise Exception.Create('Sizes does not match');

    // only 24 bit pixel formats are supported
    if ((pBm1.PixelFormat <> pf24bit) or (pBm2.PixelFormat <> pf24bit)) then
        raise Exception.Create('Incompatible pixel formats');

    // needed before ScanLine
    GdiFlush;

    // calculate blend rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, width, height, blendRect)) then
        Exit;

    // get area to blend values
    startX := blendRect.Left;
    startY := blendRect.Top;
    endX   := blendRect.Right;
    endY   := blendRect.Bottom;

    // calculate pixel size
    pixelSize := SizeOf(TRGBTriple);

    // iterate through overlay and mask lines
    for y := startY to endY - 1 do
    begin
        // get lines
        pBm1Line := PWRGBTripleArray(pBm1.ScanLine[y]);
        pBm2Line := PWRGBTripleArray(pBm2.ScanLine[y]);

        // iterate through overlay and mask pixels
        for x := startX to endX - 1 do
        begin
            // are pixels identical?
            if (CompareMem(@pBm1Line[x], @pBm2Line[x], pixelSize)) then
                continue;

            red   := pBm2Line[x].rgbtRed   - pBm1Line[x].rgbtRed;
            green := pBm2Line[x].rgbtGreen - pBm1Line[x].rgbtGreen;
            blue  := pBm2Line[x].rgbtBlue  - pBm1Line[x].rgbtBlue;

            // blend images
            Inc(pBm1Line[x].rgbtRed,   Trunc(opacity * red));
            Inc(pBm1Line[x].rgbtGreen, Trunc(opacity * green));
            Inc(pBm1Line[x].rgbtBlue,  Trunc(opacity * blue));
        end;
    end;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Blend24(const rect: TRect; pBm1: Pointer; const pBm2: Pointer;
        width, height: NativeUInt; opacity: Single);
var
    blendRect:                                                            TRect;
    pixelFormat:                                                          WORD;
    pixelSize, stride, startX, startY, endX, endY, x, y, yOffset, offset: NativeUInt;
    pBm1Bytes, pBm2Bytes:                                                 PByte;
    red, green, blue:                                                     Single;
begin
    if (not Assigned(pBm1)) then
        Exit;

    if (not Assigned(pBm2)) then
        Exit;

    // needed before ScanLine
    GdiFlush;

    // calculate blend rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, width, height, blendRect)) then
        Exit;

    pixelFormat := 24;
    pixelSize   := (pixelFormat shr 3);
    stride      := CalculateStride(width, pixelFormat);
    startX      := blendRect.Left * Integer(pixelSize);
    startY      := blendRect.Top;
    endX        := blendRect.Right * Integer(pixelSize);
    endY        := blendRect.Bottom;
    pBm1Bytes   := PByte(pBm1);
    pBm2Bytes   := PByte(pBm2);

    // check if end values can be used for the loop
    if ((endX = 0) or (endY = 0)) then
        Exit;

    // iterate through lines to blend
    for y := startY to endY - 1 do
    begin
        // calculate y offset
        yOffset := (y * stride);
        x       := startX;

        // iterate through pixels to blend
        while (x < endX) do
        begin
            // calculate pixel offset
            offset := yOffset + x;

            // are pixels identical?
            if (CompareMem(@pBm1Bytes[offset], @pBm2Bytes[offset], pixelSize)) then
            begin
                Inc(x, pixelSize);
                continue;
            end;

            red   := pBm2Bytes[offset]     - pBm1Bytes[offset];
            green := pBm2Bytes[offset + 1] - pBm1Bytes[offset + 1];
            blue  := pBm2Bytes[offset + 2] - pBm1Bytes[offset + 2];

            // blend pixels (NOTE pixel data is BGR)
            Inc(pBm1Bytes[offset],     Trunc(opacity * red));
            Inc(pBm1Bytes[offset + 1], Trunc(opacity * green));
            Inc(pBm1Bytes[offset + 2], Trunc(opacity * blue));

            Inc(x, pixelSize);
        end;
    end;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Blend32(const rect: TRect; pBm1, pBm2: Vcl.Graphics.TBitmap; position: Single);
var
    width, height, startX, startY, endX, endY, x, y: Integer;
    pixelSize:                                       Cardinal;
    blendRect:                                       TRect;
    pBm1Line, pBm2Line:                              PWRGBQuadArray;
    red, green, blue, alpha:                         Single;
begin
    if (not Assigned(pBm1)) then
        Exit;

    if (not Assigned(pBm2)) then
        Exit;

    // get bitmap width and height
    width  := pBm1.Width;
    height := pBm1.Height;

    // overlay and mask sizes should be identical
    if ((width <> pBm2.Width) or (height <> pBm2.Height)) then
        raise Exception.Create('Sizes does not match');

    // only 32 bit pixel formats are supported
    if ((pBm1.PixelFormat <> pf32bit) or (pBm2.PixelFormat <> pf32bit)) then
        raise Exception.Create('Incompatible pixel formats');

    // needed before ScanLine
    GdiFlush;

    // calculate blend rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, width, height, blendRect)) then
        Exit;

    // get area to blend values
    startX := blendRect.Left;
    startY := blendRect.Top;
    endX   := blendRect.Right;
    endY   := blendRect.Bottom;

    // calculate pixel size
    pixelSize := SizeOf(TRGBQuad);

    // iterate through overlay and mask lines
    for y := startY to endY - 1 do
    begin
        // get lines
        pBm1Line := PWRGBQuadArray(pBm1.ScanLine[y]);
        pBm2Line := PWRGBQuadArray(pBm2.ScanLine[y]);

        // iterate through overlay and mask pixels
        for x := startX to endX - 1 do
        begin
            // are pixels identical?
            if (CompareMem(@pBm1Line[x], @pBm2Line[x], pixelSize)) then
                continue;

            red   := pBm2Line[x].rgbRed      - pBm1Line[x].rgbRed;
            green := pBm2Line[x].rgbGreen    - pBm1Line[x].rgbGreen;
            blue  := pBm2Line[x].rgbBlue     - pBm1Line[x].rgbBlue;
            alpha := pBm2Line[x].rgbReserved - pBm1Line[x].rgbReserved;

            // blend images
            Inc(pBm1Line[x].rgbRed,      Trunc(position * red));
            Inc(pBm1Line[x].rgbGreen,    Trunc(position * green));
            Inc(pBm1Line[x].rgbBlue,     Trunc(position * blue));
            Inc(pBm1Line[x].rgbReserved, Trunc(position * alpha));
        end;
    end;
end;
//---------------------------------------------------------------------------
class procedure TWGDIHelper.Blend32(const rect: TRect; pBm1: Pointer; const pBm2: Pointer;
        width, height: NativeUInt; position: Single);
var
    blendRect:                                                            TRect;
    pixelFormat:                                                          WORD;
    pixelSize, stride, startX, startY, endX, endY, x, y, yOffset, offset: NativeUInt;
    pBm1Bytes, pBm2Bytes:                                                 PByte;
    red, green, blue, alpha:                                              Single;
begin
    if (not Assigned(pBm1)) then
        Exit;

    if (not Assigned(pBm2)) then
        Exit;

    // needed before ScanLine
    GdiFlush;

    // calculate blend rectangle, and check if rect can fit the bitmap limits
    if (not CalculateBitmapRect(rect, width, height, blendRect)) then
        Exit;

    pixelFormat := 32;
    pixelSize   := (pixelFormat shr 3);
    stride      := CalculateStride(width, pixelFormat);
    startX      := blendRect.Left * Integer(pixelSize);
    startY      := blendRect.Top;
    endX        := blendRect.Right * Integer(pixelSize);
    endY        := blendRect.Bottom;
    pBm1Bytes   := PByte(pBm1);
    pBm2Bytes   := PByte(pBm2);

    // check if end values can be used for the loop
    if ((endX = 0) or (endY = 0)) then
        Exit;

    // iterate through lines to blend
    for y := startY to endY - 1 do
    begin
        // calculate y offset
        yOffset := (y * stride);
        x       := startX;

        // iterate through pixels to blend
        while (x < endX) do
        begin
            // calculate pixel offset
            offset := yOffset + x;

            // are pixels identical?
            if (CompareMem(@pBm1Bytes[offset], @pBm2Bytes[offset], pixelSize)) then
            begin
                Inc(x, pixelSize);
                continue;
            end;

            red   := pBm2Bytes[offset]     - pBm1Bytes[offset];
            green := pBm2Bytes[offset + 1] - pBm1Bytes[offset + 1];
            blue  := pBm2Bytes[offset + 2] - pBm1Bytes[offset + 2];
            alpha := pBm2Bytes[offset + 3] - pBm1Bytes[offset + 3];

            // blend pixels (NOTE pixel data is BGRA)
            Inc(pBm1Bytes[offset],     Trunc(position * red));
            Inc(pBm1Bytes[offset + 1], Trunc(position * green));
            Inc(pBm1Bytes[offset + 2], Trunc(position * blue));
            Inc(pBm1Bytes[offset + 3], Trunc(position * alpha));

            Inc(x, pixelSize);
        end;
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.OpacityToAlpha(opacity: Double): Byte;
begin
    Result := Min(Max(Round(opacity * 255.0), 0), 255);
end;
//---------------------------------------------------------------------------
class function TWGDIHelper.DrawTransparentImage(pGraphic: TGraphic; const pos: TPoint; opacity: Double;
        pCanvas: TCanvas; pOverlay: Vcl.Graphics.TBitmap): Boolean;
var
    imageWidth, imageHeight, x, y: Integer;
    pLine:                         PWRGBQuadArray;
    blendFunction:                 TBlendFunction;
    localOverlay:                  Boolean;
begin
    opacity := Min(Max(opacity, 0.0), 1.0);

    // is opacity fully transparent or fully opaque?
    if (opacity = 0.0) then
        Exit(True)
    else
    if (opacity = 1.0) then
    begin
        pCanvas.Draw(pos.X, pos.Y, pGraphic);
        Exit(True);
    end;

    // unfortunately this "magic" solution cannot be used, because unfortunately the VCL not implements
    // the DrawTransparent() function override in the graphic classes that should support it. The
    // Opacity value in this Draw() override do absolutely nothing, and is systematically ignored!!!
    // (Am I the only one which respects the convention in my WTSVGGraphic class?)
    {
    // source graphic supports the alpha transparency natively?
    if (pGraphic.SupportsPartialTransparency) then
    begin
        // draw the source graphic onto the canvas applying the global opacity
        pCanvas.Draw(pos.X, pos.Y, pGraphic, OpacityToAlpha(opacity));
        Exit(True);
    end;
    }

    localOverlay := False;

    // is overlay already created?
    if (not Assigned(pOverlay)) then
    begin
        // create and configure default overlay
        pOverlay             := Vcl.Graphics.TBitmap.Create;
        pOverlay.PixelFormat := pf32bit;
        pOverlay.AlphaFormat := afPremultiplied;
        localOverlay         := True;
    end;

    try
        imageWidth  := pGraphic.Width;
        imageHeight := pGraphic.Height;

        // do update the size?
        if ((pOverlay.Width < imageWidth) or (pOverlay.Height < imageHeight)) then
            pOverlay.SetSize(imageWidth, imageHeight);

        if (pGraphic.SupportsPartialTransparency) then
            Clear(pOverlay);

        // draw the image on the overlay
        pOverlay.Canvas.Draw(0, 0, pGraphic);

        // force alpha vale to 0xff on each pixel in case the alpha transparency is not supported by
        // the source graphic. Unfortunately this is required, because the GDI seems to not respect
        // the alpha channel in this case, and reset its value to 0 after the graphic is drawn on
        // the overlay
        if (not pGraphic.SupportsPartialTransparency) then
            // iterate through overlay lines
            for y := 0 to imageHeight - 1 do
            begin
                // get next line
                pLine := PWRGBQuadArray(pOverlay.ScanLine[y]);

                // force alpha vale to 0xff on each pixel. Unfortunately the GDI seems to not respect the
                // alpha channel and reset it to 0 automatically after any operation
                for x := 0 to imageWidth - 1 do
                    pLine[x].rgbReserved := 255;
            end;

        // initialize blend operation
        blendFunction.BlendOp             := AC_SRC_OVER;
        blendFunction.BlendFlags          := 0;
        blendFunction.SourceConstantAlpha := OpacityToAlpha(opacity);
        blendFunction.AlphaFormat         := AC_SRC_ALPHA;

        // draw image on the final canvas and apply global opacity
        Result := AlphaBlend(pCanvas.Handle, pos.X, pos.Y, imageWidth, imageHeight,
                pOverlay.Canvas.Handle, 0, 0, imageWidth, imageHeight, blendFunction);
    finally
        if (localOverlay) then
            pOverlay.Free;
    end;
end;
//---------------------------------------------------------------------------
// TWGDIPlusHelper
//---------------------------------------------------------------------------
class function TWGDIPlusHelper.Clear(pBitmap: Vcl.Graphics.TBitmap; pGraphics: TGpGraphics): Boolean;
begin
    if (not Assigned(pBitmap)) then
        Exit(False);

    // clear bitmap completely (including alpha channel)
    Result := Clear(pBitmap.Canvas, pGraphics);
end;
//---------------------------------------------------------------------------
class function TWGDIPlusHelper.Clear(pCanvas: TCanvas; pGraphics: TGpGraphics): Boolean;
begin
    if (not Assigned(pCanvas)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // clear canvas completely (including alpha channel)
    pGraphics.Clear($0);

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWGDIPlusHelper.GetGdiplusEncoderClsid(const format: UnicodeString; out pClsid: TGuid): Boolean;
var
    encoders, size, i: Cardinal;
    imageCodecInfo:    AnsiString;
    pCodecArray:       PWImageCodecInfoArray;
    status:            TStatus;
begin
    encoders := 0;
    size     := 0;
    status   := GetImageEncodersSize(encoders, size);

    if ((status <> Ok) or (size = 0)) then
        Exit(False);

    SetLength(imageCodecInfo, size);
    pCodecArray := Pointer(imageCodecInfo);

    status := GetImageEncoders(encoders, size, PImageCodecInfo(pCodecArray));

    if (status <> Ok) then
        Exit(False);

    if (encoders = 0) then
        Exit(False);

    for i := 0 to encoders - 1 do
        if (pCodecArray^[i].MimeType = format) then
        begin
            pClsid := pCodecArray^[i].Clsid;
            Exit(True);
        end;

    Result := False;
end;
//---------------------------------------------------------------------------
class function TWGDIPlusHelper.ToGraphic(pSrc: Vcl.Graphics.TBitmap; encoderType: IEEncoderType;
        compressionLevel: Cardinal): TGraphic;
var
    pixelFormat:       TPixelFormat;
    pBitmap:           IWSmartPointer<TGpBitmap>;
    pStream:           IWSmartPointer<TMemoryStream>;
    pAdapter:          TStreamAdapter;
    targetInfo:        TBitmapData;
    rect:              TGpRect;
    status:            TStatus;
    rowSize, y:        Integer;
    pRow:              PRGBQuad;
    encoderClsId:      TGuid;
    encoderParameters: TEncoderParameters;
    succeeded:         Boolean;
begin
    // only 24 bit or 32 bit pixel formats are supported
    if ((pSrc.PixelFormat <> pf24bit) and (pSrc.PixelFormat <> pf32bit)) then
        Exit(nil);

    // select the correct pixel format
    if (pSrc.PixelFormat = pf32bit) then
        pixelFormat := PixelFormat32bppARGB
    else
        pixelFormat := PixelFormat24bppRGB;

    // create GDI plus bitmap
    pBitmap := TWSmartPointer<TGpBitmap>.Create(TGpBitmap.Create(pSrc.Width, pSrc.Height, pixelFormat));

    if (pBitmap.GetLastStatus <> Ok) then
        Exit(nil);

    rect.X      := 0;
    rect.Y      := 0;
    rect.Width  := pSrc.Width;
    rect.Height := pSrc.Height;

    // lock GDI+ bitmap for write
    status := pBitmap.LockBits(rect, ImageLockModeWrite, pixelFormat, targetInfo);

    if (status <> Ok) then
        Exit(nil);

    // calculate row size
    rowSize := 4 * pSrc.Width;

    // iterate through source bitmap rows to copy
    for y := 0 to pSrc.Height - 1 do
    begin
        // get next row
        pRow := PRGBQuad(pSrc.ScanLine[y]);

        // copy it to GDI+ bitmap
        CopyMemory(Pointer(NativeUInt(targetInfo.Scan0) + NativeUInt(targetInfo.Stride * y)), pRow, rowSize);
    end;

    // unlock bits
    status := pBitmap.UnlockBits(targetInfo);

    if (status <> Ok) then
        Exit(nil);

    Result    := nil;
    succeeded := False;

    try
        // search for destination type to convert to
        case (encoderType) of
            IE_EC_BMP:
            begin
                Result  := Vcl.Graphics.TBitmap.Create;
                pStream := TWSmartPointer<TMemoryStream>.Create();

                // convert bitmap to bitmap
                if (GetGdiplusEncoderClsid('image/bmp', encoderClsId)) then
                begin
                    // open a stream adapter
                    pAdapter := TStreamAdapter.Create(pStream, soReference);

                    // save bitmap to stream
                    status := pBitmap.Save(pAdapter, encoderClsId, nil);

                    if (status <> Ok) then
                        Exit(nil);

                    // copy content to final bitmap image
                    pStream.Position := 0;
                    Result.LoadFromStream(pStream);
                end;
            end;

            IE_EC_JPG:
            begin
                Result  := TJpegImage.Create;
                pStream := TWSmartPointer<TMemoryStream>.Create();

                // convert bitmap to JPG
                if (GetGdiplusEncoderClsid('image/jpeg', encoderClsId)) then
                begin
                    // create compression level parameter
                    encoderParameters                             := Default(TEncoderParameters);
                    encoderParameters.Count                       := 1;
                    encoderParameters.Parameter[0].Guid           := EncoderQuality;
                    encoderParameters.Parameter[0].Type_          := EncoderParameterValueTypeLong;
                    encoderParameters.Parameter[0].NumberOfValues := 1;
                    encoderParameters.Parameter[0].Value          := @compressionLevel;

                    // open a stream adapter
                    pAdapter := TStreamAdapter.Create(pStream, soReference);

                    // save JPG to stream
                    status := pBitmap.Save(pAdapter, encoderClsId, @encoderParameters);

                    if (status <> Ok) then
                        Exit(nil);

                    // copy content to final PNG image
                    pStream.Position := 0;
                    Result.LoadFromStream(pStream);
                end;
            end;

            IE_EC_PNG:
            begin
                Result  := TPngImage.Create;
                pStream := TWSmartPointer<TMemoryStream>.Create();

                // convert bitmap to PNG (transparency will be preserved)
                if (GetGdiplusEncoderClsid('image/png', encoderClsId)) then
                begin
                    // create compression level parameter
                    encoderParameters                             := Default(TEncoderParameters);
                    encoderParameters.Count                       := 1;
                    encoderParameters.Parameter[0].Guid           := EncoderCompression;
                    encoderParameters.Parameter[0].Type_          := EncoderParameterValueTypeLong;
                    encoderParameters.Parameter[0].NumberOfValues := 1;
                    encoderParameters.Parameter[0].Value          := @compressionLevel;

                    // open a stream adapter
                    pAdapter := TStreamAdapter.Create(pStream, soReference);

                    // save PNG to stream
                    status := pBitmap.Save(pAdapter, encoderClsId, @encoderParameters);

                    if (status <> Ok) then
                        Exit(nil);

                    // copy content to final PNG image
                    pStream.Position := 0;
                    Result.LoadFromStream(pStream);
                end;
            end;

            IE_EC_TIFF:
            begin
                Result  := TWicImage.Create;
                pStream := TWSmartPointer<TMemoryStream>.Create();

                // convert bitmap to TIFF
                if (GetGdiplusEncoderClsid('image/tiff', encoderClsId)) then
                begin
                    // open a stream adapter
                    pAdapter := TStreamAdapter.Create(pStream, soReference);

                    // save TIFF to stream
                    status := pBitmap.Save(pAdapter, encoderClsId, nil);

                    if (status <> Ok) then
                        Exit(nil);

                    // copy content to final TIFF image
                    pStream.Position := 0;
                    Result.LoadFromStream(pStream);
                end;
            end;

            IE_EC_GIF:
            begin
                Result  := TGifImage.Create;
                pStream := TWSmartPointer<TMemoryStream>.Create();

                // convert bitmap to GIF
                if (GetGdiplusEncoderClsid('image/gif', encoderClsId)) then
                begin
                    // open a stream adapter
                    pAdapter := TStreamAdapter.Create(pStream, soReference);

                    // save GIF to stream
                    status := pBitmap.Save(pAdapter, encoderClsId, nil);

                    if (status <> Ok) then
                        Exit(nil);

                    // copy content to final GIF image
                    pStream.Position := 0;
                    Result.LoadFromStream(pStream);
                end;
            end;
        else
            Exit;
        end;

        succeeded := True;
    finally
        if (not succeeded) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
class function TWGDIPlusHelper.ToGDIPlusBitmap(const pBitmap: Vcl.Graphics.TBitmap): TGpBitmap;
var
    bmpInfo:         TBitmap;
    pGDIBmpInstance: IWSmartPointer<TGpBitmap>;
    success:         Boolean;
begin
    if (not Assigned(pBitmap)) then
        Exit(nil);

    // get bitmap info
    GetObject(pBitmap.Handle, SizeOf(TBitmap), @bmpInfo);

    // for 32bit image make a copy because FromHBITMAP() won't preserve alpha channel
    if (bmpInfo.bmBitsPixel < 32) then
    begin
        pGDIBmpInstance := TWSmartPointer<TGpBitmap>.Create();
        Exit(pGDIBmpInstance.FromHBITMAP(pBitmap.Handle, 0));
    end;

    success := False;
    Result  := nil;

    try
        // create gdi+ bitmap and flip it vertically
        Result := TGpBitmap.Create(bmpInfo.bmWidth, bmpInfo.bmHeight, bmpInfo.bmWidth * 4,
                PixelFormat32bppARGB, bmpInfo.bmBits);
        Result.RotateFlip(RotateNoneFlipY);

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
// TWImageHelper
//---------------------------------------------------------------------------
class function TWImageHelper.BmpToPng_GDI(pSrc: Vcl.Graphics.TBitmap): TPngImage;
var
    x, y:      Integer;
    pBmpRGBA:  PWRGBQuadArray;
    pPngRGB:   PWRGBTripleArray;
    pPngAlpha: Vcl.Imaging.Pngimage.PByteArray;
    success:   Boolean;
begin
    Result  := nil;
    success := False;

    try
        // convert bitmap to PNG without alpha channel
        Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, pSrc.Width , pSrc.Height);
        Result.CreateAlpha;
        Result.Canvas.CopyMode:= cmSrcCopy;
        Result.Canvas.Draw(0, 0, pSrc);

        // iterate through source bitmap rows
        for y := 0 to pSrc.Height - 1 do
        begin
            // get access to source row pixels
            pBmpRGBA := pSrc.ScanLine[y];

            // get access to dest row pixels
            if (pSrc.AlphaFormat = afPremultiplied) then
                pPngRGB := Result.Scanline[y]
            else
                pPngRGB := nil;

            // get access to dest alpha channel
            pPngAlpha := Result.AlphaScanline[y];

            // iterate through row pixels
            for x := 0 to pSrc.Width - 1 do
            begin
                // copy the alpha channel
                pPngAlpha[x] := pBmpRGBA[x].rgbReserved;

                // is pre-multiplied source bitmap?
                if (pSrc.AlphaFormat = afPremultiplied) then
                    if (pBmpRGBA[x].rgbReserved <> 0) then
                    begin
                        pPngRGB[x].rgbtRed   := Round((pBmpRGBA[x].rgbRed   / pBmpRGBA[x].rgbReserved) * 255);
                        pPngRGB[x].rgbtGreen := Round((pBmpRGBA[x].rgbGreen / pBmpRGBA[x].rgbReserved) * 255);
                        pPngRGB[x].rgbtBlue  := Round((pBmpRGBA[x].rgbBlue  / pBmpRGBA[x].rgbReserved) * 255);
                    end
                    else
                    begin
                        pPngRGB[x].rgbtRed   := Round(pBmpRGBA[x].rgbRed   * 255);
                        pPngRGB[x].rgbtGreen := Round(pBmpRGBA[x].rgbGreen * 255);
                        pPngRGB[x].rgbtBlue  := Round(pBmpRGBA[x].rgbBlue  * 255);
                    end;
            end;
        end;

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
class function TWImageHelper.BitmapTo(const pBitmap: Vcl.Graphics.TBitmap; pImage: TGraphic;
        useGDIPlus: Boolean): Boolean;
var
    pBmp:  IWSmartPointer<Vcl.Graphics.TBitmap>;
    pPng:  IWSmartPointer<TPngImage>;
    pJpg:  IWSmartPointer<TJpegImage>;
    pTiff: IWSmartPointer<TWicImage>;
    pGif:  IWSmartPointer<TGifImage>;
begin
    if (pImage is Vcl.Graphics.TBitmap) then
    begin
        if (useGDIPlus) then
        begin
            pBmp := TWSmartPointer<Vcl.Graphics.TBitmap>.Create (TWGDIPlusHelper.ToGraphic(pBitmap,
                    IE_EC_BMP, 0) as Vcl.Graphics.TBitmap);
            pImage.Assign(pBmp);
        end
        else
            pImage.Assign(pBitmap);

        Exit(True);
    end
    else
    if (pImage is TJpegImage) then
    begin
        if (useGDIPlus) then
        begin
            pJpg := TWSmartPointer<TJpegImage>.Create(TWGDIPlusHelper.ToGraphic(pBitmap, IE_EC_JPG,
                    100) as TJpegImage);
            pImage.Assign(pJpg);
        end
        else
            pImage.Assign(pBitmap);

        Exit(True);
    end
    else
    if (pImage is TPngImage) then
    begin
        if (useGDIPlus) then
            pPng := TWSmartPointer<TPngImage>.Create(TWGDIPlusHelper.ToGraphic(pBitmap, IE_EC_PNG,
                    0) as TPngImage)
        else
            pPng := TWSmartPointer<TPngImage>.Create(BmpToPng_GDI(pBitmap));

        pImage.Assign(pPng);

        Exit(True);
    end
    else
    if (pImage is TWicImage) then
    begin
        if (useGDIPlus) then
        begin
            pTiff := TWSmartPointer<TWicImage>.Create(TWGDIPlusHelper.ToGraphic(pBitmap, IE_EC_TIFF,
                    0) as TWicImage);
            pImage.Assign(pTiff);
        end
        else
            pImage.Assign(pBitmap);

        Exit(True);
    end
    else
    if (pImage is TGifImage) then
    begin
        if (useGDIPlus) then
        begin
            pGif := TWSmartPointer<TGifImage>.Create(TWGDIPlusHelper.ToGraphic(pBitmap, IE_EC_GIF,
                    0) as TGifImage);
            pImage.Assign(pGif);
        end
        else
            pImage.Assign(pBitmap);

        Exit(True);
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
class procedure TWImageHelper.GetProportionalSize(wSrc, hSrc: Integer; var wDestMax, hDestMax: Integer;
        padded: Boolean);
var
    srcSizeRatio, destSizeRatio, wSrcF, hSrcF, wDestMaxF, hDestMaxF: Single;
begin
    wSrcF     := wSrc;
    hSrcF     := hSrc;
    wDestMaxF := wDestMax;
    hDestMaxF := hDestMax;

    // calculate source aspect ratios > 1 = landscape, < 1 = portrait
    if (hSrc > 0) then
        srcSizeRatio := wSrcF / hSrcF
    else
        srcSizeRatio := 0.0;

    // if dest width is zero, use same ratio as source, with dest height as director
    if (wDestMax = 0) then
        wDestMax := Trunc(hDestMaxF * srcSizeRatio)
    else
    // if dest height is zero, use same ratio as source, with dest width as director
    if (hDestMax = 0) then
    begin
        if (srcSizeRatio > 0.0) then
            hDestMax := Trunc(wDestMaxF / srcSizeRatio)
        else
            hDestMax := 0;
    end
    else
    begin
        // calculate destination aspect ratios > 1 = landscape, < 1 = portrait
        destSizeRatio := wDestMaxF / hDestMaxF;

        if (padded) then
        begin
            // calculate with a padded image
            if (srcSizeRatio > 1.0) then
            begin
                // landscape
                if (destSizeRatio > srcSizeRatio) then
                    wDestMax := Floor(hDestMaxF * srcSizeRatio)
                else
                    hDestMax := Floor(wDestMaxF / srcSizeRatio);
            end
            else
            // portrait or square
            if (destSizeRatio < srcSizeRatio) then
            begin
                if (srcSizeRatio > 0.0) then
                    hDestMax := Floor(wDestMaxF / srcSizeRatio)
                else
                    hDestMax := 0;
            end
            else
                wDestMax := Floor(hDestMaxF * srcSizeRatio);
        end
        else
        begin
            // calculate with a cropped image
            if (srcSizeRatio > 1.0) then
            begin
                // landscape
                if (destSizeRatio > srcSizeRatio) then
                    hDestMax := Floor(wDestMaxF / srcSizeRatio)
                else
                    wDestMax := Floor(hDestMaxF * srcSizeRatio);
            end
            else
            // portrait or square
            if (destSizeRatio < srcSizeRatio) then
                wDestMax := Floor(hDestMaxF * srcSizeRatio)
            else
            if (srcSizeRatio > 0.0) then
                hDestMax := Floor(wDestMaxF / srcSizeRatio)
            else
                hDestMax := 0;
        end;
    end;

    // make sure returned result size is non zero
    if (wDestMax = 0) then
        wDestMax := 1;

    if (hDestMax = 0) then
        hDestMax := 1;
end;
//---------------------------------------------------------------------------
// TWClipboardHelper
//---------------------------------------------------------------------------
class function TWClipboardHelper.GetPNGFormatHandle: THandle;
begin
    Result := RegisterClipboardFormat('PNG');
end;
//---------------------------------------------------------------------------
class function TWClipboardHelper.StreamTo(hFormat: THandle; pStream: TStream; len: NativeUInt): Boolean;
var
    offset: NativeUInt;
    hMem:   THandle;
    pMem:   Pointer;
begin
    // keep the current stream offset
    offset := pStream.Position;

    // check if length is out of bounds
    if (NativeUInt(pStream.Position) + len > pStream.Size) then
        len := pStream.Size - pStream.Position;

    if (not OpenClipboard(0)) then
        Exit(False);

    try
        // reserve memory for stream content to copy
        hMem := GlobalAlloc(GMEM_MOVEABLE, len);

        if (hMem = 0) then
            Exit(False);

        // get access to reserved memory
        pMem := GlobalLock(hMem);

        if (not Assigned(pMem)) then
            Exit(False);

        try
            // copy the stream content in reserved memory
            pStream.Read(pMem^, len);
        finally
            // free reserved memory access
            GlobalUnlock(hMem);
        end;

        // copy stream content to clipboard
        SetClipboardData(hFormat, hMem);
    finally
        CloseClipboard;

        pStream.Position := offset;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWClipboardHelper.StringTo(hFormat: THandle; const str: UnicodeString): Boolean;
var
    hMem:       THandle;
    pMem:       Pointer;
    pStrStream: TStringStream;
begin
    // no text?
    if (Length(str) = 0) then
        Exit(False);

    if (not OpenClipboard(0)) then
        Exit(False);

    try
        pStrStream := nil;

        try
            // load text in a string stream
            pStrStream := TStringStream.Create;
            pStrStream.WriteString(str);
            pStrStream.Position := 0;

            // reserve memory for text to copy
            hMem := GlobalAlloc(GMEM_MOVEABLE, pStrStream.Size);

            if (hMem = 0) then
                Exit(False);

            // get access to reserved memory
            pMem := GlobalLock(hMem);

            if (not Assigned(pMem)) then
                Exit(False);

            try
                // copy the SVG xml data in reserved memory
                pStrStream.Read(pMem^, pStrStream.Size);
            finally
                // free reserved memory access
                GlobalUnlock(hMem);
            end;

            // copy string to clipboard
            SetClipboardData(hFormat, hMem);
        finally
            pStrStream.Free;
        end;
    finally
        CloseClipboard;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
class function TWClipboardHelper.TextTo(const text: UnicodeString): Boolean;
begin
    Result := StringTo(CF_TEXT, text);
end;
//---------------------------------------------------------------------------
// TWVCLHelper
//---------------------------------------------------------------------------
class function TWVCLHelper.GetParentForm(pControl: TControl): TCustomForm;
begin
    Result := GetNextParent(pControl, TCustomForm) as TCustomForm;
end;
//---------------------------------------------------------------------------
class function TWVCLHelper.GetNextParent(pControl: TControl; classID: TClass): TObject;
var
    pParent: TControl;
begin
    pParent := pControl;

    // navigate trough parents
    while (Assigned(pParent)) do
        // check if a form
        if (pParent.InheritsFrom(classID)) then
            // cast and return it
            Exit(pParent)
        else
            pParent := pParent.Parent;

    // none
    Result := nil;
end;
//---------------------------------------------------------------------------
class function TWVCLHelper.GetPixelsPerInchRef(pOwner: TComponent): Integer;
var
    pParentForm: TCustomForm;
begin
    if (Assigned(pOwner) and (pOwner is TControl)) then
    begin
        // get the closest parent form
        pParentForm := TWVCLHelper.GetParentForm(pOwner as TControl);

        // if parent form was found, get its pixels per inch value
        if (Assigned(pParentForm) and (pParentForm is TForm)) then
            Exit((pParentForm as TForm).PixelsPerInch);
    end;

    // hardcoded PPI if no other available
    Result := 96;
end;
//---------------------------------------------------------------------------
class function TWVCLHelper.GetSystemPixelsPerInch: Integer;
var
    logicalHeight, physicalHeight, scalingFactor: Single;
    hDCt:                                         HDC;
begin
    hDCt := 0;

    try
        // get screen device context
        hDCt := GetDC(0);

        if (hDCt = 0) then
            Exit(0);

        // get logical and physical resolution
        logicalHeight  := GetDeviceCaps(hDCt, VERTRES);
        physicalHeight := GetDeviceCaps(hDCt, DESKTOPVERTRES);

        // logical height should never be equals to 0
        if (logicalHeight = 0.0) then
            Exit(0);

        // calculate the scaling factor
        scalingFactor := physicalHeight / logicalHeight;
    finally
        if (hDCt <> 0) then
            ReleaseDC(0, hDCt);
    end;

    // calculate the system DPI
    Result := Round(96.0 * scalingFactor);
end;
//---------------------------------------------------------------------------
{$if CompilerVersion < 30}
    class function TWVCLHelper.GetMonitorPixelsPerInch(pComponent: TComponent;
            refDPI: Integer; fGetDpiForMonitor: TWfGetDpiForMonitor): Integer;
{$else}
    class function TWVCLHelper.GetMonitorPixelsPerInch(pComponent: TComponent;
            refDPI: Integer): Integer;
{$ifend}
var
    {$if CompilerVersion < 30}
        hDCt:       HDC;
        dpi:        Integer;
        xDpi, yDpi: UINT;
    {$ifend}

    pParentForm: TCustomForm;
    pMonitor:    TMonitor;
begin
    pMonitor := nil;

    // get monitor on which application is
    if (Assigned(pComponent) and (pComponent is TControl)) then
    begin
        pParentForm := TWVCLHelper.GetParentForm(pComponent as TControl);

        if (Assigned(pParentForm)) then
            pMonitor := Screen.MonitorFromWindow(pParentForm.Handle);
    end
    else
    if (Assigned(Application) and (Application.ActiveFormHandle <> 0)) then
        pMonitor := Screen.MonitorFromWindow(Application.ActiveFormHandle);

    // get monitor pixels per inch
    if (Assigned(pMonitor)) then
        {$if CompilerVersion < 30}
            begin
                dpi := refDPI;

                // is Windows 8.1 or higher?
                if (TWOSWinHelper.CheckWinVersion(6, 3)) then
                begin
                    // GetDpiForMonitor() function should exist for this Windows version, so use it
                    if (Assigned(fGetDpiForMonitor) and (fGetDpiForMonitor(pMonitor.Handle,
                            TMonitorDpiType.MDT_EFFECTIVE_DPI, yDpi, xDpi) = S_OK))
                    then
                        dpi := yDpi
                end
                else
                begin
                    hDCt := 0;

                    try
                        hDCt := GetDC(0);
                        dpi  := GetDeviceCaps(hDCt, LOGPIXELSY);
                    finally
                        if (hDCt <> 0) then
                            ReleaseDC(0, hDCt);
                    end;
                end;

                Exit(dpi);
            end;
        {$else}
            Exit(pMonitor.PixelsPerInch);
        {$ifend}

    // get screen pixels per inch
    if (Assigned(Screen)) then
        Exit(Screen.PixelsPerInch);

    // could get nothing, return default value
    Result := refDPI;
end;
//---------------------------------------------------------------------------
class function TWVCLHelper.ScaleByDPI(value, dpi, refDPI: Integer): Integer;
var
    scaleFactor: Integer;
begin
    // same DPI, nothing to do
    if (dpi = refDPI) then
        Exit(value);

    // calculate scaled value
    scaleFactor := MulDiv(dpi, 100, refDPI);
    Result      := MulDiv(value, scaleFactor, 100);
end;
//---------------------------------------------------------------------------
{$if CompilerVersion < 30}
    class function TWVCLHelper.IsDPIAware(hProcess: THandle; fGetProcessDpiAwareness: TWfGetProcessDpiAwareness): Boolean;
{$else}
    class function TWVCLHelper.IsDPIAware(hProcess: THandle): Boolean;
{$ifend}
var
    dpiAwareness: PROCESS_DPI_AWARENESS;
    res:          HRESULT;
begin
    // is Windows 8.1 or higher?
    if (TWOSWinHelper.CheckWinVersion(6, 3)) then
    begin
        {$if CompilerVersion >= 30}
            // get process awareness
            res := GetProcessDpiAwareness(hProcess, dpiAwareness);

            {$warn SYMBOL_DEPRECATED OFF}
                // succeeded?
                if (res <> S_OK) then
                    // try with older version
                    Exit(IsProcessDPIAware);
            {$warn SYMBOL_DEPRECATED ON}

            // process is DPI aware if any value than PROCESS_DPI_UNAWARE is returned
            Exit (dpiAwareness <> PROCESS_DPI_UNAWARE);
        {$else}
            // if possible, use the GetProcessDPIAwareness() function from the Windows API
            if (Assigned(fGetProcessDpiAwareness)) then
            begin
                res := fGetProcessDpiAwareness(hProcess, dpiAwareness);

                // succeeded?
                if (res <> S_OK) then
                    // try with older version
                    Exit(IsProcessDPIAware);

                // process is DPI aware if any value than PROCESS_DPI_UNAWARE is returned
                Exit (dpiAwareness <> PROCESS_DPI_UNAWARE);
            end;

            Exit(IsProcessDPIAware);
        {$ifend}
    end;

    {$warn SYMBOL_DEPRECATED OFF}
        Result := IsProcessDPIAware;
    {$warn SYMBOL_DEPRECATED ON}
end;
//---------------------------------------------------------------------------
{$if CompilerVersion < 30}
    class function TWVCLHelper.GetDPIAwarenessContext(fGetProcessDpiAwareness: TWfGetProcessDpiAwareness): EDPIAwarenessContext;
{$else}
    class function TWVCLHelper.GetDPIAwarenessContext(pControl: TWinControl): EDPIAwarenessContext;
{$ifend}
var
    {$if CompilerVersion >= 33}
        dpiAwarenessContext: DPI_AWARENESS_CONTEXT;
    {$ifend}
    dpiAwareness: PROCESS_DPI_AWARENESS;
    id:           DWORD;
    hProcess:     THandle;
begin
    // get the application process
    id       := WinApi.Windows.GetCurrentProcessId;
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, id);

    if (hProcess = 0) then
        Exit(E_AC_None);

    {$if CompilerVersion >= 30}
        // get process awareness
        if (GetProcessDpiAwareness(hProcess, dpiAwareness) <> S_OK) then
            Exit(E_AC_None);
    {$else}
        // if possible, use the GetProcessDPIAwareness() function from the Windows API
        if (not Assigned(fGetProcessDpiAwareness) or (fGetProcessDpiAwareness(hProcess, dpiAwareness) <> S_OK)) then
            Exit(E_AC_None);
    {$ifend}

    {$if CompilerVersion >= 33}
        // is Windows 10 build 1607 (Anniversary update) or higher?
        if (TWOSWinHelper.CheckWinVersion(10, 0, 14393)) then
            // can get awareness context from parent control?
            if (Assigned(pControl)) then
            begin
                dpiAwarenessContext := GetWindowDpiAwarenessContext(pControl.Handle);

                // search for matching DPI awareness context
                if (AreDpiAwarenessContextsEqual(dpiAwarenessContext, DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2)) then
                    Exit(E_AC_Per_Monitor_V2)
                else
                if (AreDpiAwarenessContextsEqual(dpiAwarenessContext, DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE)) then
                    Exit(E_AC_Per_Monitor)
                else
                if (AreDpiAwarenessContextsEqual(dpiAwarenessContext, DPI_AWARENESS_CONTEXT_SYSTEM_AWARE)) then
                    Exit(E_AC_System)
                else
                if (AreDpiAwarenessContextsEqual(dpiAwarenessContext, DPI_AWARENESS_CONTEXT_UNAWARE)) then
                    Exit(E_AC_Unaware)
            end;
    {$ifend}

    // convert awareness context from previously found dpi awareness
    case (dpiAwareness) of
        PROCESS_DPI_UNAWARE:           Exit(E_AC_Unaware);
        PROCESS_SYSTEM_DPI_AWARE:      Exit(E_AC_System);
        PROCESS_PER_MONITOR_DPI_AWARE: Exit(E_AC_Per_Monitor);
    else
        Exit(E_AC_None);
    end;
end;
//---------------------------------------------------------------------------
// TWLogHelper
//---------------------------------------------------------------------------
class procedure TWLogHelper.Dump(const pStream: TStream; maxLimit: NativeUInt);
const
    conversionTable: array[0..15] of WideChar = '0123456789ABCDEF';

var
    dumpSize: NativeUInt;
    i, index: NativeInt;
    buffer:   TWByteArray;
    text:     TWWideCharArray;
    hexStr:   UnicodeString;

begin
    // check if dump should be limited in size, truncate it if yes
    if (maxLimit <> 0) then
        dumpSize := Min(NativeUInt(pStream.Size), maxLimit)
    else
        dumpSize := pStream.Size;

    if (dumpSize = 0) then
        Exit;

    // allocate dump temp buffer
    SetLength(buffer, dumpSize);

    // copy dump data
    pStream.Seek(0, soBeginning);
    pStream.Read(buffer[0], dumpSize);

    SetLength(text, Length(buffer) * 2);

    // convert bytes to hex string. NOTE cannot use the BinToHex() function here, because for a
    // strange reason (a VCL bug?) the call results into a stack overflow
    for i := 0 to dumpSize - 1 do
    begin
        index           := i * 2;
        text[index]     := conversionTable[Byte(buffer[i]) shr 4];
        text[index + 1] := conversionTable[Byte(buffer[i]) and $F];
    end;

    //BinToHex(buffer, PWideChar(text), dumpSize);
    SetString(hexStr, PWideChar(text), dumpSize);

    // log dump data
    TWLogHelper.LogToCompiler(UpperCase(hexStr));
end;
//--------------------------------------------------------------------------------------------------
class function TWLogHelper.WinMsgToStr(pOwner: TComponent; const message: TMessage): UnicodeString;
var
    name, wParamName, lParamName: UnicodeString;
begin
    // get messag name
    case (message.Msg) of
        // standard Windows messages. This list was created using the Wine documentation:
        // https://wiki.winehq.org/List_Of_Windows_Messages
        WM_NULL:                        name := 'WM_NULL';
        WM_CREATE:                      name := 'WM_CREATE';
        WM_DESTROY:                     name := 'WM_DESTROY';
        WM_MOVE:                        name := 'WM_MOVE';
        WM_SIZE:                        name := 'WM_SIZE';
        WM_ACTIVATE:                    name := 'WM_ACTIVATE';
        WM_SETFOCUS:                    name := 'WM_SETFOCUS';
        WM_KILLFOCUS:                   name := 'WM_KILLFOCUS';
        WM_ENABLE:                      name := 'WM_ENABLE';
        WM_SETREDRAW:                   name := 'WM_SETREDRAW';
        WM_SETTEXT:                     name := 'WM_SETTEXT';
        WM_GETTEXT:                     name := 'WM_GETTEXT';
        WM_GETTEXTLENGTH:               name := 'WM_GETTEXTLENGTH';
        WM_PAINT:                       name := 'WM_PAINT';
        WM_CLOSE:                       name := 'WM_CLOSE';
        WM_QUERYENDSESSION:             name := 'WM_QUERYENDSESSION';
        WM_QUIT:                        name := 'WM_QUIT';
        WM_QUERYOPEN:                   name := 'WM_QUERYOPEN';
        WM_ERASEBKGND:                  name := 'WM_ERASEBKGND';
        WM_SYSCOLORCHANGE:              name := 'WM_SYSCOLORCHANGE';
        WM_ENDSESSION:                  name := 'WM_ENDSESSION';
        WM_SHOWWINDOW:                  name := 'WM_SHOWWINDOW';
        WM_CTLCOLOR:                    name := 'WM_CTLCOLOR';
        WM_WININICHANGE:                name := 'WM_WININICHANGE';
        WM_DEVMODECHANGE:               name := 'WM_DEVMODECHANGE';
        WM_ACTIVATEAPP:                 name := 'WM_ACTIVATEAPP';
        WM_FONTCHANGE:                  name := 'WM_FONTCHANGE';
        WM_TIMECHANGE:                  name := 'WM_TIMECHANGE';
        WM_CANCELMODE:                  name := 'WM_CANCELMODE';
        WM_SETCURSOR:                   name := 'WM_SETCURSOR';
        WM_MOUSEACTIVATE:               name := 'WM_MOUSEACTIVATE';
        WM_CHILDACTIVATE:               name := 'WM_CHILDACTIVATE';
        WM_QUEUESYNC:                   name := 'WM_QUEUESYNC';
        WM_GETMINMAXINFO:               name := 'WM_GETMINMAXINFO';
        WM_PAINTICON:                   name := 'WM_PAINTICON';
        WM_ICONERASEBKGND:              name := 'WM_ICONERASEBKGND';
        WM_NEXTDLGCTL:                  name := 'WM_NEXTDLGCTL';
        WM_SPOOLERSTATUS:               name := 'WM_SPOOLERSTATUS';
        WM_DRAWITEM:                    name := 'WM_DRAWITEM';
        WM_MEASUREITEM:                 name := 'WM_MEASUREITEM';
        WM_DELETEITEM:                  name := 'WM_DELETEITEM';
        WM_VKEYTOITEM:                  name := 'WM_VKEYTOITEM';
        WM_CHARTOITEM:                  name := 'WM_CHARTOITEM';
        WM_SETFONT:                     name := 'WM_SETFONT';
        WM_GETFONT:                     name := 'WM_GETFONT';
        WM_SETHOTKEY:                   name := 'WM_SETHOTKEY';
        WM_GETHOTKEY:                   name := 'WM_GETHOTKEY';
        WM_QUERYDRAGICON:               name := 'WM_QUERYDRAGICON';
        WM_COMPAREITEM:                 name := 'WM_COMPAREITEM';
        WM_GETOBJECT:                   name := 'WM_GETOBJECT';
        WM_COMPACTING:                  name := 'WM_COMPACTING';
        WM_COMMNOTIFY:                  name := 'WM_COMMNOTIFY';
        WM_WINDOWPOSCHANGING:           name := 'WM_WINDOWPOSCHANGING';
        WM_WINDOWPOSCHANGED:            name := 'WM_WINDOWPOSCHANGED';
        WM_POWER:                       name := 'WM_POWER';
        $49:                            name := 'WM_COPYGLOBALDATA';
        WM_COPYDATA:                    name := 'WM_COPYDATA';
        WM_CANCELJOURNAL:               name := 'WM_CANCELJOURNAL';
        WM_NOTIFY:                      name := 'WM_NOTIFY';
        WM_INPUTLANGCHANGEREQUEST:      name := 'WM_INPUTLANGCHANGEREQUEST';
        WM_INPUTLANGCHANGE:             name := 'WM_INPUTLANGCHANGE';
        WM_TCARD:                       name := 'WM_TCARD';
        WM_HELP:                        name := 'WM_HELP';
        WM_USERCHANGED:                 name := 'WM_USERCHANGED';
        WM_NOTIFYFORMAT:                name := 'WM_NOTIFYFORMAT';
        WM_CONTEXTMENU:                 name := 'WM_CONTEXTMENU';
        WM_STYLECHANGING:               name := 'WM_STYLECHANGING';
        WM_STYLECHANGED:                name := 'WM_STYLECHANGED';
        WM_DISPLAYCHANGE:               name := 'WM_DISPLAYCHANGE';
        WM_GETICON:                     name := 'WM_GETICON';
        WM_SETICON:                     name := 'WM_SETICON';
        WM_NCCREATE:                    name := 'WM_NCCREATE';
        WM_NCDESTROY:                   name := 'WM_NCDESTROY';
        WM_NCCALCSIZE:                  name := 'WM_NCCALCSIZE';
        WM_NCHITTEST:                   name := 'WM_NCHITTEST';
        WM_NCPAINT:                     name := 'WM_NCPAINT';
        WM_NCACTIVATE:                  name := 'WM_NCACTIVATE';
        WM_GETDLGCODE:                  name := 'WM_GETDLGCODE';
        $88:                            name := 'WM_SYNCPAINT';
        WM_NCMOUSEMOVE:                 name := 'WM_NCMOUSEMOVE';
        WM_NCLBUTTONDOWN:               name := 'WM_NCLBUTTONDOWN';
        WM_NCLBUTTONUP:                 name := 'WM_NCLBUTTONUP';
        WM_NCLBUTTONDBLCLK:             name := 'WM_NCLBUTTONDBLCLK';
        WM_NCRBUTTONDOWN:               name := 'WM_NCRBUTTONDOWN';
        WM_NCRBUTTONUP:                 name := 'WM_NCRBUTTONUP';
        WM_NCRBUTTONDBLCLK:             name := 'WM_NCRBUTTONDBLCLK';
        WM_NCMBUTTONDOWN:               name := 'WM_NCMBUTTONDOWN';
        WM_NCMBUTTONUP:                 name := 'WM_NCMBUTTONUP';
        WM_NCMBUTTONDBLCLK:             name := 'WM_NCMBUTTONDBLCLK';
        WM_NCXBUTTONDOWN:               name := 'WM_NCXBUTTONDOWN';
        WM_NCXBUTTONUP:                 name := 'WM_NCXBUTTONUP';
        WM_NCXBUTTONDBLCLK:             name := 'WM_NCXBUTTONDBLCLK';
        EM_GETSEL:                      name := 'EM_GETSEL';
        EM_SETSEL:                      name := 'EM_SETSEL';
        EM_GETRECT:                     name := 'EM_GETRECT';
        EM_SETRECT:                     name := 'EM_SETRECT';
        EM_SETRECTNP:                   name := 'EM_SETRECTNP';
        EM_SCROLL:                      name := 'EM_SCROLL';
        EM_LINESCROLL:                  name := 'EM_LINESCROLL';
        EM_SCROLLCARET:                 name := 'EM_SCROLLCARET';
        EM_GETMODIFY:                   name := 'EM_GETMODIFY';
        EM_SETMODIFY:                   name := 'EM_SETMODIFY';
        EM_GETLINECOUNT:                name := 'EM_GETLINECOUNT';
        EM_LINEINDEX:                   name := 'EM_LINEINDEX';
        EM_SETHANDLE:                   name := 'EM_SETHANDLE';
        EM_GETHANDLE:                   name := 'EM_GETHANDLE';
        EM_GETTHUMB:                    name := 'EM_GETTHUMB';
        EM_LINELENGTH:                  name := 'EM_LINELENGTH';
        EM_REPLACESEL:                  name := 'EM_REPLACESEL';
        $c3:                            name := 'EM_SETFONT';
        EM_GETLINE:                     name := 'EM_GETLINE';
        EM_SETLIMITTEXT:                name := 'EM_SETLIMITTEXT';
        EM_CANUNDO:                     name := 'EM_CANUNDO';
        EM_UNDO:                        name := 'EM_UNDO';
        EM_FMTLINES:                    name := 'EM_FMTLINES';
        EM_LINEFROMCHAR:                name := 'EM_LINEFROMCHAR';
        $ca:                            name := 'EM_SETWORDBREAK';
        EM_SETTABSTOPS:                 name := 'EM_SETTABSTOPS';
        EM_SETPASSWORDCHAR:             name := 'EM_SETPASSWORDCHAR';
        EM_EMPTYUNDOBUFFER:             name := 'EM_EMPTYUNDOBUFFER';
        EM_GETFIRSTVISIBLELINE:         name := 'EM_GETFIRSTVISIBLELINE ';
        EM_SETREADONLY:                 name := 'EM_SETREADONLY';
        EM_SETWORDBREAKPROC:            name := 'EM_SETWORDBREAKPROC';
        EM_GETWORDBREAKPROC:            name := 'EM_GETWORDBREAKPROC';
        EM_GETPASSWORDCHAR:             name := 'EM_GETPASSWORDCHAR';
        EM_SETMARGINS:                  name := 'EM_SETMARGINS';
        EM_GETMARGINS:                  name := 'EM_GETMARGINS';
        EM_GETLIMITTEXT:                name := 'EM_GETLIMITTEXT';
        EM_POSFROMCHAR:                 name := 'EM_POSFROMCHAR';
        EM_CHARFROMPOS:                 name := 'EM_CHARFROMPOS';
        EM_SETIMESTATUS:                name := 'EM_SETIMESTATUS';
        EM_GETIMESTATUS:                name := 'EM_GETIMESTATUS';
        SBM_SETPOS:                     name := 'SBM_SETPOS';
        SBM_GETPOS:                     name := 'SBM_GETPOS';
        SBM_SETRANGE:                   name := 'SBM_SETRANGE';
        SBM_GETRANGE:                   name := 'SBM_GETRANGE';
        SBM_ENABLE_ARROWS:              name := 'SBM_ENABLE_ARROWS';
        SBM_SETRANGEREDRAW:             name := 'SBM_SETRANGEREDRAW';
        SBM_SETSCROLLINFO:              name := 'SBM_SETSCROLLINFO';
        SBM_GETSCROLLINFO:              name := 'SBM_GETSCROLLINFO';
        SBM_GETSCROLLBARINFO:           name := 'SBM_GETSCROLLBARINFO';
        BM_GETCHECK:                    name := 'BM_GETCHECK';
        BM_SETCHECK:                    name := 'BM_SETCHECK';
        BM_GETSTATE:                    name := 'BM_GETSTATE';
        BM_SETSTATE:                    name := 'BM_SETSTATE';
        BM_SETSTYLE:                    name := 'BM_SETSTYLE';
        BM_CLICK:                       name := 'BM_CLICK';
        BM_GETIMAGE:                    name := 'BM_GETIMAGE';
        BM_SETIMAGE:                    name := 'BM_SETIMAGE';
        BM_SETDONTCLICK:                name := 'BM_SETDONTCLICK';
        WM_INPUT:                       name := 'WM_INPUT';
        WM_KEYDOWN:                     name := 'WM_KEYDOWN or WM_KEYFIRST';
        WM_KEYUP:                       name := 'WM_KEYUP';
        WM_CHAR:                        name := 'WM_CHAR';
        WM_DEADCHAR:                    name := 'WM_DEADCHAR';
        WM_SYSKEYDOWN:                  name := 'WM_SYSKEYDOWN';
        WM_SYSKEYUP:                    name := 'WM_SYSKEYUP';
        WM_SYSCHAR:                     name := 'WM_SYSCHAR';
        WM_SYSDEADCHAR:                 name := 'WM_SYSDEADCHAR';
        $108:                           name := 'WM_KEYLAST';
        $109:                           name := 'WM_UNICHAR or WM_WNT_CONVERTREQUESTEX';
        $10a:                           name := 'WM_CONVERTREQUEST';
        $10b:                           name := 'WM_CONVERTRESULT';
        $10c:                           name := 'WM_INTERIM';
        WM_IME_STARTCOMPOSITION:        name := 'WM_IME_STARTCOMPOSITION';
        WM_IME_ENDCOMPOSITION:          name := 'WM_IME_ENDCOMPOSITION';
        WM_IME_COMPOSITION:             name := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
        WM_INITDIALOG:                  name := 'WM_INITDIALOG';
        WM_COMMAND:                     name := 'WM_COMMAND';
        WM_SYSCOMMAND:                  name := 'WM_SYSCOMMAND';
        WM_TIMER:                       name := 'WM_TIMER';
        WM_HSCROLL:                     name := 'WM_HSCROLL';
        WM_VSCROLL:                     name := 'WM_VSCROLL';
        WM_INITMENU:                    name := 'WM_INITMENU';
        WM_INITMENUPOPUP:               name := 'WM_INITMENUPOPUP';
        $118:                           name := 'WM_SYSTIMER';
        WM_MENUSELECT:                  name := 'WM_MENUSELECT';
        WM_MENUCHAR:                    name := 'WM_MENUCHAR';
        WM_ENTERIDLE:                   name := 'WM_ENTERIDLE';
        WM_MENURBUTTONUP:               name := 'WM_MENURBUTTONUP';
        WM_MENUDRAG:                    name := 'WM_MENUDRAG';
        WM_MENUGETOBJECT:               name := 'WM_MENUGETOBJECT';
        WM_UNINITMENUPOPUP:             name := 'WM_UNINITMENUPOPUP';
        WM_MENUCOMMAND:                 name := 'WM_MENUCOMMAND';
        WM_CHANGEUISTATE:               name := 'WM_CHANGEUISTATE';
        WM_UPDATEUISTATE:               name := 'WM_UPDATEUISTATE';
        WM_QUERYUISTATE:                name := 'WM_QUERYUISTATE';
        WM_CTLCOLORMSGBOX:              name := 'WM_CTLCOLORMSGBOX';
        WM_CTLCOLOREDIT:                name := 'WM_CTLCOLOREDIT';
        WM_CTLCOLORLISTBOX:             name := 'WM_CTLCOLORLISTBOX';
        WM_CTLCOLORBTN:                 name := 'WM_CTLCOLORBTN';
        WM_CTLCOLORDLG:                 name := 'WM_CTLCOLORDLG';
        WM_CTLCOLORSCROLLBAR:           name := 'WM_CTLCOLORSCROLLBAR';
        WM_CTLCOLORSTATIC:              name := 'WM_CTLCOLORSTATIC';
        WM_MOUSEMOVE:                   name := 'WM_MOUSEMOVE or WM_MOUSEFIRST';
        WM_LBUTTONDOWN:                 name := 'WM_LBUTTONDOWN';
        WM_LBUTTONUP:                   name := 'WM_LBUTTONUP';
        WM_LBUTTONDBLCLK:               name := 'WM_LBUTTONDBLCLK';
        WM_RBUTTONDOWN:                 name := 'WM_RBUTTONDOWN';
        WM_RBUTTONUP:                   name := 'WM_RBUTTONUP';
        WM_RBUTTONDBLCLK:               name := 'WM_RBUTTONDBLCLK';
        WM_MBUTTONDOWN:                 name := 'WM_MBUTTONDOWN';
        WM_MBUTTONUP:                   name := 'WM_MBUTTONUP';
        WM_MBUTTONDBLCLK:               name := 'WM_MBUTTONDBLCLK or WM_MOUSELAST';
        WM_MOUSELAST:                   name := 'WM_MOUSELAST';
        WM_MOUSEWHEEL:                  name := 'WM_MOUSEWHEEL';
        WM_XBUTTONDOWN:                 name := 'WM_XBUTTONDOWN';
        WM_XBUTTONUP:                   name := 'WM_XBUTTONUP';
        WM_XBUTTONDBLCLK:               name := 'WM_XBUTTONDBLCLK';
        WM_PARENTNOTIFY:                name := 'WM_PARENTNOTIFY';
        WM_ENTERMENULOOP:               name := 'WM_ENTERMENULOOP';
        WM_EXITMENULOOP:                name := 'WM_EXITMENULOOP';
        WM_NEXTMENU:                    name := 'WM_NEXTMENU';
        WM_SIZING:                      name := 'WM_SIZING';
        WM_CAPTURECHANGED:              name := 'WM_CAPTURECHANGED';
        WM_MOVING:                      name := 'WM_MOVING';
        WM_POWERBROADCAST:              name := 'WM_POWERBROADCAST';
        WM_DEVICECHANGE:                name := 'WM_DEVICECHANGE';
        WM_MDICREATE:                   name := 'WM_MDICREATE';
        WM_MDIDESTROY:                  name := 'WM_MDIDESTROY';
        WM_MDIACTIVATE:                 name := 'WM_MDIACTIVATE';
        WM_MDIRESTORE:                  name := 'WM_MDIRESTORE';
        WM_MDINEXT:                     name := 'WM_MDINEXT';
        WM_MDIMAXIMIZE:                 name := 'WM_MDIMAXIMIZE';
        WM_MDITILE:                     name := 'WM_MDITILE';
        WM_MDICASCADE:                  name := 'WM_MDICASCADE';
        WM_MDIICONARRANGE:              name := 'WM_MDIICONARRANGE';
        WM_MDIGETACTIVE:                name := 'WM_MDIGETACTIVE';
        WM_MDISETMENU:                  name := 'WM_MDISETMENU';
        WM_ENTERSIZEMOVE:               name := 'WM_ENTERSIZEMOVE';
        WM_EXITSIZEMOVE:                name := 'WM_EXITSIZEMOVE';
        WM_DROPFILES:                   name := 'WM_DROPFILES';
        WM_MDIREFRESHMENU:              name := 'WM_MDIREFRESHMENU';
        $280:                           name := 'WM_IME_REPORT';
        WM_IME_SETCONTEXT:              name := 'WM_IME_SETCONTEXT';
        WM_IME_NOTIFY:                  name := 'WM_IME_NOTIFY';
        WM_IME_CONTROL:                 name := 'WM_IME_CONTROL';
        WM_IME_COMPOSITIONFULL:         name := 'WM_IME_COMPOSITIONFULL';
        WM_IME_SELECT:                  name := 'WM_IME_SELECT';
        WM_IME_CHAR:                    name := 'WM_IME_CHAR';
        WM_IME_REQUEST:                 name := 'WM_IME_REQUEST';
        WM_IME_KEYDOWN:                 name := 'WM_IME_KEYDOWN';
        WM_IME_KEYUP:                   name := 'WM_IME_KEYUP';
        WM_NCMOUSEHOVER:                name := 'WM_NCMOUSEHOVER';
        WM_MOUSEHOVER:                  name := 'WM_MOUSEHOVER';
        WM_NCMOUSELEAVE:                name := 'WM_NCMOUSELEAVE';
        WM_MOUSELEAVE:                  name := 'WM_MOUSELEAVE';
        WM_CUT:                         name := 'WM_CUT';
        WM_COPY:                        name := 'WM_COPY';
        WM_PASTE:                       name := 'WM_PASTE';
        WM_CLEAR:                       name := 'WM_CLEAR';
        WM_UNDO:                        name := 'WM_UNDO';
        WM_RENDERFORMAT:                name := 'WM_RENDERFORMAT';
        WM_RENDERALLFORMATS:            name := 'WM_RENDERALLFORMATS';
        WM_DESTROYCLIPBOARD:            name := 'WM_DESTROYCLIPBOARD';
        WM_DRAWCLIPBOARD:               name := 'WM_DRAWCLIPBOARD';
        WM_PAINTCLIPBOARD:              name := 'WM_PAINTCLIPBOARD';
        WM_VSCROLLCLIPBOARD:            name := 'WM_VSCROLLCLIPBOARD';
        WM_SIZECLIPBOARD:               name := 'WM_SIZECLIPBOARD';
        WM_ASKCBFORMATNAME:             name := 'WM_ASKCBFORMATNAME';
        WM_CHANGECBCHAIN:               name := 'WM_CHANGECBCHAIN';
        WM_HSCROLLCLIPBOARD:            name := 'WM_HSCROLLCLIPBOARD';
        WM_QUERYNEWPALETTE:             name := 'WM_QUERYNEWPALETTE';
        WM_PALETTEISCHANGING:           name := 'WM_PALETTEISCHANGING';
        WM_PALETTECHANGED:              name := 'WM_PALETTECHANGED';
        WM_HOTKEY:                      name := 'WM_HOTKEY';
        WM_PRINT:                       name := 'WM_PRINT';
        WM_PRINTCLIENT:                 name := 'WM_PRINTCLIENT';
        WM_APPCOMMAND:                  name := 'WM_APPCOMMAND';
        WM_HANDHELDFIRST:               name := 'WM_HANDHELDFIRST';
        WM_HANDHELDLAST:                name := 'WM_HANDHELDLAST';
        $360:                           name := 'WM_AFXFIRST';
        $37f:                           name := 'WM_AFXLAST';
        WM_PENWINFIRST:                 name := 'WM_PENWINFIRST';
        $381:                           name := 'WM_RCRESULT';
        $382:                           name := 'WM_HOOKRCRESULT';
        $383:                           name := 'WM_GLOBALRCCHANGE or WM_PENMISCINFO';
        $384:                           name := 'WM_SKB';
        $385:                           name := 'WM_HEDITCTL or WM_PENCTL';
        $386:                           name := 'WM_PENMISC';
        $387:                           name := 'WM_CTLINIT';
        $388:                           name := 'WM_PENEVENT';
        WM_PENWINLAST:                  name := 'WM_PENWINLAST';
        $400:                           name := 'DDM_SETFMT or DM_GETDEFID or NIN_SELECT or TBM_GETPOS or WM_PSD_PAGESETUPDLG or WM_USER';
        $401:                           name := 'CBEM_INSERTITEMA or DDM_DRAW or DM_SETDEFID or HKM_SETHOTKEY or PBM_SETRANGE or RB_INSERTBANDA or SB_SETTEXTA or TB_ENABLEBUTTON or TBM_GETRANGEMIN or TTM_ACTIVATE or WM_CHOOSEFONT_GETLOGFONT or WM_PSD_FULLPAGERECT';
        $402:                           name := 'CBEM_SETIMAGELIST or DDM_CLOSE or DM_REPOSITION or HKM_GETHOTKEY or PBM_SETPOS or RB_DELETEBAND or SB_GETTEXTA or TB_CHECKBUTTON or TBM_GETRANGEMAX or WM_PSD_MINMARGINRECT';
        $403:                           name := 'CBEM_GETIMAGELIST or DDM_BEGIN or HKM_SETRULES or PBM_DELTAPOS or RB_GETBARINFO or SB_GETTEXTLENGTHA or TBM_GETTIC or TB_PRESSBUTTON or TTM_SETDELAYTIME or WM_PSD_MARGINRECT';
        $404:                           name := 'CBEM_GETITEMA or DDM_END or PBM_SETSTEP or RB_SETBARINFO or SB_SETPARTS or TB_HIDEBUTTON or TBM_SETTIC or TTM_ADDTOOLA or WM_PSD_GREEKTEXTRECT';
        $405:                           name := 'CBEM_SETITEMA or PBM_STEPIT or TB_INDETERMINATE or TBM_SETPOS or TTM_DELTOOLA or WM_PSD_ENVSTAMPRECT';
        $406:                           name := 'CBEM_GETCOMBOCONTROL or PBM_SETRANGE32 or RB_SETBANDINFOA or SB_GETPARTS or TB_MARKBUTTON or TBM_SETRANGE or TTM_NEWTOOLRECTA or WM_PSD_YAFULLPAGERECT';
        $407:                           name := 'CBEM_GETEDITCONTROL or PBM_GETRANGE or RB_SETPARENT or SB_GETBORDERS or TBM_SETRANGEMIN or TTM_RELAYEVENT';
        $408:                           name := 'CBEM_SETEXSTYLE or PBM_GETPOS or RB_HITTEST or SB_SETMINHEIGHT or TBM_SETRANGEMAX or TTM_GETTOOLINFOA';
        $409:                           name := 'CBEM_GETEXSTYLE or CBEM_GETEXTENDEDSTYLE or PBM_SETBARCOLOR or RB_GETRECT or SB_SIMPLE or TB_ISBUTTONENABLED or TBM_CLEARTICS or TTM_SETTOOLINFOA';
        $40a:                           name := 'CBEM_HASEDITCHANGED or RB_INSERTBANDW or SB_GETRECT or TB_ISBUTTONCHECKED or TBM_SETSEL or TTM_HITTESTA or WIZ_QUERYNUMPAGES';
        $40b:                           name := 'CBEM_INSERTITEMW or RB_SETBANDINFOW or SB_SETTEXTW or TB_ISBUTTONPRESSED or TBM_SETSELSTART or TTM_GETTEXTA or WIZ_NEXT';
        $40c:                           name := 'CBEM_SETITEMW or RB_GETBANDCOUNT or SB_GETTEXTLENGTHW or TB_ISBUTTONHIDDEN or TBM_SETSELEND or TTM_UPDATETIPTEXTA or WIZ_PREV';
        $40d:                           name := 'CBEM_GETITEMW or RB_GETROWCOUNT or SB_GETTEXTW or TB_ISBUTTONINDETERMINATE or TTM_GETTOOLCOUNT';
        $40e:                           name := 'CBEM_SETEXTENDEDSTYLE or RB_GETROWHEIGHT or SB_ISSIMPLE or TB_ISBUTTONHIGHLIGHTED or TBM_GETPTICS or TTM_ENUMTOOLSA';
        $40f:                           name := 'SB_SETICON or TBM_GETTICPOS or TTM_GETCURRENTTOOLA';
        $410:                           name := 'RB_IDTOINDEX or SB_SETTIPTEXTA or TBM_GETNUMTICS or TTM_WINDOWFROMPOINT';
        $411:                           name := 'RB_GETTOOLTIPS or SB_SETTIPTEXTW or TBM_GETSELSTART or TB_SETSTATE or TTM_TRACKACTIVATE';
        $412:                           name := 'RB_SETTOOLTIPS or SB_GETTIPTEXTA or TB_GETSTATE or TBM_GETSELEND or TTM_TRACKPOSITION';
        $413:                           name := 'RB_SETBKCOLOR or SB_GETTIPTEXTW or TB_ADDBITMAP or TBM_CLEARSEL or TTM_SETTIPBKCOLOR';
        $414:                           name := 'RB_GETBKCOLOR or SB_GETICON or TB_ADDBUTTONSA or TBM_SETTICFREQ or TTM_SETTIPTEXTCOLOR';
        $415:                           name := 'RB_SETTEXTCOLOR or TB_INSERTBUTTONA or TBM_SETPAGESIZE or TTM_GETDELAYTIME';
        $416:                           name := 'RB_GETTEXTCOLOR or TB_DELETEBUTTON or TBM_GETPAGESIZE or TTM_GETTIPBKCOLOR';
        $417:                           name := 'RB_SIZETORECT or TB_GETBUTTON or TBM_SETLINESIZE or TTM_GETTIPTEXTCOLOR';
        $418:                           name := 'RB_BEGINDRAG or TB_BUTTONCOUNT or TBM_GETLINESIZE or TTM_SETMAXTIPWIDTH';
        $419:                           name := 'RB_ENDDRAG or TB_COMMANDTOINDEX or TBM_GETTHUMBRECT or TTM_GETMAXTIPWIDTH';
        $41a:                           name := 'RB_DRAGMOVE or TBM_GETCHANNELRECT or TB_SAVERESTOREA or TTM_SETMARGIN';
        $41b:                           name := 'RB_GETBARHEIGHT or TB_CUSTOMIZE or TBM_SETTHUMBLENGTH or TTM_GETMARGIN';
        $41c:                           name := 'RB_GETBANDINFOW or TB_ADDSTRINGA or TBM_GETTHUMBLENGTH or TTM_POP';
        $41d:                           name := 'RB_GETBANDINFOA or TB_GETITEMRECT or TBM_SETTOOLTIPS or TTM_UPDATE';
        $41e:                           name := 'RB_MINIMIZEBAND or TB_BUTTONSTRUCTSIZE or TBM_GETTOOLTIPS or TTM_GETBUBBLESIZE';
        $41f:                           name := 'RB_MAXIMIZEBAND or TBM_SETTIPSIDE or TB_SETBUTTONSIZE or TTM_ADJUSTRECT';
        $420:                           name := 'TBM_SETBUDDY or TB_SETBITMAPSIZE or TTM_SETTITLEA';
        $421:                           name := 'MSG_FTS_JUMP_VA or TB_AUTOSIZE or TBM_GETBUDDY or TTM_SETTITLEW';
        $422:                           name := 'RB_GETBANDBORDERS';
        $423:                           name := 'MSG_FTS_JUMP_QWORD or RB_SHOWBAND or TB_GETTOOLTIPS';
        $424:                           name := 'MSG_REINDEX_REQUEST or TB_SETTOOLTIPS';
        $425:                           name := 'MSG_FTS_WHERE_IS_IT or RB_SETPALETTE or TB_SETPARENT';
        $426:                           name := 'RB_GETPALETTE';
        $427:                           name := 'RB_MOVEBAND or TB_SETROWS';
        $428:                           name := 'TB_GETROWS';
        $429:                           name := 'TB_GETBITMAPFLAGS';
        $42a:                           name := 'TB_SETCMDID';
        $42b:                           name := 'RB_PUSHCHEVRON or TB_CHANGEBITMAP';
        $42c:                           name := 'TB_GETBITMAP';
        $42d:                           name := 'MSG_GET_DEFFONT or TB_GETBUTTONTEXTA';
        $42e:                           name := 'TB_REPLACEBITMAP';
        $42f:                           name := 'TB_SETINDENT';
        $430:                           name := 'TB_SETIMAGELIST';
        $431:                           name := 'TB_GETIMAGELIST';
        $432:                           name := 'TB_LOADIMAGES or EM_CANPASTE or TTM_ADDTOOLW';
        $433:                           name := 'EM_DISPLAYBAND or TB_GETRECT or TTM_DELTOOLW';
        $434:                           name := 'EM_EXGETSEL or TB_SETHOTIMAGELIST or TTM_NEWTOOLRECTW';
        $435:                           name := 'EM_EXLIMITTEXT or TB_GETHOTIMAGELIST or TTM_GETTOOLINFOW';
        $436:                           name := 'EM_EXLINEFROMCHAR or TB_SETDISABLEDIMAGELIST or TTM_SETTOOLINFOW';
        $437:                           name := 'EM_EXSETSEL or TB_GETDISABLEDIMAGELIST or TTM_HITTESTW';
        $438:                           name := 'EM_FINDTEXT or TB_SETSTYLE or TTM_GETTEXTW';
        $439:                           name := 'EM_FORMATRANGE or TB_GETSTYLE or TTM_UPDATETIPTEXTW';
        $43a:                           name := 'EM_GETCHARFORMAT or TB_GETBUTTONSIZE or TTM_ENUMTOOLSW';
        $43b:                           name := 'EM_GETEVENTMASK or TB_SETBUTTONWIDTH or TTM_GETCURRENTTOOLW';
        $43c:                           name := 'EM_GETOLEINTERFACE or TB_SETMAXTEXTROWS';
        $43d:                           name := 'EM_GETPARAFORMAT or TB_GETTEXTROWS';
        $43e:                           name := 'EM_GETSELTEXT or TB_GETOBJECT';
        $43f:                           name := 'EM_HIDESELECTION or TB_GETBUTTONINFOW';
        $440:                           name := 'EM_PASTESPECIAL or TB_SETBUTTONINFOW';
        $441:                           name := 'EM_REQUESTRESIZE or TB_GETBUTTONINFOA';
        $442:                           name := 'EM_SELECTIONTYPE or TB_SETBUTTONINFOA';
        $443:                           name := 'EM_SETBKGNDCOLOR or TB_INSERTBUTTONW';
        $444:                           name := 'EM_SETCHARFORMAT or TB_ADDBUTTONSW';
        $445:                           name := 'EM_SETEVENTMASK or TB_HITTEST';
        $446:                           name := 'EM_SETOLECALLBACK or TB_SETDRAWTEXTFLAGS';
        $447:                           name := 'EM_SETPARAFORMAT or TB_GETHOTITEM';
        $448:                           name := 'EM_SETTARGETDEVICE or TB_SETHOTITEM';
        $449:                           name := 'EM_STREAMIN or TB_SETANCHORHIGHLIGHT';
        $44a:                           name := 'EM_STREAMOUT or TB_GETANCHORHIGHLIGHT';
        $44b:                           name := 'EM_GETTEXTRANGE or TB_GETBUTTONTEXTW';
        $44c:                           name := 'EM_FINDWORDBREAK or TB_SAVERESTOREW';
        $44d:                           name := 'EM_SETOPTIONS or TB_ADDSTRINGW';
        $44e:                           name := 'EM_GETOPTIONS or TB_MAPACCELERATORA';
        $44f:                           name := 'EM_FINDTEXTEX or TB_GETINSERTMARK';
        $450:                           name := 'EM_GETWORDBREAKPROCEX or TB_SETINSERTMARK';
        $451:                           name := 'EM_SETWORDBREAKPROCEX or TB_INSERTMARKHITTEST';
        $452:                           name := 'EM_SETUNDOLIMIT or TB_MOVEBUTTON';
        $453:                           name := 'TB_GETMAXSIZE';
        $454:                           name := 'EM_REDO or TB_SETEXTENDEDSTYLE';
        $455:                           name := 'EM_CANREDO or TB_GETEXTENDEDSTYLE';
        $456:                           name := 'EM_GETUNDONAME or TB_GETPADDING';
        $457:                           name := 'EM_GETREDONAME or TB_SETPADDING';
        $458:                           name := 'EM_STOPGROUPTYPING or TB_SETINSERTMARKCOLOR';
        $459:                           name := 'EM_SETTEXTMODE or TB_GETINSERTMARKCOLOR';
        $45a:                           name := 'EM_GETTEXTMODE or TB_MAPACCELERATORW';
        $45b:                           name := 'EM_AUTOURLDETECT or TB_GETSTRINGW';
        $45c:                           name := 'EM_GETAUTOURLDETECT or TB_GETSTRINGA';
        $45d:                           name := 'EM_SETPALETTE';
        $45e:                           name := 'EM_GETTEXTEX';
        $45f:                           name := 'EM_GETTEXTLENGTHEX';
        $460:                           name := 'EM_SHOWSCROLLBAR';
        $461:                           name := 'EM_SETTEXTEX';
        $463:                           name := 'TAPI_REPLY';
        $464:                           name := 'ACM_OPENA or BFFM_SSETSTATUSTEXTA or CDM_FIRST or CDM_GETSPEC or EM_SETPUNCTUATION or IPM_CLEARADDRESS or WM_CAP_UNICODE_START';
        $465:                           name := 'ACM_PLAY or BFFM_SENABLEOK or CDM_GETFILEPATH or EM_GETPUNCTUATION or IPM_SETADDRESS or PSM_SETCURSEL or UDM_SETRANGE or WM_CHOOSEFONT_SETLOGFONT';
        $466:                           name := 'ACM_STOP or BFFM_SSETSELECTIONA or CDM_GETFOLDERPATH or EM_SETWORDWRAPMODE or IPM_GETADDRESS or PSM_REMOVEPAGE or UDM_GETRANGE or WM_CAP_SET_CALLBACK_ERRORW or WM_CHOOSEFONT_SETFLAGS';
        $467:                           name := 'ACM_OPENW or BFFM_SSETSELECTIONW or CDM_GETFOLDERIDLIST or EM_GETWORDWRAPMODE or IPM_SETRANGE or PSM_ADDPAGE or UDM_SETPOS or WM_CAP_SET_CALLBACK_STATUSW';
        $468:                           name := 'BFFM_SSETSTATUSTEXTW or CDM_SETCONTROLTEXT or EM_SETIMECOLOR or IPM_SETFOCUS or PSM_CHANGED or UDM_GETPOS';
        $469:                           name := 'CDM_HIDECONTROL or EM_GETIMECOLOR or IPM_ISBLANK or PSM_RESTARTWINDOWS or UDM_SETBUDDY';
        $46a:                           name := 'CDM_SETDEFEXT or EM_SETIMEOPTIONS or PSM_REBOOTSYSTEM or UDM_GETBUDDY';
        $46b:                           name := 'EM_GETIMEOPTIONS or PSM_CANCELTOCLOSE or UDM_SETACCEL';
        $46c:                           name := 'EM_CONVPOSITION or EM_CONVPOSITION or PSM_QUERYSIBLINGS or UDM_GETACCEL';
        $46d:                           name := 'MCIWNDM_GETZOOM or PSM_UNCHANGED or UDM_SETBASE';
        $46e:                           name := 'PSM_APPLY or UDM_GETBASE';
        $46f:                           name := 'PSM_SETTITLEA or UDM_SETRANGE32';
        $470:                           name := 'PSM_SETWIZBUTTONS or UDM_GETRANGE32 or WM_CAP_DRIVER_GET_NAMEW';
        $471:                           name := 'PSM_PRESSBUTTON or UDM_SETPOS32 or WM_CAP_DRIVER_GET_VERSIONW';
        $472:                           name := 'PSM_SETCURSELID or UDM_GETPOS32';
        $473:                           name := 'PSM_SETFINISHTEXTA';
        $474:                           name := 'PSM_GETTABCONTROL';
        $475:                           name := 'PSM_ISDIALOGMESSAGE';
        $476:                           name := 'MCIWNDM_REALIZE or PSM_GETCURRENTPAGEHWND';
        $477:                           name := 'MCIWNDM_SETTIMEFORMATA or PSM_INSERTPAGE';
        $478:                           name := 'EM_SETLANGOPTIONS or MCIWNDM_GETTIMEFORMATA or PSM_SETTITLEW or WM_CAP_FILE_SET_CAPTURE_FILEW';
        $479:                           name := 'EM_GETLANGOPTIONS or MCIWNDM_VALIDATEMEDIA or PSM_SETFINISHTEXTW or WM_CAP_FILE_GET_CAPTURE_FILEW';
        $47a:                           name := 'EM_GETIMECOMPMODE';
        $47b:                           name := 'EM_FINDTEXTW or MCIWNDM_PLAYTO or WM_CAP_FILE_SAVEASW';
        $47c:                           name := 'EM_FINDTEXTEXW or MCIWNDM_GETFILENAMEA';
        $47d:                           name := 'EM_RECONVERSION or MCIWNDM_GETDEVICEA or PSM_SETHEADERTITLEA or WM_CAP_FILE_SAVEDIBW';
        $47e:                           name := 'EM_SETIMEMODEBIAS or MCIWNDM_GETPALETTE or PSM_SETHEADERTITLEW';
        $47f:                           name := 'EM_GETIMEMODEBIAS or MCIWNDM_SETPALETTE or PSM_SETHEADERSUBTITLEA';
        $480:                           name := 'MCIWNDM_GETERRORA or PSM_SETHEADERSUBTITLEW';
        $481:                           name := 'PSM_HWNDTOINDEX';
        $482:                           name := 'PSM_INDEXTOHWND';
        $483:                           name := 'MCIWNDM_SETINACTIVETIMER or PSM_PAGETOINDEX';
        $484:                           name := 'PSM_INDEXTOPAGE';
        $485:                           name := 'DL_BEGINDRAG or MCIWNDM_GETINACTIVETIMER or PSM_IDTOINDEX';
        $486:                           name := 'DL_DRAGGING or PSM_INDEXTOID';
        $487:                           name := 'DL_DROPPED or PSM_GETRESULT';
        $488:                           name := 'DL_CANCELDRAG or PSM_RECALCPAGESIZES';
        $48c:                           name := 'MCIWNDM_GET_SOURCE';
        $48d:                           name := 'MCIWNDM_PUT_SOURCE';
        $48e:                           name := 'MCIWNDM_GET_DEST';
        $48f:                           name := 'MCIWNDM_PUT_DEST';
        $490:                           name := 'MCIWNDM_CAN_PLAY';
        $491:                           name := 'MCIWNDM_CAN_WINDOW';
        $492:                           name := 'MCIWNDM_CAN_RECORD';
        $493:                           name := 'MCIWNDM_CAN_SAVE';
        $494:                           name := 'MCIWNDM_CAN_EJECT';
        $495:                           name := 'MCIWNDM_CAN_CONFIG';
        $496:                           name := 'IE_GETINK or IE_MSGFIRST or MCIWNDM_PALETTEKICK';
        $497:                           name := 'IE_SETINK';
        $498:                           name := 'IE_GETPENTIP';
        $499:                           name := 'IE_SETPENTIP';
        $49a:                           name := 'IE_GETERASERTIP';
        $49b:                           name := 'IE_SETERASERTIP';
        $49c:                           name := 'IE_GETBKGND';
        $49d:                           name := 'IE_SETBKGND';
        $49e:                           name := 'IE_GETGRIDORIGIN';
        $49f:                           name := 'IE_SETGRIDORIGIN';
        $4a0:                           name := 'IE_GETGRIDPEN';
        $4a1:                           name := 'IE_SETGRIDPEN';
        $4a2:                           name := 'IE_GETGRIDSIZE';
        $4a3:                           name := 'IE_SETGRIDSIZE';
        $4a4:                           name := 'IE_GETMODE';
        $4a5:                           name := 'IE_SETMODE';
        $4a6:                           name := 'IE_GETINKRECT or WM_CAP_SET_MCI_DEVICEW';
        $4a7:                           name := 'WM_CAP_GET_MCI_DEVICEW';
        $4b4:                           name := 'WM_CAP_PAL_OPENW';
        $4b5:                           name := 'WM_CAP_PAL_SAVEW';
        $4b8:                           name := 'IE_GETAPPDATA';
        $4b9:                           name := 'IE_SETAPPDATA';
        $4ba:                           name := 'IE_GETDRAWOPTS';
        $4bb:                           name := 'IE_SETDRAWOPTS';
        $4bc:                           name := 'IE_GETFORMAT';
        $4bd:                           name := 'IE_SETFORMAT';
        $4be:                           name := 'IE_GETINKINPUT';
        $4bf:                           name := 'IE_SETINKINPUT';
        $4c0:                           name := 'IE_GETNOTIFY';
        $4c1:                           name := 'IE_SETNOTIFY';
        $4c2:                           name := 'IE_GETRECOG';
        $4c3:                           name := 'IE_SETRECOG';
        $4c4:                           name := 'IE_GETSECURITY';
        $4c5:                           name := 'IE_SETSECURITY';
        $4c6:                           name := 'IE_GETSEL';
        $4c7:                           name := 'IE_SETSEL';
        $4c8:                           name := 'CDM_LAST or EM_SETBIDIOPTIONS or IE_DOCOMMAND or MCIWNDM_NOTIFYMODE';
        $4c9:                           name := 'EM_GETBIDIOPTIONS or IE_GETCOMMAND';
        $4ca:                           name := 'EM_SETTYPOGRAPHYOPTIONS or IE_GETCOUNT';
        $4cb:                           name := 'EM_GETTYPOGRAPHYOPTIONS or IE_GETGESTURE or MCIWNDM_NOTIFYMEDIA';
        $4cc:                           name := 'EM_SETEDITSTYLE or IE_GETMENU';
        $4cd:                           name := 'EM_GETEDITSTYLE or IE_GETPAINTDC or MCIWNDM_NOTIFYERROR';
        $4ce:                           name := 'IE_GETPDEVENT';
        $4cf:                           name := 'IE_GETSELCOUNT';
        $4d0:                           name := 'IE_GETSELITEMS';
        $4d1:                           name := 'IE_GETSTYLE';
        $4db:                           name := 'MCIWNDM_SETTIMEFORMATW';
        $4dc:                           name := 'EM_OUTLINE or MCIWNDM_GETTIMEFORMATW';
        $4dd:                           name := 'EM_GETSCROLLPOS';
        $4de:                           name := 'EM_SETSCROLLPOS';
        $4df:                           name := 'EM_SETFONTSIZE';
        $4e0:                           name := 'EM_GETZOOM or MCIWNDM_GETFILENAMEW';
        $4e1:                           name := 'EM_SETZOOM or MCIWNDM_GETDEVICEW';
        $4e2:                           name := 'EM_GETVIEWKIND';
        $4e3:                           name := 'EM_SETVIEWKIND';
        $4e4:                           name := 'EM_GETPAGE or MCIWNDM_GETERRORW';
        $4e5:                           name := 'EM_SETPAGE';
        $4e6:                           name := 'EM_GETHYPHENATEINFO';
        $4e7:                           name := 'EM_SETHYPHENATEINFO';
        $4eb:                           name := 'EM_GETPAGEROTATE';
        $4ec:                           name := 'EM_SETPAGEROTATE';
        $4ed:                           name := 'EM_GETCTFMODEBIAS';
        $4ee:                           name := 'EM_SETCTFMODEBIAS';
        $4f0:                           name := 'EM_GETCTFOPENSTATUS';
        $4f1:                           name := 'EM_SETCTFOPENSTATUS';
        $4f2:                           name := 'EM_GETIMECOMPTEXT';
        $4f3:                           name := 'EM_ISIME';
        $4f4:                           name := 'EM_GETIMEPROPERTY';
        $50d:                           name := 'EM_GETQUERYRTFOBJ';
        $50e:                           name := 'EM_SETQUERYRTFOBJ';
        $600:                           name := 'FM_GETFOCUS';
        $601:                           name := 'FM_GETDRIVEINFOA';
        $602:                           name := 'FM_GETSELCOUNT';
        $603:                           name := 'FM_GETSELCOUNTLFN';
        $604:                           name := 'FM_GETFILESELA';
        $605:                           name := 'FM_GETFILESELLFNA';
        $606:                           name := 'FM_REFRESH_WINDOWS';
        $607:                           name := 'FM_RELOAD_EXTENSIONS';
        $611:                           name := 'FM_GETDRIVEINFOW';
        $614:                           name := 'FM_GETFILESELW';
        $615:                           name := 'FM_GETFILESELLFNW';
        $659:                           name := 'WLX_WM_SAS';
        $7e8:                           name := 'SM_GETSELCOUNT or UM_GETSELCOUNT or WM_CPL_LAUNCH';
        $7e9:                           name := 'SM_GETSERVERSELA or UM_GETUSERSELA or WM_CPL_LAUNCHED';
        $7ea:                           name := 'SM_GETSERVERSELW or UM_GETUSERSELW';
        $7eb:                           name := 'SM_GETCURFOCUSA or UM_GETGROUPSELA';
        $7ec:                           name := 'SM_GETCURFOCUSW or UM_GETGROUPSELW';
        $7ed:                           name := 'SM_GETOPTIONS or UM_GETCURFOCUSA';
        $7ee:                           name := 'UM_GETCURFOCUSW';
        $7ef:                           name := 'UM_GETOPTIONS';
        $7f0:                           name := 'UM_GETOPTIONS2';
        $1000:                          name := 'LVM_FIRST or LVM_GETBKCOLOR';
        $1001:                          name := 'LVM_SETBKCOLOR';
        $1002:                          name := 'LVM_GETIMAGELIST';
        $1003:                          name := 'LVM_SETIMAGELIST';
        $1004:                          name := 'LVM_GETITEMCOUNT';
        $1005:                          name := 'LVM_GETITEMA';
        $1006:                          name := 'LVM_SETITEMA';
        $1007:                          name := 'LVM_INSERTITEMA';
        $1008:                          name := 'LVM_DELETEITEM';
        $1009:                          name := 'LVM_DELETEALLITEMS';
        $100a:                          name := 'LVM_GETCALLBACKMASK';
        $100b:                          name := 'LVM_SETCALLBACKMASK';
        $100c:                          name := 'LVM_GETNEXTITEM';
        $100d:                          name := 'LVM_FINDITEMA';
        $100e:                          name := 'LVM_GETITEMRECT';
        $100f:                          name := 'LVM_SETITEMPOSITION';
        $1010:                          name := 'LVM_GETITEMPOSITION';
        $1011:                          name := 'LVM_GETSTRINGWIDTHA';
        $1012:                          name := 'LVM_HITTEST';
        $1013:                          name := 'LVM_ENSUREVISIBLE';
        $1014:                          name := 'LVM_SCROLL';
        $1015:                          name := 'LVM_REDRAWITEMS';
        $1016:                          name := 'LVM_ARRANGE';
        $1017:                          name := 'LVM_EDITLABELA';
        $1018:                          name := 'LVM_GETEDITCONTROL';
        $1019:                          name := 'LVM_GETCOLUMNA';
        $101a:                          name := 'LVM_SETCOLUMNA';
        $101b:                          name := 'LVM_INSERTCOLUMNA';
        $101c:                          name := 'LVM_DELETECOLUMN';
        $101d:                          name := 'LVM_GETCOLUMNWIDTH';
        $101e:                          name := 'LVM_SETCOLUMNWIDTH';
        $101f:                          name := 'LVM_GETHEADER';
        $1021:                          name := 'LVM_CREATEDRAGIMAGE';
        $1022:                          name := 'LVM_GETVIEWRECT';
        $1023:                          name := 'LVM_GETTEXTCOLOR';
        $1024:                          name := 'LVM_SETTEXTCOLOR';
        $1025:                          name := 'LVM_GETTEXTBKCOLOR';
        $1026:                          name := 'LVM_SETTEXTBKCOLOR';
        $1027:                          name := 'LVM_GETTOPINDEX';
        $1028:                          name := 'LVM_GETCOUNTPERPAGE';
        $1029:                          name := 'LVM_GETORIGIN';
        $102a:                          name := 'LVM_UPDATE';
        $102b:                          name := 'LVM_SETITEMSTATE';
        $102c:                          name := 'LVM_GETITEMSTATE';
        $102d:                          name := 'LVM_GETITEMTEXTA';
        $102e:                          name := 'LVM_SETITEMTEXTA';
        $102f:                          name := 'LVM_SETITEMCOUNT';
        $1030:                          name := 'LVM_SORTITEMS';
        $1031:                          name := 'LVM_SETITEMPOSITION32';
        $1032:                          name := 'LVM_GETSELECTEDCOUNT';
        $1033:                          name := 'LVM_GETITEMSPACING';
        $1034:                          name := 'LVM_GETISEARCHSTRINGA';
        $1035:                          name := 'LVM_SETICONSPACING';
        $1036:                          name := 'LVM_SETEXTENDEDLISTVIEWSTYLE';
        $1037:                          name := 'LVM_GETEXTENDEDLISTVIEWSTYLE';
        $1038:                          name := 'LVM_GETSUBITEMRECT';
        $1039:                          name := 'LVM_SUBITEMHITTEST';
        $103a:                          name := 'LVM_SETCOLUMNORDERARRAY';
        $103b:                          name := 'LVM_GETCOLUMNORDERARRAY';
        $103c:                          name := 'LVM_SETHOTITEM';
        $103d:                          name := 'LVM_GETHOTITEM';
        $103e:                          name := 'LVM_SETHOTCURSOR';
        $103f:                          name := 'LVM_GETHOTCURSOR';
        $1040:                          name := 'LVM_APPROXIMATEVIEWRECT';
        $1041:                          name := 'LVM_SETWORKAREAS';
        $1042:                          name := 'LVM_GETSELECTIONMARK';
        $1043:                          name := 'LVM_SETSELECTIONMARK';
        $1044:                          name := 'LVM_SETBKIMAGEA';
        $1045:                          name := 'LVM_GETBKIMAGEA';
        $1046:                          name := 'LVM_GETWORKAREAS';
        $1047:                          name := 'LVM_SETHOVERTIME';
        $1048:                          name := 'LVM_GETHOVERTIME';
        $1049:                          name := 'LVM_GETNUMBEROFWORKAREAS';
        $104a:                          name := 'LVM_SETTOOLTIPS';
        $104b:                          name := 'LVM_GETITEMW';
        $104c:                          name := 'LVM_SETITEMW';
        $104d:                          name := 'LVM_INSERTITEMW';
        $104e:                          name := 'LVM_GETTOOLTIPS';
        $1053:                          name := 'LVM_FINDITEMW';
        $1057:                          name := 'LVM_GETSTRINGWIDTHW';
        $105f:                          name := 'LVM_GETCOLUMNW';
        $1060:                          name := 'LVM_SETCOLUMNW';
        $1061:                          name := 'LVM_INSERTCOLUMNW';
        $1073:                          name := 'LVM_GETITEMTEXTW';
        $1074:                          name := 'LVM_SETITEMTEXTW';
        $1075:                          name := 'LVM_GETISEARCHSTRINGW';
        $1076:                          name := 'LVM_EDITLABELW';
        $108b:                          name := 'LVM_GETBKIMAGEW';
        $108c:                          name := 'LVM_SETSELECTEDCOLUMN';
        $108d:                          name := 'LVM_SETTILEWIDTH';
        $108e:                          name := 'LVM_SETVIEW';
        $108f:                          name := 'LVM_GETVIEW';
        $1091:                          name := 'LVM_INSERTGROUP';
        $1093:                          name := 'LVM_SETGROUPINFO';
        $1095:                          name := 'LVM_GETGROUPINFO';
        $1096:                          name := 'LVM_REMOVEGROUP';
        $1097:                          name := 'LVM_MOVEGROUP';
        $109a:                          name := 'LVM_MOVEITEMTOGROUP';
        $109b:                          name := 'LVM_SETGROUPMETRICS';
        $109c:                          name := 'LVM_GETGROUPMETRICS';
        $109d:                          name := 'LVM_ENABLEGROUPVIEW';
        $109e:                          name := 'LVM_SORTGROUPS';
        $109f:                          name := 'LVM_INSERTGROUPSORTED';
        $10a0:                          name := 'LVM_REMOVEALLGROUPS';
        $10a1:                          name := 'LVM_HASGROUP';
        $10a2:                          name := 'LVM_SETTILEVIEWINFO';
        $10a3:                          name := 'LVM_GETTILEVIEWINFO';
        $10a4:                          name := 'LVM_SETTILEINFO';
        $10a5:                          name := 'LVM_GETTILEINFO';
        $10a6:                          name := 'LVM_SETINSERTMARK';
        $10a7:                          name := 'LVM_GETINSERTMARK';
        $10a8:                          name := 'LVM_INSERTMARKHITTEST';
        $10a9:                          name := 'LVM_GETINSERTMARKRECT';
        $10aa:                          name := 'LVM_SETINSERTMARKCOLOR';
        $10ab:                          name := 'LVM_GETINSERTMARKCOLOR';
        $10ad:                          name := 'LVM_SETINFOTIP';
        $10ae:                          name := 'LVM_GETSELECTEDCOLUMN';
        $10af:                          name := 'LVM_ISGROUPVIEWENABLED';
        $10b0:                          name := 'LVM_GETOUTLINECOLOR';
        $10b1:                          name := 'LVM_SETOUTLINECOLOR';
        $10b3:                          name := 'LVM_CANCELEDITLABEL';
        $10b4:                          name := 'LVM_MAPINDEXTOID';
        $10b5:                          name := 'LVM_MAPIDTOINDEX';
        $10b6:                          name := 'LVM_ISITEMVISIBLE';
        $2000:                          name := 'OCM_BASE';
        $2005:                          name := 'LVM_SETUNICODEFORMAT';
        $2006:                          name := 'LVM_GETUNICODEFORMAT';
        $2019:                          name := 'OCM_CTLCOLOR';
        $202b:                          name := 'OCM_DRAWITEM';
        $202c:                          name := 'OCM_MEASUREITEM';
        $202d:                          name := 'OCM_DELETEITEM';
        $202e:                          name := 'OCM_VKEYTOITEM';
        $202f:                          name := 'OCM_CHARTOITEM';
        $2039:                          name := 'OCM_COMPAREITEM';
        $204e:                          name := 'OCM_NOTIFY';
        $2111:                          name := 'OCM_COMMAND';
        $2114:                          name := 'OCM_HSCROLL';
        $2115:                          name := 'OCM_VSCROLL';
        $2132:                          name := 'OCM_CTLCOLORMSGBOX';
        $2133:                          name := 'OCM_CTLCOLOREDIT';
        $2134:                          name := 'OCM_CTLCOLORLISTBOX';
        $2135:                          name := 'OCM_CTLCOLORBTN';
        $2136:                          name := 'OCM_CTLCOLORDLG';
        $2137:                          name := 'OCM_CTLCOLORSCROLLBAR';
        $2138:                          name := 'OCM_CTLCOLORSTATIC';
        $2210:                          name := 'OCM_PARENTNOTIFY';
        WM_APP:                         name := 'WM_APP';
        $cccd:                          name := 'WM_RASDIALEVENT';

        // RAD studio messages
        CM_ACTIVATE:                    name := 'CM_ACTIVATE';
        CM_DEACTIVATE:                  name := 'CM_DEACTIVATE';
        CM_GOTFOCUS:                    name := 'CM_GOTFOCUS';
        CM_LOSTFOCUS:                   name := 'CM_LOSTFOCUS';
        CM_CANCELMODE:                  name := 'CM_CANCELMODE';
        CM_DIALOGKEY:                   name := 'CM_DIALOGKEY';
        CM_DIALOGCHAR:                  name := 'CM_DIALOGCHAR';
        CM_FOCUSCHANGED:                name := 'CM_FOCUSCHANGED';
        CM_PARENTFONTCHANGED:           name := 'CM_PARENTFONTCHANGED';
        CM_PARENTCOLORCHANGED:          name := 'CM_PARENTCOLORCHANGED';
        CM_HITTEST:                     name := 'CM_HITTEST';
        CM_VISIBLECHANGED:              name := 'CM_VISIBLECHANGED';
        CM_ENABLEDCHANGED:              name := 'CM_ENABLEDCHANGED';
        CM_COLORCHANGED:                name := 'CM_COLORCHANGED';
        CM_FONTCHANGED:                 name := 'CM_FONTCHANGED';
        CM_CURSORCHANGED:               name := 'CM_CURSORCHANGED';
        CM_CTL3DCHANGED:                name := 'CM_CTL3DCHANGED';
        CM_PARENTCTL3DCHANGED:          name := 'CM_PARENTCTL3DCHANGED';
        CM_TEXTCHANGED:                 name := 'CM_TEXTCHANGED';
        CM_MOUSEENTER:                  name := 'CM_MOUSEENTER';
        CM_MOUSELEAVE:                  name := 'CM_MOUSELEAVE';
        CM_MENUCHANGED:                 name := 'CM_MENUCHANGED';
        CM_APPKEYDOWN:                  name := 'CM_APPKEYDOWN';
        CM_APPSYSCOMMAND:               name := 'CM_APPSYSCOMMAND';
        CM_BUTTONPRESSED:               name := 'CM_BUTTONPRESSED';
        CM_SHOWINGCHANGED:              name := 'CM_SHOWINGCHANGED';
        CM_ENTER:                       name := 'CM_ENTER';
        CM_EXIT:                        name := 'CM_EXIT';
        CM_DESIGNHITTEST:               name := 'CM_DESIGNHITTEST';
        CM_ICONCHANGED:                 name := 'CM_ICONCHANGED';
        CM_WANTSPECIALKEY:              name := 'CM_WANTSPECIALKEY';
        CM_INVOKEHELP:                  name := 'CM_INVOKEHELP';
        CM_WINDOWHOOK:                  name := 'CM_WINDOWHOOK';
        CM_RELEASE:                     name := 'CM_RELEASE';
        CM_SHOWHINTCHANGED:             name := 'CM_SHOWHINTCHANGED';
        CM_PARENTSHOWHINTCHANGED:       name := 'CM_PARENTSHOWHINTCHANGED';
        CM_SYSCOLORCHANGE:              name := 'CM_SYSCOLORCHANGE';
        CM_WININICHANGE:                name := 'CM_WININICHANGE';
        CM_FONTCHANGE:                  name := 'CM_FONTCHANGE';
        CM_TIMECHANGE:                  name := 'CM_TIMECHANGE';
        CM_TABSTOPCHANGED:              name := 'CM_TABSTOPCHANGED';
        CM_UIACTIVATE:                  name := 'CM_UIACTIVATE';
        CM_UIDEACTIVATE:                name := 'CM_UIDEACTIVATE';
        CM_DOCWINDOWACTIVATE:           name := 'CM_DOCWINDOWACTIVATE';
        CM_CONTROLLISTCHANGE:           name := 'CM_CONTROLLISTCHANGE';
        CM_GETDATALINK:                 name := 'CM_GETDATALINK';
        CM_CHILDKEY:                    name := 'CM_CHILDKEY';
        CM_DRAG:                        name := 'CM_DRAG';
        CM_HINTSHOW:                    name := 'CM_HINTSHOW';
        CM_DIALOGHANDLE:                name := 'CM_DIALOGHANDLE';
        CM_ISTOOLCONTROL:               name := 'CM_ISTOOLCONTROL';
        CM_RECREATEWND:                 name := 'CM_RECREATEWND';
        CM_INVALIDATE:                  name := 'CM_INVALIDATE';
        CM_SYSFONTCHANGED:              name := 'CM_SYSFONTCHANGED';
        CM_CONTROLCHANGE:               name := 'CM_CONTROLCHANGE';
        CM_CHANGED:                     name := 'CM_CHANGED';
        CM_DOCKCLIENT:                  name := 'CM_DOCKCLIENT';
        CM_UNDOCKCLIENT:                name := 'CM_UNDOCKCLIENT';
        CM_FLOAT:                       name := 'CM_FLOAT';
        CM_BORDERCHANGED:               name := 'CM_BORDERCHANGED';
        CM_BIDIMODECHANGED:             name := 'CM_BIDIMODECHANGED';
        CM_PARENTBIDIMODECHANGED:       name := 'CM_PARENTBIDIMODECHANGED';
        CM_ALLCHILDRENFLIPPED:          name := 'CM_ALLCHILDRENFLIPPED';
        CM_ACTIONUPDATE:                name := 'CM_ACTIONUPDATE';
        CM_ACTIONEXECUTE:               name := 'CM_ACTIONEXECUTE';
        CM_HINTSHOWPAUSE:               name := 'CM_HINTSHOWPAUSE';
        CM_DOCKNOTIFICATION:            name := 'CM_DOCKNOTIFICATION';
        CM_MOUSEWHEEL:                  name := 'CM_MOUSEWHEEL';
        CM_ISSHORTCUT:                  name := 'CM_ISSHORTCUT';
        CM_UPDATEACTIONS:               name := 'CM_UPDATEACTIONS';
        CM_INVALIDATEDOCKHOST:          name := 'CM_INVALIDATEDOCKHOST';
        CM_SETACTIVECONTROL:            name := 'CM_SETACTIVECONTROL';
        CM_POPUPHWNDDESTROY:            name := 'CM_POPUPHWNDDESTROY';
        CM_CREATEPOPUP:                 name := 'CM_CREATEPOPUP';
        CM_DESTROYHANDLE:               name := 'CM_DESTROYHANDLE';
        CM_MOUSEACTIVATE:               name := 'CM_MOUSEACTIVATE';
        CM_CONTROLLISTCHANGING:         name := 'CM_CONTROLLISTCHANGING';
        CM_BUFFEREDPRINTCLIENT:         name := 'CM_BUFFEREDPRINTCLIENT';
        CM_UNTHEMECONTROL:              name := 'CM_UNTHEMECONTROL';
        CM_DOUBLEBUFFEREDCHANGED:       name := 'CM_DOUBLEBUFFEREDCHANGED';
        CM_PARENTDOUBLEBUFFEREDCHANGED: name := 'CM_PARENTDOUBLEBUFFEREDCHANGED';
        CM_STYLECHANGED:                name := 'CM_STYLECHANGED';
        CM_GESTURE:                     name := 'CM_GESTURE';
        CM_CUSTOMGESTURESCHANGED:       name := 'CM_CUSTOMGESTURESCHANGED';
        CM_GESTUREMANAGERCHANGED:       name := 'CM_GESTUREMANAGERCHANGED';
        CM_STANDARDGESTURESCHANGED:     name := 'CM_STANDARDGESTURESCHANGED';
        CM_INPUTLANGCHANGE:             name := 'CM_INPUTLANGCHANGE';
        CM_TABLETOPTIONSCHANGED:        name := 'CM_TABLETOPTIONSCHANGED';
        CM_PARENTTABLETOPTIONSCHANGED:  name := 'CM_PARENTTABLETOPTIONSCHANGED';
        CM_CUSTOMSTYLECHANGED:          name := 'CM_CUSTOMSTYLECHANGED';

        // RAD Studio VCL control notification IDs
        CN_BASE:                        name := 'CN_BASE';
        CN_CHARTOITEM:                  name := 'CN_CHARTOITEM';
        CN_COMMAND:                     name := 'CN_COMMAND';
        CN_COMPAREITEM:                 name := 'CN_COMPAREITEM';
        CN_CTLCOLORBTN:                 name := 'CN_CTLCOLORBTN';
        CN_CTLCOLORDLG:                 name := 'CN_CTLCOLORDLG';
        CN_CTLCOLOREDIT:                name := 'CN_CTLCOLOREDIT';
        CN_CTLCOLORLISTBOX:             name := 'CN_CTLCOLORLISTBOX';
        CN_CTLCOLORMSGBOX:              name := 'CN_CTLCOLORMSGBOX';
        CN_CTLCOLORSCROLLBAR:           name := 'CN_CTLCOLORSCROLLBAR';
        CN_CTLCOLORSTATIC:              name := 'CN_CTLCOLORSTATIC';
        CN_DELETEITEM:                  name := 'CN_DELETEITEM';
        CN_DRAWITEM:                    name := 'CN_DRAWITEM';
        CN_HSCROLL:                     name := 'CN_HSCROLL';
        CN_MEASUREITEM:                 name := 'CN_MEASUREITEM';
        CN_PARENTNOTIFY:                name := 'CN_PARENTNOTIFY';
        CN_VKEYTOITEM:                  name := 'CN_VKEYTOITEM';
        CN_VSCROLL:                     name := 'CN_VSCROLL';
        CN_KEYDOWN:                     name := 'CN_KEYDOWN';
        CN_KEYUP:                       name := 'CN_KEYUP';
        CN_CHAR:                        name := 'CN_CHAR';
        CN_SYSKEYDOWN:                  name := 'CN_SYSKEYDOWN';
        CN_SYSCHAR:                     name := 'CN_SYSCHAR';
        CN_NOTIFY:                      name := 'CN_NOTIFY';
    else
        // unknown message
        name := 'Unknown [' + IntToHex(message.Msg, 8) + ']';
    end;

    // convert wparam, whenever possible
    case (message.Msg) of
        WM_LBUTTONDOWN:
        begin
            if ((message.WParam and MK_CONTROL) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_CONTROL'
                else
                    wParamName := wParamName + ' or MK_CONTROL';

            if ((message.WParam and MK_LBUTTON) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_LBUTTON'
                else
                    wParamName := wParamName + ' or MK_LBUTTON';

            if ((message.WParam and MK_MBUTTON) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_MBUTTON'
                else
                    wParamName := wParamName + ' or MK_MBUTTON';

            if ((message.WParam and MK_MBUTTON) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_RBUTTON'
                else
                    wParamName := wParamName + ' or MK_RBUTTON';

            if ((message.WParam and MK_SHIFT) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_SHIFT'
                else
                    wParamName := wParamName + ' or MK_SHIFT';

            if ((message.WParam and $20) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_XBUTTON1'
                else
                    wParamName := wParamName + ' or MK_XBUTTON1';

            if ((message.WParam and $40) = 0) then
                if (TWStringHelper.IsEmpty(wParamName)) then
                    wParamName := 'MK_XBUTTON2'
                else
                    wParamName := wParamName + ' or MK_XBUTTON2';
        end;
    else
        wParamName := IntToHex(message.WParam, 8);
    end;

    // convert lparam, whenever possible
    case (message.Msg) of
        WM_LBUTTONDOWN:
            lParamName := 'x <'   + IntToStr(Integer(ShortInt(LOWORD(message.LParam)))) +
                          '> y <' + IntToStr(Integer(ShortInt(HIWORD(message.LParam)))) + '>';
        WM_PRINT:
        begin
            case (message.LParam) of
                PRF_CHECKVISIBLE: lParamName := 'PRF_CHECKVISIBLE';
                PRF_CHILDREN:     lParamName := 'PRF_CHILDREN';
                PRF_CLIENT:       lParamName := 'PRF_CLIENT';
                PRF_ERASEBKGND:   lParamName := 'PRF_ERASEBKGND';
                PRF_NONCLIENT:    lParamName := 'PRF_NONCLIENT';
                PRF_OWNED:        lParamName := 'PRF_OWNED';
            else
                lParamName := IntToHex(message.WParam, 8);
            end;
        end;

        WM_PRINTCLIENT:
        begin
            case (message.LParam) of
                PRF_CHECKVISIBLE: lParamName := 'PRF_CHECKVISIBLE';
                PRF_CHILDREN:     lParamName := 'PRF_CHILDREN';
                PRF_CLIENT:       lParamName := 'PRF_CLIENT';
                PRF_ERASEBKGND:   lParamName := 'PRF_ERASEBKGND';
                PRF_NONCLIENT:    lParamName := 'PRF_NONCLIENT';
                PRF_OWNED:        lParamName := 'PRF_OWNED';
            else
                lParamName := IntToHex(message.WParam, 8);
            end;
        end;
    else
        lParamName := IntToHex(message.WParam, 8);
    end;

    // format and return Windows message as text
    if (Assigned(pOwner)) then
        Result := 'Component - '  + pOwner.Name +
                  ' - message - ' + name        +
                  ' - wParam ['   + wParamName  +
                  '] - lParam ['  + lParamName  +
                  ']'
    else
        Result := 'Message - '   + name       +
                  ' - wParam ['  + wParamName +
                  '] - lParam [' + lParamName +
                  ']';
end;
//---------------------------------------------------------------------------
class function TWLogHelper.GdiPlusStatusToStr(status: TStatus): UnicodeString;
begin
    case (status) of
        Ok:                        Exit('Ok');
        GenericError:              Exit('GenericError');
        InvalidParameter:          Exit('InvalidParameter');
        OutOfMemory:               Exit('OutOfMemory');
        ObjectBusy:                Exit('ObjectBusy');
        InsufficientBuffer:        Exit('InsufficientBuffer');
        NotImplemented:            Exit('NotImplemented');
        Win32Error:                Exit('Win32Error');
        WrongState:                Exit('WrongState');
        Aborted:                   Exit('Aborted');
        FileNotFound:              Exit('FileNotFound');
        ValueOverflow:             Exit('ValueOverflow');
        AccessDenied:              Exit('AccessDenied');
        UnknownImageFormat:        Exit('UnknownImageFormat');
        FontFamilyNotFound:        Exit('FontFamilyNotFound');
        FontStyleNotFound:         Exit('FontStyleNotFound');
        NotTrueTypeFont:           Exit('NotTrueTypeFont');
        UnsupportedGdiplusVersion: Exit('UnsupportedGdiplusVersion');
        GdiplusNotInitialized:     Exit('GdiplusNotInitialized');
        PropertyNotFound:          Exit('PropertyNotFound');
        PropertyNotSupported:      Exit('PropertyNotSupported');
    else
        // unknown message
        Exit('Unknown [' + IntToHex(Integer(status), 8) + ']');
    end;
end;
//---------------------------------------------------------------------------
class function TWLogHelper.GdiPlusMatrixToStr(pMatrix: TGpMatrix): UnicodeString;
var
    elements: TMatrixArray;
begin
    pMatrix.GetElements(elements);

    Result := 'Table[0][0] = ' + FloatToStr(elements[0]) + ' - Table[1][0] = ' + FloatToStr(elements[2]) + ' - Table[2][0] = ' + FloatToStr(elements[4]) + #13 + #10 +
              'Table[0][1] = ' + FloatToStr(elements[1]) + ' - Table[1][1] = ' + FloatToStr(elements[3]) + ' - Table[2][1] = ' + FloatToStr(elements[5]) + #13 + #10 +
              'Table[0][2] = 0.0 - Table[1][2] = 0.0 - Table[2][2] = 1.0';
end;
//---------------------------------------------------------------------------
class procedure TWLogHelper.LogToCompiler(const msg: UnicodeString);
begin
  {$IFDEF DEBUG}
  OutputDebugString(PWideChar(msg));
  {$ENDIF}
end;
//---------------------------------------------------------------------------
class procedure TWLogHelper.LogBlockToCompiler(const title: UnicodeString);
begin
    LogToCompiler(TWStringHelper.DelimitFillStr(title, 80, '-'));
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
// Global initialization procedure
//---------------------------------------------------------------------------
begin
    // configure the cache controller
    g_GDICacheController.m_DCs                     := True;
    g_GDICacheController.m_Bitmaps                 := False;
    g_GDICacheController.m_DIBBitmaps              := True;
    g_GDICacheController.m_UseDIBBitmapsToDrawText := True;
    g_GDICacheController.m_UseAlphaBlendToDrawText := False;
    g_GDICacheController.m_UseDIBBitmapsToDrawRect := True;
    g_GDICacheController.m_UseAlphaBlendToDrawRect := False;

    // get and cache the current Windows version
    TWOSWinHelper.m_WinVersion := TWOSWinHelper.GetWinVersion;
end;
//---------------------------------------------------------------------------

end.
