{**
 @abstract(@name provides a color class that is able to convert the color value between several
           platforms and librairies, like e.g. Windows, VCL, GDI, GDI+, ... @name provides also
           several useful functions to work with the color, like blender or grayscale functions)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWColor;
{$I SVGMagic.inc}

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     System.Math,
     System.Generics.Defaults,
     System.UITypes,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     {$ifdef USE_VCL}
         Vcl.Graphics,
         Vcl.GraphUtil,
         Winapi.GdipObj,
         Winapi.GdipApi,
     {$endif}
     {$ifdef MSWINDOWS}
         Winapi.Windows,
     {$endif}
     UTWTypes,
     UTWMajorSettings;

const
    // unfortunately no doc explain which are the range of the returned hsl values, that are between
    // 0 and 240, as mentioned on the only doc that speak correctly about this function, see:
    // https://source.winehq.org/WineAPI/ColorRGBToHLS.html
    C_TWColor_Max_HSL_Value:  Byte = 240;
    C_TWColor_Max_RGBA_Value: Byte = 255;

type
    {**
     Color pointer type
    }
    PWColor = ^TWColor;

    {**
     Universal color class, supporting VCL, GDI and GDI+ colors and conversion tools
     @br @bold(NOTE) The code has been optimized to gain maximum possible speed, therefore, do not
                     clean it unless you are really aware of what you are doing
    }
    TWColor = record
        public type
            {**
             Hexadecimal format enumeration
             @value(IE_HF_RGB Hexadecimal will be formatted as Red, Green and Blue value (#RRGGBB))
             @value(IE_HF_BGR Hexadecimal will be formatted as Blue, Green and Red value (#BBGGRR))
             @value(IE_HF_ARGB Hexadecimal will be formatted as Alpha, Red, Green and Blue value (#AARRGGBB))
             @value(IE_HF_RGBA Hexadecimal will be formatted as Red, Green, Blue and Alpha value (#RRGGBBAA))
             @value(IE_HF_ABGR Hexadecimal will be formatted as Alpha, Blue, Green and Red value (#AABBGGRR))
             @value(IE_HF_BGRA Hexadecimal will be formatted as Blue, Green, Red and Alpha value (#BBGGRRAA))
            }
            IEHexFormat =
            (
                IE_HF_RGB,
                IE_HF_BGR,
                IE_HF_ARGB,
                IE_HF_RGBA,
                IE_HF_ABGR,
                IE_HF_BGRA
            );

        private
            m_Red:   Byte;
            m_Green: Byte;
            m_Blue:  Byte;
            m_Alpha: Byte;

            {**
             Gets a substring of a string
             @param(str String from which the substring should be extracted)
             @param(index Start index in the source string from which the substring should be copied)
             @param(length Substring length to copy in the source string)
             @br @bold(NOTE) This function is a duplicate of the TWStringHelper.Substr() one,
                             unfortunately it's not possible to use the helper version here because
                             UTWColor unit is included in UTWHelpers. For that reason this function
                             was duplicated as a workaround
            }
            function Substr(const str: UnicodeString; index, length: Integer): UnicodeString; inline;

            {**
             Converts a string to upper case
             @param(str Source string to convert)
             @returns(Converted string to uppercase)
             @br @bold(NOTE) This function is a duplicate of the TWStringHelper.ToUpper() one,
                             unfortunately it's not possible to use the helper version here because
                             UTWColor unit is included in UTWHelpers. For that reason this function
                             was duplicated as a workaround
            }
            class function ToUpper(const str: UnicodeString): UnicodeString; inline; static;

            {**
             Converts a string to lower case
             @param(str Source string to convert)
             @returns(Converted string to lowercase)
             @br @bold(NOTE) This function is a duplicate of the TWStringHelper.ToLower() one,
                             unfortunately it's not possible to use the helper version here because
                             UTWColor unit is included in UTWHelpers. For that reason this function
                             was duplicated as a workaround
            }
            class function ToLower(const str: UnicodeString): UnicodeString; inline; static;

            {**
             Convert the hue value of a HSL color to a RGB value
             @param(hue Hue value to convert)
             @param(mid1 Calculated intermediate min color value)
             @param(mid2 Calculated intermediate max color value)
             @return(RGB color value)
            }
            {$ifndef MSWINDOWS}
                function HueToRGB(hue, mid1, mid2: Word): Word; inline;
            {$endif}

            {**
             Convert the hue value of a HSL color from HSL color space to RGB
             @param(hue Hue value to convert)
             @param(mid1 Calculated intermediate min color value)
             @param(mid2 Calculated intermediate max color value)
             @return(RGB color value)
            }
            {$ifndef MSWINDOWS}
                function ConvertHue(hue: Integer; mid1, mid2: Word): Word; inline;
            {$endif}

        public
            {**
             Constructor
             @param(red Color red component)
             @param(green Color green component)
             @param(blue Color blue component)
            }
            constructor Create(red, green, blue: Byte); overload;

            {**
             Constructor
             @param(red Color red component)
             @param(green Color green component)
             @param(blue Color blue component)
             @param(alpha Color alpha component)
             @br @bold(NOTE) Use an overload instead of a default parameter, otherwise the wrong
                             code will be emitted in the .hpp files
            }
            constructor Create(red, green, blue, alpha: Byte); overload;

            {**
             Constructor
             @param(color Standard VCL color)
            }
            {$ifdef USE_VCL}
                constructor Create(const color: TColor); overload;
            {$endif}

            {**
             Constructor
             @param(color Standard VCL color)
             @param(alpha Color alpha component)
             @br @bold(NOTE) Use an overload instead of a default parameter, otherwise the wrong
                             code will be emitted in the .hpp files
            }
            {$ifdef USE_VCL}
                constructor Create(const color: TColor; alpha: Byte); overload;
            {$endif}

            {**
             Constructor
             @param(color Standard GDI color)
            }
            {$ifdef MSWINDOWS}
                constructor Create(const color: TColorRef); overload;
            {$endif}

            {**
             Constructor
             @param(color Standard GDI color)
             @param(alpha Color alpha component)
             @br @bold(NOTE) Use an overload instead of a default parameter, otherwise the wrong
                             code will be emitted in the .hpp files
            }
            {$ifdef MSWINDOWS}
                constructor Create(const color: TColorRef; alpha: Byte); overload;
            {$endif}

            {**
             Constructor
             @param(color VCL color with alpha component)
             @param(gdiPlus If @true, color is a GDI+ color, otherwise a GDI color)
            }
            {$ifdef USE_VCL}
                constructor Create(const color: TAlphaColor; gdiPlus: Boolean); overload;
            {$endif}

            {**
             Copy constructor
             @param(pOther Other color to copy from)
            }
            constructor Create(const pOther: PWColor); overload;

            {**
             Equality operator (allows operations on colors like a = b)
             @param(a First color to compare)
             @param(b Second color to compare)
             @returns(@true if colors are equal, otherwise @false)
            }
            class operator Equal(a, b: TWColor): Boolean;

            {**
             Not equality operator (allows operations on colors like a <> b)
             @param(a First color to compare)
             @param(b Second color to compare)
             @returns(@true if colors are not equal, otherwise @false)
            }
            class operator NotEqual(a, b: TWColor): Boolean;

            {**
             Get the default color
             @returns(A new color instance initialized with the default color)
            }
            class function GetDefault: TWColor; inline; static;

            {**
             Clone color
             @returns(Cloned color)
            }
            function Clone: TWColor; inline;

            {**
             Clear color
            }
            procedure Clear; inline;

            {**
             Check if color is empty
             @returns(@true if color is empty, otherwise @false)
            }
            function IsEmpty: Boolean; inline;

            {**
             Check if color content is equal to another color
             @param(other Other color to compare with)
             @returns(@true if colors are equals, otherwise @false)
            }
            function IsEqual(const other: TWColor): Boolean; inline;

            {**
             Check if color content differs from another color
             @param(other Other color to compare with)
             @returns(@true if colors differ, otherwise @false)
            }
            function Differs(const other: TWColor): Boolean; inline;

            {**
             Assign color
             @param(other Other color to copy from)
            }
            procedure Assign(const other: TWColor); inline;

            {**
             Get the hash code based on the class content
             @param(initValue Initialization value)
             @returns(Hash code)
            }
            function GetHashCode(initValue: Integer): Integer; inline;

            {**
             Set color
             @param(red Color red component)
             @param(green Color green component)
             @param(blue Color blue component)
             @param(alpha Color alpha component)
            }
            procedure SetColor(red, green, blue: Byte; alpha: Byte = 255); overload; inline;

            {**
             Get VCL color
             @returns(Color)
            }
            {$ifdef USE_VCL}
                function GetColor: TColor; overload; inline;
            {$endif}

            {**
             Get 24 bit bitmap pixel color
             @param(color @bold([out]) Color to get)
            }
            {$ifdef USE_VCL}
                procedure GetColor(out color: TRGBTriple); overload; inline;
            {$endif}

            {**
             Get 32 bit bitmap pixel color
             @param(color @bold([out]) Color to get)
            }
            {$ifdef USE_VCL}
                procedure GetColor(out color: TRGBQuad); overload; inline;
            {$endif}

            {**
             Set VCL color
             @param(color Color)
             @param(alpha Color alpha component)
            }
            {$ifdef USE_VCL}
                procedure SetColor(const color: TColor; alpha: Byte = 255); overload; inline;
            {$endif}

            {**
             Set 24 bit bitmap pixel color
             @param(color Color)
             @param(alpha Color alpha component)
            }
            {$ifdef USE_VCL}
                procedure SetColor(const color: TRGBTriple; alpha: Byte = 255); overload; inline;
            {$endif}

            {**
             Set 32 bit bitmap pixel color
             @param(color Color)
            }
            {$ifdef USE_VCL}
                procedure SetColor(const color: TRGBQuad); overload; inline;
            {$endif}

            {**
             Get RGB color
             @returns(Color)
            }
            function GetRGBColor: TWUInt32; inline;

            {**
             Get standard Windows RGB color
             @returns(Windows color)
            }
            {$ifdef MSWINDOWS}
                function GetWinColor: TColorRef; inline;
            {$endif}

            {**
             Set color from standard Windows RGB color
             @param(color Windows color)
             @param(alpha Alpha value)
            }
            {$ifdef MSWINDOWS}
                procedure SetWinColor(color: TColorRef); overload; inline;
            {$endif}

            {**
             Set color from standard Windows RGB color
             @param(color Windows color)
             @param(alpha Alpha value)
            }
            {$ifdef USE_VCL}
                procedure SetWinColor(color: TColorRef; alpha: Byte); overload; inline;
            {$endif}

            {**
             Get GDI+ color
             @returns(GDI+ color)
            }
            {$ifdef USE_VCL}
                function GetGDIPlusColor: TGpColor; inline;
            {$endif}

            {**
             Set GDI+ color
             @param(color GDI+ color)
            }
            {$ifdef USE_VCL}
                procedure SetGDIPlusColor(const color: TGpColor); inline;
            {$endif}

            {**
             Set 16 bit (RGB 565) color
             @param(color Color to set)
            }
            procedure SetRGB565(const color: Word); inline;

            {**
             Get 16 bit (RGB 565) color
             @return color
            }
            function GetRGB565: Word; inline;

            {**
             Set 16 bit (RGB 555) color
             @param(color Color to set)
            }
            procedure SetRGB555(const color: Word); inline;

            {**
             Get 16 bit (RGB 555) color
             @return color
            }
            function GetRGB555: Word; inline;

            {**
             Get color red component
             @returns(Red component)
            }
            function GetRed: Byte; inline;

            {**
             Set color red component
             @param(red Color red component)
            }
            procedure SetRed(red: Byte); inline;

            {**
             Get color green component
             @returns(Green component)
            }
            function GetGreen: Byte; inline;

            {**
             Set color green component
             @param(green Color green component)
            }
            procedure SetGreen(green: Byte); inline;

            {**
             Get color blue component
             @returns(Blue component)
            }
            function GetBlue: Byte; inline;

            {**
             Set color blue component
            @param(blue Color blue component)
            }
            procedure SetBlue(blue: Byte); inline;

            {**
             Get color alpha value
             @returns(Alpha value (between 0 and 255))
            }
            function GetAlpha: Byte; inline;

            {**
             Set color alpha component
             @param(alpha Color alpha component)
            }
            procedure SetAlpha(alpha: Byte); inline;

            {**
             Get opacity
             @returns(Opacity (in percent, between 0 and 100))
            }
            function GetOpacity: TWUInt32; inline;

            {**
             Set opacity
             @param(opacity Opacity (in percent, between 0 and 100))
            }
            procedure SetOpacity(opacity: TWUInt32); inline;

            {**
             Get grayscale value
             @returns(Grayscale value)
            }
            function GetGrayscale: Byte; inline;

            {**
             Set color from HSL (hue, saturation, luminance) values
             @param(hue Hue level)
             @param(saturation Saturation level)
             @param(luminance Luminance level)
            }
            procedure FromHSL(hue, saturation, luminance: Word); inline;

            {**
             Get color as HSL (hue, saturation, luminance) values
             @param(hue @bold([out]) Hue level)
             @param(saturation @bold([out]) Saturation level)
             @param(luminance @bold([out]) Luminance level)
            }
            procedure ToHSL(out hue, saturation, luminance: Word);

            {**
             Blend colors
             @param(red Other color red component to blend with internal color)
             @param(green Other color green component to blend with internal color)
             @param(blue Other color blue component to blend with internal color)
             @param(alpha Other color alpha component to blend with internal color)
             @param(level Blending level between internal and other colors in percent (1.0f = 100%))
             @returns(Blended color)
             @br @bold(NOTE) Internal color will be returned if offset is set to 0.0f, other color
                             if set to 1.0f
            }
            function Blend(red, green, blue, alpha: Byte; level: Single): TWColor; overload; inline;

            {**
             Blend colors
             @param(other Other color to blend with internal color)
             @param(level Blending level between internal and other colors in percent (1.0f = 100%))
             @returns(Blended color)
             @br @bold(NOTE) Internal color will be returned if offset is set to 0.0f, other color
                             if set to 1.0f
            }
            function Blend(const other: TWColor; level: Double): TWColor; overload; inline;

            {**
             Blend color with other color, and apply result inside color directly
             @param(other Other color to blend with internal color)
             @param(level Blending level between internal and other colors in percent (1.0f = 100%))
            }
            procedure BlendAndApply(const other: TWColor; level: Double);

            {**
             Change color brightness
             @param(brightness Brightness, from -1.0f (-100%) to 1.0f (100%))
            }
            function ChangeBrightness(brightness: Double): TWColor;

            {**
             Get color as formatted hexadecimal string
             @param(upperCase If @true, returned value will be in upper case)
             @param(prefix Prefix to add)
             @param(format Color format, see IEHexFormat enumeration)
             @returns(Color as HTML string)
            }
            function ToHex(upperCase: Boolean; const prefix: UnicodeString ='#';
                    format: IEHexFormat = IE_HF_RGB): UnicodeString;

            {**
             Set color from hexadecimal string
             @param(hex Hexadecimal string)
             @param(format Color format, see IEHexFormat enumeration)
             @returns(@true on success, otherwise @false)
            }
            function FromHex(const hex: UnicodeString; format: IEHexFormat ): Boolean; overload;

            {**
             Set color from hexadecimal string
             @param(hex Hexadecimal string)
             @param(format Color format, see IEHexFormat enumeration)
             @param(opacity Opacity (in percent, from 0 to 100))
             @returns(@true on success, otherwise @false)
            }
            function FromHex(const hex: UnicodeString; opacity: TWUInt32; format: IEHexFormat): Boolean; overload;

            {**
             Check if a string contains a color function (e.g. rgb(255, 0, 255))
             @param(functionStr String containing the function to parse)
             @returns(@true if the string contains a valid color function, otherwise @false)
            }
            function ValidateFunction(functionStr: UnicodeString): Boolean;

            {**
             Get color as function (e.g. rgb(255, 0, 255))
             @param(hsl If @true, the function will be of type hue, saturation, luminance)
             @param(includeAlpha If @true, alpha channel will be included in function)
             @param(upperCase If @true, returned value will be in upper case)
             @returns(Color as function)
            }
            function ToFunction(hsl, includeAlpha: Boolean; upperCase: Boolean = False;
                    includeSpaces: Boolean = False): UnicodeString;

            {**
             Set color from a string containing a function (e.g. rgb(255, 0, 255))
             @param(functionStr String containing the function to parse)
             @returns(@true on success, otherwise @false)
            }
            function FromFunction(functionStr: UnicodeString): Boolean;

            {**
             Get ready-to-use GDI brush from color
             @returns(Brush)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            {$ifdef USE_VCL}
                function GetBrush: TBrush; inline;
            {$endif}

            {**
             Get ready-to-use GDI+ solid brush from color
             @returns(Brush)
             @br @bold(NOTE) Brush must be deleted when no longer needed
            }
            {$ifdef USE_VCL}
                function GetGDIPlusSolidBrush: TGpSolidBrush; inline;
            {$endif}

            {**
             Get ready-to-use GDI pen from color
             @returns(Pen)
             @br @bold(NOTE) Pen must be deleted when no longer needed
            }
            {$ifdef USE_VCL}
                function GetPen: TPen; inline;
            {$endif}

            {**
             Get ready-to-use GDI+ pen from color
             @returns(Pen)
             @br @bold(NOTE) Pen must be deleted when no longer needed
            }
            {$ifdef USE_VCL}
                function GetGDIPlusPen: TGpPen; inline;
            {$endif}

        // Properties
        public
            {**
             Gets or sets the color red component (from 0 to 255)
            }
            property R: Byte read GetRed write SetRed;

            {**
             Gets or sets the color green component (from 0 to 255)
            }
            property G: Byte read GetGreen write SetGreen;

            {**
             Gets or sets the color blue component (from 0 to 255)
            }
            property B: Byte read GetBlue write SetBlue;

            {**
             Gets or sets the color alpha component (from 0 to 255)
            }
            property A: Byte read GetAlpha write SetAlpha;

            {**
             Gets or sets the color opacity, in percent (from 0 to 100)
            }
            property Opacity: TWUInt32 read GetOpacity write SetOpacity;

            {**
             Gets or sets the color from or to a Windows color
            }
            {$ifdef MSWINDOWS}
                property WinColor: TColorRef read GetWinColor write SetWinColor;
            {$endif}

            {**
             Gets or sets the color from or to a GDI+ color
            }
            {$ifdef USE_VCL}
                property GDIPlusColor: TGpColor read GetGDIPlusColor write SetGDIPlusColor;
            {$endif}

            {**
             Gets the color grayscale
            }
            property Grayscale: Byte read GetGrayscale;

            {**
             Gets a solid GDI brush from the color
             @br @bold(NOTE) The brush should be deleted after use
            }
            {$ifdef USE_VCL}
                property Brush: TBrush read GetBrush;
            {$endif}

            {**
             Gets a solid GDI+ brush from the color
             @br @bold(NOTE) The brush should be deleted after use
            }
            {$ifdef USE_VCL}
                property GDIPlusSolidBrush: TGpSolidBrush read GetGDIPlusSolidBrush;
            {$endif}

            {**
             Gets a GDI pen from the color
             @br @bold(NOTE) The pen should be deleted after use
            }
            {$ifdef USE_VCL}
                property Pen: TPen read GetPen;
            {$endif}

            {**
             Gets a GDI" pen from the color
             @br @bold(NOTE) The pen should be deleted after use
            }
            {$ifdef USE_VCL}
                property GDIPlusPen: TGpPen read GetGDIPlusPen;
            {$endif}
    end;

implementation
//---------------------------------------------------------------------------
constructor TWColor.Create(red, green, blue: Byte);
begin
    m_Red   := red;
    m_Green := green;
    m_Blue  := blue;
    m_Alpha := $ff;
end;
//---------------------------------------------------------------------------
constructor TWColor.Create(red, green, blue, alpha: Byte);
begin
    m_Red   := red;
    m_Green := green;
    m_Blue  := blue;
    m_Alpha := alpha;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWColor.Create(const color: TColor);
    var
        rgbColor: Longint;
    begin
        // strip VCL palette information and keep RGB value
        rgbColor := Vcl.Graphics.ColorToRGB(color);

        // convert from TColor
        m_Red   :=  (rgbColor         and $ff);
        m_Green := ((rgbColor shr 8)  and $ff);
        m_Blue  := ((rgbColor shr 16) and $ff);
        m_Alpha :=                        $ff;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWColor.Create(const color: TColor; alpha: Byte);
    var
        rgbColor: Longint;
    begin
        // strip VCL palette information and keep RGB value
        rgbColor := Vcl.Graphics.ColorToRGB(color);

        // convert from TColor
        m_Red   :=  (rgbColor         and $ff);
        m_Green := ((rgbColor shr 8)  and $ff);
        m_Blue  := ((rgbColor shr 16) and $ff);
        m_Alpha :=   alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef MSWINDOWS}
    constructor TWColor.Create(const color: TColorRef);
    begin
        // convert from Windows color
        m_Red   :=  (color         and $ff);
        m_Green := ((color shr 8)  and $ff);
        m_Blue  := ((color shr 16) and $ff);
        m_Alpha :=                     $ff;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef MSWINDOWS}
    constructor TWColor.Create(const color: TColorRef; alpha: Byte);
    begin
        // convert from Windows color
        m_Red   :=  (color         and $ff);
        m_Green := ((color shr 8)  and $ff);
        m_Blue  := ((color shr 16) and $ff);
        m_Alpha :=   alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    constructor TWColor.Create(const color: TAlphaColor; gdiPlus: Boolean);
    begin
        // convert from GDI+ color
        if (gdiPlus) then
        begin
            SetGDIPlusColor(color);
            Exit;
        end;

        // convert from GDI color
        m_Red   :=  (color         and $ff);
        m_Green := ((color shr 8)  and $ff);
        m_Blue  := ((color shr 16) and $ff);
        m_Alpha := ((color shr 24) and $ff);
    end;
{$endif}
//---------------------------------------------------------------------------
constructor TWColor.Create(const pOther: PWColor);
begin
    Assign(pOther^);
end;
//---------------------------------------------------------------------------
function TWColor.Substr(const str: UnicodeString; index, length: Integer): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        // NOTE + 1 to compensate 1 based UnicodeString indexes
        Result := Copy(str, index + 1, length);
    {$else}
        Result := str.Substring(index, length);
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWColor.ToUpper(const str: UnicodeString): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        Result := WideUpperCase(str);
    {$else}
        Result := str.ToUpper;
    {$ifend}
end;
//---------------------------------------------------------------------------
class function TWColor.ToLower(const str: UnicodeString): UnicodeString;
begin
    {$if CompilerVersion <= 24}
        Result := WideLowerCase(str);
    {$else}
        Result := str.ToLower;
    {$ifend}
end;
//---------------------------------------------------------------------------
{$ifndef MSWINDOWS}
    function TWColor.HueToRGB(hue, mid1, mid2: Word): Word;
    begin
        Result := (ConvertHue(hue, mid1, mid2) * 255 + 120) div 240;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifndef MSWINDOWS}
    function TWColor.ConvertHue(hue: Integer; mid1, mid2: Word): Word;
    begin
        if (hue > 240) then
            Dec(hue, 240)
        else
        if (hue < 0) then
            Inc(hue, 240);

        if (hue > 160) then
            Exit(mid1)
        else
        if (hue > 120) then
            hue := 160 - hue
        else
        if (hue > 40) then
            Exit(mid2);

        Result := ((hue * (mid2 - mid1) + 20) div 40) + mid1;
    end;
{$endif}
//---------------------------------------------------------------------------
class operator TWColor.Equal(a, b: TWColor): Boolean;
begin
    Result := a.IsEqual(b);
end;
//---------------------------------------------------------------------------
class operator TWColor.NotEqual(a, b: TWColor): Boolean;
begin
    Result := a.Differs(b);
end;
//---------------------------------------------------------------------------
class function TWColor.GetDefault: TWColor;
begin
    Result := TWColor.Create(0, 0, 0, 255);
end;
//---------------------------------------------------------------------------
function TWColor.Clone: TWColor;
begin
    Result := TWColor.Create(m_Red, m_Green, m_Blue, m_Alpha);
end;
//---------------------------------------------------------------------------
procedure TWColor.Clear;
begin
    m_Red   := 0;
    m_Green := 0;
    m_Blue  := 0;
    m_Alpha := 0;
end;
//---------------------------------------------------------------------------
function TWColor.IsEmpty: Boolean;
begin
    Result := ((m_Red = 0) and (m_Green = 0) and (m_Blue = 0) and (m_Alpha = 0));
end;
//---------------------------------------------------------------------------
function TWColor.IsEqual(const other: TWColor): Boolean;
begin
    Result := ((m_Red = other.m_Red) and (m_Green = other.m_Green) and (m_Blue = other.m_Blue)
            and (m_Alpha = other.m_Alpha));
end;
//---------------------------------------------------------------------------
function TWColor.Differs(const other: TWColor): Boolean;
begin
    Result := ((m_Red <> other.m_Red) or (m_Green <> other.m_Green) or (m_Blue <> other.m_Blue)
            or (m_Alpha <> other.m_Alpha));
end;
//---------------------------------------------------------------------------
function TWColor.GetHashCode(initValue: Integer): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(m_Red,   SizeOf(Byte), initValue);
        Result := THashBobJenkins.GetHashValue(m_Green, SizeOf(Byte), Result);
        Result := THashBobJenkins.GetHashValue(m_Blue,  SizeOf(Byte), Result);
        Result := THashBobJenkins.GetHashValue(m_Alpha, SizeOf(Byte), Result);
    {$else}
        Result := BobJenkinsHash(m_Red,   SizeOf(Byte), initValue);
        Result := BobJenkinsHash(m_Green, SizeOf(Byte), Result);
        Result := BobJenkinsHash(m_Blue,  SizeOf(Byte), Result);
        Result := BobJenkinsHash(m_Alpha, SizeOf(Byte), Result);
    {$ifend}
end;
//---------------------------------------------------------------------------
procedure TWColor.Assign(const other: TWColor);
begin
    m_Red   := other.m_Red;
    m_Green := other.m_Green;
    m_Blue  := other.m_Blue;
    m_Alpha := other.m_Alpha;
end;
//---------------------------------------------------------------------------
procedure TWColor.SetColor(red, green, blue, alpha: Byte);
begin
    m_Red   := red;
    m_Green := green;
    m_Blue  := blue;
    m_Alpha := alpha;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetColor: TColor;
    begin
        // build and return color
        Result := TColor((m_Blue shl 16) + (m_Green shl 8) + m_Red);
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.GetColor(out color: TRGBTriple);
    begin
        color.rgbtRed   := m_Red;
        color.rgbtGreen := m_Green;
        color.rgbtBlue  := m_Blue;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.GetColor(out color: TRGBQuad);
    begin
        color.rgbRed      := m_Red;
        color.rgbGreen    := m_Green;
        color.rgbBlue     := m_Blue;
        color.rgbReserved := m_Alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.SetColor(const color: TColor; alpha: Byte);
    var
        rgbColor: Longint;
    begin
        // strip VCL palette information and keep RGB value
        rgbColor := Vcl.Graphics.ColorToRGB(color);

        // convert from TColor
        m_Red   :=  (rgbColor         and $ff);
        m_Green := ((rgbColor shr 8)  and $ff);
        m_Blue  := ((rgbColor shr 16) and $ff);
        m_Alpha :=   alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.SetColor(const color: TRGBTriple; alpha: Byte);
    begin
        m_Red   := color.rgbtRed;
        m_Green := color.rgbtGreen;
        m_Blue  := color.rgbtBlue;
        m_Alpha := alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.SetColor(const color: TRGBQuad);
    begin
        m_Red   := color.rgbRed;
        m_Green := color.rgbGreen;
        m_Blue  := color.rgbBlue;
        m_Alpha := color.rgbReserved;
    end;
{$endif}
//---------------------------------------------------------------------------
function TWColor.GetRGBColor: TWUInt32;
begin
    Result := ((m_Red shl 16) or (m_Green shl 8) or m_Blue);
end;
//---------------------------------------------------------------------------
{$ifdef MSWINDOWS}
    function TWColor.GetWinColor: TColorRef;
    begin
        Result := RGB(m_Red, m_Green, m_Blue);
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef MSWINDOWS}
    procedure TWColor.SetWinColor(color: TColorRef);
    begin
        // convert from Windows color
        m_Red   :=  (color         and $ff);
        m_Green := ((color shr 8)  and $ff);
        m_Blue  := ((color shr 16) and $ff);
        m_Alpha :=                     $ff;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.SetWinColor(color: TColorRef; alpha: Byte);
    begin
        // convert from Windows color
        m_Red   :=  (color         and $ff);
        m_Green := ((color shr 8)  and $ff);
        m_Blue  := ((color shr 16) and $ff);
        m_Alpha :=   alpha;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetGDIPlusColor: TGpColor;
    begin
        // convert to GDI+ color and return result
        Result := TGpColor((m_Alpha shl AlphaShift) + (m_Red shl RedShift) + (m_Green shl GreenShift)
                + (m_Blue shl BlueShift));
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    procedure TWColor.SetGDIPlusColor(const color: TGpColor);
    begin
        // convert from GDI+ color
        m_Red   := ((color shr RedShift)   and $ff);
        m_Green := ((color shr GreenShift) and $ff);
        m_Blue  := ((color shr BlueShift)  and $ff);
        m_Alpha := ((color shr AlphaShift) and $ff);
    end;
{$endif}
//---------------------------------------------------------------------------
procedure TWColor.SetRGB565(const color: Word);
begin
    // based on https://msdn.microsoft.com/en-us/library/windows/desktop/dd390989(v=vs.85).aspx
    m_Alpha := 255;
    m_Blue  := (color and $1F)   shl 3;
    m_Green := (color and $7E0)  shr 3;
    m_Red   := (color and $F800) shr 8;
end;
//---------------------------------------------------------------------------
function TWColor.GetRGB565: Word;
begin
    // based on https://msdn.microsoft.com/en-us/library/windows/desktop/dd390989(v=vs.85).aspx
    Result := (((Word(m_Red) shl 8) and $F800) or ((Word(m_Green) shl 3) and $7E0) or (Word(m_Blue) shr 3));
end;
//---------------------------------------------------------------------------
procedure TWColor.SetRGB555(const color: Word);
begin
    // based on https://msdn.microsoft.com/en-us/library/windows/desktop/dd390989(v=vs.85).aspx
    m_Alpha := 255;
    m_Blue  := (color and $1F)   shl 3;
    m_Green := (color and $3E0)  shr 2;
    m_Red   := (color and $7C00) shr 7;
end;
//---------------------------------------------------------------------------
function TWColor.GetRGB555: Word;
begin
    // based on https://msdn.microsoft.com/en-us/library/windows/desktop/dd390989(v=vs.85).aspx
    Result := (((Word(m_Red) shl 7) and $7C00) or ((Word(m_Green) shl 2) and $3E0) or (Word(m_Blue) shr 3));
end;
//---------------------------------------------------------------------------
function TWColor.GetRed: Byte;
begin
    Result := m_Red;
end;
//---------------------------------------------------------------------------
procedure TWColor.SetRed(red: Byte);
begin
    m_Red := red;
end;
//---------------------------------------------------------------------------
function TWColor.GetGreen: Byte;
begin
    Result := m_Green;
end;
//---------------------------------------------------------------------------
procedure TWColor.SetGreen(green: Byte);
begin
    m_Green := green;
end;
//---------------------------------------------------------------------------
function TWColor.GetBlue: Byte;
begin
    Result := m_Blue;
end;
//---------------------------------------------------------------------------
procedure TWColor.SetBlue(blue: Byte);
begin
    m_Blue := blue;
end;
//---------------------------------------------------------------------------
function TWColor.GetAlpha: Byte;
begin
    Result := m_Alpha;
end;
//---------------------------------------------------------------------------
procedure TWColor.SetAlpha(alpha: Byte);
begin
    m_Alpha := alpha;
end;
//---------------------------------------------------------------------------
function TWColor.GetOpacity: TWUInt32;
begin
    // convert alpha to opacity and return value
    Result := ((m_Alpha * 100) div 255);
end;
//---------------------------------------------------------------------------
procedure TWColor.SetOpacity(opacity: TWUInt32);
begin
    // is opacity equal to 0 or out of bounds?
    if (opacity = 0) then
    begin
        m_Alpha := 0;
        Exit;
    end
    else
    if (opacity > 100) then
    begin
        m_Alpha := 255;
        Exit;
    end;

    // convert opacity to alpha
    m_Alpha := ((opacity * 255) div 100);
end;
//---------------------------------------------------------------------------
function TWColor.GetGrayscale: Byte;
var
    r, g, b: Single;
begin
    r := m_Red;
    g := m_Green;
    b := m_Blue;

    // convert color to grayscale and return value
    Result := Byte(Min(Round((r * 0.3) + (g * 0.59) + (b * 0.11)), 255));
end;
//---------------------------------------------------------------------------
procedure TWColor.FromHSL(hue, saturation, luminance: Word);
{$ifndef MSWINDOWS}
    var
        mid1, mid2, red, green, blue, grayscale: Word;
{$endif}
begin
    {$ifdef MSWINDOWS}
        SetWinColor(ColorHLSToRGB(hue, luminance, saturation));
    {$else}
        if (saturation <> 0) then
        begin
            if (luminance > 120) then
                mid2 := saturation + luminance - (saturation * luminance + 120) div 240
            else
                mid2 := ((saturation + 240) * luminance + 120) div 240;

            mid1 := luminance * 2 - mid2;

            red   := HueToRGB(hue + 80, mid1, mid2);
            green := HueToRGB(hue,      mid1, mid2);
            blue  := HueToRGB(hue - 80, mid1, mid2);

            SetColor(red, green, blue);
            Exit;
        end;

        grayscale := luminance * 255 div 240;
        SetColor(grayscale, grayscale, grayscale);
    {$endif}
end;
//---------------------------------------------------------------------------
procedure TWColor.ToHSL(out hue, saturation, luminance: Word);
{$ifndef MSWINDOWS}
    var
        h, s, l, minCC, maxCC, delta, rNorm, gNorm, bNorm: Integer;
{$endif}
begin
    {$ifdef MSWINDOWS}
        ColorRGBToHLS(GetWinColor, hue, luminance, saturation);
    {$else}
        // calculate the min and max color components
        minCC := Min(Integer(m_Red), Min(Integer(m_Green), Integer(m_Blue)));
        maxCC := Max(Integer(m_Red), Max(Integer(m_Green), Integer(m_Blue)));

        // calculate the luminance
        l := ((maxCC + minCC) * 240 + 255) div 510;

        if (minCC = maxCC) then
        begin
            // achromatic case, hue is now unrepresentable, but this is what native returns
            s := 0;
            h := 160;
        end
        else
        begin
            // chromatic case
            delta := maxCC - minCC;

            // to avoid division by 0 later
            if (delta = 0) then
                delta := 1;

            // calculate the saturation
            if (l <= 120) then
                s := ((maxCC + minCC) div 2 + delta * 240) div (maxCC + minCC)
            else
                s := ((510 - maxCC - minCC) div 2 + delta * 240) div (510 - maxCC - minCC);

            // calculate the hue
            rNorm := (delta div 2 + maxCC * 40 - Integer(m_Red)   * 40) div delta;
            gNorm := (delta div 2 + maxCC * 40 - Integer(m_Green) * 40) div delta;
            bNorm := (delta div 2 + maxCC * 40 - Integer(m_Blue)  * 40) div delta;

            if (Integer(m_Red) = maxCC) then
                h := bNorm - gNorm
            else
            if (Integer(m_Green) = maxCC) then
                h := 80 + rNorm - bNorm
            else
                h := 160 + gNorm - rNorm;

            if (h < 0) then
                Inc(h, 240)
            else
            if (h > 240) then
                Dec(h, 240);
        end;

        hue        := h;
        saturation := s;
        luminance  := l;
    {$endif}
end;
//------------------------------------------------------------------------------
function TWColor.Blend(red, green, blue, alpha: Byte; level: Single): TWColor;
var
    redDelta, greenDelta, blueDelta, alphaDelta: Double;
begin
    // do return internal color?
    if (level <= 0.0) then
        Exit(Self);

    // do return other color?
    if (level >= 1.0) then
        Exit(TWColor.Create(red, green, blue, alpha));

    redDelta   := (red   - m_Red);
    greenDelta := (green - m_Green);
    blueDelta  := (blue  - m_Blue);
    alphaDelta := (alpha - m_Alpha);

    // calculate and return blended color
    Result := TWColor.Create(m_Red   + Round(level * redDelta),
                             m_Green + Round(level * greenDelta),
                             m_Blue  + Round(level * blueDelta),
                             m_Alpha + Round(level * alphaDelta));
end;
//------------------------------------------------------------------------------
function TWColor.Blend(const other: TWColor; level: Double): TWColor;
var
    redDelta, greenDelta, blueDelta, alphaDelta: Double;
begin
    // do return internal color?
    if (level <= 0.0) then
        Exit(Self);

    // do return other color?
    if (level >= 1.0) then
        Exit(other);

    redDelta   := (other.m_Red   - m_Red);
    greenDelta := (other.m_Green - m_Green);
    blueDelta  := (other.m_Blue  - m_Blue);
    alphaDelta := (other.m_Alpha - m_Alpha);

    // calculate and return blended color
    Result := TWColor.Create(m_Red   + Round(level * redDelta),
                             m_Green + Round(level * greenDelta),
                             m_Blue  + Round(level * blueDelta),
                             m_Alpha + Round(level * alphaDelta));
end;
//------------------------------------------------------------------------------
procedure TWColor.BlendAndApply(const other: TWColor; level: Double);
var
    redDelta, greenDelta, blueDelta, alphaDelta: Double;
begin
    // nothing to do?
    if (level <= 0.0) then
        Exit;

    // do apply other color?
    if (level >= 1.0) then
    begin
        m_Red   := other.m_Red;
        m_Green := other.m_Green;
        m_Blue  := other.m_Blue;
        m_Alpha := other.m_Alpha;
        Exit;
    end;

    redDelta   := (other.m_Red   - m_Red);
    greenDelta := (other.m_Green - m_Green);
    blueDelta  := (other.m_Blue  - m_Blue);
    alphaDelta := (other.m_Alpha - m_Alpha);

    // calculate blended color components
    Inc(m_Red,   Round(level * redDelta));
    Inc(m_Green, Round(level * greenDelta));
    Inc(m_Blue,  Round(level * blueDelta));
    Inc(m_Alpha, Round(level * alphaDelta));
end;
//---------------------------------------------------------------------------
function TWColor.ChangeBrightness(brightness: Double): TWColor;
begin
    // no changes?
    if (brightness = 0.0) then
        Exit(Self);

    // max brightness?
    if (brightness >= 1.0) then
        // return white color
        Exit(TWColor.Create(255, 255, 255));

    // min brightness?
    if (brightness <= -1.0) then
        // return black color
        Exit(TWColor.Create(0, 0, 0));

    // do increase or decrease brightness?
    if (brightness < 0.0) then
    begin
        // decrease brightness
        Result := TWColor.Create(Max(m_Red   + Round(m_Red   * brightness), 0),
                                 Max(m_Green + Round(m_Green * brightness), 0),
                                 Max(m_Blue  + Round(m_Blue  * brightness), 0),
                                 m_Alpha);
    end
    else
    begin
        // increase brightness
        Result := TWColor.Create(Min(m_Red   + Round(($ff - m_Red)   * brightness), $ff),
                                 Min(m_Green + Round(($ff - m_Green) * brightness), $ff),
                                 Min(m_Blue  + Round(($ff - m_Blue)  * brightness), $ff),
                                 m_Alpha);
    end;
end;
//---------------------------------------------------------------------------
function TWColor.ToHex(upperCase: Boolean; const prefix: UnicodeString; format: IEHexFormat): UnicodeString;
begin
    // do add prefix?
    if (Length(prefix) > 0) then
        Result := prefix;

    // search for format
    case (format) of
        IE_HF_RGB:
        begin
            // format string
            Result := Result + IntToHex(m_Red,   2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Blue,  2);
        end;

        IE_HF_BGR:
        begin
            // format string
            Result := Result + IntToHex(m_Blue,  2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Red,   2);
        end;

        IE_HF_ARGB:
        begin
            // format string
            Result := Result + IntToHex(m_Alpha, 2);
            Result := Result + IntToHex(m_Red,   2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Blue,  2);
        end;

        IE_HF_RGBA:
        begin
            // format string
            Result := Result + IntToHex(m_Red,   2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Blue,  2);
            Result := Result + IntToHex(m_Alpha, 2);
        end;

        IE_HF_ABGR:
        begin
            // format string
            Result := Result + IntToHex(m_Alpha, 2);
            Result := Result + IntToHex(m_Blue,  2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Red,   2);
        end;

        IE_HF_BGRA:
        begin
            // format string
            Result := Result + IntToHex(m_Blue,  2);
            Result := Result + IntToHex(m_Green, 2);
            Result := Result + IntToHex(m_Red,   2);
            Result := Result + IntToHex(m_Alpha, 2);
        end;
    end;

    if (upperCase) then
        Result := ToUpper(Result)
    else
        Result := ToLower(Result);
end;
//---------------------------------------------------------------------------
function TWColor.FromHex(const hex: UnicodeString; format: IEHexFormat): Boolean;
var
    value, hexValue: UnicodeString;
    c:               WideChar;
    count:           NativeUInt;
begin
    // is value empty?
    if (Length(hex) = 0) then
        Exit(False);

    // convert value to lower case
    value := ToLower(hex);

    // get char count
    count := Length(value);

    // string begins with a 0x prefix?
    if ((count >= 2) and (value[1] = '0') and (value[2] = 'x')) then
    begin
        // remove prefix
        Dec(count, 2);
        value := Substr(value, 2, count);
    end;

    // iterate through chars
    for c in value do
        // is hexadecimal value?
        if (((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or ((c >= 'A') and (c <= 'F'))) then
            // add value to final hex value to convert
            hexValue := hexValue + c;

    // search for format
    case (format) of
        IE_HF_RGB:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 6) then
                Exit(False);

            // convert RGB values
            m_Red   := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Blue  := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Alpha := 255;
            Result  := True;
        end;

        IE_HF_BGR:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 6) then
                Exit(False);

            // convert RGB values
            m_Blue  := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Red   := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Alpha := 255;
            Result  := True;
        end;

        IE_HF_ARGB:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 8) then
                Exit(False);

            // convert ARGB values
            m_Alpha := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Red   := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Blue  := StrToInt('$' + Substr(hexValue, 6, 2));
            Result  := True;
        end;

        IE_HF_RGBA:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 8) then
                Exit(False);

            // convert ARGB values
            m_Red   := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Blue  := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Alpha := StrToInt('$' + Substr(hexValue, 6, 2));
            Result  := true;
        end;

        IE_HF_ABGR:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 8) then
                Exit(False);

            // convert ARGB values
            m_Alpha := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Blue  := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Red   := StrToInt('$' + Substr(hexValue, 6, 2));
            Result  := True;
        end;

        IE_HF_BGRA:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 8) then
                Exit(False);

            // convert ARGB values
            m_Blue  := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Red   := StrToInt('$' + Substr(hexValue, 4, 2));
            m_Alpha := StrToInt('$' + Substr(hexValue, 6, 2));
            Result  := True;
        end;
    else
        Result := False;
    end;
end;
//---------------------------------------------------------------------------
function TWColor.FromHex(const hex: UnicodeString; opacity: TWUInt32; format: IEHexFormat): Boolean;
var
    value, hexValue: UnicodeString;
    c:               WideChar;
    count:           NativeUInt;
begin
    // is value empty?
    if (Length(hex) = 0) then
        Exit(False);

    // convert value to lower case
    value := ToLower(hex);

    // get char count
    count := Length(value);

    // string begins with a 0x prefix?
    if ((count >= 2) and (value[1] = '0') and (value[2] = 'x')) then
    begin
        // remove prefix
        Dec(count, 2);
        value := Substr(value, 2, count);
    end;

    // iterate through chars
    for c in value do
        // is hexadecimal value?
        if (((c >= '0') and (c <= '9')) or ((c >= 'a') and (c <= 'f')) or ((c >= 'A') and (c <= 'F'))) then
            // add value to final hex value to convert
            hexValue := hexValue + c;

    // search for format
    case (format) of
        IE_HF_RGB:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 6) then
                Exit(False);

            // convert RGB values
            m_Red   := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Blue  := StrToInt('$' + Substr(hexValue, 4, 2));
            SetOpacity(opacity);

            Result := True;
        end;

        IE_HF_BGR:
        begin
            // incorrect hexadecimal value?
            if (Length(hexValue) <> 6) then
                Exit(False);

            // convert BGR values
            m_Blue  := StrToInt('$' + Substr(hexValue, 0, 2));
            m_Green := StrToInt('$' + Substr(hexValue, 2, 2));
            m_Red   := StrToInt('$' + Substr(hexValue, 4, 2));
            SetOpacity(opacity);

            Result := True;
        end;
    else
        Result := False;
    end;
end;
//---------------------------------------------------------------------------
function TWColor.ValidateFunction(functionStr: UnicodeString): Boolean;
var
    rgbFunc, rgbaFunc, hslFunc, hslaFunc, percent: Boolean;
    srcStr, valueList, value:                      UnicodeString;
    c:                                             WideChar;
    valueCount:                                    NativeUInt;
begin
    srcStr := ToLower(functionStr);

    // detect the function type
    rgbFunc  := (Pos('rgb(',  srcStr) = 1);
    rgbaFunc := (Pos('rgba(', srcStr) = 1);
    hslFunc  := (Pos('hsl(',  srcStr) = 1);
    hslaFunc := (Pos('hsla(', srcStr) = 1);

    // check if string begins with a valid function name
    if (not rgbFunc and not rgbaFunc and not hslFunc and not hslaFunc) then
        Exit(False);

    // check if string ends with a closing parenthesis
    if (srcStr[Length(srcStr)] <> ')') then
        Exit(False);

    // extract the function value list (i.e. remove the function name and the start/end parenthesis)
    if (rgbFunc) then
        valueList := Substr(srcStr, 4, Length(srcStr) - 5)
    else
        valueList := Substr(srcStr, 5, Length(srcStr) - 6);

    valueCount := 0;
    percent    := False;

    // iterate through value chars
    for c in valueList do
    begin
        // found a number?
        if ((c >= '0') and (c <= '9') or (c = '.')) then
        begin
            value := value + c;
            continue;
        end;

        // found something else?
        case (c) of
            ' ': continue;

            '%':
            begin
                // a value may be represented in percent, but only one percent is allowed by value
                if (percent) then
                    Exit(False);

                percent := True;
            end;

            ',':
            begin
                // found one value, increment value count and reset the variables to read the next
                Inc(valueCount);
                percent := False;
                value   := '';
            end;
        else
            Exit(False);
        end;
    end;

    // check if last value contains numbers
    if (Length(value) > 0) then
        Inc(valueCount);

    // is a rgb or hsl function and contain the correct number of values?
    if ((rgbFunc or hslFunc) and (valueCount = 3)) then
        Exit(True);

    // is a rgba or hsla function and contain the correct number of values?
    if ((rgbaFunc or hslaFunc) and (valueCount = 4)) then
        Exit(True);

    Result := False;
end;
//---------------------------------------------------------------------------
function TWColor.ToFunction(hsl, includeAlpha, upperCase, includeSpaces: Boolean): UnicodeString;
var
    h, s, l:          Word;
    hVal, sVal, lVal: Integer;
begin
    if (hsl) then
    begin
        ToHSL(h, s, l);

        // calculate the final HSL values. In CSS, hue is between 0 and 360 while saturation and
        // luminance are between 0 and 100
        hVal := (h * 360) div C_TWColor_Max_HSL_Value;
        sVal := (s * 100) div C_TWColor_Max_HSL_Value;
        lVal := (l * 100) div C_TWColor_Max_HSL_Value;

        // do generate a rgba() function?
        if (includeAlpha) then
        begin
            // do generate function in upper case?
            if (upperCase) then
                Result := 'HSLA('
            else
                Result := 'hsla(';

            // do include blank spaces?
            if (includeSpaces) then
                Result := Result + IntToStr(hVal) + ', ' + IntToStr(sVal) + '%, ' + IntToStr(lVal)
                        + '%, ' + FloatToStr(GetOpacity / 100.0) + ')'
            else
                Result := Result + IntToStr(hVal) + ',' + IntToStr(sVal) + '%,' + IntToStr(lVal)
                        + '%,' + FloatToStr(GetOpacity / 100.0) + ')';

            Exit;
        end;

        // do generate function in upper case?
        if (upperCase) then
            Result := 'HSL('
        else
            Result := 'hsl(';

        // do include blank spaces?
        if (includeSpaces) then
            Result := Result + IntToStr(hVal) + ', ' + IntToStr(sVal) + '%, ' + IntToStr(lVal) + '%)'
        else
            Result := Result + IntToStr(hVal) + ',' + IntToStr(sVal) + '%,' + IntToStr(lVal) + '%)';

        Exit;
    end;

    // do generate a rgba() function?
    if (includeAlpha) then
    begin
        // do generate function in upper case?
        if (upperCase) then
            Result := 'RGBA('
        else
            Result := 'rgba(';

        // do include blank spaces?
        if (includeSpaces) then
            Result := Result + IntToStr(m_Red) + ', ' + IntToStr(m_Green) + ', ' + IntToStr(m_Blue)
                    + ', ' + FloatToStr(GetOpacity / 100.0) + ')'
        else
            Result := Result + IntToStr(m_Red) + ',' + IntToStr(m_Green) + ',' + IntToStr(m_Blue)
                    + ',' + FloatToStr(GetOpacity / 100.0) + ')';

        Exit;
    end;

    // do generate function in upper case?
    if (upperCase) then
        Result := 'RGB('
    else
        Result := 'rgb(';

    // do include blank spaces?
    if (includeSpaces) then
        Result := Result + IntToStr(m_Red) + ', ' + IntToStr(m_Green) + ', ' + IntToStr(m_Blue) + ')'
    else
        Result := Result + IntToStr(m_Red) + ',' + IntToStr(m_Green) + ',' + IntToStr(m_Blue) + ')';
end;
//---------------------------------------------------------------------------
function TWColor.FromFunction(functionStr: UnicodeString): Boolean;
var
    rgbFunc, rgbaFunc, hslFunc, hslaFunc, percent: Boolean;
    srcStr, valueList, value:                      UnicodeString;
    c:                                             WideChar;
    valueCount:                                    NativeUInt;
    hue, saturation, luminance:                    Cardinal;

    {**
     Convert a numeric value in a text in a cardinal value
     @param(value Text containing the value to convert)
     @param(percent If @true, the value is expressed in percent)
     @param(maxVal Maximum possible value for the conversion (e.g. equals to 255 for a RGB component))
     @return(The converted value)
    }
    function ToValue(const value: UnicodeString; percent: Boolean; maxVal: Cardinal): Cardinal;
    var
        maxValF: Single;
    begin
        if (percent) then
        begin
            maxValF := maxVal;
            Result  := Max(Round((StrToFloat(value, g_InternationalFormatSettings) * maxValF) / 100.0), maxVal)
        end
        else
            Result := StrToInt(value);
    end;
begin
    srcStr := ToLower(functionStr);

    // detect the function type
    rgbFunc  := (Pos('rgb(',  srcStr) = 1);
    rgbaFunc := (Pos('rgba(', srcStr) = 1);
    hslFunc  := (Pos('hsl(',  srcStr) = 1);
    hslaFunc := (Pos('hsla(', srcStr) = 1);

    // check if string begins with a valid function name
    if (not rgbFunc and not rgbaFunc and not hslFunc and not hslaFunc) then
        Exit(False);

    // check if string ends with a closing parenthesis
    if (functionStr[Length(functionStr)] <> ')') then
        Exit(False);

    // extract the function value list (i.e. remove the function name and the start/end parenthesis)
    if (rgbFunc) then
        valueList := Substr(functionStr, 4, Length(functionStr) - 5)
    else
        valueList := Substr(functionStr, 5, Length(functionStr) - 6);

    valueCount := 0;
    hue        := 0;
    saturation := 0;
    luminance  := 0;
    percent    := False;

    // iterate through value chars
    for c in valueList do
    begin
        // found a number?
        if ((c >= '0') and (c <= '9') or (c = '.')) then
        begin
            value := value + c;
            continue;
        end;

        // found something else?
        case (c) of
            ' ': continue;

            '%':
            begin
                // a value may be represented in percent, but only one percent is allowed by value
                if (percent) then
                    Exit(False);

                percent := True;
            end;

            ',':
            begin
                case (valueCount) of
                    0:
                        if (hslFunc or hslaFunc) then
                            hue := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                        else
                            m_Red := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

                    1:
                        if (hslFunc or hslaFunc) then
                            saturation := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                        else
                            m_Green := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

                    2:
                        if (hslFunc or hslaFunc) then
                            luminance := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                        else
                            m_Blue := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

                    3: SetOpacity(Round(StrToFloat(value, g_InternationalFormatSettings) * 100.0));
                end;

                // found one value, increment value count and reset the variables to read the next
                Inc(valueCount);
                percent := False;
                value   := '';
            end;
        else
            Exit(False);
        end;
    end;

    // parse the final color
    if (Length(value) > 0) then
        case (valueCount) of
            0:
                if (hslFunc or hslaFunc) then
                    hue := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                else
                    m_Red := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

            1:
                if (hslFunc or hslaFunc) then
                    saturation := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                else
                    m_Green := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

            2:
                if (hslFunc or hslaFunc) then
                    luminance := ToValue(value, percent, C_TWColor_Max_HSL_Value)
                else
                    m_Blue := ToValue(value, percent, C_TWColor_Max_RGBA_Value);

            3: SetOpacity(Round(StrToFloat(value, g_InternationalFormatSettings) * 100.0));
        end;

    // is a hsl function?
    if (hslFunc or hslaFunc) then
        FromHSL(hue, saturation, luminance);

    Result := True;
end;
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetBrush: TBrush;
    begin
        Result       := TBrush.Create;
        Result.Color := GetColor;
        Result.Style := bsSolid;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetGDIPlusSolidBrush: TGpSolidBrush;
    begin
        Result := TGpSolidBrush.Create(GetGDIPlusColor);
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetPen: TPen;
    begin
        Result       := TPen.Create;
        Result.Color := GetColor;
        Result.Style := psSolid;
    end;
{$endif}
//---------------------------------------------------------------------------
{$ifdef USE_VCL}
    function TWColor.GetGDIPlusPen: TGpPen;
    begin
        Result := TGpPen.Create(GetGDIPlusColor);
    end;
{$endif}
//---------------------------------------------------------------------------

end.
