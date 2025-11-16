unit UTWMajorSettings;

interface

uses System.SysUtils,
     UTWVersion;

var
    {**
     Global library version
    }
    TWLibraryVersion: TWVersion = nil;

    {**
     Global format settings to use to convert standardized float values in like SVG or SMIL
    }
    g_InternationalFormatSettings: TFormatSettings;

implementation

//---------------------------------------------------------------------------
initialization
begin
    TWLibraryVersion := TWVersion.Create(1, 0, 0, 9);

    // create a global format settings to use to convert standardized float values in like SVG or SMIL
    FillChar(g_InternationalFormatSettings, SizeOf(g_InternationalFormatSettings), 0);
    g_InternationalFormatSettings.ThousandSeparator := ',';
    g_InternationalFormatSettings.DecimalSeparator  := '.';
end;
//---------------------------------------------------------------------------
finalization
begin
    FreeAndNil(TWLibraryVersion);
end;
//---------------------------------------------------------------------------

end.

