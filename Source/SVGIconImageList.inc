//To silent Hint messege of engine used
{.$DEFINE SvgDisableEngineHint}

//To use old OpenPictureDialog with image preview
{.$DEFINE OldPictureDialog}

//Prefer Engine Direct2D by Kiriakos Vlahos
//if supported by Windows Platform (from Windows Creators update)
{.$DEFINE PreferNativeSvgSupport}
{.$DEFINE GPUSupport}
{$IFDEF PreferNativeSvgSupport}
  // Throw an exception if the SVG contains text elements or class attributes
  // which are not supported by Windows SVG. Since it costs some performance,
  // you should only turn it on during debugging or if it's absolutely necessary.
  {.$DEFINE CheckForUnsupportedSvg}
{$ENDIF}

//if PreferNativeSvgSupport not active or not available:
//use Delphi Engine from Image32 library by Angus Johnson (included into Image32 folder)
{$DEFINE Image32_SVGEngine}
//or use Engine Skia with skia4delphi wrapper by Vinícius Felipe Botelho Barbosa (included into skia4delphi folder)
{.$DEFINE Skia_SVGEngine}

// When IgnoreAntiAliasedColor is set it svgs paints well but not perfect on
// all backgrounds.  Works best with Delphi_SVGEngine
{.$DEFINE IgnoreAntiAliasedColor}
{$IF defined(Delphi_SVGEngine) and not defined(PreferNativeSvgSupport)}
  {$DEFINE IgnoreAntiAliasedColor}
{$IFEND}

// Delphi XE3
{$IFDEF VER240}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
{$ENDIF}

// Delphi XE6
{$IFDEF VER270}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
{$ENDIF}

// Delphi 10 Seattle
{$IFDEF VER300}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
{$ENDIF}

// Delphi 10.1 Berlin
{$IFDEF VER310}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
{$ENDIF}

// Delphi 10.2 Tokyo
{$IFDEF VER320}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
{$ENDIF}

// Delphi 10.3 Rio
{$IFDEF VER330}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
{$ENDIF}

// Delphi 10.4 Sydney
{$IFDEF VER340}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
{$ENDIF}

// Delphi 11 Alexandria
{$IFDEF VER350}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11+}
{$ENDIF}

// Delphi 12
{$IFDEF VER360}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11+}
  {$DEFINE D12+}
{$ENDIF}

{$IFDEF D10_3+}
  {$Define HiDPISupport}
{$ENDIF}

{$IFDEF DXE8+}
  {$DEFINE UseRESTClientSearch}
{$ENDIF}


