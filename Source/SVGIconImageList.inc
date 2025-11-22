//To silent Hint messege of engine used
{.$DEFINE SvgDisableEngineHint}

//To use old OpenPictureDialog with image preview
{.$DEFINE OldPictureDialog}

//***********************************************************************
// Start of VCL Directives
//***********************************************************************
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

  //if PreferNativeSvgSupport not active or not available, will use predefined engine:
  //if XX_SVGEngine is not set at Compiler Options (or PreferNativeSvgSupport defined), one is defined here
  {$IF NOT DEFINED(Image32_SVGEngine) and NOT DEFINED(Skia_SVGEngine) and NOT DEFINED(PreferNativeSvgSupport) and NOT DEFINED(SVGMagic_Engine)}
    //use Delphi Engine from Image32 library by Angus Johnson (included into Image32 folder)
    {$DEFINE Image32_SVGEngine}
    //or use Engine Skia with skia4delphi wrapper by Vinícius Felipe Botelho Barbosa (included into skia4delphi folder)
    {.$DEFINE Skia_SVGEngine}
    //or use SVGMagic Engine with SVGMagicFactory
    {.$DEFINE SVGMagic_Engine}
  {$ENDIF}

  // When IgnoreAntiAliasedColor is set it svgs paints well but not perfect on
  // all backgrounds.  Works best with Delphi_SVGEngine
  {.$DEFINE IgnoreAntiAliasedColor}
  {$IF DEFINED(Delphi_SVGEngine) and NOT DEFINED(PreferNativeSvgSupport)}
    {$DEFINE IgnoreAntiAliasedColor}
  {$ENDIF}

  //Check correct defines for VCL Apps
  {$IF NOT DEFINED(Image32_SVGEngine) and NOT DEFINED(Skia_SVGEngine) and NOT DEFINED(PreferNativeSvgSupport) and NOT DEFINED(SVGMagic_Engine)}
    {$MESSAGE FATAL 'You must define at least one engine (Image32_SVGEngine or Skia_Engine or SVGMagic_Engine or PreferNativeSvgSupport) at SVGIconImageList.inc or Compiler options)'}
  {$IFEND}

  {$IF (DEFINED(Image32_SVGEngine) and DEFINED(Skia_SVGEngine)) or
       (DEFINED(Image32_SVGEngine) and DEFINED(SVGMagic_Engine)) or
       (DEFINED(Image32_SVGEngine) and DEFINED(PreferNativeSvgSupport)) or
       (DEFINED(Skia_SVGEngine) and DEFINED(SVGMagic_Engine)) or
       (DEFINED(Skia_SVGEngine) and DEFINED(PreferNativeSvgSupport)) or
       (DEFINED(SVGMagic_Engine) and DEFINED(PreferNativeSvgSupport)) }
    {$MESSAGE FATAL 'You must define only one engine for VCL (Image32_SVGEngine or Skia_Engine or SVGMagic_Engine or PreferNativeSvgSupport) at SVGIconImageList.inc or Compiler options)'}
  {$ENDIF}
//***********************************************************************
// End of VCL Directives
//***********************************************************************

//***********************************************************************
// Start of FMX Directives
//***********************************************************************
  //if FMX_XX_SVGEngine is not set at Compiler Options, one is defined here
  {$IF NOT (DEFINED(FMX_Image32_SVGEngine) or DEFINED(FMX_Skia_SVGEngine))}
    //use Delphi Engine from Image32 library by Angus Johnson (included into Image32 folder)
    {$DEFINE FMX_Image32_SVGEngine}
    //or use Engine Skia with skia4delphi wrapper by Vinícius Felipe Botelho Barbosa (included into skia4delphi folder)
    {.$DEFINE FMX_Skia_SVGEngine}
  {$ENDIF}

  //Check correct defines for FMX Apps
  {$IF NOT DEFINED(FMX_Image32_SVGEngine) and NOT DEFINED(FMX_Skia_SVGEngine)}
    {$MESSAGE FATAL 'You must define at least one engine for FMX (FMX_Image32_SVGEngine or FMX_Skia_Engine) at SVGIconImageList.inc or Compiler options)'}
  {$ENDIF}

  {$IF DEFINED(FMX_Image32_SVGEngine) and DEFINED(FMX_Skia_SVGEngine)}
    {$MESSAGE FATAL 'You must define only one engine for FMX (FMX_Image32_SVGEngine or FMX_Skia_Engine) at SVGIconImageList.inc or Compiler options)'}
  {$ENDIF}
//***********************************************************************
// End of FMX Directives
//***********************************************************************


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

// Delphi 12 Athens
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

// Delphi 13 Florence
{$IFDEF VER370}
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
  {$DEFINE D13+}
{$ENDIF}

{$IFDEF D10_3+}
  {$Define HiDPISupport}
{$ENDIF}

{$IFDEF DXE8+}
  {$DEFINE UseRESTClientSearch}
{$ENDIF}


