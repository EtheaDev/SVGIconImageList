{-----------------------------------------------------------------------------
 Unit Name: SVGInterfaces
 Author:    PyScripter
 Purpose:   Inteface-based access to Svg parsing and drawing
 History:
-----------------------------------------------------------------------------}
/// <summary>
///   Core interfaces for SVG document abstraction and factory pattern.
///   This unit provides the fundamental interfaces used by all SVG rendering
///   engines in the SVGIconImageList library.
/// </summary>
/// <remarks>
///   The library supports multiple SVG rendering engines through a factory pattern:
///   <list type="bullet">
///     <item>Image32 - Native Delphi implementation (default)</item>
///     <item>SVGMagic - Advanced engine with animated SVG support</item>
///     <item>Skia4Delphi - Google Skia graphics wrapper</item>
///     <item>Direct2D - Windows native implementation (Windows 10+)</item>
///   </list>
///   Use GlobalSVGFactory to obtain the active factory and create ISVG instances.
/// </remarks>
unit SVGInterfaces;

interface

Uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes;

const
  /// <summary>
  ///   Special color constant indicating that the SVG should inherit the color
  ///   from its parent or use the original color defined in the SVG source.
  /// </summary>
  SVG_INHERIT_COLOR = TColors.SysDefault;

  /// <summary>
  ///   Special color constant indicating no color should be applied.
  /// </summary>
  SVG_NONE_COLOR = TColors.SysNone;

type
  /// <summary>
  ///   Exception class for SVG-related errors during parsing, loading, or rendering.
  /// </summary>
  ESVGException = class(Exception);

  /// <summary>
  ///   Interface for SVG document abstraction. Provides methods for loading,
  ///   saving, and rendering SVG images with various visual transformations.
  /// </summary>
  /// <remarks>
  ///   ISVG is the core interface implemented by all SVG rendering engines.
  ///   Use GlobalSVGFactory.NewSvg to create instances of this interface.
  ///   The actual implementation depends on the configured SVG engine.
  /// </remarks>
  /// <example>
  ///   <code>
  ///   var
  ///     SVG: ISVG;
  ///   begin
  ///     SVG := GlobalSVGFactory.NewSvg;
  ///     SVG.LoadFromFile('icon.svg');
  ///     SVG.FixedColor := clRed;
  ///     SVG.PaintTo(Canvas.Handle, RectF(0, 0, 32, 32));
  ///   end;
  ///   </code>
  /// </example>
  ISVG = interface
    ['{70F71B0C-95FA-4D2D-84F6-481BD871B20B}']
    // property access methods
    function GetWidth: Single;
    function GetHeight: Single;
    function GetOpacity: Single;
    procedure SetOpacity(const Opacity: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const IsGrayScale: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const Color: TColor);
    function GetApplyFixedColorToRootOnly: Boolean;
    procedure SetApplyFixedColorToRootOnly(Value:Boolean);
    function GetSource: string;
    procedure SetSource(const ASource: string);

    /// <summary>
    ///   Checks whether the SVG document is empty or contains no valid content.
    /// </summary>
    /// <returns>
    ///   True if the SVG is empty or invalid; False otherwise.
    /// </returns>
    function IsEmpty: Boolean;

    /// <summary>
    ///   Clears the SVG document, releasing all loaded content.
    /// </summary>
    procedure Clear;

    /// <summary>
    ///   Saves the SVG document to a stream.
    /// </summary>
    /// <param name="Stream">
    ///   The stream to save the SVG content to.
    /// </param>
    procedure SaveToStream(Stream: TStream);

    /// <summary>
    ///   Saves the SVG document to a file.
    /// </summary>
    /// <param name="FileName">
    ///   The full path of the file to save the SVG content to.
    /// </param>
    procedure SaveToFile(const FileName: string);

    /// <summary>
    ///   Loads an SVG document from a stream.
    /// </summary>
    /// <param name="Stream">
    ///   The stream containing the SVG content to load.
    /// </param>
    /// <exception cref="ESVGException">
    ///   Raised if the stream contains invalid SVG content.
    /// </exception>
    procedure LoadFromStream(Stream: TStream);

    /// <summary>
    ///   Loads an SVG document from a file.
    /// </summary>
    /// <param name="FileName">
    ///   The full path of the SVG file to load.
    /// </param>
    /// <exception cref="ESVGException">
    ///   Raised if the file does not exist or contains invalid SVG content.
    /// </exception>
    procedure LoadFromFile(const FileName: string);

    /// <summary>
    ///   Renders the SVG document to a device context.
    /// </summary>
    /// <param name="DC">
    ///   The device context handle to render to.
    /// </param>
    /// <param name="R">
    ///   The destination rectangle for rendering.
    /// </param>
    /// <param name="KeepAspectRatio">
    ///   When True, maintains the original aspect ratio of the SVG.
    ///   When False, stretches to fill the entire rectangle.
    ///   Default is True.
    /// </param>
    procedure PaintTo(DC: HDC; R: TRectF; KeepAspectRatio: Boolean = True);

    /// <summary>
    ///   The intrinsic width of the SVG document as defined in the viewBox
    ///   or width attribute.
    /// </summary>
    property Width: Single read GetWidth;

    /// <summary>
    ///   The intrinsic height of the SVG document as defined in the viewBox
    ///   or height attribute.
    /// </summary>
    property Height: Single read GetHeight;

    /// <summary>
    ///   The opacity level for rendering the SVG.
    /// </summary>
    /// <value>
    ///   A value between 0.0 (fully transparent) and 1.0 (fully opaque).
    ///   Default is 1.0.
    /// </value>
    property Opacity: Single read GetOpacity write SetOpacity;

    /// <summary>
    ///   Renders the SVG in grayscale when set to True.
    /// </summary>
    /// <value>
    ///   Default is False.
    /// </value>
    property GrayScale: Boolean read GetGrayScale write SetGrayScale;

    /// <summary>
    ///   Applies a fixed color to the SVG, replacing all colors in the image.
    /// </summary>
    /// <value>
    ///   Set to SVG_INHERIT_COLOR to use the original colors from the SVG source.
    ///   Default is SVG_INHERIT_COLOR.
    /// </value>
    /// <remarks>
    ///   This property is useful for monochrome icon sets where you want to
    ///   apply a consistent color theme.
    /// </remarks>
    property FixedColor: TColor read GetFixedColor write SetFixedColor;

    /// <summary>
    ///   When True, applies FixedColor only to the root SVG element,
    ///   preserving colors of child elements.
    /// </summary>
    /// <value>
    ///   Default is False (color is applied to all elements).
    /// </value>
    property ApplyFixedColorToRootOnly: Boolean read GetApplyFixedColorToRootOnly
      write SetApplyFixedColorToRootOnly;

    /// <summary>
    ///   The SVG source code as a string.
    /// </summary>
    /// <remarks>
    ///   Setting this property parses and loads the SVG content.
    ///   Getting this property returns the original or modified SVG source.
    /// </remarks>
    property Source: string read GetSource write SetSource;
  end;

  /// <summary>
  ///   Factory interface for creating ISVG instances.
  /// </summary>
  /// <remarks>
  ///   Each SVG rendering engine provides its own implementation of this interface.
  ///   Use GlobalSVGFactory to access the currently configured factory.
  /// </remarks>
  ISVGFactory = interface
    ['{D81A7410-F0DB-457E-BA9D-480A335A1337}']
    /// <summary>
    ///   Creates a new empty ISVG instance.
    /// </summary>
    /// <returns>
    ///   A new ISVG interface instance ready to load SVG content.
    /// </returns>
    function NewSvg: ISVG;
  end;

/// <summary>
///   Returns the global SVG factory instance used to create ISVG objects.
/// </summary>
/// <returns>
///   The currently configured ISVGFactory implementation.
/// </returns>
/// <remarks>
///   The factory is automatically configured based on the defines in
///   SVGIconImageList.inc. If no factory has been set, this function
///   initializes the default factory based on compile-time configuration.
/// </remarks>
function GlobalSVGFactory: ISVGFactory;

/// <summary>
///   Sets the global SVG factory instance.
/// </summary>
/// <param name="SVGFactory">
///   The ISVGFactory implementation to use for creating SVG instances.
/// </param>
/// <remarks>
///   Use this procedure to override the default factory at runtime.
///   This allows switching between SVG engines dynamically.
/// </remarks>
procedure SetGlobalSVGFactory(const SVGFactory : ISVGFactory);

/// <summary>
///   Returns a human-readable description of the currently active SVG engine.
/// </summary>
/// <returns>
///   A string describing the active engine, such as "Delphi Image32 Engine",
///   "Skia4delphi Engine", "SVGMagic Engine", or "Direct2D Windows Engine".
/// </returns>
function GetGlobalSVGFactoryDesc: string;

implementation

{$INCLUDE SVGIconImageList.inc}

Uses
  {$IF DEFINED(Image32_SVGEngine)}
    {$IFNDEF SvgDisableEngineHint}
    {$MESSAGE HINT 'Use Delphi native Image32 SVG-Engine for SVGIconImageList'}
    {$ENDIF}
    Image32SVGFactory
  {$ELSEIF DEFINED(Skia_SVGEngine)}
    {$IFNDEF SvgDisableEngineHint}
    {$MESSAGE HINT 'Use Skia4Delphi "wrapper" SVG-Engine for SVGIconImageList'}
    {$ENDIF}
    SkiaSVGFactory
  {$ELSEIF DEFINED(SVGMagic_Engine)}
    {$IFNDEF SvgDisableEngineHint}
    {$MESSAGE HINT 'Use Delphi native SVGMagic Engine for SVGIconImageList'}
    {$ENDIF}
    SVGMagicFactory
  {$IFEND}
  {$IFDEF PreferNativeSvgSupport}
    {$IFNDEF SvgDisableEngineHint}
    {$MESSAGE HINT 'but Prefer Windows Direct-2D SVG-Engine if available'}
    {$ENDIF}
    , D2DSVGFactory
  {$ENDIF}
  ;

Var
 FGlobalSVGFactory: ISVGFactory;

function GlobalSVGFactory: ISVGFactory;
begin
  if not Assigned(FGlobalSVGFactory) then
  begin
    {$IFDEF PreferNativeSvgSupport}
    if WinSvgSupported then
      FGlobalSVGFactory := GetD2DSVGFactory
    else
    {$ENDIF}
    {$IF DEFINED(Image32_SVGEngine)}
      FGlobalSVGFactory := GetImage32SVGFactory;
    {$ELSEIF DEFINED(Skia_SVGEngine)}
      FGlobalSVGFactory := GetSkiaSVGFactory;
    {$ELSEIF DEFINED(SVGMagic_Engine)}
      FGlobalSVGFactory := GetSVGMagicFactory;
    {$IFEND}
  end;
  Result := FGlobalSVGFactory;
end;

procedure SetGlobalSVGFactory(const SVGFactory : ISVGFactory);
begin
  FGlobalSVGFactory := SVGFactory;
end;

function GetGlobalSVGFactoryDesc: string;
begin
  {$IFDEF PreferNativeSvgSupport}
  if WinSvgSupported then
    Result := 'Direct2D Windows Engine'
  else
  {$ENDIF}
  {$IF DEFINED(Image32_SVGEngine)}
    Result := 'Delphi Image32 Engine'
  {$ELSEIF DEFINED(Skia_SVGEngine)}
    Result := 'Skia4delphi Engine'
  {$ELSEIF DEFINED(SVGMagic_Engine)}
    Result := 'SVGMagic Engine'
  {$IFEND}
end;

end.
