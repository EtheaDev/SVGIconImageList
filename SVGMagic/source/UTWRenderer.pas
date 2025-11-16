{**
 @abstract(@name provides a base class for all renderers.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWRenderer;

interface

uses System.Classes,
     System.Types,
     System.Math,
     System.SysUtils,
     System.Generics.Defaults,
     System.Generics.Collections,
     Vcl.Graphics,
     UTWRendererCommon,
     UTWPoint,
     UTWSize,
     UTWRect,
     UTWColor,
     UTWFillAndStroke,
     UTWMatrix,
     UTWHelpers,
     UTWGraphicPath,
     UTWControlFont,
     Winapi.Windows;

type
    {**
     Base class to draw common shapes and to make common operations with graphic libraries
     @author(JMR)
    }
    TWRenderer = class(TObject)
        public type
            IPointList = array of TWPointF;

            {**
             Draw capabilities enumeration
             @value(IE_None Indicates no draw capabilities)
             @value(IE_DeviceSupported Indicates that drawing is supported by device)
            }
            IEDrawCaps =
            (
                IE_None,
                IE_DeviceSupported
            );

            IDrawCaps = set of IEDrawCaps;

            {**
             Radius options
            }
            IRadius = class
                private
                    m_LeftTop:     TPoint;
                    m_LeftBottom:  TPoint;
                    m_RightTop:    TPoint;
                    m_RightBottom: TPoint;

                protected
                    {**
                     Get left top radius
                     @returns(Left top radius)
                    }
                    function GetLeftTop: PPoint; virtual;

                    {**
                     Get left Bottom radius
                     @returns(Left Bottom radius)
                    }
                    function GetLeftBottom: PPoint; virtual;

                    {**
                     Get right top radius
                     @returns(Right top radius)
                    }
                    function GetRightTop: PPoint; virtual;

                    {**
                     Get right Bottom radius
                     @returns(Right Bottom radius)
                    }
                    function GetRightBottom: PPoint; virtual;

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
                     Set left top radius
                     @param(radius Left top radius to set)
                    }
                    procedure SetLeftTop(const radius: TPoint); virtual;

                    {**
                     Set left Bottom radius
                     @param(radius Left Bottom radius to set)
                    }
                    procedure SetLeftBottom(const radius: TPoint); virtual;

                    {**
                     Set right top radius
                     @param(radius Right top radius to set)
                    }
                    procedure SetRightTop(const radius: TPoint); virtual;

                    {**
                     Set right Bottom radius
                     @param(radius Right Bottom radius to set)
                    }
                    procedure SetRightBottom(const radius: TPoint); virtual;

                    {**
                     Check if radius is symmetric (i.e. rx = ry)
                     @returns(@true if radius is symmetric, otherwise @false)
                    }
                    function IsSymmetric: Boolean; inline;

                public
                    {**
                     Get the left top radius value
                    }
                    property LeftTop: PPoint read GetLeftTop;

                    {**
                     Get the left bottom radius value
                    }
                    property LeftBottom: PPoint read GetLeftBottom;

                    {**
                     Get the right top radius value
                    }
                    property RightTop: PPoint read GetRightTop;

                    {**
                     Get the right bottom radius value
                    }
                    property RightBottom: PPoint read GetRightBottom;
            end;

            {**
             Outline options
            }
            IOutline = class
                private
                    m_ShowLeft:        Boolean;
                    m_ShowTop:         Boolean;
                    m_ShowRight:       Boolean;
                    m_ShowBottom:      Boolean;
                    m_ShowLeftTop:     Boolean;
                    m_ShowLeftBottom:  Boolean;
                    m_ShowRightTop:    Boolean;
                    m_ShowRightBottom: Boolean;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set if the left edge is shown
                    }
                    property ShowLeft: Boolean read m_ShowLeft write m_ShowLeft default True;

                    {**
                     Get or set if the top edge is shown
                    }
                    property ShowTop: Boolean read m_ShowTop write m_ShowTop default True;

                    {**
                     Get or set if the right edge is shown
                    }
                    property ShowRight: Boolean read m_ShowRight write m_ShowRight default True;

                    {**
                     Get or set if the bottom edge is shown
                    }
                    property ShowBottom: Boolean read m_ShowBottom write m_ShowBottom default True;

                    {**
                     Get or set if the left top corner is shown
                    }
                    property ShowLeftTop: Boolean read m_ShowLeftTop write m_ShowLeftTop default True;

                    {**
                     Get or set if the left bottom corner is shown
                    }
                    property ShowLeftBottom: Boolean read m_ShowLeftBottom write m_ShowLeftBottom default True;

                    {**
                     Get or set if the right top edge is shown
                    }
                    property ShowRightTop: Boolean read m_ShowRightTop write m_ShowRightTop default True;

                    {**
                     Get or set if the right bottom edge is shown
                    }
                    property ShowRightBottom: Boolean read m_ShowRightBottom write m_ShowRightBottom default True;
            end;

            {**
             Bubble rectangle tail options
            }
            ITailOptions = class
                private
                    m_Width:       Cardinal;
                    m_Height:      Cardinal;
                    m_Position:    Single;
                    m_Orientation: EOrientation;
                    m_Visible:     Boolean;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set the width
                    }
                    property Width: Cardinal read m_Width write m_Width;

                    {**
                     Get or set the height
                    }
                    property Height: Cardinal read m_Height write m_Height;

                    {**
                     Get or set the position
                    }
                    property Position: Single read m_Position write m_Position;

                    {**
                     Get or set the orientation
                    }
                    property Orientation: EOrientation read m_Orientation write m_Orientation default E_O_Top;

                    {**
                     Get or set if the tail is visible
                    }
                    property Visible: Boolean read m_Visible write m_Visible;
            end;

            PITailOptions = ^ITailOptions;

            {**
             Basic shape options
            }
            IShapeOptions = class
                private
                    m_pFill:           TWFill;
                    m_pStroke:         TWStroke;
                    m_AntiAliasing:    Boolean;
                    m_LayeredMode:     Boolean;
                    m_TransformMatrix: TWMatrix3x3;

                protected
                    {**
                     Get the transformation matrix
                     @returns(Transformation matrix)
                    }
                    function GetTransformMatrix: PWMatrix3x3; virtual;

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
                     Set the transformation matrix
                     @param(pMatrix Transformation matrix to set)
                    }
                    procedure SetTransformMatrix(const pMatrix: PWMatrix3x3); virtual;

                public
                    {**
                     Get or set the fill properties
                    }
                    property Fill: TWFill read m_pFill write m_pFill;

                    {**
                     Get or set the stroke properties
                    }
                    property Stroke: TWStroke read m_pStroke write m_pStroke;

                    {**
                     Enable or disable the antialiasing
                    }
                    property Antialiasing: Boolean read m_AntiAliasing write m_AntiAliasing;

                    {**
                     Enable or disable the layered mode
                     @br @bold(NOTE) The layered mode should be enabled when the paint is done on a
                                     layered window (i.e. if WS_EX_LAYERED is set in the window
                                     extended styles). If this happen, the alpha blending may result
                                     in an incorrect color, for that a special drawing process should
                                     be applied during paint)
                    }
                    property LayeredMode: Boolean read m_LayeredMode write m_LayeredMode;

                    {**
                     Get the transformation matrix
                    }
                    property TransformMatrix: PWMatrix3x3 read GetTransformMatrix;
            end;

            {**
             Rectangle options
            }
            IRectOptions = class(IShapeOptions)
                private
                    m_pRadius:  IRadius;
                    m_pOutline: IOutline;
                    m_pTail:    ITailOptions;

                public
                    {**
                     Constructor
                    }
                    constructor Create; override;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set the radius
                    }
                    property Radius: IRadius read m_pRadius write m_pRadius;

                    {**
                     Get or set the outline
                    }
                    property Outline: IOutline read m_pOutline write m_pOutline;

                    {**
                     Get or set the tail description
                    }
                    property Tail: ITailOptions read m_pTail write m_pTail;
            end;

            {**
             Text options
            }
            ITextOptions = class
                private
                    m_Alpha:            Cardinal;
                    m_Contrast:         Cardinal;
                    m_RightToLeft:      Boolean;
                    m_Vertical:         Boolean;
                    m_NoWrap:           Boolean;
                    m_NoClip:           Boolean;
                    m_ShowHotkeyPrefix: Boolean;
                    m_Layered:          Boolean;
                    m_pFont:            TFont;
                    m_AlignHorz:        EAlignHorz;
                    m_AlignVert:        EAlignVert;
                    m_ShadowColor:      TWColor;
                    m_ShadowDelta:      TPoint;
                    m_ShadowBlur:       TSize;
                    m_TextRendering:    ETextRendering;
                    m_TextTrimming:     ETextTrimming;
                    m_DrawTextFunc:     EDrawTextFunc;

                protected
                    {**
                     Get the shadow color
                     @returns(The shadow color)
                    }
                    function GetShadowColor: PWColor; virtual;

                    {**
                     Get the shadow delta
                     @returns(The shadow delta)
                    }
                    function GetShadowDelta: PPoint; virtual;

                    {**
                     Get the shadow blur
                     @returns(The shadow blur)
                    }
                    function GetShadowBlur: PSize; virtual;

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
                     Assign (i.e. copy) the text options content from another text options
                     @param(pOther Other text options to copy from)
                    }
                    procedure Assign(const pOther: ITextOptions); virtual;

                    {**
                     Set shadow color
                     @param(pColor Shadow color)
                    }
                    procedure SetShadowColor(const pColor: PWColor); virtual;

                    {**
                     Set shadow delta
                     @param(delta Shadow delta, in pixels)
                    }
                    procedure SetShadowDelta(const delta: TPoint); virtual;

                    {**
                     Set shadow blur
                     @param(blur Shadow blur size, in pixels)
                    }
                    procedure SetShadowBlur(const blur: TSize); virtual;

                public
                    {**
                     Get or set color alpha value
                    }
                    property Alpha: Cardinal read m_Alpha write m_Alpha default 255;

                    {**
                     Get or set the contrast
                    }
                    property Contrast: Cardinal read m_Contrast write m_Contrast;

                    {**
                     Enable or disable right to left layout
                    }
                    property RightToLeft: Boolean read m_RightToLeft write m_RightToLeft;

                    {**
                     Enable or disable vertical layout
                    }
                    property Vertical: Boolean read m_Vertical write m_Vertical;

                    {**
                     Enable or disable wrapping
                    }
                    property NoWrap: Boolean read m_NoWrap write m_NoWrap;

                    {**
                     Enable or disable clipping
                    }
                    property NoClip: Boolean read m_NoClip write m_NoClip;

                    {**
                     Get or set if the hotkey prefix should be shown
                    }
                    property ShowHotkeyPrefix: Boolean read m_ShowHotkeyPrefix write m_ShowHotkeyPrefix;

                    {**
                     Enable or disable the layered mode
                     @br @bold(NOTE) The layered mode should be enabled when the paint is done on a
                                     layered window (i.e. if WS_EX_LAYERED is set in the window
                                     extended styles). If this happen, the alpha blending may result
                                     in an incorrect color, for that a special drawing process should
                                     be applied during paint)
                    }
                    property Layered: Boolean read m_Layered write m_Layered;

                    {**
                     Get or set the font to use
                    }
                    property Font: TFont read m_pFont write m_pFont;

                    {**
                     Get or set the horizontal alignment
                    }
                    property AlignHorz: EAlignHorz read m_AlignHorz write m_AlignHorz default E_H_Left;

                    {**
                     Get or set vertical alignment
                    }
                    property AlignVert: EAlignVert read m_AlignVert write m_AlignVert default E_V_Top;

                    {**
                     Get or set the text rendering mode
                    }
                    property TextRendering: ETextRendering read m_TextRendering write m_TextRendering default E_R_Default;

                    {**
                     Get or set the text trimming
                    }
                    property TextTrimming: ETextTrimming read m_TextTrimming write m_TextTrimming default E_TT_EllipsisCharacter;

                    {**
                     Get or set the draw text function to use
                    }
                    property DrawTextFunc: EDrawTextFunc read m_DrawTextFunc write m_DrawTextFunc default E_DF_Default;

                    {**
                     Get the shadow color
                    }
                    property ShadowColor: PWColor read GetShadowColor;

                    {**
                     Get the shadow delta
                    }
                    property ShadowDelta: PPoint read GetShadowDelta;

                    {**
                     Get the shadow blur
                    }
                    property ShadowBlur: PSize read GetShadowBlur;
            end;

            {**
             Image options
            }
            IImageOptions = class
                private
                    m_ResizeMode:  EImageResizeMode;
                    m_Opacity:     Double;
                    m_Transparent: Boolean;
                    m_Vectorial:   Boolean;

                public
                    {**
                     Constructor
                    }
                    constructor Create; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get or set the resize mode
                    }
                    property ResizeMode: EImageResizeMode read m_ResizeMode write m_ResizeMode;

                    {**
                     Get or set the opacity
                    }
                    property Opacity: Double read m_Opacity write m_Opacity;

                    {**
                     Get or set if image is transparent
                    }
                    property Transparent: Boolean read m_Transparent write m_Transparent;

                    {**
                     Get or set if image is vectorial
                    }
                    property Vectorial: Boolean read m_Vectorial write m_Vectorial;
            end;

        private
            m_DrawTextFunc: EDrawTextFunc;

        protected
            {**
             Get brushes from a fill
             @param(pFill Fill to extract brushes from)
             @param(pSolid @bold([out]) Solid brush, @nil if not found)
             @param(pLinear @bold([out]) Linear gradient brush, @nil if not found)
             @param(pRadial @bold([out]) Radial gradient brush, @nil if not found)
            }
            procedure GetBrushes(const pFill: TWFill; out pSolid: TWSolidBrush;
                    out pLinear: TWLinearGradientBrush; out pRadial: TWRadialGradientBrush); overload; virtual;

            {**
             Get brushes from a stroke
             @param(pStroke Stroke to extract brushes from)
             @param(pSolid @bold([out]) Solid brush, @nil if not found)
             @param(pLinear @bold([out]) Linear gradient brush, @nil if not found)
             @param(pRadial @bold([out]) Radial gradient brush, @nil if not found)
            }
            procedure GetBrushes(const pStroke: TWStroke; out pSolid: TWSolidBrush;
                    out pLinear: TWLinearGradientBrush; out pRadial: TWRadialGradientBrush); overload; virtual;

            {**
             Get x radius
             @param(radius Radius)
             @returns(Radius value)
            }
            function GetRadiusX(const radius: TPoint): Cardinal; virtual;

            {**
             Get y radius
             @param(radius Radius)
             @returns(Radius value)
            }
            function GetRadiusY(const radius: TPoint): Cardinal; virtual;

            {**
             Get symmetric radius at index
             @param(pRadius Radius)
             @param(index Index (between 0 and 3) representing corner to get)
             @returns(Radius value)
            }
            function GetSymmetricRadius(const pRadius: IRadius; index: Cardinal): Cardinal; virtual;

            {**
             Change line length
             @param(startPoint Start line point)
             @param(endPoint @bold([in, out]) End line point, shorten or extended line end point on return)
             @param(delta Length to shorten (negative value) or extend in pixels)
            }
            procedure ChangeLineLength(const startPoint: TWPointF; var endPoint: TWPointF;
                    const delta: Double); virtual;

            {**
             Convert the stroke width from floating point value to integer value
             @param(pStroke Stroke containing the width to convert)
             @returns(Converted width)
            }
            class function ConvertStrokeWidth(const pStroke: TWStroke): Integer; static;

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
             Begin a scene
             @param(hDC Device context to draw on)
            }
            procedure BeginScene(hDC: THandle); virtual; abstract;

            {**
             End a scene
            }
            procedure EndScene; virtual; abstract;

            {**
             Draw rectangle
             @param(rect Rectangle area to draw)
             @param(pOptions Rectangle paint options)
             @param(hDC Device context handle)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
            }
            function DrawRect(const rect: TWRectF; pOptions: IRectOptions; hDC: THandle;
                    out iRect: TRect): Boolean; virtual; abstract;

            {**
             Draw polygon
             @param(rect Rectangle area to draw)
             @param(pointList Point list to draw)
             @param(pOptions Polygon paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawPolygon(const rect: TWRectF; const pointList: IPointList;
                    const pOptions: IShapeOptions; hDC: THandle): Boolean; virtual; abstract;

            {**
             Draw path
             @param(rect Rectangle in which path should be drawn)
             @param(path Path to draw)
             @param(pOptions Path paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawPath(const rect: TWRectF; const path: TWPathCmds;
                    const pOptions: IShapeOptions; hDC: THandle): Boolean; virtual; abstract;

            {**
             Get text size
             @param(text Text to measure)
             @param(rect Text bounding rectangle)
             @param(pOptions Text options)
             @param(hDC Device context handle)
             @param(pCharacters @bold([out]) Pointer to variable to receive the number of characters
                                             that will be displayed in rect, if @nil will be ingored)
             @param(pLines @bold([out]) Pointer to variable to receive the number of lines that will
                                        be displayed in rect, if @nil will be ingored)
             @returns(Text size, empty size on error)
            }
            function GetTextSize(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
                    pLines: PInteger = nil): TWSizeF; virtual; abstract;

            {**
             Draw text
             @param(text Text to draw)
             @param(rect Rectangle area containing text)
             @param(pOptions Text GDI+ options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawText(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: ITextOptions; hDC: THandle): Boolean; virtual; abstract;

            {**
             Draw text on layered form
             @param(text Text to draw)
             @param(rect Rectangle area containing text)
             @param(pOptions Text GDI+ options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawLayeredText(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: ITextOptions; hDC: THandle): Boolean; virtual; abstract;

            {**
             Draw image
             @param(pGraphic Image to draw)
             @param(srcRect Source image rect to draw)
             @param(hDC Destination device context handle)
             @param(destRect Rect where image will be drawn on dest, stretched if not equal to source)
             @param(pOptions Options)
            }
            procedure DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
                    const destRect: TWRectF; const pOptions: IImageOptions); virtual; abstract;

            {**
             Get supported draw capabilities
             @returns(Supported capabilities)
            }
            function GetCaps: IDrawCaps; virtual; abstract;

            {**
             Add new custom font from file and set it available for applications opened in Windows session
             @param(name Font name to add)
             @param(fileName Font file name)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Thus added font will be available for ALL applications until the Windows
                             session is closed or the font is removed using RemoveFont or RemoveFonts.
                             But be careful, this is true ONLY for GDI fonts, and NOT for GDI+
            }
            function AddFontToSession(const name, fileName: UnicodeString): Boolean; virtual; abstract;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(fileName Font file name)
             @returns(font handle, @nil on error)
            }
            function AddFont(const name, fileName: UnicodeString): THandle; overload; virtual; abstract;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(pStream Stream containing font data)
             @param(fontLength Font data length)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Thus added font will be available for ALL applications until the Windows
                             session is closed or the font is removed using RemoveFont or RemoveFonts.
                             But be careful, this is true ONLY for GDI fonts, and NOT for GDI+
            }
            function AddFont(const name: UnicodeString; pStream: TStream; fontLength: Cardinal):
                    THandle; overload; virtual; abstract;

            {**
             Get font
             @param(name Font name)
             @returns(font, @nil if not found)
             @br @bold(NOTE) Only font previously added with AddFont() can be get. Standard or custom
                             font belonging to Windows session are not included
            }
            function GetFont(const name: UnicodeString): THandle; virtual;

            {**
             Get font name from custom font file
             @param(fileName Font file name)
             @returns(Font name, empty string on error)
            }
            function GetFontName(const fileName: UnicodeString): UnicodeString; virtual;

            {**
             Remove previously added font from Windows session
             @param(name Font name)
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            procedure RemoveFontFromSession(const name: UnicodeString); virtual;

            {**
             Remove all previously added fonts from Windows session
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            procedure RemoveFontsFromSession; virtual;

            {**
             Remove previously added font
             @param(name Font name)
            }
            procedure RemoveFont(const name: UnicodeString); virtual;

            {**
             Remove all previously added fonts
            }
            procedure RemoveFonts; virtual;

            {**
             Get font size so text can fit in rectangle
             @param(text Text to measure)
             @param(rect Text bounding rectangle to fit)
             @param(pOptions Text GDI+ options)
             @param(minWrapFontSize Min font size after which text wrap is authorized)
             @param(minFontSize Minimum allowed font size)
             @param(hDC Device context handle)
             @param(fontSize @bold([out]) Result font size)
             @returns(@true if font size is found for text to fit rect, @false if still too big or
                      on invalid argument)
             @br @bold(NOTE) This function makes certain operations on the options.m_pFont pointer
                             directly. By doing this, the original source font object is modified,
                             even if restored after function ends. This may affect source object,
                             such as e.g. calling its OnFontChange event
             @author Niki, David
            }
            function GetFontSizeToFit(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: ITextOptions; minWrapFontSize, minFontSize: Cardinal;
                    hDC: THandle; out fontSize: Cardinal): Boolean; virtual;

            {**
             Check if fill is visible (i.e. if fill color isn't fully transparent)
             @param(pFill Fill to check)
             @returns(@true if fill is visible, otherwise @false)
            }
            class function IsFillVisible(const pFill: TWFill): Boolean; static;

            {**
             Check if stroke is visible (i.e. if stroke color isn't fully transparent and if stroke
             has a width)
             @param(pStroke Stroke to check)
             @param(ignoreColor If @true, stroke color will be ignored, only stroke width will be tested)
             @returns(@true if stroke is visible, otherwise @false)
            }
            class function IsStrokeVisible(const pStroke: TWStroke; ignoreColor: Boolean = False): Boolean; static;

            {**
             Check if gradient is visible (i.e if at least 1 color is not fully transparent)
             @param(pFill Fill to check)
             @returns(@true if gradient is visible, otherwise @false)
            }
            class function IsGradientVisible(const pFill: TWFill): Boolean; overload; static;

            {**
             Check if gradient is visible (i.e if at least 1 color is not fully transparent)
             @param(pStroke Stroke to check)
             @returns(@true if gradient is visible, otherwise @false)
            }
            class function IsGradientVisible(const pStroke: TWStroke): Boolean; overload; static;

            {**
             Check if outline is visible (pen should be visible and at least 1 segment should be shown)
             @param(pStroke Stroke to use to draw outline)
             @param(pOutline Outline parameters)
             @param(ignoreColor If @true, stroke color will be ignored, only stroke width will be tested)
            }
            class function IsOutlineVisible(const pStroke: TWStroke; const pOutline: IOutline;
                    ignoreColor: Boolean = True): Boolean; static;

            {**
             Check if at least 1 corner is visible
             @param(pRadius Radius to check)
             @param(pOutline Outline)
             @returns(@true if at least 1 corner is visible, otherwise @false)
            }
            class function IsCornerVisible(const pRadius: IRadius; const pOutline: IOutline): Boolean; static;

            {**
             Check if outline is completely visible (i.e. if all outline parts are visible)
             @param(pOutline Outline)
             @returns(@true if outline is completely visible, otherwise @false)
            }
            class function IsCompletelyVisible(const pOutline: IOutline): Boolean; static;

            {**
             Check if radius values are regular (i.e. each radius value is equivalent to others)
             @param(pRadius Radius to check)
             @returns(@true if radius is regular, otherwise @false)
            }
            class function IsRegular(const pRadius: IRadius): Boolean; static;

            {**
             Check if tail is visible in rect
             @param(pOptions Rectangle options to check)
             @returns(@true if tail is visible, otherwise @false)
            }
            class function IsTailVisible(const pOptions: IRectOptions): Boolean; static;

            {**
             Check if rectangle is visible
             @param(pOptions Rectangle options to check)
             @returns(@true if rectangle is visible, otherwise @false)
            }
            class function IsRectVisible(const pOptions: IRectOptions): Boolean; static;

            {**
             Calculate and get max size to use with pen width and radius
             @param(rect Rect in which shape will be drawn)
             @returns(Max size)
            }
            class function GetMaxSize(const rect: TWRectF): Single; static;

            {**
             Calculate and get max size to use with pen width and x radius
             @param(rect Rect in which shape will be drawn)
             @returns(Max size)
            }
            class function GetMaxSizeX(const rect: TWRectF): Single; static;

            {**
             Calculate and get max size to use with pen width and y radius
             @param(rect Rect in which shape will be drawn)
             @returns(Max size)
            }
            class function GetMaxSizeY(const rect: TWRectF): Single; static;

            {**
             Calculate draw rectangle considering outline
             @param(rect Rectangle area)
             @param(pOptions Rectangle options (outline, radius, ...))
             @param(useShifting If @true, special width and height shifting will be used on values)
             @returns(Draw rectangle)
            }
            class function CalculateDrawRect(const rect: TWRectF; const pOptions: IRectOptions;
                    useShifting: Boolean): TWRectF; static;

        public
            {**
             Get or set the default draw text function to use
            }
            property DrawTextFunc: EDrawTextFunc read m_DrawTextFunc write m_DrawTextFunc default E_DF_Default;
    end;

implementation
//---------------------------------------------------------------------------
// TWRenderer.IRadius
//---------------------------------------------------------------------------
constructor TWRenderer.IRadius.Create;
begin
    inherited Create;

    m_LeftTop     := Default(TPoint);
    m_LeftBottom  := Default(TPoint);
    m_RightTop    := Default(TPoint);
    m_RightBottom := Default(TPoint);
end;
//---------------------------------------------------------------------------
destructor TWRenderer.IRadius.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWRenderer.IRadius.GetLeftTop: PPoint;
begin
    Result := @m_LeftTop;
end;
//---------------------------------------------------------------------------
function TWRenderer.IRadius.GetLeftBottom: PPoint;
begin
    Result := @m_LeftBottom;
end;
//---------------------------------------------------------------------------
function TWRenderer.IRadius.GetRightTop: PPoint;
begin
    Result := @m_RightTop;
end;
//---------------------------------------------------------------------------
function TWRenderer.IRadius.GetRightBottom: PPoint;
begin
    Result := @m_RightBottom;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.IRadius.SetLeftTop(const radius: TPoint);
begin
    m_LeftTop := TPoint.Create(radius);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.IRadius.SetLeftBottom(const radius: TPoint);
begin
    m_LeftBottom := TPoint.Create(radius);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.IRadius.SetRightTop(const radius: TPoint);
begin
    m_RightTop := TPoint.Create(radius);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.IRadius.SetRightBottom(const radius: TPoint);
begin
    m_RightBottom := TPoint.Create(radius);
end;
//---------------------------------------------------------------------------
function TWRenderer.IRadius.IsSymmetric: Boolean;
begin
    // if radius contains only one value (x or y, it's equal), this value will be used as radius
    // value. If both x and y values are declared, the radius is assymetric, except if both
    // values are equal. If both x and y values are empty, radius isn't visible
    Result := (((m_LeftTop.X     = 0) or (m_LeftTop.Y     = 0) or (m_LeftTop.X     = m_LeftTop.Y))
           and ((m_LeftBottom.X  = 0) or (m_LeftBottom.Y  = 0) or (m_LeftBottom.X  = m_LeftBottom.Y))
           and ((m_RightTop.X    = 0) or (m_RightTop.Y    = 0) or (m_RightTop.X    = m_RightTop.Y))
           and ((m_RightBottom.X = 0) or (m_RightBottom.Y = 0) or (m_RightBottom.X = m_RightBottom.Y)));
end;
//---------------------------------------------------------------------------
// TWRenderer.IOutline
//---------------------------------------------------------------------------
constructor TWRenderer.IOutline.Create;
begin
    inherited Create;

    m_ShowLeft        := True;
    m_ShowTop         := True;
    m_ShowRight       := True;
    m_ShowBottom      := True;
    m_ShowLeftTop     := True;
    m_ShowLeftBottom  := True;
    m_ShowRightTop    := True;
    m_ShowRightBottom := True;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.IOutline.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer.ITailOptions
//---------------------------------------------------------------------------
constructor TWRenderer.ITailOptions.Create;
begin
    inherited Create;

    m_Width       := 0;
    m_Height      := 0;
    m_Position    := 0.5;
    m_Orientation := E_O_Top;
    m_Visible     := False;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.ITailOptions.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer.IShapeOptions
//---------------------------------------------------------------------------
constructor TWRenderer.IShapeOptions.Create;
begin
    inherited Create;

    m_pFill           := TWFill.Create;
    m_pStroke         := TWStroke.Create;
    m_AntiAliasing    := False;
    m_LayeredMode     := False;
    m_TransformMatrix := TWMatrix3x3.GetDefault;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.IShapeOptions.Destroy;
begin
    m_pStroke.Free;
    m_pFill.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWRenderer.IShapeOptions.GetTransformMatrix: PWMatrix3x3;
begin
    Result := @m_TransformMatrix;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.IShapeOptions.SetTransformMatrix(const pMatrix: PWMatrix3x3);
begin
    if (not Assigned(pMatrix)) then
        raise Exception.Create('Matrix is undefined');

    m_TransformMatrix := TWMatrix3x3.Create(pMatrix^);
end;
//---------------------------------------------------------------------------
// TWRenderer.IRectOptions
//---------------------------------------------------------------------------
constructor TWRenderer.IRectOptions.Create;
begin
    inherited Create;

    m_pRadius  := IRadius.Create;
    m_pOutline := IOutline.Create;
    m_pTail    := ITailOptions.Create;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.IRectOptions.Destroy;
begin
    m_pTail.Free;
    m_pOutline.Free;
    m_pRadius.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer.ITextOptions
//---------------------------------------------------------------------------
constructor TWRenderer.ITextOptions.Create;
begin
    inherited Create;

    m_Alpha            := 255;
    m_Contrast         := 0;
    m_RightToLeft      := False;
    m_Vertical         := False;
    m_NoWrap           := False;
    m_NoClip           := False;
    m_ShowHotkeyPrefix := False;
    m_Layered          := False;
    m_pFont            := TFont.Create;
    m_AlignHorz        := E_H_Left;
    m_AlignVert        := E_V_Top;
    m_ShadowColor      := TWColor.Create(clBlack);
    m_TextRendering    := E_R_Default;
    m_TextTrimming     := E_TT_EllipsisCharacter;
    m_DrawTextFunc     := E_DF_Default;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.ITextOptions.Destroy;
begin
    m_pFont.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.ITextOptions.Assign(const pOther: ITextOptions);
begin
    m_Alpha            := pOther.m_Alpha;
    m_Contrast         := pOther.m_Contrast;
    m_RightToLeft      := pOther.m_RightToLeft;
    m_Vertical         := pOther.m_Vertical;
    m_NoWrap           := pOther.m_NoWrap;
    m_NoClip           := pOther.m_NoClip;
    m_ShowHotkeyPrefix := pOther.m_ShowHotkeyPrefix;
    m_Layered          := pOther.m_Layered;
    m_AlignHorz        := pOther.m_AlignHorz;
    m_AlignVert        := pOther.m_AlignVert;
    m_ShadowDelta      := pOther.m_ShadowDelta;
    m_ShadowBlur       := pOther.m_ShadowBlur;
    m_TextRendering    := pOther.m_TextRendering;
    m_TextTrimming     := pOther.m_TextTrimming;
    m_DrawTextFunc     := pOther.m_DrawTextFunc;

    m_pFont.Assign(pOther.m_pFont);
    m_ShadowColor.Assign(pOther.m_ShadowColor);
end;
//---------------------------------------------------------------------------
function TWRenderer.ITextOptions.GetShadowColor: PWColor;
begin
    Result := @m_ShadowColor;
end;
//---------------------------------------------------------------------------
function TWRenderer.ITextOptions.GetShadowDelta: PPoint;
begin
    Result := @m_ShadowDelta;
end;
//---------------------------------------------------------------------------
function TWRenderer.ITextOptions.GetShadowBlur: PSize;
begin
    Result := @m_ShadowBlur;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.ITextOptions.SetShadowColor(const pColor: PWColor);
begin
    if (not Assigned(pColor)) then
        raise Exception.Create('Color is undefined');

    m_ShadowColor := TWColor.Create(pColor);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.ITextOptions.SetShadowDelta(const delta: TPoint);
begin
    m_ShadowDelta := TPoint.Create(delta);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.ITextOptions.SetShadowBlur(const blur: TSize);
begin
    m_ShadowBlur := TSize.Create(blur);
end;
//---------------------------------------------------------------------------
// TWRenderer.IImageOptions
//---------------------------------------------------------------------------
constructor TWRenderer.IImageOptions.Create;
begin
    inherited Create;

    m_ResizeMode  := E_RzMode_Auto;
    m_Opacity     := 1.0;
    m_Transparent := False;
    m_Vectorial   := False;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.IImageOptions.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer
//---------------------------------------------------------------------------
constructor TWRenderer.Create;
begin
    inherited Create;

    m_DrawTextFunc := E_DF_Default;
end;
//---------------------------------------------------------------------------
destructor TWRenderer.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.GetBrushes(const pFill: TWFill; out pSolid: TWSolidBrush;
        out pLinear: TWLinearGradientBrush; out pRadial: TWRadialGradientBrush);
begin
    pSolid  := nil;
    pLinear := nil;
    pRadial := nil;

    if (not Assigned(pFill)) then
        Exit;

    case (pFill.BrushType) of
        E_BT_Solid:  pSolid  := pFill.Brush as TWSolidBrush;
        E_BT_Linear: pLinear := pFill.Brush as TWLinearGradientBrush;
        E_BT_Radial: pRadial := pFill.Brush as TWRadialGradientBrush;
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.GetBrushes(const pStroke: TWStroke; out pSolid: TWSolidBrush;
        out pLinear: TWLinearGradientBrush; out pRadial: TWRadialGradientBrush);
begin
    pSolid  := nil;
    pLinear := nil;
    pRadial := nil;

    if (not Assigned(pStroke)) then
        Exit;

    case (pStroke.BrushType) of
        E_BT_Solid:  pSolid  := pStroke.Brush as TWSolidBrush;
        E_BT_Linear: pLinear := pStroke.Brush as TWLinearGradientBrush;
        E_BT_Radial: pRadial := pStroke.Brush as TWRadialGradientBrush;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer.GetRadiusX(const radius: TPoint): Cardinal;
begin
    // get radius x if set. In case radius x isn't set but radius y is, get radius y value, because
    // only one raidus value means symmetrical radius
    if (radius.X <> 0) then
        Result := radius.X
    else
        Result := radius.Y;
end;
//---------------------------------------------------------------------------
function TWRenderer.GetRadiusY(const radius: TPoint): Cardinal;
begin
    // get radius y if set. In case radius y isn't set but radius x is, get radius x value, because
    // only one raidus value means symmetrical radius
    if (radius.Y <> 0) then
        Result := radius.Y
    else
        Result := radius.X;
end;
//---------------------------------------------------------------------------
function TWRenderer.GetSymmetricRadius(const pRadius: IRadius; index: Cardinal): Cardinal;
begin
    // search for index matching value
    case (index and 3) of
        0: Result := GetRadiusX(pRadius.m_RightTop);
        1: Result := GetRadiusX(pRadius.m_RightBottom);
        2: Result := GetRadiusX(pRadius.m_LeftBottom);
        3: Result := GetRadiusX(pRadius.m_LeftTop);
    else
        raise Exception.CreateFmt('Unknown index - %d', [index]);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.ChangeLineLength(const startPoint: TWPointF; var endPoint: TWPointF;
        const delta: Double);
var
    dx, dy, length, scale: Double;
begin
    // nothing to change?
    if (delta = 0.0) then
        Exit;

    // not a line?
    if ((startPoint.X = endPoint.X) and (startPoint.Y = endPoint.Y)) then
        Exit;

    // calculate delta x and y
    dx := endPoint.X - startPoint.X;
    dy := endPoint.Y - startPoint.Y;

    // is horizontal or vertical line?
    if (dx = 0.0) then
    begin
        // vertical line, calculate and update end point y position
        if (endPoint.Y < startPoint.Y) then
            endPoint.Y := endPoint.Y - delta
        else
            endPoint.Y := endPoint.Y + delta;
    end
    else
    if (dy = 0.0) then
    begin
        // horizontal line, calculate and update end point x position
        if (endPoint.X < startPoint.X) then
            endPoint.X := endPoint.X - delta
        else
            endPoint.X := endPoint.X + delta;
    end
    else
    begin
        // non horizontal or vertical line, calculate and update end point
        length     := Sqrt(dx * dx + dy * dy);
        scale      := (length + delta) / length;
        dx         := dx * scale;
        dy         := dy * scale;
        endPoint.X := startPoint.X + dx;
        endPoint.Y := startPoint.Y + dy;
    end;
end;
//---------------------------------------------------------------------------
class function TWRenderer.ConvertStrokeWidth(const pStroke: TWStroke): Integer;
begin
    if (pStroke.Width < 0.0) then
        Exit(Floor(pStroke.Width));

    Result := Ceil(pStroke.Width);
end;
//---------------------------------------------------------------------------
function TWRenderer.GetFont(const name: UnicodeString): THandle;
begin
    Result := TWControlFont.GetFont(name);
end;
//---------------------------------------------------------------------------
function TWRenderer.GetFontName(const fileName: UnicodeString): UnicodeString;
var
    extension: UnicodeString;
begin
    // is file name empty?
    if (Length(fileName) = 0) then
        Exit('');

    // extract file extension
    extension := LowerCase(ExtractFileExt(fileName));

    // is truetype font?
    if (extension <> '.ttf') then
        Exit('');

    // get font name
    Result := TWControlFont.GetFontNameFromFile(fileName);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.RemoveFontFromSession(const name: UnicodeString);
begin
    TWControlFont.RemoveFontFromSession(name);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.RemoveFontsFromSession;
begin
    TWControlFont.RemoveFontsFromSession;
end;
//---------------------------------------------------------------------------
procedure TWRenderer.RemoveFont(const name: UnicodeString);
begin
    TWControlFont.RemoveFont(name);
end;
//---------------------------------------------------------------------------
procedure TWRenderer.RemoveFonts;
begin
    TWControlFont.RemoveFonts;
end;
//---------------------------------------------------------------------------
function TWRenderer.GetFontSizeToFit(const text: UnicodeString; const rect: TWRectF;
        const pOptions: ITextOptions; minWrapFontSize, minFontSize: Cardinal;
        hDC: THandle; out fontSize: Cardinal): Boolean;
var
    pTextOptions:                               ITextOptions;
    refRect:                                    TWRectF;
    sizeRef, size, wordSize:                    TWSizeI;
    sizeRefF, sizeF, wordSizeF:                 TWSizeF;
    originalSize, i:                            Cardinal;
    words:                                      array of UnicodeString;
    word:                                       UnicodeString;
    characters, lines, refCharacters, refLines: Integer;
    wordStart, count, j:                        NativeInt;
    notGood:                                    Boolean;
begin
    fontSize := 0;

    // empty text has zero size
    if (Length(text) = 0) then
        Exit(True);

    // no device context?
    if (hDC = 0) then
        Exit(False);

    // no font?
    if (not Assigned(pOptions.m_pFont)) then
        Exit(False);

    // make options copy (except for font, will use the same object)
    pTextOptions := pOptions;

    // force align to left and top
    pTextOptions.m_AlignHorz := E_H_Left;
    pTextOptions.m_AlignVert := E_V_Top;

    // min font size can't be zero
    minWrapFontSize := Max(1, minWrapFontSize);
    minFontSize     := Max(1, minFontSize);

    // rect to use to measure text (must be high, otherwise GetTextSize() may not compute text height correctly)
    refRect      := TWRectF.Create(0.0, 0.0, 999.0, 999.0);
    originalSize := pTextOptions.m_pFont.Size;

    try
        // count new lines (becaues can't trust GDI+ MeasureString, it can break words in the middle)
        characters    := 0;
        lines         := 0;
        refCharacters := 0;
        refLines      := 1;

        // fit text in rectangle by lowering font size by one (can be optimized, divide and conquer)
        for i := pTextOptions.m_pFont.Size downto minFontSize do
        begin
            fontSize                  := i;
            pTextOptions.m_pFont.Size := fontSize;

            // get text size for that font size
            sizeF := GetTextSize(text, rect, pTextOptions, hDC, @characters, @lines);
            size  := TWSizeI.Create(Round(sizeF.Width), Round(sizeF.Height));

            {$ifdef M_LogTextAutoSize}
                TWLogHelper.LogToCompiler(PWideChar(UnicodeString('GetFontSizeToFit - ' + text
                        + ' - ' + IntToStr(fontSize) + ' - rect - ' + IntToStr(rect.Width) + 'x'
                        + IntToStr(rect.Height) + ' - res size - ' + IntToStr(size.Width) + 'x'
                        + IntToStr(size.Height))));
            {$endif}

            if (not pTextOptions.m_NoWrap) then
            begin
                // get reference size when not wrapped
                pTextOptions.m_NoWrap := True;
                sizeRefF              := GetTextSize(text, refRect, pTextOptions, hDC, @refCharacters, @refLines);
                sizeRef               := TWSizeI.Create(Round(sizeRefF.Width), Round(sizeRefF.Height));
                pTextOptions.m_NoWrap := pOptions.m_NoWrap;
            end
            else
                sizeRef := size;

            // can it wrap?
            if (fontSize <= minWrapFontSize) then
            begin
                // must fit rectangle with all characters
                if ((characters >= refCharacters) and (size.Width < rect.Width)
                        and (size.Height < rect.Height))
                then
                begin
                    ///* probably not needed, and causes problem with Japanese
                    // create word list from text if not already done
                    if (Length(words) = 0) then
                    begin
                        wordStart := 0;
                        count     := Length(text);

                        // explode into words
                        for j := 1 to count do
                        begin
                            case (text[j]) of
                                ' ',
                                #09,
                                #10,
                                #13,
                                // japanese space
                                #12288:
                                begin
                                    if (wordStart = j) then
                                        // skip character
                                        wordStart := j + 1
                                    else
                                    begin
                                        // new word, add it
                                        SetLength(words, Length(words) + 1);
                                        words[Length(words) - 1] := TWStringHelper.Substr(text, wordStart, j - wordStart);

                                        // skip character from next word
                                        wordStart := j + 1;
                                    end;
                                end;
                            else
                                // check if character can be wrapped (e.g. all latin characters
                                // can't be wrapped). However all japanese, chineseese can (there
                                // are rules, but this is a simplified version)
                                if (TWStringHelper.CanWrap(text[j])) then
                                begin
                                    // todo -cBug -oNiki: this code breaks the text autosize for non latin texts
                                    // skip single characters
                                    if (wordStart = j) then
                                        wordStart := j + 1
                                    else
                                    begin
                                        // new word, add it
                                        SetLength(words, Length(words) + 1);
                                        words[Length(words) - 1] := TWStringHelper.Substr(text, wordStart, j - wordStart);

                                        wordStart := j;
                                    end;
                                end;
                            end;
                        end;

                        // last word
                        if (Integer(wordStart) < Length(text)) then
                        begin
                            SetLength(words, Length(words) + 1);
                            words[Length(words) - 1] := TWStringHelper.Substr(text, wordStart, Length(text) - Integer(wordStart));
                        end;
                    end;

                    notGood               := False;
                    pTextOptions.m_NoWrap := True;

                    // measure every word width, if biggest is longer than available rect, we are
                    // not good. GDI+ can wrap words in the middle if does not have enough space to
                    // display them
                    for word in words do
                    begin
                        // FIXME niki: optimize with direct use of MeasureString to avoid setting
                        // Graphic options on every call
                        // measure word size without a break
                        wordSizeF := GetTextSize(word, refRect, pTextOptions, hDC, @refCharacters,
                                @refLines);
                        wordSize  := TWSizeI.Create(Round(wordSizeF.Width), Round(wordSizeF.Height));

                        // if word width bigger than measured size, it will be wrapped, no good
                        if (size.Width < wordSize.Width) then
                        begin
                            notGood := True;
                            break;
                        end;
                    end;

                    if (notGood) then
                    begin
                        // restore wrap setting and check next font size
                        pTextOptions.m_NoWrap := pOptions.m_NoWrap;
                        continue;
                    end;

                    // found font size small enough
                    Exit(True);
                end;
            end
            else
            // for no wrapping text, must fit rectangle with all characters and on same number of
            // lines as reference
            if ((characters >= refCharacters) and (lines <= refLines)
                    and (sizeRef.Width < rect.Width) and (sizeRef.Height < rect.Height))
            then
                Exit(True);
        end;
    finally
        // restore original font size
        pTextOptions.m_pFont.Size := originalSize;
    end;

    // can't fit, return min font size
    fontSize := minFontSize;
    Result   := False;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsFillVisible(const pFill: TWFill): Boolean;
var
    pSolidBrush:          TWSolidBrush;
    pLinearGradientBrush: TWLinearGradientBrush;
    pRadialGradientBrush: TWRadialGradientBrush;
    pStop:                TWGradientStop;
begin
    case (pFill.BrushType) of
        E_BT_Solid:
        begin
            // get the solid brush
            pSolidBrush := pFill.Brush as TWSolidBrush;

            if (not Assigned(pSolidBrush)) then
                Exit(False);

            Result := pSolidBrush.Color.GetAlpha > 0;
        end;

        E_BT_Linear:
        begin
            // get the linear gradient brush
            pLinearGradientBrush := pFill.Brush as TWLinearGradientBrush;

            if (not Assigned(pLinearGradientBrush)) then
                Exit(False);

            // iterate through brush gradient stops
            for pStop in pLinearGradientBrush.Stops do
                // is brush gradient color fully transparent?
                if (pStop.Color.GetAlpha <> 0) then
                    Exit(True);

            Exit(False);
        end;

        E_BT_Radial:
        begin
            // get the radial gradient brush
            pRadialGradientBrush := pFill.Brush as TWRadialGradientBrush;

            if (not Assigned(pRadialGradientBrush)) then
                Exit(False);

            // iterate through brush gradient stops
            for pStop in pRadialGradientBrush.Stops do
                // is brush gradient color fully transparent?
                if (pStop.Color.GetAlpha <> 0) then
                    Exit(True);

            Exit(False);
        end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pFill.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsStrokeVisible(const pStroke: TWStroke; ignoreColor: Boolean = False): Boolean;
var
    pSolidBrush:          TWSolidBrush;
    pLinearGradientBrush: TWLinearGradientBrush;
    pRadialGradientBrush: TWRadialGradientBrush;
    pStop:                TWGradientStop;
begin
    if (not Assigned(pStroke)) then
        Exit(False);

    // no stroke width?
    if (pStroke.Width = 0.0) then
        Exit(False);

    // do test only stroke width?
    if (ignoreColor) then
        Exit(True);

    case (pStroke.BrushType) of
        E_BT_Solid:
        begin
            // get the solid brush
            pSolidBrush := pStroke.Brush as TWSolidBrush;

            if (not Assigned(pSolidBrush)) then
                Exit(False);

            Result := pSolidBrush.Color.GetAlpha > 0;
        end;

        E_BT_Linear:
        begin
            // get the linear gradient brush
            pLinearGradientBrush := pStroke.Brush as TWLinearGradientBrush;

            if (not Assigned(pLinearGradientBrush)) then
                Exit(False);

            // iterate through brush gradient stops
            for pStop in pLinearGradientBrush.Stops do
                // is brush gradient color fully transparent?
                if (pStop.Color.GetAlpha <> 0) then
                    Exit(True);

            Exit(False);
        end;

        E_BT_Radial:
        begin
            // get the radial gradient brush
            pRadialGradientBrush := pStroke.Brush as TWRadialGradientBrush;

            if (not Assigned(pRadialGradientBrush)) then
                Exit(False);

            // iterate through brush gradient stops
            for pStop in pRadialGradientBrush.Stops do
                // is brush gradient color fully transparent?
                if (pStop.Color.GetAlpha <> 0) then
                    Exit(True);

            Exit(False);
        end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pStroke.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsGradientVisible(const pFill: TWFill): Boolean;
var
    pLinearGradientBrush:  TWLinearGradientBrush;
    pRadialGradientBrush:  TWRadialGradientBrush;
    brushGradientCount, i: NativeInt;
begin
    if (not Assigned(pFill)) then
        Exit(False);

    case (pFill.BrushType) of
        E_BT_Solid: Exit(False);

        E_BT_Linear:
        begin
            // get the linear gradient brush
            pLinearGradientBrush := pFill.Brush as TWLinearGradientBrush;

            if (not Assigned(pLinearGradientBrush)) then
                Exit(False);

            // get brush gradient stop count
            brushGradientCount := pLinearGradientBrush.Stops.Count;

            // iterate through brush gradient stops
            for i := 1 to brushGradientCount - 1 do
                // at least 1 color is different than others in color gradient list?
                if (pLinearGradientBrush.Stops[i].Color.Differs(pLinearGradientBrush.Stops[0].Color^)) then
                    Exit(True);

            Exit(False);
        end;

        E_BT_Radial:
        begin
            // get the radial gradient brush
            pRadialGradientBrush := pFill.Brush as TWRadialGradientBrush;

            if (not Assigned(pRadialGradientBrush)) then
                Exit(False);

            // get brush gradient stop count
            brushGradientCount := pRadialGradientBrush.Stops.Count;

            // iterate through brush gradient stops
            for i := 1 to brushGradientCount - 1 do
                // at least 1 color is different than others in color gradient list?
                if (pRadialGradientBrush.Stops[i].Color.Differs(pRadialGradientBrush.Stops[0].Color^)) then
                    Exit(True);

            Exit(False);
        end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pFill.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsGradientVisible(const pStroke: TWStroke): Boolean;
var
    pLinearGradientBrush:   TWLinearGradientBrush;
    pRadialGradientBrush:   TWRadialGradientBrush;
    strokeGradientCount, i: NativeInt;
begin
    if (not Assigned(pStroke)) then
        Exit(False);

    case (pStroke.BrushType) of
        E_BT_Solid: Exit(False);

        E_BT_Linear:
        begin
            // get the linear gradient brush
            pLinearGradientBrush := pStroke.Brush as TWLinearGradientBrush;

            if (not Assigned(pLinearGradientBrush)) then
                Exit(False);

            // get pen gradient stop count
            strokeGradientCount := pLinearGradientBrush.Stops.Count;

            // iterate through pen gradient stops
            for i := 1 to strokeGradientCount - 1 do
                // at least 1 color is different than others in color gradient list?
                if (pLinearGradientBrush.Stops[i].Color.Differs(pLinearGradientBrush.Stops[0].Color^)) then
                    Exit(True);

            Exit(False);
        end;

        E_BT_Radial:
        begin
            // get the radial gradient brush
            pRadialGradientBrush := pStroke.Brush as TWRadialGradientBrush;

            if (not Assigned(pRadialGradientBrush)) then
                Exit(False);

            // get pen gradient stop count
            strokeGradientCount := pRadialGradientBrush.Stops.Count;

            // iterate through pen gradient stops
            for i := 1 to strokeGradientCount - 1 do
                // at least 1 color is different than others in color gradient list?
                if (pRadialGradientBrush.Stops[i].Color.Differs(pRadialGradientBrush.Stops[0].Color^)) then
                    Exit(True);

            Exit(False);
        end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pStroke.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsOutlineVisible(const pStroke: TWStroke; const pOutline: IOutline;
        ignoreColor: Boolean): Boolean;
begin
    Result := (Assigned(pStroke) and IsStrokeVisible(pStroke, ignoreColor) and (pOutline.m_ShowLeft
            or pOutline.m_ShowTop or pOutline.m_ShowRight or pOutline.m_ShowBottom or pOutline.m_ShowLeftTop
            or pOutline.m_ShowLeftBottom or pOutline.m_ShowRightTop or pOutline.m_ShowRightBottom));
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsCornerVisible(const pRadius: IRadius; const pOutline: IOutline): Boolean;
begin
    Result := (((not pRadius.m_LeftTop.IsZero)     and pOutline.m_ShowLeftTop)
            or ((not pRadius.m_LeftBottom.IsZero)  and pOutline.m_ShowLeftBottom)
            or ((not pRadius.m_RightTop.IsZero)    and pOutline.m_ShowRightTop)
            or ((not pRadius.m_RightBottom.IsZero) and pOutline.m_ShowRightBottom));
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsCompletelyVisible(const pOutline: IOutline): Boolean;
begin
    Result := (pOutline.m_ShowLeft and pOutline.m_ShowTop and pOutline.m_ShowRight and pOutline.m_ShowBottom
            and pOutline.m_ShowLeftTop and pOutline.m_ShowLeftBottom and pOutline.m_ShowRightTop
            and pOutline.m_ShowRightBottom);
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsRegular(const pRadius: IRadius): Boolean;
begin
    Result := ((pRadius.m_LeftTop = pRadius.m_LeftBottom) and (pRadius.m_LeftTop = pRadius.m_RightTop)
            and (pRadius.m_LeftTop = pRadius.m_RightBottom));
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsTailVisible(const pOptions: IRectOptions): Boolean;
begin
    Result := pOptions.Tail.m_Visible;
end;
//---------------------------------------------------------------------------
class function TWRenderer.IsRectVisible(const pOptions: IRectOptions): Boolean;
begin
    Result := (IsFillVisible(pOptions.Fill) or IsOutlineVisible(pOptions.Stroke, pOptions.Outline, False));
end;
//---------------------------------------------------------------------------
class function TWRenderer.GetMaxSize(const rect: TWRectF): Single;
begin
    rect.Normalize;

    // get max size
    Result := Min(rect.Width, rect.Height) / 2.0;
end;
//---------------------------------------------------------------------------
class function TWRenderer.GetMaxSizeX(const rect: TWRectF): Single;
begin
    rect.Normalize;

    // get max size
    Result := rect.Width / 2.0;
end;
//---------------------------------------------------------------------------
class function TWRenderer.GetMaxSizeY(const rect: TWRectF): Single;
begin
    rect.Normalize;

    // get max size
    Result := rect.Height / 2.0;
end;
//---------------------------------------------------------------------------
class function TWRenderer.CalculateDrawRect(const rect: TWRectF; const pOptions: IRectOptions;
        useShifting: Boolean): TWRectF;
var
    outlinedLeft, outlinedTop, outlinedRight, outlinedBottom: Boolean;
    strokeWidth, deltaWidth, shifting:                        Integer;
begin
    // do draw outline?
    if (not IsOutlineVisible(pOptions.Stroke, pOptions.Outline, False)) then
        // no outline, rectangle remains unchanged
        Exit(rect);

    // check which edge should be outlined
    outlinedLeft   :=   pOptions.Outline.m_ShowLeft                                                    or
                  ((not pOptions.Radius.m_LeftTop.IsZero)     and pOptions.Outline.m_ShowLeftTop)      or
                  ((not pOptions.Radius.m_LeftBottom.IsZero)  and pOptions.Outline.m_ShowLeftBottom);
    outlinedTop    :=   pOptions.Outline.m_ShowTop                                                     or
                  ((not pOptions.Radius.m_LeftTop.IsZero)     and pOptions.Outline.m_ShowLeftTop)      or
                  ((not pOptions.Radius.m_RightTop.IsZero)    and pOptions.Outline.m_ShowRightTop);
    outlinedRight  :=   pOptions.Outline.m_ShowRight                                                   or
                  ((not pOptions.Radius.m_RightTop.IsZero)    and pOptions.Outline.m_ShowRightTop)     or
                  ((not pOptions.Radius.m_RightBottom.IsZero) and pOptions.Outline.m_ShowRightBottom);
    outlinedBottom :=   pOptions.Outline.m_ShowBottom                                                  or
                  ((not pOptions.Radius.m_LeftBottom.IsZero)  and pOptions.Outline.m_ShowLeftBottom)   or
                  ((not pOptions.Radius.m_RightBottom.IsZero) and pOptions.Outline.m_ShowRightBottom);

    // is outline larger than 1 pixel?
    if (pOptions.Stroke.Width > 1.0) then
    begin
        strokeWidth := ConvertStrokeWidth(pOptions.Stroke);

        // calculate outline delta value
        deltaWidth := (strokeWidth div 2);

        // calculate outline delta shifting
        if ((not useShifting) or ((strokeWidth mod 2) <> 0)) then
            shifting := 0
        else
            shifting := 1;

        // calculate draw rectangle left corner considering outline width
        if (outlinedLeft) then
            Result.Left := rect.Left + deltaWidth
        else
            Result.Left := rect.Left;

        // calculate draw rectangle top corner considering outline width
        if (outlinedTop) then
            Result.Top := rect.Top + deltaWidth
        else
            Result.Top := rect.Top;

        // calculate draw rectangle right corner considering outline width
        if (outlinedRight) then
            Result.Right := Result.Left + rect.Width - strokeWidth - shifting
        else
            Result.Right := rect.Right;

        // calculate draw rectangle bottom corner considering outline width
        if (outlinedBottom) then
            Result.Bottom := Result.Top + rect.Height - strokeWidth - shifting
        else
            Result.Bottom := rect.Bottom;
    end
    else
    begin
        // calculate draw rectangle considering outline width
        Result := rect;

        if (outlinedRight) then
            Result.Right := Result.Right - pOptions.Stroke.Width;

        if (outlinedBottom) then
            Result.Bottom := Result.Bottom - pOptions.Stroke.Width;
    end;
end;
//---------------------------------------------------------------------------

end.
