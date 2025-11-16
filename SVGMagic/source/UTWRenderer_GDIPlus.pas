{**
 @abstract(@name provides a renderer using the GDI+ to perform the drawing.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWRenderer_GDIPlus;

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

    // uncomment line below to enable cache logging (never in release)
    {$ifdef WTCONTROLS_DEBUG}
        {$define ENABLE_GDIPLUS_CACHE_LOGGING}
    {$endif}

    // enable or disable this to draw round rectangle using Bezier curves or using AddArc() function
    {$define DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}

uses System.Classes,
     System.TypInfo,
     System.Rtti,
     System.SysUtils,
     System.Math,
     System.UITypes,
     System.Generics.Defaults,
     System.Generics.Collections,
     {$if CompilerVersion >= 29}
         System.Hash,
     {$ifend}
     Vcl.Graphics,
     Winapi.GDIPAPI,
     Winapi.GDIPOBJ,
     Winapi.Windows,
     UTWCacheHit,
     UTWSmartPointer,
     UTWHelpers,
     UTWColor,
     UTWFillAndStroke,
     UTWPoint,
     UTWSize,
     UTWRect,
     UTWPadding,
     UTWVector,
     UTWMatrix,
     UTWGraphicPath,
     UTWGDIPlusGradient,
     UTWRendererCommon,
     UTWBlur,
     UTWRenderer,
     UTWRenderer_GDI;

const
    {**
     Missing (and uncommented in official documentation) GDI+ flag
    }
    StringFormatFlagsBypassGDI: NativeUInt = $80000000;

    {**
     GDI+ Bezier curve ratio
    }
    Bezier_Curve_Ratio: Single = 2.234;

    {**
     Extra width pixels for text size
    }
    TextSizeExtraWidthPixels = 0;

    {**
     Extra height pixels for text size
    }
    TextSizeExtraHeightPixels = 0;

    {**
     Default interpolation mode to use
    }
    DefaultInterpolationMode = InterpolationModeBicubic;

type
    {**
     Controller that can enable or disable some specific GDI+ cache
    }
    TWGDIPlusCacheController = packed record
        m_Brushes:      Boolean;
        m_Pens:         Boolean;
        m_Fonts:        Boolean;
        m_Graphics:     Boolean;
        m_StringFormat: Boolean;
    end;

    {**
     GDI+ specialized renderer
    }
    TWRenderer_GDIPlus = class(TWRenderer)
        private type
            {**
             Cached brush, it's the key used to cache a GDI+ brush
            }
            ICachedBrush = class
                private
                    m_pFill: TWFill;

                public
                    {**
                     Constructor
                     @param(pFill Fill to use to create the brush)
                    }
                    constructor Create(const pFill: TWFill); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Cached brush comparer, required to use the brush content as key in cache dictionary
            }
            ICachedBrushKeyComparer = class(TEqualityComparer<ICachedBrush>)
                public
                    {**
                     Check if brushes content are equals
                     @param(pLeft Left brush to compare)
                     @param(pRight Right brush to compare with)
                     @returns(@true if brushes are equals, otherwise @false)
                    }
                    function Equals(const pLeft, pRight: ICachedBrush): Boolean; override;

                    {**
                     Get hash code based on a brush content
                     @param(pValue Brush on which hash code should be generated)
                     @returns(The hash code)
                    }
                    function GetHashCode(const pValue: ICachedBrush): Integer; override;
            end;

            {**
             Cached brushes dictionary
            }
            IBrushCache = TObjectDictionary<ICachedBrush, TGpBrush>;

            {**
             "first in/first out" brush list
            }
            IFIFOBrush = TList<ICachedBrush>;

            {**
            * Cached pen, it's the key used to cache a GDI+ pen
            }
            ICachedPen = class
                private
                    m_pStroke:      TWStroke;
                    m_BaseBrushKey: NativeUInt;

                public
                    {**
                     Constructor
                     @param(pStroke Stroke to use to create the pen)
                    }
                    constructor Create(const pStroke: TWStroke); overload; virtual;

                    {**
                     Constructor
                     @param(pStroke Stroke to use to create the pen)
                     @param(pBaseBrush Base brush used to build the pen)
                    }
                    constructor Create(const pStroke: TWStroke; pBaseBrush: TGpBrush); overload; virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Cached pen comparer, required to use the pen content as key in cache dictionary
            }
            ICachedPenKeyComparer = class(TEqualityComparer<ICachedPen>)
                public
                    {**
                     Check if pens content are equals
                     @param(pLeft Left pen to compare)
                     @param(pRight Right pen to compare with)
                     @returns(@true if pens are equals, otherwise @false)
                    }
                    function Equals(const pLeft, pRight: ICachedPen): Boolean; override;

                    {**
                     Get hash code based on a brush content
                     @param(pValue Brush on which hash code should be generated)
                     @returns(The hash code)
                    }
                    function GetHashCode(const pValue: ICachedPen): Integer; override;
            end;

            {**
             Cached pens dictionary
            }
            IPenCache = TObjectDictionary<ICachedPen, TGpPen>;

            {**
             "first in/first out" pen list
            }
            IFIFOPen = TList<ICachedPen>;

            {**
            * Cached font, it's the key used to cache a GDI+ font
            }
            ICachedFont = class
                private
                    m_Name:          TFontName;
                    m_Charset:       TFontCharset;
                    m_Color:         TWColor;
                    m_Height:        Integer;
                    m_Style:         Integer;
                    m_Orientation:   Integer;
                    m_PixelsPerInch: Integer;
                    m_Quality:       TFontQuality;
                    m_Pitch:         TFontPitch;
                    m_hDC:           THandle;

                public
                    {**
                     Constructor
                     @param(pFont GDI font on which GDI+ font was built)
                     @param(hDC Device context used to create GDI+ font)
                    }
                    constructor Create(const pFont: TFont; hDC: THandle); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;
            end;

            {**
             Cached font comparer, required to use the font content as key in cache dictionary
            }
            ICachedFontKeyComparer = class(TEqualityComparer<ICachedFont>)
                public
                    {**
                     Check if fonts content are equals
                     @param(pLeft Left font to compare)
                     @param(pRight Right font to compare with)
                     @returns(@true if fonts are equals, otherwise @false)
                    }
                    function Equals(const pLeft, pRight: ICachedFont): Boolean; override;

                    {**
                     Get hash code based on a font content
                     @param(pValue Font on which hash code should be generated)
                     @returns(The hash code)
                    }
                    function GetHashCode(const pValue: ICachedFont): Integer; override;
            end;

            {**
             Cached fonts
            }
            IFontCache = TObjectDictionary<ICachedFont, TGpFont>;

            {**
             "first in/first out" font list
            }
            IFIFOFont = TList<ICachedFont>;

            {**
             Cached GDI+ graphics object
            }
            ICachedGraphics = class
                private
                    m_pGraphics:      TGpGraphics;
                    m_hDC:            THandle;
                    m_pGraphicsCount: TWCacheHit;

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

                    {**
                     Get cached GDI+ graphics object
                     @param(hDC Device context used by GDI+ graphics object)
                    }
                    function GetGraphics(hDC: THandle): TGpGraphics; virtual;
            end;

            ICustomFontList = TStringList;

            {**
             Cached data
             @br @bold(NOTE) WARNING, cached objects are not locked while they are in use. This means
                             that a cached object can be deleted even if still used, if it's the oldest
                             cached object, and if the cache needs to create a new object while it's
                             full. For this reason, the caching limit values should be changed carefully
            }
            ICache = class
                private
                    m_pBrushes:         IBrushCache;
                    m_pPens:            IPenCache;
                    m_pFonts:           IFontCache;
                    m_pGraphics:        ICachedGraphics;
                    m_pFIFOBrushList:   IFIFOBrush;
                    m_MaxCachedBrushes: NativeUInt;
                    m_pFIFOPenList:     IFIFOPen;
                    m_MaxCachedPens:    NativeUInt;
                    m_pFIFOFontList:    IFIFOFont;
                    m_MaxCachedFonts:   NativeUInt;
                    m_pCustomFontList:  ICustomFontList;
                    m_pFontCollection:  TGpPrivateFontCollection;
                    m_pStringFormat:    TGpStringFormat;
                    m_pBrushesCount:    TWCacheHit;
                    m_pPensCount:       TWCacheHit;
                    m_pFontsCount:      TWCacheHit;
                    m_Locked:           Boolean;

                    {**
                     Configure pen
                     @param(pStroke Stroke to use to build the pen)
                     @param(pPen Pen to configure)
                    }
                    procedure ConfigurePen(const pStroke: TWStroke; pPen: TGpPen);

                    {**
                     Get linejoin to use
                     @param(pStroke Stroke to use to build the pen)
                     @param(defValue Default value)
                     @returns(Linejoin to use)
                     @raises(Exception if pen linejoin is unknown)
                    }
                    function GetLineJoin(const pStroke: TWStroke; defValue: TLineJoin): TLineJoin;

                    {**
                     Get linecap to use from pen options
                     @param(pStroke Stroke to use to build the pen)
                     @param(defValue Default value)
                     @returns(Linecap to use)
                     @raises(Exception if pen linecap is unknown)
                    }
                    function GetLineCap(const pStroke: TWStroke; defValue: TLineCap): TLineCap;

                    {**
                     Get dashcap to use from pen options
                     @param(pStroke Stroke to use to build the pen)
                     @param(defValue Default value)
                     @returns(Dashcap to use)
                     @raises(Exception if pen dashcap is unknown)
                    }
                    function GetDashCap(const pStroke: TWStroke; defValue: TDashCap): TDashCap;

                    {**
                     Create GDI+ font
                     @param(pFont GDI font on which GDI+ font was built)
                     @param(hDC Device context used to create GDI+ font)
                    }
                    function CreateFont(pFont: TFont; hDC: THandle): TGpFont;

                    {**
                     Delete oldest objects from cache, until number of cached objects not exceeds
                     the max limit
                     @param(pCachedObjs @bold([in, out]) Cached objects containing objects to delete)
                     @param(pCachedObjList @bold([in, out]) List containing cached objects, sorted
                                                            by first to last created)
                     @param(maxLimit Max cached object count limit)
                    }
                    procedure DeleteOldestObjsFromCache<T, U>(var pCachedObjs: TObjectDictionary<T, U>;
                            var pCachedObjList: TList<T>; maxLimit: NativeUInt);

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

                    {**
                     Get brush from cache, add it if not found
                     @param(pFill Fill matching with the brush to find)
                     @param(pGradientFactory The gradient factory used to generate the brush)
                     @returns(GDI+ brush)
                    }
                    function GetBrush(const pFill: TWFill; pGradientFactory: TWGDIPlusGradient): TGpBrush; virtual;

                    {**
                     Get pen from cache, add it if not found
                     @param(pStroke Stroke matching with the pen to find)
                     @param(pradientFactory The gradient factory used to generate the pen)
                     @returns(GDI+ pen)
                    }
                    function GetPen(const pStroke: TWStroke; pGradientFactory: TWGDIPlusGradient): TGpPen; overload; virtual;

                    {**
                     Get pen from cache, add it if not found
                     @param(pStroke Stroke matching with the pen to find)
                     @param(pBaseBrush Base brush used to build the pen)
                     @returns(GDI+ pen)
                    }
                    function GetPen(const pStroke: TWStroke; pBaseBrush: TGpBrush): TGpPen; overload; virtual;

                    {**
                     Get font from cache, add it if not found
                     @param(pFont GDI font on which GDI+ font was built)
                     @param(hDC Device context used to create GDI+ font)
                     @returns(GDI+ font)
                    }
                    function GetFont(pFont: TFont; hDC: THandle): TGpFont; virtual;

                    {**
                     Get string format from cache, add it if not found
                     @returns(GDI+ string format)
                    }
                    function GetStringFormat: TGpStringFormat; virtual;

                    {**
                     Stop cache auto-cleaning
                     @br @bold(NOTE) Cache should be cleaned and auto-cleaning should be reenabled
                                     after critical code block is executed. This can be done by
                                     calling the Unlock() function
                     @br @bold(NOTE) Don't confuse, Lock() and Unlock() functions stop and start
                                     again the cache auto-cleaning, nothing else. These functions
                                     are unrelated to threding
                    }
                    procedure Lock; inline;

                    {**
                     Start cache auto-cleaning again, check if the cache capacity is exceeded, clear
                     it if yes
                     @br @bold(NOTE) Don't confuse, Lock() and Unlock() functions stop and start
                                     again the cache auto-cleaning, nothing else. These functions
                                     are unrelated to threding
                    }
                    procedure Unlock; virtual;
            end;

        protected type
            {**
             Padding measure type enumeration
             @value(IE_All - Do measure the padding on all edges)
             @value(IE_Left - Do measure the padding on the left)
             @value(IE_Top - Do measure the padding on the top)
             @value(IE_Right - Do measure the padding on the right)
             @value(IE_Bottom - Do measure the padding on the bottom)
            }
            // FIXME set already do the job so delete the assignation
            IEPaddingMeasureTypes =
            (
                IE_All    = $00,
                IE_Left   = $01,
                IE_Top    = $02,
                IE_Right  = $04,
                IE_Bottom = $08
            );

            IEPaddingMeasureType = set of IEPaddingMeasureTypes;
            IStopOffsets         = array of Single;

            PPGpFont         = ^TGpFont;
            PPGpBrush        = ^TGpBrush;
            PPGpStringFormat = ^TGpStringFormat;

        public type
            IGDIPlusPointList = array of TGpPointF;

        private
            {$if CompilerVersion >= 34}
                m_GDIPlusToken:   ULONG_PTR;
            {$else}
                m_GDIPlusToken:   Cardinal;
            {$ifend}
            m_GDIPlusInitialized: Boolean;
            m_pRenderer_GDI:      TWRenderer_GDI;
            m_pCache:             ICache;

            {**
             Initialize library
            }
            function Initialize: Boolean;

            {**
             Release library
            }
            procedure Release;

            {**
             Apply the clip region to a radial gradient
             @param(pFill Fill containing the radial gradient)
             @param(radius Radial gradient radius)
             @param(gradientFactory Gradient factory)
             @param(pGraphics Graphics on which the clip region will take place)
            }
            procedure ApplyRadialGradientClipRegion(const pFill: TWFill; const radius: TWSizeF;
                    const pGradientFactory: TWGDIPlusGradient; pGraphics: TGpGraphics);

            {**
             Calculate and apply the next radius and center point to apply to radial gradient
             @param(pFill Fill containing the radial gradient)
             @param(scaleFactor Scale factor to apply to next gradient)
             @param(center Radial gradient center point)
             @param(delta Delta between the radial gradient focus and center point)
             @param(radius @bold([out]) Radial gradient radius)
             @param(pGradientFactory Gradient factory)
            }
            procedure CalculateRadialGradientNextRadiusAndCenterPoint(const pFill: TWFill;
                    scaleFactor: NativeUInt; const center, delta: TWPointF; out radius: TWSizeF;
                    pGradientFactory: TWGDIPlusGradient);

            {**
             Get the next radial gradient brush to apply
             @param(pFill Fill containing the radial gradient)
             @param(radius Radial gradient radius)
             @param(pGradientFactory Gradient factory)
            }
            function GetRadialGradientWrapBrush(const pFill: TWFill; const radius: TWSizeF;
                    const pGradientFactory: TWGDIPlusGradient): TGpBrush;

            {**
             Update radial gradient stops
             @param(offsets Stop offsets)
             @param(scaleFactor Scale factor to apply to next gradient)
             @param(stopCount Radial gradient stop count)
             @param(swapColors If @true, stop colors will also be swapped)
             @param(pGradientFactory Gradient factory)
             @br @bold(NOTE) The returned gradient stop is only a reference to an existing gradient
                             stop, don't delete it from outside
            }
            function UpdateRadialGradientStops(const offsets: IStopOffsets;
                    scaleFactor, stopCount: NativeUInt; swapColors: Boolean;
                    const pGradientFactory: TWGDIPlusGradient): TWGDIPlusGradient.IStop;

        protected
            {**
             Convert generic point list to GDI+ point list
             @param(pointList Generic point list to convert)
             @param(pGdiPlusPointList @bold([out]) GDI+ point list to populate)
             @param(boundingBox @bold([out]) The polygon bounding box)
            }
            procedure ToGDIPlusPointList(const pointList: TWRenderer.IPointList;
                    out gdiPlusPointList: IGDIPlusPointList; out boundingBox: TWRectF); virtual;

            {**
             Create and configure text draw font, brush and string format and gdiplus graphic object
             @param(hDC Device context to draw on)
             @param(pOptions Text options)
             @param(pGraphics @bold([out]) GDI+ graphic object to configure)
             @param(ppGdiPlusFont @bold([out]) Poitner to receive newly created font object, if @nil will be ignored)
             @param(ppBrush @bold([out]) Poitner to receive newly created brush object, if @nil will be ignored)
             @param(ppFormat @bold([out]) Poitner to receive newly created format object, if @nil will be ignored)
            }
            // FIXME Needs to be specifically and strongly tested
            procedure ConfigTextDraw(hDC: THandle; const pOptions: TWRenderer.ITextOptions;
                    pGraphics: TGpGraphics; ppGdiPlusFont: PPGpFont; ppBrush: PPGpBrush;
                    ppFormat: PPGpStringFormat); virtual;

            {**
             Measure text padding between desired draw rect and text really drawn by GDI+
             @param(text Text to measure)
             @param(rect Desired draw rect)
             @param(pOptions Text options)
             @param(measureType Measure type)
             @param(pFont GDI+ font used to draw text)
             @param(pFormat GDI+ string format used to draw text)
             @returns(Padding between desired draw rect and text really drawn by GDI+)
            }
            function MeasureTextPadding(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; measureType: IEPaddingMeasureType;
                    pFont: TGpFont; pFormat: TGpStringFormat): TWPaddingI; virtual;

            {**
             Convert black&white mask to RGBA bitmap by applying color
             @param(color Final color)
             @param(pMask Mask to convert)
             @param(pResult Converted RGBA bitmap)
             @returns(@true on success, otherwise @false)
            }
            function ConvertMaskToRGBA(const color: TWColor; pMask, pResult: Vcl.Graphics.TBitmap): Boolean; virtual;

            {**
             Create cartoon bubble
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pPath Graphics path to populate)
             @param(clientRect @bold([out]) Internal frame client rectangle)
             @returns(@true on success, otherwise @false)
            }
            function CreateCartoonBubble(const rect: TWRectF; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath;
                    out clientRect: TRect): Boolean; virtual;

            {**
             Create rounded rectangle
             @param(rect Rectangle area)
             @param(pRadius Corner radius)
             @param(pPath Graphic path to create to)
             @returns(@true on success, otherwise @false)
            }
            function CreateRoundedRect(const rect: TWRectF; const pRadius: TWRenderer.IRadius;
                    pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Create rounded rectangle using Darren Session solution (from CodeProjects)
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(reset If @true, path content will be reseted before drawing new)
             @param(pPath Graphic path to create to)
             @returns(@true on success, otherwise @false)
            }
            function CreateRoundedRectDS(const rect: TWRectF; const pRadius: TWRenderer.IRadius;
                    reset: Boolean; pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Draw rounded rectangle using Darren Session solution (from CodeProjects)
             @param(rect Rectangle area)
             @param(pOptions Rectangle options)
             @param(pPen Pen to use to outline rect)
             @param(pMatrix Transformation matrix)
             @param(pGraphics GDI+ graphics to draw to)
             @param(iRect @bold([out]) Internal rectangle (i.e. rectangle inside the outline))
             @returns(@true on success, otherwise @false)
            }
            function DrawRoundedRect(const rect: TWRectF; const pOptions: TWRenderer.IRectOptions;
                    pPen: TGpPen; const pMatrix: TGpMatrix; pGraphics: TGpGraphics;
                    out iRect: TRect): Boolean; virtual;

            {**
             Fill rounded rectangle using Darren Session solution (from CodeProjects)
             @param(rect Rectangle area)
             @param(pOptions Rectangle options)
             @param(pBrush Brush to use to fill rect)
             @param(pPen Pen to use to outline rect)
             @param(pMatrix Transformation matrix)
             @param(pGraphics GDI+ graphics to draw to)
             @param(iRect @bold([out]) Internal rectangle (i.e. rectangle inside the outline))
             @returns(@true on success, otherwise @false)
            }
            function FillRoundedRect(const rect: TWRectF; const pOptions: TWRenderer.IRectOptions;
                    pBrush: TGpBrush; pPen: TGpPen; const pMatrix: TGpMatrix; pGraphics: TGpGraphics;
                    out iRect: TRect): Boolean; virtual;

            {**
             Draw cartoon bubble
             @param(rect Rectangle area to draw)
             @param(pOptions Cartoon bubble paint options)
             @param(hDC Device context to draw on)
             @param(iRect @bold([out]) Internal rectangle (i.e. rectangle inside the outline))
             @returns(@true on success, otherwise @false)
            }
            function DrawCartoonBubble(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
                    hDC: THandle; out iRect: TRect): Boolean; virtual;

            {**
             Draw left top rounded corner
             @param(pPen Corner drawing pen)
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function DrawLeftTopCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw right top rounded corner
             @param(pPen Corner drawing pen)
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function DrawRightTopCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw left bottom rounded corner
             @param(pPen Corner drawing pen)
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function DrawLeftBottomCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw right bottom rounded corner
             @param(pPen Corner drawing pen)
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function DrawRightBottomCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Add left top rounded corner to graphical path
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pPath Graphic path to add to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function AddLeftTopCornerToPath(const rect: TWRectF; const radius: TPoint;
                    pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add right top rounded corner to graphical path
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pPath Graphic path to add to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function AddRightTopCornerToPath(const rect: TWRectF; const radius: TPoint;
                    pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add left bottom rounded corner to graphical path
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pPath Graphic path to add to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function AddLeftBottomCornerToPath(const rect: TWRectF; const radius: TPoint;
                    pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add right bottom rounded corner to graphical path
             @param(rect Rectangle area)
             @param(radius Corner radius)
             @param(pPath Graphic path to add to)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Round corner will be drawn clockwise
            }
            function AddRightBottomCornerToPath(const rect: TWRectF; const radius: TPoint;
                    pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Draw left tail
             @param(pPen Tail drawing pen)
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
            }
            function DrawLeftTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw top tail
             @param(pPen Tail drawing pen)
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
            }
            function DrawTopTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw right tail
             @param(pPen Tail drawing pen)
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
            }
            function DrawRightTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw bottom tail
             @param(pPen Tail drawing pen)
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pGraphics Graphics to draw to)
             @returns(@true on success, otherwise @false)
            }
            function DrawBottomTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Add left tail to path
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pPath Graphics path to populate)
             @returns(@true on success, otherwise @false)
            }
            function AddLeftTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add top tail to path
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pPath Graphics path to populate)
             @returns(@true on success, otherwise @false)
            }
            function AddTopTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add right tail to path
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pPath Graphics path to populate)
             @returns(@true on success, otherwise @false)
            }
            function AddRightTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Add bottom tail to path
             @param(rect Cartoon bubble rectangle area)
             @param(pTail Cartoon bubble tail properties)
             @param(pRadius Border radius)
             @param(pPath Graphics path to populate)
             @returns(@true on success, otherwise @false)
            }
            function AddBottomTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
                    const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean; virtual;

            {**
             Draw transparent round rect without outline correctly, as GDI+ is unable to do that
             @param(hDC Device context to draw to)
             @param(rect Rect)
             @param(pOptions Rectangle options)
             @param(fillWithOutline If @true, the rect should be filled with the outline pen)
             @param(pPath Path containing geometry to draw)
            }
            procedure DrawTransparentRoundRectWithoutOutline(hDC: THandle; const rect: TWRectF;
                    const pOptions: TWRenderer.IRectOptions; fillWithOutline: Boolean;
                    pPath: TGpGraphicsPath); virtual;

            {**
             Draw outlined round rect without antialiasing correctly, as GDI+ is unable to do that
             @param(hDC Device context to draw to)
             @param(rect Rect)
             @param(pOptions Rectangle paint options)
             @param(iRect @bold([out]) Internal rect (inside outline))
            }
            procedure DrawNonAntialiasedSymmetricalRoundRect(hDC: THandle; const rect: TWRectF;
                    const pOptions: TWRenderer.IRectOptions; out iRect: TRect); virtual;

            {**
             Outline a rect
             @param(pOptions Rect options)
             @param(drawRect Rect to outline)
             @param(maxSize Max radius size)
             @param(pGraphics Graphics on which the rect will be outlined)
            }
            procedure OutlineRect(pOptions: TWRenderer.IRectOptions; const drawRect: TWRectF;
                    const maxSize: TWSizeF; pGraphics: TGpGraphics); overload; virtual;

            {**
             Outline a rect
             @param(pOptions Rect options)
             @param(drawRect Rect to outline)
             @param(maxSize Max radius size)
             @param(pPen Pen to use to outline the rect)
             @param(pGraphics Graphics on which the rect will be outlined)
            }
            procedure OutlineRect(pOptions: TWRenderer.IRectOptions; const drawRect: TWRectF;
                    const maxSize: TWSizeF; pPen: TGpPen; pGraphics: TGpGraphics); overload; virtual;

        public
            {**
             Constructor
            }
            constructor Create; override;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Get GDI+ token
             @returns(Token, 0 if GDI+ is not initialized or on error)
             @br @bold(NOTE) GDI+ will be initialized if needed
            }
            function GetToken: ULONG_PTR; virtual;

            {**
             Link GDI renderer
             @param(pRenderer Renderer to link)
            }
            procedure LinkGDIRenderer(pRenderer: TWRenderer_GDI); virtual;

            {**
             Begin a scene
             @param(hDC Device context to draw on)
            }
            procedure BeginScene(hDC: THandle); override;

            {**
             End a scene
            }
            procedure EndScene; override;

            {**
             Draw rectangle
             @param(rect Rectangle area to draw)
             @param(pOptions Rectangle paint options)
             @param(hDC Device context handle)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) WARNING internal rectangle isn't transformed by matrix
            }
            function DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions; hDC: THandle;
                    out iRect: TRect): Boolean; overload; override;

            {**
             Draw rectangle
             @param(rect Rectangle area to draw)
             @param(pOptions Rectangle paint options)
             @param(pGraphics Gdi+ graphics to draw to)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) WARNING internal rectangle isn't transformed by matrix
            }
            function DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions; pGraphics: TGpGraphics;
                    out iRect: TRect): Boolean; reintroduce; overload; virtual;

            {**
             Draw polygon
             @param(rect Rectangle area to draw)
             @param(pointList Point list to draw)
             @param(pOptions Polygon paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean; overload; override;

            {**
             Draw path
             @param(rect Rectangle in which path should be drawn)
             @param(path Path to draw)
             @param(pOptions Path paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawPath(const rect: TWRectF; const path: TWPathCmds;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean; overload; override;

            {**
             Get text size
             @param(text Text to measure)
             @param(rect Text bounding rectangle)
             @param(pOptions text options)
             @param(hDC Device context handle)
             @param(pCharacters @bold([out]) Pointer to variable to receive the number of characters
                                             that will be displayed in rect, if @nil will be ingored)
             @param(pLines @bold([out]) Pointer to variable to receive the number of lines that will
                                        be displayed in rect, if @nil will be ingored)
             @returns(Text size, empty size on error)
            }
            function GetTextSize(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
                    pLines: PInteger = nil): TWSizeF; override;

            {**
             Draw text
             @param(text Text to draw)
             @param(rect Rectangle area containing text)
             @param(pOptions Text GDI+ options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawText(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean; override;

            {**
             Draw text on layered form
             @param(text Text to draw)
             @param(rect Rectangle area containing text)
             @param(pOptions Text GDI+ options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            function DrawLayeredText(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean; override;

            {**
             Draw image
             @param(pGraphic Image to draw)
             @param(srcRect Source image rect to draw)
             @param(hDC Destination device context handle)
             @param(destRect Rect where image will be drawn on dest, stretched if not equal to source)
             @param(pOptions Options)
             @br @bold(NOTE) WARNING Not tested with transparent images
            }
            procedure DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
                    const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions); overload; override;

            {**
             Draw image
             @param(pGraphic Image to draw)
             @param(srcRect Source image rect to draw)
             @param(pGraphics Destination GDI+ graphics to draw to)
             @param(destRect Rect where image will be drawn on dest, stretched if not equal to source)
             @param(pOptions Options)
             @br @bold(NOTE) WARNING Not tested with transparent images
            }
            procedure DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; pGraphics: TGpGraphics;
                    const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions); reintroduce; overload; virtual;

            {**
             Get supported draw capabilities
             @returns(Supported capabilities)
            }
            function GetCaps: TWRenderer.IDrawCaps; override;

            {**
             Add new custom font from file and set it available for applications opened in Windows session
             @param(name Font name to add)
             @param(fileName Font file name)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Thus added font will be available for ALL applications until the Windows
                             session is closed or the font is removed using RemoveFont or RemoveFonts.
                             But be careful, this is true ONLY for GDI fonts, and NOT for GDI+
            }
            function AddFontToSession(const name, fileName: UnicodeString): Boolean; override;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(fileName Font file name)
             @returns(Font handle, 0 on error)
            }
            function AddFont(const name, fileName: UnicodeString): THandle; overload; override;

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
            function AddFont(const name: UnicodeString; pStream: TStream;
                    fontLength: Cardinal): THandle; overload; override;

            {**
             Fill a rectangle. It's a wrapper around the GDI+ FillRectangle() that supports the radial wrapping
             @param(rect Rectangle to fill)
             @param(pFill Fill options)
             @param(pGraphics GDI+ graphics on which the rectangle will be filled)
             @returns(@true on success, otherwise @false)
            }
            function FillRectangle(const rect: TWRectF; const pFill: TWFill;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Draw a rectangle. It's a wrapper around the GDI+ DrawRectangle() that supports the radial wrapping
             @param(rect Rectangle to draw)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the rectangle will be filled)
             @returns(@true on success, otherwise @false)
            }
            function DrawRectangle(const rect: TWRectF; const pStroke: TWStroke;
                    pGraphics: TGpGraphics): Boolean; virtual;

            {**
             Fill a path. It's a wrapper around the GDI+ FillPath() that supports the radial wrapping
             @param(pPath Path to fill)
             @param(pFill Fill options)
             @param(pGraphics GDI+ graphics on which the path will be filled)
             @param(rect Rect surrounding the path. If empty, the path bounding rect will be used)
             @returns(@true on success, otherwise @false)
            }
            function FillPath(pPath: TGpGraphicsPath; const pFill: TWFill; pGraphics: TGpGraphics;
                    const rect: TWRectF): Boolean; virtual;

            {**
             Draw a path. It's a wrapper around the GDI+ DrawPath() that supports the radial wrapping
             @param(pPath Path to fill)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the path will be drawn)
             @param(rect Rect surrounding the path. If empty, the path bounding rect will be used)
             @returns(@true on success, otherwise @false)
            }
            function DrawPath(pPath: TGpGraphicsPath; const pStroke: TWStroke; pGraphics: TGpGraphics;
                    const rect: TWRectF): Boolean; reintroduce; overload;

            {**
             Fill an ellipse. It's a wrapper around the GDI+ FillEllipse() that supports the radial wrapping
             @param(x Ellipse center position on the x axis)
             @param(y Ellipse center position on the y axis)
             @param(width Ellipse width)
             @param(height Ellipse height)
             @param(pFill Fill options)
             @param(pGraphics GDI+ graphics on which the ellipse will be filled)
             @param(rect Rect surrounding the ellipse. If empty, the ellipse bounding rect will be used)
             @returns(@true on success, otherwise @false)
            }
            function FillEllipse(x, y, width, height: Single; const pFill: TWFill; pGraphics: TGpGraphics;
                    const rect: TWRectF): Boolean; virtual;

            {**
             Draw an ellipse. It's a wrapper around the GDI+ DrawEllipse() that supports the radial wrapping
             @param(x Ellipse center position on the x axis)
             @param(y Ellipse center position on the y axis)
             @param(width Ellipse width)
             @param(height Ellipse height)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the ellipse will be drawn)
             @param(rect Rect surrounding the ellipse. If empty, the ellipse bounding rect will be used)
             @returns(@true on success, otherwise @false)
            }
            function DrawEllipse(x, y, width, height: Single; const pStroke: TWStroke;
                    pGraphics: TGpGraphics; const rect: TWRectF): Boolean; virtual;

            {**
             Fill a polygon. It's a wrapper around the GDI+ FillPolygon() that supports the radial wrapping
             @param(points Point list composing the polygon)
             @param(pFill Fill options)
             @param(pGraphics GDI+ graphics on which the polygon will be filled)
             @param(rect Rect surrounding the polygon)
             @returns(@true on success, otherwise @false)
            }
            function FillPolygon(const points: IGDIPlusPointList; const pFill: TWFill;
                    pGraphics: TGpGraphics; const rect: TWRectF): Boolean; virtual;

            {**
             Draw a polygon. It's a wrapper around the GDI+ DrawPolygon() that supports the radial wrapping
             @param(points Point list composing the polygon)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the polygon will be drawn)
             @param(rect Rect surrounding the polygon)
             @returns(@true on success, otherwise @false)
            }
            function DrawPolygon(const points: IGDIPlusPointList; const pStroke: TWStroke;
                    pGraphics: TGpGraphics; const rect: TWRectF): Boolean; reintroduce; overload;

            {**
             Draw a line. It's a wrapper around the GDI+ DrawLine() that supports the radial wrapping
             @param(x1 Line start position on the x axis)
             @param(y1 Line start position on the y axis)
             @param(x2 Line end position on the x axis)
             @param(y2 Line end position on the y axis)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the line will be drawn)
             @param(rect Rect surrounding the line. If empty, the line bounding rect will be used)
             @returns(@true on success, otherwise @false)
            }
            function DrawLine(x1, y1, x2, y2: Single; const pStroke: TWStroke; pGraphics: TGpGraphics;
                    const rect: TWRectF): Boolean; virtual;

            {**
             Draw lines. It's a wrapper around the GDI+ DrawLines() that supports the radial wrapping
             @param(points Point list composing the lines)
             @param(pStroke Stroke options)
             @param(pGraphics GDI+ graphics on which the lines will be drawn)
             @param(rect Rect surrounding the lines)
             @returns(@true on success, otherwise @false)
            }
            function DrawLines(const points: IGDIPlusPointList; const pStroke: TWStroke;
                    pGraphics: TGpGraphics; const rect: TWRectF): Boolean; virtual;

            {**
             Draw a string. It's a wrapper around the GDI+ DrawString() that supports the radial wrapping
             @param(text Text to draw)
             @param(textPos Text position)
             @param(pFont Text font)
             @param(pFill Fill options)
             @param(pGraphics GDI+ graphics on which the text will be drawn)
             @param(rect Rect surrounding the text)
             @returns(@true on success, otherwise @false)
            }
            function DrawString(const text: UnicodeString; const textPos: TGpPointF; pFont: TGpFont;
                    const pFill: TWFill; pGraphics: TGpGraphics; const rect: TWRectF): Boolean; virtual;

            {**
             Get brush from cache, add it if not found
             @param(pFill Fill matching with the brush to find)
             @param(gradientFactory @bold([in, out]) The gradient factory used to generate the brush)
             @returns(GDI+ brush)
            }
            function GetBrush(const pFill: TWFill;
                    const pGradientFactory: TWGDIPlusGradient): TGpBrush; overload; virtual;

            {**
             Get solid brush from cache, add it if not found
             @param(color Solid brush color to find)
             @returns(GDI+ brush)
            }
            function GetBrush(const color: TWColor): TGpBrush; overload; virtual;

            {**
             Get pen from cache, add it if not found
             @param(pStroke Stroke matching with the pen to find)
             @param(gradientFactory @bold([in, out]) The gradient factory used to generate the brush)
             @returns(GDI+ pen)
            }
            function GetPen(const pStroke: TWStroke;
                    const pGradientFactory: TWGDIPlusGradient): TGpPen; overload; virtual;

            {**
             Get solid pen from cache, add it if not found
             @param(color Pen color)
             @param(width Pen width)
             @returns(GDI+ pen)
            }
            function GetPen(const color: TWColor; width: Single): TGpPen; overload; virtual;

            {**
             Compare GDI font instances
             @param(pFont1 First font to compare)
             @param(pFont2 Second font to compare with)
             @returns(@true if GDI font instances are equals, otherwise @false)
             @br @bold(NOTE) This function is needed to avoid to include WGDIHelper in this header
            }
            class function CompareGDIFontInstances(pFont1, pFont2: TFont): Boolean; static;

            {**
             Convert GDI+ status to string
             @param(status GDI+ status to convert)
             @returns(GDI+ status as string)
            }
            class function GdiplusStatusToStr(status: GpStatus): UnicodeString; static;
    end;

var
    g_GDIPlusCacheController: TWGDIPlusCacheController;

implementation
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedBrush
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICachedBrush.Create(const pFill: TWFill);
begin
    inherited Create;

    m_pFill := TWFill.Create;
    m_pFill.Assign(pFill);
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.ICachedBrush.Destroy;
begin
    m_pFill.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedBrushKeyComparer
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedBrushKeyComparer.Equals(const pLeft, pRight: ICachedBrush): Boolean;
begin
    // are type identical?
    if (pLeft.m_pFill.BrushType <> pRight.m_pFill.BrushType) then
        Exit(False);

    Result := pLeft.m_pFill.Brush.IsEqual(pRight.m_pFill.Brush);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedBrushKeyComparer.GetHashCode(const pValue: ICachedBrush): Integer;
begin
    Result := $BE0F;
    Result := pValue.m_pFill.Brush.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedPen
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICachedPen.Create(const pStroke: TWStroke);
begin
    inherited Create;

    m_pStroke      := TWStroke.Create;
    m_BaseBrushKey := 0;

    m_pStroke.Assign(pStroke);
end;
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICachedPen.Create(const pStroke: TWStroke; pBaseBrush: TGpBrush);
begin
    inherited Create;

    m_pStroke      := TWStroke.Create;
    m_BaseBrushKey := NativeUInt(pBaseBrush);

    m_pStroke.Assign(pStroke);
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.ICachedPen.Destroy;
begin
    m_pStroke.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedPenKeyComparer
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedPenKeyComparer.Equals(const pLeft, pRight: ICachedPen): Boolean;
var
    dpCount, otherDpCount, i: Integer;
begin
    // pen was created from a brush?
    if ((pLeft.m_BaseBrushKey <> 0) or (pRight.m_BaseBrushKey <> 0)) then
    begin
        // compare the base brushes
        if (pLeft.m_BaseBrushKey <> pRight.m_BaseBrushKey) then
            Exit(False);

        // compare the pen width
        if (pLeft.m_pStroke.Width <> pRight.m_pStroke.Width) then
            Exit(False);

        // compare the line caps
        if (pLeft.m_pStroke.LineCap <> pRight.m_pStroke.LineCap) then
            Exit(False);

        // compare the dash caps
        if (pLeft.m_pStroke.DashCap <> pRight.m_pStroke.DashCap) then
            Exit(False);

        // compare the dash offsets
        if (pLeft.m_pStroke.DashOffset <> pRight.m_pStroke.DashOffset) then
            Exit(False);

        // get dash pattern values count
        dpCount      := pLeft.m_pStroke.DashPatternCount;
        otherDpCount := pRight.m_pStroke.DashPatternCount;

        // compare the dash pattern count
        if (dpCount <> otherDpCount) then
            Exit(False);

        // compare each dash pattern items
        for i := 0 to dpCount - 1 do
            if (pLeft.m_pStroke.DashPattern[i] <> pRight.m_pStroke.DashPattern[i]) then
                Exit(False);

        // compare the matrices
        if (pLeft.m_pStroke.Matrix.Differs(pRight.m_pStroke.Matrix^)) then
            Exit(False);

        Exit(True);
    end;

    Result := pLeft.m_pStroke.IsEqual(pRight.m_pStroke);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedPenKeyComparer.GetHashCode(const pValue: ICachedPen): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(pValue.m_BaseBrushKey, SizeOf(NativeUInt), $ABCD);
    {$else}
        Result := BobJenkinsHash(pValue.m_BaseBrushKey, SizeOf(NativeUInt), $ABCD);
    {$ifend}
    Result := pValue.m_pStroke.GetHashCode(Result);
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedFont
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICachedFont.Create(const pFont: TFont; hDC: THandle);
var
    fontStyle: System.UITypes.TFontStyle;
begin
    inherited Create;

    m_hDC           := hDC;
    m_Height        := pFont.Height;
    m_Style         := 0;
    m_Color         := TWColor.Create(pFont.Color);
    m_Charset       := pFont.Charset;
    m_Name          := pFont.Name;
    m_Orientation   := pFont.Orientation;
    m_Quality       := pFont.Quality;
    m_Pitch         := pFont.Pitch;
    m_PixelsPerInch := pFont.PixelsPerInch;

    // convert the font style to integer (required for comparison purposes)
    for fontStyle := Low(System.UITypes.TFontStyle) to High(System.UITypes.TFontStyle) do
        if (fontStyle in pFont.Style) then
            m_Style := m_Style or (1 shl Ord(fontStyle));
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.ICachedFont.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedFontKeyComparer
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedFontKeyComparer.Equals(const pLeft, pRight: ICachedFont): Boolean;
begin
    // compare font height
    if (pLeft.m_Height <> pRight.m_Height) then
        Exit(False);

    // compare font names
    if (pLeft.m_Name <> pRight.m_Name) then
        Exit(False);

    // compare charsets
    if (pLeft.m_Charset <> pRight.m_Charset) then
        Exit(False);

    // compare font styles
    if (pLeft.m_Style <> pRight.m_Style) then
        Exit(False);

    // compare font quality
    if (pLeft.m_Quality <> pRight.m_Quality) then
        Exit(False);

    // compare font orientation
    if (pLeft.m_Orientation <> pRight.m_Orientation) then
        Exit(False);

    // compare font pitch
    if (pLeft.m_Pitch <> pRight.m_Pitch) then
        Exit(False);

    // compare pixels per inch
    if (pLeft.m_PixelsPerInch <> pRight.m_PixelsPerInch) then
        Exit(False);

    // compare device contexts
    Result := (pLeft.m_hDC = pRight.m_hDC);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedFontKeyComparer.GetHashCode(const pValue: ICachedFont): Integer;
begin
    {$if CompilerVersion >= 29}
        Result := THashBobJenkins.GetHashValue(pValue.m_Height,        SizeOf(Integer),                      $BA51);
        Result := THashBobJenkins.GetHashValue(PChar(pValue.m_Name)^,  Length(pValue.m_Name) * SizeOf(Char), Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_Charset,       SizeOf(TFontCharset),                 Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_Style,         SizeOf(Integer),                      Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_Quality,       SizeOf(TFontQuality),                 Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_Orientation,   SizeOf(Integer),                      Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_Pitch,         SizeOf(TFontPitch),                   Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_PixelsPerInch, SizeOf(Integer),                      Result);
        Result := THashBobJenkins.GetHashValue(pValue.m_hDC,           SizeOf(THandle),                      Result);
    {$else}
        Result := BobJenkinsHash(pValue.m_Height,        SizeOf(Integer),                      $BA51);
        Result := BobJenkinsHash(PChar(pValue.m_Name)^,  Length(pValue.m_Name) * SizeOf(Char), Result);
        Result := BobJenkinsHash(pValue.m_Charset,       SizeOf(TFontCharset),                 Result);
        Result := BobJenkinsHash(pValue.m_Style,         SizeOf(Integer),                      Result);
        Result := BobJenkinsHash(pValue.m_Quality,       SizeOf(TFontQuality),                 Result);
        Result := BobJenkinsHash(pValue.m_Orientation,   SizeOf(Integer),                      Result);
        Result := BobJenkinsHash(pValue.m_Pitch,         SizeOf(TFontPitch),                   Result);
        Result := BobJenkinsHash(pValue.m_PixelsPerInch, SizeOf(Integer),                      Result);
        Result := BobJenkinsHash(pValue.m_hDC,           SizeOf(THandle),                      Result);
    {$ifend}
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICachedGraphics
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICachedGraphics.Create;
begin
    inherited Create;

    m_hDC            := 0;
    m_pGraphics      := nil;
    m_pGraphicsCount := nil;

    {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
        m_pGraphicsCount      := TWCacheHit.Create;
        m_pGraphicsCount.Name := 'Graphics';
    {$endif}
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.ICachedGraphics.Destroy;
begin
    Clear;

    {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
        m_pGraphicsCount.Free;
    {$endif}

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICachedGraphics.Clear;
begin
    FreeAndNil(m_pGraphics);

    m_hDC := 0;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICachedGraphics.GetGraphics(hDC: THandle): TGpGraphics;
var
    hNewDC: THandle;
begin
    // can use graphics cache?
    if (not g_GDIPlusCacheController.m_Graphics) then
        Exit(TGpGraphics.Create(hDC));

    // device context has changed?
    if (hDC = m_hDC) then
    begin
        hNewDC := m_pGraphics.GetHDC;

        // todo -cBug -oJean: why invoke a new DC here? Should be compared with hDC or update m_hDC?

        if (hNewDC <> 0) then
            m_pGraphics.ReleaseHDC(hNewDC);

        {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
            m_pGraphicsCount.Hit := m_pGraphicsCount.Hit + 1;
        {$endif}

        Exit(m_pGraphics);
    end;

    {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
        m_pGraphicsCount.Miss := m_pGraphicsCount.Miss + 1;
    {$endif}

    Clear;

    try
        // recreate graphics object
        m_pGraphics := TGpGraphics.Create(hDC);
        m_hDC       := hDC;
    finally
        if (not Assigned(m_pGraphics)) then
            m_hDC := 0;
    end;

    Result := m_pGraphics;
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus.ICache
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.ICache.Create;
begin
    inherited Create;

    m_MaxCachedBrushes := 50;
    m_MaxCachedPens    := 50;
    m_MaxCachedFonts   := 50;
    m_pBrushes         := IBrushCache.Create([doOwnsKeys, doOwnsValues], ICachedBrushKeyComparer.Create);
    m_pPens            := IPenCache.Create  ([doOwnsKeys, doOwnsValues], ICachedPenKeyComparer.Create);
    m_pFonts           := IFontCache.Create ([doOwnsKeys, doOwnsValues], ICachedFontKeyComparer.Create);
    m_pGraphics        := ICachedGraphics.Create;
    m_pFIFOBrushList   := IFIFOBrush.Create;
    m_pFIFOPenList     := IFIFOPen.Create;
    m_pFIFOFontList    := IFIFOFont.Create;
    m_pCustomFontList  := ICustomFontList.Create;
    m_pStringFormat    := nil;
    m_pFontCollection  := nil;
    m_pBrushesCount    := nil;
    m_pPensCount       := nil;
    m_pFontsCount      := nil;
    m_Locked           := False;

    {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
        m_pBrushesCount := TWCacheHit.Create;
        m_pPensCount    := TWCacheHit.Create;
        m_pFontsCount   := TWCacheHit.Create;

        m_pBrushesCount.Name := 'Brushes';
        m_pPensCount.Name    := 'Pens';
        m_pFontsCount.Name   := 'Fonts';
    {$endif}
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.ICache.Destroy;
begin
    Clear;

    m_pBrushes.Free;
    m_pPens.Free;
    m_pFonts.Free;
    m_pGraphics.Free;
    m_pFIFOBrushList.Free;
    m_pFIFOPenList.Free;
    m_pFIFOFontList.Free;
    m_pCustomFontList.Free;

    {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
        m_pBrushesCount.Free;
        m_pPensCount.Free;
        m_pFontsCount.Free;
    {$endif}

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICache.ConfigurePen(const pStroke: TWStroke; pPen: TGpPen);
var
    dashArray:                           array of Single;
    penWidth, dashFactor, delta:         Single;
    dashPatternSize, dashArrayLength, i: NativeUInt;
    lineCapStart, lineCapEnd:            TLineCap;
    dashCap:                             TDashCap;
    lineJoin:                            TLineJoin;
begin
    penWidth := pPen.GetWidth;

    // the dash factor is a value used to take care of the difference between svg and GDI+. In fact,
    // GDI+ multiplies each dash values by the stroke width
    if (penWidth <> 0.0) then
        dashFactor := penWidth
    else
        dashFactor := 1.0;

    pPen.SetDashOffset(pStroke.DashOffset / dashFactor);

    lineCapStart := GetLineCap(pStroke, pPen.GetStartCap);
    lineCapEnd   := GetLineCap(pStroke, pPen.GetEndCap);
    dashCap      := GetDashCap(pStroke, pPen.GetDashCap);

    pPen.SetLineCap(lineCapStart, lineCapEnd, dashCap);

    lineJoin := GetLineJoin(pStroke, pPen.GetLineJoin);
    pPen.SetLineJoin(lineJoin);

    dashPatternSize := pStroke.DashPatternCount;

    // do apply dash pattern?
    if (dashPatternSize > 0) then
    begin
        // configure dash style to custom (meaning that dash pattern should be used)
        pPen.SetDashStyle(DashStyleCustom);

        // delta is the value to apply to correct the size used by the linecap. This is done this way
        // because, when a linecap is selected, GDI+ may increase each dash pattern items by the
        // radius of each caps. To keep the pattern compatible with those written in the SVG, a
        // delta must be applied to compensate the difference
        if (pStroke.DashCap = E_DC_Flat) then
            delta := 0.0
        else
            delta := penWidth;

        // odd value?
        if ((dashPatternSize mod 2) <> 0) then
        begin
            dashArrayLength := dashPatternSize * 2;

            // an additional dash space length is required, so repeat the length
            SetLength(dashArray, dashArrayLength);

            // iterate through pattern items
            for i := 0 to dashPatternSize - 1 do
            begin
                // copy each item and apply the factor
                if ((i mod 2) <> 0) then
                    dashArray[i] := (pStroke.DashPattern[i] - delta) / dashFactor
                else
                    dashArray[i] := (pStroke.DashPattern[i] + delta) / dashFactor;

                // copy item on the 2nd pattern (a pattern declared 3;5;2 in SVG file means 3;5;2;3;5;2)
                dashArray[i + dashPatternSize] := dashArray[i];
            end;
        end
        else
        begin
            dashArrayLength := dashPatternSize;

            // an additional dash space length is required, so repeat the length
            SetLength(dashArray, dashArrayLength);

            // iterate through pattern items
            for i := 0 to dashPatternSize - 1 do
                // copy each item and apply the factor
                if ((i mod 2) <> 0) then
                    dashArray[i] := (pStroke.DashPattern[i] - delta) / dashFactor
                else
                    dashArray[i] := (pStroke.DashPattern[i] + delta) / dashFactor;
        end;

        // configure dash pattern
        pPen.SetDashPattern(PSingle(dashArray), dashArrayLength);
        Exit;
    end;

    // no dash used, set solid line
    pPen.SetDashStyle(DashStyleSolid);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetLineJoin(const pStroke: TWStroke; defValue: TLineJoin): TLineJoin;
begin
    case (pStroke.LineJoin) of
        E_LJ_Default:      Exit(defValue);
        E_LJ_Round:        Exit(LineJoinRound);
        E_LJ_Bevel:        Exit(LineJoinBevel);
        E_LJ_Miter:        Exit(LineJoinMiter);
        E_LJ_MiterClipped: Exit(LineJoinMiterClipped);
    else
        raise Exception.CreateFmt('Unknown linejoin - %d', [Integer(pStroke.DashCap)]);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetLineCap(const pStroke: TWStroke; defValue: TLineCap): TLineCap;
begin
    case (pStroke.LineCap) of
        E_LC_Default:       Exit(defValue);
        E_LC_Flat:          Exit(LineCapFlat);
        E_LC_Square:        Exit(LineCapSquare);
        E_LC_Round:         Exit(LineCapRound);
        E_LC_Triangle:      Exit(LineCapTriangle);
        E_LC_NoAnchor:      Exit(LineCapNoAnchor);
        E_LC_SquareAnchor:  Exit(LineCapSquareAnchor);
        E_LC_RoundAnchor:   Exit(LineCapRoundAnchor);
        E_LC_DiamondAnchor: Exit(LineCapDiamondAnchor);
        E_LC_ArrowAnchor:   Exit(LineCapArrowAnchor);
        E_LC_Custom:        Exit(LineCapCustom);
    else
        raise Exception.CreateFmt('Unknown linecap - %d', [Integer(pStroke.LineCap)]);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetDashCap(const pStroke: TWStroke; defValue: TDashCap): TDashCap;
begin
    case (pStroke.DashCap) of
        E_DC_Default:  Exit(defValue);
        E_DC_Flat:     Exit(DashCapFlat);
        E_DC_Round:    Exit(DashCapRound);
        E_DC_Triangle: Exit(DashCapTriangle);
    else
        raise Exception.CreateFmt('Unknown dashcap - %d', [Integer(pStroke.DashCap)]);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.CreateFont(pFont: TFont; hDC: THandle): TGpFont;
var
    pGDIPlusFont:                    TGpFont;
    index, familyCount, numFound, i: Integer;
    fontFamilies:                    array of TGpFontFamily;
    name:                            string;
    fontName:                        UnicodeString;
    found:                           Boolean;
begin
    pGDIPlusFont := nil;

    try
        // search for font name in custom font list
        index := m_pCustomFontList.IndexOf(pFont.Name);

        // found it?
        if (index = -1) then
            // create GDI+ font from standard font
            pGDIPlusFont := TGpFont.Create(hDC, pFont.Handle)
        else
        begin
            // cannot get font if font collection isn't initialized
            if (not Assigned(m_pFontCollection)) then
                Exit(nil);

            familyCount := m_pFontCollection.GetFamilyCount;
            numFound    := 0;
            found       := False;

            // create font families container
            SetLength(fontFamilies, familyCount);

            // get font families from custom private collection
            m_pFontCollection.GetFamilies(familyCount, fontFamilies, numFound);

            // iterate through font families
            for i := 0 to numFound - 1 do
            begin
                // get family name
                fontFamilies[i].GetFamilyName(name, LANG_NEUTRAL);
                fontName := name;

                // found font to create?
                if (fontName <> m_pCustomFontList[index]) then
                    continue;

                // create GDI+ font from custom font list
                pGDIPlusFont := TGpFont.Create(@fontFamilies[i], pFont.Size, FontStyleRegular,
                        UnitPixel);

                found := True;
                break;
            end;

            // could not found font to use?
            if (not found) then
                // failed, let GDI+ create font from standard font
                pGDIPlusFont := TGpFont.Create(hDC, pFont.Handle);
        end;

        Result       := pGDIPlusFont;
        pGDIPlusFont := nil;
    finally
        pGDIPlusFont.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICache.DeleteOldestObjsFromCache<T, U>(var pCachedObjs: TObjectDictionary<T, U>;
        var pCachedObjList: TList<T>; maxLimit: NativeUInt);
var
    pKey: T;
begin
    // is cache currently locked?
    if (m_Locked) then
        Exit;

    // caching is ignored if max cached objects is equal to 0
    if (maxLimit = 0) then
        Exit;

    // is number of objects allowed in cache exceeded?
    while (NativeUInt(pCachedObjs.Count) >= maxLimit) do
    begin
        // get first entered object and delete it from "first in/first out" list
        pKey := pCachedObjList.Extract(pCachedObjList[0]);

        // get cached object entry
        if (not pCachedObjs.ContainsKey(pKey)) then
            raise Exception.Create('Could not found object to delete');

        // delete object from cache
        pCachedObjs.Remove(pKey);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICache.Clear;
begin
    m_pBrushes.Clear;
    m_pPens.Clear;
    m_pFonts.Clear;

    m_pFIFOBrushList.Clear;
    m_pFIFOPenList.Clear;
    m_pFIFOFontList.Clear;

    m_pGraphics.Clear;

    FreeAndNil(m_pStringFormat);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetBrush(const pFill: TWFill; pGradientFactory: TWGDIPlusGradient): TGpBrush;
var
    pGpBrush:       TGpBrush;
    pKey:           ICachedBrush;
    pSolidBrush:    TWSolidBrush;
    pLinearBrush:   TWLinearGradientBrush;
    pRadialBrush:   TWRadialGradientBrush;
    panicModeColor: TWColor;
    stopCount:      NativeInt;
    success:        Boolean;
begin
    // can use brushes cache?
    if (not g_GDIPlusCacheController.m_Brushes) then
    begin
        // search for brush type to get
        case (pFill.BrushType) of
            E_BT_Solid:
            begin
                // get the solid brush
                pSolidBrush := pFill.Brush as TWSolidBrush;
                Assert(Assigned(pSolidBrush));

                // create a simple solid brush using the provided color
                Exit(pSolidBrush.Color.GetGDIPlusSolidBrush);
            end;

            E_BT_Linear:
            begin
                // get the linear gradient brush
                pLinearBrush := pFill.Brush as TWLinearGradientBrush;
                Assert(Assigned(pLinearBrush));

                // build a gradient factory to create the linear gradient brush
                pGradientFactory.FromBrush(pLinearBrush);

                pGpBrush := nil;

                try
                    // create a new linear gradient fill. NOTE the clamp rect is used only if wrap
                    // mode is clamp. For other modes the rect is simply ignored
                    pGpBrush := pGradientFactory.GetLinear(pLinearBrush.Vector);

                    // in case no brush was created, then a solid color brush should be used instead,
                    // using the last gradient stop color as solid color
                    if (not Assigned(pGpBrush) or not(pGpBrush is TGpLinearGradientBrush)) then
                    begin
                        stopCount := pLinearBrush.Stops.Count;

                        if (stopCount <> 0) then
                            Exit(pLinearBrush.Stops[stopCount - 1].Color.GetGDIPlusSolidBrush);

                        // panic mode, nothing works as expected, create a brush with the solid color
                        panicModeColor.SetColor(255, 255, 255, 255);
                        Exit(panicModeColor.GetGDIPlusSolidBrush);
                    end;

                    Result   := pGpBrush;
                    pGpBrush := nil;
                finally
                    pGpBrush.Free;
                end;

                Exit;
            end;

            E_BT_Radial:
            begin
                // get the radial gradient brush
                pRadialBrush := pFill.Brush as TWRadialGradientBrush;
                Assert(Assigned(pRadialBrush));

                // build a gradient factory to create the radial gradient brush
                pGradientFactory.FromBrush(pRadialBrush);

                pGpBrush := nil;

                try
                    // create a new radial gradient brush
                    pGpBrush := pGradientFactory.GetRadial(pRadialBrush.Radius);

                    // in case no brush was created, then a solid color brush should be used instead,
                    // using the last gradient stop color as solid color
                    if (not Assigned(pGpBrush) or not(pGpBrush is TGpPathGradientBrush)) then
                    begin
                        stopCount := pRadialBrush.Stops.Count;

                        if (stopCount <> 0) then
                            Exit(pRadialBrush.Stops[stopCount - 1].Color.GetGDIPlusSolidBrush);

                        // panic mode, nothing works as expected, create a brush with a default
                        // solid color
                        panicModeColor.SetColor(255, 255, 255, 255);
                        Exit(panicModeColor.GetGDIPlusSolidBrush);
                    end;

                    Result   := pGpBrush;
                    pGpBrush := nil;
                finally
                    pGpBrush.Free;
                end;

                Exit;
            end;
        else
            raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pFill.BrushType)]);
        end;
    end;

    pKey    := nil;
    Result  := nil;
    success := False;

    try
        pKey := ICachedBrush.Create(pFill);

        // search for cached brush, get it as result if found. NOTE don't forget that the brush
        // comparison function is overloaded, and will compare the key content, and not just the key
        // addresses, for that a newly created brush may be used as key directly
        if (m_pBrushes.TryGetValue(pKey, Result)) then
        begin
            {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
                m_pBrushesCount.Hit := m_pBrushesCount.Hit + 1;
            {$endif}

            // get the gradient factory to use for linear or radial gradients. This is required to
            // achieve several effects like e.g. the radial wrapping
            pGradientFactory.FromBrush(pFill.Brush);

            success := True;
            pKey.Free;
            Exit;
        end;

        {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
            m_pBrushesCount.Miss := m_pBrushesCount.Miss + 1;
        {$endif}

        DeleteOldestObjsFromCache<ICachedBrush, TGpBrush>(m_pBrushes, m_pFIFOBrushList, m_MaxCachedBrushes);

        // search for brush type to get
        case (pFill.BrushType) of
            E_BT_Solid:
            begin
                // get the solid brush
                pSolidBrush := pFill.Brush as TWSolidBrush;
                Assert(Assigned(pSolidBrush));

                // create a simple solid brush using the provided color
                Result := pSolidBrush.Color.GetGDIPlusSolidBrush;
            end;

            E_BT_Linear:
            begin
                // get the linear gradient brush
                pLinearBrush := pFill.Brush as TWLinearGradientBrush;
                Assert(Assigned(pLinearBrush));

                // build a gradient factory to create the linear gradient brush
                pGradientFactory.FromBrush(pLinearBrush);

                // create a new linear gradient fill. NOTE the clamp rect is used only if wrap mode
                // is clamp. For other modes the rect is simply ignored
                Result := pGradientFactory.GetLinear(pLinearBrush.Vector);

                // in case no brush was created, then a solid color brush should be used instead,
                // using the last gradient stop color as solid color
                if (not Assigned(Result) or not(Result is TGpLinearGradientBrush)) then
                begin
                    stopCount := pLinearBrush.Stops.Count;

                    if (stopCount <> 0) then
                        Result := pLinearBrush.Stops[stopCount - 1].Color.GetGDIPlusSolidBrush
                    else
                    begin
                        // panic mode, nothing works as expected, create a brush with a default
                        // solid color
                        panicModeColor.SetColor(255, 255, 255, 255);
                        Exit(panicModeColor.GetGDIPlusSolidBrush);
                    end;
                end
            end;

            E_BT_Radial:
            begin
                // get the radial gradient brush
                pRadialBrush := pFill.Brush as TWRadialGradientBrush;
                Assert(Assigned(pRadialBrush));

                // build a gradient factory to create the radial gradient brush
                pGradientFactory.FromBrush(pRadialBrush);

                // create a new radial gradient brush
                Result := pGradientFactory.GetRadial(pRadialBrush.Radius);

                // in case no brush was created, then a solid color brush should be used instead,
                // using the last gradient stop color as solid color
                if (not Assigned(Result) or not(Result is TGpPathGradientBrush)) then
                begin
                    stopCount := pRadialBrush.Stops.Count;

                    if (stopCount <> 0) then
                        Result := pRadialBrush.Stops[stopCount - 1].Color.GetGDIPlusSolidBrush
                    else
                    begin
                        // panic mode, nothing works as expected, create a brush with a default
                        // solid color
                        panicModeColor.SetColor(255, 255, 255, 255);
                        Exit(panicModeColor.GetGDIPlusSolidBrush);
                    end;
                end;
            end;
        else
            raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pFill.BrushType)]);
        end;

        // cache the newly created brush
        m_pBrushes.Add(pKey, Result);
        m_pFIFOBrushList.Add(pKey);

        success := True;
    finally
        if (not success) then
        begin
            pKey.Free;
            FreeAndNil(Result);
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetPen(const pStroke: TWStroke; pGradientFactory: TWGDIPlusGradient): TGpPen;
var
    pKey:        ICachedPen;
    pBaseBrush:  TGpBrush;
    paBaseBrush: IWSmartPointer<TGpBrush>;
    pFill:       IWSmartPointer<TWFill>;
    pSolidBrush: TWSolidBrush;
    success:     Boolean;
begin
    pKey    := nil;
    Result  := nil;
    success := False;

    try
        // can use pens cache?
        if (not g_GDIPlusCacheController.m_Pens) then
        begin
            // search for stroke brush type
            case (pStroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the solid base brush
                    pSolidBrush := pStroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidBrush));

                    // create a simple solid pen
                    Result := pSolidBrush.Color.GetGDIPlusPen;
                    Result.SetWidth(pStroke.Width);
                end;

                E_BT_Linear,
                E_BT_Radial:
                begin
                    // get the base brush fill
                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pStroke.Brush);

                    // get the pen base brush
                    pBaseBrush := GetBrush(pFill, pGradientFactory);

                    // is cache used? If not keep pointer in a smart pointer to auto-delete object
                    // on function ends
                    if (not g_GDIPlusCacheController.m_Brushes) then
                        paBaseBrush := TWSmartPointer<TGpBrush>.Create(pBaseBrush);

                    // create a pen from brush
                    Result := TGpPen.Create(pBaseBrush, pStroke.Width);
                end;
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d', [Integer(pStroke.BrushType)]);
            end;

            // configure pen
            ConfigurePen(pStroke, Result);

            success := True;
            Exit;
        end;

        pKey := ICachedPen.Create(pStroke);

        // search for cached pen, get it as result if found. NOTE don't forget that the pen
        // comparison function is overloaded, and will compare the key content, and not just the key
        // addresses, for that a newly created pen may be used as key directly
        if (m_pPens.TryGetValue(pKey, Result)) then
        begin
            {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
                m_pPensCount.Hit := m_pPensCount.Hit + 1;
            {$endif}

            // update pen width, as this value can change during pen lifecycle
            Result.SetWidth(pStroke.Width);

            success := True;
            pKey.Free;
            Exit;
        end;

        {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
            m_pPensCount.Miss := m_pPensCount.Miss + 1;
        {$endif}

        DeleteOldestObjsFromCache<ICachedPen, TGpPen>(m_pPens, m_pFIFOPenList, m_MaxCachedPens);

        // search for pen base brush type
        case (pStroke.BrushType) of
            E_BT_Solid:
            begin
                // get the solid base brush
                pSolidBrush := pStroke.Brush as TWSolidBrush;
                Assert(Assigned(pSolidBrush));

                // create a simple solid pen
                Result := pSolidBrush.Color.GetGDIPlusPen;
                Result.SetWidth(pStroke.Width);
            end;

            E_BT_Linear,
            E_BT_Radial:
            begin
                // get the base brush fill
                pFill := TWSmartPointer<TWFill>.Create();
                pFill.SetBrush(pStroke.Brush);

                // get the pen base brush
                pBaseBrush := GetBrush(pFill, pGradientFactory);

                // is cache used? If not keep pointer in a smart pointer to auto-delete object on
                // function ends
                if (not g_GDIPlusCacheController.m_Brushes) then
                    paBaseBrush := TWSmartPointer<TGpBrush>.Create(pBaseBrush);

                // create a pen from brush
                Result := TGpPen.Create(pBaseBrush, pStroke.Width);
            end;

        else
            raise Exception.CreateFmt('Unknown pen base brush type - %d', [Integer(pStroke.BrushType)]);
        end;

        // configure pen
        ConfigurePen(pStroke, Result);

        // cache the newly created pen
        m_pPens.Add(pKey, Result);
        m_pFIFOPenList.Add(pKey);

        success := True;
    finally
        if (not success) then
        begin
            pKey.Free;
            FreeAndNil(Result);
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetPen(const pStroke: TWStroke; pBaseBrush: TGpBrush): TGpPen;
var
    pKey:    ICachedPen;
    success: Boolean;
begin
    pKey    := nil;
    Result  := nil;
    success := False;

    try
        // can use pens cache?
        if (not g_GDIPlusCacheController.m_Pens) then
        begin
            Result := TGpPen.Create(pBaseBrush, pStroke.Width);

            // configure pen
            ConfigurePen(pStroke, Result);

            success := True;
            Exit;
        end;

        pKey := ICachedPen.Create(pStroke);

        // search for cached pen, get it as result if found. NOTE don't forget that the pen
        // comparison function is overloaded, and will compare the key content, and not just the key
        // addresses, for that a newly created pen may be used as key directly
        if (m_pPens.TryGetValue(pKey, Result)) then
        begin
            {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
                m_pPensCount.Hit := m_pPensCount.Hit + 1;
            {$endif}

            // update pen width, as this value can change during pen lifecycle
            Result.SetWidth(pStroke.Width);

            success := True;
            pKey.Free;
            Exit;
        end;

        {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
            m_pPensCount.Miss := m_pPensCount.Miss + 1;
        {$endif}

        DeleteOldestObjsFromCache<ICachedPen, TGpPen>(m_pPens, m_pFIFOPenList, m_MaxCachedPens);

        Result := TGpPen.Create(pBaseBrush, pStroke.Width);

        // configure pen
        ConfigurePen(pStroke, Result);

        // cache the newly created pen
        m_pPens.Add(pKey, Result);
        m_pFIFOPenList.Add(pKey);

        success := True;
    finally
        if (not success) then
        begin
            pKey.Free;
            FreeAndNil(Result);
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetFont(pFont: TFont; hDC: THandle): TGpFont;
var
    pKey:    ICachedFont;
    success: Boolean;
begin
    // can use fonts cache?
    if (not g_GDIPlusCacheController.m_Fonts) then
        Exit(CreateFont(pFont, hDC));

    pKey    := nil;
    Result  := nil;
    success := False;

    try
        pKey := ICachedFont.Create(pFont, hDC);

        // search for cached pen, get it as result if found. NOTE don't forget that the pen
        // comparison function is overloaded, and will compare the key content, and not just the key
        // addresses, for that a newly created pen may be used as key directly
        if (m_pFonts.TryGetValue(pKey, Result)) then
        begin
            {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
                m_pFontsCount.Hit := m_pFontsCount.Hit + 1;
            {$endif}

            success := True;
            pKey.Free;
            Exit;
        end;

        {$ifdef ENABLE_GDIPLUS_CACHE_LOGGING}
            m_pFontsCount.Miss := m_pFontsCount.Miss + 1;
        {$endif}

        DeleteOldestObjsFromCache<ICachedFont, TGpFont>(m_pFonts, m_pFIFOFontList, m_MaxCachedFonts);

        // create and cache new font
        Result := CreateFont(pFont, hDC);
        m_pFonts.Add(pKey, Result);
        m_pFIFOFontList.Add(pKey);

        success := True;
    finally
        if (not success) then
        begin
            pKey.Free;
            Result.Free;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ICache.GetStringFormat: TGpStringFormat;
begin
    // can use string format cache?
    if (not g_GDIPlusCacheController.m_StringFormat) then
        // create text string format
        Exit(TGpStringFormat.Create);

    // create cached text string format, if needed
    if (not Assigned(m_pStringFormat)) then
        m_pStringFormat := TGpStringFormat.Create;

    Result := m_pStringFormat;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICache.Lock;
begin
    m_Locked := True;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ICache.Unlock;
begin
    m_Locked := False;

    // clean locked caches
    DeleteOldestObjsFromCache<ICachedBrush, TGpBrush>(m_pBrushes, m_pFIFOBrushList, m_MaxCachedBrushes);
    DeleteOldestObjsFromCache<ICachedPen,   TGpPen>  (m_pPens,    m_pFIFOPenList,   m_MaxCachedPens);
    DeleteOldestObjsFromCache<ICachedFont,  TGpFont> (m_pFonts,   m_pFIFOFontList,  m_MaxCachedFonts);
end;
//---------------------------------------------------------------------------
// TWRenderer_GDIPlus
//---------------------------------------------------------------------------
constructor TWRenderer_GDIPlus.Create;
begin
    inherited Create;

    m_pCache             := ICache.Create;
    m_GDIPlusToken       := 0;
    m_GDIPlusInitialized := False;
    m_pRenderer_GDI      := nil;

    Initialize;
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDIPlus.Destroy;
begin
    Release;

    m_pCache.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.Initialize: Boolean;
var
    startupInput:  GdiplusStartupInput;
    startupOutput: GdiplusStartupOutput;
    status:        GpStatus;
begin
    // already initialized?
    if (m_GDIPlusInitialized) then
        Exit(True);

    startupInput.DebugEventCallback       := nil;
    startupInput.SuppressBackgroundThread := True;
    startupInput.SuppressExternalCodecs   := False;
    startupInput.GdiplusVersion           := 1;

    // initialize GDI+
    status := GdiplusStartup(m_GDIPlusToken, @startupInput, @startupOutput);

    // succeeded?
    if (status <> Ok) then
    begin
        TWLogHelper.LogToCompiler('GDI+ renderer - initialize - FAILED - GDI+ status - '
                + GdiPlusStatusToStr(status));

        m_GDIPlusToken := 0;
        Exit(False);
    end;

    m_pCache.m_pFontCollection := TGpPrivateFontCollection.Create;
    m_GDIPlusInitialized       := True;
    Result                     := True;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.Release;
begin
    m_pCache.Clear;
    FreeAndNil(m_pCache.m_pFontCollection);

    // not initialized?
    if (not m_GDIPlusInitialized) then
        Exit;

    // release GDI+, if needed
    GdiplusShutdown(m_GDIPlusToken);

    m_GDIPlusToken       := 0;
    m_GDIPlusInitialized := False;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ApplyRadialGradientClipRegion(const pFill: TWFill; const radius: TWSizeF;
        const pGradientFactory: TWGDIPlusGradient; pGraphics: TGpGraphics);
var
    pPath:   IWSmartPointer<TGpGraphicsPath>;
    pRegion: IWSmartPointer<TGpRegion>;
begin
    if (pFill.BrushType <> E_BT_Radial) then
        Exit;

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get the graphic path used to generate the radial gradient brush
    pGradientFactory.GetRadialPath(radius, pPath);

    // clip the main region
    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pPath));
    pGraphics.SetClip(pRegion, CombineModeXor);
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.CalculateRadialGradientNextRadiusAndCenterPoint(const pFill: TWFill;
        scaleFactor: NativeUInt; const center, delta: TWPointF; out radius: TWSizeF;
        pGradientFactory: TWGDIPlusGradient);
var
    nextCenter: TWPointF;
    pRadial:    TWRadialGradientBrush;
begin
    if (pFill.BrushType <> E_BT_Radial) then
        Exit;

    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // increase the radius for the next brush
    radius.Width  := pRadial.Radius.Width  * (scaleFactor + 1);
    radius.Height := pRadial.Radius.Height * (scaleFactor + 1);

    // calculate next center point
    nextCenter.X := center.X - (delta.X * NativeInt(scaleFactor));
    nextCenter.Y := center.Y - (delta.Y * NativeInt(scaleFactor));

    pGradientFactory.SetCenterPoint(nextCenter);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetRadialGradientWrapBrush(const pFill: TWFill; const radius: TWSizeF;
        const pGradientFactory: TWGDIPlusGradient): TGpBrush;
begin
    Result := nil;

    if (pFill.BrushType <> E_BT_Radial) then
        Exit;

    Result := pGradientFactory.GetRadial(radius);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.UpdateRadialGradientStops(const offsets: IStopOffsets;
        scaleFactor, stopCount: NativeUInt; swapColors: Boolean;
        const pGradientFactory: TWGDIPlusGradient): TWGDIPlusGradient.IStop;
var
    i, index:     NativeUInt;
    pStart, pEnd: TWGDIPlusGradient.IStop;
    tmpColor:     TWColor;
begin
    // move the gradient start pos to the correct location
    Result := pGradientFactory.GetStop(0);
    Result.Offset := 1.0 - (1.0 / (scaleFactor + 1));

    if (stopCount > 0) then
        for i := 0 to stopCount - 1 do
        begin
            index := i + 1;

            pStart := pGradientFactory.GetStop(index);
            pEnd   := pGradientFactory.GetStop(stopCount - i);

            // start and end stops are the same (i.e. the middle is reached)?
            if (pStart.IsEqual(pEnd)) then
            begin
                // nothing to swap, just update the offset
                pStart.Offset := Result.Offset + (offsets[index] * (1.0 - Result.Offset));

                if (i > stopCount div 2) then
                    Exit;

                continue;
            end;

            // swap the colors
            if (swapColors) then
            begin
                tmpColor := pStart.Color^;
                pEnd.SetColor(pStart.Color);
                pStart.SetColor(@tmpColor);
            end;

            // update the offsets
            pStart.Offset := Result.Offset + (offsets[index]               * (1.0 - Result.Offset));
            pStart.Offset := Result.Offset + (offsets[(stopCount - i) - 1] * (1.0 - Result.Offset));

            if (i > stopCount div 2) then
                Exit;
        end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ToGDIPlusPointList(const pointList: TWRenderer.IPointList;
        out gdiPlusPointList: IGDIPlusPointList; out boundingBox: TWRectF);
var
    point: TWPointF;
    index: NativeInt;
begin
    // resize destination point list
    SetLength(gdiPlusPointList, Length(pointList));

    index := 0;

    // iterate through points to copy
    for point in pointList do
    begin
        // update bounding box
        if (boundingBox.Left <> 0) then
            boundingBox.Left := Min(pointList[index].X, boundingBox.Left)
        else
            boundingBox.Left := pointList[index].X;

        if (boundingBox.Right <> 0) then
            boundingBox.Right := Max(pointList[index].X, boundingBox.Right)
        else
            boundingBox.Right := pointList[index].X;

        if (boundingBox.Top <> 0) then
            boundingBox.Top := Min(pointList[index].Y, boundingBox.Top)
        else
            boundingBox.Top := pointList[index].Y;

        if (boundingBox.Bottom <> 0) then
            boundingBox.Bottom := Max(pointList[index].Y, boundingBox.Bottom)
        else
            boundingBox.Bottom := pointList[index].Y;

        // copy point
        gdiPlusPointList[index] := MakePoint(point.X, point.Y);
        Inc(index);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.ConfigTextDraw(hDC: THandle; const pOptions: TWRenderer.ITextOptions;
        pGraphics: TGpGraphics; ppGdiPlusFont: PPGpFont; ppBrush: PPGpBrush; ppFormat: PPGpStringFormat);
var
    textRHint: TextRenderingHint;
    textColor: TWColor;
    align:     TStringAlignment;
    flags:     StringFormatFlags;
    trimming:  StringTrimming;
begin
    // search for text rendering type
    case (pOptions.TextRendering) of
        E_R_Default:                   textRHint := TextRenderingHintSystemDefault;
        E_R_ClearType_GridFit:         textRHint := TextRenderingHintClearTypeGridFit;
        E_R_SingleBitPerPixel:         textRHint := TextRenderingHintSingleBitPerPixel;
        E_R_SingleBitPerPixel_GridFit: textRHint := TextRenderingHintSingleBitPerPixelGridFit;

        E_R_AntiAlias:
        begin
            // set anti aliasing mode and contrast (useless otherwise)
            textRHint := TextRenderingHintAntiAlias;
            pGraphics.SetTextContrast(pOptions.Contrast);
        end;

        E_R_AntiAlias_GridFit:
        begin
            // set anti aliasing grid fit mode and contrast (useless otherwise)
            textRHint := TextRenderingHintAntiAliasGridFit;
            pGraphics.SetTextContrast(pOptions.Contrast);
        end;
    else
        textRHint := TextRenderingHintSystemDefault;
    end;

    // set text rendering hint
    pGraphics.SetTextRenderingHint(textRHint);

    // get font from cache
    if (Assigned(ppGdiPlusFont)) then
        ppGdiPlusFont^ := m_pCache.GetFont(pOptions.Font, hDC);

    // get text color
    textColor := TWColor.Create(pOptions.Font.Color, pOptions.Alpha);

    // create text brush
    if (Assigned(ppBrush)) then
        ppBrush^ := GetBrush(textColor);

    if (not Assigned(ppFormat)) then
        Exit;

    // get cached text string format
    ppFormat^ := m_pCache.GetStringFormat;

    // search for horizontal alignment
    case (pOptions.AlignHorz) of
        E_H_Left:  align := StringAlignmentNear;
        E_H_Right: align := StringAlignmentFar;
    else
        align := StringAlignmentCenter;
    end;

    // set horiz alignment
    ppFormat^.SetAlignment(align);

    // search for vertical alignment
    case (pOptions.AlignVert) of
        E_V_Top:    align := StringAlignmentNear;
        E_V_Bottom: align := StringAlignmentFar;
    else
        align := StringAlignmentCenter;
    end;

    // set vert alignment
    ppFormat^.SetLineAlignment(align);

    // use bypass GDI flag to avoid alpha blending problems
    flags := StringFormatFlagsBypassGDI;

    // do draw from right to left?
    if (pOptions.RightToLeft) then
        flags := flags or StringFormatFlagsDirectionRightToLeft;

    // do draw vertically?
    if (pOptions.Vertical) then
        flags := flags or StringFormatFlagsDirectionVertical;

    // do ignore word wrapping?
    if (pOptions.NoWrap) then
        flags := flags or StringFormatFlagsNoWrap;

    // do ignore clipping?
    if (pOptions.NoClip) then
        flags := flags or StringFormatFlagsNoClip;

    // set string format flags
    ppFormat^.SetFormatFlags(flags);

    // search for text trimming
    case (pOptions.TextTrimming) of
        E_TT_None:              trimming := StringTrimmingNone;
        E_TT_Character:         trimming := StringTrimmingCharacter;
        E_TT_Word:              trimming := StringTrimmingWord;
        E_TT_EllipsisCharacter: trimming := StringTrimmingEllipsisCharacter;
        E_TT_EllipsisWord:      trimming := StringTrimmingEllipsisWord;
        E_TT_EllipsisPath:      trimming := StringTrimmingEllipsisPath;
    else
        raise Exception.CreateFmt('Unknown text trimming type - %d', [Integer(pOptions.TextTrimming)]);
    end;

    ppFormat^.SetTrimming(trimming);

    // when in text there is '&' (e.g. "&Save"), show it as shortcut (important for buttons)
    if (pOptions.ShowHotkeyPrefix) then
        ppFormat^.SetHotkeyPrefix(HotkeyPrefixShow)
    else
        ppFormat^.SetHotkeyPrefix(HotkeyPrefixNone);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.MeasureTextPadding(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; measureType: IEPaddingMeasureType;
        pFont: TGpFont; pFormat: TGpStringFormat): TWPaddingI;
var
    x, y, width, height:                   Integer;
    maxWidth, maxHeight, xOffset, yOffset: NativeUInt;
    pBitmap:                               IWSmartPointer<Vcl.Graphics.TBitmap>;
    pGraphics:                             TGpGraphics;
    paGraphics:                            IWSmartPointer<TGpGraphics>;
    pBgBrush, pTxtBrush:                   TGpBrush;
    paBgBrush, paTxtBrush:                 IWSmartPointer<TGpBrush>;
    textRect:                              TGpRectF;
    blackColor, whiteColor:                TWColor;
    pLine:                                 PWRGBTripleArray;
begin
    // get whished text width and height
    width  := Round(rect.Width);
    height := Round(rect.Height);

    // no width or no height? (NOTE be careful, width and height can be negative, causing thus an
    // exception later if not handled here)
    if ((width <= 0) or (height <= 0)) then
        Exit(Default(TWPaddingI));

    // initialize default padding. If no text is drawn, padding is assumed to be 0 on the left and
    // the top, and occupies entire space on the right and the bottom
    Result := TWPaddingI.Create(0, 0, width, height);

    // is text empty?
    if (Length(text) = 0) then
    begin
        // in case padding to measure is the right, swap horizontal resulting values
        if (IE_Right in measureType) then
        begin
            // swap the properties content (using the XOR trick, see:
            // http://chris-taylor.github.io/blog/2013/02/25/xor-trick/)
            Result.Left  := Result.Left xor Result.Right;
            Result.Right := Result.Left xor Result.Right;
            Result.Left  := Result.Left xor Result.Right;
        end;

        // in case padding to measure is the bottom, swap vertical resulting values
        if (IE_Bottom in measureType) then
        begin
            // swap the properties content (using the XOR trick, see:
            // http://chris-taylor.github.io/blog/2013/02/25/xor-trick/)
            Result.Top    := Result.Top xor Result.Bottom;
            Result.Bottom := Result.Top xor Result.Bottom;
            Result.Top    := Result.Top xor Result.Bottom;
        end;

        Exit;
    end;

    // create compatible bitmap to draw text
    pBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
    TWGDIHelper.ConfigBitmap(pBitmap, width, height, False, bmDIB, pf24bit);

    // get GDI+ graphics from bitmap canvas
    pGraphics := m_pCache.m_pGraphics.GetGraphics(pBitmap.Canvas.Handle);

    // FIXME need to check VERY WELL if this solution works as expected (also modify all other occurrences)
    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    whiteColor := TWColor.Create(255, 255, 255, 255);

    // get white brush from cache to paint background
    pBgBrush := GetBrush(whiteColor);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBgBrush := TWSmartPointer<TGpBrush>.Create(pBgBrush);

    textRect        := Default(TGpRectF);
    textRect.Width  := width;
    textRect.Height := height;

    // fill background with white color
    pGraphics.FillRectangle(pBgBrush, textRect);

    blackColor := TWColor.Create(0, 0, 0, 255);

    // get black brush from cache to draw text
    pTxtBrush := GetBrush(blackColor);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paTxtBrush := TWSmartPointer<TGpBrush>.Create(pTxtBrush);

    // draw text, or just the first letter if no wrapping and only left padding must be measured
    if (pOptions.NoWrap and (IE_Left in measureType) and (text[1] = ' ')) then
        pGraphics.DrawString(text, 1, pFont, textRect, pFormat, pTxtBrush)
    else
        pGraphics.DrawString(text, Length(text), pFont, textRect, pFormat, pTxtBrush);

    // do measure left/top or all padding?
    if ((measureType = []) or (IE_Left in measureType) or (IE_Top in measureType)) then
    begin
        // initialize left and top padding (needed to find the smallest length)
        Result.Left := width;
        Result.Top  := height;

        // iterate through bitmap lines to search left/top padding
        for y := 0 to height - 1 do
        begin
            // get line
            pLine := PWRGBTripleArray(pBitmap.ScanLine[y]);

            // iterate through bitmap pixels
            for x := 0 to width - 1 do
            begin
                if ((pLine[x].rgbtRed   = 255) and
                    (pLine[x].rgbtGreen = 255) and
                    (pLine[x].rgbtBlue  = 255))
                then
                    continue;

                // update padding values
                Result.Left := Min(Result.Left, x);
                Result.Top  := Min(Result.Top,  y);

                // break the loop only if top padding shouldn't be measured
                if (not(IE_Top in measureType)) then
                    break;
            end;
        end;
    end;

    // do measure right/bottom or all padding?
    if ((measureType = []) or (IE_Right in measureType) or (IE_Bottom in measureType)) then
    begin
        // calculate max height position where it's possible to iterate
        maxHeight := (height - 1);

        // iterate through bitmap lines to search right/bottom padding
        for y := 0 to height - 1 do
        begin
            // calculate next line y offset
            yOffset := (maxHeight - NativeUInt(y));

            // get line
            pLine := PWRGBTripleArray(pBitmap.ScanLine[yOffset]);

            // calculate max width position where it's possible to iterate
            maxWidth := (width - 1);

            // iterate through bitmap pixels
            for x := 0 to width - 1 do
            begin
                // calculate next pixel x offset
                xOffset := (maxWidth - NativeUInt(x));

                if ((pLine[xOffset].rgbtRed   = 255) and
                    (pLine[xOffset].rgbtGreen = 255) and
                    (pLine[xOffset].rgbtBlue  = 255))
                then
                    continue;

                // update padding values
                Result.Right  := Min(Result.Right,  width  - (x + 1));
                Result.Bottom := Min(Result.Bottom, height - (y + 1));

                // break the loop only if bottom padding shouldn't be measured
                if (not(IE_Bottom in measureType)) then
                    break;
            end;
        end;
    end;

    // do measure all paddings?
    if (measureType <> []) then
    begin
        // reset left padding, if needed
        if (not(IE_Left in measureType)) then
            Result.Left := 0;

        // reset top padding, if needed
        if (not(IE_Top in measureType)) then
            Result.Top := 0;

        // reset right padding, if needed
        if (not(IE_Right in measureType)) then
            Result.Right := 0;

        // reset bottom padding, if needed
        if (not(IE_Bottom in measureType)) then
            Result.Bottom := 0;
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.ConvertMaskToRGBA(const color: TWColor; pMask, pResult: Vcl.Graphics.TBitmap): Boolean;
var
    pMaskData, pResultData, pPtr:        PByte;
    pixelCount, maskSize, pixelDelta, i: Cardinal;
    offset:                              NativeUInt;
    mask:                                Bitmap;
    maskColor:                           TWColor;
    grayScale, alpha:                    Byte;
    premultipliedAlpha:                  Single;
begin
    pMaskData   := nil;
    pResultData := nil;

    try
        // get pixel count
        pixelCount := pMask.Width * pMask.Height;

        // get mask bitmap
        GetObject(pMask.Handle, sizeof(Bitmap), @mask);

        // get mask size
        maskSize := mask.bmWidthBytes * mask.bmHeight;

        // get mask data
        GetMem(pMaskData, maskSize);
        GetBitmapBits(pMask.Handle, maskSize, pMaskData);

        // create result data
        GetMem(pResultData, maskSize);

        // get mask pixel delta
        pixelDelta := mask.bmBitsPixel shr 3;

        // iterate through pixels to convert
        if (pixelCount > 0) then
            for i := 0 to pixelCount - 1 do
            begin
                // calculate pixel offset
                offset := i * pixelDelta;

                // get pixel color from mask
                maskColor := TWColor.Create(pMaskData[offset + 2], pMaskData[offset + 1], pMaskData[offset]);

                // convert to gray scale value
                grayScale := maskColor.GetGrayscale;

                // is pixel fully transparent?
                if (grayScale = 0) then
                begin
                    // set black pixel in destination
                    pPtr := pResultData + offset;
                    FillChar(pPtr, $0, pixelDelta);
                    continue;
                end;

                // calculate final alpha value and color premultiplication value
                alpha              := ((color.GetAlpha * grayScale) div 255);
                premultipliedAlpha := alpha;
                premultipliedAlpha := (premultipliedAlpha / 255.0);

                // calculate final pixel color and alpha
                pResultData[offset]     := Trunc(color.GetBlue  * premultipliedAlpha);
                pResultData[offset + 1] := Trunc(color.GetGreen * premultipliedAlpha);
                pResultData[offset + 2] := Trunc(color.GetRed   * premultipliedAlpha);
                pResultData[offset + 3] := alpha;
            end;

        // create compatible bitmap that will contain text
        pResult.PixelFormat := pf32bit;
        pResult.AlphaFormat := afPremultiplied;
        pResult.SetSize(pMask.Width, pMask.Height);
        SetBitmapBits(pResult.Handle, maskSize, pResultData);
    finally
        // delete mask data, if exists
        if (Assigned(pMaskData)) then
            FreeMem(pMaskData);

        // delete result data, if exists
        if (Assigned(pResultData)) then
            FreeMem(pResultData);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.CreateCartoonBubble(const rect: TWRectF; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath;
        out clientRect: TRect): Boolean;
var
    doDrawLeftLine, doDrawTopLine, doDrawRightLine, doDrawBottomLine: Boolean;
begin
    if (not Assigned(pTail)) then
        Exit(False);

    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    // no tail?
    if (not pTail.Visible) then
    begin
        clientRect := rect.ToTRect(True);

        case (pTail.Orientation) of
            E_O_Left:   clientRect.Left   := clientRect.Left   + Integer(pTail.Height);
            E_O_Top:    clientRect.Top    := clientRect.Top    + Integer(pTail.Height);
            E_O_Right:  clientRect.Right  := clientRect.Right  - Integer(pTail.Height);
            E_O_Bottom: clientRect.Bottom := clientRect.Bottom - Integer(pTail.Height);
        else
            TWLogHelper.LogToCompiler('GDI+ renderer - CreateCartoonBubble - unknown orientation - '
                    + IntToStr(Integer(pTail.Orientation)));
            Exit(False);
        end;

        // create simple rounded rectangle
        Exit(CreateRoundedRect(TWRectF.Create(clientRect), pRadius, pPath));
    end;

    // create cartoon bubble path (by assembling some graphic figures and closing all of them)
    case (pTail.Orientation) of
        E_O_Left:
        begin
            // add left tail to path
            if (not AddLeftTailToPath(rect.ToTRect(True), pTail, pRadius, pPath)) then
                Exit(False);

            // calculate client rect
            clientRect      := rect.ToTRect(True);
            clientRect.Left := clientRect.Left + Integer(pTail.Height);

            // select lines to draw
            doDrawTopLine    := pRadius.LeftTop.IsZero    or pRadius.RightTop.IsZero;
            doDrawRightLine  := pRadius.RightTop.IsZero   or pRadius.RightBottom.IsZero;
            doDrawBottomLine := pRadius.LeftBottom.IsZero or pRadius.RightBottom.IsZero;

            // left top radius is set?
            if (not pRadius.LeftTop.IsZero) then
                // FIXME need to check if building TWRectF this way will not drive to empty struct in called func (also modify other occurrences)
                // add rounded left top corner to path
                AddLeftTopCornerToPath(TWRectF.Create(clientRect), pRadius.LeftTop^, pPath);

            // do draw top line?
            if (doDrawTopLine) then
                pPath.AddLine(clientRect.Left + Integer(GetRadiusX(pRadius.LeftTop^)), clientRect.Top,
                        clientRect.Right - Integer(GetRadiusX(pRadius.RightTop^)), clientRect.Top);

            // right top radius is set?
            if (not pRadius.RightTop.IsZero) then
                // add rounded right top corner to path
                AddRightTopCornerToPath(TWRectF.Create(clientRect), pRadius.RightTop^, pPath);

            // do draw right line?
            if (doDrawRightLine) then
                pPath.AddLine(clientRect.Right, clientRect.Top + Integer(GetRadiusY(pRadius.RightTop^)),
                        clientRect.Right, clientRect.Bottom - Integer(GetRadiusY(pRadius.RightBottom^)));

            // right bottom radius is set?
            if (not pRadius.RightBottom.IsZero) then
                // add rounded right bottom corner to path
                AddRightBottomCornerToPath(TWRectF.Create(clientRect), pRadius.RightBottom^, pPath);

            // do draw bottom line?
            if (doDrawBottomLine) then
                pPath.AddLine(clientRect.Right - Integer(GetRadiusX(pRadius.RightBottom^)),
                        clientRect.Bottom, clientRect.Left + Integer(GetRadiusX(pRadius.LeftBottom^)),
                        clientRect.Bottom);

            // left bottom radius is set?
            if (not pRadius.LeftBottom.IsZero) then
                // add rounded left bottom corner to path
                AddLeftBottomCornerToPath(TWRectF.Create(clientRect), pRadius.LeftBottom^, pPath);
        end;

        E_O_Top:
        begin
            // add top tail to path
            if (not AddTopTailToPath(rect.ToTRect(True), pTail, pRadius, pPath)) then
                Exit(False);

            // calculate corner rect
            clientRect     := rect.ToTRect(True);
            clientRect.Top := clientRect.Top + Integer(pTail.Height);

            // select lines to draw
            doDrawLeftLine   := pRadius.LeftTop.IsZero    or pRadius.LeftBottom.IsZero;
            doDrawRightLine  := pRadius.RightTop.IsZero   or pRadius.RightBottom.IsZero;
            doDrawBottomLine := pRadius.LeftBottom.IsZero or pRadius.RightBottom.IsZero;

            // right top radius is set?
            if (not pRadius.RightTop.IsZero) then
                // add rounded right top corner to path
                AddRightTopCornerToPath(TWRectF.Create(clientRect), pRadius.RightTop^, pPath);

            // do draw right line?
            if (doDrawRightLine) then
                pPath.AddLine(clientRect.Right, clientRect.Top + Integer(GetRadiusY(pRadius.RightTop^)),
                        clientRect.Right, clientRect.Bottom - Integer(GetRadiusY(pRadius.RightBottom^)));

            // right bottom radius is set?
            if (not pRadius.RightBottom.IsZero) then
                // add rounded right bottom corner to path
                AddRightBottomCornerToPath(TWRectF.Create(clientRect), pRadius.RightBottom^, pPath);

            // do draw bottom line?
            if (doDrawBottomLine) then
                pPath.AddLine(clientRect.Right - Integer(GetRadiusX(pRadius.RightBottom^)),
                        clientRect.Bottom, clientRect.Left + Integer(GetRadiusX(pRadius.LeftBottom^)),
                        clientRect.Bottom);

            // left bottom radius is set?
            if (not pRadius.LeftBottom.IsZero) then
                // add rounded left bottom corner to path
                AddLeftBottomCornerToPath(TWRectF.Create(clientRect), pRadius.LeftBottom^, pPath);

            // do draw left line?
            if (doDrawLeftLine) then
                pPath.AddLine(clientRect.Left, clientRect.Bottom - Integer(GetRadiusY(pRadius.LeftBottom^)),
                        clientRect.Left, clientRect.Top + Integer(GetRadiusY(pRadius.LeftTop^)));

            // left top radius is set?
            if (not pRadius.LeftTop.IsZero) then
                // add rounded left top corner to path
                AddLeftTopCornerToPath(TWRectF.Create(clientRect), pRadius.LeftTop^, pPath);
        end;

        E_O_Right:
        begin
            // add right tail to path
            if (not AddRightTailToPath(rect.ToTRect(True), pTail, pRadius, pPath)) then
                Exit(False);

            // calculate corner rect
            clientRect       := rect.ToTRect(True);
            clientRect.Right := clientRect.Right - Integer(pTail.Height);

            // select lines to draw
            doDrawLeftLine   := pRadius.LeftTop.IsZero    or pRadius.LeftBottom.IsZero;
            doDrawTopLine    := pRadius.LeftTop.IsZero    or pRadius.RightTop.IsZero;
            doDrawBottomLine := pRadius.LeftBottom.IsZero or pRadius.RightBottom.IsZero;

            // right bottom radius is set?
            if (not pRadius.RightBottom.IsZero) then
                // add rounded right bottom corner to path
                AddRightBottomCornerToPath(TWRectF.Create(clientRect), pRadius.RightBottom^, pPath);

            // do draw bottom line?
            if (doDrawBottomLine) then
                pPath.AddLine(clientRect.Right - Integer(GetRadiusX(pRadius.RightBottom^)), clientRect.Bottom,
                        clientRect.Left + Integer(GetRadiusX(pRadius.LeftBottom^)), clientRect.Bottom);

            // left bottom radius is set?
            if (not pRadius.LeftBottom.IsZero) then
                // add rounded left bottom corner to path
                AddLeftBottomCornerToPath(TWRectF.Create(clientRect), pRadius.LeftBottom^, pPath);

            // do draw left line?
            if (doDrawLeftLine) then
                pPath.AddLine(clientRect.Left, clientRect.Bottom - Integer(GetRadiusY(pRadius.LeftBottom^)),
                        clientRect.Left, clientRect.Top + Integer(GetRadiusY(pRadius.LeftTop^)));

            // left top radius is set?
            if (not pRadius.LeftTop.IsZero) then
                // add rounded left top corner to path
                AddLeftTopCornerToPath(TWRectF.Create(clientRect), pRadius.LeftTop^, pPath);

            // do draw top line?
            if (doDrawTopLine) then
                pPath.AddLine(clientRect.Left + Integer(GetRadiusX(pRadius.LeftTop^)), clientRect.Top,
                        clientRect.Right - Integer(GetRadiusX(pRadius.RightTop^)), clientRect.Top);

            // right top radius is set?
            if (not pRadius.RightTop.IsZero) then
                // add rounded right top corner to path
                AddRightTopCornerToPath(TWRectF.Create(clientRect), pRadius.RightTop^, pPath);
        end;

        E_O_Bottom:
        begin
            // add bottom tail to path
            if (not AddBottomTailToPath(rect.ToTRect(True), pTail, pRadius, pPath)) then
                Exit(False);

            // calculate corner rect
            clientRect        := rect.ToTRect(True);
            clientRect.Bottom := clientRect.Bottom - Integer(pTail.Height);

            // select lines to draw
            doDrawLeftLine  := pRadius.LeftTop.IsZero  or pRadius.LeftBottom.IsZero;
            doDrawTopLine   := pRadius.LeftTop.IsZero  or pRadius.RightTop.IsZero;
            doDrawRightLine := pRadius.RightTop.IsZero or pRadius.RightBottom.IsZero;

            // left bottom radius is set?
            if (not pRadius.LeftBottom.IsZero) then
                // add rounded left bottom corner to path
                AddLeftBottomCornerToPath(TWRectF.Create(clientRect), pRadius.LeftBottom^, pPath);

            // do draw left line?
            if (doDrawLeftLine) then
                pPath.AddLine(clientRect.Left, clientRect.Bottom - Integer(GetRadiusY(pRadius.LeftBottom^)),
                        clientRect.Left, clientRect.Top + Integer(GetRadiusY(pRadius.LeftTop^)));

            // left top radius is set?
            if (not pRadius.LeftTop.IsZero) then
                // add rounded left top corner to path
                AddLeftTopCornerToPath(TWRectF.Create(clientRect), pRadius.LeftTop^, pPath);

            // do draw top line?
            if (doDrawTopLine) then
                pPath.AddLine(clientRect.Left + Integer(GetRadiusX(pRadius.LeftTop^)), clientRect.Top,
                        clientRect.Right - Integer(GetRadiusX(pRadius.RightTop^)), clientRect.Top);

            // right top radius is set?
            if (not pRadius.RightTop.IsZero) then
                // add rounded right top corner to path
                AddRightTopCornerToPath(TWRectF.Create(clientRect), pRadius.RightTop^, pPath);

            // do draw right line?
            if (doDrawRightLine) then
                pPath.AddLine(clientRect.Right, clientRect.Top + Integer(GetRadiusY(pRadius.RightTop^)),
                        clientRect.Right, clientRect.Bottom - Integer(GetRadiusY(pRadius.RightBottom^)));

            // right bottom radius is set?
            if (not pRadius.RightBottom.IsZero) then
                // add rounded right bottom corner to path
                AddRightBottomCornerToPath(TWRectF.Create(clientRect), pRadius.RightBottom^, pPath);
        end;
    else
        TWLogHelper.LogToCompiler('GDI+ renderer - CreateCartoonBubble - unknown orientation - '
                + IntToStr(Integer(pTail.Orientation)));
        Exit(False);
    end;

    // finalize balloon hint figure
    pPath.CloseFigure;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.CreateRoundedRect(const rect: TWRectF; const pRadius: TWRenderer.IRadius;
        pPath: TGpGraphicsPath): Boolean;
begin
    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    // create round rectangle path (by assembling some graphic figures and closing all of them)
    AddLeftTopCornerToPath(rect, pRadius.LeftTop^, pPath);
    AddRightTopCornerToPath(rect, pRadius.RightTop^, pPath);
    AddRightBottomCornerToPath(rect, pRadius.RightBottom^, pPath);
    AddLeftBottomCornerToPath(rect, pRadius.LeftBottom^, pPath);
    pPath.CloseFigure;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.CreateRoundedRectDS(const rect: TWRectF; const pRadius: TWRenderer.IRadius;
        reset: Boolean; pPath: TGpGraphicsPath): Boolean;
var
    leftTopDiamX,
    leftTopDiamY,
    leftBottomDiamX,
    leftBottomDiamY,
    rightTopDiamX,
    rightTopDiamY,
    rightBottomDiamX,
    rightBottomDiamY:    Integer;
    leftTopCorner,
    leftBottomCorner,
    rightTopCorner,
    rightBottomCorner,
    r:                   TGpRect;
    doShortenRectWidth,
    doShortenRectHeight: Boolean;
begin
    if (not Assigned(pRadius)) then
        Exit(False);

    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    // convert vcl rect to gdi+ rect
    r := rect.ToGpRect(False);

    // get corner diameter
    leftTopDiamX     := Min(Max((GetRadiusX(pRadius.LeftTop^)     * 2), 1), r.Width);
    leftTopDiamY     := Min(Max((GetRadiusY(pRadius.LeftTop^)     * 2), 1), r.Height);
    leftBottomDiamX  := Min(Max((GetRadiusX(pRadius.LeftBottom^)  * 2), 1), r.Width);
    leftBottomDiamY  := Min(Max((GetRadiusY(pRadius.LeftBottom^)  * 2), 1), r.Height);
    rightTopDiamX    := Min(Max((GetRadiusX(pRadius.RightTop^)    * 2), 1), r.Width);
    rightTopDiamY    := Min(Max((GetRadiusY(pRadius.RightTop^)    * 2), 1), r.Height);
    rightBottomDiamX := Min(Max((GetRadiusX(pRadius.RightBottom^) * 2), 1), r.Width);
    rightBottomDiamY := Min(Max((GetRadiusY(pRadius.RightBottom^) * 2), 1), r.Height);

    // create corners
    leftTopCorner.X          := r.X;
    leftTopCorner.Y          := r.Y;
    leftTopCorner.Width      := leftTopDiamX;
    leftTopCorner.Height     := leftTopDiamY;
    leftBottomCorner.X       := r.X;
    leftBottomCorner.Y       := r.Y;
    leftBottomCorner.Width   := leftBottomDiamX;
    leftBottomCorner.Height  := leftBottomDiamY;
    rightTopCorner.X         := r.X;
    rightTopCorner.Y         := r.Y;
    rightTopCorner.Width     := rightTopDiamX;
    rightTopCorner.Height    := rightTopDiamY;
    rightBottomCorner.X      := r.X;
    rightBottomCorner.Y      := r.Y;
    rightBottomCorner.Width  := rightBottomDiamX;
    rightBottomCorner.Height := rightBottomDiamY;

    // begin path
    if (reset) then
        pPath.Reset;

    // top left
    pPath.AddArc(leftTopCorner, 180, 90);

    doShortenRectWidth  := False;
    doShortenRectHeight := False;

    if (IsRegular(pRadius)) then
    begin
        // tweak needed for left bottom radius of 10 (diameter of 20)
        if (leftBottomDiamY = 20) then
        begin
            leftBottomCorner.Height := leftBottomCorner.Height + 1;
            doShortenRectHeight     := True;
        end;

        // tweak needed for right top radius of 10 (diameter of 20)
        if (rightTopDiamX = 20) then
        begin
            rightTopCorner.Width := rightTopCorner.Width + 1;
            doShortenRectWidth   := True;
        end;

        // tweak needed for right bottom radius X of 10 (diameter of 20)
        if (rightBottomDiamX = 20) then
        begin
            rightBottomCorner.Width := rightBottomCorner.Width + 1;
            doShortenRectWidth      := True;
        end;

        // tweak needed for right bottom radius Y of 10 (diameter of 20)
        if (rightBottomDiamY = 20) then
        begin
            rightBottomCorner.Height := rightBottomCorner.Height + 1;
            doShortenRectHeight      := True;
        end;
    end;

    // do shorten rect width?
    if (doShortenRectWidth) then
        r.Width := r.Width - 1;

    // do shorten rect height?
    if (doShortenRectHeight) then
        r.Height := r.Height - 1;

    // top right
    rightTopCorner.X := rightTopCorner.X + (r.Width - rightTopDiamX - 1);
    pPath.AddArc(rightTopCorner, 270, 90);

    // bottom right
    rightBottomCorner.X := rightBottomCorner.X + (r.Width  - rightBottomDiamX - 1);
    rightBottomCorner.Y := rightBottomCorner.Y + (r.Height - rightBottomDiamY - 1);
    pPath.AddArc(rightBottomCorner, 0, 90);

    // bottom left
    leftBottomCorner.Y := leftBottomCorner.Y + (r.Height - leftBottomDiamY - 1);
    pPath.AddArc(leftBottomCorner, 90, 90);

    // end path
    pPath.CloseFigure;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRoundedRect(const rect: TWRectF; const pOptions: TWRenderer.IRectOptions;
        pPen: TGpPen; const pMatrix: TGpMatrix; pGraphics: TGpGraphics;
        out iRect: TRect): Boolean;
var
    doTransformRect, doFill:     Boolean;
    maxSize, width, i:           Cardinal;
    oldPenWidth:                 Single;
    oldPenAlignment:             TPenAlignment;
    oldPageUnit:                 TUnit;
    pPath, pFillPath, pClipPath: IWSmartPointer<TGpGraphicsPath>;
    pClipRegion:                 TGpRegion;
    color:                       TGpColor;
    brushColor:                  TWColor;
    pBrush:                      TGpBrush;
    paBrush:                     IWSmartPointer<TGpBrush>;
begin
    if (not Assigned(pOptions)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no GDI+ graphics?
    if (not Assigned(pGraphics)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no pen?
    if (not Assigned(pPen)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // enable antialiasing, if needed
    if (pOptions.AntiAliasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias)
    else
        pGraphics.SetSmoothingMode(SmoothingModeNone);

    iRect := rect.ToTRect(True);

    doTransformRect := Assigned(pMatrix) and not pMatrix.IsIdentity;

    // calculate max possible outline width
    maxSize := Round(GetMaxSize(rect));

    // limit outline width to max
    width := Min(ConvertStrokeWidth(pOptions.Stroke), maxSize);

    oldPenWidth     := pPen.GetWidth;
    oldPenAlignment := pPen.GetAlignment;

    // save current page unit
    oldPageUnit := pGraphics.GetPageUnit;

    // set to pixel mode
    pGraphics.SetPageUnit(UnitPixel);

    try
        // configure pen
        pPen.SetWidth(1);
        pPen.SetAlignment(PenAlignmentCenter);

        // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
        // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
        // tries to auto-create the object, causing thus that the path is never drawn
        pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

        // get outline path
        CreateRoundedRectDS(rect, pOptions.Radius, True, pPath);

        doFill := False;

        if (width > 1) then
            for i := 1 to width - 1 do
            begin
                // left stroke
                iRect.Inflate(-1, 0);

                // get stroke path
                CreateRoundedRectDS(TWRectF.Create(iRect), pOptions.Radius, False, pPath);

                // up stroke
                iRect.Inflate(0, -1);

                // get stroke path
                CreateRoundedRectDS(TWRectF.Create(iRect), pOptions.Radius, False, pPath);

                // fill only if transformation is applied (because in this case the workaround above doesn't work)
                doFill := doTransformRect;
            end;

        // apply transformation matrix, if needed
        if (doTransformRect) then
            pGraphics.SetTransform(pMatrix);

        // do fill outline?
        if (doFill) then
        begin
            // create fill path. NOTE create explicitly the graphics path before keep it inside the
            // smart pointer, because otherwise the incorrect constructor is called while the smart
            // pointer tries to auto-create the object, causing thus that the path is never drawn
            pFillPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

            // get fill path
            CreateRoundedRectDS(TWRectF.Create(rect.Left + 1, rect.Top + 1, rect.Right - 1, rect.Bottom - 1),
                    pOptions.Radius, True, pFillPath);

            // create clipping path. NOTE create explicitly the graphics path before keep it inside the
            // smart pointer, because otherwise the incorrect constructor is called while the smart
            // pointer tries to auto-create the object, causing thus that the path is never drawn
            pClipPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

            // get clipping path
            CreateRoundedRectDS(TWRectF.Create(iRect.Left - 1, iRect.Top - 1, iRect.Right + 1, iRect.Bottom + 1),
                    pOptions.Radius, True, pClipPath);

            // apply clipping to avoid rect center is filled too
            pClipRegion := TGpRegion.Create(pClipPath);
            pGraphics.SetClip(pClipRegion, CombineModeXor);

            // get fill color from pen
            pPen.GetColor(color);
            brushColor := TWColor.Create(TAlphaColor(color));
            pBrush     := GetBrush(brushColor);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

            // fill outline (needed because in case the filling paths above can not fill the entire
            // outline area if rect is transformed by a matrix)
            pGraphics.FillPath(pBrush, pFillPath);

            // remove clipping path
            pGraphics.ResetClip;
        end;

        // draw round rect
        pGraphics.DrawPath(pPen, pPath);
    finally
        // restore page unit
        pGraphics.SetPageUnit(oldPageUnit);

        // restore previous pen values
        pPen.SetWidth(oldPenWidth);
        pPen.SetAlignment(oldPenAlignment);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.FillRoundedRect(const rect: TWRectF; const pOptions: TWRenderer.IRectOptions;
        pBrush: TGpBrush; pPen: TGpPen; const pMatrix: TGpMatrix; pGraphics: TGpGraphics;
        out iRect: TRect): Boolean;
var
    oldPageUnit:     TUnit;
    pPath:           IWSmartPointer<TGpGraphicsPath>;
    oldPenWidth:     Single;
    oldPenAlignment: TPenAlignment;
begin
    if (not Assigned(pOptions)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no GDI+ graphics to draw on?
    if (not Assigned(pGraphics)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no brush?
    if (not Assigned(pBrush)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no pen?
    if (not Assigned(pPen)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // save current page unit
    oldPageUnit := pGraphics.GetPageUnit;

    // set to pixel mode
    pGraphics.SetPageUnit(UnitPixel);

    try
        // get the corner path. NOTE create explicitly the graphics path before keep it inside the
        // smart pointer, because otherwise the incorrect constructor is called while the smart
        // pointer tries to auto-create the object, causing thus that the path is never drawn
        pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

        // get path
        CreateRoundedRectDS(rect, pOptions.Radius, True, pPath);

        // apply transformation matrix, if needed
        if (Assigned(pMatrix) and (not pMatrix.IsIdentity)) then
            pGraphics.SetTransform(pMatrix);

        // fill
        pGraphics.FillPath(pBrush, pPath);

        oldPenWidth     := pPen.GetWidth;
        oldPenAlignment := pPen.GetAlignment;

        try
            // configure pen
            pPen.SetWidth(1);
            pPen.SetAlignment(PenAlignmentCenter);

            // draw the outline last so it will be on top in case the color is different
            pGraphics.DrawPath(pPen, pPath);
        finally
            // restore pen configuration
            pPen.SetWidth(oldPenWidth);
            pPen.SetAlignment(oldPenAlignment);
        end;
    finally
        // restore page unit
        pGraphics.SetPageUnit(oldPageUnit);
    end;

    iRect  := rect.ToTRect(True);
    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawCartoonBubble(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
        hDC: THandle; out iRect: TRect): Boolean;
var
    drawRect:                                                      TWRectF;
    pGraphics:                                                     TGpGraphics;
    paGraphics:                                                    IWSmartPointer<TGpGraphics>;
    pGraphicPath:                                                  IWSmartPointer<TGpGraphicsPath>;
    pBrush:                                                        TGpBrush;
    paBrush:                                                       IWSmartPointer<TGpBrush>;
    pPen:                                                          TGpPen;
    paPen:                                                         IWSmartPointer<TGpPen>;
    pFillColor:                                                    PWColor;
    pDefaultColor:                                                 TWColor;
    oldPageUnit:                                                   TUnit;
    oldMode:                                                       CompositingMode;
    maxSize:                                                       Cardinal;
    drawGradient, drawOutline, fillWithOutline, forceAntialiasing: Boolean;
    pGradientFactory:                                              TWGDIPlusGradient;
    oldPenWidth:                                                   Single;
    oldPenAlignment:                                               TPenAlignment;
begin
    // no GDI+?
    if (not m_GDIPlusInitialized) then
        Exit(False);

    if (not Assigned(pOptions)) then
        Exit(False);

    // no device context?
    if (hDC = 0) then
        Exit(False);

    // get GDI+ graphics from device context
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    // enable antialiasing, if needed
    if (pOptions.AntiAliasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // check if gradient and outline should be drawn
    drawGradient := IsGradientVisible(pOptions.Fill);
    drawOutline  := IsOutlineVisible(pOptions.Stroke, pOptions.Outline, False);

    maxSize := Round(GetMaxSize(rect));

    // check if rect is completely filled by outline
    fillWithOutline := drawOutline and (Cardinal(ConvertStrokeWidth(pOptions.Stroke)) >= maxSize);

    // calculate real draw surface
    drawRect := CalculateDrawRect(rect, pOptions, true);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pGraphicPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // do draw outline?
    if ((not drawOutline) or fillWithOutline) then
    begin
        // drawing a round rectangle without border is a little special, because standard GDI+
        // curves algorithms used in paths draw asymmetrical corners if border is not drawn too, see:
        // http://www.codeproject.com/Articles/27228/A-class-for-creating-round-rectangles-in-GDI-with
        drawRect.Right  := drawRect.Right  - 1;
        drawRect.Bottom := drawRect.Bottom - 1;

        // save current page unit
        oldPageUnit := pGraphics.GetPageUnit;

        // save current alpha blending mode
        oldMode := pGraphics.GetCompositingMode;

        // set to pixel mode
        pGraphics.SetPageUnit(UnitPixel);

        try
            // create balloon hint path and get text rectangle
            if (not CreateCartoonBubble(drawRect, pOptions.Tail, pOptions.Radius, pGraphicPath, iRect)) then
                Exit(False);

            forceAntialiasing := False;

            // do draw gradient background?
            if (drawGradient and (not fillWithOutline)) then
            begin
                // do force antialiasing? (This is needed to draw round rectangle with transparent
                // color but without outlines, otherwise an unwished outline appears around rectangle)
                if (not pOptions.AntiAliasing) then
                begin
                    // force antialiasing
                    pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
                    forceAntialiasing := True;
                end;

                pGradientFactory := nil;

                try
                    pGradientFactory := TWGDIPlusGradient.Create;

                    // get linear gradient brush
                    pBrush := m_pCache.GetBrush(pOptions.Fill, pGradientFactory);
                finally
                    pGradientFactory.Free;
                end;

                // is cache used? If not keep pointer in a smart pointer to auto-delete object on
                // function ends
                if (not g_GDIPlusCacheController.m_Brushes) then
                    paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

                // found it?
                if (not Assigned(pBrush)) then
                    Exit(False);

                // use antialiasing?
                if (not forceAntialiasing and (not pOptions.AntiAliasing)) then
                begin
                    // get pen to draw rect outline
                    pPen := m_pCache.GetPen(pOptions.Stroke, pBrush);

                    // is cache used? If not keep pointer in a smart pointer to auto-delete object
                    // on function ends
                    if (not g_GDIPlusCacheController.m_Pens) then
                        paPen := TWSmartPointer<TGpPen>.Create(pPen);

                    oldPenWidth     := pPen.GetWidth;
                    oldPenAlignment := pPen.GetAlignment;

                    try
                        pPen.SetWidth(1);
                        pPen.SetAlignment(PenAlignmentCenter);

                        // fill path
                        pGraphics.FillPath(pBrush, pGraphicPath);

                        // change alpha blending mode
                        pGraphics.SetCompositingMode(CompositingModeSourceCopy);

                        // draw outline last so it will be on top
                        pGraphics.DrawPath(pPen, pGraphicPath);
                    finally
                        // restore pen configuration
                        pPen.SetWidth(oldPenWidth);
                        pPen.SetAlignment(oldPenAlignment);
                    end;
                end
                else
                    // fill path
                    pGraphics.FillPath(pBrush, pGraphicPath);
            end
            else
            begin
                pFillColor := nil;

                // get fill color
                if (fillWithOutline) then
                begin
                    if (pOptions.Stroke.BrushType = E_BT_Solid) then
                        pFillColor := (pOptions.Stroke.Brush as TWSolidBrush).Color;
                end
                else
                begin
                    if (pOptions.Stroke.BrushType = E_BT_Solid) then
                        pFillColor := (pOptions.Fill.Brush as TWSolidBrush).Color;
                end;

                if (not Assigned(pFillColor)) then
                begin
                    pDefaultColor := TWColor.GetDefault;
                    pFillColor    := @pDefaultColor;
                end;

                // do force antialiasing? (This is needed to draw round rectangle with transparent
                // color but without outline, otherwise an unwished outline appears around rectangle)
                if (not pOptions.AntiAliasing and (pFillColor.GetAlpha <> 255)) then
                begin
                    // force antialiasing
                    pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
                    forceAntialiasing := True;
                end;

                // get solid brush
                pBrush := GetBrush(pFillColor^);

                // is cache used? If not keep pointer in a smart pointer to auto-delete object on
                // function ends
                if (not g_GDIPlusCacheController.m_Brushes) then
                    paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

                // use antialiasing?
                if (not forceAntialiasing and (not pOptions.AntiAliasing)) then
                begin
                    // get pen to draw rect outline
                    pPen := m_pCache.GetPen(pOptions.Stroke, pBrush);

                    // is cache used? If not keep pointer in a smart pointer to auto-delete object
                    // on function ends
                    if (not g_GDIPlusCacheController.m_Pens) then
                        paPen := TWSmartPointer<TGpPen>.Create(pPen);

                    oldPenWidth     := pPen.GetWidth;
                    oldPenAlignment := pPen.GetAlignment;

                    try
                        pPen.SetWidth(1);
                        pPen.SetAlignment(PenAlignmentCenter);

                        // fill path
                        pGraphics.FillPath(pBrush, pGraphicPath);

                        // change alpha blending mode
                        pGraphics.SetCompositingMode(CompositingModeSourceCopy);

                        // draw outline last so it will be on top
                        pGraphics.DrawPath(pPen, pGraphicPath);
                    finally
                        // restore pen configuration
                        pPen.SetWidth(oldPenWidth);
                        pPen.SetAlignment(oldPenAlignment);
                    end;
                end
                else
                    // fill path
                    pGraphics.FillPath(pBrush, pGraphicPath);
            end;
        finally
            // restore previous alpha blending mode
            pGraphics.SetCompositingMode(oldMode);

            // restore page unit
            pGraphics.SetPageUnit(oldPageUnit);
        end;

        Exit(True);
    end;

    // create balloon hint path and get text rectangle
    if (not CreateCartoonBubble(drawRect, pOptions.Tail, pOptions.Radius, pGraphicPath, iRect)) then
        Exit(False);

    pGradientFactory := nil;

    try
        // get linear gradient brush
        pBrush := m_pCache.GetBrush(pOptions.Fill, pGradientFactory);
    finally
        pGradientFactory.Free;
    end;

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

    // found it?
    if (not Assigned(pBrush)) then
        Exit(False);

    // draw rectangle background
    pGraphics.FillPath(pBrush, pGraphicPath);

    pGradientFactory := nil;

    try
        // get pen to draw rect border
        pPen := m_pCache.GetPen(pOptions.Stroke, pGradientFactory);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Pens) then
            paPen := TWSmartPointer<TGpPen>.Create(pPen);
    finally
        pGradientFactory.Free;
    end;

    // do draw all borders?
    if (IsCompletelyVisible(pOptions.Outline)) then
        // draw rectangle border
        pGraphics.DrawPath(pPen, pGraphicPath)
    else
    begin
        // do draw left top corner?
        if (pOptions.Outline.ShowLeftTop and (not pOptions.Radius.LeftTop.IsZero)) then
            // draw left top corner
            DrawLeftTopCorner(pPen, TWRectF.Create(iRect), pOptions.Radius.LeftTop^, pGraphics);

        // do draw left bottom corner?
        if (pOptions.Outline.ShowLeftBottom and (not pOptions.Radius.LeftBottom.IsZero)) then
            // draw left bottom corner
            DrawLeftBottomCorner(pPen, TWRectF.Create(iRect), pOptions.Radius.LeftBottom^, pGraphics);

        // do draw right top corner?
        if (pOptions.Outline.ShowRightTop and (not pOptions.Radius.RightTop.IsZero)) then
            // draw right top corner
            DrawRightTopCorner(pPen, TWRectF.Create(iRect), pOptions.Radius.RightTop^, pGraphics);

        // do draw right bottom corner?
        if (pOptions.Outline.ShowRightBottom and (not pOptions.Radius.RightBottom.IsZero)) then
            // draw right bottom corner
            DrawRightBottomCorner(pPen, TWRectF.Create(iRect), pOptions.Radius.RightBottom^, pGraphics);

        // do draw left border?
        if (pOptions.Outline.ShowLeft) then
            // is tail visible on left edge?
            if (pOptions.Tail.Visible and (pOptions.Tail.Orientation = E_O_Left)) then
            begin
                // draw left tail
                if (not DrawLeftTail(pPen, drawRect.ToTRect(True), pOptions.Tail, pOptions.Radius,
                        pGraphics))
                then
                    Exit(False);
            end
            else
                // draw left border
                pGraphics.DrawLine(pPen, iRect.Left, iRect.Top + Integer(GetRadiusY(pOptions.Radius.LeftTop^)),
                        iRect.Left, iRect.Bottom - Integer(GetRadiusY(pOptions.Radius.LeftBottom^)));

        if (pOptions.Outline.ShowTop) then
            // is tail visible on top edge?
            if (pOptions.Tail.Visible and (pOptions.Tail.Orientation = E_O_Top)) then
            begin
                // draw top tail
                if (not DrawTopTail(pPen, drawRect.ToTRect(True), pOptions.Tail, pOptions.Radius,
                        pGraphics))
                then
                    Exit(False);
            end
            else
                // draw top border
                pGraphics.DrawLine(pPen, iRect.Left + Integer(GetRadiusX(pOptions.Radius.LeftTop^)),
                        iRect.Top, iRect.Right - Integer(GetRadiusX(pOptions.Radius.RightTop^)),
                        iRect.Top);

        if (pOptions.Outline.ShowRight) then
            // is tail visible on right edge?
            if (pOptions.Tail.Visible and (pOptions.Tail.Orientation = E_O_Right)) then
            begin
                // draw right tail
                if (not DrawRightTail(pPen, drawRect.ToTRect(True), pOptions.Tail, pOptions.Radius,
                        pGraphics))
                then
                    Exit(False);
            end
            else
                // draw right border
                pGraphics.DrawLine(pPen, iRect.Right, iRect.Top + Integer(GetRadiusY(pOptions.Radius.RightTop^)),
                        iRect.Right, iRect.Bottom - Integer(GetRadiusY(pOptions.Radius.RightBottom^)));

        if (pOptions.Outline.ShowBottom) then
            // is tail visible on bottom edge?
            if (pOptions.Tail.Visible and (pOptions.Tail.Orientation = E_O_Bottom)) then
            begin
                // draw bottom tail
                if (not DrawBottomTail(pPen, drawRect.ToTRect(True), pOptions.Tail, pOptions.Radius,
                        pGraphics))
                then
                    Exit(False);
            end
            else
                // draw bottom border
                pGraphics.DrawLine(pPen, iRect.Left + Integer(GetRadiusX(pOptions.Radius.LeftBottom^)),
                        iRect.Bottom, iRect.Right - Integer(GetRadiusX(pOptions.Radius.RightBottom^)),
                        iRect.Bottom);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLeftTopCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
        pGraphics: TGpGraphics): Boolean;
var
    radiusX, radiusY:                                   Single;
    leftTopRect:                                        TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate left top corner rectangle
    leftTopRect.Left   := rect.Left;
    leftTopRect.Top    := rect.Top;
    leftTopRect.Right  := rect.Left + radiusX;
    leftTopRect.Bottom := rect.Top  + radiusY;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := leftTopRect.Width;
        height := leftTopRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := leftTopRect.Left;
        startPoint.Y   := leftTopRect.Top  + height;
        endPoint.X     := leftTopRect.Left + width;
        endPoint.Y     := leftTopRect.Top;
        startControl.X := leftTopRect.Left;
        startControl.Y := leftTopRect.Top  + (height / Bezier_Curve_Ratio);
        endControl.X   := leftTopRect.Left + (width  / Bezier_Curve_Ratio);
        endControl.Y   := leftTopRect.Top;

        // draw corner
        pGraphics.DrawBezier(pPen, startPoint, startControl, endControl, endPoint);
    {$else}
        // draw corner
        pGraphics.DrawArc(pPen, leftTopRect.Left, leftTopRect.Top, leftTopRect.Width,
                leftTopRect.Height, 180.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRightTopCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
        pGraphics: TGpGraphics): Boolean;
var
    radiusX, radiusY:                                   Single;
    rightTopRect:                                       TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate right top corner rectangle
    rightTopRect.Left   := rect.Right - radiusX;
    rightTopRect.Top    := rect.Top;
    rightTopRect.Right  := rect.Right;
    rightTopRect.Bottom := rect.Top   + radiusY;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := rightTopRect.Width;
        height := rightTopRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := rightTopRect.Left;
        startPoint.Y   := rightTopRect.Top;
        endPoint.X     := rightTopRect.Left + width;
        endPoint.Y     := rightTopRect.Top  + height;
        startControl.X := rightTopRect.Left + (width / Bezier_Curve_Ratio);
        startControl.Y := rightTopRect.Top;
        endControl.X   := rightTopRect.Left + width;
        endControl.Y   := rightTopRect.Top  + (height / Bezier_Curve_Ratio);

        // draw corner
        pGraphics.DrawBezier(pPen, startPoint, startControl, endControl, endPoint);
    {$else}
        // draw corner
        pGraphics.DrawArc(pPen, rightTopRect.Left, rightTopRect.Top, rightTopRect.Width,
                rightTopRect.Height, 270.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLeftBottomCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
        pGraphics: TGpGraphics): Boolean;
var
    radiusX, radiusY:                                   Single;
    leftBottomRect:                                     TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate left bottom corner rectangle
    leftBottomRect.Left   := rect.Left;
    leftBottomRect.Top    := rect.Bottom - radiusY;
    leftBottomRect.Right  := rect.Left   + radiusX;
    leftBottomRect.Bottom := rect.Bottom;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := leftBottomRect.Width;
        height := leftBottomRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := leftBottomRect.Left + width;
        startPoint.Y   := leftBottomRect.Top  + height;
        endPoint.X     := leftBottomRect.Left;
        endPoint.Y     := leftBottomRect.Top;
        startControl.X := leftBottomRect.Left + (width  / Bezier_Curve_Ratio);
        startControl.Y := leftBottomRect.Top  +  height;
        endControl.X   := leftBottomRect.Left;
        endControl.Y   := leftBottomRect.Top  + (height / Bezier_Curve_Ratio);

        // draw corner
        pGraphics.DrawBezier(pPen, startPoint, startControl, endControl, endPoint);
    {$else}
        // draw corner
        pGraphics.DrawArc(pPen, leftBottomRect.Left, leftBottomRect.Top, leftBottomRect.Width,
                leftBottomRect.Height, 90.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRightBottomCorner(pPen: TGpPen; const rect: TWRectF; const radius: TPoint;
        pGraphics: TGpGraphics): Boolean;
var
    radiusX, radiusY:                                   Single;
    rightBottomRect:                                    TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate right bottom corner rectangle
    rightBottomRect.Left   := rect.Right  - radiusX;
    rightBottomRect.Top    := rect.Bottom - radiusY;
    rightBottomRect.Right  := rect.Right;
    rightBottomRect.Bottom := rect.Bottom;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := rightBottomRect.Width;
        height := rightBottomRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := rightBottomRect.Left + width;
        startPoint.Y   := rightBottomRect.Top;
        endPoint.X     := rightBottomRect.Left;
        endPoint.Y     := rightBottomRect.Top  +  height;
        startControl.X := rightBottomRect.Left +  width;
        startControl.Y := rightBottomRect.Top  + (height / Bezier_Curve_Ratio);
        endControl.X   := rightBottomRect.Left + (width  / Bezier_Curve_Ratio);
        endControl.Y   := rightBottomRect.Top  +  height;

        // draw corner
        pGraphics.DrawBezier(pPen, startPoint, startControl, endControl, endPoint);
    {$else}
        // draw corner
        pGraphics.DrawArc(pPen, rightBottomRect.Left, rightBottomRect.Top, rightBottomRect.Width,
                rightBottomRect.Height, 0.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLeftTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean;
var
    pPath: IWSmartPointer<TGpGraphicsPath>;
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get path to draw
    if (not AddLeftTailToPath(rect, pTail, pRadius, pPath)) then
        Exit(False);

    // draw path
    pGraphics.DrawPath(pPen, pPath);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawTopTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean;
var
    pPath: IWSmartPointer<TGpGraphicsPath>;
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get path to draw
    if (not AddTopTailToPath(rect, pTail, pRadius, pPath))  then
        Exit(False);

    // draw path
    pGraphics.DrawPath(pPen, pPath);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRightTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean;
var
    pPath: IWSmartPointer<TGpGraphicsPath>;
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get path to draw
    if (not AddRightTailToPath(rect, pTail, pRadius, pPath)) then
        Exit(False);

    // draw path
    pGraphics.DrawPath(pPen, pPath);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawBottomTail(pPen: TGpPen; const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pGraphics: TGpGraphics): Boolean;
var
    pPath: IWSmartPointer<TGpGraphicsPath>;
begin
    if (not Assigned(pPen)) then
        Exit(False);

    if (not Assigned(pGraphics)) then
        Exit(False);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get path to draw
    if (not AddBottomTailToPath(rect, pTail, pRadius, pPath)) then
        Exit(False);

    // draw path
    pGraphics.DrawPath(pPen, pPath);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddLeftTopCornerToPath(const rect: TWRectF; const radius: TPoint;
        pPath: TGpGraphicsPath): Boolean;
var
    radiusX, radiusY:                                   Single;
    leftTopRect:                                        TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPath)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate left top corner rectangle
    leftTopRect.Left   := rect.Left;
    leftTopRect.Top    := rect.Top;
    leftTopRect.Right  := rect.Left + radiusX;
    leftTopRect.Bottom := rect.Top  + radiusY;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := leftTopRect.Width;
        height := leftTopRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := leftTopRect.Left;
        startPoint.Y   := leftTopRect.Top  + height;
        endPoint.X     := leftTopRect.Left + width;
        endPoint.Y     := leftTopRect.Top;
        startControl.X := leftTopRect.Left;
        startControl.Y := leftTopRect.Top  + (height / Bezier_Curve_Ratio);
        endControl.X   := leftTopRect.Left + (width  / Bezier_Curve_Ratio);
        endControl.Y   := leftTopRect.Top;

        // add corner to path
        pPath.AddBezier(startPoint, startControl, endControl, endPoint);
    {$else}
        // add corner to path
        pPath.AddArc(leftTopRect.Left, leftTopRect.Top, leftTopRect.Width, leftTopRect.Height,
                180.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddRightTopCornerToPath(const rect: TWRectF; const radius: TPoint;
        pPath: TGpGraphicsPath): Boolean;
var
    radiusX, radiusY:                                   Single;
    rightTopRect:                                       TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPath)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate right top corner rectangle
    rightTopRect.Left   := rect.Right - radiusX;
    rightTopRect.Top    := rect.Top;
    rightTopRect.Right  := rect.Right;
    rightTopRect.Bottom := rect.Top   + radiusY;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := rightTopRect.Width;
        height := rightTopRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := rightTopRect.Left;
        startPoint.Y   := rightTopRect.Top;
        endPoint.X     := rightTopRect.Left +  width;
        endPoint.Y     := rightTopRect.Top  +  height;
        startControl.X := rightTopRect.Left + (width / Bezier_Curve_Ratio);
        startControl.Y := rightTopRect.Top;
        endControl.X   := rightTopRect.Left +  width;
        endControl.Y   := rightTopRect.Top  + (height / Bezier_Curve_Ratio);

        // add corner to path
        pPath.AddBezier(startPoint, startControl, endControl, endPoint);
    {$else}
        // add corner to path
        pPath.AddArc(rightTopRect.Left, rightTopRect.Top, rightTopRect.Width, rightTopRect.Height,
                270.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddLeftBottomCornerToPath(const rect: TWRectF; const radius: TPoint;
        pPath: TGpGraphicsPath): Boolean;
var
    radiusX, radiusY:                                   Single;
    leftBottomRect:                                     TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPath)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate left bottom corner rectangle
    leftBottomRect.Left   := rect.Left;
    leftBottomRect.Top    := rect.Bottom - radiusY;
    leftBottomRect.Right  := rect.Left   + radiusX;
    leftBottomRect.Bottom := rect.Bottom;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := leftBottomRect.Width;
        height := leftBottomRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := leftBottomRect.Left + width;
        startPoint.Y   := leftBottomRect.Top  + height;
        endPoint.X     := leftBottomRect.Left;
        endPoint.Y     := leftBottomRect.Top;
        startControl.X := leftBottomRect.Left + (width  / Bezier_Curve_Ratio);
        startControl.Y := leftBottomRect.Top  +  height;
        endControl.X   := leftBottomRect.Left;
        endControl.Y   := leftBottomRect.Top  + (height / Bezier_Curve_Ratio);

        // add corner to path
        pPath.AddBezier(startPoint, startControl, endControl, endPoint);
    {$else}
        // add corner to path
        pPath.AddArc(leftBottomRect.Left, leftBottomRect.Top, leftBottomRect.Width,
                leftBottomRect.Height, 90.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddRightBottomCornerToPath(const rect: TWRectF; const radius: TPoint;
        pPath: TGpGraphicsPath): Boolean;
var
    radiusX, radiusY:                                   Single;
    rightBottomRect:                                    TWRectF;
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        width, height:                                  Single;
        startPoint, endPoint, startControl, endControl: TGpPointF;
    {$endif}
begin
    if (not Assigned(pPath)) then
        Exit(False);

    // get radius x and y values, and limit radius to highest rect size half value
    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0);
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0);
    {$else}
        radiusX := Min(GetRadiusX(radius), rect.Width  / 2.0) * 2.0;
        radiusY := Min(GetRadiusY(radius), rect.Height / 2.0) * 2.0;
    {$endif}

    // calculate right bottom corner rectangle
    rightBottomRect.Left   := rect.Right  - radiusX;
    rightBottomRect.Top    := rect.Bottom - radiusY;
    rightBottomRect.Right  := rect.Right;
    rightBottomRect.Bottom := rect.Bottom;

    {$ifdef DRAW_GDIPLUS_ROUND_RECT_USING_BEZIER_CURVES}
        // calculate rect half width and height
        width  := rightBottomRect.Width;
        height := rightBottomRect.Height;

        // calculate Bezier curve start, end and controls points
        startPoint.X   := rightBottomRect.Left +  width;
        startPoint.Y   := rightBottomRect.Top;
        endPoint.X     := rightBottomRect.Left;
        endPoint.Y     := rightBottomRect.Top  +  height;
        startControl.X := rightBottomRect.Left +  width;
        startControl.Y := rightBottomRect.Top  + (height / Bezier_Curve_Ratio);
        endControl.X   := rightBottomRect.Left + (width  / Bezier_Curve_Ratio);
        endControl.Y   := rightBottomRect.Top  +  height;

        // add corner to path
        pPath.AddBezier(startPoint, startControl, endControl, endPoint);
    {$else}
        // add corner to path
        pPath.AddArc(rightBottomRect.Left, rightBottomRect.Top, rightBottomRect.Width,
                rightBottomRect.Height, 0.0, 90.0);
    {$endif}

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddLeftTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean;
var
    tailPosX, tailPosY: Integer;
    tailOffset:         Single;
begin
    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    tailOffset := (rect.Height - Integer(GetRadiusY(pRadius.LeftTop^) + GetRadiusY(pRadius.LeftBottom^))
            - Integer(pTail.Width * 2));

    // calculate tail positions
    tailPosX := rect.Left + Integer(pTail.Height);
    tailPosY := rect.Top  + Integer(GetRadiusY(pRadius.LeftTop^) + pTail.Width) + Round(pTail.Position * tailOffset);

    // create tail
    pPath.AddLine(tailPosX, rect.Bottom - Integer(GetRadiusY(pRadius.LeftBottom^)), tailPosX,
            tailPosY + Integer(pTail.Width));
    pPath.AddLine(tailPosX, tailPosY + Integer(pTail.Width), rect.Left, tailPosY);
    pPath.AddLine(rect.Left, tailPosY, tailPosX, tailPosY - Integer(pTail.Width));
    pPath.AddLine(tailPosX, rect.Top + Integer(GetRadiusY(pRadius.LeftTop^)), tailPosX,
            tailPosY - Integer(pTail.Width));

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddTopTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean;
var
    tailPosX, tailPosY: Integer;
    tailOffset:         Single;
begin
    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    tailOffset := (rect.Width - Integer(GetRadiusX(pRadius.LeftTop^) + GetRadiusX(pRadius.RightTop^))
            - Integer(pTail.Width * 2));

    // calculate tail position
    tailPosX := rect.Left + Integer(GetRadiusX(pRadius.LeftTop^) + pTail.Width) + Round(pTail.Position * tailOffset);
    tailPosY := rect.Top  + Integer(pTail.Height);

    // create tail
    pPath.AddLine(rect.Left + Integer(GetRadiusX(pRadius.LeftTop^)), tailPosY,
            tailPosX - Integer(pTail.Width), tailPosY);
    pPath.AddLine(tailPosX - Integer(pTail.Width), tailPosY, tailPosX, rect.Top);
    pPath.AddLine(tailPosX, rect.Top, tailPosX + Integer(pTail.Width), tailPosY);
    pPath.AddLine(tailPosX + Integer(pTail.Width), tailPosY,
            rect.Right - Integer(GetRadiusX(pRadius.RightTop^)), tailPosY);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddRightTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean;
var
    tailPosX, tailPosY: Integer;
    tailOffset:         Single;
begin
    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    tailOffset := (rect.Height - Integer(GetRadiusY(pRadius.RightTop^) + GetRadiusY(pRadius.RightBottom^))
            - Integer(pTail.Width * 2));

    // calculate tail position
    tailPosX := rect.Right - Integer(pTail.Height);
    tailPosY := rect.Top   + Integer(GetRadiusY(pRadius.RightTop^) + pTail.Width)
            + Round(pTail.Position * tailOffset);

    // create tail
    pPath.AddLine(tailPosX, rect.Top + Integer(GetRadiusY(pRadius.RightTop^)), tailPosX,
            tailPosY - Integer(pTail.Width));
    pPath.AddLine(tailPosX, tailPosY - Integer(pTail.Width), rect.Right, tailPosY);
    pPath.AddLine(rect.Right, tailPosY, tailPosX, tailPosY + Integer(pTail.Width));
    pPath.AddLine(tailPosX, tailPosY + Integer(pTail.Width), tailPosX,
            rect.Bottom - Integer(GetRadiusY(pRadius.RightBottom^)));

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddBottomTailToPath(const rect: TRect; const pTail: TWRenderer.ITailOptions;
        const pRadius: TWRenderer.IRadius; pPath: TGpGraphicsPath): Boolean;
var
    tailPosX, tailPosY: Integer;
    tailOffset:         Single;
begin
    // no path to fill?
    if (not Assigned(pPath)) then
        Exit(False);

    tailOffset := (rect.Width - Integer(GetRadiusX(pRadius.LeftBottom^) + GetRadiusX(pRadius.RightBottom^))
            - Integer(pTail.Width * 2));

    // calculate tail position
    tailPosX := rect.Left   + (Integer(GetRadiusX(pRadius.LeftBottom^) + pTail.Width))
            + Round(pTail.Position * tailOffset);
    tailPosY := rect.Bottom - Integer(pTail.Height);

    // create tail
    pPath.AddLine(rect.Right - Integer(GetRadiusX(pRadius.RightBottom^)), tailPosY,
            tailPosX + Integer(pTail.Width), tailPosY);
    pPath.AddLine(tailPosX + Integer(pTail.Width), tailPosY, tailPosX, rect.Bottom);
    pPath.AddLine(tailPosX, rect.Bottom, tailPosX - Integer(pTail.Width), tailPosY);
    pPath.AddLine(tailPosX - Integer(pTail.Width), tailPosY,
            rect.Left + Integer(GetRadiusX(pRadius.LeftBottom^)), tailPosY);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.DrawTransparentRoundRectWithoutOutline(hDC: THandle; const rect: TWRectF;
        const pOptions: TWRenderer.IRectOptions; fillWithOutline: Boolean; pPath: TGpGraphicsPath);
var
    pGdiPlusMatrix:                                                TGpMatrix;
    pMaskBrush:                                                    TGpBrush;
    pMaskPen:                                                      TGpPen;
    whiteColor:                                                    TWColor;
    width, height, x, y, rectLeft, rectTop, rectWidth, rectHeight: Integer;
    doApplyTransform:                                              Boolean;
    pColorBitmap, pMaskBitmap, pGeometryBitmap:                    IWSmartPointer<Vcl.Graphics.TBitmap>;
    pColorGraphics, pMaskGraphics:                                 IWSmartPointer<TGpGraphics>;
    paMaskBrush:                                                   IWSmartPointer<TGpBrush>;
    paMaskPen:                                                     IWSmartPointer<TGpPen>;
    pFill:                                                         IWSmartPointer<TWFill>;
    pMaskLine, pColorLine, pGeometryLine:                          PWRGBQuadArray;
    blendFunction:                                                 TBlendFunction;
    oldPenWidth:                                                   Single;
    oldPenAlignment:                                               TPenAlignment;
begin
    // no device context?
    if (hDC = 0) then
        Exit;

    // no path?
    if (not Assigned(pPath)) then
        Exit;

    // check if transform matrix should be applied
    doApplyTransform := not pOptions.TransformMatrix.IsIdentity;

    pGdiPlusMatrix := nil;

    try
        // do apply transformation matrix?
        if (doApplyTransform) then
        begin
            pGdiPlusMatrix := TGpMatrix.Create;

            // convert transform matrix to gdi+ matrix
            pOptions.TransformMatrix.ToGpMatrix(pGdiPlusMatrix);
        end;

        // get width and height to use
        width  := Round(rect.Right);
        height := Round(rect.Bottom);

        // create and initialize color bitmap. This will contain the colors to apply to final bitmap
        pColorBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        pColorBitmap.PixelFormat := pf32bit;
        pColorBitmap.AlphaFormat := afPremultiplied;
        pColorBitmap.SetSize(width, height);
        TWGDIHelper.Clear(pColorBitmap);

        pColorGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pColorBitmap.Canvas.Handle));

        // do apply transformation matrix?
        if (doApplyTransform) then
            // apply transformation
            pColorGraphics.SetTransform(pGdiPlusMatrix);

        pFill := TWSmartPointer<TWFill>.Create();

        if (fillWithOutline) then
            pFill.SetBrush(pOptions.Stroke.Brush)
        else
            pFill.SetBrush(pOptions.Fill.Brush);

        // fill color bitmap
        if (not FillRectangle(rect, pFill, pColorGraphics)) then
            Exit;

        // create and initialize mask bitmap. This will contain the geometry to draw
        pMaskBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        pMaskBitmap.PixelFormat := pf32bit;
        pMaskBitmap.AlphaFormat := afPremultiplied;
        pMaskBitmap.SetSize(width, height);
        TWGDIHelper.Clear(pMaskBitmap);

        whiteColor := TWColor.Create(255, 255, 255, 255);

        // get mask GDI+ graphics and create mask brush
        pMaskGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pMaskBitmap.Canvas.Handle));
        pMaskBrush    := GetBrush(whiteColor);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Brushes) then
            paMaskBrush := TWSmartPointer<TGpBrush>.Create(pMaskBrush);

        // get mask pen to draw geometry outline
        pMaskPen := m_pCache.GetPen(pOptions.Stroke, pMaskBrush);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Pens) then
            paMaskPen := TWSmartPointer<TGpPen>.Create(pMaskPen);

        oldPenWidth     := pMaskPen.GetWidth;
        oldPenAlignment := pMaskPen.GetAlignment;

        try
            pMaskPen.SetWidth(1);
            pMaskPen.SetAlignment(PenAlignmentCenter);

            // do apply transformation matrix?
            if (doApplyTransform) then
                // apply transformation
                pMaskGraphics.SetTransform(pGdiPlusMatrix);

            // fill path
            pMaskGraphics.FillPath(pMaskBrush, pPath);

            // change alpha blending mode
            pMaskGraphics.SetCompositingMode(CompositingModeSourceCopy);

            // draw outline last so it will be on top
            pMaskGraphics.DrawPath(pMaskPen, pPath);
        finally
            // restore pen configuration
            pMaskPen.SetWidth(oldPenWidth);
            pMaskPen.SetAlignment(oldPenAlignment);
        end;
    finally
        pGdiPlusMatrix.Free;
    end;

    // create and initialize geometry bitmap. This will contain the final geometry in true colors to
    // blend with device context
    pGeometryBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
    pGeometryBitmap.PixelFormat := pf32bit;
    pGeometryBitmap.AlphaFormat := afPremultiplied;
    pGeometryBitmap.SetSize(width, height);
    TWGDIHelper.Clear(pGeometryBitmap);

    rectLeft := Round(rect.Left);
    rectTop  := Round(rect.Top);

    // iterate through bitmap lines
    for y := Max(rectTop, 0) to height - 1 do
    begin
        // get lines
        pMaskLine     := PWRGBQuadArray(pMaskBitmap.ScanLine[y]);
        pColorLine    := PWRGBQuadArray(pColorBitmap.ScanLine[y]);
        pGeometryLine := PWRGBQuadArray(pGeometryBitmap.ScanLine[y]);

        // iterate through bitmap pixels
        for x := Max(rectLeft, 0) to width - 1 do
        begin
            // do apply color pixel to final geometry?
            if ((pMaskLine[x].rgbRed      = 0) and
                (pMaskLine[x].rgbGreen    = 0) and
                (pMaskLine[x].rgbBlue     = 0) and
                (pMaskLine[x].rgbReserved = 0))
            then
                continue;

            pGeometryLine[x].rgbRed      := pColorLine[x].rgbRed;
            pGeometryLine[x].rgbGreen    := pColorLine[x].rgbGreen;
            pGeometryLine[x].rgbBlue     := pColorLine[x].rgbBlue;
            pGeometryLine[x].rgbReserved := pColorLine[x].rgbReserved;
        end;
    end;

    // initialize blend operation
    blendFunction.BlendOp             := AC_SRC_OVER;
    blendFunction.BlendFlags          := 0;
    blendFunction.SourceConstantAlpha := 255;
    blendFunction.AlphaFormat         := AC_SRC_ALPHA;

    // get width and height to use
    rectWidth  := Round(rect.Width);
    rectHeight := Round(rect.Height);

    // copy geometry in true color to device context
    AlphaBlend(hDC, rectLeft, rectTop, rectWidth, rectHeight, pGeometryBitmap.Canvas.Handle,
            rectLeft, rectTop, rectWidth, rectHeight, blendFunction);
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.DrawNonAntialiasedSymmetricalRoundRect(hDC: THandle; const rect: TWRectF;
        const pOptions: TWRenderer.IRectOptions; out iRect: TRect);
var
    pMatrix:              TGpMatrix;
    pMaskBrush:           TGpBrush;
    pMaskPen:             TGpPen;
    whiteColor,
    bgColor,
    blendedColor:         TWColor;
    internalRect:         TWRectF;
    tempRect:             TRect;
    width,
    height,
    halfWidth,
    x,
    y,
    prevXOffset,
    nextXOffset,
    rectLeft,
    rectTop,
    rectWidth,
    rectHeight:           Integer;
    fillOpacity,
    strokeOpacity:        Single;
    doApplyTransform,
    fillPixel:            Boolean;
    paMaskBrush:          IWSmartPointer<TGpBrush>;
    paMaskPen:            TWSmartPointer<TGpPen>;
    pColorBitmap,
    pMaskBitmap,
    pOutlineMaskBitmap,
    pGeometryBitmap:      IWSmartPointer<Vcl.Graphics.TBitmap>;
    pColorGraphics,
    pMaskGraphics,
    pOutlineMaskGraphics: IWSmartPointer<TGpGraphics>;
    pMaskLine,
    pOutlineMaskLine,
    pColorLine,
    pGeometryLine:        PWRGBQuadArray;
    oldPenWidth:          Single;
    oldPenAlignment:      TPenAlignment;
    pFillBrush,
    pStrokeBrush:         TWSolidBrush;
    color:                TWColor;
begin
    // no device context?
    if (hDC = 0) then
    begin
        iRect := Default(TRect);
        Exit;
    end;

    // check if transform matrix should be applied
    doApplyTransform := not pOptions.TransformMatrix.IsIdentity;

    pMatrix := nil;

    try
        // do apply transformation matrix?
        if (doApplyTransform) then
        begin
            pMatrix := TGpMatrix.Create;

            // convert transform matrix to gdi+ matrix
            pOptions.TransformMatrix.ToGpMatrix(pMatrix);
        end;

        // get width and height to use
        width  := Round(rect.Right);
        height := Round(rect.Bottom);

        halfWidth := ConvertStrokeWidth(pOptions.Stroke) div 2;

        internalRect.Left   := rect.Left   + halfWidth;
        internalRect.Top    := rect.Top    + halfWidth;
        internalRect.Right  := rect.Right  - halfWidth;
        internalRect.Bottom := rect.Bottom - halfWidth;

        // create and initialize color bitmap. This will contain the colors to apply to final bitmap
        pColorBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        TWGDIHelper.ConfigBitmap(pColorBitmap, width, height, True, bmDIB, pf32bit, afDefined);
        TWGDIHelper.Clear(pColorBitmap);

        pColorGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pColorBitmap.Canvas.Handle));

        // do apply transformation matrix?
        if (doApplyTransform) then
            // apply transformation
            pColorGraphics.SetTransform(pMatrix);

        // fill color bitmap
        if (not FillRectangle(rect, pOptions.Fill, pColorGraphics)) then
            Exit;

        // create and initialize mask bitmap. This will contain the filled rect geometry to draw
        pMaskBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        pMaskBitmap.PixelFormat := pf32bit;
        pMaskBitmap.AlphaFormat := afDefined;
        pMaskBitmap.SetSize(width, height);
        TWGDIHelper.Clear(pMaskBitmap);

        whiteColor := TWColor.Create(255, 255, 255, 255);

        // get mask GDI+ graphics and create mask brush
        pMaskGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pMaskBitmap.Canvas.Handle));
        pMaskBrush    := GetBrush(whiteColor);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Brushes) then
            paMaskBrush := TWSmartPointer<TGpBrush>.Create(pMaskBrush);

        // get mask pen to draw geometry outline
        pMaskPen := m_pCache.GetPen(pOptions.Stroke, pMaskBrush);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Pens) then
            paMaskPen := TWSmartPointer<TGpPen>.Create(pMaskPen);

        oldPenWidth     := pMaskPen.GetWidth;
        oldPenAlignment := pMaskPen.GetAlignment;

        try
            pMaskPen.SetWidth(1);
            pMaskPen.SetAlignment(PenAlignmentCenter);

            // fill internal rect using Darren Session algorithm
            if (not FillRoundedRect(internalRect, pOptions, pMaskBrush, pMaskPen, pMatrix,
                    pMaskGraphics, tempRect))
            then
            begin
                iRect := Default(TRect);
                Exit;
            end;
        finally
            // restore pen configuration
            pMaskPen.SetWidth(oldPenWidth);
            pMaskPen.SetAlignment(oldPenAlignment);
        end;

        // create and initialize mask bitmap. This will contain the outlined rect geometry to draw
        pOutlineMaskBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        TWGDIHelper.ConfigBitmap(pOutlineMaskBitmap, width, height, True, bmDIB, pf32bit, afDefined);
        TWGDIHelper.Clear(pOutlineMaskBitmap);

        // get outline mask GDI+ graphics
        pOutlineMaskGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pOutlineMaskBitmap.Canvas.Handle));

        // draw outlined rect using Darren Session algorithm
        if (not DrawRoundedRect(rect, pOptions, pMaskPen, pMatrix, pOutlineMaskGraphics, iRect)) then
        begin
            iRect := Default(TRect);
            Exit;
        end;
    finally
        pMatrix.Free;
    end;

    // create and initialize geometry bitmap. This will contain the final geometry in true colors to
    // blend with device context
    pGeometryBitmap  := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
    TWGDIHelper.ConfigBitmap(pGeometryBitmap, width, height, True, bmDIB, pf32bit, afDefined);

    // copy background to geometry bitmap
    BitBlt(pGeometryBitmap.Canvas.Handle, 0, 0, width, height, hDC, 0, 0, SRCCOPY);

    if (pOptions.Fill.BrushType = E_BT_Solid) then
        pFillBrush := pOptions.Fill.Brush as TWSolidBrush
    else
        pFillBrush := nil;

    if (pOptions.Stroke.BrushType = E_BT_Solid) then
        pStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush
    else
        pStrokeBrush := nil;

    // calculate fill opacity
    if (Assigned(pFillBrush)) then
        fillOpacity := pFillBrush.Color.GetOpacity / 100.0
    else
        fillOpacity := 1.0;

    // calculate stroke opacity
    if (Assigned(pStrokeBrush)) then
        strokeOpacity := pStrokeBrush.Color.GetOpacity / 100.0
    else
        strokeOpacity := 1.0;

    rectLeft := Round(rect.Left);
    rectTop  := Round(rect.Top);

    // iterate through bitmap lines
    for y := rectTop to height - 1 do
    begin
        // get lines
        pMaskLine        := PWRGBQuadArray(pMaskBitmap.ScanLine[y]);
        pOutlineMaskLine := PWRGBQuadArray(pOutlineMaskBitmap.ScanLine[y]);
        pColorLine       := PWRGBQuadArray(pColorBitmap.ScanLine[y]);
        pGeometryLine    := PWRGBQuadArray(pGeometryBitmap.ScanLine[y]);

        // iterate through bitmap pixels
        for x := rectLeft to width - 1 do
        begin
            // do apply color pixel to final geometry?
            if ((pMaskLine[x].rgbRed      = 0) or
                (pMaskLine[x].rgbGreen    = 0) or
                (pMaskLine[x].rgbBlue     = 0) or
                (pMaskLine[x].rgbReserved = 0))
            then
            begin
                // get background color to blend
                bgColor := TWColor.Create(pColorLine[x].rgbRed, pColorLine[x].rgbGreen,
                        pColorLine[x].rgbBlue, pColorLine[x].rgbReserved);

                // blend background pixel with outline
                {$if CompilerVersion <= 24}
                    blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                            pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved);
                    blendedColor := blendedColor.Blend(bgColor, fillOpacity);
                {$else}
                    blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                            pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved).Blend(bgColor,
                            fillOpacity);
                {$ifend}

                // set newly blended pixel
                pGeometryLine[x].rgbRed      := blendedColor.GetRed;
                pGeometryLine[x].rgbGreen    := blendedColor.GetGreen;
                pGeometryLine[x].rgbBlue     := blendedColor.GetBlue;
                pGeometryLine[x].rgbReserved := blendedColor.GetAlpha;
            end;

            fillPixel := False;

            // do apply transformation?
            if (doApplyTransform) then
            begin
                // calculate prev and next x pixel
                prevXOffset := x - 1;
                nextXOffset := x + 1;

                // fill missing pixels due to Darren Session algorithm. In case rect is transformed
                // by a matrix, the Darren Session algorithm lets empty pixels inside the outline path.
                // This solution is not perfect but works in most situations
                if ((prevXOffset >= rect.Left) and (nextXOffset < width)) then
                    fillPixel := (((pOutlineMaskLine[prevXOffset].rgbRed      <> 0)  or
                                   (pOutlineMaskLine[prevXOffset].rgbGreen    <> 0)  or
                                   (pOutlineMaskLine[prevXOffset].rgbBlue     <> 0)  or
                                   (pOutlineMaskLine[prevXOffset].rgbReserved <> 0)) and
                                  ((pOutlineMaskLine[nextXOffset].rgbRed      <> 0)  or
                                   (pOutlineMaskLine[nextXOffset].rgbGreen    <> 0)  or
                                   (pOutlineMaskLine[nextXOffset].rgbBlue     <> 0)  or
                                   (pOutlineMaskLine[nextXOffset].rgbReserved <> 0)));
            end;

            // do apply outline pixel to final geometry?
            if ((pOutlineMaskLine[x].rgbRed      <> 0) or
                (pOutlineMaskLine[x].rgbGreen    <> 0) or
                (pOutlineMaskLine[x].rgbBlue     <> 0) or
                (pOutlineMaskLine[x].rgbReserved <> 0) or
                 fillPixel)
            then
            begin
                // blend background pixel with outline
                if (Assigned(pStrokeBrush)) then
                    {$if CompilerVersion <= 24}
                    begin
                        blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                                pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved);
                        blendedColor := blendedColor.Blend(pStrokeBrush.Color^, strokeOpacity)
                    end
                    {$else}
                        blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                                pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved).Blend
                                        (pStrokeBrush.Color^, strokeOpacity)
                    {$ifend}
                else
                begin
                    color.SetColor(255, 255, 255, 255);
                    {$if CompilerVersion <= 24}
                        blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                                pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved);
                        blendedColor := blendedColor.Blend(color, strokeOpacity);
                    {$else}
                        blendedColor := TWColor.Create(pGeometryLine[x].rgbRed, pGeometryLine[x].rgbGreen,
                                pGeometryLine[x].rgbBlue, pGeometryLine[x].rgbReserved).Blend(color,
                                        strokeOpacity);
                    {$ifend}
                end;

                // set newly blended pixel
                pGeometryLine[x].rgbRed      := blendedColor.GetRed;
                pGeometryLine[x].rgbGreen    := blendedColor.GetGreen;
                pGeometryLine[x].rgbBlue     := blendedColor.GetBlue;
                pGeometryLine[x].rgbReserved := blendedColor.GetAlpha;
            end;
        end;
    end;

    // get width and height to use
    rectWidth  := Round(rect.Width);
    rectHeight := Round(rect.Height);

    // copy final result to device context
    BitBlt(hDC, rectLeft, rectTop, rectWidth, rectHeight, pGeometryBitmap.Canvas.Handle, rectLeft,
            rectTop, SRCCOPY);
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.OutlineRect(pOptions: TWRenderer.IRectOptions; const drawRect: TWRectF;
        const maxSize: TWSizeF; pGraphics: TGpGraphics);
var
    pGradientFactory:     IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush:           IWSmartPointer<TGpBrush>;
    pWrapPen:             IWSmartPointer<TGpPen>;
    pPen, pClampPen:      TGpPen;
    paPen, paClampPen:    IWSmartPointer<TGpPen>;
    radius:               TWSizeF;
    pStop:                TWGDIPlusGradient.IStop;
    tmpColor:             TWColor;
    center, focus, delta: TWPointF;
    count, maxCount:      Cardinal;
    stopCount, i:         NativeUInt;
    maxLimitX, maxLimitY: Single;
    doIgnoreAndFill:      Boolean;
    offsets:              IStopOffsets;
    pRadial:              TWRadialGradientBrush;
    pFill:                IWSmartPointer<TWFill>;
begin
    if (not Assigned(pGraphics)) then
        Exit;

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get pen from cache, create one and cache it if still not exists
    pPen := m_pCache.GetPen(pOptions.Stroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pPen);

    if (not Assigned(pPen)) then
        Exit;

    // outline the rect using the cached brush
    OutlineRect(pOptions, drawRect, maxSize, pPen, pGraphics);

    // do apply a radial wrapping?
    if (pOptions.Stroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit;

    // get the radial brush
    pRadial := pOptions.Stroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations
    radius := pRadial.Radius;

    // do apply a radial wrapping?
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit;

    // todo FIXME -cBug -oJean: Don't found a better solution for now but this is incorrect and may
    //                          possibly cause side effects, so fix it whenever possible
    // below a radius of 2 pixels the gradient may generate artefacts so fill it completely
    maxLimitX       := radius.Width  * pRadial.ScaleFactor.Width  * 0.5;
    maxLimitY       := radius.Height * pRadial.ScaleFactor.Height * 0.5;
    doIgnoreAndFill := ((maxLimitX < 2.5) and (maxLimitY < 2.5));

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place between
                    // this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                count    := 0;
                maxCount := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    // todo FIXME -cBug -oJean: Don't found a better solution for now but this is
                    //                          incorrect and may possibly create side effects, so
                    //                          fix it whenever possible
                    // do ignore the previously painted gradient and fill with the new one?
                    if (not doIgnoreAndFill) then
                        ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false, pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pOptions.Stroke.Width));

                    // outline the surround using the cached pen
                    OutlineRect(pOptions, drawRect, maxSize, pWrapPen, pGraphics);

                    if (count > maxCount) then
                        break;

                    Inc(count);
                until (not pGradientFactory.DoWrap(drawRect, radius));
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place between
                    // this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                count    := 0;
                maxCount := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    // todo FIXME -cBug -oJean: Don't found a better solution for now but this is
                    //                          incorrect and may possibly create side effects, so
                    //                          fix it whenever possible
                    // do ignore the previously painted gradient and fill with the new one?
                    if (not doIgnoreAndFill) then
                        ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pOptions.Stroke.Width));

                    // outline the surround using the cached pen
                    OutlineRect(pOptions, drawRect, maxSize, pWrapPen, pGraphics);

                    if (count > maxCount) then
                        break;

                    Inc(count);
                until (not pGradientFactory.DoWrap(drawRect, radius));
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pOptions.Stroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            // todo FIXME -cBug -oJean: Don't found a better solution for now but this is incorrect
            //                          and may possibly create side effects, so fix it whenever
            //                          possible
            // below a radius of 2 pixels the gradient may generate artefacts so fill it completely
            if (not doIgnoreAndFill) then
            begin
                pFill := TWSmartPointer<TWFill>.Create();
                pFill.SetBrush(pRadial);

                ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);
            end;

            // outline the surround using the cached pen
            OutlineRect(pOptions, drawRect, maxSize, pClampPen, pGraphics);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.OutlineRect(pOptions: TWRenderer.IRectOptions; const drawRect: TWRectF;
        const maxSize: TWSizeF; pPen: TGpPen; pGraphics: TGpGraphics);
var
    startX, endX, startY, endY:                       Integer;
    radiusLeft, radiusTop, radiusRight, radiusBottom: Single;
begin
    // do draw left top corner?
    if (pOptions.Outline.ShowLeftTop and ((pOptions.Radius.LeftTop.X <> 0)
            or (pOptions.Radius.LeftTop.Y <> 0)))
    then
        // draw left top corner
        DrawLeftTopCorner(pPen, drawRect, pOptions.Radius.LeftTop^, pGraphics);

    // do draw left bottom corner?
    if (pOptions.Outline.ShowLeftBottom and ((pOptions.Radius.LeftBottom.X <> 0)
            or (pOptions.Radius.LeftBottom.Y <> 0)))
    then
        // draw left bottom corner
        DrawLeftBottomCorner(pPen, drawRect, pOptions.Radius.LeftBottom^, pGraphics);

    // do draw right top corner?
    if (pOptions.Outline.ShowRightTop and ((pOptions.Radius.RightTop.X <> 0)
            or (pOptions.Radius.RightTop.Y <> 0)))
    then
        // draw right top corner
        DrawRightTopCorner(pPen, drawRect, pOptions.Radius.RightTop^, pGraphics);

    // do draw right bottom corner?
    if (pOptions.Outline.ShowRightBottom and ((pOptions.Radius.RightBottom.X <> 0)
            or (pOptions.Radius.RightBottom.Y <> 0)))
    then
        // draw right bottom corner
        DrawRightBottomCorner(pPen, drawRect, pOptions.Radius.RightBottom^, pGraphics);

    // do draw left outline?
    if (pOptions.Outline.ShowLeft) then
    begin
        radiusTop    := GetRadiusY(pOptions.Radius.LeftTop^);
        radiusBottom := GetRadiusY(pOptions.Radius.LeftBottom^);

        // calculate start and end y position
        startY := Round(drawRect.Top    + Min(radiusTop,    maxSize.Height));
        endY   := Round(drawRect.Bottom - Min(radiusBottom, maxSize.Height));

        // is line out of bounds?
        if (startY >= endY) then
        begin
            startY := Round(drawRect.Top + (drawRect.Height / 2.0));
            endY   := startY + 1;
        end;

        pGraphics.DrawLine(pPen, drawRect.Left, startY, drawRect.Left, endY);
    end;

    // do draw top outline?
    if (pOptions.Outline.ShowTop) then
    begin
        radiusLeft  := GetRadiusX(pOptions.Radius.LeftTop^);
        radiusRight := GetRadiusX(pOptions.Radius.RightTop^);

        // calculate start and end x position
        startX := Round(drawRect.Left  + Min(radiusLeft,  maxSize.Width));
        endX   := Round(drawRect.Right - Min(radiusRight, maxSize.Width));

        // can draw line?
        if (startX >= endX) then
        begin
            startX := Round(drawRect.Left + (drawRect.Width / 2.0));
            endX   := startX + 1;
        end;

        pGraphics.DrawLine(pPen, startX, drawRect.Top, endX, drawRect.Top);
    end;

    // do draw right outline?
    if (pOptions.Outline.ShowRight) then
    begin
        radiusTop    := GetRadiusY(pOptions.Radius.RightTop^);
        radiusBottom := GetRadiusY(pOptions.Radius.RightBottom^);

        // calculate start and end y position
        startY := Round(drawRect.Top    + Min(radiusTop,    maxSize.Height));
        endY   := Round(drawRect.Bottom - Min(radiusBottom, maxSize.Height));

        // is line out of bounds?
        if (startY >= endY) then
        begin
            startY := Round(drawRect.Top + (drawRect.Height / 2.0));
            endY   := startY + 1;
        end;

        pGraphics.DrawLine(pPen, drawRect.Right, startY, drawRect.Right, endY);
    end;

    // do draw bottom outline?
    if (pOptions.Outline.ShowBottom) then
    begin
        radiusLeft  := GetRadiusX(pOptions.Radius.LeftBottom^);
        radiusRight := GetRadiusX(pOptions.Radius.RightBottom^);

        // calculate start and end x position
        startX := Round(drawRect.Left  + Min(radiusLeft,  maxSize.Width));
        endX   := Round(drawRect.Right - Min(radiusRight, maxSize.Width));

        // is line out of bounds?
        if (startX >= endX) then
        begin
            startX := Round(drawRect.Left + (drawRect.Width / 2.0));
            endX   := startX + 1;
        end;

        pGraphics.DrawLine(pPen, startX, drawRect.Bottom, endX, drawRect.Bottom);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetToken: ULONG_PTR;
begin
    // GDI+ isn't initialized?
    if (not m_GDIPlusInitialized) then
        Initialize;

    Result := m_GDIPlusToken;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.LinkGDIRenderer(pRenderer: TWRenderer_GDI);
begin
    m_pRenderer_GDI := pRenderer;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.BeginScene(hDC: THandle);
begin
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.EndScene;
begin
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions; hDC: THandle;
        out iRect: TRect): Boolean;
var
    useGradientBrush,
    useGradientPen,
    drawOutline,
    doTransformRect:    Boolean;
    pFillSolidBrush,
    pStrokeSolidBrush:  TWSolidBrush;
    pFillLinearBrush,
    pStrokeLinearBrush: TWLinearGradientBrush;
    pFillRadialBrush,
    pStrokeRadialBrush: TWRadialGradientBrush;
    pGraphics:          TGpGraphics;
    paGraphics:         IWSmartPointer<TGpGraphics>;
    gdiOptions:         TWRenderer.IRectOptions;
begin
    // no device context?
    if (hDC = 0) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    if (Assigned(m_pRenderer_GDI)) then
    begin
        useGradientBrush := IsGradientVisible(pOptions.Fill);
        useGradientPen   := IsGradientVisible(pOptions.Stroke);
        drawOutline      := IsOutlineVisible(pOptions.Stroke, pOptions.Outline, False);
        doTransformRect  := not pOptions.TransformMatrix.IsIdentity;

        GetBrushes(pOptions.Fill,   pFillSolidBrush,   pFillLinearBrush,   pFillRadialBrush);
        GetBrushes(pOptions.Stroke, pStrokeSolidBrush, pStrokeLinearBrush, pStrokeRadialBrush);

        // can fill background using GDI?
        if ((not pOptions.LayeredMode) and ((Assigned(pFillSolidBrush) and (pFillSolidBrush.Color.GetAlpha() = 255))
                or  (Assigned(pFillLinearBrush) and (pFillLinearBrush.Stops.Count = 2)
                and (pFillLinearBrush.Stops[0].Color.GetAlpha = 255)
                and (pFillLinearBrush.Stops[1].Color.GetAlpha = 255)
                and (((pFillLinearBrush.Stops[0].Position.X
                        = pFillLinearBrush.Stops[1].Position.X)
                and (pFillLinearBrush.Stops[0].Position.Y = rect.Top)
                and (pFillLinearBrush.Stops[1].Position.Y = rect.Bottom))
                or  ((pFillLinearBrush.Stops[0].Position.Y
                        = pFillLinearBrush.Stops[1].Position.Y)
                and (pFillLinearBrush.Stops[0].Position.X = rect.Left)
                and (pFillLinearBrush.Stops[1].Position.X = rect.Right)))))
                and not doTransformRect)
        then
            // use gdi to draw rect
            Exit(m_pRenderer_GDI.DrawRect(rect, pOptions, hDC, iRect));

        // can simply use GDI function to draw round rectangle?
        if (not pOptions.LayeredMode and  not pOptions.AntiAliasing and not useGradientBrush
                and not useGradientPen and IsRegular(pOptions.Radius)
                and pOptions.Radius.IsSymmetric and (Assigned(pFillSolidBrush)
                and (pFillSolidBrush.Color.GetAlpha = 255)) and (not drawOutline
                or  (IsCompletelyVisible(pOptions.Outline) and (Assigned(pStrokeSolidBrush)
                and (pStrokeSolidBrush.Color.GetAlpha = 255))))
                and not doTransformRect)
        then
        begin
            // populate gdi round rect options
            gdiOptions := pOptions;
            gdiOptions.Radius.SetLeftTop(pOptions.Radius.LeftTop^);
            gdiOptions.Radius.SetLeftBottom(pOptions.Radius.LeftTop^);
            gdiOptions.Radius.SetRightTop(pOptions.Radius.LeftTop^);
            gdiOptions.Radius.SetRightBottom(pOptions.Radius.LeftTop^);

            // use gdi to draw rect
            Exit(m_pRenderer_GDI.DrawRect(rect, gdiOptions, hDC, iRect));
        end;
    end;

    // do draw cartoon bubble rect?
    if (IsTailVisible(pOptions)) then
        Exit(DrawCartoonBubble(rect, pOptions, hDC, iRect));

    // get GDI+ graphics from device context
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    Result := DrawRect(rect, pOptions, pGraphics, iRect);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
        pGraphics: TGpGraphics; out iRect: TRect): Boolean;
var
    maxSize:            TWSizeF;
    useGradientBrush,
    useGradientPen,
    drawOutline,
    drawCorner,
    doTransformRect,
    fillWithOutline,
    doUseAntialiasing:  Boolean;
    drawRect:           TWRectF;
    pBrush:             TGpBrush;
    paBrush:            IWSmartPointer<TGpBrush>;
    pMatrix:            IWSmartPointer<TGpMatrix>;
    pGraphicPath:       IWSmartPointer<TGpGraphicsPath>;
    oldPageUnit:        TUnit;
    oldMode:            CompositingMode;
    fillColor:          TWColor;
    pFillSolidBrush,
    pStrokeSolidBrush:  TWSolidBrush;
    pFillLinearBrush,
    pStrokeLinearBrush: TWLinearGradientBrush;
    pFillRadialBrush,
    pStrokeRadialBrush: TWRadialGradientBrush;
    pFill:              IWSmartPointer<TWFill>;
begin
    // is GDI+ initialized?
    if (not m_GDIPlusInitialized) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // no gdi+ graphics?
    if (not Assigned(pGraphics)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // nothing to draw?
    if (not IsRectVisible(pOptions)) then
    begin
        iRect := rect.ToTRect(True);
        Exit(True);
    end;

    maxSize := TWSizeF.Create(GetMaxSizeX(rect), GetMaxSizeY(rect));

    // check if gradient, outline and corner should be drawn
    useGradientBrush := IsGradientVisible(pOptions.Fill);
    useGradientPen   := IsGradientVisible(pOptions.Stroke);
    drawOutline      := IsOutlineVisible(pOptions.Stroke, pOptions.Outline, False);
    drawCorner       := IsCornerVisible(pOptions.Radius, pOptions.Outline);
    doTransformRect  := not pOptions.TransformMatrix.IsIdentity;

    // check if rect is completely filled by outline
    fillWithOutline := drawOutline and IsStrokeVisible(pOptions.Stroke)
            and (pOptions.Stroke.Width >= Min(maxSize.Width, maxSize.Height));

    GetBrushes(pOptions.Fill,   pFillSolidBrush,   pFillLinearBrush,   pFillRadialBrush);
    GetBrushes(pOptions.Stroke, pStrokeSolidBrush, pStrokeLinearBrush, pStrokeRadialBrush);

    // do only draw a solid background color?
    if ((not drawOutline) and (not drawCorner) and (not useGradientBrush)
            and (not fillWithOutline or not useGradientPen))
    then
    begin
        if (fillWithOutline) then
        begin
            if (Assigned(pStrokeSolidBrush)) then
                fillColor := pStrokeSolidBrush.Color^
            else
            if (Assigned(pStrokeLinearBrush) and (pStrokeLinearBrush.Stops.Count > 0)) then
                fillColor := pStrokeLinearBrush.Stops[pStrokeLinearBrush.Stops.Count - 1].Color^
            else
                // panic mode, nothing works as expected, try to create a brush with a default
                // solid color
                fillColor.SetColor(255, 255, 255, 255);
        end
        else
        begin
            if (Assigned(pFillSolidBrush)) then
                fillColor := pFillSolidBrush.Color^
            else
            if (Assigned(pFillLinearBrush) and (pFillLinearBrush.Stops.Count > 0)) then
                fillColor := pFillLinearBrush.Stops[pFillLinearBrush.Stops.Count - 1].Color^
            else
                // panic mode, nothing works as expected, try to create a brush with a default
                // solid color
                fillColor.SetColor(255, 255, 255, 255);
        end;

        // is pen visible?
        if (fillColor.GetAlpha = 0) then
        begin
            // nothing to do
            iRect := Default(TRect);
            Exit(True);
        end;

        if (fillWithOutline) then
            drawRect := CalculateDrawRect(rect, pOptions, False)
        else
            drawRect := rect;

        // enable antialiasing, if needed
        if (pOptions.AntiAliasing) then
            pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

        // create brush to fill rect background
        pBrush := GetBrush(fillColor);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Brushes) then
            paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

        // do apply transformation matrix?
        if (doTransformRect) then
        begin
            pMatrix := TWSmartPointer<TGpMatrix>.Create();

            // convert transform matrix to gdi+ matrix
            pOptions.TransformMatrix.ToGpMatrix(pMatrix);

            // apply transformation
            pGraphics.SetTransform(pMatrix);
        end;

        // fill rectangle with GDI+ color
        pGraphics.FillRectangle(pBrush, drawRect.ToGpRectF);

        iRect := drawRect.ToTRect(True);
        Exit(True);
    end;

    doUseAntialiasing := pOptions.AntiAliasing and (drawCorner or not IsCompletelyVisible(pOptions.Outline)
            or not doTransformRect);

    // enable antialiasing, if needed
    if (doUseAntialiasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // calculate real draw surface
    if (fillWithOutline) then
        drawRect := rect
    else
        drawRect := CalculateDrawRect(rect, pOptions, doUseAntialiasing);

    iRect := drawRect.ToTRect(True);

    // create new GDI+ path. NOTE create explicitly the graphics path before keep it inside the
    // smart pointer, because otherwise the incorrect constructor is called while the smart pointer
    // tries to auto-create the object, causing thus that the path is never drawn
    pGraphicPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // do draw corner?
    if (not drawCorner) then
        // create rectangle path
        pGraphicPath.AddRectangle(drawRect.ToGpRectF)
    else
    if (drawOutline and not fillWithOutline) then
    begin
        // do draw fully outlined rect without antialiasing?
        if (not doUseAntialiasing and IsCompletelyVisible(pOptions.Outline)) then
        begin
            // draw outlined round rect without antialiasing correctly, as GDI+ is unable to do that
            DrawNonAntialiasedSymmetricalRoundRect(pGraphics.GetHDC(), rect, pOptions, iRect);
            Exit(True);
        end;

        // create rounded rectangle
        if (not CreateRoundedRect(drawRect, pOptions.Radius, pGraphicPath)) then
        begin
            iRect := Default(TRect);
            Exit(False);
        end;
    end
    else
    begin
        // drawing a round rectangle without outline is a little special, because standard GDI+
        // curves algorithms used in paths draw asymmetrical corners if outline is not drawn too, see:
        // http://www.codeproject.com/Articles/27228/A-class-for-creating-round-rectangles-in-GDI-with
        drawRect.Right  := drawRect.Right  - 1;
        drawRect.Bottom := drawRect.Bottom - 1;

        // save current page unit
        oldPageUnit := pGraphics.GetPageUnit;

        // save current alpha blending mode
        oldMode := pGraphics.GetCompositingMode;

        // set to pixel mode
        pGraphics.SetPageUnit(UnitPixel);

        try
            // create rounded rectangle
            if (not CreateRoundedRect(drawRect, pOptions.Radius, pGraphicPath)) then
            begin
                iRect := Default(TRect);
                Exit(False);
            end;

            // use antialiasing?
            if (not pOptions.AntiAliasing) then
                // draw transparent round rect without outline correctly, as GDI+ is unable to do that
                DrawTransparentRoundRectWithoutOutline(pGraphics.GetHDC(), rect, pOptions, fillWithOutline,
                        pGraphicPath)
            else
            begin
                // do apply transformation matrix?
                if (doTransformRect) then
                begin
                    pMatrix := TWSmartPointer<TGpMatrix>.Create();

                    // convert transform matrix to gdi+ matrix
                    pOptions.TransformMatrix.ToGpMatrix(pMatrix);

                    // apply transformation
                    pGraphics.SetTransform(pMatrix);
                end;

                pFill := TWSmartPointer<TWFill>.Create();

                // fill path
                if (fillWithOutline) then
                    pFill.SetBrush(pOptions.Stroke.Brush)
                else
                    pFill.SetBrush(pOptions.Fill.Brush);

                if (not FillPath(pGraphicPath, pFill, pGraphics, rect)) then
                begin
                    iRect := Default(TRect);
                    Exit(False);
                end;
            end;
        finally
            // restore previous alpha blending mode
            pGraphics.SetCompositingMode(oldMode);

            // restore page unit
            pGraphics.SetPageUnit(oldPageUnit);
        end;

        iRect := drawRect.ToTRect(True);
        Exit(True);
    end;

    // do apply transformation matrix?
    if (doTransformRect) then
    begin
        pMatrix := TWSmartPointer<TGpMatrix>.Create();

        // convert transform matrix to gdi+ matrix
        pOptions.TransformMatrix.ToGpMatrix(pMatrix);

        // apply transformation
        pGraphics.SetTransform(pMatrix);
    end;

    pFill := TWSmartPointer<TWFill>.Create();

    if (fillWithOutline) then
        pFill.SetBrush(pOptions.Stroke.Brush)
    else
        pFill.SetBrush(pOptions.Fill.Brush);

    // fill the rect
    if (not FillPath(pGraphicPath, pFill, pGraphics, rect)) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // do draw outline?
    if (drawOutline and not fillWithOutline) then
    begin
        // is outline completely visible?
        if (IsCompletelyVisible(pOptions.Outline)) then
        begin
            // outline the rect
            if (not DrawPath(pGraphicPath, pOptions.Stroke, pGraphics, rect)) then
            begin
                iRect := Default(TRect);
                Exit(False);
            end;
        end
        else
            // outline the rect
            OutlineRect(pOptions, drawRect, maxSize, pGraphics);
    end;

    iRect  := drawRect.ToTRect(True);
    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean;
var
    pGraphics:                    TGpGraphics;
    paGraphics:                   IWSmartPointer<TGpGraphics>;
    points:                       IGDIPlusPointList;
    pMatrix:                      IWSmartPointer<TGpMatrix>;
    pFill:                        IWSmartPointer<TWFill>;
    maxSize:                      Cardinal;
    drawOutline, fillWithOutline: Boolean;
    bbox:                         TWRectF;
    drawRect:                     TWRectF;
begin
    // is GDI+ initialized?
    if (not m_GDIPlusInitialized) then
        Exit(False);

    // no device context?
    if (hDC = 0) then
        Exit(False);

    // get GDI+ graphics from device context
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    // enable antialiasing, if needed
    if (pOptions.AntiAliasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // convert generic point list to GDI+ point list
    ToGDIPlusPointList(pointList, points, bbox);

    // do get the path bounding box?
    if (rect.IsEmpty) then
    begin
        drawRect.Left   := Round(bbox.Left);
        drawRect.Top    := Round(bbox.Top);
        drawRect.Right  := Round(bbox.Right);
        drawRect.Bottom := Round(bbox.Bottom);
    end
    else
        drawRect := rect;

    maxSize := Round(GetMaxSize(drawRect));

    // check if gradient and outline should be drawn
    drawOutline := (pOptions.Stroke.Width <> 0.0);

    // check if rect is completely filled by outline
    fillWithOutline := drawOutline and (Cardinal(ConvertStrokeWidth(pOptions.Stroke)) >= maxSize);

    // do apply transformation matrix?
    if (not pOptions.TransformMatrix.IsIdentity) then
    begin
        pMatrix := TWSmartPointer<TGpMatrix>.Create();

        // convert transform matrix to gdi+ matrix
        pOptions.TransformMatrix.ToGpMatrix(pMatrix);

        // apply transformation
        pGraphics.SetTransform(pMatrix);
    end;

    pFill := TWSmartPointer<TWFill>.Create();

    if (fillWithOutline) then
        pFill.SetBrush(pOptions.Stroke.Brush)
    else
        pFill.SetBrush(pOptions.Fill.Brush);

    // fill the polygon
    if (not FillPolygon(points, pFill, pGraphics, drawRect)) then
        Exit(False);

    // do draw outline?
    if (drawOutline and not fillWithOutline) then
    begin
        // outline the polygon
        if (not DrawPolygon(points, pOptions.Stroke, pGraphics, drawRect)) then
            Exit(False);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawPath(const rect: TWRectF; const path: TWPathCmds;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean;
var
    pGraphics:                    TGpGraphics;
    paGraphics:                   IWSmartPointer<TGpGraphics>;
    pGraphicPath:                 IWSmartPointer<TGpGraphicsPath>;
    pPathConverter:               IWSmartPointer<TWGraphicPathConverter_GDIPlus>;
    pMatrix:                      IWSmartPointer<TGpMatrix>;
    pFill:                        IWSmartPointer<TWFill>;
    maxSize:                      Cardinal;
    drawOutline, fillWithOutline: Boolean;
begin
    // is GDI+ initialized?
    if (not m_GDIPlusInitialized) then
        Exit(False);

    // no device context?
    if (hDC = 0) then
        Exit(False);

    // get GDI+ graphics
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    // enable antialiasing, if needed
    if (pOptions.AntiAliasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // initialize new graphic path. NOTE create explicitly the graphics path before keep it inside
    // the smart pointer, because otherwise the incorrect constructor is called while the smart
    // pointer tries to auto-create the object, causing thus that the path is never drawn
    pGraphicPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

    // get GDI+ path to draw
    pPathConverter :=
            TWSmartPointer<TWGraphicPathConverter_GDIPlus>.Create(TWGraphicPathConverter_GDIPlus.Create(pGraphicPath));
    pPathConverter.Process(rect, path);

    maxSize := Round(GetMaxSize(rect));

    // check if gradient and outline should be drawn
    drawOutline := (pOptions.Stroke.Width <> 0.0);

    // check if rect is completely filled by outline
    fillWithOutline := drawOutline and (Cardinal(ConvertStrokeWidth(pOptions.Stroke)) >= maxSize);

    // do apply transformation matrix?
    if (not pOptions.TransformMatrix.IsIdentity) then
    begin
        pMatrix := TWSmartPointer<TGpMatrix>.Create();

        // convert transform matrix to gdi+ matrix
        pOptions.TransformMatrix.ToGpMatrix(pMatrix);

        // apply transformation
        pGraphics.SetTransform(pMatrix);
    end;

    pFill := TWSmartPointer<TWFill>.Create();

    if (fillWithOutline) then
        pFill.SetBrush(pOptions.Stroke.Brush)
    else
        pFill.SetBrush(pOptions.Fill.Brush);

    // fill the path
    if (not FillPath(pGraphicPath, pFill, pGraphics, rect)) then
        Exit(False);

    // do draw outline?
    if (drawOutline and not fillWithOutline) then
        // outline the path
        if (not DrawPath(pGraphicPath, pOptions.Stroke, pGraphics, rect)) then
            Exit(False);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetTextSize(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
        pLines: PInteger = nil): TWSizeF;
var
    pGraphics:  TGpGraphics;
    paGraphics: IWSmartPointer<TGpGraphics>;
    pFont:      TGpFont;
    paFont:     IWSmartPointer<TGpFont>;
    pFormat:    TGpStringFormat;
    paFormat:   IWSmartPointer<TGpStringFormat>;
    outputRect: TGpRectF;
begin
    // empty text has zero size
    if (Length(text) = 0) then
        Exit(Default(TWSizeF));

    // can use GDI+?
    if (not m_GDIPlusInitialized) then
        Exit(Default(TWSizeF));

    // no device context?
    if (hDC = 0) then
        Exit(Default(TWSizeF));

    // no font?
    if (not Assigned(pOptions.Font)) then
        Exit(Default(TWSizeF));

    // get GDI+ graphics from device context
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    pFont   := nil;
    pFormat := nil;

    // configure tools to draw text
    ConfigTextDraw(hDC, pOptions, pGraphics, @pFont, nil, @pFormat);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Fonts) then
        paFont := TWSmartPointer<TGpFont>.Create(pFont);

    if (not g_GDIPlusCacheController.m_StringFormat) then
        paFormat := TWSmartPointer<TGpStringFormat>.Create(pFormat);

    // measure string size
    pGraphics.MeasureString(text, Length(text), pFont, rect.ToGpRectF, pFormat, outputRect,
            pCharacters, pLines);

    // get text size
    Result := TWSizeF.Create(outputRect.Width, outputRect.Height);

    // add shadow to final result
    Result.Width  := Result.Width  + Abs(pOptions.ShadowDelta.X);
    Result.Height := Result.Height + Abs(pOptions.ShadowDelta.Y);

    // add shadow blur to final result
    Result.Width  := Result.Width  + (pOptions.ShadowBlur.Width  * 2);
    Result.Height := Result.Height + (pOptions.ShadowBlur.Height * 2);

    // add one extra pixels that seems to be lost (otherwise text will wrap)
    Result.Width  := Result.Width  + TextSizeExtraWidthPixels;
    Result.Height := Result.Height + TextSizeExtraHeightPixels;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawText(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean;
var
    pGraphics:                            TGpGraphics;
    paGraphics, pShadowGraphics:          IWSmartPointer<TGpGraphics>;
    pFont:                                TGpFont;
    paFont:                               IWSmartPointer<TGpFont>;
    pBrush, pShadowBrush:                 TGpBrush;
    paBrush, paShadowBrush:               IWSmartPointer<TGpBrush>;
    pFormat:                              TGpStringFormat;
    paFormat:                             IWSmartPointer<TGpStringFormat>;
    pBlur:                                TWBlur;
    shadowWidth, shadowHeight:            NativeUInt;
    pShadowOverlay, pBluredShadowOverlay: IWSmartPointer<Vcl.Graphics.TBitmap>;
    shadowTextF, shadowRectF, textRect:   TGpRectF;
    blendFunction:                        TBlendFunction;
    padding:                              TWPaddingI;
begin
    // is text empty?
    if (Length(text) = 0) then
        Exit(True);

    // can use GDI+?
    if (not m_GDIPlusInitialized) then
        Exit(False);

    // no device context?
    if (hDC = 0) then
        Exit(False);

    try
        // lock cache, as some needed objects can be accidentally deleted while function is executed
        m_pCache.Lock;

        // get GDI+ graphics from device context
        pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

        // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
        if (not g_GDIPlusCacheController.m_Graphics) then
            paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

        pFont   := nil;
        pBrush  := nil;
        pFormat := nil;

        // configure tools to draw text
        ConfigTextDraw(hDC, pOptions, pGraphics, @pFont, @pBrush, @pFormat);

        // ensure that GDI+ objects will be released when useless
        if (not g_GDIPlusCacheController.m_Fonts) then
            paFont := TWSmartPointer<TGpFont>.Create(pFont);

        if (not g_GDIPlusCacheController.m_Brushes) then
            paBrush := TWSmartPointer<TGpBrush>.Create(pBrush);

        if (not g_GDIPlusCacheController.m_StringFormat) then
            paFormat := TWSmartPointer<TGpStringFormat>.Create(pFormat);

        // do draw shadow?
        if (not pOptions.ShadowDelta.IsZero or (not pOptions.ShadowBlur.IsZero)) then
        begin
            // get text shadow brush
            pShadowBrush := GetBrush(pOptions.ShadowColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paShadowBrush := TWSmartPointer<TGpBrush>.Create(pShadowBrush);

            // do use blured shadow?
            if (not pOptions.ShadowBlur.IsZero) then
            begin
                // calculate shadow blur size
                shadowWidth  := Round(rect.Width  + (pOptions.ShadowBlur.Width  * 2.0));
                shadowHeight := Round(rect.Height + (pOptions.ShadowBlur.Height * 2.0));

                pShadowOverlay             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
                pShadowOverlay.PixelFormat := pf32bit;
                pShadowOverlay.AlphaFormat := afPremultiplied;
                pShadowOverlay.SetSize(shadowWidth, shadowHeight);

                // clear overlay
                TWGDIHelper.Clear(pShadowOverlay);

                // get GDI+ graphics from device context
                pShadowGraphics :=
                        TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pShadowOverlay.Canvas.Handle));

                // calculate shadow text rectangle
                shadowTextF.X      := pOptions.ShadowBlur.Width;
                shadowTextF.Y      := pOptions.ShadowBlur.Height;
                shadowTextF.Width  := rect.Width;
                shadowTextF.Height := rect.Height;

                // draw shadow text
                pShadowGraphics.DrawString(text, Length(text), pFont, shadowTextF, pFormat,
                        pShadowBrush);

                pBlur := nil;

                try
                    pBlur := TWBlur.Create;

                    // apply blur to shadow text in overlay
                    pBluredShadowOverlay :=
                            TWSmartPointer<Vcl.Graphics.TBitmap>.Create(pBlur.Apply(pShadowOverlay,
                                    pOptions.ShadowBlur^, False));

                    // initialize blend operation
                    blendFunction.BlendOp             := AC_SRC_OVER;
                    blendFunction.BlendFlags          := 0;
                    blendFunction.SourceConstantAlpha := 255;
                    blendFunction.AlphaFormat         := AC_SRC_ALPHA;

                    // copy shadow text from overlay
                    AlphaBlend(hDC, (Round(rect.Left) + pOptions.ShadowDelta.X) - pOptions.ShadowBlur.Width,
                            (Round(rect.Top) + pOptions.ShadowDelta.Y) - pOptions.ShadowBlur.Height, shadowWidth,
                            shadowHeight, pShadowOverlay.Canvas.Handle, 0, 0, shadowWidth, shadowHeight,
                            blendFunction);
                finally
                    pBlur.Free;
                end;
            end
            else
            begin
                // calculate shadow rectangle
                shadowRectF.X      := rect.Left + pOptions.ShadowDelta.X;
                shadowRectF.Y      := rect.Top  + pOptions.ShadowDelta.Y;
                shadowRectF.Width  := rect.Width;
                shadowRectF.Height := rect.Height;

                // draw shadow
                pGraphics.DrawString(text, Length(text), pFont, shadowRectF, pFormat, pShadowBrush);
            end;
        end;

        // get text rect
        textRect := rect.ToGpRectF;

        // search for horizontal text alignment
        case (pOptions.AlignHorz) of
            E_H_Left:
            begin
                // get text padding and apply correction, as GDI+ is unable to draw a correct left aligned text
                padding    := MeasureTextPadding(text, rect, pOptions, [IE_Left], pFont, pFormat);
                textRect.X := textRect.X - padding.Left;
            end;

            E_H_Right:
            begin
                // get text padding and apply correction, as GDI+ is unable to draw a correct right aligned text
                padding    := MeasureTextPadding(text, rect, pOptions, [IE_Right], pFont, pFormat);
                textRect.X := textRect.X + padding.Right;
            end;
        end;

        // draw text
        pGraphics.DrawString(text, Length(text), pFont, textRect, pFormat, pBrush);
    finally
        // unlock cache, clear content if needed
        m_pCache.Unlock;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLayeredText(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean;
var
    pMask, pShadowBitmap, pTextBitmap: IWSmartPointer<Vcl.Graphics.TBitmap>;
    pMaskFont:                         IWSmartPointer<TFont>;
    maskRect:                          TWRectF;
    pMaskOptions:                      ITextOptions;
    whiteColor, textColor:             TWColor;
    rectWidth, rectHeight:             Integer;
    blendFunction:                     TBlendFunction;
begin
    // search for render mode
    case (pOptions.TextRendering) of
        E_R_None,
        E_R_Default:
        begin
            // create text mask
            pMask             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
            pMask.PixelFormat := pf32bit;
            pMask.AlphaFormat := afPremultiplied;
            pMask.SetSize(Round(rect.Width), Round(rect.Height));

            // get mask rect
            maskRect := TWRectF.Create(0.0, 0.0, pMask.Width, pMask.Height);

            // fill mask background with black color
            pMask.Canvas.Brush.Style := bsSolid;
            pMask.Canvas.Brush.Color := clBlack;
            pMask.Canvas.FillRect(maskRect.ToTRect(True));

            // get mask font, same as text font except for color
            pMaskFont := TWSmartPointer<TFont>.Create();
            pMaskFont.Assign(pOptions.Font);
            pMaskFont.Color := clBlack;

            pMaskOptions := nil;

            try
                whiteColor := TWColor.Create(clWhite);

                pMaskOptions := ITextOptions.Create;
                pMaskOptions.Assign(pOptions);

                // populate mask options, same as text options, but force shadow to maximum
                pMaskOptions.Font.Assign(pMaskFont);
                pMaskOptions.Alpha := 255;
                pMaskOptions.SetShadowColor(@whiteColor);

                // draw text mask
                if (not DrawText(text, maskRect, pMaskOptions, pMask.Canvas.Handle)) then
                    Exit(False);

                pShadowBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();

                // convert black&white mask to RGBA bitmap using shadow color
                if (not ConvertMaskToRGBA(pOptions.ShadowColor^, pMask, pShadowBitmap)) then
                    Exit(False);

                // initialize blend operation
                blendFunction.BlendOp             := AC_SRC_OVER;
                blendFunction.BlendFlags          := 0;
                blendFunction.SourceConstantAlpha := 255;
                blendFunction.AlphaFormat         := AC_SRC_ALPHA;

                rectWidth  := Round(rect.Width);
                rectHeight := Round(rect.Height);

                // copy transparent text to canvas
                AlphaBlend(hDC, Round(rect.Left), Round(rect.Top), rectWidth, rectHeight,
                        pShadowBitmap.Canvas.Handle, 0, 0, rectWidth, rectHeight, blendFunction);

                // fill mask background with black color
                pMask.Canvas.Brush.Style := bsSolid;
                pMask.Canvas.Brush.Color := clBlack;
                pMask.Canvas.FillRect(maskRect.ToTRect(True));

                // set text mask color
                pMaskFont.Color := clWhite;

                // populate mask options, same as text options, except for text color, and without shadow
                pMaskOptions.Assign(pOptions);
                pMaskOptions.Font        := pMaskFont;
                pMaskOptions.Alpha       := 255;
                pMaskOptions.SetShadowDelta(Default(TPoint));
                pMaskOptions.SetShadowBlur(Default(TSize));

                // draw text mask
                if (not DrawText(text, maskRect, pMaskOptions, pMask.Canvas.Handle)) then
                    Exit(False);
            finally
                pMaskOptions.Free;
            end;

            // get source text color
            textColor   := TWColor.Create(pOptions.Font.Color, pOptions.Alpha);
            pTextBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();

            // convert black&white mask to RGBA bitmap using text color
            if (not ConvertMaskToRGBA(textColor, pMask, pTextBitmap)) then
                Exit(False);

            rectWidth  := Round(rect.Width);
            rectHeight := Round(rect.Height);

            // copy transparent text to canvas
            AlphaBlend(hDC, Round(rect.Left), Round(rect.Top), rectWidth, rectHeight,
                    pTextBitmap.Canvas.Handle, 0, 0, rectWidth, rectHeight, blendFunction);

            Result := True;
        end;
    else
        // draw text normally
        Result := DrawText(text, rect, pOptions, hDC);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
        const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions);
var
    pGraphics:  TGpGraphics;
    paGraphics: IWSmartPointer<TGpGraphics>;
begin
    // get GDI+ graphics from device context
    pGraphics := m_pCache.m_pGraphics.GetGraphics(hDC);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Graphics) then
        paGraphics := TWSmartPointer<TGpGraphics>.Create(pGraphics);

    DrawImage(pGraphic, srcRect, pGraphics, destRect, pOptions);
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDIPlus.DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; pGraphics: TGpGraphics;
        const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions);
var
    iMode:       InterpolationMode;
    pBitmap:     IWSmartPointer<Vcl.Graphics.TBitmap>;
    pGDIPBitmap: IWSmartPointer<TGpBitmap>;
begin
    if (not Assigned(pGraphic)) then
        Exit;

    if (not Assigned(pGraphics)) then
        Exit;

    if (not Assigned(pOptions)) then
        Exit;

    case (pOptions.ResizeMode) of
        E_RzMode_Auto:
            // for vector based images, use native draw, otherwise default
            if (not pOptions.Vectorial) then
                iMode := DefaultInterpolationMode
            else
                iMode := InterpolationModeNearestNeighbor;

        E_RzMode_Nearest:    iMode := InterpolationModeNearestNeighbor;
        E_RzMode_Bilinear:   iMode := InterpolationModeBilinear;
        E_RzMode_BilinearHQ: iMode := InterpolationModeHighQualityBilinear;
        E_RzMode_Bicubic:    iMode := InterpolationModeBicubic;
        E_RzMode_BicubicHQ:  iMode := InterpolationModeHighQualityBicubic;
    else
        iMode := InterpolationModeHighQualityBicubic;
    end;

    // create temp bitmap
    pBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();

    // set 32bit only if transparent (otherwise non transparent PNG won't render)
    if (pOptions.Transparent or (pOptions.Opacity < 1.0)) then
    begin
        pBitmap.PixelFormat := pf32bit;
        SetBkMode(pBitmap.Canvas.Handle, TRANSPARENT);
    end
    else
        pBitmap.PixelFormat := pf24bit;

    pBitmap.SetSize(pGraphic.Width, pGraphic.Height);

    // copy TGraphic to bitmap
    if (pOptions.Opacity < 1.0) then
    begin
        TWGDIHelper.Clear(pBitmap);
        pBitmap.Canvas.Draw(0, 0, pGraphic, TWGDIHelper.OpacityToAlpha(pOptions.Opacity));
        TWGDIHelper.DrawTransparentImage(pGraphic, TPoint.Create(0, 0), pOptions.Opacity,
                pBitmap.Canvas, nil);
    end
    else
        pBitmap.Canvas.Draw(0, 0, pGraphic);

    // create GDI Image from bitmap
    pGDIPBitmap := TWSmartPointer<TGpBitmap>.Create(TWGDIPlusHelper.ToGDIPlusBitmap(pBitmap));

    // set interpolation mode
    pGraphics.SetInterpolationMode(iMode);

    // draw image to canvas
    pGraphics.DrawImage(pGDIPBitmap, destRect.ToGpRectF, srcRect.Left, srcRect.Top, srcRect.Width,
            srcRect.Height, UnitPixel);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetCaps: TWRenderer.IDrawCaps;
begin
    Result := [];

    // is gdi+ initialized?
    if (m_GDIPlusInitialized) then
        Include(Result, IE_DeviceSupported);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddFontToSession(const name, fileName: UnicodeString): Boolean;
var
    familyCount: Integer;
    resStatus:   Status;
begin
    // no font name?
    if (Length(name) = 0) then
        Exit(False);

    // add font in GDI too
    if (Assigned(m_pRenderer_GDI) and not m_pRenderer_GDI.AddFontToSession(name, fileName)) then
        Exit(False);

    // no font collection?
    if (not Assigned(m_pCache.m_pFontCollection)) then
        Exit(False);

    // get family count before adding new font
    familyCount := m_pCache.m_pFontCollection.GetFamilyCount;

    // add new font from file
    resStatus := m_pCache.m_pFontCollection.AddFontFile(fileName);

    // succeeded?
    if (resStatus <> Ok) then
        Exit(False);

    // added font was part of an existing family? (e.g. arial bold font should not be added in
    // custom font list if arial font was already added)
    if (familyCount <> m_pCache.m_pFontCollection.GetFamilyCount) then
        // add new font in added list
        m_pCache.m_pCustomFontList.Add(name);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddFont(const name, fileName: UnicodeString): THandle;
var
    familyCount: Integer;
    status:      TStatus;
begin
    Result := 0;

    // no font name?
    if (Length(name) = 0) then
        Exit;

    // add font using GDI
    if (Assigned(m_pRenderer_GDI)) then
        Result := m_pRenderer_GDI.AddFont(name, fileName);

    // succeeded?
    if (Result = 0) then
        Exit;

    // no font collection?
    if (not Assigned(m_pCache.m_pFontCollection)) then
        Exit(0);

    // get family count before adding new font
    familyCount := m_pCache.m_pFontCollection.GetFamilyCount;

    // add new font from file
    status := m_pCache.m_pFontCollection.AddFontFile(fileName);

    // succeeded?
    if (status <> Ok) then
        Exit(0);

    // added font was part of an existing family? (e.g. arial bold font should not be added in
    // custom font list if arial font was already added)
    if (familyCount <> m_pCache.m_pFontCollection.GetFamilyCount) then
        // add new font in added list
        m_pCache.m_pCustomFontList.Add(name);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.AddFont(const name: UnicodeString; pStream: TStream;
        fontLength: Cardinal): THandle;
var
    offset:      NativeUInt;
    familyCount: Integer;
    status:      TStatus;
    buffer:      TWByteArray;
begin
    Result := 0;

    // no font name?
    if (Length(name) = 0) then
        Exit;

    // no stream?
    if (not Assigned(pStream)) then
        Exit;

    // no data length
    if (fontLength = 0) then
        Exit;

    // keep stream offset
    offset := pStream.Position;

    // add font in GDI
    if (Assigned(m_pRenderer_GDI)) then
        Result := m_pRenderer_GDI.AddFont(name, pStream, fontLength);

    // succeeded?
    if (Result = 0) then
        Exit;

    // restore stream offset
    pStream.Seek(offset, soBeginning);

    // no font collection?
    if (not Assigned(m_pCache.m_pFontCollection)) then
        Exit(0);

    // get family count before adding new font
    familyCount := m_pCache.m_pFontCollection.GetFamilyCount;

    try
        // read font data content from stream
        SetLength(buffer, fontLength);
        pStream.Read(buffer, fontLength);

        // add new font from file
        status := m_pCache.m_pFontCollection.AddMemoryFont(buffer, fontLength);

        // succeeded?
        if (status <> Ok) then
            Exit(0);
    finally
        SetLength(buffer, 0);
    end;

    // added font was part of an existing family? (e.g. arial bold font should not be added in
    // custom font list if arial font was already added)
    if (familyCount <> m_pCache.m_pFontCollection.GetFamilyCount) then
        // add new font in added list
        m_pCache.m_pCustomFontList.Add(name);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.FillRectangle(const rect: TWRectF; const pFill: TWFill;
        pGraphics: TGpGraphics): Boolean;
var
    pGradientFactory:                  IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush, paBrush, paClampBrush: IWSmartPointer<TGpBrush>;
    pGpBrush, pClampBrush:             TGpBrush;
    pRadial:                           TWRadialGradientBrush;
    radius:                            TWSizeF;
    pStop:                             TWGDIPlusGradient.IStop;
    tmpColor:                          TWColor;
    center, focus, delta:              TWPointF;
    count, maxCount:                   Cardinal;
    stopCount, i:                      NativeUInt;
    stopOnNextLoop:                    Boolean;
    offsets:                           IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsFillVisible(pFill)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get fill brush
    pGpBrush := m_pCache.GetBrush(pFill, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pGpBrush);

    if (not Assigned(pGpBrush)) then
        Exit(False);

    // fill the rectangle using the cached brush
    pGraphics.FillRectangle(pGpBrush, rect.ToGpRectF);

    // do apply a radial wrapping?
    if (pFill.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false, pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillRectangle(pWrapBrush, rect.ToGpRectF);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillRectangle(pWrapBrush, rect.ToGpRectF);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampBrush := GetBrush(pGradientFactory.GetEndColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paClampBrush := TWSmartPointer<TGpBrush>.Create(pClampBrush);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // fill the surround using the wrap brush
            pGraphics.FillRectangle(pClampBrush, rect.ToGpRectF);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawRectangle(const rect: TWRectF; const pStroke: TWStroke;
        pGraphics: TGpGraphics): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the rectangle using the cached pen
    pGraphics.DrawRectangle(pGpPen, rect.ToGpRectF);

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place between
                    // this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawRectangle(pWrapPen, rect.ToGpRectF);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawRectangle(pWrapPen, rect.ToGpRectF);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the wrap pen
            pGraphics.DrawRectangle(pClampPen, rect.ToGpRectF);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.FillPath(pPath: TGpGraphicsPath; const pFill: TWFill;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:                  IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush, paBrush, paClampBrush: IWSmartPointer<TGpBrush>;
    pGpBrush, pClampBrush:             TGpBrush;
    drawRect:                          TWRectF;
    pRadial:                           TWRadialGradientBrush;
    radius:                            TWSizeF;
    pStop:                             TWGDIPlusGradient.IStop;
    tmpColor:                          TWColor;
    bbox:                              TGpRectF;
    center, focus, delta:              TWPointF;
    count, maxCount:                   Cardinal;
    stopCount, i:                      NativeUInt;
    stopOnNextLoop:                    Boolean;
    offsets:                           IStopOffsets;
begin
    if (not Assigned(pFill)) then
        Exit(False);

    // nothing to draw?
    if (not IsFillVisible(pFill)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get fill brush
    pGpBrush := m_pCache.GetBrush(pFill, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pGpBrush);

    if (not Assigned(pGpBrush)) then
        Exit(False);

    // fill the path using the cached brush
    pGraphics.FillPath(pGpBrush, pPath);

    // do get the path bounding box as draw rect?
    if (rect.IsEmpty) then
    begin
        // get the path bounding box
        pPath.GetBounds(bbox, nil, nil);
        drawRect := TWRectF.Create(bbox, False);
    end
    else
        drawRect := rect;

    // do apply a radial wrapping?
    if (pFill.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place between
                    // this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillPath(pWrapBrush, pPath);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place between
                    // this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillPath(pWrapBrush, pPath);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampBrush := GetBrush(pGradientFactory.GetEndColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paClampBrush := TWSmartPointer<TGpBrush>.Create(pClampBrush);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // fill the surround using the clamp brush
            pGraphics.FillPath(pClampBrush, pPath);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawPath(pPath: TGpGraphicsPath; const pStroke: TWStroke;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    drawRect:                    TWRectF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    bbox:                        TGpRectF;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the path using the cached brush
    pGraphics.DrawPath(pGpPen, pPath);

    // do get the path bounding box as draw rect?
    if (rect.IsEmpty) then
    begin
        // get the path bounding box
        pPath.GetBounds(bbox, nil, nil);
        drawRect := TWRectF.Create(bbox, False);
    end
    else
        drawRect := rect;

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawPath(pWrapPen, pPath);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawPath(pWrapPen, pPath);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp pen
            pGraphics.DrawPath(pClampPen, pPath);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.FillEllipse(x, y, width, height: Single; const pFill: TWFill;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:                  IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush, paBrush, paClampBrush: IWSmartPointer<TGpBrush>;
    pGpBrush, pClampBrush:             TGpBrush;
    pRadial:                           TWRadialGradientBrush;
    radius:                            TWSizeF;
    drawRect:                          TWRectF;
    pStop:                             TWGDIPlusGradient.IStop;
    tmpColor:                          TWColor;
    center, focus, delta:              TWPointF;
    count, maxCount:                   Cardinal;
    stopCount, i:                      NativeUInt;
    stopOnNextLoop:                    Boolean;
    offsets:                           IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsFillVisible(pFill)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get fill brush
    pGpBrush := m_pCache.GetBrush(pFill, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pGpBrush);

    if (not Assigned(pGpBrush)) then
        Exit(False);

    // fill the ellipse using the cached brush
    pGraphics.FillEllipse(pGpBrush, x, y, width, height);

    // do get the ellipse bounding box as draw rect?
    if (rect.IsEmpty) then
    begin
        drawRect.Left   := Round(x             - (width  * 0.5));
        drawRect.Top    := Round(y             - (height * 0.5));
        drawRect.Right  := Round(drawRect.Left +  width);
        drawRect.Bottom := Round(drawRect.Top  +  height);
    end
    else
        drawRect := rect;

    // do apply a radial wrapping?
    if (pFill.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillEllipse(pWrapBrush, x, y, width, height);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillEllipse(pWrapBrush, x, y, width, height);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampBrush := GetBrush(pGradientFactory.GetEndColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paClampBrush := TWSmartPointer<TGpBrush>.Create(pClampBrush);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // fill the surround using the clamp brush
            pGraphics.FillEllipse(pClampBrush, x, y, width, height);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawEllipse(x, y, width, height: Single; const pStroke: TWStroke;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    drawRect:                    TWRectF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the path using the cached brush
    pGraphics.DrawEllipse(pGpPen, x, y, width, height);

    // do get the ellipse bounding box as draw rect?
    if (rect.IsEmpty) then
    begin
        drawRect.Left   := Round(x             - (width  * 0.5));
        drawRect.Top    := Round(y             - (height * 0.5));
        drawRect.Right  := Round(drawRect.Left +  width);
        drawRect.Bottom := Round(drawRect.Top  +  height);
    end
    else
        drawRect := rect;

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false, pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawEllipse(pWrapPen, x, y, width, height);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawEllipse(pWrapPen, x, y, width, height);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp pen
            pGraphics.DrawEllipse(pClampPen, x, y, width, height);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.FillPolygon(const points: IGDIPlusPointList; const pFill: TWFill;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:                  IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush, paBrush, paClampBrush: IWSmartPointer<TGpBrush>;
    pGpBrush, pClampBrush:             TGpBrush;
    pRadial:                           TWRadialGradientBrush;
    radius:                            TWSizeF;
    pStop:                             TWGDIPlusGradient.IStop;
    tmpColor:                          TWColor;
    center, focus, delta:              TWPointF;
    count, maxCount:                   Cardinal;
    stopCount, i:                      NativeUInt;
    stopOnNextLoop:                    Boolean;
    offsets:                           IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsFillVisible(pFill)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get fill brush
    pGpBrush := m_pCache.GetBrush(pFill, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pGpBrush);

    if (not Assigned(pGpBrush)) then
        Exit(False);

    // fill the polygon using the cached brush
    pGraphics.FillPolygon(pGpBrush, PGPPointF(points), Length(points));

    // do apply a radial wrapping?
    if (pFill.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false, pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillPolygon(pWrapBrush, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // fill the surround using the wrap brush
                    pGraphics.FillPolygon(pWrapBrush, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampBrush := GetBrush(pGradientFactory.GetEndColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paClampBrush := TWSmartPointer<TGpBrush>.Create(pClampBrush);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // fill the surround using the clamp brush
            pGraphics.FillPolygon(pClampBrush, PGPPointF(points), Length(points));
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawPolygon(const points: IGDIPlusPointList; const pStroke: TWStroke;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the polygon using the cached pen
    pGraphics.DrawPolygon(pGpPen, PGPPointF(points), Length(points));

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawPolygon(pWrapPen, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawPolygon(pWrapPen, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp pen
            pGraphics.DrawPolygon(pClampPen, PGPPointF(points), Length(points));
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLine(x1, y1, x2, y2: Single; const pStroke: TWStroke;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    drawRect:                    TWRectF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the line using the cached brush
    pGraphics.DrawLine(pGpPen, x1, y1, x2, y2);

    // do get the line bounding box as draw rect?
    if (rect.IsEmpty) then
    begin
        if (x1 > x2) then
        begin
            drawRect.Left  := Round(x2);
            drawRect.Right := Round(x1);
        end
        else
        begin
            drawRect.Left  := Round(x1);
            drawRect.Right := Round(x2);
        end;

        if (y1 > y2) then
        begin
            drawRect.Top    := Round(y2);
            drawRect.Bottom := Round(y1);
        end
        else
        begin
            drawRect.Top    := Round(y1);
            drawRect.Bottom := Round(y2);
        end;
    end
    else
        drawRect := rect;

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(drawRect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawLine(pWrapPen, x1, y1, x2, y2);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawLine(pWrapPen, x1, y1, x2, y2);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(drawRect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp pen
            pGraphics.DrawLine(pClampPen, x1, y1, x2, y2);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawLines(const points: IGDIPlusPointList; const pStroke: TWStroke;
        pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:            IWSmartPointer<TWGDIPlusGradient>;
    pFill:                       IWSmartPointer<TWFill>;
    pWrapBrush:                  IWSmartPointer<TGpBrush>;
    pWrapPen, paPen, paClampPen: IWSmartPointer<TGpPen>;
    pGpPen, pClampPen:           TGpPen;
    pRadial:                     TWRadialGradientBrush;
    radius:                      TWSizeF;
    pStop:                       TWGDIPlusGradient.IStop;
    tmpColor:                    TWColor;
    center, focus, delta:        TWPointF;
    count, maxCount:             Cardinal;
    stopCount, i:                NativeUInt;
    stopOnNextLoop:              Boolean;
    offsets:                     IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsStrokeVisible(pStroke)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get outline pen
    pGpPen := m_pCache.GetPen(pStroke, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Pens) then
        paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

    if (not Assigned(pGpPen)) then
        Exit(False);

    // draw the lines using the cached brush
    pGraphics.DrawLines(pGpPen, PGPPointF(points), Length(points));

    // do apply a radial wrapping?
    if (pStroke.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pStroke.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false, pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawLines(pWrapPen, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    pFill := TWSmartPointer<TWFill>.Create();
                    pFill.SetBrush(pRadial);

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // get the wrap pen from base brush. Unfortunately cannot cache it for now
                    pWrapPen := TWSmartPointer<TGpPen>.Create(TGpPen.Create(pWrapBrush,
                            pStroke.Width));

                    // draw the surround using the wrap pen
                    pGraphics.DrawLines(pWrapPen, PGPPointF(points), Length(points));

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampPen := GetPen(pGradientFactory.GetEndColor^, pStroke.Width);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Pens) then
                paClampPen := TWSmartPointer<TGpPen>.Create(pClampPen);

            pFill := TWSmartPointer<TWFill>.Create();
            pFill.SetBrush(pRadial);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp pen
            pGraphics.DrawLines(pClampPen, PGPPointF(points), Length(points));
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.DrawString(const text: UnicodeString; const textPos: TGpPointF;
        pFont: TGpFont; const pFill: TWFill; pGraphics: TGpGraphics; const rect: TWRectF): Boolean;
var
    pGradientFactory:                  IWSmartPointer<TWGDIPlusGradient>;
    pWrapBrush, paBrush, paClampBrush: IWSmartPointer<TGpBrush>;
    pGpBrush, pClampBrush:             TGpBrush;
    pRadial:                           TWRadialGradientBrush;
    radius:                            TWSizeF;
    pStop:                             TWGDIPlusGradient.IStop;
    tmpColor:                          TWColor;
    center, focus, delta:              TWPointF;
    count, maxCount:                   Cardinal;
    stopCount, i:                      NativeUInt;
    stopOnNextLoop:                    Boolean;
    offsets:                           IStopOffsets;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    // nothing to draw?
    if (not IsFillVisible(pFill)) then
        Exit(True);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    // get fill brush
    pGpBrush := m_pCache.GetBrush(pFill, pGradientFactory);

    // is cache used? If not keep pointer in a smart pointer to auto-delete object on function ends
    if (not g_GDIPlusCacheController.m_Brushes) then
        paBrush := TWSmartPointer<TGpBrush>.Create(pGpBrush);

    if (not Assigned(pGpBrush)) then
        Exit(False);

    // draw the string using the cached brush
    pGraphics.DrawString(text, Length(text), pFont, textPos, pGpBrush);

    // do apply a radial wrapping?
    if (pFill.BrushType <> E_BT_Radial) then
        // nothing else to do
        Exit(True);

    // get the radial brush
    pRadial := pFill.Brush as TWRadialGradientBrush;
    Assert(Assigned(pRadial));

    // get the radius, may be modified in above calculations so don't set it const
    radius := pRadial.Radius;

    // check if radial gradient can be applied
    if (not pGradientFactory.DoWrap(rect, radius)) then
        // nothing else to do
        Exit(True);

    try
        // search for radial gradient wrap mode to apply
        case (pRadial.WrapMode) of
            E_WM_Tile:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    UpdateRadialGradientStops(offsets, (count + 1), stopCount, false,
                            pGradientFactory);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // draw the surround using the wrap brush
                    pGraphics.DrawString(text, Length(text), pFont, textPos, pWrapBrush);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;

            E_WM_TileFlipX,
            E_WM_TileFlipY,
            E_WM_TileFlipXY:
            begin
                stopCount := pGradientFactory.GetStopCount;
                center    := pGradientFactory.GetCenterPoint;
                focus     := pGradientFactory.GetFocusPoint;
                delta     := focus - center;

                SetLength(offsets, pGradientFactory.GetStopCount);

                // get the current stop offsets
                if (stopCount > 0) then
                    for i := 0 to stopCount - 1 do
                        offsets[i] := pGradientFactory.GetStop(i).Offset;

                pStop := nil;

                try
                    pStop := TWGDIPlusGradient.IStop.Create;

                    // insert a new stop in the gradient. The real gradient will take place
                    // between this stop and the end point
                    pStop.SetColor(pGradientFactory.StartColor);
                    pStop.Offset := 0.5;
                    pGradientFactory.InsertStop(0, pStop);
                finally
                    pStop.Free;
                end;

                stopOnNextLoop := False;
                count          := 0;
                maxCount       := 1000;

                repeat
                    pGraphics.ResetClip;

                    ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

                    CalculateRadialGradientNextRadiusAndCenterPoint(pFill, (count + 1), center,
                            delta, radius, pGradientFactory);

                    // mirror the start and end colors
                    tmpColor := pGradientFactory.GetStartColor^;
                    pGradientFactory.SetStartColor(pGradientFactory.GetEndColor);
                    pGradientFactory.SetEndColor(@tmpColor);

                    pStop := UpdateRadialGradientStops(offsets, (count + 1), stopCount, True,
                            pGradientFactory);

                    if (Assigned(pStop)) then
                        pStop.SetColor(pGradientFactory.GetStartColor);

                    // get the next wrap brush. Unfortunately cannot cache it for now
                    pWrapBrush := TWSmartPointer<TGpBrush>.Create
                            (GetRadialGradientWrapBrush(pFill, radius, pGradientFactory));

                    // draw the surround using the wrap brush
                    pGraphics.DrawString(text, Length(text), pFont, textPos, pWrapBrush);

                    if (stopOnNextLoop) then
                        break;

                    // needs to wrap again?
                    if (not pGradientFactory.DoWrap(rect, radius)) then
                        // stop on next loop to ensure the final wrap will also be drawn
                        stopOnNextLoop := True;

                    Inc(count);
                until (count > maxCount);
            end;
        else
            // get the clamp brush. It will get from the last gradient color
            pClampBrush := GetBrush(pGradientFactory.GetEndColor^);

            // is cache used? If not keep pointer in a smart pointer to auto-delete object on
            // function ends
            if (not g_GDIPlusCacheController.m_Brushes) then
                paClampBrush := TWSmartPointer<TGpBrush>.Create(pClampBrush);

            // apply a clip region to retain the painted gradient and fill the surround with the end
            // color (making thus the gradient clamped)
            ApplyRadialGradientClipRegion(pFill, radius, pGradientFactory, pGraphics);

            // draw the surround using the clamp brush
            pGraphics.DrawString(text, Length(text), pFont, textPos, pClampBrush);
        end;
    finally
        // reset the default clip region
        pGraphics.ResetClip;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetBrush(const pFill: TWFill; const pGradientFactory: TWGDIPlusGradient): TGpBrush;
begin
    Result := m_pCache.GetBrush(pFill, pGradientFactory);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetBrush(const color: TWColor): TGpBrush;
var
    pBrush:           IWSmartPointer<TWSolidBrush>;
    pFill:            IWSmartPointer<TWFill>;
    pGradientFactory: IWSmartPointer<TWGDIPlusGradient>;
begin
    pBrush       := TWSmartPointer<TWSolidBrush>.Create();
    pBrush.Color := @color;

    pFill := TWSmartPointer<TWFill>.Create();
    pFill.SetBrush(pBrush);

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    Result := m_pCache.GetBrush(pFill, pGradientFactory);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetPen(const pStroke: TWStroke; const pGradientFactory: TWGDIPlusGradient): TGpPen;
begin
    Result := m_pCache.GetPen(pStroke, pGradientFactory);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDIPlus.GetPen(const color: TWColor; width: Single): TGpPen;
var
    pBrush:           IWSmartPointer<TWSolidBrush>;
    pStroke:          IWSmartPointer<TWStroke>;
    pGradientFactory: IWSmartPointer<TWGDIPlusGradient>;
begin
    pBrush       := TWSmartPointer<TWSolidBrush>.Create();
    pBrush.Color := @color;

    pStroke       := TWSmartPointer<TWStroke>.Create();
    pStroke.SetBrush(pBrush);
    pStroke.Width := width;

    pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

    Result := m_pCache.GetPen(pStroke, pGradientFactory);
end;
//---------------------------------------------------------------------------
class function TWRenderer_GDIPlus.CompareGDIFontInstances(pFont1, pFont2: TFont): Boolean;
begin
    Result := TWGDIHelper.CompareFontInstances(pFont1, pFont2);
end;
//---------------------------------------------------------------------------
class function TWRenderer_GDIPlus.GdiPlusStatusToStr(status: GpStatus): UnicodeString;
begin
    // search for gdi+ status and return matching string
    case (status) of
        Ok:                        Result := 'Ok';
        GenericError:              Result := 'GenericError';
        InvalidParameter:          Result := 'InvalidParameter';
        OutOfMemory:               Result := 'OutOfMemory';
        ObjectBusy:                Result := 'ObjectBusy';
        InsufficientBuffer:        Result := 'InsufficientBuffer';
        NotImplemented:            Result := 'NotImplemented';
        Win32Error:                Result := 'Win32Error';
        WrongState:                Result := 'WrongState';
        Aborted:                   Result := 'Aborted';
        FileNotFound:              Result := 'FileNotFound';
        ValueOverflow:             Result := 'ValueOverflow';
        AccessDenied:              Result := 'AccessDenied';
        UnknownImageFormat:        Result := 'UnknownImageFormat';
        FontFamilyNotFound:        Result := 'FontFamilyNotFound';
        FontStyleNotFound:         Result := 'FontStyleNotFound';
        NotTrueTypeFont:           Result := 'NotTrueTypeFont';
        UnsupportedGdiplusVersion: Result := 'UnsupportedGdiplusVersion';
        GdiplusNotInitialized:     Result := 'GdiplusNotInitialized';
        PropertyNotFound:          Result := 'PropertyNotFound';
        PropertyNotSupported:      Result := 'PropertyNotSupported';
    else
        Result := 'Unknown';
    end;
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
// Global initialization procedure
//---------------------------------------------------------------------------
begin
    // configure the cache controller
    g_GDIPlusCacheController.m_Brushes      := True;
    g_GDIPlusCacheController.m_Pens         := True;
    g_GDIPlusCacheController.m_Fonts        := True;
    g_GDIPlusCacheController.m_Graphics     := False;
    g_GDIPlusCacheController.m_StringFormat := True;
end;
//---------------------------------------------------------------------------

end.
