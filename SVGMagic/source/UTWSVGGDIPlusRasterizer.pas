{**
 @abstract(@name provides a rasterizer that uses GDI+ to perform the painting of a Scalable Vector
           Graphics (SVG) image.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGGDIPlusRasterizer;

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     System.Classes,
     System.Types,
     System.Math,
     {$if CompilerVersion > 24}
        System.NetEncoding,
     {$ifend}
     Soap.EncdDecd,
     Vcl.Graphics,
     Winapi.GDIPAPI,
     Winapi.GDIPObj,
     Winapi.Windows,
     UTWColor,
     UTWFillAndStroke,
     UTWPoint,
     UTWSize,
     UTWRect,
     UTWVector,
     UTWMatrix,
     UTWHelpers,
     UTWSmartPointer,
     UTWGraphicPath,
     UTWGDIPlusGradient,
     UTWRendererCommon,
     UTWRenderer,
     UTWRenderer_GDIPlus,
     UTWControlRenderer,
     UTWSVGCommon,
     UTWSVGTags,
     UTWSVGItems,
     UTWSVGProperties,
     UTWSVGElements,
     UTWSVGGradients,
     UTWSVGAnimation,
     UTWSVGStyle,
     UTWSVGParser,
     UTWSVG,
     UTWSVGAnimationDescriptor,
     UTWSVGRasterizer;

type
    {**
     Scalable Vector Graphics (SVG) rasterizer using GDI+
     @br @bold(NOTE) See http://www.w3.org/TR/SVGCompositing/
    }
    TWSVGGDIPlusRasterizer = class(TWSVGRasterizer)
        private type
            {**
             Aspect ratio, allows to override and redefine current aspect ratio for children drawing
            }
            IAspectRatio = class
                private
                    m_ElementBox:  TWRectF; // original element view box as defined in the SVG file
                    m_ViewBox:     TWRectF; // render view box onto the canvas, this area is clipped
                    m_BoundingBox: TWRectF; // object bounding box, which will define drawing
                                            // position and size relative to the view box

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
                    property ElementBox:  TWRectF read m_ElementBox  write m_ElementBox;
                    property ViewBox:     TWRectF read m_ViewBox     write m_ViewBox;
                    property BoundingBox: TWRectF read m_BoundingBox write m_BoundingBox;
            end;

        private
            m_GDIPlusToken: ULONG_PTR;

            {**
             Draw SVG elements
             @param(pElements Elements to draw)
             @param(pos SVG position)
             @param(scaleW Scale factor to apply to width)
             @param(scaleH Scale factor to apply to height)
             @param(antialiasing If @true, antialiasing will be used, if possible)
             @param(switchMode If @true, function will return after first element is drawn (because
                               reading switch statement))
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pCanvas GDI Canvas to draw on)
             @param(pGraphics GDI+ graphics area to draw on)
             @returns(@true on success, otherwise @false)
            }
            function DrawElements(const pElements: TWSVGContainer.IElements; const pos: TPoint;
                    scaleW, scaleH: Single; antialiasing, switchMode: Boolean;
                    const animation: TWSVGRasterizer.IAnimation; pCanvas: TCanvas;
                    pGraphics: TGpGraphics): Boolean; overload;

            {**
             Draw SVG elements
             @param(pHeader SVG header)
             @param(viewBox View box as declared in SVG header)
             @param(pParentProps Properties inherited from parent group, switch or root)
             @param(pElements Elements to draw)
             @param(pos SVG position)
             @param(scaleW Scale factor to apply to width)
             @param(scaleH Scale factor to apply to height)
             @param(antialiasing If @true, antialiasing will be used, if possible)
             @param(switchMode If @true, function will return after first element is drawn (because
                               reading switch statement))
             @param(clippingMode If @true, a clip should be performed instead of a drawing)
             @param(useMode If @true, the current element to read is owned by an use link)
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pAspectRatio Aspect ratio override, ignored if @nil)
             @param(pCanvas GDI Canvas to draw on)
             @param(pGraphics GDI+ graphics area to draw on)
             @returns(@true on success, otherwise @false)
            }
            function DrawElements(const pHeader: TWSVGParser.IHeader; const viewBox: TGpRectF;
                    const pParentProps: TWSVGGDIPlusRasterizer.IProperties;
                    const pElements: TWSVGContainer.IElements; const pos: TPoint;
                    scaleW, scaleH: Single; antialiasing, switchMode, clippingMode, useMode, intersection: Boolean;
                    const animation: TWSVGRasterizer.IAnimation; pAspectRatio: IAspectRatio;
                    pCanvas: TCanvas; pGraphics: TGpGraphics): Boolean; overload;

            {**
             Check if a clipping path should be applied and apply it if yes
             @param(pHeader SVG header)
             @param(viewBox View box as declared in SVG header)
             @param(pParentProps Properties inherited from parent group, switch or root)
             @param(pElements Elements to draw)
             @param(pos SVG position)
             @param(scaleW Scale factor to apply to width)
             @param(scaleH Scale factor to apply to height)
             @param(antialiasing If @true, antialiasing will be used, if possible)
             @param(switchMode If @true, function will return after first element is drawn (because
                               reading switch statement))
             @param(clippingMode If @true, a clip should be performed instead of a drawing)
             @param(useMode If @true, the current element to read is owned by an use link)
             @param(intersection If @true, and if clipping mode is enabled, perform an intersection instead of an union)
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pAspectRatio Aspect ratio override, ignored if @nil)
             @param(pCanvas GDI Canvas to draw on)
             @param(pGraphics GDI+ graphics area to draw on)
             @param(pElement The element for which the clip path should be get)
             @param(prevRegion The previous region, if a new one was applied)
             @returns(@true on success, otherwise @false)
            }
            function ApplyClipPath(const pHeader: TWSVGParser.IHeader; const viewBox: TGpRectF;
                    const pParentProps: TWSVGGDIPlusRasterizer.IProperties;
                    const pElements: TWSVGContainer.IElements; const pos: TPoint;
                    scaleW, scaleH: Single; antialiasing, switchMode, clippingMode, useMode: Boolean;
                    const animation: TWSVGRasterizer.IAnimation; pAspectRatio: IAspectRatio;
                    pCanvas: TCanvas; pGraphics: TGpGraphics; pElement: TWSVGElement; prevRegion: TGpRegion): Boolean;

            {**
             Populate aspect ratio from element properties
             @param(pos Element position)
             @param(width Element width)
             @param(height Element height)
             @param(scaleW Scale factor to apply to width)
             @param(scaleH Scale factor to apply to height)
             @param(elementViewBox Viewbox surrounding the element)
             @param(pProps Element properties)
             @param(pAspectRatio Aspect ratio to populate)
             @returns(@true on success, otherwise @false)
            }
            function PopulateAspectRatio(pos: TPoint; width, height, scaleW, scaleH: Single;
                    elementViewBox: TWRectF; pProps: TWSVGGDIPlusRasterizer.IProperties;
                    pAspectRatio: IAspectRatio): Boolean;

            {**
             Get the closest square contained inside a rect
             @param(rect Parent rectangle)
             @returns(The closest square inside the rect)
            }
            function GetClosestSquare(const rect: TRect): TRect;

            {**
             Get the GDI+ fill mode to apply
             @param(pGlobalProps Global (or parent) properties set)
             @param(pProps Properties set)
             @returns(GDI+ fill mode to apply)
            }
            function GetFillMode(const pGlobalProps, pProps: TWSVGRasterizer.IProperties): TFillMode;

            {**
             Get brush
             @param(pStyle Style)
             @param(viewBox SVG view box)
             @param(boundingBox Bounding box of the shape on which the brush will be applied)
             @param(scaleW Scaling factor to apply to the x axis)
             @param(scaleH Scaling factor to apply to the y axis)
             @param(pRenderer GDI+ renderer)
             @param(pFill @bold([out]) Fill containing the brush, can be the key to use to get the brush from the cache)
             @returns(@true if a valid brush can be used to fill the shape, otherwise @false)
            }
            function GetBrush(const pStyle: TWSVGRasterizer.IStyle; const viewBox, boundingBox: TGpRectF;
                    scaleW, scaleH: Single; pRenderer: TWRenderer_GDIPlus; pFill: TWFill): Boolean;

            {**
             Get linear gradient brush
             @param(pGradient Linear gradient)
             @param(viewBox SVG view box)
             @param(boundingBox Bounding box of the shape on which the gradient will be applied)
             @param(stopCount Gradient stop count)
             @param(opacity Global opacity to apply to gradient)
             @param(pBrush @bold([out]) Linear gradient brush)
            }
            procedure GetLinearGradientBrush(const pGradient: TWSVGRasterizer.ILinearGradient;
                    const viewBox, boundingBox: TGpRectF; stopCount: NativeUInt; opacity: Single;
                    pBrush: TWLinearGradientBrush);

            {**
             Get radial gradient brush
             @param(pGradient Radial gradient)
             @param(viewBox SVG view box)
             @param(boundingBox Bounding box of the shape on which the gradient will be applied)
             @param(scaleW Scaling factor to apply to the x axis)
             @param(scaleH Scaling factor to apply to the y axis)
             @param(stopCount Gradient stop count)
             @param(pBrush @bold([out]) Radial gradient brush)
            }
            procedure GetRadialGradientBrush(const pGradient: TWSVGRasterizer.IRadialGradient;
                    const viewBox, boundingBox: TGpRectF; scaleW, scaleH: Single;
                    stopCount: NativeUInt; pBrush: TWRadialGradientBrush);

            {**
             Get pen
             @param(pStyle Style)
             @param(viewBox SVG view box)
             @param(boundingBox Bounding box of the shape on which the pen will be applied)
             @param(scaleW Scaling factor to apply to the x axis)
             @param(scaleH Scaling factor to apply to the y axis)
             @param(pStroke @bold([out]) Stroke containing the pen, can be the key to use to get the pen from the cache)
             @returns(@true if a valid pen can be used to draw the outline, otherwise @false)
            }
            function GetPen(const pStyle: TWSVGRasterizer.IStyle; const viewBox, boundingBox: TGpRectF;
                    scaleW, scaleH: Single; pRenderer: TWRenderer_GDIPlus; pStroke: TWStroke): Boolean;

            {**
             Calculate linear gradient vector
             @param(pGradient Linear gradient)
             @param(viewBox SVG view box)
             @param(boundingBox Bounding box of the area affected by the gradient)
             @param(vector @bold([out]) Linear gradient vector)
            }
            procedure CalculateLinearGradientVector(const pGradient: TWSVGRasterizer.ILinearGradient;
                    const viewBox, boundingBox: TGpRectF;
                    out vector: TWSVGRasterizer.ILinearGradientVector);

            {**
             Calculate final position where svg should be drawn
             @param(pos User defined position)
             @param(viewBox Viewbox bounding svg element)
             @param(scaleW Scale width factor)
             @param(scaleH Scale height factor)
             @returns(Position)
            }
            function CalculateFinalPos(const pos: TPoint; const viewBox: TGpRectF;
                    scaleW, scaleH: Single): TPoint;

            {**
             Apply aspect ratio
             @param(pAspectRatio Aspect ratio to apply)
             @param(pProps Element properties)
             @param(boundingBox Bounding box surrounding the image for which the aspect ratio should be applied)
             @param(pMatrix Image transformation matrix)
             @param(pGraphics GDI+ graphics on which the image will be rendered)
             @param(prevRegion Region applied to image before the aspect ratio was applied)
             @returns(@true on success, otherwise @false)
            }
            function ApplyAspectRatio(const pAspectRatio: IAspectRatio;
                    const pProps: TWSVGGDIPlusRasterizer.IProperties; const boundingBox: TGpRectF;
                    const pMatrix: TGpMatrix; pGraphics: TGpGraphics; prevRegion: TGpRegion): Boolean;

            {**
             Apply transformation matrix to GDI+ graphics
             @param(pMatrix Global matrix to apply)
             @param(pos SVG position)
             @param(scaleW Scale factor to apply to width)
             @param(scaleH Scale factor to apply to height)
             @param(pGraphics GDI+ graphics on which matrix should be applied)
            }
            procedure ApplyMatrix(const pMatrix: TGpMatrix; const pos: TPoint; scaleW, scaleH: Single;
                    pGraphics: TGpGraphics);

            {**
             Update a bounding box
             @param(point New point to add to bounding box)
             @param(boundingBox @bold([in, out]) Bounding box, new bounding box on function ends)
            }
            procedure UpdateBoundingBox(const point: TGpPointF; var boundingBox: TGpRectF);

            {**
             Apply the watermark above the draw when the library is compiled as trial version
             @param(rect SVG rect)
             @param(pos SVG position)
             @param(scaleW Scale width factor)
             @param(scaleH Scale height factor)
             @param(pCanvas GDI canvas from which the GDI+ graphics was get)
             @param(pGraphics GDI+ graphics on which the SVG is drawn)
             @br @bold(NOTE) Actual method name is "obfuscated"
             @exclude(From PasDoc documentation)
            }
            {$ifdef TRIAL_BUILD}
                procedure PrepareRenderer(const rect: TWRectF; const pos: TPoint; scaleW, scaleH: Single;
                        pCanvas: TCanvas; pGraphics: TGpGraphics);
            {$endif}

        public
            {**
             Constructor
             @param(token GDI+ token)
            }
            constructor Create(token: ULONG_PTR); reintroduce; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Draw SVG on canvas
             @param(pSVG SVG to draw)
             @param(pos Draw position in pixels)
             @param(scale Scale factor)
             @param(antialiasing If @true, antialiasing should be used (if possible))
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pCanvas Canvas to draw on)
             @returns(@true on success, otherwise @false)
            }
            function Draw(const pSVG: TWSVG; const pos: TPoint; scale: Single; antialiasing: Boolean;
                    const animation: TWSVGRasterizer.IAnimation; pCanvas: TCustomCanvas): Boolean; overload; override;

            {**
             Draw SVG on canvas
             @param(pSVG SVG to draw)
             @param(rect Rect in which svg will be drawn)
             @param(proportional If @true, svg proportions will be conserved)
             @param(antialiasing If @true, antialiasing should be used (if possible))
             @param(animation Animation params, containing e.g. position in percent (between 0 and 100))
             @param(pCanvas Canvas to draw on)
             @returns(@true on success, otherwise @false)
            }
            function Draw(const pSVG: TWSVG; const rect: TRect; proportional, antialiasing: Boolean;
                    const animation: TWSVGRasterizer.IAnimation; pCanvas: TCustomCanvas): Boolean; overload; override;
    end;

implementation

uses
  System.UITypes;

//---------------------------------------------------------------------------
// Resources
//---------------------------------------------------------------------------
{**
 @exclude(Trademark stamp used for the watermark)
 @exclude(NOTE The real source data is 'mmt332*TTM%çt3f®nwfdv x ööénf+Q"*çTç89Ltkn'. The rule to
               apply is:
               - The length of the string is constant and equals to 56 chars
               - The length of the decoded string is constant and equals to 42 chars
               - The CRC of the decoded string is $6566259 for the full version, and $6d712bd for the trial
               - The char to use to draw the watermark is located on the 16th position)
}
{$ifdef TRIAL_BUILD}
    const C_WMTM_Stamp: AnsiString = 'bW10MzMyKlRUTSXndDNmrm53ZmR2IHgg9vbpbmYrUSIq51TnODlMdGtu';
{$else}
    const C_WMTM_Stamp: AnsiString = 'Z0QzNGZfIOjpM0RmQ0NndiBzIiVnRDE5akZ2P29wZ0zgUmPnNOdkY2Zm';
{$endif}
//---------------------------------------------------------------------------
// TWSVGGDIPlusRasterizer.IAspectRatio
//---------------------------------------------------------------------------
constructor TWSVGGDIPlusRasterizer.IAspectRatio.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWSVGGDIPlusRasterizer.IAspectRatio.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
// TWSVGGDIPlusRasterizer
//---------------------------------------------------------------------------
constructor TWSVGGDIPlusRasterizer.Create(token: ULONG_PTR);
begin
    inherited Create;

    m_GDIPlusToken := token;
end;
//---------------------------------------------------------------------------
destructor TWSVGGDIPlusRasterizer.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.DrawElements(const pElements: TWSVGContainer.IElements; const pos: TPoint;
        scaleW, scaleH: Single; antialiasing, switchMode: Boolean;
        const animation: TWSVGRasterizer.IAnimation; pCanvas: TCanvas;
        pGraphics: TGpGraphics): Boolean;
var
    pProperties, pHeaderProps: IWSmartPointer<IProperties>;
    pElement:                  TWSVGElement;
    pHeader:                   TWSVGParser.IHeader;
    viewBox:                   TWRectF;
    pAnimationData:            IWSmartPointer<IAnimationData>;
begin
    pProperties := TWSmartPointer<IProperties>.Create();

    // load default properties
    pProperties.Default;

    // iterate through SVG elements
    for pElement in pElements do
    begin
        // get svg header (should always be the first element, because header is contained inside
        // svg tag itself, that is the root tag)
        if ((pElement.ItemName = C_SVG_Tag_Name) and (pElement is TWSVGParser.IHeader)) then
        begin
            pHeader := pElement as TWSVGParser.IHeader;

            // found it?
            if (not Assigned(pHeader)) then
                continue;

            // get view box
            viewBox := GetViewBox(pHeader);

            try
                pHeaderProps   := TWSmartPointer<IProperties>.Create();
                pAnimationData := TWSmartPointer<IAnimationData>.Create();

                // get header properties
                if (not GetElementProps(pElement, pHeaderProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                pHeaderProps.Merge(pProperties);

                Exit(DrawElements(pHeader, viewBox.ToGpRectF, pHeaderProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, False, False, False, animation, nil,
                        pCanvas, pGraphics));
            finally
                {$ifdef TRIAL_BUILD}
                    // apply trial timesamp
                    PrepareRenderer(viewBox, pos, scaleW, scaleH, pCanvas, pGraphics);
                {$endif}
            end;
        end;
    end;

    Result := False;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.DrawElements(const pHeader: TWSVGParser.IHeader; const viewBox: TGpRectF;
        const pParentProps: TWSVGGDIPlusRasterizer.IProperties; const pElements: TWSVGContainer.IElements;
        const pos: TPoint; scaleW, scaleH: Single; antialiasing, switchMode, clippingMode, useMode, intersection: Boolean;
        const animation: TWSVGRasterizer.IAnimation; pAspectRatio: IAspectRatio; pCanvas: TCanvas;
        pGraphics: TGpGraphics): Boolean;
var
    pClonedElements:                                                                                  IWSmartPointer<TWSVGContainer.IElements>;
    pRenderer:                                                                                        TWRenderer_GDIPlus;
    pElement, pLinkedElement:                                                                         TWSVGElement;
    pClone:                                                                                           IWSmartPointer<TWSVGElement>;
    pSwitch:                                                                                          TWSVGSwitch;
    pGroup:                                                                                           TWSVGGroup;
    pAction:                                                                                          TWSVGAction;
    pUse:                                                                                             TWSVGUse;
    pSymbol:                                                                                          TWSVGSymbol;
    pEmbeddedSVG:                                                                                     TWSVGSVG;
    pPath:                                                                                            TWSVGPath;
    pRect:                                                                                            TWSVGRect;
    pCircle:                                                                                          TWSVGCircle;
    pEllipse:                                                                                         TWSVGEllipse;
    pLine:                                                                                            TWSVGLine;
    pPolygon:                                                                                         TWSVGPolygon;
    pPolyline:                                                                                        TWSVGPolyline;
    pImage:                                                                                           TWSVGImage;
    pText:                                                                                            TWSVGText;
    pProps:                                                                                           IWSmartPointer<IProperties>;
    pAnimationData:                                                                                   IWSmartPointer<IAnimationData>;
    pGraphicsPath, pPolylinePath, pTextPath:                                                          IWSmartPointer<TGpGraphicsPath>;
    pMatrix:                                                                                          IWSmartPointer<TGpMatrix>;
    pPathConverter:                                                                                   IWSmartPointer<TWGraphicPathConverter_GDIPlus>;
    pRectOptions:                                                                                     IWSmartPointer<TWRenderer.IRectOptions>;
    pFill:                                                                                            IWSmartPointer<TWFill>;
    pStroke:                                                                                          IWSmartPointer<TWStroke>;
    paPen:                                                                                            IWSmartPointer<TGpPen>;
    pFont:                                                                                            IWSmartPointer<TGpFont>;
    pFontFamily:                                                                                      IWSmartPointer<TGPFontFamily>;
    pTextFont:                                                                                        IWSmartPointer<TFont>;
    pTextFormat:                                                                                      IWSmartPointer<TGpStringFormat>;
    pGradientFactory:                                                                                 IWSmartPointer<TWGDIPlusGradient>;
    pRegion, pPrevRegion, pCurRegion, pPrevAspectRatioRegion:                                         IWSmartPointer<TGpRegion>;
    pImageData:                                                                                       IWSmartPointer<TMemoryStream>;
    pAspectRatioOverride:                                                                             IWSmartPointer<IAspectRatio>;
    pAspectRatioToUse:                                                                                IAspectRatio;
    pGraphic:                                                                                         TGraphic;
    textMetrics:                                                                                      TEXTMETRIC;
    charRange:                                                                                        TCharacterRange;
    charRegions:                                                                                      array of TGpRegion;
    points:                                                                                           TWRenderer_GDIPlus.IGDIPlusPointList;
    pGpPen, pFakePen:                                                                                 TGpPen;
    point, textPos:                                                                                   TGpPointF;
    boundingBox, rectToDraw, charRect:                                                                TGpRectF;
    rect, imageRect, elementViewBox:                                                                  TWRectF;
    iRect:                                                                                            TRect;
    svgPos, posFromProps:                                                                             TPoint;
    color:                                                                                            TWColor;
    outputMatrix:                                                                                     TWMatrix3x3;
    dashPatternCount, i:                                                                              NativeInt;
    count:                                                                                            NativeUInt;
    x, y, initialX, initialY, x1, y1, x2, y2, r, rx, ry, d, dx, dy, width, height, dashFactor, coord: Single;
    fontSize, fontStyleAngle:                                                                         Single;
    fontWeight:                                                                                       Cardinal;
    fontFamily, fontFamilyLowerCase:                                                                  UnicodeString;
    anchor:                                                                                           IETextAnchor;
    decoration:                                                                                       IETextDecoration;
    imageType:                                                                                        IEImageType;
    pImageOptions:                                                                                    TWRenderer.IImageOptions;
    fontStyle:                                                                                        TWSVGText.IEFontStyle;
    gdiFontStyle:                                                                                     TFontStyles;
    isXCoord, isClipped, isAspectRatioClipped, bolder, lighter:                                       Boolean;
begin
    // svg header should always be declared, otherwise svg data is malformed (NOTE svg header
    // element should exist even if the svg tag contains nothing else)
    if (not Assigned(pHeader)) then
        raise Exception.Create('SVG is malformed');

    // get GDI+ renderer
    pRenderer := TWControlRenderer.GetGDIPlusRenderer;

    // found it?
    if (not Assigned(pRenderer)) then
        Exit(False);

    // iterate through SVG elements
    for pElement in pElements do
    begin
        // is a group?
        if (pElement is TWSVGGroup) then
        begin
            // get group
            pGroup := pElement as TWSVGGroup;

            // found it?
            if (Assigned(pGroup)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pGroup, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pGroup, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pGroup, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the group position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // draw group subelements
                if (not DrawElements(pHeader, viewBox, pProps, pGroup.ElementList, posFromProps,
                        scaleW, scaleH, antialiasing, False, False, useMode, intersection, animation,
                        pAspectRatio, pCanvas, pGraphics))
                then
                begin
                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                    Exit(False);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                continue;
            end;
        end;

        // is a switch?
        if (pElement is TWSVGSwitch) then
        begin
            // get switch
            pSwitch := pElement as TWSVGSwitch;

            // found it?
            if (Assigned(pSwitch)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pSwitch, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pSwitch, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pSwitch, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the switch position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // draw switch subelements
                if (not DrawElements(pHeader, viewBox, pProps, pSwitch.ElementList, posFromProps,
                        scaleW, scaleH, antialiasing, True, False, useMode, intersection, animation,
                        pAspectRatio, pCanvas, pGraphics))
                then
                begin
                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                    Exit(False);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                continue;
            end;
        end;

        // is an action?
        if (pElement is TWSVGAction) then
        begin
            // get action
            pAction := pElement as TWSVGAction;

            // found it?
            if (Assigned(pAction)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pAction, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pAction, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pAction, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the action position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // draw action subelements
                if (not DrawElements(pHeader, viewBox, pProps, pAction.ElementList, posFromProps,
                        scaleW, scaleH, antialiasing, False, False, useMode, intersection, animation,
                        pAspectRatio, pCanvas, pGraphics))
                then
                begin
                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                    Exit(False);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                continue;
            end;
        end;

        // is an use instruction?
        if (pElement is TWSVGUse) then
        begin
            // get use instruction
            pUse := pElement as TWSVGUse;

            // found it?
            if (Assigned(pUse)) then
            begin
                // get the linked element to use
                if (not GetLinkedElementToUse(pUse, pLinkedElement)) then
                    // don't care if link was not found, just continue with next element. It's not
                    // unusual that a SVG contains links pointing to nothing
                    continue;

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pUse, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pUse, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pUse, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the group position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // clone the source element. This is required because several additional properties
                // will be merged from the use instruction
                pClone := TWSmartPointer<TWSVGElement>.Create(pLinkedElement.CreateInstance(pLinkedElement.Parent));
                pClone.Assign(pLinkedElement);

                // enable the lines below to log the use and clone elements properties
                {$ifdef DEBUG}
                    //LogProps(pUse);
                    //LogProps(pClone);
                {$endif}

                // create a pseudo element container, and add the cloned element to draw
                pClonedElements := TWSmartPointer<TWSVGContainer.IElements>.Create(TWSVGContainer.IElements.Create(False));
                pClonedElements.Add(pClone);

                // draw the cloned element
                if (not DrawElements(pHeader, viewBox, pProps, pClonedElements, posFromProps, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, True, intersection, animation,
                        pAspectRatio, pCanvas, pGraphics))
                then
                    Exit(False);

                continue;
            end;
        end;

        // is a symbol?
        if (pElement is TWSVGSymbol) then
        begin
            // not allowed to be drawn if not called from an use instruction
            if (not useMode) then
                continue;

            // get group
            pSymbol := pElement as TWSVGSymbol;

            // found it?
            if (Assigned(pSymbol)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pSymbol, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pSymbol, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pSymbol, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the symbol position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // should apply an aspect ratio?
                if ((pProps.AspectRatio.Defined.Value) and (not elementViewBox.IsEmpty)) then
                begin
                    pAspectRatioOverride := TWSmartPointer<IAspectRatio>.Create();
                    pAspectRatioToUse    := pAspectRatioOverride;

                    // calculate aspect ratio from svg properties and populate it
                    if (not PopulateAspectRatio(posFromProps, width, height, scaleW, scaleH,
                            elementViewBox, pProps, pAspectRatioOverride))
                    then
                        pAspectRatioToUse := pAspectRatio;
                end
                else
                    pAspectRatioToUse := pAspectRatio;

                // draw symbol subelements
                if (not DrawElements(pHeader, viewBox, pProps, pSymbol.ElementList, posFromProps,
                        scaleW, scaleH, antialiasing, switchMode, clippingMode, useMode, intersection,
                        animation, pAspectRatioToUse, pCanvas, pGraphics))
                then
                begin
                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                    Exit(False);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                continue;
            end;
        end;

        // is an embedded SVG?
        if (pElement is TWSVGSVG) then
        begin
            // get embedded SVG
            pEmbeddedSVG := pElement as TWSVGSVG;

            // found it?
            if (Assigned(pEmbeddedSVG)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pEmbeddedSVG, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this container
                GetAnimations(pEmbeddedSVG, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pEmbeddedSVG, x, y, width, height, elementViewBox,
                        pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                // get the embedded SVG position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // should apply an aspect ratio?
                if ((pProps.AspectRatio.Defined.Value) and (not elementViewBox.IsEmpty)) then
                begin
                    pAspectRatioOverride := TWSmartPointer<IAspectRatio>.Create();
                    pAspectRatioToUse    := pAspectRatioOverride;

                    // calculate aspect ratio from svg properties and populate it
                    if (not PopulateAspectRatio(posFromProps, width, height, scaleW, scaleH,
                            elementViewBox, pProps, pAspectRatioOverride))
                    then
                        pAspectRatioToUse := pAspectRatio;
                end
                else
                    pAspectRatioToUse := pAspectRatio;

                // draw embedded SVG subelements
                if (not DrawElements(pHeader, viewBox, pProps, pEmbeddedSVG.ElementList,
                        posFromProps, scaleW, scaleH, antialiasing, False, False, useMode, intersection,
                        animation, pAspectRatioToUse, pCanvas, pGraphics))
                then
                begin
                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                    Exit(False);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                continue;
            end;
        end;

        // is a path?
        if (pElement is TWSVGPath) then
        begin
            // get path
            pPath := pElement as TWSVGPath;

            // found it?
            if (Assigned(pPath)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pPath, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pPath, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pPath, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the path position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // create new GDI+ path. NOTE create explicitly the graphics path before keep it
                // inside the smart pointer, because otherwise the incorrect constructor is called
                // while the smart pointer tries to auto-create the object, causing thus that the
                // path is never drawn
                pGraphicsPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                rect := Default(TWRectF);

                // should apply an aspect ratio onto the path?
                if (Assigned(pAspectRatio)) then
                begin
                    // use the aspect ratio bounding box position instead
                    rect.Right  := pAspectRatio.m_BoundingBox.Width;
                    rect.Bottom := pAspectRatio.m_BoundingBox.Height;
                end
                else
                begin
                    rect.Right  := viewBox.Width;
                    rect.Bottom := viewBox.Height;
                end;

                pPathConverter :=
                        TWSmartPointer<TWGraphicPathConverter_GDIPlus>.Create
                                (TWGraphicPathConverter_GDIPlus.Create(pGraphicsPath));

                // get path to draw
                if (not pPathConverter.Process(rect, pPath.Commands)) then
                    Exit(False);

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the path?
                if (Assigned(pAspectRatio)) then
                begin
                    pFakePen := nil;

                    // create a fake pen. The color is not important, but the width will be used to
                    // measure the path bounds. Without a such pen, the stroke may be calculated
                    // incorrectly around the path. NOTE don't worry if the pen is nil, in this case
                    // the GetBounds() function will measure from the middle of the stroke
                    if (pProps.Style.Stroke.Width.Value > 0.0) then
                    begin
                        color.SetColor(clBlack);
                        pFakePen := pRenderer.GetPen(color, pProps.Style.Stroke.Width.Value);
                    end;

                    // measure the path bounding box
                    pGraphicsPath.GetBounds(boundingBox, nil, pFakePen);

                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(posFromProps, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    if (intersection) then
                        pGraphicsPath.SetFillMode(FillModeWinding)
                    else
                        pGraphicsPath.SetFillMode(GetFillMode(pParentProps, pProps));

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pGraphicsPath));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    pGraphicsPath.SetFillMode(GetFillMode(pParentProps, pProps));

                     // get the path bounding box
                    if ((pProps.Style.Fill.Brush.BrushType <> E_BT_Solid)
                            or (pProps.Style.Stroke.Brush.BrushType <> E_BT_Solid))
                    then
                    begin
                        pFakePen := nil;

                        // create a fake pen. The color is not important, but the width will be used to
                        // measure the path bounds. Without a such pen, the stroke may be calculated
                        // incorrectly around the path. NOTE don't worry if the pen is nil, in this case
                        // the GetBounds() function will measure from the middle of the stroke
                        if (pProps.Style.Stroke.Width.Value > 0.0) then
                        begin
                            color.SetColor(clBlack);
                            pFakePen := pRenderer.GetPen(color, pProps.Style.Stroke.Width.Value);
                        end;

                        // measure the path bounding box
                        pGraphicsPath.GetBounds(boundingBox, nil, pFakePen);
                    end;

                    pFill := TWSmartPointer<TWFill>.Create();

                    // draw the path
                    if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                        pRenderer.FillPath(pGraphicsPath, pFill, pGraphics, TWRectF.Create(boundingBox, False));

                    pStroke := TWSmartPointer<TWStroke>.Create();

                    // outline the path
                    if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                        pRenderer.DrawPath(pGraphicsPath, pStroke, pGraphics, TWRectF.Create(boundingBox, False));
                end;

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a rectangle?
        if (pElement is TWSVGRect) then
        begin
            // get rectangle
            pRect := pElement as TWSVGRect;

            // found it?
            if (Assigned(pRect)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pRect, pPrevRegion);

                pRectOptions := TWSmartPointer<TWRenderer.IRectOptions>.Create();

                // create and populate rect options
                pRectOptions.Antialiasing := antialiasing;

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pRect, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract properties from rectangle
                if (not GetRectProps(pRect, rectToDraw.X, rectToDraw.Y, rectToDraw.Width, rectToDraw.Height,
                        rx, ry, pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    pMatrix := TWSmartPointer<TGpMatrix>.Create();
                    pProps.Matrix.Value.ToGpMatrix(pMatrix);

                    // set svg element to final size
                    pMatrix.Scale(scaleW, scaleH, MatrixOrderAppend);

                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                    // set svg element to final location (applying user position and viewbox correction)
                    pMatrix.Translate(svgPos.X, svgPos.Y, MatrixOrderAppend);

                    // apply transformation matrix to rectangle
                    pGraphics.SetTransform(pMatrix);

                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(rectToDraw));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    // the dash factor is a value used to take care of the difference between svg and GDI+.
                    // In fact, GDI+ multiplies each dash values by the stroke width
                    if (pProps.Style.Stroke.Width.Value > 0) then
                        dashFactor := pProps.Style.Stroke.Width.Value
                    else
                        dashFactor := 1.0;

                    dashPatternCount := pProps.Style.Stroke.DashPattern.Value.Count;

                    // apply the dash factor on the dash pattern
                    for i := 0 to dashPatternCount - 1 do
                        pRectOptions.Stroke.DashPattern.Add
                                (pProps.Style.Stroke.DashPattern.Value[i] / dashFactor);

                    // populate rect options with read values
                    pRectOptions.Stroke.Width         := pProps.Style.Stroke.Width.Value;
                    pRectOptions.Stroke.DashOffset    := pProps.Style.Stroke.DashOffset.Value / dashFactor;
                    pRectOptions.Radius.LeftTop.X     := Round(rx);
                    pRectOptions.Radius.LeftTop.Y     := Round(ry);
                    pRectOptions.Radius.LeftBottom.X  := pRectOptions.Radius.LeftTop.X;
                    pRectOptions.Radius.LeftBottom.Y  := pRectOptions.Radius.LeftTop.Y;
                    pRectOptions.Radius.RightTop.X    := pRectOptions.Radius.LeftTop.X;
                    pRectOptions.Radius.RightTop.Y    := pRectOptions.Radius.LeftTop.Y;
                    pRectOptions.Radius.RightBottom.X := pRectOptions.Radius.LeftTop.X;
                    pRectOptions.Radius.RightBottom.Y := pRectOptions.Radius.LeftTop.Y;

                    pMatrix := TWSmartPointer<TGpMatrix>.Create();
                    pProps.Matrix.Value.ToGpMatrix(pMatrix);

                    // set svg element to final size
                    pMatrix.Scale(scaleW, scaleH, MatrixOrderAppend);

                    isAspectRatioClipped := False;

                    // should apply an aspect ratio onto the rectangle?
                    if (Assigned(pAspectRatio)) then
                    begin
                        pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                        // apply aspect ratio and get the previous clipping region, if any
                        isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, rectToDraw,
                                pMatrix, pGraphics, pPrevAspectRatioRegion);
                    end
                    else
                    begin
                        // calculate position at which svg element should be drawn
                        svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                        // set svg element to final location (applying user position and viewbox correction)
                        pMatrix.Translate(svgPos.X, svgPos.Y, MatrixOrderAppend);
                    end;

                    // apply transformation matrix to rectangle
                    outputMatrix := TWMatrix3x3.Create(pMatrix);
                    pRectOptions.TransformMatrix.Assign(outputMatrix);

                    GetBrush(pProps.Style, viewBox, rectToDraw, scaleW, scaleH, pRenderer, pRectOptions.Fill);
                    GetPen  (pProps.Style, viewBox, rectToDraw, scaleW, scaleH, pRenderer, pRectOptions.Stroke);

                    // draw rectangle
                    pRenderer.DrawRect(TWRectF.Create(rectToDraw, False), pRectOptions, pGraphics, iRect);

                    // restore the previous cliping before aspect ratio, if any
                    if (isAspectRatioClipped) then
                        pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);
                end;

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a circle?
        if (pElement is TWSVGCircle) then
        begin
            // get circle
            pCircle := pElement as TWSVGCircle;

            // found it?
            if (Assigned(pCircle)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pCircle, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pCircle, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pCircle, initialX, initialY, width, height, elementViewBox,
                        pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                // extract properties from circle
                if (not GetCircleProps(pCircle, x, y, r, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // calculate the final circle position
                x := x + initialX;
                y := y + initialY;

                // calculate diameter
                d := r * 2.0;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the circle?
                if (Assigned(pAspectRatio)) then
                begin
                    // get the bounding box surrounding the circle (before transformation)
                    boundingBox.X      := x - r;
                    boundingBox.Y      := y - r;
                    boundingBox.Width  := d;
                    boundingBox.Height := d;

                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    // create new GDI+ path
                    pGraphicsPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                    if (intersection) then
                        pGraphicsPath.SetFillMode(FillModeWinding);

                    pGraphicsPath.AddEllipse(x - r, y - r, d, d);

                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pGraphicsPath));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    // calculate the circle bounding box
                    if ((pProps.Style.Fill.Brush.BrushType <> E_BT_Solid)
                            or (pProps.Style.Stroke.Brush.BrushType <> E_BT_Solid))
                    then
                    begin
                        // todo FIXME -cCheck -cBug -oJean: There is something wrong in the way the
                        //                                  bounding box width and height are calculated,
                        //                                  should not include the x and y positions.
                        //                                  Also check if may be merged with the aspect
                        //                                  ratio bounding box calculation above
                        boundingBox.X      := x             - r;
                        boundingBox.Y      := y             - r;
                        boundingBox.Width  := boundingBox.X + d;
                        boundingBox.Height := boundingBox.Y + d;
                    end;

                    pFill := TWSmartPointer<TWFill>.Create();

                    // draw the circle
                    if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                        pRenderer.FillEllipse(x - r, y - r, d, d, pFill, pGraphics, TWRectF.Create(boundingBox, False));

                    pStroke := TWSmartPointer<TWStroke>.Create();

                    // outline the circle
                    if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                        pRenderer.DrawEllipse(x - r, y - r, d, d, pStroke, pGraphics, TWRectF.Create(boundingBox, False));
                end;

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is an ellipse?
        if (pElement is TWSVGEllipse) then
        begin
            // get ellipse
            pEllipse := pElement as TWSVGEllipse;

            // found it?
            if (Assigned(pEllipse)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pEllipse, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pEllipse, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pEllipse, initialX, initialY, width, height, elementViewBox,
                        pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                // extract properties from ellipse
                if (not GetEllipseProps(pEllipse, x, y, rx, ry, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // calculate the final ellipse position
                x := x + initialX;
                y := y + initialY;

                // calculate x and y diameters
                dx := rx * 2.0;
                dy := ry * 2.0;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the ellipse?
                if (Assigned(pAspectRatio)) then
                begin
                    // get the bounding box surrounding the ellipse (before transformation)
                    boundingBox.X      := x - rx;
                    boundingBox.Y      := y - ry;
                    boundingBox.Width  := dx;
                    boundingBox.Height := dy;

                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    // create new GDI+ path
                    pGraphicsPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                    if (intersection) then
                        pGraphicsPath.SetFillMode(FillModeWinding);

                    pGraphicsPath.AddEllipse(x - rx, y - ry, dx, dy);

                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pGraphicsPath));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    // calculate the circle bounding box
                    if ((pProps.Style.Fill.Brush.BrushType <> E_BT_Solid)
                            or (pProps.Style.Stroke.Brush.BrushType <> E_BT_Solid))
                    then
                    begin
                        // todo FIXME -cCheck -cBug -oJean: There is something wrong in the way the
                        //                                  bounding box width and height are calculated,
                        //                                  should not include the x and y positions.
                        //                                  Also check if may be merged with the aspect
                        //                                  ratio bounding box calculation above
                        boundingBox.X      := x             - rx;
                        boundingBox.Y      := y             - ry;
                        boundingBox.Width  := boundingBox.X + dx;
                        boundingBox.Height := boundingBox.Y + dy;
                    end;

                    pFill := TWSmartPointer<TWFill>.Create();

                    // draw the ellipse
                    if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                        pRenderer.FillEllipse(x - rx, y - ry, dx, dy, pFill, pGraphics, TWRectF.Create(boundingBox, False));

                    pStroke := TWSmartPointer<TWStroke>.Create();

                    // outline the ellipse
                    if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                        pRenderer.DrawEllipse(x - rx, y - ry, dx, dy, pStroke, pGraphics, TWRectF.Create(boundingBox, False));
                end;

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a line?
        if (pElement is TWSVGLine) then
        begin
            // get line
            pLine := pElement as TWSVGLine;

            // found it?
            if (Assigned(pLine)) then
            begin
                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pLine, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pLine, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // extract properties from line
                if (not GetLineProps(pLine, x1, y1, x2, y2, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // calculate the final line position
                x1 := x1 + x;
                y1 := y1 + y;
                x2 := x2 + x;
                y2 := y2 + y;

                // calculate the line bounding box
                if ((pProps.Style.Stroke.Brush.BrushType <> E_BT_Solid) or Assigned(pAspectRatio)) then
                begin
                    if (x1 > x2) then
                    begin
                        boundingBox.X      := x2;
                        boundingBox.Width  := x1 - x2;
                    end
                    else
                    begin
                        boundingBox.X      := x1;
                        boundingBox.Width  := x2 - x1;
                    end;

                    if (y1 > y2) then
                    begin
                        boundingBox.Y      := y2;
                        boundingBox.Height := y1 - y2;
                    end
                    else
                    begin
                        boundingBox.Y      := y1;
                        boundingBox.Height := y2 - y1;
                    end;
                end;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the line?
                if (Assigned(pAspectRatio)) then
                begin
                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                pStroke := TWSmartPointer<TWStroke>.Create();

                // draw the line
                if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                    pRenderer.DrawLine(x1, y1, x2, y2, pStroke, pGraphics, TWRectF.Create(boundingBox, False));

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a polygon?
        if (pElement is TWSVGPolygon) then
        begin
            // get polygon
            pPolygon := pElement as TWSVGPolygon;

            // found it?
            if (Assigned(pPolygon)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pPolygon, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pPolygon, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pPolygon, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the polygon position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));
                count        := 0;

                // reset point list (to avoid that previous content interfers with the new one)
                SetLength(points, 0);

                // iterate through points
                for coord in pPolygon.Points do
                begin
                    // is x or y coordinate?
                    isXCoord := ((count mod 2) = 0);

                    // is x cordinate?
                    if (isXCoord) then
                    begin
                        // set point x coordinate
                        point.X := coord;
                        Inc(count);
                        continue;
                    end;

                    // set point y coordinate
                    point.Y := coord;

                    // update bounding box
                    UpdateBoundingBox(point, boundingBox);

                    // add point to list
                    SetLength(points, Length(points) + 1);
                    points[Length(points) - 1] := point;

                    Inc(count);
                end;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the polygon?
                if (Assigned(pAspectRatio)) then
                begin
                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(posFromProps, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    // create new GDI+ path
                    pGraphicsPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                    if (intersection) then
                        pGraphicsPath.SetFillMode(FillModeWinding);

                    pGraphicsPath.AddPolygon(PGPPointF(points), Length(points));

                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pGraphicsPath));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    pFill := TWSmartPointer<TWFill>.Create();

                    // draw the polygon
                    if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                        pRenderer.FillPolygon(points, pFill, pGraphics, TWRectF.Create(boundingBox, False));

                    pStroke := TWSmartPointer<TWStroke>.Create();

                    // outline the polygon
                    if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                        pRenderer.DrawPolygon(points, pStroke, pGraphics, TWRectF.Create(boundingBox, False));
                end;

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a polyline?
        if (pElement is TWSVGPolyline) then
        begin
            // get polyline
            pPolyline := pElement as TWSVGPolyline;

            // found it?
            if (Assigned(pPolyline)) then
            begin
                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pPolyline, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract position and size properties
                if (not GetPosAndSizeProps(pPolyline, x, y, width, height, elementViewBox, pAnimationData,
                        animation.m_pCustomData))
                then
                    Exit(False);

                // get the polyline position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                // reset point list (to avoid that previous content interfers with the new one)
                SetLength(points, 0);

                count := 0;

                // iterate through points
                for coord in pPolyline.Points do
                begin
                    // is x or y coordinate?
                    isXCoord := ((count mod 2) = 0);

                    // is x cordinate?
                    if (isXCoord) then
                    begin
                        // set point x coordinate
                        point.X := coord;
                        Inc(count);
                        continue;
                    end;

                    // set point y coordinate
                    point.Y := coord;

                    // update bounding box
                    UpdateBoundingBox(point, boundingBox);

                    // add point to list
                    SetLength(points, Length(points) + 1);
                    points[Length(points) - 1] := point;

                    Inc(count);
                end;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the lines?
                if (Assigned(pAspectRatio)) then
                begin
                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(posFromProps, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                pFill := TWSmartPointer<TWFill>.Create();

                // draw the lines
                if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                begin
                    // create new GDI+ path. NOTE create explicitly the graphics path before keep it
                    // inside the smart pointer, because otherwise the incorrect constructor is called
                    // while the smart pointer tries to auto-create the object, causing thus that the
                    // path is never drawn
                    pPolylinePath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                    // add lines in path, thus they will be able to be filled (there are no native
                    // FillPath() function in the graphics)
                    pPolylinePath.StartFigure;
                    pPolylinePath.AddLines(PGPPointF(points), Length(points));
                    pPolylinePath.CloseFigure;

                    pPolylinePath.SetFillMode(GetFillMode(pParentProps, pProps));

                    pRenderer.FillPath(pPolylinePath, pFill, pGraphics, TWRectF.Create(boundingBox, False));
                end;

                pStroke := TWSmartPointer<TWStroke>.Create();

                // draw the lines
                if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                    pRenderer.DrawLines(points, pStroke, pGraphics, TWRectF.Create(boundingBox, False));

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is an image?
        if (pElement is TWSVGImage) then
        begin
            // get image element
            pImage := pElement as TWSVGImage;

            // found it?
            if (Assigned(pImage)) then
            begin
                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pImage, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pImage, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                pImageData := TWSmartPointer<TMemoryStream>.Create();

                // extract properties from image
                if (not GetImageProps(pImage, x, y, width, height, elementViewBox, imageType,
                        pImageData, pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                // get the image position (in relation to the initial position)
                posFromProps := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

                pAspectRatioToUse := nil;

                // should apply an aspect ratio?
                if ((pProps.AspectRatio.Defined.Value) and (not elementViewBox.IsEmpty)) then
                begin
                    pAspectRatioOverride := TWSmartPointer<IAspectRatio>.Create();
                    pAspectRatioToUse    := pAspectRatioOverride;

                    // calculate aspect ratio from svg properties and populate it
                    if (not PopulateAspectRatio(posFromProps, width, height, scaleW, scaleH,
                            elementViewBox, pProps, pAspectRatioOverride))
                    then
                        pAspectRatioToUse := nil;
                end;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                pGraphic             := nil;
                pImageOptions        := nil;
                isAspectRatioClipped := False;

                try
                    // get the image to draw
                    if ((not Assigned(OnGetImage)) or (not OnGetImage(Self, pImageData,
                            imageType, pGraphic)))
                    then
                        continue;

                    // if no width defined, use the image width instead
                    if (width = 0) then
                        width := pGraphic.Width;

                    // if no height defined, use the image height instead
                    if (height = 0) then
                        height := pGraphic.Height;

                    // should apply an aspect ratio onto the image?
                    if (Assigned(pAspectRatioToUse)) then
                    begin
                        pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                        // do clip the image?
                        if (pProps.AspectRatio.Reference.Value = TWSVGPropAspectRatio.IEReference.IE_R_Slice) then
                        begin
                            // first reset the transform matrix. This is required because the aspect
                            // ratio viewbox is already scaled and should not be influenced by any
                            // previously defined transform matrix
                            pGraphics.ResetTransform();

                            // save the current region
                            pGraphics.GetClip(pPrevAspectRatioRegion);

                            // create a new clip region around the aspect ratio viewbox
                            pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pAspectRatioToUse.ViewBox.ToGpRectF()));

                            // set the clipping region, replace the existing one if any
                            if (pPrevAspectRatioRegion.IsInfinite(pGraphics)) then
                                pGraphics.SetClip(pRegion)
                            else
                                pGraphics.SetClip(pRegion, CombineModeReplace);

                            // notify that the image was clipped
                            isAspectRatioClipped := True;
                        end;

                        // set the image rect
                        imageRect        := Default(TWRectF);
                        imageRect.Right  := pGraphic.Width;
                        imageRect.Bottom := pGraphic.Height;

                        // set the target rect in which the image will be drawn
                        rect := pAspectRatioToUse.BoundingBox;
                    end
                    else
                    begin
                        // calculate position at which svg element should be drawn
                        svgPos := CalculateFinalPos(posFromProps, viewBox, scaleW, scaleH);

                        ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);

                        // set the image rect
                        imageRect        := Default(TWRectF);
                        imageRect.Right  := width;
                        imageRect.Bottom := height;

                        // set the draw rect
                        rect.Left   := 0;
                        rect.Top    := 0;
                        rect.Right  := width;
                        rect.Bottom := height;
                    end;

                    // set the image options
                    pImageOptions             := TWRenderer.IImageOptions.Create;
                    pImageOptions.ResizeMode  := E_RzMode_BicubicHQ;
                    pImageOptions.Opacity     := 1.0;
                    pImageOptions.Transparent := (imageType = IE_IT_PNG) or (imageType = IE_IT_SVG);
                    pImageOptions.Vectorial   := (imageType = IE_IT_SVG);

                    // draw the image
                    pRenderer.DrawImage(pGraphic, imageRect, pGraphics, rect, pImageOptions);
                finally
                    if (Assigned(pImageOptions)) then
                        pImageOptions.Free;

                    if (Assigned(pGraphic)) then
                        pGraphic.Free;

                    // restore the previous cliping before aspect ratio, if any
                    if (isAspectRatioClipped) then
                        pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                    // restore the previous clipping, if any
                    if (isClipped) then
                        pGraphics.SetClip(pPrevRegion, CombineModeReplace);
                end;

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;

        // is a text?
        if (pElement is TWSVGText) then
        begin
            // get text element
            pText := pElement as TWSVGText;

            // found it?
            if (Assigned(pText)) then
            begin
                // no text to draw?
                if (Length(pText.Text) = 0) then
                    continue;

                pPrevRegion := TWSmartPointer<TGpRegion>.Create();

                isClipped := ApplyClipPath(pHeader, viewBox, pParentProps, pElements, pos, scaleW,
                        scaleH, antialiasing, switchMode, clippingMode, useMode, animation, pAspectRatio,
                        pCanvas, pGraphics, pText, pPrevRegion);

                // configure animation
                pAnimationData          := TWSmartPointer<IAnimationData>.Create();
                pAnimationData.Position := animation.m_Position;

                // get all animations linked to this shape
                GetAnimations(pText, pAnimationData);

                pProps := TWSmartPointer<IProperties>.Create();

                // get draw properties from element
                if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
                    Exit(False);

                // the transform animations should absolutely be applied to the local matrix BEFORE
                // combining it with its parents
                GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

                pProps.Merge(pParentProps);

                // can display element? (NOTE for now the only supported mode is "none". All other modes are
                // considered as fully visible)
                if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
                    continue;

                // is element visible?
                if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
                    continue;

                // extract properties from text
                if (not GetTextProps(pText, x, y, fontFamily, fontSize, fontWeight, bolder, lighter,
                        fontStyle, fontStyleAngle, anchor, decoration, pAnimationData, animation.m_pCustomData))
                then
                    Exit(False);

                gdiFontStyle := [];

                // apply the font style svg property
                case fontStyle of
                    IE_FS_Italic:  gdiFontStyle := gdiFontStyle + [fsItalic];
                    IE_FS_Oblique: gdiFontStyle := gdiFontStyle + [fsItalic];
                end;

                // apply the text decoration svg property
                case decoration of
                    IE_TD_Underline:   gdiFontStyle := gdiFontStyle + [fsUnderline];
                    IE_TD_LineThrough: gdiFontStyle := gdiFontStyle + [fsStrikeOut];
                end;

                // apply the font weight (NOTE the font weight isn't well supported in GDI/GDI+,
                // instead the text becomes bold if weight is equal or higher to 600, and nothing else)
                if (fontWeight >= 600) then
                    gdiFontStyle := gdiFontStyle + [fsBold];

                // configure the font to use. NOTE be careful, the SVG font size matches with the
                // GDI font HEIGHT property, and not with the font SIZE
                pTextFont        :=  TWSmartPointer<TFont>.Create();
                pTextFont.Name   :=  fontFamily;
                pTextFont.Height := -Round(fontSize);
                pTextFont.Style  :=  gdiFontStyle;
                pFont            :=  TWSmartPointer<TGpFont>.Create(TGpFont.Create(pCanvas.Handle,
                        pTextFont.Handle));

                // char range is required to extract the first char bounds
                charRange.First  := 0;
                charRange.Length := 1;

                // configure the text format
                pTextFormat := TWSmartPointer<TGpStringFormat>.Create(TGpStringFormat.Create());
                pTextFormat.SetLineAlignment(StringAlignmentNear);
                pTextFormat.SetFormatFlags(StringFormatFlagsNoWrap);
                pTextFormat.SetTrimming(StringTrimmingNone);
                pTextFormat.SetMeasurableCharacterRanges(1, @charRange);

                // apply the text anchoring
                case (anchor) of
                    IE_TA_Start:  pTextFormat.SetAlignment(StringAlignmentNear);
                    IE_TA_Middle: pTextFormat.SetAlignment(StringAlignmentCenter);
                    IE_TA_End:    pTextFormat.SetAlignment(StringAlignmentFar);
                else
                    raise Exception.CreateFmt('Unknown text anchor value - %d', [Integer(anchor)]);
                end;

                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                pProps.Matrix.Value.ToGpMatrix(pMatrix);

                isAspectRatioClipped := False;

                // should apply an aspect ratio onto the lines?
                if (Assigned(pAspectRatio)) then
                begin
                    // measure the rect surrounding the text (before transformation)
                    pGraphics.MeasureString(pText.Text, Length(pText.Text), pFont, viewBox,
                            pTextFormat, boundingBox);

                    pPrevAspectRatioRegion := TWSmartPointer<TGpRegion>.Create();

                    // apply aspect ratio and get the previous clipping region, if any
                    isAspectRatioClipped := ApplyAspectRatio(pAspectRatio, pProps, boundingBox,
                            pMatrix, pGraphics, pPrevAspectRatioRegion);
                end
                else
                begin
                    // calculate position at which svg element should be drawn
                    svgPos := CalculateFinalPos(pos, viewBox, scaleW, scaleH);

                    ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);
                end;

                // measure the rect surrounding the text
                if (pGraphics.MeasureString(pText.Text, Length(pText.Text), pFont, viewBox,
                        pTextFormat, boundingBox) <> Ok)
                then
                begin
                    // todo FIXME -cFeature -oJean: find a better way for the font fallback
                    fontFamilyLowerCase := LowerCase(fontFamily);

                    // if the font family is a known unsupported font, map it to a supported one
                    if (fontFamilyLowerCase = 'courier') then
                    begin
                        pTextFont.Name := 'Courier New';
                        pFont          :=  TWSmartPointer<TGpFont>.Create(TGpFont.Create(pCanvas.Handle,
                        pTextFont.Handle));
                    end
                    else
                    begin
                        TWLogHelper.LogToCompiler('Draw text - FAILED - font unsupported by GDI+ - name - '
                                + fontFamily);
                        continue;
                    end;

                    // and try again
                    if (pGraphics.MeasureString(pText.Text, Length(pText.Text), pFont, viewBox,
                            pTextFormat, boundingBox) <> Ok)
                    then
                    begin
                        TWLogHelper.LogToCompiler('Draw text - FAILED - font unsupported by GDI+ - name - '
                                + fontFamily);
                        continue;
                    end;
                end;

                pCanvas.Font.Assign(pTextFont);

                // get text metrics. They will contain the required info about text height
                if (not GetTextMetrics(pCanvas.Handle, textMetrics)) then
                    continue;

                // apply the text anchoring
                case (anchor) of
                    IE_TA_Start:
                    begin
                        try
                            // create and populate a region list to get the first char region
                            SetLength(charRegions, 1);
                            charRegions[0] := TGpRegion.Create;

                            // measure the char ranges. In this case only the first char will be measured
                            if (pGraphics.MeasureCharacterRanges(pText.Text, 1, pFont, boundingBox,
                                    pTextFormat, 1, charRegions) <> Ok)
                            then
                                continue;

                            // get first char bounding box
                            charRegions[0].GetBounds(charRect, pGraphics);
                        finally
                            for i := 0 to Length(charRegions) - 1 do
                                charRegions[i].Free;

                            SetLength(charRegions, 0);
                        end;

                        // calculate the real text position. This is required, because the GDI+ will
                        // draw the text from the left top corner of the bounding box, and will add
                        // an extra pad space before the first letter, whereas the SVG coordinates
                        // represent the text from its baseline, and starting immediately on the
                        // first letter, without padding
                        textPos.X := x - charRect.X;
                        textPos.Y := y - textMetrics.tmAscent;

                        // move the bounding box to correct location
                        boundingBox.X := textPos.X;
                        boundingBox.Y := textPos.Y;
                    end;

                    IE_TA_Middle:
                    begin
                        textPos.X := x - (boundingBox.Width / 2.0);
                        textPos.Y := y - textMetrics.tmAscent;

                        // move the bounding box to correct location
                        boundingBox.X := textPos.X;
                        boundingBox.Y := textPos.Y;
                    end;

                    IE_TA_End:
                    begin
                        try
                            // create and populate a region list to get the first char region
                            SetLength(charRegions, 1);
                            charRegions[0] := TGpRegion.Create;

                            // measure the char ranges. In this case only the first char will be measured
                            if (pGraphics.MeasureCharacterRanges(pText.Text, 1, pFont, boundingBox,
                                    pTextFormat, 1, charRegions) <> Ok)
                            then
                                continue;

                            // get first char bounding box
                            charRegions[0].GetBounds(charRect, pGraphics);
                        finally
                            for i := 0 to Length(charRegions) - 1 do
                                charRegions[i].Free;

                            SetLength(charRegions, 0);
                        end;

                        // calculate the real text position. This is required, because the GDI+ will
                        // draw the text from the left top corner of the bounding box, and will add
                        // an extra pad space before the first letter, whereas the SVG coordinates
                        // represent the text from its baseline, and starting immediately on the
                        // first letter, without padding
                        textPos.X := x - charRect.X;
                        textPos.Y := y - textMetrics.tmAscent;

                        // move the bounding box to correct location
                        boundingBox.X := textPos.X;
                        boundingBox.Y := textPos.Y;
                    end;
                else
                    raise Exception.CreateFmt('Unknown text anchor value - %d', [Integer(anchor)]);
                end;

                if (antialiasing) then
                    pGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);

                // do apply a clipping path?
                if (clippingMode) then
                begin
                    pFontFamily := TWSmartPointer<TGPFontFamily>.Create();

                    // get the font family
                    pFont.GetFamily(pFontFamily);

                    // create a path containing the text to clip. NOTE the size must be converted from
                    // point size to "em" size
                    pGraphicsPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                    if (intersection) then
                        pGraphicsPath.SetFillMode(FillModeWinding);

                    pGraphicsPath.AddString(pText.Text, Length(pText.Text), pFontFamily, pFont.GetStyle,
                            fontSize, textPos, pTextFormat);

                    // get the current region
                    pCurRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create);
                    pGraphics.GetClip(pCurRegion);

                    pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pGraphicsPath));

                    // set the clipping region, unify to the existing one if any
                    if (pCurRegion.IsInfinite(pGraphics)) then
                        pGraphics.SetClip(pRegion)
                    else
                    if (intersection) then
                        pGraphics.SetClip(pRegion, CombineModeIntersect)
                    else
                        pGraphics.SetClip(pRegion, CombineModeUnion);
                end
                else
                begin
                    pStroke := TWSmartPointer<TWStroke>.Create();

                    // outline the text (NOTE instead of shapes above, this should be done before drawing
                    // the text)
                    if (GetPen(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pStroke)) then
                    begin
                        // todo FIXME -cFeature -oJean: add an OutlineText function in GDI+ renderer
                        pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();

                        // get outline pen
                        pGpPen := pRenderer.GetPen(pStroke, pGradientFactory);

                        // is cache used? If not keep pointer in a smart pointer to auto-delete object
                        // on function ends
                        if (not g_GDIPlusCacheController.m_Pens) then
                            paPen := TWSmartPointer<TGpPen>.Create(pGpPen);

                        pFontFamily := TWSmartPointer<TGPFontFamily>.Create();

                        // get the font family
                        pFont.GetFamily(pFontFamily);

                        // create new GDI+ path. NOTE create explicitly the graphics path before keep it
                        // inside the smart pointer, because otherwise the incorrect constructor is
                        // called while the smart pointer tries to auto-create the object, causing thus
                        // that the path is never drawn
                        pTextPath := TWSmartPointer<TGpGraphicsPath>.Create(TGpGraphicsPath.Create);

                        // create a path containing the text to outline. NOTE the size must be converted
                        // from point size to "em" size
                        pTextPath.AddString(pText.Text, Length(pText.Text), pFontFamily, pFont.GetStyle,
                                fontSize, textPos, pTextFormat);

                        // outline the text
                        pGraphics.DrawPath(pGpPen, pTextPath);
                    end;

                    pFill := TWSmartPointer<TWFill>.Create();

                    // draw the text
                    if (GetBrush(pProps.Style, viewBox, boundingBox, scaleW, scaleH, pRenderer, pFill)) then
                        pRenderer.DrawString(pText.Text, textPos, pFont, pFill, pGraphics,
                                TWRectF.Create(boundingBox, True));
                end;

                // restore the previous cliping before aspect ratio, if any
                if (isAspectRatioClipped) then
                    pGraphics.SetClip(pPrevAspectRatioRegion, CombineModeReplace);

                // restore the previous clipping, if any
                if (isClipped) then
                    pGraphics.SetClip(pPrevRegion, CombineModeReplace);

                // is switch mode enabled?
                if (switchMode) then
                    Exit(True);

                continue;
            end;
        end;
    end;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.ApplyClipPath(const pHeader: TWSVGParser.IHeader; const viewBox: TGpRectF;
        const pParentProps: TWSVGGDIPlusRasterizer.IProperties; const pElements: TWSVGContainer.IElements;
        const pos: TPoint; scaleW, scaleH: Single; antialiasing, switchMode, clippingMode, useMode: Boolean;
        const animation: TWSVGRasterizer.IAnimation; pAspectRatio: IAspectRatio; pCanvas: TCanvas;
        pGraphics: TGpGraphics; pElement: TWSVGElement; prevRegion: TGpRegion): Boolean;
var
    pClipPath:             TWSVGClipPath;
    pProps:                IWSmartPointer<IProperties>;
    pAnimationData:        IWSmartPointer<IAnimationData>;
    clipPathPos:           TPoint;
    x, y, width, height:   Single;
    elementViewBox:        TWRectF;
    pIntersectionClipPath: TWSVGClipPath;
    intersect, clipResult: Boolean;
begin
    pClipPath := nil;

    if (not GetClipPath(pElement, pClipPath)) then
        Exit(False);

    pIntersectionClipPath := nil;
    intersect             := False;
    clipResult            := True;

    // is an intersection clip path defined?
    if (GetClipPath(pClipPath, pIntersectionClipPath)) then
    begin
        // configure animation
        pAnimationData          := TWSmartPointer<IAnimationData>.Create();
        pAnimationData.Position := animation.m_Position;

        // get all animations linked to this container
        GetAnimations(pIntersectionClipPath, pAnimationData);

        pProps := TWSmartPointer<IProperties>.Create();

        // get draw properties from element
        if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
            Exit(False);

        // the transform animations should absolutely be applied to the local matrix BEFORE
        // combining it with its parents
        GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

        pProps.Merge(pParentProps);

        // can display element? (NOTE for now the only supported mode is "none". All other modes are
        // considered as fully visible)
        if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
            Exit(False);

        // is element visible?
        if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
            Exit(False);

        // extract position and size properties
        if (not GetPosAndSizeProps(pIntersectionClipPath, x, y, width, height, elementViewBox,
                pAnimationData, animation.m_pCustomData))
        then
            Exit(False);

        // get the clip path position (in relation to the initial position)
        clipPathPos := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

        // save the current clip
        pGraphics.GetClip(prevRegion);

        // apply clipping
        clipResult := DrawElements(pHeader, viewBox, pProps, pIntersectionClipPath.ElementList,
                clipPathPos, scaleW, scaleH, antialiasing, False, True, useMode, True, animation,
                pAspectRatio, pCanvas, pGraphics);

        intersect := clipResult;
    end;

    // configure animation
    pAnimationData          := TWSmartPointer<IAnimationData>.Create();
    pAnimationData.Position := animation.m_Position;

    // get all animations linked to this container
    GetAnimations(pClipPath, pAnimationData);

    pProps := TWSmartPointer<IProperties>.Create();

    // get draw properties from element
    if (not GetElementProps(pElement, pProps, pAnimationData, animation.m_pCustomData)) then
        Exit(False);

    // the transform animations should absolutely be applied to the local matrix BEFORE
    // combining it with its parents
    GetTransformAnimMatrix(pAnimationData, pProps.Matrix, animation.m_pCustomData);

    pProps.Merge(pParentProps);

    // can display element? (NOTE for now the only supported mode is "none". All other modes are
    // considered as fully visible)
    if (pProps.Style.DisplayMode.Value = TWSVGStyle.IPropDisplay.IEValue.IE_V_None) then
        Exit(False);

    // is element visible?
    if (pProps.Style.Visibility.Value <> TWSVGStyle.IPropVisibility.IEValue.IE_V_Visible) then
        Exit(False);

    // extract position and size properties
    if (not GetPosAndSizeProps(pClipPath, x, y, width, height, elementViewBox, pAnimationData,
            animation.m_pCustomData))
    then
        Exit(False);

    // get the clip path position (in relation to the initial position)
    clipPathPos := TPoint.Create(Round(pos.X + (x * scaleW)), Round(pos.Y + (y * scaleH)));

    // save the current clip
    if (not intersect) then
        pGraphics.GetClip(prevRegion);

    // apply clipping
    Result := DrawElements(pHeader, viewBox, pProps, pClipPath.ElementList, clipPathPos, scaleW,
            scaleH, antialiasing, False, True, useMode, intersect, animation, pAspectRatio, pCanvas,
            pGraphics) and clipResult;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.PopulateAspectRatio(pos: TPoint; width, height, scaleW, scaleH: Single;
        elementViewBox: TWRectF; pProps: TWSVGGDIPlusRasterizer.IProperties; pAspectRatio: IAspectRatio): Boolean;
var
    aspectRatioViewBox, aspectRatioBoundingBox: TWRectF;
    aspectBBoxWidth, aspectBBoxHeight:          Single;
begin
    // calculate the aspect ratio viewbox considering the scaling
    aspectRatioViewBox.Left   := pos.X;
    aspectRatioViewBox.Top    := pos.Y;
    aspectRatioViewBox.Right  := pos.X + (width  * scaleW);
    aspectRatioViewBox.Bottom := pos.Y + (height * scaleH);

    // calculate the aspect ratio bounding box width and height
    case pProps.AspectRatio.Reference.Value of
        TWSVGPropAspectRatio.IEReference.IE_R_Slice:
        begin
            // sliced, search for bounding box orientation
            if (elementViewBox.Width < elementViewBox.Height) then
            begin
                // portrait
                aspectBBoxWidth  := aspectRatioViewBox.Width;
                aspectBBoxHeight := elementViewBox.Height * (aspectRatioViewBox.Width / elementViewBox.Width);
            end
            else
            if (elementViewBox.Width > elementViewBox.Height) then
            begin
                // landscape
                aspectBBoxWidth  := elementViewBox.Width * (aspectRatioViewBox.Height / elementViewBox.Height);
                aspectBBoxHeight := aspectRatioViewBox.Height;
            end
            else
            begin
                // squared, in this case use viewbox for calculation
                if (aspectRatioViewBox.Width <= aspectRatioViewBox.Height) then
                begin
                    // portrait or squared
                    aspectBBoxWidth  := elementViewBox.Width * (aspectRatioViewBox.Height / elementViewBox.Height);
                    aspectBBoxHeight := aspectRatioViewBox.Height;
                end
                else
                begin
                    // landscape
                    aspectBBoxWidth  := aspectRatioViewBox.Width;
                    aspectBBoxHeight := elementViewBox.Height * (aspectRatioViewBox.Width / elementViewBox.Width);
                end
            end;
        end;
    else
        // meet or default, search for bounding box orientation
        if (elementViewBox.Width < elementViewBox.Height) then
        begin
            // portrait
            aspectBBoxWidth  := elementViewBox.Width * (aspectRatioViewBox.Height / elementViewBox.Height);
            aspectBBoxHeight := aspectRatioViewBox.Height;
        end
        else
        if (elementViewBox.Width > elementViewBox.Height) then
        begin
            // landscape
            aspectBBoxWidth  := aspectRatioViewBox.Width;
            aspectBBoxHeight := elementViewBox.Height * (aspectRatioViewBox.Width / elementViewBox.Width);
        end
        else
        begin
            // squared, in this case use viewbox for calculation
            if (aspectRatioViewBox.Width <= aspectRatioViewBox.Height) then
            begin
                // portrait or squared
                aspectBBoxWidth  := aspectRatioViewBox.Width;
                aspectBBoxHeight := elementViewBox.Height * (aspectRatioViewBox.Width / elementViewBox.Width);
            end
            else
            begin
                // landscape
                aspectBBoxWidth  := elementViewBox.Width * (aspectRatioViewBox.Height / elementViewBox.Height);
                aspectBBoxHeight := aspectRatioViewBox.Height;
            end
        end;
    end;

    // calculate the aspect ratio bounding box position relative to its viewbox
    case pProps.AspectRatio.AspectRatio.Value of
        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_None:
        begin
            // no aspect ratio, the content is just stretched in the viewbox
            aspectRatioBoundingBox.Left   := pos.X;
            aspectRatioBoundingBox.Top    := pos.Y;
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + (width  * scaleW);
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + (height * scaleH);
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMinYMin:
        begin
            // left and top
            aspectRatioBoundingBox.Left   := pos.X;
            aspectRatioBoundingBox.Top    := pos.Y;
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMin:
        begin
            // centered and top
            aspectRatioBoundingBox.Left   := pos.X + ((aspectRatioViewBox.Width - aspectBBoxWidth) / 2);
            aspectRatioBoundingBox.Top    := pos.Y;
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMaxYMin:
        begin
            // right and top
            aspectRatioBoundingBox.Left   := pos.X + (aspectRatioViewBox.Width - aspectBBoxWidth);
            aspectRatioBoundingBox.Top    := pos.Y;
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMinYMid:
        begin
            // left and centered
            aspectRatioBoundingBox.Left   := pos.X;
            aspectRatioBoundingBox.Top    := pos.Y + ((aspectRatioViewBox.Height - aspectBBoxHeight) / 2);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMid:
        begin
            // centered
            aspectRatioBoundingBox.Left   := pos.X + ((aspectRatioViewBox.Width  - aspectBBoxWidth)  / 2);
            aspectRatioBoundingBox.Top    := pos.Y + ((aspectRatioViewBox.Height - aspectBBoxHeight) / 2);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMaxYMid:
        begin
            // right and centered
            aspectRatioBoundingBox.Left   := pos.X + ( aspectRatioViewBox.Width  - aspectBBoxWidth);
            aspectRatioBoundingBox.Top    := pos.Y + ((aspectRatioViewBox.Height - aspectBBoxHeight) / 2);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMinYMax:
        begin
            // left and bottom
            aspectRatioBoundingBox.Left   := pos.X;
            aspectRatioBoundingBox.Top    := pos.Y + (aspectRatioViewBox.Height - aspectBBoxHeight);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMidYMax:
        begin
            // centered and bottom
            aspectRatioBoundingBox.Left   := pos.X + ((aspectRatioViewBox.Width  - aspectBBoxWidth) / 2);
            aspectRatioBoundingBox.Top    := pos.Y + ( aspectRatioViewBox.Height - aspectBBoxHeight);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;

        TWSVGPropAspectRatio.IEAspectRatio.IE_AR_XMaxYMax:
        begin
            // right and bottom
            aspectRatioBoundingBox.Left   := pos.X + (aspectRatioViewBox.Width  - aspectBBoxWidth);
            aspectRatioBoundingBox.Top    := pos.Y + (aspectRatioViewBox.Height - aspectBBoxHeight);
            aspectRatioBoundingBox.Right  := aspectRatioBoundingBox.Left + aspectBBoxWidth;
            aspectRatioBoundingBox.Bottom := aspectRatioBoundingBox.Top  + aspectBBoxHeight;
        end;
    else
        Exit(False);
    end;

    // set aspect ratio view and bounding box
    pAspectRatio.ElementBox  := elementViewBox;
    pAspectRatio.ViewBox     := aspectRatioViewBox;
    pAspectRatio.BoundingBox := aspectRatioBoundingBox;

    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.GetClosestSquare(const rect: TRect): TRect;
begin
    // is height higher than width?
    if (rect.Height > rect.Width) then
    begin
        // calculate the closest square on the vertical axis
        Result.Left   := 0;
        Result.Top    := ((rect.Height - rect.Width) div 2);
        Result.Right  := Result.Left + rect.Width;
        Result.Bottom := Result.Top  + rect.Width;
        Exit;
    end;

    // calculate the closest square on the horizontal axis
    Result.Left   := ((rect.Width - rect.Height) div 2);
    Result.Top    := 0;
    Result.Right  := Result.Left + rect.Height;
    Result.Bottom := Result.Top  + rect.Height;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.GetFillMode(const pGlobalProps, pProps: TWSVGGDIPlusRasterizer.IProperties): TFillMode;
var
    fillRule: TWSVGFill.IERule;
begin
    if (pProps.Style.Fill.FillRule.Value = TWSVGFill.IERule.IE_FR_Default) then
        fillRule := pGlobalProps.Style.Fill.FillRule.Value
    else
        fillRule := pProps.Style.Fill.FillRule.Value;

    // set fill mode
    case (fillRule) of
        TWSVGFill.IERule.IE_FR_EvenOdd: Exit(FillModeAlternate);
    else
        Exit(FillModeWinding);
    end;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.GetBrush(const pStyle: TWSVGGDIPlusRasterizer.IStyle; const viewBox,
        boundingBox: TGpRectF; scaleW, scaleH: Single; pRenderer: TWRenderer_GDIPlus;
        pFill: TWFill): Boolean;
var
    pSolid:    IWSmartPointer<TWSolidBrush>;
    pLinear:   IWSmartPointer<TWLinearGradientBrush>;
    pRadial:   IWSmartPointer<TWRadialGradientBrush>;
    stopCount: NativeUInt;
    color:     TWColor;
begin
    // no fill?
    if (pStyle.Fill.NoFill.Value) then
    begin
        color.SetColor(0, 0, 0, 0);

        // configure a fully transparent brush
        pSolid       := TWSmartPointer<TWSolidBrush>.Create();
        pSolid.Color := @color;
        pFill.SetBrush(pSolid);

        Exit(False);
    end;

    // get the brush type to use
    case (pStyle.Fill.Brush.BrushType) of
        E_BT_Solid:
        begin
            pSolid       := TWSmartPointer<TWSolidBrush>.Create();
            color        := pStyle.GetFillColor;
            pSolid.Color := @color;

            pFill.SetBrush(pSolid);

            Exit(not pSolid.Color.IsEmpty);
        end;

        E_BT_Linear:
        begin
            pLinear := TWSmartPointer<TWLinearGradientBrush>.Create();

            // get fill gradient stop count
            stopCount := pStyle.Fill.Brush.LinearGradient.GradientStops.Count;

            GetLinearGradientBrush(pStyle.Fill.Brush.LinearGradient, viewBox, boundingBox,
                    stopCount, pStyle.Opacity.Value, pLinear);

            pFill.SetBrush(pLinear);

            Exit(pRenderer.IsGradientVisible(pFill));
        end;

        E_BT_Radial:
        begin
            pRadial := TWSmartPointer<TWRadialGradientBrush>.Create();

            // get fill gradient stop count
            stopCount := pStyle.Fill.Brush.RadialGradient.GradientStops.Count;

            GetRadialGradientBrush(pStyle.Fill.Brush.RadialGradient, viewBox, boundingBox, scaleW,
                    scaleH, stopCount, pRadial);

            pFill.SetBrush(pRadial);

            Exit(pRenderer.IsGradientVisible(pFill));
        end;
    else
        raise Exception.CreateFmt('Unknown fill brush type - %d', [Integer(pStyle.Fill.Brush.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGDIPlusRasterizer.GetLinearGradientBrush(const pGradient: TWSVGGDIPlusRasterizer.ILinearGradient;
        const viewBox, boundingBox: TGpRectF; stopCount: NativeUInt; opacity: Single;
        pBrush: TWLinearGradientBrush);
var
    vectorLimits:         TWRectF;
    vector:               ILinearGradientVector;
    gradientStopCount, i: NativeUInt;
    pStop:                TWGradientStop;
begin
    // get the bounding rect surrounding the gradient
    vectorLimits := TWRectF.Create(boundingBox, False);

    // apply the unit mode
    case (pGradient.GradientUnit) of
        TWSVGPropUnit.IEType.IE_UT_UserSpaceOnUse: pBrush.GradientUnit := E_GU_UserSpaceOnUse;
    else
        pBrush.GradientUnit := E_GU_ObjectBoundingBox;

        // increase the rect 1 pixel higher otherwise artifacts may appear on the filled
        // shape edges
        vectorLimits.Inflate(1.0, 1.0);
    end;

    // set the gradient vector
    pBrush.Vector.GradientStart := TWVector2.Create(vectorLimits.Left,  vectorLimits.Top);
    pBrush.Vector.GradientEnd   := TWVector2.Create(vectorLimits.Right, vectorLimits.Bottom);

    // apply the spread method (it's the manner how the gradient behaves out of its bounds limits)
    case (pGradient.SpreadMethod) of
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad:     pBrush.WrapMode := E_WM_Clamp;
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Reflect: pBrush.WrapMode := E_WM_TileFlipXY;
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Repeat:  pBrush.WrapMode := E_WM_Tile;
    else
        pBrush.WrapMode := E_WM_Clamp;
    end;

    // is gradient matrix defined?
    if (pGradient.MatrixType <> TWSVGPropMatrix.IEType.IE_Unknown) then
        pBrush.SetMatrix(pGradient.Matrix);

    // calculate the linear gradient vector
    CalculateLinearGradientVector(pGradient, viewBox, boundingBox, vector);

    gradientStopCount := pGradient.GradientStops.Count;

    // iterate through gradient stops to create
    if (gradientStopCount > 0) then
        for i := 0 to gradientStopCount - 1 do
        begin
            pStop := nil;

            try
                pStop := TWGradientStop.Create;
                pStop.SetColor(pGradient.GradientStops[i].Color);

                // apply the global opacity to the gradient, if required
                if (opacity < 1.0) then
                    pStop.Color.SetOpacity(Round(opacity * pStop.Color.Opacity));

                // start, end or stop color?
                if (i = 0) then
                begin
                    pStop.Position.X := vector.m_Start.X;
                    pStop.Position.Y := vector.m_Start.Y;
                end
                else
                if (i = gradientStopCount - 1) then
                begin
                    pStop.Position.X := vector.m_End.X;
                    pStop.Position.Y := vector.m_End.Y;
                end
                else
                    pStop.Percent := pGradient.GradientStops[i].Offset;

                pBrush.Stops.Add(pStop);
                pStop := nil;
            finally
                pStop.Free;
            end;
        end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGDIPlusRasterizer.GetRadialGradientBrush(const pGradient: TWSVGGDIPlusRasterizer.IRadialGradient;
        const viewBox, boundingBox: TGpRectF; scaleW, scaleH: Single;
        stopCount: NativeUInt; pBrush: TWRadialGradientBrush);
var
    radius, scaleFactor:  TWSizeF;
    center, focus:        TWPointF;
    boundingRect:         TWRectF;
    gradientStopCount, i: NativeUInt;
    pStop:                TWGradientStop;
begin
    // apply the unit mode
    case (pGradient.GradientUnit) of
        TWSVGPropUnit.IEType.IE_UT_UserSpaceOnUse:
        begin
            radius := TWSizeF.Create(pGradient.R * 2.0, pGradient.R * 2.0);
            center := TWPointF.Create(pGradient.CX, pGradient.CY);
            focus  := TWPointF.Create(pGradient.FX, pGradient.FY);

            pBrush.Radius       := radius;
            pBrush.Center       := center;
            pBrush.Focus        := focus;
            pBrush.GradientUnit := E_GU_UserSpaceOnUse;
        end;
    else
        // calculate the gradient bounding rect
        boundingRect := TWRectF.Create(boundingBox.X - 1.0, boundingBox.Y - 1.0,
                (boundingBox.X + boundingBox.Width)  + 2.0,
                (boundingBox.Y + boundingBox.Height) + 2.0);

        radius := TWSizeF.Create(boundingRect.Width * pGradient.R * 2.0,
                boundingRect.Height * pGradient.R * 2.0);
        center := TWPointF.Create(boundingRect.Left + (pGradient.CX * boundingRect.Width),
                boundingRect.Top + (pGradient.CY * boundingRect.Height));
        focus  := TWPointF.Create(boundingRect.Left + (pGradient.FX * boundingRect.Width),
                boundingRect.Top + (pGradient.FY * boundingRect.Height));

        pBrush.Radius       := radius;
        pBrush.Center       := center;
        pBrush.Focus        := focus;
        pBrush.GradientUnit := E_GU_ObjectBoundingBox;
    end;

    // apply the spread method (it's the manner how the gradient behaves out of its bounds limits)
    case (pGradient.SpreadMethod) of
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Pad:     pBrush.WrapMode := E_WM_Clamp;
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Reflect: pBrush.WrapMode := E_WM_TileFlipXY;
        TWSVGGradient.IEGradientSpreadMethod.IE_GS_Repeat:  pBrush.WrapMode := E_WM_Tile;
    else
        pBrush.WrapMode := E_WM_Clamp;
    end;

    // is gradient matrix defined?
    if (pGradient.MatrixType <> TWSVGPropMatrix.IEType.IE_Unknown) then
        pBrush.SetMatrix(pGradient.Matrix);

    scaleFactor := TWSizeF.Create(scaleW, scaleH);

    // also keep the scaling (because radial gradient is based on a real geometrical shape)
    pBrush.ScaleFactor := scaleFactor;

    gradientStopCount := pGradient.GradientStops.Count;

    // iterate through gradient stops to create
    if (gradientStopCount > 0) then
        for i := 0 to gradientStopCount - 1 do
        begin
            pStop := nil;

            try
                pStop := TWGradientStop.Create;

                // create and configure gradient stop
                pStop.Color.Assign(pGradient.GradientStops[i].Color^);
                pStop.Percent := pGradient.GradientStops[i].Offset;
                pBrush.Stops.Add(pStop);
                pStop := nil;
            finally
                pStop.Free;
            end;
        end;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.GetPen(const pStyle: TWSVGGDIPlusRasterizer.IStyle; const viewBox,
        boundingBox: TGpRectF; scaleW, scaleH: Single; pRenderer: TWRenderer_GDIPlus;
        pStroke: TWStroke): Boolean;
var
    pSolid:    IWSmartPointer<TWSolidBrush>;
    pLinear:   IWSmartPointer<TWLinearGradientBrush>;
    pRadial:   IWSmartPointer<TWRadialGradientBrush>;
    color:     TWColor;
    stopCount: NativeUInt;
    item:      Single;
begin
    // set stroke empty by default (in case it should not be used)
    pStroke.Width := 0;

    // no stroke?
    if (pStyle.Stroke.NoStroke.Value) then
        Exit(False);

    // stroke has no width?
    if (pStyle.Stroke.Width.Value = 0.0) then
        Exit(False);

    // get the brush to use to build the pen
    case (pStyle.Stroke.Brush.BrushType) of
        E_BT_Solid:
        begin
            pSolid       := TWSmartPointer<TWSolidBrush>.Create();
            color        := pStyle.GetStrokeColor;
            pSolid.Color := @color;

            // check if stroke color isn't fully transparent
            if (pSolid.Color.IsEmpty) then
                Exit(False);

            pStroke.SetBrush(pSolid);
        end;

        E_BT_Linear:
        begin
            pLinear := TWSmartPointer<TWLinearGradientBrush>.Create();

            // get base brush gradient stop count
            stopCount := pStyle.Stroke.Brush.LinearGradient.GradientStops.Count;

            GetLinearGradientBrush(pStyle.Stroke.Brush.LinearGradient, viewBox, boundingBox,
                    stopCount, pStyle.Opacity.Value, pLinear);

            pStroke.SetBrush(pLinear);

            // check if stroke gradient isn't fully transparent
            if (not pRenderer.IsGradientVisible(pStroke)) then
                Exit(False);
        end;

        E_BT_Radial:
        begin
            pRadial := TWSmartPointer<TWRadialGradientBrush>.Create();

            // get base brush gradient stop count
            stopCount := pStyle.Stroke.Brush.RadialGradient.GradientStops.Count;

            GetRadialGradientBrush(pStyle.Stroke.Brush.RadialGradient, viewBox, boundingBox,
                    scaleW, scaleH, stopCount, pRadial);

            pStroke.SetBrush(pRadial);

            // check if stroke gradient isn't fully transparent
            if (not pRenderer.IsGradientVisible(pStroke)) then
                Exit(False);
        end;
    else
        raise Exception.CreateFmt('Unknown stroke base brush type - %d', [Integer(pStyle.Stroke.Brush.BrushType)]);
    end;

    // set stroke width
    pStroke.Width := pStyle.Stroke.Width.Value;

    // configure the pen line and dash caps
    case (pStyle.Stroke.LineCap.Value) of
        TWSVGStroke.IELineCap.IE_LC_Butt:   begin; pStroke.LineCap := E_LC_Flat;   pStroke.DashCap := E_DC_Flat;  end;
        TWSVGStroke.IELineCap.IE_LC_Round:  begin; pStroke.LineCap := E_LC_Round;  pStroke.DashCap := E_DC_Round; end;
        TWSVGStroke.IELineCap.IE_LC_Square: begin; pStroke.LineCap := E_LC_Square; pStroke.DashCap := E_DC_Flat;  end;
    end;

    // configure the stroke line join
    case (pStyle.Stroke.LineJoin.Value) of
        TWSVGStroke.IELineJoin.IE_LJ_Round: pStroke.LineJoin := E_LJ_Round;
        TWSVGStroke.IELineJoin.IE_LJ_Bevel: pStroke.LineJoin := E_LJ_Bevel;
    else
        pStroke.LineJoin := E_LJ_Miter;
    end;

    // configure the dash pattern
    pStroke.DashOffset := pStyle.Stroke.DashOffset.Value;

    // iterate through pattern items
    for item in pStyle.Stroke.DashPattern.Value do
        // copy each dash pattern item in the stroke
        pStroke.DashPattern.Add(item);

    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGGDIPlusRasterizer.CalculateLinearGradientVector(const pGradient: TWSVGGDIPlusRasterizer.ILinearGradient;
        const viewBox, boundingBox: TGpRectF; out vector: TWSVGGDIPlusRasterizer.ILinearGradientVector);
var
    startVec, endVec: TWVector2;
    usePercent:       Boolean;
begin
    // are values expressed in percent?
    usePercent := (((pGradient.Vector.m_Start.X >= 0.0) and (pGradient.Vector.m_Start.X <= 1.0))
            and ((pGradient.Vector.m_Start.Y >= 0.0) and (pGradient.Vector.m_Start.Y <= 1.0))
            and ((pGradient.Vector.m_End.X   >= 0.0) and (pGradient.Vector.m_End.X   <= 1.0))
            and ((pGradient.Vector.m_End.Y   >= 0.0) and (pGradient.Vector.m_End.Y   <= 1.0)));

    // start and end values are expressed as coordinates and a gradient transform matrix is defined?
    if ((not usePercent) and (pGradient.MatrixType <> TWSVGPropMatrix.IEType.IE_Unknown)) then
    begin
        // apply the matrix to the gradient vector
        startVec := pGradient.Matrix.Transform(TWVector2.Create(pGradient.Vector.m_Start.X, pGradient.Vector.m_Start.Y));
        endVec   := pGradient.Matrix.Transform(TWVector2.Create(pGradient.Vector.m_End.X,   pGradient.Vector.m_End.Y));
    end
    else
    begin
        // use the gradient vector without transformation
        startVec := TWVector2.Create(pGradient.Vector.m_Start.X, pGradient.Vector.m_Start.Y);
        endVec   := TWVector2.Create(pGradient.Vector.m_End.X,   pGradient.Vector.m_End.Y);
    end;

    // search for calculation type to apply
    case (pGradient.GradientUnit) of
        // do calculate the gradient pos relatively to the viewport. The values contain coordinates
        // expressed in the viewport unit
        TWSVGPropUnit.IEType.IE_UT_UserSpaceOnUse:
        begin
            // are values expressed in percent?
            if (usePercent) then
            begin
                vector.m_Start.X := viewBox.X + (viewBox.Width  * startVec.X);
                vector.m_Start.Y := viewBox.Y + (viewBox.Height * startVec.Y);
                vector.m_End.X   := viewBox.X + (viewBox.Width  * endVec.X);
                vector.m_End.Y   := viewBox.Y + (viewBox.Height * endVec.Y);
            end
            else
            begin
                vector.m_Start.X := startVec.X;
                vector.m_Start.Y := startVec.Y;
                vector.m_End.X   := endVec.X;
                vector.m_End.Y   := endVec.Y;
            end;
        end;
    else
        // are values expressed in percent?
        if (usePercent) then
        begin
            vector.m_Start.X := boundingBox.X + (boundingBox.Width  * startVec.X);
            vector.m_Start.Y := boundingBox.Y + (boundingBox.Height * startVec.Y);
            vector.m_End.X   := boundingBox.X + (boundingBox.Width  * endVec.X);
            vector.m_End.Y   := boundingBox.Y + (boundingBox.Height * endVec.Y);
        end
        else
        begin
            vector.m_Start.X := boundingBox.X + startVec.X;
            vector.m_Start.Y := boundingBox.Y + startVec.Y;
            vector.m_End.X   := boundingBox.X + endVec.X;
            vector.m_End.Y   := boundingBox.Y + endVec.Y;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.CalculateFinalPos(const pos: TPoint; const viewBox: TGpRectF;
        scaleW, scaleH: Single): TPoint;
begin
    // apply viewbox correction
    Result.X := pos.X - Trunc(viewBox.X * scaleW);
    Result.Y := pos.Y - Trunc(viewBox.Y * scaleH);
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.ApplyAspectRatio(const pAspectRatio: IAspectRatio;
        const pProps: TWSVGGDIPlusRasterizer.IProperties; const boundingBox: TGpRectF;
        const pMatrix: TGpMatrix; pGraphics: TGpGraphics; prevRegion: TGpRegion): Boolean;
var
    pRegion: IWSmartPointer<TGpRegion>;
    svgPos:  TPoint;
begin
    // the svg position is the aspect ratio bounding box position (already scaled)
    svgPos.X := Round(pAspectRatio.BoundingBox.Left);
    svgPos.Y := Round(pAspectRatio.BoundingBox.Top);

    Result := False;

    // resize and clip the image to fit the aspect ratio bounding box / viewbox
    if (pProps.AspectRatio.AspectRatio.Value = TWSVGPropAspectRatio.IEAspectRatio.IE_AR_None) then
        // no alignment, fit the image to the aspect ratio bounding box
        ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Width / boundingBox.Width,
                pAspectRatio.BoundingBox.Height / boundingBox.Height, pGraphics)
    else
    if (pProps.AspectRatio.Reference.Value = TWSVGPropAspectRatio.IEReference.IE_R_Slice) then
    begin
        // slice image, first reset the transform matrix. This is required because the
        // aspect ratio viewbox is already scaled and should not be influenced by any
        // previously defined transform matrix
        pGraphics.ResetTransform();

        // save the current region
        pGraphics.GetClip(prevRegion);

        // create a new clip region around the aspect ratio viewbox
        pRegion := TWSmartPointer<TGpRegion>.Create(TGpRegion.Create(pAspectRatio.ViewBox.ToGpRectF()));

        // set the clipping region, replace the existing one if any
        if (prevRegion.IsInfinite(pGraphics)) then
            pGraphics.SetClip(pRegion)
        else
            pGraphics.SetClip(pRegion, CombineModeReplace);

        // notify that the image was clipped
        Result := True;

        // calculate the transform matrix to apply, in order to fit the image in the
        // aspect ratio bounding box
        if (pAspectRatio.ElementBox.Width < pAspectRatio.ElementBox.Height) then
            ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Height / boundingBox.Height,
                    pAspectRatio.BoundingBox.Height / boundingBox.Height, pGraphics)
        else
        if (pAspectRatio.ElementBox.Width > pAspectRatio.ElementBox.Height) then
            ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Width / boundingBox.Width,
                    pAspectRatio.BoundingBox.Width / boundingBox.Width, pGraphics)
        else
        begin
            if (pAspectRatio.ViewBox.Width <= pAspectRatio.ViewBox.Width) then
                ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Width / boundingBox.Width,
                        pAspectRatio.BoundingBox.Width / boundingBox.Width, pGraphics)
            else
                ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Height / boundingBox.Height,
                        pAspectRatio.BoundingBox.Height / boundingBox.Height, pGraphics);
        end;
    end
    else
    begin
        // meet image, calculate the transform matrix to apply, in order to fit
        // the image in the aspect ratio bounding box
        if (pAspectRatio.ElementBox.Width < pAspectRatio.ElementBox.Height) then
            ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Width / boundingBox.Width,
                    pAspectRatio.BoundingBox.Width / boundingBox.Width, pGraphics)
        else
        if (pAspectRatio.ElementBox.Width > pAspectRatio.ElementBox.Height) then
            ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Height / boundingBox.Height,
                    pAspectRatio.BoundingBox.Height / boundingBox.Height, pGraphics)
        else
        begin
            if (pAspectRatio.ViewBox.Width <= pAspectRatio.ViewBox.Width) then
                ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Height / boundingBox.Height,
                        pAspectRatio.BoundingBox.Height / boundingBox.Height, pGraphics)
            else
                ApplyMatrix(pMatrix, svgPos, pAspectRatio.BoundingBox.Width / boundingBox.Width,
                        pAspectRatio.BoundingBox.Width / boundingBox.Width, pGraphics);
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGDIPlusRasterizer.ApplyMatrix(const pMatrix: TGpMatrix; const pos: TPoint;
        scaleW, scaleH: Single; pGraphics: TGpGraphics);
var
    pTransformMatrix: IWSmartPointer<TGpMatrix>;
begin
    if (not Assigned(pGraphics)) then
        Exit;

    // clone global matrix to apply (global matrix should not be modified because it may be applied
    // again to children elements)
    pTransformMatrix := TWSmartPointer<TGpMatrix>.Create(pMatrix.Clone);

    // set svg element to final size
    pTransformMatrix.Scale(scaleW, scaleH, MatrixOrderAppend);

    // set svg element to final location (applying user position and viewbox correction)
    pTransformMatrix.Translate(pos.X, pos.Y, MatrixOrderAppend);

    // apply transformation matrix to graphics
    pGraphics.SetTransform(pTransformMatrix);
end;
//---------------------------------------------------------------------------
procedure TWSVGGDIPlusRasterizer.UpdateBoundingBox(const point: TGpPointF; var boundingBox: TGpRectF);
begin
    // update bounding box
    if (boundingBox.X <> 0.0) then
        boundingBox.X := Min(point.X, boundingBox.X)
    else
        boundingBox.X := point.X;

    if (boundingBox.Width <> 0.0) then
        boundingBox.Width := Max(point.X, boundingBox.X + boundingBox.Width)
    else
        boundingBox.Width := point.X;

    if (boundingBox.Y <> 0.0) then
        boundingBox.Y := Min(point.Y, boundingBox.Y)
    else
        boundingBox.Y := point.Y;

    if (boundingBox.Height <> 0.0) then
        boundingBox.Height := Max(point.Y, boundingBox.Y + boundingBox.Height)
    else
        boundingBox.Height := point.Y;
end;
//---------------------------------------------------------------------------
{$ifdef TRIAL_BUILD}
    // apply trial time watermark function
    procedure TWSVGGDIPlusRasterizer.PrepareRenderer(const rect: TWRectF; const pos: TPoint;
            scaleW, scaleH: Single; pCanvas: TCanvas; pGraphics: TGpGraphics);
    var
        pRenderer:             TWRenderer_GDIPlus;
        pTextFormat:           IWSmartPointer<TGpStringFormat>;
        pTextFont:             IWSmartPointer<TFont>;
        pFont:                 IWSmartPointer<TGpFont>;
        pBrush:                IWSmartPointer<TWSolidBrush>;
        pFill:                 IWSmartPointer<TWFill>;
        pGradientFactory:      IWSmartPointer<TWGDIPlusGradient>;
        pBgBrush:              IWSmartPointer<TGpBrush>;
        pMatrix:               IWSmartPointer<TGpMatrix>;
        textRect:              TWRectF;
        textPos:               TGpPointF;
        boundingBox, drawRect: TGpRectF;
        svgPos:                TPoint;
        textColor, bgColor:    TWColor;
        textHeight:            Integer;
        codeCheck:             NativeUInt;
        stamp:                 TBytes;
        c:                     Byte;
    begin
        if (not Assigned(pGraphics)) then
            Exit;

        // decode the stamp containing the watermark
        stamp     := DecodeBase64(C_WMTM_Stamp);
        codeCheck := $0dff4c2d;

        for c in stamp do
            codeCheck := codeCheck xor ((codeCheck shl 5) + (codeCheck shr 2) + Ord(c));

        drawRect := rect.ToGpRectF;

        // check if security code is tampered
        {$ifdef CPUX64}
            if ((Length(C_WMTM_Stamp) <> 56) or (Length(stamp) < 42) or (codeCheck <> $defc93e87eee0cd9)) then
        {$else}
            if ((Length(C_WMTM_Stamp) <> 56) or (Length(stamp) < 42) or (codeCheck <> $6d712bd)) then
        {$endif}
            begin
                // if tampered, paint a full white rect to kill the SVG painting
                svgPos  := CalculateFinalPos(pos, drawRect, scaleW, scaleH);
                pMatrix := TWSmartPointer<TGpMatrix>.Create();
                ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);

                bgColor  := TWColor.Create(255, 255, 255, 255);
                pBgBrush := TWSmartPointer<TGpBrush>.Create(bgColor.GetGDIPlusSolidBrush);
                pGraphics.FillRectangle(pBgBrush, drawRect);
                Exit;
            end;

        // check if watermark should be drawn (this happen only if the trial code is used)
        if ((stamp[1] <> $6d) and (stamp[Length(stamp)] <> $6e)) then
            Exit;

        // get GDI+ renderer
        pRenderer := TWControlRenderer.GetGDIPlusRenderer;

        // found it?
        if (not Assigned(pRenderer)) then
            Exit;

        // enable the antialiasing for this rendering (it's the final rendering so don't care about
        // restoring the state)
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

        // calculate the text height to apply
        if (rect.Width > rect.Height) then
            textHeight := -Round(rect.Height / 6.0)
        else
            textHeight := -Round(rect.Width / 6.0);

        // prepare the font to draw the watermark
        pTextFont        := TWSmartPointer<TFont>.Create();
        pTextFont.Name   := 'Arial';
        pTextFont.Height := textHeight;
        pFont            := TWSmartPointer<TGpFont>.Create(TGpFont.Create(pCanvas.Handle, pTextFont.Handle));

        // calculate the watermark position
        textPos.X := Round(rect.Right  - ((rect.Width  / 6.0) * 2.0));
        textPos.Y := Round(rect.Bottom - ((rect.Height / 6.0) * 2.0));

        // calculate and apply the final transformations to put the watermark in the SVG space
        svgPos  := CalculateFinalPos(pos, drawRect, scaleW, scaleH);
        pMatrix := TWSmartPointer<TGpMatrix>.Create();
        ApplyMatrix(pMatrix, svgPos, scaleW, scaleH, pGraphics);

        // configure the text format
        pTextFormat := TWSmartPointer<TGpStringFormat>.Create(TGpStringFormat.Create());
        pTextFormat.SetAlignment(StringAlignmentNear);
        pTextFormat.SetLineAlignment(StringAlignmentNear);
        pTextFormat.SetFormatFlags(StringFormatFlagsNoWrap);
        pTextFormat.SetTrimming(StringTrimmingNone);

        // measure the rect surrounding the watermark
        pGraphics.MeasureString(Chr(stamp[15]), 1, pFont, drawRect, pTextFormat, boundingBox);

        // move the rect to final location
        boundingBox.X := textPos.X;
        boundingBox.Y := textPos.Y;

        boundingBox.X      := boundingBox.X      - 1.0;
        boundingBox.Y      := boundingBox.Y      - 1.0;
        boundingBox.Width  := boundingBox.Width  + 2.0;
        boundingBox.Height := boundingBox.Height + 2.0;

        // create a gradient brush, thus the watermark remains discrete but don't disappear completely
        // if the background is dark
        pGradientFactory := TWSmartPointer<TWGDIPlusGradient>.Create();
        pGradientFactory.SetCenterPoint(TWPointF.Create(Round(boundingBox.X + (boundingBox.Width / 2.0)),
                Round(boundingBox.Y + (boundingBox.Height / 2.0))));

        // set the start color white opaque
        bgColor   := TWColor.GetDefault;
        bgColor.R := 255;
        bgColor.G := 255;
        bgColor.B := 255;
        bgColor.A := 255;
        pGradientFactory.SetStartColor(@bgColor);

        // set the end color white transparent
        bgColor.A := 0;
        pGradientFactory.SetEndColor(@bgColor);

        // get the radial brush
        pBgBrush := TWSmartPointer<TGpBrush>.Create(pGradientFactory.GetRadial
                (TWSizeF.Create(boundingBox.Width - 1.0, boundingBox.Height - 1.0)));

        // fill the text background with the gradient brush
        pGraphics.FillRectangle(pBgBrush, boundingBox);

        textColor := TWColor.GetDefault;

        // get the brush to use to draw the watermark text
        pBrush       := TWSmartPointer<TWSolidBrush>.Create();
        pBrush.Color := @textColor;

        // get text rect
        textRect.Left   := boundingBox.X;
        textRect.Top    := boundingBox.Y;
        textRect.Right  := boundingBox.X + boundingBox.Width;
        textRect.Bottom := boundingBox.Y + boundingBox.Height;

        pFill := TWSmartPointer<TWFill>.Create();
        pFill.SetBrush(pBrush);

        // draw the watermark
        pRenderer.DrawString(Chr(stamp[15]), textPos, pFont, pFill, pGraphics, textRect);
    end;
{$endif}
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.Draw(const pSVG: TWSVG; const pos: TPoint; scale: Single; antialiasing: Boolean;
        const animation: TWSVGRasterizer.IAnimation; pCanvas: TCustomCanvas): Boolean;
var
    pGDICanvas: TCanvas;
    pGraphics:  IWSmartPointer<TGpGraphics>;
begin
    // is GDI+ initialized?
    if (m_GDIPlusToken = 0) then
        Exit(False);

    if (not(pCanvas is TCanvas)) then
        Exit(False);

    // get GDI canvas
    pGDICanvas := pCanvas as TCanvas;

    // found it?
    if (not Assigned(pGDICanvas)) then
        Exit(False);

    // get GDI+ graphics from canvas
    pGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pGDICanvas.Handle));

    // found it?
    if (not Assigned(pGraphics)) then
        Exit(False);

    Initialize(pSVG);

    // enable antialiasing, if needed
    if (antialiasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // draw all elements contained in SVG
    Result := DrawElements(pSVG.Parser.ElementList, pos, scale, scale, antialiasing, False,
            animation, pGDICanvas, pGraphics);
end;
//---------------------------------------------------------------------------
function TWSVGGDIPlusRasterizer.Draw(const pSVG: TWSVG; const rect: TRect; proportional, antialiasing: Boolean;
        const animation: TWSVGRasterizer.IAnimation; pCanvas: TCustomCanvas): Boolean;
var
    pGDICanvas:                                TCanvas;
    pGraphics:                                 IWSmartPointer<TGpGraphics>;
    pos:                                       TPoint;
    sourceSize:                                TSize;
    square, drawRect:                          TRect;
    scale, width, srcWidth, height, srcHeight: Single;
begin
    // is GDI+ initialized?
    if (m_GDIPlusToken = 0) then
        Exit(False);

    if (not(pCanvas is TCanvas)) then
        Exit(False);

    // get GDI canvas
    pGDICanvas := pCanvas as TCanvas;

    // found it?
    if (not Assigned(pGDICanvas)) then
        Exit(False);

    // get GDI+ graphics from canvas
    pGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pGDICanvas.Handle));

    // found it?
    if (not Assigned(pGraphics)) then
        Exit(False);

    Initialize(pSVG);

    // enable antialiasing, if needed
    if (antialiasing) then
        pGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

    // get source size
    sourceSize := GetSize(pSVG);

    // is size valid?
    if ((sourceSize.Width = 0) or (sourceSize.Height = 0)) then
    begin
        // calculate svg position
        pos := TPoint.Create(rect.Left, rect.Top);

        // cannot determine the size, so draw the svg without size calculation
        Exit(Draw(pSVG, pos, 1.0, antialiasing, animation, pCanvas));
    end;

    // do keep image proportional?
    if (proportional) then
    begin
        // get closest square contained inside rect
        square := GetClosestSquare(rect);

        drawRect.Left   := 0;
        drawRect.Top    := 0;
        drawRect.Right  := square.Width;
        drawRect.Bottom := square.Height;

        // calculate the proportional size to draw the complete svg inside the draw rect
        TWImageHelper.GetProportionalSize(sourceSize.Width, sourceSize.Height, drawRect.Right,
                drawRect.Bottom, True);

        // calculate svg position
        pos.Create(rect.Left + ((rect.Width - drawRect.Width) div 2), rect.Top
                + ((rect.Height - drawRect.Height) div 2));

        // calculate scale factor
        width    := drawRect.Width;
        srcWidth := sourceSize.Width;
        scale    := (width / srcWidth);

        // draw svg inside draw rectangle
        Exit(DrawElements(pSVG.Parser.ElementList, pos, scale, scale, antialiasing, False, animation,
                pGDICanvas, pGraphics));
    end;

    // calculate svg position
    pos := TPoint.Create(rect.Left, rect.Top);

    width     := rect.Width;
    srcWidth  := sourceSize.Width;
    height    := rect.Height;
    srcHeight := sourceSize.Height;

    // draw svg inside draw rectangle
    Result := DrawElements(pSVG.Parser.ElementList, pos, (width / srcWidth), (height / srcHeight),
            antialiasing, False, animation, pGDICanvas, pGraphics);
end;
//---------------------------------------------------------------------------

end.
