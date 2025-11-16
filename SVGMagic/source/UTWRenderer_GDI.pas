{**
 @abstract(@name provides a renderer using the GDI to perform the drawing.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWRenderer_GDI;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     Vcl.Graphics,
     Winapi.Windows,
     UTWSmartPointer,
     UTWColor,
     UTWFillAndStroke,
     UTWPoint,
     UTWSize,
     UTWRect,
     UTWHelpers,
     UTWGraphicPath,
     UTWRendererCommon,
     UTWRenderer,
     UTWControlFont;

const
    //---------------------------------------------------------------------------
    // Global constants
    //---------------------------------------------------------------------------
    {**
     Determine which draw function should be used by default
    }
    C_Default_Draw_Text_Func: TWGDIHelper.IEDrawTextFunc = TWGDIHelper.IEDrawTextFunc.IE_DF_GDI;
    //---------------------------------------------------------------------------

type
    {**
     Renderer using GDI to perform the drawing
    }
    TWRenderer_GDI = class(TWRenderer)
        private
            m_pGDIHelper: TWGDIHelper;

            {**
             Draw a rect using solid colors
             @param(fillColor Color to use to fill the rect)
             @param(strokeColor Color to use to outline the rect)
             @param(strokeWidth Stroke width to use)
             @param(fillWithOutline If @true, the rect should be filled with outline (i.e. pen) color)
             @param(rect Rect to paint)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @param(pCanvas Canvas on which rect should be painted)
            }
            procedure DrawSolidRect(const fillColor, strokeColor: TWColor; strokeWidth: Integer;
                    fillWithOutline: Boolean; const rect: TRect; out iRect: TRect;
                    pCanvas: TCanvas);

            {**
             Draw a rounded rect using solid colors
             @param(fillColor Color to use to fill the rect)
             @param(strokeColor Color to use to outline the rect)
             @param(strokeWidth Stroke width to use)
             @param(radius Corner radius)
             @param(fillWithOutline If @true, the rect should be filled with outline (i.e. pen) color)
             @param(rect Rect to paint)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @param(pCanvas Canvas on which rect should be painted)
            }
            procedure DrawSolidRoundedRect(const fillColor, strokeColor: TWColor; strokeWidth: Integer;
                    radius: Cardinal; fillWithOutline: Boolean; const rect: TRect; out iRect: TRect;
                    pCanvas: TCanvas);

        protected
            {**
             Draw rounded rectangle
             @param(rect Rectangle area to draw)
             @param(pOptions @bold([in, out]) Rectangle paint options)
             @param(hDC Graphic device context to draw on)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) This function is needed because GDI round rects are ugly when pen width
                             is higher than 1
             @br @bold(NOTE) This function is partially implemented
            }
            function DrawRoundedRect(const rect: TWRectF; var pOptions: TWRenderer.IRectOptions;
                    hDC: THandle; out iRect: TRect): Boolean; virtual;

        public
            constructor Create; override;
            destructor Destroy; override;

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
             @param(pOptions @bold([in, out]) Rectangle paint options)
             @param(hDC Device context handle)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) This function is partially implemented
            }
            function DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions; hDC: THandle;
                    out iRect: TRect): Boolean; override;

            {**
             Draw polygon
             @param(rect Rectangle area to draw)
             @param(pointList Point list to draw)
             @param(pOptions Polygon paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) This function is partially implemented
            }
            function DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle):Boolean; override;

            {**
             Draw path
             @param(rect Rectangle in which path should be drawn)
             @param(path Path to draw)
             @param(pOptions Path paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) This function is partially implemented
            }
            function DrawPath(const rect: TWRectF; const path: TWPathCmds;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean; override;

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
            }
            procedure DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
                    const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions); override;

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
             @returns(Font handle, @nil on error)
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
    end;

implementation

uses
  System.UITypes;

//---------------------------------------------------------------------------
// Global functions
//---------------------------------------------------------------------------
procedure ConvertTextOptions(drawTextFunc: EDrawTextFunc; const inOpt: TWRenderer.ITextOptions;
        out outOpt: TWGDIHelper.ITextOptions);
begin
    // convert general options
    outOpt.m_RightToLeft      := inOpt.RightToLeft;
    outOpt.m_NoWrap           := inOpt.NoWrap;
    outOpt.m_NoClip           := inOpt.NoClip;
    outOpt.m_ShowHotkeyPrefix := inOpt.ShowHotkeyPrefix;

    // convert horz alignment
    case (inOpt.AlignHorz) of
        E_H_Left:   outOpt.m_HAlign := TWGDIHelper.IEHAlign.IE_HA_Left;
        E_H_Center: outOpt.m_HAlign := TWGDIHelper.IEHAlign.IE_HA_Center;
        E_H_Right:  outOpt.m_HAlign := TWGDIHelper.IEHAlign.IE_HA_Right;
    else
        raise Exception.CreateFmt('Unknown horizontal alignment - %d', [Integer(inOpt.AlignHorz)]);
    end;

    // convert vert alignment
    case (inOpt.AlignVert) of
        E_V_Top:    outOpt.m_VAlign := TWGDIHelper.IEVAlign.IE_VA_Top;
        E_V_Center: outOpt.m_VAlign := TWGDIHelper.IEVAlign.IE_VA_Center;
        E_V_Bottom: outOpt.m_VAlign := TWGDIHelper.IEVAlign.IE_VA_Bottom;
    else
        raise Exception.CreateFmt('Unknown vertical alignment - %d', [Integer(inOpt.AlignVert)]);
    end;

    // convert text trimming
    case (inOpt.TextTrimming) of
        E_TT_None:              outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_None;
        E_TT_Character:         outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_Character;
        E_TT_Word:              outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_Word;
        E_TT_EllipsisCharacter: outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_EllipsisCharacter;
        E_TT_EllipsisWord:      outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_EllipsisWord;
        E_TT_EllipsisPath:      outOpt.m_TextTrimming := TWGDIHelper.IETextTrimming.IE_TT_EllipsisPath;
    else
        raise Exception.CreateFmt('Unknown trimming - %d', [Integer(inOpt.TextTrimming)]);
    end;

    // convert draw text function to use
    case (drawTextFunc) of
        E_DF_Default:
        begin
            // convert user selected value
            case (inOpt.DrawTextFunc) of
                E_DF_Default:      outOpt.m_DrawTextFunc := C_Default_Draw_Text_Func;
                E_DF_GDI:          outOpt.m_DrawTextFunc := TWGDIHelper.IEDrawTextFunc.IE_DF_GDI;
                E_DF_WS_Optimized: outOpt.m_DrawTextFunc := TWGDIHelper.IEDrawTextFunc.IE_DF_WS_Optimized;
            else
                raise Exception.CreateFmt('Unknown draw text function - %d', [Integer(inOpt.DrawTextFunc)]);
            end;
        end;

        E_DF_GDI:          outOpt.m_DrawTextFunc := TWGDIHelper.IEDrawTextFunc.IE_DF_GDI;
        E_DF_WS_Optimized: outOpt.m_DrawTextFunc := TWGDIHelper.IEDrawTextFunc.IE_DF_WS_Optimized;
    else
        raise Exception.CreateFmt('Unknown draw text function - %d', [Integer(drawTextFunc)]);
    end;
end;
//---------------------------------------------------------------------------
// TWRenderer_GDI
//---------------------------------------------------------------------------
constructor TWRenderer_GDI.Create;
begin
    inherited Create;

    m_pGDIHelper := TWGDIHelper.Create;
end;
//---------------------------------------------------------------------------
destructor TWRenderer_GDI.Destroy;
begin
    m_pGDIHelper.Free;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDI.DrawSolidRect(const fillColor, strokeColor: TWColor; strokeWidth: Integer;
        fillWithOutline: Boolean; const rect: TRect; out iRect: TRect;
        pCanvas: TCanvas);
var
    hPrevRgn:   THandle;
    clipResult: Integer;
begin
    // configure canvas brush
    if (fillWithOutline) then
        pCanvas.Brush.Color := strokeColor.GetColor
    else
        pCanvas.Brush.Color := fillColor.GetColor;

    pCanvas.Brush.Style := bsSolid;

    // simply fill the rectangle with configured color
    pCanvas.FillRect(rect);

    // no outline to draw?
    if ((strokeWidth = 0) or fillWithOutline) then
    begin
        // get internal rectangle
        if (fillWithOutline) then
            iRect := Default(TRect)
        else
            iRect := rect;

        Exit;
    end;

    // get current clipping region
    hPrevRgn   := 0;
    clipResult := GetClipRgn(pCanvas.Handle, hPrevRgn);

    try
        // calculate internal rectangle
        iRect := TRect.Create(rect.Left + strokeWidth, rect.Top + strokeWidth, rect.Right - strokeWidth,
                rect.Bottom - strokeWidth);

        // select rounded rectangle as clip region
        ExcludeClipRect(pCanvas.Handle, iRect.Left, iRect.Top, iRect.Right, iRect.Bottom);

        // configure canvas brush
        pCanvas.Brush.Color := strokeColor.GetColor;
        pCanvas.Brush.Style := bsSolid;

        // draw outline
        pCanvas.FillRect(rect);
    finally
        // unselect clip region
        if (clipResult > 0) then
            SelectClipRgn(pCanvas.Handle, hPrevRgn)
        else
            SelectClipRgn(pCanvas.Handle, 0);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDI.DrawSolidRoundedRect(const fillColor, strokeColor: TWColor; strokeWidth: Integer;
        radius: Cardinal; fillWithOutline: Boolean; const rect: TRect; out iRect: TRect;
        pCanvas: TCanvas);
var
    color: TColor;
    i:     Integer;
begin
    // get fill color
    if (fillWithOutline) then
        color := strokeColor.GetColor
    else
        color := fillColor.GetColor;

    // configure global canvas brush
    pCanvas.Brush.Style := bsSolid;

    // do draw outline?
    if ((strokeWidth = 0) or fillWithOutline) then
    begin
        if (fillWithOutline) then
            iRect := Default(TRect)
        else
            iRect := rect;

        // configure canvas brush
        pCanvas.Brush.Color := color;

        // configure canvas pen
        pCanvas.Pen.Width := 1;
        pCanvas.Pen.Color := color;
        pCanvas.Pen.Style := psSolid;

        // draw round rectangle
        pCanvas.RoundRect(rect, radius, radius);
        Exit;
    end;

    iRect := rect;

    // configure canvas pen
    pCanvas.Pen.Style := psSolid;
    pCanvas.Pen.Color := strokeColor.GetColor;
    pCanvas.Pen.Width := 1;

    // configure canvas brush
    pCanvas.Brush.Color := pCanvas.Pen.Color;

    // for each width level, draw a complete rounded rect using an outline width of 1 and decrease
    // draw rect size by 1 until last level is reached. This is a workaround, otherwise GDI draws
    // ugly asymmetric rounded rect when outline width is higher than 1
    for i := 0 to strokeWidth - 1 do
    begin
        // do draw last rect?
        if (i = strokeWidth - 1) then
            // configure background color to real final color
            pCanvas.Brush.Color := fillColor.GetColor;

        // draw outlined round rectangle
        pCanvas.RoundRect(iRect, radius, radius);

        // decrease draw rect to draw next width level
        iRect.Inflate(-1, -1);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawRoundedRect(const rect: TWRectF; var pOptions: TWRenderer.IRectOptions;
        hDC: THandle; out iRect: TRect): Boolean;
var
    drawRect:                           TRect;
    strokeWidth, clipResult:            Integer;
    maxSize, outlineWidth, radius, i:   Cardinal;
    gradientStopCount:                  NativeUInt;
    fillWithOutline:                    Boolean;
    mode:                               ULONG;
    hPrevRgn:                           THandle;
    gRect:                              GRADIENT_RECT;
    pSolidFillBrush, pSolidStrokeBrush: TWSolidBrush;
    pLinearFillBrush:                   TWLinearGradientBrush;
    pRadialFillBrush:                   TWRadialGradientBrush;
    pCanvas:                            IWSmartPointer<TCanvas>;
    vertex:                             array [0..1] of TRIVERTEX;
begin
    // no device context?
    if (hDC = 0) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // get canvas
    pCanvas        := TWSmartPointer<TCanvas>.Create();
    pCanvas.Handle := hDC;

    maxSize      := Round(GetMaxSize(rect));
    outlineWidth := 0;

    // get radius and pen width
    radius      := Min(pOptions.Radius.LeftTop.X * 2, NativeInt(maxSize));
    strokeWidth := ConvertStrokeWidth(pOptions.Stroke);

    // do draw outline?
    if (strokeWidth <> 0) then
        // limit outline width to max value
        outlineWidth := Min(strokeWidth, maxSize);

    // check if rect is completely filled by outline
    fillWithOutline := (strokeWidth <> 0) and (Cardinal(strokeWidth) >= maxSize);

    // get fill type
    case (pOptions.Fill.BrushType) of
        E_BT_Solid:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the solid fill brush
                    pSolidFillBrush := pOptions.Fill.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    DrawSolidRoundedRect(pSolidFillBrush.Color^, pSolidStrokeBrush.Color^,
                            outlineWidth, radius, fillWithOutline, rect.ToTRect(True), iRect,
                            pCanvas);
                    Exit(True);
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;

        E_BT_Linear:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the linear gradient fill brush
                    pLinearFillBrush := pOptions.Fill.Brush as TWLinearGradientBrush;
                    Assert(Assigned(pLinearFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    // get gradient stop count
                    gradientStopCount := pLinearFillBrush.Stops.Count;

                    if (gradientStopCount = 0) then
                        Exit(False);

                    if (gradientStopCount = 1) then
                    begin
                        DrawSolidRoundedRect(pLinearFillBrush.Stops[0].Color^, pSolidStrokeBrush.Color^,
                                outlineWidth, radius, fillWithOutline, rect.ToTRect(True), iRect, pCanvas);
                        Exit(True);
                    end;

                    drawRect := rect.ToTRect(True);

                    hPrevRgn := 0;

                    // get current clipping region
                    clipResult := GetClipRgn(hDC, hPrevRgn);

                    try
                        // create a path containing the rounded rect
                        BeginPath(hDC);
                        RoundRect(hDC, drawRect.Left, drawRect.Top, drawRect.Right, drawRect.Bottom,
                                radius, radius);
                        EndPath(hDC);

                        // clip round rectangle
                        SelectClipPath(hDC, RGN_COPY);

                        vertex[0].x     := drawRect.Left;
                        vertex[0].y     := drawRect.Top;
                        vertex[0].Red   := pLinearFillBrush.Stops[0].Color.GetRed   shl 8;
                        vertex[0].Green := pLinearFillBrush.Stops[0].Color.GetGreen shl 8;
                        vertex[0].Blue  := pLinearFillBrush.Stops[0].Color.GetBlue  shl 8;
                        vertex[0].Alpha := pLinearFillBrush.Stops[0].Color.GetAlpha shl 8;

                        vertex[1].x     := drawRect.Right;
                        vertex[1].y     := drawRect.Bottom;
                        vertex[1].Red   := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetRed   shl 8;
                        vertex[1].Green := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetGreen shl 8;
                        vertex[1].Blue  := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetBlue  shl 8;
                        vertex[1].Alpha := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetAlpha shl 8;

                        // create a GRADIENT_RECT structure that references the TRIVERTEX vertices
                        gRect.UpperLeft  := 0;
                        gRect.LowerRight := 1;

                        // is gradient horizontal or vertical?
                        if (pLinearFillBrush.Stops[0].Position.X = pLinearFillBrush.Stops[1].Position.X)
                        then
                            mode := GRADIENT_FILL_RECT_V
                        else
                            mode := GRADIENT_FILL_RECT_H;

                        // draw gradient fill
                        GradientFill(hDC, PTriVertex(@vertex[0]), 2, @gRect, 1, mode);
                    finally
                        // unselect clip region
                        if (clipResult > 0) then
                            SelectClipRgn(hDC, hPrevRgn)
                        else
                            SelectClipRgn(hDC, 0);
                    end;

                    // no outline to draw?
                    if (strokeWidth = 0) then
                    begin
                        iRect := Default(TRect);
                        Exit(True);
                    end;

                    iRect := drawRect;

                    // configure canvas pen
                    pCanvas.Pen.Style := psSolid;
                    pCanvas.Pen.Color := pSolidStrokeBrush.Color.GetColor;
                    pCanvas.Pen.Width := 1;

                    // for each width level, draw a complete rounded rect using an outline width of 1 and decrease
                    // draw rect size by 1 until last level is reached. This is a workaround, otherwise GDI draws
                    // ugly asymmetric rounded rect when outline width is higher than 1
                    for i := 0 to outlineWidth - 1 do
                    begin
                        // configure canvas brush
                        pCanvas.Brush.Style := bsClear;

                        // draw outlined round rectangle
                        pCanvas.RoundRect(iRect, radius, radius);

                        // is last rect?
                        if (i <> outlineWidth - 1) then
                        begin
                            // get current clipping region
                            clipResult := GetClipRgn(hDC, hPrevRgn);

                            try
                                // create a path containing the surface rect to draw
                                BeginPath(hDC);
                                Rectangle(hDC, drawRect.Left, drawRect.Top, drawRect.Right, drawRect.Bottom);
                                EndPath(hDC);

                                // select entire draw area as clipping path
                                SelectClipPath(hDC, RGN_COPY);

                                // create a path containing the gradient zone
                                BeginPath(hDC);
                                RoundRect(hDC, iRect.Left + 1, iRect.Top + 1, iRect.Right - 1,
                                        iRect.Bottom - 1, radius, radius);
                                EndPath(hDC);

                                // create a mask to preserve gradient zone
                                SelectClipPath(hDC, RGN_XOR);

                                // configure canvas brush
                                pCanvas.Brush.Color := pSolidStrokeBrush.Color.GetColor;
                                pCanvas.Brush.Style := bsSolid;

                                // draw outlined round rectangle
                                pCanvas.RoundRect(iRect, radius, radius);
                            finally
                                // unselect clip region
                                if (clipResult > 0) then
                                    SelectClipRgn(hDC, hPrevRgn)
                                else
                                    SelectClipRgn(hDC, 0);
                            end;
                        end;

                        // decrease internal rect to draw next width level
                        iRect.Inflate(-1, -1);
                    end;

                    Exit(True);
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;

        E_BT_Radial:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the radial gradient fill brush
                    pRadialFillBrush := pOptions.Fill.Brush as TWRadialGradientBrush;
                    Assert(Assigned(pRadialFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    // get gradient stop count
                    gradientStopCount := pRadialFillBrush.Stops.Count;

                    if (gradientStopCount = 0) then
                        Exit(False);

                    if (gradientStopCount = 1) then
                    begin
                        DrawSolidRoundedRect(pRadialFillBrush.Stops[0].Color^, pSolidStrokeBrush.Color^,
                                outlineWidth, radius, fillWithOutline, rect.ToTRect(True), iRect, pCanvas);
                        Exit(True);
                    end;

                    raise Exception.Create('NOT IMPLEMENTED');
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pOptions.Fill.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDI.BeginScene(hDC: THandle);
begin
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDI.EndScene;
begin
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
        hDC: THandle; out iRect: TRect): Boolean;
var
    drawRect:                           TRect;
    maxSize:                            Cardinal;
    strokeWidth, clipResult:            Integer;
    gradientStopCount:                  NativeUInt;
    hPrevRgn:                           THandle;
    fillWithOutline:                    Boolean;
    mode:                               ULONG;
    gRect:                              GRADIENT_RECT;
    pSolidFillBrush, pSolidStrokeBrush: TWSolidBrush;
    pLinearFillBrush:                   TWLinearGradientBrush;
    pRadialFillBrush:                   TWRadialGradientBrush;
    pCanvas:                            IWSmartPointer<TCanvas>;
    vertex:                             array [0..1] of TRIVERTEX;
begin
    // no device context?
    if (hDC = 0) then
    begin
        iRect := Default(TRect);
        Exit(False);
    end;

    // is radius regular?
    if (not IsRegular(pOptions.Radius)) then
        raise Exception.Create('NOT IMPLEMENTED');

    // do draw rounded rectangle?
    if (not pOptions.Radius.LeftTop.IsZero) then
        Exit(DrawRoundedRect(rect, pOptions, hDC, iRect));

    // get canvas
    pCanvas        := TWSmartPointer<TCanvas>.Create();
    pCanvas.Handle := hDC;

    maxSize     := Round(GetMaxSize(rect));
    strokeWidth := ConvertStrokeWidth(pOptions.Stroke);

    // check if rect is completely filled by outline
    fillWithOutline := (strokeWidth <> 0) and (Cardinal(strokeWidth) >= maxSize);

    // get fill type
    case (pOptions.Fill.BrushType) of
        E_BT_Solid:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the solid fill brush
                    pSolidFillBrush := pOptions.Fill.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    // draw a solid rect with a solid outline
                    DrawSolidRect(pSolidFillBrush.Color^, pSolidStrokeBrush.Color^, strokeWidth,
                            fillWithOutline, rect.ToTRect(True), iRect, pCanvas);
                    Exit(True);
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;

        E_BT_Linear:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the linear gradient fill brush
                    pLinearFillBrush := pOptions.Fill.Brush as TWLinearGradientBrush;
                    Assert(Assigned(pLinearFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    gradientStopCount := pLinearFillBrush.Stops.Count;

                    if (gradientStopCount = 0) then
                        Exit(False);

                    // draw a solid rect with a solid outline
                    if (gradientStopCount = 1) then
                    begin
                        DrawSolidRect(pLinearFillBrush.Stops[0].Color^, pSolidStrokeBrush.Color^,
                                strokeWidth, fillWithOutline, rect.ToTRect(True), iRect, pCanvas);
                        Exit(True);
                    end;

                    if (gradientStopCount > 2) then
                        raise Exception.Create('NOT IMPLEMENTED');

                    drawRect := rect.ToTRect(True);

                    vertex[0].x     := drawRect.Left;
                    vertex[0].y     := drawRect.Top;
                    vertex[0].Red   := pLinearFillBrush.Stops[0].Color.GetRed   shl 8;
                    vertex[0].Green := pLinearFillBrush.Stops[0].Color.GetGreen shl 8;
                    vertex[0].Blue  := pLinearFillBrush.Stops[0].Color.GetBlue  shl 8;
                    vertex[0].Alpha := pLinearFillBrush.Stops[0].Color.GetAlpha shl 8;

                    vertex[1].x     := drawRect.Right;
                    vertex[1].y     := drawRect.Bottom;
                    vertex[1].Red   := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetRed   shl 8;
                    vertex[1].Green := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetGreen shl 8;
                    vertex[1].Blue  := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetBlue  shl 8;
                    vertex[1].Alpha := pLinearFillBrush.Stops[gradientStopCount - 1].Color.GetAlpha shl 8;

                    // create a GRADIENT_RECT structure that references the TRIVERTEX vertices
                    gRect.UpperLeft  := 0;
                    gRect.LowerRight := 1;

                    // is gradient horizontal or vertical?
                    if (pLinearFillBrush.Stops[0].Position.X = pLinearFillBrush.Stops[1].Position.X)
                    then
                        mode := GRADIENT_FILL_RECT_V
                    else
                        mode := GRADIENT_FILL_RECT_H;

                    // draw gradient fill
                    GradientFill(hDC, PTriVertex(@vertex[0]), 2, @gRect, 1, mode);

                    // no outline to draw?
                    if (strokeWidth = 0) then
                    begin
                        iRect := drawRect;
                        Exit(True);
                    end;

                    // get current clipping region
                    hPrevRgn   := 0;
                    clipResult := GetClipRgn(hDC, hPrevRgn);

                    try
                        // calculate internal rectangle
                        iRect := TRect.Create(drawRect.Left + strokeWidth, drawRect.Top + strokeWidth,
                                drawRect.Right - strokeWidth, drawRect.Bottom - strokeWidth);

                        // select rounded rectangle as clip region
                        ExcludeClipRect(hDC, iRect.Left, iRect.Top, iRect.Right, iRect.Bottom);

                        // configure canvas brush
                        pCanvas.Brush.Color := pSolidStrokeBrush.Color.GetColor;
                        pCanvas.Brush.Style := bsSolid;

                        // draw outline
                        pCanvas.FillRect(drawRect);
                    finally
                        // unselect clip region
                        if (clipResult > 0) then
                            SelectClipRgn(hDC, hPrevRgn)
                        else
                            SelectClipRgn(hDC, 0);
                    end;

                    Exit(True);
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;

        E_BT_Radial:
            // get stroke type
            case (pOptions.Stroke.BrushType) of
                E_BT_Solid:
                begin
                    // get the radial gradient fill brush
                    pRadialFillBrush := pOptions.Fill.Brush as TWRadialGradientBrush;
                    Assert(Assigned(pRadialFillBrush));

                    // get the solid stroke brush
                    pSolidStrokeBrush := pOptions.Stroke.Brush as TWSolidBrush;
                    Assert(Assigned(pSolidStrokeBrush));

                    gradientStopCount := pRadialFillBrush.Stops.Count;

                    if (gradientStopCount = 0) then
                        Exit(False);

                    // draw a solid rect with a solid outline
                    if (gradientStopCount = 1) then
                    begin
                        // draw a solid rect with a solid outline
                        DrawSolidRect(pRadialFillBrush.Stops[0].Color^, pSolidStrokeBrush.Color^,
                                strokeWidth, fillWithOutline, rect.ToTRect(True), iRect, pCanvas);
                        Exit(True);
                    end;

                    raise Exception.Create('NOT IMPLEMENTED');
                end;

                E_BT_Linear,
                E_BT_Radial: raise Exception.Create('NOT IMPLEMENTED');
            else
                raise Exception.CreateFmt('Unknown pen base brush type - %d',
                        [Integer(pOptions.Stroke.BrushType)]);
            end;
    else
        raise Exception.CreateFmt('Unknown brush type - %d', [Integer(pOptions.Fill.BrushType)]);
    end;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle):Boolean;
begin
    // not implemented for now
    raise Exception.Create('NOT IMPLEMENTED');
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawPath(const rect: TWRectF; const path: TWPathCmds;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean;
begin
    // not implemented for now
    raise Exception.Create('NOT IMPLEMENTED');
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.GetTextSize(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle; pCharacters: PInteger;
        pLines: PInteger): TWSizeF;
var
    textOptions: TWGDIHelper.ITextOptions;
    size:        TSize;
begin
    // no font?
    if (not Assigned(pOptions.Font)) then
        Exit(Default(TWSizeF));

    if (not Assigned(m_pGDIHelper)) then
        Exit(Default(TWSizeF));

    // prepare options
    textOptions := TWGDIHelper.ITextOptions.GetDefault;
    ConvertTextOptions(DrawTextFunc, pOptions, textOptions);

    // measure text size
    size := m_pGDIHelper.GetTextSize(text, rect.ToTRect(True), pOptions.Font, textOptions, hDC,
            pCharacters, pLines);

    // return text size
    Result := TWSizeF.Create(size.Width, size.Height);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawText(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean;
var
    textOptions:                TWGDIHelper.ITextOptions;
    textOpacity, shadowOpacity: Single;
    drawRect, shadowRect:       TRect;
    pShadowFont:                IWSmartPointer<TFont>;
begin
    // no font?
    if (not Assigned(pOptions.Font)) then
        Exit(False);

    if (not Assigned(m_pGDIHelper)) then
        Exit(False);

    // prepare options
    textOptions := TWGDIHelper.ITextOptions.GetDefault;
    ConvertTextOptions(DrawTextFunc, pOptions, textOptions);

    drawRect := rect.ToTRect(True);

    // do draw shadow?
    if (not pOptions.ShadowDelta.IsZero) then
    begin
        // calculate shadow opacity
        shadowOpacity := pOptions.ShadowColor.GetAlpha;
        shadowOpacity := shadowOpacity / 255.0;

        // get shadow font to use
        pShadowFont := TWSmartPointer<TFont>.Create();
        pShadowFont.Assign(pOptions.Font);
        pShadowFont.Color := pOptions.ShadowColor.GetColor;

        // calculate shadow rectangle
        shadowRect.Left   := drawRect.Left   + pOptions.ShadowDelta.X;
        shadowRect.Top    := drawRect.Top    + pOptions.ShadowDelta.Y;
        shadowRect.Right  := shadowRect.Left + drawRect.Width;
        shadowRect.Bottom := shadowRect.Top  + drawRect.Height;

        // draw shadow
        m_pGDIHelper.DrawText(text, shadowRect, pShadowFont, shadowOpacity, textOptions, hDC);
    end;

    // calculate text opacity
    textOpacity := pOptions.Alpha;
    textOpacity := textOpacity / 255.0;

    // draw text
    m_pGDIHelper.DrawText(text, drawRect, pOptions.Font, textOpacity, textOptions, hDC);

    Result := True;
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.DrawLayeredText(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean;
begin
    // DrawLayeredText is a workaround needed to draw a text correctly on a layered form using GDI+.
    // In some case, GDI+ draws a text surrounded by a black aura due to an alpha issue on layered
    // form. Because GDI doesn't support alpha transparency, it should not be used to draw on a layered
    // form, but for compatibility reason, this function is redirected to normal DrawText() function
    Result := DrawText(text, rect, pOptions, hDC);
end;
//---------------------------------------------------------------------------
procedure TWRenderer_GDI.DrawImage(const pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
        const destRect: TWRectF; const pOptions: TWRenderer.IImageOptions);
var
    pCanvas: IWSmartPointer<TCanvas>;
    pBitmap: IWSmartPointer<Vcl.Graphics.TBitmap>;
begin
    // create canvas from device context
    pCanvas        := TWSmartPointer<TCanvas>.Create();
    pCanvas.Handle := hDC;

    // do copy entire source image?
    if ((srcRect.Width = pGraphic.Width) and (srcRect.Height = pGraphic.Height)) then
    begin
        // stretch draw image. NOTE resize mode is always nearest neighbor, because it's the unique
        // resize mode available using GDI
        pCanvas.StretchDraw(destRect.ToTRect(True), pGraphic);
        Exit;
    end;

    pBitmap := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();

    // set 32bit only if transparent (otherwise non transparent PNG won't render)
    if (pGraphic.Transparent) then
    begin
        pBitmap.PixelFormat := pf32bit;
        SetBkMode(pBitmap.Canvas.Handle, TRANSPARENT);
    end
    else
        pBitmap.PixelFormat := pf24bit;

    pBitmap.SetSize(Round(srcRect.Width), Round(srcRect.Height));

    // copy TGraphic to bitmap
    pBitmap.Canvas.Draw(Round(srcRect.Left), Round(srcRect.Top), pGraphic);

    // stretch draw image. NOTE resize mode is always nearest neighbor, because it's the unique
    // resize mode available using GDI
    pCanvas.StretchDraw(destRect.ToTRect(True), pBitmap);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.GetCaps: TWRenderer.IDrawCaps;
begin
    Result := [IE_DeviceSupported];
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.AddFontToSession(const name, fileName: UnicodeString): Boolean;
begin
    // add font in opened Windows session
    Result := TWControlFont.AddFontToSession(name, fileName);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.AddFont(const name, fileName: UnicodeString): THandle;
begin
    // add custom font
    Result := TWControlFont.AddFont(name, fileName);
end;
//---------------------------------------------------------------------------
function TWRenderer_GDI.AddFont(const name: UnicodeString; pStream: TStream;
        fontLength: Cardinal): THandle;
begin
    // add custom font
    Result := TWControlFont.AddFont(name, pStream, fontLength);
end;
//---------------------------------------------------------------------------

end.
