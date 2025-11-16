{**
 @abstract(@name provides a generic renderer to use with WTControls.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWControlRenderer;

interface

uses System.Classes,
     System.SysUtils,
     Vcl.Graphics,
     Winapi.Windows,
     Winapi.GDIPAPI,
     UTWSize,
     UTWRect,
     UTWGraphicPath,
     UTWRendererCommon,
     UTWRenderer,
     UTWRenderer_GDI,
     UTWRenderer_GDIPlus
     {$ifdef WTCONTROLS_USE_DIRECT2D}
         ,
         TWRenderer_Direct2D
     {$endif}
     ;

type
    {**
     WTControl renderer, used to select the most appropriate renderer for drawing operations
    }
    TWControlRenderer = class
        public type
            {**
             Renderer type enumeration
            }
            IEType =
            (
                IE_RT_None,
                IE_RT_Auto,
                IE_RT_GDI,
                IE_RT_GDIPlus
                {$ifdef WTCONTROLS_USE_DIRECT2D}
                    ,
                    IE_RT_D2D
                {$endif}
            );

        private
            class var m_RendererType:          IEType;
            class var m_CanBuildScene:         Boolean;
            class var m_pActiveRenderer:       TWRenderer;
            class var m_pGDIRenderer:          TWRenderer_GDI;
            class var m_pGDIPlusRenderer:      TWRenderer_GDIPlus;
            {$ifdef WTCONTROLS_USE_DIRECT2D}
                class var m_pDirect2DRenderer: TWRenderer_Direct2D;
            {$endif}

            {**
             Select renderer to use
             @param(pTextOptions Text options, @nil if renderer is not required to draw text)
             @param(pImageOptions Image options, @nil if renderer is not required to draw image)
             @returns(@true if renderer was selected, otherwise @false)
            }
            class function SelectRenderer(const pTextOptions: TWRenderer.ITextOptions;
                    const pImageOptions: TWRenderer.IImageOptions): Boolean; static;

        public
            {**
             Constructor/Destructor
             @br @bold(NOTE) Because some objects used by this class, as e.g. WTRenderer_GDIPlus,
                             must be initialized only once during application lifetime, no instance
                             of this object should be created dynamically
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Get GDI+ token
             @returns(token, 0 if GDI+ is not initialized or on error)
             @br @bold(NOTE) GDI+ will be initialized if needed
            }
            class function GetGDIPlusToken: ULONG_PTR; static;

            {**
             Get GDI+ renderer
             @returns(GDI+ renderer)
            }
            class function GetGDIPlusRenderer: TWRenderer_GDIPlus; static;

            {**
             Select renderer to use
             @param(type Renderer type to use)
            }
            class procedure SetRendererType(rendererType: IEType); static;

            {**
             Get global draw text function currently used
             @returns(Global draw text function)
             @br @bold(NOTE) Global value will override the draw text function value passed in text
                             options if the value is other than E_DF_Default
             @br @bold(NOTE) Draw text function parameter can only be used with GDI renderer, in other
                             renderers this value do nothing
            }
            class function GetDrawTextFunc: EDrawTextFunc; static;

            {**
             Set global draw text function to use
             @param(drawTextFunc Draw text function to use)
             @br @bold(NOTE) Global value will override the draw text function value passed in text
                             options if the value is other than E_DF_Default
             @br @bold(NOTE) Draw text function parameter can only be used with GDI renderer, in
                             other renderers this value do nothing
            }
            class procedure SetDrawTextFunc(drawTextFunc: EDrawTextFunc); static;

            {**
             Begin a scene
             @param(hDC Device context to draw on)
            }
            class procedure BeginScene(hDC: THandle); static;

            {**
             End a scene
            }
            class procedure EndScene; static;

            {**
             Draw rectangle
             @param(rect Rectangle area to draw)
             @param(pOptions Rectangle paint options)
             @param(hDC Device context handle)
             @param(iRect @bold([out]) Internal rectangle (inside outline))
             @returns(@true on success, otherwise @false)
            }
            class function DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
                    hDC: THandle; out iRect: TRect): Boolean; static;

            {**
             Draw polygon
             @param(rect Rectangle area to draw)
             @param(pointList Point list to draw)
             @param(pOptions Polygon paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            class function DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean; static;

            {**
             Draw path
             @param(rect Rectangle in which path should be drawn)
             @param(path Path to draw)
             @param(pOptions Path paint options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            class function DrawPath(const rect: TWRectF; const path: TWPathCmds;
                    const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean; static;

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
             @returns(text size, empty size on error)
            }
            class function GetTextSize(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
                    pLines: PInteger = nil): TWSizeF; static;

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
            }
            class function GetFontSizeToFit(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; minWrapFontSize, minFontSize: Cardinal;
                    hDC: THandle; out fontSize: Cardinal): Boolean; static;

            {**
             Draw text
             @param(text Text to draw)
             @param(rect Rectangle area containing text)
             @param(pOptions Text GDI+ options)
             @param(hDC Device context handle)
             @returns(@true on success, otherwise @false)
            }
            class function DrawText(const text: UnicodeString; const rect: TWRectF;
                    const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean; static;

            {**
             Draw image
             @param(pGraphic Image to draw)
             @param(srcRect Source image rect to draw)
             @param(hDC Destination device context handle)
             @param(destRect Rect where image will be drawn on dest, stretched if not equal to source)
             @param(pOptions @bold([in, out]) Options, some can be updated after function ends)
            }
            class procedure DrawImage(pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
                    const destRect: TWRectF; pOptions: TWRenderer.IImageOptions); static;

            {**
             Add new custom font from file and set it available for applications opened in Windows session
             @param(name Font name to add)
             @param(fileName Font file name)
             @returns(@true on success, otherwise @false)
             @br @bold(NOTE) Thus added font will be available for ALL applications until the Windows
                             session is closed or the font is removed using RemoveFont or RemoveFonts.
                             But be careful, this is true ONLY for GDI fonts, and NOT for GDI+
            }
            class function AddFontToSession(const name, fileName: UnicodeString): Boolean; static;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(fileName Font file name)
             @returns(font handle, @nil on error)
            }
            class function AddFont(const name, fileName: UnicodeString): THandle; overload; static;

            {**
             Add new truetype font from stream
             @param(name Font name)
             @param(pStream Stream containing font data)
             @param(length Font data length)
             @returns(@true on success, otherwise @false)
            }
            class function AddFont(const name: UnicodeString; pStream: TStream; length: Cardinal): THandle; overload; static;

            {**
             Get font
             @param(name Font name)
             @returns(font, @nil if not found)
             @br @bold(NOTE) Only font previously added with AddFont() can be get. Standard or custom
                             font belonging to Windows session are not included
            }
            class function GetFont(const name: UnicodeString): THandle; static;

            {**
             Get font name from custom font file
             @param(fileName Font file name)
             @returns(font name, empty string on error)
            }
            class function GetFontName(const fileName: UnicodeString): UnicodeString; static;

            {**
             Remove previously added font from Windows session
             @param(name Font name)
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            class procedure RemoveFontFromSession(const name: UnicodeString); static;

            {**
             Remove all previously added fonts from Windows session
             @br @bold(NOTE) Be careful, on success, font will no longer be available for other
                             applications that eventually use it
             @br @bold(NOTE) All opened applications will be notified that fonts changed
            }
            class procedure RemoveFontsFromSession; static;

            {**
             Remove previously added font
             @param(name Font name)
            }
            class procedure RemoveFont(const name: UnicodeString); static;

            {**
             Remove all previously added fonts
            }
            class procedure RemoveFonts; static;
    end;

implementation
//---------------------------------------------------------------------------
constructor TWControlRenderer.Create;
begin
    inherited Create;

    raise Exception.Create('Instance of this object isn''t allowed');
end;
//---------------------------------------------------------------------------
destructor TWControlRenderer.Destroy;
begin
    inherited Destroy;

    raise Exception.Create('Instance of this object isn''t allowed');
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.SelectRenderer(const pTextOptions: TWRenderer.ITextOptions;
        const pImageOptions: TWRenderer.IImageOptions): Boolean;
var
    rendererType: IEType;
begin
    // build GDI renderer if still not exists
    if (not Assigned(m_pGDIRenderer)) then
        m_pGDIRenderer := TWRenderer_GDI.Create;

    // build GDI+ renderer if still not exists
    if (not Assigned(m_pGDIPlusRenderer)) then
        m_pGDIPlusRenderer := TWRenderer_GDIPlus.Create;

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        // build Direct2D renderer if still not exists
        if (not Assigned(m_pDirect2DRenderer)) then
            m_pDirect2DRenderer := TWRenderer_Direct2D.Create;
    {$endif}

    m_CanBuildScene := False;

    // link GDI renderer to GDI+
    m_pGDIPlusRenderer.LinkGDIRenderer(m_pGDIRenderer);

    // get renderer type to select
    rendererType := m_RendererType;

    // do auto select renderer?
    if (rendererType = IE_RT_Auto) then
    begin
        // search for first supported renderer, beginning by the most powerful
        {$ifdef WTCONTROLS_USE_DIRECT2D}
            // can use Direct2D?
            if (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pDirect2DRenderer.GetCaps) then
            begin
                m_CanBuildScene   := True;
                m_pActiveRenderer := m_pDirect2DRenderer;
                Exit(True);
            end;
        {$endif}

        // can use GDI+?
        if (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pGDIPlusRenderer.GetCaps) then
        begin
            // GDI renderer is better to draw text, except on layered forms, or if text is drawn
            // vertically or blurred
            if (Assigned(pTextOptions) and not pTextOptions.Layered and not pTextOptions.Vertical
                    and pTextOptions.ShadowBlur.IsZero
                    and (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pGDIRenderer.GetCaps))
            then
            begin
                m_pActiveRenderer := m_pGDIRenderer;
                Exit(True);
            end;

            // use GDI in case an image should be drawn and a simple neightbor nearest resize is needed
            if (Assigned(pImageOptions) and ((pImageOptions.ResizeMode = E_RzMode_Nearest)
                    or (pImageOptions.Vectorial and (pImageOptions.ResizeMode = E_RzMode_Auto))))
            then
            begin
                m_pActiveRenderer := m_pGDIRenderer;
                Exit(True);
            end;

            m_pActiveRenderer := m_pGDIPlusRenderer;
            Exit(True);
        end;

        // can use GDI?
        if (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pGDIRenderer.GetCaps) then
        begin
            m_pActiveRenderer := m_pGDIRenderer;
            Exit(True);
        end;

        m_pActiveRenderer := nil;
        Exit(False);
    end;

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        // do use Direct2D renderer and renderer is supported by system?
        if ((rendererType = IE_RT_D2D)
                and (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pDirect2DRenderer.GetCaps))
        then
        begin
            m_CanBuildScene   := True;
            m_pActiveRenderer := m_pDirect2DRenderer;
            Exit(True);
        end;
    {$endif}

    // do use Direct2D renderer and renderer is supported by system?
    if ((rendererType = IE_RT_GDIPlus)
            and (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pGDIPlusRenderer.GetCaps))
    then
    begin
        m_pActiveRenderer := m_pGDIPlusRenderer;
        Exit(True);
    end;

            // do use Direct2D renderer and renderer is supported by system?
    if ((rendererType = IE_RT_GDI)
            and (TWRenderer.IEDrawCaps.IE_DeviceSupported in m_pGDIRenderer.GetCaps))
    then
    begin
        m_pActiveRenderer := m_pGDIRenderer;
        Exit(True);
    end;

    // no supported renderer
    m_pActiveRenderer := nil;
    Result            := False;
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetGDIPlusToken: ULONG_PTR;
begin
    // build GDI+ renderer if still not exists
    if (not Assigned(m_pGDIPlusRenderer)) then
        m_pGDIPlusRenderer := TWRenderer_GDIPlus.Create;

    Result := m_pGDIPlusRenderer.GetToken;
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetGDIPlusRenderer: TWRenderer_GDIPlus;
begin
    // build GDI+ renderer if still not exists
    if (not Assigned(m_pGDIPlusRenderer)) then
        m_pGDIPlusRenderer := TWRenderer_GDIPlus.Create;

    Result := m_pGDIPlusRenderer;
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.SetRendererType(rendererType: IEType);
begin
    m_RendererType := rendererType;
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetDrawTextFunc: EDrawTextFunc;
begin
    // build GDI renderer if still not exists
    if (not Assigned(m_pGDIRenderer)) then
        m_pGDIRenderer := TWRenderer_GDI.Create;

    Result := m_pGDIRenderer.DrawTextFunc;
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.SetDrawTextFunc(drawTextFunc: EDrawTextFunc);
begin
    // build GDI renderer if still not exists
    if (not Assigned(m_pGDIRenderer)) then
        m_pGDIRenderer := TWRenderer_GDI.Create;

    m_pGDIRenderer.DrawTextFunc := drawTextFunc;
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.BeginScene(hDC: THandle);
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.BeginScene(hDC);
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.EndScene;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.EndScene;
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.DrawRect(const rect: TWRectF; pOptions: TWRenderer.IRectOptions;
        hDC: THandle; out iRect: TRect): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(False);

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            Result := m_pActiveRenderer.DrawRect(rect, pOptions, hDC, iRect);
            EndScene;
            Exit;
        end;
    {$endif}

    Result := m_pActiveRenderer.DrawRect(rect, pOptions, hDC, iRect);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.DrawPolygon(const rect: TWRectF; const pointList: TWRenderer.IPointList;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(False);

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            Result := m_pActiveRenderer.DrawPolygon(rect, pointList, pOptions, hDC);
            EndScene;
            Exit;
        end;
    {$endif}

    Result := m_pActiveRenderer.DrawPolygon(rect, pointList, pOptions, hDC);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.DrawPath(const rect: TWRectF; const path: TWPathCmds;
        const pOptions: TWRenderer.IShapeOptions; hDC: THandle): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(False);

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            Result := m_pActiveRenderer.DrawPath(rect, path, pOptions, hDC);
            EndScene;
            Exit;
        end;
    {$endif}

    Result := m_pActiveRenderer.DrawPath(rect, path, pOptions, hDC);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetTextSize(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle; pCharacters: PInteger = nil;
        pLines: PInteger = nil): TWSizeF;
begin
    // select renderer to use
    if (not SelectRenderer(pOptions, nil)) then
        Exit(Default(TWSizeF));

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            Result := m_pActiveRenderer.GetTextSize(text, rect, pOptions, hDC, pCharacters, pLines);
            EndScene;
            Exit;
        end;
    {$endif}

    Result := m_pActiveRenderer.GetTextSize(text, rect, pOptions, hDC, pCharacters, pLines);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetFontSizeToFit(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; minWrapFontSize, minFontSize: Cardinal;
        hDC: THandle; out fontSize: Cardinal): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(pOptions, nil)) then
        Exit(False);

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            Result := m_pActiveRenderer.GetFontSizeToFit(text, rect, pOptions, minWrapFontSize,
                    minFontSize, hDC, fontSize);
            EndScene;
            Exit;
        end;
    {$endif}

    Result := m_pActiveRenderer.GetFontSizeToFit(text, rect, pOptions, minWrapFontSize, minFontSize,
            hDC, fontSize);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.DrawText(const text: UnicodeString; const rect: TWRectF;
        const pOptions: TWRenderer.ITextOptions; hDC: THandle): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(pOptions, nil)) then
        Exit(False);

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);

            if (pOptions.Layered) then
                Result := m_pActiveRenderer.DrawLayeredText(text, rect, pOptions, hDC)
            else
                Result := m_pActiveRenderer.DrawText(text, rect, pOptions, hDC);

            EndScene;
            Exit;
        end;
    {$endif}

    if (pOptions.Layered) then
        Exit(m_pActiveRenderer.DrawLayeredText(text, rect, pOptions, hDC));

    Result := m_pActiveRenderer.DrawText(text, rect, pOptions, hDC);
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.DrawImage(pGraphic: TGraphic; const srcRect: TWRectF; hDC: THandle;
        const destRect: TWRectF; pOptions: TWRenderer.IImageOptions);
begin
    // is image vectorial? (e.g. SVG are vectorial images)
    pOptions.Vectorial := (AnsiPos('SVG', pGraphic.ClassName) <> 0);

    // select renderer to use
    if (not SelectRenderer(nil, pOptions)) then
        Exit;

    {$ifdef WTCONTROLS_USE_DIRECT2D}
        if (m_CanBuildScene) then
        begin
            // todo FIXME -cImprovement -oJean: ugly workaround for BeginScene/EndScene, call to
            //                                  these functions should be moved to appropriate
            //                                  location
            BeginScene(hDC);
            m_pActiveRenderer->DrawImage(pGraphic, srcRect, hDC, destRect, options);
            EndScene;
            Exit;
        end;
    {$endif}

    m_pActiveRenderer.DrawImage(pGraphic, srcRect, hDC, destRect, pOptions);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.AddFontToSession(const name, fileName: UnicodeString): Boolean;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(False);

    Result := m_pActiveRenderer.AddFontToSession(name, fileName);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.AddFont(const name, fileName: UnicodeString): THandle;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(0);

    Result := m_pActiveRenderer.AddFont(name, fileName);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.AddFont(const name: UnicodeString; pStream: TStream; length: Cardinal): THandle;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(0);

    Result := m_pActiveRenderer.AddFont(name, pStream, length);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetFont(const name: UnicodeString): THandle;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit(0);

    Result := m_pActiveRenderer.GetFont(name);
end;
//---------------------------------------------------------------------------
class function TWControlRenderer.GetFontName(const fileName: UnicodeString): UnicodeString;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit('');

    Result := m_pActiveRenderer.GetFontName(fileName);
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.RemoveFontFromSession(const name: UnicodeString);
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.RemoveFontFromSession(name);
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.RemoveFontsFromSession;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.RemoveFontsFromSession;
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.RemoveFont(const name: UnicodeString);
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.RemoveFont(name);
end;
//---------------------------------------------------------------------------
class procedure TWControlRenderer.RemoveFonts;
begin
    // select renderer to use
    if (not SelectRenderer(nil, nil)) then
        Exit;

    m_pActiveRenderer.RemoveFonts;
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
begin
    TWControlRenderer.m_RendererType          := TWControlRenderer.IEType.IE_RT_Auto;
    TWControlRenderer.m_CanBuildScene         := False;
    TWControlRenderer.m_pActiveRenderer       := nil;
    TWControlRenderer.m_pGDIRenderer          := nil;
    TWControlRenderer.m_pGDIPlusRenderer      := nil;
    {$ifdef WTCONTROLS_USE_DIRECT2D}
        TWControlRenderer.m_pDirect2DRenderer := nil;
    {$endif}
end;
//---------------------------------------------------------------------------

finalization
//---------------------------------------------------------------------------
begin
    TWControlRenderer.m_pGDIRenderer.Free;
    TWControlRenderer.m_pGDIPlusRenderer.Free;
    {$ifdef WTCONTROLS_USE_DIRECT2D}
        TWControlRenderer.m_pDirect2DRenderer.Free;
    {$endif}
end;
//---------------------------------------------------------------------------

end.
