{**
 @abstract(@name provides graphic class for the Scalable Vector Graphics (SVG) images.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGGraphic;

interface
    //{$define WTCONTROLS_LOG}
    //{$define ENABLE_SVG_GRAPHIC_LOGGING}

uses System.Classes,
     System.SysUtils,
     System.StrUtils,
     System.Math,
     Vcl.Graphics,
     Vcl.Imaging.jpeg,
     Vcl.Imaging.PngImage,
     Vcl.Clipbrd,
     Winapi.Windows,
     UTWMajorSettings,
     UTWColor,
     UTWHelpers,
     UTWSmartPointer,
     UTWDesignPatterns,
     UTWSVGAnimationDescriptor,
     UTWAnimationTimer,
     UTWSVGFrameCalculator,
     UTWControlRenderer,
     UTWSVG,
     UTWSVGRasterizer,
     UTWSVGGDIPlusRasterizer;

const
    //---------------------------------------------------------------------------
    // Global constants
    //---------------------------------------------------------------------------
    C_TWSVGGraphic_Default_Proportional  = False;
    C_TWSVGGraphic_Default_Antialiasing  = True;
    C_TWSVGGraphic_Default_Animate       = False;
    C_TWSVGGraphic_Default_FramePosition = 0.0;
    //---------------------------------------------------------------------------

type
    {**
     Class to register SVG graphic file filter (e.g. used by graphic file dialog box in design time)
    }
    TWSVGGraphicFilter = class
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
             Register svg class and filters
            }
            procedure Register; virtual;

            {**
             Unregister svg class and filters
            }
            procedure Unregister; virtual;

            {**
             Check if svg class and filters are registered
             @returns(@true if registered, otherwise @false)
            }
            function Registered: Boolean; virtual;
    end;

    {**
     Graphic based on SVG standard
    }
    TWSVGGraphic = class(TGraphic, IWObserver)
        public type
            {**
             Called while animation is running
             @param(pSender Event sender)
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            ITfSVGAnimateEvent = function (pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
                    pCustomData: Pointer): Boolean of object;

        private
            m_pSVG:                     TWSVG;
            m_pSVGRasterizer:           TWSVGGDIPlusRasterizer;
            m_pFrameCalculator:         TWSVGFrameCalculator;
            m_hClipboardFormat:         THandle;
            m_hInkscapeClipboardFormat: THandle;
            m_Data:                     UnicodeString;
            m_Width:                    Integer;
            m_Height:                   Integer;
            m_FramePos:                 Double;
            m_AnimSpeed:                Double;
            m_PerformedAnimLoops:       Cardinal;
            m_AnimLoopCount:            Cardinal;
            m_AnimLoop:                 Boolean;
            m_Animate:                  Boolean;
            m_Proportional:             Boolean;
            m_Antialiasing:             Boolean;
            m_FramePosChanging:         Boolean;
            m_ForceOriginalSave:        Boolean;
            m_Opened:                   Boolean;
            m_OnError:                  Boolean;
            m_pCustomData:              Pointer;
            m_fOnAnimate:               ITfSVGAnimateEvent;
            m_fOnAnimationBegin:        TNotifyEvent;
            m_fOnAnimationEnd:          TNotifyEvent;
            m_fOnAnimationLoop:         TNotifyEvent;

            {**
             Get the library version
             @returns(Library version, #ERROR on error)
            }
            function GetVersion: UnicodeString;

            {*
             Write raw data to string
             @param(data Data to write to stream)
             @param(pStream Stream to write to)
            }
            procedure WriteRawDataToStream(data: UnicodeString; pStream: TStream);

        protected
            {**
             Draw svg
             @param(pCanvas Canvas to draw on)
             @param(rect Draw rect)
            }
            procedure Draw(pCanvas: TCanvas; const rect: TRect); override;

            {**
             Get SVG rasterizer
             @returns(SVG rasterizer)
            }
            function GetRasterizer: TWSVGRasterizer; virtual;

            {**
             Get if svg is empty
             @returns(@true if svg is empty, otherwise @false)
            }
            function GetEmpty: Boolean; override;

            {**
             Get image width
             @returns(Image width)
            }
            function GetWidth: Integer; override;

            {**
             Get image height
             @returns(Image height)
            }
            function GetHeight: Integer; override;

            {**
             Set image width
             @param(value Image width)
            }
            procedure SetWidth(value: Integer); override;

            {**
             Set image height
             @param(value Image height)
            }
            procedure SetHeight(value: Integer); override;

            {**
             Set frame position
             @param(position Frame position in percent (between 0.0 and 1.0))
            }
            procedure SetFramePos(position: Double); virtual;

            {**
             Set animation speed
             @param(speed Animation speed (value < 1.0 decreases the speed and > 1.0 increases it))
            }
            procedure SetAnimSpeed(speed: Double); virtual;

            {**
             Get animation duration
             @returns(Animation duration in milliseconds)
            }
            function GetAnimDuration: NativeUInt; virtual;

            {**
             Set animation loop
             @param(value If @true, the animation will loop while end is reached)
            }
            procedure SetAnimLoop(value: Boolean); virtual;

            {**
             Set animation loop count
             @param(value Number of times the animation should loop, 0 means infinie)
            }
            procedure SetAnimLoopCount(value: Cardinal); virtual;

            {**
             Get the frame count
             @returns(The frame count)
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used
                             to determine how many frames, in an ideal situation, should be rendered
                             by seconds, and thus allows to calculate the time interval between each
                             frames. Instead, the FPS represents the number of frames per seconds
                             a system can effectively process
            }
            function GetFrameCount: Cardinal; virtual;

            {**
             Set the frame count
             @param(count Frame count)
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used
                             to determine how many frames, in an ideal situation, should be rendered
                             by seconds, and thus allows to calculate the time interval between each
                             frames. Instead, the FPS represents the number of frames per seconds
                             a system can effectively process
            }
            procedure SetFrameCount(count: Cardinal); virtual;

            {**
             Set animate state
             @param(value If @true, animation is enabled, disabled otherwise)
            }
            procedure SetAnimate(value: Boolean); virtual;

            {**
             Called while animation is running
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            function DoAnimate(pAnimDesc: TWSVGAnimationDescriptor; pCustomData: Pointer): Boolean; virtual;

            {**
             Called while animation is running
             @param(pSender Event sender)
             @param(pStream Stream containing the image to read)
             @param(imageType The image type)
             @param(pGraphic The read image graphic)
             @returns(@true on success, otherwise @false)
            }
            function DoGetImage(pSender: TObject; pStream: TMemoryStream; imageType: TWSVGRasterizer.IEImageType;
                    var pGraphic: TGraphic): Boolean; virtual;

            {**
             Define properties to load or save to DFM
             @param(pFiler DFM stream)
            }
            procedure DefineProperties(pFiler: TFiler); override;

            {**
             Run animation
            }
            procedure RunAnimation; virtual;

            {**
             Calculate the next animation frame
            }
            procedure CalculateNextFrame; virtual;

            {**
             Called when next animation frame should be processed
            }
            procedure OnProcessAnimation; virtual;

            {**
             Called when subject send a notification to the observer
             @param(message Notification message)
            }
            procedure OnNotified(message: TWMessage); virtual;

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
             Clear all data
            }
            procedure Clear; virtual;

            {**
             Copy object content
             @param(pOther Other object to copy from)
            }
            procedure Assign(pOther: TPersistent); override;

            {**
             Load svg from string
             @param(str String to load from)
            }
            procedure LoadFromStr(str: UnicodeString); virtual;

            {**
             Load svg from stream
             @param(pStream Stream to load from)
            }
            procedure LoadFromStream(pStream: TStream); override;

            {**
             Save svg to stream
             @param(pStream Stream to save to)
            }
            procedure SaveToStream(pStream: TStream); override;

            {**
             Get the SVG clipboard format (and register it if still not done)
             @returns(SVG clipboard format)
            }
            function GetClipboardFormat: Thandle; virtual;

            {**
             Get the SVG clipboard format used by Inkscape (and register it if still not done)
             @returns(SVG clipboard format)
            }
            function GetInkscapeClipboardFormat: Thandle; virtual;

            {**
             Load svg from clipboard
             @param(format Format that identifies the SVG in the clipboard)
             @param(data Handle refering to the received SVG data from clipboard)
             @param(hPalette Image palette. With SVG the palette is ignored and should always be equal to 0)
            }
            procedure LoadFromClipboardFormat(format: Word; data: THandle; hPalette: HPALETTE); override;

            {**
             Prepare SVG to be saved to clipboard
             @param(format @bold([in, out]) Format that will identify the SVG in the clipboard)
             @param(data @bold([in, out]) Handle refering to the SVG data to save to clipboard)
             @param(hPalette @bold([in, out]) Image palette. With SVG the palette is ignored and always equals to 0)
            }
            procedure SaveToClipboardFormat(var format: Word; var data: THandle; var hPalette: HPALETTE); override;

            {**
             Copy the SVG content to the clipboard, in a such manner that the largest amount
             possible of apps may be able to read the clipboard content
            }
            procedure CopyToClipboard; virtual;

            {**
             Restart animation
            }
            procedure RestartAnimation; virtual;

            {**
             Set custom data
             @param(pCustomData Custom data)
            }
            procedure SetCustomData(pCustomData: Pointer); virtual;

        public
            {**
             Get the library version number
            }
            property Version: UnicodeString read GetVersion;

            {**
             Get the native SVG object
            }
            property Native: TWSVG read m_pSVG;

            {**
             Get the rasterizer used to draw the SVG
            }
            property Rasterizer: TWSVGRasterizer read GetRasterizer;

            {**
             Get the SVG data
            }
            property Data: UnicodeString read m_Data;

            {**
             Get or set if the SVG is animated
            }
            property Animate: Boolean read m_Animate write SetAnimate default C_TWSVGGraphic_Default_Animate;

            {**
             Get or set the animation speed (value < 1.0 decreases speed and > 1.0 increases it)
            }
            property AnimationSpeed: Double read m_AnimSpeed write SetAnimSpeed;

            {**
             Get the animation duration in milliseconds
            }
            property AnimationDuration: NativeUInt read GetAnimDuration;

            {**
             Get or set if the animation loops while end is reached
            }
            property Loop: Boolean read m_AnimLoop write SetAnimLoop;

            {**
             Get or set the animation loop count, 0 means infinie
            }
            property LoopCount: Cardinal read m_AnimLoopCount write SetAnimLoopCount;

            {**
             Get or set the number of frame to render per seconds
             @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count is used
                             to determine how many frames, in an ideal situation, should be rendered
                             by seconds, and thus allows to calculate the time interval between each
                             frames. Instead, the FPS represents the number of frames per seconds
                             a system can effectively process
            }
            property FrameCount: Cardinal read GetFrameCount write SetFrameCount nodefault;

            {**
             Get or set the frame position, in percent (between 0.0 and 1.0)
            }
            property Position: Double read m_FramePos write SetFramePos;

            {**
             Get or set if image is proportional
            }
            property Proportional: Boolean read m_Proportional write m_Proportional default C_TWSVGGraphic_Default_Proportional;

            {**
             Get or set if antialiasing is used
            }
            property Antialiasing: Boolean read m_Antialiasing write m_Antialiasing default C_TWSVGGraphic_Default_Antialiasing;

            {**
             Get the clipboard format to use for SVG graphics
            }
            property ClipboardFormat: Thandle read GetClipboardFormat;

            {**
             Get the clipboard format used by Inkscape for SVG graphics
            }
            property InkscapeClipboardFormat: Thandle read GetInkscapeClipboardFormat;

            {**
             Get or set if the original SaveToFile function from the TGraphic class should be used
            }
            property ForceOriginalSave: Boolean read m_ForceOriginalSave write m_ForceOriginalSave;

            {**
             Get or set the OnAnimate callback
            }
            property OnAnimate: ITfSVGAnimateEvent read m_fOnAnimate write m_fOnAnimate;

            {**
             Get or set the OnAnimationBegin callback
            }
            property OnAnimationBegin: TNotifyEvent read m_fOnAnimationBegin write m_fOnAnimationBegin;

            {**
             Get or set the OnAnimationEnd callback
            }
            property OnAnimationEnd: TNotifyEvent read m_fOnAnimationEnd write m_fOnAnimationEnd;

            {**
             Get or set the OnAnimationLoop callback
            }
            property OnAnimationLoop: TNotifyEvent read m_fOnAnimationLoop write m_fOnAnimationLoop;
    end;

    {**
     List of SVG graphics that will compose the image list
    }
    TWSVGGraphics = array of TWSVGGraphic;

    {**
     SVG graphic class
    }
    TWSVGGraphicClass = class of TWSVGGraphic;

var
    g_pSVGGraphicFilter: TWSVGGraphicFilter;

implementation

uses
  System.UITypes;

//---------------------------------------------------------------------------
// TWSVGGraphicFilter
//---------------------------------------------------------------------------
constructor TWSVGGraphicFilter.Create;
begin
    inherited Create;

    // register svg graphic filter(s)
    Register;
end;
//---------------------------------------------------------------------------
destructor TWSVGGraphicFilter.Destroy;
begin
    // unregister svg graphic filter(s)
    Unregister;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphicFilter.Register;
begin
    // class already registered?
    if (Registered) then
        Exit;

    // register svg file format
    Vcl.Graphics.TPicture.RegisterFileFormat('svg', 'Scalable Vector Graphics', TWSVGGraphic);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphicFilter.Unregister;
begin
    // class already unregistered?
    if (not Registered) then
        Exit;

    // unregister graphic filter
    Vcl.Graphics.TPicture.UnregisterGraphicClass(TWSVGGraphic);
end;
//---------------------------------------------------------------------------
function TWSVGGraphicFilter.Registered: Boolean;
var
    fileMask: UnicodeString;
begin
    // get registered file mask
    fileMask := GraphicFileMask(TWSVGGraphic);

    // found it and is valid?
    Result := ((Length(fileMask) > 0) and ContainsText(fileMask, 'svg'));
end;
//---------------------------------------------------------------------------
// TWSVGGraphic
//---------------------------------------------------------------------------
constructor TWSVGGraphic.Create;
begin
    inherited Create;

    m_hClipboardFormat         := 0;
    m_hInkscapeClipboardFormat := 0;
    m_Width                    := 0;
    m_Height                   := 0;
    m_FramePos                 := C_TWSVGGraphic_Default_FramePosition;
    m_AnimSpeed                := 1.0;
    m_PerformedAnimLoops       := 0;
    m_AnimLoopCount            := 0;
    m_AnimLoop                 := True;
    m_Animate                  := C_TWSVGGraphic_Default_Animate;
    m_Proportional             := C_TWSVGGraphic_Default_Proportional;
    m_Antialiasing             := C_TWSVGGraphic_Default_Antialiasing;
    m_FramePosChanging         := False;
    m_ForceOriginalSave        := False;
    m_Opened                   := False;
    m_OnError                  := False;
    m_pSVG                     := nil;
    m_pCustomData              := nil;
    m_fOnAnimate               := nil;
    m_fOnAnimationBegin        := nil;
    m_fOnAnimationEnd          := nil;
    m_fOnAnimationLoop         := nil;

    // create internal SVG object
    m_pSVG             := TWSVG.Create;
    m_pSVGRasterizer   := TWSVGGDIPlusRasterizer.Create(TWControlRenderer.GetGDIPlusToken);
    m_pFrameCalculator := TWSVGFrameCalculator.Create;

    // link internal callbacks
    m_pSVGRasterizer.OnAnimate  := DoAnimate;
    m_pSVGRasterizer.OnGetImage := DoGetImage;

    // set background transparent by default
    Transparent := True;

    // enable the animation support in the rasterizer for the entire graphic lifecycle. This is
    // required to allow the graphic to work in a "marquee mode", where a frame position may be
    // selected manually
    m_pSVGRasterizer.EnableAnimation(True);

    // attach to animation timer to receive time notifications
    TWAnimationTimer.GetInstance.Attach(Self);
end;
//---------------------------------------------------------------------------
destructor TWSVGGraphic.Destroy;
begin
    // unlink internal callbacks
    m_pSVGRasterizer.OnAnimate := nil;

    // detach from animation timer and stop to receive time notifications
    TWAnimationTimer.GetInstance.Detach(Self);

    FreeAndNil(m_pFrameCalculator);
    FreeAndNil(m_pSVGRasterizer);
    FreeAndNil(m_pSVG);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetVersion: UnicodeString;
begin
    if (not Assigned(TWLibraryVersion)) then
        Exit('#ERROR');

    Result := TWLibraryVersion.ToStr;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.WriteRawDataToStream(data: UnicodeString; pStream: TStream);
var
    pStrWriter: TStreamWriter;
begin
    pStrWriter := nil;

    try
        // write data to stream
        pStrWriter := TStreamWriter.Create(pStream);
        pStrWriter.Write(data);
    finally
        pStrWriter.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.Draw(pCanvas: TCanvas; const rect: TRect);
var
    pageColor, borderColor: TWColor;
    borderOpacity:          Single;
    animation:              TWSVGRasterizer.IAnimation;
begin
    try
        // no canvas?
        if (not Assigned(pCanvas)) then
            Exit;

        // is svg file or stream opened?
        if (not m_Opened) then
            Exit;

        // is svg on error?
        if (m_OnError) then
            Exit;

        // is background transparent?
        if (not Transparent) then
        begin
            // get page style
            if (m_pSVGRasterizer.GetPageStyle(m_pSVG, pageColor, borderColor, borderOpacity)) then
            begin
                // is page color empty?
                if (pageColor.IsEmpty) then
                    // by default, set page color to white
                    pageColor := TWColor.Create(clWhite);
            end
            else
                // by default, set page color to white
                pageColor := TWColor.Create(clWhite);

            // fill background
            pCanvas.Brush.Color := pageColor.GetColor;
            pCanvas.FillRect(rect);
        end;

        // populate animation structure
        animation.m_Position    := m_FramePos;
        animation.m_pCustomData := m_pCustomData;

        // draw svg to canvas
        m_pSVGRasterizer.Draw(m_pSVG, rect, m_Proportional, m_Antialiasing, animation, pCanvas);
    except
        // catch exception
        on e: Exception do
        begin
            // as the draw function may be called inside a message loop, don't try to draw again if
            // an error occurred. Instead put the svg in panic mode
            m_OnError := True;

            TWLogHelper.LogToCompiler(e.Message);
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetRasterizer: TWSVGRasterizer;
begin
    Result := m_pSVGRasterizer;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetEmpty: Boolean;
begin
    Result := ((not m_Opened) or (m_Width = 0) or (m_Height = 0));
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetWidth: Integer;
begin
    Result := m_Width;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetHeight: Integer;
begin
    Result := m_Height;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetWidth(value: Integer);
begin
    m_Width := value;

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetHeight(value: Integer);
begin
    m_Height := value;

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetFramePos(position: Double);
begin
    // sometimes frame position may be updated while Changed() function is called. This prevent an
    // infinite callback loop in this case
    if (m_FramePosChanging) then
        Exit;

    // NOTE don't limit to 1.0, because 1.0 needs to be equal to 0.0 in the animation cycle. This is
    // required, otherwise the loop cycle would become chaotic or may jump while the end is reached
    m_FramePos := TWMathHelper.Clamp(position, 0.0, 0.999999);

    // invalidate the owning control (if any) if animation isn't enabled
    if (not m_Animate) then
        try
            m_FramePosChanging := True;
            Changed(Self);
        finally
            m_FramePosChanging := False;
        end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetAnimSpeed(speed: Double);
begin
    m_AnimSpeed := speed;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetAnimDuration: NativeUInt;
begin
    Result := m_pSVGRasterizer.GetAnimationDuration(m_pSVG);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetAnimLoop(value: Boolean);
begin
    m_AnimLoop := value;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetAnimLoopCount(value: Cardinal);
begin
    m_AnimLoopCount := value;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetFrameCount: Cardinal;
begin
    Result := Round(m_pFrameCalculator.FrameCount);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetFrameCount(count: Cardinal);
begin
    m_pFrameCalculator.FrameCount := count;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetAnimate(value: Boolean);
begin
    // nothing to change?
    if (m_Animate = value) then
        Exit;

    m_Animate := value;

    // run the animation, if needed
    if (m_Animate) then
    begin
        RunAnimation;
        Exit;
    end;

    // invalidate the owning control (if any)
    Changed(Self);
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.DoAnimate(pAnimDesc: TWSVGAnimationDescriptor; pCustomData: Pointer): Boolean;
begin
    // ask user about continuing animation
    if (Assigned(m_fOnAnimate)) then
        Exit(m_fOnAnimate(Self, pAnimDesc, pCustomData));

    // notify that animation is allowed to continue
    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.DoGetImage(pSender: TObject; pStream: TMemoryStream; imageType: TWSVGRasterizer.IEImageType;
        var pGraphic: TGraphic): Boolean;
begin
    if ((not Assigned(pStream)) or (pStream.Size = 0)) then
        Exit(False);

    pStream.Position := 0;

    case (imageType) of
        TWSVGRasterizer.IEImageType.IE_IT_JPG:
        begin
            pGraphic := TJpegImage.Create;
            pGraphic.LoadFromStream(pStream);
            Exit(True);
        end;

        TWSVGRasterizer.IEImageType.IE_IT_PNG:
        begin
            pGraphic := TPngImage.Create;
            pGraphic.LoadFromStream(pStream);
            Exit(True);
        end;

        TWSVGRasterizer.IEImageType.IE_IT_SVG:
        begin
            pGraphic := TWSVGGraphic.Create;
            pGraphic.LoadFromStream(pStream);
            Exit(True);
        end;
    else
        Exit(False);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.DefineProperties(pFiler: TFiler);
begin
    // no filer?
    if (not Assigned(pFiler)) then
        Exit;

    // add a new property in the DFM parser that will contain the SVG data. Register also the
    // functions able to read from and save to this DFM property
    pFiler.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.RunAnimation;
var
    duration: Double;
begin
    // can animate this SVG?
    if (not m_Animate) then
        Exit;

    // get animation duration
    duration := AnimationDuration;

    // may svg be animated?
    if (duration > 0.0) then
    begin
        // configure animation duration
        m_pFrameCalculator.SetDuration(duration, 100);

        // start frame animation
        m_pFrameCalculator.StartTimer;

        // notify that animation begins
        if (Assigned(m_fOnAnimationBegin)) then
            m_fOnAnimationBegin(Self);
    end
    else
    begin
        m_Animate := False;

        // animation aborted, notify that animation ends
        if (Assigned(m_fOnAnimationEnd)) then
            m_fOnAnimationEnd(Self);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.CalculateNextFrame;
var
    info:          TWSVGFrameCalculator.IInfo;
    newFramePos:   Double;
    framesToAdd:   Double;
    animPerformed: Cardinal;
begin
    // get info required to calculate animation
    m_pFrameCalculator.GetInfo(info);

    framesToAdd := info.m_FrameCountSinceLastPaint;

    // calculate the next frame position
    newFramePos   := m_FramePos + (framesToAdd * 0.01 * m_AnimSpeed);
    animPerformed := 0;

    // remove the exceeding frame position and count the number of times the animation ended
    while (newFramePos >= 1.0) do
    begin
        newFramePos := newFramePos - 1.0;
        Inc(animPerformed);
    end;

    // count the number of times the animation ended
    Inc(m_PerformedAnimLoops, animPerformed);

    // do loop the animation?
    if (m_AnimLoop) then
    begin
        // if yes, check if number of loops is limited and if performed loop count doesn't exceeded the limit
        if ((m_AnimLoopCount > 0) and (m_PerformedAnimLoops >= m_AnimLoopCount)) then
        begin
            // number of allowed loops exceeded, stop the animation on the last frame
            m_Animate   := False;
            newFramePos := 1.0;

            // notify that animation ends
            if (Assigned(m_fOnAnimationEnd)) then
                m_fOnAnimationEnd(Self);
        end
        else
        // animation looped?
        if (animPerformed > 0) then
        begin
            // notify that animation looped
            if (Assigned(m_fOnAnimationLoop)) then
                m_fOnAnimationLoop(Self);
        end;
    end
    else
    // animation doesn't loop, check if the end was reached
    if (animPerformed > 0) then
    begin
        // animation end reached, stop the animation on the last frame
        m_Animate   := False;
        newFramePos := 1.0;

        // notify that animation ends
        if (Assigned(m_fOnAnimationEnd)) then
            m_fOnAnimationEnd(Self);
    end;

    SetFramePos(newFramePos);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.OnProcessAnimation;
begin
    CalculateNextFrame;

    // calling the Changed() function force any component owning this graphic, like e.g. a TImage,
    // to invaliate itself. So the animation may be processed in this case without having to keep
    // a pointer on a such component
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.OnNotified(message: TWMessage);
begin
    case (TWAnimationTimer.EWAnimationTimerMessages(message.m_Type)) of
        TWAnimationTimer.EWAnimationTimerMessages.IE_AM_Animate:
        begin
            if (not m_Animate) then
                Exit;

            OnProcessAnimation;
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.Clear;
begin
    m_Width              := 0;
    m_Height             := 0;
    m_FramePos           := C_TWSVGGraphic_Default_FramePosition;
    m_AnimSpeed          := 1.0;
    m_PerformedAnimLoops := 0;
    m_AnimLoopCount      := 0;
    m_AnimLoop           := True;
    m_Animate            := C_TWSVGGraphic_Default_Animate;
    m_Proportional       := C_TWSVGGraphic_Default_Proportional;
    m_Antialiasing       := C_TWSVGGraphic_Default_Antialiasing;
    m_ForceOriginalSave  := False;
    m_Opened             := False;
    m_OnError            := False;
    m_pCustomData        := nil;
    m_Data               := '';

    m_pSVG.Parser.Clear;

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.Assign(pOther: TPersistent);
var
    pSource: TWSVGGraphic;
begin
    // found it?
    if (not Assigned(pOther) or not(pOther is TWSVGGraphic)) then
    begin
        // clear previous svg data
        Clear;
        Exit;
    end;

    // get source object to copy from
    pSource := pOther as TWSVGGraphic;

    // copy data from source
    m_Data              := pSource.m_Data;
    m_Width             := pSource.m_Width;
    m_Height            := pSource.m_Height;
    m_FramePos          := pSource.m_FramePos;
    m_AnimSpeed         := pSource.m_AnimSpeed;
    m_AnimLoopCount     := pSource.m_AnimLoopCount;
    m_AnimLoop          := pSource.m_AnimLoop;
    m_Animate           := pSource.m_Animate;
    m_Proportional      := pSource.m_Proportional;
    m_Antialiasing      := pSource.m_Antialiasing;
    m_ForceOriginalSave := pSource.m_ForceOriginalSave;
    m_Opened            := pSource.m_Opened;
    m_OnError           := pSource.m_OnError;
    m_pSVG.Assign(pSource.m_pSVG);

    m_pSVGRasterizer.EnableAnimation(pSource.m_pSVGRasterizer.IsAnimationEnabled);

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.LoadFromStr(str: UnicodeString);
var
    pStrStream: TStringStream;
begin
    pStrStream := TStringStream.Create;

    try
        // load SVG data in a string stream
        pStrStream.WriteString(str);
        pStrStream.Position := 0;

        LoadFromStream(pStrStream);
    finally
        pStrStream.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.LoadFromStream(pStream: TStream);
var
    svgSize:    TSize;
    pStrStream: TStringStream;
begin
    // clear previous svg data
    Clear;

    // no stream?
    if (not Assigned(pStream)) then
        Exit;

    // stream is empty?
    if (pStream.Size = 0) then
        Exit;

    pStrStream := nil;

    try
        // convert raw stream to string stream
        pStrStream := TStringStream.Create;
        pStrStream.CopyFrom(pStream, 0);

        // keep the XML data, this is required to save back the SVG content, or to copy to clipboard
        m_Data := pStrStream.DataString;
    finally
        pStrStream.Free;
    end;

    pStream.Position := 0;

    // load svg from data buffer
    if (not m_pSVG.LoadFromStream(pStream)) then
    begin
        TWLogHelper.LogToCompiler('Load SVG from stream - FAILED');
        Exit;
    end;

    // get SVG size
    svgSize := m_pSVGRasterizer.GetSize(m_pSVG);

    // update image width, if needed
    if (m_Width = 0) then
        m_Width := svgSize.Width;

    // update image height, if needed
    if (m_Height = 0) then
        m_Height := svgSize.Height;

    m_Opened  := True;
    m_OnError := False;

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SaveToStream(pStream: TStream);
var
    {$if defined (WTCONTROLS_LOG) and defined (ENABLE_SVG_GRAPHIC_LOGGING)}
        pStrStream: TStringStream;
        content:    UnicodeString;
    {$ifend}
    encodingName, data: UnicodeString;
    encoding:           TEncoding;
    buffer, preamble:   TBytes;
    c:                  WideChar;
    i:                  Integer;
    writeBOM:           Boolean;
begin
    // no stream?
    if (not Assigned(pStream)) then
        Exit;

    // no data to write?
    if (Length(m_Data) = 0) then
        Exit;

    {$if defined (WTCONTROLS_LOG) and defined (ENABLE_SVG_GRAPHIC_LOGGING)}
        pStrStream := nil;

        try
            // convert raw stream to string stream
            pStrStream := TStringStream.Create;
            pStrStream.CopyFrom(pStream, 0);

            // get stream content
            content := pStrStream.DataString;
        finally
            pStrStream.Free;
        end;

        TWLogHelper.LogToCompiler('WTSVGGraphic - save - stream content - ' + content
                + ' - buffer content - ' + m_Data);
    {$ifend}

    i := 0;

    // skip all the existing TWSVGGraphic prefixes and find the real SVG data starting pos
    for c in m_Data do
    begin
        if (c = '<') then
            break;

        Inc(i);
    end;

    // convert encoding to lower case
    {$if CompilerVersion <= 24}
        encodingName := WideLowerCase(m_pSvg.Encoding);
    {$else}
        encodingName := m_pSvg.Encoding.ToLower;
    {$ifend}

    // get the SVG data without the TWSVGGraphic prefixes
    data := TWStringHelper.Substr(m_Data, i);

    // convert the string respecting the encoding
    if (m_ForceOriginalSave) then
    begin
        // force to original save system, write raw data
        WriteRawDataToStream(data, pStream);
        Exit;
    end
    else
    if (Length(encodingName) = 0) or (encodingName = 'us-ascii') or (encodingName = 'iso-8859-1') then
    begin
        // default or ASCII encoding, write raw data
        WriteRawDataToStream(data, pStream);
        Exit;
    end
    else
    if (encodingName = 'utf-8') then
    begin
        // utf8 encoding
        data     := UTF8ToString(AnsiString(data));
        encoding := TEncoding.UTF8;
        writeBOM := False;
    end
    else
    begin
        // unknown encoding, write raw data
        TWLogHelper.LogToCompiler('SaveToStream - unknown encoding - ' + encodingName);
        WriteRawDataToStream(data, pStream);
        Exit;
    end;

    // encode the string and get a buffer containing it
    buffer := encoding.GetBytes(data);

    // do write a BOM before?
    if writeBOM then
    begin
        // get BOM data
        preamble := encoding.GetPreamble;

        // write it
        if (Length(preamble) > 0) then
            {$if CompilerVersion <= 24}
                pStream.WriteBuffer(preamble[0], Length(preamble));
            {$else}
                pStream.WriteBuffer(preamble, Length(preamble));
            {$ifend}
    end;

    // write the string to the stream
    {$if CompilerVersion <= 24}
        pStream.WriteBuffer(buffer[0], Length(buffer));
    {$else}
        pStream.WriteBuffer(buffer, Length(buffer));
    {$ifend}
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetClipboardFormat: Thandle;
begin
    // already registered?
    if (m_hClipboardFormat <> 0) then
        Exit(m_hClipboardFormat);

    // this kind of format is well understanded by several SVG editor like Inkscape
    m_hClipboardFormat := RegisterClipboardFormat('image/svg+xml');
    Result             := m_hClipboardFormat;
end;
//---------------------------------------------------------------------------
function TWSVGGraphic.GetInkscapeClipboardFormat: Thandle;
begin
    // already registered?
    if (m_hInkscapeClipboardFormat <> 0) then
        Exit(m_hInkscapeClipboardFormat);

    // Inkscape writes his SVG data using this handle, and omit the 'image/svg+xml'
    m_hInkscapeClipboardFormat := RegisterClipboardFormat('image/x-inkscape-svg');
    Result                     := m_hInkscapeClipboardFormat;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.LoadFromClipboardFormat(format: Word; data: NativeUInt ; hPalette: HPALETTE);
var
    hFormat, hInkscapeFormat: THandle;
    dataSize:                 NativeUInt;
    pStrStream:               TStringStream;
    pMem:                     Pointer;
begin
    // get the possible SVG clipboard formats
    hFormat         := GetClipboardFormat;
    hInkscapeFormat := GetInkscapeClipboardFormat;

    // todo FIXME -cFeature -oJean: for now the Inkscape content is considered as identical as the
    //                              standard 'image/svg+xml'. However the Inkscape data contains
    //                              several important info like e.g. a clipboard structure, that are
    //                              ignored for now
    // validate the received image format and data
    if (((format <> hFormat) and (format <> hInkscapeFormat)) or (data = 0)) then
        raise EInvalidGraphic.Create('The clipboard is empty');

    dataSize   := GlobalSize(data);
    pStrStream := nil;

    try
        // load SVG xml data in a string stream
        pStrStream := TStringStream.Create;

        // get the source memory to copy from
        pMem := GlobalLock(data);

        if (not Assigned(pMem)) then
            Exit;

        try
            // copy the data inside the string stream
            pStrStream.Write(pMem^, dataSize);
        finally
            GlobalUnlock(data);
        end;

        pStrStream.Position := 0;

        // open the SVG
        LoadFromStream(pStrStream);

    finally
        pStrStream.Free;
    end;

    // notify that content has changed
    Changed(Self);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SaveToClipboardFormat(var format: Word; var data: THandle; var hPalette: HPALETTE);
var
    pMem:       Pointer;
    pStrStream: TStringStream;
begin
    format   := 0;
    data     := 0;
    hPalette := 0;

    // no data to export?
    if (Length(m_Data) = 0) then
        Exit;

    pStrStream := nil;

    try
        // load SVG xml data in a string stream
        pStrStream := TStringStream.Create;
        pStrStream.WriteString(m_Data);
        pStrStream.Position := 0;

        // get the SVG clipboard format
        format := GetClipboardFormat;

        if (format = 0) then
            Exit;

        // reserve memory for SVG xml data to copy
        data := GlobalAlloc(GMEM_MOVEABLE, pStrStream.Size);

        if (data = 0) then
            Exit;

        // get access to reserved memory
        pMem := GlobalLock(data);

        if (not Assigned(pMem)) then
            Exit;

        try
            // copy the SVG xml data in reserved memory
            pStrStream.Read(pMem^, pStrStream.Size);
        finally
            // free reserved memory access
            GlobalUnlock(data);
        end;
    finally
        pStrStream.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.CopyToClipboard;
var
    pBitmap:   IWSmartPointer<Vcl.Graphics.TBitmap>;
    pPng:      IWSmartPointer<TPngImage>;
    pStream:   IWSmartPointer<TMemoryStream>;
    animation: TWSVGRasterizer.IAnimation;
    imgFormat: Word;
    hPal:      HPALETTE;
    hData:     THandle;
begin
    // is SVG empty?
    if (m_pSVG.IsEmpty) then
        Exit;

    // create target bitmap
    pBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
    pBitmap.PixelFormat := pf32bit;
    pBitmap.AlphaFormat := afDefined;
    pBitmap.Width       := m_Width;
    pBitmap.Height      := m_Height;

    // clear target bitmap canvas
    TWGDIHelper.Clear(pBitmap);

    // populate animation structure
    animation.m_Position    := m_FramePos;
    animation.m_pCustomData := m_pCustomData;

    // draw SVG to target bitmap
    m_pSVGRasterizer.Draw(m_pSVG, Default(TPoint), 1.0, True, animation, pBitmap.Canvas);

    // save bitmap to clipboard. This way the transparency is lost, but several apps can only
    // process bitmaps, so...
    pBitmap.SaveToClipboardFormat(imgFormat, hData, hPal);
    Clipboard.SetAsHandle(imgFormat, hData);

    // now convert bitmap to PNG, and save the PNG to clipboard. Thus all apps that supports the
    // transparency will be able to benefit from it
    pPng    := TWSmartPointer<TPngImage>.Create(TWImageHelper.BmpToPng_GDI(pBitmap));
    pStream := TWSmartPointer<TMemoryStream>.Create();
    pPng.SaveToStream(pStream);
    pStream.Position := 0;
    TWClipboardHelper.StreamTo(TWClipboardHelper.GetPNGFormatHandle, pStream, pStream.Size);

    // also save the SVG XML data as text to the clipboard
    TWClipboardHelper.TextTo(m_Data);

    // and finally, save it as the SVG/XML format, thus SVG editors like Inkscape will be able to
    // deal with
    TWClipboardHelper.StringTo(GetClipboardFormat,         m_Data);
    TWClipboardHelper.StringTo(GetInkscapeClipboardFormat, m_Data);
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.RestartAnimation;
begin
    // already animated?
    if (m_Animate) then
        Exit;

    // reset the animation values
    m_FramePos           := C_TWSVGGraphic_Default_FramePosition;
    m_PerformedAnimLoops := 0;
    m_Animate            := True;

    RunAnimation;
end;
//---------------------------------------------------------------------------
procedure TWSVGGraphic.SetCustomData(pCustomData: Pointer);
begin
    m_pCustomData := pCustomData;
end;
//---------------------------------------------------------------------------

initialization
//---------------------------------------------------------------------------
// Global initialization procedure
//---------------------------------------------------------------------------
begin
    // register the SVG filter
    g_pSVGGraphicFilter := TWSVGGraphicFilter.Create;
end;
//---------------------------------------------------------------------------

finalization
//---------------------------------------------------------------------------
// Global finalization procedure
//---------------------------------------------------------------------------
begin
    // release the SVG filter
    g_pSVGGraphicFilter.Free;
end;
//---------------------------------------------------------------------------

end.
