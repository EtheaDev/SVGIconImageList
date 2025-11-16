{**
 @abstract(@name provides an overridden image that acts as a button and supports SVG graphics.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGImageButton;

interface

uses System.Classes,
     System.SysUtils,
     Vcl.Graphics,
     Vcl.Controls,
     Winapi.Messages,
     Winapi.Windows,
     Winapi.UxTheme,
     UTWSVGGraphic,
     UTWSVGAnimationDescriptor,
     UTWSVGImage,
     UTWSVGFrameCalculator;

type
    {**
     Image that acts as a button and supports animated SVG graphics
    }
    TWSVGImageButton = class(TWSVGImage)
        private
            m_pCanvas:                      TCanvas;
            m_HoveredImgGUID:               UnicodeString;
            m_ClickedImgGUID:               UnicodeString;
            m_DisabledImgGUID:              UnicodeString;
            m_pHoveredPicture:              TPicture;
            m_pClickedPicture:              TPicture;
            m_pDisabledPicture:             TPicture;
            m_pHoveredAnimationProps:       TWSVGImage.IAnimationProps;
            m_pClickedAnimationProps:       TWSVGImage.IAnimationProps;
            m_pDisabledAnimationProps:      TWSVGImage.IAnimationProps;
            m_Hovered:                      Boolean;
            m_Clicked:                      Boolean;
            m_fOnHoveredPictureAnimate:     TWSVGImage.ITfSVGAnimateEvent;
            m_fOnClickedPictureAnimate:     TWSVGImage.ITfSVGAnimateEvent;
            m_fOnDisabledPictureAnimate:    TWSVGImage.ITfSVGAnimateEvent;
            m_fPrevOnHoveredPictureChange:  TNotifyEvent;
            m_fPrevOnClickedPictureChange:  TNotifyEvent;
            m_fPrevOnDisabledPictureChange: TNotifyEvent;

            {**
             Windows paint message override
             @param(message @bold([in, out]) Windows message, may contains result on function ends)
            }
            procedure WMPaint(var message: TWMPaint); message WM_PAINT;

            {**
             Get the SVG matching with an animation properties set
             @param(pAnimProps Animation properties set)
             @returns(The matching SVG graphic, @nil if not found or on error)
            }
            function GetSVG(pAnimProps: TWSVGImage.IAnimationProps): TWSVGGraphic;

            {**
             Get the animation properties set matching with an SVG
             @param(pSVG The SVG graphic)
             @returns(The matching animation set, @nil if not found or on error)
            }
            function GetAnimationProps(pSVG: TWSVGGraphic): TWSVGImage.IAnimationProps;

        protected
            {**
             Set hovered picture
             @param(pPicture Picture to set)
            }
            procedure SetHoveredPicture(pPicture: TPicture); virtual;

            {**
             Set clicked picture
             @param(pPicture Picture to set)
            }
            procedure SetClickedPicture(pPicture: TPicture); virtual;

            {**
             Set disabled picture
             @param(pPicture Picture to set)
            }
            procedure SetDisabledPicture(pPicture: TPicture); virtual;

            {**
             Called when mouse is down
             @param(button Clicked mouse button)
             @param(shift Special shift keys state)
             @param(x Mouse x position on the client rect, in pixels)
             @param(y Mouse y position on the client rect, in pixels)
            }
            procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: Integer); override;

            {**
             Called when mouse is up
             @param(button Clicked mouse button)
             @param(shift Special shift keys state)
             @param(x Mouse x position on the client rect, in pixels)
             @param(y Mouse y position on the client rect, in pixels)
            }
            procedure MouseUp(button: TMouseButton; shift: TShiftState; x, y: Integer); override;

            {**
             Windows procedure
             @param(message @bold([in, out]) Windows message, may contains result on function ends)
            }
            procedure WndProc(var message: TMessage); override;

            {**
             Check if a picture is empty
             @param(pPicture Picture to check)
             @returns(@true if picture is empty, otherwise @false)
            }
            function IsEmpty(pPicture: TPicture): Boolean; virtual;

            {**
             Calculate the destination rect in which the picture should be drawn
             @param(pPicture Picture to draw)
             @returns(The destination rect)
            }
            function CalculateDestRect(pPicture: TPicture): TRect; virtual;

            {**
             Paint the image
             @param(pPicture Picture to paint)
             @param(pCanvas Canvas on which the picture will be painted)
            }
            procedure Paint(pPicture: TPicture; pCanvas: TCanvas); reintroduce; virtual;

            {**
             Called when the frame count should be set to properties
             @param(pSender Sender for which the frame count should be set)
             @param(value Frame count)
            }
            procedure DoSetFrameCount(pSender: TWSVGImage.IAnimationProps; value: Cardinal); override;

            {**
             Set animation position
             @param(pSender Sender for which the position should be set)
             @param(value Animation position in percent (between 0 and 100))
            }
            procedure DoSetPosition(pSender: TWSVGImage.IAnimationProps; value: Cardinal); override;

            {**
             Enable or disable the animation
             @param(pSender Sender for which the animation should be enabled or disabled)
             @param(value If @true the animation will be enabled, disabled otherwise)
            }
            procedure DoSetAnimate(pSender: TWSVGImage.IAnimationProps; value: Boolean); override;

            {**
             Called while SVG animation is running
             @param(pSender Event sender)
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            function DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
                    pCustomData: Pointer): Boolean; override;

            {**
             Called when the hovered picture changed
             @param(pSender Event sender)
            }
            procedure OnHoveredPictureChange(pSender: TObject); virtual;

            {**
             Called when the clicked picture changed
             @param(pSender Event sender)
            }
            procedure OnClickedPictureChange(pSender: TObject); virtual;

            {**
             Called when the disabled picture changed
             @param(pSender Event sender)
            }
            procedure OnDisabledPictureChange(pSender: TObject); virtual;

        public
            {**
             Constructor
             @param(pOwner Component owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;

        published
            {**
             Get or set the hovered state animation properties
            }
            property HoveredAnimation: TWSVGImage.IAnimationProps read m_pHoveredAnimationProps write m_pHoveredAnimationProps;

            {**
             Get or set the hovered picture
            }
            property HoveredPicture: TPicture read m_pHoveredPicture write SetHoveredPicture;

            {**
             Get or set the OnHoverPictureAnimate event
            }
            property OnHoveredPictureAnimate: TWSVGImage.ITfSVGAnimateEvent read m_fOnHoveredPictureAnimate write m_fOnHoveredPictureAnimate;

            {**
             Get or set the clicked state animation properties
            }
            property ClickedAnimation: TWSVGImage.IAnimationProps read m_pClickedAnimationProps write m_pClickedAnimationProps;

            {**
             Get or set the clicked picture
            }
            property ClickedPicture: TPicture read m_pClickedPicture write SetClickedPicture;

            {**
             Get or set the OnClickedPictureAnimate event
            }
            property OnClickedPictureAnimate: TWSVGImage.ITfSVGAnimateEvent read m_fOnClickedPictureAnimate write m_fOnClickedPictureAnimate;

            {**
             Get or set the disabled state animation properties
            }
            property DisabledAnimation: TWSVGImage.IAnimationProps read m_pDisabledAnimationProps write m_pDisabledAnimationProps;

            {**
             Get or set the disabled picture
            }
            property DisabledPicture: TPicture read m_pDisabledPicture write SetDisabledPicture;

            {**
             Get or set the OnDisabledPictureAnimate event
            }
            property OnDisabledPictureAnimate: TWSVGImage.ITfSVGAnimateEvent read m_fOnDisabledPictureAnimate write m_fOnDisabledPictureAnimate;
    end;

implementation

uses
  System.Types;

//---------------------------------------------------------------------------
constructor TWSVGImageButton.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pCanvas                   := TCanvas.Create;
    m_pHoveredPicture           := TPicture.Create;
    m_pClickedPicture           := TPicture.Create;
    m_pDisabledPicture          := TPicture.Create;
    m_pHoveredAnimationProps    := TWSVGImage.IAnimationProps.Create(Self);
    m_pClickedAnimationProps    := TWSVGImage.IAnimationProps.Create(Self);
    m_pDisabledAnimationProps   := TWSVGImage.IAnimationProps.Create(Self);
    m_fOnHoveredPictureAnimate  := nil;
    m_fOnClickedPictureAnimate  := nil;
    m_fOnDisabledPictureAnimate := nil;
    m_Clicked                   := False;

    // configure pictures
    m_fPrevOnHoveredPictureChange  := m_pHoveredPicture.OnChange;
    m_pHoveredPicture.OnChange     := OnHoveredPictureChange;
    m_fPrevOnClickedPictureChange  := m_pClickedPicture.OnChange;
    m_pClickedPicture.OnChange     := OnClickedPictureChange;
    m_fPrevOnDisabledPictureChange := m_pDisabledPicture.OnChange;
    m_pDisabledPicture.OnChange    := OnDisabledPictureChange;
end;
//---------------------------------------------------------------------------
destructor TWSVGImageButton.Destroy;
begin
    FreeAndNil(m_pCanvas);
    FreeAndNil(m_pHoveredPicture);
    FreeAndNil(m_pClickedPicture);
    FreeAndNil(m_pDisabledPicture);
    FreeAndNil(m_pHoveredAnimationProps);
    FreeAndNil(m_pClickedAnimationProps);
    FreeAndNil(m_pDisabledAnimationProps);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.WMPaint(var message: TWMPaint);
begin
    if (message.DC <> 0) and not (csDestroying in ComponentState) then
    begin
        m_pCanvas.Lock;

        try
            m_pCanvas.Handle := message.DC;

            try
                // select the button state to paint
                if (csDesigning in ComponentState) then
                    Paint(Picture, m_pCanvas)
                else
                if (not Enabled and not IsEmpty(m_pDisabledPicture)) then
                    Paint(m_pDisabledPicture, m_pCanvas)
                else
                if (Enabled and m_Clicked and not IsEmpty(m_pClickedPicture)) then
                    Paint(m_pClickedPicture, m_pCanvas)
                else
                if (Enabled and m_Hovered and not IsEmpty(m_pHoveredPicture)) then
                    Paint(m_pHoveredPicture, m_pCanvas)
                else
                    Paint(Picture, m_pCanvas);
            finally
                m_pCanvas.Handle := 0;
            end;
        finally
            m_pCanvas.Unlock;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TWSVGImageButton.GetSVG(pAnimProps: TWSVGImage.IAnimationProps): TWSVGGraphic;
begin
    if (not Assigned(pAnimProps)) then
        Exit(nil);

    // search for SVG matching with the animation properties set
    if (pAnimProps = m_pHoveredAnimationProps) then
    begin
        if (Assigned(m_pHoveredPicture.Graphic) and (m_pHoveredPicture.Graphic is TWSVGGraphic)) then
            Exit(m_pHoveredPicture.Graphic as TWSVGGraphic);
    end
    else
    if (pAnimProps = m_pClickedAnimationProps) then
    begin
        if (Assigned(m_pClickedPicture.Graphic) and (m_pClickedPicture.Graphic is TWSVGGraphic)) then
            Exit(m_pClickedPicture.Graphic as TWSVGGraphic);
    end
    else
    if (pAnimProps = m_pDisabledAnimationProps) then
    begin
        if (Assigned(m_pDisabledPicture.Graphic) and (m_pDisabledPicture.Graphic is TWSVGGraphic)) then
            Exit(m_pDisabledPicture.Graphic as TWSVGGraphic);
    end;

    Result := nil;
end;
//---------------------------------------------------------------------------
function TWSVGImageButton.GetAnimationProps(pSVG: TWSVGGraphic): TWSVGImage.IAnimationProps;
var
    pSrcSVG: TWSVGGraphic;
begin
    if (not Assigned(pSVG)) then
        Exit(nil);

    if ((m_pHoveredPicture.Graphic is TWSVGGraphic) and (not IsEmpty(m_pHoveredPicture))) then
    begin
        pSrcSvg := (m_pHoveredPicture.Graphic as TWSVGGraphic);

        if (pSrcSvg.Native.UUID = pSVG.Native.UUID) then
            Exit (m_pHoveredAnimationProps);
    end;

    if ((m_pClickedPicture.Graphic is TWSVGGraphic) and (not IsEmpty(m_pClickedPicture))) then
    begin
        pSrcSvg := (m_pClickedPicture.Graphic as TWSVGGraphic);

        if (pSrcSvg.Native.UUID = pSVG.Native.UUID) then
            Exit (m_pClickedAnimationProps);
    end;

    if ((m_pDisabledPicture.Graphic is TWSVGGraphic) and (not IsEmpty(m_pDisabledPicture))) then
    begin
        pSrcSvg := (m_pDisabledPicture.Graphic as TWSVGGraphic);

        if (pSrcSvg.Native.UUID = pSVG.Native.UUID) then
            Exit (m_pDisabledAnimationProps);
    end;

    Result := nil;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.SetHoveredPicture(pPicture: TPicture);
begin
    m_pHoveredPicture.Assign(pPicture);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.SetClickedPicture(pPicture: TPicture);
begin
    m_pClickedPicture.Assign(pPicture);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.SetDisabledPicture(pPicture: TPicture);
begin
    m_pDisabledPicture.Assign(pPicture);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.MouseDown(button: TMouseButton; shift: TShiftState; x, y: Integer);
begin
    inherited MouseDown(button, shift, x, y);

    m_Clicked := True;

    Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.MouseUp(button: TMouseButton; shift: TShiftState; x, y: Integer);
begin
    inherited MouseUp(button, shift, x, y);

    m_Clicked := False;

    Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.WndProc(var message: TMessage);
begin
    case (message.Msg) of
        CM_MOUSEENTER: begin; m_Hovered := True;  Invalidate; end;
        CM_MOUSELEAVE: begin; m_Hovered := False; Invalidate; end;
    end;

    inherited WndProc(message);
end;
//---------------------------------------------------------------------------
function TWSVGImageButton.IsEmpty(pPicture: TPicture): Boolean;
begin
    // is picture empty?
    if (not Assigned(pPicture.Graphic)) then
        Exit(True);

    // is a bitmap?
    if (pPicture.Graphic is Vcl.Graphics.TBitmap) then
    begin
        // is picture bitmap empty?
        if ((pPicture.Bitmap.Width = 0) or (pPicture.Bitmap.Height = 0)) then
            Exit(True);
    end
    else
    // is picture graphic empty?
    if ((pPicture.Graphic.Width = 0) or (pPicture.Graphic.Height = 0)) then
        Exit(True);

    Result := False;
end;
//---------------------------------------------------------------------------
function TWSVGImageButton.CalculateDestRect(pPicture: TPicture): TRect;
var
    w, h, cw, ch: Integer;
    xyAspect:     Double;
begin
    w  := pPicture.Width;
    h  := pPicture.Height;
    cw := ClientWidth;
    ch := ClientHeight;

    if (Stretch or (Proportional and ((w > cw) or (h > ch)))) then
    begin
        if (Proportional and (w > 0) and (h > 0)) then
        begin
            xyAspect := w / h;

            if (w > h) then
            begin
                w := cw;
                h := Trunc(cw / xyAspect);

                // is too big?
                if (h > ch) then
                begin
                    h := ch;
                    w := Trunc(ch * xyAspect);
                end;
            end
            else
            begin
                h := ch;
                w := Trunc(ch * xyAspect);

                // is too big?
                if (w > cw) then
                begin
                    w := cw;
                    h := Trunc(cw / xyAspect);
                end;
            end;
        end
        else
        begin
            w := cw;
            h := ch;
        end;
    end;

    with Result do
    begin
        Left   := 0;
        Top    := 0;
        Right  := w;
        Bottom := h;
    end;

    if Center then
        OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.Paint(pPicture: TPicture; pCanvas: TCanvas);
var
    hPaintBuffer, hMemDC: THandle;
    rect:                 TRect;
begin
    // draw the design time rect around the image
    if (csDesigning in ComponentState) then
    begin
        pCanvas.Pen.Style   := psDash;
        pCanvas.Brush.Style := bsClear;
        pCanvas.Rectangle(0, 0, Width, Height);
    end;

    if (((csGlassPaint in ControlState) and (Assigned(pPicture.Graphic)))
            and not pPicture.Graphic.SupportsPartialTransparency)
    then
    begin
        rect         := CalculateDestRect(pPicture);
        hPaintBuffer := BeginBufferedPaint(pCanvas.Handle, rect, BPBF_TOPDOWNDIB, nil, HDC(hMemDC));

        try
            pCanvas.Handle := hMemDC;
            pCanvas.StretchDraw(rect, pPicture.Graphic);
            BufferedPaintMakeOpaque(hPaintBuffer, rect);
        finally
            EndBufferedPaint(hPaintBuffer, True);
        end;
    end
    else
        pCanvas.StretchDraw(CalculateDestRect(pPicture), pPicture.Graphic);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.DoSetFrameCount(pSender: TWSVGImage.IAnimationProps; value: Cardinal);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // get the SVG matching with the sender
    pSVG := GetSVG(pSender);

    // found the SVG?
    if (Assigned(pSVG)) then
    begin
        position := pSender.Position;

        // configure the SVG. NOTE the animate property should be set first because it may overwrite
        // the other values
        pSVG.Animate    := pSender.Animate and (not(csDesigning in ComponentState));
        pSVG.FrameCount := value;
        pSVG.Position   := (position * 0.01);

        Exit;
    end;

    // prevent the incorrect animation set to be applied to default picture
    if (pSender <> Animation) then
        Exit;

    inherited DoSetFrameCount(pSender, value);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.DoSetPosition(pSender: TWSVGImage.IAnimationProps; value: Cardinal);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // get the SVG matching with the sender
    pSVG := GetSVG(pSender);

    // found the SVG?
    if (Assigned(pSVG)) then
    begin
        position := value;

        // configure the SVG. NOTE the animate property should be set first because it may overwrite
        // the other values
        pSVG.Animate    := pSender.Animate and (not(csDesigning in ComponentState));
        pSVG.FrameCount := pSender.FrameCount;
        pSVG.Position   := (position * 0.01);

        Exit;
    end;

    // prevent the incorrect animation set to be applied to default picture
    if (pSender <> Animation) then
        Exit;

    inherited DoSetPosition(pSender, value);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.DoSetAnimate(pSender: TWSVGImage.IAnimationProps; value: Boolean);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // get the SVG matching with the sender
    pSVG := GetSVG(pSender);

    // found the SVG?
    if (Assigned(pSVG)) then
    begin
        position := pSender.Position;

        // configure the SVG. NOTE the animate property should be set first because it may overwrite
        // the other values
        pSVG.Animate    := value and (not(csDesigning in ComponentState));
        pSVG.FrameCount := pSender.FrameCount;
        pSVG.Position   := (position * 0.01);

        Exit;
    end;

    // prevent the incorrect animation set to be applied to default picture
    if (pSender <> Animation) then
        Exit;

    inherited DoSetAnimate(pSender, value);
end;
//---------------------------------------------------------------------------
function TWSVGImageButton.DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
        pCustomData: Pointer): Boolean;
var
    pSVG:       TWSVGGraphic;
    pAnimProps: TWSVGImage.IAnimationProps;
begin
    if (pSender is TWSVGGraphic) then
    begin
        // get the SVG
        pSVG := pSender as TWSVGGraphic;

        // get the matching animation properties
        pAnimProps := GetAnimationProps(pSVG);

        if (Assigned(pAnimProps)) then
        begin
            // update the published values
            pAnimProps.FrameCount := pSVG.FrameCount;
            pAnimProps.Position   := Round(pSVG.Position * 100.0);

            // ask user about continuing animation
            if (Assigned(m_fOnHoveredPictureAnimate) and (pAnimProps = m_pHoveredAnimationProps)) then
                Exit(m_fOnHoveredPictureAnimate(pSender, pAnimDesc, pCustomData))
            else
            if (Assigned(m_fOnClickedPictureAnimate) and (pAnimProps = m_pClickedAnimationProps)) then
                Exit(m_fOnClickedPictureAnimate(pSender, pAnimDesc, pCustomData))
            else
            if (Assigned(m_fOnDisabledPictureAnimate) and (pAnimProps = m_pDisabledAnimationProps)) then
                Exit(m_fOnDisabledPictureAnimate(pSender, pAnimDesc, pCustomData));

            // notify that animation is allowed to continue
            Exit(True);
        end;
    end;

    Result := inherited DoAnimate(pSender, pAnimDesc, pCustomData);
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.OnHoveredPictureChange(pSender: TObject);
begin
    RunAnimation(m_HoveredImgGUID, m_pHoveredPicture.Graphic, m_pHoveredAnimationProps);

    if (Assigned(m_fPrevOnHoveredPictureChange)) then
        m_fPrevOnHoveredPictureChange(pSender);

    Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.OnClickedPictureChange(pSender: TObject);
begin
    RunAnimation(m_ClickedImgGUID, m_pClickedPicture.Graphic, m_pClickedAnimationProps);

    if (Assigned(m_fPrevOnClickedPictureChange)) then
        m_fPrevOnClickedPictureChange(pSender);

    Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGImageButton.OnDisabledPictureChange(pSender: TObject);
begin
    RunAnimation(m_DisabledImgGUID, m_pDisabledPicture.Graphic, m_pDisabledAnimationProps);

    if (Assigned(m_fPrevOnDisabledPictureChange)) then
        m_fPrevOnDisabledPictureChange(pSender);

    Invalidate;
end;
//---------------------------------------------------------------------------

end.
