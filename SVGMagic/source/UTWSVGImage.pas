{**
 @abstract(@name provides an overridden image that supports the SVG graphics in a such manner that
           the animations are taken into account internally.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGImage;

interface

uses System.Classes,
     System.SysUtils,
     System.Math,
     Vcl.Graphics,
     Vcl.ExtCtrls,
     UTWMajorSettings,
     UTWAnimationTimer,
     UTWSVGAnimationDescriptor,
     UTWSVGRasterizer,
     UTWSVGGraphic,
     UTWSVGFrameCalculator;

type
    {**
     Image that supports animated SVG graphics
    }
    TWSVGImage = class(TImage)
        public type
            {**
             Class to group and expose all SVG animation properties
            }
            IAnimationProps = class(TPersistent)
                private
                    m_pOwner:     TWSVGImage;
                    m_FrameCount: Cardinal;
                    m_Position:   Cardinal;
                    m_Animate:    Boolean;

                protected
                    {**
                     Set the frame count
                     @param(value Frame count)
                     @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count
                                     is used to determine how many frames, in an ideal situation,
                                     should be rendered by seconds, and thus allows to calculate the
                                     time interval between each frames. Instead, the FPS represents
                                     the number of frames per seconds a system can effectively process
                    }
                    procedure SetFrameCount(value: Cardinal); virtual;

                    {**
                     Set animation position
                     @param(value Animation position in percent (between 0 and 100))
                    }
                    procedure SetPosition(value: Cardinal); virtual;

                    {**
                     Enable or disable the animation
                     @param(value If @true the animation will be enabled, disabled otherwise)
                    }
                    procedure SetAnimate(value: Boolean); virtual;

                public
                    {**
                     Constructor
                     @param(pOwner Properties owner)
                    }
                    constructor Create(pOwner: TWSVGImage); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                published
                    {**
                     Get or set the number of frame to render per seconds
                     @br @bold(NOTE) Be careful, this is not identical to the FPS. The frame count
                                     is used to determine how many frames, in an ideal situation,
                                     should be rendered by seconds, and thus allows to calculate the
                                     time interval between each frames. Instead, the FPS represents
                                     the number of frames per seconds a system can effectively process
                    }
                    property FrameCount: Cardinal read m_FrameCount write SetFrameCount nodefault;

                    {**
                     Get or set the animation position, in percent (between 0 and 100)
                    }
                    property Position: Cardinal read m_Position write SetPosition nodefault;

                    {**
                     Enable or disable the animation
                    }
                    property Animate: Boolean read m_Animate write SetAnimate default C_TWSVGGraphic_Default_Animate;
            end;

            {**
             Called when a SVG is about to be animated
             @param(pSender Event sender)
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            ITfSVGAnimateEvent = function (pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
                    pCustomData: Pointer): Boolean of object;

        private
            m_ImgGUID:              UnicodeString;
            m_pAnimationProps:      IAnimationProps;
            m_fOnAnimate:           ITfSVGAnimateEvent;
            m_fPrevOnPictureChange: TNotifyEvent;

            {**
             Get the library version
             @returns(Library version, #ERROR on error)
            }
            function GetVersion: UnicodeString;

        protected
            {**
             Called when the frame count should be set to properties
             @param(pSender Sender for which the frame count should be set)
             @param(value Frame count)
            }
            procedure DoSetFrameCount(pSender: IAnimationProps; value: Cardinal); virtual;

            {**
             Set animation position
             @param(pSender Sender for which the position should be set)
             @param(value Animation position in percent (between 0 and 100))
            }
            procedure DoSetPosition(pSender: IAnimationProps; value: Cardinal); virtual;

            {**
             Enable or disable the animation
             @param(pSender Sender for which the animation should be enabled or disabled)
             @param(value If @true the animation will be enabled, disabled otherwise)
            }
            procedure DoSetAnimate(pSender: IAnimationProps; value: Boolean); virtual;

            {**
             Called while SVG animation is running
             @param(pSender Event sender)
             @param(pAnimDesc Animation description)
             @param(pCustomData Custom data)
             @returns(@true if animation can continue, otherwise @false)
            }
            function DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
                    pCustomData: Pointer): Boolean; virtual;

            {**
             Gets the animation properties from SVG
             @param(pSVG SVG from which animation properties should be get)
             @param(pProps Animation properties to update)
             @returns(@true on success, otherwise @false)
            }
            function GetAnimPropsFromSVG(pSVG: TWSVGGraphic; pAnimProps: IAnimationProps): Boolean; virtual;

            {**
             Run the animation
             @param(guid Currently loaded SVG guid)
             @param(pGraphic Graphic for which animation should be run if required)
             @param(pAnimProps Animation properties)
            }
            procedure RunAnimation(var guid: UnicodeString; pGraphic: TGraphic; pAnimProps: IAnimationProps); virtual;

            {**
             Called when the internal picture changed
             @param(pSender Event sender)
            }
            procedure OnPictureChange(pSender: TObject); virtual;

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
             Get the library version number
            }
            property Version: UnicodeString read GetVersion;

            {**
             Get or set the animation properties
            }
            property Animation: IAnimationProps read m_pAnimationProps write m_pAnimationProps;

            {**
             Get or set the OnAnimate event
            }
            property OnAnimate: ITfSVGAnimateEvent read m_fOnAnimate write m_fOnAnimate;
    end;

implementation
//---------------------------------------------------------------------------
// TWSVGImage.IAnimationProps
//---------------------------------------------------------------------------
constructor TWSVGImage.IAnimationProps.Create(pOwner: TWSVGImage);
begin
    inherited Create;

    m_pOwner     := pOwner;
    m_FrameCount := 0;
    m_Position   := 0;
    m_Animate    := C_TWSVGGraphic_Default_Animate;
end;
//---------------------------------------------------------------------------
destructor TWSVGImage.IAnimationProps.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.IAnimationProps.SetFrameCount(value: Cardinal);
begin
    if (value = m_FrameCount) then
        Exit;

    m_FrameCount := value;

    m_pOwner.DoSetFrameCount(Self, m_FrameCount);
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.IAnimationProps.SetPosition(value: Cardinal);
var
    pos: Cardinal;
begin
    pos := Min(value, 100);

    if (pos = m_Position) then
        Exit;

    m_Position := pos;

    m_pOwner.DoSetPosition(Self, m_Position);
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.IAnimationProps.SetAnimate(value: Boolean);
begin
    if (value = m_Animate) then
        Exit;

    m_Animate := value;

    m_pOwner.DoSetAnimate(Self, m_Animate);
end;
//---------------------------------------------------------------------------
// TWSVGImage
//---------------------------------------------------------------------------
constructor TWSVGImage.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_pAnimationProps := IAnimationProps.Create(Self);
    m_fOnAnimate      := nil;

    // override the picture OnChange event
    m_fPrevOnPictureChange := Picture.OnChange;
    Picture.OnChange       := OnPictureChange;
end;
//---------------------------------------------------------------------------
destructor TWSVGImage.Destroy;
begin
    FreeAndNil(m_pAnimationProps);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWSVGImage.GetVersion: UnicodeString;
begin
    if (not Assigned(TWLibraryVersion)) then
        Exit('#ERROR');

    Result := TWLibraryVersion.ToStr;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.DoSetFrameCount(pSender: IAnimationProps; value: Cardinal);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // is a SVG?
    if (Assigned(Picture.Graphic) and (Picture.Graphic is TWSVGGraphic)) then
    begin
        position := pSender.Position;

        // get and configure the SVG. NOTE the animate property should be set first because it may
        // overwrite the other values
        pSVG            := Picture.Graphic as TWSVGGraphic;
        pSVG.Animate    := pSender.Animate and (not(csDesigning in ComponentState));
        pSVG.FrameCount := value;
        pSVG.Position   := (position * 0.01);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.DoSetPosition(pSender: IAnimationProps; value: Cardinal);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // is a SVG?
    if (Assigned(Picture.Graphic) and (Picture.Graphic is TWSVGGraphic)) then
    begin
        position := value;

        // get and configure the SVG. NOTE the animate property should be set first because it may
        // overwrite the other values
        pSVG            := Picture.Graphic as TWSVGGraphic;
        pSVG.Animate    := pSender.Animate and (not(csDesigning in ComponentState));
        pSVG.FrameCount := pSender.m_FrameCount;
        pSVG.Position   := (position * 0.01);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.DoSetAnimate(pSender: IAnimationProps; value: Boolean);
var
    pSVG:     TWSVGGraphic;
    position: Double;
begin
    if (csDesigning in ComponentState) then
        Exit;

    // is a SVG?
    if (Assigned(Picture.Graphic) and (Picture.Graphic is TWSVGGraphic)) then
    begin
        position := pSender.Position;

        // get and configure the SVG. NOTE the animate property should be set first because it may
        // overwrite the other values
        pSVG            := Picture.Graphic as TWSVGGraphic;
        pSVG.Animate    := value and (not(csDesigning in ComponentState));
        pSVG.FrameCount := pSender.FrameCount;
        pSVG.Position   := (position * 0.01);
    end;
end;
//---------------------------------------------------------------------------
function TWSVGImage.DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
        pCustomData: Pointer): Boolean;
var
    pSVG: TWSVGGraphic;
begin
    if (pSender is TWSVGGraphic) then
    begin
        // get the SVG
        pSVG := pSender as TWSVGGraphic;

        // update the published values
        m_pAnimationProps.FrameCount := pSVG.FrameCount;
        m_pAnimationProps.Position   := Round(pSVG.Position * 100.0);
    end;

    // ask user about continuing animation
    if (Assigned(m_fOnAnimate)) then
        Exit(m_fOnAnimate(pSender, pAnimDesc, pCustomData));

    // notify that animation is allowed to continue
    Result := True;
end;
//---------------------------------------------------------------------------
function TWSVGImage.GetAnimPropsFromSVG(pSVG: TWSVGGraphic; pAnimProps: IAnimationProps): Boolean;
begin
    if (not Assigned(pSVG)) then
        Exit(False);

    if (not Assigned(pAnimProps)) then
        Exit(False);

    // ignore if loading from properties
    if (csLoading in ComponentState) then
        Exit(False);

    // set the default animation properties values
    if (pSVG.AnimationDuration <> 0.0) then
    begin
        pAnimProps.FrameCount := Round(100.0 * (1000.0 / pSVG.AnimationDuration));
        pAnimProps.Animate    := True;
    end
    else
    begin
        pAnimProps.FrameCount := 0;
        pAnimProps.Animate    := False;
    end;

    pAnimProps.Position := 0;
    Result              := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.RunAnimation(var guid: UnicodeString; pGraphic: TGraphic; pAnimProps: IAnimationProps);
var
    pSVG: TWSVGGraphic;
begin
    if (not Assigned(pAnimProps)) then
        Exit;

    // is a SVG?
    if (Assigned(pGraphic) and (pGraphic is TWSVGGraphic)) then
    begin
        // get the SVG
        pSVG := pGraphic as TWSVGGraphic;

        // design time?
        if (csDesigning in ComponentState) then
        begin
            GetAnimPropsFromSVG(pSVG, pAnimProps);
            Exit;
        end;

        // svg changed since last check?
        if (pSVG.Native.GetUUID <> guid) then
        begin
            // get and apply animation properties from SVG
            if (not GetAnimPropsFromSVG(pSVG, pAnimProps)) then
            begin
                // properties weren't loaded from svg, use the existing one to configure the svg.
                // NOTE the animate property should be set first because it may overwrite the other
                // values
                pSVG.Animate    := pAnimProps.Animate;
                pSVG.FrameCount := pAnimProps.FrameCount;
                pSVG.Position   := pAnimProps.Position;
            end;

            pSVG.OnAnimate := DoAnimate;
            guid           := pSVG.Native.GetUUID;
        end;
    end;

    // design time and not a SVG graphic?
    if (csDesigning in ComponentState) then
    begin
        // reset animation properties
        pAnimProps.FrameCount := 0;
        pAnimProps.Position   := 0;
        pAnimProps.Animate    := False;

        Exit;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGImage.OnPictureChange(pSender: TObject);
begin
    RunAnimation(m_ImgGUID, Picture.Graphic, m_pAnimationProps);

    if (Assigned(m_fPrevOnPictureChange)) then
        m_fPrevOnPictureChange(pSender);
end;
//---------------------------------------------------------------------------

end.
