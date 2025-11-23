{**
 @abstract(@name provides a class that allows component glyphs to be overriden by SVG.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGComponentStyle;

interface

uses System.Classes,
     System.SysUtils,
     System.Generics.Collections,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.ComCtrls,
     Winapi.Messages,
     Winapi.Windows,
     UTWMajorSettings,
     UTWHelpers,
     UTWDesignPatterns,
     UTWAnimationTimer,
     UTWSmartPointer,
     UTWSVGAnimationDescriptor,
     UTWSVGRasterizer,
     UTWSVGGraphic,
     UTWSVGFrameCalculator;

const
    //---------------------------------------------------------------------------
    // Global constants
    //---------------------------------------------------------------------------
    C_TWSVGComponentStyle_Default_DPIScale = True;
    //---------------------------------------------------------------------------

type
    {**
     Called when component style detects a DPI change and should update its content
     @param(oldDPI Old DPI value)
     @param(newDPI New DPI value)
     @returns(@true if event was handled and should no longer be considered, otherwise @false)
    }
    TWfOnSVGComponentStyleDPIChanged = function(oldDPI, newDPI: Integer): Boolean of object;

    {**
     Component style
    }
    TWSVGComponentStyle = class(TComponent)
        protected type
            IWGlyph = class
                private
                    m_pOwnerStyle:          TWSVGComponentStyle;
                    m_pPicture:             TPicture;
                    m_fPrevOnPictureChange: TNotifyEvent;

                protected
                    {**
                     Set the glyph picture
                     @param(pPicture Picture to set)
                    }
                    procedure SetPicture(pPicture: TPicture); virtual;

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
                     Called when the internal picture changed
                     @param(pSender Event sender)
                    }
                    procedure OnPictureChange(pSender: TObject); virtual;

                public
                    {**
                     Constructor
                     @param(pOwnerStyle Owner style)
                    }
                    constructor Create(pOwnerStyle: TWSVGComponentStyle); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                    {**
                     Check if the glyph is empty
                     @returns(@true if the glyph is empty, otherwise @false)
                    }
                    function IsEmpty: Boolean; virtual;

                    {**
                     Updates the animation status
                     @param(animated If @true, the picture should be animated)
                    }
                    procedure UpdateAnimStatus(animated: Boolean); virtual;

                public
                    {**
                     Get or set the glyph picture
                    }
                    property Picture: TPicture read m_pPicture write SetPicture;
            end;

            {**
             Style hook, used to subclass the target painting
            }
            IWSVGComponentStyleHook = class
                private
                    m_pOwnerStyle: TWSVGComponentStyle;
                    m_pTarget:     TWinControl;
                    m_fOldWndProc: TWndMethod;

                    {**
                     Hook the target Windows procedure
                    }
                    procedure HookControlProcedure;

                    {**
                     Restore the Windows procedure that the target was using before the hook
                    }
                    procedure RestoreHookedProcedure;

                protected
                    {**
                     Called when the Windows procedure is executed on a target
                     @param(message Windows message)
                    }
                    procedure TargetWndProc(var message: TMessage); virtual;

                public
                    {**
                     Constructor
                     @param(pOwnerStyle Owner style)
                     @param(pTarget Target component to hook)
                    }
                    constructor Create(pOwnerStyle: TWSVGComponentStyle; pTarget: TWinControl); virtual;

                    {**
                     Destructor
                    }
                    destructor Destroy; override;

                public
                    {**
                     Get the target control
                    }
                    property Target: TWinControl read m_pTarget;
            end;

            {**
             Target list
            }
            IWTargets = TList<TWinControl>;

        public type
            {**
             Style hook list
            }
            IWSVGComponentStyleHooks = TObjectList<IWSVGComponentStyleHook>;

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
            m_pOwnerCtrl:                     TControl;
            m_Position:                       NativeUInt;
            m_Animate:                        Boolean;
            m_Enabled:                        Boolean;
            m_All:                            Boolean;
            m_fOldParentWndProc:              TWndMethod;
            m_fOnAnimate:                     ITfSVGAnimateEvent;
            m_fOnSVGComponentStyleDPIChanged: TWfOnSVGComponentStyleDPIChanged;

            {$if CompilerVersion < 30}
                m_fGetDpiForMonitor:       TWfGetDpiForMonitor;
                m_fGetProcessDPIAwareness: TWfGetProcessDPIAwareness;
            {$ifend}

            {**
             Get the library version
             @returns(Library version, #ERROR on error)
            }
            function GetVersion: UnicodeString;

        protected
            m_pHooks:              IWSVGComponentStyleHooks;
            m_pOverlay:            Vcl.Graphics.TBitmap;
            m_pCanvas:             TCanvas;
            m_ParentPixelsPerInch: Integer;
            m_RefPixelsPerInch:    Integer;
            m_StartPixelsPerInch:  Integer;
            m_PixelsPerInch:       Integer;
            m_DPIScale:            Boolean;
            m_PerMonitorV2Mode:    Boolean;
            m_RestorePaint:        Boolean;

            {**
             Called on application starts, after DFM files were read and applied
            }
            procedure Loaded; override;

            {**
             Get measurements required to draw correctly a check glyph
             @param(dpiScale If true returned measures should be scaled regarding to the DPI value)
             @param(width @bold([out]) Menu style check glyph width in pixels)
             @param(height @bold([out]) Menu style check glyph height in pixels)
             @param(xInner @bold([out]) Menu style check glyph inner width in pixels)
             @param(yInner @bold([out]) Menu style check glyph inner height in pixels)
            }
            procedure MeasureCheckGlyph(dpiScale: Boolean; out width: Integer; out height: Integer;
                    out xInner: Integer; out yInner: Integer); virtual;

            {**
             Calculate checkbox rects
             @param(pTarget Target component for which the rects should be calculated)
             @param(alignment Target component alignment)
             @param(bgRect @bold([out]) Checkbox glyph background rect)
             @param(cbRect @bold([out]) Checkbox glyph rect)
            }
            procedure CalculateCheckGlyphRects(pTarget: TWinControl; alignment: TLeftRight; var bgRect: TRect;
                    var cbRect: TRect); virtual;

            {**
             Set image list DPI scale
             @param(value If @true, DPI scale is enabled, disabled otherwise)
            }
            procedure SetDPIScale(value: Boolean); virtual;

            {**
             Set image list pixels per inch
             @param(value Pixels per inch value)
            }
            procedure SetPixelsPerInch(value: Integer); virtual;

            {**
             Check if pixels per inch value should be stored in DFM file
             @returns(@true if value should be stored, otherwise @false)
            }
            function IsPixelsPerInchStored: Boolean; virtual;

            {**
             Called when original component size should be saved (before any DPI change was applied)
            }
            procedure DoSaveOriginalSize; virtual; abstract;

            {**
             Get if glyphs should be scaled with DPI
             @returns(@true if glyphs should be scaled with DPI, otherwise @false)
            }
            function DoScaleWithDPI: Boolean; virtual;

            {**
             Called when size should be recalculated due to a DPI change
             @param(oldDPI Previous pixels per inch value)
             @param(newDPI New pixels per inch value)
            }
            procedure DoApplyDPIChange(oldDPI, newDPI: Integer); virtual; abstract;

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
             Check if advanced paint should be done
             @param(pTarget Target component to paint)
             @returns(@true if advanced paint should be done, otherwise @false)
            }
            function DoProcessAdvancedPaint(pTarget: TWinControl): Boolean; virtual; abstract;

            {**
             Check if a newly added control should be added to skin engine, and add it if yes
             @param(pControl Newly added control)
            }
            procedure DoAddTarget(pControl: TControl); virtual; abstract;

            {**
             Notify that the animation status has changed and should be updated
            }
            procedure DoUpdateAnimStatus; virtual; abstract;

            {**
             Draw the advanced paint on the target component
             @param(hDC Device context to paint on)
             @param(pTarget Target control to paint)
            }
            procedure Draw(hDC: THandle; pTarget: TWinControl); virtual; abstract;

            {**
             Fill a rect using the background color
             @param(rect Background rect to fill)
             @param(pTarget Target control for which background should be painted)
             @param(pCanvas Canvas on which the background should be filled)
            }
            procedure FillBg(const rect: TRect; pTarget: TWinControl; pCanvas: TCanvas); virtual;

            {**
             Enable or disable if animations are running
             @param(value If @true, animations are running)
            }
            procedure SetAnimate(value: Boolean); virtual;

            {**
             Enable or disable the style
             @param(value If @true, the style is enabled)
            }
            procedure SetEnabled(value: Boolean); virtual;

            {**
             Enable or disable if all components mathcing with style are skinned
             @param(value If @true, all components matching with style will be skinned)
            }
            procedure SetAll(value: Boolean); overload; virtual; abstract;

            {**
             Enable or disable if all components mathcing with style are skinned
             @param(value If @true, all components matching with style will be skinned)
             @param(styleClass Style class)
             @param(targetClass Target class)
            }
            procedure SetAll(value: Boolean; styleClass, targetClass: TClass); overload; virtual;

            {**
             Add a target control to the style
             @param(pTarget Target control to add)
            }
            procedure AddTarget(pTarget: TWinControl); virtual;

            {**
             Remove a target control from the style
             @param(pTarget Target control to remove)
            }
            procedure RemoveTarget(pTarget: TWinControl); virtual;

            {**
             Invalidate all the targets
            }
            procedure InvalidateAllTargets; virtual;

            {**
             Repaint all the targets
            }
            procedure RepaintAllTargets; virtual;

            {**
             Receive notification from the target control
             @param(pComponent Target control that raised the event)
             @param(operation Operation currently applied on the target control)
            }
            procedure Notification(pComponent: TComponent; operation: TOperation); override;

            {**
             Called when a target is freed
             @param(pTarget Target about to be freed)
            }
            procedure OnFreeTarget(pTarget: TWinControl); virtual; abstract;

            {**
             Called when the Windows procedure is executed on the parent component
             @param(message Windows message)
            }
            procedure ParentWndProc(var message: TMessage); virtual;

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

            {**
             Called before the component is freeing
            }
            procedure BeforeDestruction; override;

        published
            {**
             Get the library version number
            }
            property Version: UnicodeString read GetVersion;

            {**
             Enable or disable the animation
            }
            property Animate: Boolean read m_Animate write SetAnimate default True;

            {**
             Enable or disable the style
            }
            property Enabled: Boolean read m_Enabled write SetEnabled default True;

            {**
             Enable or disable if all components on the form are skinned
            }
            property All: Boolean read m_All write SetAll default False;

            {**
             Get or set if the image is scaled by the DPI value
            }
            property DPIScale: Boolean read m_DPIScale write SetDPIScale default C_TWSVGComponentStyle_Default_DPIScale;

            {**
             Get or set if the per monitor v2 mode is used
            }
            property PerMonitorV2Mode: Boolean read m_PerMonitorV2Mode write m_PerMonitorV2Mode default True;

            {**
             Get or set the pixels per inch value used to scale the image list content
            }
            property PixelsPerInch: Integer read m_PixelsPerInch write SetPixelsPerInch stored IsPixelsPerInchStored nodefault;

            {**
             Get or set the OnAnimate event
            }
            property OnAnimate: ITfSVGAnimateEvent read m_fOnAnimate write m_fOnAnimate;

            {**
             Get or set OnSVGImageListDPIChanged event
            }
            property OnSVGComponentStyleDPIChanged: TWfOnSVGComponentStyleDPIChanged read m_fOnSVGComponentStyleDPIChanged write m_fOnSVGComponentStyleDPIChanged;
    end;

    {**
     Control access that allows to access several protected properties in TControl descendents
    }
    TWSVGStyleControlAccess = class(TControl);

    {**
     Control access that allows to access several protected properties in TWinControl descendents
    }
    TWSVGStyleWinControlAccess = class(TWinControl);

implementation

uses
  System.UITypes;

//---------------------------------------------------------------------------
// TWSVGComponentStyle.IWGlyph
//---------------------------------------------------------------------------
constructor TWSVGComponentStyle.IWGlyph.Create(pOwnerStyle: TWSVGComponentStyle);
begin
    inherited Create;

    m_pOwnerStyle := pOwnerStyle;
    m_pPicture    := TPicture.Create;

    // override the picture OnChange event
    m_fPrevOnPictureChange := m_pPicture.OnChange;
    m_pPicture.OnChange    := OnPictureChange;
end;
//---------------------------------------------------------------------------
destructor TWSVGComponentStyle.IWGlyph.Destroy;
begin
    FreeAndNil(m_pPicture);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWGlyph.SetPicture(pPicture: TPicture);
begin
    m_pPicture.Assign(pPicture);

    m_pOwnerStyle.InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.IWGlyph.DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
        pCustomData: Pointer): Boolean;
begin
    // notify that animation is allowed to continue
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWGlyph.OnPictureChange(pSender: TObject);
begin
    UpdateAnimStatus(m_pOwnerStyle.Animate);

    if (Assigned(m_fPrevOnPictureChange)) then
        m_fPrevOnPictureChange(pSender);

    m_pOwnerStyle.InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.IWGlyph.IsEmpty: Boolean;
begin
    // is picture empty?
    if (not Assigned(m_pPicture.Graphic)) then
        Exit(True);

    // is a bitmap?
    if (m_pPicture.Graphic is Vcl.Graphics.TBitmap) then
    begin
        // is picture bitmap empty?
        if ((m_pPicture.Bitmap.Width = 0) or (m_pPicture.Bitmap.Height = 0)) then
            Exit(True);
    end
    else
        // is picture graphic empty?
        if ((m_pPicture.Graphic.Width = 0) or (m_pPicture.Graphic.Height = 0)) then
            Exit(True);

    Result := False;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWGlyph.UpdateAnimStatus(animated: Boolean);
var
    pSVG: TWSVGGraphic;
begin
    // is a SVG?
    if (Assigned(m_pPicture.Graphic) and (m_pPicture.Graphic is TWSVGGraphic)) then
    begin
        // get and configure the SVG
        pSVG           := m_pPicture.Graphic as TWSVGGraphic;
        pSVG.OnAnimate := DoAnimate;
        pSVG.Animate   := animated and (not(csDesigning in m_pOwnerStyle.ComponentState));
    end;
end;
//---------------------------------------------------------------------------
// TWSVGComponentStyle.IWSVGComponentStyleHook
//---------------------------------------------------------------------------
constructor TWSVGComponentStyle.IWSVGComponentStyleHook.Create(pOwnerStyle: TWSVGComponentStyle;
        pTarget: TWinControl);
begin
    inherited Create;

    m_pOwnerStyle := pOwnerStyle;
    m_pTarget     := pTarget;
    m_fOldWndProc := nil;

    HookControlProcedure;
end;
//---------------------------------------------------------------------------
destructor TWSVGComponentStyle.IWSVGComponentStyleHook.Destroy;
begin
    RestoreHookedProcedure;

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWSVGComponentStyleHook.HookControlProcedure;
begin
    m_fOldWndProc        := m_pTarget.WindowProc;
    m_pTarget.WindowProc := TargetWndProc;

    // tell the target control to notify when important actions are taken
    m_pTarget.FreeNotification(m_pOwnerStyle);
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWSVGComponentStyleHook.RestoreHookedProcedure;
begin
    if (Assigned(m_pTarget) and Assigned(m_fOldWndProc)) then
        m_pTarget.WindowProc := m_fOldWndProc;

    m_fOldWndProc := nil;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.IWSVGComponentStyleHook.TargetWndProc(var message: TMessage);
var
    hDC: THandle;
    ps:  PAINTSTRUCT;
begin
    {$IFDEF DEBUG}
        //if (Assigned(m_pTarget)) then
            //TWLogHelper.LogToCompiler('TargetWndProc - ' + TWLogHelper.WinMsgToStr(m_pTarget, message));
    {$ENDIF}

    case (message.Msg) of
        WM_PAINT:
        begin
            // do subclass the control painting?
            if (m_pOwnerStyle.Enabled and Assigned(m_pTarget) and m_pOwnerStyle.DoProcessAdvancedPaint(m_pTarget)
                    and not m_pOwnerStyle.m_RestorePaint and not (csDestroying in m_pTarget.ComponentState))
            then
            begin
                // do increase the overlay size?
                if ((m_pOwnerStyle.m_pOverlay.Width < m_pTarget.ClientWidth)
                        or (m_pOwnerStyle.m_pOverlay.Height < m_pTarget.ClientHeight))
                then
                    m_pOwnerStyle.m_pOverlay.SetSize(m_pTarget.ClientWidth, m_pTarget.ClientHeight);

                // paint the target control content on the overlay
                try
                    m_pOwnerStyle.m_pOverlay.Canvas.Lock;
                    m_pTarget.Perform(WM_ERASEBKGND,  m_pOwnerStyle.m_pOverlay.Canvas.Handle, 0);
                    m_pTarget.Perform(WM_PRINTCLIENT, m_pOwnerStyle.m_pOverlay.Canvas.Handle,
                            PRF_CLIENT);
                finally
                    m_pOwnerStyle.m_pOverlay.Canvas.Unlock;
                end;

                // draw the glyph to subclass
                m_pOwnerStyle.Draw(m_pOwnerStyle.m_pOverlay.Canvas.Handle, m_pTarget);

                // get device context from message
                hDC := message.WParam;

                // no device context in received message?
                if (hDC <> 0) then
                    // copy the overlay content on the control canvas
                    BitBlt(hDC, 0, 0, m_pTarget.ClientWidth, m_pTarget.ClientHeight,
                            m_pOwnerStyle.m_pOverlay.Canvas.Handle, 0, 0, SRCCOPY)
                else
                begin
                    try
                        // begin paint
                        hDC := BeginPaint(m_pTarget.Handle, ps);

                        // succeeded?
                        if (hDC = 0) then
                        begin
                            if (Assigned(m_fOldWndProc)) then
                                m_fOldWndProc(message);

                            Exit;
                        end;

                        // copy the overlay content on the control canvas
                        BitBlt(hDC, 0, 0, m_pTarget.ClientWidth, m_pTarget.ClientHeight,
                                m_pOwnerStyle.m_pOverlay.Canvas.Handle, 0, 0, SRCCOPY);
                    finally
                        // end paint
                        if (hDC <> 0) then
                            EndPaint(m_pTarget.Handle, ps);
                    end;
                end;

                // validate entire client rect (it has just been completely redrawn)
                ValidateRect(m_pTarget.Handle, nil);

                // notify main windows procedure that component was repainted
                message.Result := 0;
                Exit;
            end;
        end;
    end;

    if (Assigned(m_fOldWndProc)) then
        m_fOldWndProc(message);
end;
//---------------------------------------------------------------------------
// TWSVGComponentStyle
//---------------------------------------------------------------------------
constructor TWSVGComponentStyle.Create(pOwner: TComponent);
{$if CompilerVersion < 30}
    var
        hSHCore: HMODULE;
{$ifend}
begin
    inherited Create(pOwner);

    m_pOwnerCtrl                     := nil;
    m_pHooks                         := IWSVGComponentStyleHooks.Create;
    m_pOverlay                       := Vcl.Graphics.TBitmap.Create;
    m_pCanvas                        := TCanvas.Create;
    m_Position                       := 0;
    m_Animate                        := True;
    m_Enabled                        := True;
    m_All                            := False;
    m_RestorePaint                   := False;
    m_RefPixelsPerInch               := TWVCLHelper.GetPixelsPerInchRef(pOwner);
    m_StartPixelsPerInch             := m_RefPixelsPerInch;
    m_ParentPixelsPerInch            := m_RefPixelsPerInch;
    m_PixelsPerInch                  := m_RefPixelsPerInch;
    m_DPIScale                       := C_TWSVGComponentStyle_Default_DPIScale;
    m_PerMonitorV2Mode               := True;
    m_fOnSVGComponentStyleDPIChanged := nil;
    m_fOldParentWndProc              := nil;
    m_fOnAnimate                     := nil;

    // intercepts the parent Windows procedure
    if (Assigned(pOwner) and (pOwner is TControl)) then
    begin
        m_pOwnerCtrl            := pOwner as TControl;
        m_fOldParentWndProc     := m_pOwnerCtrl.WindowProc;
        m_pOwnerCtrl.WindowProc := ParentWndProc;
    end;

    {$if CompilerVersion < 30}
        hSHCore := GetModuleHandleA('shcore.dll');

        // hook GetDpiForMonitor() function from shcore.dll
        if (hSHCore <> 0) then
        begin
            m_fGetDpiForMonitor       := GetProcAddress(hSHCore, 'GetDpiForMonitor');
            m_fGetProcessDpiAwareness := GetProcAddress(hSHCore, 'GetProcessDpiAwareness');
        end
        else
        begin
            m_fGetDpiForMonitor       := nil;
            m_fGetProcessDpiAwareness := nil;
        end;
    {$ifend}
end;
//---------------------------------------------------------------------------
destructor TWSVGComponentStyle.Destroy;
begin
    // restore the parent Windows procedure
    if (Assigned(m_fOldParentWndProc) and Assigned(m_pOwnerCtrl)) then
        m_pOwnerCtrl.WindowProc := m_fOldParentWndProc;

    FreeAndNil(m_pHooks);
    FreeAndNil(m_pOverlay);
    FreeAndNil(m_pCanvas);

    inherited Destroy;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.Loaded;
begin
    // update reference with the one defined by user
    m_RefPixelsPerInch := m_PixelsPerInch;

    // update pixels per inch to match with the current context
    {$if CompilerVersion < 30}
        m_PixelsPerInch := TWVCLHelper.GetMonitorPixelsPerInch(Owner, m_RefPixelsPerInch,
                m_fGetDPIForMonitor);
    {$else}
        m_PixelsPerInch := TWVCLHelper.GetMonitorPixelsPerInch(Owner, m_RefPixelsPerInch);
    {$ifend}

    // keep the pixels per inch found on the starting monitor
    m_StartPixelsPerInch := m_PixelsPerInch;

    // notify the children that the glyph reference size should be kept
    DoSaveOriginalSize;

    // request a change in the glyphs size
    DoApplyDPIChange(m_RefPixelsPerInch, m_PixelsPerInch);

    inherited Loaded;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.MeasureCheckGlyph(dpiScale: Boolean; out width: Integer;
        out height: Integer; out xInner: Integer; out yInner: Integer);
var
    systemDPI, dpi: Integer;
begin
    {$if CompilerVersion >= 33}
        // is Windows 10 build 1607 (Anniversary update) or higher?
        if (TWOSWinHelper.CheckWinVersion(10, 0, 14393)) then
        begin
            // do component be scaled with DPI?
            if (dpiScale) then
            begin
                // for per monitor v2 only, use the current monitor DPI, for all other use the dpi value found on start
                if (m_PerMonitorV2Mode) then
                    dpi := m_PixelsPerInch
                else
                    dpi := m_StartPixelsPerInch;

                // get system metric values matching with currently selected DPI
                width  := GetSystemMetricsForDpi(SM_CXMENUCHECK, dpi);
                height := GetSystemMetricsForDpi(SM_CYMENUCHECK, dpi);
                xInner := GetSystemMetricsForDpi(SM_CXEDGE,      dpi);
                yInner := GetSystemMetricsForDpi(SM_CYEDGE,      dpi);
                Exit;
            end;

            // get system metric values matching with DPI at which interface was designed
            width  := GetSystemMetricsForDpi(SM_CXMENUCHECK, m_RefPixelsPerInch);
            height := GetSystemMetricsForDpi(SM_CYMENUCHECK, m_RefPixelsPerInch);
            xInner := GetSystemMetricsForDpi(SM_CXEDGE,      m_RefPixelsPerInch);
            yInner := GetSystemMetricsForDpi(SM_CYEDGE,      m_RefPixelsPerInch);
            Exit;
        end;
    {$ifend}

    // get the system DPI
    systemDPI := TWVCLHelper.GetSystemPixelsPerInch;

    // assume that values returned by GetSystemMetrics() are always scaled to system DPI, so
    // revert them to DPI at which interface was designed
    width  := TWVCLHelper.ScaleByDPI(GetSystemMetrics(SM_CXMENUCHECK), m_RefPixelsPerInch, systemDPI);
    height := TWVCLHelper.ScaleByDPI(GetSystemMetrics(SM_CYMENUCHECK), m_RefPixelsPerInch, systemDPI);
    xInner := TWVCLHelper.ScaleByDPI(GetSystemMetrics(SM_CXEDGE),      m_RefPixelsPerInch, systemDPI);
    yInner := TWVCLHelper.ScaleByDPI(GetSystemMetrics(SM_CYEDGE),      m_RefPixelsPerInch, systemDPI);

    // do component be scaled with DPI?
    if (dpiScale) then
    begin
        // for per monitor v2 only, use the current monitor DPI, for all other use the dpi value found on start
        if (m_PerMonitorV2Mode) then
            dpi := m_PixelsPerInch
        else
            dpi := m_StartPixelsPerInch;

        // apply final DPI scaling
        width  := TWVCLHelper.ScaleByDPI(width,  dpi, m_RefPixelsPerInch);
        height := TWVCLHelper.ScaleByDPI(height, dpi, m_RefPixelsPerInch);
        xInner := TWVCLHelper.ScaleByDPI(xInner, dpi, m_RefPixelsPerInch);
        yInner := TWVCLHelper.ScaleByDPI(yInner, dpi, m_RefPixelsPerInch);
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.CalculateCheckGlyphRects(pTarget: TWinControl; alignment: TLeftRight;
        var bgRect: TRect; var cbRect: TRect);
var
    menuCheckWidth, menuCheckHeight, xInner, yInner, checkWidth, checkHeight, xPos, yPos: Integer;
    i:                                                                                    Cardinal;
begin
    if (not Assigned(pTarget)) then
        Exit;

    for i := 0 to 1 do
    begin
        // get several checkbox measurements. NOTE force DPI to be considered while background rect
        // is computed, even if option is disabled, otherwise background will not really cover the
        // default system glyph
        if (i = 0) then
            MeasureCheckGlyph(True, menuCheckWidth, menuCheckHeight, xInner, yInner)
        else
            MeasureCheckGlyph(DoScaleWithDPI, menuCheckWidth, menuCheckHeight, xInner, yInner);

        checkWidth  := menuCheckWidth  - xInner;
        checkHeight := menuCheckHeight - yInner;
        yPos        := (pTarget.ClientHeight div 2) - (checkHeight div 2);

        // calculate the checkbox rect
        if (((alignment = taLeftJustify) or (pTarget.BiDiMode = bdRightToLeft))
                and not ((alignment = taLeftJustify) and (pTarget.BiDiMode = bdRightToLeft)))
        then
            // rect is shown on the right
            xPos := pTarget.ClientWidth - menuCheckWidth
        else
            // rect is shown on the left
            xPos := 0;

        // calculate the rects
        case (i) of
            0: bgRect := TRect.Create(xPos - 1, yPos - 1, xPos + checkWidth + 2, yPos + checkHeight + 2);
            1: cbRect := TRect.Create(xPos, yPos, xPos + checkWidth, yPos + checkHeight);
        end;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.SetDPIScale(value: Boolean);
begin
    if (m_DPIScale = value) then
        Exit;

    m_DPIScale := value;

    // request a change in the glyphs size
    DoApplyDPIChange(m_RefPixelsPerInch, m_PixelsPerInch);
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.SetPixelsPerInch(value: Integer);
begin
    if (m_PixelsPerInch = value) then
        Exit;

    m_PixelsPerInch := value;

    // request a change in the glyphs size
    DoApplyDPIChange(m_RefPixelsPerInch, m_PixelsPerInch);
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.IsPixelsPerInchStored: Boolean;
begin
    Result := (m_PixelsPerInch <> m_ParentPixelsPerInch);
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.BeforeDestruction;
begin
    // as the style is shutting down, force all targets to repaint their native content
    try
        m_RestorePaint := True;
        InvalidateAllTargets;
    finally
        m_RestorePaint := False;
    end;

    inherited BeforeDestruction;
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.GetVersion: UnicodeString;
begin
    if (not Assigned(TWLibraryVersion)) then
        Exit('#ERROR');

    Result := TWLibraryVersion.ToStr;
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.DoScaleWithDPI: Boolean;
var
    id:       DWORD;
    hProcess: THandle;
begin
    // get the application process
    id       := WinApi.Windows.GetCurrentProcessId;
    hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, id);

    if (hProcess = 0) then
        Exit(False);

    {$if CompilerVersion < 30}
        Result := m_DPIScale and TWVCLHelper.IsDPIAware(hProcess, m_fGetProcessDpiAwareness);
    {$else}
        Result := m_DPIScale and TWVCLHelper.IsDPIAware(hProcess);
    {$ifend}
end;
//---------------------------------------------------------------------------
function TWSVGComponentStyle.DoAnimate(pSender: TObject; pAnimDesc: TWSVGAnimationDescriptor;
        pCustomData: Pointer): Boolean;
begin
    // ask user about continuing animation
    if (Assigned(m_fOnAnimate)) then
        Exit(m_fOnAnimate(pSender, pAnimDesc, pCustomData));

    // notify that animation is allowed to continue
    Result := True;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.FillBg(const rect: TRect; pTarget: TWinControl; pCanvas: TCanvas);
    {**
     Get color property value of a TControl
     @param(pControl Control for which the property should be get)
     @returns(Color property value)
    }
    function ColorOf(var pControl: TWinControl): TColor;
    begin
        Result := TWSVGStyleControlAccess(pControl).Color;
    end;

    {**
     Get parent color property value of a TControl
     @param(pControl Control for which the property should be get)
     @returns(Parent color property value)
    }
    function ParentColorOf(var pControl: TWinControl): Boolean;
    begin
        Result := TWSVGStyleControlAccess(pControl).ParentColor;
    end;

    {**
     Get parent background property value of a TWinControl
     @param(pControl Control for which the property should be get)
     @returns(Parent background property value)
    }
    function ParentBackgroundOf(var pControl: TWinControl): Boolean;
    begin
        Result := TWSVGStyleWinControlAccess(pControl).ParentBackground;
    end;
var
    pParent:   TWinControl;
    pPageCtrl: TPageControl;
begin
    // check if parent color should be used
    if (ParentColorOf(pTarget) and Assigned(pTarget.Parent)) then
    begin
        // configure the default parent color and get next parent
        m_pCanvas.Brush.Color := pTarget.Parent.Brush.Color;
        pParent               := pTarget.Parent;

        // iterate through parents
        while (Assigned(pParent)) do
            // check if color of this parent may be used
            if (pParent is TTabSheet) then
            begin
                // parent is a tab sheet, the color is always clWhite, except if page control style
                // property is set to another value than tsTabs, in this case the color is always
                // clBtnFace
                if (Assigned(pParent.Parent) and (pParent.Parent is TPageControl)) then
                begin
                    pPageCtrl := pParent.Parent as TPageControl;

                    if (pPageCtrl.Style = tsTabs) then
                        m_pCanvas.Brush.Color := clWhite
                    else
                        m_pCanvas.Brush.Color := clBtnFace;
                end
                else
                    m_pCanvas.Brush.Color := clWhite;

                break;
            end
            else
            if (ParentBackgroundOf(pParent) or ParentColorOf(pParent)) then
            begin
                // go to next parent
                pParent := pParent.Parent;
                continue;
            end
            else
            begin
                // get the parent color and break the loop
                m_pCanvas.Brush.Color := ColorOf(pParent);
                break;
            end;
    end
    else
        // use the control color
        m_pCanvas.Brush.Color := pTarget.Brush.Color;

    // fill the rect with the background color
    m_pCanvas.Brush.Style := bsSolid;
    m_pCanvas.FillRect(rect);
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.SetAnimate(value: Boolean);
begin
    // nothing to do?
    if (m_Animate = value) then
        Exit;

    m_Animate := value;

    DoUpdateAnimStatus;
    InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.SetEnabled(value: Boolean);
begin
    // nothing to do?
    if (m_Enabled = value) then
        Exit;

    m_Enabled := value;

    InvalidateAllTargets;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.SetAll(value: Boolean; styleClass, targetClass: TClass);
var
    count, i:   NativeInt;
    pComponent: TComponent;
    pTargets:   IWSmartPointer<IWTargets>;
    pTarget:    TWinControl;
    pItem:      IWSVGComponentStyleHook;
begin
    // value changed?
    if (value = m_All) then
        Exit;

    m_All := value;

    pTargets := TWSmartPointer<IWTargets>.Create();

    // copy the targets to invalidate on the end
    for pItem in m_pHooks do
        pTargets.Add(pItem.Target);

    // unhook and clear all previous targets
    m_pHooks.Clear;

    try
        // do enable style on all components?
        if (m_All) then
        begin
            // is owner defined?
            if (not Assigned(Owner)) then
                Exit;

            // get owner children count
            count := Owner.ComponentCount;

            // iterate htrough owner children
            for i := 0 to count - 1 do
            begin
                // get next component
                pComponent := Owner.Components[i];

                // found itself?
                if (pComponent = Self) then
                    continue;

                // is a style clas?
                if (pComponent.InheritsFrom(styleClass)) then
                    // disable the "all" property in this class
                    (pComponent as TWSVGComponentStyle).All := False
            end;

            // iterate htrough owner children again
            for i := 0 to count - 1 do
            begin
                // get next component
                pComponent := Owner.Components[i];

                // found itself?
                if (pComponent = Self) then
                    continue;

                // is a target class?
                if (pComponent.InheritsFrom(targetClass)) then
                begin
                    pTarget := pComponent as TWinControl;

                    // link it with the style
                    AddTarget(pTarget);

                    // remove the target from the previous target list to avoid to invalidate twice
                    pTargets.Remove(pTarget);
                end;
            end;
        end;
    except
        // assignment failed, unhook and clear all targets
        m_pHooks.Clear;
        raise;
    end;

    InvalidateAllTargets;

    // also invalidate removed targets
    for pTarget in pTargets do
        pTarget.Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.AddTarget(pTarget: TWinControl);
var
    pItem: IWSVGComponentStyleHook;
begin
    if (not Assigned(pTarget)) then
        Exit;

    // check if target already exists in list
    for pItem in m_pHooks do
        if (pItem.Target = pTarget) then
            Exit;

    pItem := nil;

    // hook the target control
    try
        pItem := IWSVGComponentStyleHook.Create(Self, pTarget);
        m_pHooks.Add(pItem);
        pItem := nil;
    finally
        pItem.Free;
    end;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.RemoveTarget(pTarget: TWinControl);
var
    pItem: IWSVGComponentStyleHook;
begin
    if (not Assigned(pTarget)) then
        Exit;

    // check if target exists in list
    for pItem in m_pHooks do
        if (pItem.Target = pTarget) then
        begin
            // unhook the target. NOTE the list will free the item so do it explicitly isn't required
            m_pHooks.Remove(pItem);
            Exit;
        end;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.InvalidateAllTargets;
var
    pItem: IWSVGComponentStyleHook;
begin
    if (not Assigned(m_pHooks)) then
        Exit;

    // invalidate all targets
    for pItem in m_pHooks do
        pItem.Target.Invalidate;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.RepaintAllTargets;
var
    pItem: IWSVGComponentStyleHook;
begin
    if (not Assigned(m_pHooks)) then
        Exit;

    // invalidate all targets
    for pItem in m_pHooks do
        pItem.Target.Repaint;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.Notification(pComponent: TComponent; operation: TOperation);
var
    pItem:   IWSVGComponentStyleHook;
    pTarget: TWinControl;
begin
    inherited Notification(pComponent, operation);

    // is target deleting?
    if (operation <> opRemove) then
        Exit;

    // check if target exists in list
    if (Assigned(m_pHooks)) then
        for pItem in m_pHooks do
            if (pItem.Target = pComponent) then
            begin
                pTarget := pItem.Target;

                // unhook the target. NOTE the list will free the item so do it explicitly isn't required
                m_pHooks.Remove(pItem);

                // notify that the target is removing
                OnFreeTarget(pTarget);

                // invalidate all remaining targets
                InvalidateAllTargets;
                Exit;
            end;
end;
//---------------------------------------------------------------------------
procedure TWSVGComponentStyle.ParentWndProc(var message: TMessage);
var
    pControl: TControl;
    handled:  Boolean;
begin
    {$IFDEF DEBUG}
        //TWLogHelper.LogToCompiler('ParentWndProc - ' + TWLogHelper.WinMsgToStr(Self, message));
    {$ENDIF}

    case (message.Msg) of
        CM_CONTROLLISTCHANGE:
        begin
            // all controls should be skinned?
            if (m_All) then
            begin
                // a new control was added?
                if (message.LParam = 1) then
                begin
                    // get it
                    pControl := TControl(message.WParam);

                    // successfully assigned?
                    if (Assigned(pControl)) then
                        DoAddTarget(pControl);
                end;
            end;
        end;

        WM_DPICHANGED:
        begin
            handled := False;

            // notify that DPI is changing
            if (Assigned(m_fOnSVGComponentStyleDPIChanged)) then
                handled := m_fOnSVGComponentStyleDPIChanged(m_PixelsPerInch, message.WParamLo);

            // update pixels per inch to match with the current context
            m_PixelsPerInch := message.WParamLo;

            // request a change in the component size
            if (not handled) then
                DoApplyDPIChange(m_RefPixelsPerInch, m_PixelsPerInch);
        end;
    end;

    if (Assigned(m_fOldParentWndProc)) then
        m_fOldParentWndProc(message);
end;
//---------------------------------------------------------------------------

end.
