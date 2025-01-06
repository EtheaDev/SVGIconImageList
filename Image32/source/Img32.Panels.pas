unit Img32.Panels;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Component that displays images on a TPanel descendant           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, SysUtils, Classes, Messages, Types, Graphics,
  Controls, Forms, ExtCtrls, Themes, uxTheme, Math, ShellApi, ClipBrd,
  Img32;

{$I Img32.inc}

const
  WM_MOUSEHWHEEL = $020E;

type
  TShowScrollBtns = (ssbFocused, ssAlways, ssNever);
  TPanelBkgType = (pbtSolidColor, pbtChessBoard);

  //TDrawImageEvent: template for TBaseImgPanel's OnDrawImage event property.
  //nb: with scaling, srcRect & dstRect may have different widths +/- heights.
  TDrawImageEvent = procedure (Sender: TObject;
    dstCanvas: TCanvas; const srcRect, dstRect: TRect) of Object;

  TFileDropEvent = procedure (Sender: TObject; const filename: string) of Object;

  //TPanelScrollbar: used internally by TBaseImgPanel and TImage32Panel
  TPanelScrollbar = record
    btnSize        : integer; //in dst coords
    btnDelta       : double;  //how much src moves for each px of the ScrollBar
    srcOffset      : integer; //offset in unscaled src coords
    maxSrcOffset   : double;  //max offset in unscaled src coords
    MouseOver      : Boolean;
    MouseDown      : Boolean;
    MouseDownPos   : integer;
  end;

  TBaseImgPanel = class(TPanel)
  private
    fImageSize      : TSize;
    fScale          : double;
    fScaleMin       : double;
    fScaleMax       : double;
    fFocusedColor   : TColor;
    fUnfocusedColor : TColor;
    fMouseDown      : Boolean;
    fScrollbarVert  : TPanelScrollbar;
    fScrollbarHorz  : TPanelScrollbar;
    fAutoCenter     : Boolean;
    fAllowZoom      : Boolean;
    fAllowKeyScroll : Boolean;
    fAllowScrnScroll: Boolean;
    fShowScrollBtns : TShowScrollBtns;
    fOnKeyDown      : TKeyEvent;
    fOnKeyUp        : TKeyEvent;
    fOnScrolling    : TNotifyEvent;
    fOnZooming      : TNotifyEvent;
    fOnMouseWheel   : TMouseWheelEvent;
{$IFDEF GESTURES}
    fLastDistance: integer;
    fLastLocation: TPoint;
{$ENDIF}
    fBkgType   : TPanelBkgType;
    fBkgChBrdColor1 : TColor32;
    fBkgChBrdColor2 : TColor32;
    fBkgChBrdSize : Integer;
    procedure UpdateOffsetDelta(resetOrigin: Boolean);
    function  GetMinScrollBtnSize: integer;
    function  GetDstOffset: TPoint;
    function  GetInnerMargin: integer;
    function  GetOffset: TPoint;
    procedure SetOffset(const value: TPoint);
    function  GetInnerClientRect: TRect;
    procedure SetScale(scale: double);
    procedure SetScaleMin(value: double);
    procedure SetScaleMax(value: double);
    function  GetColor: TColor;
    procedure SetColor(acolor: TColor);
    procedure SetAutoCenter(value: Boolean);
    procedure SetAllowZoom(value: Boolean);
    procedure SetShowScrollButtons(value: TShowScrollBtns);
    procedure SetBkgType(value : TPanelBkgType);
    procedure SetBkgChBrdColor1(value : TColor32);
    procedure SetBkgChBrdColor2(value : TColor32);
    procedure SetBkgChBrdSize(value : Integer);
{$IFDEF GESTURES}
    procedure Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
{$ENDIF}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseHWheel(var Message: TCMMouseWheel); message WM_MOUSEHWHEEL;
  protected
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseHWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DrawToPanelCanvas(const srcRect, dstRect: TRect); virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKey); message WM_KEYUP;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScaleToFit;
    function IsEmpty: Boolean;
    function IsScaledToFit: Boolean;
    function ClientToImage(const clientPt: TPoint): TPoint;
    function ImageToClient(const surfacePt: TPoint): TPoint;
    function RecenterImageAt(const imagePt: TPoint): Boolean;
    //ScaleAtPoint: zooms in or out keeping 'pt' stationary relative to display
    procedure ScaleAtPoint(scaleDelta: double; const pt: TPoint);
    property InnerClientRect: TRect read GetInnerClientRect;
    property InnerMargin: integer read GetInnerMargin;
    property Offset: TPoint read GetOffset write SetOffset;
    property ScrollbarHorz: TPanelScrollbar
      read fScrollbarHorz write fScrollbarHorz;
    property ScrollbarVert: TPanelScrollbar
      read fScrollbarVert write fScrollbarVert;
  published
    //AutoCenter: centers the image when its size is less than the display size
    property AutoCenter: Boolean read fAutoCenter write SetAutoCenter;
    property Color: TColor read GetColor write SetColor;
    //FocusedColor: colour of the border when the panel is focused
    property FocusedColor: TColor read fFocusedColor write fFocusedColor;
    property UnFocusedColor: TColor read fUnfocusedColor write fUnfocusedColor;
    //Scale: image scale (between ScaleMin and ScaleMax) if AllowZoom is enabled
    property Scale: double read fScale write SetScale;
    property ScaleMin: double read fScaleMin write SetScaleMin;
    property ScaleMax: double read fScaleMax write SetScaleMax;
    //ShowScrollButtons: defaults to ssbFocused (ie only when Panel has focus)
    property ShowScrollButtons : TShowScrollBtns
      read fShowScrollBtns write SetShowScrollButtons;
    property AllowKeyScroll: Boolean read fAllowKeyScroll write fAllowKeyScroll;
    property AllowScrnScroll: Boolean read fAllowScrnScroll write fAllowScrnScroll;
    property AllowZoom: Boolean read fAllowZoom write SetAllowZoom;
    //Hatched background option
    property BkgType        : TPanelBkgType read fBkgType        write SetBkgType;
    property BkgChBrdColor1 : TColor32      read fBkgChBrdColor1 write SetBkgChBrdColor1;
    property BkgChBrdColor2 : TColor32      read fBkgChBrdColor2 write SetBkgChBrdColor2;
    property BkgChBrdSize   : Integer       read fBkgChBrdSize   write SetBkgChBrdSize;
    //OnKeyDown: optional event for custom keyboard actions
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write fOnKeyUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnScrolling: TNotifyEvent read fOnScrolling write fOnScrolling;
    property OnZooming: TNotifyEvent read fOnZooming write fOnZooming;
  end;

  TImage32Panel = class(TBaseImgPanel)
  private
    fImage             : TImage32;
    fAllowCopy         : Boolean;
    fAllowPaste        : Boolean;
    fOnFileDrop        : TFileDropEvent;
    fAllowFileDrop     : Boolean;
    fOnCopy            : TNotifyEvent;
    fOnPaste           : TNotifyEvent;
    procedure SetAllowFileDrop(value: Boolean);
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure DrawToPanelCanvas(const srcRect, dstRect: TRect); override;
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  ClearImage;
    //CopyToImage: avoids a full redraw
    procedure  CopyToImage(srcImg: TImage32; const rec: TRect);
    function   CopyToClipboard: Boolean;
    function   PasteFromClipboard: Boolean;
    property Image: TImage32 read fImage;
  published
    property AllowCopy: Boolean read fAllowCopy write fAllowCopy;
    property AllowPaste: Boolean read fAllowPaste write fAllowPaste;
    property AllowFileDrop: Boolean
      read fAllowFileDrop write SetAllowFileDrop;
    property OnFileDrop: TFileDropEvent
      read fOnFileDrop write fOnFileDrop;
    property OnCopy: TNotifyEvent read fOnCopy write fOnCopy;
    property OnPaste: TNotifyEvent read fOnPaste write fOnPaste;
  end;

procedure Register;

implementation

uses
  Img32.Extra, Img32.Vector;

procedure Register;
begin
  RegisterComponents('Image32 Panels', [TImage32Panel]);
end;

type
  TNotifyImage32 = class(TImage32)
  protected
    fImage32Panel: TImage32Panel;
    procedure Changed; override;
  public
    constructor Create(owner: TImage32Panel);
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  //The minimum width for scrolling buttons. If borders are too narrow
  //to properly display scroll buttons then scroll buttons will be disabled.
  MinBorderWidth: integer = 0; //see initialization

const
  MinImageScale = 0.001;
  MaxImageScale = 1000;
  tolerance = 0.01;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function RgbColor(rgb: TColor): TColor;
var
  res: TARGB absolute Result;
begin
  if rgb < 0 then
    result := GetSysColor(rgb and $FFFFFF) else
    result := rgb;
end;

//------------------------------------------------------------------------------
function Size(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;
//------------------------------------------------------------------------------

procedure ScaleRect(var R: TRect; scale: double);
begin
  if scale = 1.0 then Exit;
  R.Left   := Round(R.Left * scale);
  R.Right  := Round(R.Right * scale);
  R.Top    := Round(R.Top * scale);
  R.Bottom := Round(R.Bottom * scale);
end;
//------------------------------------------------------------------------------

procedure InflateRect(var R: TRect; dx, dy: double);
begin
  R.Left   := Round(R.Left - dx);
  R.Right  := Round(R.Right + dx);
  R.Top    := Round(R.Top - dy);
  R.Bottom := Round(R.Bottom + dy);
end;
//------------------------------------------------------------------------------

procedure SetRectWidth(var rec: TRect; width: integer);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  rec.Right := rec.Left + width;
end;
//------------------------------------------------------------------------------

procedure SetRectHeight(var rec: TRect; height: integer);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  rec.Bottom := rec.Top + height;
end;
//------------------------------------------------------------------------------

function IsEmptyRect(const rec: TRect): Boolean;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (rec.Right <= rec.Left) or (rec.Bottom <= rec.Top);
end;
//------------------------------------------------------------------------------

function HasThemeManifest: boolean;
begin
  result := FindResource(hInstance, makeintresource(1), MakeIntResource(24)) > 0;
end;
//------------------------------------------------------------------------------

function GetThemeColor(const className: widestring;
  part, state, propID: integer; out Color: TColor): boolean;
var
  thmHdl: HTheme;
  clrRef: COLORREF ABSOLUTE Color;
begin
  result := false;
{$IFDEF STYLESERVICES}
  if not StyleServices.Enabled or not HasThemeManifest then exit;
{$ELSE}
  if not ThemeServices.ThemesEnabled or not HasThemeManifest then exit;
{$ENDIF}
  thmHdl := OpenThemeData(0, LPCWSTR(className));
  if thmHdl <> 0 then
  try
    result :=
      Succeeded(uxTheme.GetThemeColor(thmHdl, part, state, propID, clrRef));
  finally
    CloseThemeData(thmHdl);
  end;
end;
//------------------------------------------------------------------------------

function LeftMouseBtnDown: Boolean;
begin
  Result := (GetKeyState(VK_LBUTTON) shr 8 > 0);
end;
//------------------------------------------------------------------------------

function MakeDarker(color: TColor; percent: integer): TColor;
var
  hsl: THsl;
begin
  hsl := RgbToHsl(Color32(color));
  hsl.lum := ClampByte(hsl.lum - (percent/100 * hsl.lum));
  Result := HslToRgb(hsl) and $FFFFFF;
end;

//------------------------------------------------------------------------------
// TLayerNotifyImage32
//------------------------------------------------------------------------------

constructor TNotifyImage32.Create(owner: TImage32Panel);
begin
  inherited Create;
  fImage32Panel := owner;
end;
//------------------------------------------------------------------------------

procedure TNotifyImage32.Changed;
begin
  if (Self.UpdateCount = 0) then
    fImage32Panel.ImageChanged(Self);
  inherited;
end;

//------------------------------------------------------------------------------
// TBaseImgPanel
//------------------------------------------------------------------------------

constructor TBaseImgPanel.Create(AOwner: TComponent);
begin
  inherited;
  Height := 200;
  Width  := 200;
  {$IFnDEF FPC}
  {$IF COMPILERVERSION >= 17} //this is a guess
  ShowCaption := false;
  {$IFEND}
  {$ENDIF}
  BevelWidth := 1;
  BorderWidth := 12;
  BevelInner := bvLowered;
  DoubleBuffered := true;
  TabStop := true;
  {$IFDEF GESTURES}
  OnGesture := Gesture;
  Touch.InteractiveGestures := [igPressAndTap, igZoom, igPan];
  {$ENDIF}
  fShowScrollBtns := ssbFocused;
  fAllowScrnScroll := true;
  fAllowKeyScroll := true;
  fAllowZoom := true;
  fAutoCenter := true;
  fFocusedColor := RgbColor(clActiveCaption);
  fUnfocusedColor := clBtnFace;
  fScale := 1.0;
  fScaleMin := 0.05;
  fScaleMax := 20;
  fImageSize := Size(200,200);
  fBkgType        := pbtSolidColor;
  fBkgChBrdColor1 := clLiteBtn32;
  fBkgChBrdColor2 := clWhite32;
  fBkgChBrdSize   := 10;
end;
//------------------------------------------------------------------------------

destructor TBaseImgPanel.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.GetDstOffset: TPoint;
begin
  if not fAutoCenter then
    Result := Types.Point(0,0)
  else
    with GetInnerClientRect do
    begin
      Result.X := Max(0, ((Right -Left) -Round(fImageSize.cx * fScale)) div 2);
      Result.Y := Max(0, ((Bottom -Top) -Round(fImageSize.cy * fScale)) div 2);
    end;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.GetInnerMargin: integer;
begin
  //nb: BorderWidth is the space between outer and inner bevels
  Result := DpiAware(BorderWidth);
  if BevelInner <> bvNone then inc(result, BevelWidth);
  if BevelOuter <> bvNone then inc(result, BevelWidth);
  //BorderStyle changes the OUTSIDE of the panel so won't affect InnerMargin.
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.GetInnerClientRect: TRect;
var
  marg: integer;
begin
  marg := GetInnerMargin;
  result := ClientRect;
  InflateRect(result, -marg, -marg);
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.GetOffset: TPoint;
begin
  with fScrollbarHorz do Result.X := srcOffset;
  with fScrollbarVert do Result.Y := srcOffset;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetOffset(const value: TPoint);
begin
  fScrollbarHorz.srcOffset := value.X;
  fScrollbarVert.srcOffset := value.Y;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.IsEmpty: Boolean;
begin
  Result := (fImageSize.cx = 0) or (fImageSize.cy = 0);
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.IsScaledToFit: Boolean;
var
  rec: TRect;
  h,w: integer;
begin
  rec := GetInnerClientRect;
  h := RectHeight(rec); w := RectWidth(rec);
  Result := (abs(fImageSize.cx * fScale - w) < 1) or
    (abs(fImageSize.cy * fScale - h) < 1);
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.ScaleToFit;
var
  rec: TRect;
  h,w: integer;
begin
  if IsEmpty then Exit;
  //fScale := 1;
  fScrollbarHorz.srcOffset := 0;
  fScrollbarVert.srcOffset := 0;
  rec := GetInnerClientRect;
  h := RectHeight(rec); w := RectWidth(rec);
  if w / fImageSize.cx < h / fImageSize.cy then
    SetScale(w / fImageSize.cx) else
    SetScale(h / fImageSize.cy);
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.ScaleAtPoint(scaleDelta: double; const pt: TPoint);
var
  marg: integer;
  p,q: double;
  pt1, pt2: TPoint;
begin
  p := scaleDelta * fScale;
  if p < fScaleMin then
  begin
    if fScale <= fScaleMin then Exit;
    scaleDelta := fScaleMin/fScale;
  end else if p > fScaleMax then
  begin
    if fScale >= fScaleMax then Exit;
    scaleDelta := fScaleMax/fScale;
  end;
  q := 1 - 1/scaleDelta;
  marg := GetInnerMargin;
  pt1 := ClientToImage(pt);
  pt2 := ClientToImage(Types.Point(marg, marg));
  SetScale(fScale * scaleDelta);
  with fScrollbarHorz do
    inc(srcOffset, Round((pt1.X - pt2.X) * q));
  with fScrollbarVert do
    inc(srcOffset, Round((pt1.Y - pt2.Y) * q));
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetScale(scale: double);
begin
  if scale < fScaleMin then scale := fScaleMin
  else if scale > fScaleMax then scale := fScaleMax;
  if (fScale = scale) then Exit;
  fScale := scale;
  UpdateOffsetDelta(false);
  if Assigned(fOnZooming) then fOnZooming(Self);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetScaleMin(value: double);
begin
  fScaleMin := Max(MinImageScale, Min(fScaleMax, value));
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetScaleMax(value: double);
begin
  fScaleMax := Max(fScaleMin, Min(MaxImageScale, value));
end;
//------------------------------------------------------------------------------

function  TBaseImgPanel.GetColor: TColor;
begin
  Result := inherited Color;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetColor(acolor: TColor);
begin
  if inherited Color = acolor then Exit;
  ParentBackground := false;
  ParentColor := false;
  inherited Color := acolor
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetAutoCenter(value: Boolean);
begin
  if value = fAutoCenter then Exit;
  fAutoCenter := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetAllowZoom(value: Boolean);
begin
  if value = fAllowZoom then Exit;
  fAllowZoom := value;
  if not value then Exit;
  fAllowScrnScroll := true;
  fAllowKeyScroll := true;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.GetMinScrollBtnSize: integer;
begin
  Result := Max(1, GetInnerMargin - DpiAware(5));
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.UpdateOffsetDelta(resetOrigin: Boolean);
var
  innerRec: TRect;
  innerClientW, innerClientH, btnMin: integer;
  scaledW, scaledH: double;
begin
  //we need to determine 2 things:
  //  1. scroll button size
  //  2. how much a 1px button move moves the scaled image
  if (fImageSize.cx = 0) or (fImageSize.cy = 0) then Exit;
  btnMin        := GetMinScrollBtnSize;
  innerRec      := GetInnerClientRect;
  innerClientW  := innerRec.Right - innerRec.Left;
  innerClientH  := innerRec.Bottom - innerRec.Top;
  scaledW       := fImageSize.cx * fScale;
  scaledH       := fImageSize.cy * fScale;
  with fScrollbarVert do
  begin
    if resetOrigin then srcOffset := 0;
    if (scaledH < innerClientH + tolerance) then //no scroll button needed
    begin
      btnSize := 0; btnDelta := 0; maxSrcOffset := 0;
    end else
    begin
      btnSize := Max(btnMin, Round(innerClientH * innerClientH / scaledH));
      maxSrcOffset := (scaledH - innerClientH) / fScale;
      btnDelta := (innerClientH - btnSize) / maxSrcOffset;
    end;
  end;
  with fScrollbarHorz do
  begin
    if resetOrigin then srcOffset := 0;
    if (scaledW < innerClientW + tolerance) then  //no scroll button needed
    begin
      btnSize := 0; btnDelta := 0; maxSrcOffset := 0;
    end else
    begin
      btnSize := Max(btnMin, Round(innerClientW * innerClientW / scaledW));
      maxSrcOffset := (scaledW - innerClientW) / fScale;
      btnDelta := (innerClientW - btnSize) / maxSrcOffset;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Self.Font);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetShowScrollButtons(value: TShowScrollBtns);
begin
  if value = fShowScrollBtns then Exit;
  fShowScrollBtns := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetBkgType(value : TPanelBkgType);
begin
  if value = fBkgType then Exit;
  fBkgType := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetBkgChBrdColor1(value : TColor32);
begin
  if value = fBkgChBrdColor1 then Exit;
  fBkgChBrdColor1 := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetBkgChBrdColor2(value : TColor32);
begin
  if value = fBkgChBrdColor2 then Exit;
  fBkgChBrdColor2 := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.SetBkgChBrdSize(value : Integer);
begin
  if value = fBkgChBrdSize then Exit;
  fBkgChBrdSize := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.ClientToImage(const clientPt: TPoint): TPoint;
var
  marg: integer;
  pt: TPoint;
begin
  pt := GetDstOffset;
  marg := GetInnerMargin;
  Result.X := Round((clientPt.X -pt.X -marg)/fScale) +fScrollbarHorz.srcOffset;
  Result.Y := Round((clientPt.Y -pt.Y -marg)/fScale) +fScrollbarVert.srcOffset;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.ImageToClient(const surfacePt: TPoint): TPoint;
var
  marg: integer;
  pt: TPoint;
begin
  pt := GetDstOffset;
  marg := GetInnerMargin;
  Result.X := Round((surfacePt.X -fScrollbarHorz.srcOffset)*fScale +marg +pt.X);
  Result.Y := Round((surfacePt.Y -fScrollbarVert.srcOffset)*fScale +marg +pt.Y);
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.RecenterImageAt(const imagePt: TPoint): Boolean;
var
  scaledW, scaledH: Double;
  marg, innerW, innerH: Integer;
  pt1, pt2: TPoint;
  q, maxOffset: double;
begin
  Result := (fScrollbarHorz.maxSrcOffset > 0) or
    (fScrollbarVert.maxSrcOffset = 0);
  if not Result then Exit;
  scaledW := fImageSize.cx * fScale;
  scaledH := fImageSize.cy * fScale;
  marg := GetInnerMargin;
  innerW := ClientWidth - marg*2;
  innerH := ClientHeight - marg*2;
  pt1 := imagePt;
  pt2 := ClientToImage(Types.Point(marg + innerW div 2, marg + innerH div 2));
  with fScrollbarHorz do
  begin
    q := (pt1.X - pt2.X);
    maxOffset := (scaledW - innerW) / fScale;
    srcOffset := Round(Max(0,Min(maxOffset, q)));
  end;
  with fScrollbarVert do
  begin
    q := (pt1.Y - pt2.Y);
    maxOffset := (scaledH - innerH) / fScale;
    srcOffset := Round(Max(0,Min(maxOffset, q)));
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rec: TRect;
begin
  fMouseDown := true;
  if fAllowScrnScroll or fAllowKeyScroll then
  begin
    fScrollbarHorz.MouseDownPos := X;
    fScrollbarVert.MouseDownPos := Y;
    rec := GetInnerClientRect;
    if (X > rec.Right) and (Y > rec.Top) and (Y < rec.Bottom) and
      (fScrollbarVert.btnSize > 0) then
    begin
      fScrollbarVert.MouseDown := true;
    end
    else if (Y > rec.Bottom)  and (X > rec.Left) and (X < rec.Right) and
      (fScrollbarHorz.btnSize > 0) then
    begin
      fScrollbarHorz.MouseDown := true;
    end;
  end;
  if not (fScrollbarHorz.MouseDown or fScrollbarVert.MouseDown) then
    inherited;
  if TabStop and not Focused and CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  rec: TRect;
  inDrawRegion: Boolean;
begin
  rec := GetInnerClientRect;
  inDrawRegion := Windows.PtInRect(rec, Types.Point(X,Y));
  if inDrawRegion and
    not (fScrollbarHorz.MouseDown or fScrollbarVert.MouseDown) then
  begin
    if fScrollbarVert.MouseOver or fScrollbarHorz.MouseOver then
    begin
      Invalidate;
      fScrollbarHorz.MouseOver := false;
      fScrollbarVert.MouseOver := false;
    end;
    cursor := crDefault;
    inherited;
    Exit;
  end;
  if not fMouseDown or
    not (fAllowScrnScroll or fAllowKeyScroll) then
  begin
    if (BorderWidth >= MinBorderWidth) and
      ((fShowScrollBtns = ssAlways) or
        (focused and (fShowScrollBtns = ssbFocused))) then
    begin
      if (X >= rec.Right) and (fScrollbarVert.btnSize > 0) then
      begin
        if (Y < rec.Bottom) then
        begin
          cursor := crSizeNS;
          if not fScrollbarVert.MouseOver then Invalidate;
          fScrollbarVert.MouseOver := true;
        end else
          cursor := crDefault;
      end
      else if (Y >= rec.Bottom) and (fScrollbarHorz.btnSize > 0) then
      begin
        Cursor := crSizeWE;
        if not fScrollbarHorz.MouseOver then Invalidate;
        fScrollbarHorz.MouseOver := true;
      end else
        cursor := crDefault;
    end;
    Exit;
  end;
  fScrollbarHorz.MouseOver := false;
  fScrollbarVert.MouseOver := false;
  if not (fAllowScrnScroll or fAllowKeyScroll) then Exit;
  if fScrollbarVert.MouseDown then
  begin
    //dragging vertical scrollbar
    with fScrollbarVert do
    begin
      inc(srcOffset, Round((Y - MouseDownPos) / btnDelta));
      MouseDownPos := Y;
    end;
  end
  else if fScrollbarHorz.MouseDown then
  begin
    //dragging horizontal scrollbar
    with fScrollbarHorz do
    begin
      inc(srcOffset, Round((X - MouseDownPos) / btnDelta));
      MouseDownPos := X;
    end;
  end else if fAllowScrnScroll then
  begin
    //click and drag the drawing image
    with fScrollbarVert do if btnDelta > 0 then
    begin
      dec(srcOffset, Round((Y - MouseDownPos) / fScale));
      MouseDownPos := Y;
    end;
    with fScrollbarHorz do if btnDelta > 0 then
    begin
      dec(srcOffset, Round((X - MouseDownPos) / fScale));
      MouseDownPos := X;
    end;
  end else
  begin
    Exit; //ie exit here if NOT scrolling
  end;
  if assigned(fOnScrolling) then fOnScrolling(self);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not fMouseDown then Exit;
  inherited;
  fMouseDown := false;
  fScrollbarHorz.MouseDown := false;
  fScrollbarVert.MouseDown := false;
  Invalidate;
end;
//------------------------------------------------------------------------------


procedure TBaseImgPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if fScrollbarHorz.MouseOver then
    fScrollbarHorz.MouseOver := false
  else if fScrollbarVert.MouseOver then
    fScrollbarVert.MouseOver := false
  else
    Exit;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.WMEraseBkgnd(var message: TMessage);
begin
  message.Result := 0; //ie don't bother erasing background
end;
//------------------------------------------------------------------------------

type TWinControl = class(Controls.TWinControl); //access protected Color property

procedure TBaseImgPanel.DrawToPanelCanvas(const srcRect, dstRect: TRect);
begin
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.Paint;

  procedure DrawChessBoard(rec: TRect);
  var
    tImage : TImage32;
  begin
    tImage := TImage32.Create(RectWidth(rec), RectHeight(rec));
    try
      HatchBackground(timage,
        fBkgChBrdColor1, fBkgChBrdColor2, DpiAware(fBkgChBrdSize));
      tImage.CopyToDc(rec, rec, canvas.Handle);
    finally
      tImage.Free;
    end;
  end;

  procedure DrawFrame(rec: TRect; tlColor, brColor: TColor; width: integer);
  var
    bl, tr: TPoint;
  begin
    dec(rec.Right); dec(rec.Bottom);
    Canvas.Pen.Width := 1;
    while width > 0 do
    begin
      tr := Types.Point(rec.Right, rec.Top);
      bl := Types.Point(rec.Left, rec.Bottom);
      Canvas.Pen.Color := tlColor;
      Canvas.PolyLine([bl, rec.TopLeft, tr]);
      Canvas.Pen.Color := brColor;
      Canvas.PolyLine([tr, rec.BottomRight, bl]);
      InflateRect(rec, integer(-1), integer(-1));
      dec(width);
    end;
  end;

  procedure DrawScrollButton(const rec: TRect);
  begin
    Canvas.FillRect(rec);
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(rec.Left, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Top);
    Canvas.LineTo(rec.Right, rec.Top);
    Canvas.Pen.Color := cl3DDkShadow;
    Canvas.LineTo(rec.Right, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Bottom);
  end;

var
  marg, btnMin, dpiAwareBW: integer;
  tmpRec, innerRec, srcRec, dstRec: TRect;
  backgroundPainted: Boolean;
  pt: TPoint;
begin
  //calculate un-scaled source rectangle that corresponds with dstRec
  marg := GetInnerMargin;
  innerRec := GetInnerClientRect;
  dpiAwareBW := DpiAware(BorderWidth);
  dstRec := innerRec;
  srcRec := dstRec;
  TranslateRect(srcRec, -marg, -marg);
  ScaleRect(srcRec, 1/fScale);
  //if the scaled drawing is smaller than InnerClientRect then center it
  pt := GetDstOffset;
  if pt.X > 0 then
  begin
    inc(dstRec.Left, pt.X); dec(dstRec.Right, pt.X);
    fScrollbarHorz.srcOffset := 0;
    srcRec.Left := 0;
    srcRec.Right := fImageSize.cx;
  end;
  if pt.Y > 0 then
  begin
    inc(dstRec.Top, pt.Y); dec(dstRec.Bottom, pt.Y);
    fScrollbarVert.srcOffset := 0;
    srcRec.Top := 0;
    srcRec.Bottom := fImageSize.cy;
  end;
  //calc offsets
  with fScrollbarHorz do
    if (srcOffset < 0) or (btnSize = 0) then srcOffset := 0;
  with fScrollbarVert do
    if (srcOffset < 0) or (btnSize = 0) then srcOffset := 0;
  if fScrollbarVert.srcOffset > fScrollbarVert.maxSrcOffset then
    fScrollbarVert.srcOffset := Round(fScrollbarVert.maxSrcOffset);
  if fScrollbarHorz.srcOffset > fScrollbarHorz.maxSrcOffset then
    fScrollbarHorz.srcOffset := Round(fScrollbarHorz.maxSrcOffset);
  TranslateRect(srcRec, fScrollbarHorz.srcOffset, fScrollbarVert.srcOffset);
  //paint innerRec background
  backgroundPainted := ParentBackground and
  {$IFDEF STYLESERVICES}
    StyleServices.Enabled and (seClient in StyleElements) and
  {$ELSE}
    ThemeServices.ThemesEnabled and
  {$ENDIF}
    Succeeded(DrawThemeParentBackground(Handle, Canvas.Handle, @innerRec));
  if (csDesigning in ComponentState) or not backgroundPainted then
  begin
    if ParentColor then
      Canvas.Brush.Color := TWinControl(parent).Color else
      Canvas.Brush.Color := self.Color;
    Canvas.FillRect(innerRec);
  end;
  // draw cheesboard
  if fBkgType = pbtChessBoard then
  begin
    DrawChessBoard(innerRec);
  end;
  //draw the image
  DrawToPanelCanvas(srcRec, dstRec);
  //prevent recursive paints (in case Invalidate etc called in fOnDrawImage)
  RedrawWindow(Handle, nil, 0, RDW_NOERASE or RDW_NOINTERNALPAINT or RDW_VALIDATE);
  //Exit;//////////////////
  //paint the outer bevel
  tmpRec := ClientRect;
  case BevelOuter of
    bvLowered: DrawFrame(tmpRec, clBtnShadow, clBtnHighlight, BevelWidth);
    bvRaised:  DrawFrame(tmpRec, clBtnHighlight, clBtnShadow, BevelWidth);
  end;
  //paint the border
  InflateRect(tmpRec, integer(-BevelWidth), integer(-BevelWidth));
  if Focused then
    DrawFrame(tmpRec, fFocusedColor, fFocusedColor, dpiAwareBW)
  else
    DrawFrame(tmpRec, fUnfocusedColor, fUnfocusedColor, dpiAwareBW);
  InflateRect(tmpRec, integer(-dpiAwareBW), integer(-dpiAwareBW));
  //paint the inner bevel
  case BevelInner of
    bvLowered: DrawFrame(tmpRec, clBtnShadow, clBtnHighlight, BevelWidth);
    bvRaised:  DrawFrame(tmpRec, clBtnHighlight, clBtnShadow, BevelWidth);
  end;
  if (BorderWidth >= MinBorderWidth) and
    (fAllowScrnScroll or fAllowKeyScroll) and
    ((fShowScrollBtns = ssAlways) or
      (Focused and (fShowScrollBtns = ssbFocused))) then
  begin
    btnMin := GetMinScrollBtnSize;
    //draw vertical scrollbar
    with fScrollbarVert do
      if (btnSize > 0) then
      begin
        tmpRec.Top := marg + Round(srcOffset * btnDelta);
        tmpRec.Bottom := tmpRec.Top + btnSize;
        tmpRec.Right := ClientWidth - DpiAware(3);
        tmpRec.Left := tmpRec.Right - btnMin;
        if MouseOver or MouseDown then Canvas.Brush.Color := clHotLight
        else if Focused then Canvas.Brush.Color := MakeDarker(fFocusedColor, 20)
        else Canvas.Brush.Color := MakeDarker(Color, 20);
        DrawScrollButton(tmpRec);
      end;
    //draw horizontal scrollbar
    with fScrollbarHorz do
      if (btnSize > 0) then
      begin
        tmpRec.Left := marg + Round(srcOffset * btnDelta);
        tmpRec.Right := tmpRec.Left + btnSize;
        tmpRec.Bottom := ClientHeight - DpiAware(3);
        tmpRec.Top := tmpRec.Bottom - btnMin;
        if MouseOver or MouseDown then Canvas.Brush.Color := clHotLight
        else if Focused then Canvas.Brush.Color := MakeDarker(fFocusedColor, 20)
        else Canvas.Brush.Color := MakeDarker(Color, 20);
        DrawScrollButton(tmpRec);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.Resize;
begin
  UpdateOffsetDelta(true);
  inherited;
end;
//------------------------------------------------------------------------------

{$IFDEF GESTURES}
procedure TBaseImgPanel.Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  p: double;
begin
  inherited;
  case EventInfo.GestureID of
    igiZoom:
      begin
        if not fAllowZoom then Exit;
        if not (gfBegin in EventInfo.Flags) then
        begin
          p := 1 + (EventInfo.Distance - FLastDistance)/fImageSize.cx;
          ScaleAtPoint(p, EventInfo.Location);
        end;
        FLastDistance := EventInfo.Distance;
        Handled := true;
      end;
    igiPan:
      begin
        if not (fAllowScrnScroll or fAllowKeyScroll) then Exit;
        if not (gfBegin in EventInfo.Flags) then
        begin
          with fScrollbarHorz do
            inc(srcOffset,
              Round((FLastLocation.X - EventInfo.Location.X) * btnDelta));
          with fScrollbarVert do
            inc(srcOffset,
              Round((FLastLocation.Y - EventInfo.Location.Y) * btnDelta));
          Invalidate;
        end;
        FLastLocation := EventInfo.Location;
        if assigned(fOnScrolling) then fOnScrolling(self);
        Handled := true;
      end;
  end;
end;
//------------------------------------------------------------------------------

{$ENDIF}
function TBaseImgPanel.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  isZooming: Boolean;
begin
  Result := false;
  if Assigned(FOnMouseWheel) then
    fOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then Exit;
  isZooming := (ssCtrl in Shift) and fAllowZoom;
  if not isZooming and not (fAllowScrnScroll or fAllowKeyScroll) then Exit;
  {$IFNDEF FPC}
  MousePos := ScreenToClient(MousePos);
  {$ENDIF}
  if isZooming then
  begin
    if WheelDelta > 0 then
      ScaleAtPoint(1.1, MousePos) else
      ScaleAtPoint(0.9, MousePos);
  end else
  begin
    dec(fScrollbarVert.srcOffset, Round(WheelDelta / fScale));
    Invalidate;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.WMMouseHWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    if DoMouseHWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1 else
      Message.Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function TBaseImgPanel.DoMouseHWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := focused and (fAllowScrnScroll or fAllowKeyScroll);
  if not Result then Exit;
  {$IFNDEF FPC}
  MousePos := ScreenToClient(MousePos);
  {$ENDIF}
  inc(fScrollbarHorz.srcOffset, Round(WheelDelta / fScale));
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if not TabStop then Exit;
  Message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.WMKeyDown(var Message: TWMKey);
var
  mul: integer;
  midPoint: TPoint;
  charCode: Word;
  shiftState: TShiftState;
begin
  inherited;
  shiftState := KeyDataToShiftState(Message.KeyData);
  if Assigned(fOnKeyDown) then
  begin
    charCode := Message.CharCode;
    fOnKeyDown(Self, charCode, shiftState);
    if charCode = 0 then Exit;
  end;
  if not fAllowZoom and not fAllowKeyScroll then Exit;
  case Message.CharCode of
    VK_LEFT..VK_DOWN:
      begin
        if ssCtrl in shiftState then
        begin
          if not fAllowZoom then Exit;
          //zoom in and out with CTRL+UP and CTRL+DOWN respectively
          midPoint := Types.Point(ClientWidth div 2, ClientHeight div 2);
          case Message.CharCode of
            VK_UP: ScaleAtPoint(1.1, midPoint);
            VK_DOWN: ScaleAtPoint(0.9, midPoint);
            else Exit;
          end;
        end else
        begin
          if not fAllowKeyScroll then Exit;
          //otherwise scroll the image with the arrow keys
          if ssShift in shiftState then
            mul := 5 else //ie scrolls 5 times faster with Shift key down
            mul := 1;
          case Message.CharCode of
            VK_LEFT:
              with fScrollbarHorz do
                dec(srcOffset, 5 * mul);
            VK_RIGHT:
              with fScrollbarHorz do
                inc(srcOffset, 5 * mul);
            VK_UP:
              with fScrollbarVert do
                dec(srcOffset, 5 * mul);
            VK_DOWN:
              with fScrollbarVert do
                inc(srcOffset, 5 * mul);
          end;
          if assigned(fOnScrolling) then fOnScrolling(self);
        end;
        Invalidate;
      end;
      Ord('0'):
        if fAllowZoom and not (ssCtrl in shiftState) then
        begin
          ScaleToFit;
        end;
      Ord('1')..Ord('9'): if not (ssCtrl in shiftState) then
        begin
          if not AllowZoom then Exit
          else if ssShift in shiftState then
            SetScale((Message.CharCode - Ord('0')) /10)
          else
            SetScale(Message.CharCode - Ord('0'));
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TBaseImgPanel.WMKeyUp(var Message: TWMKey);
var
  charCode: Word;
  shiftState: TShiftState;
begin
  if Assigned(fOnKeyUp) then
  begin
    shiftState := KeyDataToShiftState(Message.KeyData);
    charCode := Message.CharCode;
    fOnKeyUp(Self, charCode, shiftState);
    if charCode = 0 then Exit;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// TBitmapPanel
//------------------------------------------------------------------------------

constructor TImage32Panel.Create(AOwner: TComponent);
begin
  inherited;
  Color := clWhite;
  fImage := TNotifyImage32.Create(Self);
  fImage.Resampler := rBicubicResampler;
  fImage.SetSize(200,200);
  fAllowCopy := true;
  fAllowPaste := true;
end;
//------------------------------------------------------------------------------

destructor TImage32Panel.Destroy;
begin
  if fAllowFileDrop and HandleAllocated then
    DragAcceptFiles(Handle, False);
  fImage.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.ImageChanged(Sender: TObject);
begin
  if (fImageSize.cx <> Image.Width) or (fImageSize.cy <> Image.Height) then
  begin
    fImageSize.cx := Image.Width;
    fImageSize.cy := Image.Height;
    fScale := 1.0;
    UpdateOffsetDelta(true);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.DrawToPanelCanvas(const srcRect, dstRect: TRect);
begin
  fImage.CopyToDc(srcRect, dstRect, canvas.Handle);
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.ClearImage;
begin
  fImage.Clear;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.CopyToImage(srcImg: TImage32; const rec: TRect);
var
  srcRect, dstRect: TRect;
begin
  fImage.BlockNotify;
  try
    dstRect.TopLeft := ImageToClient(rec.TopLeft);
    dstRect.BottomRight := ImageToClient(rec.BottomRight);
    fImage.Copy(srcImg, rec, rec);
    Types.IntersectRect(dstRect, dstRect, InnerClientRect);
    srcRect.TopLeft := ClientToImage(dstRect.TopLeft);
    srcRect.BottomRight := ClientToImage(dstRect.BottomRight);
    fImage.CopyToDc(srcRect, dstRect, canvas.Handle);
  finally
    fImage.UnblockNotify;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.CreateWnd;
begin
  inherited;
  if fAllowFileDrop then
    DragAcceptFiles(Handle, True);
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.DestroyWnd;
begin
  if fAllowFileDrop then
  begin
    DragAcceptFiles(Handle, False);
    fAllowFileDrop := false;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.SetAllowFileDrop(value: Boolean);
begin
  if (fAllowFileDrop = value) then Exit;
  if not (csDesigning in ComponentState) and HandleAllocated then
  begin
    if fAllowFileDrop then
      DragAcceptFiles(Handle, false) else
      DragAcceptFiles(Handle, true);
  end;
  fAllowFileDrop := value;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  filenameLen: Integer;
  filename: string;
begin
  Msg.Result := 0;
  hDrop:= Msg.wParam;
  filenameLen := DragQueryFile(hDrop, 0, nil, 0);
  SetLength(filename, filenameLen);
  DragQueryFile(hDrop, 0, Pointer(filename), filenameLen+1);
  DragFinish(hDrop);
  if assigned(fOnFileDrop) then fOnFileDrop(Self, filename)
  else if (Lowercase(ExtractFileExt(filename)) = '.bmp') then
  try
    fImage.LoadFromFile(filename);
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32Panel.WMKeyDown(var Message: TWMKey);
var
  shiftState: TShiftState;
begin
  inherited;
  shiftState := KeyDataToShiftState(Message.KeyData);
  case Message.CharCode of
    Ord('C'):
      if (ssCtrl in shiftState) and fAllowCopy and
        not fImage.IsEmpty and
        fImage.CopyToClipboard and
        assigned(fOnCopy) then
          fOnCopy(Self);
    Ord('V'):
      if (ssCtrl in shiftState) and fAllowPaste and
        fImage.CanPasteFromClipBoard and
        fImage.PasteFromClipboard and
        assigned(fOnPaste) then
          fOnPaste(self);
  end;
end;
//------------------------------------------------------------------------------

function TImage32Panel.CopyToClipboard: Boolean;
begin
  Result := fAllowCopy and Image.CopyToClipBoard;
end;
//------------------------------------------------------------------------------

function TImage32Panel.PasteFromClipboard: Boolean;
begin
  Result := fAllowPaste and Image.PasteFromClipBoard;
  if Result and assigned(fOnPaste) then fOnPaste(self);
end;
//------------------------------------------------------------------------------

initialization
  MinBorderWidth := 10;
end.
