unit Img32.Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.2                                                             *
* Date      :  19 August 2021                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Layer support for the Image32 library                           *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, {$ENDIF}
  Img32, Img32.Draw, Img32.Extra, Img32.Vector, Img32.Transform;

type
  TSizingStyle = (ssCorners, ssEdges, ssEdgesAndCorners);
  TButtonShape = Img32.Extra.TButtonShape;

  TLayer32 = class;
  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;
  TGroupLayer32 = class;

  TLayerHitTestEvent =
    function (layer: TLayer32; const pt: TPoint): Boolean of Object;

  TArrayOfPointer = array of Pointer;

  //THitTestRec is used for hit-testing (see TLayeredImage32.GetLayerAt).
  THitTestRec = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    PtrPixels : TArrayOfPointer;
    width  : integer;
    height : integer;
    function IsEmpty: Boolean;
    procedure Init(ownerLayer: TLayer32);
    procedure Clear;
  end;

  TLayerNotifyImage32 = class(TImage32)
  protected
    fOwnerLayer: TLayer32;
    procedure Changed; override;
  public
    constructor Create(owner: TLayer32);
  end;

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

  TLayer32 = class  //base layer class (rarely if ever instantiated)
  private
    fLayeredImage   : TLayeredImage32;
    fGroupOwner     : TGroupLayer32;
    fLeft           : integer;
    fTop            : integer;
    fImage          : TImage32;
    fName           : string;
    fIndex          : integer;
    fVisible        : Boolean;
    fOpacity        : Byte;
    fCursorId       : integer;
    fUserData       : TObject;
    fBlendFunc      : TBlendFunction;
    fOldBounds      : TRect;    //bounds at last layer merge
    fRefreshPending : boolean;
    function   GetMidPoint: TPointD;
    procedure  SetVisible(value: Boolean);
    function   GetHeight: integer;
    function   GetWidth: integer;
    procedure  SetBlendFunc(func: TBlendFunction);
  protected
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate;   virtual;
    function   CanUpdate: Boolean;
    function   GetBounds: TRect;
    procedure  SetOpacity(value: Byte); virtual;
    procedure  ImageChanged(Sender: TImage32); virtual;
  public
    constructor Create(groupOwner: TGroupLayer32;
      const name: string = ''); virtual;
    destructor Destroy; override;
    procedure  SetSize(width, height: integer);

    function   BringForwardOne: Boolean;
    function   SendBackOne: Boolean;
    function   BringToFront: Boolean;
    function   SendToBack: Boolean;
    function   Move(newGroupOwner: TGroupLayer32; idx: integer): Boolean;

    procedure  PositionAt(const pt: TPoint); overload;
    procedure  PositionAt(x, y: integer); overload;
    procedure  PositionCenteredAt(X, Y: integer); overload;
    procedure  PositionCenteredAt(const pt: TPoint); overload;
    procedure  PositionCenteredAt(const pt: TPointD); overload;
    procedure  SetBounds(const newBounds: TRect); virtual;
    procedure  Invalidate(rec: TRect); virtual;

    procedure  Offset(dx, dy: integer); virtual;
    property   Bounds: TRect read GetBounds;
    property   CursorId: integer read fCursorId write fCursorId;
    property   GroupOwner: TGroupLayer32 read fGroupOwner;
    property   Height: integer read GetHeight;
    property   Image: TImage32 read fImage;
    property   Index: integer read fIndex;
    property   Left: integer read fLeft;
    property   MidPoint: TPointD read GetMidPoint;
    property   Name: string read fName write fName;
    property   Opacity: Byte read fOpacity write SetOpacity;
    property   RootOwner: TLayeredImage32 read fLayeredImage;
    property   Top: integer read fTop;
    property   Visible: Boolean read fVisible write SetVisible;
    property   Width: integer read GetWidth;
    property   UserData: TObject read fUserData write fUserData;
    property   BlendFunc: TBlendFunction read fBlendFunc write SetBlendFunc;
  end;

  TUpdateType = (utUndefined, utShowDesigners, utHideDesigners);

  TGroupLayer32 = class(TLayer32) //container class for other layers
  private
{$IFDEF XPLAT_GENERICS}
    fChilds                 : TList<TLayer32>;
{$ELSE}
    fChilds                 : TList;
{$ENDIF}
    fLastUpdateType         : TUpdateType;
    fInvalidRect            : TRect;
    fUpdateCount            : Integer; //see beginUpdate/EndUpdate
    fClipPath               : TPathD;
    function  GetChildCount: integer;
    function  GetChild(index: integer): TLayer32;
    function  FindLayerNamed(const name: string): TLayer32; virtual;
    procedure ReindexChildsFrom(startIdx: Integer);
  protected
    procedure  BeginUpdate; override;
    procedure  EndUpdate;   override;
    procedure  RefreshPending;
    procedure  SetOpacity(value: Byte); override;
    procedure PreMerge(hideDesigners, forceRefresh: Boolean);
    procedure Merge(hideDesigners: Boolean; const staleRect: TRect);
    function  GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean): TLayer32;
    procedure InternalDeleteChild(index: integer; fromChild: Boolean);
  public
    constructor Create(groupOwner: TGroupLayer32; const name: string = ''); override;
    destructor Destroy; override;
    function   AddChild(layerClass: TLayer32Class; const name: string = ''): TLayer32;
    function   InsertChild(layerClass: TLayer32Class; index: integer; const name: string = ''): TLayer32;
    procedure  DeleteChild(index: integer);
    procedure  Invalidate(rec: TRect); override;
    procedure  Offset(dx, dy: integer); override;
    procedure  ClearChildren;

    property   ChildCount: integer read GetChildCount;
    property   Child[index: integer]: TLayer32 read GetChild; default;
    property   ClipPath: TPathD read fClipPath write fClipPath;
  end;

  THitTestLayer32 = class(TLayer32) //abstract classs
  private
    fHitTestRec     : THitTestRec;
  protected
    procedure  ImageChanged(Sender: TImage32); override;
    property   HitTestRec : THitTestRec read fHitTestRec write fHitTestRec;
  public
    function GetPathsFromHitTestMask: TPathsD;
    procedure ClearHitTesting;
  end;

  TRotateLayer32 = class(THitTestLayer32) //abstract rotating layer class
  private
    fAngle      : double;
    fPivotPt    : TPointD;
    fAutoPivot  : Boolean;
    procedure SetAngle(newAngle: double);
    function  GetPivotPt: TPointD;
    procedure SetAutoPivot(val: Boolean);
  protected
    procedure SetPivotPt(const pivot: TPointD); virtual;
  public
    constructor Create(groupOwner: TGroupLayer32; const name: string = ''); override;
    procedure Rotate(angleDelta: double); virtual;
    procedure ResetAngle;
    procedure Offset(dx, dy: integer); override;
    property  Angle: double read fAngle write SetAngle;
    property  PivotPt: TPointD read GetPivotPt write SetPivotPt;
    property  AutoPivot: Boolean read fAutoPivot write SetAutoPivot;
  end;

  TVectorLayer32 = class(TRotateLayer32) //display layer for vector images
  private
    fPaths      : TPathsD;
    fMargin     : integer;
    fOnDraw     : TNotifyEvent;
    procedure SetMargin(new: integer);
    procedure RepositionAndDraw;
    procedure SetPaths(const newPaths: TPathsD);
  protected
    procedure Draw; virtual;
  public
    constructor Create(groupOwner: TGroupLayer32; const name: string = ''); override;
    procedure SetBounds(const newBounds: TRect); override;
    procedure Offset(dx,dy: integer); override;
    procedure Rotate(angleDelta: double); override;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD;
      fillRule: TFillRule); virtual;
    property  Paths: TPathsD read fPaths write SetPaths;
    property  Margin: integer read fMargin write SetMargin;
    property  OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
  end;

  TRasterLayer32 = class(TRotateLayer32) //display laer for raster images
  private
    fMasterImg    : TImage32;
    //a matrix allows the combining any number of sizing & rotating
    //operations into a single transformation
    fMatrix       : TMatrixD;
    fRotating     : Boolean;
    fSavedMidPt   : TPointD;
    fSavedSize    : TSize;
    fAutoHitTest  : Boolean;
    procedure DoAutoHitTest;
    procedure DoPreScaleCheck;
    procedure DoPreRotationCheck;
    function  GetMatrix: TMatrixD;
  protected
    procedure ImageChanged(Sender: TImage32); override;
    procedure SetPivotPt(const pivot: TPointD); override;
  public
    constructor Create(groupOwner: TGroupLayer32; const name: string = ''); override;
    destructor  Destroy; override;
    procedure Offset(dx,dy: integer); override;
    procedure UpdateHitTestMaskOpaque;
    procedure UpdateHitTestMaskTransparent; overload; virtual;
    procedure UpdateHitTestMaskTransparent(compareFunc: TCompareFunction;
      referenceColor: TColor32; tolerance: integer); overload; virtual;
    procedure SetBounds(const newBounds: TRect); override;
    procedure Rotate(angleDelta: double); override;

    property  AutoSetHitTestMask: Boolean read fAutoHitTest write fAutoHitTest;
    property  MasterImage: TImage32 read fMasterImg;
    property  Matrix: TMatrixD read GetMatrix;
  end;

  TDesignerLayer32 = class;
  TButtonDesignerLayer32 = class;
  TButtonDesignerLayer32Class = class of TButtonDesignerLayer32;

  TSizingGroupLayer32 = class(TGroupLayer32) //groups sizing buttons
  private
    fSizingStyle: TSizingStyle;
  public
    property SizingStyle: TSizingStyle read fSizingStyle write fSizingStyle;
  end;

  TRotatingGroupLayer32 = class(TGroupLayer32) //groups rotation buttons
  private
    fZeroOffset: double;
    function GetDistance: double;
    function GetAngle: double;
    function GetPivot: TPointD;
    function GetAngleBtn: TButtonDesignerLayer32;
    function GetPivotBtn: TButtonDesignerLayer32;
    function GetDesignLayer: TDesignerLayer32;
  protected
    procedure Init(const rec: TRect; buttonSize: integer;
      centerButtonColor, movingButtonColor: TColor32;
      startingAngle: double; startingZeroOffset: double;
      buttonLayerClass: TButtonDesignerLayer32Class); virtual;
    property DesignLayer: TDesignerLayer32 read GetDesignLayer;
  public
    property Angle: double read GetAngle;
    property PivotPoint: TPointD read GetPivot;
    property AngleButton: TButtonDesignerLayer32 read GetAngleBtn;
    property PivotButton: TButtonDesignerLayer32 read GetPivotBtn;
    property DistBetweenButtons: double read GetDistance;
  end;


  TButtonGroupLayer32 = class(TGroupLayer32) //groups generic buttons
  private
    fBtnSize: integer;
    fBtnShape: TButtonShape;
    fBtnColor: TColor32;
    fBbtnLayerClass: TButtonDesignerLayer32Class;
  public
    function AddButton(const pt: TPointD): TButtonDesignerLayer32;
    function InsertButton(const pt: TPointD; btnIdx: integer): TButtonDesignerLayer32;
  end;

  TDesignerLayer32 = class(THitTestLayer32) //generic design layer
  public
    procedure UpdateHitTestMask(const vectorRegions: TPathsD;
      fillRule: TFillRule); virtual;
  end;

  TButtonDesignerLayer32 = class(TDesignerLayer32) //button (design) layer
  private
    fSize     : integer;
    fColor    : TColor32;
    fShape    : TButtonShape;
    fButtonOutline: TPathD;
  protected
    procedure SetButtonAttributes(const shape: TButtonShape;
      size: integer; color: TColor32); virtual;
  public
    constructor Create(groupOwner: TGroupLayer32;
      const name: string = ''); override;
    procedure Draw; virtual;

    property Size  : integer read fSize write fSize;
    property Color : TColor32 read fColor write fColor;
    property Shape : TButtonShape read fShape write fShape;
    property ButtonOutline: TPathD read fButtonOutline write fButtonOutline;
  end;

  TLayeredImage32 = class
  private
    fRoot              : TGroupLayer32;
    fBounds            : TRect;
    fBackColor         : TColor32;
    fResampler         : integer;
    function  GetRootLayersCount: integer;
    function  GetLayer(index: integer): TLayer32;
    function  GetImage: TImage32;
    function  GetHeight: integer;
    procedure SetHeight(value: integer);
    function  GetWidth: integer;
    procedure SetWidth(value: integer);
    procedure SetBackColor(color: TColor32);
    function  GetMidPoint: TPointD;
    procedure SetResampler(newSamplerId: integer);
  public
    constructor Create(Width: integer = 0; Height: integer =0); virtual;
    destructor Destroy; override;
    procedure SetSize(width, height: integer);
    procedure Clear;
    procedure Invalidate;
    function  AddLayer(layerClass: TLayer32Class = nil;
      group: TGroupLayer32 = nil; const name: string = ''): TLayer32;
    function  InsertLayer(layerClass: TLayer32Class; group: TGroupLayer32;
      index: integer; const name: string = ''): TLayer32;
    procedure DeleteLayer(layer: TLayer32); overload;
    procedure DeleteLayer(layerIndex: integer;
      groupOwner: TGroupLayer32 = nil); overload;
    function  FindLayerNamed(const name: string): TLayer32;
    function  GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean = false): TLayer32;
    function  GetMergedImage(hideDesigners: Boolean = false): TImage32; overload;
    function  GetMergedImage(hideDesigners: Boolean;
      out updateRect: TRect): TImage32; overload;

    property Resampler: integer read fResampler write SetResampler;
    property BackgroundColor: TColor32 read fBackColor write SetBackColor;
    property Bounds: TRect read fBounds;
    property Count: integer read GetRootLayersCount;
    property Height: integer read GetHeight write SetHeight;
    property Image: TImage32 read GetImage;
    property Layer[index: integer]: TLayer32 read GetLayer; default;
    property MidPoint: TPointD read GetMidPoint;
    property Root: TGroupLayer32 read fRoot;
    property Width: integer read GetWidth write SetWidth;
  end;

function CreateSizingButtonGroup(targetLayer: TLayer32;
  sizingStyle: TSizingStyle; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TSizingGroupLayer32;

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  const pivot: TPointD; buttonSize: integer = 0;
  pivotButtonColor: TColor32 = clWhite32;
  angleButtonColor: TColor32 = clBlue32;
  initialAngle: double = 0; angleOffset: double = 0;
  enablePivotMove: Boolean = True;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TRotatingGroupLayer32; overload;

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  buttonSize: integer = 0;
  pivotButtonColor: TColor32 = clWhite32;
  angleButtonColor: TColor32 = clBlue32;
  initialAngle: double = 0; angleOffset: double = 0;
  enablePivotMove: Boolean = True;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TRotatingGroupLayer32; overload;

function CreateButtonGroup(groupOwner: TGroupLayer32;
  const buttonPts: TPathD; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonGroupLayer32;

function UpdateSizingButtonGroup(movedButton: TLayer32): TRect;

function UpdateRotatingButtonGroup(rotateButton: TLayer32): double;

var
  DefaultButtonSize: integer;
  dashes: TArrayOfInteger;

const
  crDefault   =   0;
  crArrow     =  -2;
  crSizeNESW  =  -6;
  crSizeNS    =  -7;
  crSizeNWSE  =  -8;
  crSizeWE    =  -9;
  crHandPoint = -21;
  crSizeAll   = -22;

implementation

{$IFNDEF MSWINDOWS}
uses
  Img32.FMX;
{$ENDIF}

resourcestring
  rsRoot                   = 'root';
  rsCreateLayerError       = 'TLayer32 error - no group owner defined.';
  rsButton                 = 'Button';
  rsSizingButtonGroup      = 'SizingButtonGroup';
  rsRotatingButtonGroup    = 'RotatingButtonGroup';
  rsChildIndexRangeError   = 'TGroupLayer32 - child index error';
  rsCreateButtonGroupError = 'CreateButtonGroup - invalid target layer';
  rsUpdateRotateGroupError = 'UpdateRotateGroup - invalid group';

//------------------------------------------------------------------------------
// TLayerNotifyImage32
//------------------------------------------------------------------------------

constructor TLayerNotifyImage32.Create(owner: TLayer32);
begin
  inherited Create;
  fOwnerLayer := owner;
end;
//------------------------------------------------------------------------------

procedure TLayerNotifyImage32.Changed;
begin
  if (Self.UpdateCount = 0) then
    fOwnerLayer.ImageChanged(Self);
  inherited;
end;

//------------------------------------------------------------------------------
// THitTestRec
//------------------------------------------------------------------------------

function THitTestRec.IsEmpty: Boolean;
begin
  Result := (width <= 0) or (height <= 0);
end;
//------------------------------------------------------------------------------

procedure THitTestRec.Init(ownerLayer: TLayer32);
begin
  width := ownerLayer.width;
  height := ownerLayer.height;
  PtrPixels := nil;
  if not IsEmpty then
    SetLength(PtrPixels, width * height);
end;
//------------------------------------------------------------------------------

procedure THitTestRec.Clear;
begin
  width := 0; height := 0; PtrPixels := nil;
end;

//------------------------------------------------------------------------------
// TPointerRenderer: renders both 32 & 64bits pointer 'pixels' for hit-testing
//------------------------------------------------------------------------------

type

  TPointerRenderer = class(TCustomRenderer)
  private
    fObjPointer: Pointer;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    constructor Create(objectPtr: Pointer);
  end;

procedure TPointerRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PPointer;
begin
  dst := GetDstPixel(x1,y);
  for i := x1 to x2 do
  begin
    if Byte(alpha^) > 127 then dst^ := fObjPointer;
    inc(PByte(dst), PixelSize); inc(alpha);
  end;
end;
//------------------------------------------------------------------------------

constructor TPointerRenderer.Create(objectPtr: Pointer);
begin
  fObjPointer := objectPtr;
end;

//------------------------------------------------------------------------------
// THitTestRec helper functions
//------------------------------------------------------------------------------

procedure UpdateHitTestMaskUsingPath(layer: THitTestLayer32;
  const paths: TPathsD; fillRule: TFillRule);
var
  monoRenderer: TPointerRenderer;
begin
  monoRenderer := TPointerRenderer.Create(layer);
  try
    with layer.HitTestRec do
    begin
      if IsEmpty then Exit;
      monoRenderer.Initialize(@PtrPixels[0], width, height, sizeOf(Pointer));
      Rasterize(paths, Rect(0,0,width,height), fillRule, monoRenderer);
    end;
  finally
    monoRenderer.Free;
  end;
end;
//------------------------------------------------------------------------------

//Creates a pointer hit-test mask using the supplied image and compareFunc.
procedure UpdateHitTestMaskUsingImage(var htr: THitTestRec;
  objPtr: Pointer; img: TImage32; compareFunc: TCompareFunction;
  referenceColor: TColor32; tolerance: integer);
var
  i: integer;
  pc: PColor32;
  pp: PPointer;
begin
  with img do
  begin
    htr.width := Width;
    htr.height := Height;
    htr.PtrPixels := nil;
    if IsEmpty then Exit;
    SetLength(htr.PtrPixels, Width * Height);
  end;

  pc := img.PixelBase;
  pp := @htr.PtrPixels[0];
  for i := 0 to high(htr.PtrPixels) do
  begin
    if compareFunc(referenceColor, pc^, tolerance) then pp^ := objPtr;
    inc(pc); inc(pp);
  end;
end;

//------------------------------------------------------------------------------
// TLayer32 class
//------------------------------------------------------------------------------

constructor TLayer32.Create(groupOwner: TGroupLayer32; const name: string);
begin
  fGroupOwner := groupOwner;
  if assigned(groupOwner) then
    fLayeredImage := groupOwner.fLayeredImage
  else if name <> rsRoot then
    raise Exception.Create(rsCreateLayerError);
  fImage      := TLayerNotifyImage32.Create(self);
  fName       := name;
  fVisible    := True;
  fOpacity    := 255;
  CursorId    := crDefault;
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
begin
  fImage.Free;
  if Assigned(fGroupOwner) then
    fGroupOwner.InternalDeleteChild(Index, true);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayer32.BeginUpdate;
begin
  if not fRefreshPending then
    Invalidate(fOldBounds);
  Inc(fGroupOwner.fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TLayer32.EndUpdate;
begin
  Dec(fGroupOwner.fUpdateCount);
end;
//------------------------------------------------------------------------------

function TLayer32.CanUpdate: Boolean;
begin
  Result := not fRefreshPending and (fGroupOwner.fUpdateCount = 0);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Invalidate(rec: TRect);
begin
  fRefreshPending := true;
  if assigned(GroupOwner) then
    GroupOwner.Invalidate(rec);
end;
//------------------------------------------------------------------------------

procedure TLayer32.ImageChanged(Sender: TImage32);
begin
  if not (Self is TGroupLayer32) and not fRefreshPending then
    Invalidate(fOldBounds);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetSize(width, height: integer);
begin
  Image.SetSize(width, height);
end;
//------------------------------------------------------------------------------

function TLayer32.GetHeight: integer;
begin
  Result := Image.Height;
end;
//------------------------------------------------------------------------------

function TLayer32.GetWidth: integer;
begin
  Result := Image.Width;
end;
//------------------------------------------------------------------------------

procedure  TLayer32.SetBounds(const newBounds: TRect);
begin
  fLeft := newBounds.Left;
  fTop := newBounds.Top;
  //nb: Image.SetSize will call the ImageChanged method
  Image.SetSize(RectWidth(newBounds),RectHeight(newBounds));
end;
//------------------------------------------------------------------------------

function TLayer32.GetBounds: TRect;
begin
  Result := Rect(fLeft, fTop, fLeft + fImage.Width, fTop + fImage.Height)
end;
//------------------------------------------------------------------------------

function TLayer32.GetMidPoint: TPointD;
begin
  Result := Img32.Vector.MidPoint(RectD(Bounds));
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(const pt: TPoint);
begin
  PositionAt(pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(x, y: integer);
begin
  if (fLeft = x) and (fTop = y) then Exit;
  fLeft := x; fTop := y;
  if not fRefreshPending then
    Invalidate(fOldBounds);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(X, Y: integer);
begin
  PositionCenteredAt(PointD(X,Y));
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(const pt: TPoint);
begin
  PositionCenteredAt(PointD(pt));
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(const pt: TPointD);
var
  l,t: integer;
begin
  l := Round(pt.X - Image.Width * 0.5);
  t := Round(pt.Y - Image.Height * 0.5);

  if (l = fLeft) and (t = fTop) then Exit;

  fLeft := l; fTop := t;
  if not fRefreshPending then
    Invalidate(fOldBounds);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Offset(dx, dy: integer);
begin
  if (dx <> 0) or (dy <> 0) then
    PositionAt(fLeft + dx, fTop + dy);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetVisible(value: Boolean);
begin
  if (value = fVisible) or (RootOwner.Root = Self) then Exit;
  fVisible := value;
  Invalidate(fOldBounds);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetOpacity(value: Byte);
begin
  if value = fOpacity then Exit;
  fOpacity := value;
  Invalidate(fOldBounds);
end;
//------------------------------------------------------------------------------

function TLayer32.BringForwardOne: Boolean;
begin
  Result := assigned(fGroupOwner) and (index < fGroupOwner.ChildCount -1);
  if not Result then Exit;
  fGroupOwner.fChilds.Move(index, index +1);
  fGroupOwner.ReindexChildsFrom(index);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.SendBackOne: Boolean;
begin
  Result := assigned(fGroupOwner) and (index > 0);
  if not Result then Exit;
  fGroupOwner.fChilds.Move(index, index -1);
  fGroupOwner.ReindexChildsFrom(index -1);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.BringToFront: Boolean;
begin
  Result := assigned(fGroupOwner) and
    (index < fGroupOwner.ChildCount -1);
  if not Result then Exit;
  fGroupOwner.fChilds.Move(index, fGroupOwner.ChildCount -1);
  fGroupOwner.ReindexChildsFrom(index);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.SendToBack: Boolean;
begin
  Result := assigned(fGroupOwner) and (index > 0);
  if not Result then Exit;
  fGroupOwner.fChilds.Move(index, 0);
  fGroupOwner.ReindexChildsFrom(0);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.Move(newGroupOwner: TGroupLayer32; idx: integer): Boolean;
begin
  Result := assigned(fGroupOwner) and assigned(newGroupOwner);
  if not Result then Exit;

  with newGroupOwner do
    if idx < 0 then idx := 0
    else if idx >= ChildCount then idx := ChildCount;

  if newGroupOwner = fGroupOwner then
  begin
    if idx = fIndex then Exit;
    fGroupOwner.fChilds.Move(fIndex, idx);
    newGroupOwner.ReindexChildsFrom(Min(idx, fIndex));
  end else
  begin
    if Visible then
      fGroupOwner.Invalidate(Bounds);
    fGroupOwner.ReindexChildsFrom(fIndex);

    fIndex := idx;
    newGroupOwner.fChilds.Insert(idx, self);
    newGroupOwner.ReindexChildsFrom(idx +1);
  end;
  newGroupOwner.RefreshPending;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetBlendFunc(func: TBlendFunction);
begin
  if not Assigned(fGroupOwner) then Exit;
  fBlendFunc := func;
  if Visible then
    fGroupOwner.Invalidate(Bounds);
end;

//------------------------------------------------------------------------------
// TGroupLayer32 class
//------------------------------------------------------------------------------

constructor TGroupLayer32.Create(groupOwner: TGroupLayer32; const name: string);
begin
  inherited;
{$IFDEF XPLAT_GENERICS}
  fChilds := TList<TLayer32>.Create;
{$ELSE}
  fChilds := TList.Create;
{$ENDIF}

  fLastUpdateType := utUndefined;
end;
//------------------------------------------------------------------------------

destructor TGroupLayer32.Destroy;
begin
  ClearChildren;
  fChilds.Free;
  if Assigned(fGroupOwner) then
    fGroupOwner.Invalidate(fInvalidRect);
  inherited;
end;
//------------------------------------------------------------------------------

function TGroupLayer32.GetChildCount: integer;
begin
  Result := fChilds.Count;
end;
//------------------------------------------------------------------------------

function TGroupLayer32.GetChild(index: integer): TLayer32;
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create(rsChildIndexRangeError);
  Result := TLayer32(fChilds[index]);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.ClearChildren;
var
  i: integer;
begin
  for i := fChilds.Count -1 downto 0 do
    TLayer32(fChilds[i]).Free;
  fChilds.Clear;
  fLastUpdateType := utUndefined;
  Image.SetSize(0, 0);
end;
//------------------------------------------------------------------------------

function   TGroupLayer32.AddChild(layerClass: TLayer32Class;
  const name: string = ''): TLayer32;
begin
  Result := InsertChild(layerClass, MaxInt, name);
end;
//------------------------------------------------------------------------------

function   TGroupLayer32.InsertChild(layerClass: TLayer32Class;
  index: integer; const name: string = ''): TLayer32;
begin
  Result := layerClass.Create(self, name);
  if index >= ChildCount then
  begin
    Result.fIndex := ChildCount;
    fChilds.Add(Result);
  end else
  begin
    Result.fIndex := index;
    fChilds.Insert(index, Result);
    ReindexChildsFrom(index +1);
  end;
end;
//------------------------------------------------------------------------------

procedure  TGroupLayer32.InternalDeleteChild(index: integer; fromChild: Boolean);
var
  child: TLayer32;
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create(rsChildIndexRangeError);

  child := TLayer32(fChilds[index]);
  fChilds.Delete(index);

  if child.Visible then
    child.Invalidate(child.Bounds);

  if not fromChild then
  begin
    child.fGroupOwner := nil; //avoids recursion :)
    child.Free;
  end;
  if index < ChildCount then
    ReindexChildsFrom(index);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.DeleteChild(index: integer);
begin
  InternalDeleteChild(index, false);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.Offset(dx, dy: integer);
var
  i: integer;
begin
  if (dx = 0) and (dy = 0) then Exit;
  for i := 0 to ChildCount -1 do
    Child[i].Offset(dx, dy);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.Invalidate(rec: TRect);
begin
  if not IsEmptyRect(rec) then
    fInvalidRect := Img32.Vector.UnionRect(fInvalidRect, rec);
  RefreshPending;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.RefreshPending;
begin
  fRefreshPending := true;
  if Assigned(GroupOwner) then
    GroupOwner.RefreshPending;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.BeginUpdate;
begin
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.EndUpdate;
begin
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.SetOpacity(value: Byte);
begin
  if fOpacity = value then Exit;
  fOpacity := value;
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.PreMerge(hideDesigners, forceRefresh: Boolean);
var
  i           : integer;
  rec, rec2   : TRect;
  aChild      : TLayer32;
  aChildGroup : TGroupLayer32;
begin
  if forceRefresh then fRefreshPending := true;
  if not fRefreshPending then Exit;

  //this method is recursive and updates Bounds and fInvalidRect
  rec := NullRect;
  for i := 0 to ChildCount -1 do
  begin
    if not Child[i].Visible or
      (hideDesigners and (Child[i] is TDesignerLayer32)) then
        Continue;

    aChild := TLayer32(Child[i]);
    if (Child[i] is TGroupLayer32) then
      aChildGroup := TGroupLayer32(Child[i]) else
      aChildGroup := nil;

    if Assigned(aChildGroup) then
    begin
      aChildGroup.PreMerge(hideDesigners, forceRefresh);
      if not forceRefresh then
        fInvalidRect := Img32.Vector.UnionRect(fInvalidRect, aChildGroup.fInvalidRect);
      aChildGroup.fInvalidRect := NullRect;
    end else
    begin
      if not forceRefresh and aChild.fRefreshPending then
        fInvalidRect := Img32.Vector.UnionRect(fInvalidRect, aChild.Bounds);
    end;
    rec := Img32.Vector.UnionRect(rec, aChild.Bounds);
    aChild.fOldBounds := aChild.Bounds;
  end;

  //nb: the root's bounds is fixed to the layeredImage's bounds
  if not Assigned(GroupOwner) then Exit;

  if Assigned(fClipPath) then
  begin
    rec2 := img32.Vector.GetBounds(fClipPath);
    types.IntersectRect(rec, rec, rec2);
  end;
  SetBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.Merge(hideDesigners: Boolean; const staleRect: TRect);
var
  ChildLayer: TLayer32;
  tmpOpacity: byte;
  i: integer;
  tmp: TImage32;
  childRect, groupRect: TRect;
  clpPath: TPathD;
begin
  if not Visible or (Opacity < 2) or
    Image.IsEmpty or not fRefreshPending then
      Exit;

  //merge redraw the entire grouplayer
  for i := 0 to ChildCount -1 do
  begin
    ChildLayer := Child[i];

    if not ChildLayer.Visible or
      (hideDesigners and (ChildLayer is TDesignerLayer32)) then
        Continue;

    //any layer that's outside 'staleRect' can safely be ignored
    if (self = fLayeredImage.fRoot) and
      not RectsOverlap(staleRect, ChildLayer.Bounds) then
        Continue;

    //recursive merge
    if (ChildLayer is TGroupLayer32) then
      TGroupLayer32(ChildLayer).Merge(hideDesigners, staleRect);

    //childRect - the source rect in the child's image
    childRect := ChildLayer.Bounds;

    //groupRect - the destination rect in the group's image
    groupRect :=  childRect;
    Types.OffsetRect(groupRect, -Left, -Top);
    Types.OffsetRect(childRect, -ChildLayer.Left, -ChildLayer.Top);

    //get the opacity
    if ChildLayer.Opacity < 254 then
      tmpOpacity := MulBytes(fOpacity, ChildLayer.Opacity) else
      tmpOpacity := fOpacity;

    //finally, draw to the group's image
    if (tmpOpacity < 254) or Assigned(fClipPath) then
    begin
      tmp := TImage32.Create(ChildLayer.Image);
      try
        if (tmpOpacity < 254) then
          tmp.ReduceOpacity(tmpOpacity);

        if Assigned(fClipPath) then
        begin
          clpPath := OffsetPath(fClipPath, -ChildLayer.Left, -ChildLayer.Top);
          EraseOutsidePath(tmp, clpPath, frNonZero, tmp.Bounds);
        end;

        if Assigned(ChildLayer.BlendFunc) then
          Image.CopyBlend(tmp, childRect, groupRect, ChildLayer.BlendFunc) else
          Image.CopyBlend(tmp, childRect, groupRect, BlendToAlpha);

      finally
        tmp.Free;
      end;
    end
    else if Assigned(ChildLayer.BlendFunc) then
      Image.CopyBlend(ChildLayer.Image,
        childRect, groupRect, ChildLayer.BlendFunc)
    else
      Image.CopyBlend(ChildLayer.Image,
        childRect, groupRect, BlendToAlpha);

    ChildLayer.fRefreshPending := false;
  end;
  fInvalidRect := NullRect;
  fRefreshPending := false;
end;
//------------------------------------------------------------------------------

function TGroupLayer32.GetLayerAt(const pt: TPoint;
  ignoreDesigners: Boolean): TLayer32;
var
  i, htIdx: integer;
begin
  Result := nil;
  for i := ChildCount -1 downto 0 do
  begin
    if Child[i] is TGroupLayer32 then //recursive
      Result := TGroupLayer32(Child[i]).GetLayerAt(pt, ignoreDesigners)
    else if not Child[i].Visible or
      (ignoreDesigners and (Child[i] is TDesignerLayer32)) then
        Continue
    else if Child[i] is THitTestLayer32 then
      with THitTestLayer32(Child[i]) do
        if not fHitTestRec.IsEmpty and PtInRect(Bounds, pt) then
        begin
          htIdx := fHitTestRec.width * (pt.Y - Top) + (pt.X  -Left);
          Result := fHitTestRec.PtrPixels[htIdx];
        end;
    if Assigned(Result) then Break;
  end;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.ReindexChildsFrom(startIdx: Integer);
var
  i: integer;
begin
  for i := startIdx to ChildCount -1 do
    Child[i].fIndex := i;
end;
//------------------------------------------------------------------------------

function TGroupLayer32.FindLayerNamed(const name: string): TLayer32;
var
  i: integer;
begin
  if SameText(self.Name, name) then
  begin
    Result := self;
    Exit;
  end;

  Result := nil;
  for i := 0 to ChildCount -1 do
  begin
    if Child[i] is TGroupLayer32 then
    begin
      Result := TGroupLayer32(Child[i]).FindLayerNamed(name);
      if assigned(Result) then Break;
    end else if SameText(self.Name, name) then
    begin
      Result := Child[i];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------
// THitTestLayer32 class
//------------------------------------------------------------------------------

procedure THitTestLayer32.ImageChanged(Sender: TImage32);
begin
  inherited;
  fHitTestRec.Clear;
end;
//------------------------------------------------------------------------------

function THitTestLayer32.GetPathsFromHitTestMask: TPathsD;
var
  i, len: integer;
  tmpImg: TImage32;
  pp: PPointer;
  pc: PColor32;
begin
  Result := nil;
  with fHitTestRec do
  begin
    len := Length(PtrPixels);
    if (len = 0) or (len <> width * height) then Exit;
    tmpImg := TImage32.Create(width, height);
    try
      pc := tmpImg.PixelBase;
      pp := @PtrPixels[0];
      for i := 0 to len -1 do
      begin
        if pp^ <> nil then pc^ := clWhite32;
        inc(pp); inc(pc);
      end;
      result := Vectorize(tmpImg, clWhite32, CompareAlpha, 0);
    finally
      tmpImg.Free;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure THitTestLayer32.ClearHitTesting;
begin
  fHitTestRec.Clear;
end;

//------------------------------------------------------------------------------
// TRotateLayer32 class
//------------------------------------------------------------------------------

constructor TRotateLayer32.Create(groupOwner: TGroupLayer32;
  const name: string = '');
begin
  inherited;
  fAutoPivot := true;
  fPivotPt := InvalidPointD;
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.SetAngle(newAngle: double);
begin
  NormalizeAngle(newAngle);
  if newAngle = fAngle then Exit;
  if PointsEqual(fPivotPt, InvalidPointD) then
    fPivotPt := MidPoint;
  Rotate(newAngle - fAngle);
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.Rotate(angleDelta: double);
begin
  if angleDelta = 0 then Exit;
  fAngle := fAngle + angleDelta;
  NormalizeAngle(fAngle);
  //the rest is done in descendant classes
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.ResetAngle;
begin
  fAngle := 0;
  fPivotPt := InvalidPointD;
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.Offset(dx, dy: integer);
begin
  inherited;
  if fAutoPivot then
  begin
    fPivotPt.X := fPivotPt.X + dx;
    fPivotPt.Y := fPivotPt.Y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TRotateLayer32.GetPivotPt: TPointD;
begin
  if PointsEqual(fPivotPt, InvalidPointD) then
    Result := MidPoint else
    Result := fPivotPt;
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.SetPivotPt(const pivot: TPointD);
begin
  if fAutoPivot then fAutoPivot := false;
  fPivotPt := pivot;
end;
//------------------------------------------------------------------------------

procedure TRotateLayer32.SetAutoPivot(val: Boolean);
begin
  if val = fAutoPivot then Exit;
  fAutoPivot := val;
  fPivotPt := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TVectorLayer32 class
//------------------------------------------------------------------------------

constructor TVectorLayer32.Create(groupOwner: TGroupLayer32;
  const name: string = '');
begin
  inherited;
  fMargin := DpiAwareI *2;
  fCursorId := crHandPoint;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Rotate(angleDelta: double);
begin
  if angleDelta = 0 then Exit;
  inherited;

  fPaths := RotatePath(fPaths, fPivotPt, angleDelta);
  RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.SetPaths(const newPaths: TPathsD);
begin
  fPaths := CopyPaths(newPaths);
  fPivotPt := InvalidPointD;
  RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.SetBounds(const newBounds: TRect);
var
  w,h, m2: integer;
  rec: TRect;
  mat: TMatrixD;
begin
  m2 := Margin *2;
  w := RectWidth(newBounds) -m2;
  h := RectHeight(newBounds) -m2;

  //make sure the bounds are large enough to scale safely
  if (Width > m2) and (Height > m2) and (w > 1) and (h > 1)  then
  begin
    //apply scaling and translation
    mat := IdentityMatrix;
    rec := Img32.Vector.GetBounds(fPaths);
    MatrixTranslate(mat, -rec.Left, -rec.Top);
    MatrixScale(mat, w/(Width - m2), h/(Height - m2));
    MatrixTranslate(mat, newBounds.Left + Margin, newBounds.Top + Margin);
    MatrixApply(mat, fPaths);
    if fAutoPivot then fPivotPt := InvalidPointD;
    RepositionAndDraw;
  end else
    inherited;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Offset(dx,dy: integer);
begin
  inherited;
  fPaths := OffsetPath(fPaths, dx,dy);
  if fAutoPivot and not PointsEqual(fPivotPt, InvalidPointD) then
    fPivotPt := OffsetPoint(fPivotPt, dx,dy);
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.SetMargin(new: integer);
begin
  if fMargin = new then Exit;
  fMargin := new;
  if not Image.IsEmpty then
    RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.RepositionAndDraw;
var
  rec: TRect;
begin
  rec := Img32.Vector.GetBounds(fPaths);
  Img32.Vector.InflateRect(rec, Margin, Margin);
  inherited SetBounds(rec);
  Image.BlockUpdate;
  try
    Draw;
  finally
    Image.UnblockUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Draw;
begin
  //to draw the layer, either override this event
  //in a descendant class or assign the OnDraw property
  if Assigned(fOnDraw) then fOnDraw(self);
end;
//------------------------------------------------------------------------------

procedure  TVectorLayer32.UpdateHitTestMask(const vectorRegions: TPathsD;
  fillRule: TFillRule);
begin
  fHitTestRec.Init(self);
  UpdateHitTestMaskUsingPath(self, vectorRegions, fillRule);
end;

//------------------------------------------------------------------------------
// TRasterLayer32 class
//------------------------------------------------------------------------------

constructor TRasterLayer32.Create(groupOwner: TGroupLayer32; const name: string = '');
begin
  inherited;
  fMasterImg := TLayerNotifyImage32.Create(self);
  fCursorId := crHandPoint;
  fAutoHitTest := true;
end;
//------------------------------------------------------------------------------

destructor TRasterLayer32.Destroy;
begin
  fMasterImg.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.UpdateHitTestMaskOpaque;
var
  i: integer;
  htr: THitTestRec;
  pp: PPointer;
begin
  //fill the entire Hittest mask with self pointers
  with Image do
  begin
    htr.width := Width;
    htr.height := Height;
    SetLength(htr.PtrPixels, Width * Height);
    if not IsEmpty then
    begin
      pp := @htr.PtrPixels[0];
      for i := 0 to High(htr.PtrPixels) do
      begin
        pp^ := self; inc(pp);
      end;
    end;
  end;
  HitTestRec := htr;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.UpdateHitTestMaskTransparent;
begin
  //this method will mark all pixels with an opacity > 127.
  UpdateHitTestMaskUsingImage(fHitTestRec,
    Self, Image, CompareAlpha, clWhite32, 128);
end;
//------------------------------------------------------------------------------

procedure  TRasterLayer32.UpdateHitTestMaskTransparent(
  compareFunc: TCompareFunction;
   referenceColor: TColor32; tolerance: integer);
begin
  UpdateHitTestMaskUsingImage(fHitTestRec, Self, Image,
  compareFunc, referenceColor, tolerance);
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.DoAutoHitTest;
begin
  if fAutoHitTest then
  begin
    if Image.HasTransparency then
      UpdateHitTestMaskTransparent else
      UpdateHitTestMaskOpaque;
  end else
    HitTestRec.Clear;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.ImageChanged(Sender: TImage32);
begin
  if (Sender = MasterImage) then
  begin
    MasterImage.BlockUpdate; //avoid endless recursion
    try
      //reset the layer whenever MasterImage changes
      fAngle := 0;
      fMatrix := IdentityMatrix;
      fRotating := false;

      if not fRefreshPending then
      begin
        if not IsEmptyRect(fOldBounds) then Invalidate(fOldBounds);
        fRefreshPending := true;
      end;

      with MasterImage do
        fSavedSize := Img32.Vector.Size(Width, Height);
      Image.Assign(MasterImage); //this will call ImageChange for Image
      Image.Resampler := RootOwner.Resampler;
    finally
      MasterImage.UnblockUpdate;
    end;
  end else
    inherited;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.Offset(dx,dy: integer);
begin
  inherited;
  fSavedMidPt := OffsetPoint(fSavedMidPt, dx,dy);
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.SetPivotPt(const pivot: TPointD);
begin
  inherited;
  fSavedMidPt := MidPoint;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.SetBounds(const newBounds: TRect);
var
  newWidth, newHeight: integer;
begin
  DoPreScaleCheck;
  newWidth := RectWidth(newBounds);
  newHeight := RectHeight(newBounds);

  //make sure the image is large enough to scale safely
  if (MasterImage.Width > 1) and (MasterImage.Height > 1) and
    (newWidth > 1) and (newHeight > 1) then
  begin
    Image.BeginUpdate;
    try
      Image.Assign(MasterImage);
      Image.Resampler := RootOwner.Resampler;
      //apply any prior transformations
      AffineTransformImage(Image, fMatrix);
      //unfortunately cropping isn't an affine transformation
      //so we have to crop separately and before the final resize
      SymmetricCropTransparent(Image);
      Image.Resize(newWidth, newHeight);
      PositionAt(newBounds.TopLeft);
    finally
      Image.EndUpdate;
    end;
    DoAutoHitTest;
  end else
    inherited;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.DoPreScaleCheck;
begin
  if not fRotating or not Assigned(Image) then Exit;
  fRotating := false;

  //rotation has just ended so add the rotation angle to fMatrix
  if (fAngle <> 0) then
    MatrixRotate(fMatrix, Image.MidPoint, fAngle);
  //and since we're about to start scaling, we need
  //to store the starting size, and reset the angle
  fSavedSize := Size(Image.Width, Image.Height);
  fAngle := 0;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.DoPreRotationCheck;
begin
  if fRotating or not Assigned(Image) then Exit;
  fRotating := true;

  fSavedMidPt := MidPoint;
  if fAutoPivot then fPivotPt := fSavedMidPt;

  //scaling has just ended and rotating is about to start
  //so apply the current scaling to the matrix
  MatrixScale(fMatrix, Image.Width/fSavedSize.cx,
    Image.Height/fSavedSize.cy);
end;
//------------------------------------------------------------------------------

function TRasterLayer32.GetMatrix: TMatrixD;
begin
  Result := fMatrix;

  //and update for transformations not yet unapplied to fMatrix
  if fRotating then
  begin
    if fAngle <> 0 then
      MatrixRotate(Result, MidPoint, fAngle);
  end else
  begin
    MatrixScale(Result, Image.Width/fSavedSize.cx,
      Image.Height/fSavedSize.cy);
  end;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.Rotate(angleDelta: double);
var
  mat: TMatrixD;
  rec: TRectD;
begin
  if MasterImage.IsEmpty or (angleDelta = 0) then Exit;
  inherited;

  DoPreRotationCheck;

  if not fAutoPivot then
    RotatePoint(fSavedMidPt, PivotPt, angleDelta);

  Image.BeginUpdate;
  try
    Image.Assign(MasterImage);
    Image.Resampler := RootOwner.Resampler;
    rec := GetRotatedRectBounds(RectD(Image.Bounds), Angle);

    //get prior transformations and apply new rotation
    mat := fMatrix;
    MatrixTranslate(mat, -Width/2,-Height/2);
    MatrixRotate(mat, NullPointD, Angle);
    MatrixTranslate(mat, rec.Width/2, rec.Height/2);

    AffineTransformImage(Image, mat);
    //symmetric cropping prevents center wobbling
    SymmetricCropTransparent(Image);
  finally
    Image.EndUpdate;
  end;
  PositionCenteredAt(fSavedMidPt);
  DoAutoHitTest;
end;

//------------------------------------------------------------------------------
// TRotatingGroupLayer32 class
//------------------------------------------------------------------------------

procedure TRotatingGroupLayer32.Init(const rec: TRect;
  buttonSize: integer; centerButtonColor, movingButtonColor: TColor32;
  startingAngle: double; startingZeroOffset: double;
  buttonLayerClass: TButtonDesignerLayer32Class);
var
  i, dist: integer;
  pivot, pt: TPoint;
  rec2, r: TRectD;
begin
  //startingZeroOffset: default = 0 (ie 3 o'clock)
  if not ClockwiseRotationIsAnglePositive then
    startingZeroOffset := -startingZeroOffset;
  fZeroOffset := startingZeroOffset;

  if buttonSize <= 0 then buttonSize := DefaultButtonSize;
  pivot := Img32.Vector.MidPoint(rec);
  dist := Average(RectWidth(rec), RectHeight(rec)) div 2;
  rec2 := RectD(pivot.X -dist,pivot.Y -dist,pivot.X +dist,pivot.Y +dist);

  with AddChild(TDesignerLayer32) do     //Layer 0 - design layer
  begin
    SetBounds(Rect(rec2));
    i := DpiAwareI*2;
    r := rec2;
    Img32.Vector.InflateRect(r, -i,-i);
    OffsetRect(r, -Left, -Top);
    DrawDashedLine(Image, Ellipse(r), dashes, nil, i, clRed32, esPolygon);
  end;

  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  with TButtonDesignerLayer32(AddChild(  //Layer 1 - pivot button
    buttonLayerClass, rsButton)) do
  begin
    SetButtonAttributes(bsRound, buttonSize, centerButtonColor);
    PositionCenteredAt(Img32.Vector.MidPoint(rec));
    CursorId := crSizeAll;
  end;

  with TButtonDesignerLayer32(AddChild(  //layer 2 - angle (rotating) button
    buttonLayerClass, rsButton)) do
  begin
    SetButtonAttributes(bsRound, buttonSize, movingButtonColor);

    pt := Point(GetPointAtAngleAndDist(PointD(pivot),
      startingAngle + startingZeroOffset, dist));
    PositionCenteredAt(pt);
    CursorId := crHandPoint;
  end;

end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetPivot: TPointD;
begin
  Result := Child[1].MidPoint;
end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetAngleBtn: TButtonDesignerLayer32;
begin
  Result := Child[2] as TButtonDesignerLayer32;
end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetPivotBtn: TButtonDesignerLayer32;
begin
  Result := Child[1] as TButtonDesignerLayer32;
end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetDesignLayer: TDesignerLayer32;
begin
  Result := Child[0] as TDesignerLayer32;
end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetAngle: double;
begin
  Result :=
    Img32.Vector.GetAngle(Child[1].MidPoint, Child[2].MidPoint)  - fZeroOffset;
  NormalizeAngle(Result);
end;
//------------------------------------------------------------------------------

function TRotatingGroupLayer32.GetDistance: double;
begin
  Result := Img32.Vector.Distance(Child[1].MidPoint, Child[2].MidPoint);
end;

//------------------------------------------------------------------------------
// TDesignerLayer32
//------------------------------------------------------------------------------

procedure  TDesignerLayer32.UpdateHitTestMask(const vectorRegions: TPathsD;
  fillRule: TFillRule);
begin
  fHitTestRec.Init(self);
  UpdateHitTestMaskUsingPath(self, vectorRegions, fillRule);
end;

//------------------------------------------------------------------------------
// TButtonGroupLayer32 class
//------------------------------------------------------------------------------

function TButtonGroupLayer32.AddButton(const pt: TPointD): TButtonDesignerLayer32;
begin
  result := InsertButton(pt, MaxInt);
end;
//------------------------------------------------------------------------------

function TButtonGroupLayer32.InsertButton(const pt: TPointD;
  btnIdx: integer): TButtonDesignerLayer32;
begin
  result := TButtonDesignerLayer32(InsertChild(fBbtnLayerClass, btnIdx));
  with result do
  begin
    SetButtonAttributes(fBtnShape, FBtnSize, fBtnColor);
    PositionCenteredAt(pt);
    CursorId := crHandPoint;
  end;
end;

//------------------------------------------------------------------------------
// TButtonDesignerLayer32 class
//------------------------------------------------------------------------------

constructor TButtonDesignerLayer32.Create(groupOwner: TGroupLayer32;
  const name: string = '');
begin
  inherited;
  SetButtonAttributes(bsRound, DefaultButtonSize, clGreen32);
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.SetButtonAttributes(const shape: TButtonShape;
  size: integer; color: TColor32);
begin
  fSize := size;
  fShape := shape;
  fColor := color;
  size := Ceil(fSize * 1.25); //add room for button shadow
  SetSize(size, size);
  Draw;
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.Draw;
begin
  fButtonOutline := Img32.Extra.DrawButton(Image,
    image.MidPoint, fSize, fColor, fShape, [ba3D, baShadow]);
  UpdateHitTestMask(Img32.Vector.Paths(fButtonOutline), frEvenOdd);
end;

//------------------------------------------------------------------------------
// TLayeredImage32 class
//------------------------------------------------------------------------------

constructor TLayeredImage32.Create(Width: integer; Height: integer);
begin
  fRoot := TGroupLayer32.Create(nil, rsRoot);
  fRoot.fLayeredImage := self;
  fBounds := Rect(0, 0, Width, Height);
  fRoot.SetSize(width, Height);
  fResampler := DefaultResampler;
end;
//------------------------------------------------------------------------------

destructor TLayeredImage32.Destroy;
begin
  fRoot.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetSize(width, height: integer);
begin
  fBounds := Rect(0, 0, Width, Height);

  fRoot.SetBounds(fBounds);
  Invalidate;
  if fBackColor <> clNone32 then
    fRoot.Image.Clear(fBackColor);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMergedImage(hideDesigners: Boolean): TImage32;
var
  updateRect: TRect;
begin
  Root.fLastUpdateType := utUndefined; //forces a full repaint
  Result := GetMergedImage(hideDesigners, updateRect);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMergedImage(hideDesigners: Boolean;
  out updateRect: TRect): TImage32;
var
  forceRefresh: Boolean;
begin
  updateRect := NullRect;

  forceRefresh := (Root.fLastUpdateType = utUndefined) or
    (hideDesigners <> (Root.fLastUpdateType = utHideDesigners));
  with Root do
  begin
    //PreMerge resizes (and clears) invalidated groups
    PreMerge(hideDesigners, forceRefresh);

    //clip fInvalidRect to the drawing surface
    updateRect := Self.Bounds;
    if not forceRefresh then
      Types.IntersectRect(updateRect, fInvalidRect, updateRect);

    if not IsEmptyRect(updateRect) then
    begin
      Image.Clear(updateRect, fBackColor);
      Merge(hideDesigners, updateRect);
      if hideDesigners then
        fLastUpdateType := utHideDesigners else
        fLastUpdateType := utShowDesigners;
    end;
    fInvalidRect := NullRect;
    Result := Image;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Clear;
begin
  fRoot.ClearChildren;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Invalidate;
begin
  fRoot.fInvalidRect := fBounds;
  Root.fLastUpdateType := utUndefined;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetResampler(newSamplerId: integer);
begin
  if fResampler = newSamplerId then Exit;
  fResampler := newSamplerId;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetRootLayersCount: integer;
begin
  Result := fRoot.ChildCount;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayer(index: integer): TLayer32;
begin
  Result := fRoot[index];
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetImage: TImage32;
begin
  Result := fRoot.Image;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetHeight: integer;
begin
  Result := RectHeight(fBounds);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetHeight(value: integer);
begin
  if Height <> value then SetSize(Width, value);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetWidth: integer;
begin
  Result := RectWidth(fBounds);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetWidth(value: integer);
begin
  if Width <> value then SetSize(value, Height);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetBackColor(color: TColor32);
begin
  if color = fBackColor then Exit;
  fBackColor := color;
  fRoot.Image.Clear(fBackColor);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMidPoint: TPointD;
begin
  Result := PointD(Width * 0.5, Height * 0.5);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddLayer(layerClass: TLayer32Class;
  group: TGroupLayer32; const name: string): TLayer32;
begin
  if not Assigned(layerClass) then layerClass := TLayer32;
  Result := InsertLayer(layerClass, group, MaxInt, name);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertLayer(layerClass: TLayer32Class;
  group: TGroupLayer32; index: integer; const name: string): TLayer32;
begin
  if not Assigned(group) then group := fRoot;
  Result := group.InsertChild(layerClass, index, name);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.FindLayerNamed(const name: string): TLayer32;
begin
  Result := Root.FindLayerNamed(name);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(layer: TLayer32);
begin
  if not assigned(layer) or not assigned(layer.fGroupOwner) then Exit;
  layer.fGroupOwner.DeleteChild(layer.Index);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(layerIndex: integer;
  groupOwner: TGroupLayer32 = nil);
begin
  if not assigned(groupOwner) then groupOwner := Root;
  if (layerIndex < 0) or (layerIndex >= groupOwner.ChildCount) then
    raise Exception.Create(rsChildIndexRangeError);
  groupOwner.DeleteChild(layerIndex);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean): TLayer32;
begin
  result := Root.GetLayerAt(pt, ignoreDesigners);
end;

//------------------------------------------------------------------------------
// Miscellaneous button functions
//------------------------------------------------------------------------------

function GetRectEdgeMidPoints(const rec: TRectD): TPathD;
var
  mp: TPointD;
begin
  mp := MidPoint(rec);
  SetLength(Result, 4);
  Result[0] := PointD(mp.X, rec.Top);
  Result[1] := PointD(rec.Right, mp.Y);
  Result[2] := PointD(mp.X, rec.Bottom);
  Result[3] := PointD(rec.Left, mp.Y);
end;
//------------------------------------------------------------------------------

function CreateSizingButtonGroup(targetLayer: TLayer32;
  sizingStyle: TSizingStyle; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TSizingGroupLayer32;
var
  i: integer;
  rec: TRectD;
  corners, edges: TPathD;
const
  cnrCursorIds: array [0..3] of integer =
    (crSizeNWSE, crSizeNESW, crSizeNWSE, crSizeNESW);
  edgeCursorIds: array [0..3] of integer =
    (crSizeNS, crSizeWE, crSizeNS, crSizeWE);
begin
  if not assigned(targetLayer) or
    not (targetLayer is THitTestLayer32) then
      raise Exception.Create(rsCreateButtonGroupError);
  Result := TSizingGroupLayer32(
    targetLayer.RootOwner.AddLayer(TSizingGroupLayer32,
    nil, rsSizingButtonGroup));
  Result.SizingStyle := sizingStyle;
  rec := RectD(targetLayer.Bounds);
  corners := Rectangle(rec);
  edges := GetRectEdgeMidPoints(rec);

  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  for i := 0 to 3 do
  begin
    if sizingStyle <> ssEdges then
    begin
      with TButtonDesignerLayer32(Result.AddChild(
        buttonLayerClass, rsButton)) do
      begin
        SetButtonAttributes(buttonShape, buttonSize, buttonColor);
        PositionCenteredAt(corners[i]);
        CursorId := cnrCursorIds[i];
      end;
    end;
    if sizingStyle <> ssCorners then
    begin
      with TButtonDesignerLayer32(Result.AddChild(
        buttonLayerClass, rsButton)) do
      begin
        SetButtonAttributes(buttonShape, buttonSize, buttonColor);
        PositionCenteredAt(edges[i]);
        CursorId := edgeCursorIds[i];
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function UpdateSizingButtonGroup(movedButton: TLayer32): TRect;
var
  i: integer;
  path, corners, edges: TPathD;
  group: TSizingGroupLayer32;
  rec: TRectD;
begin
  //nb: it might be tempting to store the targetlayer parameter in the
  //CreateSizingButtonGroup function call and automatically update its
  //bounds here, except that there are situations where blindly updating
  //the target's bounds using the returned TRect is undesirable
  //(eg when targetlayer needs to preserve its width/height ratio).
  Result := NullRect;
  if not assigned(movedButton) or
    not (movedButton is TButtonDesignerLayer32) or
    not (movedButton.GroupOwner is TSizingGroupLayer32) then Exit;

  group := TSizingGroupLayer32(movedButton.GroupOwner);
  with group do
  begin
    SetLength(path, ChildCount);
    for i := 0 to ChildCount -1 do
      path[i] := Child[i].MidPoint;
  end;
  rec := GetBoundsD(path);

  case group.SizingStyle of
    ssCorners:
      begin
        if Length(path) <> 4 then Exit;
        with movedButton.MidPoint do
        begin
          case movedButton.Index of
            0: begin rec.Left := X; rec.Top := Y; end;
            1: begin rec.Right := X; rec.Top := Y; end;
            2: begin rec.Right := X; rec.Bottom := Y; end;
            3: begin rec.Left := X; rec.Bottom := Y; end;
          end;
        end;
        corners := Rectangle(rec);
        with group do
          for i := 0 to 3 do
            Child[i].PositionCenteredAt(corners[i]);
      end;
    ssEdges:
      begin
        if Length(path) <> 4 then Exit;
        with movedButton.MidPoint do
        begin
          case movedButton.Index of
            0: rec.Top := Y;
            1: rec.Right := X;
            2: rec.Bottom := Y;
            3: rec.Left := X;
          end;
        end;
        edges := GetRectEdgeMidPoints(rec);
        with group do
          for i := 0 to 3 do
            Child[i].PositionCenteredAt(edges[i]);
      end;
    else
      begin
        if Length(path) <> 8 then Exit;
        with movedButton.MidPoint do
        begin
          case movedButton.Index of
            0: begin rec.Left := X; rec.Top := Y; end;
            1: rec.Top := Y;
            2: begin rec.Right := X; rec.Top := Y; end;
            3: rec.Right := X;
            4: begin rec.Right := X; rec.Bottom := Y; end;
            5: rec.Bottom := Y;
            6: begin rec.Left := X; rec.Bottom := Y; end;
            7: rec.Left := X;
          end;
        end;
        corners := Rectangle(rec);
        edges := GetRectEdgeMidPoints(rec);
        with group do
          for i := 0 to 3 do
          begin
            Child[i*2].PositionCenteredAt(corners[i]);
            Child[i*2 +1].PositionCenteredAt(edges[i]);
          end;
      end;
  end;
  Result := Rect(rec);
end;
//------------------------------------------------------------------------------

function GetMaxToDistRectFromPointInRect(const pt: TPointD;
  const rec: TRectD): double;
var
  d: double;
begin
  with rec do
  begin
    Result := Distance(pt, TopLeft);
    d := Distance(pt, PointD(Right, Top));
    if d > Result then Result := d;
    d := Distance(pt, BottomRight);
    if d > Result then Result := d;
    d := Distance(pt, PointD(Left, Bottom));
    if d > Result then Result := d;
  end;
end;
//------------------------------------------------------------------------------

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  const pivot: TPointD; buttonSize: integer;
  pivotButtonColor, angleButtonColor: TColor32;
  initialAngle: double; angleOffset: double;
  enablePivotMove: Boolean;
  buttonLayerClass: TButtonDesignerLayer32Class): TRotatingGroupLayer32;
var
  rec: TRectD;
  radius: integer;
begin
  if not assigned(targetLayer) or
    not (targetLayer is THitTestLayer32) then
      raise Exception.Create(rsCreateButtonGroupError);

  Result := TRotatingGroupLayer32(targetLayer.RootOwner.AddLayer(
    TRotatingGroupLayer32, nil, rsRotatingButtonGroup));

  radius := Min(targetLayer.Width, targetLayer.Height) div 2;
  if PointsNearEqual(pivot, targetLayer.MidPoint, 1) then
    rec := RectD(targetLayer.Bounds)
  else
    rec := RectD(pivot.X -radius, pivot.Y -radius,
      pivot.X +radius,pivot.Y +radius);

  Result.Init(Rect(rec), buttonSize,
    pivotButtonColor, angleButtonColor, initialAngle,
    angleOffset, buttonLayerClass);

  if not enablePivotMove then
    Result.PivotButton.ClearHitTesting;
end;
//------------------------------------------------------------------------------

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  buttonSize: integer;
  pivotButtonColor: TColor32;
  angleButtonColor: TColor32;
  initialAngle: double; angleOffset: double;
  enablePivotMove: Boolean;
  buttonLayerClass: TButtonDesignerLayer32Class): TRotatingGroupLayer32;
var
  pivot: TPointD;
begin
  pivot := PointD(Img32.Vector.MidPoint(targetLayer.Bounds));
  Result := CreateRotatingButtonGroup(targetLayer, pivot, buttonSize,
    pivotButtonColor, angleButtonColor, initialAngle, angleOffset,
    enablePivotMove, buttonLayerClass);
end;
//------------------------------------------------------------------------------

function UpdateRotatingButtonGroup(rotateButton: TLayer32): double;
var
  rec: TRect;
  mp, pt2: TPointD;
  i, radius: integer;
  rotateGroup: TRotatingGroupLayer32;
begin

  rotateGroup := nil;
  if assigned(rotateButton) then
  begin
    if rotateButton is TRotatingGroupLayer32 then
      rotateGroup := TRotatingGroupLayer32(rotateButton)
    else if (rotateButton.GroupOwner is TRotatingGroupLayer32) then
      rotateGroup := TRotatingGroupLayer32(rotateButton.GroupOwner);
  end;
  if not assigned(rotateGroup) then
        raise Exception.Create(rsUpdateRotateGroupError);

  with rotateGroup do
  begin
    mp := PivotButton.MidPoint;
    pt2 := AngleButton.MidPoint;
    radius := Round(Distance(mp, pt2));
    rec := Rect(RectD(mp.X -radius, mp.Y -radius, mp.X +radius,mp.Y +radius));
    DesignLayer.SetBounds(rec);
    i :=  DpiAwareI *2;
    DrawDashedLine(Child[0].Image, Ellipse(Rect(i,i,radius*2 -i, radius*2 -i)),
      dashes, nil, i, clRed32, esPolygon);
    Result := Angle;
  end;
end;
//------------------------------------------------------------------------------

function CreateButtonGroup(groupOwner: TGroupLayer32;
  const buttonPts: TPathD; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonGroupLayer32;
var
  i: integer;
begin
  if not assigned(groupOwner) then
    raise Exception.Create(rsCreateButtonGroupError);

  Result := TButtonGroupLayer32(groupOwner.AddChild(TButtonGroupLayer32));
  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  Result.fBtnSize := buttonSize;
  Result.fBtnShape := buttonShape;
  Result.fBtnColor := buttonColor;
  Result.fBbtnLayerClass := buttonLayerClass;

  for i := 0 to high(buttonPts) do
  begin
    Result.AddButton(buttonPts[i]);
    Result[i].CursorId := crSizeAll;
  end;
end;
//------------------------------------------------------------------------------

procedure InitDashes;
begin
  setLength(dashes, 2);
  dashes[0] := DpiAwareI *2; dashes[1] := DpiAwareI *4;
end;

initialization
  InitDashes;
  DefaultButtonSize := DpiAwareI*10;

end.
