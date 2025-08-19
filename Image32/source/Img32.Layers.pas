unit Img32.Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.9                                                             *
* Date      :  9 August 2025                                                   *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Layered images support                                          *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
  {$IFDEF USE_FILE_STORAGE} TypInfo, {$ENDIF}
  Img32, Img32.Storage, Img32.Draw, Img32.Extra,
  Img32.Vector, Img32.Transform;
type
  TSizingStyle = (ssCorners, ssEdges, ssEdgesAndCorners);
  TButtonShape = Img32.Extra.TButtonShape;

  TLayer32 = class;
  TGroupLayer32 = class;
  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;

  //THitTest is used for hit-testing (see TLayeredImage32.GetLayerAt).
  THitTest = class
    htImage     : TImage32;
    enabled     : Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TUpdateMethod = (umNone, umSelf, umChild);
  TUpdateInfo = record
    updateMethod  : TUpdateMethod;
    priorPosition : TRectD; //ie update the previous position when moved.
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

  //TLayer32 - base layer class
  TLayer32 = class(TStorage)
  private
    fLeft           : double;
    fTop            : double;
    fWidth          : double;
    fHeight         : double;
    fOuterMargin    : double;   // the drawing margin around a layer
    fImage          : TImage32;
    fMergeImage     : TImage32; // contains a merge of child images
    fClipImage      : TImage32; // used to restrict child drawing inside a layer
    fVisible        : Boolean;
    fOpacity        : Byte;
    fCursorId       : integer;
    fUserData       : TObject;
    fBlendFunc      : TBlendFunction;   // defaults to BlendToAlpha
    fLayeredImage   : TLayeredImage32;
    fClipPath       : TPathsD;          // used in conjunction with fClipImage
    fNonLayerChilds : Boolean;
{$IFDEF USE_FILE_STORAGE}
    fStreamingRec   : TRectWH;
    procedure SetLeft(value: double);
    procedure SetTop(value: double);
{$ENDIF}
    function  GetMidPoint: TPointD;
    procedure SetHeight(value: double);
    procedure SetWidth(value: double);
    procedure SetBlendFunc(func: TBlendFunction);
    function  GetChild(index: integer): TLayer32;
    function  GetRoot: TGroupLayer32;
    function  FindLayerNamed(const name: string): TLayer32; virtual;
    procedure SetClipPath(const path: TPathsD);
    function  GetNextLayerInGroup: TLayer32;
    function  GetPrevLayerInGroup: TLayer32;
    function  GetLayer32Parent: TLayer32;
    procedure SetLayer32Parent(parent: TLayer32);
    procedure CreateInternal(parent: TStorage = nil; const name: string = '');
  protected
    fIsDesignLayer  : Boolean;
    fUpdateInfo     : TUpdateInfo;
    // children may not always be TLayer32 objects
    function  GetStorageChild(index: integer): TStorage;
    function  AddStorageChild(storeClass: TStorageClass): TStorage;
    function  InsertStorageChild(idx: integer; storeClass: TStorageClass): TStorage;
    function  ChildNotTLayer32(childIdx: integer): Boolean;
    procedure SetVisible(value: Boolean); virtual;
    procedure SetDesignerLayer(value: Boolean);
    procedure SetOuterMargin(value: double); virtual;
    function  GetUpdateNeeded: Boolean;
    procedure DoBeforeMerge; virtual;
    procedure DoAfterMerge; virtual;
    procedure PreMerge(hideDesigners: Boolean); virtual;
    procedure MergeChild(child: TLayer32;
      dstImg: TImage32; hideDesigners: Boolean; const updateRect: TRect);
    procedure Merge(hideDesigners: Boolean; updateRect: TRect);
    function  GetLayerAt(const pt: TPointD; ignoreDesigners: Boolean): TLayer32;
    function  RemoveChildFromList(index: integer): TStorage; override;
    function  GetInnerRectD: TRectD;   // in client (ie layer) coordinates
    function  GetInnerBounds: TRectD;  // in fLayeredImage coordinates
    function  GetOuterBounds: TRectD;  // margin inflated GetInnerBounds
{$IFDEF USE_FILE_STORAGE}
    procedure BeginRead; override;
    procedure EndRead; override;
{$ENDIF}
    procedure SetOpacity(value: Byte); virtual;
    procedure ImageChanged(Sender: TImage32); virtual;
    procedure UpdateLayeredImage(newLayeredImage: TLayeredImage32);
    property  UpdateInfo: TUpdateInfo read fUpdateInfo;
    property  UpdateNeeded  : Boolean read GetUpdateNeeded;
    property  MergeImage    : TImage32 read fMergeImage; // a merge of child images
  public
    constructor Create(parent: TStorage = nil;
      const name: string = ''); overload; override;
    constructor Create(parent: TLayer32;
      const name: string = ''); reintroduce; overload; virtual;
    destructor Destroy; override;
    function   BringForwardOne: Boolean;
    function   SendBackOne: Boolean;
    function   BringToFront: Boolean;
    function   SendToBack: Boolean;
    function   Move(newParent: TLayer32; idx: integer): Boolean;

    // convert relative to absolute and absolute to relative functions
    // where absolute is still relative to the container TLayeredImage32
    function   MakeAbsolute(const pt: TPointD): TPointD; overload;
    function   MakeAbsolute(const rec: TRectD): TRectD; overload;
    function   MakeRelative(const pt: TPoint): TPoint; overload;
    function   MakeRelative(const pt: TPointD): TPointD; overload;
    function   MakeRelative(const rec: TRectD): TRectD; overload;

    // positioning functions
    procedure  PositionAt(const pt: TPointD); overload;
    procedure  PositionAt(x, y: double); overload; virtual;
    procedure  PositionCenteredAt(X, Y: double); overload;
    procedure  PositionCenteredAt(const pt: TPointD); overload;
    procedure  SetInnerBounds(const newBounds: TRectD); virtual;
    procedure  SetSize(width, height: double);

    procedure  Invalidate; virtual;

    function   AddChild(layerClass: TLayer32Class;
      const name: string = ''): TLayer32; reintroduce; virtual;
    function   InsertChild(index: integer; layerClass: TLayer32Class;
      const name: string = ''): TLayer32; reintroduce; overload; virtual;
    function   InsertChild(index: integer;
      storeClass: TStorageClass): TStorage;  overload; override;
    procedure  ClearChildren; override;
    function   Contains(layer: TLayer32): Boolean;

    property   Child[index: integer]: TLayer32 read GetChild; default;
    //ClipPath: defines a region that's inside the layer's rectangular region.
    //Portions of child layers residing outside this region  will be clipped.
    property   ClipPath: TPathsD read fClipPath write SetClipPath;
    procedure  Offset(dx, dy: double); overload; virtual;
    property   IsDesignerLayer: Boolean
      read fIsDesignLayer write SetDesignerLayer;
    property   InnerBounds: TRectD read GetInnerBounds;
    property   InnerRect: TRectD read GetInnerRectD;
    property   OuterBounds: TRectD read GetOuterBounds;
    property   Image: TImage32 read fImage;
    property   MidPoint: TPointD read GetMidPoint;
    property   Parent: TLayer32 read GetLayer32Parent write SetLayer32Parent;
    property   Root: TGroupLayer32 read GetRoot;
    property   RootOwner: TLayeredImage32 read fLayeredImage;
    property   UserData: TObject read fUserData write fUserData;
    property   BlendFunc: TBlendFunction read fBlendFunc write SetBlendFunc;
    property   PrevLayerInGroup: TLayer32 read GetPrevLayerInGroup;
    property   NextLayerInGroup: TLayer32 read GetNextLayerInGroup;
{$IFDEF USE_FILE_STORAGE}
  published
    property   Left: double read fLeft write SetLeft;
    property   Top: double read fTop write SetTop;
{$ELSE}
    property   Left: double read fLeft;
    property   Top: double read fTop;
{$ENDIF}
    property   CursorId: integer read fCursorId write fCursorId;
    property   Height: double read fHeight write SetHeight;
    property   Opacity: Byte read fOpacity write SetOpacity;
    property   OuterMargin: double read fOuterMargin write SetOuterMargin;
    property   Visible: Boolean read fVisible write SetVisible;
    property   Width: double read fWidth write SetWidth;
  end;

  TGroupLayer32 = class(TLayer32)
  protected
    procedure  UpdateGroupBounds;
    procedure  PreMerge(hideDesigners: Boolean); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure  Invalidate; override;
    procedure  ClearChildren; override;
    procedure  Offset(dx, dy: double); override;
  end;

  THitTestLayer32 = class(TLayer32) //abstract class
  private
    fHitTest  : THitTest;
    procedure ClearHitTesting;
    function  GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
  protected
    procedure ImageChanged(Sender: TImage32); override;
    property  HitTest : THitTest read fHitTest write fHitTest;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure UpdateHitTestMask;
    property HitTestEnabled: Boolean read GetEnabled write SetEnabled;
  end;

  //TRotLayer32: rotating and scaling methods added
  //(abstract base layer for TVectorLayer32 and TRasterLayer32)
  TRotLayer32 = class(THitTestLayer32)
  private
    fAngle      : double;
    fScaleX     : double;
    fScaleY     : double;
    fPivotPt    : TPointD;
    fAutoPivot  : Boolean;
    function  GetPivotPt: TPointD;
    procedure SetAutoPivot(val: Boolean);
    procedure SetAngle(newAngle: double);
    procedure Scale(sx, sy: double); virtual;
  protected
    procedure SetPivotPt(const pivot: TPointD); virtual;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function    Rotate(angleDelta: double): Boolean; virtual;
    procedure Reset;
    procedure Offset(dx, dy: double); override;
{$IFDEF USE_FILE_STORAGE}
  published
{$ENDIF}
    property  Angle: double read fAngle write SetAngle;
    property  PivotPt: TPointD read GetPivotPt write SetPivotPt;
    property  AutoPivot: Boolean read fAutoPivot write SetAutoPivot;
  end;

  //TVectorLayer32: either repositions when Paths change,
  //or transforms Paths when bounds change
  TVectorLayer32 = class(TRotLayer32)
  private
    fPaths      : TPathsD;
    fIsDrawing  : Boolean;
    fOnDraw     : TNotifyEvent;
    procedure RepositionAndDraw;
    function  GetRelativePaths: TPathsD;
  protected
    procedure SetPaths(const newPaths: TPathsD); virtual;
    procedure Draw; virtual;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Offset(dx,dy: double); override;
    function  Rotate(angleDelta: double): Boolean; override;
    procedure Scale(sx, sy: double); override;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD); virtual;
    procedure UpdateHitTestMaskFromImage;
    procedure AppendPoint(const pt: TPointD);
    procedure AppendPath(const path: TPathD);
    property  Paths: TPathsD read fPaths write SetPaths;
    property  PathsRelativeToLayer: TPathsD read GetRelativePaths;
    property  OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
  end;

  TRasterLayer32 = class(TRotLayer32) //display layer for raster images
  private
    fMasterImg    : TImage32;
    fAutoHitTest  : Boolean;
    fAutoCrop     : Boolean;
    fCropMargins  : TPoint;
    procedure DoAutoHitTest;
  protected
    procedure ImageChanged(Sender: TImage32); override;
    procedure UpdateHitTestMaskTranspar(compareFunc: TCompareFunction;
      referenceColor: TColor32; tolerance: integer);
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor  Destroy; override;
    procedure UpdateHitTestMaskOpaque; virtual;
    procedure UpdateHitTestMaskTransparent(alphaValue: Byte = 127); overload; virtual;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    function  Rotate(angleDelta: double): Boolean; override;
    procedure Scale(sx, sy: double); override;

    property  AutoSetHitTestMask: Boolean read fAutoHitTest write fAutoHitTest;
    property  AutoCrop          : Boolean read fAutoCrop write fAutoCrop;
    property  MasterImage       : TImage32 read fMasterImg;
  end;

  TButtonDesignerLayer32 = class;
  TButtonDesignerLayer32Class = class of TButtonDesignerLayer32;

  TSizingGroupLayer32 = class(TGroupLayer32) //groups sizing buttons
  private
    fSizingStyle: TSizingStyle;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
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
    function GetDesignLayer: TLayer32;
  protected
    procedure Init(const rec: TRect; buttonSize: integer;
      centerButtonColor, movingButtonColor: TColor32;
      startingAngle: double; startingZeroOffset: double;
      buttonLayerClass: TButtonDesignerLayer32Class); virtual;
    property DesignLayer: TLayer32 read GetDesignLayer;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    property Angle: double read GetAngle;
    property PivotPoint: TPointD read GetPivot;
    property AngleButton: TButtonDesignerLayer32 read GetAngleBtn;
    property PivotButton: TButtonDesignerLayer32 read GetPivotBtn;
    property DistBetweenButtons: double read GetDistance;
  end;

  TButtonGroupLayer32 = class(TGroupLayer32) //groups generic buttons
  private
    fBtnSize  : integer;
    fBtnShape : TButtonShape;
    fBtnColor : TColor32;
    fBtnLayerClass: TButtonDesignerLayer32Class;
    fBtnCount : integer;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function AddButton(const pt: TPointD): TButtonDesignerLayer32;
    function InsertButton(const pt: TPointD; btnIdx: integer): TButtonDesignerLayer32;
    property ButtonCnt: integer read fBtnCount;

  end;

  TButtonDesignerLayer32 = class(THitTestLayer32) //button (design) layer
  private
    fSize     : integer;
    fColor    : TColor32;
    fShape    : TButtonShape;
    fEnabled  : Boolean;
    fOutline  : TPathD;
    fBtnIdx   : integer;
    procedure SetEnabled(value: Boolean);
  protected
    procedure SetButtonAttributes(const shape: TButtonShape;
      size: integer; color: TColor32); virtual;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure Draw; virtual;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD); virtual;

    property Enabled  : Boolean read fEnabled write SetEnabled;
    property Size     : integer read fSize write fSize;
    property Color    : TColor32 read fColor write fColor;
    property Shape    : TButtonShape read fShape write fShape;
    property ButtonOutline: TPathD read fOutline write fOutline;
    //BtnIdx: useful when Buttons are 'moved' (ie their level changed)
    property BtnIdx   : integer read fBtnIdx;
  end;

  TUpdateType = (utUndefined, utShowDesigners, utHideDesigners);

  TLayeredImage32 = class(TStorage)
  private
    fRoot              : TGroupLayer32;
    fBounds            : TRect;
    fBackColor         : TColor32;
    fResampler         : integer;
    fLastUpdateType    : TUpdateType;
    fInvalidRect       : TRectD;
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
    function GetRepaintNeeded: Boolean;
  protected
    property  InvalidRect: TRectD read fInvalidRect;
  public
    constructor Create(parent: TStorage = nil; const name: string = ''); overload; override;
    constructor Create(Width, Height: integer); reintroduce; overload; virtual;
    procedure SetSize(width, height: integer);
    procedure Clear;
    procedure Invalidate;
    function  InsertChild(index: integer;
      storeClass: TLayer32Class): TLayer32; reintroduce; overload; virtual;
    function  AddLayer(layerClass: TLayer32Class = nil;
      parent: TLayer32 = nil; const name: string = ''): TLayer32;
    function  InsertLayer(layerClass: TLayer32Class; parent: TLayer32;
      index: integer; const name: string = ''): TLayer32;
    procedure DeleteLayer(layer: TLayer32); overload;
    procedure DeleteLayer(layerIndex: integer;
      parent: TLayer32 = nil); overload;
    function  FindLayerNamed(const name: string): TLayer32;
    function  GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean = false): TLayer32; overload;
    function  GetLayerAt(const pt: TPointD; ignoreDesigners: Boolean = false): TLayer32; overload;
    function  GetMergedImage(hideDesigners: Boolean = false): TImage32; overload;
    function  GetMergedImage(hideDesigners: Boolean;
      out updateRect: TRect): TImage32; overload;
    function  DetachRoot: TGroupLayer32;
    function  AttachRoot(newRoot: TGroupLayer32): Boolean;

    property Bounds: TRect read fBounds;
    property Count: integer read GetRootLayersCount;
    property Image: TImage32 read GetImage;
    property Layer[index: integer]: TLayer32 read GetLayer; default;
    property MidPoint: TPointD read GetMidPoint;
    property Root: TGroupLayer32 read fRoot;
    property RepaintNeeded : Boolean read GetRepaintNeeded;
{$IFDEF USE_FILE_STORAGE}
  published
{$ENDIF}
    property BackgroundColor: TColor32 read fBackColor write SetBackColor;
    property Resampler: integer read fResampler write SetResampler;
    property Height: integer read GetHeight write SetHeight;
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
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TRotatingGroupLayer32; overload;

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  buttonSize: integer = 0;
  pivotButtonColor: TColor32 = clWhite32;
  angleButtonColor: TColor32 = clBlue32;
  initialAngle: double = 0; angleOffset: double = 0;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TRotatingGroupLayer32; overload;

function CreateButtonGroup(parent: TLayer32;
  const buttonPts: TPathD; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonGroupLayer32;

function UpdateSizingButtonGroup(movedButton: TLayer32): TRect;

function UpdateRotatingButtonGroup(rotateButton: TLayer32): double;

var
  DefaultButtonSize: integer;
  dashes: TArrayOfDouble;

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

  {$IFDEF USING_FMX}
  uses Img32.FMX;
  {$ENDIF}

resourcestring
  rsRoot                   = 'root';
  rsButton                 = 'Button';
  rsSizingButtonGroup      = 'SizingButtonGroup';
  rsRotatingButtonGroup    = 'RotatingButtonGroup';
  rsCreateButtonGroupError = 'CreateButtonGroup - invalid target layer';
  rsUpdateRotateGroupError = 'UpdateRotateGroup - invalid group';
  rsLayeredImage32Error    = 'TLayeredImage32: ''root'' must be a TGroupLayer32';
  rsLayer32Error           = 'TLayer32 child expected';
  rsVectorLayer32Error     = 'TVectorLayer32 - updating Paths during draw events will cause recursion.';

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
// THitTest
//------------------------------------------------------------------------------

constructor THitTest.Create;
begin
  htImage := TImage32.Create;
  htImage.BlockNotify; //ie never notifies :)
  enabled := true;
end;
//------------------------------------------------------------------------------

destructor THitTest.Destroy;
begin
  htImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// THitTest helper functions
//------------------------------------------------------------------------------

procedure UpdateHitTestMaskUsingPath(layer: THitTestLayer32; const paths: TPathsD);
begin
  with layer.Image do
    layer.HitTest.htImage.SetSize(width, height);
  if layer.Image.IsEmpty then Exit;
  layer.HitTest.enabled := true;
  DrawPolygon(layer.HitTest.htImage, paths, frEvenOdd, clWhite32);
end;
//------------------------------------------------------------------------------

//Creates a hit-test mask using the supplied image and compareFunc.
procedure UpdateHitTestMaskUsingImage(var htr: THitTest;
  objPtr: Pointer; img: TImage32; compareFunc: TCompareFunction;
  referenceColor: TColor32; tolerance: integer);
var
  i: integer;
  pSrc, pDst: PColor32;
begin
  with img do
  begin
    htr.htImage.SetSize(Width, Height);
    if htr.htImage.IsEmpty then Exit;

    pSrc := PixelBase;
    pDst := htr.htImage.PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      if compareFunc(referenceColor, pSrc^, tolerance) then
        pDst^ := clWhite32;
      inc(pSrc); inc(pDst);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TLayer32 class
//------------------------------------------------------------------------------

procedure TLayer32.CreateInternal(parent: TStorage = nil; const name: string = '');
begin
  inherited Create(parent, name);
  fImage        := TLayerNotifyImage32.Create(self);
  fVisible      := True;
  fOpacity      := 255;
  CursorId      := crDefault;
  fBlendFunc    := BlendToAlpha;

  if Assigned(parent) then
  begin
    if (parent is TLayeredImage32) then
    begin
      TLayeredImage32(parent).fRoot := TGroupLayer32(self);
      fLayeredImage := TLayeredImage32(parent);
    end else
    begin
      fLayeredImage := TLayer32(parent).fLayeredImage;
      if Assigned(fLayeredImage) then
        Image.Resampler := fLayeredImage.Resampler;
    end;
  end;

  if name <> '' then
    self.Name := name else
    self.Name := ClassName;
end;
//------------------------------------------------------------------------------

constructor TLayer32.Create(parent: TStorage; const name: string);
begin
  fIsDesignLayer := true; //must do this first
  if not Assigned(parent) then
    CreateInternal(nil, name)
  else if parent.InheritsFrom(TLayer32) then
    //this constructor is commonly overridden in descendant layer classes
    Create(TLayer32(parent), name)
  else
  begin
    //make sure this is the root layer
    if not (parent is TLayeredImage32) or
      (parent.ChildCount > 0) or
      not (self is TGroupLayer32) then
        raise Exception.Create(rsLayeredImage32Error);
    CreateInternal(parent, rsRoot);
  end;
end;
//------------------------------------------------------------------------------

constructor TLayer32.Create(parent: TLayer32; const name: string);
begin
  CreateInternal(parent, name);
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
var
  rec: TRectD;
begin
  if Assigned(Parent) and
    Assigned(fLayeredImage) and Visible then
  begin
    Invalidate;
    rec := OuterBounds;
    if not fUpdateInfo.priorPosition.IsEmpty then
    begin
      rec := Parent.MakeAbsolute(fUpdateInfo.priorPosition);
      with fLayeredImage do
        fInvalidRect := UnionRect(fInvalidRect, rec);
    end;
  end;
  FreeAndNil(fImage);
  FreeAndNil(fMergeImage);
  FreeAndNil(fClipImage);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetDesignerLayer(value: Boolean);
begin
  fIsDesignLayer := value;
end;
//------------------------------------------------------------------------------

function TLayer32.GetLayer32Parent: TLayer32;
var
  stgParent: TStorage;
begin
  stgParent := inherited Parent;
  if (stgParent is TLayer32) then
    Result := TLayer32(stgParent) else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetLayer32Parent(parent: TLayer32);
begin
  if inherited Parent = parent then Exit;
  SetParent(parent);
  if Visible then Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.GetUpdateNeeded: Boolean;
begin
  Result := (fUpdateInfo.updateMethod <> umNone);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Invalidate;
var
  layer : TLayer32;
begin
  if (fUpdateInfo.updateMethod = umSelf) then Exit;
  fUpdateInfo.updateMethod := umSelf;

  layer := Parent;
  while Assigned(layer) do
  begin
    if layer.fUpdateInfo.updateMethod <> umNone then Break;
    layer.fUpdateInfo.updateMethod := umChild;
    layer := layer.Parent;
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.GetNextLayerInGroup: TLayer32;
begin
  if not Assigned(Parent) or (Index = Parent.ChildCount -1) then
    Result := nil else
    Result := Parent.Childs[Index +1];
end;
//------------------------------------------------------------------------------

function TLayer32.GetPrevLayerInGroup: TLayer32;
begin
  if not Assigned(Parent) or (Index = 0) then
    Result := nil else
    Result := Parent.Childs[Index -1];
end;
//------------------------------------------------------------------------------

procedure TLayer32.ImageChanged(Sender: TImage32);
begin
{$IFDEF USE_FILE_STORAGE}
  if (StorageState = ssLoading) then Exit;
{$ENDIF}
  fWidth := Image.Width -fOuterMargin * 2;
  fHeight := Image.Height -fOuterMargin * 2;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetSize(width, height: double);
var
  w,h: integer;
begin
{$IFDEF USE_FILE_STORAGE}
  if StorageState = ssDestroying then Exit;
{$ENDIF}
  fWidth := width; fHeight := height;
  w := Ceil(fWidth + fOuterMargin * 2);
  h := Ceil(fHeight + fOuterMargin * 2);
  Image.SetSize(w, h);
end;
//------------------------------------------------------------------------------

procedure  TLayer32.SetInnerBounds(const newBounds: TRectD);
begin
  fWidth := newBounds.Width;
  fHeight := newBounds.Height;
  Image.BlockNotify;
  Image.SetSize(Ceil(fWidth + fOuterMargin * 2),
    Ceil(fHeight + fOuterMargin * 2));
  Image.UnBlockNotify;
  PositionAt(newBounds.Left, newBounds.Top);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.GetInnerRectD: TRectD;
begin
  Result := RectD(0, 0, fWidth, fHeight);
end;
//------------------------------------------------------------------------------

function TLayer32.GetInnerBounds: TRectD;
begin
  Result := RectD(fLeft, fTop, fLeft + fWidth, fTop + fHeight)
end;
//------------------------------------------------------------------------------

function TLayer32.GetOuterBounds: TRectD;
begin
  Result.Left := fLeft-fOuterMargin;
  Result.Top  :=  fTop-fOuterMargin;
  Result.Right := fLeft + fWidth +fOuterMargin;
  Result.Bottom := fTop + fHeight +fOuterMargin;
end;
//------------------------------------------------------------------------------

function TLayer32.GetMidPoint: TPointD;
begin
  Result := Img32.Vector.MidPoint(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(const pt: TPointD);
begin
  PositionAt(pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(x, y: double);
begin
  if (fLeft = x) and (fTop = y) then Exit;
  fLeft := x; fTop := y;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(X, Y: double);
begin
  PositionAt(X - fWidth  * 0.5, Y - fHeight * 0.5);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(const pt: TPointD);
begin
  PositionCenteredAt(Pt.X, Pt.Y);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Offset(dx, dy: double);
begin
  PositionAt(fLeft + dx, fTop + dy);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetVisible(value: Boolean);
begin
  if (value = fVisible) or (Root = Self) then Exit;
  fVisible := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

{$IFDEF USE_FILE_STORAGE}
procedure TLayer32.SetLeft(value: double);
begin
  fLeft := value;
  fStreamingRec.Left := value;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetTop(value: double);
begin
  fTop := value;
  fStreamingRec.Top := value;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TLayer32.SetHeight(value: double);
begin
  SetSize(fWidth, value);
{$IFDEF USE_FILE_STORAGE}
  fStreamingRec.Height := fHeight;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetWidth(value: double);
begin
  SetSize(value, fHeight);
{$IFDEF USE_FILE_STORAGE}
  fStreamingRec.Width := fWidth;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetOuterMargin(value: double);
begin
  if fOuterMargin = value then Exit;
  fOuterMargin := value;
  Image.BlockNotify;
  Image.SetSize(Ceil(fWidth + fOuterMargin * 2),
    Ceil(fHeight + fOuterMargin * 2));
  Image.UnBlockNotify;
end;
//------------------------------------------------------------------------------

{$IFDEF USE_FILE_STORAGE}
procedure TLayer32.BeginRead;
var
  stgParent: TStorage;
begin
  inherited;
  fStreamingRec := RectWH(0, 0, -Infinity, -Infinity);

  stgParent := inherited Parent;
  if Assigned(Parent) then
    fLayeredImage := Parent.fLayeredImage
  else if Assigned(stgParent) and (stgParent is TLayeredImage32) then
    fLayeredImage := TLayeredImage32(stgParent);
end;
//------------------------------------------------------------------------------

procedure TLayer32.EndRead;
begin
  if fStreamingRec.IsValid then
      SetInnerBounds(fStreamingRec.RectD);
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TLayer32.SetOpacity(value: Byte);
begin
  if value = fOpacity then Exit;
  fOpacity := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.BringForwardOne: Boolean;
begin
  Result := assigned(Parent) and (index < Parent.ChildCount -1);
  if not Result then Exit;
  Parent.Childs.Move(index, index +1);
  Parent.ReindexChilds(index);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.SendBackOne: Boolean;
begin
  Result := assigned(Parent) and (index > 0);
  if not Result then Exit;
  Parent.Childs.Move(index, index -1);
  Parent.ReindexChilds(index -1);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.BringToFront: Boolean;
begin
  Result := assigned(Parent) and (index < Parent.ChildCount -1);
  if not Result then Exit;
  Parent.Childs.Move(index, Parent.ChildCount -1);
  Parent.ReindexChilds(index);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.SendToBack: Boolean;
begin
  Result := assigned(Parent) and (index > 0);
  if not Result then Exit;
  Parent.Childs.Move(index, 0);
  Parent.ReindexChilds(0);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.MakeAbsolute(const pt: TPointD): TPointD;
var
  layer: TLayer32;
begin
  Result := pt;
  layer := Self;
  while assigned(layer) do
  begin
    if not (layer is TGroupLayer32) then
      Result := TranslatePoint(Result, layer.Left, layer.Top);
    layer := layer.Parent;
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.MakeAbsolute(const rec: TRectD): TRectD;
var
  tmp: TPointD; //accommodates older compilers that lack record methods
begin
  tmp := MakeAbsolute(rec.TopLeft);
  Result.Left := tmp.X;
  Result.Top := tmp.Y;
  Result.Right := rec.Right + tmp.X - rec.Left;
  Result.Bottom := rec.Bottom + tmp.Y - rec.Top;
end;
//------------------------------------------------------------------------------

function TLayer32.MakeRelative(const pt: TPoint): TPoint;
begin
  Result := Point(MakeAbsolute(NullPointD));
  Result.X := pt.X - Result.X;
  Result.Y := pt.Y - Result.Y;
end;
//------------------------------------------------------------------------------

function TLayer32.MakeRelative(const pt: TPointD): TPointD;
begin
  Result := MakeAbsolute(NullPointD);
  Result.X := pt.X - Result.X;
  Result.Y := pt.Y - Result.Y;
end;
//------------------------------------------------------------------------------

function TLayer32.MakeRelative(const rec: TRectD): TRectD;
var
  tmp: TPointD; //to accommodate older compilers without record methods
begin
  tmp := MakeRelative(rec.TopLeft);
  Result.Left := tmp.X;
  Result.Top := tmp.Y;
  tmp := MakeRelative(rec.BottomRight);
  Result.Right := tmp.X;
  Result.Bottom := tmp.Y;
end;
//------------------------------------------------------------------------------

function TLayer32.Move(newParent: TLayer32; idx: integer): Boolean;
var
  layer: TLayer32;
begin
  Result := false;
  if not assigned(Parent) or not assigned(newParent) then
    Exit;

  Invalidate; // at old location

  //make sure we don't create circular parenting
  layer := newParent;
  while Assigned(layer) and (layer is TLayer32) do
    if (layer = self) then Exit
    else layer := Layer.Parent;

  if newParent = Parent then
  begin
    if idx >= Parent.ChildCount then
      idx := Parent.ChildCount -1
    else if idx < 0 then idx := 0;
    if idx = Index then Exit;

    Parent.Childs.Move(Index, idx);
    Parent.ReindexChilds(Min(idx, Index));
  end else
  begin
    Parent := newParent; //ie appends to newParent Childs list
    if idx >= newParent.ChildCount then
      idx := newParent.ChildCount -1
    else if idx < 0 then idx := 0;
    Parent.Childs.Move(Index, idx);
    Parent.ReindexChilds(idx);
  end;
  fUpdateInfo.updateMethod := umNone;
  Invalidate; // at new position :)
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetBlendFunc(func: TBlendFunction);
begin
  if not Assigned(Parent) then Exit;
  fBlendFunc := func;
  if Visible then
    Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.GetChild(index: integer): TLayer32;
begin
  Result := TLayer32(inherited GetChild(index));
end;
//------------------------------------------------------------------------------

function TLayer32.GetStorageChild(index: integer): TStorage;
begin
  Result := inherited GetChild(index);
end;
//------------------------------------------------------------------------------

function TLayer32.Contains(layer: TLayer32): Boolean;
begin
  while Assigned(layer.Parent) and (layer.Parent <> self) do
    layer := layer.Parent;
  Result := Assigned(layer.Parent);
end;
//------------------------------------------------------------------------------

procedure TLayer32.ClearChildren;
begin
  inherited;
  FreeAndNil(fMergeImage);
  fClipPath := nil;
end;
//------------------------------------------------------------------------------

function TLayer32.AddChild(layerClass: TLayer32Class;
  const name: string = ''): TLayer32;
begin
  Result := InsertChild(MaxInt, layerClass, name);
end;
//------------------------------------------------------------------------------

function TLayer32.AddStorageChild(storeClass: TStorageClass): TStorage;
begin
  Result := InsertStorageChild(MaxInt, storeClass);
end;
//------------------------------------------------------------------------------

function TLayer32.InsertStorageChild(idx: integer;
  storeClass: TStorageClass): TStorage;
begin
  Result := inherited InsertChild(idx, storeClass);
  if not (Result is TLayer32) then fNonLayerChilds := true;
end;
//------------------------------------------------------------------------------

function TLayer32.ChildNotTLayer32(childIdx: integer): Boolean;
begin
  Result := not (GetStorageChild(childIdx) is TLayer32);
end;
//------------------------------------------------------------------------------

function TLayer32.InsertChild(index: integer; layerClass: TLayer32Class;
  const name: string = ''): TLayer32;
begin
  Result := layerClass.Create(self, name) as TLayer32;
  if index < ChildCount -1 then
  begin
    Childs.Move(Result.Index, index);
    ReindexChilds(index);
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  // must use explicit InsertStorageChild() for non-TLayer32 children
  if not storeClass.InheritsFrom(TLayer32) then
    raise Exception.Create(rsLayer32Error);
  Result := InsertChild(index, TLayer32Class(storeClass), '');
end;
//------------------------------------------------------------------------------

function TLayer32.GetRoot: TGroupLayer32;
begin
  if Assigned(fLayeredImage) then
    Result := fLayeredImage.fRoot else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetClipPath(const path: TPathsD);
var
  pp: TPathsD;
begin
  Invalidate;
  fClipPath := path;
  if Assigned(fClipPath) and (self is THitTestLayer32) then
  begin
    //create a clip mask
    if Assigned(fClipImage) then
      fClipImage.SetSize(Image.Width, Image.Height) else
      fClipImage := TImage32.Create(Image.Width, Image.Height);
    pp := TranslatePath(path, fOuterMargin, fOuterMargin);
    DrawPolygon(fClipImage, pp, frEvenOdd, clWhite32);
  end else
    FreeAndNil(fClipImage);
end;
//------------------------------------------------------------------------------

function TLayer32.RemoveChildFromList(index: integer): TStorage;
begin
  Result := inherited RemoveChildFromList(index);
  if ChildCount = 0 then FreeAndNil(fMergeImage);
end;
//------------------------------------------------------------------------------

procedure TLayer32.DoBeforeMerge;
begin
end;
//------------------------------------------------------------------------------

procedure TLayer32.DoAfterMerge;
begin
end;
//------------------------------------------------------------------------------

procedure TLayer32.PreMerge(hideDesigners: Boolean);
var
  i: integer;
  childLayer: TLayer32;
  rec: TRectD;
begin
  //this method is recursive and updates each group's fInvalidRect
  for i := 0 to ChildCount -1 do
  begin
    if fNonLayerChilds and ChildNotTLayer32(i) then Continue;

    childLayer := Child[i];
    with childLayer do
    begin
      if not Visible or
         (hideDesigners and IsDesignerLayer) or
         (fUpdateInfo.updateMethod = umNone) then
           Continue;

      if fUpdateInfo.updateMethod = umSelf then
      begin
        rec := Parent.MakeAbsolute(fUpdateInfo.priorPosition);
        with fLayeredImage do
          fInvalidRect := UnionRect(fInvalidRect, rec);

        // and repeat with updated fUpdateInfo.priorPosition
        fUpdateInfo.priorPosition := OuterBounds;
        rec := Parent.MakeAbsolute(fUpdateInfo.priorPosition);
        with fLayeredImage do
          fInvalidRect := UnionRect(fInvalidRect, rec);
      end;

      // premerge children - updates each visible child's fInvalidRect
      DoBeforeMerge;
      PreMerge(hideDesigners);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.MergeChild(child: TLayer32;
  dstImg: TImage32; hideDesigners: Boolean; const updateRect: TRect);
var
  childImg  : TImage32;
  childImg2 : TImage32;
  srcRect   : TRect;
  dstRect   : TRect;
  rec, rec2 : TRect;
begin
    with child do
    begin
      if not visible or (hideDesigners and IsDesignerLayer) then Exit;

      //recursive merge
      if (fUpdateInfo.updateMethod <> umNone) then
        Merge(hideDesigners, updateRect);

      if Assigned(fMergeImage) then
        childImg := fMergeImage else
        childImg := Image;

      dstRect := Rect(OuterBounds);
      if (self = Root) then
      begin
        //root layer
        Types.IntersectRect(dstRect, dstRect, fLayeredImage.Bounds);
        Types.IntersectRect(dstRect, dstRect, updateRect);
      end else
      begin
        //childs of group layers are positioned
        //independently of the group layer's positioning
        if (self is TGroupLayer32) then
          TranslateRect(dstRect, Floor(-self.Left), Floor(-self.Top));
        Types.IntersectRect(dstRect, dstRect, self.Image.Bounds);
      end;

      if IsEmptyRect(dstRect) then Exit;

      //get srcRect (offset to childLayer coords)
      //and further adjust dstRect to accommodate OuterMargin
      srcRect.Left := Floor(dstRect.Left - fLeft + fOuterMargin);
      srcRect.Top := Floor(dstRect.Top - fTop + fOuterMargin);
      srcRect.Right := srcRect.Left + RectWidth(dstRect);
      srcRect.Bottom := srcRect.Top + RectHeight(dstRect);
    end;

    if (self is TGroupLayer32) then
      TranslateRect(srcRect, Floor(fLeft), Floor(fTop))
    else //nb: offsetting **dstRect** below
      TranslateRect(dstRect,
        Round(fOuterMargin), Round(fOuterMargin));

    //DRAW THE CHILD  ONTO THE PARENT'S IMAGE

    childImg2 := nil;
    try
      if (child.Opacity < 254) or Assigned(fClipPath) then
      begin
        childImg2 := TImage32.Create(childImg);
        childImg2.ReduceOpacity(child.Opacity);
        if Assigned(fClipImage) then
        begin
          //use the clipping mask to 'trim' childLayer's image
          rec := fClipImage.Bounds;
          rec2 := rec;
          TranslateRect(rec2,
            Floor(child.fOuterMargin -child.Left -fOuterMargin),
            Floor(child.fOuterMargin -child.Top -fOuterMargin));
          childImg2.CopyBlend(fClipImage, rec, rec2, BlendMaskLine);
        end;
      end else
        childImg2 := childImg;

      if Assigned(child.BlendFunc) then
        dstImg.CopyBlend(childImg2, srcRect, dstRect, child.BlendFunc) else
        dstImg.Copy(childImg2, srcRect, dstRect);
    finally
      if childImg2 <> childImg then
        childImg2.Free;
    end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.Merge(hideDesigners: Boolean; updateRect: TRect);
var
  i: integer;
  img: TImage32;
begin
  //layers with children will merge to fMergeImage to preserve its own image.
  //that fMergeImage will then merge with its parent fMergeImage until root.
  if not (self is TGroupLayer32) then
    TranslateRect(updateRect, -Floor(fLeft), -Floor(fTop));

  if (self is TGroupLayer32) or (ChildCount = 0) then
  begin
    //safe to merge any child images onto self.Image
    img := fImage;
  end else
  begin
    if not Assigned(fMergeImage) then
      fMergeImage := TImage32.Create;
    fMergeImage.Assign(fImage);
    img := fMergeImage;
  end;

  img.BlockNotify;
  try
    //merge redraw all children
    for i := 0 to ChildCount -1 do
    begin
      if fNonLayerChilds and ChildNotTLayer32(i) then Continue;
      MergeChild(child[i], img, hideDesigners, updateRect);
    end;
  finally
    DoAfterMerge;
    img.UnblockNotify;
  end;

  with fUpdateInfo do
  begin
    priorPosition := OuterBounds;
    updateMethod := umNone;
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.GetLayerAt(const pt: TPointD; ignoreDesigners: Boolean): TLayer32;
var
  i: integer;
  childLayer: TLayer32;
  pt2: TPointD;
  Result2: TLayer32;
begin
  Result := nil;

  if (self is TGroupLayer32) then
    pt2 := pt else
    pt2 := TranslatePoint(pt, -fLeft, -fTop);

  //if 'pt2' is outside the clip mask then don't continue
  if Assigned(fClipImage) then
    if TARGB(fClipImage.Pixel[
      Round(pt2.X + fOuterMargin),
      Round(pt2.Y + fOuterMargin)]).A < 128 then Exit;

  for i := ChildCount -1 downto 0 do
  begin
    if fNonLayerChilds and ChildNotTLayer32(i) then Continue;
    childLayer := Child[i];
    with childLayer do
      if not Visible or not PtInRect(InnerBounds, pt2) or
        (ignoreDesigners and IsDesignerLayer) then
          Continue;

    if (childLayer is THitTestLayer32) then
      with THitTestLayer32(childLayer) do
        if not HitTest.enabled then
          Continue
        else if fHitTest.htImage.IsEmpty then
          Result := childLayer //ie rectangles
        else
        begin
          if TARGB(fHitTest.htImage.Pixel[
            Round(pt2.X -fLeft + fOuterMargin),
            Round(pt2.Y -fTop + fOuterMargin)]).A >= 128 then
              Result := childLayer;
          if Assigned(Result) and not childLayer.HasChildren then Exit;
        end;

    if childLayer.HasChildren then
    begin
      //recursive
      Result2 := childLayer.GetLayerAt(pt2, ignoreDesigners);
      if Assigned(Result2) then Result := Result2;
    end;
    if Assigned(Result) then Exit;
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.FindLayerNamed(const name: string): TLayer32;
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
    if fNonLayerChilds and ChildNotTLayer32(i) then Continue;
    Result := Child[i].FindLayerNamed(name);
    if assigned(Result) then Break;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.UpdateLayeredImage(newLayeredImage: TLayeredImage32);
var
  i: integer;
begin
  fLayeredImage := newLayeredImage;
  for i := 0 to ChildCount -1 do
    if fNonLayerChilds and ChildNotTLayer32(i) then
      Continue else
      Child[i].UpdateLayeredImage(newLayeredImage);
end;

//------------------------------------------------------------------------------
// TGroupLayer32 class
//------------------------------------------------------------------------------

constructor TGroupLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  SetDesignerLayer(false);
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.UpdateGroupBounds;
var
  i: integer;
  rec: TRectD;
begin
  rec := nullRectD;
  fOuterMargin := 0;
  for i := 0 to ChildCount -1 do
    if fNonLayerChilds and ChildNotTLayer32(i) then
      Continue else
      rec := UnionRect(rec, Child[i].OuterBounds);
  Image.BlockNotify;
  SetInnerBounds(rec);
  Image.UnblockNotify;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.Invalidate;
begin
  //do nothing
end;
//------------------------------------------------------------------------------

procedure  TGroupLayer32.PreMerge(hideDesigners: Boolean);
begin
  inherited;
  if (self <> Root) and (fUpdateInfo.updateMethod <> umNone) then
    UpdateGroupBounds;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.ClearChildren;
begin
  inherited;
  if Assigned(Image) then
  begin
    Image.BlockNotify;
    Image.SetSize(0, 0);
    Image.UnblockNotify;
  end;
end;
//------------------------------------------------------------------------------

procedure TGroupLayer32.Offset(dx, dy: double);
var
  i: integer;
begin
  if (dx = 0) and (dy = 0) then Exit;
  Invalidate;
  PositionAt(fLeft + dx, fTop + dy);
  for i := 0 to ChildCount -1 do
    if fNonLayerChilds and ChildNotTLayer32(i) then
      Continue else
      Child[i].Offset(dx, dy);
end;

//------------------------------------------------------------------------------
// THitTestLayer32 class
//------------------------------------------------------------------------------

constructor THitTestLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  SetDesignerLayer(false);
  fHitTest := THitTest.Create;
  fHitTest.enabled := true;
end;
//------------------------------------------------------------------------------

destructor THitTestLayer32.Destroy;
begin
  inherited;
  fHitTest.Free; //do last
end;
//------------------------------------------------------------------------------

procedure THitTestLayer32.ImageChanged(Sender: TImage32);
begin
  ClearHitTesting; //todo: check this
  inherited;
end;
//------------------------------------------------------------------------------

function THitTestLayer32.GetEnabled: Boolean;
begin
  Result := fHitTest.enabled;
end;
//------------------------------------------------------------------------------

procedure THitTestLayer32.SetEnabled(value: Boolean);
begin
  if fHitTest.enabled = value then Exit;
  fHitTest.enabled := value;
  if not value then ClearHitTesting;
end;
//------------------------------------------------------------------------------

procedure THitTestLayer32.ClearHitTesting;
begin
  if not fHitTest.htImage.IsEmpty then
    fHitTest.htImage.SetSize(0,0);
end;
//------------------------------------------------------------------------------

procedure THitTestLayer32.UpdateHitTestMask;
begin
  if fHitTest.enabled then
    fHitTest.htImage.Assign(Image);
end;

//------------------------------------------------------------------------------
// TRotateLayer32 class
//------------------------------------------------------------------------------

constructor TRotLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fAutoPivot := true;
  Reset;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.SetAngle(newAngle: double);
begin
  NormalizeAngle(newAngle);
  if newAngle = fAngle then Exit;
  if not IsValid(fPivotPt) then fPivotPt := MidPoint;
  Rotate(newAngle - fAngle);
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.Scale(sx, sy: double);
begin
  if (sx > 0) then fScaleX := fScaleX * sx;
  if (sy > 0) then fScaleY := fScaleY * sy;
end;
//------------------------------------------------------------------------------

function TRotLayer32.Rotate(angleDelta: double): Boolean;
begin
  Result := (angleDelta <> 0) and not HasChildren;
  if not Result then Exit;
  fAngle := fAngle + angleDelta;
  NormalizeAngle(fAngle);
  Invalidate;
  //the rest is done in descendant classes
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.Reset;
begin
  fPivotPt := InvalidPointD;
  fAngle := 0;
  fScaleX := 1.0;
  fScaleY := 1.0;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.Offset(dx, dy: double);
begin
  inherited;
  if fAutoPivot then
  begin
    fPivotPt.X := fPivotPt.X + dx;
    fPivotPt.Y := fPivotPt.Y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TRotLayer32.GetPivotPt: TPointD;
begin
  if PointsEqual(fPivotPt, InvalidPointD) then
    Result := MidPoint else
    Result := fPivotPt;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.SetPivotPt(const pivot: TPointD);
begin
  if fAutoPivot then fAutoPivot := false;
  fPivotPt := pivot;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.SetAutoPivot(val: Boolean);
begin
  if val = fAutoPivot then Exit;
  fAutoPivot := val;
  fPivotPt := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TVectorLayer32 class
//------------------------------------------------------------------------------

constructor TVectorLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCursorId := crHandPoint;
end;
//------------------------------------------------------------------------------

function TVectorLayer32.Rotate(angleDelta: double): Boolean;
begin
  Result := inherited Rotate(angleDelta);
  if not Result then Exit;
  fPaths := RotatePath(fPaths, fPivotPt, angleDelta);
  RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Scale(sx, sy: double);
begin
  inherited;
  SetInnerBounds(RectD(fLeft, fTop, fWidth * sx, fHeight * sy));
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.SetPaths(const newPaths: TPathsD);
begin
  fPaths := CopyPaths(newPaths);
  fPivotPt := InvalidPointD;
  if not Assigned(fPaths) then inherited SetInnerBounds(NullRectD)
  else if fIsDrawing then Raise Exception.Create(rsVectorLayer32Error)
  else RepositionAndDraw;
end;
//------------------------------------------------------------------------------

function  TVectorLayer32.GetRelativePaths: TPathsD;
begin
  Result := TranslatePath(fPaths, -fLeft + fOuterMargin, -fTop  + fOuterMargin);
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.SetInnerBounds(const newBounds: TRectD);
var
  w,h: double;
  rec: TRect;
  mat: TMatrixD;
begin
  w := newBounds.Width;
  h := newBounds.Height;

  //make sure the bounds are large enough to scale safely
  if Assigned(fPaths) and (w > 1) and (h > 1) then
  begin
    //apply scaling and translation
    mat := IdentityMatrix;
    rec := Img32.Vector.GetBounds(fPaths);
    MatrixTranslate(mat, -rec.Left, -rec.Top);
    MatrixScale(mat, w / Width, h / Height);
    MatrixTranslate(mat, newBounds.Left, newBounds.Top);
    MatrixApply(mat, fPaths);
    if fAutoPivot then fPivotPt := InvalidPointD;
    RepositionAndDraw;
  end else
  begin
    inherited;
    RepositionAndDraw;
  end;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Offset(dx,dy: double);
begin
  inherited;
  fPaths := TranslatePath(fPaths, dx, dy);
  if fAutoPivot and not PointsEqual(fPivotPt, InvalidPointD) then
    fPivotPt := TranslatePoint(fPivotPt, dx, dy);
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.AppendPoint(const pt: TPointD);
var
  highPaths, lenPts: integer;
begin
  highPaths := High(fPaths);
  if highPaths < 0 then
  begin
    SetLength(fPaths, 1);
    SetLength(fPaths[0], 1);
    fPaths[0][0] := pt;
  end else
  begin
    lenPts := Length(fPaths[highPaths]);
    SetLength(fPaths[highPaths], lenPts +1);
    fPaths[highPaths][lenPts] := pt;
  end;
  RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.AppendPath(const path: TPathD);
begin
  Img32.Vector.AppendPath(fPaths, path);
  RepositionAndDraw;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.RepositionAndDraw;
var
  rec: TRectD;
begin
  if Assigned(fPaths) then
  begin
    rec := Img32.Vector.GetBoundsD(fPaths);
    inherited SetInnerBounds(rec);
  end;
  Image.BlockNotify;
  try
    Draw;
  finally
    Image.UnblockNotify;
  end;
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.Draw;
begin
  //to draw the layer, either override this event
  //in a descendant class or assign the OnDraw property
  if not Assigned(fOnDraw) then Exit;
  fIsDrawing := true;
  fOnDraw(self);
  fIsDrawing := false;
end;
//------------------------------------------------------------------------------

procedure  TVectorLayer32.UpdateHitTestMask(const vectorRegions: TPathsD);
begin
  UpdateHitTestMaskUsingPath(self, vectorRegions);
end;
//------------------------------------------------------------------------------

procedure TVectorLayer32.UpdateHitTestMaskFromImage;
begin
  fHitTest.enabled := true;
  fHitTest.htImage.Assign(Image);
end;

//------------------------------------------------------------------------------
// TRasterLayer32 class
//------------------------------------------------------------------------------

constructor TRasterLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited Create(parent);
  fMasterImg := TLayerNotifyImage32.Create(self);
  if Assigned(fLayeredImage) then
    fMasterImg.Resampler := fLayeredImage.Resampler;
  fCursorId := crHandPoint;
  fAutoHitTest := true;
  fOuterMargin := 0;
  fAutoCrop := true;
end;
//------------------------------------------------------------------------------

destructor TRasterLayer32.Destroy;
begin
  fMasterImg.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.UpdateHitTestMaskOpaque;
begin
  UpdateHitTestMask;
end;
//------------------------------------------------------------------------------

function CompareAlpha(master, current: TColor32; data: integer): Boolean;
var
  mARGB: TARGB absolute master;
  cARGB: TARGB absolute current;
begin
  Result := mARGB.A - cARGB.A < data;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.UpdateHitTestMaskTransparent(alphaValue: Byte);
begin
  if alphaValue = 127 then
    UpdateHitTestMask else
    UpdateHitTestMaskTranspar(CompareAlpha, clWhite32, alphaValue);
end;
//------------------------------------------------------------------------------

procedure  TRasterLayer32.UpdateHitTestMaskTranspar(
  compareFunc: TCompareFunction;
  referenceColor: TColor32; tolerance: integer);
begin
  UpdateHitTestMaskUsingImage(fHitTest, Self, Image,
  compareFunc, referenceColor, tolerance);
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.DoAutoHitTest;
begin
  if fAutoHitTest then
    fHitTest.htImage.Assign(Image) else
    HitTest.htImage.SetSize(0,0);
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.ImageChanged(Sender: TImage32);
begin
  if (Sender = MasterImage) then
  begin
    Reset;
    if MasterImage.IsEmpty then Exit;
    if fAutoCrop then
    begin
      MasterImage.BlockNotify;
      MasterImage.CropTransparentPixels;
      MasterImage.UnblockNotify;
    end;
    //reset whenever MasterImage changes
    if Image.IsEmpty and (TLayerNotifyImage32(Image).UpdateCount = 0) then
      Image.Assign(MasterImage);
    fCropMargins := NullPoint;
    Invalidate;
  end else
  begin
    if MasterImage.IsEmpty and not Image.IsEmpty then
    begin
      Image.BlockNotify;
      try
        if fAutoCrop then
          fCropMargins := SymmetricCropTransparent(Image);
        PositionAt(fLeft + fCropMargins.X, fTop + fCropMargins.Y);
        MasterImage.Assign(Image);
      finally
        Image.UnblockNotify;
      end;
    end;
    inherited;
    DoAutoHitTest;
  end;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.SetInnerBounds(const newBounds: TRectD);
var
  x,y, rx,ry: double;
  mat: TMatrixD;
  sinA, cosA, tanA: double;
begin
  if not MasterImage.IsEmpty and
    //the image must be large enough to scale safely
    (newBounds.Width > 1) and (newBounds.Height > 1) then
  begin
    Image.BeginUpdate;
    try
      // determine the amount of scaling in the **un-rotated**
      // image that will fit 'newBounds' once the image is rotated

      // given:
      //   the pivot point is unimportant (final position already defined)
      //   rotated x = sin(angle)*y + cos(angle)*x
      //   rotated y = cos(angle)*y + sin(angle)*x
      // let:
      //   X, Y     : unrotated image width & height
      //   rX, rY   : rotated image width & height

      // rX = sinA * Y + cosA * X
      // X = rX / cosA - sinA/cosA * Y
      // X = rX / cosA - tanA * Y
      // rY = cosA*Y + sinA*X
      // Y =  rY/cosA - tanA * X
      // X = rX / cosA - tanA * (rY / cosA - tanA * X)
      // X = rX / cosA - tanA * rY/cosA + tanA*tanA * X
      // X - tanA*tanA * X = rX / cosA - tanA * rY/cosA
      // X * (1 - tanA*tanA) = rX / cosA - tanA * rY/cosA
      // X = (rX / cosA - tanA * rY/cosA) / (1 - tanA*tanA)
      // Y := (rY - sinA * x) /cosA;

      sinA := Abs(Sin(fAngle));
      cosA := Abs(Cos(fAngle));
      if sinA = 0.0 then            // no rotation (or 180 deg. rotation)
      begin
        fScaleX := newBounds.Width / MasterImage.Width;
        fScaleY := newBounds.Height / MasterImage.Height;
      end
      else if cosA = 0.0 then       // rotated 90 or 270 degrees
      begin
        fScaleX := newBounds.Height / MasterImage.Width;
        fScaleY := newBounds.Width / MasterImage.Height;
      end else
      begin
        tanA := sinA / cosA;
        // adjust for rotational cropping
        rx := newBounds.Width + fCropMargins.X * 2;
        ry := newBounds.Height + fCropMargins.Y * 2;
        x := (rx / cosA - tanA * ry / cosA) / (1 - tanA * tanA);
        y := (ry - sinA * x) / cosA;

        if (x <= 0) or (y <= 0) then
        begin
          Image.SetSize(Round(newBounds.Width), Round(newBounds.Height));
          PositionAt(newBounds.Left, newBounds.Top);
          Exit;
        end;

        fScaleX := x / MasterImage.Width;
        fScaleY := y / MasterImage.Height;
      end;

      Image.AssignSettings(MasterImage);
      Image.Resampler := rWeightedBilinear;
      mat := IdentityMatrix;
      MatrixScale(mat, fScaleX, fScaleY);
      MatrixRotate(mat, fAngle);
      AffineTransformImage(MasterImage, Image, mat, true);
      if fAutoCrop then
        fCropMargins := SymmetricCropTransparent(Image);
      PositionAt(newBounds.Left, newBounds.Top);
    finally
      Image.EndUpdate;
    end;
    DoAutoHitTest;
  end else
    inherited;
end;
//------------------------------------------------------------------------------

function TRasterLayer32.Rotate(angleDelta: double): Boolean;
var
  mat : TMatrixD;
  mp  : TPointD;
begin
  Result := (angleDelta <> 0) and
    not MasterImage.IsEmpty and
    inherited Rotate(angleDelta);

  if not Result then Exit;

  mp := MidPoint;
  mat := IdentityMatrix;
  MatrixScale(mat, fScaleX, fScaleY);
  MatrixRotate(mat, fAngle);
  RotatePoint(mp, PivotPt, angleDelta);

  Image.BlockNotify;
  try
    Image.AssignSettings(MasterImage);
    Image.Resampler := rWeightedBilinear;
    AffineTransformImage(MasterImage, Image, mat, true);
  finally
    Image.UnblockNotify;
  end;

  // cropping the image significantly improves performance
  if fAutoCrop then
    fCropMargins := SymmetricCropTransparent(Image);

  fWidth := Image.Width;
  fHeight := Image.Height;
  PositionCenteredAt(mp);
  DoAutoHitTest;
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.Scale(sx, sy: double);
begin
  inherited;
  SetInnerBounds(RectD(fLeft, fTop,
    MasterImage.Width * fSCaleX, MasterImage.Height * fSCaleY));
end;

//------------------------------------------------------------------------------
// TSizingGroupLayer32 class
//------------------------------------------------------------------------------

constructor TSizingGroupLayer32.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  SetDesignerLayer(true);
{$IFDEF USE_FILE_STORAGE}
  IgnoreOnWrite := true;
{$ENDIF}
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// TRotatingGroupLayer32 class
//------------------------------------------------------------------------------

constructor TRotatingGroupLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  SetDesignerLayer(true);
{$IFDEF USE_FILE_STORAGE}
  IgnoreOnWrite := true;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TRotatingGroupLayer32.Init(const rec: TRect;
  buttonSize: integer; centerButtonColor, movingButtonColor: TColor32;
  startingAngle: double; startingZeroOffset: double;
  buttonLayerClass: TButtonDesignerLayer32Class);
var
  w,h: integer;
  q, dist: double;
  pt: TPointD;
  pivot: TPointD;
  rec2: TRectD;
begin
  //startingZeroOffset: default = 0 (ie 3 o'clock)
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
    startingZeroOffset := -startingZeroOffset;
{$ENDIF}
  fZeroOffset := startingZeroOffset;

  if buttonSize <= 0 then buttonSize := DefaultButtonSize;
  pivot := Img32.Vector.MidPoint(RectD(rec));
  RectWidthHeight(rec, w, h);

  dist := Average(w, h) / 2;
  rec2 := RectD(
    pivot.X -dist, pivot.Y -dist,
    pivot.X +dist,pivot.Y +dist);

  with AddChild(TLayer32) do //Layer 0 - design layer
  begin
    SetInnerBounds(rec2);
    q := DPIAware(2);
    pt := TranslatePoint(pivot, -fLeft, -fTop);
    DrawDashedLine(Image, Circle(pt, dist - q),
      dashes, nil, q, clRed32, esClosed);
  end;

  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  with TButtonDesignerLayer32(AddChild( //Layer 1 - pivot button
    buttonLayerClass, rsButton)) do
  begin
    SetButtonAttributes(bsRound, buttonSize, centerButtonColor);
    PositionCenteredAt(Img32.Vector.MidPoint(RectD(rec)));
    CursorId := crSizeAll;
  end;

  with TButtonDesignerLayer32(AddChild(  //layer 2 - angle (rotating) button
    buttonLayerClass, rsButton)) do
  begin
    SetButtonAttributes(bsRound, buttonSize, movingButtonColor);
    pt := GetPointAtAngleAndDist(pivot,
      startingAngle + startingZeroOffset, dist);
    PositionCenteredAt(pt);
    CursorId := crSizeAll;
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

function TRotatingGroupLayer32.GetDesignLayer: TLayer32;
begin
  Result := Child[0];
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
// TButtonGroupLayer32 class
//------------------------------------------------------------------------------

constructor TButtonGroupLayer32.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  SetDesignerLayer(true);
{$IFDEF USE_FILE_STORAGE}
  IgnoreOnWrite := true;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function TButtonGroupLayer32.AddButton(const pt: TPointD): TButtonDesignerLayer32;
begin
  result := InsertButton(pt, MaxInt);
end;
//------------------------------------------------------------------------------

function TButtonGroupLayer32.InsertButton(const pt: TPointD;
  btnIdx: integer): TButtonDesignerLayer32;
begin
  result := TButtonDesignerLayer32(InsertChild(btnIdx, fBtnLayerClass));
  with result do
  begin
    SetButtonAttributes(fBtnShape, FBtnSize, fBtnColor);
    PositionCenteredAt(pt);
    CursorId := crHandPoint;
    fBtnIdx := Self.fBtnCount;
  end;
  inc(fBtnCount);
end;

//------------------------------------------------------------------------------
// TButtonDesignerLayer32 class
//------------------------------------------------------------------------------

constructor TButtonDesignerLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  SetDesignerLayer(true);
  fEnabled := true;
  fHitTest.enabled := fEnabled;
  SetButtonAttributes(bsRound, DefaultButtonSize, clGreen32);
{$IFDEF USE_FILE_STORAGE}
  IgnoreOnWrite := true;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.SetEnabled(value: Boolean);
begin
  if value = fEnabled then Exit;
  fEnabled := value;
  fHitTest.enabled := fEnabled;
  Draw;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.SetButtonAttributes(const shape: TButtonShape;
  size: integer; color: TColor32);
begin
  fSize := size;
  fShape := shape;
  fColor := color;
  OuterMargin := size / 4; //add room for button shadow
  SetSize(size, size);
  Draw;
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.UpdateHitTestMask(const vectorRegions: TPathsD);
begin
  UpdateHitTestMaskUsingPath(self, vectorRegions);
end;
//------------------------------------------------------------------------------

procedure TButtonDesignerLayer32.Draw;
var
  c: TColor32;
begin
  if fEnabled then c := fColor else c := clWhite32;
  fOutline := Img32.Extra.DrawButton(Image,
    image.MidPoint, fSize, c, fShape, [ba3D, baShadow]);
end;

//------------------------------------------------------------------------------
// TLayeredImage32 class
//------------------------------------------------------------------------------

constructor TLayeredImage32.Create(parent: TStorage; const name: string);
begin
  inherited;
  fBackColor := clNone32;
  fResampler := DefaultResampler;
  fLastUpdateType := utUndefined;

{$IFDEF USE_FILE_STORAGE}
  if StorageState = ssLoading then Exit;
{$ENDIF}
  // automatically create a TGroupLayer32 root control
  fRoot := AddChild(TGroupLayer32) as TGroupLayer32;
end;
//------------------------------------------------------------------------------

constructor TLayeredImage32.Create(Width, Height: integer);
begin
  Create(nil, '');
  fBounds := Rect(0, 0, Width, Height);
  fRoot.SetSize(width, Height);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetSize(width, height: integer);
begin
  fBounds := Rect(0, 0, Width, Height);
  if not Assigned(fRoot) then Exit;
  fRoot.SetInnerBounds(RectD(fBounds));
  if fBackColor <> clNone32 then
    fRoot.Image.Clear(fBackColor);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMergedImage(hideDesigners: Boolean): TImage32;
var
  updateRect: TRect;
begin
  fLastUpdateType := utUndefined; //forces a full repaint
  Result := GetMergedImage(hideDesigners, updateRect);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMergedImage(hideDesigners: Boolean;
  out updateRect: TRect): TImage32;
var
  forcingRefresh: Boolean;
begin
  Result := Image;
  if IsEmptyRect(Bounds) then Exit;

  forcingRefresh := (fLastUpdateType = utUndefined) or
    (hideDesigners <> (fLastUpdateType = utHideDesigners));

  with Root do
  begin
    //get 'old bounds' that will need erasing
    PreMerge(hideDesigners);

    if forcingRefresh then
      updateRect := Self.Bounds else
      Types.IntersectRect(updateRect, Rect(fInvalidRect), Self.Bounds);

    fInvalidRect := nullRectD;
    if IsEmptyRect(updateRect) then Exit;

    Image.Clear(updateRect, fBackColor);
    Merge(hideDesigners, updateRect);

    if fOpacity < 254 then Image.ReduceOpacity(fOpacity);
    if hideDesigners then
      fLastUpdateType := utHideDesigners else
      fLastUpdateType := utShowDesigners;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Clear;
begin
  if not Assigned(fRoot) then Exit;
  fRoot.ClearChildren;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Invalidate;

  procedure InvalidateAll(layer: TLayer32);
  var
    i: integer;
  begin
    layer.Invalidate;
    for i := 0 to layer.ChildCount -1 do
      if layer.fNonLayerChilds and layer.ChildNotTLayer32(i) then Continue
      else InvalidateAll(layer[i]);
  end;

begin
  if not Assigned(fRoot) then Exit;
  fInvalidRect := RectD(fBounds);
  fLastUpdateType := utUndefined;
  InvalidateAll(fRoot);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetRepaintNeeded: Boolean;
begin
  Result := Root.fUpdateInfo.updateMethod <> umNone;
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
  if Assigned(fRoot) then
    Result := fRoot.ChildCount else
    Result := 0;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayer(index: integer): TLayer32;
begin
  if GetRootLayersCount > 0 then
    Result := fRoot[index] else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetImage: TImage32;
begin
  if Assigned(fRoot) then
    Result := fRoot.Image else
    Result := nil;
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
  if not Assigned(fRoot) then Exit;
  fRoot.Image.Clear(fBackColor);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMidPoint: TPointD;
begin
  Result := PointD(Width * 0.5, Height * 0.5);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertChild(index: integer; storeClass: TLayer32Class): TLayer32;
begin
  if ChildCount = 0 then
  begin
    Result := inherited InsertChild(index, storeClass) as TLayer32;
{$IFDEF USE_FILE_STORAGE}
    if (StorageState = ssLoading) then
    begin
      if not (Result is TGroupLayer32) then
        raise Exception.Create(rsLayeredImage32Error);
      fRoot := TGroupLayer32(Result);
      fRoot.Name := rsRoot;
      fRoot.fLayeredImage := self;
    end;
{$ENDIF}
  end else
    Result := fRoot.InsertChild(index, storeClass) as TLayer32;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddLayer(layerClass: TLayer32Class;
  parent: TLayer32; const name: string): TLayer32;
begin
  if not Assigned(layerClass) then layerClass := TLayer32;
  Result := InsertLayer(layerClass, parent, MaxInt, name);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertLayer(layerClass: TLayer32Class;
  parent: TLayer32; index: integer; const name: string): TLayer32;
begin
  if not Assigned(fRoot) then
  begin
    Result := nil;
    Exit;
  end;
  if not Assigned(parent) then parent := fRoot;
  Result := parent.InsertChild(index, layerClass, name);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.FindLayerNamed(const name: string): TLayer32;
begin
  if not Assigned(fRoot) then
    Result := nil else
    Result := Root.FindLayerNamed(name);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(layer: TLayer32);
begin
  if not Assigned(fRoot) or not assigned(layer) or
    not assigned(layer.Parent) then Exit;
  layer.Parent.DeleteChild(layer.Index);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(layerIndex: integer;
  parent: TLayer32 = nil);
begin
  if not Assigned(fRoot) then Exit;
  if not assigned(parent) then parent := Root;
  parent.CheckChildIndex(layerIndex);
  parent.DeleteChild(layerIndex);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPoint;
  ignoreDesigners: Boolean): TLayer32;
begin
  if not Assigned(fRoot) then result := nil
  else result := Root.GetLayerAt(PointD(pt), ignoreDesigners);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPointD;
  ignoreDesigners: Boolean): TLayer32;
begin
  if not Assigned(fRoot) then result := nil
  else result := Root.GetLayerAt(pt, ignoreDesigners);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.DetachRoot: TGroupLayer32;
begin
  Result := fRoot;
  if not Assigned(fRoot) then Exit;
  fRoot := Nil;
  Result.UpdateLayeredImage(nil);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AttachRoot(newRoot: TGroupLayer32): Boolean;
begin
  Result := not Assigned(fRoot) and Assigned(newRoot);
  if not Result then Exit;
  fRoot := newRoot;
  fRoot.UpdateLayeredImage(self);
  fRoot.SetInnerBounds(RectD(0,0,Width, Height));
  fRoot.Visible := true;
  Invalidate;
end;

//------------------------------------------------------------------------------
// Miscellaneous button functions
//------------------------------------------------------------------------------

function GetRectEdgeMidPoints(const rec: TRectD): TPathD;
var
  mp: TPointD;
begin
  mp := MidPoint(rec);
  NewPointDArray(Result, 4, True);
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
    targetLayer.RootOwner.AddLayer(TSizingGroupLayer32, nil,
    rsSizingButtonGroup));
  Result.SizingStyle := sizingStyle;

  //get targetlayer's absolute bounds (disregarding nesting)
  with targetLayer do
    rec := targetLayer.MakeAbsolute(InnerRect);
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
    not (movedButton.Parent is TSizingGroupLayer32) then Exit;

  group := TSizingGroupLayer32(movedButton.Parent);
  with group do
  begin
    NewPointDArray(path, ChildCount, True);
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
            Child[i * 2].PositionCenteredAt(corners[i]);
            Child[i * 2 + 1].PositionCenteredAt(edges[i]);
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
  buttonLayerClass: TButtonDesignerLayer32Class): TRotatingGroupLayer32;
var
  rec: TRectD;
  radius: double;
begin
  if not assigned(targetLayer) or
    not (targetLayer is TRotLayer32) then
      raise Exception.Create(rsCreateButtonGroupError);

  Result := TRotatingGroupLayer32(targetLayer.RootOwner.AddLayer(
    TRotatingGroupLayer32, nil, rsRotatingButtonGroup));

  radius := Min(targetLayer.Width, targetLayer.Height) / 2;
  if PointsNearEqual(pivot, targetLayer.MidPoint, 1) then
    rec := targetLayer.InnerBounds
  else
    rec := RectD(pivot.X -radius, pivot.Y -radius,
      pivot.X +radius,pivot.Y +radius);

  Result.Init(Rect(rec), buttonSize,
    pivotButtonColor, angleButtonColor, initialAngle,
    angleOffset, buttonLayerClass);


  if TRotLayer32(targetLayer).AutoPivot then
    Result.PivotButton.HitTestEnabled := false;
end;
//------------------------------------------------------------------------------

function CreateRotatingButtonGroup(targetLayer: TLayer32;
  buttonSize: integer;
  pivotButtonColor: TColor32;
  angleButtonColor: TColor32;
  initialAngle: double; angleOffset: double;
  buttonLayerClass: TButtonDesignerLayer32Class): TRotatingGroupLayer32;
var
  pivot: TPointD;
begin
  pivot := MidPoint(targetLayer.InnerBounds);
  Result := CreateRotatingButtonGroup(targetLayer, pivot, buttonSize,
    pivotButtonColor, angleButtonColor, initialAngle, angleOffset,
    buttonLayerClass);
end;
//------------------------------------------------------------------------------

function UpdateRotatingButtonGroup(rotateButton: TLayer32): double;
var
  rec: TRectD;
  mp, pt2: TPointD;
  radius: double;
  designer: TLayer32;
  rotateGroup: TRotatingGroupLayer32;
begin

  rotateGroup := nil;
  if assigned(rotateButton) then
  begin
    if rotateButton is TRotatingGroupLayer32 then
      rotateGroup := TRotatingGroupLayer32(rotateButton)
    else if (rotateButton.Parent is TRotatingGroupLayer32) then
      rotateGroup := TRotatingGroupLayer32(rotateButton.Parent);
  end;
  if not assigned(rotateGroup) then
        raise Exception.Create(rsUpdateRotateGroupError);

  with rotateGroup do
  begin
    mp := PivotButton.MidPoint;
    pt2 := AngleButton.MidPoint;
    radius := Distance(mp, pt2);
    rec := RectD(mp.X -radius, mp.Y -radius, mp.X +radius,mp.Y +radius);
    designer := DesignLayer;
    designer.SetInnerBounds(rec);
    pt2 := TranslatePoint(mp, -rec.Left, -rec.Top);
    DrawDashedLine(designer.Image,
      Circle(pt2, radius -dpiAwareOne),
      dashes, nil, DPIAware(2), clRed32, esClosed);
    Result := Angle;
  end;
end;
//------------------------------------------------------------------------------

function CreateButtonGroup(parent: TLayer32; const buttonPts: TPathD; buttonShape: TButtonShape;
  buttonSize: integer; buttonColor: TColor32;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonGroupLayer32;
var
  i: integer;
begin
  if not assigned(parent) then
    raise Exception.Create(rsCreateButtonGroupError);

  Result := TButtonGroupLayer32(parent.AddChild(TButtonGroupLayer32));
  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  Result.fBtnSize := buttonSize;
  Result.fBtnShape := buttonShape;
  Result.fBtnColor := buttonColor;
  Result.fBtnLayerClass := buttonLayerClass;

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
  dashes[0] := dpiAware1 * 2; dashes[1] := dpiAware1 * 4;
end;

initialization
  InitDashes;
  DefaultButtonSize := dpiAware1 * 10;

{$IFDEF USE_FILE_STORAGE}
  RegisterStorageClass(TLayeredImage32);
  RegisterStorageClass(TLayer32);
  RegisterStorageClass(TGroupLayer32);
{$ENDIF}

end.
