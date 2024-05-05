unit Img32.Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.4                                                             *
* Date      :  16 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  Layered images support                                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
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
    fOuterMargin    : double;
    fImage          : TImage32;
    fMergeImage     : TImage32; //contains a merge of child images
    fClipImage      : TImage32; //used to restrict child drawing inside a layer
    fVisible        : Boolean;
    fOpacity        : Byte;
    fCursorId       : integer;
    fUserData       : TObject;
    fBlendFunc      : TBlendFunction; //defaults to BlendToAlpha
    fLayeredImage   : TLayeredImage32;
    fClipPath       : TPathsD;  //used in conjunction with fClipImage
{$IFNDEF NO_STORAGE}
    fStreamingRec   : TRectWH;
{$ENDIF}
    fDesignerLayer  : Boolean;
    function  GetMidPoint: TPointD;
    procedure SetVisible(value: Boolean);
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
    procedure SetOuterMargin(value: double);
    procedure CreateInternal(parent: TStorage = nil; const name: string = '');
  protected
    UpdateInfo : TUpdateInfo;
    procedure SetDesignerLayer(value: Boolean);
    function  GetUpdateNeeded: Boolean;
    procedure DoBeforeMerge; virtual;
    procedure PreMerge(hideDesigners: Boolean); virtual;
    procedure Merge(hideDesigners: Boolean; updateRect: TRect);
    function  GetLayerAt(const pt: TPointD; ignoreDesigners: Boolean): TLayer32;
    function  RemoveChildFromList(index: integer): TStorage; override;
    function  GetInnerRectD: TRectD;
    function  GetInnerBounds: TRectD;
    function  GetOuterBounds: TRectD;
{$IFNDEF NO_STORAGE}
    procedure BeginRead; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
    procedure EndRead; override;
{$ENDIF}
    procedure SetOpacity(value: Byte); virtual;
    procedure ImageChanged(Sender: TImage32); virtual;
    procedure UpdateLayeredImage(newLayeredImage: TLayeredImage32);
    property  UpdateNeeded : Boolean read GetUpdateNeeded;
  public
    constructor Create(parent: TStorage = nil; const name: string = ''); overload; override;
    constructor Create(parent: TLayer32; const name: string = ''); reintroduce; overload; virtual;
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
    //procedure  Invalidate(const rec: TRectD); overload; virtual;

    function   AddChild(layerClass: TLayer32Class;
      const name: string = ''): TLayer32; reintroduce; virtual;
    function   InsertChild(layerClass: TLayer32Class;
      index: integer; const name: string = ''): TLayer32; reintroduce; overload; virtual;
    function   InsertChild(index: integer; storeClass: TStorageClass): TStorage;  overload; override;
    procedure  ClearChildren; override;

    property   Child[index: integer]: TLayer32 read GetChild; default;
    //ClipPath: defines a region that's inside the layer's rectangular region.
    //Portions of child layers residing outside this region  will be clipped.
    property   ClipPath: TPathsD read fClipPath write SetClipPath;
    procedure  Offset(dx, dy: double); overload; virtual;
    property   IsDesignerLayer: Boolean read fDesignerLayer;
    property   InnerBounds: TRectD read GetInnerBounds;
    property   InnerRect: TRectD read GetInnerRectD;
    property   OuterBounds: TRectD read GetOuterBounds;
    property   CursorId: integer read fCursorId write fCursorId;
    property   Height: double read fHeight write SetHeight;
    property   Image: TImage32 read fImage;
    property   Left: double read fLeft;
    property   MidPoint: TPointD read GetMidPoint;
    property   Opacity: Byte read fOpacity write SetOpacity;
    property   OuterMargin: double read fOuterMargin write SetOuterMargin;
    property   Parent: TLayer32 read GetLayer32Parent write SetLayer32Parent;
    property   Root: TGroupLayer32 read GetRoot;
    property   RootOwner: TLayeredImage32 read fLayeredImage;
    property   Top: double read fTop;
    property   Visible: Boolean read fVisible write SetVisible;
    property   Width: double read fWidth write SetWidth;
    property   UserData: TObject read fUserData write fUserData;
    property   BlendFunc: TBlendFunction read fBlendFunc write SetBlendFunc;
    property   PrevLayerInGroup: TLayer32 read GetPrevLayerInGroup;
    property   NextLayerInGroup: TLayer32 read GetNextLayerInGroup;
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

  //TRotLayer32: rotation methods added
  //(abstract base layer for TVectorLayer32 and TRasterLayer32)
  TRotLayer32 = class(THitTestLayer32)
  private
    fAngle      : double;
    fPivotPt    : TPointD;
    fAutoPivot  : Boolean;
    function  GetPivotPt: TPointD;
    procedure SetAutoPivot(val: Boolean);
    procedure SetAngle(newAngle: double);
  protected
    procedure SetPivotPt(const pivot: TPointD); virtual;
{$IFNDEF NO_STORAGE}
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
{$ENDIF}
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function    Rotate(angleDelta: double): Boolean; virtual;
    procedure ResetAngle;
    procedure Offset(dx, dy: double); override;
    property  Angle: double read fAngle write SetAngle;
    property  PivotPt: TPointD read GetPivotPt write SetPivotPt;
    property  AutoPivot: Boolean read fAutoPivot write SetAutoPivot;
  end;

  //TVectorLayer32: either repositions when Paths change,
  //or transforms Paths when bounds change
  TVectorLayer32 = class(TRotLayer32)
  private
    fPaths    : TPathsD;
    fOnDraw   : TNotifyEvent;
    procedure RepositionAndDraw;
  protected
    procedure SetPaths(const newPaths: TPathsD); virtual;
    procedure Draw; virtual;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Offset(dx,dy: double); override;
    function  Rotate(angleDelta: double): Boolean; override;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD); virtual;
    procedure UpdateHitTestMaskFromImage;
    property  Paths: TPathsD read fPaths write SetPaths;
    property  OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
  end;

  TRasterLayer32 = class(TRotLayer32) //display layer for raster images
  private
    fMasterImg    : TImage32;
    //fMatrix: allows combining any number of scaling & rotating ops.
    fMatrix       : TMatrixD;
    fRotating     : Boolean;
    fPreScaleSize : TSize;
    fAutoHitTest  : Boolean;
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

    property  AutoSetHitTestMask: Boolean read fAutoHitTest write fAutoHitTest;
    property  MasterImage: TImage32 read fMasterImg;
  end;

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
{$IFNDEF NO_STORAGE}
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
{$ENDIF}
    property  InvalidRect: TRectD read fInvalidRect;
  public
    constructor Create(parent: TStorage = nil; const name: string = ''); overload; override;
    constructor Create(Width, Height: integer); reintroduce; overload; virtual;
    procedure SetSize(width, height: integer);
    procedure Clear;
    procedure Invalidate;
    function  InsertChild(index: integer; storeClass: TStorageClass): TStorage; override;
    function  AddLayer(layerClass: TLayer32Class = nil;
      parent: TLayer32 = nil; const name: string = ''): TLayer32;
    function  InsertLayer(layerClass: TLayer32Class; parent: TLayer32;
      index: integer; const name: string = ''): TLayer32;
    procedure DeleteLayer(layer: TLayer32); overload;
    procedure DeleteLayer(layerIndex: integer;
      parent: TLayer32 = nil); overload;
    function  FindLayerNamed(const name: string): TLayer32;
    function   GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean = false): TLayer32; overload;
    function  GetLayerAt(const pt: TPointD; ignoreDesigners: Boolean = false): TLayer32; overload;
    function  GetMergedImage(hideDesigners: Boolean = false): TImage32; overload;
    function  GetMergedImage(hideDesigners: Boolean;
      out updateRect: TRect): TImage32; overload;
    function  DetachRoot: TGroupLayer32;
    function  AttachRoot(newRoot: TGroupLayer32): Boolean;

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
    property RepaintNeeded : Boolean read GetRepaintNeeded;
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
  rsLayer32Error           = 'TLayer32 - children must also be TLayer32';

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
  fDesignerLayer := true; //must do this first
  if not Assigned(parent) then
    CreateInternal(nil, name)
  else if parent.InheritsFrom(TLayer32) then
    //this constructor is commonly overrided in descendant layer classes
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
    if not UpdateInfo.priorPosition.IsEmpty then
    begin
      rec := Parent.MakeAbsolute(UpdateInfo.priorPosition);
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
  fDesignerLayer := value;
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
  if inherited parent = parent then Exit;
  inherited SetParent(parent);
  if Visible then Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.GetUpdateNeeded: Boolean;
begin
     Result := (UpdateInfo.updateMethod <> umNone);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Invalidate;
var
  layer : TLayer32;
begin
  if (UpdateInfo.updateMethod = umSelf) then Exit;
  UpdateInfo.updateMethod := umSelf;

  layer := Parent;
  while Assigned(layer) do
  begin
    if layer.UpdateInfo.updateMethod <> umNone then Break;
    layer.UpdateInfo.updateMethod := umChild;
    layer := layer.Parent;
  end;
end;
//------------------------------------------------------------------------------

//procedure TLayer32.Invalidate(const rec: TRectD);
//var
//  layer : TLayer32;
//begin
//  if (UpdateInfo.updateMethod = umAll) or
//    not Assigned(fLayeredImage) or (self = Root) then Exit;
//
//  with UpdateInfo do
//  begin
//    updateMethod := umRegion;
//    updateRegion := UnionRect(updateRegion, rec);
//  end;
//
//  layer := Parent;
//  while Assigned(layer) do
//  begin
//    if layer.UpdateInfo.childUpdating then Break;
//    layer.UpdateInfo.childUpdating := true;
//    layer := layer.Parent;
//  end;
//end;
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
  if (StorageState = ssLoading) then Exit;
  fWidth := Image.Width -fOuterMargin *2;
  fHeight := Image.Height -fOuterMargin *2;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetSize(width, height: double);
var
  w,h: integer;
begin
  if StorageState = ssDestroying then Exit;
  fWidth := width; fHeight := height;
  w := Ceil(fWidth + fOuterMargin *2);
  h := Ceil(fHeight + fOuterMargin *2);
  Image.SetSize(w, h);
end;
//------------------------------------------------------------------------------

procedure  TLayer32.SetInnerBounds(const newBounds: TRectD);
begin
  fWidth := newBounds.Width;
  fHeight := newBounds.Height;
  Image.BlockNotify;
  Image.SetSize(Ceil(fWidth + fOuterMargin *2),
    Ceil(fHeight + fOuterMargin *2));
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

procedure TLayer32.SetHeight(value: double);
begin
  SetSize(fWidth, value);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetWidth(value: double);
begin
  SetSize(value, fHeight);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetOuterMargin(value: double);
begin
  if fOuterMargin = value then Exit;
  fOuterMargin := value;
  Image.BlockNotify;
  Image.SetSize(Ceil(fWidth + fOuterMargin *2),
    Ceil(fHeight + fOuterMargin *2));
  Image.UnBlockNotify;
end;
//------------------------------------------------------------------------------

{$IFNDEF NO_STORAGE}
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

function TLayer32.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit;
  if propName = 'Left' then
    fStreamingRec.Left := GetDoubleProp(propVal, Result)
  else if propName = 'Top' then
    fStreamingRec.Top := GetDoubleProp(propVal, Result)
  else if propName = 'Width' then
    fStreamingRec.Width := GetDoubleProp(propVal, Result)
  else if propName = 'Height' then
    fStreamingRec.Height := GetDoubleProp(propVal, Result)
  else if propName = 'OuterMargin' then
    OuterMargin := GetIntProp(propVal, Result)
  else if propName = 'Visible' then
    fVisible := GetBoolProp(propVal, Result)
  else if propName = 'Opacity' then
    fOpacity := GetIntProp(propVal, Result)
  else if propName = 'CursorId' then
    fCursorId := GetIntProp(propVal, Result);
end;
//------------------------------------------------------------------------------

procedure TLayer32.EndRead;
begin
  if fStreamingRec.IsValid then
      SetInnerBounds(fStreamingRec.RectD);
end;
//------------------------------------------------------------------------------

procedure TLayer32.WriteProperties;
begin
  inherited;
  WriteDoubleProp('Left', Left);
  WriteDoubleProp('Top', Top);
  WriteDoubleProp('Width', Width);
  WriteDoubleProp('Height', Height);
  if CursorId <> 0 then WriteIntProp('CursorId', CursorId);
  if Opacity < 255 then WriteIntProp('Opacity', Opacity);
  if OuterMargin > 0 then WriteDoubleProp('OuterMargin', OuterMargin);
  if not Visible then WriteBoolProp('Visible', false);
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
  Result := assigned(Parent) and
    (index < Parent.ChildCount -1);
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
  Invalidate;
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
  Result := InsertChild(layerClass, MaxInt, name);
end;
//------------------------------------------------------------------------------

function TLayer32.InsertChild(layerClass: TLayer32Class;
  index: integer; const name: string = ''): TLayer32;
begin
  Result := inherited InsertChild(index, layerClass) as TLayer32;
  if name = '' then
    Result.Name := Result.ClassName else
    Result.Name := name;
end;
//------------------------------------------------------------------------------

function TLayer32.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  if not storeClass.InheritsFrom(TLayer32) then
    raise Exception.Create(rsLayer32Error);
  Result := InsertChild(TLayer32Class(storeClass), index, '');
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

procedure TLayer32.PreMerge(hideDesigners: Boolean);
var
  i: integer;
  childLayer: TLayer32;
  rec: TRectD;
begin
  //this method is recursive and updates each group's fInvalidRect
  for i := 0 to ChildCount -1 do
  begin
    childLayer := Child[i];
    with childLayer do
    begin
      if not Visible or
         (hideDesigners and IsDesignerLayer) or
         (UpdateInfo.updateMethod = umNone) then
           Continue;

      if UpdateInfo.updateMethod = umSelf then
      begin
        rec := Parent.MakeAbsolute(UpdateInfo.priorPosition);
        with fLayeredImage do
          fInvalidRect := UnionRect(fInvalidRect, rec);
        UpdateInfo.priorPosition := OuterBounds;
        rec := Parent.MakeAbsolute(UpdateInfo.priorPosition);
        with fLayeredImage do
          fInvalidRect := UnionRect(fInvalidRect, rec);
      end;

      // premerge children (recursion)
      DoBeforeMerge;
      PreMerge(hideDesigners);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.Merge(hideDesigners: Boolean; updateRect: TRect);
var
  i: integer;
  childLayer: TLayer32;
  img, childImg, childImg2: TImage32;
  rec, rec2, dstRect, srcRect: TRect;
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

  //merge redraw all children
  for i := 0 to ChildCount -1 do
  begin
    childLayer := Child[i];
    with childLayer do
    begin
      if not visible or (hideDesigners and IsDesignerLayer) then
         Continue;

      //recursive merge
      if (UpdateInfo.updateMethod <> umNone) then
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

      if IsEmptyRect(dstRect) then Continue;

      //get srcRect (offset to childLayer coords)
      //and further adjust dstRect to accommodate OuterMargin
      srcRect.Left := Floor(dstRect.Left - Left + fOuterMargin);
      srcRect.Top := Floor(dstRect.Top - Top + fOuterMargin);
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
    img.BlockNotify;
    try
      if (childLayer.Opacity < 254) or Assigned(fClipPath) then
      begin
        childImg2 := TImage32.Create(childImg);
        childImg2.ReduceOpacity(childLayer.Opacity);
        if Assigned(fClipImage) then
        begin
          //use the clipping mask to 'trim' childLayer's image
          rec := fClipImage.Bounds;
          rec2 := rec;
          TranslateRect(rec2,
            Floor(childLayer.fOuterMargin -childLayer.Left -fOuterMargin),
            Floor(childLayer.fOuterMargin -childLayer.Top -fOuterMargin));
          childImg2.CopyBlend(fClipImage, rec, rec2, BlendMask);
        end;
      end else
        childImg2 := childImg;

      if Assigned(childLayer.BlendFunc) then
        img.CopyBlend(childImg2, srcRect, dstRect, childLayer.BlendFunc) else
        img.Copy(childImg2, srcRect, dstRect);
    finally
      if childImg2 <> childImg then
        childImg2.Free;
      img.UnblockNotify;
    end;
  end;

  with UpdateInfo do
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
    pt2 := TranslatePoint(pt, -Left, -Top);

  //if 'pt2' is outside the clip mask then don't continue
  if Assigned(fClipImage) then
    if TARGB(fClipImage.Pixel[
      Round(pt2.X+ fOuterMargin),
      Round(pt2.Y+ fOuterMargin)]).A < 128 then Exit;

  for i := ChildCount -1 downto 0 do
  begin
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
            Round(pt2.X -left + fOuterMargin),
            Round(pt2.Y -top + fOuterMargin)]).A >= 128 then
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
    if Child[i] is TLayer32 then
    begin
      Result := Child[i].FindLayerNamed(name);
      if assigned(Result) then Break;
    end else if SameText(self.Name, name) then
    begin
      Result := Child[i];
      Break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.UpdateLayeredImage(newLayeredImage: TLayeredImage32);
var
  i: integer;
begin
  fLayeredImage := newLayeredImage;
  for i := 0 to ChildCount -1 do
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
  if (self <> Root) and (UpdateInfo.updateMethod <> umNone) then
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
  fPivotPt := InvalidPointD;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.SetAngle(newAngle: double);
begin
  NormalizeAngle(newAngle);
  if newAngle = fAngle then Exit;
  if PointsEqual(fPivotPt, InvalidPointD) then
    fPivotPt := MidPoint;
  Rotate(newAngle - fAngle);
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

procedure TRotLayer32.ResetAngle;
begin
  fAngle := 0;
  fPivotPt := InvalidPointD;
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

{$IFNDEF NO_STORAGE}
function TRotLayer32.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'Angle' then
    fAngle := GetDoubleProp(propVal, Result)
  else if propName = 'AutoPivot' then
    fAutoPivot := GetBoolProp(propVal, Result)
  else if propName = 'PivotPt' then
    fPivotPt := GetPointDProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TRotLayer32.WriteProperties;
begin
  inherited;
  WriteDoubleProp('Angle', Angle);
  WritePointDProp('PivotPt', PivotPt);
  WriteBoolProp('AutoPivot', AutoPivot)
end;
{$ENDIF}

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

procedure TVectorLayer32.SetPaths(const newPaths: TPathsD);
begin
  fPaths := CopyPaths(newPaths);
  fPivotPt := InvalidPointD;
  if Assigned(fPaths) then RepositionAndDraw
  else inherited SetInnerBounds(NullRectD);
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
    MatrixScale(mat, w/Width, h/Height);
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
  fPaths := TranslatePath(fPaths, dx,dy);
  if fAutoPivot and not PointsEqual(fPivotPt, InvalidPointD) then
    fPivotPt := TranslatePoint(fPivotPt, dx,dy);
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
  if Assigned(fOnDraw) then fOnDraw(self);
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
    if MasterImage.IsEmpty then Exit;
    MasterImage.BlockNotify;
    MasterImage.CropTransparentPixels;
    MasterImage.UnblockNotify;
    Invalidate;

    //reset whenever MasterImage changes
    fAngle := 0;
    fMatrix := IdentityMatrix;
    fRotating := false;
    fPreScaleSize := Size(MasterImage.Width, MasterImage.Height);

    if Image.IsEmpty and
      (TLayerNotifyImage32(Image).UpdateCount = 0) then
        Image.Assign(MasterImage);
  end else
  begin
    if MasterImage.IsEmpty and not Image.IsEmpty then
    begin
      Image.BlockNotify;
      try
        Image.CropTransparentPixels;
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
  newWidth, newHeight: double;
  w,h: integer;
begin

  if fRotating and Assigned(Image) then
  begin
    //rotation has just ended
    fRotating := false;
    //update fMatrix with the new rotation angle
    if (fAngle <> 0) then
      MatrixRotate(fMatrix, Image.MidPoint, fAngle);

    //and since we're about to start scaling, we need
    //to store the starting size, and reset the angle
    fPreScaleSize := Size(Image.Width, Image.Height);
    fAngle := 0;
  end;

  newWidth := newBounds.Width;
  newHeight := newBounds.Height;

  //make sure the image is large enough to scale safely
  if not MasterImage.IsEmpty and (newWidth > 1) and (newHeight > 1) then
  begin
    Image.BeginUpdate;
    try
      Image.Assign(MasterImage);
      //apply any prior transformations
      Image.Resampler := rWeightedBilinear;
      AffineTransformImage(Image, fMatrix, true); // assumes no skew
      //cropping is very important with rotation
      SymmetricCropTransparent(Image);
      w := Ceil(newBounds.Right) - Floor(newBounds.Left);
      h := Ceil(newBounds.Bottom) - Floor(newBounds.Top);
      Image.Resize(w, h); //nb: stretch resizes
      PositionAt(newBounds.TopLeft);
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
  mat: TMatrixD;
  pt, mp: TPointD;
begin
  Result := (angleDelta <> 0) and
    not MasterImage.IsEmpty and
    inherited Rotate(angleDelta);

  if not Result then Exit;

  mp := MidPoint;

  //if not already rotating, then update fMatrix with prior scaling
  if not fRotating then
  begin
    Assert((fPreScaleSize.cx > 0) and (fPreScaleSize.cy > 0), 'oops!');
    MatrixScale(fMatrix,
      Image.Width/fPreScaleSize.cx,
      Image.Height/fPreScaleSize.cy);

    fRotating := true;
    if fAutoPivot then fPivotPt := mp;
  end;

  RotatePoint(mp, PivotPt, angleDelta);

  Image.BlockNotify;
  try
    Image.Assign(MasterImage);
    mat := fMatrix;
    pt := PointD(PivotPt.X - fLeft, PivotPt.Y - fTop);
    MatrixRotate(mat, pt, Angle);
    Image.Resampler := rWeightedBilinear;
    AffineTransformImage(Image, mat, true); // assumes no skew
  finally
    Image.UnblockNotify;
  end;

  fWidth := Image.Width;
  fHeight := Image.Height;
  PositionCenteredAt(mp);
  DoAutoHitTest;
end;

//------------------------------------------------------------------------------
// TRotatingGroupLayer32 class
//------------------------------------------------------------------------------

constructor TRotatingGroupLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;
  SetDesignerLayer(true);
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
  if not ClockwiseRotationIsAnglePositive then
    startingZeroOffset := -startingZeroOffset;
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
    pt := TranslatePoint(pivot, -Left, -Top);
    DrawDashedLine(Image, Circle(pt, dist - q),
      dashes, nil, q, clRed32, esPolygon);
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
  result := TButtonDesignerLayer32(InsertChild(fBtnLayerClass, btnIdx));
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
  fBackColor := clBtnFace32;
  fResampler := DefaultResampler;
  fLastUpdateType := utUndefined;

  if StorageState = ssLoading then Exit;
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

{$IFNDEF NO_STORAGE}
function  TLayeredImage32.ReadProperty(const propName, propVal: string): Boolean;
begin
  if propName = 'Resampler' then
    Resampler := GetIntProp(propVal, Result)
  else if propName = 'BackgroundColor' then
    BackgroundColor := GetColorProp(propVal, Result)
  else if propName = 'Width' then
    Width := GetIntProp(propVal, Result)
  else if propName = 'Height' then
    Height := GetIntProp(propVal, Result)
  else
    Result := false;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.WriteProperties;
begin
  inherited;
  WriteIntProp('Resampler', Resampler);
  WriteColorProp('BackgroundColor', BackgroundColor);
  WriteIntProp('Width', Width);
  WriteIntProp('Height', Height);
end;
//------------------------------------------------------------------------------
{$ENDIF}

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
begin
  if not Assigned(fRoot) then Exit;
  fInvalidRect := RectD(fBounds);
  fLastUpdateType := utUndefined;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetRepaintNeeded: Boolean;
begin
  Result := Root.UpdateInfo.updateMethod <> umNone;
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
  if Assigned(fRoot) then
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

function TLayeredImage32.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  Result := inherited InsertChild(index, storeClass);
  if (StorageState = ssLoading) and (ChildCount = 1) then
  begin
    if not (Result is TGroupLayer32) then
      raise Exception.Create(rsLayeredImage32Error);
    fRoot := TGroupLayer32(Result);
    fRoot.Name := rsRoot;
    fRoot.fLayeredImage := self;
  end;
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
  Result := parent.InsertChild(layerClass, index, name);
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
      dashes, nil, DPIAware(2), clRed32, esPolygon);
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
  dashes[0] := dpiAware1 *2; dashes[1] := dpiAware1 *4;
end;

initialization
  InitDashes;
  DefaultButtonSize := dpiAware1*10;

{$IFNDEF NO_STORAGE}
  RegisterStorageClass(TLayeredImage32);
  RegisterStorageClass(TLayer32);
  RegisterStorageClass(TGroupLayer32);
{$ENDIF}

end.
