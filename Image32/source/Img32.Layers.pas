unit Img32.Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.5                                                             *
* Date      :  31 October 2021                                                 *
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
  TGroupLayer32 = class;
  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;

  //THitTest is used for hit-testing (see TLayeredImage32.GetLayerAt).
  THitTest = class
    htImage   : TImage32;
    enabled   : Boolean;
    constructor Create;
    destructor Destroy; override;
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
    fLeft           : integer;
    fTop            : integer;
    fImage          : TImage32;
    fMergeImage     : TImage32;
    fClipImage      : TImage32;
    fName           : string;
    fIndex          : integer;
    fVisible        : Boolean;
    fOpacity        : Byte;
    fCursorId       : integer;
    fUserData       : TObject;
    fBlendFunc      : TBlendFunction;
    fOldBounds      : TRect;    //bounds when layer was last merged
    fRefreshPending : boolean;
    fLayeredImage   : TLayeredImage32;
    fParent         : TLayer32;
{$IFDEF XPLAT_GENERICS}
    fChilds         : TList<TLayer32>;
{$ELSE}
    fChilds         : TList;
{$ENDIF}
    fInvalidRect    : TRect;
    fUpdateCount    : Integer; //see beginUpdate/EndUpdate
    fClipPath       : TPathsD;
    function   TopLeft: TPoint;
    function   GetMidPoint: TPointD;
    procedure  SetVisible(value: Boolean);
    function   GetHeight: integer;
    function   GetWidth: integer;
    procedure  SetBlendFunc(func: TBlendFunction);

    function   GetChildCount: integer;
    function   GetChild(index: integer): TLayer32;
    function   GetRoot: TGroupLayer32;
    function   FindLayerNamed(const name: string): TLayer32; virtual;
    procedure  ReindexChildsFrom(startIdx: Integer);
    procedure  SetClipPath(const path: TPathsD);
    procedure  UpdateBounds;
    function   GetNextLayerInGroup: TLayer32;
    function   GetPrevLayerInGroup: TLayer32;
  protected
    procedure  BeginUpdate; virtual;
    procedure  EndUpdate;   virtual;
    procedure  RefreshPending;
    procedure  PreMerge(hideDesigners: Boolean);
    procedure  PreMergeAll(hideDesigners: Boolean);
    procedure  Merge(hideDesigners: Boolean; const updateRect: TRect);
    function   GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean): TLayer32;
    procedure  InternalDeleteChild(index: integer; fromChild: Boolean);

    function   HasChildren: Boolean;
    function   GetBounds: TRect;
    procedure  SetOpacity(value: Byte); virtual;
    procedure  ImageChanged(Sender: TImage32); virtual;
    procedure  UpdateLayeredImage(newLayeredImage: TLayeredImage32);
  public
    constructor Create(parent: TLayer32; const name: string = ''); virtual;
    destructor Destroy; override;
    procedure  SetSize(width, height: integer);

    function   BringForwardOne: Boolean;
    function   SendBackOne: Boolean;
    function   BringToFront: Boolean;
    function   SendToBack: Boolean;
    function   Move(newParent: TLayer32; idx: integer): Boolean;
    function   GetAbsoluteOrigin: TPoint;

    procedure  PositionAt(const pt: TPoint); overload;
    procedure  PositionAt(x, y: integer); overload; virtual;
    procedure  PositionCenteredAt(X, Y: integer); overload;
    procedure  PositionCenteredAt(const pt: TPoint); overload;
    procedure  PositionCenteredAt(const pt: TPointD); overload;
    procedure  SetBounds(const newBounds: TRect); virtual;
    procedure  Invalidate(rec: TRect); virtual;

    function   AddChild(layerClass: TLayer32Class; const name: string = ''): TLayer32;
    function   InsertChild(layerClass: TLayer32Class;
      index: integer; const name: string = ''): TLayer32;
    procedure  DeleteChild(index: integer);
    procedure  ClearChildren;

    property   ChildCount: integer read GetChildCount;
    property   Child[index: integer]: TLayer32 read GetChild; default;
    //ClipPath: defines a client region for child layers (in child coords).
    //When undefined, the clip region is the layer's bounds.
    property   ClipPath: TPathsD read fClipPath write SetClipPath;
    procedure  Offset(dx, dy: integer); virtual;
    property   Bounds: TRect read GetBounds;
    property   CursorId: integer read fCursorId write fCursorId;
    property   Parent: TLayer32 read fParent;
    property   Height: integer read GetHeight;
    property   Image: TImage32 read fImage;
    property   Index: integer read fIndex;
    property   Left: integer read fLeft;
    property   Location: TPoint read TopLeft;
    property   MidPoint: TPointD read GetMidPoint;
    property   Name: string read fName write fName;
    property   Opacity: Byte read fOpacity write SetOpacity;
    property   Root: TGroupLayer32 read GetRoot;
    property   RootOwner: TLayeredImage32 read fLayeredImage;
    property   Top: integer read fTop;
    property   Visible: Boolean read fVisible write SetVisible;
    property   Width: integer read GetWidth;
    property   UserData: TObject read fUserData write fUserData;
    property   BlendFunc: TBlendFunction read fBlendFunc write SetBlendFunc;
    property   PrevLayerInGroup: TLayer32 read GetPrevLayerInGroup;
    property   NextLayerInGroup: TLayer32 read GetNextLayerInGroup;
  end;

  TGroupLayer32 = class(TLayer32)
  private
    function   GetFirstChild: TLayer32;
    function   GetLastChild: TLayer32;
  public
    procedure  Offset(dx, dy: integer); override;
    property   FirstChild: TLayer32 read GetFirstChild;
    property   LastChild: TLayer32 read GetLastChild;
  end;

  THitTestLayer32 = class(TLayer32) //abstract classs
  private
    fHitTest    : THitTest;
    procedure ClearHitTesting;
    function  GetEnabled: Boolean;
    procedure SetEnabled(value: Boolean);
  protected
    procedure  ImageChanged(Sender: TImage32); override;
    property   HitTestRec : THitTest read fHitTest write fHitTest;
  public
    constructor Create(parent: TLayer32; const name: string = ''); override;
    destructor Destroy; override;
    property HitTestEnabled: Boolean read GetEnabled write SetEnabled;
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
    constructor Create(parent: TLayer32; const name: string = ''); override;
    function Rotate(angleDelta: double): Boolean; virtual;
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
  protected
    procedure SetPaths(const newPaths: TPathsD); virtual;
    procedure Draw; virtual;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    procedure SetBounds(const newBounds: TRect); override;
    procedure Offset(dx,dy: integer); override;
    function Rotate(angleDelta: double): Boolean; override;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD); virtual;
    procedure UpdateHitTestMaskFromImage;
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
    procedure UpdateHitTestMaskTranspar(compareFunc: TCompareFunction;
      referenceColor: TColor32; tolerance: integer);
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    destructor  Destroy; override;
    procedure Offset(dx,dy: integer); override;
    procedure UpdateHitTestMask;
    procedure UpdateHitTestMaskOpaque; virtual;
    procedure UpdateHitTestMaskTransparent(alphaValue: Byte = 127); overload; virtual;
    procedure SetBounds(const newBounds: TRect); override;
    function  Rotate(angleDelta: double): Boolean; override;

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
    fBtnSize  : integer;
    fBtnShape : TButtonShape;
    fBtnColor : TColor32;
    fBtnLayerClass: TButtonDesignerLayer32Class;
    fBtnCount : integer;
  public
    function AddButton(const pt: TPointD): TButtonDesignerLayer32;
    function InsertButton(const pt: TPointD; btnIdx: integer): TButtonDesignerLayer32;
    property ButtonCnt: integer read fBtnCount;

  end;

  TDesignerLayer32 = class(THitTestLayer32) //generic design layer
  public
    constructor Create(parent: TLayer32; const name: string = ''); override;
    procedure UpdateHitTestMask(const vectorRegions: TPathsD); virtual;
  end;

  TButtonDesignerLayer32 = class(TDesignerLayer32) //button (design) layer
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
    constructor Create(parent: TLayer32; const name: string = ''); override;
    procedure Draw; virtual;

    property Enabled  : Boolean read fEnabled write SetEnabled;
    property Size     : integer read fSize write fSize;
    property Color    : TColor32 read fColor write fColor;
    property Shape    : TButtonShape read fShape write fShape;
    property ButtonOutline: TPathD read fOutline write fOutline;
    //BtnIdx: useful when Buttons are 'moved' (ie their level changed)
    property BtnIdx   : integer read fBtnIdx;
  end;

  TUpdateType = (utUndefined, utShowDesigners, utHideDesigners);

  TLayeredImage32 = class
  private
    fRoot              : TGroupLayer32;
    fBounds            : TRect;
    fBackColor         : TColor32;
    fResampler         : integer;
    fLastUpdateType    : TUpdateType;
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
      parent: TLayer32 = nil; const name: string = ''): TLayer32;
    function  InsertLayer(layerClass: TLayer32Class; parent: TLayer32;
      index: integer; const name: string = ''): TLayer32;
    procedure DeleteLayer(layer: TLayer32); overload;
    procedure DeleteLayer(layerIndex: integer;
      parent: TLayer32 = nil); overload;
    function  FindLayerNamed(const name: string): TLayer32;
    function  GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean = false): TLayer32;
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

{$IFNDEF MSWINDOWS}
uses
  Img32.FMX, Img32.Layers;
{$ENDIF}

resourcestring
  rsRoot                   = 'root';
  rsCreateLayerError       = 'TLayer32 error - no group owner defined.';
  rsButton                 = 'Button';
  rsSizingButtonGroup      = 'SizingButtonGroup';
  rsRotatingButtonGroup    = 'RotatingButtonGroup';
  rsChildIndexRangeError   = 'TLayer32 - child index error';
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
// THitTest
//------------------------------------------------------------------------------

constructor THitTest.Create;
begin
  htImage := TImage32.Create;
  htImage.BlockNotify; //ie never notify :)
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
    layer.HitTestRec.htImage.SetSize(width, height);
  if layer.Image.IsEmpty then Exit;
  layer.HitTestRec.enabled := true;
  DrawPolygon(layer.HitTestRec.htImage, paths, frEvenOdd, clWhite32);
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

constructor TLayer32.Create(parent: TLayer32; const name: string);
begin
{$IFDEF XPLAT_GENERICS}
  fChilds       := TList<TLayer32>.Create;
{$ELSE}
  fChilds       := TList.Create;
{$ENDIF}
  fImage        := TLayerNotifyImage32.Create(self);
  fParent       := parent;
  fName         := name;
  fVisible      := True;
  fOpacity      := 255;
  CursorId      := crDefault;
  fInvalidRect  := NullRect;
  if assigned(parent) then
    fLayeredImage := parent.fLayeredImage;
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
begin
  ClearChildren;
  if fVisible then
  begin
    fVisible := false;
    Invalidate(fOldBounds);
    Invalidate(Bounds);
  end;
  fImage.Free;
  fChilds.Free;
  FreeAndNil(fMergeImage);
  FreeAndNil(fClipImage);
  if Assigned(fParent) then
  begin
    if fRefreshPending then
      fParent.Invalidate(fOldBounds);
    fParent.Invalidate(fInvalidRect);
    fParent.InternalDeleteChild(Index, true);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayer32.BeginUpdate;
begin
  if not fRefreshPending then
    Invalidate(fOldBounds);
  Inc(fParent.fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TLayer32.EndUpdate;
begin
  Dec(fParent.fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Invalidate(rec: TRect);
begin
  fInvalidRect := Img32.Vector.UnionRect(fInvalidRect, rec);
  RefreshPending;
end;
//------------------------------------------------------------------------------

function TLayer32.GetNextLayerInGroup: TLayer32;
begin
  if not Assigned(fParent) or (fIndex = fParent.ChildCount -1) then
    Result := nil else
    Result := fParent[fIndex +1];
end;
//------------------------------------------------------------------------------

function TLayer32.GetPrevLayerInGroup: TLayer32;
begin
  if not Assigned(fParent) or (fIndex = 0) then
    Result := nil else
    Result := fParent[fIndex -1];
end;
//------------------------------------------------------------------------------

procedure TLayer32.ImageChanged(Sender: TImage32);
begin
  if not fRefreshPending then Invalidate(fOldBounds);
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
var
  w,h: integer;
begin
  fLeft := newBounds.Left;
  fTop := newBounds.Top;
  //nb: Image.SetSize will call the ImageChanged method
  RectWidthHeight(newBounds, w, h);
  Image.SetSize(w, h);
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
  if (value = fVisible) or (Root = Self) then Exit;
  fVisible := value;
  if Visible then
    Invalidate(fOldBounds) else
    Parent.Invalidate(fOldBounds);
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
  Result := assigned(fParent) and (index < fParent.ChildCount -1);
  if not Result then Exit;
  fParent.fChilds.Move(index, index +1);
  fParent.ReindexChildsFrom(index);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.SendBackOne: Boolean;
begin
  Result := assigned(fParent) and (index > 0);
  if not Result then Exit;
  fParent.fChilds.Move(index, index -1);
  fParent.ReindexChildsFrom(index -1);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.BringToFront: Boolean;
begin
  Result := assigned(fParent) and
    (index < fParent.ChildCount -1);
  if not Result then Exit;
  fParent.fChilds.Move(index, fParent.ChildCount -1);
  fParent.ReindexChildsFrom(index);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.SendToBack: Boolean;
begin
  Result := assigned(fParent) and (index > 0);
  if not Result then Exit;
  fParent.fChilds.Move(index, 0);
  fParent.ReindexChildsFrom(0);
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.TopLeft: TPoint;
begin
  Result := Types.Point(fLeft, fTop);
end;
//------------------------------------------------------------------------------

function TLayer32.GetAbsoluteOrigin: TPoint;
var
  layer: TLayer32;
begin
  Result := NullPoint;
  layer := Parent;
  while Assigned(layer) do
    with layer do
    begin
      if not (layer is TGroupLayer32) then
        Result := OffsetPoint(Result, Left, Top);
      layer := Parent;
  end;
end;
//------------------------------------------------------------------------------

function TLayer32.Move(newParent: TLayer32; idx: integer): Boolean;
var
  layer: TLayer32;
begin
  Result := false;
  if not assigned(fParent) or not assigned(newParent) then
    Exit;

  //make sure we don't create circular parenting
  layer := newParent;
  while assigned(layer) do
    if (layer = self) then Exit
    else layer := layer.Parent;


  if newParent = fParent then
  begin
    if idx >= fParent.ChildCount then idx := fParent.ChildCount-1
    else if idx < 0 then idx := 0;
    if idx = fIndex then Exit;

    fParent.fChilds.Move(fIndex, idx);
    fParent.ReindexChildsFrom(Min(idx, fIndex));
  end else
  begin
    if idx >= newParent.ChildCount then idx := newParent.ChildCount
    else if idx < 0 then idx := 0;

    if Visible then
      fParent.Invalidate(Bounds);
    fParent.fChilds.Delete(fIndex);
    fParent.ReindexChildsFrom(fIndex);

    newParent.fChilds.Insert(idx, self);
    self.fParent := newParent;
    newParent.ReindexChildsFrom(idx);
  end;
  newParent.RefreshPending;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetBlendFunc(func: TBlendFunction);
begin
  if not Assigned(fParent) then Exit;
  fBlendFunc := func;
  if Visible then
    fParent.Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

function TLayer32.GetChildCount: integer;
begin
  Result := fChilds.Count;
end;
//------------------------------------------------------------------------------

function TLayer32.HasChildren: Boolean;
begin
  Result := fChilds.Count > 0;
end;
//------------------------------------------------------------------------------

function TLayer32.GetChild(index: integer): TLayer32;
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create(rsChildIndexRangeError);
  Result := TLayer32(fChilds[index]);
end;
//------------------------------------------------------------------------------

procedure TLayer32.ClearChildren;
var
  i: integer;
begin
  for i := fChilds.Count -1 downto 0 do
    TLayer32(fChilds[i]).Free;
  fChilds.Clear;
  Image.BlockNotify;
  Image.SetSize(0, 0);
  Image.UnblockNotify;
  FreeAndNil(fMergeImage);
  fClipPath := nil;
end;
//------------------------------------------------------------------------------

function   TLayer32.AddChild(layerClass: TLayer32Class;
  const name: string = ''): TLayer32;
begin
  Result := InsertChild(layerClass, MaxInt, name);
end;
//------------------------------------------------------------------------------

function   TLayer32.InsertChild(layerClass: TLayer32Class;
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

function TLayer32.GetRoot: TGroupLayer32;
begin
  if Assigned(fLayeredImage) then
    Result := fLayeredImage.fRoot else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetClipPath(const path: TPathsD);
begin
  RefreshPending;
  fClipPath := path;
  if Assigned(fClipPath) and (self is THitTestLayer32) then
  begin
    //create a clip mask
    if Assigned(fClipImage) then
      fClipImage.SetSize(Width, Height) else
      fClipImage := TImage32.Create(Width, Height);
    DrawPolygon(fClipImage, path, frEvenOdd, clWhite32);
  end else
    FreeAndNil(fClipImage);
end;
//------------------------------------------------------------------------------

procedure  TLayer32.InternalDeleteChild(index: integer; fromChild: Boolean);
var
  child: TLayer32;
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create(rsChildIndexRangeError);

  child := TLayer32(fChilds[index]);
  fChilds.Delete(index);
  FreeAndNil(fMergeImage);
//  if child.Visible then
//  begin
//    Invalidate(child.fOldBounds);
//    Invalidate(child.Bounds);
//  end;

  if not fromChild then
  begin
    child.fParent := nil; //avoids recursion :)
    child.Free;
  end;
  if index < ChildCount then
    ReindexChildsFrom(index);
end;
//------------------------------------------------------------------------------

procedure TLayer32.DeleteChild(index: integer);
begin
  if (ChildCount = 1) then
    ClearChildren else
    InternalDeleteChild(index, false);
end;
//------------------------------------------------------------------------------

procedure TLayer32.RefreshPending;
begin
  if fRefreshPending then Exit;
  fRefreshPending := true;
  if Assigned(Parent) then
    Parent.RefreshPending;
end;
//------------------------------------------------------------------------------

procedure TLayer32.UpdateBounds;
var
  i: integer;
  rec: TRect;
begin
  rec := nullRect;
  for i := 0 to ChildCount -1 do
    rec := Img32.Vector.UnionRect(rec, Child[i].Bounds);
  Image.BlockNotify;
  SetBounds(rec);
  Image.UnblockNotify;
end;
//------------------------------------------------------------------------------

procedure TLayer32.PreMerge(hideDesigners: Boolean);
var
  i         : integer;
  childLayer: TLayer32;
begin
  fInvalidRect := NullRect;
  //this method is recursive and updates each group's fInvalidRect
  for i := 0 to ChildCount -1 do
  begin
    childLayer := Child[i];
    with childLayer do
    begin
      if not Visible or
        ((childLayer is TDesignerLayer32) and hideDesigners) then
          Continue;

      if fRefreshPending then
      begin
        //get childLayer's 'old' Bounds
        Self.fInvalidRect :=
          Img32.Vector.UnionRect(Self.fInvalidRect, fInvalidRect);
        if HasChildren then
          PreMerge(hideDesigners);

        //get childLayer's new Bounds
        if (childLayer is TGroupLayer32) then
        begin
          UpdateBounds;
          Self.fInvalidRect :=
            Img32.Vector.UnionRect(Self.fInvalidRect, fInvalidRect);
        end else
          //todo: this can be improved
          Self.fInvalidRect :=
            Img32.Vector.UnionRect(Self.fInvalidRect, Bounds);
      end;

      fInvalidRect := NullRect;
      fOldBounds := Bounds;
    end;

  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.PreMergeAll(hideDesigners: Boolean);
var
  i         : integer;
  childLayer: TLayer32;
begin
  //this method is recursive and updates each group's fInvalidRect
  for i := 0 to ChildCount -1 do
  begin
    childLayer := Child[i];

    if not childLayer.Visible or
      (hideDesigners and (childLayer is TDesignerLayer32)) then
        Continue;

    if childLayer.HasChildren then
      childLayer.PreMergeAll(hideDesigners);

    if (childLayer is TGroupLayer32) then
      childLayer.UpdateBounds;

    childLayer.fInvalidRect := NullRect;
    childLayer.fOldBounds := childLayer.Bounds;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.Merge(hideDesigners: Boolean; const updateRect: TRect);
var
  childLayer: TLayer32;
  i: integer;
  img, img2, childImg: TImage32;
  origOffset: TPoint;
  rec, dstRect, srcRect: TRect;
begin
  if not Assigned(fLayeredImage) or
    not Visible or (Opacity < 2) or
    not fRefreshPending or Image.IsEmpty then
      Exit;

  if assigned(Parent) and (ChildCount > 0) then
  begin
    if not Assigned(fMergeImage) then
      fMergeImage := TImage32.Create(fImage) else
      fMergeImage.Assign(fImage);
    img := fMergeImage;
  end else
    img := fImage;

  if (self is TGroupLayer32) then
    origOffset := TopLeft else
    origOffset := NullPoint;

  //merge redraw all children
  for i := 0 to ChildCount -1 do
  begin
    childLayer := Child[i];

    if not childLayer.Visible or
      (hideDesigners and (childLayer is TDesignerLayer32)) then
        Continue;

    //recursive merge
    if (childLayer.HasChildren) then
      TLayer32(childLayer).Merge(hideDesigners, updateRect);

    if Assigned(childLayer.fMergeImage) then
      childImg := childLayer.fMergeImage else
      childImg := childLayer.Image;

    if Assigned(fParent) then
    begin
      dstRect := childLayer.Bounds;
      Types.OffsetRect(dstRect, -origOffset.X, -origOffset.Y);
      Types.IntersectRect(dstRect, dstRect, Image.Bounds);
    end else
    begin
      //this must be the root layer
      dstRect := childLayer.Bounds;
      Types.IntersectRect(dstRect, dstRect, fLayeredImage.Bounds);
      Types.IntersectRect(dstRect, dstRect, updateRect);
    end;

    srcRect := dstRect;
    with childLayer do
      Types.OffsetRect(srcRect, origOffset.X - Left, origOffset.Y - Top);

    //draw the child  onto the parent's image
    img2 := nil;
    img.BlockNotify;
    try
      if (childLayer.Opacity < 254) or Assigned(fClipPath) then
      begin
        img2 := TImage32.Create(childImg);
        img2.ReduceOpacity(childLayer.Opacity);
        if Assigned(fClipImage) then
        begin
          rec := fClipImage.Bounds;
          Types.OffsetRect(rec, -childLayer.Left, -childLayer.Top);
          img2.CopyBlend(fClipImage, fClipImage.Bounds, rec, BlendMask);
        end;
      end else
      begin
        img2 := childImg;
      end;

      if Assigned(childLayer.BlendFunc) then
      begin
        img.CopyBlend(img2, srcRect, dstRect, childLayer.BlendFunc);
      end else
      begin
        img.CopyBlend(img2, srcRect, dstRect, BlendToAlpha);
      end;

    finally
      if Assigned(img2) and (img2 <> childImg) then
        img2.Free;
      img.UnblockNotify;
    end;
    childLayer.fRefreshPending := false;
  end;
  fInvalidRect := NullRect;
  fRefreshPending := false;
end;
//------------------------------------------------------------------------------

function TLayer32.GetLayerAt(const pt: TPoint;
  ignoreDesigners: Boolean): TLayer32;
var
  i: integer;
  childLayer: TLayer32;
  pt2: TPoint;
  Result2: TLayer32;
begin
  Result := nil;

  if (self is TGroupLayer32) then
    pt2 := pt else
    pt2 := OffsetPoint(pt, -Left, -Top);

  //if 'pt2' is outside the clip mask then don't continue
  if Assigned(fClipImage) then
    if TARGB(fClipImage.Pixel[pt2.X, pt2.Y]).A < 128 then Exit;

  for i := ChildCount -1 downto 0 do
  begin
    childLayer := Child[i];

    if not childLayer.Visible or not PtInRect(childLayer.Bounds, pt2) or
      (ignoreDesigners and (childLayer is TDesignerLayer32)) then
        Continue;

    if (childLayer is THitTestLayer32) then
      with THitTestLayer32(childLayer) do
        if not HitTestRec.enabled then
          Continue
        else if fHitTest.htImage.IsEmpty then
          Result := childLayer //ie rectangles
        else
        begin
          if TARGB(fHitTest.htImage.Pixel[pt2.X-left, pt2.Y-top]).A >= 128 then
            Result := childLayer;
          if Assigned(Result) and not childLayer.HasChildren then Exit;
        end;

    if childLayer.HasChildren and
      (Assigned(Result) or (childLayer is TGroupLayer32)) then
    begin
      //recursive
      Result2 := childLayer.GetLayerAt(pt2, ignoreDesigners);
      if Assigned(Result2) then Result := Result2;
    end;
    if Assigned(Result) then Exit;
  end;
end;
//------------------------------------------------------------------------------

procedure TLayer32.ReindexChildsFrom(startIdx: Integer);
var
  i: integer;
begin
  for i := startIdx to ChildCount -1 do
    Child[i].fIndex := i;
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
      Result := TLayer32(Child[i]).FindLayerNamed(name);
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

procedure TGroupLayer32.Offset(dx, dy: integer);
var
  i: integer;
begin
  if (dx = 0) and (dy = 0) then Exit;
  Invalidate(Bounds);
  PositionAt(fLeft + dx, fTop + dy);
  BeginUpdate;
  try
  for i := 0 to ChildCount -1 do
    Child[i].Offset(dx, dy);
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function TGroupLayer32.GetFirstChild: TLayer32;
begin
  if ChildCount = 0 then Result := nil
  else Result := Child[0];
end;
//------------------------------------------------------------------------------

function TGroupLayer32.GetLastChild: TLayer32;
begin
  if ChildCount = 0 then Result := nil
  else Result := Child[ChildCount -1];
end;

//------------------------------------------------------------------------------
// THitTestLayer32 class
//------------------------------------------------------------------------------

constructor THitTestLayer32.Create(parent: TLayer32; const name: string = '');
begin
  inherited;
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
  inherited;
  ClearHitTesting;
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
// TRotateLayer32 class
//------------------------------------------------------------------------------

constructor TRotateLayer32.Create(parent: TLayer32; const name: string = '');
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

function TRotateLayer32.Rotate(angleDelta: double): Boolean;
begin
  Result := (angleDelta <> 0) and not HasChildren;
  if not Result then Exit;
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

constructor TVectorLayer32.Create(parent: TLayer32;
  const name: string = '');
begin
  inherited;
  fMargin := dpiAware1 *2;
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
  else inherited SetBounds(NullRect);
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
  if Assigned(fPaths) and
    (Width > m2) and (Height > m2) and (w > 1) and (h > 1)  then
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
  begin
    inherited;
    RepositionAndDraw;
  end;
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
  if Assigned(fPaths) then
  begin
    rec := Img32.Vector.GetBounds(fPaths);
    Img32.Vector.InflateRect(rec, Margin, Margin);
    inherited SetBounds(rec);
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

constructor TRasterLayer32.Create(parent: TLayer32;  const name: string = '');
begin
  inherited Create(parent, name);
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

procedure TRasterLayer32.UpdateHitTestMask;
begin
  fHitTest.htImage.Assign(Image);
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
    HitTestRec.htImage.SetSize(0,0);
end;
//------------------------------------------------------------------------------

procedure TRasterLayer32.ImageChanged(Sender: TImage32);
begin
  if (Sender = MasterImage) then
  begin
    if MasterImage.IsBlank then Exit;
    //reset the layer whenever MasterImage changes
    fAngle := 0;
    fMatrix := IdentityMatrix;
    fRotating := false;

    if not fRefreshPending then
      Invalidate(fOldBounds);

    if not Image.IsEmpty then Exit;
    MasterImage.BlockNotify;
    MasterImage.CropTransparentPixels;
    MasterImage.UnblockNotify;
    fSavedSize := Img32.Vector.Size(MasterImage.Width, MasterImage.Height);
    if (TLayerNotifyImage32(Image).UpdateCount = 0) then
      Image.Assign(MasterImage); //this will call ImageChange for Image
    if Assigned(fLayeredImage) then
      Image.Resampler := fLayeredImage.Resampler;

  end else
  begin
    if MasterImage.IsEmpty and not Image.IsBlank then
    begin
      Image.BlockNotify;
      try
        Image.CropTransparentPixels;
        MasterImage.Assign(Image); //this will call ImageChanged again
      finally
        Image.UnblockNotify;
      end;
    end;
    inherited;
    DoAutoHitTest;
  end;
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
  RectWidthHeight(newBounds, newWidth, newHeight);

  //make sure the image is large enough to scale safely
  if (MasterImage.Width > 1) and (MasterImage.Height > 1) and
    (newWidth > 1) and (newHeight > 1) then
  begin
    Image.BeginUpdate;
    try
      Image.Assign(MasterImage);
      if Assigned(fLayeredImage) then
        Image.Resampler := fLayeredImage.Resampler;
      //apply any prior transformations
      AffineTransformImage(Image, fMatrix);
      //cropping is very important with rotation
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
  if fRotating or not Assigned(Image) or
    not Assigned(MasterImage) then Exit;
  fRotating := true;
  fSavedMidPt := MidPoint;
  if fAutoPivot then fPivotPt := fSavedMidPt;

  //scaling has just ended and rotating is about to start
  //so apply the current scaling to the matrix
  if (fSavedSize.cx = 0) or (fSavedSize.cy = 0) then
  begin
    fSavedSize.cx := Image.Width;
    fSavedSize.cy := Image.Height;
    MatrixScale(fMatrix, Image.Width/MasterImage.Width,
      Image.Height/MasterImage.Height);
  end else
  begin
    MatrixScale(fMatrix, Image.Width/fSavedSize.cx,
      Image.Height/fSavedSize.cy);
  end;
end;
//------------------------------------------------------------------------------

function TRasterLayer32.GetMatrix: TMatrixD;
begin
  Result := fMatrix;
  //update for transformations not yet unapplied to fMatrix
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

function TRasterLayer32.Rotate(angleDelta: double): Boolean;
var
  mat: TMatrixD;
  rec: TRectD;
begin
  Result := not MasterImage.IsEmpty and inherited Rotate(angleDelta);
  if not Result then Exit;

  DoPreRotationCheck;

  if not fAutoPivot then
    RotatePoint(fSavedMidPt, PivotPt, angleDelta);

  Image.BeginUpdate;
  try
    ////////////////////////////////
    Image.Assign(MasterImage);
    ////////////////////////////////
    if Assigned(fLayeredImage) then
      Image.Resampler := fLayeredImage.Resampler;
    rec := GetRotatedRectBounds(RectD(Image.Bounds), Angle);

    //get prior transformations and apply new rotation
    mat := fMatrix;
    MatrixTranslate(mat, -Width/2,-Height/2);
    MatrixRotate(mat, NullPointD, Angle);
    MatrixTranslate(mat, rec.Width/2, rec.Height/2);

    ////////////////////////////////
    AffineTransformImage(Image, mat);
    //symmetric cropping prevents center wobbling
    SymmetricCropTransparent(Image);
    ////////////////////////////////
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

  with AddChild(TDesignerLayer32) do    //Layer 0 - design layer
  begin
    SetBounds(Rect(rec2));
    i := dpiAware1*2;
    r := rec2;
    Img32.Vector.InflateRect(r, -i,-i);
    OffsetRect(r, -Left, -Top);
    DrawDashedLine(Image, Ellipse(r), dashes, nil, i, clRed32, esPolygon);
  end;

  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  with TButtonDesignerLayer32(AddChild( //Layer 1 - pivot button
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

constructor TDesignerLayer32.Create(parent: TLayer32; const name: string = '');
begin
  inherited;
  fHitTest.enabled := false;
end;
//------------------------------------------------------------------------------

procedure  TDesignerLayer32.UpdateHitTestMask(const vectorRegions: TPathsD);
begin
  UpdateHitTestMaskUsingPath(self, vectorRegions);
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

constructor TButtonDesignerLayer32.Create(parent: TLayer32;
  const name: string = '');
begin
  inherited;
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
  Invalidate(Bounds);
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

constructor TLayeredImage32.Create(Width: integer; Height: integer);
begin
  fRoot := TGroupLayer32.Create(nil, rsRoot);
  fRoot.fLayeredImage := self;
  fBounds := Rect(0, 0, Width, Height);
  fRoot.SetSize(width, Height);
  fBackColor := clBtnFace32;
  fResampler := DefaultResampler;
  fLastUpdateType := utUndefined;
end;
//------------------------------------------------------------------------------

destructor TLayeredImage32.Destroy;
begin
  FreeAndNil(fRoot);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetSize(width, height: integer);
begin
  fBounds := Rect(0, 0, Width, Height);
  if not Assigned(fRoot) then Exit;
  fRoot.SetBounds(fBounds);
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
  forceRefresh: Boolean;
begin
  Result := Image;
  if IsEmptyRect(Bounds) then Exit;

  forceRefresh := (fLastUpdateType = utUndefined) or
    (hideDesigners <> (fLastUpdateType = utHideDesigners));

  with Root do
  begin
    //get 'old bounds' that will need erasing
    if forceRefresh then
    begin
      updateRect := Self.Bounds;
      //PreMergeAll resizes all groups
      PreMergeAll(hideDesigners);
    end else
    begin
      updateRect := fInvalidRect;
      //PreMerge resizes (and clears) invalidated groups
      PreMerge(hideDesigners);
      //and include 'new bounds' that need redrawing
      updateRect := Img32.Vector.UnionRect(updateRect, fInvalidRect);
      Types.IntersectRect(updateRect, updateRect, Self.Bounds);
    end;

    fInvalidRect := NullRect;
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
  fRoot.fInvalidRect := fBounds;
  fLastUpdateType := utUndefined;
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
    not assigned(layer.fParent) then Exit;
  layer.fParent.DeleteChild(layer.Index);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(layerIndex: integer;
  parent: TLayer32 = nil);
begin
  if not Assigned(fRoot) then Exit;
  if not assigned(parent) then parent := Root;
  if (layerIndex < 0) or (layerIndex >= parent.ChildCount) then
    raise Exception.Create(rsChildIndexRangeError);
  parent.DeleteChild(layerIndex);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPoint; ignoreDesigners: Boolean): TLayer32;
begin
  if not Assigned(fRoot) then
    result := nil else
    result := Root.GetLayerAt(pt, ignoreDesigners);
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
  fRoot.SetBounds(Rect(0,0,Width, Height));
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
  pt: TPoint;
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
  rec := RectD(targetLayer.Bounds);
  pt := targetLayer.GetAbsoluteOrigin;
  OffsetRect(rec, pt.X, pt.Y);

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
  pt: TPoint;
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
    pt := Parent.TopLeft;
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
  radius: integer;
begin
  if not assigned(targetLayer) or
    not (targetLayer is TRotateLayer32) then
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


  if TRotateLayer32(targetLayer).AutoPivot then
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
  pivot := PointD(Img32.Vector.MidPoint(targetLayer.Bounds));
  Result := CreateRotatingButtonGroup(targetLayer, pivot, buttonSize,
    pivotButtonColor, angleButtonColor, initialAngle, angleOffset,
    buttonLayerClass);
end;
//------------------------------------------------------------------------------

function UpdateRotatingButtonGroup(rotateButton: TLayer32): double;
var
  rec: TRect;
  mp, pt2: TPointD;
  i, radius: integer;
  designer: TDesignerLayer32;
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
    radius := Round(Distance(mp, pt2));
    rec := Rect(RectD(mp.X -radius, mp.Y -radius, mp.X +radius,mp.Y +radius));
    designer := DesignLayer;
    designer.SetBounds(rec);
    i :=  dpiAware1 *2;
    DrawDashedLine(designer.Image, Ellipse(Rect(i,i,radius*2 -i, radius*2 -i)),
      dashes, nil, i, clRed32, esPolygon);
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

end.
