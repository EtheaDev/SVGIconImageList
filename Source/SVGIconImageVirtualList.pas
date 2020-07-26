unit SVGIconImageVirtualList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  SVGColor,
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  TSVGIconVirtualImageList = class(TSVGIconImageListBase)
  private
    FCollection : TSVGIconImageCollection;
    FStopDrawingMessageID : Integer;
    FRecreateBitmapsMessageID : integer;
  protected
    procedure SetCollection(const value: TSVGIconImageCollection);

  {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  {$ENDIF}
    procedure RecreateBitmapsMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure StopDrawingMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);

    procedure RecreateBitmaps;override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure DoAssign(const source : TPersistent);override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property Opacity;
    property Width;
    property Height;
    property Size;
    property FixedColor;
    property GrayScale;
    property DisabledGrayScale;
    property DisabledOpacity;

    property Collection : TSVGIconImageCollection read FCollection write SetCollection;

    {$IFDEF HiDPISupport}
    property Scaled;
    {$ENDIF}
  end;


implementation

uses
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ImgList;

{ TSVGIconVirtualImageList }


constructor TSVGIconVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
  FStopDrawingMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TSVGStopDrawingMessage, StopDrawingMessageHandler) ;
  FRecreateBitmapsMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TSVGRecreateBitmapsMessage, RecreateBitmapsMessageHandler);

end;

procedure TSVGIconVirtualImageList.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Info: Longint;
begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then
    Info := Ancestor.DesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft, LongRec(DesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop, LongRec(DesignInfo).Hi <> LongRec(Info).Hi);

end;

destructor TSVGIconVirtualImageList.Destroy;
begin
  FCollection := nil;
{$IFDEF HiDPISupport}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
{$ENDIF}
  TMessageManager.DefaultManager.Unsubscribe(TSVGStopDrawingMessage, FStopDrawingMessageID);
  TMessageManager.DefaultManager.Unsubscribe(TSVGRecreateBitmapsMessage, FRecreateBitmapsMessageID);
  inherited;
end;

procedure TSVGIconVirtualImageList.DoAssign(const source: TPersistent);
begin
  inherited;
  if Source is TSVGIconVirtualImageList then
    FCollection := TSVGIconVirtualImageList(Source).FCollection;

end;


procedure TSVGIconVirtualImageList.RecreateBitmaps;
begin

end;

procedure TSVGIconVirtualImageList.RecreateBitmapsMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  if TSVGRecreateBitmapsMessage(Msg).Collection = FCollection then
    RecreateBitmaps;
end;

{$IFDEF HiDPISupport}
procedure TSVGIconVirtualImageList.DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
var
  W, H: Integer;
begin
  if FScaled and (TChangeScaleMessage(Msg).Sender = Owner) then
  begin
    W := MulDiv(Width, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    H := MulDiv(Height, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    FScaling := True;
    try
      SetSize(W, H);
    finally
      FScaling := False;
    end;
  end;
end;
{$ENDIF}

procedure TSVGIconVirtualImageList.SetCollection(const value: TSVGIconImageCollection);
begin
  if FCollection <> Value then
  begin
    FCollection := Value;
    if not (csLoading in ComponentState) then
      RecreateBitmaps;
  end;
end;


procedure TSVGIconVirtualImageList.StopDrawingMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  if TSVGStopDrawingMessage(Msg).Collection = FCollection then
    StopDrawing(TSVGStopDrawingMessage(Msg).State);
end;


end.
