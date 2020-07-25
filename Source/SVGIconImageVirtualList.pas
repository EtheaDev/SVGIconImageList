unit SVGIconImageVirtualList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  SVGColor,
  SVGIconImageList,
  SVGIconImageCollection;

type
  TSVGIconVirtualImageList = class(TDragImageList)
  private
    FCollection : TSVGIconImageCollection;
    FStopDrawing: Integer;
    FOpacity: Byte;
    FFixedColor: TSVGColor;
    FGrayScale: Boolean;
    FDisabledGrayScale: Boolean;
    FDisabledOpacity: Byte;

    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    FStopDrawingMessageID : Integer;
    FRecreateBitmapsMessageID : integer;

  protected
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOpacity(const Value: Byte);
    function GetSize: Integer;

    procedure SetCollection(const value: TSVGIconImageCollection);
    procedure SetSize(const Value: Integer);
    procedure SetFixedColor(const Value: TSVGColor);
    procedure SetGrayScale(const Value: Boolean);
    procedure SetDisabledGrayScale(const Value: Boolean);
    procedure SetDisabledOpacity(const Value: Byte);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);



  {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  {$ENDIF}
    procedure RecreateBitmapsMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure StopDrawingMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);

    procedure StopDrawing(const AStop: Boolean);
    procedure RecreateBitmaps;
    procedure Change; override;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property FixedColor: TSVGColor read FFixedColor write SetFixedColor default TSVGColor.inherit_color;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
    property DisabledGrayScale: Boolean read FDisabledGrayScale write SetDisabledGrayScale default True;
    property DisabledOpacity: Byte read FDisabledOpacity write SetDisabledOpacity default 125;


    property Collection : TSVGIconImageCollection read FCollection write SetCollection;

    {$IFDEF HiDPISupport}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
  end;


implementation

uses
  System.Math,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ImgList;

{ TSVGIconVirtualImageList }

procedure TSVGIconVirtualImageList.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconVirtualImageList then
  begin
    StopDrawing(True);
    try
      Width := TSVGIconVirtualImageList(Source).Width;
      Height := TSVGIconVirtualImageList(Source).Height;
      FOpacity := TSVGIconVirtualImageList(Source).FOpacity;
      FStoreAsText := TSVGIconVirtualImageList(Source).FStoreAsText;
      FFixedColor := TSVGIconVirtualImageList(Source).FFixedColor;
      FGrayScale := TSVGIconVirtualImageList(Source).FGrayScale;
      FCollection := TSVGIconVirtualImageList(Source).FCollection;
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;

end;

procedure TSVGIconVirtualImageList.Change;
begin
  //Optimization: Do not notify to components during redrawing of icons
  if FStopDrawing = 0 then
    inherited;
end;

constructor TSVGIconVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
  ColorDepth := cd32Bit;
  Width := DEFAULT_SIZE;
  Height := DEFAULT_SIZE;
  FOpacity := 255;
  FFixedColor := inherit_color;
  FGrayScale := False;
  {$IFDEF HiDPISupport}
  FScaled := True;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF}
  FDisabledGrayScale := True;
  FDisabledOpacity := 125;

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

function TSVGIconVirtualImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TSVGIconVirtualImageList.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TSVGIconVirtualImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TSVGIconVirtualImageList.Loaded;
begin
  inherited;
  RecreateBitmaps;
end;

procedure TSVGIconVirtualImageList.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconVirtualImageList.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
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

procedure TSVGIconVirtualImageList.SetDisabledGrayScale(const Value: Boolean);
begin
  if FDisabledGrayScale <> Value then
  begin
    FDisabledGrayScale := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetDisabledOpacity(const Value: Byte);
begin
  if FDisabledOpacity <> Value then
  begin
    FDisabledOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetFixedColor(const Value: TSVGColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> inherit_color then
      FGrayScale := False;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := inherit_color;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetHeight(const Value: Integer);
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.SetSize(const Value: Integer);
begin
  if (Height <> Value) or (Width <> Value) then
  begin
    StopDrawing(True);
    try
      Width := Value;
      Height := Value;
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;

end;

procedure TSVGIconVirtualImageList.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconVirtualImageList.StopDrawing(const AStop: Boolean);
begin
  if AStop then
    Inc(FStopDrawing)
  else
    Dec(FStopDrawing);
end;

procedure TSVGIconVirtualImageList.StopDrawingMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  if TSVGStopDrawingMessage(Msg).Collection = FCollection then
    StopDrawing(TSVGStopDrawingMessage(Msg).State);
end;

function TSVGIconVirtualImageList.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TSVGIconVirtualImageList.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TSVGIconVirtualImageList.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
end;

procedure TSVGIconVirtualImageList.WriteLeft(Writer: TWriter);
begin

end;

procedure TSVGIconVirtualImageList.WriteTop(Writer: TWriter);
begin

end;

end.
