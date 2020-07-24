unit SVGIconImageCollection;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  SVGIconItems;

//alias to make usage simpler (less uses clause entries).
type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;


type
  TSVGIconImageCollection = class;


  TSVGStopDrawingMessage = class(System.Messaging.TMessage)
  private
    FState : boolean;
    FCollection : TSVGIconImageCollection;
  public
    constructor Create(const collection : TSVGIconImageCollection; const state : boolean);
    property Collection : TSVGIconImageCollection read FCollection;
    property State : boolean read FState;
  end;

  TSVGRecreateBitmapsMessage = class(System.Messaging.TMessage)
    FCollection : TSVGIconImageCollection;
  public
    constructor Create(const collection : TSVGIconImageCollection);
    property Collection : TSVGIconImageCollection read FCollection;
  end;

  TSVGIconImageCollection = class(TComponent, ISVGNotifyOwner)
  private
    FSVGItems: TSVGIconItems;

  protected
    //todo - these 2 should not be public, hide behind interface!
    procedure RecreateBitmaps;
    procedure StopDrawing(const value : boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items : TSVGIconItems read FSVGItems;

  end;

implementation

uses
  System.SysUtils;


{ TSVGIconImageCollection }

constructor TSVGIconImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
end;

destructor TSVGIconImageCollection.Destroy;
begin
  FSVGItems.Free;
  inherited;
end;

procedure TSVGIconImageCollection.RecreateBitmaps;
begin
  //Notify TSVGIconVirtualImageList to recreate bitmaps due to change.
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self, TSVGRecreateBitmapsMessage.Create(Self));

end;

procedure TSVGIconImageCollection.StopDrawing(const value: boolean);
begin
  //Notify TSVGIconVirtualImageList to stop/start painting
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self,TSVGStopDrawingMessage.Create(Self, value));
end;

{ TSVGDrawStopMessage }

constructor TSVGStopDrawingMessage.Create(const collection : TSVGIconImageCollection; const state: boolean);
begin
  FCollection := collection;
  FState := state;
end;

{ TSVGRecreateBitmapsMessage }

constructor TSVGRecreateBitmapsMessage.Create(const collection: TSVGIconImageCollection);
begin
  FCollection := collection;
end;

end.
