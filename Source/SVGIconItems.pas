unit SVGIconItems;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  SVG,
  SVGColor  ;

type
  ISVGNotifyOwner = interface
  ['{15CE1D87-5351-44FA-9350-B5F1009161EA}']
    procedure RecreateBitmaps;
    procedure StopDrawing(const value : boolean);
  end;

  TSVGIconItems = class;

  TSVGIconItem = class(TCollectionItem)
  private
    FIconName: string;
    FSVG: TSVG;
    FFixedColor: TSVGColor;
    FGrayScale: Boolean;
    procedure SetIconName(const Value: string);
    procedure SetSVG(const Value: TSVG);
    procedure SetSVGText(const Value: string);
    function GetSVGText: string;
    procedure SetFixedColor(const Value: TSVGColor);
    procedure SetGrayScale(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property SVG: TSVG read FSVG write SetSVG;
  published
    property IconName: string read FIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
    property FixedColor: TSVGColor read FFixedColor write SetFixedColor default TSVGColor.inherit_color;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

  {TSVGIconItems}
  TSVGIconItems = class(TOwnedCollection)
  strict private
    FOwner: TComponent;
    function GetItem(Index: Integer): TSVGIconItem;
    procedure SetItem(Index: Integer; const Value: TSVGIconItem);
  private
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function GetOwner: TPersistent; override;
    procedure StopDrawing(const value : boolean);
    procedure RecreateBitmaps;
  public
    constructor Create(AOwner: TComponent);
    procedure AssignTo(Dest: TPersistent); override;
    function Add: TSVGIconItem;
    procedure Assign(Source: TPersistent); override;
    function GetIconByName(const AIconName: string): TSVGIconItem;
    property Items[index: Integer]: TSVGIconItem read GetItem write SetItem; default;
  end;


implementation

uses
  System.SysUtils,
  SVGIconImageList,
  SVGIconImageCollection;

{ TSVGIconItem }

procedure TSVGIconItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSVGIconItem then
  begin
    FIconName := TSVGIconItem(Source).FIconName;
    FFixedColor := TSVGIconItem(Source).FFixedColor;
    FGrayScale := TSVGIconItem(Source).FGrayScale;
    FSVG.LoadFromText(TSVGIconItem(Source).FSVG.Source);
  end;
end;

procedure TSVGIconItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSVGIconItem then
  begin
    TSVGIconItem(Dest).FIconName := FIconName;
    TSVGIconItem(Dest).FSVG.LoadFromText(FSVG.Source);
  end;
end;

constructor TSVGIconItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSVG := TSVG.Create;
end;

destructor TSVGIconItem.Destroy;
begin
  FreeAndNil(FSVG);
  inherited;
end;

function TSVGIconItem.GetDisplayName: string;
begin
  if IconName <> '' then
    Result := Format('%s',[IconName])
  else
    Result := 'SVGIconItem';
end;

function TSVGIconItem.GetSVGText: string;
begin
  Result := SVG.Source;
end;

procedure TSVGIconItem.SetFixedColor(const Value: TSVGColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> inherit_color then
      FGrayScale := False;
    TSVGIconItems(Collection).Update(Self);
  end;
end;

procedure TSVGIconItem.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := inherit_color;
    TSVGIconItems(Collection).Update(Self);
  end;
end;

procedure TSVGIconItem.SetIconName(const Value: string);
begin
  if FIconName <> Value then
  begin
    FIconName := Value;
    TSVGIconItems(Collection).Update(Self);
  end;
end;

procedure TSVGIconItem.SetSVG(const Value: TSVG);
begin
  if not Assigned(Value) then
  begin
    FSVG.Clear;
    TSVGIconItems(Collection).Update(Self);
  end
  else
  begin
    if FSVG.Source <> Value.Source then
    begin
      FSVG.LoadFromText(Value.Source);
      TSVGIconItems(Collection).Update(Self);
    end;
  end;
end;

procedure TSVGIconItem.SetSVGText(const Value: string);
begin
  if FSVG.Source <> Value then
  begin
    FSVG.LoadFromText(Value);
    TSVGIconItems(Collection).Update(Self);
  end;
end;

{ TSVGIconItems }

function TSVGIconItems.Add: TSVGIconItem;
begin
  Result := TSVGIconItem(inherited Add);
end;

procedure TSVGIconItems.Assign(Source: TPersistent);
var
  C: Integer;
  Item: TSVGIconItem;
begin
  inherited;
  if (Source is TSVGIconItems) and (FOwner <> nil) then
  begin
    StopDrawing(True);
    try
      BeginUpdate;
      Clear;
      for C := 0 to TSVGIconItems(Source).Count - 1 do
      begin
        Item := Add;
        Item.Assign(TSVGIconItems(Source)[C]);
      end;
    finally
      EndUpdate;
      Update(nil);
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;
end;

procedure TSVGIconItems.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

constructor TSVGIconItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TSVGIconItem);
  FOwner := AOwner;
end;

function TSVGIconItems.GetIconByName(const AIconName: string): TSVGIconItem;
var
  I: Integer;
  LSVGIconItem: TSVGIconItem;
begin
  Result := nil;
  for I := 0 to Count -1 do
  begin
    LSVGIconItem := Items[I];
    if SameText(LSVGIconItem.IconName, AIconName) then
    begin
      Result := LSVGIconItem;
      Break;
    end;
  end;
end;


function TSVGIconItems.GetItem(
  Index: Integer): TSVGIconItem;
begin
  Result := TSVGIconItem(inherited GetItem(Index));
end;

function TSVGIconItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSVGIconItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  //this was commented out before so leaving it here.
//  if FOwner is TSVGIconImageList then
//    TSVGIconImageList(FOwner).RecreateBitmaps;
end;

procedure TSVGIconItems.RecreateBitmaps;
begin
  if FOwner <> nil then
     (FOwner as ISVGNotifyOwner).RecreateBitmaps;
end;

procedure TSVGIconItems.StopDrawing(const value : boolean);
begin
  if FOwner <> nil then
     (FOwner as ISVGNotifyOwner).StopDrawing(value);
end;

procedure TSVGIconItems.SetItem(Index: Integer;
  const Value: TSVGIconItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSVGIconItems.Update(Item: TCollectionItem);
begin
  inherited;
  if FOwner <> nil then
    RecreateBitmaps;
end;



end.
