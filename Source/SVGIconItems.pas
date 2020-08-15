unit SVGIconItems;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.UITypes,
  System.Classes,
  System.Messaging,
  SVG,
  SVGColor;

type
  TSVGIconItems = class;

  TSVGItemsUpdateMessage = class(System.Messaging.TMessage)
  end;

  TSVGIconItem = class(TCollectionItem)
  private
    FIconName: string;
    FSVG: TSVG;
    FFixedColor: TColor;
    FGrayScale: Boolean;
    procedure SetIconName(const Value: string);
    procedure SetSVG(const Value: TSVG);
    procedure SetSVGText(const Value: string);
    function GetSVGText: string;
    procedure SetFixedColor(const Value: TColor);
    procedure SetGrayScale(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property SVG: TSVG read FSVG write SetSVG;
  published
    property IconName: string read FIconName write SetIconName;
    property SVGText: string read GetSVGText write SetSVGText;
    property FixedColor: TColor read FFixedColor write SetFixedColor default TColors.SysDefault;
    property GrayScale: Boolean read FGrayScale write SetGrayScale default False;
  end;

  {TSVGIconItems}
  TSVGIconItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSVGIconItem;
    procedure SetItem(Index: Integer; const Value: TSVGIconItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TSVGIconItem;
    procedure Assign(Source: TPersistent); override;
    function GetIconByName(const AIconName: string): TSVGIconItem;
    function LoadFromFiles(const AFileNames: TStrings; const AAppend: Boolean = True): Integer;
    property Items[Index: Integer]: TSVGIconItem read GetItem write SetItem; default;
  end;

implementation

uses
  System.SysUtils,
  SVGIconImageList,
  SVGIconImageCollection;

resourcestring
  ERROR_LOADING_FILES = 'SVG error loading files:';

{ TSVGIconItem }

procedure TSVGIconItem.Assign(Source: TPersistent);
begin
  if Source is TSVGIconItem then
  begin
    FIconName := TSVGIconItem(Source).FIconName;
    FFixedColor := TSVGIconItem(Source).FFixedColor;
    FGrayScale := TSVGIconItem(Source).FGrayScale;
    FSVG.LoadFromText(TSVGIconItem(Source).FSVG.Source);
  end else
    inherited;
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

procedure TSVGIconItem.SetFixedColor(const Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    if FFixedColor <> TColors.SysDefault then
      FGrayScale := False;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetGrayScale(const Value: Boolean);
begin
  if FGrayScale <> Value then
  begin
    FGrayScale := Value;
    if FGrayScale then
      FixedColor := TColors.SysDefault;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetIconName(const Value: string);
begin
  if FIconName <> Value then
  begin
    FIconName := Value;
    Changed(False);
  end;
end;

procedure TSVGIconItem.SetSVG(const Value: TSVG);
begin
  if not Assigned(Value) then
  begin
    FSVG.Clear;
    Changed(False);
  end
  else
  begin
    if FSVG.Source <> Value.Source then
    begin
      FSVG.LoadFromText(Value.Source);
      Changed(False);
    end;
  end;
end;

procedure TSVGIconItem.SetSVGText(const Value: string);
begin
  if FSVG.Source <> Value then
  begin
    FSVG.LoadFromText(Value);
    Changed(False);
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
  if (Source is TSVGIconItems) and (Owner <> nil) then
  begin
    BeginUpdate;
    try
      Clear;
      for C := 0 to TSVGIconItems(Source).Count - 1 do
      begin
        Item := Add;
        Item.Assign(TSVGIconItems(Source)[C]);
      end;
    finally
      EndUpdate;
    end;
  end else
    inherited;
end;

constructor TSVGIconItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TSVGIconItem);
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

function TSVGIconItems.LoadFromFiles(const AFileNames: TStrings;
  const AAppend: Boolean): Integer;
Var
  LIndex: Integer;
  LSVG: TSVG;
  LFileName: string;
  LItem: TSVGIconItem;
  LErrors: string;
begin
  Result := 0;
  BeginUpdate;
  try
    LErrors := '';
    LSVG := TSVG.Create;
    try
      if not AAppend then
        Clear;
      for LIndex := 0 to AFileNames.Count - 1 do
      begin
        LFileName := AFileNames[LIndex];
        try
          LSVG.LoadFromFile(LFileName);
          LItem := Add;
          LItem.IconName := ChangeFileExt(ExtractFileName(LFileName), '');
          LItem.SVG := LSVG;
          Inc(Result);
        except
          on E: Exception do
            LErrors := LErrors + Format('%s (%s)',[E.Message, LFileName]) + sLineBreak;
        end;
      end;
      if LErrors <> '' then
        raise Exception.Create(ERROR_LOADING_FILES+sLineBreak+LErrors);
    finally
      LSVG.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSVGIconItems.SetItem(Index: Integer;
  const Value: TSVGIconItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TSVGIconItems.Update(Item: TCollectionItem);
begin
  inherited;
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self,
    TSVGItemsUpdateMessage.Create);
  end;

end.
