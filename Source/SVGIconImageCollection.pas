unit SVGIconImageCollection;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  SVG,
  SVGColor,
  SVGIconItems;

//alias to make usage simpler (less uses clause entries).
type
  TSVGIconItem = SVGIconItems.TSVGIconItem;
  TSVGIconItems = SVGIconItems.TSVGIconItems;

type
  TSVGIconImageCollection = class;


  TSVGCollectionMessage = class(System.Messaging.TMessage)
    FCollection : TSVGIconImageCollection;
  public
    constructor Create(const collection : TSVGIconImageCollection);
    property Collection : TSVGIconImageCollection read FCollection;
  end;

  TSVGStopDrawingMessage = class(TSVGCollectionMessage)
  private
    FState : boolean;
  public
    constructor Create(const collection : TSVGIconImageCollection; const state : boolean);
    property State : boolean read FState;
  end;

  TSVGRecreateBitmapsMessage = class(TSVGCollectionMessage);

  TSVGCollectionDestroyedMessage = class(TSVGCollectionMessage);


  TSVGIconImageCollection = class(TComponent, ISVGNotifyOwner)
  private
    FSVGItems: TSVGIconItems;
    FStoreAsText : boolean;
    procedure SetSVGIconItems(const Value: TSVGIconItems);

  protected
    //todo - these 2 should not be public, hide behind interface!
    procedure RecreateBitmaps;
    procedure StopDrawing(const value : boolean);


    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    procedure ReadImageData(Stream: TStream);
    procedure WriteImageData(Stream: TStream);

    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(const ASVG: TSVG; const AIconName: string;
       const AGrayScale: Boolean = False;
       const AFixedColor: TSVGColor = inherit_color): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Name: string);
    function IndexOf(const Name: string): Integer;
    procedure ClearIcons;


    procedure LoadFromResource(const hInstance : THandle; const resourceName : string; const iconName : string);

  published
    property SVGIconItems: TSVGIconItems read FSVGItems write SetSVGIconItems stored FStoreAsText;

    property StoreAsText: boolean read FStoreAsText write FStoreAsText default False;

  end;

implementation

uses
  System.Types,
  System.SysUtils;


{ TSVGIconImageCollection }

function TSVGIconImageCollection.Add(const ASVG: TSVG; const AIconName: string; const AGrayScale: Boolean; const AFixedColor: TSVGColor): Integer;
var
  Item: TSVGIconItem;
begin
  try
    Item := FSVGItems.Add;
    Item.SVG := ASVG;
    Item.IconName := AIconName;
    Item.FixedColor := AFixedColor;
    Item.GrayScale := AGrayScale;
  finally
    RecreateBitmaps;
  end;
  Result := FSVGItems.Count - 1;
end;

procedure TSVGIconImageCollection.ClearIcons;
begin
  StopDrawing(True);
  try
    FSVGItems.Clear;
  finally
    StopDrawing(False);
    RecreateBitmaps;
  end;
end;

constructor TSVGIconImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FSVGItems := TSVGIconItems.Create(Self);
  FStoreAsText := false;
end;

procedure TSVGIconImageCollection.DefineProperties(Filer: TFiler);
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

  Filer.DefineBinaryProperty('Images', ReadImageData, WriteImageData, True);

end;

procedure TSVGIconImageCollection.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FSVGItems.Count) then
    FSVGItems.Delete(Index);
end;

destructor TSVGIconImageCollection.Destroy;
begin
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self,TSVGCollectionDestroyedMessage.Create(Self));
  FSVGItems.Free;
  inherited;
end;

function TSVGIconImageCollection.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to FSVGItems.Count - 1 do
    if FSVGItems[Result].IconName = Name then
      Exit;
  Result := -1;
end;

procedure TSVGIconImageCollection.LoadFromResource(const hInstance: THandle; const resourceName, iconName: string);
var
  resStream: TResourceStream;
  svg : TSVG;
begin
  resStream := TResourceStream.Create(hInstance, resourceName, RT_RCDATA);
  try
    svg := TSVG.Create;
    try
      svg.LoadFromStream(resStream);
      Add(svg, iconName);
    except
      svg.Free;
      raise;
    end;
  finally
    resStream.Free;
  end;
end;

procedure TSVGIconImageCollection.ReadImageData(Stream: TStream);
var
  LStream: TMemoryStream;
  LCount, LSize: Integer;
  LSVG: TSVG;
  C: Integer;
  LPos: Int64;
  LIconName: string;
  LTag: TBytes;
  LFixedColorStr: AnsiString;
  LGrayScale: Boolean;
  LFixedColor: TSVGColor;
begin
  if FStoreAsText then
    Exit;
  try
    StopDrawing(True);
    LStream := TMemoryStream.Create;
    //Read Count of Images
    Stream.Read(LCount, SizeOf(Integer));
    LSVG := TSVG.Create(nil);
    for C := 0 to LCount - 1 do
    begin
      //Read IconName
      Stream.Read(LSize, SizeOf(Integer));
      SetLength(LIconName, LSize);
      Stream.Read(PChar(LIconName)^, LSize * SizeOf(Char));
      //Read SVG Stream Size
      Stream.Read(LSize, SizeOf(Integer));
      LStream.CopyFrom(Stream, LSize);
      //Read SVG Stream data
      LSVG.LoadFromStream(LStream);

      //Check for FixedColor attribute
      LPos := Stream.Position;
      LFixedColor := SVGColor.inherit_color;
      SetLength(LTag, 10);
      Stream.Read(Pointer(LTag)^, 10);
      SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 10);
      if LFixedColorStr = 'FixedColor' then
        //Read Fixed Color value
        Stream.Read(LFixedColor, SizeOf(Integer))
      else
        Stream.Position := LPos;

      //Check for GrayScale attribute
      LPos := Stream.Position;
      LGrayScale := False;
      SetLength(LTag, 9);
      Stream.Read(Pointer(LTag)^, 9);
      SetString(LFixedColorStr, PAnsiChar(@LTag[0]), 9);
      if LFixedColorStr = 'GrayScale' then
        LGrayScale := True
      else
        Stream.Position := LPos;

      Add(LSVG, LIconName, LGrayScale, LFixedColor);
      LStream.Clear;
    end;
    LStream.Free;
    LSVG.Free;
  finally
    StopDrawing(False);
    RecreateBitmaps;
  end;
end;

procedure TSVGIconImageCollection.ReadLeft(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.ReadTop(Reader: TReader);
var
  FDesignInfo: LongInt;
begin
  FDesignInfo := DesignInfo;
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
  DesignInfo := FDesignInfo;
end;

procedure TSVGIconImageCollection.RecreateBitmaps;
begin
  //Notify TSVGIconVirtualImageList to recreate bitmaps due to change.
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self, TSVGRecreateBitmapsMessage.Create(Self));

end;

procedure TSVGIconImageCollection.Remove(const Name: string);
begin
  Delete(IndexOf(Name));
end;

procedure TSVGIconImageCollection.SetSVGIconItems(const Value: TSVGIconItems);
begin
  //shouldn't this use assign?
  //FSVGItems := Value;

  if FSVGItems <> Value then
  begin
    FSVGItems.Assign(Value);
  end;
end;

procedure TSVGIconImageCollection.StopDrawing(const value: boolean);
begin
  //Notify TSVGIconVirtualImageList to stop/start painting
  System.Messaging.TMessageManager.DefaultManager.SendMessage(Self,TSVGStopDrawingMessage.Create(Self, value));
end;

procedure TSVGIconImageCollection.WriteImageData(Stream: TStream);
var
  Count, Size: Integer;
  SVG: TSVG;
  LIconName: string;
  LTag: AnsiString;
  LItem: TSVGIconItem;
  C: Integer;
  SVGStream: TMemoryStream;
begin
  if FStoreAsText then
    Exit;
  Count := FSVGItems.Count;
  //Store count
  Stream.Write(Count, SizeOf(Integer));

  SVGStream := TMemoryStream.Create;
  for C := 0 to Count - 1 do
  begin
    LItem := FSVGItems[C];
    //Store IconName
    LIconName := LItem.IconName;
    SVG := FSVGItems[C].SVG;
    Size := Length(LIconName);
    Stream.Write(Size, SizeOf(Integer));
    Stream.WriteBuffer(PChar(LIconName)^, Size * SizeOf(Char));
    //Store SVG Stream Size
    SVG.SaveToStream(SVGStream);
    Size := SVGStream.Size;
    Stream.Write(Size, SizeOf(Integer));
    SVGStream.Position := 0;
    //Store SVG Data
    Stream.CopyFrom(SVGStream, Size);
    //Store FixedColor (optionally)
    if LItem.FixedColor <> TSVGColor.inherit_color then
    begin
      LTag := 'FixedColor';
      Stream.WriteBuffer(PAnsiChar(LTag)^, 10);
      Size := Ord(LItem.FixedColor);
      Stream.Write(Size, SizeOf(Integer));
    end;
    //Store GrayScale (optionally)
    if LItem.GrayScale then
    begin
      LTag := 'GrayScale';
      Stream.WriteBuffer(PAnsiChar(LTag)^, 9);
    end;
    SVGStream.Clear;
  end;
  SVGStream.Free;
end;

procedure TSVGIconImageCollection.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TSVGIconImageCollection.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

{ TSVGDrawStopMessage }

constructor TSVGStopDrawingMessage.Create(const collection : TSVGIconImageCollection; const state: boolean);
begin
  inherited Create(collection);
  FState := state;
end;


{ TSVGCollectionMessage }

constructor TSVGCollectionMessage.Create(const collection: TSVGIconImageCollection);
begin
  FCollection := collection;
end;

end.
