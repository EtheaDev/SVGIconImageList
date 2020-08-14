unit SVGIconVirtualImageList;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Classes,
  System.Messaging,
  Vcl.Controls,
  Vcl.Graphics,
  SVG,
  SVGColor,
  SVGIconImageListBase,
  SVGIconImageCollection;

type
  TSVGIconVirtualImageList = class(TSVGIconImageListBase)
  private
    FCollection : TSVGIconImageCollection;
  protected
    // override abstract methods
    function GetSVGIconItems: TSVGIconItems; override;
    procedure RecreateBitmaps; override;
    procedure SetCollection(const value: TSVGIconImageCollection);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoAssign(const source : TPersistent);override;
    function GetCount: Integer;override;

  public
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean = True); override;

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
  WinApi.Windows,
  Winapi.CommCtrl,
  Winapi.GDIPAPI,
  Vcl.Forms,
  Vcl.ImgList,
  SVGTypes,
  SVGCommon,
  SVGIconImageList;

{ TSVGIconVirtualImageList }

procedure TSVGIconVirtualImageList.DoAssign(const source: TPersistent);
begin
  inherited;
  if Source is TSVGIconImageList then
  begin
    if FCollection <> nil then
    begin
      FCollection.SVGIconItems.Assign(TSVGIconImageList(Source).SVGIconItems);
      FCollection.StoreAsText := TSVGIconImageList(Source).StoreAsText;
    end;
  end
  else if Source is TSVGIconVirtualImageList then
    SetCollection(TSVGIconVirtualImageList(Source).FCollection);
end;

function TSVGIconVirtualImageList.GetCount: Integer;
begin
  if FCollection <> nil then
    result := FCollection.SVGIconItems.Count
  else
    result := 0;
end;

function TSVGIconVirtualImageList.GetSVGIconItems: TSVGIconItems;
begin
  if Assigned(FCollection) then
    Result := FCollection.SVGIconItems
  else
    Result := nil;
end;

procedure TSVGIconVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCollection) then
  begin
    BeginUpdate;
    try
      FCollection := nil;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.PaintTo(const ACanvas: TCanvas; const AIndex: Integer; const X, Y, AWidth, AHeight: Single; AEnabled: Boolean);
var
  R: TGPRectF;
  SVG: TSVG;
  LItem: TSVGIconItem;
  LOpacity: Byte;
begin
  if (FCollection <> nil) and (AIndex >= 0) and (AIndex < FCollection.SVGIconItems.Count) then
  begin
    LItem := FCollection.SVGIconItems[AIndex];
    SVG := LItem.SVG;
    if LItem.FixedColor <> inherit_color then
      SVG.FixedColor := LItem.FixedColor
    else
      SVG.FixedColor := FFixedColor;
    LOpacity := FOpacity;
    if AEnabled then
    begin
      if LItem.GrayScale or FGrayScale then
        SVG.Grayscale := True
      else
        SVG.Grayscale := False;
    end
    else
    begin
      if FDisabledGrayScale then
        SVG.Grayscale := True
      else
        LOpacity := FDisabledOpacity;
    end;
    SVG.SVGOpacity := LOpacity / 255;
    R := FittedRect(MakeRect(X, Y, AWidth, AHeight), SVG.Width, SVG.Height);
    SVG.PaintTo(ACanvas.Handle, R, nil, 0);
    SVG.SVGOpacity := 1;
  end;
end;

procedure TSVGIconVirtualImageList.RecreateBitmaps;
var
  C: Integer;
  SVG: TSVG;
  Icon: HIcon;
  LItem: TSVGIconItem;
begin
  if not Assigned(FCollection) or
    ([csLoading, csDestroying, csUpdating] * ComponentState <> [])
  then
    Exit;

  ImageList_Remove(Handle, -1);
  if (Width > 0) and (Height > 0) then
  begin
    HandleNeeded;
    for C := 0 to FCollection.SVGIconItems.Count - 1 do
    begin
      LItem := FCollection.SVGIconItems[C];
      SVG := LItem.SVG;
      if Assigned(SVG) then
      begin
        if LItem.FixedColor <> inherit_color then
          SVG.FixedColor := LItem.FixedColor
        else
          SVG.FixedColor := FixedColor;
        if LItem.GrayScale or GrayScale then
          SVG.Grayscale := True
        else
          SVG.Grayscale := False;
        Icon := SVGToIcon(SVG);
        ImageList_AddIcon(Handle, Icon);
        DestroyIcon(Icon);
      end;
    end;
  end;
end;

procedure TSVGIconVirtualImageList.SetCollection(const value: TSVGIconImageCollection);
begin
  if FCollection <> Value then
  begin
    if FCollection <> nil then
      FCollection.RemoveFreeNotification(Self);
    FCollection := Value;
    if FCollection <> nil then
      FCollection.FreeNotification(Self);
    Change;
  end;
end;

end.
