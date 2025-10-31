unit FrameViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  SVGInterfaces;

type
  TFrameView = class(TFrame)
    ClientPanel: TPanel;
    SVGPaintBox: TPaintBox;
    TitlePanel: TPanel;
    procedure SVGPaintBoxPaint(Sender: TObject);
  private
    FSVG: ISVG;
    FKeeAspectRatio: Boolean;
    function GetOpacity: Single;
    procedure SetOpacity(const AValue: Single);
    function GetGrayScale: Boolean;
    procedure SetGrayScale(const AValue: Boolean);
    function GetFixedColor: TColor;
    procedure SetFixedColor(const AValue: TColor);
    function GetApplyFixedColorToRootOnly: Boolean;
    procedure SetApplyFixedColorToRootOnly(const AValue: Boolean);
    procedure SetKeepAspectRatio(const AValue: Boolean);
    procedure SetFlipHorizontal(const Value: Boolean);
    procedure SetFlipVertically(const Value: Boolean);
    function GetApplyDrawFullPathsInCenter: Boolean;
    procedure SetApplyDrawFullPathsInCenter(const Value: Boolean);
    function GetFlipHorizontal: Boolean;
    function GetFlipVertically: Boolean;
  public
    procedure InitViewer(const ATitle: string; const SVGFactory : ISVGFactory);
    procedure DrawFile(const AFileName: string);
    property Opacity: Single read GetOpacity write SetOpacity;
    property GrayScale: Boolean read GetGrayScale write SetGrayScale;
    property FixedColor: TColor read GetFixedColor write SetFixedColor;
    property ApplyFixedColorToRootOnly: Boolean read GetApplyFixedColorToRootOnly write SetApplyFixedColorToRootOnly;
    property KeepAspectRatio: Boolean read FKeeAspectRatio write SetKeepAspectRatio;
    property DrawFullPathsInCenter: Boolean read GetApplyDrawFullPathsInCenter write SetApplyDrawFullPathsInCenter;
    property FlipHorizontal: Boolean read GetFlipHorizontal write SetFlipHorizontal;
    property FlipVertically: Boolean read GetFlipVertically write SetFlipVertically;
    property SVG: ISVG read FSVG;
  end;

implementation

{$R *.dfm}

uses
  System.Types,
  System.UITypes;

{ TFrameView }

procedure TFrameView.DrawFile(const AFileName: string);
begin
  try
    FSVG.LoadFromFile(AFileName);
  except
    On E: ESVGException do ;
    else raise;
  end;
  SVGPaintBox.Invalidate;
end;

function TFrameView.GetApplyFixedColorToRootOnly: Boolean;
begin
  Result := FSVG.ApplyFixedColorToRootOnly;
end;

function TFrameView.GetApplyDrawFullPathsInCenter: Boolean;
begin
  Result := FSVG.ApplyDrawFullPathsInCenter;
end;

function TFrameView.GetFixedColor: TColor;
begin
  Result := FSVG.FixedColor;
end;

function TFrameView.GetFlipHorizontal: Boolean;
begin
  Result := FSVG.FlipHorizontal;
end;

function TFrameView.GetFlipVertically: Boolean;
begin
  Result := FSVG.FlipVertically;
end;

function TFrameView.GetGrayScale: Boolean;
begin
  Result := FSVG.GrayScale;
end;

function TFrameView.GetOpacity: Single;
begin
  Result := FSVG.Opacity;
end;

procedure TFrameView.InitViewer(
  const ATitle: string; const SVGFactory: ISVGFactory);
begin
  TitlePanel.Caption := ATitle;
  SetGlobalSvgFactory(SVGFactory);
  FKeeAspectRatio := True;
  FSVG := GlobalSvgFactory.NewSvg;
  FSVG.Grayscale := False;
  FSVG.Opacity := 1;
end;

procedure TFrameView.SetApplyFixedColorToRootOnly(const AValue: Boolean);
begin
  FSVG.ApplyFixedColorToRootOnly := AValue;
  SVGPaintBox.invalidate;
end;

procedure TFrameView.SetApplyDrawFullPathsInCenter(const Value: Boolean);
begin
  FSVG.ApplyDrawFullPathsInCenter := Value;
  SVGPaintBox.invalidate;
end;

procedure TFrameView.SetFixedColor(const AValue: TColor);
begin
  FSVG.FixedColor := AValue;
  SVGPaintBox.invalidate;
end;

procedure TFrameView.SetFlipHorizontal(const Value: Boolean);
begin
  FSVG.FlipHorizontal := Value;
  SVGPaintBox.invalidate;
end;

procedure TFrameView.SetFlipVertically(const Value: Boolean);
begin
  FSVG.FlipVertically := Value;
  SVGPaintBox.invalidate;
end;

procedure TFrameView.SetGrayScale(const AValue: Boolean);
begin
  FSVG.GrayScale := AValue;
  SVGPaintBox.Invalidate;
end;

procedure TFrameView.SetKeepAspectRatio(const AValue: Boolean);
begin
  FKeeAspectRatio := AValue;
  SVGPaintBox.Invalidate;
end;

procedure TFrameView.SetOpacity(const AValue: Single);
begin
  FSVG.Opacity := AValue;
  SVGPaintBox.Invalidate;
end;

procedure TFrameView.SVGPaintBoxPaint(Sender: TObject);
begin
  FSVG.PaintTo(SVGPaintBox.Canvas.Handle,
    TRectF.Create(0, 0, SVGPaintBox.Width, SVGPaintBox.Height), FKeeAspectRatio);
end;

end.
