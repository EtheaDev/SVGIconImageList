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
    FGrayScale: Boolean;
    FSVG: ISVG;
  public
    procedure InitViewer(const ATitle: string;
      const SVGFactory : ISVGFactory);
    procedure DrawFile(const AFileName: string);
  end;

implementation

{$R *.dfm}

uses
  System.Types;

{ TFrameView }

procedure TFrameView.DrawFile(const AFileName: string);
begin
  try
    FSvg.LoadFromFile(AFileName);
  except
    On E: ESVGException do ;
    else raise;
  end;
  SVGPaintBox.Invalidate;
end;

procedure TFrameView.InitViewer(
  const ATitle: string; const SVGFactory: ISVGFactory);
begin
  TitlePanel.Caption := ATitle;
  TitlePanel.Font.Style := TitlePanel.Font.Style + [fsBold];
  SetGlobalSvgFactory(SVGFactory);
  FSVG := GlobalSvgFactory.NewSvg;
end;

procedure TFrameView.SVGPaintBoxPaint(Sender: TObject);
begin
  FSVG.Grayscale := FGrayScale;
  FSVG.PaintTo(SVGPaintBox.Canvas.Handle,
    TRectF.Create(0, 0, SVGPaintBox.Width, SVGPaintBox.Height), True);
end;

end.
