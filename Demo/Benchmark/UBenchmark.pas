unit UBenchmark;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGInterfaces,
  SVGIconImageCollection, SVGIconImage, Vcl.Samples.Spin, Vcl.ExtCtrls;

type
  TfrmBenchmark = class(TForm)
    SVGIconImageCollection: TSVGIconImageCollection;
    imlIcons: TVirtualImageList;
    memOutput: TMemo;
    btnClear: TButton;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    SVGIconImage: TSVGIconImage;
    btnRunBenchmark: TButton;
    speLoops: TSpinEdit;
    lblLoops: TLabel;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnRunBenchmarkClick(Sender: TObject);
  private
    FSvg      : string;
    FStartTick: Int64;
    FLastTick : Int64;

    procedure BenchmarkLoad;
    procedure BenchmarkGrayScale;
    procedure BenchmarkFixedColor;
    procedure BenchmarkDraw;

    procedure LogTicks(var AMessage: string; const ASuffix: string; ATick: Int64);
    procedure RunBenchmark(AFactoryName: string; AFactory: ISVGFactory);
  public
    { Public-Deklarationen }
  end;

var
  frmBenchmark: TfrmBenchmark;

implementation

uses
  CairoSVGFactory,
  D2DSVGFactory,
  PasSVGFactory,
  System.IOUtils,
  System.Types;

{$R *.dfm}


type
  TCanvasImage = class(TSVGIconImage) // Just to have something to paint on
  public
    property Canvas;
  end;

procedure TfrmBenchmark.BenchmarkDraw;
var
  I    : integer;
  LStep: real;
  LSize: real;
  LRect: TRect;
begin
  // Benchmark Draw
  LStep := SvgIconImage.Height / speLoops.Value;
  LSize := 0;
  for I := 1 to speLoops.Value do
    begin
      LSize := LSize + LStep;
      LRect := TRect.Create(0, 0, Round(LSize), Round(LSize));
      SvgIconImageCollection.Draw(TCanvasImage(SVGIconImage).Canvas, LRect, 0, true);
    end;
end;

procedure TfrmBenchmark.BenchmarkFixedColor;
begin
  SVGIconImageCollection.FixedColor := clLime;
end;

procedure TfrmBenchmark.BenchmarkGrayScale;
begin
  SVGIconImageCollection.GrayScale := true;
end;

procedure TfrmBenchmark.BenchmarkLoad;
var
  I   : integer;
  LSvg: ISvg;
begin
  SVGIconImageCollection.SVGIconItems.BeginUpdate;
  try
    SVGIconImageCollection.SVGIconItems.Clear;
    SVGIconImageCollection.FixedColor := clDefault;
    SVGIconImageCollection.GrayScale := false;

    for I := 1 to speLoops.Value do
      begin
        LSvg := GlobalSvgFactory.NewSvg;
        LSvg.Source := FSvg;
        SVGIconImageCollection.Add(LSvg, '');
      end;
  finally
    SVGIconImageCollection.SVGIconItems.EndUpdate;
  end;
end;

procedure TfrmBenchmark.btnClearClick(Sender: TObject);
begin
  memOutput.Clear;
  SVGIconImageCollection.ClearIcons;
end;

procedure TfrmBenchmark.btnLoadClick(Sender: TObject);
var
  LSvg: ISvg;
begin
  if OpenDialog.Execute then
    begin
      FSvg := TFile.ReadAllText(OpenDialog.FileName);

      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.Source := FSvg;

      SVGIconImageCollection.SVGIconItems.Clear;
      SVGIconImageCollection.Add(LSvg, '');
      SVGIconImage.ImageIndex := 0;
    end;
end;

procedure TfrmBenchmark.btnRunBenchmarkClick(Sender: TObject);
begin
  if (FSvg = '') then
    memOutput.Lines.Add('Please load a SVG image first')
  else
    begin
      SVGIconImage.ImageIndex := -1;

      memOutput.Lines.Add(Format('Repeat %d times', [speLoops.Value]));
      memOutput.Lines.Add('Factory    |  Load  |  Draw  |  Gray  |  Draw  |  Fixed |  Draw  |  Total |');
      RunBenchmark('Pascal', GetPasSVGFactory);
      RunBenchmark('Direct 2D', GetD2DSVGFactory);
      RunBenchmark('Cairo', GetCairoSVGFactory);

      SVGIconImage.ImageIndex := 0;
    end;
end;

procedure TfrmBenchmark.LogTicks(var AMessage: string; const ASuffix: string; ATick: Int64);
var
  LCurrentTick: Int64;
begin
  LCurrentTick := GetTickCount;
  AMessage := Format('%s | %6d%s', [AMessage, LCurrentTick - ATick, ASuffix]);
  memOutput.Lines[memOutput.Lines.Count - 1] := AMessage;
  FLastTick := LCurrentTick;
end;

procedure TfrmBenchmark.RunBenchmark(AFactoryName: string; AFactory: ISVGFactory);
var
  LLine: string;
begin
  SetGlobalSvgFactory(AFactory);

  LLine := Format('%-10s', [AFactoryName]);
  memOutput.Lines.Add(LLine);

  FStartTick := GetTickCount;
  FLastTick := FStartTick;

  BenchmarkLoad;
  LogTicks(LLine, '', FLastTick);

  BenchmarkDraw;
  LogTicks(LLine, '', FLastTick);

  BenchmarkGrayScale;
  LogTicks(LLine, '', FLastTick);

  BenchmarkDraw;
  LogTicks(LLine, '', FLastTick);

  BenchmarkFixedColor;
  LogTicks(LLine, '', FLastTick);

  BenchmarkDraw;
  LogTicks(LLine, '', FLastTick);

  LogTicks(LLine, ' |', FStartTick);
end;

end.
