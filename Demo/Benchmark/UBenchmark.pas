{-----------------------------------------------------------------------------
 Unit Name: UBenchmark
 Author:    L�bbe Onken
 Purpose:   Main form for SVG Factories Benchmark
 History:
-----------------------------------------------------------------------------}
unit UBenchmark;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.ImageList, //if you are compiling with older version than XE7 remove this line
  Vcl.ImgList,
  SVGIconImageCollection, SVGIconImage, Vcl.Samples.Spin, Vcl.ExtCtrls,
  SVGIconImageListBase, SVGIconVirtualImageList, Vcl.BaseImageCollection,
  Vcl.ComCtrls, Vcl.VirtualImageList;

type
  TSVGFactory = (svgImage32, svgSkia, svgDirect2D, svgSVGMagic);

const
  ASVGFactoryNames: Array[TSVGFactory] of string =
    ('Native Image32', 'Skia4Delphi', 'Direct2D', 'SVGMagic');

type
  TfrmBenchmark = class(TForm)
    SVGIconImageCollection: TSVGIconImageCollection;
    memOutput: TMemo;
    btnClear: TButton;
    btnLoad: TButton;
    OpenDialog: TOpenDialog;
    btnRunBenchmark: TButton;
    speLoops: TSpinEdit;
    lblLoops: TLabel;
    pnlButtons: TPanel;
    chkGrayScale: TCheckBox;
    chkFixedColor: TCheckBox;
    splHorizontal: TSplitter;
    pnlLoops: TPanel;
    grpFactory: TRadioGroup;
    chkDrawVisible: TCheckBox;
    KeepAspectCheckBox: TCheckBox;
    Panel1: TPanel;
    BtnSelDir: TButton;
    FilesListBox: TListBox;
    SVGIconVirtualImageList: TSVGIconVirtualImageList;
    SVGIconImage: TSVGIconImage;
    BenchMarkImage: TSVGIconImage;
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnRunBenchmarkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grpFactoryClick(Sender: TObject);
    procedure KeepAspectCheckBoxClick(Sender: TObject);
    procedure BtnSelDirClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSvgSource  : string;
    FStartTick  : Int64;
    FLastTick   : Int64;
    FLine       : string;
    FInBenchmark: boolean;
    FFiles      : TStringList;

    procedure LoadFilesDir(const APath, AFilter: string);
    function GetFactoryName: string;
    procedure SetFactory(AFactory: TSVGFactory);

    procedure BenchmarkLoad;
    procedure BenchmarkGrayScale;
    procedure BenchmarkFixedColor;
    procedure BenchmarkDraw;

    procedure LogTicks(var AMessage: string; ATick: Int64);
    procedure PrepareBenchmark(ACaption: string);
    procedure ReloadImage;
    procedure RunBenchmark(AFactory: TSVGFactory);
  end;

var
  frmBenchmark: TfrmBenchmark;

implementation

uses
  FileCtrl,
  SVGInterfaces,
  Image32SVGFactory,
  D2DSVGFactory,
  SkiaSVGFactory,
  SVGMagicFactory,
  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.TypInfo,
  System.Types;

{$R *.dfm}


type
  TCanvasImage = class(TSVGIconImage) // Just to have something to paint on
  public
    property Canvas;
  end;

procedure TfrmBenchmark.BenchmarkDraw;

  procedure DrawOnCanvas(ACanvas: TCanvas);
  var
    I    : integer;
    LStep: real;
    LSize: real;
    LRect: TRect;
  begin
    LStep := Min(ACanvas.ClipRect.Width, ACanvas.ClipRect.Height) / speLoops.Value;
    LSize := 0;

    for I := 1 to speLoops.Value do
      begin
        LSize := LSize + LStep;
        LRect := TRect.Create(0, 0, Round(LSize), Round(LSize));

        SvgIconImageCollection.Draw(ACanvas, LRect, 0, KeepAspectCheckBox.Checked);
      end;
  end;

var
  LBitmap: TBitmap;
begin
  // Benchmark Draw
  if chkDrawVisible.Checked then
  begin
    DrawOnCanvas(TCanvasImage(BenchMarkImage).Canvas);
    BenchMarkImage.Repaint;
  end
  else
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.SetSize(SvgIconImage.Width, SvgIconImage.Height);
        DrawOnCanvas(LBitmap.Canvas);
      finally
        LBitmap.Free;
      end;
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
        LSvg.Source := FSvgSource;
        SVGIconImageCollection.Add(LSvg, '');
      end;
  finally
    SVGIconImageCollection.SVGIconItems.EndUpdate;
  end;
end;

procedure TfrmBenchmark.btnClearClick(Sender: TObject);
begin
  memOutput.Clear;
end;

procedure TfrmBenchmark.btnLoadClick(Sender: TObject);
var
  LSvg: ISvg;
begin
  if OpenDialog.Execute then
    begin
      FSvgSource := TFile.ReadAllText(OpenDialog.FileName, TEncoding.UTF8);

      PrepareBenchmark('Factory         |  Load  |  Draw  | Total');

      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.Source := FSvgSource;
      LogTicks(FLine, FLastTick);

      SVGIconImageCollection.SVGIconItems.Clear;
      SVGIconImageCollection.Add(LSvg, 'IconName');

      SVGIconVirtualImageList.Clear;
      SVGIconVirtualImageList.Add('IconName', 'IconName');
      SVGIconImage.ImageIndex := 0;

      LogTicks(FLine, FLastTick);
      LogTicks(FLine, FStartTick);
    end;
end;

procedure TfrmBenchmark.btnRunBenchmarkClick(Sender: TObject);
var
  LLine: string;
  LFactory: TSVGFactory;
begin
  if (FSvgSource = '') then
    memOutput.Lines.Add('Please load a SVG image first')
  else
    begin
      FInBenchmark := true;
      try
        SVGIconImage.ImageIndex := -1;

        LLine := 'Factory         |  Load  |  Draw  ';
        if chkGrayScale.Checked then
          LLine := LLine + '|  Gray  |  Draw  ';
        if chkFixedColor.Checked then
          LLine := LLine + '|  Fixed |  Draw  ';
        LLine := LLine + '|  Total';

        memOutput.Lines.Add('');
        memOutput.Lines.Add(Format('Benchmark: Repeat %d times. Draw %svisible.',
          [speLoops.Value, IfThen(chkDrawVisible.Checked, '', 'in')]));

        memOutput.Lines.Add(LLine);
        for LFactory := Low(TSVGFactory) to high(TSVGFactory) do
          RunBenchmark(LFactory);

        SVGIconImage.ImageIndex := 0;
      finally
        FInBenchmark := false;
      end;
    end;
end;

procedure TfrmBenchmark.LoadFilesDir(const APath, AFilter: string);
var
  SR: TSearchRec;
  LFilter: string;
  LErrors: string;
begin
  Screen.Cursor := crHourGlass;
  Try
    FFiles.Clear;
    LErrors := '';
    LFilter := Format('%s*%s*.svg', [IncludeTrailingPathDelimiter(APath), AFilter]);
    if FindFirst(LFilter, faArchive, SR) = 0 then
    begin
      repeat
        FFiles.Add(SR.Name); //Fill the list
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
    FilesListBox.Items.Assign(FFiles);
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TfrmBenchmark.BtnSelDirClick(Sender: TObject);
var
  LDir: string;
begin
  LDir := ExtractFilePath(Application.ExeName)+'..\svg_examples';
  if FileCtrl.SelectDirectory(LDir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    LoadFilesDir(LDir, '');
  end;
end;

procedure TfrmBenchmark.FormCreate(Sender: TObject);
var
  LFactory: TSVGFactory;
begin
  FFiles := TStringList.Create;
  Caption := Application.Title;
  FInBenchmark := false;
  for LFactory := Low(TSVGFactory) to high(TSVGFactory) do
    grpFactory.Items.Add(ASVGFactoryNames[LFactory]);
  SetFactory(Low(TSVGFactory));
end;

procedure TfrmBenchmark.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
  inherited;
end;

function TfrmBenchmark.GetFactoryName: string;
begin
  if grpFactory.ItemIndex > -1 then
    Result := grpFactory.Items[grpFactory.ItemIndex]
  else
    Result := '';
end;

procedure TfrmBenchmark.grpFactoryClick(Sender: TObject);
begin
  if not FInBenchmark then
    begin
      SetFactory(TSVGFactory(grpFactory.ItemIndex));
      ReloadImage;
    end;
end;

procedure TfrmBenchmark.KeepAspectCheckBoxClick(Sender: TObject);
begin
  SVGIconImage.Proportional := KeepAspectCheckBox.Checked;
end;

procedure TfrmBenchmark.LogTicks(var AMessage: string; ATick: Int64);
var
  LCurrentTick: Int64;
begin
  LCurrentTick := GetTickCount;
  AMessage := Format('%s | %6d', [AMessage, LCurrentTick - ATick]);
  memOutput.Lines[memOutput.Lines.Count - 1] := AMessage;
  FLastTick := LCurrentTick;
end;

procedure TfrmBenchmark.PrepareBenchmark(ACaption: string);
begin
  if ACaption <> '' then
    memOutput.Lines.Add(ACaption);

  FLine := Format('%-15s', [GetFactoryName]);
  memOutput.Lines.Add(FLine);

  FStartTick := GetTickCount;
  FLastTick := FStartTick;
end;

procedure TfrmBenchmark.ReloadImage;
var
  LSvg: ISvg;
begin
  if FSvgSource <> '' then
    begin
      PrepareBenchmark('Factory         |  Load  |  Draw  | Total');

      LSvg := GlobalSvgFactory.NewSvg;
      LSvg.Source := FSvgSource;
      LogTicks(FLine, FLastTick);

      SVGIconImage.ImageIndex := -1;
      SVGIconImageCollection.SVGIconItems.Clear;
      SVGIconImageCollection.Add(LSvg, '');
      SVGIconImage.ImageIndex := 0;
      LogTicks(FLine, FLastTick);
      LogTicks(FLine, FStartTick);
    end;
end;

procedure TfrmBenchmark.RunBenchmark(AFactory: TSVGFactory);

  procedure Benchmark(AProc: TPRoc);
  begin
    if Assigned(AProc) then
      begin
        AProc;
        LogTicks(FLine, FLastTick);
      end;
  end;

begin
  SetFactory(AFactory);

  PrepareBenchmark('');

  Benchmark(BenchmarkLoad);
  Benchmark(BenchmarkDraw);

  if chkGrayScale.Checked then
    begin
      Benchmark(BenchmarkGrayScale);
      Benchmark(BenchmarkDraw);
    end;

  if chkFixedColor.Checked then
    begin
      Benchmark(BenchmarkFixedColor);
      Benchmark(BenchmarkDraw);
    end;

  LogTicks(FLine, FStartTick);
end;

procedure TfrmBenchmark.SetFactory(AFactory: TSVGFactory);
begin
  case AFactory of
    svgDirect2D:
      SetGlobalSvgFactory(GetD2DSVGFactory);
    svgImage32:
      SetGlobalSvgFactory(GetImage32SVGFactory);
    svgSkia:
      SetGlobalSvgFactory(GetSkiaSVGFactory);
    svgSVGMagic:
      SetGlobalSvgFactory(GetSVGMagicFactory);
  end;
  grpFactory.ItemIndex := Ord(AFactory);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
