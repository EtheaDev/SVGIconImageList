unit SvgViewerUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  SVGInterfaces, FrameViewer;

type
  TSVGViewerForm = class(TForm)
    OpenDialog1: TOpenDialog;
    RightPanel: TPanel;
    ListBox: TListBox;
    OpenPanel: TPanel;
    OpenButton: TButton;
    LeftPanel: TPanel;
    FrameViewSkia: TFrameView;
    FrameViewTSVG: TFrameView;
    ClientPanel: TPanel;
    FrameViewImage32: TFrameView;
    FrameViewerD2D: TFrameView;
    SetPathButton: TButton;
    ColorGroupBox: TGroupBox;
    FixedColorComboBox: TColorBox;
    ApplyToRootOnlyCheckBox: TCheckBox;
    AspectGroupBox: TGroupBox;
    KeepCheckBox: TCheckBox;
    OpacityGroupBox: TGroupBox;
    OpacityTrackBar: TTrackBar;
    GrayScaleCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SetPathButtonClick(Sender: TObject);
    procedure OpacityTrackBarChange(Sender: TObject);
    procedure FixedColorComboBoxChange(Sender: TObject);
    procedure GrayScaleCheckBoxClick(Sender: TObject);
    procedure ApplyToRootOnlyCheckBoxClick(Sender: TObject);
    procedure KeepCheckBoxClick(Sender: TObject);
  private
    FSourcePath: string;
    procedure DrawImage(const AFileName: string);
    procedure SetSourcePath(const Value: string);
  public
    property SourcePath: string read FSourcePath write SetSourcePath;
  end;

var
  SVGViewerForm: TSVGViewerForm;

implementation

{$R *.dfm}

uses
  Winapi.GDIPAPI, System.IOUtils, System.Types, SVGColor, FileCtrl,
  PasSVGFactory, Image32SVGFactory, D2DSVGFactory, SkiaSVGFactory;

procedure TSVGViewerForm.ApplyToRootOnlyCheckBoxClick(Sender: TObject);
begin
  FrameViewTSVG.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
  FrameViewerD2D.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
  FrameViewSkia.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
  FrameViewImage32.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
end;

procedure TSVGViewerForm.DrawImage(const AFileName: string);
begin
  FrameViewTSVG.DrawFile(AFileName);
  FrameViewerD2D.DrawFile(AFileName);
  FrameViewSkia.DrawFile(AFileName);
  FrameViewImage32.DrawFile(AFileName);
end;

procedure TSVGViewerForm.OpacityTrackBarChange(Sender: TObject);
begin
  FrameViewTSVG.Opacity := OpacityTrackBar.position / 100;
  FrameViewerD2D.Opacity := OpacityTrackBar.position / 100;
  FrameViewSkia.Opacity := OpacityTrackBar.position / 100;
  FrameViewImage32.Opacity := OpacityTrackBar.position / 100;
end;

procedure TSVGViewerForm.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute(Handle) then
    DrawImage(OpenDialog1.FileName);
end;

procedure TSVGViewerForm.SetPathButtonClick(Sender: TObject);
var
  LPath: string;
begin
  LPath := FSourcePath;
  if Vcl.FileCtrl.SelectDirectory(LPath, [sdPrompt], 0) then
    SourcePath := LPath;
end;

procedure TSVGViewerForm.SetSourcePath(const Value: string);
var
  Files: TStringDynArray;
  F: TArray<string>;
  LFileName: string;
begin
  if FSourcePath <> Value then
  begin
    FSourcePath := Value;
    ListBox.Items.Clear;
    Files := TDirectory.GetFiles(FSourcePath, '*.svg');
    for LFileName in Files do
    begin
      SetLength(F, Length(F)+1);
      F[High(F)] := ExtractFileName(LFileName);
    end;
    ListBox.Items.AddStrings(F);
  end;
end;

procedure TSVGViewerForm.FixedColorComboBoxChange(Sender: TObject);
begin
  FrameViewTSVG.FixedColor := FixedColorComboBox.Selected;
  FrameViewerD2D.FixedColor := FixedColorComboBox.Selected;
  FrameViewSkia.FixedColor := FixedColorComboBox.Selected;
  FrameViewImage32.FixedColor := FixedColorComboBox.Selected;
end;

procedure TSVGViewerForm.FormCreate(Sender: TObject);
begin
  SourcePath := ExtractFilePath(Application.ExeName)+'..\svg_examples';

  FrameViewTSVG.InitViewer('Delphi TSVG', GetPasSVGFactory);
  FrameViewerD2D.InitViewer('Native Direct2D', GetD2DSVGFactory);
  FrameViewSkia.InitViewer('Skia SVG', GetSkiaSVGFactory);
  FrameViewImage32.InitViewer('Delphi Image32', GetImage32SVGFactory);

end;

procedure TSVGViewerForm.FormResize(Sender: TObject);
var
  LHeight, LWidth: Integer;
begin
  LHeight := LeftPanel.ClientHeight div 2;
  FrameViewTSVG.Height := LHeight;
  FrameViewerD2D.Height := LHeight;
  LWidth := (Self.ClientWidth - RightPanel.Width) div 2;
  LeftPanel.Width := LWidth;
end;

procedure TSVGViewerForm.GrayScaleCheckBoxClick(Sender: TObject);
begin
  FrameViewTSVG.GrayScale := GrayScaleCheckBox.Checked;
  FrameViewerD2D.GrayScale := GrayScaleCheckBox.Checked;
  FrameViewSkia.GrayScale := GrayScaleCheckBox.Checked;
  FrameViewImage32.GrayScale := GrayScaleCheckBox.Checked;
end;

procedure TSVGViewerForm.KeepCheckBoxClick(Sender: TObject);
begin
  FrameViewTSVG.KeepAspectRatio := KeepCheckBox.Checked;
  FrameViewerD2D.KeepAspectRatio := KeepCheckBox.Checked;
  FrameViewSkia.KeepAspectRatio := KeepCheckBox.Checked;
  FrameViewImage32.KeepAspectRatio := KeepCheckBox.Checked;
end;

procedure TSVGViewerForm.ListBoxClick(Sender: TObject);
begin
  DrawImage(TPath.Combine(FSourcePath,
    ListBox.Items[ListBox.ItemIndex]));
end;

end.
