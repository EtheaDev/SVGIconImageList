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
    FilesPanel: TPanel;
    ListBox: TListBox;
    OpenPanel: TPanel;
    ControlPanel: TPanel;
    OpenButton: TButton;
    RightPanel: TPanel;
    FrameViewSkia: TFrameView;
    ClientPanel: TPanel;
    FrameViewImage32: TFrameView;
    FrameViewerD2D: TFrameView;
    SetPathButton: TButton;
    ColorGroupBox: TGroupBox;
    FixedColorComboBox: TColorBox;
    ApplyToRootOnlyCheckBox: TCheckBox;
    AspectGroupBox: TGroupBox;
    KeepCheckBox: TCheckBox;
    GrayScaleCheckBox: TCheckBox;
    OpacityGroupBox: TGroupBox;
    OpacityTrackBar: TTrackBar;
    TitlePanel: TPanel;
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
  Winapi.GDIPAPI, System.IOUtils, System.Types, FileCtrl,
  Image32SVGFactory,
  {$IFDEF SKIA}SkiaSVGFactory,{$ENDIF}
  D2DSVGFactory;

procedure TSVGViewerForm.ApplyToRootOnlyCheckBoxClick(Sender: TObject);
begin
  FrameViewerD2D.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
  {$IFDEF SKIA}FrameViewSkia.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;{$ENDIF}
  FrameViewImage32.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
end;

procedure TSVGViewerForm.DrawImage(const AFileName: string);
begin
  FrameViewerD2D.DrawFile(AFileName);
  {$IFDEF SKIA}FrameViewSkia.DrawFile(AFileName);{$ENDIF}
  FrameViewImage32.DrawFile(AFileName);
end;

procedure TSVGViewerForm.OpacityTrackBarChange(Sender: TObject);
begin
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
  FrameViewerD2D.FixedColor := FixedColorComboBox.Selected;
  {$IFDEF SKIA}FrameViewSkia.FixedColor := FixedColorComboBox.Selected;{$ENDIF}
  FrameViewImage32.FixedColor := FixedColorComboBox.Selected;
end;

procedure TSVGViewerForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  SourcePath := ExtractFilePath(Application.ExeName)+'..\svg_examples';

  FrameViewerD2D.InitViewer('Native Direct2D', GetD2DSVGFactory);
  {$IFDEF SKIA}
  FrameViewSkia.InitViewer('Skia SVG', GetSkiaSVGFactory);
  {$ELSE}
  FrameViewSkia.Visible := False;
  {$ENDIF}
  FrameViewImage32.InitViewer('Delphi Image32', GetImage32SVGFactory);
end;

procedure TSVGViewerForm.FormResize(Sender: TObject);
var
  LHeight, LWidth: Integer;
begin
  LHeight := RightPanel.ClientHeight div 2;
  FrameViewerD2D.Height := LHeight;
  ControlPanel.Height := LHeight;
  LWidth := (Self.ClientWidth - FilesPanel.Width) div 2;
  RightPanel.Width := LWidth;
end;

procedure TSVGViewerForm.GrayScaleCheckBoxClick(Sender: TObject);
begin
  FrameViewerD2D.GrayScale := GrayScaleCheckBox.Checked;
  {$IFDEF SKIA}FrameViewSkia.GrayScale := GrayScaleCheckBox.Checked;{$ENDIF}
  FrameViewImage32.GrayScale := GrayScaleCheckBox.Checked;
end;

procedure TSVGViewerForm.KeepCheckBoxClick(Sender: TObject);
begin
  FrameViewerD2D.KeepAspectRatio := KeepCheckBox.Checked;
  {$IFDEF SKIA}FrameViewSkia.KeepAspectRatio := KeepCheckBox.Checked;{$ENDIF}
  FrameViewImage32.KeepAspectRatio := KeepCheckBox.Checked;
end;

procedure TSVGViewerForm.ListBoxClick(Sender: TObject);
begin
  DrawImage(TPath.Combine(FSourcePath,
    ListBox.Items[ListBox.ItemIndex]));
end;

end.
