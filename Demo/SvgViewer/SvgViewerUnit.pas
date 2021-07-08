unit SvgViewerUnit;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  SVGInterfaces, FrameViewer;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    RightPanel: TPanel;
    ListBox: TListBox;
    OpenPanel: TPanel;
    OpenButton: TButton;
    LeftPanel: TPanel;
    FrameViewCairo: TFrameView;
    FrameViewTSVG: TFrameView;
    ClientPanel: TPanel;
    FrameViewImage32: TFrameView;
    FrameViewerD2D: TFrameView;
    SetPathButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SetPathButtonClick(Sender: TObject);
  private
    FSourcePath: string;
    { Private declarations }
    procedure DrawImage(const AFileName: string);
    procedure SetSourcePath(const Value: string);
  public
    property SourcePath: string read FSourcePath write SetSourcePath;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Winapi.GDIPAPI, System.IOUtils, System.Types, SVGColor, FileCtrl,
  PasSVGFactory, Image32SVGFactory, D2DSVGFactory, CairoSVGFactory;

procedure TForm1.DrawImage(const AFileName: string);
begin
  FrameViewTSVG.DrawFile(AFileName);
  FrameViewerD2D.DrawFile(AFileName);
  FrameViewCairo.DrawFile(AFileName);
  FrameViewImage32.DrawFile(AFileName);
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute(Handle) then
    DrawImage(OpenDialog1.FileName);
end;

procedure TForm1.SetPathButtonClick(Sender: TObject);
var
  LPath: string;
begin
  LPath := FSourcePath;
  if Vcl.FileCtrl.SelectDirectory(LPath, [sdPrompt], 0) then
    SourcePath := LPath;
end;

procedure TForm1.SetSourcePath(const Value: string);
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
      F := F + [ExtractFileName(LFileName)];
    end;
    ListBox.Items.AddStrings(F);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SourcePath := ExtractFilePath(Application.ExeName)+'..\svg_examples';

  FrameViewTSVG.InitViewer('Delphi TSVG', GetPasSVGFactory);
  FrameViewerD2D.InitViewer('Native Direct2D', GetD2DSVGFactory);
  FrameViewCairo.InitViewer('Cairo SVG', GetCairoSVGFactory);
  FrameViewImage32.InitViewer('Delphi Image32', GetImage32SVGFactory);

end;

procedure TForm1.FormResize(Sender: TObject);
var
  LHeight, LWidth: Integer;
begin
  LHeight := LeftPanel.ClientHeight div 2;
  FrameViewTSVG.Height := LHeight;
  FrameViewerD2D.Height := LHeight;
  LWidth := (Self.ClientWidth - RightPanel.Width) div 2;
  LeftPanel.Width := LWidth;
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  DrawImage(TPath.Combine(FSourcePath,
    ListBox.Items[ListBox.ItemIndex]));
end;

end.
