unit UMainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Forms, FMX.Controls, System.ImageList, FMX.ImgList, FMX.SVGIconImageList,
  FMX.StdCtrls, FMX.Ani, FMX.ListBox, FMX.Layouts, FMX.Colors, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Types, FMX.Controls.Presentation, FMX.SVGIconImage;

type
  TSVGIconImageListForm = class(TForm)
    NextButton: TButton;
    Panel1: TPanel;
    IconsLabel: TLabel;
    CurrentLabel: TLabel;
    PrevButton: TButton;
    ShowEditorButton: TButton;
    ImageView: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    TopPanel: TPanel;
    Glyph2: TGlyph;
    Glyph1: TGlyph;
    Glyph: TGlyph;
    SVGIconImageList: TSVGIconImageList;
    ColorAnimation1: TColorAnimation;
    ComboColorBox: TComboColorBox;
    Panel2: TPanel;
    AutosizeSwitch: TSwitch;
    GrayscaleSwitch: TSwitch;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    Label2: TLabel;
    FixedColorSwitch: TSwitch;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure ShowEditorButtonClick(Sender: TObject);
    procedure ZoomSpinBoxChange(Sender: TObject);
    procedure AutosizeSwitchClick(Sender: TObject);
    procedure ComboColorBoxChange(Sender: TObject);
    procedure GrayscaleSwitchClick(Sender: TObject);
    procedure FixedColorSwitchClick(Sender: TObject);
  private
    procedure UpdateGUI;
  public
    { Public declarations }
  end;

var
  SVGIconImageListForm: TSVGIconImageListForm;

implementation

uses
  System.Math
  {$IFDEF MSWINDOWS}, FMX.SVGIconImageListEditorUnit{$ENDIF}
  , FMX.Consts
  ;

{$R *.fmx}

procedure TSVGIconImageListForm.NextButtonClick(Sender: TObject);
begin
  if SVGIconImageList.Count-1 = Glyph.ImageIndex  then
    Glyph.ImageIndex := 0
  else
    Glyph.ImageIndex := Glyph.ImageIndex +1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.PrevButtonClick(Sender: TObject);
begin
  if Glyph.ImageIndex = 0 then
    Glyph.ImageIndex := SVGIconImageList.Count-1
  else
    Glyph.ImageIndex := Glyph.ImageIndex -1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.ShowEditorButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}EditSVGIconImageList(SVGIconImageList);{$ENDIF}
end;

procedure TSVGIconImageListForm.ZoomSpinBoxChange(Sender: TObject);
begin
//  SVGIconImageList.Zoom := Round(ZoomSpinBox.Value);
end;

procedure TSVGIconImageListForm.UpdateGUI;
begin
  IconsLabel.Text := Format('Total icons: %d', [SVGIconImageList.Count]);
  CurrentLabel.Text := Format('Current: %d', [Glyph.ImageIndex]);
end;

procedure TSVGIconImageListForm.AutosizeSwitchClick(Sender: TObject);
begin
  SVGIconImageList.AutoSizeBitmaps := AutosizeSwitch.IsChecked;
end;

procedure TSVGIconImageListForm.ComboColorBoxChange(Sender: TObject);
begin
  FixedColorSwitch.IsChecked := True;
  SVGIconImageList.FixedColor := ComboColorBox.Color;
end;

procedure TSVGIconImageListForm.FixedColorSwitchClick(Sender: TObject);
begin
  if FixedColorSwitch.IsChecked then
    SVGIconImageList.FixedColor := ComboColorBox.Color
  else
    SVGIconImageList.FixedColor := SVG_NONE_COLOR;
end;

procedure TSVGIconImageListForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}
  ShowEditorButton.Visible := False;
  {$ENDIF}
  UpdateGUI;
end;

procedure TSVGIconImageListForm.GrayscaleSwitchClick(Sender: TObject);
begin
  SVGIconImageList.GrayScale := GrayscaleSwitch.IsChecked;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
