unit UMainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects, FMX.MultiresBitmap, System.Rtti, System.Messaging,
  FMX.ListBox, FMX.Colors, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.SVGIconImageList, FMX.SVGIconImage, FMX.Ani;

type
  TSVGIconImageListForm = class(TForm)
    NextButton: TButton;
    Panel1: TPanel;
    RandomButton: TButton;
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
    OpenDialog: TOpenDialog;
    ZoomSpinBox: TSpinBox;
    ZoomLabel: TLabel;
    FixedColorCheckBox: TCheckBox;
    ColorAnimation1: TColorAnimation;
    ComboColorBox: TComboColorBox;
    Panel2: TPanel;
    AutosizeSwitch: TSwitch;
    GrayscaleSwitch: TSwitch;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure RandomButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure ShowEditorButtonClick(Sender: TObject);
    procedure ZoomSpinBoxChange(Sender: TObject);
    procedure FixedColorCheckBoxChange(Sender: TObject);
    procedure AutosizeSwitchClick(Sender: TObject);
    procedure ComboColorBoxChange(Sender: TObject);
    procedure GrayscaleSwitchClick(Sender: TObject);
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

procedure TSVGIconImageListForm.RandomButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    //Screen.Cursor := crHourGlass;
    try
      SVGIconImageList.LoadFromFiles(OpenDialog.Files);
    finally
      UpdateGUI;
      //Screen.Cursor := crDefault;
    end;
  end;
  Glyph.ImageIndex := SVGIconImageList.Count-1;
  UpdateGUI;
end;

procedure TSVGIconImageListForm.ShowEditorButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}EditSVGIconImageList(SVGIconImageList);{$ENDIF}
end;

procedure TSVGIconImageListForm.ZoomSpinBoxChange(Sender: TObject);
begin
  SVGIconImageList.Zoom := Round(ZoomSpinBox.Value);
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
  FixedColorCheckBox.IsChecked := True;
  SVGIconImageList.FixedColor := ComboColorBox.Color;
end;

procedure TSVGIconImageListForm.FixedColorCheckBoxChange(Sender: TObject);
begin
  if FixedColorCheckBox.IsChecked then
    SVGIconImageList.FixedColor := ComboColorBox.Color
  else
    SVGIconImageList.FixedColor := SVG_NONE_COLOR;
end;

procedure TSVGIconImageListForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}ShowEditorButton.Visible := False;{$ENDIF}
  UpdateGUI;
end;

procedure TSVGIconImageListForm.GrayscaleSwitchClick(Sender: TObject);
begin
  SVGIconImageList.GrayScale := GrayscaleSwitch.IsChecked;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
