{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VLC+FMX           }
{       to simplify use of Icons (resize, opacity and more...)                 }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Volodymyr B.                                             }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit FMX.SVGIconImageListEditorUnit;

interface

{$INCLUDE ..\Source\SVGIconImageList.inc}

uses
  System.SysUtils, System.Types, System.UITypes, FMX.Controls, System.Classes,
  System.Actions, System.ImageList, System.UIConsts, FMX.Forms, FMX.Graphics, FMX.ActnList, FMX.StdCtrls, FMX.Colors, FMX.ListBox,
  FMX.Controls.Presentation, FMX.ImgList, FMX.Types, FMX.Layouts,
  FMX.SVGIconImageList, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, FMX.Dialogs, FMX.Memo.Types, FMX.ComboEdit;

type
  TSVGIconImageListEditorFMX = class(TForm)
    ListBoxItemStyleBook: TStyleBook;
    OpenDialog: TOpenDialog;
    BottomPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    TopPanel: TPanel;
    ButtonsPanel: TPanel;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearAllButton: TButton;
    NewButton: TButton;
    paClient: TPanel;
    ImageListGroupBox: TGroupBox;
    AutoSizeCheckBox: TCheckBox;
    DefaultOpacitySpinBox: TSpinBox;
    DefaultOpacityLabel: TLabel;
    SizeSpinBox: TSpinBox;
    SizeLabel: TLabel;
    FixedColorLabel: TLabel;
    FixedColorComboColorBox: TComboColorBox;
    FixedColorEdit: TEdit;
    GrayScaleCheckBox: TCheckBox;
    ItemPanel: TPanel;
    ItemGroupBox: TGroupBox;
    IconName: TEdit;
    IconNameLabel: TLabel;
    OpacityLabel: TLabel;
    OpacitySpinBox: TSpinBox;
    SVGText: TMemo;
    GrayScaleItemCheckBox: TCheckBox;
    FixedColorItemLabel: TLabel;   
    FixedColorItemComboColorBox: TComboColorBox;
    FixedColorItemEdit: TEdit;
    IconPanel: TPanel;
    IconImage: TGlyph;
    WidthLabel: TLabel;
    WidthSpinBox: TSpinBox;
    HeightLabel: TLabel;
    HeightSpinBox: TSpinBox;
    ZoomLabel: TLabel;
    ZoomSpinBox: TSpinBox;
    IconsGroupBox: TGroupBox;
    ImageView: TListBox;
    BottomSplitter: TSplitter;
    ApplyToRootOnlyCheckBox: TCheckBox;
    ReformatXMLButton: TButton;
    IconButtonsPanel: TPanel;
    IconControlsPanel: TPanel;
    ApplyToRootOnlyItemCheckBox: TCheckBox;
    AddWebButton: TButton;
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IconNameExit(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AutoSizeCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DefaultOpacitySpinBoxChange(Sender: TObject);
    procedure OpacitySpinBoxChange(Sender: TObject);
    procedure SizeChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure FixedColorComboColorBoxChange(Sender: TObject);
    procedure FixedColorEditChange(Sender: TObject);
    procedure GrayScaleCheckBoxChange(Sender: TObject);
    procedure SVGTextExit(Sender: TObject);
    procedure GrayScaleItemCheckBoxChange(Sender: TObject);
    procedure ImageViewDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure ImageViewDragChange(SourceItem, DestItem: TListBoxItem;
      var Allow: Boolean);
    procedure ImageViewDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure ZoomChange(Sender: TObject);
    procedure ApplyToRootOnlyCheckBoxChange(Sender: TObject);
    procedure ReformatXMLButtonClick(Sender: TObject);
    procedure ApplyToRootOnlyItemCheckBoxChange(Sender: TObject);
    procedure FixedColorItemComboColorBoxChange(Sender: TObject);
    procedure FixedColorItemEditChange(Sender: TObject);
    procedure AddWebButtonClick(Sender: TObject);
  private
    FIconIndexLabel: string;
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FEditingList: TSVGIconImageList;
    procedure AddNewItem;
    procedure DeleteSelectedItem;
    procedure ClearAllImages;
    procedure UpdateGUI;
    procedure SetImageOpacity(Opacity: Single);
    procedure SetImageIconName(IconName: String);
    function SelectedSVGIcon: TSVGIconSourceItem;
    function GetClearAlphaColorString(AColor: TAlphaColor): string;
  public
    destructor Destroy; override;
  end;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;

implementation

{$R *.fmx}

uses
  Winapi.Messages
  , Winapi.Windows
  , Winapi.shellApi
  , Xml.XMLDoc
  {$IFDEF Image32_SVGEngine}
  , Img32.SVG.Core
  {$ENDIF}
  , FMX.SVGIconsUtils
  , System.Math
  , FMX.SVGRESTClientFormUnit
  ;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  ItemPanelHeight: Single;

function UpdateSVGIconListViewCaptions(const AListBox: TListBox;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconSourceItem;
  LSVGIconImageList: TSVGIconImageList;
begin
  LSVGIconImageList := AListBox.Images as TSVGIconImageList;
  //AListView.Items.BeginUpdate;
  try
    Result := LSVGIconImageList.Source.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LSVGIconImageList.Source[I] as TSVGIconSourceItem;
      if AShowCaption then
      begin
        AListBox.Items[I] := Format('%d.%s', [LItem.Index, Litem.IconName]);
      end
      else
        AListBox.Items[I] := '';
    end;
  finally
    //AListView.Items.EndUpdate;
  end;
end;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;
var
  LEditor: TSVGIconImageListEditorFMX;
begin
  LEditor := TSVGIconImageListEditorFMX.Create(nil);
  with LEditor do
  begin
    try
      //Screen.Cursor := crHourglass;
      try
        FEditingList.Assign(AImageList);
        FEditingList.RefreshAllIcons;
        SizeSpinBox.Value := Max(FEditingList.Width, FEditingList.Height);
        WidthSpinBox.Value := FEditingList.Width;
        HeightSpinBox.Value := FEditingList.Height;
        ZoomSpinBox.Value := FEditingList.Zoom;
        ImageView.Images := FEditingList;
        UpdateSVGIconListView(ImageView);
        //UpdateGUI;
        if ImageView.Items.Count > 0 then
          ImageView.ItemIndex := 0;

      finally
        //Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        //Screen.Cursor := crHourglass;
        try
        AImageList.Assign(FEditingList);
        finally
          //Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := Bounds;
      ItemPanelHeight := ItemPanel.Height;
    finally
      Free;
    end;
  end;
end;

{ TSVGIconImageListEditorFMX }

procedure TSVGIconImageListEditorFMX.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/Overview-(FMX).html'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TSVGIconImageListEditorFMX.SizeChange(Sender: TObject);
begin
  if not FUpdating then
  begin
    if Sender = SizeSpinBox then
      FEditingList.Size := Round(SizeSpinBox.Value);
    if Sender = WidthSpinBox then
      FEditingList.Width := Round(WidthSpinBox.Value);
    if Sender = HeightSpinBox then
      FEditingList.Height := Round(HeightSpinBox.Value);
    UpdateGUI;
  end;
end;

procedure TSVGIconImageListEditorFMX.SVGTextExit(Sender: TObject);
begin
  SelectedSVGIcon.SVGText := SVGText.Lines.Text;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.AutoSizeCheckBoxClick(Sender: TObject);
begin
  FEditingList.AutoSizeBitmaps := AutoSizeCheckBox.IsChecked;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.SetImageIconName(IconName: String);
begin
  SelectedSVGIcon.IconName := IconName;
  UpdateGUI;
  UpdateSVGIconListViewCaptions(ImageView);
end;

procedure TSVGIconImageListEditorFMX.SetImageOpacity(Opacity: Single);
begin
  SelectedSVGIcon.Opacity := Opacity / 100;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LIconItem: TSVGIconSourceItem;
begin
  FUpdating := True;
  try
    SizeSpinBox.Value := Max(FEditingList.Width, FEditingList.Height);
    WidthSpinBox.Value := FEditingList.Width;
    HeightSpinBox.Value := FEditingList.Height;
    ZoomSpinBox.Value := FEditingList.Zoom;
    ImageView.ItemHeight := SizeSpinBox.Value;
    LIconItem := SelectedSVGIcon;
    LIsItemSelected := LIconItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    DeleteButton.Enabled := LIsItemSelected;
    OpacitySpinBox.Enabled := LIsItemSelected;
    IconName.Enabled := LIsItemSelected;
    SVGText.Enabled := LIsItemSelected;
    //ShowCharMapButton.Enabled := (FEditingList.FontName <> '');
    IconsGroupBox.Text := Format(FTotIconsLabel, [FEditingList.Count]);
    SizeSpinBox.Value := FEditingList.Size;
    AutoSizeCheckBox.IsChecked := FEditingList.AutoSizeBitmaps;
    DefaultOpacitySpinBox.Value := FEditingList.Opacity * 100;
    GrayScaleCheckBox.IsChecked := FEditingList.GrayScale;
    
    if FEditingList.FixedColor <> SVG_INHERIT_COLOR then
      FixedColorEdit.Text := GetClearAlphaColorString(FEditingList.FixedColor)
    else FixedColorEdit.Text := '';
    FixedColorComboColorBox.Color := FEditingList.FixedColor;

    if LIsItemSelected then
    begin
      ItemGroupBox.Text := Format(FIconIndexLabel,[LIconItem.Index]);
      IconName.Text := LIconItem.IconName;
      SVGText.Lines.Text := LIconItem.SVGText;
      OpacitySpinBox.Value := LIconItem.Opacity * 100;
      IconImage.ImageIndex := LIconItem.Index;
      GrayScaleItemCheckBox.IsChecked := LIconItem.GrayScale;

      if LIconItem.FixedColor <> SVG_INHERIT_COLOR then
        FixedColorItemEdit.Text := GetClearAlphaColorString(LIconItem.FixedColor)
      else FixedColorItemEdit.Text := '';
      FixedColorItemComboColorBox.Color := LIconItem.FixedColor;

      IconImage.Repaint;
    end
    else
    begin
      IconName.Text := '';
      SVGText.Lines.Clear;
      IconImage.ImageIndex := -1;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TSVGIconImageListEditorFMX.ZoomChange(Sender: TObject);
begin
  if not FUpdating then
    FEditingList.Zoom := Round(ZoomSpinBox.Value);
end;

procedure TSVGIconImageListEditorFMX.DeleteSelectedItem;
var
  LIndex: Integer;
begin
  LIndex := ImageView.Selected.Index;
  FEditingList.DeleteIcon(LIndex);
  UpdateSVGIconListView(ImageView);
  if LIndex < ImageView.Items.Count then
    ImageView.ItemIndex := LIndex
  else if ImageView.Items.Count > 0 then
    ImageView.ItemIndex := LIndex-1;
  UpdateGUI;
end;

destructor TSVGIconImageListEditorFMX.Destroy;
begin
  inherited;
end;

procedure TSVGIconImageListEditorFMX.ClearAllImages;
begin
  //Screen.Cursor := crHourglass;
  try
    FEditingList.ClearIcons;
  finally
    //Screen.Cursor := crDefault;
  end;
end;

(*
procedure TSVGIconImageListEditorFMX.CloseCharMap(Sender: TObject;
  var Action: TCloseAction);
begin
  if FCharMap.ModalResult = mrOK then
  begin
    if FCharMap.CharsEdit.Text <> '' then
    begin
      FEditingList.AddIcons(FCharMap.CharsEdit.Text, FCharMap.DefaultFontName.Text);
      UpdateSVGIconListView(ImageView);
    end;
  end;
end;
*)

procedure TSVGIconImageListEditorFMX.ClearAllButtonClick(Sender: TObject);
begin
  ClearAllImages;
  UpdateSVGIconListView(ImageView);
  UpdateGUI;
end;

(*
procedure TSVGIconImageListEditorFMX.SVGIconImageListFontMissing(
  const AFontName: TFontName);
begin
  MessageDlg(Format(ERR_SVGIcon_FONT_NOT_INSTALLED,[AFontName]),
    mtError, [mbOK], 0);
end;
*)

procedure TSVGIconImageListEditorFMX.IconNameExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageIconName(IconName.Text);
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.ImageViewDragChange(SourceItem,
  DestItem: TListBoxItem; var Allow: Boolean);
var
  LOriginalIcon, LIcon: TSVGIconSourceItem;
  LNewIndex, LSourceIndex: Integer;
  LIconName: string;
begin
  Allow := False;
  if SourceItem.Index = DestItem.Index then Exit;

  LSourceIndex := SourceItem.Index;
  LNewIndex := DestItem.Index;
  if LNewIndex < 0 then LNewIndex := 0;
  if LNewIndex > FEditingList.Count then LNewIndex := FEditingList.Count;

  LOriginalIcon := FEditingList.Source.Items[LSourceIndex] as TSVGIconSourceItem;
  LIconName := LOriginalIcon.IconName;

  if LSourceIndex < LNewIndex then
  begin
    LIcon := FEditingList.CloneIcon(LSourceIndex, LNewIndex + 1);
    FEditingList.DeleteIcon(LSourceIndex);
  end
  else
  begin
    LIcon := FEditingList.CloneIcon(LSourceIndex, LNewIndex);
    FEditingList.DeleteIcon(LSourceIndex + 1);
  end;

  LIcon.IconName := LIconName;

  UpdateSVGIconListView(ImageView);
end;

procedure TSVGIconImageListEditorFMX.ImageViewDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  LFiles: TStringList;
begin
  if Length(Data.Files) <= 0 then Exit;

  LFiles := TStringList.Create;
  LFiles.AddStrings(TArray<string>(Data.Files));
  try
    FEditingList.LoadFromFiles(LFiles);
  finally
    FreeAndNil(LFiles);
  end;

  UpdateSVGIconListView(ImageView);
end;

procedure TSVGIconImageListEditorFMX.ImageViewDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if Length(Data.Files) > 0 then Operation := TDragOperation.Copy
  else Operation := TDragOperation.None;
end;

procedure TSVGIconImageListEditorFMX.ImageViewSelectItem(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.NewButtonClick(Sender: TObject);
begin
  AddNewItem;
end;

procedure TSVGIconImageListEditorFMX.OpacitySpinBoxChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageOpacity(OpacitySpinBox.Value);
end;

procedure TSVGIconImageListEditorFMX.ReformatXMLButtonClick(Sender: TObject);
begin
  SVGText.Lines.Text := Xml.XMLDoc.FormatXMLData(SVGText.Lines.Text);
end;

procedure TSVGIconImageListEditorFMX.DefaultOpacitySpinBoxChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageOpacity(DefaultOpacitySpinBox.Value);
end;

procedure TSVGIconImageListEditorFMX.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TSVGIconImageListEditorFMX.FixedColorComboColorBoxChange(Sender: TObject);
begin
  FixedColorEdit.Text := GetClearAlphaColorString(FixedColorComboColorBox.Color);
end;

procedure TSVGIconImageListEditorFMX.FixedColorEditChange(Sender: TObject);
var
  LColorText: string;
begin
  if FUpdating then Exit;
  if (Length(FixedColorEdit.Text) > 0) 
    and (Length(FixedColorEdit.Text) < 6) then Exit;

  try
    if FixedColorEdit.Text <> '' then
    begin
      LColorText := FixedColorEdit.Text;
      if (LColorText.Chars[0] <> '#') and (LColorText.Chars[0] <> 'x') then LColorText := '#' + LColorText;
      FEditingList.FixedColor := StringToAlphaColor(LColorText);
    end
    else FEditingList.FixedColor := TAlphaColors.Null;

    UpdateGUI;
  except end;
end;

procedure TSVGIconImageListEditorFMX.FixedColorItemComboColorBoxChange(
  Sender: TObject);
begin
  FixedColorItemEdit.Text := GetClearAlphaColorString(FixedColorItemComboColorBox.Color);
end;

procedure TSVGIconImageListEditorFMX.FixedColorItemEditChange(Sender: TObject);
var
  LColorText: string;
begin
  if FUpdating then Exit;
  if (Length(FixedColorItemEdit.Text) > 0) 
    and (Length(FixedColorItemEdit.Text) < 6) then Exit;

  try
    if FixedColorItemEdit.Text <> '' then
    begin
      LColorText := FixedColorItemEdit.Text;
      if (LColorText.Chars[0] <> '#') and (LColorText.Chars[0] <> 'x') then LColorText := '#' + LColorText;
      SelectedSVGIcon.FixedColor := StringToAlphaColor(LColorText);
    end
    else SelectedSVGIcon.FixedColor := TAlphaColors.Null;

    UpdateGUI;
  except end;
end;

procedure TSVGIconImageListEditorFMX.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus
  else
    CancelButton.SetFocus;
end;

procedure TSVGIconImageListEditorFMX.FormCreate(Sender: TObject);
begin
  Caption := Format(Caption, [SVGIconImageListVersion]);
  {$IFDEF D11+}
  Constraints.MinHeight := 500;
  Constraints.MinWidth := 700;
  {$ENDIF}
  FUpdating := True;
  FEditingList := TSVGIconImageList.Create(nil);
  FIconIndexLabel := ItemGroupBox.Text;
  FTotIconsLabel := IconsGroupBox.Text;
  IconImage.Images := FEditingList;
  BottomSplitter.Position.X := 1;
end;

procedure TSVGIconImageListEditorFMX.FormDestroy(Sender: TObject);
begin
  IconImage.Images := nil;
  ImageView.Images := nil;
     
  FreeAndNil(FEditingList);
  //Screen.Cursors[crColorPick] := 0;
end;

procedure TSVGIconImageListEditorFMX.FormResize(Sender: TObject);
begin
  if ClientWidth < 760 then
    ClientWidth := 760;
  if ClientHeight < 500 then
    ClientHeight := 500;
end;

function TSVGIconImageListEditorFMX.SelectedSVGIcon: TSVGIconSourceItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.Source.Count) then
    Result := FEditingList.Source.Items[ImageView.Selected.Index] as TSVGIconSourceItem
  else
    Result := nil;
end;

procedure TSVGIconImageListEditorFMX.AddButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    //Screen.Cursor := crHourGlass;
    try
      FEditingList.LoadFromFiles(OpenDialog.Files);
      UpdateSVGIconListView(ImageView);
    finally
      //Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TSVGIconImageListEditorFMX.AddWebButtonClick(Sender: TObject);
begin
  if SearchSVGIconsFromWeb(FEditingList) then
  begin
    //Screen.Cursor := crHourGlass;
    try
      UpdateSVGIconListView(ImageView);
    finally
      //Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TSVGIconImageListEditorFMX.FormShow(Sender: TObject);
begin
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);

  if ItemPanelHeight <> 0 then
    ItemPanel.Height := ItemPanelHeight;

  if ImageView.CanFocus then
    ImageView.SetFocus;
end;

procedure TSVGIconImageListEditorFMX.GrayScaleCheckBoxChange(Sender: TObject);
begin
  FEditingList.GrayScale := GrayScaleCheckBox.IsChecked;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.GrayScaleItemCheckBoxChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedSVGIcon.GrayScale := GrayScaleItemCheckBox.IsChecked;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.AddNewItem;
var
  LInsertIndex: Integer;
begin
  if (ImageView.Selected <> nil) then
    LInsertIndex := ImageView.Selected.Index +1
  else
    LInsertIndex := ImageView.Items.Count;
  FEditingList.InsertIcon(LInsertIndex,'','');
  UpdateSVGIconListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
end;

procedure TSVGIconImageListEditorFMX.ApplyToRootOnlyCheckBoxChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.IsChecked;
  UpdateGUI;
end;

procedure TSVGIconImageListEditorFMX.ApplyToRootOnlyItemCheckBoxChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedSVGIcon.ApplyFixedColorToRootOnly := ApplyToRootOnlyItemCheckBox.IsChecked;
  UpdateGUI;
end;

function TSVGIconImageListEditorFMX.GetClearAlphaColorString(AColor: TAlphaColor): string;
begin
  Result := AlphaColorToString(AColor);
  if (Length(Result) > 3) and (Result.Chars[0] = '#') then Result := '#FF' + Result.Substring(3);
end;

initialization
  ItemPanelHeight := 0;

end.
