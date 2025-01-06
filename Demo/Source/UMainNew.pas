{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VCL               }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia, Vincent Parrett                                    }
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
unit UMainNew;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  SVGIconImage,
  Vcl.ExtDlgs,
  System.Actions,
  System.ImageList,
  SVGIconImageCollection,
  UDataModule,
  Vcl.VirtualImageList,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.ImgList,
  System.Classes,
  Vcl.ActnList,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  Vcl.Forms;

type
  TMainForm = class(TForm)
    ActionList: TActionList;
    ChangeIconAction: TAction;
    Panel1: TPanel;
    SelectThemeRadioGroup: TRadioGroup;
    TopToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    paButtons: TPanel;
    DeleteButton: TButton;
    ChangeIconButton: TButton;
    ClientPanel: TPanel;
    TreeView: TTreeView;
    ImageView: TListView;
    ImageListLabel: TLabel;
    LoadGroupBox: TGroupBox;
    BuildFromFilesButton: TButton;
    DeleteIconAction: TAction;
    SliderPanel: TPanel;
    TrackBar: TTrackBar;
    IconSizeLabel: TLabel;
    ButtonsPanel: TPanel;
    ClearButton: TButton;
    ShowImageEditorButton: TButton;
    ColorDialog: TColorDialog;
    DisabledAction: TAction;
    OpenDialog: TOpenPictureDialog;
    SVGIconImage: TSVGIconImage;
    Splitter: TSplitter;
    ColorGroupBox: TGroupBox;
    FixedColorComboBox: TColorBox;
    GrayScaleCheckBox: TCheckBox;
    VirtualImageList: TVirtualImageList;
    NewFormButton: TButton;
    NewFormAction: TAction;
    tmrTrackbar: TTimer;
    ApplyToRootOnlyCheckBox: TCheckBox;
    Panel2: TPanel;
    IconOpacityLabel: TLabel;
    OpacityTrackBar: TTrackBar;
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    procedure ChangeIconActionExecute(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowImageEditorButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure DeleteIconActionExecute(Sender: TObject);
    procedure ChangeColorActionExecute(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure BuildFromFilesButtonClick(Sender: TObject);
    procedure SVGIconImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure paButtonsResize(Sender: TObject);
    procedure GrayScaleCheckBoxClick(Sender: TObject);
    procedure FixedColorComboBoxSelect(Sender: TObject);
    procedure NewFormActionExecute(Sender: TObject);
    procedure tmrTrackbarTimer(Sender: TObject);
    procedure ApplyToRootOnlyCheckBoxClick(Sender: TObject);
    procedure SVGIconImageDblClick(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure UpdateButtons;
    procedure UpdateGUI(UpdateIcons: Boolean = True);
    procedure UpdateListView;
    procedure UpdateTreeView;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Vcl.Themes
  , System.SysUtils
  {$IFDEF DXE3+}
  , System.UITypes
  {$ENDIF}
  , SVGRESTClientFormUnit
  , SVGIconUtils
  , SVGTextPropertyEditorUnit
  , SVGIconImageListEditorUnit;

procedure TMainForm.UpdateButtons;
begin
  DeleteButton.Action := DeleteIconAction;
  ChangeIconButton.Action := ChangeIconAction;
  NewFormButton.Action := NewFormAction;
end;

procedure TMainForm.UpdateListView;
var
  LItemsCount: Integer;
begin
  LItemsCount := UpdateSVGIconListView(ImageView);
  ImageListLabel.Caption := Format('SVG Image List Preview: %d icons',[LItemsCount]);
end;

procedure TMainForm.ApplyToRootOnlyCheckBoxClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if FixedColorComboBox.ItemIndex >= 0 then
    begin
      //Warning: native VirtualImageList can change FixedColor only at Collection level
      //so the changes are affected to all forms opened!
      (VirtualImageList.ImageCollection as TSVGIconImageCollection).ApplyFixedColorToRootOnly :=
        ApplyToRootOnlyCheckBox.Checked;
      SVGIconImage.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
      UpdateGUI;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.BuildFromFilesButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      ImageDataModule.SVGIconImageCollection.LoadFromFiles(OpenDialog.Files);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  UpdateGUI;
end;

procedure TMainForm.ChangeColorActionExecute(Sender: TObject);
begin
  ColorDialog.Color := ImageDataModule.SVGIconImageCollection.FixedColor;
  if ColorDialog.Execute then
    ImageDataModule.SVGIconImageCollection.FixedColor := ColorDialog.Color;
  UpdateGUI;
end;

procedure TMainForm.ChangeIconActionExecute(Sender: TObject);
var
  LAction: TAction;
begin
  //Detach Action
  ChangeIconButton.Action := nil;
  LAction := Sender as TAction;
  //Change icon of the connected action
  LAction.ImageIndex := LAction.ImageIndex+1;
  //Attach Action
  ChangeIconButton.Action := ChangeIconAction;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  //Clear Collection
  VirtualImageList.Clear;
  ImageDataModule.SVGIconImageCollection.ClearIcons;
  UpdateGUI;
end;

procedure TMainForm.DeleteIconActionExecute(Sender: TObject);
begin
  if ImageDataModule.SVGIconImageCollection.Count > 0 then
  begin
    ImageDataModule.SVGIconImageCollection.Delete(0);
    UpdateGUI;
  end;
end;

procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  UpdateGUI(False);
end;

procedure TMainForm.FixedColorComboBoxSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if FixedColorComboBox.ItemIndex >= 0 then
    begin
      //Warning: native VirtualImageList can change FixedColor only at Collection level
      //so the changes are affected to all forms opened!
      (VirtualImageList.ImageCollection as TSVGIconImageCollection).FixedColor :=
        FixedColorComboBox.Selected;
      SVGIconImage.FixedColor := FixedColorComboBox.Selected;
      UpdateGUI;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  {$IFDEF HiDPISupport}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
  {$ENDIF}

  //Build available VCL Styles
  SelectThemeRadioGroup.Items.Clear;
  for I := 0 to High(TStyleManager.StyleNames) do
    SelectThemeRadioGroup.Items.Add(TStyleManager.StyleNames[I]);
  TStringList(SelectThemeRadioGroup.Items).Sort;
  SelectThemeRadioGroup.OnClick := nil;
  try
    SelectThemeRadioGroup.ItemIndex :=
      SelectThemeRadioGroup.Items.IndexOf(TStyleManager.ActiveStyle.Name);
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
  end;
  TreeView.Items[0].Expand(True);
  TreeView.Items[2].Expand(True);

  TrackBar.Position := VirtualImageList.Height;
  OpacityTrackBar.Position := ImageDataModule.SVGIconImageCollection.Opacity;
  TrackBarChange(TrackBar);

//  SelectThemeRadioGroupClick(SelectThemeRadioGroup);
end;

procedure TMainForm.GrayScaleCheckBoxClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    //Warning: native VirtualImageList can change FixedColor only at Collection level
    //so the changes are affected to all forms opened!
    (VirtualImageList.ImageCollection as TSVGIconImageCollection).GrayScale :=
      GrayScaleCheckBox.Checked;
    SVGIconImage.GrayScale := GrayScaleCheckBox.Checked;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SelectThemeRadioGroupClick(Sender: TObject);
var
  LStyleName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    LStyleName := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex];
    if TStyleManager.ActiveStyle.Name <> LStyleName then
      TStyleManager.TrySetStyle(LStyleName);
    ImageDataModule.SVGIconImageCollection.Change;
    UpdateGUI(False);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ImageViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  SVGIconImage.ImageIndex := Item.Index;
end;

procedure TMainForm.NewFormActionExecute(Sender: TObject);
begin
  With TMainForm.Create(Application) do
    Show;
end;

procedure TMainForm.paButtonsResize(Sender: TObject);
begin
  SVGIconImage.Height := SVGIconImage.width;
end;

procedure TMainForm.ShowImageEditorButtonClick(Sender: TObject);
begin
  //Image Editor for ImageCollection
  if EditSVGIconImageCollection(ImageDataModule.SVGIconImageCollection) then
    UpdateGUI;
end;

procedure TMainForm.SVGIconImageDblClick(Sender: TObject);
var
  LSVGText: string;
begin
  LSVGText := SVGIconImage.SVGText;
  if EditSVGTextProperty(LSVGText) then
    SVGIconImage.SVGText := LSVGText;
end;

procedure TMainForm.SVGIconImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SVGIconImage.ImageIndex := SVGIconImage.ImageIndex + 1
  else
    SVGIconImage.ImageIndex := SVGIconImage.ImageIndex - 1;
end;

procedure TMainForm.tmrTrackbarTimer(Sender: TObject);
begin
  // Disable trackbar while updating. Otherwise a second event will be fired
  tmrTrackbar.Enabled := false;
  try
    TrackBar.Enabled := false;
    OpacityTrackBar.Enabled := false;
    //Set opacity of Image collection and Image
    ImageDataModule.SVGIconImageCollection.Opacity := OpacityTrackBar.Position;
    SVGIconImage.Opacity := OpacityTrackBar.Position;
    //Resize all icons into ImageList
    VirtualImageList.SetSize(TrackBar.Position, TrackBar.Position);
    UpdateGUI(False);
  finally
    TrackBar.Enabled := true;
    OpacityTrackBar.Enabled := true;
  end;
end;

procedure TMainForm.updateGUI(UpdateIcons: Boolean = True);
var
  LSize: Integer;
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    if UpdateIcons then
    begin
      //Fill VirtualImageList with all icons in the collection of SVGIconVirtualImageList
      //it's only a demo: normally a VirtualImageList contains only the icons needed for
      //the Form
      VirtualImageList.BeginUpdate;
      try
        VirtualImageList.Clear;
        VirtualImageList.Add('', 0, ImageDataModule.SVGIconImageCollection.Count-1 );
      finally
        VirtualImageList.EndUpdate;
      end;
    end;
    LSize := VirtualImageList.Height;
    IconSizeLabel.Caption := Format('Icons size: %d',[LSize]);
    IconOpacityLabel.Caption := Format('Icons opacity: %d',[
      ImageDataModule.SVGIconImageCollection.Opacity]);
    TopToolBar.ButtonHeight := LSize + 2;
    TopToolBar.ButtonWidth := LSize + 2;
    TopToolBar.Height := LSize + 6;
    Splitter.MinSize := DeleteButton.Width + 8;
    UpdateButtons;
    UpdateListView;
    UpdateTreeView;
    GrayScaleCheckBox.Checked := ImageDataModule.SVGIconImageCollection.GrayScale;
    FixedColorComboBox.Selected := ImageDataModule.SVGIconImageCollection.FixedColor;
    OpacityTrackBar.Position := ImageDataModule.SVGIconImageCollection.Opacity;
  finally
    FUpdating := False;
  end;
end;

procedure TMainForm.UpdateTreeView;
var
  LItem: TTreeNode;
  I: Integer;
begin
  for I := 0 to TreeView.Items.Count - 1 do
  begin
    LItem := TreeView.Items[I];
    if VirtualImageList.Count > LItem.ImageIndex then
    begin
      LItem.Text := VirtualImageList.Images[LItem.ImageIndex].Name;
    end
    else
    begin
      LItem.Text := '';
    end;
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  tmrTrackbar.Enabled := true;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.
