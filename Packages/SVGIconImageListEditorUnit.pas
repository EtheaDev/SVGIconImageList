{******************************************************************************}
{                                                                              }
{       SVGIconImageList Component Editor                                      }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconsImageList                          }
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
unit SVGIconImageListEditorUnit;

interface

{$INCLUDE ..\Source\SVGIconImageList.inc}

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Controls
  , System.Classes
  , Vcl.Dialogs
  , Vcl.ComCtrls
  , Vcl.ImgList
  , Vcl.ExtDlgs
  , Vcl.Samples.Spin
  , SVGIconImage
  , SVGIconImageListBase
  , SVGIconImageList
  , SVGIconImageCollection
  , SVGIconVirtualImageList
  ;

resourcestring
  SELECT_DIR = 'Select directory';
  FILES_SAVED = '%d File(s) saved into "%s" folder';
  USING_ENGINE = 'Using %s';
  ENGINE_HINT = 'Current Active Engine for rendering SVG images';

type
  TOpenPictureDialogSvg = class(TOpenPictureDialog)
  protected
    procedure DoSelectionChange; override;
  public
    function Execute(ParentWnd: HWND): Boolean; override;
  end;

  TSVGIconImageListEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    SaveDialog: TSavePictureDialog;
    ImageListGroup: TGroupBox;
    paTop: TPanel;
    paClient: TPanel;
    SVGText: TMemo;
    SizeLabel: TLabel;
    SizeEdit: TSpinEdit;
    WidthLabel: TLabel;
    WidthEdit: TSpinEdit;
    HeightEdit: TSpinEdit;
    HeightLabel: TLabel;
    BottomPanel: TPanel;
    ApplyButton: TButton;
    HelpButton: TButton;
    paIcon: TPanel;
    IconButtonsPanel: TPanel;
    NewButton: TButton;
    ItemGroupBox: TGroupBox;
    IconNameLabel: TLabel;
    IconFixedColorLabel: TLabel;
    IconPanel: TPanel;
    IconImage: TSVGIconImage;
    AddButton: TButton;
    NameEdit: TEdit;
    FixedColorItemComboBox: TColorBox;
    GrayScaleItemCheckBox: TCheckBox;
    paImages: TPanel;
    CategorySplitter: TSplitter;
    ImageView: TListView;
    ReformatXMLButton: TButton;
    paButtons: TPanel;
    ImageListGroupBox: TGroupBox;
    ReplaceButton: TButton;
    ClearAllButton: TButton;
    ExportButton: TButton;
    ImagesPanel: TPanel;
    CategoryGroupBox: TGroupBox;
    CategoryListBox: TListBox;
    PropertiesGroupBox: TGroupBox;
    FixedColorLabel: TLabel;
    FixedColorComboBox: TColorBox;
    GrayScaleCheckBox: TCheckBox;
    OpacityLabel: TLabel;
    OpacitySpinEdit: TSpinEdit;
    BottomSplitter: TSplitter;
    IconIndexEdit: TEdit;
    IconIndexLabel: TLabel;
    SetCategoriesButton: TButton;
    DeleteAllButton: TButton;
    DeleteButton: TButton;
    CategoryEdit: TEdit;
    CategoryLabel: TLabel;
    SVGErrorStaticText: TStaticText;
    AntiAliasColorLabel: TLabel;
    AntialiasColorComboBox: TColorBox;
    ApplyToRootOnlyCheckBox: TCheckBox;
    ApplyToRootOnlyItemCheckBox: TCheckBox;
    ExportPngButton: TButton;
    DiskBevel: TBevel;
    AddWebButton: TButton;
    ReplaceWebButton: TButton;
    WebBevel: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteAllButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ImageViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExportButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NameEditExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImageViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SizeEditChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure HeightEditChange(Sender: TObject);
    procedure OpacitySpinEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewButtonClick(Sender: TObject);
    procedure SVGTextChange(Sender: TObject);
    procedure GrayScaleCheckBoxClick(Sender: TObject);
    procedure FixedColorComboBoxSelect(Sender: TObject);
    procedure FixedColorItemComboBoxSelect(Sender: TObject);
    procedure AntialiasColorComboBoxSelect(Sender: TObject);
    procedure GrayScaleItemCheckBoxClick(Sender: TObject);
    procedure ReformatXMLButtonClick(Sender: TObject);
    procedure CategoryListBoxClick(Sender: TObject);
    procedure SetCategoriesButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure CategoryEditExit(Sender: TObject);
    procedure SVGTextExit(Sender: TObject);
    procedure SVGTextEnter(Sender: TObject);
    procedure SVGTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExportPngButtonClick(Sender: TObject);
    procedure ApplyToRootOnlyCheckBoxClick(Sender: TObject);
    procedure ApplyToRootOnlyItemCheckBoxClick(Sender: TObject);
    procedure AddWebButtonClick(Sender: TObject);
    procedure ReplaceWebButtonClick(Sender: TObject);
  private
    FOldSVGText: string;
    FOpenDialog: TOpenPictureDialogSvg;
    FSelectedCategory: string;
    FSourceList, FEditingList: TSVGIconImageList;
    FImageCollection: TSVGIconImageCollection;
    {$IFNDEF D10_3+}
    FSVGIconVirtualImageList: TSVGIconVirtualImageList;
    {$ENDIF}
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FChanged: Boolean;
    FModified: Boolean;

    procedure BuildList(Selected: Integer);
    procedure UpdateCategories;
    procedure Apply;
    procedure ApplyToImageCollection;
    {$IFNDEF D10_3+}
    procedure ApplyToSVGIconVirtualImageList;
    {$ENDIF}
    procedure UpdateGUI;
    function SelectedIcon: TSVGIconItem;
    procedure UpdateSizeGUI;
    procedure AddNewItem;
    procedure DeleteSelectedItems;
    procedure ResetError;
  public
    property Modified: Boolean read FModified;
    property SVGIconImageList: TSVGIconImageList read FEditingList;
  end;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;

{$IFNDEF D10_3+}
function EditSVGIconVirtualImageList(const AImageList: TSVGIconVirtualImageList): Boolean;
{$ENDIF}

function EditSVGIconImageCollection(const AImageCollection: TSVGIconImageCollection): Boolean;

implementation

{$R *.dfm}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

uses
  SVGInterfaces
  , System.Types
  , System.IOUtils
  , Winapi.ShellAPI
  , Vcl.FileCtrl
  , Xml.XMLDoc
  , Vcl.Themes
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseSVGEditorsAtRunTime}
  , ToolsAPI
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  , Winapi.CommDlg
  , SVGIconUtils
  {$IFDEF UseRESTClientSearch}
  , SVGRESTClientFormUnit
  {$ENDIF}
  , SVGIconSetFormUnit
  , dlgExportPNG;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  paTopHeight: Integer;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  LEditor := TSVGIconImageListEditor.Create(nil);
  try
    Screen.Cursor := crHourglass;
    try
      LEditor.FSourceList := AImageList;
      LEditor.FEditingList.Assign(AImageList);
      LEditor.UpdateGUI;
    finally
      Screen.Cursor := crDefault;
    end;
    Result := LEditor.ShowModal = mrOk;
    if Result then
    begin
      Screen.Cursor := crHourglass;
      try
        AImageList.Assign(LEditor.FEditingList);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
    SavedBounds := LEditor.BoundsRect;
    paTopHeight := LEditor.paTop.Height;
  finally
    LEditor.Free;
  end;
end;

{$IFNDEF D10_3+}
function EditSVGIconVirtualImageList(const AImageList: TSVGIconVirtualImageList): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  if AImageList.ImageCollection = nil then
    exit(false);
  LEditor := TSVGIconImageListEditor.Create(nil);
  try
    Screen.Cursor := crHourglass;
    try
      LEditor.FSVGIconVirtualImageList := AImageList;
      LEditor.FSourceList := TSVGIconImageList.Create(LEditor);
      LEditor.FSourceList.Assign(AImageList);
      LEditor.FEditingList.Assign(AImageList);
      LEditor.UpdateGUI;
    finally
      Screen.Cursor := crDefault;
    end;
    Result := LEditor.ShowModal = mrOk;
    if Result then
    begin
      Screen.Cursor := crHourglass;
      try
        LEditor.ApplyToSVGIconVirtualImageList;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
    SavedBounds := LEditor.BoundsRect;
    paTopHeight := LEditor.paTop.Height;
  finally
    LEditor.Free;
  end;
end;
{$ENDIF}

function EditSVGIconImageCollection(const AImageCollection: TSVGIconImageCollection): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  LEditor := TSVGIconImageListEditor.Create(nil);
  try
    Screen.Cursor := crHourglass;
    try
      LEditor.FImageCollection := AImageCollection;
      LEditor.FSourceList := TSVGIconImageList.Create(LEditor);
      LEditor.FSourceList.SVGIconItems.Assign(AImageCollection.SVGIconItems);
      LEditor.FSourceList.Size := 64; //Force 64 pixel size for image collection icons
      LEditor.FSourceList.GrayScale := AImageCollection.GrayScale;
      LEditor.FSourceList.FixedColor := AImageCollection.FixedColor;
      LEditor.FSourceList.ApplyFixedColorToRootOnly := AImageCollection.ApplyFixedColorToRootOnly;
      LEditor.FSourceList.AntiAliasColor := AImageCollection.AntiAliasColor;
      LEditor.FSourceList.Opacity := AImageCollection.Opacity;
      LEditor.ImageListGroupBox.Visible := False;
      LEditor.FSourceList.SVGIconItems.Assign(AImageCollection.SVGIconItems);
      LEditor.FEditingList.Assign(LEditor.FSourceList);
      LEditor.UpdateGUI;
    finally
      Screen.Cursor := crDefault;
    end;
    Result := LEditor.ShowModal = mrOk;
    if Result then
    begin
      Screen.Cursor := crHourglass;
      try
        LEditor.ApplyToImageCollection;
      finally
        Screen.Cursor := crDefault;
      end;
    end;
    SavedBounds := LEditor.BoundsRect;
    paTopHeight := LEditor.paTop.Height;
  finally
    LEditor.Free;
  end;
end;

{ TSVGIconImageListEditor }

procedure TSVGIconImageListEditor.UpdateSizeGUI;
var
  LIconPanelSize, LIconWidth, LIconHeight: Integer;
begin
  FUpdating := True;
  try
    Screen.Cursor := crHourGlass;
    SizeEdit.Value := FEditingList.Size;
    WidthEdit.Value := FEditingList.Width;
    HeightEdit.Value := FEditingList.Height;
    LIconPanelSize := IconPanel.Height - (IconPanel.BorderWidth * 2);
    if FEditingList.Width > FEditingList.Height then
    begin
      LIconWidth := LIconPanelSize;
      LIconHeight := Round(LIconWidth * FEditingList.Height / FEditingList.Width);
    end
    else if FEditingList.Width < FEditingList.Height then
    begin
      LIconHeight := LIconPanelSize;
      LIconWidth := Round(LIconHeight * FEditingList.Width / FEditingList.Height);
    end
    else
    begin
      LIconWidth := LIconPanelSize;
      LIconHeight := LIconPanelSize;
    end;
    Iconimage.Margins.SetBounds(
      (LIconPanelSize-LIconWidth) div 2,
      (LIconPanelSize-LIconHeight) div 2,
      (LIconPanelSize-LIconWidth) div 2,
      (LIconPanelSize-LIconHeight) div 2);
  finally
    FUpdating := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.ApplyButtonClick(Sender: TObject);
begin
  Apply;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.ApplyToImageCollection;
begin
  FImageCollection.SVGIconItems.Assign(FEditingList.SVGIconItems);
  FImageCollection.GrayScale := GrayScaleCheckBox.Checked;
  FImageCollection.FixedColor := FixedColorComboBox.Selected;
  FImageCollection.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
  FImageCollection.AntiAliasColor := AntialiasColorComboBox.Selected;
  FImageCollection.Opacity := OpacitySpinEdit.Value;
end;

procedure TSVGIconImageListEditor.ApplyToRootOnlyCheckBoxClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Screen.Cursor := crHourGlass;
  try
    FEditingList.ApplyFixedColorToRootOnly := ApplyToRootOnlyCheckBox.Checked;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.ApplyToRootOnlyItemCheckBoxClick(
  Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.ApplyFixedColorToRootOnly := ApplyToRootOnlyItemCheckBox.Checked;
  UpdateGUI;
end;

{$IFNDEF D10_3+}
procedure TSVGIconImageListEditor.ApplyToSVGIconVirtualImageList;
begin
  FSVGIconVirtualImageList.ImageCollection.SVGIconItems.Assign(FEditingList.SVGIconItems);
  FSVGIconVirtualImageList.Assign(FEditingList);
end;
{$ENDIF}

procedure TSVGIconImageListEditor.AddButtonClick(Sender: TObject);
begin
  FOpenDialog.Options := FOpenDialog.Options + [ofAllowMultiSelect];
  if FOpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FEditingList.SvgIconItems.LoadFromFiles(FOpenDialog.Files);
      BuildList(MaxInt);
      FChanged := True;
      UpdateGUI;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TSVGIconImageListEditor.UpdateCategories;
var
  I: Integer;
  LCategory: string;
begin
  CategoryListBox.Items.Clear;
  CategoryListBox.AddItem('All', nil);
  for I := 0 to FEditingList.SVGIconItems.Count -1 do
  begin
    LCategory := FEditingList.SVGIconItems[I].Category;
    if (LCategory <> '') and (CategoryListBox.Items.IndexOf(LCategory)<0) then
      CategoryListBox.AddItem(LCategory,nil);
  end;
  if (FSelectedCategory <> '') then
  begin
    I := CategoryListBox.Items.IndexOf(FSelectedCategory);
    if I >= 0 then
      CategoryListBox.Selected[I] := True;
  end
  else
    CategoryListBox.Selected[0] := True;
end;

procedure TSVGIconImageListEditor.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LIconItem: TSVGIconItem;
begin
  FUpdating := True;
  try
    UpdateCategories;
    LIconItem := SelectedIcon;
    LIsItemSelected := LIconItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    ExportButton.Enabled := LIsItemSelected;
    DeleteAllButton.Enabled := LIsItemSelected;
    DeleteButton.Enabled := LIsItemSelected;
    ExportPngButton.Enabled := LIsItemSelected;
    ReformatXMLButton.Enabled := LIsItemSelected;
    ReplaceButton.Enabled := LIsItemSelected;
    ReplaceWebButton.Enabled := LIsItemSelected;
    SetCategoriesButton.Enabled := LIsItemSelected;
    ApplyButton.Enabled := FChanged;
    NameEdit.Enabled := LIsItemSelected;
    CategoryEdit.Enabled := LIsItemSelected;
    FixedColorItemComboBox.Enabled := LIsItemSelected;
    ApplyToRootOnlyItemCheckBox.Enabled := LIsItemSelected;
    GrayScaleItemCheckBox.Enabled := LIsItemSelected;
    IconIndexEdit.Enabled := LIsItemSelected;
    SVGText.Enabled := LIsItemSelected;
    ImageListGroup.Caption := Format(FTotIconsLabel, [FEditingList.Count]);
    GrayScaleCheckBox.Checked := SVGIconImageList.GrayScale;
    FixedColorComboBox.Selected := SVGIconImageList.FixedColor;
    ApplyToRootOnlyCheckBox.Checked := SVGIconImageList.ApplyFixedColorToRootOnly;
    AntialiasColorComboBox.Selected := SVGIconImageList.AntiAliasColor;
    OpacitySpinEdit.Value := SVGIconImageList.Opacity;
    if LIsItemSelected then
    begin
      IconImage.ImageIndex := SelectedIcon.Index;
      IconImage.Opacity := SVGIconImageList.Opacity;
      IconImage.FixedColor := SelectedIcon.FixedColor;
      IconImage.ApplyFixedColorToRootOnly := SelectedIcon.ApplyFixedColorToRootOnly;
      IconImage.GrayScale := SVGIconImageList.GrayScale or SelectedIcon.GrayScale;
      NameEdit.Text := LIconItem.Name;
      CategoryEdit.Text := LIconItem.Category;
      IconIndexEdit.Text := IntToStr(LIconItem.Index);
      SVGText.Lines.Text := LIconItem.SVGText;
      FixedColorItemComboBox.Selected := LIconItem.FixedColor;
      ApplyToRootOnlyItemCheckBox.Checked := LIconItem.ApplyFixedColorToRootOnly;
      GrayScaleItemCheckBox.Checked := SelectedIcon.GrayScale;
    end
    else
    begin
      IconImage.ImageIndex := -1;
      NameEdit.Text := '';
      SVGText.Lines.Text := '';
      CategoryEdit.Text := '';
      FixedColorItemComboBox.Selected := clDefault;
      ApplyToRootOnlyItemCheckBox.Checked := False;
      GrayScaleItemCheckBox.Checked := False;
      IconIndexEdit.Text := '';
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TSVGIconImageListEditor.WidthEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Width := WidthEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.ImageViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LTargetItem: TListItem;
  LItem: TCollectionItem;
  SIndex, DIndex: Integer;
begin
  LTargetItem := ImageView.GetItemAt(X, Y);

  if not Assigned(LTargetItem) then
    LTargetItem := ImageView.GetNearestItem(Point(X, Y), sdRight);

  if Assigned(LTargetItem) then
    DIndex := LTargetItem.ImageIndex
  else
    DIndex := ImageView.Items.Count - 1;

  SIndex := ImageView.Items[ImageView.ItemIndex].ImageIndex;
  LItem := FEditingList.SVGIconItems[SIndex];
  LItem.Index := DIndex;
  BuildList(LItem.Index);
  if SIndex <> DIndex then
    FChanged := True;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.ImageViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Sender;
end;

procedure TSVGIconImageListEditor.ImageViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.AddNewItem;
begin
  FEditingList.SVGIconItems.Add;
  FChanged := True;
  BuildList(MaxInt);
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.NewButtonClick(Sender: TObject);
begin
  AddNewItem;
end;

procedure TSVGIconImageListEditor.CategoryListBoxClick(Sender: TObject);
var
  LIndex: Integer;
begin
  if SelectedIcon <> nil then
    LIndex := SelectedIcon.Index
  else
    LIndex := -1;  
  if CategoryListBox.ItemIndex <= 0 then
    FSelectedCategory := ''
  else
    FSelectedCategory := CategoryListBox.Items[CategoryListBox.ItemIndex];
  BuildList(LIndex);
end;

procedure TSVGIconImageListEditor.ClearAllButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ImageView.Clear;
    FEditingList.ClearIcons;
    FChanged := True;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.ReformatXMLButtonClick(Sender: TObject);
begin
  SVGText.Lines.Text := Xml.XMLDoc.FormatXMLData(SVGText.Lines.Text);
end;

procedure TSVGIconImageListEditor.ReplaceButtonClick(Sender: TObject);
var
  FileName: string;
  Item: TSVGIconItem;
begin
  FOpenDialog.Options := FOpenDialog.Options - [ofAllowMultiSelect];
  if FOpenDialog.Execute then
  begin
    FEditingList.BeginUpdate;
    try
      Screen.Cursor := crHourGlass;
      if FOpenDialog.Files.Count > 0 then
      begin
        Item := FEditingList.SVGIconItems[ImageView.ItemIndex];
        FileName := ChangeFileExt(ExtractFileName(FOpenDialog.Files[0]), '');
        Item.SVG.LoadFromFile(FOpenDialog.Files[0]);
        Item.IconName := FileName;
        FChanged := True;
      end;
      BuildList(ImageView.Items[ImageView.ItemIndex].ImageIndex);
    finally
      FEditingList.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TSVGIconImageListEditor.ReplaceWebButtonClick(Sender: TObject);
begin
  {$IFDEF UseRESTClientSearch}
  FEditingList.BeginUpdate;
  try
    Screen.Cursor := crHourGlass;
    if SearchSVGIconsFromWeb(FEditingList, ImageView.ItemIndex) then
    begin
      FChanged := True;
      BuildList(ImageView.Items[ImageView.ItemIndex].ImageIndex);
    end;
  finally
    FEditingList.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF}
end;

procedure TSVGIconImageListEditor.AddWebButtonClick(Sender: TObject);
begin
  {$IFDEF UseRESTClientSearch}
  FEditingList.BeginUpdate;
  try
    if SearchSVGIconsFromWeb(FEditingList) then
    begin
      BuildList(MaxInt);
      FChanged := True;
      UpdateGUI;
    end;
  finally
    FEditingList.EndUpdate;
  end;
  {$ENDIF}
end;

procedure TSVGIconImageListEditor.ImageViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_INSERT) and (Shift = []) then
    AddNewItem
  else if (Key = VK_DELETE) and (Shift = []) then
    DeleteSelectedItems;
end;

function TSVGIconImageListEditor.SelectedIcon: TSVGIconItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.ImageIndex < FEditingList.SVGIconItems.Count) then
    Result := FEditingList.SVGIconItems[ImageView.Selected.ImageIndex]
  else
    Result := nil;
end;

procedure TSVGIconImageListEditor.SetCategoriesButtonClick(Sender: TObject);
var
  LIndex: Integer;
  Selected: Integer;
  LSVGIconItem: TSVGIconItem;
  LCategoryName: string;
begin
  if SVGPropInputCategory(Self, LCategoryName) and
    (LCategoryName <> FSelectedCategory) then
  begin
    Screen.Cursor := crHourGlass;
    try
      FSelectedCategory := LCategoryName;
      Selected := ImageView.ItemIndex;
      FEditingList.BeginUpdate;
      try
        for LIndex := ImageView.Items.Count - 1 downto 0 do
        begin
          if ImageView.Items[LIndex].Selected then
          begin
            LSVGIconItem := FEditingList.SVGIconItems[ImageView.Items[LIndex].ImageIndex];
            LSVGIconItem.Category := FSelectedCategory;
          end;
        end;
      finally
        FEditingList.EndUpdate;
      end;
      FChanged := True;
      BuildList(Selected);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TSVGIconImageListEditor.SizeEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Size := SizeEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.SVGTextChange(Sender: TObject);
var
  LOldText: string;
begin
  if FUpdating then Exit;
  LOldText := SelectedIcon.SVGText;
  try
    SelectedIcon.SVGText := SVGText.Lines.Text;
    ResetError;
    IconImage.Repaint;
    UpdateGUI;
  except
    on E: Exception do
    begin
      SVGErrorStaticText.Font.Color := clRed;
      SVGErrorStaticText.Font.Style := [fsBold];
      SVGErrorStaticText.Caption := E.Message;
      SVGErrorStaticText.Hint := E.Message;
      SelectedIcon.SVGText := '';
      FEditingList.RecreateBitmaps;
      IconImage.Repaint;
      ImageView.Invalidate;
    end;
  end;
end;

procedure TSVGIconImageListEditor.ResetError;
begin
  SVGErrorStaticText.Font.Color := clWindowText;
  SVGErrorStaticText.Caption := Format(USING_ENGINE, [GetGlobalSVGFactoryDesc]);
  SVGErrorStaticText.Hint := ENGINE_HINT;
end;

procedure TSVGIconImageListEditor.SVGTextEnter(Sender: TObject);
begin
  FOldSVGText := SVGText.Lines.Text;
end;

procedure TSVGIconImageListEditor.SVGTextExit(Sender: TObject);
begin
  ResetError;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.SVGTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  LSelStart: Integer;
begin
  if (Key = VK_ESCAPE) or ((upcase(Char(Key)) = 'Z') and (ssCtrl in Shift)) then
  begin
    LSelStart := SVGText.SelStart;
    SVGText.Lines.Text := FOldSVGText;
    SVGText.SelStart := LSelStart;
    SVGText.SelLength := 0;
    SelectedIcon.SVGText := FOldSVGText;
  end;
end;

procedure TSVGIconImageListEditor.BuildList(Selected: Integer);
begin
  FEditingList.BeginUpdate;
  try
    UpdateSVGIconListView(ImageView, FSelectedCategory);
  finally
    FEditingList.EndUpdate;
  end;

  if Selected < -1 then
    Selected := -1
  else if Selected >= ImageView.Items.Count then
    Selected := ImageView.Items.Count - 1;

  ImageView.ItemIndex := Selected;
  UpdateGUI;
  if (ImageView.ItemIndex >= 0) and ImageView.CanFocus then
    ImageView.SetFocus;
end;

procedure TSVGIconImageListEditor.DeleteSelectedItems;
var
  LIndex: Integer;
  LSelectedImageIndex: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    LSelectedImageIndex := ImageView.Items[ImageView.ItemIndex].ImageIndex;
    FEditingList.BeginUpdate;
    try
      for LIndex := ImageView.Items.Count - 1 downto 0 do
        if ImageView.Items[LIndex].Selected then
          FEditingList.SVGIconItems.Delete(ImageView.Items[LIndex].ImageIndex);
    finally
      FEditingList.EndUpdate;
    end;
    FChanged := True;
    BuildList(LSelectedImageIndex);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.DeleteAllButtonClick(Sender: TObject);
begin
  DeleteSelectedItems;
end;

procedure TSVGIconImageListEditor.DeleteButtonClick(Sender: TObject);
var
  LSelectedImageIndex: Integer;
begin
  FEditingList.BeginUpdate;
  try
    LSelectedImageIndex := ImageView.Items[ImageView.ItemIndex].ImageIndex;
    FEditingList.SVGIconItems.Delete(ImageView.Items[ImageView.ItemIndex].ImageIndex);
  finally
    FEditingList.EndUpdate;
  end;
  FChanged := True;
  BuildList(LSelectedImageIndex);
end;

procedure TSVGIconImageListEditor.FixedColorComboBoxSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if FixedColorComboBox.ItemIndex >= 0 then
    begin
      FEditingList.FixedColor := FixedColorComboBox.Selected;
      UpdateGUI;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.AntialiasColorComboBoxSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if AntialiasColorComboBox.ItemIndex >= 0 then
    begin
      FEditingList.AntiAliasColor := AntialiasColorComboBox.Selected;
      UpdateGUI;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.FixedColorItemComboBoxSelect(Sender: TObject);
begin
  if FUpdating then Exit;
  if FixedColorItemComboBox.ItemIndex >= 0 then
  begin
    SelectedIcon.FixedColor := FixedColorItemComboBox.Selected;
    UpdateGUI;
  end;
end;

procedure TSVGIconImageListEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus
  else
    CancelButton.SetFocus;
end;

procedure TSVGIconImageListEditor.FormCreate(Sender: TObject);
{$IFNDEF UseSVGEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
  var
    LStyle: TCustomStyleServices;
  {$IFEND}
{$ENDIF}
begin
{$IFNDEF UseSVGEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
    {$IF (CompilerVersion <= 34.0)}
    if UseThemeFont then
      Self.Font.Assign(GetThemeFont);
    {$IFEND}
    //{$IF CompilerVersion > 34.0}
    //if TIDEThemeMetrics.Font.Enabled then
    //  Self.Font.Assign(TIDEThemeMetrics.Font.GetFont);
    //{$IFEND}

    if IDEThemeAvailable then
    begin
      LStyle := ThemeProperties.StyleServices;
      StyleElements := StyleElements - [seClient];
      Color := LStyle.GetSystemColor(clWindow);
      BottomPanel.StyleElements := BottomPanel.StyleElements - [seClient];
      BottomPanel.ParentBackground := False;
      BottomPanel.Color := LStyle.GetSystemColor(clBtnFace);
      IDEThemeManager.RegisterFormClass(TSVGIconImageListEditor);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}

  {$IFNDEF UseRESTClientSearch}
  ReplaceWebButton.Visible := False;
  AddWebButton.Visible := False;
  WebBevel.Visible := False;
  {$ENDIF}

  FUpdating := False;
  ResetError;
  FEditingList := TSVGIconImageList.Create(Self);
  FOpenDialog := TOpenPictureDialogSvg.Create(Self);
  FOpenDialog.Filter := 'Scalable Vector Graphics (*.svg)|*.svg';
  FOpenDialog.Options := [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing];
  ImageView.LargeImages := FEditingList;
  IconImage.ImageList := FEditingList;
  FTotIconsLabel := ImageListGroup.Caption;
  FChanged := False;
  FModified := False;
  SVGText.Font.Name := 'Consolas';
  Caption := Format(Caption, [SVGIconImageListVersion]);
end;

procedure TSVGIconImageListEditor.Apply;
begin
  if not FChanged then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    if Assigned(FImageCollection) then
      ApplyToImageCollection
    {$IFNDEF D10_3+}
    else if Assigned(FSVGIconVirtualImageList) then
      ApplyToSVGIconVirtualImageList
    {$ENDIF}
    else
      FSourceList.Assign(FEditingList);
    FChanged := False;
    FModified := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.OkButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TSVGIconImageListEditor.OpacitySpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Opacity := OpacitySpinEdit.Value;
end;

procedure TSVGIconImageListEditor.ExportButtonClick(Sender: TObject);
var
  LOutputPath: string;

  procedure SaveIconsToFiles(const AOutDir: string);
  var
    I, C: Integer;
    LItem: TSVGIconItem;
    LFileName: string;
  begin
    Screen.Cursor := crHourGlass;
    try
      System.SysUtils.ForceDirectories(AOutDir);
      C := 0;
      for I := 0 to ImageView.Items.Count-1 do
      begin
        if ImageView.Items[I].Selected then
        begin
          LItem := FEditingList.SVGIconItems.Items[I];
          if LItem.IconName <> '' then
            LFileName := AOutDir+LItem.IconName+'.svg'
          else
            LFileName := AOutDir+IntToStr(I)+'.svg';
          LItem.SVG.SaveToFile(LFileName);
          Inc(C);
        end;
      end;
      ShowMessageFmt(FILES_SAVED, [C, AOutDir]);

    finally
      Screen.Cursor := crDefault;
    end;
  end;

begin
  LOutputPath := ExtractFilePath(FOpenDialog.FileName);
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := SELECT_DIR;
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
        DefaultFolder := LOutputPath;
        FileName := LOutputPath;
        if Execute then
          SaveIconsToFiles(IncludeTrailingPathDelimiter(FileName));
      finally
        Free;
      end
  else
    if SelectDirectory(SELECT_DIR,
      ExtractFileDrive(LOutputPath), LOutputPath,
      [sdNewUI, sdNewFolder]) then
    SaveIconsToFiles(IncludeTrailingPathDelimiter(LOutputPath));
end;

procedure TSVGIconImageListEditor.ExportPngButtonClick(Sender: TObject);
var
  LOutputPath: string;
  LPngSize, LPngWidth, LPngHeight: Integer;

  procedure SaveIconsToFiles(const AOutDir: string);
  var
    I, C: Integer;
    LItem: TSVGIconItem;
    LFileName: string;
  begin
    Screen.Cursor := crHourGlass;
    try
      if not System.SysUtils.DirectoryExists(AOutDir) then
        System.SysUtils.ForceDirectories(AOutDir);
      C := 0;
      for I := 0 to ImageView.Items.Count-1 do
      begin
        if ImageView.Items[I].Selected then
        begin
          LItem := FEditingList.SVGIconItems.Items[I];
          if LItem.IconName <> '' then
            LFileName := AOutDir+LItem.IconName+'.png'
          else
            LFileName := AOutDir+IntToStr(I)+'.png';
          LOutputPath := ExtractFilePath(LFileName);
          if not System.SysUtils.DirectoryExists(LOutputPath) then
            System.SysUtils.ForceDirectories(LOutputPath);

          //Apply "root" attributes to the item SVG Interface
          LItem.ApplyAttributesToInterface(FixedColorComboBox.Selected,
            ApplyToRootOnlyCheckBox.Checked,
            OpacitySpinEdit.Value,
            GrayScaleCheckBox.Checked);

          //Export image by the Interface
          SVGExportToPng(LPngWidth, LPngHeight,
            LItem.SVG, LOutputPath, ExtractFileName(LFileName));
          Inc(C);
        end;
      end;
      ShowMessageFmt(FILES_SAVED, [C, AOutDir]);

    finally
      Screen.Cursor := crDefault;
    end;
  end;

begin
  LPngSize := FEditingList.Size;
  LPngWidth := FEditingList.Width;
  LPngHeight := FEditingList.Height;
  if not SVGPropInputPngSize(Self, LPngSize, LPngWidth, LPngHeight) then
    Exit;

  LOutputPath := ExtractFilePath(FOpenDialog.FileName);
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := SELECT_DIR;
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
        DefaultFolder := LOutputPath;
        FileName := LOutputPath;
        if Execute then
          SaveIconsToFiles(IncludeTrailingPathDelimiter(FileName));
      finally
        Free;
      end
  else
    if SelectDirectory(SELECT_DIR,
      ExtractFileDrive(LOutputPath), LOutputPath,
      [sdNewUI, sdNewFolder]) then
    SaveIconsToFiles(IncludeTrailingPathDelimiter(LOutputPath));
end;

procedure TSVGIconImageListEditor.FormDestroy(Sender: TObject);
begin
  FEditingList.Free;
end;

procedure TSVGIconImageListEditor.NameEditExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.Name := NameEdit.Text;
  UpdateSVGIconListViewCaptions(ImageView);
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.CategoryEditExit(Sender: TObject);
begin
  if FUpdating then Exit;
  if SelectedIcon.Category <> CategoryEdit.Text then
  begin
    SelectedIcon.Category := CategoryEdit.Text;
    if FSelectedCategory <> SelectedIcon.Category then
    begin
      FSelectedCategory := SelectedIcon.Category;
      UpdateCategories;
      BuildList(SelectedIcon.Index);
    end
    else
      UpdateSVGIconListViewCaptions(ImageView);
  end;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.FormShow(Sender: TObject);
begin
  UpdateSizeGUI;

  BuildList(0);

  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);

  if paTopHeight <> 0 then
    paTop.Height := paTopHeight;

  if ImageView.CanFocus then
    ImageView.SetFocus;
end;

procedure TSVGIconImageListEditor.GrayScaleCheckBoxClick(Sender: TObject);
begin
  if FUpdating then Exit;
  Screen.Cursor := crHourGlass;
  try
    FEditingList.GrayScale := GrayScaleCheckBox.Checked;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.GrayScaleItemCheckBoxClick(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.GrayScale := GrayScaleItemCheckBox.Checked;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.HeightEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Height := HeightEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/Component-Editor-(VCL).html'), nil, nil,
    SW_SHOWNORMAL)
end;

{ TOpenPictureDialogSvg }

function TOpenPictureDialogSvg.Execute(ParentWnd: HWND): Boolean;
begin
  {$IFDEF OldPictureDialog}
  Template := 'DLGTEMPLATE';
  Result := DoExecute(@GetOpenFileName, ParentWnd);
  {$ELSE}
  Result := inherited Execute(ParentWnd);
  {$ENDIF}
end;

procedure TOpenPictureDialogSvg.DoSelectionChange;
begin
  ImageCtrl.Picture := nil;
  inherited;
end;

initialization
  paTopHeight := 0;

end.
