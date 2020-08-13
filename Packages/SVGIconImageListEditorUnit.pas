{******************************************************************************}
{                                                                              }
{       SVGIconImageList Component Editor                                      }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconsImageList                          }
{                                                                              }
{******************************************************************************}
{       Original version (c) 2005, 2008 Martin Walter with license:            }
{       Use of this file is permitted for commercial and non-commercial        }
{       use, as long as the author is credited.                                }
{       home page: http://www.mwcs.de                                          }
{       email    : martin.walter@mwcs.de                                       }
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
  Windows
  , Messages
  , SysUtils
  , Graphics
  , Forms
  , StdCtrls
  , ExtCtrls
  , Controls
  , Classes
  , Dialogs
  , ComCtrls
  , ImgList
  , ExtDlgs
  , Spin
  , SVGIconImage
  , SVGIconImageListBase
  , SVGIconImageList
  , SVGIconImageCollection
  , SVGIconVirtualImageList
  ;

resourcestring
  SELECT_DIR = 'Select directory';
  FILES_SAVED = '%d File(s) saved into "%s" folder';
  NOT_APPLYED_TO_COLLECTION = '(not applied to image collection)';
type
  TSVGIconImageListEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ImageListGroup: TGroupBox;
    paTop: TPanel;
    paClient: TPanel;
    SVGText: TMemo;
    SizeLabel: TLabel;
    SizeSpinEdit: TSpinEdit;
    StoreAsTextCheckBox: TCheckBox;
    ItemGroupBox: TGroupBox;
    IconNameLabel: TLabel;
    IconPanel: TPanel;
    IconImage: TSVGIconImage;
    AddButton: TButton;
    IconName: TEdit;
    paButtons: TPanel;
    ImageListGroupBox: TGroupBox;
    HelpButton: TButton;
    ApplyButton: TButton;
    WidthLabel: TLabel;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    HeightLabel: TLabel;
    BottomPanel: TPanel;
    DeleteButton: TButton;
    ClearAllButton: TButton;
    ExportButton: TButton;
    ReplaceButton: TButton;
    ImageView: TListView;
    OpacityLabel: TLabel;
    OpacitySpinEdit: TSpinEdit;
    NewButton: TButton;
    TopSplitter: TSplitter;
    FixedColorComboBox: TComboBox;
    FixedColorLabel: TLabel;
    GrayScaleCheckBox: TCheckBox;
    Label1: TLabel;
    FixedColorItemComboBox: TComboBox;
    GrayScaleItemCheckBox: TCheckBox;
    ReformatXMLButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ImageViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExportButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IconNameExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImageViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ReplaceButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure SizeSpinEditChange(Sender: TObject);
    procedure WidthSpinEditChange(Sender: TObject);
    procedure HeightSpinEditChange(Sender: TObject);
    procedure OpacitySpinEditChange(Sender: TObject);
    procedure StoreAsTextCheckBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewButtonClick(Sender: TObject);
    procedure SVGTextChange(Sender: TObject);
    procedure GrayScaleCheckBoxClick(Sender: TObject);
    procedure FixedColorComboBoxSelect(Sender: TObject);
    procedure FixedColorItemComboBoxSelect(Sender: TObject);
    procedure GrayScaleItemCheckBoxClick(Sender: TObject);
    procedure ReformatXMLButtonClick(Sender: TObject);
  private
    FSourceList, FEditingList: TSVGIconImageList;
    FIconIndexLabel: string;
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FChanged: Boolean;
    FModified: Boolean;

    procedure BuildList(Selected: Integer);
    procedure Apply;
    procedure UpdateGUI;
    function SelectedIcon: TSVGIconItem;
    procedure UpdateSizeGUI;
    procedure AddNewItem;
    procedure DeleteSelectedItem;
  public
    property Modified: Boolean read FModified;
    property SVGIconImageList: TSVGIconImageList read FEditingList;
  end;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;

function EditSVGIconVirtualImageList(const AImageList: TSVGIconVirtualImageList): Boolean;

function EditSVGIconImageCollection(const AImageCollection: TSVGIconImageCollection): Boolean;

implementation

{$R *.dfm}

uses
  SVG
  , SVGColor
  , Types
  , ShellApi
  , FileCtrl
  , XMLDoc
  , SVGIconUtils;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  paTopHeight: Integer;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  LEditor := TSVGIconImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := AImageList;
        FEditingList.Assign(AImageList);
        OpacitySpinEdit.Value := FSourceList.Opacity;
        StoreAsTextCheckBox.Checked := FSourceList.StoreAsText;
        UpdateGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageList.Assign(LEditor.FEditingList);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
      paTopHeight := paTop.Height;
    finally
      Free;
    end;
  end;
end;

function EditSVGIconVirtualImageList(const AImageList: TSVGIconVirtualImageList): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  if AImageList.Collection = nil then
    exit(false);
  LEditor := TSVGIconImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := TSVGIconImageList.Create(LEditor);
        FSourceList.Assign(AImageList);
        FEditingList.Assign(AImageList);
        OpacitySpinEdit.Value := FSourceList.Opacity;
        StoreAsTextCheckBox.Checked := AImageList.Collection.StoreAsText;
        OpacitySpinEdit.Value := AImageList.Opacity;
        StoreAsTextCheckBox.Checked := FSourceList.StoreAsText;
        UpdateGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageList.Collection.SVGIconItems.Assign(LEditor.FEditingList.SVGIconItems);
          AImageList.Collection.StoreAsText := StoreAsTextCheckBox.Checked;
          AImageList.Assign(LEditor.FEditingList);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
      paTopHeight := paTop.Height;
    finally
      Free;
    end;
  end;
end;

function EditSVGIconImageCollection(const AImageCollection: TSVGIconImageCollection): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  LEditor := TSVGIconImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := TSVGIconImageList.Create(LEditor);
        FSourceList.SVGIconItems.Assign(AImageCollection.SVGIconItems);
        FEditingList.Size := 32; //Force 32 pixel size for image collection icons
        ImageListGroupBox.Caption := ImageListGroupBox.Caption + ' ' + NOT_APPLYED_TO_COLLECTION;
        FEditingList.SVGIconItems.Assign(AImageCollection.SVGIconItems);
        OpacitySpinEdit.Value := FSourceList.Opacity;
        StoreAsTextCheckBox.Checked := AImageCollection.StoreAsText;
        UpdateGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageCollection.SVGIconItems.Assign(LEditor.FEditingList.SVGIconItems);
          AImageCollection.StoreAsText := StoreAsTextCheckBox.Checked;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
      paTopHeight := paTop.Height;
    finally
      Free;
    end;
  end;

end;


{ TSVGIconImageListEditor }

procedure TSVGIconImageListEditor.UpdateSizeGUI;
begin
  SizeSpinEdit.Value := FEditingList.Size;
  WidthSpinEdit.Value := FEditingList.Width;
  HeightSpinEdit.Value := FEditingList.Height;
end;

procedure TSVGIconImageListEditor.ApplyButtonClick(Sender: TObject);
begin
  Apply;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.AddButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FEditingList.SvgIconItems.LoadFromFiles(OpenDialog.Files);
      BuildList(MaxInt);
      FChanged := True;
      UpdateGUI;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TSVGIconImageListEditor.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LIconItem: TSVGIconItem;
begin
  FUpdating := True;
  try
    LIconItem := SelectedIcon;
    LIsItemSelected := LIconItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    ExportButton.Enabled := LIsItemSelected;
    DeleteButton.Enabled := LIsItemSelected;
    ReformatXMLButton.Enabled := LIsItemSelected;
    ReplaceButton.Enabled := LIsItemSelected;
    ApplyButton.Enabled := FChanged;
    IconName.Enabled := LIsItemSelected;
    SVGText.Enabled := LIsItemSelected;
    ImageListGroup.Caption := Format(FTotIconsLabel, [FEditingList.Count]);
    GrayScaleCheckBox.Checked := SVGIconImageList.GrayScale;
    FixedColorComboBox.ItemIndex := FixedColorComboBox.Items.IndexOf(SVGColorToSVGColorName(SVGIconImageList.FixedColor));
    if LIsItemSelected then
    begin
      IconImage.ImageIndex := SelectedIcon.Index;
      IconImage.Invalidate;
      ItemGroupBox.Caption := Format(FIconIndexLabel,[LIconItem.Index]);
      IconName.Text := LIconItem.IconName;
      SVGText.Lines.Text := LIconItem.SVGText;
      FixedColorItemComboBox.ItemIndex :=
        FixedColorItemComboBox.Items.IndexOf(SVGColorToSVGColorName(SelectedIcon.FixedColor));
      GrayScaleItemCheckBox.Checked := SelectedIcon.GrayScale;
    end
    else
    begin
      IconImage.ImageIndex := -1;
      ItemGroupBox.Caption := '';
      IconName.Text := '';
      SVGText.Lines.Text := '';
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TSVGIconImageListEditor.WidthSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Width := WidthSpinEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.ImageViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Target: TListItem;
  Item: TCollectionItem;
  SIndex, DIndex: Integer;
begin
  SIndex := ImageView.ItemIndex;
  Target := ImageView.GetItemAt(X, Y);
  if Target = nil then
    Target := ImageView.GetNearestItem(Point(X, Y), sdRight);

  if Assigned(Target) then
    DIndex := ImageView.Items.IndexOf(Target)
  else
    DIndex := ImageView.Items.Count - 1;

  Item := FEditingList.SVGIconItems[SIndex];
  Item.Index := DIndex;
  BuildList(Item.Index);
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
  SVGText.Lines.Text := xmlDoc.FormatXMLData(SVGText.Lines.Text);
end;

procedure TSVGIconImageListEditor.ReplaceButtonClick(Sender: TObject);
var
  LIndex: Integer;
  SVG: TSVG;
  FileName: string;
  Item: TSVGIconItem;
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    FEditingList.BeginUpdate;
    try
      SVG := TSVG.Create;
      try
        for LIndex := 0 to OpenDialog.Files.Count - 1 do
        begin
          FileName := ChangeFileExt(ExtractFileName(OpenDialog.Files[LIndex]), '');
          try
            SVG.LoadFromFile(OpenDialog.Files[LIndex]);
            Item := FEditingList.SVGIconItems[ImageView.ItemIndex];
            Item.IconName := FileName;
            Item.SVG := SVG;
            FChanged := True;
          finally
          end;
        end;
      finally
        SVG.Free;
      end;
      BuildList(ImageView.ItemIndex);
    finally
      FEditingList.EndUpdate;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TSVGIconImageListEditor.ImageViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_INSERT) and (Shift = []) then
    AddNewItem
  else if (Key = VK_DELETE) and (Shift = []) then
    DeleteSelectedItem;
end;

function TSVGIconImageListEditor.SelectedIcon: TSVGIconItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.SVGIconItems.Count) then
    Result := FEditingList.SVGIconItems[ImageView.Selected.Index]
  else
    Result := nil;
end;

procedure TSVGIconImageListEditor.SizeSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Size := SizeSpinEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.StoreAsTextCheckBoxClick(Sender: TObject);
begin
  FEditingList.StoreAsText := StoreAsTextCheckBox.Checked;
end;

procedure TSVGIconImageListEditor.SVGTextChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.SVGText := SVGText.Lines.Text;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.BuildList(Selected: Integer);
begin
  FEditingList.BeginUpdate;
  try
    UpdateSVGIconListView(ImageView);
  finally
    FEditingList.EndUpdate;
  end;

  if Selected < -1 then
    Selected := -1;
  if Selected >= FEditingList.SVGIconItems.Count then
    Selected := FEditingList.SVGIconItems.Count - 1;

  ImageView.ItemIndex := Selected;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.DeleteSelectedItem;
var
  LIndex: Integer;
  Selected: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Selected := ImageView.ItemIndex;
    FEditingList.BeginUpdate;
    try
      for LIndex := ImageView.Items.Count - 1 downto 0 do
        if ImageView.Items[LIndex].Selected then
          FEditingList.SVGIconItems.Delete(LIndex);
    finally
      FEditingList.EndUpdate;
    end;
    FChanged := True;
    BuildList(Selected);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TSVGIconImageListEditor.FixedColorComboBoxSelect(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FEditingList.FixedColor := SVGColorNameToSVGColor(FixedColorComboBox.Text);
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.FixedColorItemComboBoxSelect(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.FixedColor := SVGColorNameToSVGColor(FixedColorItemComboBox.Text);
  UpdateGUI;
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
begin
  inherited;
  FEditingList := TSVGIconImageList.Create(Self);
  AssignSVGColorList(FixedColorComboBox.Items);
  AssignSVGColorList(FixedColorItemComboBox.Items);
  ImageView.LargeImages := FEditingList;
  IconImage.ImageList := FEditingList;
  FIconIndexLabel := ItemGroupBox.Caption;
  FTotIconsLabel := ImageListGroup.Caption;
  FChanged := False;
  FModified := False;
  SVGText.Font.Name := 'Courier New';
  Caption := Format(Caption, [SVGIconImageListVersion]);
end;

procedure TSVGIconImageListEditor.Apply;
begin
  if not FChanged then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FSourceList.BeginUpdate;
    Try
      FSourceList.Assign(FEditingList);
      FChanged := False;
      FModified := True;
    Finally
      FSourceList.EndUpdate;
    End;
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
  FDir: string;

  procedure SaveIconsToFiles(const AOutDir: string);
  var
    I, C: Integer;
    LItem: TSVGIconItem;
    LFileName: string;
  begin
    Screen.Cursor := crHourGlass;
    try
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
  FDir := ExtractFilePath(OpenDialog.FileName);
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := SELECT_DIR;
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
        DefaultFolder := FDir;
        FileName := FDir;
        if Execute then
          SaveIconsToFiles(IncludeTrailingPathDelimiter(FileName));
      finally
        Free;
      end
  else
    if SelectDirectory(SELECT_DIR,
      ExtractFileDrive(FDir), FDir,
      [sdNewUI, sdNewFolder]) then
    SaveIconsToFiles(IncludeTrailingPathDelimiter(FDir));
end;

procedure TSVGIconImageListEditor.FormDestroy(Sender: TObject);
begin
  FEditingList.Free;
end;

procedure TSVGIconImageListEditor.IconNameExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.IconName := IconName.Text;
  UpdateSVGIconListViewCaptions(ImageView);
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

procedure TSVGIconImageListEditor.HeightSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Height := HeightSpinEdit.Value;
  UpdateSizeGUI;
end;

procedure TSVGIconImageListEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Image-Editor'), nil, nil,
    SW_SHOWNORMAL)
end;

initialization
  paTopHeight := 0;

end.
