{******************************************************************************}
{                                                                              }
{       SVGIconImageList Component Editor                                      }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
  , SVGIconImageList
  ;

type
  TSVGIconImageListEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    paTop: TPanel;
    paClient: TPanel;
    SVGText: TMemo;
    ImageListGroupBox: TGroupBox;
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
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
    procedure SVGTextExit(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
  private
    FSourceList, FEditingList: TSVGIconImageList;
    FIconIndexLabel: string;
    FUpdating: Boolean;
    FChanged: Boolean;
    FModified: Boolean;

    procedure BuildList(Selected: Integer);
    procedure Apply;
    procedure UpdateGUI;
    function SelectedIcon: TSVGIconItem;
    procedure UpdateSizeGUI;
  public
    constructor CreateSVGIconImageListEditor(AOwner: TComponent;
      ASVGImgList: TSVGIconImageList);
    property Modified: Boolean read FModified;
    property SVGIconImageList: TSVGIconImageList read FEditingList;
  end;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;

implementation

{$R *.dfm}

uses
  SVG
  , Types
  , GDIPAPI
  , ShellApi
  , SVGIconUtils;

function EditSVGIconImageList(const AImageList: TSVGIconImageList): Boolean;
var
  LEditor: TSVGIconImageListEditor;
begin
  Screen.Cursor := crHourGlass;
  try
    LEditor := TSVGIconImageListEditor.CreateSVGIconImageListEditor(
      nil, AImageList);
    with LEditor do
    begin
      try
        Screen.Cursor := crDefault;
        Result := ShowModal = mrOk;
        if Result then
        begin
          Screen.Cursor := crHourGlass;
          AImageList.Assign(LEditor.SVGIconImageList);
        end;
      finally
        DisposeOf;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

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

procedure TSVGIconImageListEditor.ClearAllButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ImageView.Clear;
    FEditingList.Clear;
    FChanged := True;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGIconImageListEditor.AddButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FEditingList.LoadFromFiles(OpenDialog.Files);
      BuildList(MaxInt);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TSVGIconImageListEditor.DeleteButtonClick(Sender: TObject);
var
  LIndex: Integer;
  Selected: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Selected := ImageView.ItemIndex;
    FEditingList.StopDrawing(True);
    try
      for LIndex := ImageView.Items.Count - 1 downto 0 do
        if ImageView.Items[LIndex].Selected then
          FEditingList.SVGIconItems.Delete(LIndex);
    finally
      FEditingList.StopDrawing(False);
      FEditingList.RecreateBitmaps;
    end;
    FChanged := True;
    BuildList(Selected);
  finally
    Screen.Cursor := crDefault;
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
    ExportButton.Enabled := FEditingList.Count > 0;
    DeleteButton.Enabled := LIsItemSelected;
    ReplaceButton.Enabled := LIsItemSelected;
    ApplyButton.Enabled := FChanged;
    IconName.Enabled := LIsItemSelected;
    SVGText.Enabled := LIsItemSelected;
    if LIsItemSelected then
    begin
      IconImage.ImageIndex := SelectedIcon.Index;
      ItemGroupBox.Caption := Format(FIconIndexLabel,[LIconItem.Index]);
      IconName.Text := LIconItem.IconName;
      SVGText.Lines.Text := LIconItem.SVGText;
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

procedure TSVGIconImageListEditor.NewButtonClick(Sender: TObject);
begin
  FEditingList.SVGIconItems.Add;
  BuildList(MaxInt);
  UpdateGUI;
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
    try
      SVG := TSVG.Create;
      FEditingList.StopDrawing(True);
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
        FEditingList.StopDrawing(False);
        FEditingList.RecreateBitmaps;
        SVG.Free;
      end;
      BuildList(MaxInt);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
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

procedure TSVGIconImageListEditor.SVGTextExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SelectedIcon.SVGText := SVGText.Lines.Text;
  IconImage.Invalidate;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.BuildList(Selected: Integer);
begin
  FEditingList.StopDrawing(True);
  try
    UpdateSVGIconListView(ImageView);
  finally
    FEditingList.StopDrawing(False);
    FEditingList.RecreateBitmaps;
  end;

  if Selected < -1 then
    Selected := -1;
  if Selected >= FEditingList.SVGIconItems.Count then
    Selected := FEditingList.SVGIconItems.Count - 1;

  ImageView.ItemIndex := Selected;
  UpdateGUI;
end;

procedure TSVGIconImageListEditor.ExportButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FEditingList.SVGIconItems[ImageView.ItemIndex].SVG.SaveToFile(SaveDialog.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
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
begin
  inherited;
  FEditingList := TSVGIconImageList.Create(Self);
  ImageView.LargeImages := FEditingList;
  IconImage.ImageList := FEditingList;
  FChanged := False;
  FIconIndexLabel := ItemGroupBox.Caption;
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
    FSourceList.StopDrawing(True);
    Try
      FSourceList.SVGIconItems.Assign(FEditingList.SVGIconItems);
      FChanged := False;
      FModified := True;
    Finally
      FSourceList.StopDrawing(False);
      FSourceList.RecreateBitmaps;
    End;
  finally
    Screen.Cursor := crDefault;
  end;
end;

constructor TSVGIconImageListEditor.CreateSVGIconImageListEditor(AOwner: TComponent;
  ASVGImgList: TSVGIconImageList);
begin
  inherited Create(AOwner);
  FSourceList := ASVGImgList;
  FEditingList.Assign(FSourceList);
  OpacitySpinEdit.Value := FEditingList.Opacity;
  StoreAsTextCheckBox.Checked := FEditingList.StoreAsText;
  UpdateSizeGUI;
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
  BuildList(0);
  if ImageView.CanFocus then
    ImageView.SetFocus;
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

end.
