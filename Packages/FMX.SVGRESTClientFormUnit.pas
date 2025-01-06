{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VLC+FMX           }
{       to simplify use of Icons (resize, opacity and more...)                 }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
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
unit FMX.SVGRESTClientFormUnit;

interface

{$INCLUDE ..\Source\SVGIconImageList.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.EditBox,
  FMX.SpinBox, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, FMX.SVGIconImageList, Browser.IconifyApi;

type
  TSVGRESTClientSearchForm = class(TForm)
    ItemGroupBox: TGroupBox;
    IconControlsPanel: TPanel;
    SearchEdit: TEdit;
    OpacityLabel: TLabel;
    MaxIconsEdit: TSpinBox;
    IconsGroupBox: TGroupBox;
    SearchView: TListBox;
    BottomPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    SearchButton: TButton;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    SelectedView: TListBox;
    RemovePrefixCheckBox: TCheckBox;
    TrackBar: TTrackBar;
    procedure SearchButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchViewDblClick(Sender: TObject);
    procedure SelectedViewDblClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TrackBarTracking(Sender: TObject);
  private
    FIconify: TIconifyApi;
    FSearchList, FSelectedList: TSVGIconImageList;
    FIconsSize: Integer;
    procedure AddImagesToSource(const ASourceList: TSVGIconImageList);
    procedure UpdateGUI;
    procedure SetIconsSize(const AValue: Integer);
  public
    property IconsSize: Integer read FIconsSize write SetIconsSize;
  end;

function SearchSVGIconsFromWeb(var AImageList: TSVGIconImageList): Boolean;

implementation

{$R *.fmx}

uses
  FMX.ImageSVG
  {$IFDEF Image32_SVGEngine}
  , Img32.SVG.Core
  {$ENDIF}
  , FMX.SVGIconsUtils
  {$IFDEF MSWINDOWS}
  , Winapi.shellApi
  , Winapi.Windows
  {$ENDIF}
  ;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function SearchSVGIconsFromWeb(var AImageList: TSVGIconImageList): Boolean;
var
  LEditor: TSVGRESTClientSearchForm;
begin
  LEditor := TSVGRESTClientSearchForm.Create(nil);
  with LEditor do
  begin
    try
      //Screen.Cursor := crHourglass;
      try
        UpdateSVGIconListView(SearchView);
        //UpdateGUI;
        if SearchView.Items.Count > 0 then
          SearchView.ItemIndex := 0;

      finally
        //Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        //Screen.Cursor := crHourglass;
        try
          AddImagesToSource(AImageList);
        finally
          //Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := Bounds;
    finally
      Free;
    end;
  end;
end;

procedure TSVGRESTClientSearchForm.UpdateGUI;
begin
  OKButton.Enabled := FSelectedList.Count > 0;
  SearchView.ItemHeight := FIconsSize;
end;

procedure TSVGRESTClientSearchForm.AddImagesToSource(
  const ASourceList: TSVGIconImageList);
var
  LItem: TSVGIconSourceItem;
  I: Integer;
begin
  for I := 0 to FSelectedList.Source.Count -1 do
  begin
    LItem := FSelectedList.GetIcon(I);
    ASourceList.AddIcon(LItem.SVGText, LItem.Name);
  end;
end;

procedure TSVGRESTClientSearchForm.FormCreate(Sender: TObject);
begin
  FIconsSize := 48;
  {$IFDEF D11+}
  Constraints.MinHeight := 500;
  Constraints.MinWidth := 700;
  {$ENDIF}
  FSearchList := TSVGIconImageList.Create(nil);
  FSelectedList := TSVGIconImageList.Create(nil);
  SearchView.Images := FSearchList;
  SelectedView.Images := FSelectedList;
  Caption := Format(Caption, [SVGIconImageListVersion]);
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSearchList);
  FreeAndNil(FSelectedList);
end;

procedure TSVGRESTClientSearchForm.HelpButtonClick(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/RESTAPISearch.html'), nil, nil,
    SW_SHOWNORMAL)
  {$ENDIF}
end;

procedure TSVGRESTClientSearchForm.SearchButtonClick(Sender: TObject);
var
  LSearch: TIconifySearch;
  LName: string;
  LSvgString: string;
begin
  LSearch := TIconifySearch.Create;
  try
    FSearchList.ClearIcons;
    FIconify.Search(SearchEdit.Text, Round(MaxIconsEdit.Value), LSearch);
    for LName in LSearch.Icons do
    begin
      LSvgString := FIconify.Download(LName);
      FSearchList.AddIcon(LSvgString, LName);
    end;
    UpdateSVGIconListView(SearchView);
  finally
    LSearch.Free;
  end;
end;

procedure TSVGRESTClientSearchForm.SearchViewDblClick(Sender: TObject);
var
  LItem: TSVGIconSourceItem;
  LName: string;
  LPos: Integer;
begin
  LItem := FSearchList.GetIcon(SearchView.ItemIndex);
  LName := LItem.Name;
  if RemovePrefixCheckBox.IsChecked then
  begin
    LPos := Pos(':', LName);
    if LPos > 0 then
      LName := Copy(LName, LPos+1, MaxInt);
  end;
  FSelectedList.AddIcon(LItem.SVGText, LName);
  UpdateSVGIconListView(SelectedView);
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.SelectedViewDblClick(Sender: TObject);
begin
  //Delete selected Icon
  FSelectedList.DeleteIcon(SelectedView.ItemIndex);
  UpdateSVGIconListView(SelectedView);
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.SetIconsSize(const AValue: Integer);
begin
  if FIconsSize <> AValue then
  begin
    FIconsSize := AValue;
    FSearchList.Size := FIconsSize;
    FSelectedList.Size := FIconsSize;
    if TrackBar.Value <> FIconsSize then
      TrackBar.Value := FIconsSize;
    UpdateGUI;
  end;
end;

procedure TSVGRESTClientSearchForm.TrackBarTracking(Sender: TObject);
begin
  IconsSize := Round(TrackBar.Value);
end;

end.
