{******************************************************************************}
{ Unit Name: SVGRESTClientFormUnit                                             }
{ Author: Carlo Barazzetta                                                     }
{ Contributors: Luca Minuti                                                    }
{ Purpose:   Form for search icons at https://iconify.design/                  }
{ History:                                                                     }
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
unit SVGRESTClientFormUnit;

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.ComCtrls
  , Vcl.Samples.Spin
  , Browser.IconifyApi
  , SVGIconImageList
  , SVGIconImage
  , SVGInterfaces
  , SVGIconImageCollection
  ;

type
  TSVGRESTClientSearchForm = class(TForm)
    SearchGroupBox: TGroupBox;
    SearchEdit: TEdit;
    SearchButton: TButton;
    BottomPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    TrackBar: TTrackBar;
    ClientPanel: TPanel;
    AvailGroupBox: TGroupBox;
    SearchView: TListView;
    SelectedGroupBox: TGroupBox;
    SplitterView: TSplitter;
    SelectedView: TListView;
    MaxLabel: TLabel;
    MaxIconsEdit: TSpinEdit;
    RemovePrefixCheckBox: TCheckBox;
    procedure SearchButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectedViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SelectedViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SearchViewDblClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchEditEnter(Sender: TObject);
    procedure SearchEditExit(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    FUpdating: Boolean;
    FIconify: TIconifyApi;
    FIconsSize: Integer;
    FSourceList, FSearchList, FSelectedList: TSVGIconImageList;
    FSVG: ISVG;
    FReplaceIndex: Integer;
    procedure AddImage(AStream: TStream; const AName: string);
    procedure SetIconsSize(const AValue: Integer);
    procedure UpdateGUI;
    procedure BuildList(const AImageView: TListView;
      const AImageList: TSVGIconImageList);
    procedure MoveIconToSelectView(const AIndex: Integer);
    procedure Apply;
    procedure SetReplaceIndex(const AValue: Integer);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IconsSize: Integer read FIconsSize write SetIconsSize;
    property ReplaceIndex: Integer read FReplaceIndex write SetReplaceIndex;
  end;

var
  SearchSavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  paSelectedGroupWidth: Integer;
  MaxIcons: Integer;

function SearchSVGIconsFromWeb(var AImageList: TSVGIconImageList;
  const AReplaceIndex: Integer = -1): Boolean;

implementation

uses
  WinApi.ShellAPI
  , Vcl.Themes
  , System.Math
  , SVGIconImageListBase
  , SVGIconUtils
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseSVGEditorsAtRunTime}
  , ToolsAPI
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  ;

{$R *.dfm}

function SearchSVGIconsFromWeb(var AImageList: TSVGIconImageList;
  const AReplaceIndex: Integer = -1): Boolean;
var
  LForm: TSVGRESTClientSearchForm;
begin
  LForm := TSVGRESTClientSearchForm.Create(nil);
  try
    Screen.Cursor := crHourglass;
    try
      LForm.ReplaceIndex := AReplaceIndex;
      LForm.FSourceList := AImageList;
      LForm.IconsSize := Max(AImageList.Width, AImageList.Height);
    finally
      Screen.Cursor := crDefault;
    end;
    if paSelectedGroupWidth <> 0 then
      LForm.SearchGroupBox.Width := paSelectedGroupWidth;
    if MaxIcons <> 0 then
      LForm.MaxIconsEdit.Value := MaxIcons;
    Result := LForm.ShowModal = mrOk;
    SearchSavedBounds := LForm.BoundsRect;
    paSelectedGroupWidth := LForm.SelectedGroupBox.Width;
    MaxIcons := LForm.MaxIconsEdit.Value;
  finally
    LForm.Free;
  end;
end;

{ TSVGRESTClientSearchForm }

procedure TSVGRESTClientSearchForm.UpdateGUI;
begin
  FUpdating := True;
  try
    OKButton.Enabled := FSelectedList.Count > 0;
    SearchButton.Enabled := SearchEdit.Text <> '';
  finally
    FUpdating := False;
  end;
end;

procedure TSVGRESTClientSearchForm.AddImage(AStream: TStream;
  const AName: string);
begin
  FSVG := GlobalSVGFactory.NewSvg;
  FSVG.LoadFromStream(AStream);
  FSearchList.Add(FSVG, AName);
end;

procedure TSVGRESTClientSearchForm.SearchButtonClick(Sender: TObject);
var
  LSearch: TIconifySearch;
  LName: string;
  LSvgString: string;
  LStringStream: TStringStream;
begin
  LSearch := TIconifySearch.Create;
  try
    Screen.Cursor := crHourglass;
    FSearchList.ClearIcons;
    FIconify.Search(SearchEdit.Text, MaxIconsEdit.Value, LSearch);
    for LName in LSearch.Icons do
    begin
      LSvgString := FIconify.Download(LName);
      LStringStream := TStringStream.Create(LSvgString, TEncoding.UTF8);
      try
        AddImage(LStringStream, LName);
      finally
        LStringStream.Free;
      end;
    end;
    BuildList(SearchView, FSearchList);
  finally
    LSearch.Free;
    Screen.Cursor := crDefault;
  end;
end;

constructor TSVGRESTClientSearchForm.Create(AOwner: TComponent);
begin
  inherited;
  FIconify := TIconifyApi.Create;
  FIconsSize := 48;
end;

destructor TSVGRESTClientSearchForm.Destroy;
begin
  FIconify.Free;
  inherited;
end;

procedure TSVGRESTClientSearchForm.SearchEditChange(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.SearchEditEnter(Sender: TObject);
begin
  OKButton.Default := False;
end;

procedure TSVGRESTClientSearchForm.SearchEditExit(Sender: TObject);
begin
  OKButton.Default := True;
end;

procedure TSVGRESTClientSearchForm.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := 0; // Impedisce il beep
    SearchButton.Click; // Invoca il click del TButton
  end;
end;

procedure TSVGRESTClientSearchForm.FormCreate(Sender: TObject);
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

    if ThemeProperties <> nil then
    begin
      LStyle := ThemeProperties.StyleServices;
      StyleElements := StyleElements - [seClient];
      Color := LStyle.GetSystemColor(clWindow);
      BottomPanel.StyleElements := BottomPanel.StyleElements - [seClient];
      BottomPanel.ParentBackground := False;
      BottomPanel.Color := LStyle.GetSystemColor(clBtnFace);
      IDEThemeManager.RegisterFormClass(TSVGRESTClientSearchForm);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}
  FSearchList := TSVGIconImageList.Create(Self);
  FSelectedList := TSVGIconImageList.Create(Self);
  IconsSize := 32;
  MaxIconsEdit.Value := 50;
  SearchView.LargeImages := FSearchList;
  SelectedView.LargeImages := FSelectedList;
  Caption := Format(Caption, [SVGIconImageListVersion]);
end;

procedure TSVGRESTClientSearchForm.FormDestroy(Sender: TObject);
begin
  FSearchList.Free;
  FSelectedList.Free;
end;

procedure TSVGRESTClientSearchForm.FormShow(Sender: TObject);
begin
  if SearchSavedBounds.Right - SearchSavedBounds.Left > 0 then
    SetBounds(SearchSavedBounds.Left, SearchSavedBounds.Top,
      SearchSavedBounds.Width, SearchSavedBounds.Height);

  UpdateGUI;

  if SearchEdit.CanFocus then
    SearchEdit.SetFocus;
end;

procedure TSVGRESTClientSearchForm.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/RESTAPISearch.html'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TSVGRESTClientSearchForm.Loaded;
begin
  inherited;
  {$IFDEF D10_1+}
  MaxIconsEdit.Align := alClient;
  {$ENDIF}
end;

procedure TSVGRESTClientSearchForm.Apply;
var
  I: Integer;
  LSourceIcon: TSVGIconItem;
  Item: TSVGIconItem;
begin
  Screen.Cursor := crHourGlass;
  try
    if ReplaceIndex <> -1 then
    begin
      Item := FSourceList.SVGIconItems[ReplaceIndex];
      LSourceIcon := FSearchList.SVGIconItems[SearchView.ItemIndex];
      Item.SVG.Source := LSourceIcon.SVG.Source;
    end
    else
    begin
      for I := 0 to FSelectedList.count -1 do
      begin
        LSourceIcon := FSelectedList.SVGIconItems[I];
        FSourceList.Add(LSourceIcon.SVG, LSourceIcon.IconName,
          LSourceIcon.Category);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TSVGRESTClientSearchForm.BuildList(const AImageView: TListView;
  const AImageList: TSVGIconImageList);
begin
  AImageList.BeginUpdate;
  try
    UpdateSVGIconListView(AImageView);
  finally
    AImageList.EndUpdate;
  end;
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.MoveIconToSelectView(
  const AIndex: Integer);
var
  LSourceIcon: TSVGIconItem;
  LName: string;
  LPos: Integer;
begin
  LSourceIcon := FSearchList.SVGIconItems[AIndex];
  LName := LSourceIcon.IconName;
  if RemovePrefixCheckBox.Checked then
  begin
    LPos := Pos(':', LName);
    if LPos > 0 then
      LName := Copy(LName, LPos+1, MaxInt);
  end;
  FSelectedList.Add(LSourceIcon.SVG, LName);
end;

procedure TSVGRESTClientSearchForm.OKButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TSVGRESTClientSearchForm.SearchViewDblClick(Sender: TObject);
begin
  if SearchView.Selected <> nil then
  begin
    if ReplaceIndex <> -1 then
    begin
      Apply;
      ModalResult := mrOk;
    end
    else
    begin
      MoveIconToSelectView(SearchView.Selected.Index);
      BuildList(SelectedView, FSelectedList);
      UpdateGUI;
    end;
  end;
end;

procedure TSVGRESTClientSearchForm.SelectedViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LSourceItem: TListItem;
  I: Integer;
begin
  for I := 0 to SearchView.Items.Count - 1 do
  begin
    LSourceItem := SearchView.Items[I];
    if LSourceItem.Selected then
      MoveIconToSelectView(I);
  end;
  BuildList(SelectedView, FSelectedList);
  UpdateGUI;
end;

procedure TSVGRESTClientSearchForm.SelectedViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = SearchView) and (Sender = SelectedView);
end;

procedure TSVGRESTClientSearchForm.SetIconsSize(const AValue: Integer);
begin
  if FIconsSize <> AValue then
  begin
    FIconsSize := AValue;
    FSearchList.Size := FIconsSize;
    FSelectedList.Size := FIconsSize;
    if TrackBar.Position <> FIconsSize then
      TrackBar.Position := FIconsSize;
  end;
end;

procedure TSVGRESTClientSearchForm.SetReplaceIndex(const AValue: Integer);
begin
  FReplaceIndex := AValue;
  SelectedGroupBox.Visible := FReplaceIndex = -1;
end;

procedure TSVGRESTClientSearchForm.TrackBarChange(Sender: TObject);
begin
  IconsSize := TrackBar.Position;
end;

end.
