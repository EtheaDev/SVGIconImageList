{******************************************************************************}
{                                                                              }
{       SVGTextPropertyEditorUnit: A property editor for SVGText               }
{       to simplify use of setting SVGText value                               }
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
unit SVGTextPropertyEditorUnit;

interface

{$INCLUDE ..\Source\SVGIconImageList.inc}

uses
  System.SysUtils, Vcl.ExtDlgs, SVGIconImage,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Controls,
  Vcl.Forms, Vcl.ExtCtrls, System.Classes;

type
  TSVGTextPropertyEditorForm = class(TForm)
    paBottom: TPanel;
    SVGTextMemo: TMemo;
    paButtons: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    HelpButton: TButton;
    RightSplitter: TSplitter;
    paImage: TPanel;
    paTitle: TPanel;
    LoadButton: TButton;
    SaveButton: TButton;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ImagePanel: TPanel;
    SVGIconImage: TSVGIconImage;
    BottomPanel: TPanel;
    ProportionalCheckBox: TCheckBox;
    ReformatXMLButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure paImageResize(Sender: TObject);
    procedure SVGTextMemoChange(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ProportionalCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReformatXMLButtonClick(Sender: TObject);
  private
    procedure UpdateImage;
    procedure UpdateGUI;
    function GetSVGText: string;
    procedure SetSVGText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

function EditSVGTextProperty(var ASVGText: string): boolean;

implementation

{$R *.dfm}

uses
  Vcl.Themes
  , System.Math
  {$IFDEF DXE3+}
  , System.UITypes
  {$ENDIF}
  , SVGIconImageListBase
  , SVGInterfaces
  , Xml.XMLDoc
  //WARNING: you must define this directive to use this unit outside the IDE
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseSVGEditorsAtRunTime}
  , ToolsAPI
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  , WinApi.ShellAPI
  , System.Types
  , System.UIConsts
  , Vcl.Graphics
  , WinApi.Windows
  ;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);


function EditSVGTextProperty(var ASVGText: string): boolean;
var
  LForm: TSVGTextPropertyEditorForm;
begin
  Result := False;
  LForm := TSVGTextPropertyEditorForm.Create(nil);
  try
    LForm.SVGText := ASVGText;
    if LForm.ShowModal = mrOk then
    begin
      Result := True;
      ASVGText := LForm.SVGText;
    end;
    SavedBounds := LForm.BoundsRect;
  finally
    LForm.Free;
  end;
end;

constructor TSVGTextPropertyEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  ;
end;

procedure TSVGTextPropertyEditorForm.FormCreate(Sender: TObject);
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
    {$IF CompilerVersion > 34.0}
    if TIDEThemeMetrics.Font.Enabled then
      Self.Font.Assign(TIDEThemeMetrics.Font.GetFont);
    {$IFEND}

    if ThemeProperties <> nil then
    begin
      LStyle := ThemeProperties.StyleServices;
      StyleElements := StyleElements - [seClient];
      Color := LStyle.GetSystemColor(clWindow);
      BottomPanel.StyleElements := BottomPanel.StyleElements - [seClient];
      BottomPanel.ParentBackground := False;
      BottomPanel.Color := LStyle.GetSystemColor(clBtnFace);
      IDEThemeManager.RegisterFormClass(TSVGTextPropertyEditorForm);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}
  SVGTextMemo.Font.Name := 'Consolas';
  Caption := Format(Caption, [SVGIconImageListVersion]);
end;

procedure TSVGTextPropertyEditorForm.FormResize(Sender: TObject);
begin
  paImage.Width := ClientWidth div 4;
end;

procedure TSVGTextPropertyEditorForm.FormShow(Sender: TObject);
begin
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);

  if SVGTextMemo.CanFocus then
    SVGTextMemo.SetFocus;
  UpdateGUI;
end;

function TSVGTextPropertyEditorForm.GetSVGText: string;
begin
  Result := SVGTextMemo.Lines.Text;
end;

procedure TSVGTextPropertyEditorForm.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/SVGText-Editor.html'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TSVGTextPropertyEditorForm.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SVGIconImage.LoadFromFile(OpenDialog.FileName);
    SVGText := SVGIconImage.SVGText;
  end;
end;

procedure TSVGTextPropertyEditorForm.paImageResize(Sender: TObject);
begin
  paTitle.Caption := Format('w:%d-h:%d',
   [SVGIconImage.Width, SVGIconImage.Height]);
  SVGIconImage.Hint := paTitle.Caption;
end;

procedure TSVGTextPropertyEditorForm.ProportionalCheckBoxClick(Sender: TObject);
begin
  SVGIconImage.Proportional := ProportionalCheckBox.Checked;
end;

procedure TSVGTextPropertyEditorForm.ReformatXMLButtonClick(Sender: TObject);
begin
  SVGTextMemo.Lines.Text := Xml.XMLDoc.FormatXMLData(SVGTextMemo.Lines.Text);
end;

procedure TSVGTextPropertyEditorForm.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SVGIconImage.SVG.SaveToFile(SaveDialog.FileName);
end;

procedure TSVGTextPropertyEditorForm.SetSVGText(const Value: string);
begin
  SVGTextMemo.Lines.Text := Value;
  UpdateImage;
end;

procedure TSVGTextPropertyEditorForm.SVGTextMemoChange(Sender: TObject);
begin
  UpdateImage;
end;

procedure TSVGTextPropertyEditorForm.updateGUI;
begin
  SVGIconImage.Repaint;
end;

procedure TSVGTextPropertyEditorForm.UpdateImage;
begin
  try
    SVGIconImage.SVGText := SVGTextMemo.Lines.Text;
    SVGIconImage.Repaint;
  except
    On ESVGException do
    begin
      SVGIconImage.SVGText := '';
      SVGIconImage.Repaint;
    end
    else
      raise;
  end;
end;

end.
