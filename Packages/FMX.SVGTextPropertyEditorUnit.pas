{******************************************************************************}
{                                                                              }
{       SVGTextPropertyEditorUnit: A property editor for SVGText               }
{       to simplify use of setting SVGText value                               }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
unit FMX.SVGTextPropertyEditorUnit;

interface

{$INCLUDE ..\Source\SVGIconImageList.inc}

uses
  System.SysUtils, System.Types, System.UITypes, FMX.Controls, System.Classes,
  System.Actions, FMX.Forms, FMX.Graphics, FMX.ActnList, FMX.StdCtrls, FMX.Colors, FMX.ListBox,
  FMX.Controls.Presentation, FMX.ImgList, FMX.Types, FMX.Layouts,
  System.ImageList, FMX.SVGIconImageList, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, FMX.Dialogs, FMX.Memo.Types, FMX.Objects,
  FMX.SVGIconImage;

type
  TSVGTextPropertyEditorFormFMX = class(TForm)
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
    ImagePanel: TPanel;
    SVGIconImage: TSVGIconImage;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SizeLabel: TLabel;
    ReformatXMLButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure paImageResize(Sender: TObject);
    procedure SVGTextMemoChange(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReformatXMLButtonClick(Sender: TObject);
    procedure SVGTextMemoChangeTracking(Sender: TObject);
  private
    procedure UpdateImage;
    procedure UpdateGUI;
    function GetSVGText: string;
    procedure SetSVGText(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property SVGText: string read GetSVGText write SetSVGText;
  end;

function EditSVGTextProperty(var ASVGText: string;
  const ACanEdit: Boolean = True): boolean;

implementation

{$R *.fmx}

uses
  Winapi.Messages
  , Winapi.Windows
  , Winapi.shellApi
  , System.Math
  , Xml.XMLDoc;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);


function EditSVGTextProperty(var ASVGText: string;
  const ACanEdit: Boolean = True): boolean;
var
  LForm: TSVGTextPropertyEditorFormFMX;
begin
  Result := False;
  LForm := TSVGTextPropertyEditorFormFMX.Create(nil);
  try
    LForm.SVGText := ASVGText;
    LForm.SVGTextMemo.ReadOnly := not ACanEdit;
    LForm.LoadButton.Visible := ACanEdit;
    LForm.CancelButton.Visible := ACanEdit;
    LForm.ReformatXMLButton.Visible := ACanEdit;
    if LForm.ShowModal = mrOk then
    begin
      Result := True;
      ASVGText := LForm.SVGText;
    end;
    SavedBounds := LForm.Bounds;
  finally
    LForm.Free;
  end;
end;

constructor TSVGTextPropertyEditorFormFMX.Create(AOwner: TComponent);
begin
  inherited;
  ;
end;

procedure TSVGTextPropertyEditorFormFMX.FormCreate(Sender: TObject);
begin
  SVGTextMemo.Font.Family := 'Consolas';
  Caption := Format(Caption, [SVGIconImageListVersion]);
end;

procedure TSVGTextPropertyEditorFormFMX.FormResize(Sender: TObject);
begin
  paImage.Width := ClientWidth div 4;
end;

procedure TSVGTextPropertyEditorFormFMX.FormShow(Sender: TObject);
begin
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);

  if SVGTextMemo.CanFocus then
    SVGTextMemo.SetFocus;
  UpdateGUI;
end;

function TSVGTextPropertyEditorFormFMX.GetSVGText: string;
begin
  Result := SVGTextMemo.Lines.Text;
end;

procedure TSVGTextPropertyEditorFormFMX.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    PChar('https://ethea.it/docs/svgiconimagelist/SVGText-Editor-FMX.html'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TSVGTextPropertyEditorFormFMX.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    SVGIconImage.LoadFromFile(OpenDialog.FileName);
    SVGText := SVGIconImage.SVGText;
  end;
end;

procedure TSVGTextPropertyEditorFormFMX.paImageResize(Sender: TObject);
begin
  SizeLabel.Text := Format('w:%d-h:%d',
   [Trunc(SVGIconImage.Width), Trunc(SVGIconImage.Height)]);
  SVGIconImage.Hint := SizeLabel.Text;
end;

procedure TSVGTextPropertyEditorFormFMX.ReformatXMLButtonClick(Sender: TObject);
begin
  SVGTextMemo.Lines.Text := Xml.XMLDoc.FormatXMLData(SVGTextMemo.Lines.Text);
end;

procedure TSVGTextPropertyEditorFormFMX.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SVGIconImage.SaveToFile(SaveDialog.FileName);
end;

procedure TSVGTextPropertyEditorFormFMX.SetSVGText(const Value: string);
begin
  SVGTextMemo.Lines.Text := Value;
  UpdateImage;
end;

procedure TSVGTextPropertyEditorFormFMX.SVGTextMemoChange(Sender: TObject);
begin
  UpdateImage;
end;

procedure TSVGTextPropertyEditorFormFMX.SVGTextMemoChangeTracking(
  Sender: TObject);
begin
  UpdateImage;
end;

procedure TSVGTextPropertyEditorFormFMX.updateGUI;
begin
  SVGIconImage.Repaint;
end;

procedure TSVGTextPropertyEditorFormFMX.UpdateImage;
begin
  SVGIconImage.SVGText := SVGTextMemo.Lines.Text;
  SVGIconImage.Repaint;
end;

end.
