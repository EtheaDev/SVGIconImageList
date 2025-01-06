{******************************************************************************}
{                                                                              }
{       SVGIconImageList Set Properties Form                                   }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Luca Minuti                                              }
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
unit SVGIconSetFormUnit;

interface

uses
  Winapi.Windows
  , Winapi.Messages
  , Vcl.Controls
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.Forms
  , System.Classes, Vcl.Samples.Spin
  ;

resourcestring
  SET_CATEGORY = 'Set Category';
  SET_PNG_SIZE = 'Set Png Image Size';

type
  TSVGIconSetForm = class(TForm)
    CategoryGroupBox: TGroupBox;
    CategoryEdit: TEdit;
    BottomPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    PngGroupBox: TGroupBox;
    PngHeightLabel: TLabel;
    PngWidthLabel: TLabel;
    PngWidthEdit: TSpinEdit;
    PngHeightEdit: TSpinEdit;
    CategoryNameLabel: TLabel;
    PngSizeEdit: TSpinEdit;
    SizeLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CategoryEditChange(Sender: TObject);
    procedure PngSizeEditChange(Sender: TObject);
  private
  public
  end;

function SVGPropInputCategory(const AOwner: TForm;
  var ACategory: string): Boolean;
function SVGPropInputPngSize(const AOwner: TForm;
  var APngSize, APngWidth, APngHeight: Integer): Boolean;

implementation

uses
  WinApi.ShellAPI
  , Vcl.Themes
  , Vcl.Graphics
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseSVGEditorsAtRunTime}
  , ToolsAPI
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  ;

{$R *.dfm}


function SVGPropInputCategory(const AOwner: TForm; var ACategory: string): Boolean;
var
  LForm: TSVGIconSetForm;
begin
  LForm := TSVGIconSetForm.Create(AOwner);
  try
    LForm.Caption := SET_CATEGORY;
    LForm.CategoryGroupBox.Visible := True;
    Result := LForm.ShowModal = mrOk;
    if Result then
    begin
      ACategory := LForm.CategoryEdit.Text;
    end;
  finally
    LForm.Free;
  end;
end;

function SVGPropInputPngSize(const AOwner: TForm; var APngSize, APngWidth, APngHeight: Integer): Boolean;
var
  LForm: TSVGIconSetForm;
begin
  LForm := TSVGIconSetForm.Create(AOwner);
  try
    LForm.Caption := SET_PNG_SIZE;
    LForm.PngGroupBox.Visible := True;
    LForm.PngSizeEdit.Value := APngSize;
    LForm.PngWidthEdit.Value := APngWidth;
    LForm.PngHeightEdit.Value := APngHeight;
    Result := LForm.ShowModal = mrOk;
    if Result then
    begin
      APngWidth := LForm.PngWidthEdit.Value;
      APngHeight := LForm.PngHeightEdit.Value;
    end;
  finally
    LForm.Free;
  end;
end;

{ TSVGIconSetForm }

procedure TSVGIconSetForm.CategoryEditChange(Sender: TObject);
begin
  OKButton.Enabled := CategoryEdit.Text <> '';
end;

procedure TSVGIconSetForm.FormCreate(Sender: TObject);
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
      IDEThemeManager.RegisterFormClass(TSVGIconSetForm);
      ThemeProperties.ApplyTheme(Self);
    end;
  {$IFEND}
{$ENDIF}
end;

procedure TSVGIconSetForm.PngSizeEditChange(Sender: TObject);
begin
  PngWidthEdit.Value := PngSizeEdit.Value;
  PngHeightEdit.Value := PngSizeEdit.Value;
end;

end.
