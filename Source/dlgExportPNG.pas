{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Vincent Parrett                                                }
{       Contributors: Carlo Barazzetta, Kiriakos Vlahos                        }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
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
unit dlgExportPNG;

interface

uses
  System.SysUtils, WinApi.Windows, SVGIconImage, SVGIconUtils,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, System.Classes;

resourcestring
  SVG_IMAGE_EXPORTED = 'PNG Images created into Folder';

type
  TExportToPNGDialog = class(TForm)
    FSearchOptions: TGroupBox;
    Export16x16: TCheckBox;
    Export32x32: TCheckBox;
    Export48x48: TCheckBox;
    Export64x64: TCheckBox;
    Export96x96: TCheckBox;
    Export128x128: TCheckBox;
    Export192x192: TCheckBox;
    Export256x256: TCheckBox;
    ExportCustom: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    SVGIconImage: TSVGIconImage;
    CustomSizeEdit: TEdit;
    OutputFileNameLabel: TLabel;
    FormatEdit: TEdit;
    ExampleLabel: TLabel;
    ExampleFileName: TLabel;
    SavePNGDialog: TSaveDialog;
    OutputButton: TButton;
    BtnCopyToClipboard: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormatEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure OutputButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure BtnCopyToClipboardClick(Sender: TObject);
  private
    FFileName: TFileName;
    FIconName: string;
    FOnExportToPng: TExportToPngEvent;
    procedure UpdateExampleLabel;
    procedure ExportToPNG;
    procedure SetFileName(const Value: TFileName);
    function GetOutFileName(const ASize: Integer): TFileName;
    function GetCustomSizeValue: Integer;
    procedure SetCustomSizeValue(const Value: Integer);
    property FileName: TFileName read FFileName write SetFileName;
    property CustomSizeValue: Integer read GetCustomSizeValue write SetCustomSizeValue;
  public
  end;

function ExportToPNG(const AParentRect: TRect;
  const AFileName: TFileName;
  const ASVGContent: string;
  const AShowModal: Boolean;
  const ACustomSize: Integer = 0;
  const AExportFormat: string = '';
  const ASizes: TPngExportSizes = [];
  const OnExportToPng: TExportToPngEvent = nil): Boolean;

implementation

{$R *.dfm}

uses
  System.UITypes
  , System.Math
  , Vcl.Graphics
  ;

function ExportToPNG(const AParentRect: TRect;
  const AFileName: TFileName;
  const ASVGContent: string;
  const AShowModal: Boolean;
  const ACustomSize: Integer = 0;
  const AExportFormat: string = '';
  const ASizes: TPngExportSizes = [];
  const OnExportToPng: TExportToPngEvent = nil): Boolean;
var
  LExportToPNGDialog: TExportToPNGDialog;
  I: Integer;
  LCustomSize: Integer;
begin
  LExportToPNGDialog := nil;
  if (ACustomSize <> 16) and
    (ACustomSize <> 32) and
    (ACustomSize <> 48) and
    (ACustomSize <> 64) and
    (ACustomSize <> 96) and
    (ACustomSize <> 128) and
    (ACustomSize <> 192) and
    (ACustomSize <> 256) then
    LCustomSize := ACustomSize
  else
    LCustomSize := 0;

  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassType = TExportToPNGDialog then
    begin
      LExportToPNGDialog := Screen.Forms[I] as TExportToPNGDialog;
      LExportToPNGDialog.BringToFront;
    end;

  if not Assigned(LExportToPNGDialog) then
    LExportToPNGDialog := TExportToPNGDialog.Create(nil);

  LExportToPNGDialog.FOnExportToPng := OnExportToPng;

  if ASizes <> [] then
  begin
    LExportToPNGDialog.Export16x16.Checked := es16 in ASizes;
    LExportToPNGDialog.Export32x32.Checked := es32 in ASizes;
    LExportToPNGDialog.Export48x48.Checked := es48 in ASizes;
    LExportToPNGDialog.Export64x64.Checked := es64 in ASizes;
    LExportToPNGDialog.Export96x96.Checked := es96 in ASizes;
    LExportToPNGDialog.Export128x128.Checked := es128 in ASizes;
    LExportToPNGDialog.Export192x192.Checked := es192 in ASizes;
    LExportToPNGDialog.Export256x256.Checked := es256 in ASizes;
    LExportToPNGDialog.ExportCustom.Checked := LCustomSize <> 0;
  end;

  LExportToPNGDialog.FileName := AFileName;
  if AExportFormat <> '' then
    LExportToPNGDialog.FormatEdit.Text := AExportFormat;
  LExportToPNGDialog.SVGIconImage.SVGText := ASVGContent;
  if LCustomSize <> 0 then
    LExportToPNGDialog.CustomSizeValue := LCustomSize
  else
    LExportToPNGDialog.ExportCustom.Checked := False;
  if AShowModal then
    Result := LExportToPNGDialog.ShowModal = mrOk
  else
  begin
    LExportToPNGDialog.Show;
    Result := True;
  end;
end;

procedure TExportToPNGDialog.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TExportToPNGDialog.BtnCopyToClipboardClick(Sender: TObject);
begin
  SVGCopyToClipboardAsPng(StrToInt(CustomSizeEdit.Text),
    StrToInt(CustomSizeEdit.Text),
    SVGIconImage.SVG);
end;

procedure TExportToPNGDialog.btnOKClick(Sender: TObject);
begin
  ExportToPNG;
  Close;
end;

procedure TExportToPNGDialog.CheckBoxClick(Sender: TObject);
begin
  btnOK.Enabled :=
    Export16x16.Checked or
    Export32x32.Checked or
    Export48x48.Checked or
    Export64x64.Checked or
    Export96x96.Checked or
    Export128x128.Checked or
    Export192x192.Checked or
    Export256x256.Checked or
    (ExportCustom.Checked and (CustomSizeValue > 0 ));
  BtnCopyToClipboard.Enabled := (ExportCustom.Checked and (CustomSizeValue > 0 ));
end;

procedure TExportToPNGDialog.ExportToPNG;
var
  LOutFolder: string;
  LIconName: string;
  LFileExported: TStringList;
  LExportSizes: TPngExportSizes;

  function ExportToPNG(ASize: Integer; AExportSize: TPngExportSize): string;
  begin
    Result := GetOutFileName(ASize);
    SVGExportToPng(ASize, ASize, SVGIconImage.SVG,
      LOutFolder, Result);
    LExportSizes := LExportSizes + [AExportSize];
  end;

begin
  LExportSizes := [];
  LFileExported := TStringList.Create;
  try
    LOutFolder := ExtractFilePath(FileName);
    ForceDirectories(LOutFolder);
    LIconName := ChangeFileExt(ExtractFileName(FileName),'');
    if Export16x16.Checked then
      LFileExported.Add(ExportToPNG(16, es16));
    if Export32x32.Checked then
      LFileExported.Add(ExportToPNG(32, es32));
    if Export48x48.Checked then
      LFileExported.Add(ExportToPNG(48, es48));
    if Export64x64.Checked then
      LFileExported.Add(ExportToPNG(64, es64));
    if Export96x96.Checked then
      LFileExported.Add(ExportToPNG(96, es96));
    if Export128x128.Checked then
      LFileExported.Add(ExportToPNG(128, es128));
    if Export192x192.Checked then
      LFileExported.Add(ExportToPNG(192, es192));
    if Export256x256.Checked then
      LFileExported.Add(ExportToPNG(256, es256));
    if ExportCustom.Checked and (CustomSizeValue > 0) then
      LFileExported.Add(ExportToPNG(CustomSizeValue, esCustom));

    if Assigned(FOnExportToPng) then
      FOnExportToPng(LExportSizes, SVGIconImage.SVG.Source,
        ExtractFilePath(FileName), FormatEdit.Text, CustomSizeValue*Ord(ExportCustom.Checked));

    MessageDlg(SVG_IMAGE_EXPORTED+sLineBreak+LOutFolder+sLineBreak+
      LFileExported.Text,
      mtInformation, [mbOK], 0);
  finally
    LFileExported.free;
  end;
end;

procedure TExportToPNGDialog.FormatEditChange(Sender: TObject);
begin
  UpdateExampleLabel;
end;

procedure TExportToPNGDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TExportToPNGDialog.FormShow(Sender: TObject);
begin
  ExampleFileName.Font.Style := ExampleFileName.Font.Style + [fsBold];
  UpdateExampleLabel;
end;

procedure TExportToPNGDialog.SetCustomSizeValue(const Value: Integer);
begin
  CustomSizeEdit.Text := IntToStr(Value);
end;

procedure TExportToPNGDialog.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  FIconName := ChangeFileExt(ExtractFileName(FFileName),'');
end;

function TExportToPNGDialog.GetCustomSizeValue: Integer;
begin
  if CustomSizeEdit.Text <> '' then
    Result := StrToInt(CustomSizeEdit.Text)
  else
    Result := 0;
end;

function TExportToPNGDialog.GetOutFileName(const ASize: Integer): TFileName;
begin
  Result := FormatEdit.Text;
  Result := StringReplace(Result, '%FileName%', FIconName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%Size%', IntToStr(ASize), [rfReplaceAll, rfIgnoreCase]);
  Result := ChangeFileExt(Result,'.png');
end;

procedure TExportToPNGDialog.OutputButtonClick(Sender: TObject);
begin
  SavePNGDialog.FileName := FileName;
  if SavePNGDialog.Execute then
  begin
    FileName := SavePNGDialog.FileName;
    UpdateExampleLabel;
  end;
end;

procedure TExportToPNGDialog.UpdateExampleLabel;
begin
  ExampleFileName.Caption := ExtractFilePath(FileName)+GetOutFileName(32);
end;

end.
