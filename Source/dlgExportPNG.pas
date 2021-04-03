unit dlgExportPNG;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SVGIconImage;

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
    procedure btnOKClick(Sender: TObject);
    procedure FormatEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure OutputButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
  private
    FFileName: TFileName;
    FIconName: string;
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
  ASVGContent: string;
  const AShowModal: Boolean;
  const ACustomSize: Integer = 0): boolean;

implementation

{$R *.dfm}

uses
  SVGIconUtils
  , System.UITypes
  , System.Math;

function ExportToPNG(const AParentRect: TRect;
  const AFileName: TFileName;
  ASVGContent: string;
  const AShowModal: Boolean;
  const ACustomSize: Integer = 0): Boolean;
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
  LExportToPNGDialog.FileName := AFileName;
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
end;

procedure TExportToPNGDialog.ExportToPNG;
var
  LOutFolder: string;
  LIconName: string;
  LFileExported: TStringList;

  function ExportToPNG(ASize: Integer): string;
  begin
    Result := GetOutFileName(ASize);
    SVGExportToPng(ASize, ASize, SVGIconImage.SVG,
      LOutFolder, Result);
  end;

begin
  LFileExported := TStringList.Create;
  try
    LOutFolder := ExtractFilePath(FileName);
    ForceDirectories(LOutFolder);
    LIconName := ChangeFileExt(ExtractFileName(FileName),'');
    if Export16x16.Checked then
      LFileExported.Add(ExportToPNG(16));
    if Export32x32.Checked then
      LFileExported.Add(ExportToPNG(32));
    if Export48x48.Checked then
      LFileExported.Add(ExportToPNG(48));
    if Export96x96.Checked then
      LFileExported.Add(ExportToPNG(96));
    if Export128x128.Checked then
      LFileExported.Add(ExportToPNG(128));
    if Export192x192.Checked then
      LFileExported.Add(ExportToPNG(192));
    if Export256x256.Checked then
      LFileExported.Add(ExportToPNG(256));
    if ExportCustom.Checked and (CustomSizeValue > 0) then
      LFileExported.Add(ExportToPNG(CustomSizeValue));

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
  Result := StringReplace(Result, '%Size%', ASize.ToString, [rfReplaceAll, rfIgnoreCase]);
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
