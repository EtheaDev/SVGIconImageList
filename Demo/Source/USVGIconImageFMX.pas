unit USVGIconImageFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ImgList,
  FMX.Objects, FMX.MultiresBitmap, System.Rtti, System.Messaging,
  FMX.ListBox, FMX.Colors, FMX.SVGIconImage;

type
  TSVGIconImageForm = class(TForm)
    SVGIconImage: TSVGIconImage;
    Button: TButton;
    Panel1: TPanel;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure SVGIconImageResize(Sender: TObject);
    procedure SVGIconImageDblClick(Sender: TObject);
  private
    FSVGList: TStringDynArray;
    FIndex: Integer;
  public
    { Public declarations }
  end;

var
  SVGIconImageForm: TSVGIconImageForm;

implementation

uses
  System.Math
  , System.IOUtils
  {$IFDEF MSWINDOWS}
  , FMX.SVGTextPropertyEditorUnit
  {$ENDIF}
  , FMX.Consts;

{$R *.fmx}

procedure TSVGIconImageForm.ButtonClick(Sender: TObject);
var
  LFileName: string;
begin
  Inc(FIndex);
  if FIndex > High(FSVGList) then
    FIndex := 0;
  LFileName := FSVGList[FIndex];
  SVGIconImage.LoadFromFile(LFileName);
end;

procedure TSVGIconImageForm.FormCreate(Sender: TObject);
var
  LPath: string;
begin
  LPath := GetCurrentDir+PathDelim+'..\svg_examples\';
  TDirectory.SetCurrentDirectory(LPath);
  FSVGList := TDirectory.GetFiles(LPath, '*.svg');
  FIndex := 0;
end;

procedure TSVGIconImageForm.SVGIconImageDblClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LSVGText: string;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LSVGText := SVGIconImage.SVGText;
  if EditSVGTextProperty(LSVGText) then
    SVGIconImage.SVGText := LSVGText;
  {$ENDIF}
end;

procedure TSVGIconImageForm.SVGIconImageResize(Sender: TObject);
begin
  ;
end;

//initialization
//  ReportMemoryLeaksOnShutdown := True;

end.
