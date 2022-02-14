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
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure SVGIconImageResize(Sender: TObject);
    procedure SVGIconImageClick(Sender: TObject);
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
  , FMX.Consts
  , FMX.SVGTextPropertyEditorUnit;

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
  LPath := GetCurrentDir+PathDelim+'..\svg_examples\flat-color-icons\svg\';
  TDirectory.SetCurrentDirectory(LPath);
  FSVGList := TDirectory.GetFiles(LPath, '*.svg');
  FIndex := 0;
end;

procedure TSVGIconImageForm.SVGIconImageClick(Sender: TObject);
var
  LSVGText: string;
begin
  //Show the property Editor
  LSVGText := SVGIconImage.SVGText;
  if EditSVGTextProperty(LSVGText) then
   SVGIconImage.SVGText := LSVGText;
end;

procedure TSVGIconImageForm.SVGIconImageResize(Sender: TObject);
begin
  ;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
