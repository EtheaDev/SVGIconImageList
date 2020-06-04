program SVGIconImageListTest;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  SVGTextPropertyEditorUnit in '..\..\Packages\SVGTextPropertyEditorUnit.pas' {SVGTextPropertyEditorForm},
  SVGIconImageList in '..\..\source\SVGIconImageList.pas',
  UMaintEST in '..\Source\UMaintEST.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
