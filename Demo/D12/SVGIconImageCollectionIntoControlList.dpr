program SVGIconImageCollectionIntoControlList;

uses
  Vcl.Forms,
  UControlListMain in '..\Source\UControlListMain.pas' {ControlListMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TControlListMainForm, ControlListMainForm);
  Application.Run;
end.
