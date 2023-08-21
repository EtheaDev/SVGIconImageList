program CategoryButtonTest;

uses
  Vcl.Forms,
  UMainTest in '..\Source\UMainTest.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
