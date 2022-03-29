program SvgViewer;



uses
  Vcl.Forms,
  SvgViewerUnit in 'SvgViewerUnit.pas' {SVGViewerForm},
  FrameViewer in 'FrameViewer.pas' {FrameView: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSVGViewerForm, SVGViewerForm);
  Application.Run;
end.
