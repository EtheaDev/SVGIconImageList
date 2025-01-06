program SvgViewer;



uses
  Vcl.Forms,
  SvgViewerUnit in 'SvgViewerUnit.pas' {SVGViewerForm},
  FrameViewer in 'FrameViewer.pas' {FrameView: TFrame};

{$R *.res}

begin
  Application.Title := 'SVG Preview & Engine Comparison - Copyright (c) 2020-2025 Ethea S.r.l.';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSVGViewerForm, SVGViewerForm);
  Application.Run;
end.
