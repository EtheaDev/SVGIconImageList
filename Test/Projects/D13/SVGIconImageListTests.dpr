program SVGIconImageListTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  SVGTestUtils in '..\..\Source\SVGTestUtils.pas',
  SVGEngineTests in '..\..\Source\SVGEngineTests.pas',
  SVGComponentTests in '..\..\Source\SVGComponentTests.pas';

var
  LRunner: ITestRunner;
  LResults: IRunResults;
  LConsoleLogger: ITestLogger;
  LNUnitLogger: ITestLogger;

begin
  try
    TDUnitX.CheckCommandLine;
    LRunner := TDUnitX.CreateRunner;
    LRunner.UseRTTI := True;
    // A test that only checks "this returns at all" makes no Assert call of its
    // own; that is not a defect in the test.
    LRunner.FailsOnNoAsserts := False;

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      LConsoleLogger := TDUnitXConsoleLogger.Create(
        TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      LRunner.AddLogger(LConsoleLogger);
    end;
    // NUnit-shaped XML, for a CI server to pick up (-x:<file> to choose it).
    LNUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    LRunner.AddLogger(LNUnitLogger);

    LResults := LRunner.Execute;
    if not LResults.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
end.
