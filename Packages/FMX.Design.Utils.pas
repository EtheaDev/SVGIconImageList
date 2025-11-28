unit FMX.Design.Utils;

interface

uses
  System.SysUtils
  , System.Classes
  , FMX.Forms, FMX.Types, FMX.Controls
  ;

type
  TDesignModuleUtils = class(TDataModule)
    DarkStyleBook: TStyleBook;
    LightStyleBook: TStyleBook;
  end;

procedure UpdateFormStyleFromIDE(const AForm: TForm);

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.IOUtils
  , System.UITypes
  , Vcl.Themes
  , FMX.Styles
  , FMX.Styles.Objects
  , FMX.Graphics
  //WARNING: you must define this directive to use this unit outside the IDE
{$IFNDEF UseSVGEditorsAtRunTime}
  , ToolsAPI
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND}
{$ENDIF}
  ;

(*
procedure ChangeFontSettings(const AControl: TFmxObject;
  const AIsDarkStyle: Boolean);
var
  I: Integer;
  LTextSettings: ITextSettings;
begin
  // Applica lo stile al controllo corrente se è un controllo con stile
  if (AControl is TStyledControl) and
    System.SysUtils.Supports(AControl, ITextSettings, LTextSettings) then
  begin
    LTextSettings.StyledSettings := LTextSettings.StyledSettings -
      [TStyledSetting.FontColor];
    if AIsDarkStyle then
      LTextSettings.TextSettings.FontColor := TAlphaColors.white
    else
      LTextSettings.TextSettings.FontColor := TAlphaColors.Black;
    TStyledControl(AControl).ApplyStyleLookup;
  end;

  // Applica ricorsivamente a tutti i figli
  for I := 0 to AControl.ChildrenCount - 1 do
    ChangeFontSettings(AControl.Children[I], AIsDarkStyle);
end;
*)

procedure UpdateFormStyleFromIDE(const AForm: TForm);
var
  LDesignModuleUtils: TDesignModuleUtils;
{$IFNDEF UseSVGEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
    LStyle: TCustomStyleServices;
  {$IFEND}
{$ENDIF}
begin
{$IFNDEF UseSVGEditorsAtRunTime}
  {$IF (CompilerVersion >= 32.0)}
  if ThemeProperties <> nil then
    LStyle := ThemeProperties.StyleServices
  else
    LStyle := nil;

  if Assigned(LStyle) then
  begin
    LDesignModuleUtils := TDesignModuleUtils.Create(AForm);
    //Mapping VCL-IDE Style with FMX Style
    if Pos('Dark', LStyle.Name) > 0 then //Win10IDE_Dark
      AForm.StyleBook := LDesignModuleUtils.DarkStyleBook
    else ////Other Light Styles
      AForm.StyleBook := LDesignModuleUtils.LightStyleBook;
  end;
  //ChangeFontSettings(AForm, LIsDarkStyle);
  {$IFEND}
{$ELSE}
  LDesignModuleUtils := TDesignModuleUtils.Create(AForm);
  //Light Style at RunTime
  AForm.StyleBook := LDesignModuleUtils.LightStyleBook;
{$ENDIF}
end;

end.
