unit UDataModule;

interface

uses
  System.SysUtils, System.Classes,
  {$IFDEF D10_3+}Vcl.BaseImageCollection,{$ENDIF}
  SVGIconImageCollection;

type
  TImageDataModule = class(TDataModule)
    SVGIconImageCollection: TSVGIconImageCollection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageDataModule: TImageDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
