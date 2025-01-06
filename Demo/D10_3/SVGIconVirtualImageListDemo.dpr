{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VCL               }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia, Vincent Parrett                                    }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
program SVGIconVirtualImageListDemo;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UMainNew in '..\Source\UMainNew.pas' {MainForm},
  SVGIconImageListEditorUnit in '..\..\Packages\SVGIconImageListEditorUnit.pas' {SVGIconImageListEditor},
  SVGTextPropertyEditorUnit in '..\..\Packages\SVGTextPropertyEditorUnit.pas' {SVGTextPropertyEditorForm},
  SVGIconSetFormUnit in '..\..\Packages\SVGIconSetFormUnit.pas' {SVGIconSetForm},
  SVGIconifySearchFormUnit in '..\..\Packages\SVGIconifySearchFormUnit.pas' {SVGIconifySearchForm},
  SVGIconImageList in '..\..\source\SVGIconImageList.pas',
  UDataModule in '..\Source\UDataModule.pas' {ImageDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TImageDataModule, ImageDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
