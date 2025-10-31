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
program SVGIconImageDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF MSWINDOWS}
  FMX.SVGIconImageListEditorUnit in '..\..\Packages\FMX.SVGIconImageListEditorUnit.pas' {SVGIconImageListEditorFMX},
  FMX.Design.Utils in '..\..\Packages\FMX.Design.Utils.pas',
  FMX.SVGRESTClientFormUnit in '..\..\Packages\FMX.SVGRESTClientFormUnit.pas' {SVGRESTClientSearchForm},
  Browser.IconifyApi in '..\Source\Browser.IconifyApi.pas',
  FMX.SVGTextPropertyEditorUnit in '..\..\Packages\FMX.SVGTextPropertyEditorUnit.pas' {SVGTextPropertyEditorFormFMX},
  {$ENDIF }
  USVGIconImageFMX in '..\Source\USVGIconImageFMX.pas' {SVGIconImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSVGIconImageForm, SVGIconImageForm);
  Application.Run;
end.
