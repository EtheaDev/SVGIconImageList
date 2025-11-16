{******************************************************************************}
{                                                                              }
{       SVGExplorer: Demo to explore SVG files on disk                         }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
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
{******************************************************************************}program SVGExplorer;

uses
  Vcl.Forms,
  FExplorerSVG in 'FExplorerSVG.pas' {fmExplorerSVG};

{$R *.res}

begin
  Application.Title := 'SVG Icons Explorer - Copyright (c) 2020-2025 Ethea S.r.l.';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmExplorerSVG, fmExplorerSVG);
  Application.Run;
end.
