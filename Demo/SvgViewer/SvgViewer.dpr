{******************************************************************************}
{                                                                              }
{       SVGViewer: Demo to compare four engines of SVGIconImageList library    }
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
{******************************************************************************}
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
