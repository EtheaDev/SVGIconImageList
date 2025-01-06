{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi/VCL               }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia                                                     }
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
unit UMainTest;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  System.Actions, System.ImageList, Vcl.Forms,
  SVGIconImageList, SVGIconImage, Vcl.ExtDlgs, Vcl.CategoryButtons,
  SVGIconImageListBase, Vcl.ImgList, Vcl.Controls, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.ExtCtrls, System.Classes;

type
  TMainForm = class(TForm)
    SVGIconImageList1: TSVGIconImageList;
    CategoryButtons1: TCategoryButtons;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    SVGIconImage: TSVGIconImage;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure SVGIconImageClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    {$IFDEF HiDPISupport}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    {$ENDIF}
    procedure UpdateGUI;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Themes
  {$IFDEF DXE3+}
  , UITypes
  {$ENDIF}
  , SVGIconUtils
  , SVGTextPropertyEditorUnit
  ;

{$IFDEF HiDPISupport}
procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  UpdateGUI;
end;
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
begin
  SVGIconImage.svg.PaintTo(PaintBox1.Canvas.Handle,
    PaintBox1.ClientRect, False);
end;

procedure TMainForm.SVGIconImageClick(Sender: TObject);
var
  LSVGText: string;
begin
  LSVGText := SVGIconImage.SVGText;
  SVGIconImage.ImageList := nil;
  SVGIconImage.SVGText := LSVGText;
  if EditSVGTextProperty(LSVGText) then
    SVGIconImage.SVGText := LSVGText;
end;

procedure TMainForm.updateGUI;
begin
  ;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
