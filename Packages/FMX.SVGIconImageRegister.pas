{******************************************************************************}
{                                                                              }
{       SVG Icon ImageList: An extended ImageList for Delphi                   }
{       to simplify use of Icons (resize, opacity and more...)                 }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Contributors:                                                          }
{         Carlo Barazzetta                                                     }
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
unit FMX.SVGIconImageRegister;

{$INCLUDE ..\Source\SVGIconImageList.inc}

interface

uses
  SysUtils
  , Classes
  , DesignIntf
  , DesignEditors;

type
  TSVGIconImageListCompEditorFMX = class (TComponentEditor)
  private
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TSVGIconImageCompEditorFMX = class (TComponentEditor)
  private
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TSVGTextPropertyFMX = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

uses
  FMX.SVGIconImageList
  {$IFDEF D10_3+}
  , FmxAnimationEditors
  {$ENDIF}
  , FMX.SVGIconImage
  , Winapi.ShellApi
  , Winapi.Windows
  , FMX.SVGIconImageListEditorUnit
  , FMX.SVGTextPropertyEditorUnit
  ;

{ TSVGIconImageListCompEditorFMX }

procedure TSVGIconImageListCompEditorFMX.Edit;
begin
  inherited;
end;

procedure TSVGIconImageListCompEditorFMX.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditSVGIconImageList(Component as TSVGIconImageList) then
      Designer.Modified;
  end
  else
    ShellExecute(0, 'open',
      PChar('https://ethea.it/docs/svgiconimagelist/Overview-(FMX).html'), nil, nil,
      SW_SHOWNORMAL)
end;

function TSVGIconImageListCompEditorFMX.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con VirtualImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageListCompEditorFMX.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSVGIconImageCompEditorFMX }

procedure TSVGIconImageCompEditorFMX.Edit;
begin
  inherited;
end;

procedure TSVGIconImageCompEditorFMX.ExecuteVerb(Index: Integer);
var
  LSVGText: string;
  LComponent: TSVGIconImage;
begin
  inherited;
  if Index = 0 then
  begin
    LComponent := Component as TSVGIconImage;
    LSVGText := LComponent.SVGText;
    if EditSVGTextProperty(LSVGText) then
    begin
      LComponent.SVGText := LSVGText;
      Designer.Modified;
    end;
  end
  else
    ShellExecute(0, 'open',
      PChar('https://ethea.it/docs/svgiconimagelist/Overview-(FMX).html'), nil, nil,
      SW_SHOWNORMAL)
end;

function TSVGIconImageCompEditorFMX.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG &Text Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageCompEditorFMX.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TSVGTextPropertyFMX }

procedure TSVGTextPropertyFMX.Edit;
var
  LSVGText: string;
  LComponent: TPersistent;
  LCanEdit: Boolean;
begin
  LComponent := GetComponent(0);
  if LComponent is TSVGIconImage then
  begin
    LSVGText := TSVGIconImage(LComponent).SVGText;
    LCanEdit := True;
  end
  else if LComponent is TSVGIconSourceItem then
  begin
    LSVGText := TSVGIconSourceItem(LComponent).SVGText;
    LCanEdit := False;
  end
  else
    Exit;
  if EditSVGTextProperty(LSVGText, LCanEdit) then
  begin
    if LComponent is TSVGIconImage then
      TSVGIconImage(LComponent).SVGText := LSVGText;
    Modified;
  end;
  inherited;
end;

function TSVGTextPropertyFMX.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSVGTextPropertyFMX.GetValue: string;
begin
  Result := 'Click on [...] to edit SVG Text';
end;

procedure Register;
begin
  {$IFDEF D10_3+}
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconBitmapItem, '', TFmxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconSourceItem, '', TFmxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Single), TSVGIconImageList, '', TFmxFloatProperty);
  RegisterPropertyEditor(TypeInfo(string), TSVGIconImage, 'SVGText', TSVGTextPropertyFMX);
  RegisterPropertyEditor(TypeInfo(string), TSVGIconSourceItem, 'SVGText', TSVGTextPropertyFMX);
  {$ENDIF}

  RegisterComponents('Ethea',
  [TSVGIconImage,
   TSVGIconImageList
  ]);
  RegisterComponentEditor(TSVGIconImageList, TSVGIconImageListCompEditorFMX);
  RegisterComponentEditor(TSVGIconImage, TSVGIconImageCompEditorFMX);
end;

end.
