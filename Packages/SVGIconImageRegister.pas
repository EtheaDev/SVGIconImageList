{******************************************************************************}
{                                                                              }
{       SVGIconImage Registration for Components and Editors                   }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconsImageList                          }
{                                                                              }
{******************************************************************************}
{       Original version (c) 2005, 2008 Martin Walter with license:            }
{       Use of this file is permitted for commercial and non-commercial        }
{       use, as long as the author is credited.                                }
{       home page: http://www.mwcs.de                                          }
{       email    : martin.walter@mwcs.de                                       }
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
unit SVGIconImageRegister;

interface

uses
  Classes
  , DesignIntf
  , DesignEditors;

type
  TSVGIconImageListCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TSVGIconImageListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;


procedure Register;

implementation

uses
  SysUtils
  , ShellApi
  , Windows
  , SVGIconImage
  , SVGIconImageList
  , SVGIconImageListEditorUnit;

{ TSVGIconImageListCompEditor }
procedure TSVGIconImageListCompEditor.Edit;
begin
  inherited;
end;

procedure TSVGIconImageListCompEditor.ExecuteVerb(Index: Integer);
var
  LCompEditor: TSVGIconImageListEditor;
begin
  inherited;
  if Index = 0 then
  begin
    LCompEditor := TSVGIconImageListEditor.CreateSVGIconImageListEditor(nil,
      Component as TSVGIconImageList);
    try
      LCompEditor.ShowModal;
      if LCompEditor.Modified then
        Designer.Modified;
    finally
      LCompEditor.Free;
    end;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;
end;

function TSVGIconImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'SVG I&con ImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[SVGIconImageListVersion]);
  end;
end;

function TSVGIconImageListCompEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TSVGIconImageListProperty }
procedure TSVGIconImageListProperty.Edit;
var
  LCompEditor: TSVGIconImageListEditor;
  SVGImageList: TSVGIconImageList;
begin
  SVGImageList := TSVGIconImageList(GetComponent(0));
  LCompEditor := TSVGIconImageListEditor.CreateSVGIconImageListEditor(nil, SVGImageList);
  try
    LCompEditor.ShowModal;
    if LCompEditor.Modified then
      Modified;
  finally
    LCompEditor.Free;
  end;
end;

function TSVGIconImageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TSVGIconImageListProperty.GetValue: string;
begin
  Result := 'SVGImages';
end;

procedure Register;
begin
  RegisterComponents('Ethea',
    [TSVGIconImage,
     TSVGIconImageList]);

  RegisterComponentEditor(TSVGIconImageList, TSVGIconImageListCompEditor);
  RegisterPropertyEditor(TypeInfo(TSVGIconItems), TSVGIconImageList, 'IconItems', TSVGIconImageListProperty);
end;

end.
