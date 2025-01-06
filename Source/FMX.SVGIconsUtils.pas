{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/FMX                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
unit FMX.SVGIconsUtils;

interface

Uses
  System.Types
  , FMX.ListBox
  ;

function UpdateSVGIconListView(const AListBox: TListBox): Integer;

implementation

uses
  FMX.SVGIconImageList
  , System.SysUtils
  , System.Classes
  ;

function UpdateSVGIconListView(const AListBox: TListBox): Integer;
var
  I: Integer;
  LItem: TSVGIconSourceItem;
  LListItem: TListBoxItem;
  LSVGIconImageList: TSVGIconImageList;
begin
  LSVGIconImageList := AListBox.Images as TSVGIconImageList;

  AListBox.Items.BeginUpdate;
  try
    AListBox.Clear;
    Result := LSVGIconImageList.Source.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LSVGIconImageList.Source.Items[I] as TSVGIconSourceItem;
      LListItem := TListBoxItem.Create(AListBox);
      LListItem.StyleLookup := 'CustomListBoxItemStyle';
      LListItem.Text := Format('%d.%s', [LItem.Index,Litem.IconName]);
      LListItem.ImageIndex := I;

      AListBox.AddObject(LListItem);
    end;
  finally
    AListBox.Items.EndUpdate;
  end;
end;

end.
