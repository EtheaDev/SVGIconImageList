{******************************************************************************}
{                                                                              }
{       Icon SVG ImageList: An extended ImageList for Delphi/VCL               }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/SVGIconImageList                           }
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
unit SVGIconUtils;

interface

{$INCLUDE SVGIconImageList.inc}

uses
  Classes
  , ImgList
  , SVGIconImageList
  , Graphics
  , ComCtrls;

function UpdateSVGIconListView(const AListView: TListView): Integer;
function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;

implementation

uses
  SysUtils
  , Windows
  , Themes
  ;

function UpdateSVGIconListView(const AListView: TListView): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  LListItem: TListItem;
  LSVGIconImageList: TSVGIconImageList;
begin
  LSVGIconImageList := AListView.LargeImages as TSVGIconImageList;
  AListView.Items.BeginUpdate;
  try
    AListView.Clear;
    Result := LSVGIconImageList.SVGIconItems.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LSVGIconImageList.SVGIconItems[I];
      LListItem := AListView.Items.Add;
      LListItem.Caption := Format('%d.%s',
        [LItem.Index, LItem.IconName]);
      LListItem.ImageIndex := I;
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

function UpdateSVGIconListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TSVGIconItem;
  LListItem: TListItem;
  LSVGIconImageList: TSVGIconImageList;
begin
  LSVGIconImageList := AListView.LargeImages as TSVGIconImageList;
  AListView.Items.BeginUpdate;
  try
    Result := LSVGIconImageList.SVGIconItems.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LSVGIconImageList.SVGIconItems[I];
      LListItem := AListView.Items[I];
      if AShowCaption then
      begin
        LListItem.Caption := Format('%d.%s',
          [LItem.Index, LItem.IconName]);
      end
      else
        LListItem.Caption := '';
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

end.
