{******************************************************************************}
{ Unit Name: Browser.IconifyApi                                                }
{ Author:    Luca Minuti                                                       }
{ Purpose:   Client REST API to https://iconify.design/docs/api/               }
{ History:                                                                     }
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
unit Browser.IconifyApi;

interface

uses
  System.Classes
  , System.SysUtils
  , System.Json
  , System.Net.HttpClient
  , REST.Json
  ;

type
  TIconifyError = class(Exception);

  TIconifySearch = class
  private
    FIcons: TArray<string>;
    FLimit: Integer;
    FTotal: Integer;
    FStart: Integer;
  public
    property Icons: TArray<string> read FIcons write FIcons;
    property Total: Integer read FTotal write FTotal;
    property Limit: Integer read FLimit write FLimit;
    property Start: Integer read FStart write FStart;
  end;

  TIconifyApi = class
  public
    procedure Search(const APattern: string; const AMaxIcons: Integer;
      AResult: TIconifySearch);
    function Download(const AName: string): string;
  end;

implementation

{ TIconifyApi }

const
  BaseUrl = 'https://api.iconify.design/';

function TIconifyApi.Download(const AName: string): string;
var
  LHttpClient: THTTPClient;
  LHttpResponse: IHTTPResponse;
begin
  LHttpClient := THTTPClient.Create;
  try
    LHttpResponse := LHttpClient.Get(BaseUrl + AName + '.svg');
    if LHttpResponse.StatusCode >= 400 then
      raise TIconifyError.Create(LHttpResponse.StatusText);
    Result := LHttpResponse.ContentAsString(TEncoding.UTF8);
  finally
    LHttpClient.Free;
  end;
end;

procedure TIconifyApi.Search(const APattern: string;
  const AMaxIcons: Integer; AResult: TIconifySearch);
var
  LHttpClient: THTTPClient;
  LHttpResponse: IHTTPResponse;
  LJsonObject: TJSONObject;
  LJsonString: string;
  LURL: string;
begin
  LHttpClient := THTTPClient.Create;
  try
    LURL := Format('%ssearch?query=%s&limit=%d', [BaseUrl, APattern, AMaxIcons]);
    LHttpResponse := LHttpClient.Get(LURL);
    if LHttpResponse.StatusCode >= 400 then
      raise TIconifyError.Create(LHttpResponse.StatusText);
    LJsonString := LHttpResponse.ContentAsString(TEncoding.UTF8);
    {$if compilerversion > 32}
    LJsonObject := TJSONObject.ParseJSONValue(LJsonString, False, True) as TJSONObject;
    {$else}
    LJsonObject := TJSONObject.ParseJSONValue(LJsonString) as TJSONObject;
    {$endif}
    try
      TJson.JsonToObject(AResult, LJsonObject);
    finally
      LJsonObject.Free;
    end;
  finally
    LHttpClient.Free;
  end;
end;

end.
