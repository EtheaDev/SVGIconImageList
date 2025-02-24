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
  , System.Generics.Collections
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

  TIconifyCollection = class
  private
    FName: string;
    FTotal: Integer;
    FCategory: string;
    FPrefix: string;
  public
    property Name: string read FName write FName;
    property Prefix: string read FPrefix write FPrefix;
    property Total: Integer read FTotal write FTotal;
    property Category: string read FCategory write FCategory;
  end;

  TIconifyCollections = class(TObjectList<TIconifyCollection>)
  end;

  TIconifyCollectionIcons = class
  private
    FTotal: Integer;
    FTitle: string;
    FIcons: TArray<string>;
    FPrefix: string;
  public
    property Title: string read FTitle write FTitle;
    property Prefix: string read FPrefix write FPrefix;
    property Total: Integer read FTotal write FTotal;
    property Icons: TArray<string> read FIcons write FIcons;
  end;

  TIconifyApi = class
  public
    procedure Search(const APattern, APrefix: string; const AMaxIcons: Integer;
      AResult: TIconifySearch);
    function Download(const AName: string): string;
    procedure Collections(AResult: TIconifyCollections);
    procedure Collection(const APrefix: string; AResult: TIconifyCollectionIcons);
  end;

implementation

{ TIconifyApi }

const
  BaseUrl = 'https://api.iconify.design/';

procedure TIconifyApi.Collection(const APrefix: string;
  AResult: TIconifyCollectionIcons);
var
  LHttpClient: THTTPClient;
  LHttpResponse: IHTTPResponse;
  LJsonObject: TJSONObject;
  LJsonString: string;
  LURL: string;
  LIconName: TJSONValue;
  LJsonPair: TJSONPair;
begin
  LHttpClient := THTTPClient.Create;
  try
    LURL := Format('%scollection?prefix=%s', [BaseUrl, APrefix]);
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
      //TJson.JsonToObject(AResult, LJsonObject);
      AResult.Prefix := LJsonObject.GetValue<string>('prefix');
      AResult.Total := LJsonObject.GetValue<Integer>('total');
      AResult.Title := LJsonObject.GetValue<string>('title');
      AResult.Icons := [];
      if LJsonObject.GetValue('categories') <> nil then
      begin
        for LJsonPair in LJsonObject.GetValue('categories') as TJSONObject do
        begin
          for LIconName in LJsonPair.JsonValue as TJSONArray do
          begin
            AResult.Icons := AResult.Icons + [AResult.Prefix + ':' + LIconName.Value];
          end;
        end;
      end
      else if LJsonObject.GetValue('uncategorized') <> nil then
      begin
        for LIconName in LJsonObject.GetValue('uncategorized') as TJSONArray do
        begin
          AResult.Icons := AResult.Icons + [AResult.Prefix + ':' + LIconName.Value];
        end;
      end;
    finally
      LJsonObject.Free;
    end;
  finally
    LHttpClient.Free;
  end;end;

procedure TIconifyApi.Collections(AResult: TIconifyCollections);
var
  LHttpClient: THTTPClient;
  LHttpResponse: IHTTPResponse;
  LJsonObject: TJSONObject;
  LJsonString: string;
  LURL: string;
  LItem: TIconifyCollection;
  LJsonPair: TJSONPair;
  LJsonItem: TJSONObject;
  LHidden: Boolean;
begin
  LHttpClient := THTTPClient.Create;
  try
    LURL := Format('%scollections', [BaseUrl]);
    LHttpResponse := LHttpClient.Get(LURL);
    if LHttpResponse.StatusCode >= 400 then
      raise TIconifyError.Create(LHttpResponse.StatusText + ' - ' + LURL);
    LJsonString := LHttpResponse.ContentAsString(TEncoding.UTF8);
    {$if compilerversion > 32}
    LJsonObject := TJSONObject.ParseJSONValue(LJsonString, False, True) as TJSONObject;
    {$else}
    LJsonObject := TJSONObject.ParseJSONValue(LJsonString) as TJSONObject;
    {$endif}
    try
      AResult.Clear;
      for LJsonPair in LJsonObject do
      begin
        LJsonItem := LJsonPair.JsonValue as TJSONObject;
        if LJsonItem.TryGetValue<Boolean>('hidden', LHidden) and LHidden then
          Continue;
        LItem := TIconifyCollection.Create;
        LItem.Prefix := LJsonPair.JsonString.Value;
        TJson.JsonToObject(LItem, LJsonItem);
        AResult.Add(LItem);
      end;
    finally
      LJsonObject.Free;
    end;
  finally
    LHttpClient.Free;
  end;
end;

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

procedure TIconifyApi.Search(const APattern, APrefix: string;
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
    LURL := Format('%ssearch?query=%s&limit=%d&prefix=%s', [BaseUrl, APattern, AMaxIcons, APrefix]);
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
