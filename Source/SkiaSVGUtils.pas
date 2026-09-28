{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors: Vincent Parrett, Kiriakos Vlahos                         }
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
unit SkiaSVGUtils;

interface

function InlineSvgStyle(const SvgText: string): string;

implementation

uses
  System.SysUtils
  , System.Generics.Collections
  , System.RegularExpressions
  ;

{-----------------------------------------------------------------------------
 Function: InlineSvgStyle
 Author: Zoomicon.Media.FMX.Delphi
 Original Unit: Zoomicon.Media.FMX.SkiaUtils
 License: MIT License
 Rewritten by Ethea: the style block is located with a regular expression
 (<style type="text/css"> included) and removed before the classes are
 inlined, selectors are read only before a "{" (so "1.5" is not a selector),
 class="a b" merges every class, and an attribute set by a class replaces the
 presentation attribute of the same name (CSS wins) instead of duplicating it.
-----------------------------------------------------------------------------}
function InlineSvgStyle(const SvgText: string): string;
const
  //Presentation attributes a class rule may be turned into
  SupportedProps: array[0..21] of string = (
    'fill', 'fill-opacity', 'fill-rule', 'stroke', 'stroke-width',
    'stroke-opacity', 'stroke-linecap', 'stroke-linejoin', 'stroke-dasharray',
    'stroke-dashoffset', 'stroke-miterlimit', 'opacity', 'stop-color',
    'stop-opacity', 'display', 'visibility', 'font-family', 'font-size',
    'font-weight', 'font-style', 'text-anchor', 'clip-rule');
var
  ClassMap: TDictionary<string, TList<TPair<string, string>>>;

  function IsSupported(const AName: string): Boolean;
  var
    LName: string;
  begin
    for LName in SupportedProps do
      if SameText(LName, AName) then
        Exit(True);
    Result := False;
  end;

  procedure AddProp(const AClassName, AName, AValue: string);
  var
    LProps: TList<TPair<string, string>>;
    I: Integer;
  begin
    if not ClassMap.TryGetValue(AClassName, LProps) then
    begin
      LProps := TList<TPair<string, string>>.Create;
      ClassMap.Add(AClassName, LProps);
    end;
    //A later rule for the same property wins
    for I := LProps.Count - 1 downto 0 do
      if SameText(LProps[I].Key, AName) then
        LProps.Delete(I);
    LProps.Add(TPair<string, string>.Create(AName, AValue));
  end;

  procedure ParseCss(const ACss: string);
  var
    LRule: TMatch;
    LSelector, LClassName, LProp, LName, LValue: string;
    LStrokeParts: TArray<string>;
    LColon: Integer;
  begin
    //selector-list { body }: the selector is whatever comes before "{"
    for LRule in TRegEx.Matches(ACss, '([^{}]+)\{([^{}]*)\}') do
    begin
      for LSelector in LRule.Groups[1].Value.Split([',']) do
      begin
        //only plain class selectors: ".name"
        LClassName := Trim(LSelector);
        if not TRegEx.IsMatch(LClassName, '^\.[A-Za-z_][\w-]*$') then
          Continue;
        Delete(LClassName, 1, 1);
        for LProp in LRule.Groups[2].Value.Split([';']) do
        begin
          LColon := Pos(':', LProp);
          if LColon = 0 then
            Continue;
          LName := LowerCase(Trim(Copy(LProp, 1, LColon - 1)));
          LValue := Trim(Copy(LProp, LColon + 1, MaxInt));
          if (LName = '') or (LValue = '') then
            Continue;
          // Expand shorthand stroke: "stroke: red 2px dashed"
          if LName = 'stroke' then
          begin
            LStrokeParts := LValue.Split([' '], TStringSplitOptions.ExcludeEmpty);
            AddProp(LClassName, 'stroke', LStrokeParts[0]);
            if Length(LStrokeParts) > 1 then
              AddProp(LClassName, 'stroke-width', LStrokeParts[1]);
            if Length(LStrokeParts) > 2 then
              AddProp(LClassName, 'stroke-dasharray', LStrokeParts[2]);
          end
          else if IsSupported(LName) then
            AddProp(LClassName, LName, LValue);
        end;
      end;
    end;
  end;

  function InlineTag(const ATag: string): string;
  var
    LAttr: TMatch;
    LClassValue, LClassName: string;
    LInline: TList<TPair<string, string>>;
    LProps: TList<TPair<string, string>>;
    LPair: TPair<string, string>;
    LTagEnd, LNamePart, LAttrs: string;
    LTail: Integer;
    I: Integer;
    LOverridden: Boolean;
  begin
    LClassValue := '';
    LAttr := TRegEx.Match(ATag, '\sclass\s*=\s*("([^"]*)"|''([^'']*)'')');
    if not LAttr.Success then
      Exit(ATag);
    if LAttr.Groups.Count > 2 then
      LClassValue := LAttr.Groups[2].Value;
    if (LClassValue = '') and (LAttr.Groups.Count > 3) then
      LClassValue := LAttr.Groups[3].Value;

    LInline := TList<TPair<string, string>>.Create;
    try
      //every class, in order: a later class wins
      for LClassName in LClassValue.Split([' ', #9], TStringSplitOptions.ExcludeEmpty) do
        if ClassMap.TryGetValue(LClassName, LProps) then
          for LPair in LProps do
          begin
            for I := LInline.Count - 1 downto 0 do
              if SameText(LInline[I].Key, LPair.Key) then
                LInline.Delete(I);
            LInline.Add(LPair);
          end;

      //split "<name" / attributes / ">" or "/>"
      if ATag.EndsWith('/>') then
        LTail := 2
      else
        LTail := 1;
      LTagEnd := Copy(ATag, Length(ATag) - LTail + 1, LTail);
      LNamePart := TRegEx.Match(ATag, '^<[^\s/>]+').Value;
      LAttrs := Copy(ATag, Length(LNamePart) + 1, Length(ATag) - Length(LNamePart) - LTail);

      Result := LNamePart;
      //keep every attribute but class and the ones a class overrides
      for LAttr in TRegEx.Matches(LAttrs, '([\w:.-]+)\s*=\s*("[^"]*"|''[^'']*'')') do
      begin
        if SameText(LAttr.Groups[1].Value, 'class') then
          Continue;
        LOverridden := False;
        for LPair in LInline do
          if SameText(LPair.Key, LAttr.Groups[1].Value) then
          begin
            LOverridden := True;
            Break;
          end;
        if not LOverridden then
          Result := Result + ' ' + LAttr.Value;
      end;
      for LPair in LInline do
        Result := Result + ' ' + LPair.Key + '="' + LPair.Value + '"';
      Result := Result + LTagEnd;
    finally
      LInline.Free;
    end;
  end;

var
  LStyle: TMatch;
  LCss: string;
  LProps: TList<TPair<string, string>>;
  LTag: TMatch;
  LTagRegEx: TRegEx;
  LPos: Integer;
begin
  Result := SvgText;

  // Locate the <style> block (with or without attributes)
  LStyle := TRegEx.Match(Result, '<style\b[^>]*>(.*?)</style\s*>', [roIgnoreCase, roSingleLine]);
  if not LStyle.Success then
    Exit;

  // CSS content, without CDATA wrapper and comments
  LCss := LStyle.Groups[1].Value;
  LCss := StringReplace(LCss, '<![CDATA[', '', [rfReplaceAll]);
  LCss := StringReplace(LCss, ']]>', '', [rfReplaceAll]);
  LCss := TRegEx.Replace(LCss, '/\*.*?\*/', '', [roSingleLine]);

  // Remove the style block first: positions computed on the original text
  // would be invalid once the classes are replaced
  Delete(Result, LStyle.Index, LStyle.Length);

  ClassMap := TDictionary<string, TList<TPair<string, string>>>.Create;
  try
    ParseCss(LCss);

    // Rewrite every tag that has a class attribute
    LTagRegEx := TRegEx.Create('<[A-Za-z][^<>]*\sclass\s*=[^<>]*>');
    LPos := 1;
    while True do
    begin
      LTag := LTagRegEx.Match(Result, LPos);
      if not LTag.Success then
        Break;
      LCss := InlineTag(LTag.Value);
      Result := Copy(Result, 1, LTag.Index - 1) + LCss +
        Copy(Result, LTag.Index + LTag.Length, MaxInt);
      LPos := LTag.Index + Length(LCss);
    end;
  finally
    for LProps in ClassMap.Values do
      LProps.Free;
    ClassMap.Free;
  end;
end;

end.
