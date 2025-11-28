{******************************************************************************}
{                                                                              }
{       SVGIconImageList: An extended ImageList for Delphi/VCL                 }
{       to simplify use of SVG Icons (resize, opacity and more...)             }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
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
  , System.StrUtils
  ;

{-----------------------------------------------------------------------------
 Function: InlineSvgStyle
 Author: Zoomicon.Media.FMX.Delphi
 Original Unit: Zoomicon.Media.FMX.SkiaUtils
 License: MIT License
-----------------------------------------------------------------------------}
function InlineSvgStyle(const SvgText: string): string;
begin
  // Copy input SVG into Result
  Result := SvgText;

  // Locate <style> block boundaries
  var StyleStart := Pos('<style>', Result);
  var StyleEnd := Pos('</style>', Result);

  // Proceed only if a valid style block is found
  if (StyleStart > 0) and (StyleEnd > StyleStart) then
  begin
    // Extract raw CSS content between <style> and </style>
    var StyleBlock := Copy(Result, StyleStart + Length('<style>'),
      StyleEnd - (StyleStart + Length('<style>')));

    // Dictionary to map class names to merged inline attributes
    var ClassMap: TDictionary<string, string> := TDictionary<string, string>.Create;
    try
      // Robust scan: parse successive ".selector{...}" segments across the entire style block
      var idx := 1;
      while idx <= Length(StyleBlock) do
      begin
        // Find start of selector
        var selStart := PosEx('.', StyleBlock, idx);
        if selStart = 0 then Break;

        // Find opening and closing braces
        var openBrace := PosEx('{', StyleBlock, selStart + 1);
        if openBrace = 0 then Break;
        var closeBrace := PosEx('}', StyleBlock, openBrace + 1);
        if closeBrace = 0 then Break;

        // Extract selector list and rule body
        var SelectorPart := Trim(Copy(StyleBlock, selStart, openBrace - selStart));
        var RuleBody := Copy(StyleBlock, openBrace + 1, closeBrace - openBrace - 1);

        // Advance index past this rule
        idx := closeBrace + 1;

        // Split grouped selectors like ".cls-2,.cls-3"
        var ClassNames := SelectorPart.Split([','], TStringSplitOptions.ExcludeEmpty);
        var Props := RuleBody.Split([';'], TStringSplitOptions.ExcludeEmpty);

        // Process each selector
        for var RawClass in ClassNames do
        begin
          var ClassName := Trim(RawClass);
          if ClassName.StartsWith('.') then Delete(ClassName, 1, 1); // remove leading dot

          var Existing := '';
          ClassMap.TryGetValue(ClassName, Existing); // merge with prior attributes if any

          // Process each CSS property
          for var Prop in Props do
          begin
            var Parts := Prop.Split([':'], TStringSplitOptions.ExcludeEmpty);
            if Length(Parts) = 2 then
            begin
              var Name := Trim(Parts[0]);
              var Value := Trim(Parts[1]);

              // Expand shorthand stroke: "stroke: red 2px dashed"
              if Name = 'stroke' then
              begin
                var StrokeParts := Value.Split([' '], TStringSplitOptions.ExcludeEmpty);
                if Length(StrokeParts) > 0 then Existing := Existing + ' stroke="' + StrokeParts[0] + '"';
                if Length(StrokeParts) > 1 then Existing := Existing + ' stroke-width="' + StrokeParts[1] + '"';
                if Length(StrokeParts) > 2 then Existing := Existing + ' stroke-dasharray="' + StrokeParts[2] + '"';
              end
              // Map supported properties directly to inline attributes
              else if (Name = 'fill') or (Name = 'stroke-width') or
                      (Name = 'fill-opacity') or (Name = 'stroke-opacity') or
                      (Name = 'stroke-linecap') or (Name = 'stroke-linejoin') or
                      (Name = 'stroke-dasharray') or (Name = 'fill-rule') or
                      (Name = 'font-family') or (Name = 'font-size') or
                      (Name = 'font-weight') then
                Existing := Existing + ' ' + Name + '="' + Value + '"';
            end;
          end;

          // Store merged attributes for this class
          ClassMap.AddOrSetValue(ClassName, Trim(Existing));
          // Log.d('ClassMap[' + ClassName + '] = ' + ClassMap[ClassName]);
        end;
      end;

      // Replace each class="..." in SVG with inline attributes
      var ScanPos := 1;
      while ScanPos < Length(Result) do
      begin
        // Find next class="..." occurrence
        var PosClass := PosEx('class="', Result, ScanPos);
        if PosClass = 0 then Break;

        // Extract class name between quotes
        var StartPos := PosClass + Length('class="');
        var EndPos := StartPos;
        while (EndPos <= Length(Result)) and (Result[EndPos] <> '"') do Inc(EndPos);
        var ClassName := Copy(Result, StartPos, EndPos - StartPos);

        // Replace or remove based on dictionary lookup
        if ClassMap.ContainsKey(ClassName) then
        begin
          var InlineAttrs := ClassMap[ClassName];

          Result := Copy(Result, 1, PosClass - 1) + InlineAttrs +
                       Copy(Result, EndPos + 1, Length(Result) - EndPos);

          // Log.d('Replaced class="' + ClassName + '" with: ' + InlineAttrs);
        end
        else
        begin
          Result := Copy(Result, 1, PosClass - 1) +
                       Copy(Result, EndPos + 1, Length(Result) - EndPos);

          // Log.d('Removed class="' + ClassName + '" (no matching style)');
        end;

        // Advance scan position to continue parsing
        ScanPos := PosClass + 1;
      end;

      // Remove the original <style> block entirely
      Delete(Result, StyleStart, StyleEnd + Length('</style>') - StyleStart);

    finally
      // Guaranteed cleanup: free dictionary and nil reference
      FreeAndNil(ClassMap);
    end;
  end;
end;

end.
