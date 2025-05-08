unit MakeSVGHashConsts;

//include this unit in a sample application and execute it once
//to update any new hashes for the Image32_SVG_Reader unit.


interface

uses
  SysUtils, Classes, Types, Math,
  Img32, Img32.SVG.Core, Img32.Vector,
  Img32.Transform, Img32.SVG.Reader, AnsiStrings;

implementation

type
 TAnsi = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    text: PAnsiChar;
    len : integer;
    function AsUtf8String: AnsiString;
  end;

function TAnsi.AsUtf8String: AnsiString;
begin
  SetLength(Result, len);
  if len > 0 then
    Move(text^, Result[1], len);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function ParseNameLength(var c: PAnsiChar; endC: PAnsiChar): integer; overload;
var
  c2: PAnsiChar;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  c2 := c;
  inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

function GetHashedName(const s: string): cardinal; overload;
var
  ansi: ansiString;
  name: TAnsi;
  c, endC: PAnsiChar;
begin
  ansi := ansiString(s);
  name.text := PAnsiChar(ansi);
  if s[1] = '&' then
  begin
    c := @ansi[2];
    endC := c + Length(ansi) -1;
    name.len := ParseNameLength(c, endC) +1;
  end else
  begin
    c := @ansi[1];
    endC := c + Length(ansi);
    name.len := ParseNameLength(c, endC);
  end;
  Result := Img32.SVG.Core.GetHash(name.AsUTF8String);
end;
//------------------------------------------------------------------------------

function GetHashedNameCaseSens(const s: string): cardinal; overload;
var
  ansi: ansiString;
  name: TAnsi;
  c, endC: PAnsiChar;
begin
  ansi := ansiString(s);
  name.text := PAnsiChar(ansi);
  if s[1] = '&' then
  begin
    c := @ansi[2];
    endC := c + Length(ansi) -1;
    name.len := ParseNameLength(c, endC) +1;
  end else
  begin
    c := @ansi[1];
    endC := c + Length(ansi);
    name.len := ParseNameLength(c, endC);
  end;
  Result := Img32.SVG.Core.GetHashCaseSensitive(name.text, name.len);
end;
//------------------------------------------------------------------------------

procedure MakeHashesConsts(hashHtmlEntities: Boolean);
var
  i: integer;
  hash: Cardinal;
  slNames, sl: TStringList;

  procedure AddName(name: AnsiString; caseSensitive: Boolean = false);
  begin
    if caseSensitive then
      slNames.AddObject(string(name), TObject(1)) else
      slNames.Add(string(name));
  end;

  function TidyName(const name: string): string;
  var
    i,j,len: integer;
    s: string;
    n: Char;
  begin
    //hyphens etc not allowed in const names
    len := Length(name);
    //accommodate prepended 'h' plus (?) non-alphaNum.
    SetLength(Result, len + 1 + 5 * (5));
    Result[1] := 'h';
    j := 2;
    for i := 1 to len do
    begin
      n := Upcase(name[i]);
      if ((n >= 'A') and (n <= 'Z')) or ((n >= '0') and (n <= '9')) then
      begin
        Result[j] := name[i];
        inc(j);
      end else
      begin
        Result[j] := '_';
        s := Format('%3.3d',[Ord(name[i])]);
        Result[j +1] := s[1];
        Result[j +2] := s[2];
        Result[j +3] := s[3];
        Result[j +4] := '_';
        inc(j, 5);
      end;
    end;
    SetLength(Result, j-1);
  end;

begin
  //two stringlists, one for names and one for hash consts defs
  sl := TStringList.Create;
  slNames := TStringList.Create;
  try
    if hashHtmlEntities then
    begin
      AddName('&Aacute', true);
      AddName('&aacute');
      AddName('&Acirc', true);
      AddName('&acirc');
      AddName('&acute');
      AddName('&AElig', true);
      AddName('&aelig');
      AddName('&Agrave', true);
      AddName('&agrave');
      AddName('&alefsym');
      AddName('&Alpha', true);
      AddName('&alpha');
      AddName('&amp');
      AddName('&and');
      AddName('&ang');
      AddName('&apos');
      AddName('&Aring', true);
      AddName('&aring');
      AddName('&ast');
      AddName('&asymp');
      AddName('&Atilde', true);
      AddName('&atilde');
      AddName('&Auml', true);
      AddName('&auml');
      AddName('&bigcup');
      AddName('&bigvee');
      AddName('&bigwedge');
      AddName('&brvbar');
      AddName('&bull');
      AddName('&cap');
      AddName('&Ccedil', true);
      AddName('&ccedil');
      AddName('&cedil');
      AddName('&cent');
      AddName('&Cent', true);
      AddName('&centerdot');
      AddName('&clubs');
      AddName('&cong');
      AddName('&Cong', true);
      AddName('&copy');
      AddName('&Copy', true);
      AddName('&cup');
      AddName('&curren');
      AddName('&Curren', true);
      AddName('&dagger');
      AddName('&Dagger', true);
      AddName('&darr');
      AddName('&Darr', true);
      AddName('&deg');
      AddName('&diams');
      AddName('&divide');
      AddName('&dollar');
      AddName('&dotsquare');
      AddName('&Eacute', true);
      AddName('&eacute');
      AddName('&Ecirc', true);
      AddName('&ecirc');
      AddName('&Egrave', true);
      AddName('&egrave');
      AddName('&empty');
      AddName('&emsp');
      AddName('&ensp');
      AddName('&equiv');
      AddName('&ETH', true);
      AddName('&eth');
      AddName('&Euml', true);
      AddName('&euml');
      AddName('&euro');
      AddName('&exist');
      AddName('&fnof');
      AddName('&forall');
      AddName('&frac12');
      AddName('&frac14');
      AddName('&frac34');
      AddName('&ge');
      AddName('&gt');
      AddName('&harr');
      AddName('&hArr', true);
      AddName('&hearts');
      AddName('&hellip');
      AddName('&Iacute', true);
      AddName('&iacute');
      AddName('&Icirc', true);
      AddName('&icirc');
      AddName('&iexcl');
      AddName('&Igrave', true);
      AddName('&igrave');
      AddName('&image');
      AddName('&infin');
      AddName('&int');
      AddName('&iquest');
      AddName('&isin');
      AddName('&Iuml', true);
      AddName('&iuml');
      AddName('&lang');
      AddName('&laquo');
      AddName('&larr');
      AddName('&lceil');
      AddName('&ldquo');
      AddName('&le');
      AddName('&lfloor');
      AddName('&lowast');
      AddName('&loz');
      AddName('&lsquo');
      AddName('&lt');
      AddName('&macr');
      AddName('&mdash');
      AddName('&micro');
      AddName('&middot');
      AddName('&minus');
      AddName('&nbsp');
      AddName('&ndash');
      AddName('&ne');
      AddName('&ni');
      AddName('&not');
      AddName('&notin');
      AddName('&nsub');
      AddName('&nsube');
      AddName('&nsup');
      AddName('&nsupe');
      AddName('&Ntilde', true);
      AddName('&ntilde');
      AddName('&Oacute', true);
      AddName('&oacute');
      AddName('&Ocirc', true);
      AddName('&ocirc');
      AddName('&Ograve', true);
      AddName('&ograve');
      AddName('&oplus');
      AddName('&or');
      AddName('&ordf');
      AddName('&ordm');
      AddName('&oSlash', true);
      AddName('&oslash');
      AddName('&Otilde', true);
      AddName('&otilde');
      AddName('&otimes');
      AddName('&Ouml', true);
      AddName('&ouml');
      AddName('&para');
      AddName('&part');
      AddName('&percnt');
      AddName('&permil');
      AddName('&perp');
      AddName('&phi');
      AddName('&pi');
      AddName('&plus');
      AddName('&plusmn');
      AddName('&pound');
      AddName('&Prime', true);
      AddName('&prime');
      AddName('&prop');
      AddName('&quot');
      AddName('&radic');
      AddName('&rang');
      AddName('&raquo');
      AddName('&Rarr', true);
      AddName('&rarr');
      AddName('&rceil');
      AddName('&rdquo');
      AddName('&real');
      AddName('&reg');
      AddName('&rfloor');
      AddName('&rsquo');
      AddName('&sdot');
      AddName('&sect');
      AddName('&shy');
      AddName('&sigma');
      AddName('&sim');
      AddName('&spades');
      AddName('&sub');
      AddName('&sube');
      AddName('&sum');
      AddName('&sup');
      AddName('&sup1');
      AddName('&sup2');
      AddName('&sup3');
      AddName('&supe');
      AddName('&szlig');
      AddName('&there4');
      AddName('&thinsp');
      AddName('&THORN', true);
      AddName('&thorn');
      AddName('&times');
      AddName('&trade');
      AddName('&Uacute', true);
      AddName('&uacute');
      AddName('&Uarr', true);
      AddName('&uarr');
      AddName('&Ucirc', true);
      AddName('&ucirc');
      AddName('&Ugrave', true);
      AddName('&ugrave');
      AddName('&uml');
      AddName('&Uuml', true);
      AddName('&uuml');
      AddName('&Yacute', true);
      AddName('&yacute');
      AddName('&yen');
      AddName('&Yuml', true);
      AddName('&yuml');
    end else
    begin
      AddName('Amplitude');
      AddName('arithmetic');
      AddName('Arial');
      AddName('Atop');
      AddName('auto-start-reverse');
      AddName('BackgroundAlpha');
      AddName('BackgroundImage');
      AddName('Baseline');
      AddName('baseline-shift');
      AddName('Bevel');
      AddName('Bold');
      AddName('Bolder');
      AddName('Butt');
      AddName('CDATA');
      AddName('Circle');
      AddName('Class');
      AddName('Clip-path');
      AddName('Clippath');
      AddName('collapse');
      AddName('Color');
      AddName('CurrentColor');
      AddName('Cx');
      AddName('Cy');
      AddName('D');
      AddName('Darken');
      AddName('Defs');
      AddName('diffuseConstant');
      AddName('Discrete');
      AddName('Display');
      AddName('Dx');
      AddName('Dy');
      AddName('Ellipse');
      AddName('End');
      AddName('Entity');
      AddName('Exponent');
      AddName('feBlend');
      AddName('feColorMatrix');
      AddName('feComposite');
      AddName('feComponentTransfer');
      AddName('feDefuseLighting');
      AddName('feDropShadow');
      AddName('feFlood');
      AddName('flood-color');
      AddName('flood-opacity');
      AddName('feFuncA');
      AddName('feFuncB');
      AddName('feFuncG');
      AddName('feFuncR');
      AddName('feGaussianBlur');
      AddName('feImage');
      AddName('feMerge');
      AddName('feMergeNode');
      AddName('feOffset');
      AddName('fePointLight');
      AddName('feSpecularLighting');
      AddName('Fill-Opacity');
      AddName('Fill-Rule');
      AddName('Fill');
      AddName('Filter');
      AddName('FlowRegion');
      AddName('FlowRoot');
      AddName('Font-Family');
      AddName('Font-Size');
      AddName('Font-Style');
      AddName('Font-Weight');
      AddName('Font');
      AddName('Fx');
      AddName('Fy');
      AddName('G');
      AddName('GradientTransform');
      AddName('GradientUnits');
      AddName('Gramma');
      AddName('Height');
      AddName('hidden');
      AddName('Href');
      AddName('Id');
      AddName('Image');
      AddName('In');
      AddName('In2');
      AddName('Intercept');
      AddName('Italic');
      AddName('Justify');
      AddName('kernelUnitLength');
      AddName('K1');
      AddName('K2');
      AddName('K3');
      AddName('K4');
      AddName('letter-spacing');
      AddName('Lighten');
      AddName('Lighter');
      AddName('lighting-color');
      AddName('Line');
      AddName('line-through');
      AddName('Linear');
      AddName('LinearGradient');
      AddName('Marker-End');
      AddName('Marker-Mid');
      AddName('Marker-Start');
      AddName('Marker');
      AddName('Mask');
      AddName('MarkerHeight');
      AddName('MarkerWidth');
      AddName('Matrix');
      AddName('Middle');
      AddName('Miter');
      AddName('Mode');
      AddName('Monospace');
      AddName('Multiply');
      AddName('None');
      AddName('Normal');
      AddName('ObjectBoundingBox');
      AddName('Offset');
      AddName('Opacity');
      AddName('Operator');
      AddName('Orient');
      AddName('Out');
      AddName('Over');
      AddName('Overflow-Wrap');
      AddName('Overlay');
      AddName('Pad');
      AddName('Path');
      AddName('Pattern');
      AddName('PatternContentUnits');
      AddName('PatternUnits');
      AddName('PatternTransform');
      AddName('Points');
      AddName('Polygon');
      AddName('Polyline');
      AddName('R');
      AddName('RadialGradient');
      AddName('Rect');
      AddName('Reflect');
      AddName('RefX');
      AddName('RefY');
      AddName('Repeat');
      AddName('Result');
      AddName('Rotate');
      AddName('Round');
      AddName('Rx');
      AddName('Ry');
      AddName('Sans');
      AddName('Sans-Serif');
      AddName('Screen');
      AddName('Serif');
      AddName('Slope');
      AddName('SourceAlpha');
      AddName('SourceGraphic');
      AddName('space'); // xml:space
      AddName('specularExponent');
      AddName('SpreadMethod');
      AddName('Square');
      AddName('Start');
      AddName('StartOffset');
      AddName('stdDeviation');
      AddName('Stop-Color');
      AddName('Stop-Opacity');
      AddName('Stop');
      AddName('Stroke-DashArray');
      AddName('Stroke-DashOffset');
      AddName('stroke-linecap');
      AddName('stroke-linejoin');
      AddName('stroke-miterlimit');
      AddName('Stroke-Opacity');
      AddName('Stroke-Width');
      AddName('Stroke');
      AddName('Style');
      AddName('Sub');
      AddName('Super');
      AddName('surfaceScale');
      AddName('Svg');
      AddName('Switch');
      AddName('Symbol');
      AddName('Table');
      AddName('TableValues');
      AddName('Text');
      AddName('Text-Anchor');
      AddName('Text-Decoration');
      AddName('TextArea');
      AddName('TextLength');
      AddName('TextPath');
      AddName('Times');
      AddName('Transform');
      AddName('TSpan');
      AddName('Type');
      AddName('Underline');
      AddName('Url');
      AddName('Use');
      AddName('UserSpaceOnUse');
      AddName('Values');
      AddName('Viewbox');
      AddName('Visibility');
      AddName('visible');
      AddName('White-Space');
      AddName('Word-Break');
      AddName('Width');
      AddName('X');
      AddName('X1');
      AddName('X2');
      AddName('Xlink:Href');
      AddName('Xor');
      AddName('Y');
      AddName('Y1');
      AddName('Y2');
      AddName('Z');
    end;

    //check for hash collisions
    sl.Duplicates := dupError;
    sl.Sorted := true;
    for i := 0 to slNames.Count -1 do
      if Assigned(slNames.Objects[i]) then //ie flagged as case sensitive
        sl.Add(Format('%8.8x;',[GetHashedNameCaseSens(slNames[i])]))
      else
        sl.Add(Format('%8.8x;',[GetHashedName(slNames[i])]));
    //Yay! No collisions :)
    sl.Clear;

    for i := 0 to slNames.Count -1 do
      if Assigned(slNames.Objects[i]) then
      begin
        hash := GetHashedNameCaseSens(slNames[i]);
        sl.Add(Format('  %-28s= $%8.8x;    // %s',
          [TidyName(slNames[i]) + '_', hash, slNames[i]]));
      end else
      begin
        hash := GetHashedName(slNames[i]);
        sl.Add(Format('  %-28s= $%8.8x;    // %s',
          [TidyName(slNames[i]), hash, slNames[i]]));

      end;
    sl.Sorted := false;
    sl.Insert(0, 'const');
    //sl.Insert(0, '  //nb: hash identifiers with appended underscores are case sensitive.');
    if hashHtmlEntities then
      sl.SaveToFile('./Img32.SVG.HtmlHashConsts.inc') else
      sl.SaveToFile('./Img32.SVG.HashConsts.inc');
  finally
    sl.Free;
    slNames.Free;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeHashesConsts(false);
  //MakeHashesConsts(true);
end.
