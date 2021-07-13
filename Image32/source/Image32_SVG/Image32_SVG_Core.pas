unit Image32_SVG_Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.26                                                            *
* Date      :  11 July 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Essential structures and functions to read SVG files            *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Image32, Image32_Vector, Image32_Ttf, Image32_Transform;

type
  TTriState = (tsUnknown, tsYes, tsNo);
  TSvgEncoding = (eUnknown, eUtf8, eUnicodeLE, eUnicodeBE);

  TUnitType = (utUnknown, utNumber, utPercent, utEm, utEx, utPixel,
    utCm, utMm, utInch, utPt, utPica, utDegree, utRadian);

  //////////////////////////////////////////////////////////////////////
  // TValue - Structure to store numerics with measurement units.
  // See https://www.w3.org/TR/SVG/types.html#InterfaceSVGLength
  // and https://www.w3.org/TR/SVG/types.html#InterfaceSVGAngle
  //////////////////////////////////////////////////////////////////////

  //Unfortunately unit-less values can exhibit ambiguity, especially when their
  //values are small (eg < 1.0). These values can be either absolute values or
  //relative values (ie relative to the supplied dimension size).
  //The 'assumeRelValBelow' parameter (see below) attempts to address this
  //ambiguity, such that unit-less values will be assumed to be 'relative' when
  //'rawVal' is less than the supplied 'assumeRelValBelow' value.

  TValue = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    rawVal    : double;
    unitType  : TUnitType;
    procedure Init;
    procedure SetValue(val: double; unitTyp: TUnitType = utNumber);
    function  GetValue(relSize: double; assumeRelValBelow: Double): double;
    function  GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
    function  IsValid: Boolean;
    function  HasFontUnits: Boolean;
    function  HasAngleUnits: Boolean;
  end;

  TValuePt = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    X, Y    : TValue;
    procedure Init;
    function  GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD; overload;
    function  GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD; overload;
    function  IsValid: Boolean;
  end;

  TValueRecWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    left    : TValue;
    top     : TValue;
    width   : TValue;
    height  : TValue;
    procedure Init;
    function  GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectD(relSize: double; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
    function  IsValid: Boolean;
    function  IsEmpty: Boolean;
  end;

  //////////////////////////////////////////////////////////////////////
  // TAnsi - alternative to AnsiString with less overhead.
  //////////////////////////////////////////////////////////////////////

  TAnsi = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    text: PAnsiChar;
    len : integer;
    function AsAnsiString: AnsiString;
  end;
  TArrayOfTAnsi = array of TAnsi;

  TSvgItalicSyle  = (sfsUndefined, sfsNone, sfsItalic);
  TFontDecoration = (fdUndefined, fdNone, fdUnderline, fdStrikeThrough);
  TSvgTextAlign = (staUndefined, staLeft, staCenter, staRight);

  TSVGFontInfo = record
    family      : TTtfFontFamily;
    size        : double;
    spacing     : double;
    textLength  : double;
    italic      : TSvgItalicSyle;
    weight      : Integer;
    align       : TSvgTextAlign;
    decoration  : TFontDecoration;
    baseShift   : TValue;
  end;

  //////////////////////////////////////////////////////////////////////
  // TClassStylesList: custom TStringList that stores ansistring objects
  //////////////////////////////////////////////////////////////////////

  PAnsStringiRec = ^TAnsiStringRec;   //used internally by TClassStylesList
  TAnsiStringRec = record
    ansi  : AnsiString;
  end;

  TClassStylesList = class
  private
    fList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddAppendStyle(const classname: string; const ansi: AnsiString): integer;
    function  GetStyle(const classname: AnsiString): AnsiString;
    procedure Clear;
  end;

  //////////////////////////////////////////////////////////////////////
  // TSvgParser and associated classes - a simple parser for SVG xml
  //////////////////////////////////////////////////////////////////////

  PSvgAttrib = ^TSvgAttrib;   //element attribute
  TSvgAttrib = record
    hash      : Cardinal;     //hashed name
    name      : AnsiString;
    value     : AnsiString;
  end;

  TSvgParser = class;

  TXmlEl = class              //base element class
  public
    name        : TAnsi;
    {$IFDEF XPLAT_GENERICS}
    attribs     : TList <PSvgAttrib>;
    {$ELSE}
    attribs     : TList;
    {$ENDIF}
    owner       : TSvgParser;
    selfClosed  : Boolean;
    constructor Create(owner: TSvgParser); virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    function    ParseHeader(var c: PAnsiChar; endC: PAnsiChar): Boolean; virtual;
    function    ParseAttribName(var c: PAnsiChar; endC: PAnsiChar; attrib: PSvgAttrib): Boolean;
    function    ParseAttribValue(var c: PAnsiChar; endC: PAnsiChar; attrib: PSvgAttrib): Boolean;
    function    ParseAttributes(var c: PAnsiChar; endC: PAnsiChar): Boolean; virtual;
    procedure   ParseStyleAttribute(const style: AnsiString);
  end;

  TDocTypeEl = class(TXmlEl)
  private
    procedure   SkipWord(var c, endC: PAnsiChar);
    function    ParseEntities(var c, endC: PAnsiChar): Boolean;
  public
    function    ParseAttributes(var c: PAnsiChar; endC: PAnsiChar): Boolean; override;
  end;

  TSvgTreeEl = class(TXmlEl)
  public
    hash        : Cardinal;
    text        : TAnsi;
    {$IFDEF XPLAT_GENERICS}
    childs      : TList<TSvgTreeEl>;
    {$ELSE}
    childs      : TList;
    {$ENDIF}
    constructor Create(owner: TSvgParser); override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    ParseHeader(var c: PAnsiChar; endC: PAnsiChar): Boolean; override;
    function    ParseContent(var c: PAnsiChar; endC: PAnsiChar): Boolean; virtual;
  end;

  TSvgParser = class
  private
    svgStream : TMemoryStream;
    procedure ParseStream;
  public
    classStyles :TClassStylesList;
    xmlHeader   : TXmlEl;
    docType     : TDocTypeEl;
    svgTree     : TSvgTreeEl;
    {$IFDEF XPLAT_GENERICS}
    entities    : TList<TSvgTreeEl>;
    {$ELSE}
    entities    : TList;
    {$ENDIF}
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    function  FindEntity(hash: Cardinal): PSvgAttrib;
    function  LoadFromFile(const filename: string): Boolean;
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromString(const str: string): Boolean;
  end;

  //////////////////////////////////////////////////////////////////////
  //TSvgPath structures
  //////////////////////////////////////////////////////////////////////

  TSvgPathSegType = (dsUnknown, dsMove, dsLine, dsHorz, dsVert, dsArc,
    dsQBez, dsCBez, dsQSpline, dsCSpline, dsClose);

  PSvgPathSeg = ^TSvgPathSeg;
  TSvgPathSeg = record
    segType : TSvgPathSegType;
    vals    : TArrayOfDouble;
  end;

  PSvgPath = ^TSvgPath;
  TSvgPath = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    firstPt   : TPointD;
    segs      : array of TSvgPathSeg;
    function GetBounds: TRectD;
    //scalePending: if an SVG will be scaled later, then this parameter
    //allows curve 'flattening' to occur with a corresponding precision
    function GetFlattenedPath(scalePending: double = 1.0): TPathD;
    //GetSimplePath - ignores curves and is only used with markers
    function GetSimplePath: TPathD;
    function IsClosed: Boolean;
  end;
  TSvgPaths = array of TSvgPath;

  //////////////////////////////////////////////////////////////////////
  // Miscellaneous SVG functions
  //////////////////////////////////////////////////////////////////////

  //general parsing functions //////////////////////////////////////////
  function ParseNextWord(var c: PAnsiChar; endC: PAnsiChar;
    out word: AnsiString): Boolean;
  function ParseNextWordEx(var c: PAnsiChar; endC: PAnsiChar;
    out word: AnsiString): Boolean;
  function ParseNextNum(var c: PAnsiChar; endC: PAnsiChar;
    skipComma: Boolean; out val: double): Boolean;
  function ParseNextNumEx(var c: PAnsiChar; endC: PAnsiChar; skipComma: Boolean;
    out val: double; out unitType: TUnitType): Boolean;
  function GetHash(const name: AnsiString): cardinal;
  function GetHashCaseSensitive(name: PAnsiChar; nameLen: integer): cardinal;
  function ExtractRef(const href: AnsiString): AnsiString;
  function IsNumPending(var c: PAnsiChar;
    endC: PAnsiChar; ignoreComma: Boolean): Boolean;
  function AnsiStringToColor32(const value: AnsiString; var color: TColor32): Boolean;
  function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
  function Match(c: PAnsiChar; const compare: AnsiString): Boolean; overload;
  function PAnsiCharToTAnsi(var c: PAnsiChar; endC: PAnsiChar; out value: TAnsi): Boolean;
  procedure PAnsiCharToAnsiString(var c: PAnsiChar; endC: PAnsiChar; out value: AnsiString);

  //special parsing functions //////////////////////////////////////////
  function ParseSvgPath(const value: AnsiString): TSvgPaths;
  procedure ParseStyleElementContent(const value: TAnsi; stylesList: TClassStylesList);
  function ParseTransform(const transform: AnsiString): TMatrixD;

  procedure GetSvgFontInfo(const value: AnsiString; var fontInfo: TSVGFontInfo);
  function GetSvgArcInfo(const p1, p2: TPointD; radii: TPointD; phi_rads: double;
    fA, fS: boolean; out startAngle, endAngle: double; out rec: TRectD): Boolean;
  function HtmlDecode(const html: ansiString): ansistring;

  function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
  function ClampRange(val, min, max: double): double;
  procedure AssignSVGColorList(const ATargetList: TStrings);

{$IF COMPILERVERSION < 17}
type
  TSetOfChar = set of Char;
function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
{$IFEND}

const
  clInvalid   = $00010001;
  clCurrent   = $00010002;
  sqrt2       = 1.4142135623731;
  quote       = '''';
  dquote      = '"';
  space       = #32;
  SvgDecimalSeparator = '.'; //do not localize

  {$I Image32_SVG_Hash_Consts.inc}

var
  LowerCaseTable : array[#0..#255] of AnsiChar;

implementation

type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

const
  buffSize    = 32;

  //include hashed html entity constants
  {$I html_entity_hash_consts.inc}

var
  ColorConstList : TStringList;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------
procedure AssignSVGColorList(const ATargetList: TStrings);
begin
  ATargetList.Assign(ColorConstList);
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: double): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val <= min then Result := min
  else if val >= max then Result := max
  else Result := val;
end;
//------------------------------------------------------------------------------

{$IF COMPILERVERSION < 17}
function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
begin
  Result := chr in chrs;
end;
{$IFEND}
//------------------------------------------------------------------------------

function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
var
  p: PAnsiChar;
begin
  Result := eUnknown;
  if (len < 4) or not Assigned(memory) then Exit;
  p := PAnsiChar(memory);
  case p^ of
    #$EF: if ((p +1)^ = #$BB) and ((p +2)^ = #$BF) then Result := eUtf8;
    #$FF: if ((p +1)^ = #$FE) then Result := eUnicodeLE;
    #$FE: if ((p +1)^ = #$FF) then Result := eUnicodeBE;
  end;
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PAnsiChar; endC: PAnsiChar): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function SkipBlanksAndComma(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
begin
  Result := SkipBlanks(current, currentEnd);
  if not Result or (current^ <> ',') then Exit;
  inc(current);
  Result := SkipBlanks(current, currentEnd);
end;
//------------------------------------------------------------------------------

function SkipStyleBlanks(var c: PAnsiChar; endC: PAnsiChar): Boolean;
var
  inComment: Boolean;
begin
  //style content may include multi-line comment blocks
  inComment := false;
  while (c < endC) do
  begin
    if inComment then
    begin
      if (c^ = '*') and ((c +1)^ = '/')  then
      begin
        inComment := false;
        inc(c);
      end;
    end
    else if (c^ > space) then
    begin
      inComment := (c^ = '/') and ((c +1)^ = '*');
      if not inComment then break;
    end;
    inc(c);
  end;
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function IsAlpha(c: AnsiChar): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := CharInSet(c, ['A'..'Z','a'..'z']);
end;
//------------------------------------------------------------------------------

function GetSingleDigit(var c, endC: PAnsiChar;
  out digit: integer): Boolean;
begin
  Result := SkipBlanksAndComma(c, endC) and (c^ >= '0') and (c^ <= '9');
  if not Result then Exit;
  digit := Ord(c^) - Ord('0');
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  c2: PAnsiChar;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','-'];
begin
  Result := 0;
  //nb: style names may start with a hyphen
  if (c^ = '-') then
  begin
    if not IsAlpha((c+1)^) then Exit;
  end
  else if not IsAlpha(c^) then Exit;

  c2 := c; inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

function ParseNextWord(var c: PAnsiChar; endC: PAnsiChar; out word: AnsiString): Boolean;
var
  c2: PAnsiChar;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  c2 := c;
  while (c < endC) and
    (LowerCaseTable[c^] >= 'a') and (LowerCaseTable[c^] <= 'z') do
      inc(c);
  PAnsiCharToAnsiString(c2, c, word);
end;
//------------------------------------------------------------------------------

function ParseNextWordEx(var c: PAnsiChar; endC: PAnsiChar;
  out word: AnsiString): Boolean;
var
  isQuoted: Boolean;
  c2: PAnsiChar;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  isQuoted := (c^) = quote;
  if isQuoted then
  begin
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> quote) do inc(c);
    PAnsiCharToAnsiString(c2, c,word);
    inc(c);
  end else
  begin
    Result := CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z']);
    if not Result then Exit;
    c2 := c;
    inc(c);
    while (c < endC) and
      CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z', '-', '_']) do inc(c);
    PAnsiCharToAnsiString(c2, c,word);
  end;
end;
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

{$OVERFLOWCHECKS OFF}
function GetHash(const name: AnsiString): cardinal;
var
  i: integer;
  c: PAnsiChar;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  c := PAnsiChar(name);
  Result := 0;
  if c = nil then Exit;
  for i := 1 to Length(name) do
  begin
    Result := (Result + Ord(LowerCaseTable[c^]));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(c);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
//------------------------------------------------------------------------------

function GetHashCaseSensitive(name: PAnsiChar; nameLen: integer): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to nameLen do
  begin
    Result := (Result + Ord(name^));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(name);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
{$OVERFLOWCHECKS ON}
//------------------------------------------------------------------------------

function ParseNextWordHashed(var c: PAnsiChar; endC: PAnsiChar): cardinal;
var
  name: TAnsi;
begin
  name.text := c;
  name.len := ParseNameLength(c, endC);
  if name.len = 0 then Result := 0
  else Result := GetHash(name.AsAnsiString);
end;
//------------------------------------------------------------------------------

function ParseNextNumEx(var c: PAnsiChar; endC: PAnsiChar; skipComma: Boolean;
  out val: double; out unitType: TUnitType): Boolean;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PAnsiChar;
begin
  Result := false;
  unitType := utNumber;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= space) do inc(c);
  if (c = endC) then Exit;

  decPos := -1; exp := Invalid; expIsNeg := false;
  isNeg := c^ = '-';
  if isNeg then inc(c);

  val := 0;
  start := c;
  while c < endC do
  begin
    if Ord(c^) = Ord(SvgDecimalSeparator) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (LowerCaseTable[c^] = 'e') and
      (CharInSet((c+1)^, ['-','0'..'9'])) then
    begin
      if (c +1)^ = '-' then expIsNeg := true;
      inc(c);
      exp := 0;
    end
    else if (c^ < '0') or (c^ > '9') then
      break
    else if IsValid(exp) then
    begin
      exp := exp * 10 + (Ord(c^) - Ord('0'))
    end else
    begin
      val := val *10 + Ord(c^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(c);
  end;
  Result := c > start;
  if not Result then Exit;

  if decPos > 0 then val := val * Power(10, -decPos);
  if isNeg then val := -val;
  if IsValid(exp) then
  begin
    if expIsNeg then
      val := val * Power(10, -exp) else
      val := val * Power(10, exp);
  end;

  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  case c^ of
    '%':
      begin
        inc(c);
        unitType := utPercent;
      end;
    'c': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utCm;
      end;
    'd': //ignore deg
      if ((c+1)^ = 'e') and ((c+2)^ = 'g') then
      begin
        inc(c, 3);
        unitType := utDegree;
      end;
    'e': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utEm;
      end
      else if ((c+1)^ = 'x') then
      begin
        inc(c, 2);
        unitType := utEx;
      end;
    'i': //convert inchs to pixels
      if ((c+1)^ = 'n') then
      begin
        inc(c, 2);
        unitType := utInch;
      end;
    'm': //convert mm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utMm;
      end;
    'p':
      case (c+1)^ of
        'c':
          begin
            inc(c, 2);
            unitType := utPica;
          end;
        't':
          begin
            inc(c, 2);
            unitType := utPt;
          end;
        'x':
          begin
            inc(c, 2);
            unitType := utPixel;
          end;
      end;
    'r': //convert radian angles to degrees
      if Match(c, 'rad') then
      begin
        inc(c, 3);
        unitType := utRadian;
      end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNextNum(var c: PAnsiChar; endC: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean;
var
  tmp: TValue;
begin
  tmp.Init;
  Result := ParseNextNumEx(c, endC, skipComma, tmp.rawVal, tmp.unitType);
  val := tmp.GetValue(1, 1);
end;
//------------------------------------------------------------------------------

function ExtractRef(const href: AnsiString): AnsiString; {$IFDEF INLINE} inline; {$ENDIF}
var
  c, c2, endC: PAnsiChar;
begin
  c := PAnsiChar(href);
  endC := c + Length(href);
  if Match(c, 'url(') then
  begin
    inc(c, 4);
    dec(endC); // avoid trailing ')'
  end;
  if c^ = '#' then inc(c);
  c2 := c;
  while (c < endC) and (c^ <> ')') do inc(c);
  PAnsiCharToAnsiString(c2, c, Result);
end;
//------------------------------------------------------------------------------

function ParseNextChar(var c: PAnsiChar; endC: PAnsiChar): AnsiChar;
begin
  Result := #0;
  if not SkipBlanks(c, endC) then Exit;
  Result := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseQuoteChar(var c: PAnsiChar; endC: PAnsiChar): AnsiChar;
begin
  if SkipBlanks(c, endC) and (c^ in [quote, dquote]) then
  begin
    Result := c^;
    inc(c);
  end else
    Result := #0;
end;
//------------------------------------------------------------------------------

function AnsiTrim(var name: TAnsi): Boolean;
var
  endC: PAnsiChar;
begin
  while (name.len > 0) and (name.text^ <= space) do
  begin
    inc(name.text); dec(name.len);
  end;
  Result := name.len > 0;
  if not Result then Exit;
  endC := name.text + name.len -1;
  while endC^ <= space do
  begin
    dec(endC); dec(name.len);
  end;
end;
//------------------------------------------------------------------------------

function PAnsiCharToTAnsi(var c: PAnsiChar;  endC: PAnsiChar;
  out value: TAnsi): Boolean;
begin
  SkipBlanks(c, endC);
  value.text := c;
  value.len := ParseNameLength(c, endC);
  Result := value.len > 0;
end;
//------------------------------------------------------------------------------

procedure PAnsiCharToAnsiString(var c: PAnsiChar; endC: PAnsiChar; out value: AnsiString);
var
  len: integer;
begin
  len := endC - c;
  SetLength(value, len);
  if len > 0 then
  begin
    Move(c^, value[1], len * SizeOf(AnsiChar));
    c := endC;
  end;
end;
//------------------------------------------------------------------------------

function Match(c: PAnsiChar; const compare: AnsiString): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(compare) do
  begin
    if LowerCaseTable[c^] <> compare[i] then Exit;
    inc(c);
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function Match(c: PAnsiChar; const compare: TAnsi): Boolean; overload;
var
  i: integer;
  c1, c2: PAnsiChar;
begin
  Result := false;
  c1 := c; c2 := compare.text;
  for i := 0 to compare.len -1 do
  begin
    if LowerCaseTable[c1^] <> LowerCaseTable[c2^] then Exit;
    inc(c1); inc(c2);
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function IsKnownEntity(owner: TSvgParser;
  var c: PAnsiChar; endC: PAnsiChar; out entity: PSvgAttrib): boolean;
var
  c2, c3: PAnsiChar;
  entityName: AnsiString;
begin
  inc(c); //skip ampersand.
  c2 := c; c3 := c;
  ParseNameLength(c3, endC);
  PAnsiCharToAnsiString(c2, c3, entityName);
  entity := owner.FindEntity(GetHash(entityName));
  Result := (c3^ = ';') and Assigned(entity);
  //nb: increments 'c' only if the entity is found.
  if Result then c := c3 +1 else dec(c);
end;
//------------------------------------------------------------------------------

function ParseQuotedString(var c: PAnsiChar; endC: PAnsiChar;
  out ansi: AnsiString): Boolean;
var
  quote: AnsiChar;
  c2: PAnsiChar;
begin
  quote := c^;
  inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quote) do inc(c);
  Result := (c < endC);
  if not Result then Exit;
  PAnsiCharToAnsiString(c2, c, ansi);
  inc(c);
end;
//------------------------------------------------------------------------------

function IsNumPending(var c: PAnsiChar;
  endC: PAnsiChar; ignoreComma: Boolean): Boolean;
var
  c2: PAnsiChar;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= ' ') do inc(c);
  if (c = endC) then Exit;

  c2 := c;
  if (c2^ = '-') then inc(c2);
  if (c2^ = SvgDecimalSeparator) then inc(c2);
  Result := (c2 < endC) and (c2^ >= '0') and (c2^ <= '9');
end;
//------------------------------------------------------------------------------

function ParseTransform(const transform: AnsiString): TMatrixD;
var
  i: integer;
  c, endC: PAnsiChar;
  c2: AnsiChar;
  word: AnsiString;
  values: array[0..5] of double;
  mat: TMatrixD;
begin
  c := PAnsiChar(transform);
  endC := c + Length(transform);
  Result := IdentityMatrix; //in case of invalid or referenced value

  while ParseNextWord(c, endC, word) do
  begin
    if Length(word) < 5 then Exit;
    if ParseNextChar(c, endC) <> '(' then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not ParseNextNum(c, endC, false, values[0]) then Break;
    //now get additional variables
    i := 1;
    while (i < 6) and IsNumPending(c, endC, true) and
      ParseNextNum(c, endC, true, values[i]) do inc(i);
    if ParseNextChar(c, endC) <> ')' then Exit; //syntax check

    mat := IdentityMatrix;
    //scal(e), matr(i)x, tran(s)late, rota(t)e, skew(X), skew(Y)
    case LowerCaseTable[word[5]] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(mat, values[0]) else
            MatrixScale(mat, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          mat[0,0] :=  values[0];
          mat[0,1] :=  values[1];
          mat[1,0] :=  values[2];
          mat[1,1] :=  values[3];
          mat[2,0] :=  values[4];
          mat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if Length(word) =10  then
        begin
          c2 := LowerCaseTable[word[10]];
          if c2 = 'x' then
            MatrixTranslate(mat, values[0], 0)
          else if c2 = 'y' then
            MatrixTranslate(mat, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(mat, values[0], values[1])
        else
          MatrixTranslate(mat, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(mat, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(mat, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(mat, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(mat, 0, DegToRad(values[0]));
         end;
    end;
    Result := MatrixMultiply(Result, mat);
  end;
end;
//------------------------------------------------------------------------------

procedure GetSvgFontInfo(const value: AnsiString; var fontInfo: TSVGFontInfo);
var
  c, endC: PAnsiChar;
  hash: Cardinal;
begin
  c := PAnsiChar(value);
  endC := c + Length(value);
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if c = ';' then
      break
    else if IsNumPending(c, endC, true) then
      ParseNextNum(c, endC, true, fontInfo.size)
    else
    begin
      hash := ParseNextWordHashed(c, endC);
      case hash of
        hSans_045_Serif   : fontInfo.family := ttfSansSerif;
        hSerif            : fontInfo.family := ttfSerif;
        hMonospace        : fontInfo.family := ttfMonospace;
        hBold             : fontInfo.weight := 600;
        hItalic           : fontInfo.italic := sfsItalic;
        hNormal           : 
          begin
            fontInfo.weight := 400;
            fontInfo.italic := sfsNone;
          end;
        hStart            : fontInfo.align := staLeft;
        hMiddle           : fontInfo.align := staCenter;
        hEnd              : fontInfo.align := staRight;
        hline_045_through : fontInfo.decoration := fdStrikeThrough;
        hUnderline        : fontInfo.decoration := fdUnderline;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function HtmlDecode(const html: ansiString): ansistring;
var
  val, len: integer;
  c,ce,endC: PAnsiChar;
begin
  len := Length(html);
  SetLength(Result, len*3);
  c := PAnsiChar(html);
  endC := c + len;
  ce := c;
  len := 1;
  while (ce < endC) and (ce^ <> '&') do
    inc(ce);

  while (ce < endC) do
  begin
    if ce > c then
    begin
      Move(c^, Result[len], ce - c);
      inc(len, ce - c);
    end;
    c := ce; inc(ce);
    while (ce < endC) and (ce^ <> ';') do inc(ce);
    if ce = endC then break;

    val := -1; //assume error
    if (c +1)^ = '#' then
    begin
      val := 0;
      //decode unicode value
      if (c +2)^ = 'x' then
      begin
        inc(c, 3);
        while c < ce do
        begin
          if (c^ >= 'a') and (c^ <= 'f') then
            val := val * 16 + Ord(c^) - 87
          else if (c^ >= 'A') and (c^ <= 'F') then
            val := val * 16 + Ord(c^) - 55
          else if (c^ >= '0') and (c^ <= '9') then
            val := val * 16 + Ord(c^) - 48
          else
          begin
            val := -1;
            break;
          end;
          inc(c);
        end;
      end else
      begin
        inc(c, 2);
        while c < ce do
        begin
          val := val * 10 + Ord(c^) - 48;
          inc(c);
        end;
      end;
    end else
    begin
      //decode html entity ...
      case GetHashCaseSensitive(c, ce - c) of
        {$I html_entity_values.inc}
      end;
    end;

    //convert unicode value to utf8 chars
    //this saves the overhead of multiple ansistring<-->string conversions.
    case val of
      0 .. $7F:
        begin
          result[len] := AnsiChar(val);
          inc(len);
        end;
      $80 .. $7FF:
        begin
          Result[len] := AnsiChar($C0 or (val shr 6));
          Result[len+1] := AnsiChar($80 or (val and $3f));
          inc(len, 2);
        end;
      $800 .. $7FFF:
        begin
          Result[len] := AnsiChar($E0 or (val shr 12));
          Result[len+1] := AnsiChar($80 or ((val shr 6) and $3f));
          Result[len+2] := AnsiChar($80 or (val and $3f));
          inc(len, 3);
        end;
      $10000 .. $10FFFF:
        begin
          Result[len] := AnsiChar($F0 or (val shr 18));
          Result[len+1] := AnsiChar($80 or ((val shr 12) and $3f));
          Result[len+2] := AnsiChar($80 or ((val shr 6) and $3f));
          Result[len+3] := AnsiChar($80 or (val and $3f));
          inc(len, 4);
        end;
      else
      begin
        //ie: error
        Move(c^, Result[len], ce- c +1);
        inc(len, ce - c +1);
      end;
    end;
    inc(ce);
    c := ce;
    while (ce < endC) and (ce^ <> '&') do inc(ce);
  end;
  if (c < endC) and (ce > c) then
  begin
    Move(c^, Result[len], (ce - c));
    inc(len, ce - c);
  end;
  setLength(Result, len -1);
end;
//------------------------------------------------------------------------------

function HexByteToInt(h: AnsiChar): Cardinal; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case h of
    '0'..'9': Result := Ord(h) - Ord('0');
    'A'..'F': Result := 10 + Ord(h) - Ord('A');
    'a'..'f': Result := 10 + Ord(h) - Ord('a');
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function IsFraction(val: double): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (val <> 0) and (Abs(val) < 1);
end;
//------------------------------------------------------------------------------

function AnsiStringToColor32(const value: AnsiString; var color: TColor32): Boolean;
var
  i, len  : integer;
  j       : Cardinal;
  clr     : TColor32;
  alpha   : Byte;
  vals    : array[0..3] of double;
  mus     :  array[0..3] of TUnitType;
  c, endC : PAnsiChar;
begin
  Result := false;
  len := Length(value);
  if len < 3 then Exit;
  c := PAnsiChar(value);

  if (color = clInvalid) or (color = clCurrent) or (color = clNone32) then
    alpha := 255 else
    alpha := color shr 24;

  if Match(c, 'rgb') then
  begin
    endC := c + len;
    inc(c, 3);
    if (c^ = 'a') then inc(c);
    if (ParseNextChar(c, endC) <> '(') or
      not ParseNextNumEx(c, endC, false, vals[0], mus[0]) or
      not ParseNextNumEx(c, endC, true, vals[1], mus[1]) or
      not ParseNextNumEx(c, endC, true, vals[2], mus[2]) then Exit;
    for i := 0 to 2 do
      if mus[i] = utPercent then
        vals[i] := vals[i] * 255 / 100;

    if ParseNextNumEx(c, endC, true, vals[3], mus[3]) then
      alpha := 255 else //stops further alpha adjustment
      vals[3] := 255;
    if ParseNextChar(c, endC) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Round(vals[3])) shl 24 +
      ClampByte(Round(vals[0])) shl 16 +
      ClampByte(Round(vals[1])) shl 8 +
      ClampByte(Round(vals[2]));
  end
  else if (c^ = '#') then           //#RRGGBB or #RGB
  begin
    if (len = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
      begin
        inc(c);
        clr := clr shl 4 + HexByteToInt(c^);
      end;
      clr := clr or $FF000000;
    end
    else if (len = 4) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(c);
        j := HexByteToInt(c^);
        clr := clr shl 4 + j;
        clr := clr shl 4 + j;
      end;
      clr := clr or $FF000000;
    end
    else
      Exit;
    color :=  clr;
  end else                                        //color name lookup
  begin
    i := ColorConstList.IndexOf(string(value));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and in case the opacity has been set before the color
  if (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
end;
//------------------------------------------------------------------------------

function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
var
  i, len: integer;
  dist: double;
begin
  dist := 0;
  len := Length(dblArray);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := Ceil(dblArray[i] * scale);
    dist := Result[i] + dist;
  end;
  if dist = 0 then
    Result := nil
  else if len = 1 then
  begin
    SetLength(Result, 2);
    Result[1] := Result[0];
  end;
end;
//------------------------------------------------------------------------------

function PeekNextChar(var c: PAnsiChar; endC: PAnsiChar): AnsiChar;
begin
  if not SkipBlanks(c, endC) then
    Result := #0 else
    Result := c^;
end;
//------------------------------------------------------------------------------

procedure ParseStyleElementContent(const value: TAnsi;
  stylesList: TClassStylesList);
var
  len, cap: integer;
  names: array of string;

  procedure AddName(const name: string);
  begin
    if len = cap then
    begin
      cap := cap + buffSize;
      SetLength(names, cap);
    end;
    names[len] := name;
    inc(len);
  end;

var
  i: integer;
  aclassName: TAnsi;
  aStyle: TAnsi;
  c, endC: PAnsiChar;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/style.html

  stylesList.Clear;
  if value.len = 0 then Exit;

  len := 0; cap := 0;
  c := value.text;
  endC := c + value.len;

  SkipBlanks(c, endC);
  if Match(c, '<![cdata[') then inc(c, 9);

  while SkipStyleBlanks(c, endC) and
    CharInSet(LowerCaseTable[PeekNextChar(c, endC)],
      [SvgDecimalSeparator, '#', 'a'..'z']) do
  begin
    //get one or more class names for each pending style
    aclassName.text := c;
    aclassName.len := ParseNameLength(c, endC);

    AddName(Lowercase(String(aclassName.AsAnsiString)));
    if PeekNextChar(c, endC) = ',' then
    begin
      inc(c);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len); //ie no more comma separated names

    //now get the style
    if PeekNextChar(c, endC) <> '{' then Break;
    inc(c);
    aStyle.text := c;
    while (c < endC) and (c^ <> '}') do inc(c);
    if (c = endC) then break;
    aStyle.len := c - aStyle.text;

    //finally, for each class name add (or append) this style
    for i := 0 to High(names) do
      stylesList.AddAppendStyle(names[i], aStyle.AsAnsiString);
    names := nil;
    len := 0; cap := 0;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TXmlEl classes
//------------------------------------------------------------------------------

constructor TXmlEl.Create(owner: TSvgParser);
begin
{$IFDEF XPLAT_GENERICS}
  attribs := TList<PSvgAttrib>.Create;
{$ELSE}
  attribs := TList.Create;
{$ENDIF}
  selfClosed := true;
  Self.owner := owner;
end;
//------------------------------------------------------------------------------

destructor TXmlEl.Destroy;
begin
  Clear;
  attribs.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TXmlEl.Clear;
var
  i: integer;
begin
  for i := 0 to attribs.Count -1 do
    Dispose(PSvgAttrib(attribs[i]));
  attribs.Clear;
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseHeader(var c: PAnsiChar; endC: PAnsiChar): Boolean;
var
  className, style: AnsiString;
begin
  SkipBlanks(c, endC);
  name.text := c;
  name.len := ParseNameLength(c, endC);

  //load the class's style (ie undotted style) if found.
  className := name.AsAnsiString;
  style := owner.classStyles.GetStyle(classname);
  if style <> '' then ParseStyleAttribute(style);

  Result := ParseAttributes(c, endC);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribName(var c: PAnsiChar;
  endC: PAnsiChar; attrib: PSvgAttrib): Boolean;
var
  c2: PAnsiChar;
  //attribName: AnsiString;
begin
  Result := SkipBlanks(c, endC);
  if not Result then Exit;
  c2 := c;
  ParseNameLength(c, endC);
  PAnsiCharToAnsiString(c2, c, attrib.Name);
  attrib.hash := GetHash(attrib.Name);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribValue(var c: PAnsiChar;
  endC: PAnsiChar; attrib: PSvgAttrib): Boolean;
var
  quoteChar : AnsiChar;
  c2, c3: PAnsiChar;
begin
  Result := ParseNextChar(c, endC) = '=';
  if not Result then Exit;
  quoteChar := ParseQuoteChar(c, endC);
  if quoteChar = #0 then Exit;
  //trim leading and trailing spaces
  while (c < endC) and (c^ <= space) do inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quoteChar) do inc(c);
  c3 := c;
  while (c3 > c2) and ((c3 -1)^ <= space) do 
    dec(c3);
  PAnsiCharToAnsiString(c2, c3, attrib.value);
  inc(c); //skip end quote
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttributes(var c: PAnsiChar; endC: PAnsiChar): Boolean;
var
  attrib, styleAttrib, classAttrib, idAttrib: PSvgAttrib;
  ansi: AnsiString;
begin
  Result := false;
  styleAttrib := nil;  classAttrib := nil;  idAttrib := nil;

  while SkipBlanks(c, endC) do
  begin
    if CharInSet(c^, ['/', '?', '>']) then
    begin
      if (c^ <> '>') then
      begin
        inc(c);
        if (c^ <> '>') then Exit; //error
        selfClosed := true;
      end;
      inc(c);
      Result := true;
      break;
    end
    else if (c^ = 'x') and Match(c, 'xml:') then
    begin
      inc(c, 4); //ignore xml: prefixes
    end;

    New(attrib);
    if not ParseAttribName(c, endC, attrib) or
      not ParseAttribValue(c, endC, attrib) then
    begin
      Dispose(attrib);
      Exit;
    end;

    attribs.Add(attrib);    
    case attrib.hash of
      hId     : idAttrib := attrib;
      hClass  : classAttrib := attrib;
      hStyle  : styleAttrib := attrib;
    end;    
  end;

  if assigned(classAttrib) then 
    with classAttrib^ do
    begin
      //get the 'dotted' classname
      ansi := SvgDecimalSeparator + value;
      //get the style definition
      ansi := owner.classStyles.GetStyle(ansi);
      if ansi <> '' then ParseStyleAttribute(ansi);
    end;

  if assigned(styleAttrib) then
    ParseStyleAttribute(styleAttrib.value);
    
  if assigned(idAttrib) then
  begin
    //get the 'hashed' classname
    ansi := '#' + idAttrib.value;
    //get the style definition
    ansi := owner.classStyles.GetStyle(ansi);
    if ansi <> '' then ParseStyleAttribute(ansi);
  end;
  
end;
//------------------------------------------------------------------------------

procedure TXmlEl.ParseStyleAttribute(const style: AnsiString);
var
  styleName, styleVal: TAnsi;
  c, endC: PAnsiChar;
  attrib: PSvgAttrib;
begin
  //there are 4 ways to load styles (in ascending precedence) -
  //1. a class element style (called during element contruction)
  //2. a non-element class style (called via a class attribute)
  //3. an inline style (called via a style attribute)
  //4. an id specific class style

  c := PAnsiChar(style);
  endC := c + Length(style);
  while SkipStyleBlanks(c, endC) do
  begin
    styleName.text := c;
    styleName.len := ParseStyleNameLen(c, endC);
    if styleName.len = 0 then Break;

    if (ParseNextChar(c, endC) <> ':') or  //syntax check
      not SkipBlanks(c,endC) then Break;

    styleVal.text := c;
    inc(c);
    while (c < endC) and (c^ <> ';') do inc(c);
    styleVal.len := c - styleVal.text;
    AnsiTrim(styleVal);
    inc(c);

    new(attrib);
    attrib.name := styleName.AsAnsiString;
    attrib.value := styleVal.AsAnsiString;
    attrib.hash := GetHash(attrib.name);
    attribs.Add(attrib);
  end;
end;

//------------------------------------------------------------------------------
// TDocTypeEl
//------------------------------------------------------------------------------

procedure TDocTypeEl.SkipWord(var c, endC: PAnsiChar);
begin
  while (c < endC) and (c^ > space) do inc(c);
  inc(c);
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseEntities(var c, endC: PAnsiChar): Boolean;
var
  attrib: PSvgAttrib;
begin
  attrib := nil;
  inc(c); //skip opening '['
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if (c^ = ']') then break
    else if not Match(c, '<!entity') then
    begin
      while c^ > space do inc(c); //skip word.
      Continue;
    end;
    inc(c, 8);
    new(attrib);
    if not ParseAttribName(c, endC, attrib) then break;
    SkipBlanks(c, endC);
    if not (c^ in [quote, dquote]) then break;
    if not ParseQuotedString(c, endC, attrib.value) then break;
    attribs.Add(attrib);
    attrib := nil;
    SkipBlanks(c, endC);
    if c^ <> '>' then break;
    inc(c); //skip entity's trailing '>'
  end;
  if Assigned(attrib) then Dispose(attrib);
  Result := (c < endC) and (c^ = ']');
  inc(c);
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseAttributes(var c: PAnsiChar; endC: PAnsiChar): Boolean;
var
  dummy : AnsiString;
begin
  while SkipBlanks(c, endC) do
  begin
    //we're currently only interested in ENTITY declarations
    case c^ of
      '[': ParseEntities(c, endC);
      '"', '''': ParseQuotedString(c, endC, dummy);
      '>': break;
      else SkipWord(c, endC);
    end;
  end;
  Result := (c < endC) and (c^ = '>');
  inc(c);
end;

//------------------------------------------------------------------------------
// TSvgTreeEl
//------------------------------------------------------------------------------

constructor TSvgTreeEl.Create(owner: TSvgParser);
begin
  inherited Create(owner);
{$IFDEF XPLAT_GENERICS}
  childs := TList<TSvgTreeEl>.Create;
{$ELSE}
  childs := TList.Create;
{$ENDIF}
  selfClosed := false;
end;
//------------------------------------------------------------------------------

destructor TSvgTreeEl.Destroy;
begin
  inherited;
  childs.Free;
end;
//------------------------------------------------------------------------------

procedure TSvgTreeEl.Clear;
var
  i: integer;
begin
  for i := 0 to childs.Count -1 do
    TSvgTreeEl(childs[i]).free;
  childs.Clear;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgTreeEl.ParseHeader(var c: PAnsiChar; endC: PAnsiChar): Boolean;
begin
  Result := inherited ParseHeader(c, endC);
  if Result then
    hash := GetHash(name.AsAnsiString);
end;
//------------------------------------------------------------------------------

function TSvgTreeEl.ParseContent(var c: PAnsiChar; endC: PAnsiChar): Boolean;
var
  child: TSvgTreeEl;
  entity: PSvgAttrib;
  tmpC, tmpEndC: PAnsiChar;
begin
  Result := false;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') then
    begin
      inc(c);
      case c^ of
        '!':
          begin
            if Match(c, '!--') then             //start comment
            begin
              inc(c, 3);
              while (c < endC) and ((c^ <> '-') or
                not Match(c, '-->')) do inc(c); //end comment
              inc(c, 3);
            end else
            begin
              //it's quite likely <![CDATA[
              text.text := c - 1;
              while (c < endC) and (c^ <> '<') do inc(c);
              text.len := c - text.text;
              //and if <style><![CDATA[ ... then load the styles too
              if (hash = hStyle) then
                ParseStyleElementContent(text, owner.classStyles);
            end;
          end;
        '/', '?':
          begin
            //element closing tag
            inc(c);
            if Match(c, name) then
            begin
              inc(c, name.len);
              //very rarely there's a space before '>'
              SkipBlanks(c, endC);
              Result := c^ = '>';
              inc(c);
            end;
            Exit;
          end;
        else
        begin
          //starting a new element
          child := TSvgTreeEl.Create(owner);
          childs.Add(child);
          if not child.ParseHeader(c, endC) then break;
          if not child.selfClosed then
              child.ParseContent(c, endC);
        end;
      end;
    end
    else if c^ = '>' then
    begin
      break; //oops! something's wrong
    end
    else if (c^ = '&') and IsKnownEntity(owner, c, endC, entity) then
    begin
      tmpC := PAnsiChar(entity.value);
      tmpEndC := tmpC + Length(entity.value);
      ParseContent(tmpC, tmpEndC);
    end
    else if (hash = hTSpan) or (hash = hText) or (hash = hTextPath) then
    begin
      //text content: and because text can be mixed with one or more
      //<tspan> elements we need to create sub-elements for each text block.
      //And <tspan> elements can even have <tspan> sub-elements.
      tmpC := c;
      //preserve a leading space
      if (tmpC -1)^ = space then dec(tmpC);
      while (c < endC) and (c^ <> '<') do inc(c);
      if (hash = hTextPath) then
      begin
        text.text := tmpC;
        text.len := c - tmpC;
      end else
      begin
        child := TSvgTreeEl.Create(owner);
        childs.Add(child);
        child.text.text := tmpC;
        child.text.len := c - tmpC;
      end;
    end else
    begin
      tmpC := c;
      while (c < endC) and (c^ <> '<') do inc(c);
      text.text := tmpC;
      text.len := c - tmpC;

      //if <style> element then load styles into owner.classStyles
      if (hash = hStyle) then
        ParseStyleElementContent(text, owner.classStyles);
    end;
  end;
end;
//------------------------------------------------------------------------------

constructor TSvgParser.Create;
begin
  classStyles := TClassStylesList.Create;
  svgStream   := TMemoryStream.Create;
  xmlHeader   := TXmlEl.Create(Self);
  docType     := TDocTypeEl.Create(Self);
{$IFDEF XPLAT_GENERICS}
  entities    := TList<TSvgTreeEl>.Create;
{$ELSE}
  entities    := TList.Create;
{$ENDIF}
  svgTree     := nil;
end;
//------------------------------------------------------------------------------

destructor TSvgParser.Destroy;
begin
  Clear;
  svgStream.Free;
  xmlHeader.Free;
  docType.Free;
  entities.Free;
  classStyles.Free;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.Clear;
begin
  classStyles.Clear;
  svgStream.Clear;
  xmlHeader.Clear;
  docType.Clear;
  entities.Clear;
  FreeAndNil(svgTree);
end;
//------------------------------------------------------------------------------

function TSvgParser.FindEntity(hash: Cardinal): PSvgAttrib;
var
  i: integer;
begin
  //there are usually so few, that there seems little point sorting etc.
  for i := 0 to docType.attribs.Count -1 do
    if PSvgAttrib(docType.attribs[i]).hash = hash then
    begin
      Result := PSvgAttrib(docType.attribs[i]);
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromFile(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  Result := false;
  if not FileExists(filename) then Exit;

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure FlipEndian(var ch: WideChar); {$IFDEF INLINE} inline; {$ENDIF}
begin
  Word(ch) := Swap(Word(ch));
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromStream(stream: TStream): Boolean;
var
  i, len: integer;
  encoding: TSvgEncoding;
  s: WideString;
  utf8: AnsiString;
begin
  try
    svgStream.LoadFromStream(stream);

    //check encoding and set to UTF-8 if necessary
    encoding := GetXmlEncoding(svgStream.Memory, svgStream.Size);
    case encoding of
      eUnicodeLE, eUnicodeBE:
        begin
          SetLength(s, svgStream.Size div 2);
          Move(svgStream.Memory^, s[1], svgStream.Size);
          if encoding = eUnicodeBE then
            for i := 1 to Length(s) do FlipEndian(s[i]);
          utf8 := UTF8Encode(s);
          len := Length(utf8);
          svgStream.SetSize(len);
          Move(utf8[1], svgStream.Memory^, len);
        end;
    end;

    Result := true;
    ParseStream;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromString(const str: string): Boolean;
var
  ss: TStringStream;
begin
{$IFDEF UNICODE}
  ss := TStringStream.Create(str, TEncoding.UTF8);
{$ELSE}
  ss := TStringStream.Create(UTF8Encode(str));
{$ENDIF}
  try
    Result := LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.ParseStream;
var
  c, endC: PAnsiChar;
begin
  c := svgStream.Memory;
  endC := c + svgStream.Size;
  SkipBlanks(c, endC);
  if Match(c, '<?xml') then
  begin
    inc(c, 2); //todo: accommodate space after '<' eg using sMatchEl function
    if not xmlHeader.ParseHeader(c, endC) then Exit;
    SkipBlanks(c, endC);
  end;
  if Match(c, '<!doctype') then
  begin
    inc(c, 2);
    if not docType.ParseHeader(c, endC) then Exit;
  end;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') and Match(c, '<svg') then
    begin
      inc(c);
      svgTree := TSvgTreeEl.Create(self);
      if svgTree.ParseHeader(c, endC) and
        not svgTree.selfClosed then
          svgTree.ParseContent(c, endC);
      break;
    end;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TDpath
//------------------------------------------------------------------------------

function TSvgPath.GetFlattenedPath(scalePending: double): TPathD;
var
  i,j, pathLen, pathCap: integer;
  currPt, radii, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2, bezTolerance: double;
  rec: TRectD;
  path2: TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(Result, pathCap);
    end;
    Result[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

  procedure AddPath(const p: TPathD);
  var
    i, pLen: integer;
  begin
    pLen := Length(p);
    if pLen = 0 then Exit;
    currPt := p[pLen -1];
    if pathLen + pLen >= pathCap then
    begin
      pathCap := pathLen + pLen + buffSize;
      SetLength(Result, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      Result[pathLen] := p[i];
      inc(pathLen);
    end;
  end;

  function LastSegWasCubic(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsCBez, dsCSpline]);
  end;

  function LastSegWasQuad(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsQBez, dsQSpline]);
  end;

begin
  if scalePending <= 0 then scalePending := 1.0;

  bezTolerance := BezierTolerance / scalePending;
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
              AddPoint(PointD(vals[j*2], vals[j*2 +1]));
        dsHorz:
          for j := 0 to High(vals) do
            AddPoint(PointD(vals[j], currPt.Y));
        dsVert:
          for j := 0 to High(vals) do
            AddPoint(PointD(currPt.X, vals[j]));
        dsArc:
          if High(vals) > 5 then
            for j := 0 to High(vals) div 7 do
            begin
              radii.X   := vals[j*7];
              radii.Y   := vals[j*7 +1];
              angle     := DegToRad(vals[j*7 +2]);
              arcFlag   := Round(vals[j*7 +3]);
              sweepFlag := Round(vals[j*7 +4]);
              pt2.X := vals[j*7 +5];
              pt2.Y := vals[j*7 +6];

              GetSvgArcInfo(currPt, pt2, radii, angle,
                arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
              if (sweepFlag = 0)  then
              begin
                path2 := Arc(rec, arc2, arc1, scalePending);
                path2 := ReversePath(path2);
              end else
                path2 := Arc(rec, arc1, arc2, scalePending);
              path2 := RotatePath(path2, rec.MidPoint, angle);
              AddPath(path2);
            end;
        dsQBez:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              pt2.X := vals[j*4];
              pt2.Y := vals[j*4 +1];
              pt3.X := vals[j*4 +2];
              pt3.Y := vals[j*4 +3];
              lastQCtrlPt := pt2;
              path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
              AddPath(path2);
            end;
        dsQSpline:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
            begin
              if LastSegWasQuad(i) then
                pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := vals[j*2];
              pt3.Y := vals[j*2 +1];
              lastQCtrlPt := pt2;
              path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
              AddPath(path2);
            end;
        dsCBez:
          if High(vals) > 4 then
            for j := 0 to High(vals) div 6 do
            begin
              pt2.X := vals[j*6];
              pt2.Y := vals[j*6 +1];
              pt3.X := vals[j*6 +2];
              pt3.Y := vals[j*6 +3];
              pt4.X := vals[j*6 +4];
              pt4.Y := vals[j*6 +5];
              lastCCtrlPt := pt3;
              path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
              AddPath(path2);
            end;
        dsCSpline:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              if LastSegWasCubic(i) then
                pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := vals[j*4];
              pt3.Y := vals[j*4 +1];
              pt4.X := vals[j*4 +2];
              pt4.Y := vals[j*4 +3];
              lastCCtrlPt := pt3;
              path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
              AddPath(path2);
            end;
      end;
    end;
  SetLength(Result, pathLen);
end;
//------------------------------------------------------------------------------

function TSvgPath.IsClosed: Boolean;
var
  len: integer;
begin
  len := Length(segs);
  Result := (len > 0) and (segs[len-1].segType = dsClose);
end;
//------------------------------------------------------------------------------

function TSvgPath.GetSimplePath: TPathD;
var
  i,j, pathLen, pathCap: integer;
  currPt, pt2: TPointD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(Result, pathCap);
    end;
    Result[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

begin
  pathLen := 0; pathCap := 0;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
              AddPoint(PointD(vals[j*2], vals[j*2 +1]));
        dsHorz:
          for j := 0 to High(vals) do
            AddPoint(PointD(vals[j], currPt.Y));
        dsVert:
          for j := 0 to High(vals) do
            AddPoint(PointD(currPt.X, vals[j]));
        dsArc:
          if High(vals) > 5 then
            for j := 0 to High(vals) div 7 do
              AddPoint(PointD(vals[j*7 +5], vals[j*7 +6]));
        dsQBez:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              pt2.X := vals[j*4];
              pt2.Y := vals[j*4 +1];
              AddPoint(PointD(vals[j*4 +2], vals[j*4 +3]));
            end;
        dsQSpline:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
              AddPoint(PointD(vals[j*2 +1], vals[j*2 +1]));
        dsCBez:
          if High(vals) > 4 then
            for j := 0 to High(vals) div 6 do
              AddPoint(PointD(vals[j*6 +4], vals[j*6 +5]));
        dsCSpline:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
              AddPoint(PointD(vals[j*4 +2], vals[j*4 +3]));
      end;
    end;
  SetLength(Result, pathLen);
end;
//------------------------------------------------------------------------------

function TSvgPath.GetBounds: TRectD;
var
  i,j, pathLen, pathCap: integer;
  currPt, radii, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2: double;
  rec: TRectD;
  path2, path3: TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(path2, pathCap);
    end;
    path2[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

  procedure AddPath(const p: TPathD);
  var
    i, pLen: integer;
  begin
    pLen := Length(p);
    if pLen = 0 then Exit;
    currPt := p[pLen -1];
    if pathLen + pLen >= pathCap then
    begin
      pathCap := pathLen + pLen + buffSize;
      SetLength(path2, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      path2[pathLen] := p[i];
      inc(pathLen);
    end;
  end;

  function LastSegWasCubic(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsCBez, dsCSpline]);
  end;

  function LastSegWasQuad(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsQBez, dsQSpline]);
  end;

begin
  path2 := nil;
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
              AddPoint(PointD(vals[j*2], vals[j*2 +1]));
        dsHorz:
          for j := 0 to High(vals) do
            AddPoint(PointD(vals[j], currPt.Y));
        dsVert:
          for j := 0 to High(vals) do
            AddPoint(PointD(currPt.X, vals[j]));
        dsArc:
          if High(vals) > 5 then
            for j := 0 to High(vals) div 7 do
            begin
              radii.X   := vals[j*7];
              radii.Y   := vals[j*7 +1];
              angle     := DegToRad(vals[j*7 +2]);
              arcFlag   := Round(vals[j*7 +3]);
              sweepFlag := Round(vals[j*7 +4]);
              pt2.X := vals[j*7 +5];
              pt2.Y := vals[j*7 +6];

              GetSvgArcInfo(currPt, pt2, radii, angle,
                arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
              if (sweepFlag = 0)  then
              begin
                path3 := Arc(rec, arc2, arc1, 1);
                path3 := ReversePath(path3);
              end else
                path3 := Arc(rec, arc1, arc2, 1);
              path3 := RotatePath(path3, rec.MidPoint, angle);
              AddPath(path3);
            end;
        dsQBez:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              pt2.X := vals[j*4];
              pt2.Y := vals[j*4 +1];
              pt3.X := vals[j*4 +2];
              pt3.Y := vals[j*4 +3];
              lastQCtrlPt := pt2;
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
            end;
        dsQSpline:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
            begin
              if LastSegWasQuad(i) then
                pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := vals[j*2];
              pt3.Y := vals[j*2 +1];
              lastQCtrlPt := pt2;
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
            end;
        dsCBez:
          if High(vals) > 4 then
            for j := 0 to High(vals) div 6 do
            begin
              pt2.X := vals[j*6];
              pt2.Y := vals[j*6 +1];
              pt3.X := vals[j*6 +2];
              pt3.Y := vals[j*6 +3];
              pt4.X := vals[j*6 +4];
              pt4.Y := vals[j*6 +5];
              lastCCtrlPt := pt3;
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
            end;
        dsCSpline:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              if LastSegWasCubic(i) then
                pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := vals[j*4];
              pt3.Y := vals[j*4 +1];
              pt4.X := vals[j*4 +2];
              pt4.Y := vals[j*4 +3];
              lastCCtrlPt := pt3;
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
            end;
      end;
    end;
  SetLength(path2, pathLen);
  Result := GetBoundsD(path2);
end;

//------------------------------------------------------------------------------
// TValue
//------------------------------------------------------------------------------

function ConvertValue(const value: TValue; scale: double): double;
const
  mm  = 96 / 25.4;
  cm  = 96 / 2.54;
  rad = 180 / PI;
  pt  = 4 / 3;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  //todo: still lots of units to support (eg times for animation)
  with value do
    if not IsValid or (rawVal = 0) then
      Result := 0
    else
      case value.unitType of
        utNumber:
          Result := rawVal;
        utPercent:
          Result := rawVal * 0.01 * scale;
        utRadian:
          Result := rawVal * rad;
        utInch:
          Result := rawVal * 96;
        utCm:
          Result := rawVal * cm;
        utMm:
          Result := rawVal * mm;
        utEm:
          if scale <= 0 then
            Result := rawVal * 16 else
            Result := rawVal * scale;
        utEx:
          if scale <= 0 then
            Result := rawVal * 8 else
            Result := rawVal * scale * 0.5;
        utPica:
          Result := rawVal * 16;
        utPt:
          Result := rawVal * pt;
        else
          Result := rawVal;
      end;
end;
//------------------------------------------------------------------------------

procedure TValue.Init;
begin
  rawVal      := InvalidD;
  unitType          := utNumber;
end;
//------------------------------------------------------------------------------

procedure TValue.SetValue(val: double; unitTyp: TUnitType);
begin
  rawVal  := val;
  unitType      := unitTyp;
end;
//------------------------------------------------------------------------------

function TValue.GetValue(relSize: double; assumeRelValBelow: Double): double;
begin
  if not IsValid or (rawVal = 0) then
    Result := 0
  else if (unitType = utNumber) and (Abs(rawVal) <= assumeRelValBelow) then
    Result := rawVal * relSize
  else
    Result := ConvertValue(self, relSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
begin
  //https://www.w3.org/TR/SVG11/coords.html#Units
  Result := GetValue(Hypot(relSize.Width, relSize.Height)/sqrt2, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValue.IsValid: Boolean;
begin
  Result := (unitType <> utUnknown) and Image32_Vector.IsValid(rawVal);
end;
//------------------------------------------------------------------------------

function TValue.HasFontUnits: Boolean;
begin
  case unitType of
    utEm, utEx: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TValue.HasAngleUnits: Boolean;
begin
  case unitType of
    utDegree, utRadian: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------
// TValuePt
//------------------------------------------------------------------------------

procedure TValuePt.Init;
begin
  X.Init;
  Y.Init;
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize.Width, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.IsValid: Boolean;
begin
  Result := X.IsValid and Y.IsValid;
end;

//------------------------------------------------------------------------------
// TValueRec
//------------------------------------------------------------------------------

procedure TValueRecWH.Init;
begin
  left.Init;
  top.Init;
  width.Init;
  height.Init;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD;
begin
  with GetRectWH(relSize, assumeRelValBelow) do
  begin
    Result.Left :=Left;
    Result.Top := Top;
    Result.Right := Left + Width;
    Result.Bottom := Top + Height;
  end;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(relSize: double; assumeRelValBelow: Double): TRectD;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize, assumeRelValBelow);

  Result.Right := Result.Left + width.GetValue(relSize, assumeRelValBelow);
  Result.Bottom := Result.Top + height.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize.Width, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize.Height, assumeRelValBelow);

  Result.Width := width.GetValue(relSize.Width, assumeRelValBelow);
  Result.Height := height.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsValid: Boolean;
begin
  Result := width.IsValid and height.IsValid;
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsEmpty: Boolean;
begin
  Result := (width.rawVal <= 0) or (height.rawVal <= 0);
end;

//------------------------------------------------------------------------------
// TClassStylesList
//------------------------------------------------------------------------------

constructor TClassStylesList.Create;
begin
  fList := TStringList.Create;
  fList.Duplicates := dupIgnore;
  fList.CaseSensitive := false;
  fList.Sorted := True;
end;
//------------------------------------------------------------------------------

destructor TClassStylesList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

function TClassStylesList.AddAppendStyle(const classname: string; const ansi: AnsiString): integer;
var
  i: integer;
  sr: PAnsStringiRec;
begin
  Result := fList.IndexOf(classname);
  if (Result >= 0) then
  begin
    sr := PAnsStringiRec(fList.Objects[Result]);
    i := Length(sr.ansi);
    if sr.ansi[i] <> ';' then
      sr.ansi := sr.ansi + ';' + ansi else
      sr.ansi := sr.ansi + ansi;
  end else
  begin
    new(sr);
    sr.ansi := ansi;
    Result := fList.AddObject(classname, Pointer(sr));
  end;
end;
//------------------------------------------------------------------------------

function TClassStylesList.GetStyle(const classname: AnsiString): AnsiString;
var
  i: integer;
begin
  SetLength(Result, 0);
  i := fList.IndexOf(string(className));
  if i >= 0 then
    Result := PAnsStringiRec(fList.objects[i]).ansi;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    Dispose(PAnsStringiRec(fList.Objects[i]));
  fList.Clear;
end;

//------------------------------------------------------------------------------
// TAnsi
//------------------------------------------------------------------------------

function TAnsi.AsAnsiString: AnsiString;
begin
  SetLength(Result, len);
  if len > 0 then
    Move(text^, Result[1], len);
end;

//------------------------------------------------------------------------------
// GetSvgArcInfo - and support functions
//------------------------------------------------------------------------------

function TrigClampVal(val: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  //force : -1 <= val <= 1
  if val < -1 then Result := -1
  else if val > 1 then Result := 1
  else Result := val;
end;
//------------------------------------------------------------------------------

function  Radian2(vx, vy: double): double;
begin
  Result := ArcCos( TrigClampVal(vx / Sqrt( vx * vx + vy * vy)) );
  if( vy < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

function  Radian4(ux, uy, vx, vy: double): double;
var
  dp, md: double;
begin
  dp := ux * vx + uy * vy;
  md := Sqrt( ( ux * ux + uy * uy ) * ( vx * vx + vy * vy ) );
    Result := ArcCos( TrigClampVal(dp / md) );
    if( ux * vy - uy * vx < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

//https://stackoverflow.com/a/12329083
function GetSvgArcInfo(const p1, p2: TPointD; radii: TPointD;
  phi_rads: double; fA, fS: boolean;
  out startAngle, endAngle: double; out rec: TRectD): Boolean;
var
  x1_, y1_, rxry, rxy1_, ryx1_, s_phi, c_phi: double;
  hd_x, hd_y, hs_x, hs_y, sum_of_sq, lambda, coe: double;
  cx, cy, cx_, cy_, xcr1, xcr2, ycr1, ycr2, deltaAngle: double;
const
  twoPi: double = PI *2;
begin
    Result := false;
    if (radii.X < 0) then radii.X := -radii.X;
    if (radii.Y < 0) then radii.Y := -radii.Y;
    if (radii.X = 0) or (radii.Y = 0) then Exit;

    Image32_Vector.GetSinCos(phi_rads, s_phi, c_phi);;
    hd_x := (p1.X - p2.X) / 2.0; // half diff of x
    hd_y := (p1.Y - p2.Y) / 2.0; // half diff of y
    hs_x := (p1.X + p2.X) / 2.0; // half sum of x
    hs_y := (p1.Y + p2.Y) / 2.0; // half sum of y

    // F6.5.1
    x1_ := c_phi * hd_x + s_phi * hd_y;
    y1_ := c_phi * hd_y - s_phi * hd_x;

    // F.6.6 Correction of out-of-range radii
    // Step 3: Ensure radii are large enough
    lambda := (x1_ * x1_) / (radii.X * radii.X) +
      (y1_ * y1_) / (radii.Y * radii.Y);
    if (lambda > 1) then
    begin
      radii.X := radii.X * Sqrt(lambda);
      radii.Y := radii.Y * Sqrt(lambda);
    end;

    rxry := radii.X * radii.Y;
    rxy1_ := radii.X * y1_;
    ryx1_ := radii.Y * x1_;
    sum_of_sq := rxy1_ * rxy1_ + ryx1_ * ryx1_; // sum of square
    if (sum_of_sq = 0) then Exit;

    coe := Sqrt(Abs((rxry * rxry - sum_of_sq) / sum_of_sq));
    if (fA = fS) then coe := -coe;

    // F6.5.2
    cx_ := coe * rxy1_ / radii.Y;
    cy_ := -coe * ryx1_ / radii.X;

    // F6.5.3
    cx := c_phi * cx_ - s_phi * cy_ + hs_x;
    cy := s_phi * cx_ + c_phi * cy_ + hs_y;

    xcr1 := (x1_ - cx_) / radii.X;
    xcr2 := (x1_ + cx_) / radii.X;
    ycr1 := (y1_ - cy_) / radii.Y;
    ycr2 := (y1_ + cy_) / radii.Y;

    // F6.5.5
    startAngle := Radian2(xcr1, ycr1);
    NormalizeAngle(startAngle);

    // F6.5.6
    deltaAngle := Radian4(xcr1, ycr1, -xcr2, -ycr2);
    while (deltaAngle > twoPi) do deltaAngle := deltaAngle - twoPi;
    while (deltaAngle < 0.0) do deltaAngle := deltaAngle + twoPi;
    if not fS then deltaAngle := deltaAngle - twoPi;
    endAngle := startAngle + deltaAngle;
    NormalizeAngle(endAngle);

    rec.Left := cx - radii.X;
    rec.Right := cx + radii.X;
    rec.Top := cy - radii.Y;
    rec.Bottom := cy + radii.Y;

    Result := true;
end;

//------------------------------------------------------------------------------
// DParse and support functions
//------------------------------------------------------------------------------

function GetSegType(var c, endC: PAnsiChar;
  out isRelative: Boolean): TSvgPathSegType;
var
  ch: AnsiChar;
begin
  Result := dsUnknown;
  if not SkipBlanks(c, endC) then Exit;
  ch := upcase(c^);
  if not CharInSet(ch,
    ['A','C','H','M','L','Q','S','T','V','Z']) then Exit;
  case ch of
    'M': Result := dsMove;
    'L': Result := dsLine;
    'H': Result := dsHorz;
    'V': Result := dsVert;
    'A': Result := dsArc;
    'Q': Result := dsQBez;
    'C': Result := dsCBez;
    'T': Result := dsQSpline;
    'S': Result := dsCSpline;
    'Z': Result := dsClose;
  end;
  isRelative := c^ >= 'a';
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseSvgPath(const value: AnsiString): TSvgPaths;
var
  currSeg     : PSvgPathSeg;
  currDpath   : PSvgPath;
  currSegCnt  : integer;
  currSegCap  : integer;
  currSegType : TSvgPathSegType;
  lastPt      : TPointD;

  procedure StartNewDpath;
  var
    cnt: integer;
  begin
    if Assigned(currDpath) then
    begin
      if not Assigned(currDpath.segs) then Exit;
      SetLength(currSeg.vals, currSegCnt);
    end;
    cnt := Length(Result);
    SetLength(Result, cnt +1);
    currDpath := @Result[cnt];
    currDpath.firstPt := lastPt;
    currDpath.segs := nil;
    currSeg := nil;
  end;

  procedure StartNewSeg;
  var
    cnt: integer;
  begin
    if Assigned(currSeg) then
      SetLength(currSeg.vals, currSegCnt)
    else if not Assigned(currDpath) then
      StartNewDpath;

    cnt := Length(currDpath.segs);
    SetLength(currDpath.segs, cnt +1);
    currSeg := @currDpath.segs[cnt];
    currSeg.segType := currSegType;

    currSegCap := buffSize;
    SetLength(currSeg.vals, currSegCap);
    currSegCnt := 0;
  end;

  procedure AddSegValue(val: double);
  begin
    if not Assigned(currSeg) then StartNewSeg;

    if currSegCnt = currSegCap then
    begin
      inc(currSegCap, buffSize);
      SetLength(currSeg.vals, currSegCap);
    end;
    currSeg.vals[currSegCnt] := val;
    inc(currSegCnt);
  end;

  procedure AddSegPoint(const pt: TPointD);
  begin
    AddSegValue(pt.X); AddSegValue(pt.Y);
  end;

  function Parse2Num(var c, endC: PAnsiChar;
    var pt: TPointD; isRelative: Boolean): Boolean;
  begin
    Result := ParseNextNum(c, endC, true, pt.X) and
      ParseNextNum(c, endC, true, pt.Y);
    if not Result or not isRelative then Exit;
    pt.X := pt.X + lastPt.X;
    pt.Y := pt.Y + lastPt.Y;
  end;

var
  i: integer;
  d: double;
  c, endC: PAnsiChar;
  currPt: TPointD;
  isRelative: Boolean;
begin
  currSeg     := nil;
  currSegCnt  := 0;
  currSegCap  := 0;
  currDpath   := nil;
  currSegType := dsMove;

  c := PAnsiChar(value);
  endC := c + Length(value);
  isRelative := false;
  currPt := NullPointD;

  while true do
  begin
    currSegType := GetSegType(c, endC, isRelative);
    if currSegType = dsUnknown then break;

    lastPt := currPt;
    if (currSegType = dsMove) then
    begin
      if Assigned(currSeg) then
        SetLength(currSeg.vals, currSegCnt); //trim buffer
      currDpath := nil;
      currSeg := nil;

      if not Parse2Num(c, endC, currPt, isRelative) then break;
      lastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if IsNumPending(c, endC, true) then
        currSegType := dsLine else
        Continue;
    end
    else if (currSegType = dsClose) then
    begin
      if Assigned(currDpath) then
        currPt := currDpath.firstPt;
      StartNewSeg;
      Continue;
    end;

    if Assigned(currSeg) then
      SetLength(currSeg.vals, currSegCnt); //trim buffer
    currSeg := nil;

    case currSegType of
      dsHorz:
        while IsNumPending(c, endC, true) and
          ParseNextNum(c, endC, true, currPt.X) do
        begin
          if isRelative then
            currPt.X := currPt.X + lastPt.X;
          AddSegValue(currPt.X);
          lastPt := currPt;
        end;

      dsVert:
        while IsNumPending(c, endC, true) and
          ParseNextNum(c, endC, true, currPt.Y) do
        begin
          if isRelative then
            currPt.Y := currPt.Y + lastPt.Y;
          AddSegValue(currPt.Y);
          lastPt := currPt;
        end;

      dsLine:
        while Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          lastPt := currPt;
          SkipBlanks(c, endC);
          if IsNumPending(c, endC, true) then Continue;
          if LowerCaseTable[c^] = 'l' then GetSegType(c, endC, isRelative)
          else break;
        end;

      dsQSpline:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCSpline:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsQBez:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCBez:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsArc:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, false) do
        begin
          AddSegPoint(currPt);                              //radii
          if ParseNextNum(c, endC, true, d) then
            AddSegValue(d);                                 //angle
          if not GetSingleDigit(c, endC, i) then break;     //arc-flag
          AddSegValue(i);
          if not GetSingleDigit(c, endC, i) then break;     //sweep-flag
          AddSegValue(i);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;
    end;
  end;
  if Assigned(currSeg) then
    SetLength(currSeg.vals, currSegCnt); //trim buffer
end;

//------------------------------------------------------------------------------
// initialization procedures
//------------------------------------------------------------------------------

procedure MakeLowerCaseTable;
var
  i: AnsiChar;
begin
  for i:= #0 to #$40 do LowerCaseTable[i]:= i;
  for i:= #$41 to #$5A do LowerCaseTable[i]:= AnsiChar(Ord(i) + $20);
  for i:= #$5B to #$FF do LowerCaseTable[i]:= i;
end;
//------------------------------------------------------------------------------

procedure MakeColorConstList;
var
  i: integer;
  {$I html_color_consts.inc}
begin
  ColorConstList := TStringList.Create;
  ColorConstList.CaseSensitive := false;
  ColorConstList.Capacity := Length(ColorConsts);
  for i := 0 to High(ColorConsts) do
    with ColorConsts[i] do
      ColorConstList.AddObject(ColorName, Pointer(ColorValue));
  ColorConstList.Sorted := true;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeColorConstList;

finalization
  ColorConstList.Free;
end.
