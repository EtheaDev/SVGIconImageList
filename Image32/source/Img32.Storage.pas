unit Img32.Storage;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Object persistence                                              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Img32;

type
  TStorageState = (ssNormal, ssLoading, ssSaving, ssDestroying);

  TStorage = class;
  TStorageClass = class of TStorage;
{$IFNDEF NO_STORAGE}
  TStorageManager = class;
{$ENDIF}

  TStorage = class(TInterfacedObj)
  private
    fParent   : TStorage;
{$IFNDEF NO_STORAGE}
    fManager  : TStorageManager;
{$ENDIF}
    fChilds   : TList;
    fIndex    : integer;
    fName     : string;
    fStgState : TStorageState;
    fStgId    : integer;
    function  GetChildCount: integer;
    function  GetHasChildren: Boolean;
  protected
    procedure SetName(const aName: string); virtual;
    function  GetChild(index: integer): TStorage;
    procedure SetParent(parent: TStorage); virtual;
    procedure ReindexChilds(startFrom: integer);
    procedure CheckChildIndex(index: integer); virtual;
    function  RemoveChildFromList(index: integer): TStorage; virtual;
{$IFNDEF NO_STORAGE}
    procedure BeginRead; virtual;
    function  ReadProperty(const propName, propVal: string): Boolean; virtual;
    procedure EndRead; virtual;
    procedure WriteProperties; virtual;
    procedure WriteStorageHeader(var objId: integer);
    procedure WriteStorageContent(var objId: integer);
    procedure WriteStorageFooter;
    procedure WriteIntProp(const propName: string; propVal: integer);
    procedure WriteCardinalProp(const propName: string; propVal: Cardinal);
    procedure WriteBoolProp(const propName: string; propVal: Boolean);
    procedure WriteColorProp(const propName: string; propVal: TColor32);
    procedure WriteDoubleProp(const propName: string; propVal: double);
    procedure WritePointDProp(const propName: string; propVal: TPointD);
    procedure WriteRectDProp(const propName: string; propVal: TRectD);
    procedure WritePathDProp(const propName: string; propVal: TPathD);
    procedure WriteLocalStorageProp(const propName: string; propVal: TStorage);
    procedure WriteExternalProp(const propName: string; propVal: TObject);
    procedure WriteEventProp(const propName: string; propVal: TNotifyEvent);
    procedure WriteStrProp(const propName, propVal: string);
{$ENDIF}
  public
    constructor Create(parent:  TStorage = nil; const name: string = ''); virtual;
    destructor  Destroy; override;
    procedure Free;
    procedure ClearChildren; virtual;
    function  AddChild(storeClass: TStorageClass): TStorage; virtual;
    function  InsertChild(index: integer; storeClass: TStorageClass): TStorage; virtual;
    procedure DeleteChild(index: integer);
    function  IsOwnedBy(obj: TStorage): Boolean; overload;
    function  IsOwnedBy(objClass: TStorageClass): Boolean; overload;
{$IFNDEF NO_STORAGE}
    function  FindByName(const objName: string): TStorage;
    function  FindById(const objId: integer): TStorage;
    function  FindByClass(stgClass: TStorageClass): TStorage;
    function  FindByClassAndName(stgClass: TStorageClass;
      const objName: string): TStorage;
{$ENDIF}
    property  Child[index: integer]: TStorage read GetChild;
    property  Childs: TList read fChilds;
    property  ChildCount: integer read GetChildCount;
    property  HasChildren: Boolean read GetHasChildren;
    property  Index  : integer read fIndex;
    property  LoadId : integer read fStgId;
    property  Name   : string read fName write SetName;
    property  Parent : TStorage read fParent write SetParent;
{$IFNDEF NO_STORAGE}
    property  StorageManager: TStorageManager read fManager;
{$ENDIF}
    property  StorageState : TStorageState read fStgState;
  end;

{$IFNDEF NO_STORAGE}
  TStorageManager = class(TStorage)
  private
    fDesignScreenRes  : double;
    fDesignFormScale  : double;
    fXmlStr           : Utf8String;
    fCurrLevel        : integer;
  protected
    procedure DoBeforeLoad; virtual;
    procedure DoAfterChildLoad(child: TStorage); virtual;
    procedure DoAfterLoad; virtual;
    procedure DoBeforeWrite; virtual;
    function GetEventName(event: TNotifyEvent): string; virtual;
    function GetExternPropName(prop: TObject): string; virtual;
    function ReadInfoProperty(const propName, propVal: string): Boolean; virtual;
    procedure WriteCustomProperties; virtual;
  public
    constructor Create(parent:  TStorage = nil; const name: string = ''); override;
    procedure LoadFromFile(const filename: string);
    procedure LoadFromResource(const resName: string; resType: PChar);
    procedure AddWriteString(const str: Utf8String);
    procedure SaveToFile(const filename: string;
      aScale: double; const classesToIgnore: array of TStorageClass);
    class function GetStorageClass(const storageClassname: string): TStorageClass;
    function FindObjectByClass(storageClass: TStorageClass;
      parent: TStorage = nil): TStorage;
    function FindObjectByName(const name: string; parent: TStorage = nil): TStorage;
    function FindObjectByLoadId(id: integer): TStorage;
    property DesignScreenRes: double read fDesignScreenRes;
    property DesignFormScale: double read
      fDesignFormScale write fDesignFormScale;
  end;

  TStorageInfo = class(TStorage)
  protected
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  end;
  function GetIntProp(const str: string; out success: Boolean): integer;
  function GetBoolProp(const str: string; out success: Boolean): Boolean;
  function GetCardProp(const str: string; out success: Boolean): Cardinal;
  function GetDoubleProp(const str: string; out success: Boolean): Double;
  function GetStringProp(const str: string; out success: Boolean): string;
  function GetStorageProp(const str: string; out success: Boolean): TStorage;
  function GetColorProp(const str: string; out success: Boolean): TColor32;
  function GetPointDProp(const str: string; out success: Boolean): TPointD;
  procedure RegisterStorageClass(storageClass: TStorageClass);
{$ENDIF}

implementation

resourcestring
  rsClassNotRegistered = 'Error: %s is not a registered TStorage class';

type
  TLoadPtrRec = record
    loadObj   : TStorage;
    propName  : string;
    propVal   : string;
    propValI  : integer;
  end;
  PLoadPtrRec = ^TLoadPtrRec;

{$IFNDEF PBYTE}
  UTF8Char = type Char;
  PUTF8Char = type PChar;
{$ENDIF}

var
  classList       : TStringList;
{$IFNDEF NO_STORAGE}
  objIdList       : TList;

const
  squote  = '''';
  dquote  = '"';
  space   = #32;
  lt      = '<';
  gt      = '>';
  amp     = '&';
  tab     = #9;
  spacesPerLevel = 2;

{.$IF COMPILERVERSION < 18}
function UIntToStr(value: Cardinal): string;
begin
  Result := Format('%d', [value]);
end;
//------------------------------------------------------------------------------

function TryStrToUInt(const S: string; out Value: Cardinal): Boolean;
var
  I64: Int64;
  E: Integer;
begin
  Val(S, I64, E);
  Value := I64;
  Result := (E = 0) and (I64 >= 0) and (I64 <= $FFFFFFFF);
end;
//------------------------------------------------------------------------------

function TryStrToUInt64(const S: string; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{.$IFEND}

//------------------------------------------------------------------------------
// Read from XML string functions
//------------------------------------------------------------------------------

function ConvertColorConst(const strColor: string; out color: TColor32): Boolean;
var
  s: string;
begin
  color := clNone32;
  Result :=  (Length(strColor) > 7) and
    (UpCase(strColor[1]) = 'C') and (UpCase(strColor[1]) = 'L');
  if not Result then Exit;
  s := UpperCase(strColor);
  case s[3] of
    'A':
      if s = 'CLAQUA32' then color := $FF00FFFF
      else Result := false;
    'B':
      if s = 'CLBLACK32' then color := $FF000000
      else if s = 'CLBLUE32' then color := $FFFF00FF
      else if s = 'CLBTNFACE32' then color := $FFF0F0F0
      else Result := false;
    'F':
      if s = 'CLFUCHSIA32' then color := $FF0000FF
      else Result := false;
    'G':
      if s = 'CLGRAY32' then color := $FF808080
      else if s = 'CLGREY32' then color := $FF808080
      else if s = 'CLGREEN32' then color := $FF008000
      else Result := false;
    'L':
      if s = 'CLLIME32' then color := $FF00FF00
      else Result := false;
    'M':
      if s = 'CLMAROON32' then color := $FF800000
      else Result := false;
    'N':
      if s = 'CLNAVY32' then color := $FF000080
      else if s = 'CLNONE32' then color := $00000000
      else Result := false;
    'O':
      if s = 'CLOLIVE32' then color := $FF7F7F00
      else if s = 'CLORANGE32' then color := $FFFF7F00
      else Result := false;
    'P':
      if s = 'CLPURPLE32' then color := $FF7F00FF
      else Result := false;
    'R':
      if s = 'CLRED32' then color := $FFFF0000
      else Result := false;
    'S':
      if s = 'CLSILVER32' then color := $FFC0C0C0
      else Result := false;
    'T':
      if s = 'CLTEAL32' then color := $FF007F7F
      else Result := false;
    'W':
      if s = 'CLWHITE32' then color := $FFFFFFFF
      else Result := false;
    'Y':
      if s = 'CLYELLOW32' then color := $FFFFFF00
      else Result := false;
    else Result := false;
  end;
end;
//------------------------------------------------------------------------------

function SimpleHtmlDecode(const Data: string): string;
var
  i, j: Integer;
  inSpecial: Boolean;
begin
  result := '';
  inSpecial := false; j := 1;
  for i := 1 to length(Data) do
  begin
    if inSpecial then
    begin
      if Data[i] <> ';' then Continue;
      inSpecial := false;
      case Data[j+1] of
        'g': result := result + '>';
        'l': result := result + '<';
        'q': result := result + '"';
        'a':
          case Data[j+2] of
            'm' : result := result + '&';
            'p' : result := result + '''';
          end;
        '#': result := result +
          Chr(StrToIntDef(Copy(Data, j+2, i-j-2), 32));
      end;
    end
    else if Data[i] = '&' then
    begin
      inSpecial := true;
      j := i;
    end else
    begin
      result := result + Data[i];
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetDoublefromArray(const str: string;
  var startIdx: integer; out val: double): Boolean;
var
  i,j, len, dotIdx: integer;
  isNeg: Boolean;
begin
  val := 0; dotIdx := 0;
  len := Length(str);
  Result := false;
  isNeg := str[startIdx] = '-';
  if isNeg then inc(startIdx);
  i := startIdx;
  while (startIdx < len) do
  begin
    if str[startIdx] = '.' then
    begin
      if dotIdx <> 0 then Exit; //error: double dot!
      dotIdx := startIdx +1;
    end else
    begin
      j := Ord(str[startIdx])-48;
      if (j < 0) or (j > 9) then Break;//non-numeric
      val := val * 10 + j;
    end;
    inc(startIdx);
  end;
  Result := (startIdx > i);
  if not Result then Exit; //error
  if (dotIdx > 0) then
    for i := dotIdx to startIdx-1 do
      val := val / 10;
  if isNeg then val := -val;
end;
//------------------------------------------------------------------------------

function GetDoubleArray(const str: string; out success: Boolean): TArrayOfDouble;
var
  i, len, cnt, buffSize: integer;
begin
  buffSize := 16;
  SetLength(Result, buffSize);
  len := Length(str);
  success := len > 1; //at least make sure there's room for an empty array
  if not success then Exit;
  cnt := 0;
  i := 1;
  while (i < len) and (str[i] <> '[') do inc(i);
  inc(i);
  while (i < len) and (str[i] = space) do inc(i);
  while (i < len) and GetDoublefromArray(str, i, Result[cnt]) do
  begin
    inc(cnt);
    if cnt = buffSize then
    begin
      buffSize := buffSize*2;
      SetLength(Result, buffSize);
    end;
    while (i < len) and (str[i] = space) do inc(i);
    //skip a single (optional) separating comma if found.
    if (i < len) and (str[i] = ',') then
    begin
      inc(i);
      while (i < len) and (str[i] = space) do inc(i);
    end;
  end;
  SetLength(Result, cnt);
  while (i < len) and (str[i] = space) do inc(i);
  while (i < len) and (str[i] <> ']') do inc(i);
  success := str[i] = ']';
end;
//------------------------------------------------------------------------------

function GetIntfromArray(const str: string;
  var startIdx: integer; out val: integer): Boolean;
var
  i,j,len: integer;
  isNeg: Boolean;
begin
  val := 0;
  len := Length(str);
  isNeg := str[startIdx] = '-';
  if isNeg then inc(startIdx);
  Result := (startIdx <= len) and
    (str[startIdx] >= '0') and (str[startIdx] <= '9');
  if not Result then Exit;; //error
  for i := startIdx to len do
  begin
    j := Ord(str[startIdx]) - 48;
    if (j < 0) or (j > 9) then Break;//non-numeric
    val := val * 10 + j;
    inc(startIdx);
  end;
  if Result and isNeg then val := -val;
end;
//------------------------------------------------------------------------------

function GetIntArray(const str: string; out success: Boolean): TArrayOfInteger;
var
  i, len, cnt, buffSize: integer;
begin
  buffSize := 16;
  SetLength(Result, buffSize);
  len := Length(str);
  success := len > 1; //at least make sure there's room for an empty array
  if not success then Exit;
  cnt := 0;
  i := 1;
  while (i < len) and (str[i] <> '[') do inc(i);
  inc(i);
  while (i < len) and (str[i] = space) do inc(i);
  while (i < len) and GetIntfromArray(str, i, Result[cnt]) do
  begin
    inc(cnt);
    if cnt = buffSize then
    begin
      buffSize := buffSize*2;
      SetLength(Result, buffSize);
    end;
    while (i < len) and (str[i] = space) do inc(i);
    //skip a single (optional) separating comma if found.
    if (i < len) and (str[i] = ',') then
    begin
      inc(i);
      while (i < len) and (str[i] = space) do inc(i);
    end;
  end;
  SetLength(Result, cnt);
  while (i < len) and (str[i] = space) do inc(i);
  while (i < len) and (str[i] <> ']') do inc(i);
  success := str[i] = ']';
end;
//------------------------------------------------------------------------------

function GetIntProp(const str: string; out success: Boolean): integer;
begin
  success := TryStrToInt(str, Result);
end;
//------------------------------------------------------------------------------

function GetCardProp(const str: string; out success: Boolean): Cardinal;
begin
  success := TryStrToUInt(str, Result);
end;
//------------------------------------------------------------------------------

function GetDoubleProp(const str: string; out success: Boolean): Double;
begin
  success := TryStrToFloat(str, Result);
end;
//------------------------------------------------------------------------------

function GetStringProp(const str: string; out success: Boolean): string;
begin
  Result := SimpleHtmlDecode(str);
  success := true;
end;
//------------------------------------------------------------------------------

function GetStorageProp(const str: string; out success: Boolean): TStorage;
var
  pp: UInt64;
begin
  success := TryStrToUInt64('$'+str, pp);
  //workaround for either 32bit an 64bit pointers
  if success then
    Result := TObject(pp) as TStorage else
    Result := nil;
end;
//------------------------------------------------------------------------------

function GetColorProp(const str: string; out success: Boolean): TColor32;
begin
  if (Length(str) > 7) and (UpCase(str[1]) = 'C') then
  begin
    success := ConvertColorConst(str, Result);
  end else
    success := TryStrToUInt('$'+str, Cardinal(Result));
end;
//------------------------------------------------------------------------------

function GetBoolProp(const str: string; out success: Boolean): Boolean;
var
  i: integer;
begin
  success := TryStrToInt(str, i);
  if success then Result := i <> 0 else Result := False;
end;
//------------------------------------------------------------------------------

function GetPointDProp(const str: string; out success: Boolean): TPointD;
var
  da: TArrayOfDouble;
begin
  da := GetDoubleArray(str, success);
  if not success or (Length(da) <> 2) then Exit;
  Result.X := da[0];
  Result.Y := da[1];
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function MakeUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;
var
  len: integer;
begin
  len := endC - c;
  SetLength(Result, len);
  if len = 0 then Exit;
  Move(c^, Result[1], len * SizeOf(UTF8Char));
  c := endC;
end;
//------------------------------------------------------------------------------

function PeekChar(c: PUTF8Char; endC: PUTF8Char;
  offset: integer; out chr: Utf8Char): Boolean;
begin
  inc(c, offset);
  Result := c < endC;
  if Result then chr := c^;
end;
//------------------------------------------------------------------------------

function CheckChar(c: PUTF8Char; endC: PUTF8Char;
  offset: integer; matchChr: Utf8Char): Boolean;
begin
  inc(c, offset);
  Result := (c < endC) and (c^ = matchChr);
end;
//------------------------------------------------------------------------------

function GetChar(var c: PUTF8Char; endC: PUTF8Char; out chr: Utf8Char): Boolean;
begin
  Result := (c < endC);
  if not Result then Exit;
  chr := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function GetWord(var c: PUTF8Char; endC: PUTF8Char; out word: Utf8String): Boolean;
var
  len: integer;
  c2: PUTF8Char;
begin
  c2 := c;
  inc(c);
  while (c < endC) and (c^ > space) do inc(c);
  len := c-c2;
  SetLength(word, len);
  Result := len > 0;
  if Result then Move(c2^, word[1], len * SizeOf(UTF8Char));
end;
//------------------------------------------------------------------------------

function GetNameLength(var c: PUTF8Char; endC: PUTF8Char): integer; overload;
var
  c2: PUTF8Char;
begin
  c2 := c;
  inc(c);
  while (c < endC) and
    ((c^ >= '?') or ((c^ >= '0') and (c^ <= '9'))) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

function GetAttribName(var c: PUTF8Char; endC: PUTF8Char): Utf8String;
var
  c2: PUTF8Char;
begin
  Result := '';
  if not SkipBlanks(c, endC) then Exit;
  c2 := c;
  GetNameLength(c, endC);
  Result := MakeUTF8String(c2, c);
end;
//------------------------------------------------------------------------------

function GetNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  Result := #0;
  if not SkipBlanks(c, endC) then Exit;
  Result := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function GetQuote(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if SkipBlanks(c, endC) and (c^ in [squote, dquote]) then
  begin
    Result := c^;
    inc(c);
  end else
    Result := #0;
end;
//------------------------------------------------------------------------------

function GetAttribValue(var c: PUTF8Char; endC: PUTF8Char): Utf8String;
var
  quoteChar : UTF8Char;
  c2: PUTF8Char;
begin
  Result := '';
  if GetNextChar(c, endC) <> '=' then Exit;
  quoteChar := GetQuote(c, endC);
  if quoteChar = #0 then Exit;
  c2 := c;
  while (c < endC) and (c^ <> quoteChar) do inc(c);
  Result := MakeUTF8String(c2, c);
  inc(c); //skip end quote
end;

//------------------------------------------------------------------------------
// Write to XML string functions
//------------------------------------------------------------------------------

function SimpleHtmlEncode(const Data: string): string;
var
  i: Integer;
begin
  result := '';
  for i := 1 to length(Data) do
    case Data[i] of
      #10 : result := result + '&#10;';
      #9  : result := result + '&#9;'; //string arrays :)
      '<' : result := result + '&lt;';
      '>' : result := result + '&gt;';
      '&' : result := result + '&amp;';
      '''': result := result + '&apos;';
      '"' : result := result + '&quot;';
    else
      result := result + Data[i];
    end;
end;
//------------------------------------------------------------------------------

function BooleanToStr(val: Boolean): string;
const
  bool: array[Boolean] of string = ('0','1');
begin
  Result := Bool[val];
end;
//------------------------------------------------------------------------------

function WriteSpaces(level: integer): UTF8String;
var
  len: integer;
begin
  len := level * spacesPerLevel;
  SetLength(Result, len);
  if len > 0 then FillChar(Result[1], len, space);
end;
//------------------------------------------------------------------------------

function PointDToDblArray(const pt: TPointD): TArrayOfDouble;
begin
  SetLength(Result, 2);
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;
//------------------------------------------------------------------------------

function RectDToDblArray(const rec: TRectD): TArrayOfDouble;
begin
  SetLength(Result, 4);
  Result[0] := rec.Left;
  Result[1] := rec.Top;
  Result[2] := rec.Right;
  Result[3] := rec.Bottom;
end;
//------------------------------------------------------------------------------

function PathDToDblArray(const path: TPathD): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(path);
  SetLength(Result, len*2);
  for i := 0 to len -1 do
  begin
    Result[i*2]     := path[i].X;
    Result[i*2 +1]  := path[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function DoubleArrayToStr(const dbls: TArrayOfDouble): UTF8String;
var
  i, len: integer;
  s: string;
begin
  Result := '[ ';
  len := Length(dbls);
  if len > 0 then
  begin
    s := '';
    for i := 0 to len -2 do
      if i mod 16 = 15 then
        s := s + format('%1.2f, '#10, [dbls[i]]) else
        s := s + format('%1.2f, ', [dbls[i]]);
    s := s + format('%1.2f', [dbls[len-1]]);
  end;
  Result := Result + UTF8String(s) +' ]';
end;
//------------------------------------------------------------------------------

function IntArrayToStr(const ints: TArrayOfInteger): UTF8String;
var
  i, len: integer;
  s: string;
begin
  Result := '[ ';
  len := Length(ints);
  if len > 0 then
  begin
    s := '';
    for i := 0 to len -2 do
      if i mod 32 = 31 then
        s := s + format('%d, '#10, [ints[i]]) else
        s := s + format('%d, ', [ints[i]]);
    s := s + format('%d', [ints[len-1]]);
  end;
  Result := Result + UTF8String(s) +' ]';
end;
//------------------------------------------------------------------------------

function PointerToString(p: Pointer): string;
begin
  Result := Format('%p',[p]);
end;
//------------------------------------------------------------------------------

function StorageToId(obj: TStorage): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to objIdList.Count -1 do
    if obj = objIdList[i] then
    begin
      Result := i;
      Exit;
    end;
end;

//------------------------------------------------------------------------------
// Reading from and Writing to XML streams
//------------------------------------------------------------------------------

procedure LoadStoredObjects(const utf8: UTF8String; storeManager: TStorageManager);
var
  loadingObjectsList  : TList;
  loadingPtrProps     : TList;
  xmlCurr, xmlEnd, c  : PUTF8Char;
  attName, attVal     : Utf8String;
  clssName: string;
  storeClass          : TStorageClass;
  currChild           : TStorage;
  loadPtrRec          : PLoadPtrRec;
  i, cnt              : Integer;
  savedDecSep         : Char;

  function SkipComment(var c: PUTF8Char; endC: PUTF8Char): Boolean;
  var
    str: Utf8String;
  begin
    while SkipBlanks(c, endC) and
      GetWord(c, endC, str) and (str <> '-->') do;
    Result := (str = '-->');
  end;

begin
  if not Assigned(storeManager) or (utf8 = '') then Exit;
  savedDecSep := {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  loadingPtrProps := TList.Create;
  loadingObjectsList := TList.Create;
  try
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    xmlCurr := PUTF8Char(utf8);
    xmlEnd := xmlCurr;
    inc(xmlEnd, Length(utf8));
    currChild := storeManager;
    currChild.fStgState := ssLoading;
    while (xmlCurr <= xmlEnd) do
    begin
      //get next storage object class
      if not SkipBlanks(xmlCurr, xmlEnd) or
        (xmlCurr^ <> '<') then Break;
      inc(xmlCurr);
      if (xmlCurr^ = '!') then //comment
      begin
        if not CheckChar(xmlCurr, xmlEnd, 1, '-') or
          not CheckChar(xmlCurr, xmlEnd, 2, '-') then
            Break;
        inc(xmlCurr, 3);
        if SkipComment(xmlCurr, xmlEnd) then
          Continue else
          Break;
      end
      else if (xmlCurr^ = '/') then
      begin
        //ending the current child
        inc(xmlCurr);
        c := xmlCurr;
        GetNameLength(xmlCurr, xmlEnd);
        clssName := string(MakeUTF8String(c, xmlCurr));
        if (currChild = storeManager) or
          not SameText(clssName, currChild.ClassName) or
          not SkipBlanks(xmlCurr, xmlEnd) or
          (xmlCurr^ <> '>') then Break;
        currChild.EndRead;
        currChild.fStgState := ssNormal;
        storeManager.DoAfterChildLoad(currChild);
        inc(xmlCurr); //'>'
        currChild := currChild.Parent;
        Continue;
      end;
      //get next object's classname and create
      c := xmlCurr;
      GetNameLength(xmlCurr, xmlEnd);
      clssName := string(MakeUTF8String(c, xmlCurr));
      storeClass := TStorageManager.GetStorageClass(clssName);
      if not Assigned(storeClass) then
        raise Exception.CreateFmt(rsClassNotRegistered, [clssName]);
      currChild := currChild.AddChild(storeClass);
      //get attributes
      while SkipBlanks(xmlCurr, xmlEnd) do
      begin
        if (xmlCurr^ = '<') then //assume a comment
        begin
          if not CheckChar(xmlCurr, xmlEnd, 1, '!') or
            not CheckChar(xmlCurr, xmlEnd, 2, '-') or
            not CheckChar(xmlCurr, xmlEnd, 3, '-') then Break;
          inc(xmlCurr, 4);
          if SkipComment(xmlCurr, xmlEnd) then Continue
          else Break;
        end
        else if (xmlCurr^ < '?') then Break;
        attName := GetAttribName(xmlCurr, xmlEnd);
        attVal := GetAttribValue(xmlCurr, xmlEnd);
        if (attName = '') then Break;
        if attName[1] = '@' then //local pointer (delay these)
        begin
          New(loadPtrRec);
          loadPtrRec.loadObj := currChild;
          loadPtrRec.propName := string(attName);
          Delete(loadPtrRec.propName, 1,1);
          loadPtrRec.propVal  := string(attVal);
          loadPtrRec.propValI := StrToIntDef(string(attVal), -1);
          loadingPtrProps.Add(loadPtrRec);
        end
        else if (attName = 'ObjId') then
        begin
          currChild.fStgId := StrToIntDef(string(attVal), -1);
          //if currId is valid then add currChild to loadingObjectsList
          if (currChild.fStgId >= 0) then
          begin
            //but first add nil for any missing objects
            for i := loadingObjectsList.Count to currChild.fStgId -1 do
              loadingObjectsList.Add(nil);
            if currChild.fStgId < loadingObjectsList.Count then
              loadingObjectsList[currChild.fStgId] := currChild else
              loadingObjectsList.Add(currChild);
          end;
        end
        else
          currChild.ReadProperty(string(attName), string(attVal));
      end;
      SkipBlanks(xmlCurr, xmlEnd);
      if (xmlCurr^ <> '>') then break;
      inc(xmlCurr);
    end;
    //finally lookup and linkup pointers
    cnt := loadingObjectsList.Count;
    for i := loadingPtrProps.Count -1 downto 0 do
      with PLoadPtrRec(loadingPtrProps[i])^ do
        begin
          if (propValI >= 0) and (propValI < cnt) then
            loadObj.ReadProperty(propName,
              PointerToString(loadingObjectsList[propValI]));
          Dispose(PLoadPtrRec(loadingPtrProps[i]));
        end;
  finally
    loadingObjectsList.Free;
    loadingPtrProps.Free;
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := savedDecSep;
  end;
end;
//------------------------------------------------------------------------------

procedure SaveStoredObjects(storageManager: TStorageManager;
  startIdx: integer; const classesToIgnore: array of TStorageClass);

  function ClassInIgnoreList(aclass: TClass): Boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to High(classesToIgnore) do
      if aclass = classesToIgnore[i] then Exit;
    Result := False;
  end;

  procedure AddToObjIdList(obj: TStorage);
  var
    i: integer;
  begin
    if ClassInIgnoreList(obj.ClassType) then
    begin
      obj.fStgId := -1 //use storageId to flag ignored.
    end else
    begin
      obj.fStgId := 0;
      objIdList.Add(obj);
      for i := 0 to obj.ChildCount -1 do
          AddToObjIdList(obj.Child[i]);
    end;
  end;

var
  i           : integer;
  objId       : integer;
  savedDecSep : Char;
begin
  if not Assigned(storageManager) then Exit;

  savedDecSep := {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
  storageManager.fCurrLevel := 0;
  objId := startIdx;
  objIdList := TList.Create;
  try
    //build objIdList but don't include the storagemanager
    //itself in the list, just its descendants
    //objIdList.Add(nil); // dummy for TStorageInfo
    for i := 0 to storageManager.ChildCount -1 do
      AddToObjIdList(storageManager.Child[i]);
    //it's OK to write storage now that we have object ids.
    for i := 0 to storageManager.ChildCount -1 do
      with TStorage(storageManager.Childs[i]) do
          if fStgId >= 0 then
            WriteStorageContent(objId);
  finally
    objIdList.Free;
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := savedDecSep;
  end;
end;
//------------------------------------------------------------------------------

function LoadUtf8StringFromFile(const filename: string): Utf8String;
var
  ms:TMemoryStream;
begin
  Result := '';
  if not FileExists(filename) then Exit;
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(filename);
    SetLength(Result, ms.Size);
    if ms.Size > 0 then
      Move(ms.Memory^, Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure SaveUtf8StringToFile(const utf8: Utf8String; const filename: string);
var
  ms:TMemoryStream;
  len: integer;
begin
  //if not FileExists(filename) then Exit;
  ms := TMemoryStream.Create;
  try
    len := Length(utf8);
    ms.SetSize(len);
    if len > 0 then
      Move(utf8[1], ms.Memory^, len);
    ms.SaveToFile(filename);
  finally
    ms.Free;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// TStorage
//------------------------------------------------------------------------------

constructor TStorage.Create(parent:  TStorage; const name: string);
begin
  fChilds := TList.Create;
  fName := name;
  if Assigned(parent) then
  begin
    fIndex := parent.fChilds.Add(self);
    fParent := parent;
{$IFNDEF NO_STORAGE}
    if Assigned(parent.fManager) then
    begin
      fManager := parent.fManager;
      fStgState := fManager.fStgState;
    end;
  end;
  if fStgState = ssLoading then BeginRead;
{$ELSE}
  end;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TStorage.Destroy;
begin
  ClearChildren;
  fChilds.Free;
  if Assigned(fParent) then
    fParent.RemoveChildFromList(index);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TStorage.Free;
begin
  if Self = nil then Exit;
  fStgState := ssDestroying;
  inherited Free;
end;
//------------------------------------------------------------------------------

procedure TStorage.SetName(const aName: string);
begin
  fName := aName;
end;
//------------------------------------------------------------------------------

function TStorage.RemoveChildFromList(index: integer): TStorage;
begin
  CheckChildIndex(index);
  Result := TStorage(fChilds[index]);
  Result.fParent := nil;
  fChilds.Delete(index);
  ReindexChilds(index);
end;
//------------------------------------------------------------------------------

function TStorage.IsOwnedBy(obj: TStorage): Boolean;
var
  currObj: TStorage;
begin
  Result := true;
  currObj := self;
  while Assigned(currObj) do
    if currObj.Parent = obj then Exit
    else currObj := currObj.Parent;
  Result := false;
end;
//------------------------------------------------------------------------------

function TStorage.IsOwnedBy(objClass: TStorageClass): Boolean;
var
  currObj: TStorage;
begin
  Result := true;
  currObj := self;
  while Assigned(currObj) do
    if currObj.Parent is objClass then Exit
    else currObj := currObj.Parent;
  Result := false;
end;
//------------------------------------------------------------------------------

{$IFNDEF NO_STORAGE}
function TStorage.FindByName(const objName: string): TStorage;
var
  i: integer;
begin
  if SameText(objName, Name) then
  begin
    Result := self;
    Exit;
  end;
  Result := nil;
  for i := 0 to ChildCount -1 do
    begin
      Result := TStorage(Child[i]).FindByName(objName);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

function TStorage.FindById(const objId: integer): TStorage;
var
  i: integer;
begin
  if self.fStgId = objId then
  begin
    Result := self;
    Exit;
  end;
  Result := nil;
  for i := 0 to ChildCount -1 do
    begin
      Result := TStorage(Child[i]).FindById(objId);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

function TStorage.FindByClass(stgClass: TStorageClass): TStorage;
var
  i: integer;
begin
  if (self is stgClass) then
  begin
    Result := self;
    Exit;
  end;
  Result := nil;
  for i := 0 to ChildCount -1 do
    begin
      Result := TStorage(Child[i]).FindByClass(stgClass);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

function TStorage.FindByClassAndName(stgClass: TStorageClass;
  const objName: string): TStorage;
var
  i: integer;
begin
  if (self is stgClass) and SameText(objName, Name) then
  begin
    Result := self;
    Exit;
  end;
  Result := nil;
  for i := 0 to ChildCount -1 do
    begin
      Result := TStorage(Child[i]).FindByClassAndName(stgClass, objName);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

procedure TStorage.BeginRead;
begin
end;
//------------------------------------------------------------------------------

procedure TStorage.EndRead;
begin
end;
//------------------------------------------------------------------------------

function TStorage.ReadProperty(const propName, propVal: string): Boolean;
begin
  if propName = 'Name' then
    fName := GetStringProp(propVal, Result)
else
  Result := false;
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteIntProp(const propName: string; propVal: integer);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%d"'#10, [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteCardinalProp(const propName: string; propVal: Cardinal);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, UIntToStr(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteBoolProp(const propName: string; propVal: Boolean);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, BooleanToStr(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteColorProp(const propName: string; propVal: TColor32);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%8.8x"'#10, [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteDoubleProp(const propName: string; propVal: double);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%1.2f"'#10, [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WritePointDProp(const propName: string; propVal: TPointD);
var
  da: TArrayOfDouble;
begin
  da := PointDToDblArray(propVal);
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteRectDProp(const propName: string; propVal: TRectD);
var
  da: TArrayOfDouble;
begin
  da := RectDToDblArray(propVal);
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WritePathDProp(const propName: string; propVal: TPathD);
var
  da: TArrayOfDouble;
begin
  da := PathDToDblArray(propVal);
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStrProp(const propName, propVal: string);
begin
  fManager.AddWriteString(
    UTF8String(format('  %s="%s"'#10, [propName, SimpleHtmlEncode(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteLocalStorageProp(const propName: string; propVal: TStorage);
begin
  if not Assigned(propVal) then Exit;
  fManager.AddWriteString(UTF8String(format('  @%s="%d"'#10,
    [propName, StorageToId(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteExternalProp(const propName: string; propVal: TObject);
begin
  if not Assigned(propVal) then Exit;
  fManager.AddWriteString(UTF8String(format('  ?%s="%s"'#10,
    [propName, fManager.GetExternPropName(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteEventProp(const propName: string; propVal: TNotifyEvent);
begin
  fManager.AddWriteString(
    UTF8String(format('  ?%s="%s"'#10,
      [propName, fManager.GetEventName(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStorageHeader(var objId: integer);
begin
  fManager.AddWriteString(UTF8String(Format('<%s '#10, [ClassName])));
  WriteProperties;
  fManager.AddWriteString(UTF8String(Format('  ObjId="%d">'#10, [objId])));
  inc(objId);
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStorageContent(var objId: integer);
var
  i: integer;
begin
  WriteStorageHeader(objId);
  inc(fManager.fCurrLevel);
  for i := 0 to ChildCount -1 do
    with TStorage(fChilds[i]) do
      if fStgId >= 0 then
        WriteStorageContent(objId);
  dec(fManager.fCurrLevel);
  WriteStorageFooter;
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStorageFooter;
begin
  fManager.AddWriteString(UTF8String(Format('</%s>'#10, [ClassName])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteProperties;
begin
  if Name <> '' then WriteStrProp('Name', Name);
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TStorage.GetHasChildren: Boolean;
begin
  Result := fChilds.Count > 0;
end;
//------------------------------------------------------------------------------

procedure TStorage.ClearChildren;
var
  i: integer;
begin
  for i := fChilds.Count -1 downto 0 do
    with TStorage(fChilds[i]) do
      Free;
  fChilds.Clear;
end;
//------------------------------------------------------------------------------

function TStorage.GetChildCount: integer;
begin
  Result := fChilds.Count;
end;
//------------------------------------------------------------------------------

function TStorage.GetChild(index: integer): TStorage;
begin
  CheckChildIndex(index);
  Result := TStorage(fChilds[index]);
end;
//------------------------------------------------------------------------------

procedure TStorage.CheckChildIndex(index: integer);
begin
  if (index < 0) or (index >= ChildCount) then
    Raise Exception.CreateFmt('TStorage range error (%d)',[index]);
end;
//------------------------------------------------------------------------------

procedure TStorage.SetParent(parent: TStorage);
begin
  if parent = fParent then Exit;
  if Assigned(fParent) then
    fParent.RemoveChildFromList(fIndex);
  fParent := parent;
  if Assigned(fParent) then
    Self.fIndex := fParent.fChilds.Add(self);
end;
//------------------------------------------------------------------------------

procedure TStorage.ReindexChilds(startFrom: integer);
var
  i: integer;
begin
  for i := startFrom to fChilds.Count -1 do
    TStorage(fChilds[i]).fIndex := i;
end;
//------------------------------------------------------------------------------

function TStorage.AddChild(storeClass: TStorageClass): TStorage;
begin
  Result := InsertChild(ChildCount, storeClass);
end;
//------------------------------------------------------------------------------

function TStorage.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  Result := storeClass.Create(self); //auto adds to fChilds list
  if index < ChildCount -1 then
  begin
    fChilds.Move(Result.fIndex, index);
    ReindexChilds(index);
  end;
end;
//------------------------------------------------------------------------------

procedure TStorage.DeleteChild(index: integer);
begin
  CheckChildIndex(index);
  //the child will remove itself from parent because descendants
  //may need to notify parents of properties before destruction
  TStorage(fChilds[index]).Free;
end;

{$IFNDEF NO_STORAGE}
//------------------------------------------------------------------------------
// TStorageManager
//------------------------------------------------------------------------------

constructor TStorageManager.Create(parent:  TStorage; const name: string);
begin
  inherited Create(nil, name); // TStorageManager should never have a parent
  fManager := self;
  fDesignFormScale := 1;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.DoBeforeLoad;
begin
  ClearChildren;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.DoAfterChildLoad(child: TStorage);
begin
end;
//------------------------------------------------------------------------------

procedure TStorageManager.DoAfterLoad;
begin
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromFile(const filename: string);
var
  fs: TFileStream;
  utf8: Utf8String;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(utf8, fs.Size);
    fs.ReadBuffer(utf8[1], fs.Size);
  finally
    fs.Free;
  end;
  DoBeforeLoad;
  LoadStoredObjects(utf8, self);
  DoAfterLoad;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromResource(const resName: string; resType: PChar);
var
  rs: TResourceStream;
  utf8: Utf8String;
begin
  rs := TResourceStream.Create(hInstance, resName, resType);
  try
    SetLength(utf8, rs.Size);
    rs.ReadBuffer(utf8[1], rs.Size);
  finally
    rs.Free;
  end;
  DoBeforeLoad;
  LoadStoredObjects(utf8, self);
  DoAfterLoad;
end;
//------------------------------------------------------------------------------

function TStorageManager.GetEventName(event: TNotifyEvent): string;
begin
  //override this in descendant storagemanager classes
  Result := '';
end;
//------------------------------------------------------------------------------

function TStorageManager.GetExternPropName(prop: TObject): string;
begin
  //override this in descendant storagemanager classes
  Result := '';
end;
//------------------------------------------------------------------------------

procedure TStorageManager.AddWriteString(const str: Utf8String);
begin
  fXmlStr := fXmlStr + WriteSpaces(fCurrLevel) + str;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.DoBeforeWrite;
begin
end;
//------------------------------------------------------------------------------

function TStorageManager.ReadInfoProperty(const propName, propVal: string): Boolean;
begin
  if propName = 'DesignResolution' then
  begin
    fDesignScreenRes := GetDoubleProp(propVal, Result);
  end else if propName = 'DesignScale' then
  begin
    fDesignFormScale := GetDoubleProp(propVal, Result);
    if fDesignFormScale = 0 then fDesignFormScale := 1;
  end
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.WriteCustomProperties;
begin
end;
//------------------------------------------------------------------------------

procedure TStorageManager.SaveToFile(const filename: string;
  aScale: double; const classesToIgnore: array of TStorageClass);
var
  len: integer;
  fs: TFileStream;
begin
  if ChildCount = 0 then Exit;
  fDesignFormScale := aScale;
  fXmlStr := ''; //empty the write buffer (just in case)
  DoBeforeWrite;
  if not (Child[0] is TStorageInfo) then
    InsertChild(0, TStorageInfo);
  SaveStoredObjects(self, 0, classesToIgnore);
  len := Length(fXmlStr);
  fs := TFileStream.Create(filename, fmCreate);
  try
    if len > 0 then
      fs.WriteBuffer(fXmlStr[1], len);
  finally
    fs.Free;
  end;
  fXmlStr := '';
end;
//------------------------------------------------------------------------------

class function TStorageManager.GetStorageClass(
  const storageClassname: string): TStorageClass;
var
  i: integer;
begin
  if classList.Find(storageClassname, i) then
    Result := TStorageClass(classList.Objects[i]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TStorageManager.FindObjectByClass(storageClass: TStorageClass;
  parent: TStorage): TStorage;
  function FindByClass(so: TStorage): TStorage;
  var
    i: integer;
  begin
    if so is storageClass then
      Result := so
    else
    begin
      for i := 0 to so.ChildCount -1 do
      begin
        Result := FindByClass(so.Child[i]);
        if Assigned(Result) then Exit;
      end;
      Result := nil;
    end;
  end;
begin
  if Assigned(parent) then
    Result := FindByClass(parent) else
    Result := FindByClass(self);
end;
//------------------------------------------------------------------------------

function TStorageManager.FindObjectByLoadId(id: integer): TStorage;
  function FindById(so: TStorage): TStorage;
  var
    i, highI: integer;
  begin
    if id = so.fStgId then
    begin
      Result := so;
      Exit;
    end;
    Result := nil;
    highI := so.ChildCount -1;
    if (id < so.fStgId) or (HighI < 0) then Exit;
    if id >= so.Child[highI].fStgId then
      Result := FindById(so.Child[highI])
    else
      for i := 0 to highI -1 do
      begin
       Result := FindById(so.Child[i]);
       if Assigned(Result) then break;
      end;
  end;
begin
  Result := FindById(self);
end;
//------------------------------------------------------------------------------

function TStorageManager.FindObjectByName(const name: string;
  parent: TStorage): TStorage;
  function FindByName(so: TStorage): TStorage;
  var
    i,highI: integer;
  begin
    if SameText(so.fName, name) then
    begin
      Result := so;
      Exit;
    end;
    Result := nil;
    highI := so.ChildCount -1;
    for i := 0 to highI do
    begin
     Result := FindByName(so.Child[i]);
     if Assigned(Result) then break;
    end;
  end;
begin
  if Assigned(parent) then
    Result := FindByName(parent) else
    Result := FindByName(self);
end;

//------------------------------------------------------------------------------
// TStorageInfo
//------------------------------------------------------------------------------

function TStorageInfo.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := StorageManager.ReadInfoProperty(propName, propVal);
end;
//------------------------------------------------------------------------------

procedure TStorageInfo.WriteProperties;
begin
  inherited;
  if StorageManager.fDesignScreenRes > 0 then
    WriteDoubleProp('DesignResolution', StorageManager.fDesignScreenRes) else
    WriteDoubleProp('DesignResolution', DpiAwareOne);
  WriteDoubleProp('DesignScale', StorageManager.fDesignFormScale);
  StorageManager.WriteCustomProperties;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Storage class registration
//------------------------------------------------------------------------------

procedure RegisterStorageClass(storageClass: TStorageClass);
begin
  classList.AddObject(storageClass.ClassName, Pointer(storageClass));
end;
//------------------------------------------------------------------------------

procedure InitStorageClassRegister;
begin
  classList := TStringList.Create;
  classList.Duplicates := dupIgnore;
  classList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure EndStorageClassRegister;
begin
  classList.Free;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  InitStorageClassRegister;
{$IFNDEF NO_STORAGE}
  RegisterStorageClass(TStorageInfo);
{$ENDIF}

finalization
  EndStorageClassRegister;

end.
