unit Img32.Storage;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.9                                                             *
* Date      :  9 August 2025                                                   *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  TLayer32 based object persistence                               *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************)

interface

{$I Img32.inc}

{$IFDEF USE_FILE_STORAGE}
  {$TYPEINFO ON}
{$ENDIF}

uses
  SysUtils, Classes, {$IFDEF USE_FILE_STORAGE} TypInfo, {$ENDIF} Img32;

type
  TStorageState = (ssNormal, ssLoading, ssSaving, ssDestroying);

  TStorage = class;
  TStorageClass = class of TStorage;
{$IFDEF USE_FILE_STORAGE}
  TStorageManager = class;
{$ENDIF}

  TStorage = class(TInterfacedObj)
  private
    fParent         : TStorage;
{$IFDEF USE_FILE_STORAGE}
    fStgState       : TStorageState;
    fManager        : TStorageManager;
    fIgnoreOnWrite  : Boolean;
{$ENDIF}
    fChilds         : TList;
    fIndex          : integer;
    fName           : string;
    fStgId          : integer;
    function  GetChildCount: integer;
    function  GetHasChildren: Boolean;
  protected
    procedure SetName(const aName: string); virtual;
    function  GetChild(index: integer): TStorage;
    procedure SetParent(parent: TStorage); virtual;
    procedure ReindexChilds(startFrom: integer);
    procedure CheckChildIndex(index: integer); virtual;
    function  RemoveChildFromList(index: integer): TStorage; virtual;
{$IFDEF USE_FILE_STORAGE}
    procedure BeginRead; virtual;
    function  ReadProperty(const propName, propVal: string): Boolean; virtual;
    procedure EndRead; virtual;
    procedure WriteProperties; virtual;
    function  CheckSkipWriteProperty(propInfo: PPropInfo): Boolean; virtual;
    procedure WriteStorage;
    procedure WriteEnumProp(const propName, propVal: string);
    procedure WriteIntProp(const propName: string; propVal: integer);
    procedure WriteCardinalProp(const propName: string; propVal: Cardinal);
    procedure WriteBoolProp(const propName: string; propVal: Boolean);
    procedure WriteColorProp(const propName: string; propVal: TColor32);
    procedure WriteDoubleProp(const propName: string; propVal: double);
    procedure WritePointDProp(const propName: string; propVal: TPointD);
    procedure WriteRectDProp(const propName: string; propVal: TRectD);
    procedure WritePathDProp(const propName: string; propVal: TPathD);
    procedure WriteLocalStorageProp(const propName: string; propVal: TStorage);
    procedure WriteStrProp(const propName, propVal: string);
{$ENDIF}
  public
    constructor Create(parent:  TStorage = nil; const name: string = ''); virtual;
    destructor  Destroy; override;
    procedure Free;
    procedure ClearChildren; virtual;
    function  AddChild(storeClass: TStorageClass): TStorage; virtual;
    function  InsertChild(index: integer; storeClass: TStorageClass): TStorage; virtual;
    procedure DeleteChild(index: integer); virtual;
    function  IsOwnedBy(obj: TStorage): Boolean; overload;
    function  IsOwnedBy(objClass: TStorageClass): Boolean; overload;
{$IFDEF USE_FILE_STORAGE}
//    function  StorageToId(obj: TStorage): integer;
    function  FindChildByName(const objName: string): TStorage;
    function  FindChildByID(const objId: integer): TStorage;
    function  FindChildByClass(stgClass: TStorageClass): TStorage;
    property  StorageManager  : TStorageManager read fManager;
    property  IgnoreOnWrite   : Boolean write fIgnoreOnWrite;
    property  StorageState    : TStorageState read fStgState write fStgState;
{$ENDIF}
    property  Child[index: integer]: TStorage read GetChild;
    property  Childs          : TList read fChilds;
    property  ChildCount      : integer read GetChildCount;
    property  HasChildren     : Boolean read GetHasChildren;
    property  Index           : integer read fIndex;
    property  Parent          : TStorage read fParent write SetParent;
{$IFDEF USE_FILE_STORAGE}
  published
{$ENDIF}
    property  Name            : string read fName write SetName;
    property  Id              : integer read fStgId write fStgId;
  end;

{$IFDEF USE_FILE_STORAGE}
  TStorageManager = class(TStorage)
  private
    fDesignScreenRes  : double;
    fDesignScale      : double;
    fWriteStr         : Utf8String;
    fWriteLevel       : integer;
  protected
    procedure SetDesignScale(value: double); virtual;
    procedure BeginRead; override;
    procedure EndRead; override;
    procedure DoBeforeWrite; virtual;
    procedure LoadStoredObjects(const utf8: UTF8String); virtual;
    procedure WriteAllObject; virtual;
    function  GetExternalPropName(obj: TObject): string; virtual;
    function  GetExternalEvent(const method: TMethod; out textId: string): Boolean; virtual;
  public
    constructor Create(parent:  TStorage = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure LoadFromFile(const filename: string);
    procedure LoadFromText(const text: Utf8String);
    procedure LoadFromStream(stream: TStream);
    procedure LoadFromResource(const resName: string; resType: PChar);
    procedure WriteLn(const str: Utf8String);
    procedure Write(const str: Utf8String);
    procedure SaveToFile(const filename: string; aScale: double);
    function SaveToText(aScale: double): Utf8String;
    procedure SaveToStream(stream: TStream; aScale: double);
    function FindObjectByClass(storageClass: TStorageClass;
      parent: TStorage = nil): TStorage;
    function FindObjectByName(const name: string; parent: TStorage = nil): TStorage;
  published
    property DesignResolution: double read fDesignScreenRes write fDesignScreenRes;
    property DesignScale: double read fDesignScale write SetDesignScale;
  end;

  function GetPointDProp(const str: string; out success: Boolean): TPointD;
  procedure RegisterStorageClass(storageClass: TStorageClass);

  function GetStorageClass(const storageClassname: string): TStorageClass;
  function HasPublishedProperty(const className, propName: string): Boolean;
  function SimpleXmlEncode(const Data: string): string;
  function SimpleXmlDecode(const Data: string): string;

{$ENDIF}

implementation

uses Img32.Layers;

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

{$IFDEF USE_FILE_STORAGE}
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

function SimpleXmlEncode(const Data: string): string;
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

function SimpleXmlDecode(const Data: string): string;
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
      case Data[j + 1] of
        'g': result := result + '>';
        'l': result := result + '<';
        'q': result := result + '"';
        'a':
          case Data[j+2] of
            'm' : result := result + '&';
            'p' : result := result + '''';
          end;
        '#': result := result +
          Chr(StrToIntDef(Copy(Data, j + 2, i - j - 2), 32));
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
      buffSize := buffSize * 2;
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
  if not Result then Exit; //error
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
      buffSize := buffSize * 2;
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
  SetLength(Result, len * 2);
  for i := 0 to len - 1 do
  begin
    Result[i * 2]     := path[i].X;
    Result[i * 2 + 1] := path[i].Y;
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
// Reading from and Writing to XML streams
//------------------------------------------------------------------------------

procedure TStorageManager.LoadStoredObjects(const utf8: UTF8String);
begin
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
//------------------------------------------------------------------------------
{$ENDIF}

{$IFDEF USE_FILE_STORAGE}

function GetStorageClass(const storageClassname: string): TStorageClass;
var
  i: integer;
begin
  if classList.Find(storageClassname, i) then
    Result := TStorageClass(classList.Objects[i]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function HasPublishedProperty(const className, propName: string): Boolean;
begin
  Result := TypInfo.IsPublishedProp(GetStorageClass(ClassName), propName);
end;

{$ENDIF}

//------------------------------------------------------------------------------
// TStorage
//------------------------------------------------------------------------------

constructor TStorage.Create(parent: TStorage; const name: string);
begin
  fChilds := TList.Create;
  fName := name;
  Self.Parent := parent;
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
{$IFDEF USE_FILE_STORAGE}
  fStgState := ssDestroying;
{$ENDIF}
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

{$IFDEF USE_FILE_STORAGE}

function TStorage.FindChildByName(const objName: string): TStorage;
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
      Result := TStorage(Child[i]).FindChildByName(objName);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

function TStorage.FindChildByID(const objId: integer): TStorage;
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
      Result := TStorage(Child[i]).FindChildByID(objId);
      if Assigned(Result) then Break;
    end;
end;
//------------------------------------------------------------------------------

function TStorage.FindChildByClass(stgClass: TStorageClass): TStorage;
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
      Result := TStorage(Child[i]).FindChildByClass(stgClass);
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
var
  pi: PPropInfo;
begin
  Result := False;
  if IsPublishedProp(self, propName) then
  try
    pi := GetPropInfo(self.ClassType, propName);
    case pi.PropType^.Kind of
      tkEnumeration: SetEnumProp(self, propName, propVal);
      tkInteger: SetOrdProp(self, pi, Cardinal(strToIntDef(propVal, 0)));
      tkFloat: SetFloatProp(self, propName, strToFloatDef(propVal, 0.0));
      tkUString: SetPropValue(self, propName, propVal);
      else Exit; // eg methods and classes aren't handled here
    end;
    Result := true;
  except
    Result := False;
  end;
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteIntProp(const propName: string; propVal: integer);
begin
  fManager.WriteLn(UTF8String(format('  %s="%d"', [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteCardinalProp(const propName: string; propVal: Cardinal);
begin
  fManager.WriteLn(UTF8String(format('  %s="%s"', [propName, UIntToStr(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteBoolProp(const propName: string; propVal: Boolean);
begin
  fManager.WriteLn(UTF8String(format('  %s="%s"', [propName, BooleanToStr(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteColorProp(const propName: string; propVal: TColor32);
begin
  fManager.WriteLn(UTF8String(format('  %s="$%8.8x"', [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteDoubleProp(const propName: string; propVal: double);
begin
  fManager.WriteLn(UTF8String(format('  %s="%1.2f"', [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WritePointDProp(const propName: string; propVal: TPointD);
var
  da: TArrayOfDouble;
begin
  da := PointDToDblArray(propVal);
  fManager.WriteLn(UTF8String(format('  %s="%s"', [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteRectDProp(const propName: string; propVal: TRectD);
var
  da: TArrayOfDouble;
begin
  da := RectDToDblArray(propVal);
  fManager.WriteLn(UTF8String(format('  %s="%s"', [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WritePathDProp(const propName: string; propVal: TPathD);
var
  da: TArrayOfDouble;
begin
  da := PathDToDblArray(propVal);
  fManager.WriteLn(
    UTF8String(format('  %s="%s"', [propName, DoubleArrayToStr(da)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStrProp(const propName, propVal: string);
begin
  fManager.WriteLn(
    UTF8String(format('  %s="%s"', [propName, SimpleXmlEncode(propVal)])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteEnumProp(const propName, propVal: string);
begin
  fManager.WriteLn(UTF8String(format('  %s="%s"', [propName, propVal])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteLocalStorageProp(const propName: string; propVal: TStorage);
begin
  if not Assigned(propVal) then Exit;
  fManager.WriteLn(UTF8String(format('  %s="%d"',[propName, propVal.id])));
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteStorage;
var
  i: integer;
begin
  if fIgnoreOnWrite then Exit;

  // start object element
  fManager.WriteLn(UTF8String(Format('<%s ', [ClassName])));

  // write object properties
  WriteProperties;
  fManager.Write('>');

  // write object's children
  if ChildCount > 0 then
  begin
    inc(fManager.fWriteLevel);
    for i := 0 to ChildCount -1 do
      with TStorage(fChilds[i]) do
        WriteStorage;
    dec(fManager.fWriteLevel);
  end;

  // end object element
  fManager.WriteLn(UTF8String(Format('</%s>', [ClassName])));
end;
//------------------------------------------------------------------------------

function TStorage.CheckSkipWriteProperty(propInfo: PPropInfo): Boolean;
begin
  // Return TRUE when writing of a specific published property
  // is handled in this method (in a descendant class)
  // and hence bypass the default storage writing.

  // For example, in a descendant storage class
  // only write a Font property if it differs from its parent's Font
  // //  var propName := string(propInfo.Name);
  // //  if (propName <> 'Font') or not Assigned(parent) or
  // //    not HasPublishedProperty(parent.Classname, propName) then Exit;
  // //  Result := GetObjectProp(self, propName) = GetObjectProp(Parent, propName);

  Result := False; // ie use the default writing
end;
//------------------------------------------------------------------------------

procedure TStorage.WriteProperties;
var
  i, j: integer;
  c: cardinal;
  d: double;
  m: TMethod;
  propName: string;
  s: UnicodeString;
  obj: TObject;
  propInfos: PPropList;
  typeInfo: PTypeInfo;
  typeData: PTypeData;
begin
  typeInfo := PTypeInfo(ClassType.ClassInfo);
  typeData := GetTypeData(typeInfo);
  GetMem(propInfos, typeData.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(typeInfo, propInfos);
    for i := 0 to typeData.PropCount -1 do
      with propInfos^[i]^ do
      begin
        //
        if CheckSkipWriteProperty(propInfos^[i]) then Continue;

        propName := string(name);
        case PropType^.Kind of
          tkEnumeration:
          begin
            s := GetEnumProp(self, propName);
            WriteEnumProp(propName, s);
          end;
          tkInteger:
          begin
            if GetTypeData(PropType^).OrdType = otULong then
            begin
              c := GetOrdProp(self, propName);
              if pos('Color', propName) > 0 then
                WriteColorProp(propName, c) else
                WriteCardinalProp(propName, c);
            end else
            begin
              j := GetOrdProp(self, propName);
              WriteIntProp(propName, j);
            end;
          end;
          tkFloat:
          begin
            d := GetFloatProp(self, propName);
            WriteDoubleProp(propName, d);
          end;
          tkUString:
          begin
            s := GetStrProp(self, propName);
            if s <> '' then WriteStrProp(propName, s);
          end;
          tkClass:
          begin
            obj := GetObjectProp(self, propName);
            if obj is TStorage then
              WriteLocalStorageProp(propName, TStorage(obj))
            else
            begin
              s := storageManager.GetExternalPropName(obj);
              if s <> '' then WriteStrProp(propName, s);
            end;
          end;
          tkMethod:
          begin
            m := GetMethodProp(self, propName);
            if storageManager.GetExternalEvent(m, s) then
              WriteStrProp(propName, s);
          end;
          tkRecord:
          begin
//            if PropType^.Kind = tkClass then beep;
//            if PropType^.Name = '' then beep;
          end;
        end;
      end;
  finally
    FreeMem(propInfos);
  end;
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
  if not Assigned(fParent) then Exit;
  Self.fIndex := fParent.fChilds.Add(self);
{$IFDEF USE_FILE_STORAGE}
  if Assigned(Parent.fManager) then
  begin
    fManager := Parent.fManager;
    fStgState := fManager.fStgState;
  end;
  if fStgState = ssLoading then BeginRead;
{$ENDIF}

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

{$IFDEF USE_FILE_STORAGE}
//------------------------------------------------------------------------------
// TStorageManager
//------------------------------------------------------------------------------

constructor TStorageManager.Create(parent:  TStorage; const name: string);
begin
  inherited Create(nil, name); // TStorageManager should never have a parent
  fManager := self;
  fDesignScale := 1;
  fDesignScreenRes := 1;
end;
//------------------------------------------------------------------------------

destructor TStorageManager.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.SetDesignScale(value: double);
begin
  if value <= 0 then Exit;
  fDesignScale := value;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.BeginRead;
begin
  ClearChildren;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.EndRead;
begin
  if fDesignScale = 0 then fDesignScale := 1;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromText(const text: Utf8String);
begin
  if text = '' then Exit;
  BeginRead;
  LoadStoredObjects(text);
  EndRead;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromStream(stream: TStream);
var
  utf8: Utf8String;
begin
  SetLength(utf8, stream.Size - stream.Position);
  stream.ReadBuffer(utf8[1], stream.Size - stream.Position);
  LoadFromText(utf8);
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromFile(const filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.LoadFromResource(const resName: string; resType: PChar);
var
  rs: TResourceStream;
begin
  rs := TResourceStream.Create(hInstance, resName, resType);
  try
    LoadFromStream(rs);
  finally
    rs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.WriteAllObject;

  procedure RecursiveAddToObjIdList(obj: TStorage; var idx: integer);
  var
    i: integer;
  begin
     obj.fStgId := idx; Inc(idx);
    for i := 0 to obj.ChildCount -1 do
        RecursiveAddToObjIdList(obj.Child[i], idx);
  end;

var
  i, idx : integer;
  savedDecSep : Char;
begin

  savedDecSep := {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
  try
    // assign a unique Id for each stored object
    idx := 1;
    for i := 0 to ChildCount -1 do
      RecursiveAddToObjIdList(Child[i], idx);

    DoBeforeWrite;

    // open the storageManager object
    fWriteStr := UTf8String(Format('<%s ', [ClassName]));
    WriteProperties;
    fManager.Write('>');

      fWriteLevel := 1;
      for i := 0 to ChildCount -1 do
        TStorage(Childs[i]).WriteStorage;

    // close the storageManager object
    WriteLn(UTF8String(Format('</%s>', [ClassName])));
    WriteLn('');
  finally
    //objIdList.Clear;
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := savedDecSep;
  end;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.WriteLn(const str: Utf8String);
begin
  fWriteStr := fWriteStr + #10 + WriteSpaces(fWriteLevel) + str;
end;
//------------------------------------------------------------------------------

procedure TStorageManager.Write(const str: Utf8String);
begin
  fWriteStr := fWriteStr + str;
end;
//------------------------------------------------------------------------------

function TStorageManager.GetExternalPropName(obj: TObject): string;
begin
  Result := '';
end;
//------------------------------------------------------------------------------

function TStorageManager.GetExternalEvent(const method: TMethod; out textId: string): Boolean;
begin
  textId := '';
  result := False;
end;
//------------------------------------------------------------------------------


procedure TStorageManager.DoBeforeWrite;
begin
end;
//------------------------------------------------------------------------------

function TStorageManager.SaveToText(aScale: double): Utf8String;
begin
  if ChildCount = 0 then Exit;
  fDesignScale := aScale;
  fWriteStr := ''; //empty the write buffer (just in case)
  WriteAllObject;
  Result := fWriteStr;
  fWriteStr := '';
end;
//------------------------------------------------------------------------------

procedure TStorageManager.SaveToStream(stream: TStream; aScale: double);
var
  len   : integer;
  text  : Utf8String;
begin
  if ChildCount = 0 then Exit;
  text := SaveToText(aScale);
  len := Length(text);
  if len > 0 then
    stream.WriteBuffer(text[1], len);
end;
//------------------------------------------------------------------------------

procedure TStorageManager.SaveToFile(const filename: string; aScale: double);
var
  fs: TFileStream;
begin
  if ChildCount = 0 then Exit;
  fs := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(fs, aScale);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

function TStorageManager.FindObjectByClass(storageClass: TStorageClass;
  parent: TStorage): TStorage;
  function FindChildByClass(so: TStorage): TStorage;
  var
    i: integer;
  begin
    if so is storageClass then
      Result := so
    else
    begin
      for i := 0 to so.ChildCount -1 do
      begin
        Result := FindChildByClass(so.Child[i]);
        if Assigned(Result) then Exit;
      end;
      Result := nil;
    end;
  end;
begin
  if Assigned(parent) then
    Result := FindChildByClass(parent) else
    Result := FindChildByClass(self);
end;
//------------------------------------------------------------------------------

function TStorageManager.FindObjectByName(const name: string;
  parent: TStorage): TStorage;

  function FindChildByName(so: TStorage): TStorage;
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
     Result := FindChildByName(so.Child[i]);
     if Assigned(Result) then break;
    end;
  end;

begin
  if Assigned(parent) then
    Result := FindChildByName(parent) else
    Result := FindChildByName(self);
end;

{$ENDIF}

//------------------------------------------------------------------------------
// Storage class registration
//------------------------------------------------------------------------------

procedure InitStorageClassRegister;
begin
  if Assigned(classList) then Exit;
  classList := TStringList.Create;
  classList.Duplicates := dupIgnore;
  classList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure RegisterStorageClass(storageClass: TStorageClass);
begin
  if not Assigned(classList) then InitStorageClassRegister;
  classList.AddObject(storageClass.ClassName, Pointer(storageClass));
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
{$IFDEF USE_FILE_STORAGE}
  // register any storage classes that are
  // contained by TStorageManager or its descendant
  // but do this in the unit in which the are defined.
{$ENDIF}

finalization
  EndStorageClassRegister;

end.
