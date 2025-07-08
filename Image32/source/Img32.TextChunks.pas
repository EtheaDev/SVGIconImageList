unit Img32.TextChunks;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  9 May 2025                                                      *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Manages navigating, formatting and displaying text.             *
* License   :  https://www.boost.org/LICENSE_1_0.txt                           *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, ShlObj, ActiveX, {$ENDIF}
  Types, SysUtils, Classes, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Text, Img32.Draw, Img32.Vector;

type

  {$IFNDEF Unicode}
  UnicodeString = WideString;
  {$ENDIF}

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

  TChunkedText = class;

  TTextChunk = class
  private
    fFontCache    : TFontCache;
    fText         : UnicodeString;
  public
    owner         : TChunkedText;
    index         : integer;
    length        : integer;        // text length
    width         : double;         // pixel width
    height        : double;         // pixel height
    backColor     : TColor32;
    fontColor     : TColor32;
    ascent        : double;
    userData      : Pointer;
    glyphOffsets  : TArrayOfDouble;
    arrayOfPaths  : TArrayOfPathsD;
    constructor Create(owner: TChunkedText; const chunk: UnicodeString;
      index: integer; fontCache: TFontCache; fontColor: TColor32;
      backColor: TColor32 = clNone32);
    function IsBlankSpaces: Boolean;
    function IsNewline: Boolean;
    function IsText: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    function Next: TTextChunk;
    function Prev: TTextChunk;
    property Text: UnicodeString read fText;
  end;

  TDrawChunkEvent = procedure(chunk: TTextChunk; const chunkRec: TRectD) of object;

  // TPageTextMetrics - metrics of TChunkedText for
  // supplied Bounds and starting at firstChunkIdx
  TPageTextMetrics = record
    bounds            : TRect;
    totalLines        : integer; // starting at firstChunkIdx
    visibleLines      : integer; // starting at firstChunkIdx
    lineHeight        : double;  // either supplied or calc. from visible chunks
    topLinePxOffset   : integer; // adjustment value when vert. centering text
    startPos          : TPoint;  // X: chunk index; Y; 0 based char index
    nextStartPos      : TPoint;  // position just below current visible lines
    startOfLineIdx    : TArrayOfInteger; // see extensive note below
    justifyDeltas     : TArrayOfDouble;
    lineWidths        : TArrayOfDouble;
  end;
  // When a startOfLineIdx value is negative, it indicates the line starts with
  // an incompleted text chunk in the previous line. (This only occurs when a
  // single chunk is too wide for the supplied rect.) That value is the negated
  // character offset of that chunk. When consecutive negative values are
  // encountered, the referenced text chunk will be the first non-negative
  // value that precedes the negative values. If the very first startOfLineIdx
  // value is negative, the referenced text chunk is indicated by startPos.X.

  // TChunkedText: A font formatted list of text 'chunks' (usually space
  // separated words) that will greatly speed up displaying word-wrapped text.
  TChunkedText = class
  private
{$IFDEF XPLAT_GENERICS}
    fList         : TList<TTextChunk>;
{$ELSE}
    fList         : TList;
{$ENDIF}
    fDrawChunkEvent: TDrawChunkEvent;
    function  GetChunk(index: integer): TTextChunk;
    function  GetText: UnicodeString;
    function  GetCount: integer;
    function  DeleteSubchunk(chunk: TTextChunk; y1, y2: integer): TTextChunk;
  protected
    function GetGlyphsOrDrawInternal(image: TImage32; const rec: TRect;
      textAlign: TTextAlign; textAlignV: TTextVAlign; const startChunkPos: TPoint;
      lineHeight: double; out paths: TPathsD): TPageTextMetrics;
  public
    constructor Create; overload;
    constructor Create(const text: string; font: TFontCache;
      fontColor: TColor32 = clBlack32; backColor: TColor32 = clNone32); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteChunk(Index: Integer);
    procedure DeleteChunkRange(startIdx, endIdx: Integer);
    function  Delete(const startPos, endPos: TPoint): TPoint;
    function ChunkPosToCharIdx(const pos: TPoint): integer;
    function CharIdxToChunkPos(idx: integer): TPoint;
    function GetSubText(const startPos, endPos: TPoint): UnicodeString;
    procedure AddNewline(font: TFontCache);
    procedure AddSpaces(spaceCnt: integer; font: TFontCache);
    // GetPageMetrics - PageMetrics for all text (includes hidden text)
    function  GetPageMetrics(const rec: TRect;
      lineHeightOverride: double = 0.0): TPageTextMetrics;
    // GetPartialPageMetrics - PageMetrics just for the chunk range. This
    // function is useful when page geometry changes after a number of lines.
    function  GetPartialPageMetrics(const rec: TRect;
      startingChunkPos, EndChunkPos: TPoint; lineHeightOverride: double = 0): TPageTextMetrics;
    function  GetChunkAndGlyphOffsetAtPt(const ptm: TPageTextMetrics;
      const pt: TPoint; out chunkPos: TPoint): Boolean;
    function GetChunkRect(const ptm: TPageTextMetrics; chunkIdx: Integer): TRectD;
    function InsertTextChunk(font: TFontCache; index: integer;
      const chunk: UnicodeString; fontColor: TColor32 = clBlack32;
      backColor: TColor32 = clNone32): TTextChunk;
    function AddTextChunk(font: TFontCache; const chunk: UnicodeString;
      fontColor: TColor32 = clBlack32;
      backColor: TColor32 = clNone32): TTextChunk;
    procedure SetText(const text: UnicodeString; font: TFontCache;
      fontColor: TColor32 = clBlack32; backColor: TColor32 = clNone32);
    // DrawText: see Examples/FMX2, Examples/Text & Examples/Experimental apps.
    function DrawText(image: TImage32; const rec: TRect; textAlign: TTextAlign;
      textAlignV: TTextVAlign): TPageTextMetrics; overload;
    function DrawText(image: TImage32; const rec: TRect; textAlign: TTextAlign;
      textAlignV: TTextVAlign; const startChunkPos: TPoint;
      lineHeight: double = 0.0): TPageTextMetrics; overload;
    function GetTextGlyphs(const rec: TRect;
      textAlign: TTextAlign; textAlignV: TTextVAlign; const startChunkPos: TPoint;
      lineHeight: double = 0.0): TPathsD;
    function FirstChunk: TTextChunk;
    function LastChunk: TTextChunk;
    function GetPrevChar(var chunkPos: TPoint): Boolean;
    function GetPrevWord(var chunkPos: TPoint): Boolean;
    function GetNextChar(var chunkPos: TPoint): Boolean;
    function GetNextWord(var chunkPos: TPoint): Boolean;
    procedure ApplyNewFont(font: TFontCache);
    property Count: integer read GetCount;
    property Chunk[index: integer]: TTextChunk read GetChunk; default;
    property Text: UnicodeString read GetText;
    property OnDrawChunk: TDrawChunkEvent
      read fDrawChunkEvent write fDrawChunkEvent;
  end;

function GetLineIndexFromChunkIdx(const pageMetrics: TPageTextMetrics;
  chunkIdx: integer): integer;
function GetChunkIdxAtStartOfLine(const pageMetrics: TPageTextMetrics;
  lineIdx: integer): integer;

implementation

resourcestring
  rsInvalidChunk            = 'TChunkedText: invalid chunk';
  rsChunkedTextRangeError   = 'TChunkedText: range error.';
  rsChunkedTextFontError    = 'TChunkedText: invalid font error.';
  rsPageTextMetricsError    = 'TPageTextMetrics: range error';

const
  SPACE   = #32;
  NEWLINE = #10;

type
  TFontCacheHack = class(TFontCache);

//------------------------------------------------------------------------------

function GetLineIndexFromChunkIdx(const pageMetrics: TPageTextMetrics;
  chunkIdx: integer): integer;
begin
  Result := pageMetrics.totalLines -1;
  while (Result > 0) and (chunkIdx < pageMetrics.startOfLineIdx[Result]) do
    Dec(Result);
end;
//------------------------------------------------------------------------------

function GetChunkIdxAtStartOfLine(const pageMetrics: TPageTextMetrics;
  lineIdx: integer): integer;
begin
  if (lineIdx < 0) or (lineIdx >= pageMetrics.totalLines) then
    Raise Exception.Create(rsPageTextMetricsError);
  Result := pageMetrics.startOfLineIdx[lineIdx];
end;
//------------------------------------------------------------------------------

function IsCorrectPosOrder(const pt1, pt2: TPoint): Boolean;
begin
  if pt1.X <> pt2.X then
  begin
    Result := pt1.X < pt2.X;
  end else
  begin
    Result := pt1.Y <= pt2.Y;
  end;
end;

//------------------------------------------------------------------------------
// TTextChunk class
//------------------------------------------------------------------------------

constructor TTextChunk.Create(owner: TChunkedText; const chunk: UnicodeString;
  index: integer; fontCache: TFontCache; fontColor, backColor: TColor32);
var
  i, listCnt: integer;
  w: double;
begin
  if chunk = '' then
    raise Exception.Create(rsInvalidChunk);

  Self.owner := owner;
  listCnt := owner.fList.Count;
  if index < 0 then index := 0
  else if index > listCnt then index := listCnt;

  self.index      := index;
  self.ftext      := chunk;
  self.fontColor  := fontColor;
  self.backColor  := backColor;
  self.fFontCache := fontCache;
  self.length     := System.Length(chunk);

  if Assigned(fontCache) then
  begin
    self.height := fontCache.LineHeight;
    self.ascent := fontCache.Ascent;
    if chunk[1] > SPACE then
    begin
      // relatively slow
      TFontCacheHack(fontCache).GetTextOutlineInternal(0,0,
        chunk, 0, self.arrayOfPaths, self.glyphOffsets, self.width)
    end else
    begin
      // fast
      w := fontCache.GetSpaceWidth;
      NewDoubleArray(self.glyphOffsets, length +1, true);
      glyphOffsets[0] := 0;
      for i := 1 to length do glyphOffsets[i] := w * i;
      self.width := w * length;
    end;
  end else
  begin
    self.arrayOfPaths := nil;
    SetLength(self.glyphOffsets, 1);
    self.glyphOffsets[0] := 0;
    self.width := 0;
    self.height := 0;
    self.ascent := 0;
  end;

  owner.fList.Insert(index, self);
  // reindex any trailing chunks
  if index < listCnt then
    for i := index +1 to listCnt do
      TTextChunk(owner.fList[i]).index := i;
end;
//------------------------------------------------------------------------------

function TTextChunk.IsBlankSpaces: Boolean;
begin
  Result := (text[1] = SPACE);
end;
//------------------------------------------------------------------------------

function TTextChunk.IsNewline: Boolean;
begin
  Result := text[1] = NEWLINE;
end;
//------------------------------------------------------------------------------

function TTextChunk.IsText: Boolean;
begin
  // CharInSet is slow in Win32 and generates even slower code for Win64
  //Result := not CharInSet(text[1], [SPACE, NEWLINE]);
  case text[1] of
    SPACE, NEWLINE:
      Result := False;
  else
    Result := True;
  end;
end;
//------------------------------------------------------------------------------

function TTextChunk.IsFirst: Boolean;
begin
  Result := index = 0;
end;
//------------------------------------------------------------------------------

function TTextChunk.IsLast: Boolean;
begin
  Result := index = owner.Count -1;
end;
//------------------------------------------------------------------------------

function TTextChunk.Next: TTextChunk;
begin
  if IsLast then
    Result := nil else
    Result := TTextChunk(owner.fList[index +1]);
end;
//------------------------------------------------------------------------------

function TTextChunk.Prev: TTextChunk;
begin
  if IsFirst then
    Result := nil else
    Result := TTextChunk(owner.fList[index -1]);
end;

//------------------------------------------------------------------------------
// TChunkedText
//------------------------------------------------------------------------------

constructor TChunkedText.Create;
begin
  inherited;
{$IFDEF XPLAT_GENERICS}
  fList := TList<TTextChunk>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

constructor TChunkedText.Create(const text: string; font: TFontCache;
  fontColor: TColor32; backColor: TColor32);
begin
  Create;
  SetText(text, font, fontColor, backColor);
end;
//------------------------------------------------------------------------------

destructor TChunkedText.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetChunk(index: integer): TTextChunk;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsChunkedTextRangeError);
  Result :=  TTextChunk(fList.Items[index]);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetSubText(const startPos, endPos: TPoint): UnicodeString;
var
  i: integer;
  sp, ep: TPoint;
begin
  Result := '';
  if Count = 0 then Exit;
  if startPos.X < 0 then sp := NullPoint else sp := startPos;
  if endPos.X >= Count then
    ep := Types.Point(LastChunk.index, LastChunk.length) else
    ep := endPos;
  if (ep.X = sp.X) then
  begin
    if (ep.Y <= sp.Y) then Exit;
    Result := Copy(Chunk[sp.X].text, sp.y +1, ep.y - sp.Y);
  end else
  begin
    Result := Copy(Chunk[sp.X].text, sp.y +1, MaxInt);
    for i := sp.X +1 to ep.X -1 do
      Result := Result + Chunk[i].text;
    Result := Result + Copy(Chunk[ep.X].text, 1, ep.Y);
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetText: UnicodeString;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count -1 do
    Result := Result + TTextChunk(fList.Items[i]).text;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.AddNewline(font: TFontCache);
begin
  InsertTextChunk(font, MaxInt, #10, clNone32);
end;
//------------------------------------------------------------------------------

procedure TChunkedText.AddSpaces(spaceCnt: integer; font: TFontCache);
var
  i: integer;
  spaces: UnicodeString;
begin
  if not Assigned(font) or not font.IsValidFont then
    raise Exception.Create(rsChunkedTextFontError);
  SetLength(spaces, spaceCnt);
  for i := 1 to spaceCnt do spaces[i] := SPACE;
  InsertTextChunk(font, MaxInt, spaces, clNone32);
end;
//------------------------------------------------------------------------------

function TChunkedText.AddTextChunk(font: TFontCache; const chunk: UnicodeString;
  fontColor: TColor32; backColor: TColor32): TTextChunk;
begin
  Result := InsertTextChunk(font, MaxInt, chunk, fontColor, backColor);
end;
//------------------------------------------------------------------------------

function TChunkedText.InsertTextChunk(font: TFontCache; index: integer;
  const chunk: UnicodeString; fontColor: TColor32;
  backColor: TColor32): TTextChunk;
begin
  Result := TTextChunk.Create(self, chunk, index, font, fontColor, backColor);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetCount: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
      TTextChunk(fList.Items[i]).Free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

function TChunkedText.FirstChunk: TTextChunk;
begin
  if fList.Count = 0 then
    Result := nil else
    Result := Chunk[0];
end;
//------------------------------------------------------------------------------

function TChunkedText.LastChunk: TTextChunk;
begin
  if fList.Count = 0 then
    Result := nil else
    Result := Chunk[fList.Count -1];
end;
//------------------------------------------------------------------------------

function TChunkedText.GetPrevChar(var chunkPos: TPoint): Boolean;
begin
  if (chunkPos.X < 0) or (chunkPos.X > fList.Count) then
    Result := false
  else if (chunkPos.X = fList.Count) then
  begin
    Dec(chunkPos.X);
    chunkPos.Y := Chunk[chunkPos.X].length -1;
    Result := true;
  end
  else if chunkPos.Y > 0 then
  begin
    dec(chunkPos.Y);
    Result := chunkPos.Y < Chunk[chunkPos.X].length;
  end
  else if chunkPos.X = 0 then
    Result := false
  else
  begin
    dec(chunkPos.X);
    chunkPos.Y := Chunk[chunkPos.X].length -1;
    Result := true;
  end;
end;
//------------------------------------------------------------------------------
function TChunkedText.GetPrevWord(var chunkPos: TPoint): Boolean;
begin
  if (chunkPos.X < 0) or (chunkPos.X > fList.Count) then
    Result := false
  else if (chunkPos.X = fList.Count) then
  begin
    Dec(chunkPos.X);
    chunkPos.Y := 0;
    if (chunkPos.X > 0) and Chunk[chunkPos.X].IsBlankSpaces then
      dec(chunkPos.X);
    Result := true;
  end
  else if (chunkPos.Y = 0) and (chunkPos.X > 0) then
  begin
    dec(chunkPos.X);
    if (chunkPos.X > 0) and Chunk[chunkPos.X].IsBlankSpaces then
      dec(chunkPos.X);
    Result := true;
  end else if chunkPos.Y > 0 then
  begin
    chunkPos.Y := 0;
    Result := true;
  end
  else Result := false
end;
//------------------------------------------------------------------------------

function TChunkedText.GetNextChar(var chunkPos: TPoint): Boolean;
begin
  if (chunkPos.X < 0) or (chunkPos.X >= fList.Count) then
    Result := false
  else if chunkPos.Y < Chunk[chunkPos.X].length -1 then
  begin
    inc(chunkPos.Y);
    Result := true;
  end
  else if chunkPos.X = fList.Count then
    Result := false
  else
  begin
    inc(chunkPos.X);
    chunkPos.Y := 0;
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetNextWord(var chunkPos: TPoint): Boolean;
begin
  if (chunkPos.X < 0) or (chunkPos.X >= fList.Count) then
    Result := false
  else
  begin
    Result := true;
    inc(chunkPos.X);
    chunkPos.Y := 0;
    if (chunkPos.X < fList.Count) and
      Chunk[chunkPos.X].IsBlankSpaces then
        inc(chunkPos.X);
  end;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.DeleteChunk(Index: Integer);
var
  i: integer;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsChunkedTextRangeError);
  TTextChunk(fList.Items[index]).Free;
  fList.Delete(index);
  // reindex
  for i := Index to fList.Count -1 do
    TTextChunk(fList.Items[i]).index := i;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.DeleteChunkRange(startIdx, endIdx: Integer);
var
  i, cnt: Integer;
begin
  cnt := endIdx - startIdx +1;
  if (startIdx < 0) or (endIdx >= fList.Count) or (cnt <= 0) then
    raise Exception.Create(rsChunkedTextRangeError);

  for i := startIdx to endIdx do
    TTextChunk(fList.Items[i]).Free;

{$IF defined(FPC) or not defined(LIST_DELETERANGE)}
  for i := endIdx-startIdx +1 downto startIdx do
    fList.Delete(i);
{$ELSE}
  fList.DeleteRange(startIdx, endIdx-startIdx +1);
{$IFEND}

  // reindex
  for i := startIdx to fList.Count -1 do
    TTextChunk(fList.Items[i]).index := i;
end;
//------------------------------------------------------------------------------

function TChunkedText.DeleteSubchunk(chunk: TTextChunk;
  y1, y2: integer): TTextChunk;
var
  idx     : integer;
  newWord : UnicodeString;
  fc      : TFontCache;
  cTx,cBk : TColor32;
begin
  // preconditions: ranges checked and y1 <= y2
  if y1 = y2 then
  begin
    Result := chunk;
    Exit;
  end;

  with chunk do
  begin
    y2 := Min(length, y2);
    newWord := text;
    fc      := fFontCache;
    cTx     := fontColor;
    cBk     := backColor;
    idx     := index;
  end;
  System.Delete(newWord, y1 +1, y2 - y1);
  DeleteChunk(idx);
  if newWord = '' then
    Result := nil else
    Result := InsertTextChunk(fc, idx, newWord, cTx, cBk);
end;
//------------------------------------------------------------------------------

function TChunkedText.Delete(const startPos, endPos: TPoint): TPoint;
var
  i             : integer;
  sp, ep        : TPoint;
  chnk1, chnk2  : TTextChunk;
  mergedText    : UnicodeString;
  font          : TFontCache;
  fontColor     : TColor32;
  bkColor       : TColor32;
begin
  if Count = 0 then
  begin
    Result := NullPoint;
    Exit;
  end;

  if (startPos.X < endPos.X) or
    ((startPos.X = endPos.X) and (startPos.Y < endPos.Y)) then
  begin
    sp := startPos; ep := endPos;
  end else
  begin
    sp := endPos; ep := startPos;
  end;

  if (ep.X >= Count) then
  begin
    ep.X := Count -1;
    chnk2 := Chunk[ep.X];
    ep.Y := chnk2.length;
  end;

  Result := sp;
  if (Count = 0) or PointsEqual(sp, ep) or
    (sp.X >= Count) or (ep.X < 0) then Exit;

  chnk1 := Chunk[sp.X];
  if sp.X = ep.X then
  begin
    chnk1 := DeleteSubchunk(chnk1, sp.Y, ep.Y);
    if Assigned(chnk1) then Exit;
    chnk2 := nil;
  end else
  begin
    chnk2 := DeleteSubchunk(Chunk[ep.X], 0, ep.Y);
    for i := ep.X -1 downto sp.X +1 do
      DeleteChunk(i);
    chnk1 := DeleteSubchunk(chnk1, sp.Y, MaxInt);
  end;

  if not Assigned(chnk1) then
  begin
    if (sp.X = 0) or (sp.X = Count) then Exit;
    chnk1 := Chunk[sp.X -1];
    chnk2 := Chunk[sp.X];
    Result := Types.Point(chnk1.index, chnk1.length);
  end
  else if not Assigned(chnk2) then
  begin
    if (sp.X >= Count -1) then Exit;
    chnk2 := Chunk[sp.X +1];
  end;

  // MERGE ADJACENT CHUNKS AS LONG AS TEXT ISN'T MERGED
  // WITH EITHER SPACES OR NEWLINES

  if ((chnk1.IsText and chnk2.IsText) or
    (chnk1.IsBlankSpaces and chnk2.IsBlankSpaces)) then
  begin
    if //(chnk1.fFontCache <> chnk2.fFontCache) or
      (chnk1.backColor <> chnk2.backColor) or
      (chnk1.fontColor <> chnk2.fontColor) then Exit;

    font := chnk1.fFontCache;
    fontColor := chnk1.fontColor;
    bkColor := chnk1.backColor;
    mergedText := chnk1.text + chnk2.text;
    i := chnk1.index;
    DeleteChunk(chnk2.index);
    DeleteChunk(i);
    InsertTextChunk(font, i, mergedText, fontColor, bkColor);
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.ChunkPosToCharIdx(const pos: TPoint): integer;
var
  i: integer;
begin
  Result := 1; // CharIdx is base 1
  for i := 0 to pos.X -1 do
    inc(Result, GetChunk(i).length);
  inc(Result, pos.Y);
end;
//------------------------------------------------------------------------------

function TChunkedText.CharIdxToChunkPos(idx: integer): TPoint;
var
  len: integer;
begin
  Result := NullPoint;
  Dec(idx); // CharPos is base 0
  while (idx > 0) and (Result.X < Count) do
  begin
    len := GetChunk(Result.X).length;
    if len <= idx then Break;
    inc(Result.X);
    Dec(idx, len);
  end;
  Result.Y := idx;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.SetText(const text: UnicodeString;
  font: TFontCache; fontColor: TColor32; backColor: TColor32);
var
  len: integer;
  p, p2, pEnd: PWideChar;
  s: UnicodeString;
  spaceCnt: integer;
begin
  if not Assigned(font) then Exit;

  Clear;
  spaceCnt := 0;
  p := PWideChar(text);
  pEnd := p;
  Inc(pEnd, Length(text));
  while p < pEnd do
  begin
    if (p^ <= SPACE) then
    begin
      if (p^ = SPACE) then inc(spaceCnt)
      else if (p^ = #10) then AddNewline(font);
      inc(p);
    end else
    begin
      if spaceCnt > 0 then
      begin
        AddSpaces(spaceCnt, font);
        spaceCnt := 0;
      end;
      p2 := p;
      inc(p);
      while (p < pEnd) and (p^ > SPACE) do inc(p);
      len := p - p2;
      SetLength(s, len);
      Move(p2^, s[1], len * SizeOf(Char));
      AddTextChunk(font, s, fontColor, backColor);
    end;
  end;
  if spaceCnt > 0 then AddSpaces(spaceCnt, font);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetPageMetrics(const rec: TRect;
  lineHeightOverride: double): TPageTextMetrics;
begin
  Result := GetPartialPageMetrics(rec,
    NullPoint, Types.Point(Count, 0), lineHeightOverride);
end;
//------------------------------------------------------------------------------

function  TChunkedText.GetPartialPageMetrics(const rec: TRect;
  startingChunkPos, EndChunkPos: TPoint;
  lineHeightOverride: double): TPageTextMetrics;
var
  pageWidth, pageHeight : integer;
  lh, priorSplitWidth   : double;
  currentX              : double;
  arrayCnt, arrayCap      : integer;
  chunkIdxAtStartOfLine   : integer;
  pseudoIdxAtStartOfLine  : integer; // either a chunk index or a char. offset
  currentPos              : TPoint;
  mustStop                : Boolean;

  procedure SetResultLength(len: integer);
  begin
    SetLength(Result.startOfLineIdx, len);
    SetLength(Result.justifyDeltas, len);
    SetLength(Result.lineWidths, len);
  end;

  procedure CheckArrayCap;
  begin
    if arrayCnt < arrayCap then Exit;
    inc(arrayCap, 16);
    SetResultLength(arrayCap);
  end;

  procedure AddLine;
  var
    i, spcCnt, ChunkIdxAtEndOfLine: integer;
    x: double;
    chnk: TTextChunk;
  begin
    CheckArrayCap;
    ChunkIdxAtEndOfLine := currentPos.X-1;
    // ignore spaces at the end of lines (in case of left & right justify)
    while (ChunkIdxAtEndOfLine > chunkIdxAtStartOfLine) and
      (Chunk[ChunkIdxAtEndOfLine].IsBlankSpaces) do
        Dec(ChunkIdxAtEndOfLine);

    x := -priorSplitWidth; spcCnt := 0;
    for i := chunkIdxAtStartOfLine to ChunkIdxAtEndOfLine do
    begin
      chnk := Chunk[i];
      if chnk.IsBlankSpaces then inc(spcCnt, chnk.length);
      x := x + chnk.width;
    end;
    Result.lineWidths[arrayCnt] := x;
    Result.lineHeight := lh;
    Result.startOfLineIdx[arrayCnt] := pseudoIdxAtStartOfLine;

    if spcCnt = 0 then
      Result.justifyDeltas[arrayCnt] := 0 else
      Result.justifyDeltas[arrayCnt] := (pageWidth - x)/spcCnt;

    if (Result.visibleLines < 0) and ((arrayCnt +1) * lh > pageHeight) then
    begin
      Result.visibleLines := arrayCnt;
      Result.nextStartPos := Types.Point(chunkIdxAtStartOfLine, 0);
    end;

    inc(arrayCnt);
    currentX := 0;
    priorSplitWidth := 0;
  end;

  procedure AddPartChunk;
  var
    highI: integer;
    residualChunkWidth: double;
    chnk: TTextChunk;
  begin
    chnk := Chunk[chunkIdxAtStartOfLine];
    if currentPos.Y > 0 then pseudoIdxAtStartOfLine := -currentPos.Y;

    priorSplitWidth := chnk.glyphOffsets[currentPos.Y];
    highI := High(chnk.glyphOffsets) -1;
    residualChunkWidth := chnk.width - priorSplitWidth;
    while (highI >= currentPos.Y) and (residualChunkWidth > pageWidth) do
    begin
      residualChunkWidth := chnk.glyphOffsets[highI] - priorSplitWidth;
      Dec(highI);
    end;

    if (Result.visibleLines < 0) and ((arrayCnt +1) * lh > pageHeight) then
    begin
      Result.visibleLines := arrayCnt;
      Result.nextStartPos := Types.Point(chunkIdxAtStartOfLine, currentPos.Y);
    end;

    if highI < currentPos.Y then
    begin
      // oops, even a single character won't fit !!
      mustStop := true;
      currentPos.X := chunkIdxAtStartOfLine;
      Result.nextStartPos := Types.Point(chunkIdxAtStartOfLine, 0);
    end else
    begin
      if (highI = High(chnk.glyphOffsets) -1) then
      begin
        // the rest of the chunk fits in the line
        // and additional chunk may also fit
        currentX := residualChunkWidth;
        inc(currentPos.X);
        currentPos.Y := 0;
      end else
      begin
        // this chunk needs (possibly additional) wrapping into the next line
        CheckArrayCap;
        Result.lineWidths[arrayCnt] := residualChunkWidth;
        Result.lineHeight := lh;
        Result.justifyDeltas[arrayCnt] := 0;
        Result.startOfLineIdx[arrayCnt] := pseudoIdxAtStartOfLine;
        inc(arrayCnt);
        currentPos.Y := highI +1;
        AddPartChunk; // note recursion !!
      end;
    end;
  end;

var
  chnk: TTextChunk;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.bounds := rec;
  Result.startPos := startingChunkPos;
  Result.nextStartPos := startingChunkPos;

  arrayCnt := 0; arrayCap := 0;
  if (EndChunkPos.X >= Count) then EndChunkPos := Types.Point(Count, 0);
  if (startingChunkPos.X < 0) then startingChunkPos := NullPoint;
  if (Count = 0) or not IsCorrectPosOrder(startingChunkPos, EndChunkPos) then
  begin
    SetResultLength(1);
    Exit;
  end;

  Result.visibleLines := -1;
  lh := lineHeightOverride;
  RectWidthHeight(rec, pageWidth, pageHeight);
  currentPos := startingChunkPos;
  chunkIdxAtStartOfLine := currentPos.X;
  pseudoIdxAtStartOfLine := chunkIdxAtStartOfLine;

  mustStop := false;
  currentX := 0;
  if currentPos.Y > 0 then
  begin
    chnk := Chunk[currentPos.X];
    if currentPos.Y >= chnk.length then
      currentPos.Y := chnk.length;
    priorSplitWidth := chnk.glyphOffsets[startingChunkPos.Y];
    // start by finishing a partial text chunk
    AddPartChunk;
  end else
    priorSplitWidth := 0;

  while not mustStop and (currentPos.X < EndChunkPos.X) do
  begin
    chnk := Chunk[currentPos.X];
    if (lineHeightOverride = 0) and (chnk.height > lh) then
      lh := chnk.height;

    if (chnk.IsNewline) then
    begin
      AddLine;
      if arrayCnt > 0 then
        Result.justifyDeltas[arrayCnt-1] := 0;
      inc(currentPos.X);
      chunkIdxAtStartOfLine  := currentPos.X;
      pseudoIdxAtStartOfLine := currentPos.X;
    end
    else if (currentX + chnk.width > pageWidth) then
    begin
      if (currentPos.X = chunkIdxAtStartOfLine) then
      begin
        // a single chunk is too wide for 'pageWidth'
        AddPartChunk;
      end else
      begin
        AddLine;
        // don't allow spaces to wrap to the front of the following line

        while Assigned(chnk) and chnk.IsBlankSpaces and
          (chnk.index < EndChunkPos.X) do
            chnk := chnk.Next;
        if not Assigned(chnk) then
          currentPos.X := EndChunkPos.X else
          currentPos.X := chnk.index;
        chunkIdxAtStartOfLine  := currentPos.X;
        pseudoIdxAtStartOfLine := currentPos.X;
      end;
    end else
    begin
      currentX := currentX + chnk.width;
      inc(currentPos.X);
    end;
  end;

  if not mustStop and
    (currentPos.X > chunkIdxAtStartOfLine) then AddLine;

  Result.totalLines := arrayCnt;
  if Result.visibleLines < 0 then
  begin
    Result.visibleLines := Result.totalLines;
    Result.nextStartPos := Types.Point(Count, 0);
  end;

  SetResultLength(arrayCnt);
  if (arrayCnt > 0) and (currentPos.X = Count) then
    Result.justifyDeltas[arrayCnt-1] := 0;
end;
//------------------------------------------------------------------------------

function GetRealChunkPosFromLineIdx(const ptm: TPageTextMetrics;
  lineIdx: integer): TPoint;
var
  chrOffset: integer;
begin
  if lineIdx < 0 then Result := ptm.startPos
  else if lineIdx >= ptm.visibleLines then Result := ptm.nextStartPos
  else if ptm.startOfLineIdx[lineIdx] >= 0 then
    Result := Types.Point(ptm.startOfLineIdx[lineIdx], 0)
  else
  begin
    chrOffset := -ptm.startOfLineIdx[lineIdx];
    dec(lineIdx);
    while (lineIdx >= 0) and (ptm.startOfLineIdx[lineIdx] < 0) do
      dec(lineIdx);

    if (lineIdx < 0) then
      Result := Types.Point(ptm.startPos.X, chrOffset) else
      Result := Types.Point(ptm.startOfLineIdx[lineIdx], chrOffset)
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetChunkAndGlyphOffsetAtPt(const ptm: TPageTextMetrics;
  const pt: TPoint; out chunkPos: TPoint): Boolean;
var
  y: integer;
  x, x2: Double;
  chnk: TTextChunk;
  startPos, EndPos: TPoint;
begin
  Result := false;
  x := pt.X - ptm.bounds.Left;
  y := Trunc((pt.Y - ptm.bounds.Top - ptm.topLinePxOffset) / ptm.lineHeight);

  if (x < 0) or (x > ptm.bounds.right - ptm.bounds.Left) or
    (y < 0) or (y >= ptm.visibleLines) then Exit;

  startPos := GetRealChunkPosFromLineIdx(ptm, y);
  if startPos.Y > 0 then
  begin
    chnk := Chunk[startPos.X];
    x2 := x + chnk.glyphOffsets[startPos.Y];
  end else
    x2 := x;
  chunkPos.X := startPos.X;

  chnk := Chunk[chunkPos.X];
  // first, find the correct chunk
  endPos := GetRealChunkPosFromLineIdx(ptm, y +1);

  while chunkPos.X < EndPos.X do
  begin
    chnk := Chunk[chunkPos.X];
    if chnk.IsBlankSpaces then
    begin
      if x2 < chnk.width + ptm.justifyDeltas[y] then Break;
      x2 := x2 - chnk.width - (ptm.justifyDeltas[y] * chnk.length);
    end else
    begin
      if x2 < chnk.width then Break;
      x2 := x2 - chnk.width;
    end;
    inc(chunkPos.X);
  end;

  // now find the char offset
  if chnk.index > startPos.X then
    chunkPos.Y := 0 else
    chunkPos.Y := startPos.Y;
  while x2 >= chnk.glyphOffsets[chunkPos.Y + 1] do Inc(chunkPos.Y);

  if (chunkPos.X < EndPos.X) or
    ((chunkPos.X = EndPos.X) and (chunkPos.Y < EndPos.Y)) then
      Result := true;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetChunkRect(const ptm: TPageTextMetrics;
  chunkIdx: Integer): TRectD;
var
  i: integer;
  dx: double;
  pos: TPoint;
  chnk: TTextChunk;
begin
  Result := NullRectD;
  // currently this function returns a NullRect if the chunk is line-wrapped
  if (chunkIdx < ptm.startPos.X) or (chunkIdx >= ptm.nextStartPos.X) then
    Exit;

  i := 0;
  while i < ptm.visibleLines - 1 do
    if chunkIdx < ptm.startOfLineIdx[i + 1] then Break
    else inc(i);
  pos := GetRealChunkPosFromLineIdx(ptm, i);
  chnk := Chunk[pos.X];
  Result.Top := ptm.bounds.Top + ptm.topLinePxOffset + (ptm.lineHeight * i);
  Result.Bottom := Result.Top + ptm.lineHeight;
  Result.Left := ptm.bounds.Left;

  if (pos.Y > 0) then
  begin
    if (pos.X = chunkIdx) then Exit; // wrapped chuck
    Result.Left := Result.Left + chnk.width - chnk.glyphOffsets[pos.Y];
    chnk := chnk.Next;
  end;
  while (chnk.index < chunkIdx) do
  begin
    if chnk.IsBlankSpaces then
      dx := chnk.length * ptm.justifyDeltas[i] else
      dx := 0;
    Result.Left := Result.Left + chnk.width + dx;
    chnk := chnk.Next;
  end;
  Result.Right := Result.Left + chnk.width;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetGlyphsOrDrawInternal(image: TImage32; const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign; const startChunkPos: TPoint;
  lineHeight: double; out paths: TPathsD): TPageTextMetrics;
var
  i, highI, j,k, recWidth, recHeight, chrOff: integer;
  y, top, totalHeight, spcDx: double;
  consumedWidth: double;
  pp: TPathsD;
  currPos, endPos: TPoint;
  chnk: TTextChunk;
begin
  paths := nil;
  FillChar(Result, SizeOf(Result), 0);
  Result.bounds := rec;
  if Count = 0 then Exit;

  RectWidthHeight(rec, recWidth, recHeight);

  // LINE HEIGHTS ...............
  // Getting lineheights based on a given font's ascent and descent values
  // works well only when a single font is used. Unfortunately, when using
  // multiple fonts, line spacing becomes uneven and looks ugly.
  // An alternative approach is to measure the highest and lowest bounds of all
  // the glyphs in a line, and use these and a fixed inter line space
  // to derive variable line heights. But this approach also has problems,
  // especially when lines contain no glyphs, or when they only contain glyphs
  // with minimal heights (----------). So this too can look ugly.
  // A third approach, is to get the maximum of every lines' height and use
  // that value for every line. But this approach tends to produce undesirably
  // large line heights.
  // A fourth approach is to use the height of the very first text chunk.
  // And a final approach ia simply to use a user defined line height

  if lineHeight = 0 then
    lineHeight := Chunk[0].height;
  Result := GetPartialPageMetrics(rec,
    startChunkPos, Types.Point(Count, 0), lineHeight);
  if (Result.totalLines = 0) or (lineHeight > recHeight) then Exit;

  // only return glyphs for visible lines
  totalHeight := lineHeight * Result.visibleLines;

  currPos := startChunkPos;
  chnk := chunk[currPos.X];
  top := rec.Top + chnk.ascent;
  chrOff := currPos.Y;

  case textAlignV of
    tvaMiddle: y := top + (RecHeight - totalHeight) / 2 - 1;
    tvaBottom: y := rec.bottom - totalHeight + chnk.ascent;
    else y := top;
  end;

  Result.bounds := rec;
  Result.topLinePxOffset := Round(y - top);
  currPos := GetRealChunkPosFromLineIdx(Result, 0);

  // for each visible line
  highI := Result.visibleLines - 1;
  for i := 0 to highI do
  begin
    if i = highI then
      endPos := Result.nextStartPos else
      endPos := GetRealChunkPosFromLineIdx(Result, i + 1);

    if textAlign = taJustify then
      spcDx := Result.justifyDeltas[i] else
      spcDx := 0;

    chnk := chunk[currPos.X];
    if not assigned(chnk) then Break;
    pp := nil;
    consumedWidth := chnk.glyphOffsets[chrOff];
    while true do
    begin
      if chnk.IsNewline then
      begin
        consumedWidth := 0;
      end
      else if chnk.IsBlankSpaces then
      begin
        consumedWidth := consumedWidth - chnk.width - (spcDx * chnk.length);
      end else
      begin
        if (chnk.index = endPos.X) then
          k := endPos.Y -1 else
          k := High(chnk.arrayOfPaths);

        // nb: endPos.Y -1 above disregards "surrogate paired" characters
        //if chnk.length +1 > Length(chnk.glyphOffsets) then beep;

        for j := currPos.Y to k do
          AppendPath(pp, chnk.arrayOfPaths[j]);
        pp := TranslatePath(pp, rec.Left - consumedWidth, y);
        consumedWidth := consumedWidth - chnk.width;

        if Assigned(image) then
        begin
          if Assigned(fDrawChunkEvent) then
            fDrawChunkEvent(chnk, RectD(consumedWidth, y - chnk.ascent,
              rec.Left + consumedWidth, y - chnk.ascent + chnk.height));
          DrawPolygon(image, pp, frNonZero, chnk.fontColor);
          pp := nil;
        end
        else
          AppendPath(paths, pp);
      end;

      currPos := endPos;
      chrOff := endPos.Y;
      chnk := chnk.Next;
      if not assigned(chnk) or (chnk.index >= endPos.X) then Break;
    end;
    y := y + lineHeight;
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.DrawText(image: TImage32; const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign): TPageTextMetrics;
var
  dummy: TPathsD;
begin
  Result := GetGlyphsOrDrawInternal(image,
    rec, textAlign, textAlignV, NullPoint, 0, dummy);
end;
//------------------------------------------------------------------------------

function TChunkedText.DrawText(image: TImage32; const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign;
  const startChunkPos: TPoint; lineHeight: double): TPageTextMetrics;
var
  dummy: TPathsD;
begin
  Result := GetGlyphsOrDrawInternal(image,
    rec, textAlign, textAlignV, startChunkPos, lineHeight, dummy);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetTextGlyphs(const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign;
  const startChunkPos: TPoint; lineHeight: double = 0.0): TPathsD;
begin
  GetGlyphsOrDrawInternal(nil, rec, textAlign, textAlignV,
    startChunkPos, lineHeight, Result);
end;
//------------------------------------------------------------------------------

procedure TChunkedText.ApplyNewFont(font: TFontCache);
var
  i: integer;
begin
  if not Assigned(font) then Exit;
  for i := 0 to Count -1 do
    with Chunk[i] do
    begin
      TFontCacheHack(font).GetTextOutlineInternal(0,0,
        text, 0, arrayOfPaths, glyphOffsets, width);
      height := font.LineHeight;
      ascent := font.Ascent;
    end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
