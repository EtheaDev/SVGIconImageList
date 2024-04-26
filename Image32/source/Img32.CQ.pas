unit Img32.CQ;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.4                                                             *
* Date      :  10 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  Color reduction for TImage32                                    *
*           :  Uses Octree Color Quantization & Floyd / Steinberg Dithering    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Types, Math, Img32, Img32.Vector, Img32.Transform;

type

  TArrayOfArrayOfColor32  = array of TArrayOfColor32;
  TArrayOfArrayOfInteger  = array of TArrayOfInteger;
  TArrayOfPointer         = array of Pointer;
  TOctNode = class;
  PColFreqRec = ^TColFreqRec;
  TColFreqRec = record
    color   : TColor32;
    freq    : integer;
    node    : TOctNode;
  end;
  TArrayOfColFreq = array of TColFreqRec;

  TColFreqSortFunc = function (const p1, p2: TColFreqRec): Boolean;

  // Octree Color Quantization:
  // https://web.archive.org/web/20140605161956/
  // http://www.microsoft.com/msj/archive/S3F1.aspx

  TReduceType = (rtSimple, rtMedianCut, rtFrequency, rtMixed);

  TOctree = class;
  TOctNodes8 = array[0 .. 7] of TOctNode;

  TOctNode = class
    private
      Level     : integer;
      Count     : integer;
      Next      : TOctNode;
      Childs    : TOctNodes8;
      Redirects : TOctNodes8;
      TotalR    : Int64;
      TotalG    : Int64;
      TotalB    : Int64;
      palColor  : TColor32;
      procedure Add(color: TColor32);
      function  FindFirstLeaf: TOctNode;
      function  FindLastLeaf: TOctNode;
      function  FindNearestIndex(index: integer): integer;
      function  FindLeafNearestToIndex(index: integer): TOctNode;
      function  GetColor: TColor32;
      procedure GetNodeColor(var color: TColor32);
      function  GetIsLeaf: Boolean;
      property  IsLeaf : Boolean read GetIsLeaf;
    public
      constructor Create(aLevel: byte);
      destructor  Destroy; override;
  end;

  TOctree = class
    private
      fLeaves       : cardinal;
      fTop          : TOctNode;
      ColorPxlCnt   : integer;
      fReducible8   : TOctNodes8;
      fReduceType   : TReduceType;
      procedure   Delete(var node: TOctNode);
      procedure   Add(color: TColor32);
      procedure   GetNodeColor(var color: TColor32);
      procedure   GetTreePalette(out colors: TArrayOfColor32);
      function    ReduceOne: Boolean;
    protected
      function    BasicReduce(palSize: cardinal): TArrayOfColor32;
      procedure   FrequencyCut(palSize: integer; var cfArr: TArrayOfColFreq);
      procedure   MedianCut(palSize, arrLen: integer; var cfArr: TArrayOfColFreq);
    public
      constructor Create;
      destructor  Destroy; override;
      procedure   Reset;
      procedure   BuildTree(image: TImage32);
      procedure   ApplyPalette(image: TImage32);
      function    GetColorFreqArray: TArrayOfColFreq;
      property    ColorCount: cardinal read fLeaves;
      // PixelCount: = Sum( leaves[ 0 .. n-1 ].Count )
      // and semi-transparent pixels aren't counted
      property    PixelCount: integer read ColorPxlCnt;
  end;

function ReduceImage(image: TImage32; maxColors: Cardinal;
  useDithering: Boolean = true; reduceType: TReduceType = rtMedianCut): TArrayOfColor32;

function CreatePaletteOctree(image: TImage32; reduceType: TReduceType = rtMixed): TOctree;

{$IFDEF MSWINDOWS}
function CreateLogPalette(const palColors: TArrayOfColor32): TMaxLogPalette;
{$ENDIF}

// GetNearestPaletteColor:
// This function is relatively slow so be careful how you use it :).
function GetNearestPaletteColor(color: TColor32;
  const palette: TArrayOfColor32): TColor32;

// GetPaletteIndex: returns -1 if color not found
function GetPaletteIndex(color: TColor32;
  const palette: TArrayOfColor32): integer;

// GetIndexesIntoTrancatedPal: creates an array of indexes that can be used
// to reindex (assuming truncatedPal is not larger than originalPal).
// For each color in originalPal, the result contains the index of
// that color in the truncated palette or -1 if not found.
function GetIndexesIntoTrancatedPal(const originalPal,
  truncatedPal: TArrayOfColor32): TArrayOfInteger;

// GetColorDistance: returns Euclidean distance squared
function GetColorDistance(color1, color2: TColor32): integer;
{$IFDEF INLINE} inline; {$ENDIF}

function SortPaletteByLuminence(const pal: TArrayOfColor32): TArrayOfColor32;

//DrawPalette: Useful for debugging
procedure DrawPalette(image: TImage32; const palette: TArrayOfColor32);
//SavePalette: Also useful for debugging
procedure SavePalette(const filename: string; const palette: TArrayOfColor32);

//https://en.wikipedia.org/wiki/List_of_software_palettes
function BlackWhitePal: TArrayOfColor32;
function DefaultMacPal16: TArrayOfColor32;
function DefaultWinPal16: TArrayOfColor32;

var
  OpacityThreshold: byte = $80;

implementation

resourcestring
  rsTrimPalette   = 'TrimPalette: Invalid frequency length';
  rsTrimPalette2  = 'TrimPalette: Invalid value for ''newSize''.';
  rsGetPalette    = 'TOctree.GetPalette error';

type
  PARGBArray = ^TARGBArray;
  TARGBArray = array [0 .. $FFFFFF -1] of TARGB;

  //redefine TByteArray (see also TByteArray in System.SysUtils)
  TByteArray = array[0..MaxInt -1] of Byte;
  PByteArray = ^TByteArray;

const
  // LeafLevel: assuming a maximum 256 color palette,
  // it's very inefficient to use a LeafLevel > 5
  LeafLevel = 5;

  // UnassignedColor: can't be a 'real' color added to
  // the octtree, because it has no alpha value.
  UnassignedColor = $0;

  NullOctNodes8 : TOctNodes8 =
    (nil, nil, nil, nil, nil, nil, nil, nil);

  MonoPal2: array [0..1] of TColor32 = (
    $FF000000, $FFFFFFFF);

  macPal16: array [0 .. 15] of TColor32 = (
    $FF000000, $FF404040, $FF808080, $FFC0C0C0,
    $FF3A7190, $FF052C56, $FF126400, $FF14B71F,
    $FFEAAB02, $FFD40000, $FFA50046, $FF8408F2,
    $FF0608DD, $FF0264FF, $FF05F3FC, $FFFFFFFF);

  winPal16_int: array [0..15] of TColor32 = (
    $FF000000, $FF800000, $FF008000, $FF000080,
    $FF808000, $FF008080, $FF800080, $FF808080,
    $FFC0C0C0, $FFFF0000, $FF00FF00, $FF0000FF,
    $FFFFFF00, $FFFF00FF, $FF00FFFF, $FFFFFFFF);

//------------------------------------------------------------------------------
// Miscellaneous Octree functions
//------------------------------------------------------------------------------

// GetIndex: gets the color index for a given level in OctTree
function GetIndex(color: TColor32; level: byte): byte;
{$IFDEF INLINE} inline; {$ENDIF}
var
  argb: TARGB absolute color;
  mask: Byte;
begin
  mask := $80 shr level;
  result := 0;
  if (argb.R and mask) <> 0 then result := 4;
  if (argb.G and mask) <> 0 then result := result or 2;
  if (argb.B and mask) <> 0 then result := result or 1;
end;
//------------------------------------------------------------------------------

function GetColorDistance(color1, color2: TColor32): integer;
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  // see https://en.wikipedia.org/wiki/Color_difference
  if c2.R + c1.R < 256 then
    result := 2*Sqr(c2.R - c1.R) + 4*Sqr(c2.G - c1.G) + 3*Sqr(c2.B - c1.B) else
    result := 3*Sqr(c2.R - c1.R) + 4*Sqr(c2.G - c1.G) + 2*Sqr(c2.B - c1.B);
end;
//------------------------------------------------------------------------------

function GetNearestPaletteColorIndex(color: TColor32;
  const palette: TArrayOfColor32): integer;
var
  i, highI, distI, distJ: integer;
begin
  highI := High(palette);
  if (highI < 0) or (TARGB(color).A < OpacityThreshold) then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  distJ := GetColorDistance(color, palette[0]);
  if distJ = 0 then Exit;
  for i := 1 to highI do
  begin
    distI := GetColorDistance(color, palette[i]);
    if distI >= distJ then Continue;
    Result := i;
    if distI = 0 then Exit;
    distJ := distI;
  end;
end;
//------------------------------------------------------------------------------

function GetNearestPaletteColor(color: TColor32;
  const palette: TArrayOfColor32): TColor32;
var
  i: integer;
begin
  i := GetNearestPaletteColorIndex(color, palette);
  if i < 0 then Result := clNone32 else
  Result := palette[i];
end;
//------------------------------------------------------------------------------

function GetPaletteIndex(color: TColor32;
  const palette: TArrayOfColor32): integer;
var
  i: integer;
begin
  color := color and $FFFFFF;
  for i := 0 to High(palette) do
    if color = (palette[i] and $FFFFFF) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function GetIndexesIntoTrancatedPal(const originalPal, truncatedPal: TArrayOfColor32): TArrayOfInteger;
var
  i, len: integer;
begin
  len := Length(originalPal);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := GetPaletteIndex(originalPal[i], truncatedPal);
end;

//------------------------------------------------------------------------------
// Various sort functions
//------------------------------------------------------------------------------

function PalSortDescending(const cf1, cf2: TColFreqRec): Boolean;
begin
 result := (cf2.freq > cf1.freq);
end;
//------------------------------------------------------------------------------

function PalSortAscending(const cf1, cf2: TColFreqRec): Boolean;
begin
  result := (cf2.freq < cf1.freq);
end;
//------------------------------------------------------------------------------

function PalRedSorter(const cf1, cf2: TColFreqRec): Boolean;
begin
  result := TARGB(cf2.color).R > TARGB(cf1.color).R; // descending
end;
//------------------------------------------------------------------------------

function PalGreenSorter(const cf1, cf2: TColFreqRec): Boolean;
begin
  result := TARGB(cf2.color).G > TARGB(cf1.color).G; // descending
end;
//------------------------------------------------------------------------------

function PalBlueSorter(const cf1, cf2: TColFreqRec): Boolean;
begin
  result := TARGB(cf2.color).B > TARGB(cf1.color).B; // descending
end;
//------------------------------------------------------------------------------

procedure PaletteSort(var ptrArray: TArrayOfColFreq;
  l, r: Integer; sortFunc: TColFreqSortFunc);
var
  i,j: integer;
  P, T: TColFreqRec;
begin
  repeat
    i := l;
    j := r;
    P := ptrArray[(l + r) shr 1];
    repeat
      while sortFunc(P, ptrArray[i]) do Inc(i);
      while sortFunc(ptrArray[j], P) do Dec(j);
      if i <= j then
      begin
        T := ptrArray[i];
        ptrArray[i] := ptrArray[j];
        ptrArray[j] := T;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if l < j then PaletteSort(ptrArray, l, j, sortFunc);
    l := i;
  until i >= r;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function RoundDownNearestPower2(val: Cardinal): Cardinal;
begin
  if (val and (val - 1)) > 0 then
  begin
    Result := val or val shr 1;
    Result := Result or (Result shr 2);
    Result := Result or (Result shr 3);
    Result := Result or (Result shr 4);
    Result := Result or (Result shr 16);
    Result := Result - (Result shr 1);
  end else
    Result := val;
end;
//------------------------------------------------------------------------------

procedure MedianCutInternal(var cfArr: TArrayOfColFreq;
  var ints: TArrayOfInteger; var idx: integer; var sizeAdjust: cardinal;
  start, finish, level: cardinal);
var
  i, mid: cardinal;
  loR, loG, loB, hiR, hiG, hiB, midC: byte;
  rx, gx, bx: integer;
begin
  if sizeAdjust >= level then
  begin
    dec(sizeAdjust, level);
    level := level * 2;
  end;

  if (level = 1) or (start = finish) then
  begin
    if (level > 1) then inc(sizeAdjust, level -1);
    ints[idx] := start;
    inc(idx);
    Exit;
  end;
  level := level shr 1;

  loR := 255; loG := 255; loB := 255;
  hiR := 0; hiG := 0; hiB := 0;
  for i := start to finish do
  begin
    with TARGB(cfArr[i].color) do
    begin
      if R < loR then loR := R;
      if G < loG then loG := G;
      if B < loB then loB := B;
      if R > hiR then hiR := R;
      if G > hiG then hiG := G;
      if B > hiB then hiB := B;
    end;
  end;

  rx := Abs(loR - hiR);
  gx := Abs(loG - hiG);
  bx := Abs(loB - hiB);

  // sort the sub-array based on the color channel with greatest range
  mid := start +1;
  if (gx >= rx) and (gx >= bx) then
  begin
    PaletteSort(cfArr, start, finish, PalGreenSorter);
    midC := (loG + hiG) div 2;
    while (mid <> finish) and
      (TARGB(cfArr[mid].color).G > midC) do inc(mid);
  end
  else if (bx >= rx) then
  begin
    PaletteSort(cfArr, start, finish, PalBlueSorter);
    midC := (loB + hiB) div 2;
    while (mid <> finish) and
      (TARGB(cfArr[mid].color).B > midC) do inc(mid);
  end else
  begin
    PaletteSort(cfArr, start, finish, PalRedSorter);
    midC := (loR + hiR) div 2;
    while (mid <> finish) and
      (TARGB(cfArr[mid].color).R > midC) do inc(mid);
  end;
  MedianCutInternal(cfArr, ints, idx, sizeAdjust, start, mid-1, level);
  MedianCutInternal(cfArr, ints, idx, sizeAdjust, mid, finish, level);
end;

//------------------------------------------------------------------------------
// TOctNode methods
//------------------------------------------------------------------------------

constructor TOctNode.Create(aLevel: byte);
begin
  Level   := aLevel;
  Next    := nil;
  Childs  := NullOctNodes8;
  Redirects := NullOctNodes8;
  TotalR  := 0;
  TotalG  := 0;
  TotalB  := 0;
  Count   := 0;
  palColor := UnassignedColor;
end;
//------------------------------------------------------------------------------

destructor TOctNode.Destroy;
var
  i: integer;
begin
  for i:= 0 to 7 do Childs[i].Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

function TOctNode.GetIsLeaf: Boolean;
begin
  result := (Count > 0) or (Level = LeafLevel);
end;
//------------------------------------------------------------------------------

procedure TOctNode.Add(color: TColor32);
var
  argb: TARGB absolute color;
begin
  if Count = 0 then
  begin
    TotalR := argb.R;
    TotalG := argb.G;
    TotalB := argb.B;
  end;
  inc(Self.Count);
end;
//------------------------------------------------------------------------------

function TOctNode.FindFirstLeaf: TOctNode;
var
  i: integer;
begin
  if IsLeaf then
  begin
    Result := Self;
    Exit;
  end;

  for i := 0 to 7 do
    if Assigned(Childs[i]) then
    begin
      Result := Childs[i].FindFirstLeaf;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TOctNode.FindLastLeaf: TOctNode;
var
  i: integer;
begin
  if IsLeaf then
  begin
    Result := Self;
    Exit;
  end;
  for i := 7 downto 0 do
    if Assigned(Childs[i]) then
    begin
      Result := Childs[i].FindLastLeaf;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TOctNode.FindNearestIndex(index: integer): integer;
var
  i : integer;
begin
  for i := 1 to 7 do
  begin
    if (index + i < 8) and Assigned(Childs[index + i]) then
    begin
      Result := index + i;
      Exit;
    end else if (index - i >= 0) and Assigned(Childs[index - i]) then
    begin
      Result := index - i;
      Exit;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TOctNode.FindLeafNearestToIndex(index: integer): TOctNode;
var
  i : integer;
begin
  i := FindNearestIndex(index);
  if i < 0 then Result := nil
  else if i < index then Result := Childs[i].FindLastLeaf
  else Result := Childs[i].FindFirstLeaf;
end;
//------------------------------------------------------------------------------

procedure TOctNode.GetNodeColor(var color: TColor32);
var
  i: integer;
begin
  if not IsLeaf then
  begin
    i := GetIndex(color, level);
    if not Assigned(Childs[i]) then
    begin
      if not Assigned(Redirects[i]) then
        Redirects[i] := FindLeafNearestToIndex(i);
      color := Redirects[i].GetColor;
    end else
      Childs[i].GetNodeColor(color);
  end else
    color := GetColor;
end;
//------------------------------------------------------------------------------

function TOctNode.GetColor: TColor32;
var
  argb: TARGB absolute Result;
begin
  if palColor = UnassignedColor then
  begin
    argb.R := TotalR;
    argb.G := TotalG;
    argb.B := TotalB;
    argb.A := 255;
    palColor := Result;
  end else
  begin
    Result := palColor;
  end;

end;

//------------------------------------------------------------------------------
// TOctree methods
//------------------------------------------------------------------------------

constructor TOctree.Create;
begin
  fReduceType := rtMedianCut;
  fLeaves := 0;
  fTop := TOctNode.Create(0);
  fReducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

destructor TOctree.Destroy;
begin
  if Assigned(fTop) then Delete(fTop);
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TOctree.Delete(var node: TOctNode);
var
  i: integer;
begin
  for i := Low (node.Childs) to High (node.Childs) do
    if Assigned(node.Childs[i]) then
      Delete(node.Childs[i]);
  FreeAndNil(node);
end;
//------------------------------------------------------------------------------

procedure TOctree.Reset;
begin
  if Assigned(fTop) then Delete(fTop);
  fLeaves := 0;
  fTop := TOctNode.Create(0);
  fReducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

procedure TOctree.BuildTree(image: TImage32);
var
  i   : integer;
  pc  : PARGB;
begin
  Reset;
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height - 1 do
  begin
    //ignore transparent and semi-transparent colors
    if pc.A >= OpacityThreshold then Add(pc.Color);
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

type TImg32 = class(TImage32);

procedure TOctree.ApplyPalette(image: TImage32);
var
  i: integer;
  pc: PARGB;
begin
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height -1 do
  begin
    if pc.A < OpacityThreshold then
      pc.Color := clNone32 else
      fTop.GetNodeColor(pc.Color);
    inc(pc);
  end;
  TImg32(image).ResetColorCount;
end;
//------------------------------------------------------------------------------

function TOctree.ReduceOne: Boolean;
var
  lvl, i, childCnt: integer;
  node: TOctNode;
  wc: TWeightedColor;
begin
  //find the lowest level with a reducible node ...
  lvl := LeafLevel -1;
  while (lvl > 0) and not Assigned(fReducible8[lvl]) do Dec(lvl);

  //reduce the most recently added node at level 'i' ...
  node := fReducible8[lvl];

  Result := assigned(node);
  if not Result then Exit;

  fReducible8[lvl] := node.Next;
  node.Count := 0; childCnt    := 0;
  wc.Reset;
  //now merge the leaves into the parent node ...
  for i:= 0 to 7 do
    if Assigned (node.Childs[i]) then
    begin
      wc.Add(node.Childs[i].GetColor, node.Childs[i].Count);
      FreeAndNil(node.Childs[i]);
      inc(childCnt);
    end;
  node.Add(wc.Color);
  inc(node.Count, wc.Weight -1);
  Dec(fLeaves, childCnt -1);
end;
//------------------------------------------------------------------------------

procedure TOctree.MedianCut(palSize, arrLen: integer; var cfArr: TArrayOfColFreq);
var
  i,j       : integer;
  sizeAdjust: cardinal;
  idxArrLen : integer;
  idxArr    : TArrayOfInteger;
  wc        : TWeightedColor;
  cf        : PColFreqRec;
begin
  // precondition: palSize == 2^n
  SetLength(idxArr, palSize +1);
  idxArrLen := 0; sizeAdjust := 0;
  MedianCutInternal(cfArr, idxArr, idxArrLen, sizeAdjust, 0, arrLen -1, palSize);
  idxArr[idxArrLen] := arrLen;

  // get the average color of each 'bucket' and assign it to node.palColor
  for i := 0 to idxArrLen -1 do
  begin
    cf := @cfArr[idxArr[i]];
    wc.Reset;
    wc.Add(cf.color, cf.freq);
    for j := idxArr[i] +1 to idxArr[i+1] -1 do
    begin
      wc.Add(cfArr[j].color, cfArr[j].freq);
      inc(cf.freq, cfArr[j].freq);
      cfArr[j].freq := 0;
    end;
    cf.color := wc.Color; // re-weighted color
    for j := idxArr[i] to idxArr[i+1] -1 do
      cfArr[j].node.palColor := cf.color;
  end;

end;
//------------------------------------------------------------------------------

procedure TOctree.FrequencyCut(palSize: integer; var cfArr: TArrayOfColFreq);
var
  i,len : integer;
begin
  len := Length(cfArr); // total colors in octree
  if palSize >= len then Exit;
  PaletteSort(cfArr, 0, len -1, PalSortDescending);
  for i := palSize to len -1 do
    cfArr[i].freq := 0;
end;
//------------------------------------------------------------------------------

procedure AddColor(octree: TOctree; color: TColor32; var node: TOctNode);
var
  idx, level: integer;
  child: TOctNode;
begin
  idx := GetIndex(color, node.level);
  child := node.Childs[idx];
  if not Assigned(child) then
  begin
    level := node.Level +1;
    child := TOctNode.Create(level);
    node.Childs[idx] := child;
    if child.IsLeaf then
    begin
      child.Add(color);
      Inc(octree.fLeaves);
    end else
    begin
      child.Next  := octree.fReducible8[level];
      octree.fReducible8[level] := child;
      AddColor(octree, color, child);
    end;
  end
  else if child.IsLeaf then
    child.Add(color)
  else
    AddColor(octree, color, child);
end;
//------------------------------------------------------------------------------

procedure TOctree.Add(color: TColor32);
begin
  inc(ColorPxlCnt);
  AddColor(self, color, fTop);
end;
//------------------------------------------------------------------------------

procedure TOctree.GetNodeColor(var color: TColor32);
var
  a: TColor32;
begin
  a := color and $FF000000;
  fTop.GetNodeColor(color);
  color := (color and $FFFFFF) or a;
end;
//------------------------------------------------------------------------------

procedure TOctree.GetTreePalette(out colors: TArrayOfColor32);
var
  count: integer;

  procedure FillPalette(Node: TOctNode);
  var
    i: integer;
  begin
    if (Node.IsLeaf) then
    begin
      colors[Count] := Node.GetColor;
      Inc(Count);
    end else
    begin
      for i := 0 to 7 do
        if assigned(Node.Childs[i]) then
          FillPalette(Node.Childs[i]);
    end;
  end;

begin
  SetLength(colors, fLeaves);
  count := 0;
  FillPalette(fTop);
end;
//------------------------------------------------------------------------------

function TOctree.GetColorFreqArray: TArrayOfColFreq;
var
  count: integer;

  procedure FillPalette(Node: TOctNode);
  var
    i: integer;
  begin
    if (Node.IsLeaf) then
    begin
      Result[count].node := Node;
      Result[count].freq := Node.Count;
      Result[count].color := Node.GetColor;
      Inc(Count);
    end else
    begin
      for i := 0 to 7 do
        if assigned(Node.Childs[i]) then
          FillPalette(Node.Childs[i]);
    end;
  end;

begin
  SetLength(Result, fLeaves);
  count := 0;
  FillPalette(fTop);
end;
//------------------------------------------------------------------------------

function GetLuminescence(color: TColor32): integer;
var
  c: TARGB absolute color;
begin
  Result := (Max(c.B,Max(c.G,c.R)) + Min(c.B, Min(c.G,c.R))) shr 1;
end;
//------------------------------------------------------------------------------

function SortPaletteByLuminence(const pal: TArrayOfColor32): TArrayOfColor32;
var
  i,j, len: integer;
  cfArr   : TArrayOfColFreq;
begin
  len := Length(pal);
  SetLength(Result, len);
  SetLength(cfArr, len);
  if len = 0 then Exit;

  for i := 0 to len -1 do
  begin
    cfArr[i].color := pal[i];
    cfArr[i].freq := GetLuminescence(pal[i]);
  end;
  PaletteSort(cfArr, 0, len -1, PalSortAscending);
  //remove duplicates
  Result[0] := cfArr[0].color;
  j := 0;
  for i := 1 to len -1 do
    if cfArr[i].color <> Result[j] then
    begin
      inc(j);
      Result[j] := cfArr[i].color;
    end;
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function TOctree.BasicReduce(palSize: cardinal): TArrayOfColor32;
begin
  while (fLeaves > palSize) and ReduceOne do;
  GetTreePalette(Result);
end;

//------------------------------------------------------------------------------
// Floyd-Steinberg Dithering -
// see https://en.wikipedia.org/wiki/Floyd-Steinberg_dithering
//------------------------------------------------------------------------------

type
  TMulDibTable = array [-255 .. 255] of integer;

var
  Mul1Div16Table: TMulDibTable;
  Mul3Div16Table: TMulDibTable;
  Mul5Div16Table: TMulDibTable;
  Mul7Div16Table: TMulDibTable;

function ClampByte(val: integer): Byte;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  if val < 0 then Result:= 0
  else if val >= 255 then Result:= 255
  else Result:= val;
end;
//------------------------------------------------------------------------------

function GetPColor32(pc: PARGB; offset: integer): PARGB;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  result := PARGB(PByte(pc) + (offset * SizeOf(TColor32)));
end;
//------------------------------------------------------------------------------

procedure Dither(image: TImage32; octree: TOctree);
var
  X, Y, W         : Integer;
  qeR,qeG, qeB    : integer;
  qeRq,qeGq, qeBq : integer;
  oldC            : TARGB;
  newC            : PARGB;
  tmp             : TImage32;

  procedure AdjustPixel(pc: PARGB; r, g, b: integer);
  begin
    pc.R := ClampByte(pc.R + r);
    pc.G := ClampByte(pc.G + g);
    pc.B := ClampByte(pc.B + b);
    dec(qeRq, r); dec(qeGq, g); dec(qeBq, b);
  end;

begin
  tmp := TImage32.Create(image);
  try
    W := image.Width;
    newC := PARGB(image.PixelBase);
    for Y := 0 to image.Height-1 do
      for X := 0 to W -1 do
      begin
        if newC.A > 0 then
        begin
          oldC := newC^;
          octree.GetNodeColor(newC.Color);
          qeR := oldC.R - newC.R;
          qeG := oldC.G - newC.G;
          qeB := oldC.B - newC.B;
          qeRq := qeR; qeGq := qeG; qeBq := qeB;

          if X < image.Width-1 then
            AdjustPixel(GetPColor32(newC, +1),
              Mul7Div16Table[qeR], Mul7Div16Table[qeG], Mul7Div16Table[qeB]);

          if Y < image.Height -1 then
          begin
            if X > 0 then
              AdjustPixel(GetPColor32(newC, W-1),
                Mul3Div16Table[qeR], Mul3Div16Table[qeG], Mul3Div16Table[qeB]);

            AdjustPixel(GetPColor32(newC, W),
              Mul5Div16Table[qeR], Mul5Div16Table[qeG], Mul5Div16Table[qeB]);

            if X < W -1 then
              AdjustPixel(GetPColor32(newC, W +1),
                Mul1Div16Table[qeRq], Mul1Div16Table[qeGq], Mul1Div16Table[qeBq]);
          end;
        end;
        inc(newC);
      end;
  finally
    tmp.Free;
  end;
end;

//------------------------------------------------------------------------------
// CreatePalette...
//------------------------------------------------------------------------------

function CreatePaletteOctree(image: TImage32; reduceType: TReduceType): TOctree;
var
  i: integer;
  pc: PARGB;
begin
  Result := TOctree.Create;
  Result.fReduceType := reduceType;
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height - 1 do
  begin
    //ignore transparent and semi-transparent colors
    if pc.A >= OpacityThreshold then
      Result.Add(pc.Color);
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

function ReduceImage(image: TImage32; maxColors: Cardinal;
  useDithering: Boolean; reduceType: TReduceType): TArrayOfColor32;
var
  i, len  : integer;
  j       : cardinal;
  octree  : TOctree;
  pc      : PARGB;
  cfArr   : TArrayOfColFreq;
begin
  if MaxColors < 2 then
    MaxColors := 1 else
    MaxColors := RoundDownNearestPower2(MaxColors);

  octree := CreatePaletteOctree(image, reduceType);
  try
    if octree.fReduceType = rtSimple then
    begin
      Result := octree.BasicReduce(maxColors);
      octree.ApplyPalette(image);
      Exit;
    end;

    while (octree.ColorCount > 1024) and octree.ReduceOne do ;
    cfArr := octree.GetColorFreqArray;
    len := Length(cfArr); // total colors in octree

    case octree.fReduceType of
      rtMedianCut: // mostly :)
        begin
          octree.FrequencyCut(512, cfArr); // trim only very infrequent colors
          octree.MedianCut(maxColors, Min(len, 512), cfArr); // otherwise MC
        end;
      rtFrequency:
        octree.FrequencyCut(maxColors, cfArr); // trim just on frequency
      rtMixed:
        begin
          octree.FrequencyCut(maxColors * 4, cfArr);
          octree.MedianCut(maxColors, Min(len, maxColors *4), cfArr);
        end;
    end;

    // build result palette
    j := 0;
    SetLength(Result, maxColors);
    for i := 0 to len -1 do
    begin
      if cfArr[i].freq > 0 then
      begin
        Result[j] := cfArr[i].color;
        inc(j);
        if j = maxColors then break;
      end;
    end;
    Result := SortPaletteByLuminence(Result);

    // update all (residual) nodes with its closest palette color match
    for i := maxColors to len -1 do
      cfArr[i].node.palColor :=
        GetNearestPaletteColor(cfArr[i].node.GetColor, Result);

    if useDithering then
    begin
      Dither(image, octree);
    end else
    begin
      pc := PARGB(image.PixelBase);
      for i := 0 to image.Width * image.Height -1 do
      begin
        if pc.A < OpacityThreshold then
          pc.Color := clNone32 else
          octree.fTop.GetNodeColor(pc.Color);
        inc(pc);
      end;
    end;
  finally
    octree.Free;
  end;
  TImg32(image).ResetColorCount;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function CreateLogPalette(const palColors: TArrayOfColor32): TMaxLogPalette;
var
  i, len: integer;
begin
  len := Length(palColors);
  Result.palVersion := $300;
  Result.palNumEntries := len;
  for i := 0 to len -1 do
  begin
    Result.palPalEntry[0].peRed := TARGB(palColors[i]).R;
    Result.palPalEntry[0].peGreen := TARGB(palColors[i]).G;
    Result.palPalEntry[0].peBlue := TARGB(palColors[i]).B;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure DrawPalette(image: TImage32; const palette: TArrayOfColor32);
var
  i, w, h, len: integer;
  rec: TRect;
begin
  len := length(palette);
  if len < 16 then w := len else w := 16;
  h := (len +15) div 16;
  image.SetSize(w * 16, h * 16);
  rec := Img32.Vector.Rect(0,0,16,16);
  for i := 0 to len -1 do
  begin
    image.FillRect(rec, palette[i] or $FF000000);
    if (i + 1) mod w = 0 then
      TranslateRect(rec, -15 * w, 16) else
      TranslateRect(rec, 16, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure SavePalette(const filename: string; const palette: TArrayOfColor32);
var
  tmpImg: TImage32;
begin
  tmpImg := TImage32.Create;
  try
    DrawPalette(tmpImg, palette);
    tmpImg.SaveToFile(filename);
  finally
    tmpImg.Free;
  end;
end;
//------------------------------------------------------------------------------

function BlackWhitePal: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(MonoPal2);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := MonoPal2[i];
end;
//------------------------------------------------------------------------------

function DefaultMacPal16: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(macPal16);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := macPal16[i];
end;
//------------------------------------------------------------------------------

function DefaultWinPal16: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(winPal16_int);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := winPal16_int[i];
end;

//------------------------------------------------------------------------------
// Initialization functions
//------------------------------------------------------------------------------

procedure MakeDitherTables;
const
  OneDiv16   = 0.0625;
  ThreeDiv16 = 0.1875;
  FiveDiv16  = 0.3125;
  SevenDiv16 = 0.4375;
var
  i: Integer;
begin
  for i := -255 to 255 do
  begin
    Mul1Div16Table[i] := Round(i * OneDiv16);
    Mul3Div16Table[i] := Round(i * ThreeDiv16);
    Mul5Div16Table[i] := Round(i * FiveDiv16);
    Mul7Div16Table[i] := Round(i * SevenDiv16);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeDitherTables;

end.

