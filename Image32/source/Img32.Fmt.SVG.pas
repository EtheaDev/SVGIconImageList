unit Img32.Fmt.SVG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.3                                                             *
* Date      :  21 September 2021                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  SVG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults, {$ENDIF}
  Img32, Img32.Vector, Img32.SVG.Reader;

type
  TImageFormat_SVG = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CanCopyToClipboard: Boolean; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

  TSvgListObject = class
    xml           : string;
    name          : string;
  end;

  TSvgImageList32 = class
  private
    fReader : TSvgReader;
{$IFDEF XPLAT_GENERICS}
    fList: TList<TSvgListObject>;
{$ELSE}
    fList: TList;
{$ENDIF}
    fDefWidth  : integer;
    fDefHeight : integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Find(const aName: string): integer;
    procedure GetImage(index: integer; image: TImage32);
    procedure Add(const aName, xml: string);
    procedure AddFromFile(const aName, filename: string);
    procedure AddFromResource(const aName, resName: string; resType: PChar);
    procedure Insert(index: integer; const name, xml: string);
    procedure Move(currentIndex, newIndex: integer);
    procedure Delete(index: integer);
    property DefaultWidth: integer read fDefWidth write fDefWidth;
    property DefaultHeight: integer read fDefHeight write fDefHeight;
  end;

var
  defaultSvgWidth: integer = 800;
  defaultSvgHeight: integer = 600;

implementation

//------------------------------------------------------------------------------
// TSvgImageList32
//------------------------------------------------------------------------------

constructor TSvgImageList32.Create;
begin
  fReader := TSvgReader.Create;

{$IFDEF XPLAT_GENERICS}
  fList := TList<TSvgListObject>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TSvgImageList32.Destroy;
begin
  Clear;
  fList.Free;
  fReader.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgImageList32.Count: integer;
begin
  result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    TSvgListObject(fList[i]).Free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

function TSvgImageList32.Find(const aName: string): integer;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    with TSvgListObject(fList[i]) do
      if SameText(name, aName) then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.GetImage(index: integer; image: TImage32);
begin
  if not Assigned(image) or (index < 0) or (index >= count) then Exit;
  if image.IsEmpty then
   image.SetSize(fDefWidth, fDefHeight);
  with TSvgListObject(fList[index]) do
    fReader.LoadFromString(xml);
  fReader.DrawImage(image, true);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Add(const aName, xml: string);
begin
  Insert(count, aName, xml);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.AddFromFile(const aName, filename: string);
begin
  if not FileExists(filename) then Exit;
  with TStringList.Create do
  try
    LoadFromFile(filename);
    Self.Insert(Self.Count, aName, Text);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.AddFromResource(const aName, resName: string; resType: PChar);
var
  rs: TResourceStream;
  ansi: AnsiString;
begin
  rs := TResourceStream.Create(hInstance, resName, resType);
  try
    SetLength(ansi, rs.Size);
    rs.Read(ansi[1], rs.Size);
    Self.Insert(Self.Count, aName, string(ansi));
  finally
    rs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Insert(index: integer; const name, xml: string);
var
  lo: TSvgListObject;
begin
  if index < 0 then index := 0
  else if index > Count then index := Count;

  lo := TSvgListObject.Create;
  lo.name := name;
  lo.xml := xml;
  fList.Insert(index, lo);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Move(currentIndex, newIndex: integer);
begin
  fList.Move(currentIndex, newIndex);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Delete(index: integer);
begin
  TSvgListObject(fList[index]).Free;
  fList.Delete(index);
end;

//------------------------------------------------------------------------------
// Loading (reading) SVG images from file ...
//------------------------------------------------------------------------------

function TImageFormat_SVG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  r: TRectWH;
  w,h, sx,sy: double;
begin
  with TSvgReader.Create do
  try
    Result := LoadFromStream(stream);
    if not Result then Exit;
    r := GetViewbox(img32.Width, img32.Height);

    img32.BeginUpdate;
    try
      //if the current image's dimensions are larger than the
      //SVG's viewbox, then scale the SVG image up to fit
      if not r.IsEmpty then
      begin
        w := r.Width;
        h := r.Height;
        sx := img32.Width / w;
        sy := img32.Height / h;
        if sy < sx then sx := sy;
        if not(SameValue(sx, 1, 0.00001)) then
        begin
          w := w * sx;
          h := h * sx;
        end;
        img32.SetSize(Round(w), Round(h));
      end;

      if img32.IsEmpty then
        img32.SetSize(defaultSvgWidth, defaultSvgHeight);

      //draw the SVG image to fit inside the canvas
      DrawImage(img32, True);
    finally
      img32.EndUpdate;
    end;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) SVG images to file (not currently implemented) ...
//------------------------------------------------------------------------------

class function TImageFormat_SVG.IsValidImageStream(stream: TStream): Boolean;
var
  i, savedPos, len: integer;
  buff: array [1..1024] of AnsiChar;
begin
  Result := false;
  savedPos := stream.Position;
  len := Min(1024, stream.Size - savedPos);
  stream.Read(buff[1], len);
  stream.Position := savedPos;
  for i := 1 to len -4 do
  begin
    if buff[i] < #9 then Exit
    else if (buff[i] = '<') and
      (buff[i +1] = 's') and
      (buff[i +2] = 'v') and
      (buff[i +3] = 'g') then
    begin
      Result := true;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_SVG.SaveToStream(stream: TStream; img32: TImage32);
begin
  //not enabled
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanCopyToClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CopyToClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanPasteFromClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.PasteFromClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('SVG', TImageFormat_SVG, cpLow);

end.
