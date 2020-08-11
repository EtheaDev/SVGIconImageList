{******************************************************************}
{ Color helper unit                                                }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 26-04-2005                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005 Martin Walter                                 }
{                                                                  }
{ Thanks to:                                                       }
{ Carlo Barazzetta (Changed color ref)                             }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGColor;

interface

uses
  System.UITypes
  , System.Classes;

const
  SVG_MAX_COLORS = 147;

type
  TSVGColor = (
    inherit_color,
    aliceblue,
    antiquewhite,
    aqua,
    aquamarine,
    azure,
    beige,
    bisque,
    black,
    blanchedalmond,
    blue,
    blueviolet,
    brown,
    burlywood,
    cadetblue,
    chartreuse,
    chocolate,
    coral,
    cornflowerblue,
    cornsilk,
    crimson,
    cyan,
    darkblue,
    darkcyan,
    darkgoldenrod,
    darkgray,
    darkgreen,
    darkgrey,
    darkkhaki,
    darkmagenta,
    darkolivegreen,
    darkorange,
    darkorchid,
    darkred,
    darksalmon,
    darkseagreen,
    darkslateblue,
    darkslategray,
    darkslategrey,
    darkturquoise,
    darkviolet,
    deeppink,
    deepskyblue,
    dimgray,
    dimgrey,
    dodgerblue,
    firebrick,
    floralwhite,
    forestgreen,
    fuchsia,
    gainsboro,
    ghostwhite,
    gold,
    goldenrod,
    gray,
    grey,
    green,
    greenyellow,
    honeydew,
    hotpink,
    indianred,
    indigo,
    ivory,
    khaki,
    lavender,
    lavenderblush,
    lawngreen,
    lemonchiffon,
    lightblue,
    lightcoral,
    lightcyan,
    lightgoldenrodyellow,
    lightgray,
    lightgreen,
    lightgrey,
    lightpink,
    lightsalmon,
    lightseagreen,
    lightskyblue,
    lightslategray,
    lightslategrey,
    lightsteelblue,
    lightyellow,
    lime,
    limegreen,
    linen,
    magenta,
    maroon,
    mediumaquamarine,
    mediumblue,
    mediumorchid,
    mediumpurple,
    mediumseagreen,
    mediumslateblue,
    mediumspringgreen,
    mediumturquoise,
    mediumvioletred,
    midnightblue,
    mintcream,
    mistyrose,
    moccasin,
    navajowhite,
    navy,
    oldlace,
    olive,
    olivedrab,
    orange,
    orangered,
    orchid,
    palegoldenrod,
    palegreen,
    paleturquoise,
    palevioletred,
    papayawhip,
    peachpuff,
    peru,
    pink,
    plum,
    powderblue,
    purple,
    red,
    rosybrown,
    royalblue,
    saddlebrown,
    salmon,
    sandybrown,
    seagreen,
    seashell,
    sienna,
    silver,
    skyblue,
    slateblue,
    slategray,
    slategrey,
    snow,
    springgreen,
    steelblue,
    tan,
    teal,
    thistle,
    tomato,
    turquoise,
    violet,
    wheat,
    white,
    whitesmoke,
    yellow,
    yellowgreen
    );

  TSVGColorRef = record
    Name: string;
    SVGColor: TSVGColor;
    Color: TColor;
  end;

var
  SVGColors: array [0..SVG_MAX_COLORS] of TSVGColorRef;

function GetSVGColor(const ASVGColorName: string): TColor;
function GetSVGGrayscale(aColor : TColor) : TColor;
function SVGColorToColor(const ASVGColor: TSVGColor): TColor;
function SVGColorNameToSVGColor(const AColorName: string): TSVGColor;
function SVGColorToSVGColorName(const AColor: TSVGColor): string;
function ColorToSVGColor(const AColor: TColor): TSVGColor;
function ConvertColor(Color: TColor; Alpha: Byte): Cardinal; inline;
procedure AssignSVGColorList(AList: TStrings);

implementation

uses
  Winapi.Windows, Winapi.GDIPAPI, System.SysUtils, Vcl.Graphics, SVGTypes;

function IsHex(const S: string): Boolean;
var
  C: Integer;
  Help: string;
begin
  Result := False;
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  for C := 1 to Length(Help) do
    if not (((Help[C] >= '0') and (Help[C] <= '9')) or
            ((Help[C] >= 'A') and (Help[C] <= 'F')) or
            ((Help[C] >= 'a') and (Help[C] <= 'f'))) then
    Exit;

  Result := True;
end;

function IsDecimal(const S: string): Boolean;
var
  C: Integer;
begin
  Result := False;
  for C := 1 to Length(S) do
    if not ((S[C] >= '0') and (S[C] <= '9')) then
      Exit;
  Result := True;
end;

function DecodeSVGColorToInt(const S: string): Integer;
var
  C: Integer;
  Percent: Boolean;
  Help: string;
begin
  Result := INHERIT;
  Help := '0' + S;
  Percent := False;
  if Help[Length(Help)] = '%' then
  begin
    Help := Copy(Help, 1, Length(Help) - 1);
    Percent := True;
  end;

  C := INHERIT;
  if IsDecimal(Help) then
    C := StrToInt(Help)
  else
    if IsHex(Help) then
      C := StrToInt('$' + Help);
  if C = INHERIT then
    Exit;
  if C > 255 then
    C := 255;
  if Percent then
  begin
    if C > 100 then
      C := 100;
    C := Round(C * 2.55);
  end;
  Result := C;
end;

function DecodeRGB(const S: string): Integer;
var
  RS, GS, BS: string;
  RGB: string;
  R, B, G: Integer;
begin
  Result := INHERIT;
  if not ((Copy(S, 1, 4) = 'rgb(') and (S[Length(S)] = ')')) then
    Exit;

  RGB := Copy(S, 5, Length(S) - 5);
  RGB := Trim(RGB);

  RS := Copy(RGB, 1, Pos(',', RGB) - 1);
  RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
  RGB := Trim(RGB);

  GS := Copy(RGB, 1, Pos(',', RGB) - 1);
  RGB := Copy(RGB, Pos(',', RGB) + 1, Length(RGB));
  RGB := Trim(RGB);

  BS := RGB;

  R := DecodeSVGColorToInt(RS);
  G := DecodeSVGColorToInt(GS);
  B := DecodeSVGColorToInt(BS);

  if (R = -1) or (G = -1) or (B = -1) then
    Exit;

  Result := Winapi.Windows.RGB(R, G, B);
end;

function CharToInt(const Ch: Char): Integer;
begin
  Result := Ord(Ch);
  if (Result > 47) and (Result < 58) then
    Dec(Result, 48)
  else
    if (Result > 64) and (Result < 71) then
      Dec(Result, 55)
    else
      if (Result > 96) and (Result < 103) then
        Dec(Result, 87)
      else
        Result := 0;
end;

function PrepareHex(const S: string): string;
var
  C: Integer;
  Help: string;
begin
  if S[1] = '#' then
    Help := Copy(S, 2, Length(S))
  else
    Help := S;
  if Length(Help) > 6 then
    Help := Copy(Help, 1, 6);

  if Length(Help) = 3 then
  begin
    Help := IntToHex(CharToInt(Help[1]) * 17, 2) +
         IntToHex(CharToInt(Help[2]) * 17, 2) +
         IntToHex(CharToInt(Help[3]) * 17, 2);
  end;

  Result := '$';

  for C := 0 to 2 do
    Result := Result + Copy(Help, 5 - C * 2, 2);
end;

function GetSVGColor(const ASVGColorName: string): TColor;
var
  C: Integer;
  Color: string;
begin
  if ASVGColorName = '' then
  begin
    Result := INHERIT;
    Exit;
  end;
  if SameText(ASVGColorName, 'none') then
  begin
    Result := SVG_NONE_COLOR;
    Exit;
  end;
  for C := 0 to SVG_MAX_COLORS do
    if SameText(ASVGColorName,SVGColors[C].Name) then
    begin
      Result := SVGColors[C].Color;
      Exit;
    end;

  if IsHex(ASVGColorName) and (not IsDecimal(ASVGColorName)) then
    Color := PrepareHex(ASVGColorName)
  else
    Color := ASVGColorName;

  if TryStrToInt(Color, C) then
  begin
    Result := C;
  end
  else
  begin
    Result := DecodeRGB(Color);
  end;
end;

// Converts any color to grayscale
function GetSVGGrayscale(aColor : TColor) : TColor;
var
  LGray : byte;
begin
  // Ignore reserved color values : "INHERIT" (-1) and "none" (-2) .
  if (integer(aColor) = INHERIT) or (integer(aColor) = SVG_NONE_COLOR) then exit(aColor);

  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := round((0.299 * GetRValue(aColor)) + (0.587 * GetGValue(aColor)) + (0.114 * GetBValue(aColor)));

  // set the result to the new grayscale color including the alpha info
  Result := (aColor and $FF000000) or rgb(LGray, LGray, LGray);
end;

function SVGColorToColor(const ASVGColor: TSVGColor): TColor;
var
  C: Integer;
begin
  Result := clNone;
  for C := 0 to SVG_MAX_COLORS do
    if SVGColors[C].SVGColor = ASVGColor then
    begin
      Result := SVGColors[C].Color;
      break;
    end;
end;

function ColorToSVGColor(const AColor: TColor): TSVGColor;
var
  C: Integer;
begin
  Result := inherit_color;
  for C := 0 to SVG_MAX_COLORS do
    if SVGColors[C].Color = AColor then
    begin
      Result := SVGColors[C].SVGColor;
      break;
    end;
end;

function SVGColorNameToSVGColor(const AColorName: string): TSVGColor;
var
  C: Integer;
begin
  Result := inherit_color;
  for C := 0 to SVG_MAX_COLORS do
    if SameText(SVGColors[C].Name, AColorName) then
    begin
      Result := SVGColors[C].SVGColor;
      break;
    end;
end;

function SVGColorToSVGColorName(const AColor: TSVGColor): string;
var
  C: Integer;
begin
  for C := 0 to SVG_MAX_COLORS do
    if SVGColors[C].SVGColor = AColor then
    begin
      Result := SVGColors[C].Name;
      break;
    end;
end;

function ConvertColor(Color: TColor; Alpha: Byte): Cardinal;
begin
  with TColors(Color) do
    Result := Winapi.GDIPAPI.MakeColor(Alpha, R, G, B);
end;

procedure AssignSVGColorList(AList: TStrings);
var
  C: Integer;
begin
  AList.Clear;
  for C := 0 to SVG_MAX_COLORS do
    AList.Add(SVGColors[C].Name);
end;

begin
  SVGColors[000].Name := 'inherit';             SVGColors[000].SVGColor := inherit_color;       SVGColors[000].Color := INHERIT;
  SVGColors[001].Name := 'aliceblue';           SVGColors[001].SVGColor := aliceblue;           SVGColors[001].Color := rgb(240, 248, 255);
  SVGColors[002].Name := 'antiquewhite';        SVGColors[002].SVGColor := antiquewhite;        SVGColors[002].Color := rgb(250, 235, 215);
  SVGColors[003].Name := 'aqua';                SVGColors[003].SVGColor := aqua;                SVGColors[003].Color := rgb( 0, 255, 255);
  SVGColors[004].Name := 'aquamarine';          SVGColors[004].SVGColor := aquamarine;          SVGColors[004].Color := rgb(127, 255, 212);
  SVGColors[005].Name := 'azure';               SVGColors[005].SVGColor := azure;               SVGColors[005].Color := rgb(240, 255, 255);
  SVGColors[006].Name := 'beige';               SVGColors[006].SVGColor := beige;               SVGColors[006].Color := rgb(245, 245, 220);
  SVGColors[007].Name := 'bisque';              SVGColors[007].SVGColor := bisque;              SVGColors[007].Color := rgb(255, 228, 196);
  SVGColors[008].Name := 'black';               SVGColors[008].SVGColor := black;               SVGColors[008].Color := rgb( 0, 0, 0);
  SVGColors[009].Name := 'blanchedalmond';      SVGColors[009].SVGColor := blanchedalmond;      SVGColors[009].Color := rgb(255, 235, 205);
  SVGColors[010].Name := 'blue';                SVGColors[010].SVGColor := blue;                SVGColors[010].Color := rgb( 0, 0, 255);
  SVGColors[011].Name := 'blueviolet';          SVGColors[011].SVGColor := blueviolet;          SVGColors[011].Color := rgb(138, 43, 226);
  SVGColors[012].Name := 'brown';               SVGColors[012].SVGColor := brown;               SVGColors[012].Color := rgb(165, 42, 42);
  SVGColors[013].Name := 'burlywood';           SVGColors[013].SVGColor := burlywood;           SVGColors[013].Color := rgb(222, 184, 135);
  SVGColors[014].Name := 'cadetblue';           SVGColors[014].SVGColor := cadetblue;           SVGColors[014].Color := rgb( 95, 158, 160);
  SVGColors[015].Name := 'chartreuse';          SVGColors[015].SVGColor := chartreuse;          SVGColors[015].Color := rgb(127, 255, 0);
  SVGColors[016].Name := 'chocolate';           SVGColors[016].SVGColor := chocolate;           SVGColors[016].Color := rgb(210, 105, 30);
  SVGColors[017].Name := 'coral';               SVGColors[017].SVGColor := coral;               SVGColors[017].Color := rgb(255, 127, 80);
  SVGColors[018].Name := 'cornflowerblue';      SVGColors[018].SVGColor := cornflowerblue;      SVGColors[018].Color := rgb(100, 149, 237);
  SVGColors[019].Name := 'cornsilk';            SVGColors[019].SVGColor := cornsilk;            SVGColors[019].Color := rgb(255, 248, 220);
  SVGColors[020].Name := 'crimson';             SVGColors[020].SVGColor := crimson;             SVGColors[020].Color := rgb(220, 20, 60);
  SVGColors[021].Name := 'cyan';                SVGColors[021].SVGColor := cyan;                SVGColors[021].Color := rgb( 0, 255, 255);
  SVGColors[022].Name := 'darkblue';            SVGColors[022].SVGColor := darkblue;            SVGColors[022].Color := rgb( 0, 0, 139);
  SVGColors[023].Name := 'darkcyan';            SVGColors[023].SVGColor := darkcyan;            SVGColors[023].Color := rgb( 0, 139, 139);
  SVGColors[024].Name := 'darkgoldenrod';       SVGColors[024].SVGColor := darkgoldenrod;       SVGColors[024].Color := rgb(184, 134, 11);
  SVGColors[025].Name := 'darkgray';            SVGColors[025].SVGColor := darkgray;            SVGColors[025].Color := rgb(169, 169, 169);
  SVGColors[026].Name := 'darkgreen';           SVGColors[026].SVGColor := darkgreen;           SVGColors[026].Color := rgb( 0, 100, 0);
  SVGColors[027].Name := 'darkgrey';            SVGColors[027].SVGColor := darkgrey;            SVGColors[027].Color := rgb(169, 169, 169);
  SVGColors[028].Name := 'darkkhaki';           SVGColors[028].SVGColor := darkkhaki;           SVGColors[028].Color := rgb(189, 183, 107);
  SVGColors[029].Name := 'darkmagenta';         SVGColors[029].SVGColor := darkmagenta;         SVGColors[029].Color := rgb(139, 0, 139);
  SVGColors[030].Name := 'darkolivegreen';      SVGColors[030].SVGColor := darkolivegreen;      SVGColors[030].Color := rgb( 85, 107, 47);
  SVGColors[031].Name := 'darkorange';          SVGColors[031].SVGColor := darkorange;          SVGColors[031].Color := rgb(255, 140, 0);
  SVGColors[032].Name := 'darkorchid';          SVGColors[032].SVGColor := darkorchid;          SVGColors[032].Color := rgb(153, 50, 204);
  SVGColors[033].Name := 'darkred';             SVGColors[033].SVGColor := darkred;             SVGColors[033].Color := rgb(139, 0, 0);
  SVGColors[034].Name := 'darksalmon';          SVGColors[034].SVGColor := darksalmon;          SVGColors[034].Color := rgb(233, 150, 122);
  SVGColors[035].Name := 'darkseagreen';        SVGColors[035].SVGColor := darkseagreen;        SVGColors[035].Color := rgb(143, 188, 143);
  SVGColors[036].Name := 'darkslateblue';       SVGColors[036].SVGColor := darkslateblue;       SVGColors[036].Color := rgb( 72, 61, 139);
  SVGColors[037].Name := 'darkslategray';       SVGColors[037].SVGColor := darkslategray;       SVGColors[037].Color := rgb( 47, 79, 79);
  SVGColors[038].Name := 'darkslategrey';       SVGColors[038].SVGColor := darkslategrey;       SVGColors[038].Color := rgb( 47, 79, 79);
  SVGColors[039].Name := 'darkturquoise';       SVGColors[039].SVGColor := darkturquoise;       SVGColors[039].Color := rgb( 0, 206, 209);
  SVGColors[040].Name := 'darkviolet';          SVGColors[040].SVGColor := darkviolet;          SVGColors[040].Color := rgb(148, 0, 211);
  SVGColors[041].Name := 'deeppink';            SVGColors[041].SVGColor := deeppink;            SVGColors[041].Color := rgb(255, 20, 147);
  SVGColors[042].Name := 'deepskyblue';         SVGColors[042].SVGColor := deepskyblue;         SVGColors[042].Color := rgb( 0, 191, 255);
  SVGColors[043].Name := 'dimgray';             SVGColors[043].SVGColor := dimgray;             SVGColors[043].Color := rgb(105, 105, 105);
  SVGColors[044].Name := 'dimgrey';             SVGColors[044].SVGColor := dimgrey;             SVGColors[044].Color := rgb(105, 105, 105);
  SVGColors[045].Name := 'dodgerblue';          SVGColors[045].SVGColor := dodgerblue;          SVGColors[045].Color := rgb( 30, 144, 255);
  SVGColors[046].Name := 'firebrick';           SVGColors[046].SVGColor := firebrick;           SVGColors[046].Color := rgb(178, 34, 34);
  SVGColors[047].Name := 'floralwhite';         SVGColors[047].SVGColor := floralwhite;         SVGColors[047].Color := rgb(255, 250, 240);
  SVGColors[048].Name := 'forestgreen';         SVGColors[048].SVGColor := forestgreen;         SVGColors[048].Color := rgb( 34, 139, 34);
  SVGColors[049].Name := 'fuchsia';             SVGColors[049].SVGColor := fuchsia;             SVGColors[049].Color := rgb(255, 0, 255);
  SVGColors[050].Name := 'gainsboro';           SVGColors[050].SVGColor := gainsboro;           SVGColors[050].Color := rgb(220, 220, 220);
  SVGColors[051].Name := 'ghostwhite';          SVGColors[051].SVGColor := ghostwhite;          SVGColors[051].Color := rgb(248, 248, 255);
  SVGColors[052].Name := 'gold';                SVGColors[052].SVGColor := gold;                SVGColors[052].Color := rgb(255, 215, 0);
  SVGColors[053].Name := 'goldenrod';           SVGColors[053].SVGColor := goldenrod;           SVGColors[053].Color := rgb(218, 165, 32);
  SVGColors[054].Name := 'gray';                SVGColors[054].SVGColor := gray;                SVGColors[054].Color := rgb(128, 128, 128);
  SVGColors[055].Name := 'grey';                SVGColors[055].SVGColor := grey;                SVGColors[055].Color := rgb(128, 128, 128);
  SVGColors[056].Name := 'green';               SVGColors[056].SVGColor := green;               SVGColors[056].Color := rgb( 0, 128, 0);
  SVGColors[057].Name := 'greenyellow';         SVGColors[057].SVGColor := greenyellow;         SVGColors[057].Color := rgb(173, 255, 47);
  SVGColors[058].Name := 'honeydew';            SVGColors[058].SVGColor := honeydew;            SVGColors[058].Color := rgb(240, 255, 240);
  SVGColors[059].Name := 'hotpink';             SVGColors[059].SVGColor := hotpink;             SVGColors[059].Color := rgb(255, 105, 180);
  SVGColors[060].Name := 'indianred';           SVGColors[060].SVGColor := indianred;           SVGColors[060].Color := rgb(205, 92, 92);
  SVGColors[061].Name := 'indigo';              SVGColors[061].SVGColor := indigo;              SVGColors[061].Color := rgb( 75, 0, 130);
  SVGColors[062].Name := 'ivory';               SVGColors[062].SVGColor := ivory;               SVGColors[062].Color := rgb(255, 255, 240);
  SVGColors[063].Name := 'khaki';               SVGColors[063].SVGColor := khaki;               SVGColors[063].Color := rgb(240, 230, 140);
  SVGColors[064].Name := 'lavender';            SVGColors[064].SVGColor := lavender;            SVGColors[064].Color := rgb(230, 230, 250);
  SVGColors[065].Name := 'lavenderblush';       SVGColors[065].SVGColor := lavenderblush;       SVGColors[065].Color := rgb(255, 240, 245);
  SVGColors[066].Name := 'lawngreen';           SVGColors[066].SVGColor := lawngreen;           SVGColors[066].Color := rgb(124, 252, 0);
  SVGColors[067].Name := 'lemonchiffon';        SVGColors[067].SVGColor := lemonchiffon;        SVGColors[067].Color := rgb(255, 250, 205);
  SVGColors[068].Name := 'lightblue';           SVGColors[068].SVGColor := lightblue;           SVGColors[068].Color := rgb(173, 216, 230);
  SVGColors[069].Name := 'lightcoral';          SVGColors[069].SVGColor := lightcoral;          SVGColors[069].Color := rgb(240, 128, 128);
  SVGColors[070].Name := 'lightcyan';           SVGColors[070].SVGColor := lightcyan;           SVGColors[070].Color := rgb(224, 255, 255);
  SVGColors[071].Name := 'lightgoldenrodyellow';SVGColors[071].SVGColor := lightgoldenrodyellow;SVGColors[071].Color := rgb(250, 250, 210);
  SVGColors[072].Name := 'lightgray';           SVGColors[072].SVGColor := lightgray;           SVGColors[072].Color := rgb(211, 211, 211);
  SVGColors[073].Name := 'lightgreen';          SVGColors[073].SVGColor := lightgreen;          SVGColors[073].Color := rgb(144, 238, 144);
  SVGColors[074].Name := 'lightgrey';           SVGColors[074].SVGColor := lightgrey;           SVGColors[074].Color := rgb(211, 211, 211);
  SVGColors[075].Name := 'lightpink';           SVGColors[075].SVGColor := lightpink;           SVGColors[075].Color := rgb(255, 182, 193);
  SVGColors[076].Name := 'lightsalmon';         SVGColors[076].SVGColor := lightsalmon;         SVGColors[076].Color := rgb(255, 160, 122);
  SVGColors[077].Name := 'lightseagreen';       SVGColors[077].SVGColor := lightseagreen;       SVGColors[077].Color := rgb( 32, 178, 170);
  SVGColors[078].Name := 'lightskyblue';        SVGColors[078].SVGColor := lightskyblue;        SVGColors[078].Color := rgb(135, 206, 250);
  SVGColors[079].Name := 'lightslategray';      SVGColors[079].SVGColor := lightslategray;      SVGColors[079].Color := rgb(119, 136, 153);
  SVGColors[070].Name := 'lightslategrey';      SVGColors[070].SVGColor := lightslategrey;      SVGColors[070].Color := rgb(119, 136, 153);
  SVGColors[081].Name := 'lightsteelblue';      SVGColors[081].SVGColor := lightsteelblue;      SVGColors[081].Color := rgb(176, 196, 222);
  SVGColors[082].Name := 'lightyellow';         SVGColors[082].SVGColor := lightyellow;         SVGColors[082].Color := rgb(255, 255, 224);
  SVGColors[083].Name := 'lime';                SVGColors[083].SVGColor := lime;                SVGColors[083].Color := rgb( 0, 255, 0);
  SVGColors[084].Name := 'limegreen';           SVGColors[084].SVGColor := limegreen;           SVGColors[084].Color := rgb( 50, 205, 50);
  SVGColors[085].Name := 'linen';               SVGColors[085].SVGColor := linen;               SVGColors[085].Color := rgb(250, 240, 230);
  SVGColors[086].Name := 'magenta';             SVGColors[086].SVGColor := magenta;             SVGColors[086].Color := rgb(255, 0, 255);
  SVGColors[087].Name := 'maroon';              SVGColors[087].SVGColor := maroon;              SVGColors[087].Color := rgb(128, 0, 0);
  SVGColors[088].Name := 'mediumaquamarine';    SVGColors[088].SVGColor := mediumaquamarine;    SVGColors[088].Color := rgb(102, 205, 170);
  SVGColors[089].Name := 'mediumblue';          SVGColors[089].SVGColor := mediumblue;          SVGColors[089].Color := rgb( 0, 0, 205);
  SVGColors[080].Name := 'mediumorchid';        SVGColors[080].SVGColor := mediumorchid;        SVGColors[080].Color := rgb(186, 85, 211);
  SVGColors[091].Name := 'mediumpurple';        SVGColors[091].SVGColor := mediumpurple;        SVGColors[091].Color := rgb(147, 112, 219);
  SVGColors[092].Name := 'mediumseagreen';      SVGColors[092].SVGColor := mediumseagreen;      SVGColors[092].Color := rgb( 60, 179, 113);
  SVGColors[093].Name := 'mediumslateblue';     SVGColors[093].SVGColor := mediumslateblue;     SVGColors[093].Color := rgb(123, 104, 238);
  SVGColors[094].Name := 'mediumspringgreen';   SVGColors[094].SVGColor := mediumspringgreen;   SVGColors[094].Color := rgb( 0, 250, 154);
  SVGColors[095].Name := 'mediumturquoise';     SVGColors[095].SVGColor := mediumturquoise;     SVGColors[095].Color := rgb( 72, 209, 204);
  SVGColors[096].Name := 'mediumvioletred';     SVGColors[096].SVGColor := mediumvioletred;     SVGColors[096].Color := rgb(199, 21, 133);
  SVGColors[097].Name := 'midnightblue';        SVGColors[097].SVGColor := midnightblue;        SVGColors[097].Color := rgb( 25, 25, 112);
  SVGColors[098].Name := 'mintcream';           SVGColors[098].SVGColor := mintcream;           SVGColors[098].Color := rgb(245, 255, 250);
  SVGColors[099].Name := 'mistyrose';           SVGColors[099].SVGColor := mistyrose;           SVGColors[099].Color := rgb(255, 228, 225);
  SVGColors[090].Name := 'moccasin';            SVGColors[090].SVGColor := moccasin;            SVGColors[090].Color := rgb(255, 228, 181);
  SVGColors[101].Name := 'navajowhite';         SVGColors[101].SVGColor := navajowhite;         SVGColors[101].Color := rgb(255, 222, 173);
  SVGColors[102].Name := 'navy';                SVGColors[102].SVGColor := navy;                SVGColors[102].Color := rgb( 0, 0, 128);
  SVGColors[103].Name := 'oldlace';             SVGColors[103].SVGColor := oldlace;             SVGColors[103].Color := rgb(253, 245, 230);
  SVGColors[104].Name := 'olive';               SVGColors[104].SVGColor := olive;               SVGColors[104].Color := rgb(128, 128, 0);
  SVGColors[105].Name := 'olivedrab';           SVGColors[105].SVGColor := olivedrab;           SVGColors[105].Color := rgb(107, 142, 35);
  SVGColors[106].Name := 'orange';              SVGColors[106].SVGColor := orange;              SVGColors[106].Color := rgb(255, 165, 0);
  SVGColors[107].Name := 'orangered';           SVGColors[107].SVGColor := orangered;           SVGColors[107].Color := rgb(255, 69, 0);
  SVGColors[108].Name := 'orchid';              SVGColors[108].SVGColor := orchid;              SVGColors[108].Color := rgb(218, 112, 214);
  SVGColors[109].Name := 'palegoldenrod';       SVGColors[109].SVGColor := palegoldenrod;       SVGColors[109].Color := rgb(238, 232, 170);
  SVGColors[100].Name := 'palegreen';           SVGColors[100].SVGColor := palegreen;           SVGColors[100].Color := rgb(152, 251, 152);
  SVGColors[111].Name := 'paleturquoise';       SVGColors[111].SVGColor := paleturquoise;       SVGColors[111].Color := rgb(175, 238, 238);
  SVGColors[112].Name := 'palevioletred';       SVGColors[112].SVGColor := palevioletred;       SVGColors[112].Color := rgb(219, 112, 147);
  SVGColors[113].Name := 'papayawhip';          SVGColors[113].SVGColor := papayawhip;          SVGColors[113].Color := rgb(255, 239, 213);
  SVGColors[114].Name := 'peachpuff';           SVGColors[114].SVGColor := peachpuff;           SVGColors[114].Color := rgb(255, 218, 185);
  SVGColors[115].Name := 'peru';                SVGColors[115].SVGColor := peru;                SVGColors[115].Color := rgb(205, 133, 63);
  SVGColors[116].Name := 'pink';                SVGColors[116].SVGColor := pink;                SVGColors[116].Color := rgb(255, 192, 203);
  SVGColors[117].Name := 'plum';                SVGColors[117].SVGColor := plum;                SVGColors[117].Color := rgb(221, 160, 221);
  SVGColors[118].Name := 'powderblue';          SVGColors[118].SVGColor := powderblue;          SVGColors[118].Color := rgb(176, 224, 230);
  SVGColors[119].Name := 'purple';              SVGColors[119].SVGColor := purple;              SVGColors[119].Color := rgb(128, 0, 128);
  SVGColors[110].Name := 'red';                 SVGColors[110].SVGColor := red;                 SVGColors[110].Color := rgb(255, 0, 0);
  SVGColors[121].Name := 'rosybrown';           SVGColors[121].SVGColor := rosybrown;           SVGColors[121].Color := rgb(188, 143, 143);
  SVGColors[122].Name := 'royalblue';           SVGColors[122].SVGColor := royalblue;           SVGColors[122].Color := rgb( 65, 105, 225);
  SVGColors[123].Name := 'saddlebrown';         SVGColors[123].SVGColor := saddlebrown;         SVGColors[123].Color := rgb(139, 69, 19);
  SVGColors[124].Name := 'salmon';              SVGColors[124].SVGColor := salmon;              SVGColors[124].Color := rgb(250, 128, 114);
  SVGColors[125].Name := 'sandybrown';          SVGColors[125].SVGColor := sandybrown;          SVGColors[125].Color := rgb(244, 164, 96);
  SVGColors[126].Name := 'seagreen';            SVGColors[126].SVGColor := seagreen;            SVGColors[126].Color := rgb( 46, 139, 87);
  SVGColors[127].Name := 'seashell';            SVGColors[127].SVGColor := seashell;            SVGColors[127].Color := rgb(255, 245, 238);
  SVGColors[128].Name := 'sienna';              SVGColors[128].SVGColor := sienna;              SVGColors[128].Color := rgb(160, 82, 45);
  SVGColors[129].Name := 'silver';              SVGColors[129].SVGColor := silver;              SVGColors[129].Color := rgb(192, 192, 192);
  SVGColors[120].Name := 'skyblue';             SVGColors[120].SVGColor := skyblue;             SVGColors[120].Color := rgb(135, 206, 235);
  SVGColors[131].Name := 'slateblue';           SVGColors[131].SVGColor := slateblue;           SVGColors[131].Color := rgb(106, 90, 205);
  SVGColors[132].Name := 'slategray';           SVGColors[132].SVGColor := slategray;           SVGColors[132].Color := rgb(112, 128, 144);
  SVGColors[133].Name := 'slategrey';           SVGColors[133].SVGColor := slategrey;           SVGColors[133].Color := rgb(112, 128, 144);
  SVGColors[134].Name := 'snow';                SVGColors[134].SVGColor := snow;                SVGColors[134].Color := rgb(255, 250, 250);
  SVGColors[135].Name := 'springgreen';         SVGColors[135].SVGColor := springgreen;         SVGColors[135].Color := rgb( 0, 255, 127);
  SVGColors[136].Name := 'steelblue';           SVGColors[136].SVGColor := steelblue;           SVGColors[136].Color := rgb( 70, 130, 180);
  SVGColors[137].Name := 'tan';                 SVGColors[137].SVGColor := tan;                 SVGColors[137].Color := rgb(210, 180, 140);
  SVGColors[138].Name := 'teal';                SVGColors[138].SVGColor := teal;                SVGColors[138].Color := rgb( 0, 128, 128);
  SVGColors[139].Name := 'thistle';             SVGColors[139].SVGColor := thistle;             SVGColors[139].Color := rgb(216, 191, 216);
  SVGColors[140].Name := 'tomato';              SVGColors[140].SVGColor := tomato;              SVGColors[130].Color := rgb(255, 99, 71);
  SVGColors[141].Name := 'turquoise';           SVGColors[141].SVGColor := turquoise;           SVGColors[131].Color := rgb( 64, 224, 208);
  SVGColors[142].Name := 'violet';              SVGColors[142].SVGColor := violet;              SVGColors[132].Color := rgb(238, 130, 238);
  SVGColors[143].Name := 'wheat';               SVGColors[143].SVGColor := wheat;               SVGColors[143].Color := rgb(245, 222, 179);
  SVGColors[144].Name := 'white';               SVGColors[144].SVGColor := white;               SVGColors[144].Color := rgb(255, 255, 255);
  SVGColors[145].Name := 'whitesmoke';          SVGColors[145].SVGColor := whitesmoke;          SVGColors[145].Color := rgb(245, 245, 245);
  SVGColors[146].Name := 'yellow';              SVGColors[146].SVGColor := yellow;              SVGColors[146].Color := rgb(255, 255, 0);
  SVGColors[147].Name := 'yellowgreen';         SVGColors[147].SVGColor := yellowgreen;         SVGColors[147].Color := rgb(154, 205, 50);

end.
