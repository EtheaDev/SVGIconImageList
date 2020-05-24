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
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}

unit SVGColor;

interface

uses
  System.UITypes;

type
  TColorRef = record
    Name: string;
    Color: TColor;
  end;

var
  Colors: array [0..144] of TColorRef;

function GetColor(const S: string): TColor;

function ConvertColor(Color: TColor; Alpha: Byte): Cardinal;

implementation

uses
  Winapi.Windows, Winapi.GDIPAPI, System.SysUtils;

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

function DecodeToInt(const S: string): Integer;
var
  C: Integer;
  Percent: Boolean;
  Help: string;
begin
  Result := -1;
  Help := '0' + S;
  Percent := False;
  if Help[Length(Help)] = '%' then
  begin
    Help := Copy(Help, 1, Length(Help) - 1);
    Percent := True;
  end;

  C := -1;
  if IsDecimal(Help) then
    C := StrToInt(Help)
  else
    if IsHex(Help) then
      C := StrToInt('$' + Help);
  if C = -1 then
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
  Result := -1;
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

  R := DecodeToInt(RS);
  G := DecodeToInt(GS);
  B := DecodeToInt(BS);

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

function GetColor(const S: string): TColor;
var
  C: Integer;
  Color: string;
begin
  if S = '' then
  begin
    Result := -1;
    Exit;
  end;
  for C := 0 to 144 do
    if S = Colors[C].Name then
    begin
      Result := Colors[C].Color;
      Exit;
    end;

  if IsHex(S) and (not IsDecimal(S)) then
    Color := PrepareHex(S)
  else
    Color := S;

  if TryStrToInt(Color, C) then
  begin
    Result := C;
  end
  else
  begin
    Result := DecodeRGB(Color);
  end;
end;

function ConvertColor(Color: TColor; Alpha: Byte): Cardinal;
var
  R, G, B: Byte;
begin
  R := (Color and $000000FF);
  G := (Color and $0000FF00) shr 8;
  B := (Color and $00FF0000) shr 16;
  Result := MakeColor(Alpha, R, G, B);
end;


begin
  Colors[000].Name := 'none';
  Colors[000].Color := -2;

  Colors[001].Name := 'aliceblue';
  Colors[001].Color := rgb(240, 248, 255);

  Colors[002].Name := 'antiquewhite';
  Colors[002].Color := rgb(250, 235, 215);

  Colors[003].Name := 'aqua';
  Colors[003].Color := rgb( 0, 255, 255);

  Colors[004].Name := 'aquamarine';
  Colors[004].Color := rgb(127, 255, 212);

  Colors[005].Name := 'azure';
  Colors[005].Color := rgb(240, 255, 255);

  Colors[006].Name := 'beige';
  Colors[006].Color := rgb(245, 245, 220);

  Colors[007].Name := 'bisque';
  Colors[007].Color := rgb(255, 228, 196);

  Colors[008].Name := 'black';
  Colors[008].Color := rgb( 0, 0, 0);

  Colors[009].Name := 'blanchedalmond';
  Colors[009].Color := rgb(255, 235, 205);

  Colors[010].Name := 'blue';
  Colors[010].Color := rgb( 0, 0, 255);

  Colors[011].Name := 'blueviolet';
  Colors[011].Color := rgb(138, 43, 226);

  Colors[012].Name := 'brown';
  Colors[012].Color := rgb(165, 42, 42);

  Colors[013].Name := 'burlywood';
  Colors[013].Color := rgb(222, 184, 135);

  Colors[014].Name := 'cadetblue';
  Colors[014].Color := rgb( 95, 158, 160);

  Colors[015].Name := 'chartreuse';
  Colors[015].Color := rgb(127, 255, 0);

  Colors[016].Name := 'chocolate';
  Colors[016].Color := rgb(210, 105, 30);

  Colors[017].Name := 'coral';
  Colors[017].Color := rgb(255, 127, 80);

  Colors[018].Name := 'cornflowerblue';
  Colors[018].Color := rgb(100, 149, 237);

  Colors[019].Name := 'cornsilk';
  Colors[019].Color := rgb(255, 248, 220);

  Colors[020].Name := 'crimson';
  Colors[020].Color := rgb(220, 20, 60);

  Colors[021].Name := 'cyan';
  Colors[021].Color := rgb( 0, 255, 255);

  Colors[022].Name := 'darkblue';
  Colors[022].Color := rgb( 0, 0, 139);

  Colors[023].Name := 'darkcyan';
  Colors[023].Color := rgb( 0, 139, 139);

  Colors[024].Name := 'darkgoldenrod';
  Colors[024].Color := rgb(184, 134, 11);

  Colors[025].Name := 'darkgray';
  Colors[025].Color := rgb(169, 169, 169);

  Colors[026].Name := 'darkgreen';
  Colors[026].Color := rgb( 0, 100, 0);

  Colors[027].Name := 'darkgrey';
  Colors[027].Color := rgb(169, 169, 169);

  Colors[028].Name := 'darkkhaki';
  Colors[028].Color := rgb(189, 183, 107);

  Colors[029].Name := 'darkmagenta';
  Colors[029].Color := rgb(139, 0, 139);

  Colors[030].Name := 'darkolivegreen';
  Colors[030].Color := rgb( 85, 107, 47);

  Colors[031].Name := 'darkorange';
  Colors[031].Color := rgb(255, 140, 0);

  Colors[032].Name := 'darkorchid';
  Colors[032].Color := rgb(153, 50, 204);

  Colors[033].Name := 'darkred';
  Colors[033].Color := rgb(139, 0, 0);

  Colors[034].Name := 'darksalmon';
  Colors[034].Color := rgb(233, 150, 122);

  Colors[035].Name := 'darkseagreen';
  Colors[035].Color := rgb(143, 188, 143);

  Colors[036].Name := 'darkslateblue';
  Colors[036].Color := rgb( 72, 61, 139);

  Colors[037].Name := 'darkslategray';
  Colors[037].Color := rgb( 47, 79, 79);

  Colors[038].Name := 'darkslategrey';
  Colors[038].Color := rgb( 47, 79, 79);

  Colors[039].Name := 'darkturquoise';
  Colors[039].Color := rgb( 0, 206, 209);

  Colors[040].Name := 'darkviolet';
  Colors[040].Color := rgb(148, 0, 211);

  Colors[041].Name := 'deeppink';
  Colors[041].Color := rgb(255, 20, 147);

  Colors[042].Name := 'deepskyblue';
  Colors[042].Color := rgb( 0, 191, 255);

  Colors[043].Name := 'dimgray';
  Colors[043].Color := rgb(105, 105, 105);

  Colors[044].Name := 'dimgrey';
  Colors[044].Color := rgb(105, 105, 105);

  Colors[045].Name := 'dodgerblue';
  Colors[045].Color := rgb( 30, 144, 255);

  Colors[046].Name := 'firebrick';
  Colors[046].Color := rgb(178, 34, 34);

  Colors[047].Name := 'floralwhite';
  Colors[047].Color := rgb(255, 250, 240);

  Colors[048].Name := 'forestgreen';
  Colors[048].Color := rgb( 34, 139, 34);

  Colors[049].Name := 'fuchsia';
  Colors[049].Color := rgb(255, 0, 255);

  Colors[050].Name := 'gainsboro';
  Colors[050].Color := rgb(220, 220, 220);

  Colors[051].Name := 'ghostwhite';
  Colors[051].Color := rgb(248, 248, 255);

  Colors[052].Name := 'gold';
  Colors[052].Color := rgb(255, 215, 0);

  Colors[053].Name := 'goldenrod';
  Colors[053].Color := rgb(218, 165, 32);

  Colors[054].Name := 'gray';
  Colors[054].Color := rgb(128, 128, 128);

  Colors[055].Name := 'grey';
  Colors[055].Color := rgb(128, 128, 128);

  Colors[056].Name := 'green';
  Colors[056].Color := rgb( 0, 128, 0);

  Colors[057].Name := 'greenyellow';
  Colors[057].Color := rgb(173, 255, 47);

  Colors[058].Name := 'honeydew';
  Colors[058].Color := rgb(240, 255, 240);

  Colors[059].Name := 'hotpink';
  Colors[059].Color := rgb(255, 105, 180);

  Colors[060].Name := 'indianred';
  Colors[060].Color := rgb(205, 92, 92);

  Colors[061].Name := 'indigo';
  Colors[061].Color := rgb( 75, 0, 130);

  Colors[062].Name := 'ivory';
  Colors[062].Color := rgb(255, 255, 240);

  Colors[063].Name := 'khaki';
  Colors[063].Color := rgb(240, 230, 140);

  Colors[064].Name := 'lavender';
  Colors[064].Color := rgb(230, 230, 250);

  Colors[065].Name := 'lavenderblush';
  Colors[065].Color := rgb(255, 240, 245);

  Colors[066].Name := 'lawngreen';
  Colors[066].Color := rgb(124, 252, 0);

  Colors[067].Name := 'lemonchiffon';
  Colors[067].Color := rgb(255, 250, 205);

  Colors[068].Name := 'lightblue';
  Colors[068].Color := rgb(173, 216, 230);

  Colors[069].Name := 'lightcoral';
  Colors[069].Color := rgb(240, 128, 128);

  Colors[070].Name := 'lightcyan';
  Colors[070].Color := rgb(224, 255, 255);

  Colors[071].Name := 'lightgoldenrodyellow';
  Colors[071].Color := rgb(250, 250, 210);

  Colors[072].Name := 'lightgray';
  Colors[072].Color := rgb(211, 211, 211);

  Colors[073].Name := 'lightgreen';
  Colors[073].Color := rgb(144, 238, 144);

  Colors[074].Name := 'lightpink';
  Colors[074].Color := rgb(255, 182, 193);

  Colors[075].Name := 'lightsalmon';
  Colors[075].Color := rgb(255, 160, 122);

  Colors[076].Name := 'lightseagreen';
  Colors[076].Color := rgb( 32, 178, 170);

  Colors[077].Name := 'lightskyblue';
  Colors[077].Color := rgb(135, 206, 250);

  Colors[078].Name := 'lightslategray';
  Colors[078].Color := rgb(119, 136, 153);

  Colors[079].Name := 'lightslategrey';
  Colors[079].Color := rgb(119, 136, 153);

  Colors[080].Name := 'lightsteelblue';
  Colors[080].Color := rgb(176, 196, 222);

  Colors[081].Name := 'lightyellow';
  Colors[081].Color := rgb(255, 255, 224);

  Colors[082].Name := 'lime';
  Colors[082].Color := rgb( 0, 255, 0);

  Colors[083].Name := 'limegreen';
  Colors[083].Color := rgb( 50, 205, 50);

  Colors[084].Name := 'linen';
  Colors[084].Color := rgb(250, 240, 230);

  Colors[085].Name := 'magenta';
  Colors[085].Color := rgb(255, 0, 255);

  Colors[086].Name := 'maroon';
  Colors[086].Color := rgb(128, 0, 0);

  Colors[087].Name := 'mediumaquamarine';
  Colors[087].Color := rgb(102, 205, 170);

  Colors[088].Name := 'mediumblue';
  Colors[088].Color := rgb( 0, 0, 205);

  Colors[089].Name := 'mediumorchid';
  Colors[089].Color := rgb(186, 85, 211);

  Colors[090].Name := 'mediumpurple';
  Colors[090].Color := rgb(147, 112, 219);

  Colors[091].Name := 'mediumseagreen';
  Colors[091].Color := rgb( 60, 179, 113);

  Colors[092].Name := 'mediumslateblue';
  Colors[092].Color := rgb(123, 104, 238);

  Colors[093].Name := 'mediumspringgreen';
  Colors[093].Color := rgb( 0, 250, 154);

  Colors[094].Name := 'mediumturquoise';
  Colors[094].Color := rgb( 72, 209, 204);

  Colors[095].Name := 'mediumvioletred';
  Colors[095].Color := rgb(199, 21, 133);

  Colors[096].Name := 'midnightblue';
  Colors[096].Color := rgb( 25, 25, 112);

  Colors[097].Name := 'mintcream';
  Colors[097].Color := rgb(245, 255, 250);

  Colors[098].Name := 'mistyrose';
  Colors[098].Color := rgb(255, 228, 225);

  Colors[099].Name := 'moccasin';
  Colors[099].Color := rgb(255, 228, 181);

  Colors[100].Name := 'navajowhite';
  Colors[100].Color := rgb(255, 222, 173);

  Colors[101].Name := 'navy';
  Colors[101].Color := rgb( 0, 0, 128);

  Colors[102].Name := 'oldlace';
  Colors[102].Color := rgb(253, 245, 230);

  Colors[103].Name := 'olive';
  Colors[103].Color := rgb(128, 128, 0);

  Colors[104].Name := 'olivedrab';
  Colors[104].Color := rgb(107, 142, 35);

  Colors[105].Name := 'orange';
  Colors[105].Color := rgb(255, 165, 0);

  Colors[106].Name := 'orangered';
  Colors[106].Color := rgb(255, 69, 0);

  Colors[107].Name := 'orchid';
  Colors[107].Color := rgb(218, 112, 214);

  Colors[108].Name := 'palegoldenrod';
  Colors[108].Color := rgb(238, 232, 170);

  Colors[109].Name := 'palegreen';
  Colors[109].Color := rgb(152, 251, 152);

  Colors[110].Name := 'paleturquoise';
  Colors[110].Color := rgb(175, 238, 238);

  Colors[111].Name := 'palevioletred';
  Colors[111].Color := rgb(219, 112, 147);

  Colors[112].Name := 'papayawhip';
  Colors[112].Color := rgb(255, 239, 213);

  Colors[113].Name := 'peachpuff';
  Colors[113].Color := rgb(255, 218, 185);

  Colors[114].Name := 'peru';
  Colors[114].Color := rgb(205, 133, 63);

  Colors[115].Name := 'pink';
  Colors[115].Color := rgb(255, 192, 203);

  Colors[116].Name := 'plum';
  Colors[116].Color := rgb(221, 160, 221);

  Colors[117].Name := 'powderblue';
  Colors[117].Color := rgb(176, 224, 230);

  Colors[118].Name := 'purple';
  Colors[118].Color := rgb(128, 0, 128);

  Colors[119].Name := 'red';
  Colors[119].Color := rgb(255, 0, 0);

  Colors[120].Name := 'rosybrown';
  Colors[120].Color := rgb(188, 143, 143);

  Colors[121].Name := 'royalblue';
  Colors[121].Color := rgb( 65, 105, 225);

  Colors[122].Name := 'saddlebrown';
  Colors[122].Color := rgb(139, 69, 19);

  Colors[123].Name := 'salmon';
  Colors[123].Color := rgb(250, 128, 114);

  Colors[124].Name := 'sandybrown';
  Colors[124].Color := rgb(244, 164, 96);

  Colors[125].Name := 'seagreen';
  Colors[125].Color := rgb( 46, 139, 87);

  Colors[126].Name := 'seashell';
  Colors[126].Color := rgb(255, 245, 238);

  Colors[127].Name := 'sienna';
  Colors[127].Color := rgb(160, 82, 45);

  Colors[128].Name := 'silver';
  Colors[128].Color := rgb(192, 192, 192);

  Colors[129].Name := 'skyblue';
  Colors[129].Color := rgb(135, 206, 235);

  Colors[130].Name := 'slateblue';
  Colors[130].Color := rgb(106, 90, 205);

  Colors[131].Name := 'slategray';
  Colors[131].Color := rgb(112, 128, 144);

  Colors[132].Name := 'springgreen';
  Colors[132].Color := rgb( 0, 255, 127);

  Colors[133].Name := 'steelblue';
  Colors[133].Color := rgb( 70, 130, 180);

  Colors[134].Name := 'tan';
  Colors[134].Color := rgb(210, 180, 140);

  Colors[135].Name := 'teal';
  Colors[135].Color := rgb( 0, 128, 128);

  Colors[136].Name := 'thistle';
  Colors[136].Color := rgb(216, 191, 216);

  Colors[137].Name := 'tomato';
  Colors[137].Color := rgb(255, 99, 71);

  Colors[138].Name := 'turquoise';
  Colors[138].Color := rgb( 64, 224, 208);

  Colors[139].Name := 'violet';
  Colors[139].Color := rgb(238, 130, 238);

  Colors[140].Name := 'wheat';
  Colors[140].Color := rgb(245, 222, 179);

  Colors[141].Name := 'white';
  Colors[141].Color := rgb(255, 255, 255);

  Colors[142].Name := 'whitesmoke';
  Colors[142].Color := rgb(245, 245, 245);

  Colors[143].Name := 'yellow';
  Colors[143].Color := rgb(255, 255, 0);

  Colors[144].Name := 'yellowgreen';
  Colors[144].Color := rgb(154, 205, 50);
end.
