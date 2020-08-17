{******************************************************************}
{ SVG common                                                       }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-04-2005                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (type conversion and utility functions)          }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGCommon;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.Types,
  System.Math.Vectors,
  SVGTypes;


function TryStrToTFloat(const S: string; out Value: TFloat): Boolean;

function StrToTFloat(const S: string): TFloat;

// type conversion functions
function ToGPRectF(R: TRectF): TGPRectF; inline;
function FromGPRectF(R: TGPRectF): TRectF; inline;
function ToGPMatrix(const Matrix: TMatrix): TGPMatrix;

// Utility functions
function HasValue(F: TFloat): Boolean; overload; inline;
function HasValue(I: Integer): Boolean; overload; inline;
function FittedRect(const Bounds: TGPRectF; const Width, Height: Single): TGPRectF;

implementation

uses
  System.SysUtils;

function TryStrToTFloat(const S: string; out Value: TFloat): Boolean;
var
  S1: string;
begin
  S1 := StringReplace(S, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  S1 := StringReplace(S1, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(S1, Value);
  if not Result then
    Value := 0;
end;

function StrToTFloat(const S: string): TFloat;
begin
  TryStrToTFloat(S, Result);
end;

function ToGPRectF(R: TRectF): TGPRectF;
begin
  with R do
    Result := WinApi.GDIPAPI.MakeRect(Left, Top, Width, Height);
end;

function FromGPRectF(R: TGPRectF): TRectF;
begin
  with R do
    Result := TRectF.Create(X, Y, X + Width, Y + Height);
end;

function ToGPMatrix(const Matrix: TMatrix): TGPMatrix;
begin
  Result := TGPMatrix.Create(Matrix.m11, Matrix.m12, Matrix.m21, Matrix.m22, Matrix.m31,
    Matrix.m32);
end;

function HasValue(F: TFloat): Boolean;
begin
  Result := F <> UndefinedFloat;
end;

function HasValue(I: Integer): Boolean; overload; inline;
begin
  Result := I <> UndefinedInt;
end;


function FittedRect(const Bounds: TGPRectF; const Width, Height: Single): TGPRectF;
begin
  Result := ToGPRectF(TRectF.Create(0, 0, Width, Height).FitInto(FromGPRectF(Bounds)));
end;


end.
