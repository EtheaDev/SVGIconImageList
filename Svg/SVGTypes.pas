{******************************************************************}
{ SVG types                                                        }
{                                                                  }
{ home page : http://www.mwcs.de                                   }
{ email     : martin.walter@mwcs.de                                }
{                                                                  }
{ date      : 05-04-2008                                           }
{                                                                  }
{ Use of this file is permitted for commercial and non-commercial  }
{ use, as long as the author is credited.                          }
{ This file (c) 2005, 2008 Martin Walter                           }
{                                                                  }
{                                                                  }
{ Thanks to:                                                       }
{ Kiriakos Vlahos (New Types)                                      }
{                                                                  }
{ This Software is distributed on an "AS IS" basis, WITHOUT        }
{ WARRANTY OF ANY KIND, either express or implied.                 }
{                                                                  }
{ *****************************************************************}

unit SVGTypes;

interface

uses
  System.Math,
  System.Types,
  System.UITypes;

const
  SVG_INHERIT_COLOR = TColors.SysDefault;
  SVG_NONE_COLOR = TColors.SysNone;

  FontNormal = 0;
  FontItalic = 1;

  MaxTFloat = MaxSingle;
  UndefinedFloat = -340282346638528859811704183484516925440.0;  //Single.MinValue
  UndefinedInt = -2147483648; // Integer.MinValue

type
  TFloat = single;

  TListOfPoints = array of TPointF;

  TRectarray = packed array of TRect;
  PRectArray = ^TRectArray;

  TTextDecoration = set of (tdInherit, tdUnderLine, tdOverLine, tdStrikeOut);

  TTextPathMethod = (tpmAlign, tpmStretch);

  TTextPathSpacing = (tpsAuto, tpsExact);

  TSVGUnit = (suNone, suPX, suPT, suPC, suMM, suCM, suIN, suEM, suEX, suPercent);

  TGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TLengthType = (ltHorz, ltVert, ltOther);

  TTriStateBoolean = (tbFalse, tbTrue, tbInherit);

implementation


end.
