unit Img32;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.6                                                             *
* Date      :  12 October 2024                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  The core module of the Image32 library                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  Types, SysUtils, Classes,
  {$IFDEF MSWINDOWS} Windows,{$ENDIF}
  {$IFDEF USING_VCL_LCL}
    {$IFDEF USES_NAMESPACES} Vcl.Graphics, Vcl.Forms,
    {$ELSE}Graphics, Forms,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF XPLAT_GENERICS}
    Generics.Collections, Generics.Defaults, Character,
  {$ENDIF}
  {$IFDEF UITYPES} UITypes,{$ENDIF} Math;

type
  {$IF not declared(SizeInt)} // FPC has SizeInt
    {$IF CompilerVersion < 120}
  SizeInt = Integer; // Delphi 7-2007 can't use NativeInt with "FOR"
    {$ELSE}
  SizeInt = NativeInt;
    {$IFEND}
  {$IFEND}

  TRect = Types.TRect;
  TColor32 = type Cardinal;

  TPointD = record
    X, Y: double;
  end;

  PARGB = ^TARGB;
  TARGB = packed record
    case boolean of
      false: (B: Byte; G: Byte; R: Byte; A: Byte);
      true : (Color: TColor32);
  end;
  TArrayOfARGB = array of TARGB;
  PArgbArray = ^TArrayOfARGB;

const
  clNone32     = TColor32($00000000);
  clAqua32     = TColor32($FF00FFFF);
  clBlack32    = TColor32($FF000000);
  clBlue32     = TColor32($FF0000FF);
  clFuchsia32  = TColor32($FFFF00FF);
  clGray32     = TColor32($FF808080);
  clGreen32    = TColor32($FF008000);
  clGrey32     = TColor32($FF808080);
  clLime32     = TColor32($FF00FF00);
  clMaroon32   = TColor32($FF800000);
  clNavy32     = TColor32($FF000080);
  clOlive32    = TColor32($FF7F7F00);
  clOrange32   = TColor32($FFFF7F00);
  clPurple32   = TColor32($FF7F00FF);
  clRed32      = TColor32($FFFF0000);
  clSilver32   = TColor32($FFC0C0C0);
  clTeal32     = TColor32($FF007F7F);
  clWhite32    = TColor32($FFFFFFFF);
  clYellow32   = TColor32($FFFFFF00);

  //custom gray colors
  clDarkGray32 = TColor32($FF505050);
  clDarkGrey32 = TColor32($FF505050);
  //clGray32   = TColor32($FF808080);
  //clSilver32 = TColor32($FFC0C0C0);
  clLiteGray32 = TColor32($FFD3D3D3);
  clLiteGrey32 = TColor32($FFD3D3D3);
  clPaleGray32 = TColor32($FFE0E0E0);
  clPaleGrey32 = TColor32($FFE0E0E0);
  clDarkBtn32  = TColor32($FFE8E8E8);
  clBtnFace32  = TColor32($FFF0F0F0);
  clLiteBtn32  = TColor32($FFF8F8F8);

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

RT_BITMAP = PChar(2);

type
  TClipboardPriority = (cpLow, cpMedium, cpHigh);

  PColor32 = ^TColor32;
  TArrayOfColor32 = array of TColor32;
  TArrayOfArrayOfColor32 = array of TArrayOfColor32;
  TArrayOfInteger = array of Integer;
  TArrayOfWord = array of WORD;
  TArrayOfByte = array of Byte;

  TImg32Notification = (inStateChange, inDestroy);

  //A INotifyRecipient receives change notifications though a property
  //interface from a single NotifySender (eg a Font property).
  //A NotifySender can send change notificatons to multiple NotifyRecipients
  //(eg where multiple object use the same font property). NotifyRecipients can
  //still receive change notificatons from mulitple NotifySenders, but it
  //must use a separate property for each NotifySender. (Also there's little
  //benefit in using INotifySender and INotifyRecipient interfaces where there
  //will only be one receiver - eg scroll - scrolling window.)

  INotifyRecipient = interface
    ['{95F50C62-D321-46A4-A42C-8E9D0E3149B5}']
   procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification);
  end;
  TRecipients = array of INotifyRecipient;

  INotifySender = interface
    ['{52072382-8B2F-481D-BE0A-E1C0A216B03E}']
    procedure AddRecipient(recipient: INotifyRecipient);
    procedure DeleteRecipient(recipient: INotifyRecipient);
  end;

  TInterfacedObj = class(TObject, IInterface)
  public
  {$IFDEF FPC}
    function  _AddRef: Integer;
      {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function  _Release: Integer;
      {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function QueryInterface(
      {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;
      out obj) : longint;
      {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  {$ELSE}
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
    function  QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  {$ENDIF}
  end;

  TImage32 = class;
  TImageFormatClass = class of TImageFormat;

  //TImageFormat: Abstract base class for loading and saving images in TImage32.<br>
  //This class is overridden to provide support for separate
  //file storage formats (eg BMP, PNG, GIF & JPG).<br>
  //Derived classes register with TImage32 using TImage32.RegisterImageFormatClass.
  TImageFormat = class
  public
    class function IsValidImageStream(stream: TStream): Boolean; virtual; abstract;
    procedure SaveToStream(stream: TStream; img32: TImage32; quality: integer = 0); virtual; abstract;
    function SaveToFile(const filename: string; img32: TImage32; quality: integer = 0): Boolean; virtual;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; virtual; abstract;
    function LoadFromFile(const filename: string; img32: TImage32): Boolean; virtual;
    class function GetImageCount(stream: TStream): integer; virtual;
    class function CanCopyToClipboard: Boolean; virtual;
    class function CopyToClipboard(img32: TImage32): Boolean; virtual; abstract;
    class function CanPasteFromClipboard: Boolean; virtual; abstract;
    class function PasteFromClipboard(img32: TImage32): Boolean; virtual; abstract;
  end;

  TBlendFunction = function(bgColor, fgColor: TColor32): TColor32;
  TBlendLineFunction = procedure(bgColor, fgColor: PColor32; width: nativeint);

  TCompareFunction = function(master, current: TColor32; data: integer): Boolean;
  TCompareFunctionEx = function(master, current: TColor32): Byte;

  TTileFillStyle = (tfsRepeat, tfsMirrorHorz, tfsMirrorVert, tfsRotate180);

  TResamplerFunction = function(img: TImage32; x, y: double): TColor32;

  TGrayscaleMode = (gsmSaturation, gsmLinear, gsmColorimetric);

  TImage32 = class(TObject)
  private
    fWidth: integer;
    fHeight: Integer;
    fResampler: integer;
    fIsPremultiplied: Boolean;
    fColorCount: integer;
    fPixels: TArrayOfColor32;
    fOnChange: TNotifyEvent;
    fOnResize: TNotifyEvent;
    fUpdateCnt: integer;
    fAntiAliased: Boolean;
    fNotifyBlockCnt: integer;
    function GetPixel(x,y: Integer): TColor32;
    procedure SetPixel(x,y: Integer; color: TColor32);
    function GetIsBlank: Boolean;
    function GetIsEmpty: Boolean;
    function GetPixelBase: PColor32;
    function GetPixelRow(row: Integer): PColor32;
    procedure RotateLeft90;
    procedure RotateRight90;
    procedure Rotate180;
    function GetColorCount: Integer;
    function GetHasTransparency: Boolean;
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
  protected
    procedure ResetColorCount;
    function  RectHasTransparency(const rec: TRect): Boolean;
    function  CopyPixels(const rec: TRect): TArrayOfColor32;
    //CopyInternal: Internal routine (has no scaling or bounds checking)
    procedure CopyInternal(src: TImage32;
      const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
    procedure CopyInternalLine(src: TImage32;
      const srcRec, dstRec: TRect; blendLineFunc: TBlendLineFunction);
    function CopyBlendInternal(src: TImage32; srcRec, dstRec: TRect;
      blendFunc: TBlendFunction = nil; blendLineFunc: TBlendLineFunction = nil): Boolean; overload;
    procedure  Changed; virtual;
    procedure  Resized; virtual;
    function   SetPixels(const newPixels: TArrayOfColor32): Boolean;
    property   UpdateCount: integer read fUpdateCnt;
  public
    constructor Create(width: Integer = 0; height: Integer = 0); overload;
    //Create(src:array, width, height): Uses the specified array for the pixels.
    //  Uses src for the pixels without copying it.
    constructor Create(const src: TArrayOfColor32; width: Integer; height: Integer); overload;
    constructor Create(src: TImage32); overload;
    constructor Create(src: TImage32; const srcRec: TRect); overload;
    destructor Destroy; override;
    //BeginUpdate/EndUpdate: postpones calls to OnChange event (can be nested)
    procedure BeginUpdate;
    procedure EndUpdate;
    //BlockUpdate/UnBlockUpdate: blocks calls to OnChange event (can be nested)
    procedure BlockNotify;
    procedure UnblockNotify;

    procedure Assign(src: TImage32);
    procedure AssignTo(dst: TImage32);
    procedure AssignSettings(src: TImage32);
    //AssignPixelArray: Replaces the content and takes ownership of src.
    //  Uses src for the pixels without copying it.
    procedure AssignPixelArray(const src: TArrayOfColor32; width: Integer; height: Integer);

    //SetSize: Erases any current image, and fills with the specified color.
    procedure SetSize(newWidth, newHeight: Integer; color: TColor32 = 0);
    //Resize: is very similar to Scale()
    procedure Resize(newWidth, newHeight: Integer);
    procedure ResizeTo(targetImg: TImage32; newWidth, newHeight: Integer);
    //ScaleToFit: The image will be scaled proportionally
    procedure ScaleToFit(width, height: integer);
    //ScaleToFitCentered: The new image will be scaled and also centred
    procedure ScaleToFitCentered(width, height: integer); overload;
    procedure ScaleToFitCentered(const rect: TRect); overload;
    procedure Scale(s: double); overload;
    procedure Scale(sx, sy: double); overload;
    procedure ScaleTo(targetImg: TImage32; s: double); overload;
    procedure ScaleTo(targetImg: TImage32; sx, sy: double); overload;

    function Copy(src: TImage32; srcRec, dstRec: TRect): Boolean;
    //CopyBlend: Copies part or all of another image (src) on top of the
    //existing image. If no blend function is provided, then the function
    //will behave exactly as the Copy function above. However, when a blend
    //function is specified, that function will determine how the images will
    //be blended. If srcRec and dstRec have different widths or heights,
    //then the image in srcRec will also be stretched to fit dstRec.
    function CopyBlend(src: TImage32; const srcRec, dstRec: TRect;
      blendFunc: TBlendFunction = nil): Boolean; overload; {$IFDEF INLINE} inline; {$ENDIF}
    function CopyBlend(src: TImage32; const srcRec, dstRec: TRect;
      blendLineFunc: TBlendLineFunction): Boolean; overload; {$IFDEF INLINE} inline; {$ENDIF}

{$IFDEF MSWINDOWS}
    //CopyFromDC: Copies an image from a Windows device context, erasing
    //any current image in TImage32. (eg copying from TBitmap.canvas.handle)
    procedure CopyFromDC(srcDc: HDC; const srcRect: TRect);
    //CopyToDc: Copies the image into a Windows device context
    procedure CopyToDc(dstDc: HDC; x: Integer = 0; y: Integer = 0;
      transparent: Boolean = true); overload;
    procedure CopyToDc(const srcRect: TRect; dstDc: HDC;
      x: Integer = 0; y: Integer = 0; transparent: Boolean = true); overload;
    procedure CopyToDc(const srcRect, dstRect: TRect; dstDc: HDC;
      transparent: Boolean = true); overload;
{$ENDIF}
{$IF DEFINED(USING_VCL_LCL)}
    procedure CopyFromBitmap(bmp: TBitmap);
    procedure CopyToBitmap(bmp: TBitmap);
{$IFEND}
    function CopyToClipBoard: Boolean;
    class function CanPasteFromClipBoard: Boolean;
    function PasteFromClipBoard: Boolean;
    procedure Crop(const rec: TRect);
    //SetBackgroundColor: Assumes the current image is semi-transparent.
    procedure SetBackgroundColor(bgColor: TColor32);
    procedure Clear(color: TColor32 = 0); overload;
    procedure Clear(const rec: TRect; color: TColor32 = 0); overload;
    procedure FillRect(const rec: TRect; color: TColor32);

    procedure ConvertToBoolMask(reference: TColor32;
      tolerance: integer; colorFunc: TCompareFunction;
      maskBg: TColor32 = clWhite32; maskFg: TColor32 = clBlack32);
    procedure ConvertToAlphaMask(reference: TColor32;
      colorFunc: TCompareFunctionEx);

    procedure FlipVertical;
    procedure FlipHorizontal;
    procedure PreMultiply;
    //SetAlpha: Sets 'alpha' to the alpha byte of every pixel in the image
    procedure SetAlpha(alpha: Byte);
    procedure ReduceOpacity(opacity: Byte); overload;
    procedure ReduceOpacity(opacity: Byte; rec: TRect); overload;
    //SetRGB: Sets the RGB channels leaving the alpha channel unchanged
    procedure SetRGB(rgbColor: TColor32); overload;
    procedure SetRGB(rgbColor: TColor32; rec: TRect); overload;
    //Grayscale: Only changes color channels. The alpha channel is untouched.
    procedure Grayscale(mode: TGrayscaleMode = gsmSaturation;
      linearAmountPercentage: double = 1.0);
    procedure InvertColors;
    procedure InvertAlphas;
    procedure AdjustHue(percent: Integer);         //ie +/- 100%
    procedure AdjustLuminance(percent: Integer);   //ie +/- 100%
    procedure AdjustSaturation(percent: Integer);  //ie +/- 100%

    function GetOpaqueBounds: TRect;
    //CropTransparentPixels: Trims transparent edges until each edge contains
    //at least one opaque or semi-opaque pixel.
    function CropTransparentPixels: TRect;
    procedure Rotate(angleRads: double);
    //RotateRect: Rotates part of an image, but also clips those parts of the
    //rotated image that fall outside rec. The eraseColor parameter indicates
    //the color to fill those uncovered pixels in rec following rotation.
    procedure RotateRect(const rec: TRect;
      angleRads: double; eraseColor: TColor32 = 0);
    procedure Skew(dx,dy: double);

    //ScaleAlpha: Scales the alpha byte of every pixel by the specified amount.
    procedure ScaleAlpha(scale: double);
    class procedure RegisterImageFormatClass(ext: string;
      bm32ExClass: TImageFormatClass; clipPriority: TClipboardPriority);
    class function GetImageFormatClass(const ext: string): TImageFormatClass; overload;
    class function GetImageFormatClass(stream: TStream): TImageFormatClass; overload;
    class function IsRegisteredFormat(const ext: string): Boolean;
    function SaveToFile(filename: string; quality: integer = 0): Boolean;
    function SaveToStream(stream: TStream; const FmtExt: string): Boolean;
    function LoadFromFile(const filename: string): Boolean;
    function LoadFromStream(stream: TStream; imgIdx: integer = 0): Boolean;
    function LoadFromResource(const resName: string; resType: PChar): Boolean;

    //properties ...

    property AntiAliased: Boolean read fAntiAliased write fAntiAliased;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Bounds: TRect read GetBounds;
    property IsBlank: Boolean read GetIsBlank;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsPreMultiplied: Boolean read fIsPremultiplied;
    property MidPoint: TPointD read GetMidPoint;
    property Pixel[x,y: Integer]: TColor32 read GetPixel write SetPixel;
    property Pixels: TArrayOfColor32 read fPixels;
    property PixelBase: PColor32 read GetPixelBase;
    property PixelRow[row: Integer]: PColor32 read GetPixelRow;
    property ColorCount: Integer read GetColorCount;
    //HasTransparency: Returns true if any pixel's alpha byte < 255.
    property HasTransparency: Boolean read GetHasTransparency;
    //Resampler: is used in scaling and rotation transforms
    property Resampler: integer read fResampler write fResampler;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
  end;

  TImageList32 = class
  private
{$IFDEF XPLAT_GENERICS}
    fList: TList<TImage32>;
{$ELSE}
    fList: TList;
{$ENDIF}
    fIsImageOwner: Boolean;
    function GetImage(index: integer): TImage32;
    procedure SetImage(index: integer; img: TIMage32);
    function GetLast: TImage32;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    procedure Add(image: TImage32); overload;
    function Add(width, height: integer): TImage32; overload;
    procedure Insert(index: integer; image: TImage32);
    procedure Move(currentIndex, newIndex: integer);
    procedure Delete(index: integer);
    property Image[index: integer]: TImage32 read GetImage write SetImage; default;
    property IsImageOwner: Boolean read fIsImageOwner write fIsImageOwner;
    property Last: TImage32 read GetLast;
  end;

  THsl = packed record
    hue  : byte;
    sat  : byte;
    lum  : byte;
    alpha: byte;
  end;
  PHsl = ^THsl;
  TArrayofHSL = array of THsl;

  TTriState = (tsUnknown = 0, tsYes = 1, tsChecked = 1, tsNo = 2, tsUnchecked = 2);

  PPointD = ^TPointD;
  TPathD = array of TPointD;       //nb: watch for ambiguity with Clipper.pas
  TPathsD = array of TPathD;       //nb: watch for ambiguity with Clipper.pas
  TArrayOfPathsD = array of TPathsD;

  TArrayOfDouble = array of double;
  TArrayOfString = array of string;

  TRectD = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    {$IFNDEF RECORD_METHODS}
    Left, Top, Right, Bottom: Double;
    function TopLeft: TPointD;
    function BottomRight: TPointD;
    {$ENDIF}
    function IsEmpty: Boolean;
    function Width: double;
    function Height: double;
    //Normalize: Returns True if swapping top & bottom or left & right
    function Normalize: Boolean;
    function Contains(const Pt: TPoint): Boolean; overload;
    function Contains(const Pt: TPointD): Boolean; overload;
    function MidPoint: TPointD;
    {$IFDEF RECORD_METHODS}
    case Integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft, BottomRight: TPointD);
    {$ENDIF}
  end;

  {$IFNDEF PBYTE}
  PByte = type PChar;
  {$ENDIF}

  //BLEND FUNCTIONS ( see TImage32.CopyBlend() )

  //BlendToOpaque: Blends a semi-transparent image onto an opaque background
  function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
  //BlendToAlpha: Blends two semi-transparent images (slower than BlendToOpaque)
  function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
  procedure BlendToAlphaLine(bgColor, fgColor: PColor32; width: nativeint);
  //BlendMask: Whereever the mask is, preserves the background
  function BlendMask(bgColor, alphaMask: TColor32): TColor32;
  procedure BlendMaskLine(bgColor, alphaMask: PColor32; width: nativeint);
  function BlendAltMask(bgColor, alphaMask: TColor32): TColor32;
  function BlendDifference(color1, color2: TColor32): TColor32;
  function BlendSubtract(bgColor, fgColor: TColor32): TColor32;
  function BlendLighten(bgColor, fgColor: TColor32): TColor32;
  function BlendDarken(bgColor, fgColor: TColor32): TColor32;
  function BlendInvertedMask(bgColor, alphaMask: TColor32): TColor32;
  procedure BlendInvertedMaskLine(bgColor, alphaMask: PColor32; width: nativeint);
  //BlendBlueChannel: typically useful for white color masks
  function BlendBlueChannel(bgColor, blueMask: TColor32): TColor32;
  procedure BlendBlueChannelLine(bgColor, blueMask: PColor32; width: nativeint);

  //COMPARE COLOR FUNCTIONS (ConvertToBoolMask, FloodFill, Vectorize etc.)

  function CompareRGB(master, current: TColor32; tolerance: Integer): Boolean;
  function CompareHue(master, current: TColor32; tolerance: Integer): Boolean;
  function CompareAlpha(master, current: TColor32; tolerance: Integer): Boolean;

  //CompareEx COLOR FUNCTIONS (see ConvertToAlphaMask)
  function CompareRgbEx(master, current: TColor32): Byte;
  function CompareAlphaEx(master, current: TColor32): Byte;

  //MISCELLANEOUS FUNCTIONS ...

  function GetBoolMask(img: TImage32; reference: TColor32;
    compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;

  function GetByteMask(img: TImage32; reference: TColor32;
    compareFunc: TCompareFunctionEx): TArrayOfByte;

  function GetColorMask(img: TImage32; reference: TColor32;
    compareFunc: TCompareFunction; tolerance: Integer): TArrayOfColor32;

  {$IFDEF MSWINDOWS}
  //Color32: Converts a Graphics.TColor value into a TColor32 value.
  function Color32(rgbColor: Integer): TColor32; overload;

  procedure FixPalette(p: PARGB; count: integer);
  {$ENDIF}
  function Color32(a, r, g, b: Byte): TColor32; overload;

  //RGBColor: Converts a TColor32 value into a COLORREF value
  function RGBColor(color: TColor32): Cardinal;
  function InvertColor(color: TColor32): TColor32;

  //RgbToHsl: See https://en.wikipedia.org/wiki/HSL_and_HSV
  function RgbToHsl(color: TColor32): THsl;
  //HslToRgb: See https://en.wikipedia.org/wiki/HSL_and_HSV
  function HslToRgb(hslColor: THsl): TColor32;
  function AdjustHue(color: TColor32; percent: Integer): TColor32;
  function ArrayOfColor32ToArrayHSL(const clr32Arr: TArrayOfColor32): TArrayofHSL;
  function ArrayOfHSLToArrayColor32(const hslArr: TArrayofHSL): TArrayOfColor32;

  function GetAlpha(color: TColor32): Byte;  {$IFDEF INLINE} inline; {$ENDIF}

  function PointD(const X, Y: Double): TPointD; overload;
  function PointD(const pt: TPoint): TPointD; overload;

  function RectD(left, top, right, bottom: double): TRectD; overload;
  function RectD(const rec: TRect): TRectD; overload;

  function ClampByte(val: Integer): byte; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function ClampByte(val: double): byte; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function ClampRange(val, min, max: Integer): Integer; overload;
    {$IFDEF INLINE} inline; {$ENDIF}
  function ClampRange(val, min, max: double): double; overload;
    {$IFDEF INLINE} inline; {$ENDIF}
  function IncPColor32(pc: Pointer; cnt: Integer): PColor32;

  procedure NormalizeAngle(var angle: double; tolerance: double = Pi/360);
  function GrayScale(color: TColor32): TColor32;

  //DPIAware: Useful for DPIAware sizing of images and their container controls.
  //It scales values relative to the display's resolution (PixelsPerInch).
  //See https://docs.microsoft.com/en-us/windows/desktop/hidpi/high-DPIAware-desktop-application-development-on-windows
  function DPIAware(val: Integer): Integer; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function DPIAware(val: double): double; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function DPIAware(const pt: TPoint): TPoint; overload;
  function DPIAware(const pt: TPointD): TPointD; overload;
  function DPIAware(const rec: TRect): TRect; overload;
  function DPIAware(const rec: TRectD): TRectD; overload;

{$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  function AlphaBlend(DC: HDC; p2, p3, p4, p5: Integer;
    DC6: HDC; p7, p8, p9, p10: Integer; p11: Windows.TBlendFunction): BOOL;
    stdcall; external 'msimg32.dll' name 'AlphaBlend';
  {$ENDIF}
{$ENDIF}

  //CreateResourceStream: handles both numeric and string names and types
  function CreateResourceStream(const resName: string;
    resType: PChar): TResourceStream;

  function GetResampler(id: integer): TResamplerFunction;
  function RegisterResampler(func: TResamplerFunction; const name: string): integer;
  procedure GetResamplerList(stringList: TStringList);

const
  TwoPi = Pi *2;
  angle0   = 0;
  angle1   = Pi/180;
  angle15  = Pi /12;
  angle30  = angle15 *2;
  angle45  = angle15 *3;
  angle60  = angle15 *4;
  angle75  = angle15 *5;
  angle90  = Pi /2;
  angle105 = Pi - angle75;
  angle120 = Pi - angle60;
  angle135 = Pi - angle45;
  angle150 = Pi - angle30;
  angle165 = Pi - angle15;
  angle180 = Pi;
  angle195 = Pi + angle15;
  angle210 = Pi + angle30;
  angle225 = Pi + angle45;
  angle240 = Pi + angle60;
  angle255 = Pi + angle75;
  angle270 = TwoPi - angle90;
  angle285 = TwoPi - angle75;
  angle300 = TwoPi - angle60;
  angle315 = TwoPi - angle45;
  angle330 = TwoPi - angle30;
  angle345 = TwoPi - angle15;
  angle360 = TwoPi;

  div255: Double = 1 / 255;

var
  //Resampling function identifiers (initialized in Img32.Resamplers)
  rNearestResampler : integer;
  rBilinearResampler: integer;
  rBicubicResampler : integer;
  rWeightedBilinear : integer;
  DefaultResampler: Integer = 0;

  //Both MulTable and DivTable are used in blend functions
  //MulTable[a,b] = a * b / 255
  MulTable: array [Byte,Byte] of Byte;
  //DivTable[a,b] = a * 255/b (for a &lt;= b)
  DivTable: array [Byte,Byte] of Byte;

  //Sigmoid: weight byte values towards each end
  Sigmoid: array[Byte] of Byte;

  dpiAware1   : integer = 1;
  DpiAwareOne : double  = 1.0;

  //AND BECAUSE OLDER DELPHI COMPILERS (OLDER THAN D2006)
  //DON'T SUPPORT RECORD METHODS
  procedure RectWidthHeight(const rec: TRect; out width, height: Integer);
  {$IFDEF INLINE} inline; {$ENDIF}
  function RectWidth(const rec: TRect): Integer;
  {$IFDEF INLINE} inline; {$ENDIF}
  function RectHeight(const rec: TRect): Integer;
  {$IFDEF INLINE} inline; {$ENDIF}

  function IsEmptyRect(const rec: TRect): Boolean; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function IsEmptyRect(const rec: TRectD): Boolean; overload;
  {$IFDEF INLINE} inline; {$ENDIF}

  function SwapRedBlue(color: TColor32): TColor32; overload;
  procedure SwapRedBlue(color: PColor32; count: integer); overload;

  function MulBytes(b1, b2: Byte) : Byte;

  function __Trunc(Value: Double): Integer; {$IFNDEF CPUX86} {$IFDEF INLINE} inline; {$ENDIF} {$ENDIF}

  // NewColor32Array creates a new "array of TColor32". "a" is nil'ed
  // before allocating the array. If "count" is zero or negative "a" will
  // be nil. If "uninitialized" is True, the memory will not be zero'ed.
  procedure NewColor32Array(var a: TArrayOfColor32; count: nativeint;
    uninitialized: boolean = False);
  procedure NewIntegerArray(var a: TArrayOfInteger; count: nativeint;
    uninitialized: boolean = False);
  procedure NewByteArray(var a: TArrayOfByte; count: nativeint;
    uninitialized: boolean = False);
  procedure NewPointDArray(var a: TPathD; count: nativeint;
    uninitialized: boolean = False);

  // SetLengthUninit changes the dyn. array's length but does not initialize
  // the new elements with zeros. It can be used as a replacement for
  // SetLength where the zero-initialitation is not required.
  procedure SetLengthUninit(var a: TArrayOfColor32; count: nativeint); overload;
  procedure SetLengthUninit(var a: TArrayOfInteger; count: nativeint); overload;
  procedure SetLengthUninit(var a: TArrayOfByte; count: nativeint); overload;
  procedure SetLengthUninit(var a: TPathD; count: nativeint); overload;

implementation

uses
  Img32.Vector, Img32.Resamplers, Img32.Transform, Img32.Fmt.BMP;

resourcestring
  rsImageTooLarge = 'Image32 error: the image is too large.';
  rsInvalidImageArrayData = 'Image32 error: the specified pixels array and the size does not match.';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF CPUX86}
const
  // Use faster Trunc for x86 code in this unit.
  Trunc: function(Value: Double): Integer = __Trunc;
{$ENDIF CPUX86}

type
  TByteArray = array[0..MaxInt -1] of Byte;
  PByteArray = ^TByteArray;

  {$IFDEF SUPPORTS_POINTERMATH}
    {$POINTERMATH ON}
  PStaticColor32Array = ^TColor32;
  PStaticARGBArray = ^TARGB;
    {$POINTERMATH OFF}
  {$ELSE} // Delphi 7-2007
  PStaticColor32Array = ^TStaticColor32Array;
  TStaticColor32Array = array[0..MaxInt div SizeOf(TColor32) - 1] of TColor32;
  PStaticARGBArray = ^TStaticARGBArray;
  TStaticARGBArray = array[0..MaxInt div SizeOf(TARGB) - 1] of TARGB;
  {$ENDIF}

  TImgFmtRec = record
    Fmt: string;
    SortOrder: TClipboardPriority;
    Obj: TImageFormatClass;
  end;
  PImgFmtRec = ^TImgFmtRec;

  TResamplerObj = class
    id: integer;
    name: string;
    func: TResamplerFunction;
  end;

  PDynArrayRec = ^TDynArrayRec;
  {$IFDEF FPC}
  tdynarrayindex = sizeint;
  TDynArrayRec = packed record
    refcount: ptrint;
    high: tdynarrayindex;
    Data: record end;
  end;
  {$ELSE}
  TDynArrayRec = packed record
    {$IFDEF CPU64BITS}
    _Padding: Integer;
    {$ENDIF}
    RefCnt: Integer;
    Length: NativeInt;
    Data: record end;
  end;
  {$ENDIF}

var
{$IFDEF XPLAT_GENERICS}
  ImageFormatClassList: TList<PImgFmtRec>; //list of supported file extensions
  ResamplerList: TList<TResamplerObj>;     //list of resampler functions
{$ELSE}
  ImageFormatClassList: TList;
  ResamplerList: TList;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function NewSimpleDynArray(count: nativeint; elemSize: integer; uninitialized: boolean = False): Pointer;
var
  p: PDynArrayRec;
begin
  Result := nil;
  if (count > 0) and (elemSize > 0) then
  begin
    if uninitialized then
      GetMem(Pointer(p), SizeOf(TDynArrayRec) + count * elemSize)
    else
      p := AllocMem(SizeOf(TDynArrayRec) + count * elemSize);
    {$IFDEF FPC}
    p.refcount := 1;
    p.high := count -1;
    {$ELSE}
    p.RefCnt := 1;
    p.Length := count;
    {$ENDIF}
    Result := @p.Data;
  end;
end;
//------------------------------------------------------------------------------

function InternSetSimpleDynArrayLengthUninit(a: Pointer; count: nativeint; elemSize: integer): Pointer;
var
  p: PDynArrayRec;
  oldCount: nativeint;
begin
  if a = nil then
    Result := NewSimpleDynArray(count, elemSize)
  else if (count > 0) and (elemSize > 0) then
  begin
    p := PDynArrayRec(PByte(a) - SizeOf(TDynArrayRec));
    {$IFDEF FPC}
    oldCount := p.high + 1;
    if p.refcount = 1 then
    {$ELSE}
    oldCount := p.Length;
    if p.RefCnt = 1 then
    {$ENDIF}
    begin
      // There is only one reference to this array and that is "a",
      // so we can use ReallocMem to change the array's length.
      if oldCount = count then
      begin
        Result := a;
        Exit;
      end;
      ReallocMem(Pointer(p), SizeOf(TDynArrayRec) + count * elemSize);
    end
    else
    begin
      // SetLength makes a copy of the dyn array to get RefCnt=1
      GetMem(Pointer(p), SizeOf(TDynArrayRec) + count * elemSize);
      if oldCount < 0 then oldCount := 0; // data corruption detected
      if oldCount > count then oldCount := count;
      Move(a^, p.Data, oldCount * elemSize);
      TArrayOfByte(a) := nil; // use a non-managed dyn.array type
    end;

    {$IFDEF FPC}
    p.refcount := 1;
    p.high := count -1;
    {$ELSE}
    p.RefCnt := 1;
    p.Length := count;
    {$ENDIF}
    Result := @p.Data;
  end
  else
  begin
    TArrayOfByte(a) := nil; // use a non-managed dyn.array type
    Result := nil;
  end;
end;
//------------------------------------------------------------------------------

function CanReuseDynArray(a: Pointer; count: nativeint): Boolean;
// returns True if RefCnt=1 and Length=count
begin
  //Assert(a <> nil);
  a := PByte(a) - SizeOf(TDynArrayRec);
  Result :=
    {$IFDEF FPC}
    (PDynArrayRec(a).refcount = 1) and
    (PDynArrayRec(a).high = count - 1);
    {$ELSE}
    (PDynArrayRec(a).RefCnt = 1) and
    (PDynArrayRec(a).Length = count);
    {$ENDIF}
end;
//------------------------------------------------------------------------------

procedure NewColor32Array(var a: TArrayOfColor32; count: nativeint; uninitialized: boolean);
begin
  if a <> nil then
  begin
    if uninitialized and CanReuseDynArray(a, count) then
      Exit;
    a := nil;
  end;
  Pointer(a) := NewSimpleDynArray(count, SizeOf(TColor32), uninitialized);
end;
//------------------------------------------------------------------------------

procedure NewIntegerArray(var a: TArrayOfInteger; count: nativeint; uninitialized: boolean);
begin
  if a <> nil then
  begin
    if uninitialized and CanReuseDynArray(a, count) then
      Exit;
    a := nil;
  end;
  Pointer(a) := NewSimpleDynArray(count, SizeOf(Integer), uninitialized);
end;
//------------------------------------------------------------------------------

procedure NewByteArray(var a: TArrayOfByte; count: nativeint; uninitialized: boolean);
begin
  if a <> nil then
  begin
    if uninitialized and CanReuseDynArray(a, count) then
      Exit;
    a := nil;
  end;
  Pointer(a) := NewSimpleDynArray(count, SizeOf(Byte), uninitialized);
end;
//------------------------------------------------------------------------------

procedure NewPointDArray(var a: TPathD; count: nativeint; uninitialized: boolean);
begin
  if a <> nil then
  begin
    if uninitialized and CanReuseDynArray(a, count) then
      Exit;
    a := nil;
  end;
  Pointer(a) := NewSimpleDynArray(count, SizeOf(TPointD), uninitialized);
end;
//------------------------------------------------------------------------------

procedure SetLengthUninit(var a: TArrayOfColor32; count: nativeint);
begin
  Pointer(a) := InternSetSimpleDynArrayLengthUninit(Pointer(a), count, SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

procedure SetLengthUninit(var a: TArrayOfInteger; count: nativeint);
begin
  Pointer(a) := InternSetSimpleDynArrayLengthUninit(Pointer(a), count, SizeOf(Integer));
end;
//------------------------------------------------------------------------------

procedure SetLengthUninit(var a: TArrayOfByte; count: nativeint);
begin
  Pointer(a) := InternSetSimpleDynArrayLengthUninit(Pointer(a), count, SizeOf(Byte));
end;
//------------------------------------------------------------------------------

procedure SetLengthUninit(var a: TPathD; count: nativeint);
begin
  Pointer(a) := InternSetSimpleDynArrayLengthUninit(Pointer(a), count, SizeOf(TPointD));
end;
//------------------------------------------------------------------------------

procedure CreateImageFormatList;
begin
  if Assigned(ImageFormatClassList) then Exit;

{$IFDEF XPLAT_GENERICS}
  ImageFormatClassList := TList<PImgFmtRec>.Create;
{$ELSE}
  ImageFormatClassList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function FMod(const ANumerator, ADenominator: Double): Double;
begin
  Result := ANumerator - Trunc(ANumerator / ADenominator) * ADenominator;
end;
//------------------------------------------------------------------------------

procedure NormalizeAngle(var angle: double; tolerance: double = Pi/360);
var
  aa: double;
begin
  angle := FMod(angle, angle360);
  if angle < -Angle180 then angle := angle + angle360
  else if angle > angle180 then angle := angle - angle360;

  aa := Abs(angle);
  if aa < tolerance then angle := 0
  else if aa > angle180 - tolerance then angle := angle180
  else if (aa < angle90 - tolerance) or (aa > angle90 + tolerance) then Exit
  else if angle < 0 then angle := -angle90
  else angle := angle90;
end;
//------------------------------------------------------------------------------

{$IFDEF CPUX86}
{ Trunc with FPU code is very slow because the x87 ControlWord has to be changed
  and then there is Delphi's Default8087CW variable that is not thread-safe. }

//__Trunc: An efficient Trunc() algorithm (ie rounds toward zero)
function __Trunc(Value: Double): Integer;
var
  exp: integer;
  i64: UInt64 absolute Value;
  valueBytes: array[0..7] of Byte absolute Value;
begin
  // https://en.wikipedia.org/wiki/Double-precision_floating-point_format
  // 52 bit fractional value, 11bit ($7FF) exponent, and 1bit sign
  Result := 0;
  if i64 = 0 then Exit;
  exp := Integer(Cardinal(i64 shr 52) and $7FF) - 1023;
  // nb: when exp == 1024 then Value == INF or NAN.
  if exp < 0 then
    Exit
  //else if exp > 52 then   // ie only for 64bit int results
  //  Result := ((i64 and $1FFFFFFFFFFFFF) shl (exp - 52)) or (1 shl exp)
  //else if exp > 31 then   // alternatively, range check for 32bit ints ????
  //  raise Exception.Create(rsIntegerOverflow)
  else
    Result := Integer((i64 and $1FFFFFFFFFFFFF) shr (52 - exp)) or (1 shl exp);
  // Check for the sign bit without loading Value into the FPU.
  if valueBytes[7] and $80 <> 0 then Result := -Result;
end;
//------------------------------------------------------------------------------

{$ELSE}
function __Trunc(Value: Double): Integer;
begin
  // Uses fast SSE2 instruction
  Result := System.Trunc(Value);
end;
//------------------------------------------------------------------------------
{$ENDIF CPUX86}

function SwapRedBlue(color: TColor32): TColor32;
var
  c: array[0..3] of byte absolute color;
  r: array[0..3] of byte absolute Result;
begin
  result := color;
  r[0] := c[2];
  r[2] := c[0];
end;
//------------------------------------------------------------------------------

procedure SwapRedBlue(color: PColor32; count: integer);
var
  i: integer;
begin
  for i := 1 to count do
  begin
    color^ := SwapRedBlue(color^);
    inc(color);
  end;
end;
//------------------------------------------------------------------------------

function MulBytes(b1, b2: Byte) : Byte; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := MulTable[b1, b2];
end;
//------------------------------------------------------------------------------

function ImageFormatClassListSort(item1, item2: Pointer): integer;
var
  imgFmtRec1: PImgFmtRec absolute item1;
  imgFmtRec2: PImgFmtRec absolute item2;
begin
  Result := Integer(imgFmtRec1.SortOrder) - Integer(imgFmtRec2.SortOrder);
end;
//------------------------------------------------------------------------------

function ClampByte(val: Integer): byte;
begin
  if val < 0 then result := 0
  else if val > 255 then result := 255
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampByte(val: double): byte;
begin
  if val <= 0 then result := 0
  else if val >= 255 then result := 255
  else result := Round(val);
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Blend functions - used by TImage32.CopyBlend()
//------------------------------------------------------------------------------

function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
var
  fgA: byte;
  fw,bw: PByteArray;
begin
  fgA := fgColor shr 24;
  if fgA = 0 then Result := bgColor
  else if fgA = 255 then Result := fgColor
  else
  begin
    //assuming bg.A = 255, use just fg.A for color weighting
    fw := PByteArray(@MulTable[fgA]);     //ie weight of foreground
    bw := PByteArray(@MulTable[not fgA]); //ie weight of background

    Result := $FF000000
              or (TColor32(Byte(fw[Byte(fgColor shr 16)] + bw[Byte(bgColor shr 16)])) shl 16)
              or (TColor32(Byte(fw[Byte(fgColor shr 8 )] + bw[Byte(bgColor shr  8)])) shl  8)
              or (TColor32(Byte(fw[Byte(fgColor       )] + bw[Byte(bgColor       )]))       );
  end;
end;
//------------------------------------------------------------------------------

function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
var
  fgWeight: byte;
  R, InvR: PByteArray;
  bgA, fgA: byte;
begin
  //(see https://en.wikipedia.org/wiki/Alpha_compositing)
  fgA := fgColor shr 24;
  bgA := bgColor shr 24;
  if (bgA = 0) or (fgA = 255) then Result := fgColor
  else if fgA = 0 then Result := bgColor
  else
  begin
    //combine alphas ...
    Result := not MulTable[not fgA, not bgA];
    fgWeight := DivTable[fgA, Result]; //fgWeight = amount foreground color
                                       //contibutes to total (result) color

    R     := PByteArray(@MulTable[fgWeight]);      //ie weight of foreground
    InvR  := PByteArray(@MulTable[not fgWeight]);  //ie weight of background

    Result := Result shl 24
              or (TColor32(R[Byte(fgColor shr 16)] + InvR[Byte(bgColor shr 16)]) shl 16)
              or (TColor32(R[Byte(fgColor shr 8 )] + InvR[Byte(bgColor shr  8)]) shl  8)
              or (TColor32(R[Byte(fgColor)       ] + InvR[Byte(bgColor)       ])       );
  end;
end;
//------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index is used

{$IFNDEF CPUX64}
function BlendToAlphaLineX86(bgColorArr, fgColorArr: PStaticColor32Array;
  idx: nativeint): nativeint;
// Helper function for x86 code, reduces the CPU register pressure in
// BlendToAlphaLine().
var
  fgWeight: byte;
  R, InvR: PByteArray;
  fgA, bgA, newBgA: byte;
  fgCol, bgCol: TColor32;
begin
  fgCol := fgColorArr[idx];
  bgCol := bgColorArr[idx];
  Result := idx;              // idx - negative offset into color arrays

  while True do
  begin
    fgA := fgCol shr 24;
    bgA := bgCol shr 24;

    //combine alphas ...
    newBgA := not MulTable[not fgA, not bgA];
    fgWeight := DivTable[fgA, newBgA]; //fgWeight = amount foreground color
                                       //contibutes to total (result) color

    R     := PByteArray(@MulTable[fgWeight]);      //ie weight of foreground
    InvR  := PByteArray(@MulTable[not fgWeight]);  //ie weight of foreground

    while True do
    begin
      bgColorArr[Result] := TColor32(newBgA) shl 24
            or (TColor32(R[Byte(fgCol shr 16)] + InvR[Byte(bgCol shr 16)]) shl 16)
            or (TColor32(R[Byte(fgCol shr 8 )] + InvR[Byte(bgCol shr  8)]) shl  8)
            or (TColor32(R[Byte(fgCol)       ] + InvR[Byte(bgCol)       ])       );
      inc(Result);
      if Result = 0 then exit;

      fgCol := fgColorArr[Result];
      bgCol := bgColorArr[Result];

      // if both alpha channels are the same in the new pixels, we
      // can use the already calculated R/InvR tables.
      if (fgCol shr 24 <> fgA) or (bgCol shr 24 <> bgA) then break;
    end;
    // return if we have alpha channel values for which we have special code
    if (fgCol and $FF000000 = 0) or (fgCol and $FF000000 = $FF000000) or (bgCol and $FF000000 = 0) then exit;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF ~CPUX64}

procedure BlendToAlphaLine(bgColor, fgColor: PColor32; width: nativeint);
label
  LabelBgAlphaIsZero;
var
  bgColorArr, fgColorArr: PStaticColor32Array;
  bgCol, fgCol: TColor32;
  {$IFDEF CPUX64}
  fgWeight, fgA, bgA: byte;
  R, InvR: PByteArray;
  {$ENDIF CPUX64}
begin
  //(see https://en.wikipedia.org/wiki/Alpha_compositing)

  // Use the negative offset trick to only increment the array "width"
  // until it reaches zero. And by offsetting the arrays by "width",
  // the negative "width" values also becomes the index into these arrays.
  inc(bgColor, width);
  inc(fgColor, width);
  width := -width;

  bgColorArr := PStaticColor32Array(bgColor);
  fgColorArr := PStaticColor32Array(fgColor);

  while width < 0 do
  begin
    bgCol := bgColorArr[width];
    fgCol := fgColorArr[width];

    // bgColor.A is zero => change bgColor to fgColor
    while bgCol shr 24 = 0 do
    begin
LabelBgAlphaIsZero:
      bgColorArr[width] := fgCol;
      inc(width);
      if width = 0 then exit;
      fgCol := fgColorArr[width];
      bgCol := bgColorArr[width];
    end;

    // fgColor.A is zero => don't change bgColor
    while fgCol shr 24 = 0 do
    begin
      // bgColorArr[w] := bgColorArr[w];
      inc(width);
      if width = 0 then exit;
      fgCol := fgColorArr[width];
      bgCol := bgColorArr[width];
      if bgCol shr 24 = 0 then goto LabelBgAlphaIsZero;
    end;

    // fgColor.A is 255 => change bgColor to fgColor
    while fgCol shr 24 = 255 do
    begin
      bgColorArr[width] := fgCol;
      inc(width);
      if width = 0 then exit;
      fgCol := fgColorArr[width];
      bgCol := bgColorArr[width];
      if bgCol shr 24 = 0 then goto LabelBgAlphaIsZero;
    end;

    {$IFDEF CPUX64}
    // x64 has more CPU registers than x86 and calling BlendToAlphaLineX86
    // is slower, so we inline it.

    //combine alphas ...
    fgA := fgCol shr 24;
    bgA := bgCol shr 24;
    bgA := not MulTable[not fgA, not bgA];
    fgWeight := DivTable[fgA, bgA]; //fgWeight = amount foreground color
                                    //contibutes to total (result) color

    R     := PByteArray(@MulTable[fgWeight]);      //ie weight of foreground
    InvR  := PByteArray(@MulTable[not fgWeight]);  //ie weight of foreground

    bgColorArr[width] := TColor32(bgA) shl 24
          or (TColor32(R[Byte(fgCol shr 16)] + InvR[Byte(bgCol shr 16)]) shl 16)
          or (TColor32(R[Byte(fgCol shr 8 )] + InvR[Byte(bgCol shr  8)]) shl  8)
          or (TColor32(R[Byte(fgCol)       ] + InvR[Byte(bgCol)       ])       );
    inc(width);
    {$ELSE}
    // x86 has not enough CPU registers and the loops above will suffer if we
    // inline the code. So we let the compiler use a "new set" of CPU registers
    // by calling a function.
    width := BlendToAlphaLineX86(bgColorArr, fgColorArr, width);
    {$ENDIF CPUX64}
  end;
end;
//------------------------------------------------------------------------------

{
// reference implementation
procedure BlendToAlphaLine(bgColor, fgColor: PColor32; width: nativeint);
var
  fgWeight: byte;
  R, InvR: PByteArray;
  bgA, fgA: Byte;
  bgColorArr, fgColorArr: PStaticColor32Array;
  bgCol, fgCol: TColor32;
begin
  //(see https://en.wikipedia.org/wiki/Alpha_compositing)

  // Use the negative offset trick to only increment the array "width"
  // until it reaches zero. And by offsetting the arrays by "width",
  // the negative "width" values also becomes the index into these arrays.
  inc(bgColor, width);
  inc(fgColor, width);
  width := -width;

  bgColorArr := PStaticColor32Array(bgColor);
  fgColorArr := PStaticColor32Array(fgColor);

  while width < 0 do
  begin
    bgCol := bgColorArr[width];
    fgCol := fgColorArr[width];
    bgA := bgCol shr 24;
    if bgA = 0 then bgColorArr[width] := fgCol
    else
    begin
      fgA := fgCol shr 24;
      if fgA > 0 then
      begin
        if fgA = 255 then bgColorArr[width] := fgCol
        else if fgA > 0 then
        begin
          //combine alphas ...
          bgA := not MulTable[not fgA, not bgA];
          fgWeight := DivTable[fgA, bgA]; //fgWeight = amount foreground color
                                          //contibutes to total (result) color

          R     := PByteArray(@MulTable[fgWeight]);      //ie weight of foreground
          InvR  := PByteArray(@MulTable[not fgWeight]);  //ie weight of foreground

          bgColorArr[width] := TColor32(bgA) shl 24
                or (TColor32(R[Byte(fgCol shr 16)] + InvR[Byte(bgCol shr 16)]) shl 16)
                or (TColor32(R[Byte(fgCol shr 8 )] + InvR[Byte(bgCol shr  8)]) shl  8)
                or (TColor32(R[Byte(fgCol)       ] + InvR[Byte(bgCol)       ])       );
        end;
      end;
    end;

    inc(width);
  end;
end;}
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}
//------------------------------------------------------------------------------

function BlendMask(bgColor, alphaMask: TColor32): TColor32;
var
  a: byte;
begin
  a := MulTable[bgColor shr 24, alphaMask shr 24];
  if a <> 0 then Result := (TColor32(a) shl 24) or (bgColor and $00FFFFFF)
  else Result := 0;
end;
//------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index is used

procedure BlendMaskLine(bgColor, alphaMask: PColor32; width: nativeint);
label
  SkipNone32;
var
  a: byte;
begin
  // Use the negative offset trick to only increment the array "width"
  // until it reaches zero. And by offsetting the arrays by "width",
  // the negative "width" values also becomes the index into these arrays.
  inc(bgColor, width);
  inc(alphaMask, width);
  width := -width;

  // Handle special cases Alpha=0 or 255 as those are the most
  // common values.
  while width < 0 do
  begin
    // MulTable[0, fgA] -> 0, if bgColor is already 0 => skip
    while PStaticARGBArray(bgColor)[width].Color = 0 do
    begin
SkipNone32:
      inc(width);
      if width = 0 then exit;
    end;
    a := PStaticARGBArray(bgColor)[width].A;
    // MulTable[0, fgA] -> 0 => replace color with 0
    while a = 0 do
    begin
      PStaticColor32Array(bgColor)[width] := 0;
      inc(width);
      if width = 0 then exit;
      if PStaticARGBArray(bgColor)[width].Color = 0 then
        goto SkipNone32;
      a := PStaticARGBArray(bgColor)[width].A;
    end;
    // MulTable[255, fgA] -> fgA => replace alpha with fgA
    while a = 255 do
    begin
      PStaticARGBArray(bgColor)[width].A := PStaticARGBArray(alphaMask)[width].A;
      inc(width);
      if width = 0 then exit;
      a := PStaticARGBArray(bgColor)[width].A;
    end;

    a := PStaticARGBArray(alphaMask)[width].A;
    // MulTable[bgA, 0] -> 0 => replace color with 0
    while a = 0 do
    begin
      PStaticColor32Array(bgColor)[width] := 0;
      inc(width);
      if width = 0 then exit;
      a := PStaticARGBArray(alphaMask)[width].A;
    end;
    // MulTable[bgA, 255] -> bgA => nothing to do
    while a = 255 do
    begin
      inc(width);
      if width = 0 then exit;
      a := PStaticARGBArray(alphaMask)[width].A;
    end;

    a := MulTable[PStaticARGBArray(bgColor)[width].A, a];
    if a <> 0 then PStaticARGBArray(bgColor)[width].A := a
    else PStaticColor32Array(bgColor)[width] := 0;

    inc(width);
  end;
end;
//------------------------------------------------------------------------------

{
// reference implementation
procedure BlendMaskLine(bgColor, alphaMask: PColor32; width: nativeint);
var
  a: byte;
begin
  // Use the negative offset trick to only increment the array "width"
  // until it reaches zero. And by offsetting the arrays by "width",
  // the negative "width" values also becomes the index into these arrays.
  inc(bgColor, width);
  inc(alphaMask, width);
  width := -width;

  while width < 0 do
  begin
    a := MulTable[PStaticARGBArray(bgColor)[width].A,
                  PStaticARGBArray(alphaMask)[width].A];
    if a = 0 then PStaticColor32Array(bgColor)[width] := 0
    else PStaticARGBArray(bgColor)[width].A := a;

    inc(width);
  end;
end;}
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}
//------------------------------------------------------------------------------

function BlendAltMask(bgColor, alphaMask: TColor32): TColor32;
var
  a: byte;
begin
  a := MulTable[bgColor shr 24, (alphaMask shr 24) xor 255];
  if a <> 0 then Result := (TColor32(a) shl 24) or (bgColor and $00FFFFFF)
  else Result := 0;
end;
//------------------------------------------------------------------------------

function BlendDifference(color1, color2: TColor32): TColor32;
var
  fgA, bgA: byte;
begin
  fgA := color2 shr 24;
  bgA := color1 shr 24;
  if fgA = 0 then Result := color1
  else if bgA = 0 then Result := color2
  else
  begin
    Result := TColor32(MulTable[(fgA xor 255), (bgA xor 255)] xor 255) shl 24
              or (TColor32(Abs(Byte(color2 shr 16) - Byte(color1 shr 16))) shl 16)
              or (TColor32(Abs(Byte(color2 shr  8) - Byte(color1 shr  8))) shl  8)
              or (TColor32(Abs(Byte(color2       ) - Byte(color1       )))       );
  end;
end;
//------------------------------------------------------------------------------

function BlendSubtract(bgColor, fgColor: TColor32): TColor32;
var
  fgA, bgA: byte;
begin
  fgA := fgColor shr 24;
  bgA := bgColor shr 24;
  if fgA = 0 then Result := bgColor
  else if bgA = 0 then Result := fgColor
  else
  begin
    Result := TColor32(MulTable[(fgA xor 255), (bgA xor 255)] xor 255) shl 24
              or (TColor32(ClampByte(Byte(fgColor shr 16) - Byte(bgColor shr 16))) shl 16)
              or (TColor32(ClampByte(Byte(fgColor shr 8 ) - Byte(bgColor shr  8))) shl  8)
              or (TColor32(ClampByte(Byte(fgColor       ) - Byte(bgColor       )))       );
  end;
end;
//------------------------------------------------------------------------------

function BlendLighten(bgColor, fgColor: TColor32): TColor32;
var
  fgA, bgA: byte;
begin
  fgA := fgColor shr 24;
  bgA := bgColor shr 24;
  if fgA = 0 then Result := bgColor
  else if bgA = 0 then Result := fgColor
  else
  begin
    Result := TColor32(MulTable[(fgA xor 255), (bgA xor 255)] xor 255) shl 24
              or (TColor32(Max(Byte(fgColor shr 16), Byte(bgColor shr 16))) shl 16)
              or (TColor32(Max(Byte(fgColor shr 8 ), Byte(bgColor shr  8))) shl  8)
              or (TColor32(Max(Byte(fgColor       ), Byte(bgColor       )))       );
  end;
end;
//------------------------------------------------------------------------------

function BlendDarken(bgColor, fgColor: TColor32): TColor32;
var
  fgA, bgA: byte;
begin
  fgA := fgColor shr 24;
  bgA := bgColor shr 24;
  if fgA = 0 then Result := bgColor
  else if bgA = 0 then Result := fgColor
  else
  begin
    Result := TColor32(MulTable[(fgA xor 255), (bgA xor 255)] xor 255) shl 24
              or (TColor32(Min(Byte(fgColor shr 16), Byte(bgColor shr 16))) shl 16)
              or (TColor32(Min(Byte(fgColor shr 8 ), Byte(bgColor shr  8))) shl  8)
              or (TColor32(Min(Byte(fgColor       ), Byte(bgColor       )))       );
  end;
end;
//------------------------------------------------------------------------------

function BlendBlueChannel(bgColor, blueMask: TColor32): TColor32;
begin
  Result := (bgColor and $00FFFFFF) or
            (TColor32(MulTable[bgColor shr 24, blueMask shr 24]) shl 24);
end;
//------------------------------------------------------------------------------

procedure BlendBlueChannelLine(bgColor, blueMask: PColor32; width: nativeint);
begin
  while width > 0 do
  begin
    PARGB(bgColor).A := MulTable[PARGB(bgColor).A, PARGB(blueMask).A];
    inc(bgColor);
    inc(blueMask);
    dec(width);
  end;
end;
//------------------------------------------------------------------------------

function BlendInvertedMask(bgColor, alphaMask: TColor32): TColor32;
var
  a: byte;
begin
  a := MulTable[bgColor shr 24, (alphaMask shr 24) xor 255];
  if a < 2 then Result := 0
  else Result := (bgColor and $00FFFFFF) or (TColor32(a) shl 24);
end;
//------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index is used

procedure BlendInvertedMaskLine(bgColor, alphaMask: PColor32; width: nativeint);
var
  a: byte;
begin
  // Use the negative offset trick to only increment the array "width"
  // until it reaches zero. And by offsetting the arrays by "width",
  // the negative "width" values also becomes the index into these arrays.
  inc(bgColor, width);
  inc(alphaMask, width);
  width := -width;

  while width < 0 do
  begin
    a := MulTable[PStaticARGBArray(bgColor)[width].A,
                  PStaticARGBArray(alphaMask)[width].A xor 255];
    if a < 2 then PStaticColor32Array(bgColor)[width] := 0
    else PStaticARGBArray(bgColor)[width].A := a;

    inc(width);
  end;
end;
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}

//------------------------------------------------------------------------------
// Compare functions (see ConvertToBoolMask, FloodFill & Vectorize)
//------------------------------------------------------------------------------

function CompareRGB(master, current: TColor32; tolerance: Integer): Boolean;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
begin
  if curr.A < $80 then
    Result := false
  else if (master and $FFFFFF) = (current and $FFFFFF) then
    Result := true
  else if tolerance = 0 then
    Result := false
  else result :=
    (Abs(curr.R - mast.R) <= tolerance) and
    (Abs(curr.G - mast.G) <= tolerance) and
    (Abs(curr.B - mast.B) <= tolerance);
end;
//------------------------------------------------------------------------------

function CompareAlpha(master, current: TColor32; tolerance: Integer): Boolean;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
begin
  if mast.A = curr.A then Result := true
  else if tolerance = 0 then Result := false
  else result := Abs(curr.A - mast.A) <= tolerance;
end;
//------------------------------------------------------------------------------

function CompareHue(master, current: TColor32; tolerance: Integer): Boolean;
var
  curr, mast: THsl;
  val: Integer;
begin
  if TARGB(current).A < $80 then
  begin
    Result := false;
    Exit;
  end;
  curr := RgbToHsl(current);
  mast := RgbToHsl(master);
  if curr.hue > mast.hue then
  begin
    val := curr.hue - mast.hue;
    if val > 127 then val := mast.hue - curr.hue + 255;
  end else
  begin
    val := mast.hue - curr.hue;
    if val > 127 then val := curr.hue - mast.hue + 255;
  end;
  result := val <= tolerance;
end;

//------------------------------------------------------------------------------
// CompareEx functions (see ConvertToAlphaMask)
//------------------------------------------------------------------------------

function CompareRgbEx(master, current: TColor32): Byte;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
  res: Cardinal;
begin
  res := Sqr(mast.R - curr.R) + Sqr(mast.G - curr.G) + Sqr(mast.B - curr.B);
  if res >= 65025 then result := 255
  else result := Round(Sqrt(res));
end;
//------------------------------------------------------------------------------

function CompareAlphaEx(master, current: TColor32): Byte;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
begin
  Result := abs(mast.A - curr.A);
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function IsAlphaChar(c: Char): Boolean;
begin
  Result := ((c >= 'A') and (c <= 'Z')) or ((c >= 'a') and (c <= 'z'));
end;
//------------------------------------------------------------------------------

procedure RectWidthHeight(const rec: TRect; out width, height: Integer);
begin
  width := rec.Right - rec.Left;
  height := rec.Bottom - rec.Top;
end;
//------------------------------------------------------------------------------

function RectWidth(const rec: TRect): Integer;
begin
  Result := rec.Right - rec.Left;
end;
//------------------------------------------------------------------------------

function RectHeight(const rec: TRect): Integer;
begin
  Result := rec.Bottom - rec.Top;
end;
//------------------------------------------------------------------------------

function IsEmptyRect(const rec: TRect): Boolean;
begin
  Result := (rec.Right <= rec.Left) or (rec.Bottom <= rec.Top);
end;
//------------------------------------------------------------------------------

function IsEmptyRect(const rec: TRectD): Boolean;
begin
  Result := (rec.Right <= rec.Left) or (rec.Bottom <= rec.Top);
end;
//------------------------------------------------------------------------------

function InvertColor(color: TColor32): TColor32;
begin
  Result := color xor $00FFFFFF;
end;
//------------------------------------------------------------------------------

function GetAlpha(color: TColor32): Byte;
begin
  Result := Byte(color shr 24);
end;
//------------------------------------------------------------------------------

function RGBColor(color: TColor32): Cardinal;
var
  c  : TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.R := c.B; res.G := c.G; res.B := c.R; res.A := 0;
end;
//------------------------------------------------------------------------------

function Color32(a, r, g, b: Byte): TColor32;
var
  res: TARGB absolute Result;
begin
  res.A := a; res.R := r; res.G := g; res.B := b;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function Color32(rgbColor: Integer): TColor32;
var
  res: TARGB absolute Result;
begin
  if rgbColor < 0 then
    result := GetSysColor(rgbColor and $FFFFFF) else
    result := rgbColor;
  res.A := res.B; res.B := res.R; res.R := res.A; //byte swap
  res.A := 255;
end;
//------------------------------------------------------------------------------

procedure FixPalette(p: PARGB; count: integer);
var
  i: integer;
begin
  for i := 1 to count do
  begin
    p.Color := SwapRedBlue(p.Color);
    p.A := 255;
    inc(p);
  end;
end;
//------------------------------------------------------------------------------

function Get32bitBitmapInfoHeader(width, height: Integer): TBitmapInfoHeader;
begin
  FillChar(Result, sizeof(Result), #0);
  Result.biSize := sizeof(TBitmapInfoHeader);
  Result.biWidth := width;
  Result.biHeight := height;
  Result.biPlanes := 1;
  Result.biBitCount := 32;
  Result.biSizeImage := width * Abs(height) * SizeOf(TColor32);
  Result.biCompression := BI_RGB;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function DPIAware(val: Integer): Integer;
begin
  result := Round(val * DpiAwareOne);
end;
//------------------------------------------------------------------------------

function DPIAware(val: double): double;
begin
  result := val * DpiAwareOne;
end;
//------------------------------------------------------------------------------

function DPIAware(const pt: TPoint): TPoint;
begin
  result.X := Round(pt.X * DpiAwareOne);
  result.Y := Round(pt.Y * DpiAwareOne);
end;
//------------------------------------------------------------------------------

function DPIAware(const pt: TPointD): TPointD;
begin
  result.X := pt.X * DpiAwareOne;
  result.Y := pt.Y * DpiAwareOne;
end;
//------------------------------------------------------------------------------

function DPIAware(const rec: TRect): TRect;
begin
  result.Left := Round(rec.Left * DpiAwareOne);
  result.Top := Round(rec.Top * DpiAwareOne);
  result.Right := Round(rec.Right * DpiAwareOne);
  result.Bottom := Round(rec.Bottom * DpiAwareOne);
end;
//------------------------------------------------------------------------------

function DPIAware(const rec: TRectD): TRectD;
begin
  result.Left := rec.Left * DpiAwareOne;
  result.Top := rec.Top * DpiAwareOne;
  result.Right := rec.Right * DpiAwareOne;
  result.Bottom := rec.Bottom * DpiAwareOne;
end;
//------------------------------------------------------------------------------

function GrayScale(color: TColor32): TColor32;
var
  c: TARGB absolute color;
  r: TARGB absolute result;
  g: Byte;
begin
  //https://www.w3.org/TR/AERT/#color-contrast
  g := ClampByte(0.299 * c.R + 0.587 * c.G + 0.114 * c.B);
  r.A := c.A;
  r.R := g; r.G := g; r.B := g;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: Integer): Integer;
begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: double): double;
begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

procedure ScaleRect(var rec: TRect; x,y: double);
begin
  rec.Right := rec.Left + Round((rec.Right - rec.Left) * x);
  rec.Bottom := rec.Top + Round((rec.Bottom - rec.Top) * y);
end;
//------------------------------------------------------------------------------

function IncPColor32(pc: Pointer; cnt: Integer): PColor32;
begin
  result := PColor32(PByte(pc) + cnt * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;
//------------------------------------------------------------------------------

function GetBoolMask(img: TImage32; reference: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;
var
  i: integer;
  pa: PByte;
  pc: PColor32;
begin
  result := nil;
  if not assigned(img) or img.IsEmpty then Exit;
  if not Assigned(compareFunc) then compareFunc := CompareRGB;
  NewByteArray(Result, img.Width * img.Height, True);
  pa := @Result[0];
  pc := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if compareFunc(reference, pc^, tolerance) then
  {$IFDEF PBYTE}
      pa^ := 1 else
      pa^ := 0;
  {$ELSE}
      pa^ := #1 else
      pa^ := #0;
  {$ENDIF}
    inc(pc); inc(pa);
  end;
end;
//------------------------------------------------------------------------------

function GetColorMask(img: TImage32; reference: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfColor32;
var
  i: integer;
  pDstPxl: PColor32;
  pSrcPxl: PColor32;
begin
  result := nil;
  if not assigned(img) or img.IsEmpty then Exit;
  if not Assigned(compareFunc) then compareFunc := CompareRGB;
  NewColor32Array(Result, img.Width * img.Height, True);
  pDstPxl := @Result[0];
  pSrcPxl := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if compareFunc(reference, pSrcPxl^, tolerance) then
      pDstPxl^ := clWhite32 else
      pDstPxl^ := clBlack32;
    inc(pSrcPxl); inc(pDstPxl);
  end;
end;
//------------------------------------------------------------------------------

function GetAlphaEx(master, current: TColor32): Byte;
{$IFDEF INLINE} inline; {$ENDIF}
var
  curr: TARGB absolute current;
begin
  result := curr.A; //nb: 'master' is ignored
end;
//------------------------------------------------------------------------------

function GetByteMask(img: TImage32; reference: TColor32;
  compareFunc: TCompareFunctionEx): TArrayOfByte;
var
  i: integer;
  pa: PByte;
  pc: PColor32;
begin
  result := nil;
  if not assigned(img) or img.IsEmpty then Exit;
  if not Assigned(compareFunc) then compareFunc := GetAlphaEx;
  NewByteArray(Result, img.Width * img.Height, True);
  pa := @Result[0];
  pc := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    {$IFDEF PBYTE}
    pa^ := compareFunc(reference, pc^);
    {$ELSE}
    pa^ := Char(compareFunc(reference, pc^));
    {$ENDIF}
    inc(pc); inc(pa);
  end;
end;
//------------------------------------------------------------------------------

function RgbToHsl(color: TColor32): THsl;
var
  rgba: TARGB absolute color;
  hsl: THsl absolute result;
  r,g,b: byte;
  maxRGB, minRGB, mAdd, mSub: Integer;
begin
  //https://en.wikipedia.org/wiki/HSL_and_HSV and
  //http://en.wikipedia.org/wiki/HSL_color_space
{$IF DEFINED(ANDROID)}
  color := SwapRedBlue(color);
{$IFEND}

  r := rgba.R; g := rgba.G; b := rgba.B;
  maxRGB := Max(r, Max(g, b));
  minRGB := Min(r, Min(g, b));
  mAdd := maxRGB + minRGB;
  hsl.lum := mAdd shr 1;
  hsl.alpha := rgba.A;

  if maxRGB = minRGB then
  begin
    hsl.hue := 0; //hsl.hue is undefined when gray
    hsl.sat := 0;
    Exit;
  end;

  mSub := maxRGB - minRGB;
  if mAdd <= 255 then
    hsl.sat := DivTable[mSub, mAdd] else
    hsl.sat := DivTable[mSub, 511 - mAdd];

  mSub := mSub * 6;
  if r = maxRGB then
  begin
    if g >= b then
      hsl.hue := (g - b) * 255 div mSub else
      hsl.hue := 255 - ((b - g) * 255 div mSub);
  end
  else if G = maxRGB then
  begin
    if b > r then
      hsl.hue := 85 + (b - r) * 255 div mSub else
      hsl.hue := 85 - (r - b)  * 255 div mSub;
  end else
  begin
    if r > g then
      hsl.hue := 170 + (r - g)  * 255 div mSub else
      hsl.hue := 170 - (g - r)  * 255 div mSub;
  end;
end;
//------------------------------------------------------------------------------

function HslToRgb(hslColor: THsl): TColor32;
var
  rgba: TARGB absolute result;
  hsl: THsl absolute hslColor;
  c, x, m, a: Integer;
begin
  //formula from https://www.rapidtables.com/convert/color/hsl-to-rgb.html
  c := ((255 - abs(2 * hsl.lum - 255)) * hsl.sat) shr 8;
  a := 252 - (hsl.hue mod 85) * 6;
  x := (c * (255 - abs(a))) shr 8;
  m := hsl.lum - c shr 1{div 2}; // Delphi's 64bit compiler can't optimize this
  rgba.A := hsl.alpha;
  case (hsl.hue * 6) shr 8 of
    0: begin rgba.R := c + m; rgba.G := x + m; rgba.B := 0 + m; end;
    1: begin rgba.R := x + m; rgba.G := c + m; rgba.B := 0 + m; end;
    2: begin rgba.R := 0 + m; rgba.G := c + m; rgba.B := x + m; end;
    3: begin rgba.R := 0 + m; rgba.G := x + m; rgba.B := c + m; end;
    4: begin rgba.R := x + m; rgba.G := 0 + m; rgba.B := c + m; end;
    5: begin rgba.R := c + m; rgba.G := 0 + m; rgba.B := x + m; end;
  end;
{$IF DEFINED(ANDROID)}
  Result := SwapRedBlue(Result);
{$IFEND}
end;
//------------------------------------------------------------------------------

function AdjustHue(color: TColor32; percent: Integer): TColor32;
var
  hsl: THsl;
begin
  percent := percent mod 100;
  if percent < 0 then inc(percent, 100);
  hsl := RgbToHsl(color);
  hsl.hue := (hsl.hue + Round(percent*255/100)) mod 256;
  result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

function ArrayOfColor32ToArrayHSL(const clr32Arr: TArrayOfColor32): TArrayofHSL;
var
  i, len: Integer;
begin
  len := length(clr32Arr);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := RgbToHsl(clr32Arr[i]);
end;
//------------------------------------------------------------------------------

function ArrayOfHSLToArrayColor32(const hslArr: TArrayofHSL): TArrayOfColor32;
var
  i, len: Integer;
begin
  len := length(hslArr);
  NewColor32Array(result, len, True);
  for i := 0 to len -1 do
    result[i] := HslToRgb(hslArr[i]);
end;
//------------------------------------------------------------------------------

function NameToId(Name: PChar): Longint;
begin
  if Cardinal(PWord(Name)) < 30 then
  begin
    Result := Cardinal(PWord(Name))
  end else
  begin
    if Name^ = '#' then inc(Name);
    Result := StrToIntDef(Name, 0);
    if Result > 65535 then Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function CreateResourceStream(const resName: string;
  resType: PChar): TResourceStream;
var
  nameId, typeId: Cardinal;
begin
  Result := nil;
  typeId := NameToId(resType);
  if (typeId > 0) then resType := PChar(typeId)
  else if (resType = 'BMP') then resType := RT_BITMAP;

  nameId := NameToId(PChar(resName));
  if nameId > 0 then
  begin
    if FindResource(hInstance, PChar(nameId), resType) <> 0 then
      Result := TResourceStream.CreateFromID(hInstance, nameId, resType);
  end else
  begin
    if FindResource(hInstance, PChar(resName), resType) <> 0 then
      Result := TResourceStream.Create(hInstance, PChar(resName), resType);
  end;
end;

//------------------------------------------------------------------------------
// TRectD methods (and helpers)
//------------------------------------------------------------------------------

function TRectD.IsEmpty: Boolean;
begin
  result := (right <= left) or (bottom <= top);
end;
//------------------------------------------------------------------------------

function TRectD.Width: double;
begin
  result := Max(0, right - left);
end;
//------------------------------------------------------------------------------

function TRectD.Height: double;
begin
  result := Max(0, bottom - top);
end;
//------------------------------------------------------------------------------

function TRectD.MidPoint: TPointD;
begin
  Result.X := (Right + Left)/2;
  Result.Y := (Bottom + Top)/2;
end;
//------------------------------------------------------------------------------

{$IFNDEF RECORD_METHODS}
function TRectD.TopLeft: TPointD;
begin
  Result.X := Left;
  Result.Y := Top;
end;
//------------------------------------------------------------------------------

function TRectD.BottomRight: TPointD;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TRectD.Normalize: Boolean;
var
  d: double;
begin
  Result := false;
  if Left > Right then
  begin
    d := Left;
    Left := Right;
    Right := d;
    Result := True;
  end;
  if Top > Bottom then
  begin
    d := Top;
    Top := Bottom;
    Bottom := d;
    Result := True;
  end;
end;
//------------------------------------------------------------------------------

function TRectD.Contains(const Pt: TPoint): Boolean;
begin
  Result := (pt.X >= Left) and (pt.X < Right) and
    (pt.Y >= Top) and (pt.Y < Bottom);
end;
//------------------------------------------------------------------------------

function TRectD.Contains(const Pt: TPointD): Boolean;
begin
  Result := (pt.X >= Left) and (pt.X < Right) and
    (pt.Y >= Top) and (pt.Y < Bottom);
end;
//------------------------------------------------------------------------------

function RectD(left, top, right, bottom: double): TRectD;
begin
  result.Left := left;
  result.Top := top;
  result.Right := right;
  result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function RectD(const rec: TRect): TRectD;
begin
  with rec do
  begin
    result.Left := left;
    result.Top := top;
    result.Right := right;
    result.Bottom := bottom;
  end;
end;

//------------------------------------------------------------------------------
// TImage32 methods
//------------------------------------------------------------------------------

constructor TImage32.Create(width: Integer; height: Integer);
begin
  fAntiAliased := true;
  fResampler := DefaultResampler;
  fwidth := Max(0, width);
  fheight := Max(0, height);
  NewColor32Array(fPixels, fwidth * fheight);
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(const src: TArrayOfColor32; width: Integer; height: Integer);
begin
  fAntiAliased := true;
  fResampler := DefaultResampler;

  width := Max(0, width);
  height := Max(0, height);
  if Length(src) <> width * height then
    raise Exception.Create(rsInvalidImageArrayData);

  fWidth := width;
  fHeight := height;
  fPixels := src;
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32);
begin
  Assign(src);
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32; const srcRec: TRect);
var
  rec: TRect;
begin
  fAntiAliased := src.AntiAliased;
  fResampler := src.fResampler;
  types.IntersectRect(rec, src.Bounds, srcRec);
  RectWidthHeight(rec, fWidth, fHeight);
  if (fWidth = 0) or (fheight = 0) then Exit;
  fPixels := src.CopyPixels(rec);
end;
//------------------------------------------------------------------------------

destructor TImage32.Destroy;
begin
  fPixels := nil;
  inherited;
end;
//------------------------------------------------------------------------------

class function TImage32.IsRegisteredFormat(const ext: string): Boolean;
begin
  result := Assigned(TImage32.GetImageFormatClass(ext));
end;
//------------------------------------------------------------------------------

class procedure TImage32.RegisterImageFormatClass(ext: string;
  bm32ExClass: TImageFormatClass; clipPriority: TClipboardPriority);
var
  i: Integer;
  imgFmtRec: PImgFmtRec;
  isNewFormat: Boolean;
begin
  if not Assigned(ImageFormatClassList) then CreateImageFormatList;

  if (ext = '') or (ext = '.') then Exit;
  if (ext[1] = '.') then Delete(ext, 1,1);
  if not IsAlphaChar(ext[1]) then Exit;
  isNewFormat := true;

  // avoid duplicates but still allow overriding
  for i := 0 to imageFormatClassList.count -1 do
  begin
    imgFmtRec := PImgFmtRec(imageFormatClassList[i]);
    if SameText(imgFmtRec.Fmt, ext) then
    begin
      imgFmtRec.Obj := bm32ExClass; // replace prior class
      if imgFmtRec.SortOrder = clipPriority then
        Exit; // re-sorting isn't required
      imgFmtRec.SortOrder := clipPriority;
      isNewFormat := false;
      Break;
    end;
  end;

  if isNewFormat then
  begin
    new(imgFmtRec);
    imgFmtRec.Fmt := ext;
    imgFmtRec.SortOrder := clipPriority;
    imgFmtRec.Obj := bm32ExClass;
    ImageFormatClassList.Add(imgFmtRec);
  end;

  // Sort with lower priority before higher.
  // Sorting here is arguably inefficient but, with so few
  // entries, this inefficiency will be inconsequential.

{$IFDEF XPLAT_GENERICS}
  ImageFormatClassList.Sort(TComparer<PImgFmtRec>.Construct(
      function(const imgFmtRec1, imgFmtRec2: PImgFmtRec): Integer
      begin
        Result := Integer(imgFmtRec1.SortOrder) - Integer(imgFmtRec2.SortOrder);
      end));
{$ELSE}
  ImageFormatClassList.Sort(ImageFormatClassListSort);
{$ENDIF}
end;
//------------------------------------------------------------------------------

class function TImage32.GetImageFormatClass(const ext: string): TImageFormatClass;
var
  i: Integer;
  pattern: string;
  imgFmtRec: PImgFmtRec;
begin
  Result := nil;
  pattern := ext;
  if (pattern = '')  or (pattern = '.') then Exit;
  if pattern[1] = '.' then Delete(pattern, 1,1);

  //try for highest priority first
  for i := imageFormatClassList.count -1 downto 0 do
  begin
    imgFmtRec := PImgFmtRec(imageFormatClassList[i]);
    if not SameText(imgFmtRec.Fmt, pattern) then Continue;
    Result := imgFmtRec.Obj;
    break;
  end;
end;
//------------------------------------------------------------------------------

class function TImage32.GetImageFormatClass(stream: TStream): TImageFormatClass;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to imageFormatClassList.count -1 do
    with PImgFmtRec(imageFormatClassList[i])^ do
      if Obj.IsValidImageStream(stream) then
      begin
        Result := Obj;
        break;
      end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Assign(src: TImage32);
begin
  if assigned(src) then
    src.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TImage32.AssignTo(dst: TImage32);
begin
  if dst = self then Exit;
  dst.BeginUpdate;
  try
    dst.AssignSettings(Self);
    try
      dst.fPixels := System.Copy(fPixels, 0, Length(fPixels));
      dst.fWidth := fWidth;
      dst.fHeight := fHeight;
      dst.Resized;
    except
      dst.SetSize(0,0);
    end;
  finally
    dst.EndUpdate;
  end;
  dst.fColorCount := fColorCount; // dst.EndUpdate called ResetColorCount
end;
//------------------------------------------------------------------------------

procedure TImage32.AssignSettings(src: TImage32);
begin
  if assigned(src) and (src <> Self) then
  begin
    BeginUpdate;
    try
      fResampler := src.fResampler;
      fIsPremultiplied := src.fIsPremultiplied;
      fAntiAliased := src.fAntiAliased;
      ResetColorCount;
    finally
      EndUpdate;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.AssignPixelArray(const src: TArrayOfColor32; width: Integer; height: Integer);
var
  wasResized: Boolean;
begin
  width := Max(0, width);
  height := Max(0, height);
  if Length(src) <> width * height then
    raise Exception.Create(rsInvalidImageArrayData);

  wasResized := (fWidth <> width) or (fHeight <> height);

  BeginUpdate;
  try
    fWidth := width;
    fHeight := height;
    fPixels := src;
  finally
    EndUpdate;
  end;

  if wasResized then
    Resized;
end;
//------------------------------------------------------------------------------

procedure TImage32.Changed;
begin
  if fUpdateCnt <> 0 then Exit;
  ResetColorCount;
  if Assigned(fOnChange) then fOnChange(Self);
end;
//------------------------------------------------------------------------------

procedure TImage32.Resized;
begin
  if fUpdateCnt <> 0 then Exit
  else if Assigned(fOnResize) then fOnResize(Self)
  else Changed;
end;
//------------------------------------------------------------------------------

function TImage32.SetPixels(const newPixels: TArrayOfColor32): Boolean;
var
  len: integer;
begin
  len := Length(newPixels);
  Result := (len > 0)and (len = Width * height);
  if Result then fPixels := System.Copy(newPixels, 0, len);
end;
//------------------------------------------------------------------------------

procedure TImage32.BeginUpdate;
begin
  if fNotifyBlockCnt > 0 then Exit;
  inc(fUpdateCnt);
end;
//------------------------------------------------------------------------------

procedure TImage32.EndUpdate;
begin
  if fNotifyBlockCnt > 0 then Exit;
  dec(fUpdateCnt);
  if fUpdateCnt = 0 then Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.BlockNotify;
begin
  inc(fNotifyBlockCnt);
  inc(fUpdateCnt);
end;
//------------------------------------------------------------------------------

procedure TImage32.UnblockNotify;
begin
  dec(fNotifyBlockCnt);
  dec(fUpdateCnt);
end;
//------------------------------------------------------------------------------

procedure TImage32.SetBackgroundColor(bgColor: TColor32);
var
  i: Integer;
  pc: PColor32;
begin
  pc := Pixelbase;
  for i := 0 to high(fPixels) do
  begin
    pc^ := BlendToOpaque(bgColor, pc^);
     inc(pc);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.Clear(color: TColor32);
var
  i: Integer;
  pc: PColor32;
begin
  fIsPremultiplied := false;
  if IsEmpty then Exit;
  if color = clNone32 then
    FillChar(fPixels[0], Width * Height * SizeOf(TColor32), 0)
  else
  begin
    pc := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      pc^ := color;
      inc(pc);
    end;
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.Clear(const rec: TRect; color: TColor32 = 0);
begin
  FillRect(rec, color);
end;
//------------------------------------------------------------------------------

procedure TImage32.FillRect(const rec: TRect; color: TColor32);
var
  i,j, rw, w: Integer;
  c: PColor32;
  r: TRect;
begin
  Types.IntersectRect(r, rec, bounds);
  if IsEmptyRect(r) then Exit;
  rw := RectWidth(r);
  w := Width;
  c := @Pixels[r.Top * w + r.Left];

  if (color = 0) and (w = rw) then
    FillChar(c^, (r.Bottom - r.Top) * rw * SizeOf(TColor32), 0)
  else if rw = 1 then
  begin
    for i := r.Top to r.Bottom -1 do
    begin
      c^ := color;
      inc(c, w);
    end;
  end
  else if (color = 0) and (rw > 15) then
  begin
    for i := r.Top to r.Bottom -1 do
    begin
      FillChar(c^, rw * SizeOf(TColor32), 0);
      inc(c, w);
    end;
  end
  else
  begin
    for i := r.Top to r.Bottom -1 do
    begin
      for j := 1 to rw do
      begin
        c^ := color;
        inc(c);
      end;
      inc(c, w - rw);
    end;
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.ResetColorCount;
begin
  fColorCount := 0;
end;
//------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index is used

function TImage32.RectHasTransparency(const rec: TRect): Boolean;
var
  i, j, rw: Integer;
  lineByteOffset: nativeint;
  c: PARGB;
  r: TRect;
begin
  Result := True;
  Types.IntersectRect(r, rec, bounds);
  if IsEmptyRect(r) then Exit;
  rw := RectWidth(r);
  c := @Pixels[r.Top * Width + r.Left];

  if rw = Width then // we can use one loop
  begin
    i := (r.Bottom - r.Top) * rw;
    inc(c, i);
    i := -i;
    while i < 0 do
    begin
      if PStaticARGBArray(c)[i].A < 254 then Exit;
      inc(i);
    end;
  end
  else
  begin
    lineByteOffset := (Width - rw) * SizeOf(TColor32);
    for i := r.Top to r.Bottom -1 do
    begin
      for j := 1 to rw do
      begin
        if c.A < 254 then Exit;
        inc(c);
      end;
      inc(PByte(c), lineByteOffset);
    end;
  end;
  Result := False;
end;
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}
//------------------------------------------------------------------------------

procedure CheckBlendFill(pc: PColor32; color: TColor32);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  if not assigned(pc) then Exit;
  pc^ := BlendToAlpha(pc^, color);
end;
//------------------------------------------------------------------------------

function TImage32.CopyPixels(const rec: TRect): TArrayOfColor32;
var
  i, clipW, w,h: Integer;
  pSrc, pDst, pDst2: PColor32;
  recClipped: TRect;
begin
  RectWidthHeight(rec, w,h);
  NewColor32Array(result, w * h, True);

  if w * h = 0 then Exit;
  Types.IntersectRect(recClipped, rec, Bounds);
  //if recClipped is wholely outside the bounds of the image ...
  if IsEmptyRect(recClipped) then
  begin
    //rec is considered valid even when completely outside the image bounds,
    //and so when that happens we simply return a fully transparent image ...
    FillChar(Result[0], w * h * SizeOf(TColor32), 0);
    Exit;
  end;

  //if recClipped is wholely within the bounds of the image ...
  if RectsEqual(recClipped, rec) then
  begin
    pDst := @Result[0];
    pSrc := @fPixels[recClipped.Top * Width + rec.Left];
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, Width); inc(pDst, w);
    end;
    Exit;
  end;

  //a part of 'rec' must be outside the bounds of the image ...

  pDst := @Result[0];
  for i := rec.Top to -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
  pSrc := @fPixels[recClipped.Top * Width + Max(0,rec.Left)];
  if (rec.Left < 0) or (rec.Right > Width) then
  begin
    clipW := RectWidth(recClipped);
    pDst2 := IncPColor32(pDst, -Min(0, rec.Left));
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      //when rec.left < 0 or rec.right > width it's simplest to
      //start with a prefilled row of transparent pixels
      FillChar(pDst^, w * SizeOf(TColor32), 0);
      Move(pSrc^, pDst2^, clipW * SizeOf(TColor32));
      inc(pDst, w); inc(pDst2, w); inc(pSrc, Width);
    end;
  end else
  begin
    //things are simpler when there's no part of 'rec' is
    //outside the image, at least not on the left or right sides ...
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, Width); inc(pDst, w);
    end;
  end;
  for i := Height to rec.Bottom -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Crop(const rec: TRect);
var
  newPixels: TArrayOfColor32;
  w,h: integer;
begin
  RectWidthHeight(rec, w, h);
  if (w = Width) and (h = Height) then Exit;
  newPixels := CopyPixels(rec); // get pixels **before** resizing
  BlockNotify;
  try
    SetSize(w, h);
    if not IsEmptyRect(rec) then
      fPixels := newPixels;
  finally
    UnblockNotify;
  end;
  Resized;
end;
//------------------------------------------------------------------------------

function TImage32.GetBounds: TRect;
begin
  result := Types.Rect(0, 0, Width, Height);
end;
//------------------------------------------------------------------------------

function TImage32.GetMidPoint: TPointD;
begin
  Result := PointD(fWidth * 0.5, fHeight * 0.5);
end;
//------------------------------------------------------------------------------

procedure TImage32.SetSize(newWidth, newHeight: Integer; color: TColor32);
begin
  //very large images are usually due to a bug
  if (newWidth > 20000) or (newHeight > 20000) then
    raise Exception.Create(rsImageTooLarge);
  fwidth := Max(0, newWidth);
  fheight := Max(0, newHeight);
  fPixels := nil; //forces a blank image
  NewColor32Array(fPixels, fwidth * fheight, True);
  fIsPremultiplied := false;
  BlockNotify;
  Clear(color);
  UnblockNotify;
  Resized;
end;
//------------------------------------------------------------------------------

procedure TImage32.Resize(newWidth, newHeight: Integer);
begin
  ResizeTo(Self, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure TImage32.ResizeTo(targetImg: TImage32; newWidth, newHeight: Integer);
begin
  if (newWidth <= 0) or (newHeight <= 0) then
  begin
    targetImg.SetSize(0, 0);
    Exit;
  end
  else if (newWidth = fwidth) and (newHeight = fheight) then
  begin
    if targetImg <> Self then targetImg.Assign(Self);
    Exit
  end
  else if IsEmpty then
  begin
    targetImg.SetSize(newWidth, newHeight);
    Exit;
  end;

  targetImg.BlockNotify;
  try
    if targetImg.fResampler <= rNearestResampler then
      NearestNeighborResize(Self, targetImg, newWidth, newHeight)
    else
      ResamplerResize(Self, targetImg, newWidth, newHeight);
  finally
    targetImg.UnblockNotify;
  end;
  targetImg.Resized;
end;
//------------------------------------------------------------------------------

procedure TImage32.Scale(s: double);
begin
  Scale(s, s);
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleTo(targetImg: TImage32; s: double);
begin
  ScaleTo(targetImg, s, s);
end;
//------------------------------------------------------------------------------

procedure TImage32.Scale(sx, sy: double);
begin
  if (sx > 0) and (sy > 0) then
    Resize(Round(width * sx), Round(height * sy));
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleTo(targetImg: TImage32; sx, sy: double);
begin
  if (sx > 0) and (sy > 0) then
    ResizeTo(targetImg, Round(width * sx), Round(height * sy));
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleToFit(width, height: integer);
var
  sx, sy: double;
begin
  if IsEmpty or (width < 2) or (height < 2) then Exit;
  sx := width / self.Width;
  sy := height / self.Height;
  if sx <= sy then
    Scale(sx) else
    Scale(sy);
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleToFitCentered(const rect: TRect);
begin
  ScaleToFitCentered(RectWidth(rect), RectHeight(rect));
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleToFitCentered(width, height: integer);
var
  sx, sy: double;
  tmp: TImage32;
  rec2: TRect;
begin
  if IsEmpty or (width <= 0) or (height <= 0) or
    ((width = self.Width) and (height = self.Height)) then Exit;

  sx := width / self.Width;
  sy := height / self.Height;
  BlockNotify;
  try
    if sx <= sy then
    begin
      Scale(sx);
      if height = self.Height then Exit;
      rec2 := Bounds;
      TranslateRect(rec2, 0, (height - self.Height) div 2);
      tmp := TImage32.Create(self);
      try
        SetSize(width, height);
        CopyInternal(tmp, tmp.Bounds, rec2, nil);
      finally
        tmp.Free;
      end;
    end else
    begin
      Scale(sy);
      if width = self.Width then Exit;
      rec2 := Bounds;
      TranslateRect(rec2, (width - self.Width) div 2, 0);
      tmp := TImage32.Create(self);
      try
        SetSize(width, height);
        CopyInternal(tmp, tmp.Bounds, rec2, nil);
      finally
        tmp.Free;
      end;
    end;
  finally
    UnblockNotify;
  end;
  Resized;
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateLeft90;
var
  x,y, xx: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;

  BeginUpdate;
  tmp := TImage32.create(Self);
  try
    SetSize(Height, Width);
    xx := (width - 1) * Height;
    dst := PixelBase;
    for y := 0 to Height -1 do
    begin
      src := @tmp.Pixels[xx + y];
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); dec(src, Height);
      end;
    end;
  finally
    tmp.Free;
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateRight90;
var
  x,y: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;

  BeginUpdate;
  tmp := TImage32.create(Self);
  try
    SetSize(Height, Width);
    dst := PixelBase;
    for y := 0 to Height -1 do
    begin
      src := @tmp.Pixels[Height -1 - y];
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); inc(src, Height);
      end;
    end;
  finally
    tmp.Free;
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Rotate180;
var
  x,y: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;
  tmp := TImage32.create(Self);
  try
    dst := PixelBase;
    src := @tmp.Pixels[Width * Height -1];
    for y := 0 to Height -1 do
    begin
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); dec(src);
      end;
    end;
  finally
    tmp.Free;
  end;
  Changed;
end;
//------------------------------------------------------------------------------

function TImage32.GetColorCount: Integer;
var
  allColors: PByteArray;
  i: Integer;
  c: PColor32;
const
  cube256 = 256 * 256 * 256;
begin
  result := 0;
  if IsEmpty then Exit;
  if fColorCount > 0 then
  begin
    result := fColorCount;
    Exit;
  end;
  //because 'allColors' uses quite a chunk of memory, it's
  //allocated on the heap rather than the stack
  allColors := AllocMem(cube256); //nb: zero initialized
  try
    c := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      //ignore colors with signifcant transparency
      if GetAlpha(c^)  > $80 then
        allColors[c^ and $FFFFFF] := 1;
      inc(c);
    end;
    for i := 0 to cube256 -1 do
      if allColors[i] = 1 then inc(Result);
  finally
    FreeMem(allColors);
  end;
  fColorCount := Result; //avoids repeating the above unnecessarily
end;
//------------------------------------------------------------------------------

function TImage32.GetHasTransparency: Boolean;
var
  i: Integer;
  pc: PARGB;
begin
  result := true;
  If IsEmpty then Exit;
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    if pc.A < 128 then Exit;
    inc(pc);
  end;
  result := false;
end;
//------------------------------------------------------------------------------

function TImage32.SaveToFile(filename: string; quality: integer = 0): Boolean;
var
  fileFormatClass: TImageFormatClass;
begin
  result := false;
  if IsEmpty or (length(filename) < 5) then Exit;
  //use the process's current working directory if no path supplied ...
  if ExtractFilePath(filename) = '' then
    filename := GetCurrentDir + '\'+ filename;
  fileFormatClass := GetImageFormatClass(ExtractFileExt(filename));
  if assigned(fileFormatClass) then
    with fileFormatClass.Create do
    try
      result := SaveToFile(filename, self, quality);
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.SaveToStream(stream: TStream; const FmtExt: string): Boolean;
var
  fileFormatClass: TImageFormatClass;
begin
  result := false;
  fileFormatClass := GetImageFormatClass(FmtExt);
  if assigned(fileFormatClass) then
    with fileFormatClass.Create do
    try
      SaveToStream(stream, self);
      result := true;
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromFile(const filename: string): Boolean;
var
  stream: TFileStream;
begin
  Result := false;
  if not FileExists(filename) then Exit;

  stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    result := LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromStream(stream: TStream; imgIdx: integer): Boolean;
var
  ifc: TImageFormatClass;
begin
  ifc := GetImageFormatClass(stream);
  Result := Assigned(ifc);
  if not Result then Exit;

  with ifc.Create do
  try
    result := LoadFromStream(stream, self, imgIdx);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.GetPixel(x, y: Integer): TColor32;
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then
    result := clNone32 else
    result := fPixels[y * width + x];
end;
//------------------------------------------------------------------------------

procedure TImage32.SetPixel(x,y: Integer; color: TColor32);
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then Exit;
  fPixels[y * width + x] := color;
  //nb: no notify event here
end;
//------------------------------------------------------------------------------

function TImage32.GetIsBlank: Boolean;
var
  i: integer;
  pc: PARGB;
begin
  result := IsEmpty;
  if result then Exit;
  pc := PARGB(PixelBase);
  for i := 0 to width * height -1 do
  begin
    if pc.A > 0 then Exit;
    inc(pc);
  end;
  result := true;
end;
//------------------------------------------------------------------------------

function TImage32.GetIsEmpty: Boolean;
begin
  result := fPixels = nil;
end;
//------------------------------------------------------------------------------

function TImage32.GetPixelBase: PColor32;
begin
  if IsEmpty then result := nil
  else result := @fPixels[0];
end;
//------------------------------------------------------------------------------

function TImage32.GetPixelRow(row: Integer): PColor32;
begin
  if IsEmpty then result := nil
  else result := @fPixels[row * Width];
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyInternal(src: TImage32;
  const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
var
  i, j: integer;
  srcRecWidth, srcRecHeight: nativeint;
  srcWidth, dstWidth: nativeint;
  s, d: PColor32;
begin
  // occasionally, due to rounding, srcRec and dstRec
  // don't have exactly the same widths and heights, so ...
  srcRecWidth :=
    Min(srcRec.Right - srcRec.Left, dstRec.Right - dstRec.Left);
  srcRecHeight :=
    Min(srcRec.Bottom - srcRec.Top, dstRec.Bottom - dstRec.Top);

  srcWidth := src.Width;
  dstWidth := Width;

  s := @src.Pixels[srcRec.Top * srcWidth + srcRec.Left];
  d := @Pixels[dstRec.top * dstWidth + dstRec.Left];

  if assigned(blendFunc) then
  begin
    srcWidth := (srcWidth - srcRecWidth) * SizeOf(TColor32);
    dstWidth := (dstWidth - srcRecWidth) * SizeOf(TColor32);
    for i := 1 to srcRecHeight do
    begin
      for j := 1 to srcRecWidth do
      begin
        d^ := blendFunc(d^, s^);
        inc(s); inc(d);
      end;
      inc(PByte(s), srcWidth); // byte offset to the next s line
      inc(PByte(d), dstWidth); // byte offset to the next d line
    end;
  end
  //simply overwrite src with dst (ie without blending)
  else if (srcRecWidth = dstWidth) and (srcWidth = dstWidth) then
    move(s^, d^, srcRecWidth * srcRecHeight * SizeOf(TColor32))
  else
  begin
    srcWidth := srcWidth * SizeOf(TColor32);
    dstWidth := dstWidth * SizeOf(TColor32);
    srcRecWidth := srcRecWidth * SizeOf(TColor32);
    for i := 1 to srcRecHeight do
    begin
      move(s^, d^, srcRecWidth);
      inc(PByte(s), srcWidth); // srcWidth is in bytes 
      inc(PByte(d), dstWidth); // dstWidth is in bytes
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TImage32.CopyInternalLine(src: TImage32;
  const srcRec, dstRec: TRect; blendLineFunc: TBlendLineFunction);
var
  i: integer;
  srcRecWidth, srcRecHeight: nativeint;
  srcWidth, dstWidth: nativeint;
  s, d: PColor32;
begin
  if not Assigned(blendLineFunc) then
  begin
    CopyInternal(src, srcRec, dstRec, nil);
    Exit;
  end;

  // occasionally, due to rounding, srcRec and dstRec
  // don't have exactly the same widths and heights, so ...
  srcRecWidth :=
    Min(srcRec.Right - srcRec.Left, dstRec.Right - dstRec.Left);
  srcRecHeight :=
    Min(srcRec.Bottom - srcRec.Top, dstRec.Bottom - dstRec.Top);

  srcWidth := src.Width;
  dstWidth := Width;

  s := @src.Pixels[srcRec.Top * srcWidth + srcRec.Left];
  d := @Pixels[dstRec.top * dstWidth + dstRec.Left];

  if (srcRecWidth = dstWidth) and (srcWidth = dstWidth) then
    blendLineFunc(d, s, srcRecWidth * srcRecHeight)
  else
  begin
    srcWidth := srcWidth * SizeOf(TColor32);
    dstWidth := dstWidth * SizeOf(TColor32);
    for i := 1 to srcRecHeight do
    begin
      blendLineFunc(d, s, srcRecWidth);
      inc(PByte(s), srcWidth); // srcWidth is in bytes
      inc(PByte(d), dstWidth); // dstWidth is in bytes
    end;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.Copy(src: TImage32; srcRec, dstRec: TRect): Boolean;
begin
  Result := CopyBlendInternal(src, srcRec, dstRec, nil, nil);
end;
//------------------------------------------------------------------------------

function TImage32.CopyBlend(src: TImage32; const srcRec, dstRec: TRect;
  blendFunc: TBlendFunction): Boolean;
begin
  Result := CopyBlendInternal(src, srcRec, dstRec, blendFunc, nil);
end;
//------------------------------------------------------------------------------

function TImage32.CopyBlend(src: TImage32; const srcRec, dstRec: TRect;
  blendLineFunc: TBlendLineFunction): Boolean;
begin
  Result := CopyBlendInternal(src, srcRec, dstRec, nil, blendLineFunc);
end;
//------------------------------------------------------------------------------

function TImage32.CopyBlendInternal(src: TImage32; srcRec, dstRec: TRect;
  blendFunc: TBlendFunction; blendLineFunc: TBlendLineFunction): Boolean;
var
  tmp: TImage32;
  srcRecClipped, dstRecClipped, r: TRect;
  scaleX, scaleY: double;
  w,h, dstW,dstH, srcW,srcH: integer;
begin
  result := false;
  if IsEmptyRect(srcRec) or IsEmptyRect(dstRec) then Exit;
  Types.IntersectRect(srcRecClipped, srcRec, src.Bounds);

  //get the scaling amount (if any) before
  //dstRec might be adjusted due to clipping ...
  RectWidthHeight(dstRec, dstW, dstH);
  RectWidthHeight(srcRec, srcW, srcH);

  //watching out for insignificant scaling
  if Abs(dstW - srcW) < 2 then
     scaleX := 1 else
     scaleX := dstW / srcW;
  if Abs(dstH - srcH) < 2 then
     scaleY := 1 else
     scaleY := dstH / srcH;

  //check if the source rec has been clipped ...
  if not RectsEqual(srcRecClipped, srcRec) then
  begin
    if IsEmptyRect(srcRecClipped) then Exit;
    //the source has been clipped so clip the destination too ...
    RectWidthHeight(srcRecClipped, w, h);
    RectWidthHeight(srcRec, srcW, srcH);
    ScaleRect(dstRec, w / srcW, h / srcH);
    TranslateRect(dstRec,
      srcRecClipped.Left - srcRec.Left,
      srcRecClipped.Top - srcRec.Top);
  end;

  if (scaleX <> 1.0) or (scaleY <> 1.0) then
  begin
    //scale source (tmp) to the destination then call CopyBlend() again ...^
    tmp := TImage32.Create;
    try
      tmp.AssignSettings(src);
      src.ScaleTo(tmp, scaleX, scaleY);
      ScaleRect(srcRecClipped, scaleX, scaleY);
      result := CopyBlendInternal(tmp, srcRecClipped, dstRec, blendFunc, blendLineFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;

  Types.IntersectRect(dstRecClipped, dstRec, Bounds);
  if IsEmptyRect(dstRecClipped) then Exit;

  //there's no scaling if we get here, but further clipping may be needed if
  //the destination rec is partially outside the destination image's bounds

  if not RectsEqual(dstRecClipped, dstRec) then
  begin
    //the destination rec has been clipped so clip the source too ...
    RectWidthHeight(dstRecClipped, w, h);
    RectWidthHeight(dstRec, dstW, dstH);
    ScaleRect(srcRecClipped, w / dstW, h / dstH);
    TranslateRect(srcRecClipped,
      dstRecClipped.Left - dstRec.Left,
      dstRecClipped.Top - dstRec.Top);
  end;

  //when copying to self and srcRec & dstRec overlap then
  //copy srcRec to a temporary image and use it as the source ...
  if (src = self) and Types.IntersectRect(r, srcRecClipped, dstRecClipped) then
  begin
    tmp := TImage32.Create(self, srcRecClipped);
    try
      result := src.CopyBlendInternal(tmp, tmp.Bounds, dstRecClipped, blendFunc, blendLineFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;

  if Assigned(blendLineFunc) then
    CopyInternalLine(src, srcRecClipped, dstRecClipped, blendLineFunc)
  else
    CopyInternal(src, srcRecClipped, dstRecClipped, blendFunc);
  result := true;
  Changed;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromResource(const resName: string; resType: PChar): Boolean;
var
  resStream: TResourceStream;
begin
  resStream := CreateResourceStream(resName, resType);
  try
    Result := assigned(resStream) and
      LoadFromStream(resStream);
  finally
    resStream.Free;
  end;
end;
//------------------------------------------------------------------------------

{$IF DEFINED (MSWINDOWS)}
procedure TImage32.CopyFromDC(srcDc: HDC; const srcRect: TRect);
var
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  dc, memDc: HDC;
  pixels: Pointer;
  w,h: integer;
begin
  BeginUpdate;
  try
    RectWidthHeight(srcRect, w,h);
    SetSize(w, h);
    bi := Get32bitBitmapInfoHeader(w, -h); // -h => avoids need to flip image
    dc := GetDC(0);
    memDc := CreateCompatibleDC(dc);
    try
      bm := CreateDIBSection(dc,
        PBITMAPINFO(@bi)^, DIB_RGB_COLORS, pixels, 0, 0);
      if bm = 0 then Exit;
      try
        oldBm := SelectObject(memDc, bm);
        BitBlt(memDc, 0, 0, w, h, srcDc, srcRect.Left,srcRect.Top, SRCCOPY);
        Move(pixels^, fPixels[0], w * h * sizeOf(TColor32));
        SelectObject(memDc, oldBm);
      finally
        DeleteObject(bm);
      end;
    finally
      DeleteDc(memDc);
      ReleaseDc(0, dc);
    end;
    if IsBlank then SetAlpha(255);
    //FlipVertical;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyToDc(dstDc: HDC; x,y: Integer; transparent: Boolean);
begin
  CopyToDc(Bounds, Types.Rect(x,y, x+Width, y+Height),
    dstDc, transparent);
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyToDc(const srcRect: TRect; dstDc: HDC;
  x: Integer = 0; y: Integer = 0; transparent: Boolean = true);
var
  recW, recH: integer;
begin
  RectWidthHeight(srcRect, recW, recH);
  CopyToDc(srcRect, Types.Rect(x,y, x+recW, y+recH), dstDc, transparent);
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyToDc(const srcRect, dstRect: TRect;
  dstDc: HDC; transparent: Boolean = true);
var
  i, x,y, wSrc ,hSrc, wDest, hDest, wBytes: integer;
  rec: TRect;
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  dibBits: Pointer;
  pDst, pSrc: PARGB;
  memDc: HDC;
  isTransparent: Boolean;
  bf: BLENDFUNCTION;
  oldStretchBltMode: integer;
begin
  Types.IntersectRect(rec, srcRect, Bounds);
  if IsEmpty or IsEmptyRect(rec) or IsEmptyRect(dstRect) then Exit;
  RectWidthHeight(rec, wSrc, hSrc);
  RectWidthHeight(dstRect, wDest, hDest);
  x := dstRect.Left;
  y := dstRect.Top;
  inc(x, rec.Left - srcRect.Left);
  inc(y, rec.Top - srcRect.Top);

  bi := Get32bitBitmapInfoHeader(wSrc, hSrc);

  isTransparent := transparent and RectHasTransparency(srcRect);
  memDc := CreateCompatibleDC(dstDc);
  try
    bm := CreateDIBSection(memDc, PBITMAPINFO(@bi)^,
      DIB_RGB_COLORS, dibBits, 0, 0);
    if bm = 0 then Exit;

    try
      //copy Image to dibBits (with vertical flip)
      wBytes := wSrc * SizeOf(TColor32);
      pDst := dibBits;
      pSrc := PARGB(PixelRow[rec.Bottom -1]);
      inc(pSrc, rec.Left);
      if isTransparent and not IsPremultiplied then
      begin
        //premultiplied alphas are required when alpha blending
        for i := rec.Bottom -1 downto rec.Top do
        begin
          PremultiplyAlpha(pSrc, pDst, wSrc);
          dec(pSrc, Width);
          inc(pDst, wSrc);
        end;
      end
      else
      begin
        for i := rec.Bottom -1 downto rec.Top do
        begin
          Move(pSrc^, pDst^, wBytes);
          dec(pSrc, Width);
          inc(pDst, wSrc);
        end;
      end;

      oldBm := SelectObject(memDC, bm);
      if isTransparent then
      begin
        //premultiplied alphas are required when alpha blending
        bf.BlendOp := AC_SRC_OVER;
        bf.BlendFlags := 0;
        bf.SourceConstantAlpha := 255;
        bf.AlphaFormat := AC_SRC_ALPHA;
        AlphaBlend(dstDc, x,y, wDest,hDest, memDC, 0,0, wSrc,hSrc, bf);
      end
      else if (wDest = wSrc) and (hDest = hSrc) then
      begin
        BitBlt(dstDc, x,y, wSrc, hSrc, memDc, 0,0, SRCCOPY)
      end else
      begin
        oldStretchBltMode := SetStretchBltMode(dstDc, COLORONCOLOR);
        StretchBlt(dstDc, x,y, wDest, hDest, memDc, 0,0, wSrc,hSrc, SRCCOPY);
        if oldStretchBltMode <> COLORONCOLOR then // restore mode
          SetStretchBltMode(dstDc, oldStretchBltMode);
      end;
      SelectObject(memDC, oldBm);
    finally
      DeleteObject(bm);
    end;
  finally
    DeleteDc(memDc);
  end;
end;
{$IFEND}
//------------------------------------------------------------------------------

{$IF DEFINED(USING_VCL_LCL)}
procedure TImage32.CopyFromBitmap(bmp: TBitmap);
var
  ms: TMemoryStream;
  bmpFormat: TImageFormat_BMP;
begin
  ms := TMemoryStream.create;
  bmpFormat := TImageFormat_BMP.Create;
  try
    bmp.SaveToStream(ms);
    ms.Position := 0;
    bmpFormat.LoadFromStream(ms, self);
  finally
    ms.Free;
    bmpFormat.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyToBitmap(bmp: TBitmap);
var
  ms: TMemoryStream;
  bmpFormat: TImageFormat_BMP;
begin
  ms := TMemoryStream.create;
  bmpFormat := TImageFormat_BMP.Create;
  try
     bmpFormat.IncludeFileHeaderInSaveStream := true;
     bmpFormat.SaveToStream(ms, self);
     ms.Position := 0;
     bmp.PixelFormat := pf32bit;
     {$IF DEFINED(USING_VCL) AND DEFINED(ALPHAFORMAT)}
     bmp.AlphaFormat := afDefined;
     {$IFEND}
     bmp.LoadFromStream(ms);
  finally
    ms.Free;
    bmpFormat.Free;
  end;
end;
//------------------------------------------------------------------------------
{$IFEND}

function TImage32.CopyToClipBoard: Boolean;
var
  i: Integer;
  formatClass: TImageFormatClass;
begin
  //Sadly with CF_DIB (and even CF_DIBV5) clipboard formats, transparency is
  //usually lost, so we'll copy all available formats including CF_PNG, that
  //is if it's registered.
  result := not IsEmpty;
  if not result then Exit;
  result := false;

  for i := ImageFormatClassList.Count -1 downto 0 do
  begin
    formatClass := PImgFmtRec(ImageFormatClassList[i]).Obj;
    if not formatClass.CanCopyToClipboard then Continue;
    with formatClass.Create do
    try
      result := CopyToClipboard(self);
    finally
      free;
    end;
  end;
end;
//------------------------------------------------------------------------------

class function TImage32.CanPasteFromClipBoard: Boolean;
var
  i: Integer;
  formatClass: TImageFormatClass;
begin
  result := false;
  for i := ImageFormatClassList.Count -1 downto 0 do
  begin
    formatClass := PImgFmtRec(ImageFormatClassList[i]).Obj;
    if formatClass.CanPasteFromClipboard then
    begin
      result := true;
      Exit;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.PasteFromClipBoard: Boolean;
var
  i: Integer;
  formatClass: TImageFormatClass;
begin
  result := false;
  for i := ImageFormatClassList.Count -1 downto 0 do
  begin
    formatClass := PImgFmtRec(ImageFormatClassList[i]).Obj;
    if not formatClass.CanPasteFromClipboard then Continue;

    with formatClass.Create do
    try
      result := PasteFromClipboard(self);
      if not Result then Continue;
    finally
      free;
    end;
    Changed;
    Break;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.ConvertToBoolMask(reference: TColor32; tolerance: integer;
  colorFunc: TCompareFunction; maskBg: TColor32; maskFg: TColor32);
var
  i: Integer;
  mask: TArrayOfByte;
  c: PColor32;
  b: PByte;
begin
  if IsEmpty then Exit;
  mask := GetBoolMask(self, reference, colorFunc, tolerance);
  c := PixelBase;
  b := @mask[0];
  for i := 0 to Width * Height -1 do
  begin
  {$IFDEF PBYTE}
    if b^ = 0 then c^ := maskBg else c^ := maskFg;
  {$ELSE}
    if b^ = #0 then c^ := maskBg else c^ := maskFg;
  {$ENDIF}
    inc(c); inc(b);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.ConvertToAlphaMask(reference: TColor32;
  colorFunc: TCompareFunctionEx);
var
  i: Integer;
  mask: TArrayOfByte;
  c: PColor32;
  b: PByte;
begin
  if IsEmpty then Exit;
  mask := GetByteMask(self, reference, colorFunc);
  c := PixelBase;
  b := @mask[0];
  for i := 0 to Width * Height -1 do
  begin
  {$IFDEF PBYTE}
    c^ := b^ shl 24;
  {$ELSE}
    c^ := Ord(b^) shl 24;
  {$ENDIF}
    inc(c); inc(b);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipVertical;
var
  i: Integer;
  a: TArrayOfColor32;
  src, dst: PColor32;
begin
  if IsEmpty then Exit;
  NewColor32Array(a, fWidth * fHeight, True);
  src := @fPixels[(height-1) * width];
  dst := @a[0];
  for i := 0 to fHeight -1 do
  begin
    move(src^, dst^, fWidth * SizeOf(TColor32));
    dec(src, fWidth); inc(dst, fWidth);
  end;
  fPixels := a;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipHorizontal;
var
  i,j, widthLess1: Integer;
  a: TArrayOfColor32;
  row: PColor32;
begin
  if IsEmpty then Exit;
  NewColor32Array(a, fWidth, True);
  widthLess1 := fWidth -1;
  row := @fPixels[(height-1) * width]; //top row
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[0], fWidth * SizeOf(TColor32));
    for j := 0 to widthLess1 do
    begin
      row^ := a[widthLess1 - j];
      inc(row);
    end;
    dec(row, fWidth *2);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.PreMultiply;
begin
  if IsEmpty or fIsPremultiplied then Exit;
  fIsPremultiplied := true;
  PremultiplyAlpha(PARGB(PixelBase), PARGB(PixelBase), Width * Height);
  //nb: no OnChange notify event here
end;
//------------------------------------------------------------------------------

procedure TImage32.SetRGB(rgbColor: TColor32);
var
  i: Integer;
  pc: PColor32;
  c: TColor32;
begin
  //this method leaves the alpha channel untouched
  if IsEmpty then Exit;

  pc := PixelBase;
  rgbColor := rgbColor and $00FFFFFF;
  for i := 0 to Width * Height - 1 do
  begin
    c := pc^;
    if c and $FF000000 = 0 then
      pc^ := 0 else
      pc^ := c and $FF000000 or rgbColor;
    inc(pc);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.SetRGB(rgbColor: TColor32; rec: TRect);
var
  i,j, dx: Integer;
  pc: PColor32;
begin
  Types.IntersectRect(rec, rec, bounds);
  if IsEmptyRect(rec) then Exit;
  rgbColor := rgbColor and $00FFFFFF;
  pc := PixelBase;
  inc(pc, rec.Left);
  dx := Width - RectWidth(rec);
  for i := rec.Top to rec.Bottom -1 do
  begin
    for j := rec.Left to rec.Right -1 do
    begin
      pc^ := pc^ and $FF000000 or rgbColor;
      inc(pc);
    end;
    inc(pc, dx);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.SetAlpha(alpha: Byte);
var
  i: Integer;
  c: PARGB;
begin
  //this method only changes the alpha channel
  if IsEmpty then Exit;
  c := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    c.A := alpha;
    inc(c);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.ReduceOpacity(opacity: Byte);
var
  i: Integer;
  c: PARGB;
  a: Byte;
begin
  if opacity = 255 then Exit;
  c := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    a := c.A;
    if a <> 0 then
      c.A := MulTable[a, opacity];
    inc(c);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.ReduceOpacity(opacity: Byte; rec: TRect);
var
  i,j, rw: Integer;
  c: PARGB;
  a: Byte;
  lineOffsetInBytes: integer;
begin
  Types.IntersectRect(rec, rec, bounds);
  if IsEmptyRect(rec) then Exit;
  rw := RectWidth(rec);
  c := @Pixels[rec.Top * Width + rec.Left];
  lineOffsetInBytes := (Width - rw) * SizeOf(TARGB);
  for i := rec.Top to rec.Bottom - 1 do
  begin
    for j := 1 to rw do
    begin
      a := c.A;
      if a <> 0 then
        c.A := MulTable[a, opacity];
      inc(c);
    end;
    inc(PByte(c), lineOffsetInBytes);
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.Grayscale(mode: TGrayscaleMode;
  linearAmountPercentage: double);
var
  i: SizeInt;
  cLinear: double;
  c, lastC, grayC: TColor32;
  p: PStaticColor32Array;
  amountCalc: Boolean;
  oneMinusAmount: double;
begin
  if mode = gsmSaturation then
  begin
    // linearAmountPercentage has no effect here
    AdjustSaturation(-100);
    Exit;
  end;

  // Colorimetric (perceptual luminance-preserving) conversion to grayscale
  // See https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale
  if IsEmpty then Exit;

  if linearAmountPercentage <= 0.0 then Exit;
  amountCalc := linearAmountPercentage < 1.0;
  oneMinusAmount := 1.0 - linearAmountPercentage;

  p := PStaticColor32Array(PixelBase);
  lastC := 0;
  grayC := 0;
  for i := 0 to high(fPixels) do
  begin
    c := p[i] and $00FFFFFF;
    if c <> 0 then
    begin
      if c <> lastC then // only do the calculation if the color channels changed
      begin
        lastC := c;
        {$IF DEFINED(ANDROID)}
        c := SwapRedBlue(c);
        {$IFEND}

        // We don't divide by 255 here, so can skip some division and multiplications.
        // That means cLinear is actually "cLinear * 255"
        cLinear := (0.2126 * Byte(c shr 16)) + (0.7152 * Byte(c shr 8)) + (0.0722 * Byte(c));
        //cLinear := (0.2126 * TARGB(c).R) + (0.7152 * TARGB(c).G) + (0.0722 * TARGB(c).B);

        if mode = gsmLinear then
          c := ClampByte(cLinear)
        else //if mode = gsmColorimetric then
        begin
          if cLinear <= (0.0031308 * 255) then // adjust for cLinear being "cLiniear * 255"
            c := ClampByte(Integer(Round(12.92 * cLinear)))
          else // for Power we must divide by 255 and then later multipy by 255
            //c := ClampByte(Integer(Round((1.055 * 255) * Power(cLinear / 255, 1/2.4) - (0.055 * 255))));
        end;


        if not amountCalc then
          grayC := (c shl 16) or (c shl 8) or c
        else
        begin
          cLinear := c * linearAmountPercentage;
          grayC := ClampByte(Integer(Round(Byte(lastC shr 16) * oneMinusAmount + cLinear))) shl 16 or
                   ClampByte(Integer(Round(Byte(lastC shr  8) * oneMinusAmount + cLinear))) shl  8 or
                   ClampByte(Integer(Round(Byte(lastC       ) * oneMinusAmount + cLinear)));
        end;

        {$IF DEFINED(ANDROID)}
        grayC := SwapRedBlue(grayC);
        {$IFEND}
      end;
      p[i] := (p[i] and $FF000000) or grayC;
    end;
  end;

  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertColors;
var
  pc: PStaticColor32Array;
  i: SizeInt;
begin
  pc := PStaticColor32Array(PixelBase);
  for i := 0 to Width * Height -1 do
    pc[i] := pc[i] xor $00FFFFFF; // keep the alpha channel untouched
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertAlphas;
var
  pc: PStaticColor32Array;
  i: SizeInt;
begin
  pc := PStaticColor32Array(PixelBase);
  for i := 0 to Width * Height -1 do
    pc[i] := pc[i] xor $FF000000; // keep the color channels untouched
  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustHue(percent: Integer);
var
  i: SizeInt;
  hsl: THsl;
  lut: array [byte] of byte;
  c, lastC, newC: TColor32;
  p: PStaticColor32Array;
begin
  percent := percent mod 100;
  if percent < 0 then inc(percent, 100);
  percent := Round(percent * 255 / 100);
  if (percent = 0) or IsEmpty then Exit;
  for i := 0 to 255 do lut[i] := (i + percent) mod 255;

  lastC := 0;
  newC := 0;
  p := PStaticColor32Array(fPixels);
  for i := 0 to high(fPixels) do
  begin
    c := p[i];
    c := c and $00FFFFFF;
    if c <> 0 then
    begin
      if c <> lastC then // only do the calculation if the color channels changed
      begin
        lastC := C;
        hsl := RgbToHsl(c);
        hsl.hue := lut[hsl.hue];
        newC := HslToRgb(hsl);
      end;
      p[i] := (p[i] and $FF000000) or newC; // keep the alpha channel
    end;
  end;

  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustLuminance(percent: Integer);
var
  i: SizeInt;
  hsl: THsl;
  pc: double;
  lut: array [byte] of byte;
  c, lastC, newC: TColor32;
  p: PStaticColor32Array;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := percent mod 101;
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  lastC := 0;
  newC := 0;
  p := PStaticColor32Array(fPixels);
  for i := 0 to high(fPixels) do
  begin
    c := p[i];
    c := c and $00FFFFFF;
    if c <> 0 then
    begin
      if c <> lastC then // only do the calculation if the color channels changed
      begin
        lastC := C;
        hsl := RgbToHsl(c);
        hsl.lum := lut[hsl.lum];
        newC := HslToRgb(hsl);
      end;
      p[i] := (p[i] and $FF000000) or newC; // keep the alpha channel
    end;
  end;

  Changed;
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustSaturation(percent: Integer);
var
  i: SizeInt;
  hsl: THsl;
  lut: array [byte] of byte;
  pc: double;
  c, lastC, newC: TColor32;
  p: PStaticColor32Array;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := percent mod 101;
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  lastC := 0;
  newC := 0;
  p := PStaticColor32Array(fPixels);
  for i := 0 to high(fPixels) do
  begin
    c := p[i];
    c := c and $00FFFFFF;
    if c <> 0 then
    begin
      if c <> lastC then // only do the calculation if the color channels changed
      begin
        lastC := C;
        hsl := RgbToHsl(c);
        hsl.sat := lut[hsl.sat];
        newC := HslToRgb(hsl);
      end;
      p[i] := (p[i] and $FF000000) or newC; // keep the alpha channel
    end;
  end;

  Changed;
end;
//------------------------------------------------------------------------------

function TImage32.GetOpaqueBounds: TRect;
var
  x,y, x1,x2,y1,y2: Integer;
  found: Boolean;
begin
  y1 := 0; y2 := 0;
  found := false;
  Result := NullRect;
  for y := 0 to Height -1 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y1 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  if not found then
    Exit;

  found := false;
  for y := Height -1 downto 0 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y2 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  x1 := Width; x2 := 0;
  for y := y1 to y2 do
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        if x < x1 then x1 := x;
        if x > x2 then x2 := x;
      end;

  Result := Types.Rect(x1, y1, x2+1, y2+1);
end;
//------------------------------------------------------------------------------

function TImage32.CropTransparentPixels: TRect;
begin
  Result := GetOpaqueBounds;
  if IsEmptyRect(Result) then
     SetSize(0,0) else
     Crop(Result);
end;
//------------------------------------------------------------------------------

procedure TImage32.Rotate(angleRads: double);
var
  mat: TMatrixD;
begin

{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRads := -angleRads;
{$ENDIF}

  //nb: There's no point rotating about a specific point
  //since the rotated image will be recentered.

  NormalizeAngle(angleRads);
  if IsEmpty or (angleRads = 0) then Exit;

  if angleRads = angle180 then
  begin
    Rotate180; //because we've excluded 0 & 360 deg angles
  end
  else if angleRads = angle90 then
  begin
    RotateRight90;
  end
  else if angleRads = -angle90 then
  begin
    RotateLeft90;
  end else
  begin
    mat := IdentityMatrix;
    // the rotation point isn't important
    // because AffineTransformImage() will
    // will resize and recenter the image
    MatrixRotate(mat, NullPointD, angleRads);
    AffineTransformImage(self, mat);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateRect(const rec: TRect;
  angleRads: double; eraseColor: TColor32 = 0);
var
  tmp: TImage32;
  rec2: TRect;
  recWidth, recHeight: integer;
begin
  recWidth := rec.Right - rec.Left;
  recHeight := rec.Bottom - rec.Top;
  //create a tmp image with a copy of the pixels inside rec ...
  tmp := TImage32.Create(self, rec);
  try
    tmp.Rotate(angleRads);
    //since rotating also resizes, get a centered
    //(clipped) rect of the rotated pixels ...
    rec2.Left := (tmp.Width - recWidth) div 2;
    rec2.Top := (tmp.Height - recHeight) div 2;
    rec2.Right := rec2.Left + recWidth;
    rec2.Bottom := rec2.Top + recHeight;
    //finally move the rotated rec back to the image ...
    FillRect(rec, eraseColor);
    CopyBlend(tmp, rec2, rec);
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Skew(dx,dy: double);
var
  mat: TMatrixD;
begin
  if IsEmpty or ((dx = 0) and (dy = 0)) then Exit;
  //limit skewing to twice the image's width and/or height
  dx := ClampRange(dx, -2.0, 2.0);
  dy := ClampRange(dy, -2.0, 2.0);
  mat := IdentityMatrix;
  MatrixSkew(mat, dx, dy);
  AffineTransformImage(self, mat);
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleAlpha(scale: double);
var
  i: Integer;
  pb: PARGB;
begin
  pb := PARGB(PixelBase);
  for i := 0 to Width * Height - 1 do
  begin
    pb.A := ClampByte(Integer(Round(pb.A * scale)));
    inc(pb);
  end;
  Changed;
end;

//------------------------------------------------------------------------------
// TImageList32
//------------------------------------------------------------------------------

constructor TImageList32.Create;
begin
{$IFDEF XPLAT_GENERICS}
  fList := TList<TImage32>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
  fIsImageOwner := true;
end;
//------------------------------------------------------------------------------

destructor TImageList32.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TImageList32.Count: integer;
begin
  result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TImageList32.Clear;
var
  i: integer;
begin
  if IsImageOwner then
    for i := 0 to fList.Count -1 do
      TImage32(fList[i]).Free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

function TImageList32.GetImage(index: integer): TImage32;
begin
  result := TImage32(fList[index]);
end;
//------------------------------------------------------------------------------

procedure TImageList32.SetImage(index: integer; img: TIMage32);
begin
  if fIsImageOwner then TImage32(fList[index]).Free;
  fList[index] := img;
end;
//------------------------------------------------------------------------------

function TImageList32.GetLast: TImage32;
begin
  if Count = 0 then Result := nil
  else Result := TImage32(fList[Count -1]);
end;
//------------------------------------------------------------------------------

procedure TImageList32.Add(image: TImage32);
begin
  fList.Add(image);
end;
//------------------------------------------------------------------------------

function TImageList32.Add(width, height: integer): TImage32;
begin
  Result := TImage32.create(width, height);
  fList.Add(Result);
end;
//------------------------------------------------------------------------------

procedure TImageList32.Insert(index: integer; image: TImage32);
begin
  fList.Insert(index, image);
end;
//------------------------------------------------------------------------------

procedure TImageList32.Move(currentIndex, newIndex: integer);
begin
  fList.Move(currentIndex, newIndex);
end;
//------------------------------------------------------------------------------

procedure TImageList32.Delete(index: integer);
begin
  if fIsImageOwner then TImage32(fList[index]).Free;
  fList.Delete(index);
end;

//------------------------------------------------------------------------------
// TImageFormat methods
//------------------------------------------------------------------------------

function TImageFormat.LoadFromFile(const filename: string;
  img32: TImage32): Boolean;
var
  fs: TFileStream;
begin
  result := FileExists(filename);
  if not result then Exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(fs, img32);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImageFormat.SaveToFile(const filename: string;
  img32: TImage32; quality: integer): Boolean;
var
  fs: TFileStream;
begin
  result := (pos('.', filename) = 1) or
    DirectoryExists(ExtractFilePath(filename));
  if not result then Exit;

  fs := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(fs, img32, quality);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat.CanCopyToClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat.GetImageCount(stream: TStream): integer;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------
// TInterfacedObj
//------------------------------------------------------------------------------

{$IFDEF FPC}
function TInterfacedObj._AddRef: Integer;
  {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;
//------------------------------------------------------------------------------

function TInterfacedObj._Release: Integer;
  {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;
//------------------------------------------------------------------------------

function TInterfacedObj.QueryInterface(
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;
  out obj) : longint;
begin
  if GetInterface(IID, Obj) then Result := 0
  else Result := E_NOINTERFACE;
end;

{$ELSE}

function TInterfacedObj._AddRef: Integer; stdcall;
begin
  Result := -1;
end;
//------------------------------------------------------------------------------

function TInterfacedObj._Release: Integer; stdcall;
begin
  Result := -1;
end;
//------------------------------------------------------------------------------

function TInterfacedObj.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := 0
  else Result := E_NOINTERFACE;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Initialization and Finalization functions
//------------------------------------------------------------------------------

procedure MakeBlendTables;
var
  i,j: Integer;
begin
  for j := 0 to 255 do MulTable[0, j] := 0;
  for i := 0 to 255 do MulTable[i, 0] := 0;
  for j := 0 to 255 do DivTable[0, j] := 0;
  for i := 0 to 255 do DivTable[i, 0] := 0;
  for i := 1 to 255 do
  begin
    for j := 1 to 255 do
    begin
      MulTable[i, j] := Round(i * j * div255);
      if i >= j then
        DivTable[i, j] := 255 else
        DivTable[i, j] := Round(i * $FF / j);
    end;
  end;

  Sigmoid[128] := 128;
  for i := 1 to 127 do
    Sigmoid[128+i] := 128 + Round(127 * sin(angle90 * i/127));
  for i := 0 to 127 do
    Sigmoid[i] := 255- Sigmoid[255-i];
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure GetScreenScale;
var
  dc: HDC;
  ScreenPixelsY: integer;
begin
  dc := GetDC(0);
  try
    ScreenPixelsY := GetDeviceCaps(dc, LOGPIXELSY);
    DpiAwareOne := ScreenPixelsY / 96;
  finally
    ReleaseDC(0, dc);
  end;
  dpiAware1   := Round(DpiAwareOne);
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure CleanUpImageFormatClassList;
var
  i: integer;
begin
  for i := ImageFormatClassList.Count -1 downto 0 do
    Dispose(PImgFmtRec(ImageFormatClassList[i]));
  ImageFormatClassList.Free;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure CreateResamplerList;
begin
{$IFDEF XPLAT_GENERICS}
  ResamplerList := TList<TResamplerObj>.Create;
{$ELSE}
  ResamplerList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetResampler(id: integer): TResamplerFunction;
var
  i: integer;
begin
  result := nil;
  if not Assigned(ResamplerList) then Exit;

  for i := ResamplerList.Count -1 downto 0 do
    if TResamplerObj(ResamplerList[i]).id = id then
  begin
    Result := TResamplerObj(ResamplerList[i]).func;
    Break;
  end;
end;
//------------------------------------------------------------------------------

function RegisterResampler(func: TResamplerFunction; const name: string): integer;
var
  resampleObj: TResamplerObj;
begin
  if not Assigned(ResamplerList) then
    CreateResamplerList;

  resampleObj := TResamplerObj.Create;
  Result := ResamplerList.Add(resampleObj) +1;
  resampleObj.id := Result;
  resampleObj.name := name;
  resampleObj.func := func;
end;
//------------------------------------------------------------------------------

procedure GetResamplerList(stringList: TStringList);
var
  i: integer;
  resampleObj: TResamplerObj;
begin
  stringList.Clear;
  stringList.Capacity := ResamplerList.Count;
  for i := 0 to ResamplerList.Count -1 do
  begin
    resampleObj := ResamplerList[i];
    stringList.AddObject(resampleObj.name, resampleObj);
  end;
end;
//------------------------------------------------------------------------------

procedure CleanUpResamplerClassList;
var
  i: integer;
begin
  if not Assigned(ResamplerList) then Exit;
  for i := ResamplerList.Count -1 downto 0 do
    TResamplerObj(ResamplerList[i]).Free;
  ResamplerList.Free;
end;
//------------------------------------------------------------------------------

initialization
  CreateImageFormatList;
  MakeBlendTables;

{$IFDEF MSWINDOWS}
  GetScreenScale;
{$ENDIF}

finalization
  CleanUpImageFormatClassList;
  CleanUpResamplerClassList;

end.
