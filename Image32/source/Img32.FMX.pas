unit Img32.FMX;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.0                                                             *
* Date      :  20 July 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Image file format support for TImage32 and FMX                  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}
uses
  SysUtils, Classes, Math, Img32, System.Rtti,
  System.Generics.Collections, System.Generics.Defaults,
  FMX.Platform, FMX.Types, FMX.Surfaces, FMX.Graphics;

type

  TImageFormat_FMX = class(TImageFormat)
  private
    fExt: string;
    fPixelFormat: FMX.Types.TPixelFormat;
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    function SaveToFile(const filename: string; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
    property Ext: string read fExt write fExt;
  end;

function DPIAwareFMX(val: Integer): Integer; overload; inline;
function DPIAwareFMX(val: double): double; overload; inline;

const
  RT_BITMAP = PChar(2);

var
  screenScale: double;

  {$IFNDEF MSWINDOWS}
  dpiAwareI: integer;
  DpiAwareD: double;
  {$ENDIF}

implementation

//------------------------------------------------------------------------------
// Loading (reading) images from file ...
//------------------------------------------------------------------------------

class function TImageFormat_FMX.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  flag: Cardinal;
const
  SizeOfBitmapInfoHeader = 40;
  SizeOfBitmapV4Header = 108;
  SizeOfBitmapV5Header = 124;
begin
  Result := false;
  savedPos := stream.position;
  if stream.size - savedPos <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  stream.Position := savedPos;
  case flag of
    SizeOfBitmapInfoHeader,
    SizeOfBitmapV4Header,
    SizeOfBitmapV5Header: result := true;   //BMP
    $474E5089: result := true;              //PNG
    $38464947: result := true;              //GIF
    else if ((flag and $FFFF) = $4D42) or   //BMP
      ((flag and $FFFFFF) = $FFD8FF) then
        result := true;                     //JPG
  end;
end;
//------------------------------------------------------------------------------

function TImageFormat_FMX.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  cm: TBitmapCodecManager;
  surf: TBitmapSurface;
begin
  result := false;
  surf := TBitmapSurface.Create;
  cm := TBitmapCodecManager.Create;
  try
    cm.LoadFromStream(stream, surf);
    if (surf.Width = 0) or (surf.Height = 0) then Exit;
    if (surf.PixelFormat = TPixelFormat.BGRA) or
       (surf.PixelFormat = TPixelFormat.RGBA) then
          fPixelFormat := surf.PixelFormat
    else Exit;

    img32.SetSize(surf.Width, surf.Height);
    Move(surf.Scanline[0]^, img32.PixelBase^, surf.Width * surf.Height * 4);
    result := true;
  finally
    cm.Free;
    surf.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) Jpeg images to file ...
//------------------------------------------------------------------------------

function TImageFormat_FMX.SaveToFile(const filename: string; img32: TImage32): Boolean;
begin
  Ext := ExtractFileExt(filename);
  result := inherited;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_FMX.SaveToStream(stream: TStream; img32: TImage32);
var
  cm: TBitmapCodecManager;
  surf: TBitmapSurface;
begin
  if img32.IsEmpty then Exit;
  surf := TBitmapSurface.Create;
  cm := TBitmapCodecManager.Create;
  try
    surf.SetSize(img32.Width, img32.Height, TPixelFormat.BGRA);
    Move(img32.PixelBase^, surf.Scanline[0]^, img32.Width * img32.Height * 4);

    if Ext = '' then
      cm.SaveToStream(stream, surf, 'PNG') else
      cm.SaveToStream(stream, surf, Ext);
  finally
    cm.Free;
    surf.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_FMX.CopyToClipboard(img32: TImage32): Boolean;
var
  surf: TBitmapSurface;
  svc: IFMXClipboardService;
begin
  Result := assigned(img32) and not img32.IsEmpty and
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, svc);
  if not Result then Exit;

  surf := TBitmapSurface.Create;
  try
    surf.SetSize(img32.Width, img32.Height, TPixelFormat.BGRA);
    Move(img32.PixelBase^, surf.Scanline[0]^, img32.Width * img32.Height * 4);
    svc.SetClipboard(surf);
  finally
    surf.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_FMX.CanPasteFromClipboard: Boolean;
var
  svc: IFMXClipboardService;
  value: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, svc) then
  begin
    value := svc.GetClipboard;
    Result := Value.IsType<TBitmapSurface>;
  end else
    Result := false;
end;

//------------------------------------------------------------------------------

class function TImageFormat_FMX.PasteFromClipboard(img32: TImage32): Boolean;
var
  svc: IFMXClipboardService;
  value: TValue;
  surf: TBitmapSurface;
begin
  Result := false;
  if not assigned(img32) or
    not TPlatformServices.Current.SupportsPlatformService(
    IFMXClipboardService, svc) then Exit;

  value := svc.GetClipboard;
  if not Value.IsObject then Exit;

  if Value.IsType<TBitmapSurface> and
    ((Value.AsType<TBitmapSurface>.PixelFormat = TPixelFormat.RGBA) or
    (Value.AsType<TBitmapSurface>.PixelFormat = TPixelFormat.BGRA)) then
  begin
    surf := Value.AsType<TBitmapSurface>;
    img32.SetSize(surf.Width, surf.Height);
    Move(surf.Scanline[0]^, img32.PixelBase^, surf.Width * surf.Height * 4);
    Result := true;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure CheckScreenScale;
var
  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
  IFMXScreenService, IInterface(ScreenService)) then
    ScreenScale := ScreenService.GetScreenScale else
    ScreenScale := 1;
end;
//------------------------------------------------------------------------------

function DPIAwareFMX(val: Integer): Integer;
begin
  Result := Round(screenScale * val);
end;
//------------------------------------------------------------------------------

function DPIAwareFMX(val: double): double;
begin
  Result := screenScale * val;
end;
//------------------------------------------------------------------------------

procedure InitScreenScale;
var
  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService (
    IFMXScreenService, IInterface(ScreenService)) then
      screenScale := ScreenService.GetScreenScale else
      screenScale := 1.0;
end;
//------------------------------------------------------------------------------

{$IFNDEF MSWINDOWS}
procedure InitDpiVars;
begin
  dpiAwareI := DPIAwareFMX(1);
  DpiAwareD := DPIAwareFMX(1.0);
end;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('BMP', TImageFormat_FMX, cpLow);
  TImage32.RegisterImageFormatClass('PNG', TImageFormat_FMX, cpHigh);
  TImage32.RegisterImageFormatClass('JPG', TImageFormat_FMX, cpLow);
  TImage32.RegisterImageFormatClass('GIF', TImageFormat_FMX, cpLow);
  CheckScreenScale;
{$IFNDEF MSWINDOWS}
  InitDpiVars;
{$ENDIF}

end.

