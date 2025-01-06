unit Img32.Fmt.QOI;
(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  QOI file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

(*******************************************************************************
QOI - The "Quite OK Image" format for fast, lossless image compression
Dominic Szablewski - https://phoboslab.org
LICENSE: The MIT License(MIT)
Copyright(c) 2021 Dominic Szablewski
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files(the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and / or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions :
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*******************************************************************************)

interface
{$I Img32.inc}

uses
  SysUtils, Classes, Math, Img32, Img32.Vector;

type
  TImageFormat_QOI = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; override;
    // SaveToStream: the compressionQuality parameter is ignored here
    procedure SaveToStream(stream: TStream;
      img32: TImage32; compressionQuality: integer = 0); override;
    class function CanCopyToClipboard: Boolean; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

implementation

const
  QOI_OP_INDEX    = $0;
  QOI_OP_DIFF     = $40;
  QOI_OP_LUMA     = $80;
  QOI_OP_RUN      = $C0;
  QOI_OP_RGB      = $FE;
  QOI_OP_RGBA     = $FF;
  QOI_MASK_2      = $C0;
  QOI_MAGIC       = $66696F71;
  QOI_HEADER_SIZE = 14;
  qoi_padding: array[0..7] of byte = (0,0,0,0,0,0,0,1);
  qoi_padding_size = 8;

type
  TQOI_DESC = packed record
    magic      : Cardinal;
    width      : Cardinal;
    height     : Cardinal;
    channels   : byte;
    colorspace : byte;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function QOI_COLOR_HASH(c: TARGB): Byte;  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (c.r*3 + c.g*5 + c.b*7 + c.a*11) mod 64;
end;
//------------------------------------------------------------------------------

function SwapBytes(Value: Cardinal): Cardinal;
var
  v: array[0..3] of byte absolute Value;
  r: array[0..3] of byte absolute Result;
begin
  r[3] := v[0];
  r[2] := v[1];
  r[1] := v[2];
  r[0] := v[3];
end;
//------------------------------------------------------------------------------

function ReadByte(var p: PByte): Byte; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Byte(p^); //nb: Delphi 7 compatability
  inc(p);
end;

//------------------------------------------------------------------------------
// Loading (reading) QOI images from file ...
//------------------------------------------------------------------------------

{*R-}
function TImageFormat_QOI.LoadFromStream(stream: TStream;
  img32: TImage32; imgIndex: integer = 0): Boolean;
var
  i, size, run, vg: integer;
  desc: TQOI_DESC;
  index: array[0..63] of TARGB;
  px: TARGB;
  b1, b2: byte;
  dst: PARGB;
  src: PByte;
  srcTmp: TArrayOfByte;
begin
  Result := false;
  if not Assigned(stream) or not Assigned(img32) then Exit;
  size := stream.Size - stream.Position;

  if size < QOI_HEADER_SIZE + qoi_padding_size then Exit;

  if stream is TMemoryStream then
  begin
    src := TMemoryStream(stream).Memory;
    inc(src, stream.Position);
  end else
  begin
    NewByteArray(srcTmp, size, True);
    stream.Read(srcTmp[0], size);
    src := @srcTmp[0];
  end;

  Move(src^, desc, SizeOf(TQOI_DESC));
  inc(src, SizeOf(TQOI_DESC));
  img32.BeginUpdate;
  try
    with desc do
    begin
      width := SwapBytes(width);
      height := SwapBytes(height);
      if (magic <> QOI_MAGIC) or (width = 0) or (height = 0) or
        (channels < 3) or (channels > 4) or (colorspace > 1) then
          Exit;
      img32.SetSize(width, height);
    end;
    Result := true;
    px.Color      := clBlack32;
    run := 0;
    FillChar(index, SizeOf(index), 0);
    dst := PARGB(img32.PixelBase);

    for i := 0 to img32.Width * img32.Height - 1 do
    begin
      if (run > 0) then
      begin
        Dec(run);
      end else
      begin
        b1 := ReadByte(src);
        if (b1 = QOI_OP_RGB) then
        begin
          px.R := ReadByte(src);
          px.G := ReadByte(src);
          px.B := ReadByte(src);
        end
        else if (b1 = QOI_OP_RGBA) then
        begin
          px.R := ReadByte(src);
          px.G := ReadByte(src);
          px.B := ReadByte(src);
          px.A := ReadByte(src);
        end
        else if ((b1 and QOI_MASK_2) = QOI_OP_INDEX) then
        begin
          px := index[b1];
        end
        else if (b1 and QOI_MASK_2) = QOI_OP_DIFF then
        begin
          px.R := px.R + ((b1 shr 4) and 3) - 2;
          px.G := px.G + ((b1 shr 2) and 3) - 2;
          px.B := px.B + (b1 and 3) - 2;
        end
        else if (b1 and QOI_MASK_2) = QOI_OP_LUMA then
        begin
          b2 := ReadByte(src);
          vg := (b1 and $3f) - 32;
          px.R := px.R + vg - 8 + ((b2 shr 4) and $f);
          px.G := px.G + vg;
          px.B := px.B + vg - 8 + (b2 and $f);
        end
        else if (b1 and QOI_MASK_2) = QOI_OP_RUN then
          run := (b1 and $3f);
        index[QOI_COLOR_HASH(px)] := px;
      end;
      dst.Color := px.Color;
      inc(dst);
    end;
  finally
    img32.EndUpdate;
  end;
end;
{*R+}

//------------------------------------------------------------------------------
// Saving (writing) SVG images to file (not currently implemented) ...
//------------------------------------------------------------------------------

class function TImageFormat_QOI.IsValidImageStream(stream: TStream): Boolean;
var
  savedPos: integer;
  desc: TQOI_DESC;
begin
  savedPos := stream.Position;
  Result := stream.Size - savedPos >= QOI_HEADER_SIZE + qoi_padding_size;
  if not Result then Exit;
  stream.Read(desc, QOI_HEADER_SIZE);
  stream.Position := savedPos;
  with desc do
  begin
    width := SwapBytes(width);
    height := SwapBytes(height);
    Result := (magic = QOI_MAGIC) and (width > 0) and (height > 0) and
      (channels >= 3) and (channels <= 4) and (colorspace < 2);
  end;
end;

//------------------------------------------------------------------------------

procedure qoi_write_32(var p: PByte; val: Cardinal);
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  PCardinal(p)^ := val; inc(p, SizeOf(Cardinal));
end;
//------------------------------------------------------------------------------

procedure qoi_write_8(var p: PByte; val: Byte);
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Byte(p^) := val; inc(p); //Delphi 7
end;
//------------------------------------------------------------------------------

procedure TImageFormat_QOI.SaveToStream(stream: TStream;
  img32: TImage32; compressionQuality: integer);
var
  i, max_size, run: integer;
  vr, vg, vb, vg_r, vg_b: integer;
	index_pos: integer;
  bytes: TArrayOfByte;
  dst: PByte;
  src: PARGB;
  index: array[0..63] of TARGB;
	px_prev: TARGB;
begin
  if not Assigned(stream) or not Assigned(img32) or img32.IsEmpty then Exit;
  max_size := img32.width * img32.height * Sizeof(TColor32) +
		QOI_HEADER_SIZE + qoi_padding_size;
  SetLength(bytes, max_size);
  dst := @bytes[0];
  qoi_write_32(dst, QOI_MAGIC);

	qoi_write_32(dst, SwapBytes(img32.width));
	qoi_write_32(dst, SwapBytes(img32.height));
  qoi_write_8(dst, 4); //channels
  qoi_write_8(dst, 0); //colorspace

  run := 0;
  px_prev.Color := clBlack32;
  FillChar(index, SizeOf(index), 0);

  src := PARGB(img32.PixelBase);
  max_size := img32.Width * img32.Height;

  for i := 1 to max_size do
  begin
    if src.Color = px_prev.Color then
    begin
      inc(run);
			if (run = 62) or (i = max_size) then
      begin
        qoi_write_8(dst, QOI_OP_RUN or (run - 1));
				run := 0;
      end;
    end else
    begin
      if (run > 0) then
      begin
        qoi_write_8(dst, QOI_OP_RUN or (run - 1));
        run := 0;
      end;

      index_pos := QOI_COLOR_HASH(src^);
      if (index[index_pos].Color = src.Color) then
      begin
        qoi_write_8(dst, QOI_OP_INDEX or index_pos);
      end else
      begin
        index[index_pos] := src^;
        if (src.a = px_prev.a) then
        begin
          vr := src.r - px_prev.r;
          vg := src.g - px_prev.g;
          vb := src.b - px_prev.b;
          vg_r := vr - vg;
          vg_b := vb - vg;
          if ((vr > -3) and (vr < 2) and
            (vg > -3) and (vg < 2) and
            (vb > -3) and (vb < 2)) then
          begin
            qoi_write_8(dst, QOI_OP_DIFF or
              (vr + 2) shl 4 or (vg + 2) shl 2 or (vb + 2));
          end
          else if (
            (vg_r > -9) and (vg_r < 8) and
            (vg > -33) and (vg < 32) and
            (vg_b > -9) and (vg_b < 8)) then
          begin
            qoi_write_8(dst, QOI_OP_LUMA or (vg + 32));
            qoi_write_8(dst, (vg_r + 8) shl 4 or (vg_b + 8));
          end else
          begin
            qoi_write_8(dst, QOI_OP_RGB);
            qoi_write_8(dst, src.R);
            qoi_write_8(dst, src.G);
            qoi_write_8(dst, src.B);
          end
        end else
        begin
            qoi_write_8(dst, QOI_OP_RGBA);
            qoi_write_8(dst, src.R);
            qoi_write_8(dst, src.G);
            qoi_write_8(dst, src.B);
            qoi_write_8(dst, src.A);
        end;
      end;
    end;
    px_prev := src^;
    inc(src);
  end;

  for i := 0 to 7 do
    qoi_write_8(dst, qoi_padding[i]); //colorspace
  max_size := dst - PByte(@bytes[0]);
  stream.Write(bytes[0], max_size);
end;

//------------------------------------------------------------------------------

class function TImageFormat_QOI.CanCopyToClipboard: Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------

class function TImageFormat_QOI.CopyToClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
class function TImageFormat_QOI.CanPasteFromClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_QOI.PasteFromClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('QOI', TImageFormat_QOI, cpLow);

end.
