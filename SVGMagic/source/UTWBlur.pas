{**
 @abstract(@name provides a blur effect to apply to a bitmap.)
 @author(JMR)
 @created(2016-2021, by Ursa Minor)
}
unit UTWBlur;

interface
    // do not include some GDI+ headers in hpp, because they may generate conflicts in C++ code
    (*$NOINCLUDE Winapi.GdipObj *)

uses System.SysUtils,
     Vcl.Graphics,
     Winapi.Windows,
     Winapi.GDIPAPI,
     Winapi.GDIPOBJ,
     UTWSmartPointer,
     UTWColor;

type
    {**
     Blur effect
     @br @bold(NOTE) GDI+ is required to use this class
    }
    TWBlur = class
        public type
            {**
             Resize mode
             @value(IE_Nearest Nearest neighbour algorithm will be used to resize the bitmap)
             @value(IE_Bilinear Bilinear algorithm will be used to resize the bitmap)
             @value(IE_HQBilinear High quality bilinear algorithm will be used to resize the bitmap)
             @value(IE_Bicubic Bicubic algorithm will be used to resize the bitmap)
             @value(IE_HQBicubic High quality bicubic algorithm will be used to resize the bitmap)
            }
            IEResizeMode =
            (
                IE_Nearest,
                IE_Bilinear,
                IE_HQBilinear,
                IE_Bicubic,
                IE_HQBicubic
            );

            {**
             Called when a pixel is processed
             @param(pixelNb Currently processed pixel index)
             @param(pixelCount Pixel count)
            }
            ITfOnPixelProcessed = procedure(pixelNb, pixelCount: Integer) of object;

        private type
            {**
             Byte per pixel enumeration
             @value(IE_BPP24 24 bit per pixels)
             @value(IE_BPP32 32 bit per pixels)
            }
            IEBPP =
            (
                IE_BPP24 = 24,
                IE_BPP32 = 32
            );

            IOffsets = array of Integer;

            {**
             Blur matrix, used to calculate as quick as possible the blured pixel
            }
            IMatrix = record
                private
                    m_Offsets: IOffsets;

                public
                    {**
                     Initialize matrix
                     @param(blurSize Blur size)
                     @param(bitmapWidth Bitmap width)
                    }
                    procedure Initialize(const blurSize: TSize; bitmapWidth: NativeUInt);

                    {**
                     Get pixel color
                     @param(pData Bitmap data)
                     @param(index Pixel index)
                     @param(size Size of data (pixel count))
                     @param(delta Pixel delta to skip)
                     @param(bitPerPixel Number of bits used for each bitmap pixel)
                    }
                    function GetPixelColor(pData: PByte; index, size, delta, bitPerPixel: NativeUInt): TWColor;
            end;

        public
            {**
             Constructor
            }
            constructor Create; virtual;

            {**
             Destructor
            }
            destructor Destroy; override;

            {**
             Apply gaussian blur effect to bitmap
             @param(pBitmap Bitmap for which blur is applied)
             @param(blurSize Blur size)
             @param(alphaEdgesOnly If @true, only transparent pixels will be processed)
             @param(fOnPixelProcessed On pixel processed callback function, can be @nil if not used)
             @returns(Blured bitmap, @nil on error)
             @br @bold(NOTE) A frame corresponding to blur size values must be reserved around image,
                             otherwise the resulting blured image will be cropped
             @br @bold(NOTE) Resulting bitmap must be deleted by caller
            }
            function Apply(pBitmap: Vcl.Graphics.TBitmap; const blurSize: TSize; alphaEdgesOnly: Boolean;
                    fOnPixelProcessed: ITfOnPixelProcessed = nil): Vcl.Graphics.TBitmap; overload; virtual;

            {**
             Apply blur effect to bitmap by reducing the image then enlarging again
             @param(pBitmap Bitmap for which blur is applied)
             @param(blurSize Blur size)
             @param(resizeMode Resize mode (it's the interpolation algorithm used by GDI+))
             @param(gdiPlusToken GDI+ token)
             @returns(Blured bitmap, @nil on error)
             @br @bold(NOTE) A frame corresponding to blur size values must be reserved around image,
                             otherwise the resulting blured image will be cropped
             @br @bold(NOTE) Resulting bitmap must be deleted by caller
             @br @bold(NOTE) This function uses GDI+ internally. For this reason, GDI+ should be
                             initialized externally and his token should be passed to this function
            }
            function Apply(pBitmap: Vcl.Graphics.TBitmap; const blurSize: TSize; resizeMode: IEResizeMode;
                    gdiPlusToken: ULONG_PTR): Vcl.Graphics.TBitmap; overload; virtual;
    end;

implementation
//---------------------------------------------------------------------------
// TWBlur.IMatrix
//---------------------------------------------------------------------------
procedure TWBlur.IMatrix.Initialize(const blurSize: TSize; bitmapWidth: NativeUInt);
var
    blurHalfSizeX, blurHalfSizeY, x, y: Integer;
    offset:                             NativeUInt;
begin
    // prepare offsets matrix
    SetLength(m_Offsets, blurSize.Width * blurSize.Height);

    blurHalfSizeX := blurSize.Width  div 2;
    blurHalfSizeY := blurSize.Height div 2;
    offset        := 0;

    // iterate through matrix rows and columns to create
    for y := -blurHalfSizeY to blurHalfSizeY do
        for x := -blurHalfSizeX to blurHalfSizeX do
        begin
            // calculate relative pixel offset
            m_Offsets[offset] := ((y * Integer(bitmapWidth)) + x);
            Inc(offset);
        end;
end;
//---------------------------------------------------------------------------
function TWBlur.IMatrix.GetPixelColor(pData: PByte; index, size, delta, bitPerPixel: NativeUInt): TWColor;
var
    r, g, b, a, processed: Cardinal;
    offsetCount, i:        NativeUInt;
    offset:                Integer;
begin
    r         := 0;
    g         := 0;
    b         := 0;
    a         := 0;
    processed := 0;

    // get offset count
    offsetCount := Length(m_Offsets);

    // iterate through offsets to calculate
    if (offsetCount > 0) then
        for i := 0 to offsetCount - 1 do
        begin
            // get curent offset
            offset := index + (NativeUInt(m_Offsets[i]) * delta);

            // is offset out of bounds?
            if ((offset < 0) or (NativeUInt(offset) >= (size - 1))) then
                continue;

            // search for bit per pixel sample to use
            case (IEBPP(bitPerPixel)) of
                // bitmap image contains 24 bits per pixel
                IE_BPP24:
                begin
                    // calculate pixels sum
                    Inc(r, pData[offset]);
                    Inc(g, pData[offset + 1]);
                    Inc(b, pData[offset + 2]);
                end;

                // bitmap image contains 32 bits per pixel
                IE_BPP32:
                begin
                    // calculate pixels sum
                    Inc(r, pData[offset]);
                    Inc(g, pData[offset + 1]);
                    Inc(b, pData[offset + 2]);
                    Inc(a, pData[offset + 3]);
                end;
            else
                raise Exception.CreateFmt('Unknown bit per pixel count - %d', [bitPerPixel]);
            end;

            Inc(processed);
        end;

    // no pixel processed?
    if (processed = 0) then
        Exit(TWColor.Create(0, 0, 0, 0));

    // search for bit per pixel sample to use
    case (IEBPP(bitPerPixel)) of
        IE_BPP24: Result := TWColor.Create(r div processed, g div processed, b div processed);
        IE_BPP32: Result := TWColor.Create(r div processed, g div processed, b div processed, a div processed);
    else
        raise Exception.CreateFmt('Unknown bit per pixel count - %d', [bitPerPixel]);
    end;
end;
//---------------------------------------------------------------------------
// TWBlur
//---------------------------------------------------------------------------
constructor TWBlur.Create;
begin
    inherited Create;
end;
//---------------------------------------------------------------------------
destructor TWBlur.Destroy;
begin
    inherited Destroy;
end;
//---------------------------------------------------------------------------
function TWBlur.Apply(pBitmap: Vcl.Graphics.TBitmap; const blurSize: TSize; alphaEdgesOnly: Boolean;
        fOnPixelProcessed: ITfOnPixelProcessed = nil): Vcl.Graphics.TBitmap;
var
    bitmap:                                        TBitmap;
    matrix:                                        IMatrix;
    pixelColor:                                    TWColor;
    pBitmapData, pResultData:                      PByte;
    pixelCount, bitmapSize, pixelDelta, i, offset: NativeUInt;
    success:                                       Boolean;
begin
    Result := nil;

    // no bitmap?
    if (not Assigned(pBitmap)) then
        Exit;

    success := False;

    try
        pBitmapData := nil;
        pResultData := nil;

        try
            // get pixel count
            pixelCount := pBitmap.Width * pBitmap.Height;

            // get bitmap
            GetObject(pBitmap.Handle, SizeOf(TBitmap), @bitmap);

            // get bitmap size
            bitmapSize := bitmap.bmWidthBytes * bitmap.bmHeight;

            // get bitmap data
            GetMem(pBitmapData, bitmapSize * SizeOf(Byte));
            GetBitmapBits(pBitmap.Handle, bitmapSize, pBitmapData);

            // create result data
            GetMem(pResultData, bitmapSize * SizeOf(Byte));

            // create blur matrix
            matrix.Initialize(blurSize, bitmap.bmWidth);

            // get bitmap pixel delta
            pixelDelta := bitmap.bmBitsPixel shr 3;

            // iterate through pixels to blur
            if (pixelCount > 0) then
                for i := 0 to pixelCount - 1 do
                begin
                    // calculate pixel offset
                    offset := i * pixelDelta;

                    // do ignore non-alpha pixels?
                    if (alphaEdgesOnly and (bitmap.bmBitsPixel = Integer(IE_BPP32))
                            and (pBitmapData[offset + 3] = 255))
                    then
                    begin
                        CopyMemory(@pResultData[offset], @pBitmapData[offset], 4);

                        // notify that pixel is processed
                        if (Assigned(fOnPixelProcessed)) then
                            fOnPixelProcessed(i, pixelCount);

                        continue;
                    end;

                    // get blured pixel color
                    pixelColor := matrix.GetPixelColor(pBitmapData, offset, bitmapSize, pixelDelta,
                            bitmap.bmBitsPixel);

                    // search for bit per pixel sample to use
                    case (IEBPP(bitmap.bmBitsPixel)) of
                        // bitmap image contains 24 bits per pixel
                        IE_BPP24:
                        begin
                            // apply blur to resulting bitmap
                            pResultData[offset]     := pixelColor.GetRed;
                            pResultData[offset + 1] := pixelColor.GetGreen;
                            pResultData[offset + 2] := pixelColor.GetBlue;
                        end;

                        // bitmap image contains 32 bits per pixel
                        IE_BPP32:
                        begin
                            // apply blur to resulting bitmap
                            pResultData[offset]     := pixelColor.GetRed;
                            pResultData[offset + 1] := pixelColor.GetGreen;
                            pResultData[offset + 2] := pixelColor.GetBlue;
                            pResultData[offset + 3] := pixelColor.GetAlpha;
                        end;

                    else
                        raise Exception.CreateFmt('Unknown bit per pixel count - %d', [bitmap.bmBitsPixel]);
                    end;

                    // notify that pixel is processed
                    if (Assigned(fOnPixelProcessed)) then
                        fOnPixelProcessed(i, pixelCount);
                end;

            // create compatible destination bitmap
            Result             := Vcl.Graphics.TBitmap.Create;
            Result.PixelFormat := pBitmap.PixelFormat;
            Result.AlphaFormat := pBitmap.AlphaFormat;
            Result.SetSize(pBitmap.Width, pBitmap.Height);
            SetBitmapBits(Result.Handle, bitmapSize, pResultData);
        finally
            // delete bitmap data
            if (Assigned(pBitmapData)) then
                Dispose(pBitmapData);

            // delete result data
            if (Assigned(pResultData)) then
                Dispose(pResultData);
        end;

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);
    end;
end;
//---------------------------------------------------------------------------
function TWBlur.Apply(pBitmap: Vcl.Graphics.TBitmap; const blurSize: TSize; resizeMode: IEResizeMode;
        gdiPlusToken: ULONG_PTR): Vcl.Graphics.TBitmap;
var
    pSource, pSrcInstance, pIntermediate, pIntInstance: IWSmartPointer<TGpBitmap>;
    pSmall:                                             IWSmartPointer<Vcl.Graphics.TBitmap>;
    pSmallGraphics, pResultGraphics:                    IWSmartPointer<TGpGraphics>;
    imageRect, smallRect:                               TGpRectF;
    pAttributes:                                        TGpImageAttributes;
    iMode:                                              InterpolationMode;
    smallImgWidth, smallImgHeight:                      Integer;
    success:                                            Boolean;
begin
    Result := nil;

    // is GDI+ initialized?
    if (gdiPlusToken = 0) then
        Exit;

    // no bitmap?
    if (not Assigned(pBitmap)) then
        Exit;

    // nothing to transform?
    if ((blurSize.Width < 2) and (blurSize.Height < 2)) then
    begin
        // copy and return source image
        Result := Vcl.Graphics.TBitmap.Create;
        Result.Assign(pBitmap);
        Exit;
    end;

    // get GDI+ source image
    pSrcInstance := TWSmartPointer<TGpBitmap>.Create();
    pSource      := TWSmartPointer<TGpBitmap>.Create(pSrcInstance.FromHBITMAP(pBitmap.Handle, 0));

    // found it?
    if (not Assigned(pSource)) then
        Exit;

    // calculate small image width
    if (blurSize.Width <> 0) then
        smallImgWidth := pBitmap.Width div blurSize.Width
    else
        smallImgWidth := pBitmap.Width;

    // calculate small image height
    if (blurSize.Height <> 0) then
        smallImgHeight := pBitmap.Height div blurSize.Height
    else
        smallImgHeight := pBitmap.Height;

    // create intermediate blur image
    pSmall             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
    pSmall.PixelFormat := pBitmap.PixelFormat;
    pSmall.AlphaFormat := pBitmap.AlphaFormat;
    pSmall.SetSize(smallImgWidth, smallImgHeight);

    pAttributes := nil;
    success     := False;

    try
        // create final blur image
        Result             := Vcl.Graphics.TBitmap.Create;
        Result.PixelFormat := pBitmap.PixelFormat;
        Result.AlphaFormat := pBitmap.AlphaFormat;
        Result.SetSize(pBitmap.Width, pBitmap.Height);

        // choose interpolation mode
        case (resizeMode) of
            IE_Nearest:    imode := InterpolationModeNearestNeighbor;
            IE_Bilinear:   imode := InterpolationModeBilinear;
            IE_HQBilinear: imode := InterpolationModeHighQualityBilinear;
            IE_Bicubic:    imode := InterpolationModeBicubic;
            IE_HQBicubic:  imode := InterpolationModeHighQualityBicubic;
        else
            raise Exception.CreateFmt('Unknown resize mode - %d', [Integer(resizeMode)]);
        end;

        // calculate normal image rectangles
        imageRect.X      := 0;
        imageRect.Y      := 0;
        imageRect.Width  := pSource.GetWidth;
        imageRect.Height := pSource.GetHeight;

        // calculate small image rectangles
        smallRect.X      := 0;
        smallRect.Y      := 0;
        smallRect.Width  := smallImgWidth;
        smallRect.Height := smallImgHeight;

        pAttributes := TGpImageAttributes.Create;
        pAttributes.SetNoOp(ColorAdjustTypeText);

        // get small image gdi+ graphics
        pSmallGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(pSmall.Canvas.Handle));

        // set small image interpolation mode
        pSmallGraphics.SetInterpolationMode(iMode);

        // draw small image (and thus reduce resolution)
        pSmallGraphics.DrawImage(pSource, smallRect, 0, 0, imageRect.Width, imageRect.Height,
                UnitPixel, pAttributes);

        // get GDI+ intermediate image
        pIntInstance  := TWSmartPointer<TGpBitmap>.Create();
        pIntermediate := TWSmartPointer<TGpBitmap>.Create(pIntInstance.FromHBITMAP(pSmall.Handle, 0));

        // get blured image gdi+ graphics
        pResultGraphics := TWSmartPointer<TGpGraphics>.Create(TGpGraphics.Create(Result.Canvas.Handle));

        // set blured image interpolation mode
        pResultGraphics.SetInterpolationMode(iMode);

        // draw blured image (by enlarging reduced image)
        pResultGraphics.DrawImage(pIntermediate, imageRect, 0, 0, smallRect.Width, smallRect.Height,
                UnitPixel, pAttributes);

        success := True;
    finally
        if (not success) then
            FreeAndNil(Result);

        pAttributes.Free;
    end;
end;
//---------------------------------------------------------------------------

end.
