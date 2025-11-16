{**
 @abstract(@name provides a factory to populate a TImageList from SVG graphics.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWSVGImageListFactory;

interface

uses Vcl.Controls,
     Vcl.Graphics,
     UTWHelpers,
     UTWSmartPointer,
     UTWSVGGraphic;

type
    {**
     Image list factory
    }
    TWSVGImageListFactory = record
        {**
         Add the content of a SVG graphic list in an image list
         @param(pGraphics SVG graphic list to use to populate the image list)
         @param(pImageList Image list to populate)
         @br @bold(NOTE) To support the alpha transparency, the image list should be configured as
                         follow:
                         @unorderedList(@item(@bold(ColorDepth) should be set to @italic(32bit))
                                        @item(@bold(DrawingStyle) should be set to @italic(dsTransparent))
                                        @item(@bold(Masked) should be set to @italic(False)))
         @br @bold(NOTE) The SVG images will be added immediately after the last image in the list.
                         The previous list content WILL NOT be deleted
        }
        class function AddTo(const pGraphics: TWSVGGraphics; pImageList: TImageList): Boolean; static;
    end;

implementation
//---------------------------------------------------------------------------
class function TWSVGImageListFactory.AddTo(const pGraphics: TWSVGGraphics; pImageList: TImageList): Boolean;
var
    pGraphic: TWSVGGraphic;
    pBitmap:  IWSmartPointer<Vcl.Graphics.TBitmap>;
begin
    if (not Assigned(pGraphics)) then
        Exit(False);

    if (not Assigned(pImageList)) then
        Exit(False);

    // iterate through SVG graphics to convert
    for pGraphic in pGraphics do
    begin
        // create a new 32 bit bitmap matching with the image list format
        pBitmap             := TWSmartPointer<Vcl.Graphics.TBitmap>.Create();
        pBitmap.PixelFormat := pf32bit;
        pBitmap.AlphaFormat := afDefined;
        pBitmap.Width       := pImageList.Width;
        pBitmap.Height      := pImageList.Height;

        // empty the bitmap background
        TWGDIHelper.Clear(pBitmap);

        // draw the SVG on the bitmap
        pBitmap.Canvas.Draw(0, 0, pGraphic);

        // add bitmap containing the SVG picture to the image list
        pImageList.Add(pBitmap, nil);
    end;

    Result := True;
end;
//---------------------------------------------------------------------------

end.
