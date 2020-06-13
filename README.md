# SVGIconImageList [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

## An extended ImageList for Delphi (VCL+FMX) to simplify use of SVG Icons (resize, opacity, grayscale and more...)

| Component | Description |
| - | - |
| ![https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageListComponentIcon.png](https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageListComponentIcon.png) | **SVGIconImageList is an extended ImageList for Delphi (VCL+FMX) to simplify use of SVG Icons (resize, opacity, grayscale and more...)** |
| ![https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageComponentIcon.png](https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageComponentIcon.png) | **SVGIconImage is an extended Image component for Delphi (VCL+FMX) to show any SVG image directly or included into a an SVGIconImageList with all functionality (stretch, opacity, grayscale and more...)** |

SVGIconImageList, as a TVirtualImageList+TImageCollection, can draw icons in SVG format at any resolution. The ImageList scales automatically when DPI changed. You can use those components from Delphi DXE6 to actual version.

![Delphi 10.4 Sydney Support](/Demo/Images/SupportingDelphi.jpg)

**Sample image of VCL version**
![https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/Sample.jpg](https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/Sample.jpg)

**Sample image of FMX (Windows) version**
![https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/SampleFMX.jpg](https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/SampleFMX.jpg)

**Sample image of the VCL SVGText-property editor**
![https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/SVGTextPropertyEditor.jpg](https://github.com/EtheaDev/SVGIconImageList/blob/master/Demo/Images/SVGTextPropertyEditor.jpg)

**DOCUMENTATION**

Follow the [guide in Wiki section](https://github.com/EtheaDev/SVGIconImageList/wiki) to known how to use those components to modernize your Delphi VCL or FMX Windows applications scalable, colored and beautiful with few lines of code.

![https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageListComponentIcon.png](https://github.com/EtheaDev/SVGIconImageList/blob/master/Packages/SVGIconImageListComponentIcon.png)

**RELEASE NOTES:**

13 June 2020: versione 1.5 (VCL+FMX)
 - Added support for DisabledGrayScale and DisabledOpacity as in VirtualImageList
 - Fixed drawing disabled icons also with VCLStyles active

09 June 2020: versione 1.4 (VCL+FMX)
- Added GrayScale and FixedColor to ImageList for every Icons
- Added GrayScale and FixedColor for single Icon
- Added some complex svg demo images
- Updated demos

06 June 2020: version 1.3 (VCL+FMX)
- Added property editor for TSVGIconImage.SVGText and TSVGIconItem.SVGText
- Fixed some drawing problems with transform attribute
- Fixed rescaling icons when monitor DPI changes

28 May 2020: version 1.2 (VCL+FMX)
- Complete support of **Delphi 10.4**
- Added support for other Delphi versions (VCL): **DXE6**, **DXE8**, **D10.1**
- Added position memory of component editor
- Fixed Issue: Icon Editor not keeping added icons
- Fixed Issue: SVG with exponent notation does not parse correctly and affects image display

25 May 2020: version 1.1 (VCL+FMX)
- Added the component **TSVGIconImageListFMX** with advanced component editor.
- Added the component **TSVGIconImageFMX** to show SVG into a TImage.
- Demos to show how they works.
- Very high performance for building hundreds of icons.

24 May 2020: first version 1.0 (VCL)
- Added the component **TSVGIconImageList** with advanced component editor.
- Added the component **TSVGIconImage** to show SVG into a TImage.
- Demos to show how they works.
- Very high performance for building hundreds of icons.
- Support from Delphi 10.2 to 10.4 Sydney (other Delphi versions coming soon)

Those components uses library SVG Martin Walter (Original version (c) 2005, 2008) with license:
Use of this file is permitted for commercial and non-commercial. Use, as long as the author is credited.
home page: http://www.mwcs.de  email: martin.walter@mwcs.de 
This library is included in this project into svg folder

**TSVGIconImageList** and **TSVGIconImage** are similar to **TSVGImageList** and **TSVGImage** included into project: [https://github.com/ekot1/DelphiSVG.git](https://github.com/ekot1/DelphiSVG.git)
but those versions are more efficient in performances, and adds some features like SVGText property, store icons in binary or SVGText format into dfm and more...

**TSVGIconImageListFMX** and **TSVGIconImageFMX** are similar to **TIconFontsImageListFMX** and **TIconFontsImage** included into similar project made by Ethea for Icon Fonts: [https://github.com/EtheaDev/IconFontsImageList](https://github.com/EtheaDev/IconFontsImageList)
