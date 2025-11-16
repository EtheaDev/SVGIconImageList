{**
 @abstract(@name provides the common types to use in the renderer classes.)
 @author(JMR)
 @created(2016-2021 by Ursa Minor)
}
unit UTWRendererCommon;

interface

type
    {**
     Horizontal alignment enumeration
     @value(E_H_Left Aligned on the left)
     @value(E_H_Center Centered horizontally)
     @value(E_H_Right Aligned on the right)
    }
    EAlignHorz =
    (
        E_H_Left,
        E_H_Center,
        E_H_Right
    );

    {**
     Vertical alignment enumeration
     @value(E_V_Top Aligned on the top)
     @value(E_V_Center Centered vertically)
     @value(E_V_Bottom Aligned on the bottom)
    }
    EAlignVert =
    (
        E_V_Top,
        E_V_Center,
        E_V_Bottom
    );

    {**
     Orientation enumeration
     @value(E_O_Left Left oriented)
     @value(E_O_Top Top oriented)
     @value(E_O_Right Right oriented)
     @value(E_O_Bottom Bottom oriented)
    }
    EOrientation =
    (
        E_O_Left,
        E_O_Top,
        E_O_Right,
        E_O_Bottom
    );

    {**
     Text rendering type enumeration
     @value(E_R_None No text rendering type)
     @value(E_R_Default Each character is drawn using its glyph bitmap, with the system default
                        rendering hint. The text will be drawn using whatever font-smoothing settings
                        the user has selected for the system)
     @value(E_R_AntiAlias Each character is drawn using its antialiased glyph bitmap without hinting.
                          Better quality due to antialiasing. Stem width differences may be noticeable
                          because hinting is turned off)
     @value(E_R_AntiAlias_GridFit Each character is drawn using its antialiased glyph bitmap with
                                  hinting. Much better quality due to antialiasing, but at a higher
                                  performance cost)
     @value(E_R_ClearType_GridFit Each character is drawn using its glyph ClearType bitmap with hinting.
                                  The highest quality setting. Used to take advantage of ClearType font
                                  features)
     @value(E_R_SingleBitPerPixel Each character is drawn using its glyph bitmap. Hinting is not used)
     @value(E_R_SingleBitPerPixel_GridFit Each character is drawn using its glyph bitmap. Hinting is
                                          used to improve character appearance on stems and curvature)
     @br @bold(NOTE) WARNING Don't change enum elements order or values, used in type conversions
    }
    ETextRendering =
    (
        E_R_None,
        E_R_Default,
        E_R_AntiAlias,
        E_R_AntiAlias_GridFit,
        E_R_ClearType_GridFit,
        E_R_SingleBitPerPixel,
        E_R_SingleBitPerPixel_GridFit
    );

    {**
     Text trimming type enumeration
     @value(E_TT_None No trimming)
     @value(E_TT_Character Trim on first char that exceeds the draw rect)
     @value(E_TT_Word Trim on first word that exceeds the draw rect)
     @value(E_TT_EllipsisCharacter Using ellipsis, trim on first char that exceeds the draw rect)
     @value(E_TT_EllipsisWord Using ellipsis, trim on first word that exceeds the draw rect)
     @value(E_TT_EllipsisPath Using ellipsis, cut the string on the middle and ellipsify the center)
    }
    ETextTrimming =
    (
        E_TT_None,
        E_TT_Character,
        E_TT_Word,
        E_TT_EllipsisCharacter,
        E_TT_EllipsisWord,
        E_TT_EllipsisPath
    );

    {**
     Draw text function to use
     @value(E_DF_Default Draw text function is selected automatically)
     @value(E_DF_GDI Use the GDI to draw text)
     @value(E_DF_WS_Optimized Use the WindSolution custom implementation of DrawText() to draw text)
    }
    EDrawTextFunc =
    (
        E_DF_Default,
        E_DF_GDI,
        E_DF_WS_Optimized
    );

    {**
     Image resize algorithms
     @value(E_RzMode_Auto The resize algorithm is selected automatically)
     @value(E_RzMode_Nearest The nearest neighbour algorithm will be used to resize the image)
     @value(E_RzMode_Bilinear The bilinear algorithm will be used to resize the image)
     @value(E_RzMode_BilinearHQ The high quality bilinear algorithm will be used to resize the image)
     @value(E_RzMode_Bicubic The bicubic algorithm will be used to resize the image)
     @value(E_RzMode_BicubicHQ The high quality bicubic algorithm will be used to resize the image)
     @author Niki
    }
    EImageResizeMode =
    (
        E_RzMode_Auto,
        E_RzMode_Nearest,
        E_RzMode_Bilinear,
        E_RzMode_BilinearHQ,
        E_RzMode_Bicubic,
        E_RzMode_BicubicHQ
    );

implementation

end.
