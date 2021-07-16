
Image32 - 2D graphics library for Delphi Pascal
Latest version: 2.26
Released: 13 July 2021

Copyright © 2019-2021 Angus Johnson
Freeware released under Boost Software License
https://www.boost.org/LICENSE_1_0.txt

Documentation : http://www.angusj.com/delphi/image32/Docs/
Download      : https://sourceforge.net/projects/image32/files/

Recent changes:

Version 2.26
  Image32_SVG_Reader
    minor bugfixes
  Image32_Ttf
    Added TFontManager class 
    Added FontManager function

Version 2.25
  Image32_SVG
    New unit added 
      for simple loading of SVG images into TImage32
  Image32_SVG_Reader
    Fixed a significant bug (related to DecimalSeparator)
    Added text encoding detection
Version 2.24
  Image32_SVG_Reader
    New unit added
  Image32_SVG_Writer
    New unit added
  Image32_Vector
    DefaultMiterLimit changed to 4 (same as SVG specification)
    Removed deprecated esClosed from TEndStyle
      (use esPolygon instead)
    Improved RoundRect function    
  Image32_Transform
    Changed MatrixSkew parameters
  Image32Panels
    Renamed CenterImagePoint function to RecenterImageAt
  Image32_SmoothPath
    SmoothToBezier function moved to Image32_Extra
  Image32_Resamplers
    Added BoxDownSampling function
  Image32_Draw
    Added TGradientFillStyle parameter to
      TSvgRadialGradientRenderer.SetParameters
    Minor bugfix in gradient rendering
  Image32_Extra
    Modified Erase procedure
    Added EraseInverted procedure
    Replaced buggy BoxBlur function with
      new FastGaussianBlur function
  Examples
    Added SVG & SVG2 example apps.
  Numerous minor bugfixes
    
Version 2.23 :)
	
Version 2.22
  Image32_Resamplers
    Fixed minor bug in BilinearResampler
    Fixed minor transparency bug in BicubicResampler
  Image32_Transform
    Minor updates to SplineHorzTransform and 
      SplineVertTransform functions
  Image32Panels
	Minor update
  Image32
	RegisterResampler function parameters changed
	GetResamplerList procedure added
  Image32_Vector
    Added GetDistances and GetCumulativeDistances functions
	
Version 2.21
  Image32_Resamplers
    Fixed bug in BicubicResampler
  Image32_Transform
    Tidied up Spline transform algorithms

Version 2.20
  Image32_Resamplers
    New library unit added containing 3 resampler functions
      NearestResampler  - draft quality, fast
      BilinearResampler - high quality , average speed (default)
      BicubicResampler  - best quality , slow
  Image32
    DefaultResampler variable added
    RegisterResampler function added
    TImage32.AntiAliased property removed and replaced with a new 
      Resampler property (defaults to DefaultResampler)
  Image32_Layers
    TLayeredImage32.AntiAliased property removed and replaced
	  with a new Resampler property
    Fixed broken layer opacity and other minor bugs
    CreateRotatingButtonGroup parameters changed
  Image32_Transform
    Transform functions will now use the resampler associated 
	with  the image being transformed
  Image32_Ttf
    TGlyphCache.GetTextGlyphs function result changed
    TGlyphCache.GetAngledTextGlyphs function result changed
  Image32_Vector
    JoinPath procedure renamed AppendPath
  Image32Panels
    Implemented WM_MOUSEHWHEEL for horizontal scrolling

Version 2.19
  Image32_Layers
    Added AntiAliased property to TLayeredImage32
    Renamed TGroupLayer32.OnMerge property to OnBeforeMerge
    Added TGroupLayer32.OnAfterMerge property
    Added THitTestLayer32.ClearHitTesting method
    Minor tidy up.
  Image32_Transform
    Transforms can now have antialiasing disabled    
    Added AffineTransformImageRaw procedure
  Image32_Extra
    Added ResizeAndCenterImgForRotation procedure 
  Added Image32Panels
    which contains a TImage32 enhanced TPanel component
	
Version 2.18
  Image32_Layers
    Refactored and improved TRotateLayer32
    Improved TRotatingGroupLayer32
  Examples
    Updated Layers201 example app.
  Other minor tweaks.

Version 2.17
  Image32
    Significantly sped up TImage32.CopyToDc
  Image32_Layers
    Bugfix TLayeredImage32.BackgroundColor
  Image32_Ttf
	Changed TGlyphCache.GetTextGlyphs function
  Image32_Vector
	Minor bugfix in Grow function
  Other minor tweaks.
	
Version 2.16a
  Fixed broken SmoothPaths Demo.
  Otherwise, a few minor tweaks.
  
Version 2.16
  Image32
    TImage32.Antialias property is applied 
      more consistently with transforms
    TImage32.Skew parameters modified 
  Bugfixes related to rotation direction
  Significant code tidy.

Version 2.15
  Image32_Layers
	Further revisions
  Other minor updates
  
Version 2.14
  Image32_Layers
	Bugfix.
	
Version 2.13
  Image32_Layers
    Bugfix - invisible layers were 'clickable'
  Image32_SmoothPath
	Bugfix and significant code tidy
	
Version 2.12
  Image32
	Added ClockwiseRotationIsAnglePositive global variable.
	IMPORTANT: This variable defaults to true, which reverses
	the previous direction of rotation. The default direction
	now copies that of other Delphi graphics libraries.
  Image32_Extra
	Added SymmetricCropTransparent procedure
  VCL_Image32 Package
	Fixed broken link to deleted Image32_Text unit.

Version 2.11
  Image32_Layers
	Minor updates to TRasterLayer32 and TVectorLayer32
  Documentation
    A number of minor corrections.
	
Version 2.1
  Image32_Layers
	Bugfix to TLayeredImage32 - partial merging was broken
	Major updates to TRasterLayer32 and TVectorLayer32
  Image32_Vector
	Moved all Matrix functions to Image32_Transform
	Moved SmoothToBezier function to Image32_SmoothPath
	Moved RamerDouglasPeucker function to Image32_Extra
  Sample Applications
    Updated Layers101 and Layers201 
	
Version 2.02
  Minor updates to several Example applications.

Version 2.01
  Fixed a significant bug in Image32_Ttf.

Version 2.0
  This is a major update. There are many changes (mainly 
  to the Image32_Layers unit), and some of these changes 
  are very likely to break your existing code. Sorry.
  
  The Image32_Layers unit has been completely rewritten.
  The old unit was poorly written and cumbersome to use.
  The most significant change in the new layers unit is the 
  use of nested groups of layers that form a tree structure
  under TLayeredImage32.Root. This structure provides several
  advantages over the old flat layer structure. These include
  faster merges, and much simpler control over layer groups.
  Hit-testing has also been dramatically improved, being
  both much simpler to setup, and faster at detecting the 
  correct layer.
  
  The Image32_Text unit that was deprecated has been removed. 
  The Image32_Ttf unit provides all the functionality of the 
  old Image32_Text unit but, unlike its predecessor, supports 
  cross-platform development.
  
  Other units have had attention with minor bug fixes and 
  assorted embellishments, including a significant code tidy
  of the esoteric Image32_SmoothPath unit.

  The sample applications have also had significant revision.
  Some overly complicated apps have been removed, while others
  have been rewritten and simplified.

  The documentation has also been updated to address most if
  not all these changes.
  