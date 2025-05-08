// While "storage" is still technically experimental,
// it does allow file system storage of layered objects etc 
// Uncomment the following preprocessor define if you do wish to 
// use file storage (eg to compile the experimental 'CtrlDemo' in Examples).
{.$DEFINE USE_FILE_STORAGE}   

// The library's default rotation direction is CLOCKWISE as angle values increase.
// A point offset at an angle of 0 degrees from the origin will always be at 3 o'clock
// relative to the origin. A point offset at 90 degrees from the origin will be at
// 6 o'clock, and a point offset at 270 degrees will be at 12 o'clock.
// However, when this compiler directive is enabled, as angles increase in value, their
// offsets will now occur in an anti-clockwise direction so that a point offset at
// 90 degrees from the origin will appear at 12 o'clock, instead of at 6 o'clock.
{.$DEFINE CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}

// Image downsampling occurs when images are reduced in size, and the default downsampling
// function is 'BoxDownSampling'. When downsampling, this function generally produces much
// clearer images than general purpose resamplers (which are much better at upsampling,
// and doing other affine transformations). However, if for some reason you do wish to use
// a general purpose resampler while downsampling, then comment out (ie disable) this define.
{$DEFINE USE_DOWNSAMPLER_AUTOMATICALLY}

// The SimplifyPath and SimplifyPaths functions have changed. Specifically the last 
// parameter has changed from IsOpenPath to IsClosedPath, though the default has also 
// changed from false to true which should minimise any inconvenience. This change was 
// made to remove an inconsistency with other functions that all contain an IsClosedPath 
// parameter. However, if this change is going to create havoc for some reason, then 
// the following (somewhat temporary) define can be enabled.
{.$DEFINE USE_OLD_SIMPLIFYPATHS}

//USING_VCL_LCL - using either Delphi Visual Component Library or Lazarus Component Library
//is required if using the TImage32Panel component
//and adds a few extra library features (eg copying to and from TBitmap objects)
{$IF DEFINED(FPC)}
  {$DEFINE USING_LCL}
  {$DEFINE USING_VCL_LCL}
{$ELSEIF defined(ANDROID) OR defined(IOS) OR defined(OSX)}
  {$DEFINE USING_FMX}
{$ELSE}
  {$DEFINE USING_VCL}
  {$DEFINE USING_VCL_LCL}
{$IFEND}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE ABSTRACT_CLASSES}
  {$DEFINE RECORD_METHODS}
  {$DEFINE PBYTE}
  {$DEFINE NEWPOSFUNC}
  {$DEFINE SUPPORTS_POINTERMATH}
  {$DEFINE CLASS_STATIC}
  {.$DEFINE UITYPES}
  {$DEFINE NESTED_TYPES}
  {$IFNDEF DEBUG}
    {$DEFINE INLINE}
    {$DEFINE INLINE_COMPATIBLE}
  {$ENDIF}
  {$DEFINE DELPHI_PNG}
  {$IFDEF WINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ELSE}
  {$IF COMPILERVERSION < 15}
    Your version of Delphi is not supported (Image32 requires Delphi version 7 or above)
  {$IFEND}
  {$IF COMPILERVERSION < 23}
    {$DEFINE CPUX86}                            // CPUX86 was added in Delphi XE2 (added Win64 compiler)
  {$IFEND}
  {$IF COMPILERVERSION >= 17}                   //Delphi 2005
    {$IFNDEF DEBUG}
      {$DEFINE INLINE}                            //added inlining
    {$ENDIF}
    {$DEFINE NESTED_TYPES}                        //added nested types & nested constants
    {$IF COMPILERVERSION >= 18}                 //Delphi 2006
      {$DEFINE ABSTRACT_CLASSES}                  //added abstract classes
      {$DEFINE REPORTMEMORYLEAKS}                 //added ReportMemoryLeaksOnShutdown
      {$WARN SYMBOL_PLATFORM OFF}
      {$DEFINE SETSIZE}                           //added TBitmap.SetSize
      {$IF COMPILERVERSION >= 18.5}             //Delphi 2007
        {$DEFINE RECORD_METHODS}                  //added records with methods
        {$DEFINE DELPHI_PNG}                      //added PNG support
        {$DEFINE DELPHI_GIF}                      //added GIF support
        {$DEFINE MAINFORMONTASKBAR}               //added TApplication.MainFormOnTaskbar
        {$if CompilerVersion >= 20}             //Delphi 2009
          {$DEFINE PBYTE}                         //added PByte
          {$DEFINE CHARINSET}                     //added CharInSet function
          {$DEFINE EXIT_PARAM}                    //added Exit(value)
          {$DEFINE ALPHAFORMAT}                   //added TBitmap.AlphaFormat property
          {$DEFINE SUPPORTS_POINTERMATH}          //added {$POINTERMATH ON/OFF}
          {$DEFINE CLASS_STATIC}                  //added class static methods
          {$IF COMPILERVERSION >= 21}           //Delphi 2010
            {$IFNDEF DEBUG}
              {$DEFINE INLINE_COMPATIBLE}         //avoid compiler bug with INLINE in Delphi 2005-2009 ("incompatible type")
            {$ENDIF}
            {$DEFINE GESTURES}                    //added screen gesture support
            {$IF COMPILERVERSION >= 23}         //DelphiXE2
              {$DEFINE USES_NAMESPACES}
              {$DEFINE FORMATSETTINGS}
              {$DEFINE TROUNDINGMODE}   
              {$DEFINE UITYPES}                   //added UITypes unit
              {$DEFINE XPLAT_GENERICS}            //cross-platform generics support
              {$DEFINE STYLESERVICES}             //added StyleServices unit
              {$IF COMPILERVERSION >= 24}       //DelphiXE3
                {$LEGACYIFEND ON}
                {$DEFINE NEWPOSFUNC}
                {$DEFINE ZEROBASEDSTR}
                {$IF COMPILERVERSION >= 25}     //DelphiXE4
                  {$LEGACYIFEND ON}               //avoids compiler warning
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OVERFLOWCHECKS_ENABLED}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE RANGECHECKS_ENABLED}
{$ENDIF}

