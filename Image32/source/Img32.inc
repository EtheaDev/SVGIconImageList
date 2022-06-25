{$DEFINE NO_STORAGE}
{$DEFINE REVERSE_ORIENTATION}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE ABSTRACT_CLASSES}
  {$DEFINE RECORD_METHODS}
  {$DEFINE PBYTE}
  {$DEFINE UITYPES}
  {$DEFINE NESTED_TYPES}
  {$IFNDEF DEBUG}
    {$DEFINE INLINE}
  {$ENDIF}
  {$DEFINE DELPHI_PNG}
{$ELSE}
  {$IF COMPILERVERSION < 15}
    Your version of Delphi is not supported (Image32 requires Delphi version 7 or above)
  {$IFEND}  
  {$IFDEF CPUX86}
      {$DEFINE ASM_X86}                         //caution: do not define in FPC 
  {$ENDIF}  
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
          {$IF COMPILERVERSION >= 21}           //Delphi 2010
            {$DEFINE GESTURES}                    //added screen gesture support
            {$IF COMPILERVERSION >= 23}         //DelphiXE2
              {$IF declared(FireMonkeyVersion)}   //defined in FMX.Types
                {$DEFINE FMX}
              {$IFEND}        
              {$DEFINE FORMATSETTINGS}
              {$DEFINE TROUNDINGMODE}   
              {$DEFINE UITYPES}                   //added UITypes unit
              {$DEFINE XPLAT_GENERICS}            //reasonable cross-platform & generics support
              {$DEFINE STYLESERVICES}             //added StyleServices unit
              {$IF COMPILERVERSION >= 24}       //DelphiXE3
                {$LEGACYIFEND ON}
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
