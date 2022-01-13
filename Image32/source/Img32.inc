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
      {$DEFINE ASM_X86}                  //nb: do not define in FPC 
  {$ENDIF}  
  {$IF COMPILERVERSION >= 17}
    {$IFNDEF DEBUG}
      {$DEFINE INLINE}                   //Delphi 2005 - added inlining
    {$ENDIF}
    {$DEFINE NESTED_TYPES}               //Delphi 2005 - added nested types & nested constants
    {$IF COMPILERVERSION >= 18}
      {$DEFINE ABSTRACT_CLASSES}         //Delphi 2006 - added abstract classes
      {$DEFINE REPORTMEMORYLEAKS}        //Delphi 2006 - added ReportMemoryLeaksOnShutdown
      {$WARN SYMBOL_PLATFORM OFF}
      {$DEFINE SETSIZE}                  //Delphi 2006 - added TBitmap.SetSize
      {$IF COMPILERVERSION >= 18.5}
        {$DEFINE RECORD_METHODS}         //Delphi 2007 - added records with methods
        {$DEFINE DELPHI_PNG}             //Delphi 2007 - added PNG support
        {$DEFINE DELPHI_GIF}             //Delphi 2007 - added GIF support
        {$DEFINE MAINFORMONTASKBAR}      //Delphi 2007 - added TApplication.MainFormOnTaskbar
        {$if CompilerVersion >= 20}
          {$DEFINE PBYTE}                //Delphi 2009 - added PByte
          {$DEFINE CHARINSET}            //Delphi 2009 - added CharInSet function
          {$DEFINE EXIT_PARAM}           //Delphi 2009 - added Exit(value)
          {$DEFINE ALPHAFORMAT}          //Delphi 2009 - added TBitmap.AlphaFormat property
          {$IF COMPILERVERSION >= 21}
            {$DEFINE GESTURES}           //Delphi 2010 - added screen gesture support
            {$IF COMPILERVERSION >= 23}
			  {$IF declared(FireMonkeyVersion)} //defined in FMX.Types
			    {$DEFINE FMX}
              {$IFEND}			  
              {$DEFINE TROUNDINGMODE}   
              {$DEFINE UITYPES}          //DelphiXE2   - added UITypes unit
              {$DEFINE XPLAT_GENERICS}   //DelphiXE2   - reasonable cross-platform & generics support
              {$DEFINE STYLESERVICES}    //DelphiXE2   - added StyleServices unit
              {$IF COMPILERVERSION >= 24}
                {$DEFINE ZEROBASEDSTR}   //DelphiXE3
                {$IF COMPILERVERSION >= 25}
                  {$LEGACYIFEND ON}      //DelphiXE4   - avoids compiler warning
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ENDIF}
