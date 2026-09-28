@echo off
:: Builds and runs the SVGIconImageList test suites (VCL + engines, then FMX),
:: then builds and runs the PreferNativeSvgSupport build check (EngineConfigCheck).
::   run_tests.cmd [platform] [config] [bdspath]
::     [platform] : Win32 | Win64        (default: Win64)
::     [config]   : Debug | Release      (default: Debug - runs with range and
::                                        overflow checking on, which is the
::                                        point of running the tests at all)
::     [bdspath]  : Delphi BDS path      (default: the BDS environment variable,
::                                        else the newest Delphi found in the
::                                        registry: 13, 12, 11, 10.4)
:: Note: the variable is called SVGPLATFORM, not PLATFORM: rsvars.bat clears the
::       latter, and MSBuild then refuses to build with an empty PLATFORM.
:: Exit code: 0 = all tests passed, 1 = build failed or some test failed.
setlocal enabledelayedexpansion

:: --- Delphi BDS path (default) ---
:: Every Delphi writes its install folder in the registry at
:: HK(CU|LM)\Software\Embarcadero\BDS\<version>\RootDir:
::   37.0 = Delphi 13, 23.0 = Delphi 12, 22.0 = Delphi 11, 21.0 = Delphi 10.4.
:: Newest first; the historical C:\BDS\Studio\37.0 is the last resort.
set DEFAULT_BDS=
if "%DEFAULT_BDS%"=="" call :FindBDS 37.0
if "%DEFAULT_BDS%"=="" call :FindBDS 23.0
if "%DEFAULT_BDS%"=="" call :FindBDS 22.0
if "%DEFAULT_BDS%"=="" call :FindBDS 21.0
if "%DEFAULT_BDS%"=="" set DEFAULT_BDS=C:\BDS\Studio\37.0
set SVGPLATFORM=%~1
if "%SVGPLATFORM%"=="" set SVGPLATFORM=Win64
set CFG=%~2
if "%CFG%"=="" set CFG=Debug
set BDS_PATH=%~3
if "%BDS_PATH%"=="" if not "%BDS%"=="" set BDS_PATH=%BDS%
if "%BDS_PATH%"=="" set BDS_PATH=%DEFAULT_BDS%

if not exist "%BDS_PATH%\bin\rsvars.bat" (
  echo [ERROR] rsvars.bat not found in "%BDS_PATH%\bin".
  echo         Pass the path as third argument, or set the BDS variable.
  exit /b 2
)
call "%BDS_PATH%\bin\rsvars.bat" >nul

set PROJ=%~dp0Projects\D13\SVGIconImageListTests.dproj
set FMXPROJ=%~dp0Projects\D13\SVGIconImageListFMXTests.dproj
set CHECKPROJ=%~dp0Projects\D13\EngineConfigCheck.dproj
set BINDIR=%~dp0Bin\%SVGPLATFORM%\%CFG%
set EXE=%BINDIR%\SVGIconImageListTests.exe
set FMXEXE=%BINDIR%\SVGIconImageListFMXTests.exe
set CHECKEXE=%BINDIR%\EngineConfigCheck.exe
:: DUnitX writes its NUnit-shaped report next to the binary, under its
:: default name; the run below sets the working directory accordingly.
set XMLOUT=%BINDIR%\dunitx-results.xml
set FMXXMLOUT=%BINDIR%\dunitx-fmx-results.xml

echo ============================================
echo  SVGIconImageList test suite - %SVGPLATFORM% / %CFG%
echo ============================================
msbuild "%PROJ%" /t:Build /p:Config=%CFG% /p:Platform=%SVGPLATFORM% /nologo /v:minimal
if errorlevel 1 (
  echo [ERROR] Build failed.
  exit /b 1
)

if not exist "%EXE%" (
  echo [ERROR] "%EXE%" not found after a successful build.
  exit /b 1
)

:: The Skia engine needs sk4d.dll next to the executable.
if /i "%SVGPLATFORM%"=="Win64" (
  if exist "%BDS_PATH%\bin64\sk4d.dll" copy /y "%BDS_PATH%\bin64\sk4d.dll" "%BINDIR%\" >nul
) else (
  if exist "%BDS_PATH%\bin\sk4d.dll" copy /y "%BDS_PATH%\bin\sk4d.dll" "%BINDIR%\" >nul
)

pushd "%BINDIR%"
"%EXE%" --exitbehavior:Continue
set TESTRESULT=%ERRORLEVEL%
popd

echo.
echo ============================================
echo  SVGIconImageList FMX test suite - %SVGPLATFORM% / %CFG%
echo ============================================
msbuild "%FMXPROJ%" /t:Build /p:Config=%CFG% /p:Platform=%SVGPLATFORM% /nologo /v:minimal
if errorlevel 1 (
  echo [ERROR] FMX build failed.
  set TESTRESULT=1
) else (
  pushd "%BINDIR%"
  "%FMXEXE%" --exitbehavior:Continue "--xmlfile:%FMXXMLOUT%"
  if errorlevel 1 set TESTRESULT=1
  popd
)

echo.
echo ============================================
echo  PreferNativeSvgSupport build check
echo ============================================
msbuild "%CHECKPROJ%" /t:Build /p:Config=%CFG% /p:Platform=%SVGPLATFORM% /nologo /v:minimal
if errorlevel 1 (
  echo [FAIL] SVGIconImageList does not build with PreferNativeSvgSupport.
  set TESTRESULT=1
) else (
  "%CHECKEXE%"
  if errorlevel 1 set TESTRESULT=1
)

echo.
if "%TESTRESULT%"=="0" (
  echo All tests passed. Reports: "%XMLOUT%", "%FMXXMLOUT%"
) else (
  echo Some tests failed. Reports: "%XMLOUT%", "%FMXXMLOUT%"
)
exit /b %TESTRESULT%

:: ============================================================
:FindBDS <version>
::   Sets DEFAULT_BDS to the RootDir of that BDS version (HKCU first, then
::   HKLM), without the trailing backslash, and only if it holds rsvars.bat.
:: ============================================================
call :ReadRootDir HKCU %~1
if "%DEFAULT_BDS%"=="" call :ReadRootDir HKLM %~1
:: Delayed expansion here: a %VAR:~-1% substring on an EMPTY variable breaks the
:: parse of the whole line (the version is not installed), !VAR:~-1! does not.
if "!DEFAULT_BDS:~-1!"=="\" set "DEFAULT_BDS=!DEFAULT_BDS:~0,-1!"
if not "%DEFAULT_BDS%"=="" if not exist "%DEFAULT_BDS%\bin\rsvars.bat" set DEFAULT_BDS=
exit /b 0

:: ============================================================
:ReadRootDir <hive> <version>
::   reg query prints "    RootDir    REG_SZ    C:\...\Studio\<version>\";
::   the line is picked by its first token (no "find": a Unix find.exe on
::   the PATH, e.g. under Git Bash, would break it) and tokens=1,2,* keeps
::   the whole path even when it contains spaces.
:: ============================================================
for /f "tokens=1,2,*" %%A in ('reg query "%~1\Software\Embarcadero\BDS\%~2" /v RootDir 2^>nul') do (
  if /i "%%A"=="RootDir" set "DEFAULT_BDS=%%C"
)
exit /b 0
