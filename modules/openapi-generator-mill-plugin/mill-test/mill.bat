@echo off

rem This is a wrapper script, that automatically selects or downloads Mill from Maven Central or GitHub release pages.
rem
rem This script determines the Mill version to use by trying these sources
rem   - env-variable `MILL_VERSION`
rem   - local file `.mill-version`
rem   - local file `.config/mill-version`
rem   - `mill-version` from YAML fronmatter of current buildfile
rem   - if accessible, find the latest stable version available on Maven Central (https://repo1.maven.org/maven2)
rem   - env-variable `DEFAULT_MILL_VERSION`
rem
rem If a version has the suffix '-native' a native binary will be used.
rem If a version has the suffix '-jvm' an executable jar file will be used, requiring an already installed Java runtime.
rem If no such suffix is found, the script will pick a default based on version and platform.
rem
rem Once a version was determined, it tries to use either
rem    - a system-installed mill, if found and it's version matches
rem    - an already downloaded version under %USERPROFILE%\.mill\download
rem
rem If no working mill version was found on the system,
rem this script downloads a binary file from Maven Central or Github Pages (this is version dependent)
rem into a cache location (%USERPROFILE%\.mill\download).
rem
rem Mill Project URL: https://github.com/com-lihaoyi/mill
rem Script Version: 1.0.0-M1-21-7b6fae-DIRTY892b63e8
rem
rem If you want to improve this script, please also contribute your changes back!
rem This script was generated from: dist/scripts/src/mill.bat
rem
rem Licensed under the Apache License, Version 2.0

rem setlocal seems to be unavailable on Windows 95/98/ME
rem but I don't think we need to support them in 2019
setlocal enabledelayedexpansion

if [!DEFAULT_MILL_VERSION!]==[] ( set "DEFAULT_MILL_VERSION=0.12.10" )

if [!MILL_GITHUB_RELEASE_CDN!]==[] ( set "MILL_GITHUB_RELEASE_CDN=" )

if [!MILL_MAIN_CLI!]==[] ( set "MILL_MAIN_CLI=%~f0" )

set "MILL_REPO_URL=https://github.com/com-lihaoyi/mill"

SET MILL_BUILD_SCRIPT=

if exist "build.mill" (
  set MILL_BUILD_SCRIPT=build.mill
) else (
    if exist "build.mill.scala" (
      set MILL_BUILD_SCRIPT=build.mill.scala
    ) else (
        if exist "build.sc" (
          set MILL_BUILD_SCRIPT=build.sc
        ) else (
            rem no-op
        )
    )
)

if [!MILL_VERSION!]==[] (
  if exist .mill-version (
    set /p MILL_VERSION=<.mill-version
  ) else (
    if exist .config\mill-version (
      set /p MILL_VERSION=<.config\mill-version
    ) else (
      if not "%MILL_BUILD_SCRIPT%"=="" (
        for /f "tokens=1-2*" %%a in ('findstr /C:"//| mill-version:" %MILL_BUILD_SCRIPT%') do (
          set "MILL_VERSION=%%c"
        )
      ) else (
        rem no-op
      )
    )
  )
)

if [!MILL_VERSION!]==[] set MILL_VERSION=%DEFAULT_MILL_VERSION%

if [!MILL_DOWNLOAD_PATH!]==[] set MILL_DOWNLOAD_PATH=%USERPROFILE%\.mill\download

rem without bat file extension, cmd doesn't seem to be able to run it

set "MILL_NATIVE_SUFFIX=-native"
set "MILL_JVM_SUFFIX=-jvm"
set "FULL_MILL_VERSION=%MILL_VERSION%"
set "MILL_EXT=.bat"
set "ARTIFACT_SUFFIX="
REM Check if MILL_VERSION contains MILL_NATIVE_SUFFIX
echo !MILL_VERSION! | findstr /C:"%MILL_NATIVE_SUFFIX%" >nul
if !errorlevel! equ 0 (
    set "MILL_VERSION=%MILL_VERSION:-native=%"
    REM -native images compiled with graal do not support windows-arm
    REM https://github.com/oracle/graal/issues/9215
    IF /I NOT "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
        set "ARTIFACT_SUFFIX=-native-windows-amd64"
        set "MILL_EXT=.exe"
    ) else (
        rem no-op
    )
) else (
    echo !MILL_VERSION! | findstr /C:"%MILL_JVM_SUFFIX%" >nul
    if !errorlevel! equ 0 (
        set "MILL_VERSION=%MILL_VERSION:-jvm=%"
    ) else (
        set "SKIP_VERSION=false"
        set "MILL_PREFIX=%MILL_VERSION:~0,4%"
        if "!MILL_PREFIX!"=="0.1." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.2." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.3." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.4." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.5." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.6." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.7." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.8." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.9." set "SKIP_VERSION=true"
        set "MILL_PREFIX=%MILL_VERSION:~0,5%"
        if "!MILL_PREFIX!"=="0.10." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.11." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.12." set "SKIP_VERSION=true"

        if "!SKIP_VERSION!"=="false" (
            IF /I NOT "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
                set "ARTIFACT_SUFFIX=-native-windows-amd64"
                set "MILL_EXT=.exe"
            )
        ) else (
            rem no-op
        )
    )
)

set MILL=%MILL_DOWNLOAD_PATH%\!FULL_MILL_VERSION!!MILL_EXT!

set MILL_RESOLVE_DOWNLOAD=

if not exist "%MILL%" (
  set MILL_RESOLVE_DOWNLOAD=true
) else (
    if defined MILL_TEST_DRY_RUN_LAUNCHER_SCRIPT (
        set MILL_RESOLVE_DOWNLOAD=true
    ) else (
        rem no-op
    )
)


if [!MILL_RESOLVE_DOWNLOAD!]==[true] (
    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,4%
    set MILL_SHORT_VERSION_PREFIX=%MILL_VERSION:~0,2%
    rem Since 0.5.0
    set MILL_DOWNLOAD_SUFFIX=-assembly
    rem Since 0.11.0
    set MILL_DOWNLOAD_FROM_MAVEN=1
    if [!MILL_VERSION_PREFIX!]==[0.0.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.1.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.2.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.3.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.4.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.5.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.6.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.7.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.8.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.9.] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,5%
    if [!MILL_VERSION_PREFIX!]==[0.10.] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,8%
    if [!MILL_VERSION_PREFIX!]==[0.11.0-M] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,5%
    set DOWNLOAD_EXT=exe
    if [!MILL_SHORT_VERSION_PREFIX!]==[0.] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION_PREFIX!]==[0.12.] set DOWNLOAD_EXT=exe
    if [!MILL_VERSION!]==[0.12.0] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.1] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.2] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.3] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.4] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.5] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.6] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.7] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.8] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.9] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.10] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.11] set DOWNLOAD_EXT=jar

    set MILL_VERSION_PREFIX=
    set MILL_SHORT_VERSION_PREFIX=

    for /F "delims=- tokens=1" %%A in ("!MILL_VERSION!") do set MILL_VERSION_BASE=%%A
    set MILL_VERSION_MILESTONE=
    for /F "delims=- tokens=2" %%A in ("!MILL_VERSION!") do set MILL_VERSION_MILESTONE=%%A
    set MILL_VERSION_MILESTONE_START=!MILL_VERSION_MILESTONE:~0,1!
    if [!MILL_VERSION_MILESTONE_START!]==[M] (
        set MILL_VERSION_TAG=!MILL_VERSION_BASE!-!MILL_VERSION_MILESTONE!
    ) else (
        set MILL_VERSION_TAG=!MILL_VERSION_BASE!
    )
    if [!MILL_DOWNLOAD_FROM_MAVEN!]==[1] (
        set MILL_DOWNLOAD_URL=https://repo1.maven.org/maven2/com/lihaoyi/mill-dist!ARTIFACT_SUFFIX!/!MILL_VERSION!/mill-dist!ARTIFACT_SUFFIX!-!MILL_VERSION!.!DOWNLOAD_EXT!
    ) else (
        set MILL_DOWNLOAD_URL=!MILL_GITHUB_RELEASE_CDN!%MILL_REPO_URL%/releases/download/!MILL_VERSION_TAG!/!MILL_VERSION!!MILL_DOWNLOAD_SUFFIX!
    )

    if defined MILL_TEST_DRY_RUN_LAUNCHER_SCRIPT (
        echo !MILL_DOWNLOAD_URL!
        echo !MILL!
        exit /b 0
    )

    rem there seems to be no way to generate a unique temporary file path (on native Windows)
    set MILL_DOWNLOAD_FILE=%MILL%.tmp

    echo Downloading mill !MILL_VERSION! from !MILL_DOWNLOAD_URL! ... 1>&2

    if not exist "%MILL_DOWNLOAD_PATH%" mkdir "%MILL_DOWNLOAD_PATH%"
    rem curl is bundled with recent Windows 10
    rem but I don't think we can expect all the users to have it in 2019
    where /Q curl
    if !ERRORLEVEL! EQU 0 (
        curl -f -L "!MILL_DOWNLOAD_URL!" -o "!MILL_DOWNLOAD_FILE!"
    ) else (
        rem bitsadmin seems to be available on Windows 7
        rem without /dynamic, github returns 403
        rem bitsadmin is sometimes needlessly slow but it looks better with /priority foreground
        bitsadmin /transfer millDownloadJob /dynamic /priority foreground "!MILL_DOWNLOAD_URL!" "!MILL_DOWNLOAD_FILE!"
    )
    if not exist "!MILL_DOWNLOAD_FILE!" (
        echo Could not download mill !MILL_VERSION! 1>&2
        exit /b 1
    )

    move /y "!MILL_DOWNLOAD_FILE!" "%MILL%"

    set MILL_DOWNLOAD_FILE=
    set MILL_DOWNLOAD_SUFFIX=
)

set MILL_DOWNLOAD_PATH=
set MILL_VERSION=
set MILL_REPO_URL=

rem Need to preserve the first position of those listed options
set MILL_FIRST_ARG=
if [%~1%]==[--bsp] (
  set MILL_FIRST_ARG=%1%
) else (
  if [%~1%]==[-i] (
    set MILL_FIRST_ARG=%1%
  ) else (
    if [%~1%]==[--interactive] (
      set MILL_FIRST_ARG=%1%
    ) else (
      if [%~1%]==[--no-server] (
        set MILL_FIRST_ARG=%1%
      ) else (
        if [%~1%]==[--no-daemon] (
          set MILL_FIRST_ARG=%1%
        ) else (
          if [%~1%]==[--repl] (
            set MILL_FIRST_ARG=%1%
          ) else (
            if [%~1%]==[--help] (
              set MILL_FIRST_ARG=%1%
            )
          )
        )
      )
    )
  )
)
set "MILL_PARAMS=%*%"

if not [!MILL_FIRST_ARG!]==[] (
  for /f "tokens=1*" %%a in ("%*") do (
    set "MILL_PARAMS=%%b"
  )
)

rem -D mill.main.cli is for compatibility with Mill 0.10.9 - 0.13.0-M2
"%MILL%" %MILL_FIRST_ARG% -D "mill.main.cli=%MILL_MAIN_CLI%" %MILL_PARAMS%
