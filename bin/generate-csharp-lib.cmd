@echo off

if "%1"=="" goto DISPLAY_USAGE
if "%2"=="" goto DISPLAY_USAGE
if "%3"=="" goto DISPLAY_USAGE
if "%4"=="" goto DISPLAY_USAGE

java -cp "target\*;target\lib\*" com.wordnik.swagger.codegen.config.csharp.CSharpLibCodeGen %*
goto END

:DISPLAY_USAGE
  echo Usage: %0 apiUrl apiKey namespace outputDir

:END
