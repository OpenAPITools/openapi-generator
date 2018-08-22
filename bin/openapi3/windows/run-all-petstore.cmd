REM this batch file will loop through all the .bat files under bin\openapi3\windows\
REM execute the script and check the error level to see if there's any error

echo IMPORTANT: this script should be run by the CI (e.g. appveyor) only. There's no need to run this script to update Petstore samples for all generators.
echo Please press CTRL+C to stop or the script will continue in 10 seconds.

timeout 10

for /f "delims=" %%i in ('dir /b ".\bin\openapi3\windows\*.bat"') do (

  CALL .\bin\openapi3\windows\%%i

  IF ERRORLEVEL 1 IF NOT ERRORLEVEL 2 exit /b 1

)
