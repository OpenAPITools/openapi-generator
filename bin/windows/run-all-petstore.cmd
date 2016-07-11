for /f "delims=" %%i in ('dir /b ".\bin\windows\*.bat"') do (

  CALL .\bin\windows\%%i

  IF ERRORLEVEL 1 IF NOT ERRORLEVEL 2 exit /b 1

)
