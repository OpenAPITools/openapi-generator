setlocal

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

set SPEC=modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml
set GENERATOR=go
set STUB_DIR=samples\client\petstore\go\go-petstore

echo Removing files and folders under %STUB_DIR%
del /F /S /Q %STUB_DIR%

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -t modules\openapi-generator\src\main\resources\go -i %SPEC% -g %GENERATOR% -o %STUB_DIR% --additional-properties packageName=petstore

java %JAVA_OPTS% -jar %executable% %ags%

endlocal