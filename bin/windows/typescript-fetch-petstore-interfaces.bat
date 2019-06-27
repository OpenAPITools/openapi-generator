@ECHO OFF

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-fetch -o samples\client\petstore\typescript-fetch\builds\with-interfaces --additional-properties withInterfaces=true

java %JAVA_OPTS% -jar %executable% %ags%
