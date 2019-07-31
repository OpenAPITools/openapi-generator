@ECHO OFF

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore-with-complex-headers.yaml -g typescript-axios -o samples\client\petstore\typescript-axios\builds\with-complex-headers

java %JAVA_OPTS% -jar %executable% %ags%
