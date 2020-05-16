@ECHO OFF

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M

echo
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\allOf-without-properties.yaml -g typescript-axios -o samples\client\petstore\typescript-axios\builds\allOf-without-properties

java %JAVA_OPTS% -jar %executable% %ags%
