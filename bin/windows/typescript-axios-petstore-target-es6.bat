@ECHO OFF

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-axios -c bin\typescript-axios-petstore-target-es6.json -o samples\client\petstore\typescript-axios\builds\es6-target

java %JAVA_OPTS% -jar %executable% %ags%
