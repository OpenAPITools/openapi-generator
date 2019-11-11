@ECHO OFF

set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-fetch -c bin/typescript-fetch-petstore-typescript-three-plus.json -o samples\client\petstore\typescript-fetch\builds\typescript-three-plus

java %JAVA_OPTS% -jar %executable% %ags%
