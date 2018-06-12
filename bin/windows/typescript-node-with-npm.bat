set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -c bin\typescript-petstore-npm.json -g typescript-node -o samples\client\petstore\typescript-node\npm

java %JAVA_OPTS% -jar %executable% %ags%
