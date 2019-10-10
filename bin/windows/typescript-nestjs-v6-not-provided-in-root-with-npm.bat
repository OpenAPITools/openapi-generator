set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-nestjs -c bin\typescript-nestjs-v7-petstore-not-provided-in-root-with-npm.json -o samples\client\petstore\typescript-nestjs-v7-not-provided-in-root\builds\with-npm --additional-properties providedInRoot=false --additional-properties nestVersion=6.0.0

java %JAVA_OPTS% -jar %executable% %ags%
