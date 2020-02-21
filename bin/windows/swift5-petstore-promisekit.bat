set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\swift\petstore-with-fake-endpoints-models-for-testing.yaml -g swift5 -c bin\swift5-petstore-promisekit.json -o samples\client\petstore\swift5\promisekitLibrary

java %JAVA_OPTS% -jar %executable% %ags%
