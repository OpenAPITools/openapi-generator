set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\openapi-generator\src\main\resources\Java\libraries\retrofit2 -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin\java-petstore-retrofit2.json -o samples\client\petstore\java\retrofit2 --additional-properties hideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
