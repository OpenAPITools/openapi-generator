set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin\java-petstore-retrofit.json -o samples\client\petstore\java\retrofit --additional-properties hideGenerationTimestamp=true,dateLibrary=joda

java %JAVA_OPTS% -jar %executable% %ags%
