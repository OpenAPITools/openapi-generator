set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate --artifact-id petstore-java-client-jersey1 -t modules\openapi-generator\src\main\resources\Java -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -o samples\client\petstore\java\jersey1  --additional-properties hideGenerationTimestamp=true --library=jersey1 --additional-properties useNullForUnknownEnumValue=true

java %JAVA_OPTS% -jar %executable% %ags%
