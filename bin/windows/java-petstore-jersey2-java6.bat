set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate --artifact-id petstore-jersey2-java6 -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -o samples\client\petstore\java\jersey2-java6 --additional-properties hideGenerationTimestamp=true,supportJava6=true,booleanGetterPrefix=is

java %JAVA_OPTS% -jar %executable% %ags%
