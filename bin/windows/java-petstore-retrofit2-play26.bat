set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate --artifact-id petstore-java-client-retrofit2-play26 -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin\java-petstore-retrofit2-play26.json -o samples\client\petstore\java\retrofit2-play26 -DhideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
