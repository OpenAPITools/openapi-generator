set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -c bin\spring-mvc-petstore-server.json -g spring --library=spring-mvc -o samples\server\petstore\spring-mvc -DhideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
