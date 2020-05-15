set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -c bin\spring-mvc-petstore-server.json -g spring --library=spring-mvc -o samples\openapi3\server\petstore\spring-mvc --additional-properties hideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
