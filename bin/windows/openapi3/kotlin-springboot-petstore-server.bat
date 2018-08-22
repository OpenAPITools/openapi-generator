set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g kotlin-spring -o samples\server\openapi3\petstore\kotlin-springboot --additional-properties=library=spring-boot

java %JAVA_OPTS% -jar %executable% %ags%
