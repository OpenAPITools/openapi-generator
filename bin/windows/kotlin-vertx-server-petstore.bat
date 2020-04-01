set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "kotlin-vertx-petstore-server" -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g kotlin-vertx -o samples\server\petstore\kotlin\vertx

java %JAVA_OPTS% -jar %executable% %ags%
