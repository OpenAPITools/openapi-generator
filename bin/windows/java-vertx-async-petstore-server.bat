set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g java-vertx -o samples\server\petstore\java-vertx\async -DvertxSwaggerRouterVersion=1.2.0 -DhideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
