set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l java-vertx -o samples\server\petstore\java-vertx\rx -DvertxSwaggerRouterVersion=1.2.0,rxInterface=true

java %JAVA_OPTS% -jar %executable% %ags%
