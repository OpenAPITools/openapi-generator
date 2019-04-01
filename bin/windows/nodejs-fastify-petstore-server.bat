set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\openapi-generator\src\main\resources\nodejs-fastify-server -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g nodejs-fastify-server -o samples\server\petstore\nodejs-fastify-server

java %JAVA_OPTS% -jar %executable% %ags%
