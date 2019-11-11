set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g nodejs-express-server -o samples\openapi3\server\petstore\nodejs-express-server

java %JAVA_OPTS% -jar %executable% %ags%
