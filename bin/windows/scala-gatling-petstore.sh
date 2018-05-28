set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\openapi-generator\src\main\resources\scala-gatling -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g scala-gatling -o samples\client\petstore\scala-gatling

java %JAVA_OPTS% -jar %executable% %ags%
