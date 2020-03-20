set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate --artifact-id "scala-sttp-petstore" -t modules\openapi-generator\src\main\resources\scala-sttp -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g scala-sttp -o samples\openapi3\client\petstore\scala-sttp

java %JAVA_OPTS% -jar %executable% %ags%
