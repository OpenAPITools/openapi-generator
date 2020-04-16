set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "scala-akka-http-petstore-server" -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g scala-akka-http-server -o samples\server\petstore\scala-akka-http-server

java %JAVA_OPTS% -jar %executable% %ags%
