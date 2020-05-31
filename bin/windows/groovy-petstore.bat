set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "groovy-petstore-client" -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g groovy -o samples\client\petstore\nim

java %JAVA_OPTS% -jar %executable% %ags%
