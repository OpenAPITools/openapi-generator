set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "nim-petstore-client" -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml --additional-properties packageName=petstore -g nim -o samples\client\petstore\nim

java %JAVA_OPTS% -jar %executable% %ags%
