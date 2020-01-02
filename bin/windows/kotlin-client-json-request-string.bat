set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "kotlin-petstore-json-request-string" -i modules\openapi-generator\src\test\resources\2_0\petstore-with-date-field.yaml -g kotlin --additional-properties requestDateConverter=toString -o samples\client\petstore\kotlin-json-request-string

java %JAVA_OPTS% -jar %executable% %ags%
