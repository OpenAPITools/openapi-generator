set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore-with-depreacted-fields.yaml -g kotlin --artifact-id "kotlin-petstore-deprecated" -o samples\openapi3\client\petstore\kotlin-deprecated

java %JAVA_OPTS% -jar %executable% %ags%