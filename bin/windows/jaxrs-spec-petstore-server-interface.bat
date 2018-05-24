set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "jaxrs-cxf-client-petstore-client" -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g jaxrs-spec -o samples\server\petstore\jaxrs-spec-interface -DhideGenerationTimestamp=true,serializableModel=true,interfaceOnly=true

java %JAVA_OPTS% -jar %executable% %ags%
