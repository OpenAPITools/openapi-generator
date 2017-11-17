set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "jaxrs-cxf-client-petstore-client" -i modules\swagger-codegen\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -l jaxrs-spec -o samples\server\petstore\jaxrs-spec-interface -DhideGenerationTimestamp=true,serializableModel=true,interfaceOnly=true

java %JAVA_OPTS% -jar %executable% %ags%
