set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -t modules\swagger-codegen\src\main\resources\JavaJaxRS\cxf -i modules\swagger-codegen\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -l jaxrs-cxf -o samples\server\petstore\jaxrs-cxf -DhideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
