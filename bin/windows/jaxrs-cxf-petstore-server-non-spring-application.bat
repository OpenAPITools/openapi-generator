set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate --artifact-id cxf-server-non-spring -t modules\openapi-generator\src\main\resources\JavaJaxRS\cxf -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g jaxrs-cxf -o samples\server\petstore\jaxrs-cxf-non-spring-app --additional-properties hideGenerationTimestamp=true,generateNonSpringApplication=true,serverPort=8082 %*

java %JAVA_OPTS% -jar %executable% %ags%
