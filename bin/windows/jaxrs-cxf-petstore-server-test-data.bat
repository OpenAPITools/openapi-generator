set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate --artifact-id cxf-test-data -t modules\openapi-generator\src\main\resources\JavaJaxRS\cxf-ext -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g jaxrs-cxf-extended -o samples\server\petstore\jaxrs-cxf-test-data --additional-properties hideGenerationTimestamp=true,useAnnotatedBasePath=true --generate-alias-as-model --additional-properties java8=true,generateSpringApplication=true,generateSpringBootApplication=true,generateOperationBody=true,loadTestDataFromFile=true %*

java %JAVA_OPTS% -jar %executable% %ags%
