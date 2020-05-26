set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\openapi-generator\src\main\resources\Java\libraries\rest-assured -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin\java-petstore-rest-assured-jackson.json -o samples\client\petstore\java\rest-assured-jackson --additional-properties hideGenerationTimestamp=true,booleanGetterPrefix=is,java8=true,dateLibrary=java8,serializationLibrary=jackson,useBeanValidation=true,performBeanValidation=true,

java %JAVA_OPTS% -jar %executable% %ags%
