set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l csharp -o samples\client\petstore\csharp\SwaggerClientWithPropertyChanged --additional-properties=generatePropertyChanged=true,packageGuid={5CD900DE-8266-412F-A758-28E1F9C623D5}

java %JAVA_OPTS% -jar %executable% %ags%
