set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l csharp -o samples\client\petstore\csharp\SwaggerClient --additional-properties packageGuid={321C8C3F-0156-40C1-AE42-D59761FB9B6C}

java %JAVA_OPTS% -jar %executable% %ags%
