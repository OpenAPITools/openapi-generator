set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l csharp -o samples\client\petstore\csharp\SwaggerClientNetStandard --additional-properties targetFramework=v5.0,packageGuid={3AB1F259-1769-484B-9411-84505FCCBD55}

java %JAVA_OPTS% -jar %executable% %ags%
