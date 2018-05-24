set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g csharp -o samples\client\petstore\csharp\SwaggerClientNetStandard --additional-properties targetFramework=v5.0,packageGuid={3AB1F259-1769-484B-9411-84505FCCBD55}

java %JAVA_OPTS% -jar %executable% %ags%
