set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g csharp -o samples/client/petstore/csharp/OpenApiClientNet35 --additional-properties packageGuid={321C8C3F-0156-40C1-AE42-D59761FB9B6C} -c ./bin/csharp-petstore-net-35.json

java %JAVA_OPTS% -jar %executable% %ags%
