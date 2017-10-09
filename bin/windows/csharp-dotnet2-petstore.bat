set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l csharp-dotnet2 -o samples/client/petstore/csharp-dotnet2/SwaggerClientTest/Lib/SwaggerClient --additional-properties hideGenerationTimestamp=true

java %JAVA_OPTS% -jar %executable% %ags%
