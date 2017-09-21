set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules/swagger-codegen/src/test/resources/2_0/petstore-security-test.yaml -l csharp -o samples/client/petstore-security-test/csharp/SwaggerClient --additional-properties packageGuid={8CE139DF-64BC-4591-85F8-8506C2B67514}

java %JAVA_OPTS% -jar %executable% %ags%
