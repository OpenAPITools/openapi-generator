set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\openapi-generator\src\main\resources\elixir -i modules\openapi-generator\src\test\resources\3_0\petstore-with-fake-endpoints-models-for-testing.yaml -g elixir -o samples\client\petstore\elixir --additional-properties invokerPackage=OpenapiPetstore

java %JAVA_OPTS% -jar %executable% %ags%
