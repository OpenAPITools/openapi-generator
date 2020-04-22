set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g csharp-netcore -o samples\client\petstore\csharp-netcore\OpenAPIClientCore --additional-properties packageGuid={321C8C3F-0156-40C1-AE42-D59761FB9B6C},useCompareNetObjects=true,targetFramework=netcoreapp2.0

java %JAVA_OPTS% -jar %executable% %ags%

REM restore csproj file
echo "restore csproject file: CI\samples.ci\client\petstore\csharp-netcore\OpenAPIClientCore\src\Org.OpenAPITools.Test\Org.OpenAPITools.Test.csproj"
copy /b/v/y CI\samples.ci\client\petstore\csharp-netcore\OpenAPIClientCore\src\Org.OpenAPITools.Test\Org.OpenAPITools.Test.csproj samples\client\petstore\csharp-netcore\OpenAPIClientCore\src\Org.OpenAPITools.Test\

