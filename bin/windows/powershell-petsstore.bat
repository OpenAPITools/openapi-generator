set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g powershell -o samples\client\petstore\powershell --additional-properties packageGuid=a27b908d-2a20-467f-bc32-af6f3a654ac5,csharpClientPath=$ScriptDir\..\..\petstore\csharp\SwaggerClient

java %JAVA_OPTS% -jar %executable% %ags%
