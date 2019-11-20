set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g dart -o samples\client\petstore\dart\openapi --additional-properties hideGenerationTimestamp=true,browserClient=false
java %JAVA_OPTS% -jar %executable% %ags%

set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g dart -o samples\client\petstore\dart\openapi-browser-client --additional-properties hideGenerationTimestamp=true,browserClient=true
java %JAVA_OPTS% -jar %executable% %ags%

set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g dart -o samples\client\petstore\dart\flutter_petstore\openapi --additional-properties hideGenerationTimestamp=true,browserClient=false
java %JAVA_OPTS% -jar %executable% %ags%
