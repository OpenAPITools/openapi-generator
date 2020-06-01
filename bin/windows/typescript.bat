set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M

set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\default --additional-properties=platform=node,npmName=ts-petstore-client
java %JAVA_OPTS% -jar %executable% %ags%

ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml  -g typescript -o samples\openapi3\client\petstore\typescript\builds\jquery --additional-properties=framework=jquery,npmName=ts-petstore-client
java %JAVA_OPTS% -jar %executable% %ags%

set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\object_params --additional-properties=platform=node,npmName=ts-petstore-client,useObjectParameters
java %JAVA_OPTS% -jar %executable% %ags%
