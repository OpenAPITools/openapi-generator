set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M

set args=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\default --additional-properties=platform=node,npmName=ts-petstore-client
java %JAVA_OPTS% -jar %executable% %args%

args=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml  -g typescript -o samples\openapi3\client\petstore\typescript\builds\jquery --additional-properties=framework=jquery,npmName=ts-petstore-client
java %JAVA_OPTS% -jar %executable% %args%

set args=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\object_params --additional-properties=platform=node,npmName=ts-petstore-client,useObjectParameters
java %JAVA_OPTS% -jar %executable% %args%

set args=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\inversify --additional-properties=platform=node,npmName=ts-petstore-client,useInversify
java %JAVA_OPTS% -jar %executable% %args%

set args=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g typescript -o samples\openapi3\client\petstore\typescript\builds\deno --additional-properties=platform=deno
java %JAVA_OPTS% -jar %executable% %args%
