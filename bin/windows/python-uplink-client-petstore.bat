set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "python-uplink-petstore-client" -t modules\openapi-generator\src\main\resources\python-uplink -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g python-uplink -o samples\client\petstore\python-uplink -DpackageName=petstore_api

java %JAVA_OPTS% -jar %executable% %ags%
