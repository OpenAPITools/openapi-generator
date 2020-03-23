set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "kotlin-uppercase-enum" -i modules\openapi-generator\src\test\resources\3_0\issue-4062.yaml -g kotlin --additional-properties enumPropertyNaming=UPPERCASE -o samples\client\petstore\kotlin\uppercase-enum

java %JAVA_OPTS% -jar %executable% %ags%
