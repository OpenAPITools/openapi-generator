set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "kotlin-petstore-okhttp3" -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g kotlin --library jvm-okhttp3 -o samples\client\petstore\kotlin-okhttp3

java %JAVA_OPTS% -jar %executable% %ags%
