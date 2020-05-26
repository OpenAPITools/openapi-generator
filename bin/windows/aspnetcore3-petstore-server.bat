set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g aspnetcore -o samples\server\petstore\aspnetcore3\ --additional-properties packageGuid={3C799344-F285-4669-8FD5-7ED9B795D5C5} --additional-properties aspnetCoreVersion=3.0

java %JAVA_OPTS% -jar %executable% %ags%
