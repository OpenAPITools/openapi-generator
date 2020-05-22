set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -t modules\openapi-generator\src\main\resources\apache2\ -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g apache2 -o samples\config\petstore\apache2\

java %JAVA_OPTS% -jar %executable% %ags%
