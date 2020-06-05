set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "markdown-petstore-documentation" -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g markdown -o samples\documentation\petstore\markdown

java %JAVA_OPTS% -jar %executable% %ags%
