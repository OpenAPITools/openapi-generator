set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "asciidoc-petstore-documentation" -t modules\openapi-generator\src\main\resources\asciidoc-documentation --additional-properties=specDir=modules\openapi-generator\src\main\resources\asciidoc-documentation,snippetDir=. -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g asciidoc -o samples\documentation\asciidoc

java %JAVA_OPTS% -jar %executable% %ags%
