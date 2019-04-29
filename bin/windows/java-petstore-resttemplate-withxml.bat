set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate --artifact-id petstore-resttemplate-withxml -i modules\openapi-generator\src\test\resources\2_0\petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin\java-petstore-resttemplate.json -o samples\client\petstore\java\resttemplate-withXml -DhideGenerationTimestamp=true,withXml=true

java %JAVA_OPTS% -jar %executable% %ags%
