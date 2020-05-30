set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\elm.yaml -g elm -t modules\openapi-generator\src\main\resources\elm -o samples\client\petstore\elm-petstore --skip-validate-spec

java %JAVA_OPTS% -jar %executable% %ags%
