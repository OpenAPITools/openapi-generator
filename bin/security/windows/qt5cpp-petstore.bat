set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules/openapi-generator/src/main/resources/qt5cpp -i modules\openapi-generator\src\test\resources\2_0\petstore-security-test.yaml -g qt5cpp -o samples\client\petstore-security-test\qt5cpp

java %JAVA_OPTS% -jar %executable% %ags%
