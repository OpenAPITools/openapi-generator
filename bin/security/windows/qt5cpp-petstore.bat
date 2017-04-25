set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules/swagger-codegen/src/main/resources/qt5cpp -i modules\swagger-codegen\src\test\resources\2_0\petstore-security-test.yaml -l qt5cpp -o samples\client\petstore-security-test\qt5cpp

java %JAVA_OPTS% -jar %executable% %ags%
