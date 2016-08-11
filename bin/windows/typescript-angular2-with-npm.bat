set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.json -c bin\typescript-petstore-npm.json -l typescript-angular2 -o samples\client\petstore\typescript-angular2\npm

java %JAVA_OPTS% -jar %executable% %ags%
