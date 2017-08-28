set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -c bin\typescript-petstore-npm.json -l typescript-angular -o samples\client\petstore\typescript-angular-v2\with-interfaces -D withInterfaces=true --additional-properties ngVersion=2

java %JAVA_OPTS% -jar %executable% %ags%
