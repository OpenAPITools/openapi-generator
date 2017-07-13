set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
echo "Typescript Petstore API client (default)"
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l typescript-angular2 -o samples\client\petstore\typescript-angular2\default
java %JAVA_OPTS% -jar %executable% %ags%

echo "Typescript Petstore API client (with interfaces generated)"
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l typescript-angular2 -o samples\client\petstore\typescript-angular2\with-interfaces -D withInterfaces=true
java %JAVA_OPTS% -jar %executable% %ags%

echo "Typescript Petstore API client (npm setting)"
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l typescript-angular2 -c bin\typescript-petstore-npm.json -o samples\client\petstore\typescript-angular2\npm
java %JAVA_OPTS% -jar %executable% %ags%
