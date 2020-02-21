set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-angular -c bin/typescript-angular-v8-petstore-provided-in-root-with-npm.json -o samples\client\petstore\typescript-angular-v8-provided-in-root\builds\single-request-parameter --additional-properties ngVersion=8.0.0,useSingleRequestParameter=true


java %JAVA_OPTS% -jar %executable% %ags%
