set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g typescript-angular -o samples\client\petstore\typescript-angular-v6-provided-in-root\builds\default --additional-properties ngVersion=6.0.0

java %JAVA_OPTS% -jar %executable% %ags%
