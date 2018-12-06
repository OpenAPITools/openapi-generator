set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l dart-jaguar -o samples\client\petstore\dart-jaguar\swagger -DhideGenerationTimestamp=true -DbrowserClient=false
java %JAVA_OPTS% -jar %executable% %ags%

set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l dart-jaguar -o samples\client\petstore\dart-jaguar\flutter_petstore\swagger -DhideGenerationTimestamp=true
java %JAVA_OPTS% -jar %executable% %ags%
