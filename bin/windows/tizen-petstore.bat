set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

set JAVA_OPTS=%JAVA_OPTS% -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -t modules\swagger-codegen\src\main\resources\tizen -i modules\swagger-codegen\src\test\resources\2_0\petstore.json -l tizen -o samples\client\petstore\tizen

java %JAVA_OPTS% -jar %executable% %ags%
