set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l spring --library=spring-mvc -o samples\server\petstore\spring-mvc-j8-async -c bin\spring-mvc-petstore-j8-async.json

java %JAVA_OPTS% -jar %executable% %ags%
