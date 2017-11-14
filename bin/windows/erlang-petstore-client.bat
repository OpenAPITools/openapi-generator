set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -t modules\swagger-codegen\src\main\resources\erlang-client -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l erlang-client -o samples\client\petstore\erlang-client

java %JAVA_OPTS% -jar %executable% %ags%
