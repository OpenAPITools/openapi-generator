set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l rust -o samples\client\petstore\rust

java %JAVA_OPTS% -jar %executable% %ags%
