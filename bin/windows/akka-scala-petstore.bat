set executable=.\modules\swagger-codegen-cli\target\swagger-codegen-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate  --artifact-id "scala-akka-petstore-client" -i modules\swagger-codegen\src\test\resources\2_0\petstore.yaml -l akka-scala -o samples\client\petstore\akka-scala

java %JAVA_OPTS% -jar %executable% %ags%
