set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g python-flask -o samples\server\petstore\python-flask-python2 -c bin\supportPython2.json -D service

java %JAVA_OPTS% -jar %executable% %ags%
