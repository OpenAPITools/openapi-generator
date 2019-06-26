set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\openapi-generator\src\test\resources\2_0\petstore.yaml -g r -o samples\client\petstore\R --additional-properties packageName=petstore,returnExceptionOnFailure=false,exceptionPackage=default

java %JAVA_OPTS% -jar %executable% %ags%

REM once the R files are generated, run the below command to generate the man folder files
Rscript -e "roxygen2::roxygenize(roclets=c('rd'), package.dir = 'samples/client/petstore/R')"
