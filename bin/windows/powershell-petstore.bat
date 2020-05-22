set executable=.\modules\openapi-generator-cli\target\openapi-generator-cli.jar

If Not Exist %executable% (
  mvn clean package
)

REM set JAVA_OPTS=%JAVA_OPTS% -Xmx1024M -DloggerPath=conf/log4j.properties
set ags=generate -i modules\openapi-generator\src\test\resources\3_0\petstore.yaml -g powershell -o samples\client\petstorep\powershell --additional-properties powershellGalleryUrl=https://www.powershellgallery.com/packages/PSPetstore,packageGuid=a27b908d-2a20-467f-bc32-af6f3a654ac5,packageName=PSPetstore,apiNamePrefix=PS,packageVersion=0.1.2,commonVerbs=Delete=Remove:Patch=Update

java %JAVA_OPTS% -jar %executable% %ags%
