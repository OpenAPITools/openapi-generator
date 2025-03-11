set generatorcli=C:\dev\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar
set root=C:\dev\openapi-generator
rem "--include-base-dir" "%root%"
set files=java-*
rem set files=spring*
set files=spring-boot-gen*
rem files=java-resttemplate-generateAliasAsModel*
rem set files=spring*
java -ea -server -Duser.timezone=UTC -jar %generatorcli% batch --fail-fast   -- bin/configs/%files%