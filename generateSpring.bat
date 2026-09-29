set path=c:\dev\src\tools\jdk21\bin
set generatorcli=C:\dev\src\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar
rem set generatorcli=C:\dev\src\repository\org\openapitools\openapi-generator-cli\7.21.0\openapi-generator-cli-7.21.0.jar

rem java -ea -server -Duser.timezone=UTC -jar %generatorcli% generate -g spring --library spring-boot -i modules/openapi-generator/src/test/resources/3_0/spring/issue_23635.yaml -o /tmp/output --inline-schema-options RESOLVE_INLINE_ENUMS=true --openapi-normalizer REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY=true

java -ea -server -Duser.timezone=UTC -jar %generatorcli% generate -g spring --library spring-boot -i modules/openapi-generator/src/test/resources/3_0/spring/issue_23635.yaml -o /tmp/output --inline-schema-options RESOLVE_INLINE_ENUMS=true --openapi-normalizer REMOVE_ANYOF_ONEOF_AND_KEEP_PROPERTIES_ONLY=true


