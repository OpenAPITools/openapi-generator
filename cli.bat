set input=modules/openapi-generator/src/test/resources/3_0/spring/mapping.yaml
set input=C:\dev\src\openapi-generator\modules\openapi-generator\src/test/resources/3_0/oneOfDiscriminator.yaml
set input=C:\dev\src\openapi-generator\modules\openapi-generator\src\test\resources\3_0\oneOf_issue_24769_petEnumDisc.yaml
echo %input%
set cli=modules/openapi-generator-cli/target/openapi-generator-cli.jar
set cli=C:\dev\src\repository\org\openapitools\openapi-generator-cli\7.21.0\openapi-generator-cli-7.21.0.jar
set cli=C:\dev\src\repository\org\openapitools\openapi-generator-cli\7.25.0\openapi-generator-cli-7.25.0.jar

del /S /Q \tmp\spring
rem java -jar %cli% generate -g spring --library spring-boot -i %input% -o /tmp/spring/
java -jar %cli% generate -g spring -i %input% -o /tmp/spring/

rem --type-mappings string+custom=MyCustomId --schema-mappings MyKey=MyCustomKey --import-mappings MyCustomId=org.myord.MyCustomId --import-mappings MyCustomKey=org.myorg.MyCustomKey
rem stream=org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody --type-mappings string+binary=stream

