set input=modules/openapi-generator/src/test/resources/3_0/spring/mapping.yaml
echo %input%
java -jar modules/openapi-generator-cli/target/openapi-generator-cli.jar  help generate -g java
rem stream=org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody --type-mappings string+binary=stream

