@echo off
set path=d:\dev\tools\jdk21\bin
set generatorcli=d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar
set root=d:\dev\projects\openapi-generator

set NAME=spring
set NAME=java-camel
set NAME=java
set NAME=java-microprofile
set NAME=groovy

set NAME=jaxrs-cxf-client

java -jar %generatorcli%  config-help -g "%NAME%" --full-details --named-header --format markdown --markdown-header -o "docs/generators/%NAME%.md"

rem for %%f in (docs/generators/ja*.md) do echo java -jar %generatorcli%  config-help -g "%%~nf" --full-details --named-header --format markdown --markdown-header -o "docs/generators/%%f"


