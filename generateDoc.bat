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

rem java -jar %generatorcli%  config-help -g "%NAME%" --full-details --named-header --format markdown --markdown-header -o "docs/generators/%NAME%.md"

rem for %%f in (docs/generators/ja*.md) do echo java -jar %generatorcli%  config-help -g "%%~nf" --full-details --named-header --format markdown --markdown-header -o "docs/generators/%%f"

java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-camel" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-camel.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-dubbo" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-dubbo.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-helidon-client" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-helidon-client.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-helidon-server" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-helidon-server.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-inflector" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-inflector.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-micronaut-client" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-micronaut-client.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-micronaut-server" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-micronaut-server.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-microprofile" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-microprofile.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-msf4j" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-msf4j.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-pkmst" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-pkmst.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-play-framework" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-play-framework.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-undertow-server" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-undertow-server.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-vertx-web" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-vertx-web.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-vertx" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-vertx.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java-wiremock" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java-wiremock.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "java" --full-details --named-header --format markdown --markdown-header -o "docs/generators/java.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-cxf-cdi" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-cxf-cdi.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-cxf-client" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-cxf-client.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-cxf-extended" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-cxf-extended.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-cxf" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-cxf.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-jersey" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-jersey.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-resteasy-eap" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-resteasy-eap.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-resteasy" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-resteasy.md"
java -jar d:\dev\projects\openapi-generator\modules\openapi-generator-cli\target\openapi-generator-cli.jar  config-help -g "jaxrs-spec" --full-details --named-header --format markdown --markdown-header -o "docs/generators/jaxrs-spec.md"


