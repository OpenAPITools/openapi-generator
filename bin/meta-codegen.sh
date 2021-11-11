#!/usr/bin/env bash

set -e

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

declare cwd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare root="$(cd "$cwd" && cd ../ && pwd)"

executable="${root}/modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]; then
  echo "Rebuilding…"
  (cd "$root" && ./mvnw -B clean package)
fi

export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="meta -n myClientCodegen -t DOCUMENTATION -p com.my.company.codegen -o samples/meta-codegen/lib $@"

java $JAVA_OPTS -jar $executable $ags

(./mvnw -B package -Djacoco.skip=true -DskipTests=true -f "$root"/samples/meta-codegen/pom.xml)

ags2="generate -g myClientCodegen -i modules/openapi-generator/src/test/resources/3_0/petstore.json -o samples/meta-codegen/usage $@"

java $JAVA_OPTS -cp ${root}/samples/meta-codegen/lib/target/myClientCodegen-openapi-generator-1.0.0.jar:$executable org.openapitools.codegen.OpenAPIGenerator $ags2
