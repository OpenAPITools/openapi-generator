#!/usr/bin/env bash

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

declare cwd="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
declare root="$(cd "$cwd" && cd ../ && pwd)"

if ! command -v gradle > /dev/null; then
		echo "[WARN] This script requires a system gradle to be installed. Not treating this as an error."
		exit 0
fi

executable="${root}/modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]
then
  (cd "$root" && ./mvnw -B clean package)
fi

\rm -rf "${root}/samples/meta-codegen-kotlin/lib"

export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="meta -n myClientCodegen -t DOCUMENTATION -p com.my.company.codegen -o samples/meta-codegen-kotlin/lib -l kotlin $@"

java $JAVA_OPTS -jar $executable $ags

if [ ! -f "${root}"samples/meta-codegen-kotlin/gradle/wrapper/gradle-wrapper.jar ]; then
  (cd "${root}"/samples/meta-codegen-kotlin/ && gradle --no-daemon wrapper --gradle-version 5.6.2 --distribution-type bin)
fi


(cp "${root}"/samples/meta-codegen-kotlin/gradlew "${root}"/samples/meta-codegen-kotlin/lib/ && \
 cp -R "${root}"/samples/meta-codegen-kotlin/gradle "${root}"/samples/meta-codegen-kotlin/lib/ && \
  cd "${root}"/samples/meta-codegen-kotlin/lib && \
  ./gradlew --no-daemon shadowJar)

ags2="generate -g myClientCodegen -i modules/openapi-generator/src/test/resources/2_0/petstore.json -o samples/meta-codegen-kotlin/usage $@"

java $JAVA_OPTS -cp ${root}/samples/meta-codegen-kotlin/lib/build/libs/my-client-codegen-openapi-generator-1.0-SNAPSHOT-all.jar:$executable org.openapitools.codegen.OpenAPIGenerator $ags2