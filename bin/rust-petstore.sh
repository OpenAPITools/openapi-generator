#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

while [ -h "$SCRIPT" ] ; do
  ls=$(ls -ld "$SCRIPT")
  link=$(expr "$ls" : '.*-> \(.*\)$')
  if expr "$link" : '/.*' > /dev/null; then
    SCRIPT="$link"
  else
    SCRIPT=$(dirname "$SCRIPT")/"$link"
  fi
done

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=$(dirname "$SCRIPT")/..
  APP_DIR=$(cd "${APP_DIR}"; pwd)
fi

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]
then
  mvn -B clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"

for spec_path in \
  modules/openapi-generator/src/test/resources/2_0/petstore.yaml \
  modules/openapi-generator/src/test/resources/3_0/rust/rust-test.yaml \
  modules/openapi-generator/src/test/resources/2_0/fileResponseTest.json\
  ; do
  spec=$(basename "$spec_path" | sed 's/.yaml//' | sed 's/.json//' )

  for library in hyper reqwest; do
    args="generate --template-dir modules/openapi-generator/src/main/resources/rust
                   --input-spec $spec_path
                   --generator-name rust
                   --output samples/client/petstore/rust/$library/$spec
                   --additional-properties packageName=${spec}-${library}
                   --library=$library $@"
    java ${JAVA_OPTS} -jar ${executable} ${args} || exit 1
  done
done
