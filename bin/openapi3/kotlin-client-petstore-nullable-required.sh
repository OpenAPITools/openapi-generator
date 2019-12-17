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
  mvn clean package
fi

export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -i modules/openapi-generator/src/test/resources/3_0/petstore-with-nullable-required.yaml -t modules/openapi-generator/src/main/resources/kotlin-client -g kotlin --artifact-id kotlin-petstore-nullable-required --additional-properties allowRequiredAsNullable=true -o samples/openapi3/client/petstore/kotlin-nullable-required $@"

echo "Cleaning previously generated files if any from samples/openapi3/client/petstore/kotlin-nullable-required"
rm -rf samples/openapi3/client/petstore/kotlin-nullable-required

echo "Generating Kotling client..."
java $JAVA_OPTS -jar $executable $ags