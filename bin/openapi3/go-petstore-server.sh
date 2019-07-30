#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

while [ -h "$SCRIPT" ] ; do
  ls=`ls -ld "$SCRIPT"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    SCRIPT="$link"
  else
    SCRIPT=`dirname "$SCRIPT"`/"$link"
  fi
done

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=`dirname "$SCRIPT"`/..
  APP_DIR=`cd "${APP_DIR}"; pwd`
fi

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

SPEC="modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml"
GENERATOR="go-server"
STUB_DIR="samples/openapi3/server/petstore/go-api-server"

echo "Removing files and folders under $STUB_DIR"
rm -rf $STUB_DIR

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"

ags="generate -t modules/openapi-generator/src/main/resources/go-server -i $SPEC -g $GENERATOR -o $STUB_DIR --additional-properties packageName=petstoreserver,hideGenerationTimestamp=true $@"

java $JAVA_OPTS -jar $executable $ags
