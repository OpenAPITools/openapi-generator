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
  mvn -B clean package
fi

#SPEC="modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml"
# petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml is the same as the above file, with
# the addition of the HTTP signature security scheme. Ideally, this would have been directly added to
# petstore-with-fake-endpoints-models-for-testing.yaml, but this cannot be done until issue #5025 is resolved.
SPEC="modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml"
GENERATOR="go-experimental"
STUB_DIR="samples/openapi3/client/petstore/go-experimental/go-petstore"

echo "Removing files and folders under $STUB_DIR"
rm -rf $STUB_DIR

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -t modules/openapi-generator/src/main/resources/$GENERATOR -i $SPEC -g $GENERATOR -o $STUB_DIR"
ags="$ags --additional-properties enumClassPrefix=true,packageName=petstore"
ags="$ags $@"

java $JAVA_OPTS -jar $executable $ags
