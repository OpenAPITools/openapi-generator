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

SPEC="modules/openapi-generator/src/test/resources/3_0/petstore.yaml"
GENERATOR="java-vertx-web"
TEMPLATE="modules/openapi-generator/src/main/resources/JavaVertXWebServer"
STUB_DIR="samples/server/petstore/java-vertx-web/rx"

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -i $SPEC -t $TEMPLATE -g $GENERATOR --artifact-id java-vertx-web-rx-server -o $STUB_DIR --additional-properties hideGenerationTimestamp=true $@"

java $JAVA_OPTS -jar $executable $ags
