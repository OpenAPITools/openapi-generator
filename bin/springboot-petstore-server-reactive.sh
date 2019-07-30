#!/bin/sh

SCRIPT="$0"

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

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate --artifact-id springboot-reactive -t modules/openapi-generator/src/main/resources/JavaSpring -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g spring -o samples/server/petstore/springboot-reactive --additional-properties reactive=true,delegatePattern=true,hideGenerationTimestamp=true $@"

echo "Removing files and folders under samples/server/petstore/springboot-reactive/src/main"
rm -rf samples/server/petstore/springboot-reactive/src/main
find samples/server/petstore/springboot-reactive -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags
