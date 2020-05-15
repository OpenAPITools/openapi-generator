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

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate --artifact-id springboot-delegate -t modules/openapi-generator/src/main/resources/JavaSpring -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g spring -o samples/openapi3/server/petstore/springboot-delegate --additional-properties delegatePattern=true,hideGenerationTimestamp=true,java8=false $@"

echo "Removing files and folders under samples/openapi3/server/petstore/springboot-delegate/src/main"
rm -rf samples/openapi3/server/petstore/springboot-delegate/src/main
find samples/openapi3/server/petstore/springboot-delegate/ -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags
