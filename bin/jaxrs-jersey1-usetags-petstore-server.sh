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
ags="generate -t modules/openapi-generator/src/main/resources/JavaJaxRS -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g jaxrs-jersey -o samples/server/petstore/jaxrs/jersey1-useTags -DhideGenerationTimestamp=true,serverPort=8082 --library=jersey1 --artifact-id=jaxrs-jersey1-useTags --additional-properties useTags=true $@"

echo "Removing files and folders under samples/server/petstore/jaxrs/jersey1-useTags/src/main"
rm -rf samples/server/petstore/jaxrs/jersey1-useTags/src/main
find samples/server/petstore/jaxrs/jersey1-useTags -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags
