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
ags="generate --artifact-id petstore-jersey2-java6 -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g java -o samples/client/petstore/java/jersey2-java6 --additional-properties hideGenerationTimestamp=true,supportJava6=true,booleanGetterPrefix=is $@"

echo "Removing files and folders under samples/client/petstore/java/jersey2-java6/src/main"
rm -rf samples/client/petstore/java/jersey2-java6/src/main
find samples/client/petstore/java/jersey2-java6 -maxdepth 1 -type f ! -name "README.md" -exec rm {} +

echo "Restoring build.gradle ... "
cp CI/samples.ci/client/petstore/java/jersey2-java6/build.gradle samples/client/petstore/java/jersey2-java6/

java $JAVA_OPTS -jar $executable $ags
