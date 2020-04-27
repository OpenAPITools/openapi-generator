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
target_dir="./samples/client/petstore/java/rest-assured-jackson/"

if [ ! -f "$executable" ]
then
  mvn -B clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
args="generate -t modules/openapi-generator/src/main/resources/Java/libraries/rest-assured -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin/java-petstore-rest-assured-jackson.json -o ${target_dir} --additional-properties hideGenerationTimestamp=true --additional-properties useBeanValidation=true --additional-properties performBeanValidation=true --additional-properties booleanGetterPrefix=is --additional-properties java8=true --additional-properties dateLibrary=java8 --additional-properties serializationLibrary=jackson $@"

echo "Removing ${target_dir}"
rm -rf "${target_dir}"

java $JAVA_OPTS -jar $executable $args
