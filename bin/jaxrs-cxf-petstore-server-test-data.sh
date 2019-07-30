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
ags="generate --artifact-id cxf-test-data -t modules/openapi-generator/src/main/resources/JavaJaxRS/cxf-ext -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g jaxrs-cxf-extended -o samples/server/petstore/jaxrs-cxf-test-data --additional-properties hideGenerationTimestamp=true,useAnnotatedBasePath=true --generate-alias-as-model --additional-properties java8=true,generateSpringApplication=true,generateSpringBootApplication=true,generateOperationBody=true,loadTestDataFromFile=true $@"

java $JAVA_OPTS -jar $executable $ags
