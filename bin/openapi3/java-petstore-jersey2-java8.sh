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
yaml="modules/openapi-generator/src/test/resources/3_0/python-experimental/petstore-with-fake-endpoints-models-for-testing-with-http-signature.yaml"
ags="generate --artifact-id petstore-openapi3-jersey2-java8 -i $yaml -g java -c bin/openapi3/java-petstore-jersey2-java8.json -o samples/openapi3/client/petstore/java/jersey2-java8 --additional-properties hideGenerationTimestamp=true --additional-properties serverPort=8082 $@"

echo "Removing files and folders under samples/openapi3/client/petstore/java/jersey2-java8/src/main"
rm -rf samples/openapi3/client/petstore/java/jersey2-java8/src/main
find samples/openapi3/client/petstore/java/jersey2-java8 -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags

# copy additional manually written unit-tests
mkdir -p samples/client/petstore/java/jersey2-java8/src/test/java/org/openapitools/client
mkdir -p samples/client/petstore/java/jersey2-java8/src/test/java/org/openapitools/client/auth
mkdir -p samples/client/petstore/java/jersey2-java8/src/test/java/org/openapitools/client/model

cp CI/samples.ci/client/petstore/java/test-manual/jersey2-java8/JSONTest.java samples/openapi3/client/petstore/java/jersey2-java8/src/test/java/org/openapitools/client/JSONTest.java
cp CI/samples.ci/client/petstore/java/test-manual/jersey2-java8/JSONComposedSchemaTest.java samples/openapi3/client/petstore/java/jersey2-java8/src/test/java/org/openapitools/client/JSONComposedSchemaTest.java
