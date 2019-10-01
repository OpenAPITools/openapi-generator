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
ags="generate --artifact-id petstore-java-client-jersey1 -t modules/openapi-generator/src/main/resources/Java -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g java -o samples/client/petstore/java/jersey1 --additional-properties hideGenerationTimestamp=true --library=jersey1 $@"

echo "Removing files and folders under samples/client/petstore/java/jersey1/src/main"
rm -rf samples/client/petstore/java/jersey1/src/main
find samples/client/petstore/java/jersey1 -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags

# copy additional manually written unit-tests
mkdir samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client
mkdir samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/auth
mkdir samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/model

cp CI/samples.ci/client/petstore/java/test-manual/common/StringUtilTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/StringUtilTest.java
cp CI/samples.ci/client/petstore/java/test-manual/jersey1/ApiClientTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/ApiClientTest.java
cp CI/samples.ci/client/petstore/java/test-manual/common/ConfigurationTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/ConfigurationTest.java
cp CI/samples.ci/client/petstore/java/test-manual/jersey1/auth/ApiKeyAuthTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/auth/ApiKeyAuthTest.java
cp CI/samples.ci/client/petstore/java/test-manual/jersey1/auth/HttpBasicAuthTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/auth/HttpBasicAuthTest.java
cp CI/samples.ci/client/petstore/java/test-manual/jersey1/model/EnumValueTest.java samples/client/petstore/java/jersey1/src/test/java/org/openapitools/client/model/EnumValueTest.java
