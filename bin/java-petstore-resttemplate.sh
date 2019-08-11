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
ags="generate -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin/java-petstore-resttemplate.json -o samples/client/petstore/java/resttemplate --additional-properties hideGenerationTimestamp=true $@"

echo "Removing files and folders under samples/client/petstore/java/resttemplate/src/main"
rm -rf samples/client/petstore/java/resttemplate/src/main
find samples/client/petstore/java/resttemplate -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags

# copy additional manually written unit-tests
mkdir samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client
mkdir samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/auth
mkdir samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/model

cp CI/samples.ci/client/petstore/java/test-manual/resttemplate/ApiClientTest.java samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/ApiClientTest.java
cp CI/samples.ci/client/petstore/java/test-manual/resttemplate/auth/ApiKeyAuthTest.java samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/auth/ApiKeyAuthTest.java
cp CI/samples.ci/client/petstore/java/test-manual/resttemplate/auth/HttpBasicAuthTest.java samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/auth/HttpBasicAuthTest.java
cp CI/samples.ci/client/petstore/java/test-manual/resttemplate/model/EnumValueTest.java samples/client/petstore/java/resttemplate/src/test/java/org/openapitools/client/model/EnumValueTest.java
