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
ags="generate -t modules/openapi-generator/src/main/resources/Java/libraries/feign -i modules/openapi-generator/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -g java -c bin/java-petstore-feign-9x.json -o samples/client/petstore/java/feign --additional-properties hideGenerationTimestamp=true,booleanGetterPrefix=is,additionalModelTypeAnnotations=@javax.annotation.concurrent.Immutable $@"

echo "Removing files and folders under samples/client/petstore/java/feign/src/main"
rm -rf samples/client/petstore/java/feign/src/main
find samples/client/petstore/java/feign -maxdepth 1 -type f ! -name "README.md" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags

# copy additional manually written unit-tests
mkdir samples/client/petstore/java/feign/src/test/java/org/openapitools/client

cp CI/samples.ci/client/petstore/java/test-manual/common/StringUtilTest.java samples/client/petstore/java/feign/src/test/java/org/openapitools/client/StringUtilTest.java
