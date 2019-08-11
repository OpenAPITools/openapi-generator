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
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"

# Generate client
ags="$@ generate -t modules/openapi-generator/src/main/resources/dart-jaguar -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart-jaguar -o samples/client/petstore/dart-jaguar/openapi --additional-properties hideGenerationTimestamp=true,pubName=openapi"
java $JAVA_OPTS -jar $executable $ags

# Generate non-browserClient and put it to the flutter sample app
ags="$@ generate -t modules/openapi-generator/src/main/resources/dart-jaguar -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart-jaguar -o samples/client/petstore/dart-jaguar/flutter_petstore/openapi --additional-properties hideGenerationTimestamp=true,pubName=openapi"
java $JAVA_OPTS -jar $executable $ags

# Generate proto and put it to the flutter sample app
ags="$@ generate -t modules/openapi-generator/src/main/resources/dart-jaguar -i modules/openapi-generator/src/test/resources/2_0/petstore-proto.yaml -g dart-jaguar -o samples/client/petstore/dart-jaguar/flutter_proto_petstore/openapi --additional-properties serialization=proto,hideGenerationTimestamp=true,pubName=openapi"
java $JAVA_OPTS -jar $executable $ags

# Generate proto and put it to the sample
ags="$@ generate -t modules/openapi-generator/src/main/resources/dart-jaguar -i modules/openapi-generator/src/test/resources/2_0/petstore-proto.yaml -g dart-jaguar -o samples/client/petstore/dart-jaguar/openapi_proto --additional-properties serialization=proto,hideGenerationTimestamp=true,pubName=openapi"
java $JAVA_OPTS -jar $executable $ags

# There is a proposal to allow importing different libraries depending on the environment:
# https://github.com/munificent/dep-interface-libraries
# When this is implemented there will only be one library.

# The current petstore test will then work for both: the browser library and the vm library.
