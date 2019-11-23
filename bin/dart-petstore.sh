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

# Generate non-browserClient
ags="generate -t modules/openapi-generator/src/main/resources/dart -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart -o samples/client/petstore/dart/openapi --additional-properties hideGenerationTimestamp=true,browserClient=false,supportDart2=false $@"

# then options to generate the library for vm would be:
#ags="generate -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart -o samples/client/petstore/dart/openapi_vm --additional-properties browserClient=false,pubName=openapi_vm --additional-properties supportDart2=false $@"
java $JAVA_OPTS -jar $executable $ags

# Generate browserClient
ags="generate -t modules/openapi-generator/src/main/resources/dart -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart -o samples/client/petstore/dart/openapi-browser-client --additional-properties hideGenerationTimestamp=true,browserClient=true,supportDart2=false $@"
java $JAVA_OPTS -jar $executable $ags

# TODO: Remove this as it duplicate output from the first example in this file.
# Generate non-browserClient and put it to the flutter sample app
ags="generate -t modules/openapi-generator/src/main/resources/dart -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g dart -o samples/client/petstore/dart/flutter_petstore/openapi --additional-properties hideGenerationTimestamp=true,browserClient=false,supportDart2=false $@"
java $JAVA_OPTS -jar $executable $ags

# There is a proposal to allow importing different libraries depending on the environment:
# https://github.com/munificent/dep-interface-libraries
# When this is implemented there will only be one library.

# The current petstore test will then work for both: the browser library and the vm library.
