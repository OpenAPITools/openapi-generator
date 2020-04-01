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
  mvn clean package
fi

# purge lib/doc folder
echo "purge ruby petstore lib, docs folder"
rm -Rf samples/openapi3/client/petstore/ruby-faraday/lib
rm -Rf samples/openapi3/client/petstore/ruby-faraday/docs

# purge test files other than integration test
# NOTE: spec/custom/*.rb and spec/petstore_helper.rb are not generated files
echo "purge ruby petstore spec"
find samples/openapi3/client/petstore/ruby-faraday/spec -type d -not -name spec -not -name custom | xargs rm -Rf
find samples/openapi3/client/petstore/ruby-faraday/spec -type f -not -name petstore_helper.rb -not -iwholename '*/spec/custom/*' | xargs rm -Rf

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -t modules/openapi-generator/src/main/resources/ruby-client -i modules/openapi-generator/src/test/resources/3_0/petstore-with-fake-endpoints-models-for-testing.yaml -g ruby -c bin/openapi3/ruby-petstore-faraday.json -o samples/openapi3/client/petstore/ruby-faraday --additional-properties skipFormModel=true $@"

java $JAVA_OPTS -jar $executable $ags
