#!/usr/bin/env bash

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
model="modules/openapi-generator/src/test/resources/2_0/petstore.yaml"
ags="generate --template-dir modules/openapi-generator/src/main/resources/Ada -g ada $@"
ags="$ags -i $model -t modules/openapi-generator/src/main/resources/Ada -o samples/client/petstore/ada"
ags="$ags --additional-properties projectName=Petstore --model-package Samples.Petstore"

java $JAVA_OPTS -jar $executable $ags
rm -rf  samples/client/petstore/ada/src/server
