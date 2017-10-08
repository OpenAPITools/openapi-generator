#!/usr/bin/env bash

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

executable="./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
model="modules/swagger-codegen/src/test/resources/2_0/petstore.yaml"
ags="$@ generate --template-dir modules/swagger-codegen/src/main/resources/Ada -l ada"
ags="$ags -i $model -t modules/swagger-codegen/src/main/resources/Ada -o samples/client/petstore/ada"
ags="$ags -DprojectName=Petstore --model-package Samples.Petstore"

java $JAVA_OPTS -jar $executable $ags
rm -rf  samples/client/petstore/ada/src/server
