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
ags="generate -g aspnetcore -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -t modules/openapi-generator/src/main/resources/aspnetcore/3.0/ -o samples/server/petstore/aspnetcore-3.0 --additional-properties packageGuid={3C799344-F285-4669-8FD5-7ED9B795D5C5} --additional-properties aspnetCoreVersion=3.0 $@"

java $JAVA_OPTS -jar $executable $ags
