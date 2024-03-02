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

[ ! -f "$executable" ] && mvn clean package -Dmaven.test.skip

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS}  -Xmx1024M -DloggerPath=conf/log4j.properties"
args="$@ generate -t modules/openapi-generator/src/main/resources/scala-cask -i modules/openapi-generator/src/test/resources/3_1/petstore.yaml -g scala-cask -o samples/scala-cask -c ./bin/config.yml"
# args="$@  help generate"

java $JAVA_OPTS -jar $executable $args
