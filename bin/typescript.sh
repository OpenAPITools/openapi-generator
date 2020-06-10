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
common_args="generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g typescript"
samples="samples/openapi3/client/petstore/typescript/builds"

if [ ! -f "$executable" ]
then
  mvn -B clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"

printf "\033[32m## Creating default (fetch) client!\033[0m\n"
args="-o $samples/default --additional-properties=platform=node,npmName=ts-petstore-client $@"
java $JAVA_OPTS -jar $executable $common_args $args

printf "\033[32m## Creating jquery client!\033[0m\n"
args="-o $samples/jquery --additional-properties=framework=jquery,npmName=ts-petstore-client $@"
java $JAVA_OPTS -jar $executable $common_args $args

printf "\033[32m## Creating fetch object client!\033[0m\n"
args="-o $samples/object_params --additional-properties=platform=node,npmName=ts-petstore-client,useObjectParameters=true $@"
java $JAVA_OPTS -jar $executable $common_args $args

printf "\033[32m## Creating fetch client with InversifyJS support!\033[0m\n"
args="-o $samples/inversify --additional-properties=platform=node,npmName=ts-petstore-client,useInversify=true $@"
java $JAVA_OPTS -jar $executable $common_args $args
