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

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"

echo "Typescript node Petstore API client (default setting)"
ags="generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g typescript-node -o samples/client/petstore/typescript-node/default $@"
java $JAVA_OPTS -jar $executable $ags

echo "Typescript node Petstore API client with npm setting"
ags="generate -i modules/openapi-generator/src/test/resources/3_0/petstore.yaml -g typescript-node -c bin/typescript-petstore-npm.json -o samples/client/petstore/typescript-node/npm $@"
java $JAVA_OPTS -jar $executable $ags
