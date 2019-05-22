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

generator=python-aiohttp
input=modules/openapi-generator/src/test/resources/2_0/petstore.yaml
out_folder=samples/server/petstore/$generator
resources=modules/openapi-generator/src/main/resources/$generator

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties -Dservice"
ags="generate -t $resources -i $input -g $generator -o $out_folder $@"

rm -rf $out_folder/.openapi*
rm -rf $out_folder/openapi_server
rm -rf $out_folder/tests*
rm $out_folder/README.md
rm $out_folder/requirements.txt
rm $out_folder/test-requirements.txt

java $JAVA_OPTS -jar $executable $ags
