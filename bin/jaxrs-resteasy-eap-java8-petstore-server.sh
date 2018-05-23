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
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate --artifact-id jaxrs-resteasy-eap-java8-server -t modules/openapi-generator/src/main/resources/JavaJaxRS/resteasy/eap -i modules/openapi-generator/src/test/resources/2_0/petstore.yaml -g jaxrs-resteasy-eap -o samples/server/petstore/jaxrs-resteasy/eap-java8 -DhideGenerationTimestamp=true -c ./bin/jaxrs-resteasy-eap-java8-petstore-server.json $@"

echo "Removing files and folders under samples/server/petstore/jaxrs-resteasy/eap-java8/src/main"
rm -rf samples/server/petstore/jaxrs-resteasy/eap-java8/src/main
find samples/server/petstore/jaxrs-resteasy/eap-java8 -maxdepth 1 -type f ! -name "README.md" -exec rm {} +

java $JAVA_OPTS -jar $executable $ags
