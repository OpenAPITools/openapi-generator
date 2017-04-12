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

executable="./modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="$@ generate -t modules/swagger-codegen/src/main/resources/MSF4J -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l msf4j -o samples/server/petstore/java-msf4j/ -DhideGenerationTimestamp=true --additional-properties artifactId=swagger-msf4j-server"

echo "Removing files and folders under samples/server/petstore/java-msf4j/src/main"
rm -rf samples/server/petstore/java-msf4j/src/main
find samples/server/petstore/java-msf4j -maxdepth 1 -type f ! -name "README.md" ! -name "pom.xml" ! -name "mvn_test_jdk8_only.sh" ! -name ".swagger-codegen-ignore" -exec rm {} +
java $JAVA_OPTS -jar $executable $ags
