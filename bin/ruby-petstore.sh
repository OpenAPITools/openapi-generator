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

root=./modules/swagger-codegen-distribution/pom.xml

# gets version of swagger-codegen
version=$(sed '/<project>/,/<\/project>/d;/<version>/!d;s/ *<\/\?version> *//g' $root | sed -n '2p' | sed -e 's,.*<version>\([^<]*\)</version>.*,\1,g')

executable="./modules/swagger-codegen-distribution/target/swagger-codegen-distribution-$version.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="$@ -i modules/swagger-codegen/src/test/resources/2_0/petstore.json -l ruby -o samples/client/petstore/ruby"

java $JAVA_OPTS -jar $executable $ags
