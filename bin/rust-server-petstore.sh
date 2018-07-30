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

for spec_path in modules/openapi-generator/src/test/resources/2_0/rust-server/* ; do
  export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
  spec=$(basename "$spec_path" | sed 's/.yaml//')
  ags="generate -t modules/openapi-generator/src/main/resources/rust-server -i $spec_path -g rust-server -o samples/server/petstore/rust-server/output/$spec -DpackageName=$spec --additional-properties hideGenerationTimestamp=true $@"

  java $JAVA_OPTS -jar $executable $ags
done
