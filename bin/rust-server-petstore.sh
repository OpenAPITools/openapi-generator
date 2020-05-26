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

for spec_path in modules/openapi-generator/src/test/resources/*/rust-server/* ; do
  echo "Generating: $spec_path"
  export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
  spec=$(basename "$spec_path" | sed 's/.yaml//')
  args="generate --template-dir modules/openapi-generator/src/main/resources/rust-server
                 --input-spec $spec_path
                 --generator-name rust-server
                 --output samples/server/petstore/rust-server/output/$spec
                 --additional-properties packageName=$spec
                 --additional-properties hideGenerationTimestamp=true
                 --generate-alias-as-model
		 $@"

  java $JAVA_OPTS -jar $executable $args

  if [ $? -ne 0 ]; then
    exit $?
  fi
done
