#!/bin/bash

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
  mvn -B clean package $@
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties -DdebugSupportingFiles=true"
ags="generate -t modules/openapi-generator/src/main/resources/python -i modules/openapi-generator/src/test/resources/3_0/issue_241.yaml -g python -o /tmp/test-debug-supporting-files/ --additional-properties packageName=petstore_api $@"

if [[ $(java $JAVA_OPTS -jar $executable $ags 2>&1 | grep "StackOverflowError") ]]; then
    echo "There are StackOverflowError. Please check the result."
    exit 1
else
    echo "No StackOverflowError found."
fi

