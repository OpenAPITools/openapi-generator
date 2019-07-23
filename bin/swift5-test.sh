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
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -t modules/openapi-generator/src/main/resources/swift5 -i modules/openapi-generator/src/test/resources/2_0/swift4Test.json -g swift5 -c ./bin/swift5-test.json -o samples/client/test/swift5default $@"

java $JAVA_OPTS -jar $executable $ags

cd samples/client/test/swift5default

if type "xcodegen" > /dev/null 2>&1; then
  xcodegen generate

  if type "carthage" > /dev/null 2>&1; then
    carthage update --no-use-binaries --platform ios --cache-builds
  fi
fi

if type "swiftlint" > /dev/null 2>&1; then
  swiftlint autocorrect --quiet
fi