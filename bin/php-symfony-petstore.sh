#!/bin/bash

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

# Make sure that the working directory is the root dir
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${SCRIPT_DIR}/../"

if [ ! -d "${APP_DIR}" ]; then
  APP_DIR=`dirname "$SCRIPT"`/..
  APP_DIR=`cd "${APP_DIR}"; pwd`
fi

# Make sure that we are regenerating the sample by removing any existing target directory
TARGET_DIR="$SCRIPT_DIR/../samples/server/petstore/php-symfony"
if [ -d "$TARGET_DIR" ]; then
	rm -rf $TARGET_DIR
fi

executable="$SCRIPT_DIR/../modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

if [ ! -f "$executable" ]
then
  mvn clean package
fi

# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
ags="generate -t $SCRIPT_DIR/../modules/swagger-codegen/src/main/resources/php-symfony -i $SCRIPT_DIR/../modules/swagger-codegen/src/test/resources/2_0/petstore.yaml -l php-symfony -o $TARGET_DIR $@"

java $JAVA_OPTS -jar $executable $ags
