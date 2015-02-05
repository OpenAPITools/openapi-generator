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

cd $APP_DIR
./bin/android-java-petstore.sh
./bin/android-java-wordnik-api.sh
./bin/dynamic-html.sh
./bin/html.sh
./bin/jaxrs-petstore-server.sh
./bin/java-petstore-filemap.sh
./bin/java-petstore.sh
./bin/java-wordnik-api.sh
./bin/objc-petstore.sh
./bin/objc-wordnik-api.sh
./bin/tizen-petstore.sh
