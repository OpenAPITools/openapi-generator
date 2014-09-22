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
./bin/java-wordnik-api.sh
./bin/php-wordnik-api.sh
./bin/python3-wordnik-api.sh
./bin/objc-wordnik-api.sh
./bin/python-wordnik-api.sh
./bin/scala-wordnik-api.sh

./bin/android-java-petstore.sh
./bin/csharp-petstore.sh
./bin/flash-petstore.sh
./bin/java-petstore.sh
./bin/objc-petstore.sh
./bin/php-petstore.sh
./bin/python-petstore.sh
./bin/python3-petstore.sh
./bin/ruby-petstore.sh
./bin/scala-petstore.sh
