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
./bin/akka-scala-petstore.sh
./bin/android-petstore.sh
./bin/clojure-petstore.sh
./bin/csharp-petstore.sh
./bin/dynamic-html.sh
./bin/haskell-petstore.sh
./bin/html-petstore.sh
./bin/java-petstore.sh
./bin/java-petstore-jersey2.sh
./bin/java-petstore-okhttp-gson.sh
./bin/java-petstore-retrofit.sh
+./bin/java-petstore-retrofit2.sh
./bin/jaxrs-petstore-server.sh
./bin/nodejs-petstore-server.sh
./bin/objc-petstore.sh
./bin/perl-petstore.sh
./bin/php-petstore.sh
./bin/python-petstore.sh
./bin/qt5-petstore.sh
./bin/ruby-petstore.sh
./bin/scala-async-petstore.sh
./bin/scala-petstore.sh
./bin/scalatra-petstore-server.sh
./bin/silex-petstore-server.sh
./bin/slim-petstore-server.sh
./bin/spring-mvc-petstore-server.sh
./bin/spring-mvc-petstore-j8-async-server.sh
./bin/swift-petstore.sh
./bin/tizen-petstore.sh
./bin/typescript-angular-petstore.sh
./bin/typescript-node-petstore.sh
