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


# if you've executed sbt assembly previously it will use that instead.
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties -DfileMap=samples/client/wordnik-api/spec-files"
ags="$@ samples/client/wordnik-api/python/PythonWordnikApiCodegen.scala http://api.wordnik.com/v4/resources.json"

if [ -f $APP_DIR/target/scala-2.9.1/swagger-codegen.jar ]; then
  scala -cp target/scala-2.9.1/swagger-codegen.jar $ags
elif [[ -f $APP_DIR/target/scala-2.10/swagger-codegen.jar  ]]; then
  scala -cp target/scala-2.10/swagger-codegen.jar $ags
else
  ./sbt assembly
  scala -cp target/swagger-codegen.jar $ags
fi
