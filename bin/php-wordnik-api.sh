#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export CLASSPATH="$DIR/../target/lib/*:$DIR/../target/*"
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties -DfileMap=samples/client/wordnik-api/spec-files"
JAVA_OPTS=$JAVA_OPTS scala -cp $CLASSPATH "$@" samples/client/wordnik-api/php/PHPWordnikApiCodegen.scala http://api.wordnik.com/v4/resources.json
