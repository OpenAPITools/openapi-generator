#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export CLASSPATH="$DIR/../target/lib/*:$DIR/../target/*"
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties -DfileMap=samples/client/wordnik-api/spec-files"
scala $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS -cp $CLASSPATH "$@" samples/client/wordnik-api/java/JavaWordnikApiCodegen.scala http://api.wordnik.com/v4/resources.json
