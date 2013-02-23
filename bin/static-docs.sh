#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export CLASSPATH="$DIR/../target/lib/*:$DIR/../target/*"
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties -DfileMap=src/test/resources/petstore"
java -cp $CLASSPATH $JAVA_OPTS "$@" SwaggerDocGenerator http://petstore.swagger.wordnik.com/api/api-docs.json special-key
