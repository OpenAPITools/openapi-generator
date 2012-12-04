#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export CLASSPATH="$DIR/../target/*:$DIR/../target/lib/*"
export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
JAVA_OPTS=$JAVA_OPTS scala -cp $CLASSPATH "$@" samples/client/petstore/objc/ObjcPetstoreCodegen.scala http://petstore.swagger.wordnik.com/api/api-docs.json special-key
