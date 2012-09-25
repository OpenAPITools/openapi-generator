#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export CLASSPATH="$DIR/../target/*:$DIR/../target/lib/*"
export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
scala $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS -cp $CLASSPATH "$@" samples/client/petstore/objc/ObjcPetstoreCodegen.scala http://petstore.swagger.wordnik.com/api/resources.json special-key
