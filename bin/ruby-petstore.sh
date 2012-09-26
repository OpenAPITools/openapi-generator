#!/bin/bash

echo "" > classpath.txt
for file in `ls target/lib`;
        do echo -n 'target/lib/' >> classpath.txt;
        echo -n $file >> classpath.txt;
        echo -n ':' >> classpath.txt;
done

for file in `ls target/*.jar`;
        do echo -n '' >> classpath.txt;
        echo -n $file >> classpath.txt;
        echo -n ':' >> classpath.txt;
done
export CLASSPATH=$(cat classpath.txt)


export JAVA_OPTS="${JAVA_OPTS} -Xmx1024M -DloggerPath=conf/log4j.properties"
scala $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS -cp $CLASSPATH "$@" samples/client/petstore/ruby/RubyPetstoreCodegen.scala http://petstore.swagger.wordnik.com/api/resources.json special-key
