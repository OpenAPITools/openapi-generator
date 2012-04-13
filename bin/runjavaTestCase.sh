#!/bin/bash
echo "" > classpath.txt
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
#first argument to the command line script give location of the library jar file
export CLASSPATH=$(cat classpath.txt)$2
export JAVA_OPTS="${JAVA_OPTS} -DrulePath=data -Dproperty=Xmx2g -DloggerPath=$BUILD_COMMON/test-config/log4j.properties"
java $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS $JAVA_OPTS -cp $CLASSPATH "$@"

