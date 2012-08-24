#!/bin/bash
echo "" > classpath.txt
for file in `ls target/*.jar`;
        do echo -n '' >> classpath.txt;
        echo -n $file >> classpath.txt;
        echo -n ':' >> classpath.txt;
done
for file in `ls target/lib`;
	do echo -n 'target/lib/' >> classpath.txt;
	echo -n $file >> classpath.txt;
	echo -n ':' >> classpath.txt;
done

WORDNIK_OPTS=${WORDNIK_OPTS:=NOPE}

if [ $WORDNIK_OPTS = NOPE ];
	then
    export WORDNIK_OPTS="-DconfigFile=conf/config.xml -DdevMode=true"
	
fi

export CLASSPATH=$(cat classpath.txt)
export JAVA_OPTS="${JAVA_OPTS} -DrulePath=data -Xmx4096M -DloggerPath=conf/log4j.properties"
scala $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS -cp $CLASSPATH "$@"
