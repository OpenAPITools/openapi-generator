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
echo -n 'build/main/java' >> classpath.txt;
echo -n ':' >> classpath.txt;
#first argument to the command line script give location of the library jar file
export CLASSPATH=$(cat classpath.txt)$2
export JAVA_OPTS="${JAVA_OPTS} -DrulePath=data -Dproperty=Xmx2g -DloggerPath=$BUILD_COMMON/test-config/log4j.properties"
java $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS $JAVA_OPTS -cp $CLASSPATH "$@" com.wordnik.swagger.api ../swagger-sample-app/sdk-libs/bin/AirExecutorApp-app.xml "/Applications/Adobe Flash Builder 4/sdks/4.1.0"