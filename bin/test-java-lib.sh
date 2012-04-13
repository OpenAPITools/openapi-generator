#!/bin/bash
if [ $# -ne 7 ]
then
    echo "Error in $0 - Invalid Argument Count"
    echo "Syntax: $0 location_of_client_library location_of_service api_key test_script_location test_data_location test_data_class_name api_classes_package_name"
    exit
fi
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
export CLASSPATH=$(cat classpath.txt)$7
echo $CLASSPATH
export JAVA_OPTS="${JAVA_OPTS} -Dproperty=Xmx2g "
java $WORDNIK_OPTS $JAVA_CONFIG_OPTIONS $JAVA_OPTS -cp $CLASSPATH com.wordnik.swagger.testframework.APITestRunner "$@" JAVA