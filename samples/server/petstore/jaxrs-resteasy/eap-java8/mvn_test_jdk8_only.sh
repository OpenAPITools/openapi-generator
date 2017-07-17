#!/bin/bash

java -version 2>&1 | grep "java version \"1.8"

if [ $? -eq 0 ]
then
    echo "Running JDK8"
    mvn test
else
    echo "Not running JDK8"
fi
