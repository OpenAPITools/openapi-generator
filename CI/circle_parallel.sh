#!/bin/bash

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}


mvn --quiet clean install

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX ..."
  cp CI/pom.xml.circleci pom.xml
  java -version
  mvn --quiet verify -Psamples
else
  echo "Running node $NODE_INDEX ..."
  sudo update-java-alternatives -s java-1.7.0-openjdk-amd64
  java -version
  cp CI/pom.xml.circleci.java7 pom.xml
  mvn --quiet verify -Psamples
fi


