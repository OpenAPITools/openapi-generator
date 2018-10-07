#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX to test CI/pom.xml.circleci ..."
  cp CI/pom.xml.circleci pom.xml
  java -version
  mvn --quiet verify -Psamples
elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test ensure-up-to-date with JDK 9"
  ls -l /usr/lib/jvm/
  sudo update-java-alternatives -s java-1.9.0-openjdk-amd64
  java -version
  #export GO_POST_PROCESS_FILE="/usr/local/bin/gofmt -w"
  # not formatting the code as different go versions may format the code a bit different
  ./bin/utils/ensure-up-to-date
else
  echo "Running node $NODE_INDEX to test CI/pom.xml.circleci.java7 ..."
  sudo update-java-alternatives -s java-1.7.0-openjdk-amd64
  java -version
  cp CI/pom.xml.circleci.java7 pom.xml
  mvn --quiet verify -Psamples
fi


