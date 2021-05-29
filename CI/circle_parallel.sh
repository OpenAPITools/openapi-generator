#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

function cleanup {
  # Show logs of 'petstore.swagger' container to troubleshoot Unit Test failures, if any.
  docker logs petstore.swagger # container name specified in circle.yml
}

trap cleanup EXIT

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci' defined in pom.xml ..."
  java -version

  mvn --no-snapshot-updates --quiet verify -Psamples.circleci -Dorg.slf4j.simpleLogger.defaultLogLevel=error

  echo "show ivy2 cache"
  ls -l /home/circleci/.ivy2/cache

elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test haskell"
  # install haskell
  curl -sSL https://get.haskellstack.org/ | sh
  stack upgrade
  stack --version
  # prepare r
  sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
  gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
  gpg -a --export E084DAB9 | sudo apt-key add -
  sudo apt-get update
  sudo apt-get -y install r-base
  R --version

  # install curl
  sudo apt-get -y build-dep libcurl4-gnutls-dev
  sudo apt-get -y install libcurl4-gnutls-dev

  # run integration tests
  mvn --no-snapshot-updates --quiet verify -Psamples.misc -Dorg.slf4j.simpleLogger.defaultLogLevel=error
else
  echo "Running node $NODE_INDEX to test 'samples.circleci.others' defined in pom.xml ..."
  #sudo update-java-alternatives -s java-1.7.0-openjdk-amd64
  java -version

  # Install golang version 1.14
  go version
  sudo mkdir /usr/local/go1.14
  wget -c https://dl.google.com/go/go1.14.linux-amd64.tar.gz -O - | sudo tar -xz -C /usr/local/go1.14
  export PATH="/usr/local/go1.14/go/bin:$PATH"
  go version

  mvn --no-snapshot-updates --quiet verify -Psamples.circleci.others -Dorg.slf4j.simpleLogger.defaultLogLevel=error
  mvn --no-snapshot-updates --quiet javadoc:javadoc -Psamples.circleci -Dorg.slf4j.simpleLogger.defaultLogLevel=error
fi


