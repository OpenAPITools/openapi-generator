#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

export NODE_ENV=test

function cleanup {
  # Show logs of 'petstore.swagger' container to troubleshoot Unit Test failures, if any.
  if [ "$NODE_INDEX" != "3" ]; then
    docker logs petstore.swagger # container name specified in circle.yml
  fi
}

trap cleanup EXIT

if [ "$NODE_INDEX" = "0" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci.node$NODE_INDEX' defined in pom.xml ..."
  #sudo update-java-alternatives -s java-1.7.0-openjdk-amd64
  java -version

  mvn --no-snapshot-updates --quiet verify -Psamples.circleci.node0 -Dorg.slf4j.simpleLogger.defaultLogLevel=error
  mvn --no-snapshot-updates --quiet javadoc:javadoc -Psamples.circleci.node1 -Dorg.slf4j.simpleLogger.defaultLogLevel=error
elif [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci.node$NODE_INDEX' defined in pom.xml ..."
  java -version

  mvn --no-snapshot-updates --quiet verify -Psamples.circleci.node1 -Dorg.slf4j.simpleLogger.defaultLogLevel=error

elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci.node$NODE_INDEX' defined in pom.xml ..."
  # install haskell
  #curl -sSLk https://get.haskellstack.org/ | sh
  #stack upgrade
  #stack --version
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

  # Install golang version 1.14
  go version
  sudo mkdir /usr/local/go1.14
  wget -c https://dl.google.com/go/go1.14.linux-amd64.tar.gz -O - | sudo tar -xz -C /usr/local/go1.14
  export PATH="/usr/local/go1.14/go/bin:$PATH"
  go version

  # run integration tests
  mvn --no-snapshot-updates --quiet verify -Psamples.circleci.node2 -Dorg.slf4j.simpleLogger.defaultLogLevel=error
elif [ "$NODE_INDEX" = "3" ]; then

  echo "Running node $NODE_INDEX to test 'samples.circleci.node$NODE_INDEX' defined in pom.xml ..."
  #wget https://www.python.org/ftp/python/3.8.9/Python-3.8.9.tgz
  #tar -xf Python-3.8.9.tgz
  #cd Python-3.8.9
  #./configure --enable-optimizations
  #sudo make altinstall

  # Install node@stable (for angular 6)
  #  set +e
  #  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
  #  export NVM_DIR="/opt/circleci/.nvm"
  #  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  #nvm install stable
  # install v16 instead of the latest stable version
  #  nvm install 16
  #  nvm alias default 16
  #  node --version

  # Each step uses the same `$BASH_ENV`, so need to modify it
  #  echo 'export NVM_DIR="/opt/circleci/.nvm"' >> $BASH_ENV
  #  echo "[ -s \"$NVM_DIR/nvm.sh\" ] && . \"$NVM_DIR/nvm.sh\"" >> $BASH_ENV
  # echo which java
  apt-get update && apt-get install -y default-jdk maven sudo iptables
  java -version
  export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
  echo $JAVA_HOME
  whoami
  sudo iptables -t nat -A OUTPUT -o lo -p tcp --dport 80 -j REDIRECT --to-port 8080

  mvn --no-snapshot-updates --quiet verify -Psamples.circleci.node3 -Dorg.slf4j.simpleLogger.defaultLogLevel=error
fi


