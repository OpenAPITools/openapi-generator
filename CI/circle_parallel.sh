#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

export NODE_ENV=test

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci' defined in pom.xml ..."
  java -version

  ./mvnw --no-snapshot-updates --quiet verify -Psamples.circleci -Dorg.slf4j.simpleLogger.defaultLogLevel=error

elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test Go"
  # install haskell
  #curl -sSLk https://get.haskellstack.org/ | sh
  #stack upgrade
  #stack --version

  # install curl
  #sudo apt-get -y build-dep libcurl4-gnutls-dev
  #sudo apt-get -y install libcurl4-gnutls-dev

  # Install golang version 1.18
  go version
  sudo mkdir /usr/local/go1.18
  wget -c https://dl.google.com/go/go1.18.linux-amd64.tar.gz -O - | sudo tar -xz -C /usr/local/go1.18
  export PATH="/usr/local/go1.18/go/bin:$PATH"
  go version

  # run integration tests
  ./mvnw --no-snapshot-updates --quiet verify -Psamples.misc -Dorg.slf4j.simpleLogger.defaultLogLevel=error
elif [ "$NODE_INDEX" = "3" ]; then

  echo "Running node $NODE_INDEX to test 'samples.circleci.node3' defined in pom.xml ..."

  # Install node@stable (for angular 6)
  set +e
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
  export NVM_DIR="/opt/circleci/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  #nvm install stable
  # install v16 instead of the latest stable version
  nvm install 16
  nvm alias default 16
  node --version

  # Each step uses the same `$BASH_ENV`, so need to modify it
  echo 'export NVM_DIR="/opt/circleci/.nvm"' >> $BASH_ENV
  echo "[ -s \"$NVM_DIR/nvm.sh\" ] && . \"$NVM_DIR/nvm.sh\"" >> $BASH_ENV

  ./mvnw --no-snapshot-updates --quiet verify -Psamples.circleci.node3 -Dorg.slf4j.simpleLogger.defaultLogLevel=error

elif [ "$NODE_INDEX" = "4" ]; then
  echo "Running node $NODE_INDEX to test 'samples.circleci.node4' defined in pom.xml ..."

  #wget https://www.python.org/ftp/python/3.8.9/Python-3.8.9.tgz
  #tar -xf Python-3.8.9.tgz
  #cd Python-3.8.9
  #./configure --enable-optimizations
  #sudo make altinstall
  pyenv install --list 
  pyenv install 3.7.12
  #pyenv install 2.7.14 #python2 no longer supported
  pyenv global 3.7.12

  (cd samples/openapi3/client/petstore/python && mvn integration-test)
  (cd samples/openapi3/client/petstore/python-pydantic-v1 && mvn integration-test)
  (cd samples/openapi3/client/petstore/python-aiohttp && mvn integration-test)
  (cd samples/openapi3/client/petstore/python-pydantic-v1-aiohttp && mvn integration-test)

else
  echo "Running node $NODE_INDEX to test 'samples.circleci.others' defined in pom.xml ..."
  java -version

  (cd samples/client/petstore/scala-akka && mvn integration-test)
  (cd samples/client/petstore/scala-sttp && mvn integration-test)
  (cd dsamples/client/petstore/scala-sttp4 && mvn integration-test)
  (cd samples/client/petstore/clojure && mvn integration-test)
  (cd samples/client/petstore/java/jersey2-java8 && mvn integration-test)
  (cd samples/openapi3/client/petstore/java/jersey2-java8 && mvn integration-test)
  (cd samples/client/petstore/java/jersey3 && mvn integration-test)
  (cd samples/client/others/java/okhttp-gson-streaming && mvn integration-test)
  (cd samples/client/petstore/java/okhttp-gson && mvn integration-test)
  (cd samples/client/petstore/java/okhttp-gson-3.1 && mvn integration-test)
  (cd samples/client/petstore/java/resteasy && mvn integration-test)
  (cd samples/client/petstore/java-micronaut-client && mvn integration-test)
  (cd samples/client/petstore/java/apache-httpclient && mvn integration-test)

  #./mvnw --no-snapshot-updates --quiet verify -Psamples.circleci.others -Dorg.slf4j.simpleLogger.defaultLogLevel=error
  #./mvnw --no-snapshot-updates --quiet javadoc:javadoc -Psamples.circleci -Dorg.slf4j.simpleLogger.defaultLogLevel=error
fi


