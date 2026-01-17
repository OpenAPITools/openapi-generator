#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

export NODE_ENV=test

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX ..."

  sudo apt-get -y install cpanminus

  echo "Testing perl"
  (cd samples/client/petstore/perl && /bin/bash ./test.bash)


elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test cpp-restsdk"

  # install cpprestsdk
  sudo apt-get install libcpprest-dev
  wget "https://github.com/aminya/setup-cpp/releases/download/v0.37.0/setup-cpp-x64-linux"
  chmod +x ./setup-cpp-x64-linux
  sudo ./setup-cpp-x64-linux --compiler llvm --cmake true --ninja true
  source ~/.cpprc # activate cpp environment variables

  (cd samples/client/petstore/cpp-restsdk/client && mvn integration-test)

elif [ "$NODE_INDEX" = "3" ]; then

  echo "Running node $NODE_INDEX ... "

  echo "Testing ruby"
  (cd samples/client/petstore/ruby && mvn integration-test)
  (cd samples/client/petstore/ruby-faraday && mvn integration-test)
  (cd samples/client/petstore/ruby-httpx && mvn integration-test)
  (cd samples/client/petstore/ruby-autoload && mvn integration-test)

else
  echo "Running node $NODE_INDEX ..."
  java -version
  #./mvnw clean install

  sudo apt-get update
  sudo apt-get install -y apt-transport-https software-properties-common
  # Add CRAN repository for R 4.x
  #sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
  sudo add-apt-repository 'deb https://cloud.r-project.org jammy-cran40/'
  sudo apt-get update
  sudo apt-get install -y r-base

  #sudo apt install r-base r-base-dev -y
  sudo apt-get install r-base-core libssl-dev libcurl4-openssl-dev -y # for httr

  R version

  (cd samples/client/petstore/R && mvn integration-test)
  #(cd samples/client/petstore/R-httr2 && mvn integration-test)

fi
