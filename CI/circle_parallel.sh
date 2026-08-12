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

  # install cpprestsdk and C++ build tools via apt (avoids setup-cpp's PPA/GPG key fetch)
  sudo apt-get install -y libcpprest-dev clang cmake

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

fi
