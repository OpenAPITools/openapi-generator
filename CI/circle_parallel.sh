#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

# Retry a command up to $1 times, with exponential backoff starting at $2 seconds.
retry() {
  local attempts=$1; shift
  local delay=$1;    shift
  local count=0
  until "$@"; do
    count=$((count + 1))
    if [ "$count" -ge "$attempts" ]; then
      echo "Command failed after $attempts attempts: $*" >&2
      return 1
    fi
    echo "Attempt $count failed. Retrying in ${delay}s..." >&2
    sleep "$delay"
    delay=$((delay * 2))
  done
}

# Wrapper for apt-get install with automatic retry on transient network errors.
apt_install() {
  retry 3 30 sudo apt-get -y install --fix-missing "$@"
}

export NODE_ENV=test

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX ..."

  apt_install cpanminus

  echo "Testing perl"
  (cd samples/client/petstore/perl && /bin/bash ./test.bash)


elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test cpp-restsdk"

  # install cpprestsdk
  apt_install libcpprest-dev
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
  ./mvnw clean install

fi
