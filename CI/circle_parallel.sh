#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

export NODE_ENV=test

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX ..."
  java -version

  sudo apt-get -y install cpanminus

  # install rust
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source "$HOME/.cargo/env"

  echo "Testing perl"
  (cd samples/client/petstore/perl && /bin/bash ./test.bash)

  echo "Testing ruby"
  (cd samples/client/petstore/ruby && mvn integration-test)
  (cd samples/client/petstore/ruby-faraday && mvn integration-test)
  (cd samples/client/petstore/ruby-httpx && mvn integration-test)
  (cd samples/client/petstore/ruby-autoload && mvn integration-test)

  echo "Testing rust"
  (cd samples/server/petstore/rust-axum && mvn integration-test)

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

  # install cpprestsdk
  sudo apt-get install libcpprest-dev
  wget "https://github.com/aminya/setup-cpp/releases/download/v0.37.0/setup-cpp-x64-linux"
  chmod +x ./setup-cpp-x64-linux
  sudo ./setup-cpp-x64-linux --compiler llvm --cmake true --ninja true
  source ~/.cpprc # activate cpp environment variables

  # run go integration tests
  (cd samples/client/petstore/go && mvn integration-test)
  (cd samples/openapi3/client/petstore/go && mvn integration-test)
  (cd samples/openapi3/client/petstore/go-petstore-generateMarshalJSON-false && mvn integration-test)
  (cd samples/client/others/go/allof_multiple_ref_and_discriminator && mvn integration-test)
  (cd samples/client/petstore/cpp-restsdk/client && mvn integration-test)

elif [ "$NODE_INDEX" = "3" ]; then

  echo "Running node $NODE_INDEX ... "

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

  (cd samples/client/others/typescript-angular && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v12-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v13-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v14-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v15-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v16-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v17-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v18-provided-in-root && mvn integration-test)
  (cd samples/client/petstore/typescript-angular-v19-provided-in-root && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/default && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/tests/default && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/jquery && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/tests/jquery && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/object_params && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/tests/object_params && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/inversify && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/tests/inversify && mvn integration-test)
  #(cd samples/openapi3/client/petstore/typescript/tests/deno && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/browser && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/tests/browser && mvn integration-test)
  (cd samples/openapi3/client/petstore/typescript/builds/nullable-enum && mvn integration-test)
  (cd samples/client/petstore/typescript-fetch/builds/default && mvn integration-test)
  (cd samples/client/petstore/typescript-fetch/builds/es6-target && mvn integration-test)
  (cd samples/client/petstore/typescript-fetch/builds/with-npm-version && mvn integration-test)
  (cd samples/client/petstore/typescript-fetch/tests/default && mvn integration-test)
  (cd samples/client/petstore/typescript-node/npm && mvn integration-test)
  (cd samples/client/petstore/typescript-rxjs/builds/with-npm-version && mvn integration-test)
  (cd samples/client/petstore/typescript-axios/builds/with-npm-version && mvn integration-test)
  (cd samples/client/petstore/typescript-axios/tests/default && mvn integration-test)
  (cd samples/client/petstore/typescript-axios/tests/with-complex-headers && mvn integration-test)
  (cd samples/client/petstore/javascript-flowtyped && mvn integration-test)
  (cd samples/client/petstore/javascript-es6 && mvn integration-test)
  (cd samples/client/petstore/javascript-promise-es6 && mvn integration-test)

else
  echo "Running node $NODE_INDEX ..."
  java -version

  (cd samples/client/petstore/scala-akka && mvn integration-test)
  (cd samples/client/petstore/scala-sttp && mvn integration-test)
  (cd samples/client/petstore/scala-sttp-circe && mvn integration-test)
  (cd samples/client/petstore/scala-sttp4 && mvn integration-test)
  (cd samples/client/petstore/clojure && mvn integration-test)
  (cd samples/client/petstore/java/jersey2-java8 && mvn integration-test)
  (cd samples/openapi3/client/petstore/java/jersey2-java8 && mvn integration-test)
  (cd samples/client/petstore/java/jersey3 && mvn integration-test)
  (cd samples/client/petstore/java/jersey3-oneOf && mvn integration-test)
  (cd samples/client/others/java/okhttp-gson-streaming && mvn integration-test)
  (cd samples/client/petstore/java/okhttp-gson && mvn integration-test)
  (cd samples/client/petstore/java/okhttp-gson-3.1 && mvn integration-test)
  (cd samples/client/petstore/java/okhttp-gson-dynamicOperations && mvn integration-test)
  (cd samples/client/petstore/java/resteasy && mvn integration-test)
  (cd samples/client/petstore/java-micronaut-client && mvn integration-test)
  (cd samples/client/petstore/java/apache-httpclient && mvn integration-test)
  (cd samples/client/petstore/java/resttemplate-jakarta && mvn integration-test)

fi
