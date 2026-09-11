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
  ruby_samples=(
    samples/client/petstore/ruby
    samples/client/petstore/ruby-faraday
    samples/client/petstore/ruby-httpx
    samples/client/petstore/ruby-autoload
  )
  if [ "${CIRCLE_SELECTED_SAMPLES_JSON+x}" ]; then
    selected=$(python3 - "${ruby_samples[@]}" <<'PY'
import json
import os
import sys

samples = json.loads(os.environ["CIRCLE_SELECTED_SAMPLES_JSON"])
if (not isinstance(samples, list) or not samples
        or any(not isinstance(sample, str) or sample not in sys.argv[1:] for sample in samples)
        or len(samples) != len(set(samples))):
    raise ValueError("Invalid selected CircleCI Ruby samples")
print("\n".join(samples))
PY
)
    readarray -t ruby_samples <<< "$selected"
  fi
  for sample in "${ruby_samples[@]}"; do
    (cd "$sample" && mvn integration-test)
  done

else
  echo "Running node $NODE_INDEX ..."
  java -version

fi
