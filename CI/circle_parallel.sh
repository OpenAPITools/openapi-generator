#!/bin/bash
#
# A bash script to run CircleCI node/test in parallel
#

NODE_INDEX=${CIRCLE_NODE_INDEX:-0}

set -e

export NODE_ENV=test

# Helper function to check if tests should run for this node based on changed files
should_run_tests() {
  local node_index=$1
  local sample_patterns=$2  # space-separated glob patterns for samples this node tests

  echo ""
  echo "════════════════════════════════════════════════════════════════"
  echo "Node $node_index: Determining if tests should run"
  echo "════════════════════════════════════════════════════════════════"

  # Always run tests if origin/master doesn't exist (e.g., fresh CI environment)
  if ! git rev-parse origin/master >/dev/null 2>&1; then
    echo "ℹ️  origin/master not available locally (fresh checkout or CI environment)"
    echo "✓ DECISION: Running tests (cannot determine changes safely)"
    return 0
  fi

  # Try to get list of changed files since origin/master
  local changed
  changed=$(git diff --name-only origin/master HEAD 2>/dev/null) || {
    echo "⚠️  git diff origin/master HEAD failed"
    echo "✓ DECISION: Running tests (cannot determine changes safely)"
    return 0
  }

  if [ -z "$changed" ]; then
    echo "ℹ️  No files changed since origin/master"
    echo "✗ DECISION: Skipping tests (nothing to test)"
    return 1
  fi

  echo "Changed files:"
  echo "$changed" | sed 's/^/  /'

  # Check if generator source or config changed (affects all samples)
  echo ""
  echo "Checking for generator/config changes (affects all samples)..."
  if echo "$changed" | grep -qE '^(modules/|bin/configs/|pom\.xml|\.mvn/)'; then
    echo "✓ Found: generator or config files changed"
    echo "✓ DECISION: Running tests (generator/config affects all samples)"
    return 0
  fi

  # Check if this node's sample directories changed
  echo ""
  echo "Checking for sample changes relevant to this node..."
  for pattern in $sample_patterns; do
    if echo "$changed" | grep -q "^$pattern"; then
      echo "✓ Found: $pattern"
      echo "✓ DECISION: Running tests (relevant sample changed)"
      return 0
    fi
  done

  echo "✗ No relevant sample changes found for node $node_index"
  echo "✗ DECISION: Skipping tests (no relevant changes)"
  return 1
}

if [ "$NODE_INDEX" = "1" ]; then
  echo "Running node $NODE_INDEX ..."

  if should_run_tests "$NODE_INDEX" "samples/client/petstore/perl/"; then
    sudo apt-get -y install cpanminus

    echo "Testing perl"
    (cd samples/client/petstore/perl && /bin/bash ./test.bash)
  else
    echo "Skipping perl tests — no relevant changes"
  fi


elif [ "$NODE_INDEX" = "2" ]; then
  echo "Running node $NODE_INDEX to test cpp-restsdk"

  if should_run_tests "$NODE_INDEX" "samples/client/petstore/cpp-restsdk/"; then
    # install cpprestsdk
    sudo apt-get install libcpprest-dev
    wget "https://github.com/aminya/setup-cpp/releases/download/v0.37.0/setup-cpp-x64-linux"
    chmod +x ./setup-cpp-x64-linux
    sudo ./setup-cpp-x64-linux --compiler llvm --cmake true --ninja true
    source ~/.cpprc # activate cpp environment variables

    (cd samples/client/petstore/cpp-restsdk/client && mvn integration-test)
  else
    echo "Skipping cpp-restsdk tests — no relevant changes"
  fi

elif [ "$NODE_INDEX" = "3" ]; then

  echo "Running node $NODE_INDEX ... "

  if should_run_tests "$NODE_INDEX" "samples/client/petstore/ruby"; then
    echo "Testing ruby"
    (cd samples/client/petstore/ruby && mvn integration-test)
    (cd samples/client/petstore/ruby-faraday && mvn integration-test)
    (cd samples/client/petstore/ruby-httpx && mvn integration-test)
    (cd samples/client/petstore/ruby-autoload && mvn integration-test)
  else
    echo "Skipping ruby tests — no relevant changes"
  fi

else
  echo "Running node $NODE_INDEX ..."
  java -version

fi
