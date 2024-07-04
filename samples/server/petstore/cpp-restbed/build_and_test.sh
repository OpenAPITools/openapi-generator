#!/usr/bin/env bash

set -u
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd ${SCRIPT_DIR}

rm -rf build

cmake -S . -B build
cd build
make

# pure C++ tests
make test -j4

# tests with Java client
# run them during development
#make run_all_java_client_test_for_cpp_server