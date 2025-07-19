#!/bin/bash

set -e
set -u

mkdir -p build
cd build

cmake ..

make

make test
