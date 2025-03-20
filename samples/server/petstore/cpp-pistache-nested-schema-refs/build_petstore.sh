#!/bin/bash
# build C++ pistache petstore
#

mkdir -p build
cd build
cmake ..
make -j
