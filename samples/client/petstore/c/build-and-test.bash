#!/bin/bash

set -e

# project
mkdir -p build
cd build

#install latest curl
wget https://curl.haxx.se/download/curl-7.61.1.zip
unzip curl-7.61.1.zip
cd curl-7.61.1
./configure
make
sudo make install
cd ..

# build project
cmake ..
make

if [ -f unit-manual-PetAPI ]; then ./unit-manual-PetAPI; fi
if [ -f unit-manual-UserAPI ]; then ./unit-manual-UserAPI; fi
if [ -f unit-manual-StoreAPI ]; then ./unit-manual-StoreAPI; fi

