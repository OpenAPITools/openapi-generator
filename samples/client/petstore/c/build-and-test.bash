#!/bin/bash

set -e

#install latest curl
wget https://curl.haxx.se/download/curl-7.61.1.zip
unzip curl-7.61.1.zip
cd curl-7.61.1
./configure
make
sudo make install
cd ..

# project
cmake .

make 

./unit-manual-PetAPI
./unit-manual-UserAPI
./unit-manual-StoreAPI
