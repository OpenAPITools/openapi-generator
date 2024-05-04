#!/bin/bash
# ref: http://pistache.io/quickstart#installing-pistache
#
echo "Installing Pistache ..."

git clone https://github.com/oktal/pistache.git || true
cd pistache || (echo "Cannot find git clone pistache directory"; exit 1)

git submodule update --init
mkdir -p build
cd build || (echo "Cannot find build directory"; exit 1)

cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ..
meson
