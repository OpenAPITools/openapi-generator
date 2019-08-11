#!/bin/bash

set -e

mkdir build
cd build
# project
qmake ../PetStore/PetStore.pro

make

./PetStore
