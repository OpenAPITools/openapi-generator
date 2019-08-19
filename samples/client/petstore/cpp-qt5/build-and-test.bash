#!/bin/bash

set -e

mkdir build
cd build

qmake ../PetStore/PetStore.pro
make

./PetStore
