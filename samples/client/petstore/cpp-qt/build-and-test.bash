#!/bin/bash

set -e
# export RUN_VALGRIND_TESTS=TRUE

mkdir -p build
cd build

cmake ..

make

if [[ -z "${RUN_VALGRIND_TESTS}" ]]; then
    echo "Running Qt5 Petstore Tests"
    ./cpp-qt5-petstore
else
  echo "Running Qt5 Petstore Tests with Valgrind"
  valgrind --leak-check=full ./cpp-qt5-petstore |& tee result.log || exit 1
  testCount=$(cat result.log | grep 'Finished testing of' | wc -l)
  if [ $testCount == 3 ]
  then
    echo "Ok"
  else
    echo "The tests were not run!!!"
    exit 1
  fi

  echo "Make sure the tests passed:"
  successCount=$(cat result.log | grep '0 failed' | wc -l)
  if [ $successCount == 3 ]
  then
    echo "Ok"
  else
    echo "The tests failed!!!"
    exit 1
  fi

  echo "Check if no memory leaks occured:"
  leakCount=$(cat result.log | grep 'lost: 0 bytes in 0 blocks' | wc -l)
  if [ $leakCount == 3 ]
  then
    echo "Ok"
  else
    echo "There was memory leaks!!!"
    exit 1
  fi
fi

