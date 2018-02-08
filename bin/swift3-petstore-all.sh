#!/bin/sh

echo "#### Petstore Swift API client (default) ####"
./bin/swift3-petstore.sh

echo "#### Petstore Swift API client (promisekit) ####"
./bin/swift3-petstore-promisekit.sh

echo "#### Petstore Swift API client (rxswift) ####"
./bin/swift3-petstore-rxswift.sh

echo "#### Petstore Swift API client (unwraprequired) ####"
./bin/swift3-petstore-unwraprequired.sh

echo "#### Petstore Swift API client (objcCompatible) ####"
./bin/swift3-petstore-objcCompatible.sh
