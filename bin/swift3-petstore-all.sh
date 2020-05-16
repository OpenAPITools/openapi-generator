#!/bin/bash

echo "#### Petstore Swift API client (default) ####"
source ./bin/swift3-petstore.sh 1>/dev/null

echo "#### Petstore Swift API client (promisekit) ####"
source ./bin/swift3-petstore-promisekit.sh 1>/dev/null

echo "#### Petstore Swift API client (rxswift) ####"
source ./bin/swift3-petstore-rxswift.sh 1>/dev/null

echo "#### Petstore Swift API client (unwraprequired) ####"
source ./bin/swift3-petstore-unwraprequired.sh 1>/dev/null

echo "#### Petstore Swift API client (objcCompatible) ####"
source ./bin/swift3-petstore-objcCompatible.sh 1>/dev/null
