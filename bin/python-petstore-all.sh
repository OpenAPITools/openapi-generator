#!/bin/bash
# update python petstore clients

source ./bin/python-asyncio-petstore.sh 1>/dev/null
source ./bin/python-petstore.sh 1>/dev/null
source ./bin/python-experimental-petstore.sh 1>/dev/null
source ./bin/python-tornado-petstore.sh 1>/dev/null
