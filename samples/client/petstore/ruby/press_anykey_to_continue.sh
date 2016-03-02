#!/bin/bash
# a script to simply wait for X seconds before contining the CI tests
# the delay can help prevent 2 CI tests running at the same time as 
# all CI tests use the same petstore server for testing.

TIMEOUT=$(( ( RANDOM % 60 )  + 1 ))

read -p 'Press any key to continue or wait for $TIMEOUT seconds' -t $TIMEOUT
