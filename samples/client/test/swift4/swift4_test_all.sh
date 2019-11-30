#/bin/bash

set -e

DIRECTORY=`dirname $0`

# example project with unit tests
mvn -f $DIRECTORY/default/TestClientApp/pom.xml integration-test

# spm build
mvn -f $DIRECTORY/default/pom.xml integration-test
