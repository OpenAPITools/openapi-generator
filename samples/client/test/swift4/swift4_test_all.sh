#/bin/bash

set -e

# example project with unit tests
mvn -f default/TestClientApp/pom.xml integration-test

# spm build
mvn -f default/pom.xml integration-test
