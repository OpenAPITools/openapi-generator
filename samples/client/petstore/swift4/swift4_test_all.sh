#!/bin/bash

set -e

DIRECTORY=`dirname $0`

# example project with unit tests
mvn -f $DIRECTORY/default/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/promisekitLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/rxswiftLibrary/SwaggerClientTests/pom.xml integration-test

# spm build
mvn -f $DIRECTORY/default/pom.xml integration-test
mvn -f $DIRECTORY/nonPublicApi/pom.xml integration-test
mvn -f $DIRECTORY/objcCompatible/pom.xml integration-test
mvn -f $DIRECTORY/promisekitLibrary/pom.xml integration-test
mvn -f $DIRECTORY/resultLibrary/pom.xml integration-test
mvn -f $DIRECTORY/rxswiftLibrary/pom.xml integration-test
mvn -f $DIRECTORY/unwrapRequired/pom.xml integration-test
