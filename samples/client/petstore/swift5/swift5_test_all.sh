#!/bin/bash

set -e

DIRECTORY=`dirname $0`

# example project with unit tests
mvn -f $DIRECTORY/alamofireLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/combineLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/default/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/promisekitLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/rxswiftLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f $DIRECTORY/urlsessionLibrary/SwaggerClientTests/pom.xml integration-test

# spm build
mvn -f $DIRECTORY/alamofireLibrary/pom.xml integration-test
# mvn -f $DIRECTORY/asyncAwaitLibrary/pom.xml integration-test
mvn -f $DIRECTORY/combineLibrary/pom.xml integration-test
mvn -f $DIRECTORY/default/pom.xml integration-test
mvn -f $DIRECTORY/deprecated/pom.xml integration-test
mvn -f $DIRECTORY/frozenEnums/pom.xml integration-test
mvn -f $DIRECTORY/nonPublicApi/pom.xml integration-test
mvn -f $DIRECTORY/objcCompatible/pom.xml integration-test
mvn -f $DIRECTORY/oneOf/pom.xml integration-test
mvn -f $DIRECTORY/promisekitLibrary/pom.xml integration-test
mvn -f $DIRECTORY/readonlyProperties/pom.xml integration-test
mvn -f $DIRECTORY/resultLibrary/pom.xml integration-test
mvn -f $DIRECTORY/rxswiftLibrary/pom.xml integration-test
mvn -f $DIRECTORY/urlsessionLibrary/pom.xml integration-test
mvn -f $DIRECTORY/vaporLibrary/pom.xml integration-test
mvn -f $DIRECTORY/x-swift-hashable/pom.xml integration-test
