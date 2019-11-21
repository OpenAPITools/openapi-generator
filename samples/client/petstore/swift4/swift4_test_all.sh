#/bin/bash

set -e

# example project with unit tests
mvn -f default/SwaggerClientTests/pom.xml integration-test
mvn -f promisekitLibrary/SwaggerClientTests/pom.xml integration-test
mvn -f rxswiftLibrary/SwaggerClientTests/pom.xml integration-test

# spm build
mvn -f default/pom.xml integration-test
# mvn -f objcCompatible/pom.xml integration-test
mvn -f promisekitLibrary/pom.xml integration-test
mvn -f rxswiftLibrary/pom.xml integration-test
mvn -f unwrapRequired/pom.xml integration-test
