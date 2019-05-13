#/bin/bash

set -e

mvn -f default/SwaggerClientTests/pom.xml integration-test
mvn -f promisekit/SwaggerClientTests/pom.xml integration-test
mvn -f rxswift/SwaggerClientTests/pom.xml integration-test
