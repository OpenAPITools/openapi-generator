#!/usr/bin/env bash

source ./bin/java-play-framework-petstore-server.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-controller-only.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-no-bean-validation.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-no-exception-handling.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-no-interface.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-no-swagger-ui.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-no-wrap-calls.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-fake-endpoints.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-api-package-override.sh 1>/dev/null
source ./bin/java-play-framework-petstore-server-async.sh 1>/dev/null
