#!/usr/bin/env bash
# script to run all generators extending AbstractJavaJAXRSServerCodegen

source ./bin/jaxrs-petstore-server-datelib-j8.sh 1>/dev/null
source ./bin/jaxrs-cxf-cdi-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-cxf-petstore-server-annotated-base-path.sh 1>/dev/null
source ./bin/jaxrs-cxf-petstore-server-non-spring-application.sh 1>/dev/null
source ./bin/jaxrs-cxf-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-jersey1-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-jersey1-usetags-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-java8-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-joda-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-joda-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-spec-petstore-server-interface.sh 1>/dev/null
source ./bin/jaxrs-spec-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-usetags-petstore-server.sh 1>/dev/null

source ./bin/openapi3/jaxrs-jersey-petstore.sh 1>/dev/null

echo " Please run ./bin/jaxrs-cxf-petstore-server-test-data.sh manually instead"
