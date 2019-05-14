#!/bin/sh
# script to run all generators extending AbstractJavaJAXRSServerCodegen

./bin/jaxrs-petstore-server-datelib-j8.sh
./bin/jaxrs-cxf-cdi-petstore-server.sh
./bin/jaxrs-cxf-petstore-server-annotated-base-path.sh
./bin/jaxrs-cxf-petstore-server-non-spring-application.sh
./bin/jaxrs-cxf-petstore-server.sh
./bin/jaxrs-jersey1-petstore-server.sh
./bin/jaxrs-jersey1-usetags-petstore-server.sh
./bin/jaxrs-petstore-server.sh
./bin/jaxrs-resteasy-eap-java8-petstore-server.sh
./bin/jaxrs-resteasy-eap-joda-petstore-server.sh
./bin/jaxrs-resteasy-eap-petstore-server.sh
./bin/jaxrs-resteasy-joda-petstore-server.sh
./bin/jaxrs-resteasy-petstore-server.sh
./bin/jaxrs-spec-petstore-server-interface.sh
./bin/jaxrs-spec-petstore-server.sh
./bin/jaxrs-usetags-petstore-server.sh

./bin/openapi3/jaxrs-jersey-petstore.sh

echo " Please run ./bin/jaxrs-cxf-petstore-server-test-data.sh manually instead"
