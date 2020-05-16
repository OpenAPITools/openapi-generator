#!/bin/bash

source ./bin/java-petstore-resteasy.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-java8-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-joda-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-eap-joda-petstore-server.sh 1>/dev/null
source ./bin/jaxrs-resteasy-petstore-server.sh* 1>/dev/null
