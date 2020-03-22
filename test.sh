#!/bin/bash
mvn package -DskipTests=true 
./bin/rust-petstore.sh && ./bin/openapi3/rust-petstore.sh 
cd samples/client/petstore/rust || exit 1
mvn integration-test -rf :RustPetstoreClientTests