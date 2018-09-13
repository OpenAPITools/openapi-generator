#!/bin/sh
# update java petstore clients for all supported http libraries

./bin/java-petstore-jersey1.sh
./bin/java-petstore-jersey2.sh
./bin/java-petstore-feign.sh
./bin/java-petstore-feign-10x.sh
./bin/java-petstore-okhttp-gson.sh
./bin/java-petstore-okhttp-gson-parcelable.sh
./bin/java-petstore-retrofit.sh
./bin/java-petstore-retrofit2.sh
./bin/java-petstore-retrofit2rx.sh
./bin/java-petstore-retrofit2rx2.sh
./bin/java8-petstore-jersey2.sh
./bin/java-petstore-retrofit2-play24.sh
./bin/java-petstore-retrofit2-play25.sh
./bin/java-petstore-retrofit2-play26.sh
./bin/java-petstore-jersey2-java6.sh
./bin/java-petstore-resttemplate.sh
./bin/java-petstore-resttemplate-withxml.sh
./bin/java-petstore-webclient.sh
./bin/java-petstore-resteasy.sh
./bin/java-petstore-google-api-client.sh
./bin/java-petstore-rest-assured.sh
./bin/java-petstore-vertx.sh
