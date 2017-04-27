#!/bin/sh
# update java petstore for all supported http libraries

./bin/java-petstore-jersey1.sh
./bin/java-petstore-jersey2.sh
./bin/java-petstore-feign.sh
./bin/java-petstore-okhttp-gson.sh
./bin/java-petstore-okhttp-gson-parcelable.sh
./bin/java-petstore-retrofit.sh
./bin/java-petstore-retrofit2.sh
./bin/java-petstore-retrofit2rx.sh
./bin/java8-petstore-jersey2.sh
./bin/java-petstore-retrofit2-play24.sh
./bin/java-petstore-jersey2-java6.sh
./bin/java-petstore-resttemplate.sh
./bin/java-petstore-resteasy.sh
