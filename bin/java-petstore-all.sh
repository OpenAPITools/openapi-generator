#!/bin/sh
# update java petstore for all supported http libraries

./bin/java-petstore.sh
./bin/java-petstore-jersey2.sh
./bin/java-petstore-feign.sh
./bin/java-petstore-okhttp-gson.sh
./bin/java-petstore-retrofit.sh
./bin/java-petstore-retrofit2.sh
