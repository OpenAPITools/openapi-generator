#!/bin/sh

# Generate clients:
./bin/spring-cloud-feign-petstore.sh
./bin/spring-cloud-feign-async-petstore.sh
./bin/spring-stubs.sh

# Generate spring-mvc servers:
./bin/spring-mvc-petstore-server.sh
./bin/spring-mvc-petstore-j8-async-server.sh
./bin/spring-mvc-petstore-j8-localdatetime.sh

# Generate springboot servers:
./bin/spring-delegate.sh
./bin/spring-delegate-j8.sh
./bin/springboot-petstore-server.sh
./bin/springboot-petstore-server-reactive.sh
./bin/springboot-petstore-server-beanvalidation.sh
./bin/springboot-petstore-server-implicitHeaders.sh
./bin/springboot-petstore-server-useOptional.sh
./bin/springboot-virtualan-petstore-server.sh