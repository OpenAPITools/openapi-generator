#!/bin/sh

# Generate clients:
./bin/openapi3/spring-cloud-feign-petstore.sh
./bin/openapi3/spring-cloud-feign-async-petstore.sh
./bin/openapi3/spring-stubs.sh

# Generate spring-mvc servers:
./bin/openapi3/spring-mvc-petstore-server.sh
./bin/openapi3/spring-mvc-petstore-j8-async-server.sh
./bin/openapi3/spring-mvc-petstore-j8-localdatetime.sh

# Generate springboot servers:
./bin/openapi3/spring-delegate.sh
./bin/openapi3/spring-delegate-j8.sh
./bin/openapi3/springboot-petstore-server.sh
./bin/openapi3/springboot-petstore-server-reactive.sh
./bin/openapi3/springboot-petstore-server-beanvalidation.sh
./bin/openapi3/springboot-petstore-server-implicitHeaders.sh
./bin/openapi3/springboot-petstore-server-useOptional.sh
./bin/openapi3/springboot-virtualan-petstore-server.sh