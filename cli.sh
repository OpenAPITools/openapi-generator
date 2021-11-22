#!/usr/bin/env sh

java -ea                          \
  ${JAVA_OPTS}                    \
  -Xms512M                        \
  -Xmx1024M                       \
  -server                         \
  -jar ./modules/openapi-generator-cli/target/openapi-generator-cli.jar \
  generate -g go -i samples/client/petstore/go/go-petstore/api/openapi.yaml -o samples/client/petstore/go/go-petstore/api/sdk/go --global-property debugModels=true
