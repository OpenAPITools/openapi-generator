sudo: required
language: java
jdk:
  - openjdk8

cache:
  directories:
  - $HOME/.m2
  - $HOME/.ivy2

services:
  - docker

addons:
  hosts:
    - petstore.swagger.io

before_install:
  # to run petstore server locally via docker
  - docker pull swaggerapi/petstore
  - docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
  - docker ps -a
  # Add bats test framework and cURL for Bash script integration tests
  - sudo add-apt-repository ppa:duggan/bats --yes
  - sudo apt-get update -qq
  - sudo apt-get install -qq bats
  - sudo apt-get install -qq curl

  # show host table to confirm petstore.swagger.io is mapped to localhost
  - cat /etc/hosts

script:
  # fail fast
  - set -e
  # run integration tests defined in maven pom.xml
  - cp pom.xml.bash pom.xml
  - mvn --no-snapshot-updates --batch-mode verify -Psamples
