IMAGE_NAME := openapi-generator

DOCKER_REPO := confluent-docker.jfrog.io
include ./mk-include/cc-begin.mk
include ./mk-include/cc-semver.mk
include ./mk-include/cc-docker.mk
include ./mk-include/cc-end.mk
