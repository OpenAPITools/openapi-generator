#!/bin/bash
set -exo pipefail

cd "$(dirname ${BASH_SOURCE})"

local_cache="${HOME}/.gradle"

mkdir -p "${local_cache}"

docker run --rm -it \
        -w /gen \
        -e GEN_DIR=/gen \
        -e GRADLE_USER_HOME=/opt/g \
        -u "$(id -u):$(id -g)" \
        -v "${PWD}:/gen" \
        -v "${local_cache}:/opt/g" \
        --entrypoint /gen/docker-entrypoint.sh \
        openjdk:8-jdk-alpine "$@"
