#!/bin/bash
set -exo pipefail

cd "$(dirname ${BASH_SOURCE})"

maven_cache_repo="${HOME}/.m2/repository"

mkdir -p "${maven_cache_repo}"

docker run --rm -it \
        -w /gen \
        -e GEN_DIR=/gen \
        -e MAVEN_CONFIG=/var/maven/.m2 \
        -u "$(id -u):$(id -u)" \
        -v "${PWD}:/gen" \
        -v "${maven_cache_repo}:/var/maven/.m2/repository" \
        --entrypoint /gen/docker-entrypoint.sh \
        maven:3-jdk-7 "$@"
