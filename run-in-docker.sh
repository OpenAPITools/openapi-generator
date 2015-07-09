#!/bin/bash
set -e
cd "$(dirname $BASH_SOURCE)"

maven_cache_repo="$HOME/.m2/repository"

if [ "$1" = "mvn" ]; then
        cmd="$1"
        shift
        args="$@"
else
        cmd="java -jar /gen/modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"
        args="$@"
fi

mkdir -p "$maven_cache_repo"

set -x

docker run -it \
        -w /gen \
        -v "${PWD}:/gen" \
        -v "${maven_cache_repo}:/root/.m2/repository" \
        maven:3-jdk-7 $cmd $args
