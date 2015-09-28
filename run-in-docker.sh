#!/bin/bash
set -e
cd "$(dirname $BASH_SOURCE)"

maven_cache_repo="$HOME/.m2/repository"
myname="$(basename $BASH_SOURCE)"

if [ "$1" = "mvn" ]; then
        cmd="$1"
        shift
        args="$@"
else
        jar="modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"
        
        # Check if project is built
        if [ ! -f "$jar" ]; then
                echo "ERROR File not found: $jar"
                echo "ERROR Did you forget to './$myname mvn package'?"
                exit 1
        fi
        
        cmd="java -jar /gen/$jar"
        args="$@"
fi

mkdir -p "$maven_cache_repo"

set -x

docker run -it \
        -w /gen \
        -v "${PWD}:/gen" \
        -v "${maven_cache_repo}:/root/.m2/repository" \
        maven:3-jdk-7 $cmd $args
