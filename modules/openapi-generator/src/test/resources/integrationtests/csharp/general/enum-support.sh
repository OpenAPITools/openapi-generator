#!/usr/bin/env bash

set -euo pipefail

declare opts="-DdebugModels -Dproject -Dmodels -DmodelTests=false -DmodelDocs=false $JAVA_OPTS"
declare curdir=$(cd $(dirname "${BASH_SOURCE}") && pwd)

# NOTE: This is sensitive to the location of this script.
declare clijar=${SWAGGER_CODEGEN_CLI_JAR:-$(cd $curdir && cd ../../../../../../../swagger-codegen-cli/target/ && echo $PWD)/swagger-codegen-cli.jar}

exec \java ${opts} -jar ${clijar} generate \
    -i enum-support-spec.json -g csharp \
    --additional-properties targetFramework=v4.5 \
    -o enum-support-expected;
