#!/usr/bin/env bash

set -eo pipefail

declare prefix="required-attributes"

declare opts="-DdebugModels -DdebugOperations -Dproject -Dmodels -Dapis -DapiTests=false -DapiDocs=false -DmodelTests=false -DmodelDocs=false $JAVA_OPTS"
declare curdir=$(cd $(dirname "${BASH_SOURCE}") && pwd)

# NOTE: This is sensitive to the location of this script.
declare clijar=${SWAGGER_CODEGEN_CLI_JAR:-$(cd $curdir && cd ../../../../../../../swagger-codegen-cli/target/ && echo $PWD)/swagger-codegen-cli.jar}

exec \java ${opts} -jar ${clijar} generate \
    -i ${prefix}-spec.json -g scala-httpclient-deprecated \
    -o ${prefix}-expected;
