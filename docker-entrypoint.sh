#!/usr/bin/env bash

set -euo pipefail

# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/openapi-generator}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -DloggerPath=conf/log4j.properties"}

cli="${GEN_DIR}/modules/openapi-generator-cli"
codegen="${cli}/target/openapi-generator-cli.jar"
cmdsrc="${cli}/src/main/java/org/openapitools/codegen/cmd"

pattern="@Command(name = \"$1\""
if expr "x$1" : 'x[a-z][a-z-]*$' > /dev/null && fgrep -qe "$pattern" "$cmdsrc"/*.java || expr "$1" = 'help' > /dev/null; then
    # If ${GEN_DIR} has been mapped elsewhere from default, and that location has not been built
    if [[ ! -f "${codegen}" ]]; then
        (cd "${GEN_DIR}" && exec mvn -am -pl "modules/openapi-generator-cli" -Duser.home=$(dirname $MAVEN_CONFIG) package)
    fi
    command=$1
    shift
    exec java ${JAVA_OPTS} -jar "${codegen}" "${command}" "$@"
else
    exec "$@"
fi
