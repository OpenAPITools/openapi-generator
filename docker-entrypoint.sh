#!/usr/bin/env bash

set -euo pipefail

# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/swagger-codegen}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -DloggerPath=conf/log4j.properties"}

cli="${GEN_DIR}/modules/swagger-codegen-cli"
codegen="${cli}/target/swagger-codegen-cli.jar"
cmdsrc="${cli}/src/main/java/io/swagger/codegen/cmd"

pattern="@Command(name = \"$1\""
if expr "x$1" : 'x[a-z][a-z-]*$' > /dev/null && fgrep -qe "$pattern" "$cmdsrc"/*.java; then
    # If ${GEN_DIR} has been mapped elsewhere from default, and that location has not been built
    if [[ ! -f "${codegen}" ]]; then
        (cd "${GEN_DIR}" && exec mvn -am -pl "modules/swagger-codegen-cli" package)
    fi
    command=$1
    shift
    exec java ${JAVA_OPTS} -jar "${codegen}" "${command}" "$@"
else
    exec "$@"
fi
