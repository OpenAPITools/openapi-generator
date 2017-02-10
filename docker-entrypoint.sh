#!/usr/bin/env bash

set -euo pipefail

# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/swagger-codegen}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -DloggerPath=conf/log4j.properties"}

codegen="${GEN_DIR}/modules/swagger-codegen-cli/target/swagger-codegen-cli.jar"

case "$1" in
    generate|help|langs|meta|config-help)
        # If ${GEN_DIR} has been mapped elsewhere from default, and that location has not been built
        if [[ ! -f "${codegen}" ]]; then
            (cd ${GEN_DIR} && exec mvn -am -pl "modules/swagger-codegen-cli" package)
        fi
        command=$1
        shift
        exec java ${JAVA_OPTS} -jar ${codegen} ${command} "$@"
        ;;
    *)  # Any other commands, e.g. docker run imagename ls -la or docker run -it imagename /bin/bash
        exec "$@"
        ;;
esac
