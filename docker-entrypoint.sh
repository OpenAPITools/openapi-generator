#!/usr/bin/env bash

set -euo pipefail

# GEN_DIR allows to share the entrypoint between Dockerfile and run-in-docker.sh (backward compatible)
GEN_DIR=${GEN_DIR:-/opt/openapi-generator}
JAVA_OPTS=${JAVA_OPTS:-"-Xmx1024M -DloggerPath=conf/log4j.properties"}

cli="${GEN_DIR}/modules/openapi-generator-cli"
codegen="${cli}/target/openapi-generator-cli.jar"

# We code in a list of commands here as source processing is potentially buggy (requires undocumented conventional use of annotations).
# A list of known commands helps us determine if we should compile CLI. There's an edge-case where a new command not added to this
# list won't be considered a "real" command. We can get around that a bit by checking CLI completions beforehand if it exists.
commands="config-help,generate,batch,help,list,meta,validate,version"

if [ $# == 0 ]; then
	echo "No command specified. Available commands:"
	for i in $(echo $commands | sed "s/,/ /g")
	do
		echo "  $i"
	done
	exit
fi

# if CLI jar exists, check $1 against completions available in the CLI
if [[ -f "${codegen}" && -n "$(java ${JAVA_OPTS} -jar "${codegen}" completion | grep "^$1\$" )" ]]; then
    command=$1
    shift
    exec java ${JAVA_OPTS} -jar "${codegen}" "${command}" "$@"
elif [[ -n "$(echo $commands | tr ',' '\n' | grep "^$1\$" )" ]]; then
    # If CLI jar does not exist, and $1 is a known CLI command, build the CLI jar and run that command.
    if [[ ! -f "${codegen}" ]]; then
        (cd "${GEN_DIR}" && exec mvn -am -pl "modules/openapi-generator-cli" -Duser.home=$(dirname $MAVEN_CONFIG) package)
    fi
    command=$1
    shift
    exec java ${JAVA_OPTS} -jar "${codegen}" "${command}" "$@"
else
    # Pass args as linux commands. This allows us to do something like: docker run -it (-e…, -v…) image ls -la
    exec "$@"
fi
