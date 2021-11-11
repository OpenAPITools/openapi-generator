#!/usr/bin/env bash

SCRIPT="$0"

if [[ "$1" != "" ]]; then
    NAME="$1"
    echo "# START SCRIPT: ${SCRIPT} ${NAME}"
else
    echo "Missing argument to ${SCRIPT}."
    echo "    Usage: ${SCRIPT} generator-name"
    echo "    Example: ${SCRIPT} groovy"
    exit 1;
fi

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

java -jar ${executable} config-help -g "${NAME}" --full-details --named-header --format markdown --markdown-header -o "docs/generators/${NAME}.md"
