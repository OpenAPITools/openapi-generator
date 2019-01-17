#!/bin/sh

SCRIPT="$0"
echo "# START SCRIPT: $SCRIPT"

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

java -jar $executable list | sed -e 's/\([A-Z]*\) generators:/* \1 generators:/g' -e 's/- \([a-z0-9\-]*\)/- [\1]\(\1.md\)/g' > docs/generators/README.md
