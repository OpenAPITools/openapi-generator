#!/bin/bash

SCRIPT="$0"
echo "# START SCRIPT: ${SCRIPT}"

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

for GENERATOR in $(java -jar ${executable} list --short --include all | sed -e 's/,/\'$'\n''/g')
do
    ./bin/utils/export_generator.sh ${GENERATOR}
done
