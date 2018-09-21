#!/bin/bash

SCRIPT="$0"
if [[ "$1" != "" ]]; then
    NAME="$1"
else
    echo "Missing argument. Usage e.g.: ./bin/utils/export-generator.sh jaxrs-jersey"
    exit 1;
fi
echo "# START SCRIPT: $SCRIPT $NAME"

executable="./modules/openapi-generator-cli/target/openapi-generator-cli.jar"

java -jar $executable config-help -g $NAME | sed -e 's/CONFIG OPTIONS/CONFIG OPTIONS for \'$NAME'\'$'\n''/g' > ./docs/generators/$NAME.md

echo "Back to the [generators list](README.md)" >> ./docs/generators/$NAME.md

echo "Writen file: ./docs/generators/$NAME.md"
