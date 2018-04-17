#!/bin/bash

# grep for \r in the templates
grep -RUIl $'\r$' modules/openapi-generator/src/main/resources/*

if [ $? -ne 1 ]; then
    echo "Templates contain carriage return '/r'. Please remove it and try again."
    exit 1;
fi


# grep for \r in the generators
grep -RUIl $'\r$' modules/openapi-generator/src/main/java/org/openapitools/codegen/*.java

if [ $? -ne 1 ]; then
    echo "Generators contain carriage return '/r'. Please remove it and try again."
    exit 1;
fi
