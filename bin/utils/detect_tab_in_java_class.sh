#!/bin/bash

# grep for \t in the generators
RESULT=`find modules -name "*.java" | xargs grep $'\t'`

echo -e "$RESULT"

if [ "$RESULT" != "" ]; then
    echo "Java files contain tab '\\t'. Please remove it and try again."
    exit 1;
fi

