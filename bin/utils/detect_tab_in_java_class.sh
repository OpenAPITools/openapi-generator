#!/bin/bash

# grep for \t in the generators
grep -RUIl $'\t$' modules/swagger-codegen/src/main/java/io/swagger/codegen/*.java

if [ $? -ne 1 ]; then
    echo "Generators (Java class files) contain tab '/t'. Please remove it and try again."
    exit 1;
fi

