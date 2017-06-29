#!/bin/bash

# grep for '<<<<<<< HEAD' in swagger codegen
grep -RUIl '<<<<<<< HEAD' modules/swagger-codegen/src

if [ $? -ne 1 ]; then
    echo "modules/swagger-codegen/src contain merge conflicts '<<<<<<< HEAD'. Please remove it and try again."
    exit 1;
fi

# grep for '<<<<<<< HEAD' in the samples
grep -RUIl '<<<<<<< HEAD' samples/

if [ $? -ne 1 ]; then
    echo "samples/ contain merge conflicts '<<<<<<< HEAD'. Please remove it and try again."
    exit 1;
fi
