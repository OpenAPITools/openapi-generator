#!/bin/bash
# a simple script to perform a syntax check on php files using "php -l"

for i in $( find . -name "*.php" ); do
    result=`php -l $i | grep "No syntax errors detected"`
    exit_status=$?
    if [ $exit_status -eq 1 ]; then
        echo "Syntax errors with $i"
        exit 1;
    fi
done
