#!bin/bash 
grep --include=\*.{yaml,json} -lrnw $1 -e "openapi\":.*\"[0-9].[0-9].[0-9]" |\
xargs -I{} java -jar /opt/openapi-generator/modules/openapi-generator-cli/target/openapi-generator-cli.jar validate -i {}