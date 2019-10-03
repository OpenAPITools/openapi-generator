#!/bin/bash

# change the working directory to the directory the script is located in.
# This snipped does not work if this file is sourced or called over $PATH
scriptDir=${0%/*}
if [[ "$scriptDir" != "$0" ]]; then
    cd $scriptDir
fi
scriptDir=$(pwd)
projectDir=$scriptDir/../../
cd $projectDir

# C# requires a test png to run the appveyor tests.
cp CI/samples.ci/test-resources/linux-logo.png samples/client/petstore/csharp-netcore/OpenAPIClient/src/Org.OpenAPITools.Test/linux-logo.png
cp CI/samples.ci/test-resources/linux-logo.png samples/client/petstore/csharp-netcore/OpenAPIClientCore/src/Org.OpenAPITools.Test/linux-logo.png
cp CI/samples.ci/test-resources/linux-logo.png samples/client/petstore/csharp/OpenAPIClient/src/Org.OpenAPITools.Test/linux-logo.png

find CI/samples.ci/client/ -type d | sed -n 's|^CI/samples.ci/||p' | xargs -i mkdir -p samples/{}
find CI/samples.ci/server/ -type d | sed -n 's|^CI/samples.ci/||p' | xargs -i mkdir -p samples/{}
find CI/samples.ci/openapi3/ -type d | sed -n 's|^CI/samples.ci/||p' | xargs -i mkdir -p samples/{}

find CI/samples.ci/client/ -type f | sed -n 's|^CI/samples.ci/||p' | xargs -i ln -srf CI/samples.ci/{} samples/{}
find CI/samples.ci/server/ -type f | sed -n 's|^CI/samples.ci/||p' | xargs -i ln -srf CI/samples.ci/{} samples/{}
find CI/samples.ci/openapi3/ -type f | sed -n 's|^CI/samples.ci/||p' | xargs -i ln -srf CI/samples.ci/{} samples/{}
