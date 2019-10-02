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
