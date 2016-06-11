#!/usr/bin/env bash
wget -nc https://nuget.org/nuget.exe
mozroots --import --sync

echo "[INFO] remove bin/Debug/SwaggerClientTest.dll"
rm src/IO.Swagger.Test/bin/Debug/IO.Swagger.Test.dll 2> /dev/null

echo "[INFO] install NUnit runners via NuGet"
wget -nc https://nuget.org/nuget.exe
mozroots --import --sync
mono nuget.exe install src/IO.Swagger.Test/packages.config -o packages

echo "[INFO] Install NUnit runners via NuGet"
mono nuget.exe install NUnit.Runners -Version 3.2.1 -OutputDirectory packages 

echo "[INFO] Build the solution and run the unit test"
xbuild IO.Swagger.sln && \
    mono ./packages/NUnit.ConsoleRunner.3.2.1/tools/nunit3-console.exe src/IO.Swagger.Test/bin/debug/IO.Swagger.Test.dll
