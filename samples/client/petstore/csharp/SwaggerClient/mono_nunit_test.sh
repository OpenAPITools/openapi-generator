#!/usr/bin/env bash
wget -nc https://nuget.org/nuget.exe;
mozroots --import --sync

echo "[INFO] remove bin/Debug/SwaggerClientTest.dll"
rm src/IO.Swagger.Test/bin/Debug/IO.Swagger.Test.dll 2> /dev/null

echo "[INFO] install NUnit runners via NuGet"
wget -nc https://nuget.org/nuget.exe;
mozroots --import --sync
mono nuget.exe install src/IO.Swagger.Test/packages.config -o packages;

echo "[INFO] Copy DLLs to the 'bin' folder"
# for project IO.Swagger
cp packages/Newtonsoft.Json.8.0.2/lib/net45/Newtonsoft.Json.dll src/IO.Swagger/bin/Debug/Newtonsoft.Json.dll
cp packages/RestSharp.105.1.0/lib/net45/RestSharp.dll src/IO.Swagger/bin/Debug/RestSharp.dll
cp packages/NUnit.2.6.3/lib/nunit.framework.dll src/IO.Swagger/bin/Debug/nunit.framework.dll
# for project IO.Swagger.Test
cp packages/Newtonsoft.Json.8.0.2/lib/net45/Newtonsoft.Json.dll src/IO.Swagger.Test/bin/Debug/Newtonsoft.Json.dll
cp packages/RestSharp.105.1.0/lib/net45/RestSharp.dll src/IO.Swagger.Test/bin/Debug/RestSharp.dll
cp packages/NUnit.2.6.3/lib/nunit.framework.dll src/IO.Swagger.Test/bin/Debug/nunit.framework.dll

echo "[INFO] build the solution and run the unit test"
xbuild IO.Swagger.sln && \
    mono ./testrunner/NUnit.Runners.2.6.4/tools/nunit-console.exe src/IO.Swagger.Test/bin/debug/IO.Swagger.Test.dll
