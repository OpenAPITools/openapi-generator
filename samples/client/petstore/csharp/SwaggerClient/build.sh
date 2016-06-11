#!/usr/bin/env bash
frameworkVersion=net45
netfx=${frameworkVersion#net}

echo "[INFO] Target framework: ${frameworkVersion}"

echo "[INFO] Download nuget and packages"
wget -nc https://nuget.org/nuget.exe;
mozroots --import --sync
mono nuget.exe install src/IO.Swagger/packages.config -o packages;
mkdir -p bin;

echo "[INFO] Copy DLLs to the 'bin' folder"
cp packages/Newtonsoft.Json.8.0.2/lib/net45/Newtonsoft.Json.dll bin/Newtonsoft.Json.dll;
cp packages/RestSharp.105.1.0/lib/net45/RestSharp.dll bin/RestSharp.dll;

echo "[INFO] Run 'mcs' to build bin/IO.Swagger.dll"
mcs -sdk:${netfx} -r:bin/Newtonsoft.Json.dll,\
bin/RestSharp.dll,\
System.Runtime.Serialization.dll \
-target:library \
-out:bin/IO.Swagger.dll \
-recurse:'src/IO.Swagger/*.cs' \
-doc:bin/IO.Swagger.xml \
-platform:anycpu
