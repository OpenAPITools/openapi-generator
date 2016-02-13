@echo off
SET CSCPATH=%SYSTEMROOT%\Microsoft.NET\Framework\v4.0.30319

if not exist ".\nuget.exe" powershell -Command "(new-object System.Net.WebClient).DownloadFile('https://nuget.org/nuget.exe', '.\nuget.exe')"
.\nuget.exe install vendor/packages.config -o vendor

cp vendor/Newtonsoft.Json.8.0.2/lib/net45/Newtonsoft.Json.dll bin/Newtonsoft.Json.dll
cp vendor/RestSharp.105.2.3/lib/net45/RestSharp.dll bin/RestSharp.dll

%CSCPATH%\csc /reference:bin/Newtonsoft.Json.dll;bin/RestSharp.dll /target:library /out:bin/IO.Swagger.dll /recurse:src\*.cs /doc:bin/IO.Swagger.xml
