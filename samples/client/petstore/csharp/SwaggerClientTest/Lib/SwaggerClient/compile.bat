@echo off

SET CSCPATH=%SYSTEMROOT%\Microsoft.NET\Framework\v4.0.30319


if not exist ".\nuget.exe" powershell -Command "(new-object System.Net.WebClient).DownloadFile('https://nuget.org/nuget.exe', '.\nuget.exe')"
.\nuget.exe install vendor/packages.config -o vendor

if not exist ".\bin" mkdir bin

copy vendor\Newtonsoft.Json.8.0.2\lib\net45\Newtonsoft.Json.dll bin\Newtonsoft.Json.dll
copy vendor\RestSharp.105.1.0\lib\net45\RestSharp.dll bin\RestSharp.dll

%CSCPATH%\csc /reference:bin\Newtonsoft.Json.dll;bin\RestSharp.dll /target:library /out:bin\IO.Swagger.dll /recurse:src\*.cs /doc:bin\IO.Swagger.xml
