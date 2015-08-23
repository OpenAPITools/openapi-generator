SET CSCPATH=%SYSTEMROOT%\Microsoft.NET\Framework\v4.0.30319
%CSCPATH%\csc /reference:bin/Newtonsoft.Json.dll;bin/RestSharp.dll /target:library /out:bin/.dll /recurse:src\*.cs /doc:bin/.xml

