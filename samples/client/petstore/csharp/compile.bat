SET CSCPATH=C:\windows\microsoft.net\Framework\v4.0.30319
%CSCPATH%\csc /reference:bin/Newtonsoft.Json.dll /target:library /out:bin/Com.Wordnik.Petstore.dll /recurse:src\*.cs
