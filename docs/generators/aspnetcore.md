
---
id: generator-opts-server-aspnetcore
title: Config Options for aspnetcore
sidebar_label: aspnetcore
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|licenseName|Package License Name| |NoLicense|
|licenseUrl|Package License Url| |http://nolicense.org|
|packageAuthors|Package Authors| |OpenAPI|
|packageCopyright|Package Copyright| |Copyright 2019|
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageTtile|Package Title| |OpenAPI Library|
|packageVersion|C# package version.| |1.0.0|
|packageGuid|The GUID that will be associated with the C# project| |null|
|sourceFolder|source folder for generated code| |src|
|aspnetCoreVersion|ASP.NET Core version: 2.1 (default), 2.0 (deprecated)| |2.1|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |true|
|generateWWWRoot|Generate the wwwroot directory.| |true|
|classModifier|Add a class modifier to the generated class. If the modfier is "abstract" then the methods are made abstract too.| |none|
|operationModifier|Override the defaultmethod type for the generate method. If the modfier is "abstract" then the method body is not generated..| |virtual|
|generateBody|Generate method body| |true|
