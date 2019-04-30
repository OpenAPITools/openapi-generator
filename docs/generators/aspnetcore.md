
---
id: generator-opts-server-aspnetcore
title: Config Options for aspnetcore
sidebar_label: aspnetcore
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|licenseUrl|The URL of the license| |http://localhost|
|licenseName|The name of the license| |NoLicense|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageVersion|C# package version.| |1.0.0|
|packageGuid|The GUID that will be associated with the C# project| |null|
|sourceFolder|source folder for generated code| |src|
|compatibilityVersion|ASP.Net Core CompatibilityVersion| |Version_2_1|
|aspnetCoreVersion|ASP.NET Core version: 2.2 (default), 2.1, 2.0 (deprecated)| |2.2|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |true|
|isLibrary|Is the build a library| |false|
|classModifier|Class Modifier can be empty, abstract| ||
|operationModifier|Operation Modifier can be virtual, abstract or partial| |virtual|
|buildTarget|Target to build an application or library| |program|
|generateBody|Generates method body.| |true|
|operationIsAsync|Set methods to async or sync.| |false|
|operationResultTask|Set methods result to Task&lt;&gt;.| |false|
|modelClassModifier|Model Class Modifier can be nothing or partial| |partial|
