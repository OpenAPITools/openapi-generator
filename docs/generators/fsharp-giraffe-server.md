---
title: Config Options for fsharp-giraffe-server
sidebar_label: fsharp-giraffe-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|licenseUrl|The URL of the license| |http://localhost|
|licenseName|The name of the license| |NoLicense|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageName|F# module name (convention: Title.Case).| |OpenAPI|
|packageVersion|F# package version.| |1.0.0|
|packageGuid|The GUID that will be associated with the C# project| |null|
|sourceFolder|source folder for generated code| |OpenAPI/src|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |false|
|generateBody|Generates method body.| |true|
|buildTarget|Target the build for a program or library.| |program|
