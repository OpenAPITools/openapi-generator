---
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
|compatibilityVersion|ASP.Net Core CompatibilityVersion| |Version_2_2|
|aspnetCoreVersion|ASP.NET Core version: 3.0 (preview4 only), 2.2, 2.1, 2.0 (deprecated)| |2.2|
|swashbuckleVersion|Swashbucke version: 3.0.0, 4.0.0| |3.0.0|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|useSwashbuckle|Uses the Swashbuckle.AspNetCore NuGet package for documentation.| |true|
|isLibrary|Is the build a library| |false|
|useFrameworkReference|Use frameworkReference for ASP.NET Core 3.0+ and  PackageReference  ASP.NET Core 2.2 or earlier.| |false|
|useNewtonsoft|Uses the Newtonsoft JSON library.| |true|
|newtonsoftVersion|Version for Microsoft.AspNetCore.Mvc.NewtonsoftJson for ASP.NET Core 3.0+| |3.0.0-preview5-19227-01|
|useDefaultRouting|Use default routing for the  ASP.NET Core version. For 3.0 turn off default because it is not yet supported.| |true|
|classModifier|Class Modifier can be empty, abstract| ||
|operationModifier|Operation Modifier can be virtual, abstract or partial| |virtual|
|buildTarget|Target to build an application or library| |program|
|generateBody|Generates method body.| |true|
|operationIsAsync|Set methods to async or sync (default).| |false|
|operationResultTask|Set methods result to Task&lt;&gt;.| |false|
|modelClassModifier|Model Class Modifier can be nothing or partial| |partial|
