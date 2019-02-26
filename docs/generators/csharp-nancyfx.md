
---
id: generator-opts-server-csharp-nancyfx
title: Config Options for csharp-nancyfx
sidebar_label: csharp-nancyfx
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageVersion|C# package version.| |1.0.0|
|sourceFolder|source folder for generated code| |src|
|interfacePrefix|Prefix interfaces with a community standard or widely accepted prefix.| ||
|packageGuid|The GUID that will be associated with the C# project| |null|
|packageContext|Optionally overrides the PackageContext which determines the namespace (namespace=packageName.packageContext). If not set, packageContext will default to basePath.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|optionalProjectFile|Generate {PackageName}.csproj.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|immutable|Enabled by default. If disabled generates model classes with setters| |true|
|writeModulePath|Enabled by default. If disabled, module paths will not mirror api base path| |true|
|asyncServer|Set to true to enable the generation of async routes/endpoints.| |false|
