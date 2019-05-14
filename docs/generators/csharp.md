
---
id: generator-opts-client-csharp
title: Config Options for csharp
sidebar_label: csharp
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageVersion|C# package version.| |1.0.0|
|sourceFolder|source folder for generated code| |src|
|packageGuid|The GUID that will be associated with the C# project| |null|
|interfacePrefix|Prefix interfaces with a community standard or widely accepted prefix.| |I|
|targetFramework|The target .NET framework version.|<dl><dt>**v3.5**</dt><dd>.NET Framework 3.5 compatible</dd><dt>**v4.0**</dt><dd>.NET Framework 4.0 compatible</dd><dt>**v4.5**</dt><dd>.NET Framework 4.5+ compatible</dd><dt>**v5.0**</dt><dd>.NET Standard 1.3 compatible</dd><dt>**uwp**</dt><dd>Universal Windows Platform (IMPORTANT: this will be decommissioned and replaced by v5.0)</dd><dl>|v4.5|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |PascalCase|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|optionalMethodArgument|C# Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).| |true|
|optionalAssemblyInfo|Generate AssemblyInfo.cs.| |true|
|optionalProjectFile|Generate {PackageName}.csproj.| |true|
|generatePropertyChanged|Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |false|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.| |false|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|netCoreProjectFile|Use the new format (.NET Core) for .NET project files (.csproj).| |false|
|validatable|Generates self-validatable models.| |true|
|useCompareNetObjects|Use KellermanSoftware.CompareNetObjects for deep recursive object comparison. WARNING: this option incurs potential performance impact.| |false|
