---
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
|targetFramework|The target .NET framework version.|<dl><dt>**v3.5**</dt><dd>.NET Framework 3.5 compatible</dd><dt>**v4.0**</dt><dd>.NET Framework 4.0 compatible</dd><dt>**v4.5**</dt><dd>.NET Framework 4.5+ compatible</dd><dt>**v5.0**</dt><dd>.NET Standard 1.3 compatible (DEPRECATED. Please use `csharp-netcore` generator instead)</dd><dt>**uwp**</dt><dd>Universal Windows Platform (DEPRECATED. Please use `csharp-netcore` generator instead)</dd><dl>|v4.5|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |PascalCase|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|optionalMethodArgument|C# Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).| |true|
|optionalAssemblyInfo|Generate AssemblyInfo.cs.| |true|
|optionalEmitDefaultValues|Set DataMember's EmitDefaultValue.| |false|
|optionalProjectFile|Generate {PackageName}.csproj.| |true|
|generatePropertyChanged|Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |false|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.| |false|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|netCoreProjectFile|Use the new format (.NET Core) for .NET project files (.csproj).| |false|
|validatable|Generates self-validatable models.| |true|
|useCompareNetObjects|Use KellermanSoftware.CompareNetObjects for deep recursive object comparison. WARNING: this option incurs potential performance impact.| |false|
|caseInsensitiveResponseHeaders|Make API response's headers case-insensitive| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|List|
|list|List|
|map|Dictionary|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>int?</li>
<li>Dictionary</li>
<li>string</li>
<li>bool</li>
<li>DateTimeOffset?</li>
<li>String</li>
<li>Guid</li>
<li>System.IO.Stream</li>
<li>bool?</li>
<li>float</li>
<li>long</li>
<li>DateTime</li>
<li>Int32</li>
<li>float?</li>
<li>DateTime?</li>
<li>List</li>
<li>Boolean</li>
<li>long?</li>
<li>double</li>
<li>Guid?</li>
<li>DateTimeOffset</li>
<li>Double</li>
<li>int</li>
<li>byte[]</li>
<li>Float</li>
<li>Int64</li>
<li>double?</li>
<li>ICollection</li>
<li>Collection</li>
<li>Object</li>
<li>decimal?</li>
<li>decimal</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>extern</li>
<li>do</li>
<li>ushort</li>
<li>float</li>
<li>while</li>
<li>operator</li>
<li>localVarQueryParams</li>
<li>ref</li>
<li>protected</li>
<li>readonly</li>
<li>continue</li>
<li>else</li>
<li>checked</li>
<li>lock</li>
<li>localVarPathParams</li>
<li>catch</li>
<li>Client</li>
<li>if</li>
<li>case</li>
<li>localVarHttpHeaderAccepts</li>
<li>new</li>
<li>using</li>
<li>static</li>
<li>void</li>
<li>localVarPostBody</li>
<li>in</li>
<li>sizeof</li>
<li>localVarResponse</li>
<li>byte</li>
<li>double</li>
<li>sealed</li>
<li>finally</li>
<li>this</li>
<li>unchecked</li>
<li>is</li>
<li>params</li>
<li>enum</li>
<li>explicit</li>
<li>as</li>
<li>null</li>
<li>localVarPath</li>
<li>true</li>
<li>fixed</li>
<li>try</li>
<li>decimal</li>
<li>object</li>
<li>implicit</li>
<li>internal</li>
<li>private</li>
<li>virtual</li>
<li>bool</li>
<li>const</li>
<li>string</li>
<li>for</li>
<li>localVarHttpHeaderAccept</li>
<li>interface</li>
<li>unsafe</li>
<li>long</li>
<li>out</li>
<li>switch</li>
<li>delegate</li>
<li>foreach</li>
<li>default</li>
<li>ulong</li>
<li>goto</li>
<li>localVarHttpContentTypes</li>
<li>localVarHttpContentType</li>
<li>public</li>
<li>localVarStatusCode</li>
<li>stackalloc</li>
<li>parameter</li>
<li>client</li>
<li>override</li>
<li>event</li>
<li>class</li>
<li>typeof</li>
<li>localVarFormParams</li>
<li>break</li>
<li>false</li>
<li>volatile</li>
<li>abstract</li>
<li>uint</li>
<li>int</li>
<li>localVarHeaderParams</li>
<li>throw</li>
<li>char</li>
<li>namespace</li>
<li>sbyte</li>
<li>short</li>
<li>localVarFileParams</li>
<li>return</li>
<li>base</li>
</ul>
