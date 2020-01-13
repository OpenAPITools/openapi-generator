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
|enumNameSuffix|Suffix that will be appended to all enum names.| |Enum|
|enumValueSuffix|Suffix that will be appended to all enum values.| |Enum|
|classModifier|Class Modifier can be empty, abstract| ||
|operationModifier|Operation Modifier can be virtual, abstract or partial| |virtual|
|buildTarget|Target to build an application or library| |program|
|generateBody|Generates method body.| |true|
|operationIsAsync|Set methods to async or sync (default).| |false|
|operationResultTask|Set methods result to Task&lt;&gt;.| |false|
|modelClassModifier|Model Class Modifier can be nothing or partial| |partial|

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
<li>ushort</li>
<li>localVarQueryParams</li>
<li>protected</li>
<li>readonly</li>
<li>else</li>
<li>lock</li>
<li>localVarPathParams</li>
<li>catch</li>
<li>if</li>
<li>case</li>
<li>localVarHttpHeaderAccepts</li>
<li>using</li>
<li>localVarPostBody</li>
<li>in</li>
<li>byte</li>
<li>double</li>
<li>var</li>
<li>is</li>
<li>params</li>
<li>enum</li>
<li>explicit</li>
<li>as</li>
<li>object</li>
<li>implicit</li>
<li>internal</li>
<li>localVarHttpHeaderAccept</li>
<li>unsafe</li>
<li>long</li>
<li>out</li>
<li>delegate</li>
<li>default</li>
<li>goto</li>
<li>localVarHttpContentTypes</li>
<li>localVarHttpContentType</li>
<li>yield</li>
<li>override</li>
<li>event</li>
<li>typeof</li>
<li>break</li>
<li>abstract</li>
<li>uint</li>
<li>throw</li>
<li>char</li>
<li>sbyte</li>
<li>localVarFileParams</li>
<li>return</li>
<li>extern</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>operator</li>
<li>ref</li>
<li>continue</li>
<li>checked</li>
<li>dynamic</li>
<li>Client</li>
<li>new</li>
<li>static</li>
<li>void</li>
<li>sizeof</li>
<li>localVarResponse</li>
<li>sealed</li>
<li>finally</li>
<li>this</li>
<li>unchecked</li>
<li>null</li>
<li>localVarPath</li>
<li>true</li>
<li>fixed</li>
<li>try</li>
<li>decimal</li>
<li>private</li>
<li>virtual</li>
<li>bool</li>
<li>const</li>
<li>string</li>
<li>for</li>
<li>interface</li>
<li>switch</li>
<li>foreach</li>
<li>ulong</li>
<li>public</li>
<li>localVarStatusCode</li>
<li>stackalloc</li>
<li>parameter</li>
<li>await</li>
<li>client</li>
<li>class</li>
<li>localVarFormParams</li>
<li>false</li>
<li>volatile</li>
<li>int</li>
<li>async</li>
<li>localVarHeaderParams</li>
<li>namespace</li>
<li>short</li>
<li>base</li>
</ul>
