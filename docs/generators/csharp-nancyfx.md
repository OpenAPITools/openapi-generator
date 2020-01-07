---
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
<li>LocalDate?</li>
<li>DateTimeOffset?</li>
<li>ZonedDateTime?</li>
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
<li>LocalTime?</li>
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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>async</li>
<li>var</li>
<li>yield</li>
<li>await</li>
<li>dynamic</li>
</ul>
