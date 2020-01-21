---
title: Config Options for csharp-nancyfx
sidebar_label: csharp-nancyfx
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|asyncServer|Set to true to enable the generation of async routes/endpoints.| |false|
|immutable|Enabled by default. If disabled generates model classes with setters| |true|
|interfacePrefix|Prefix interfaces with a community standard or widely accepted prefix.| ||
|optionalProjectFile|Generate {PackageName}.csproj.| |true|
|packageContext|Optionally overrides the PackageContext which determines the namespace (namespace=packageName.packageContext). If not set, packageContext will default to basePath.| |null|
|packageGuid|The GUID that will be associated with the C# project| |null|
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageVersion|C# package version.| |1.0.0|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |src|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|writeModulePath|Enabled by default. If disabled, module paths will not mirror api base path| |true|

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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Boolean</li>
<li>Collection</li>
<li>DateTime</li>
<li>DateTime?</li>
<li>DateTimeOffset</li>
<li>DateTimeOffset?</li>
<li>Dictionary</li>
<li>Double</li>
<li>Float</li>
<li>Guid</li>
<li>Guid?</li>
<li>ICollection</li>
<li>Int32</li>
<li>Int64</li>
<li>List</li>
<li>LocalDate?</li>
<li>LocalTime?</li>
<li>Object</li>
<li>String</li>
<li>System.IO.Stream</li>
<li>ZonedDateTime?</li>
<li>bool</li>
<li>bool?</li>
<li>byte[]</li>
<li>decimal</li>
<li>decimal?</li>
<li>double</li>
<li>double?</li>
<li>float</li>
<li>float?</li>
<li>int</li>
<li>int?</li>
<li>long</li>
<li>long?</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>async</li>
<li>await</li>
<li>dynamic</li>
<li>var</li>
<li>yield</li>
</ul>
