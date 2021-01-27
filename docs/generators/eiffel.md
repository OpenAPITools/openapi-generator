---
title: Config Options for eiffel
sidebar_label: eiffel
---

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|packageName|Eiffel Cluster name (convention: lowercase).| |openapi|
|packageVersion|Eiffel package version.| |1.0.0|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|File|FILE|
|List|LIST|
|Map|STRING_TABLE|
|Set|SET|
|file|FILE|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|ARRAYED_LIST|
|list|ARRAYED_LIST|
|map|STRING_TABLE|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>BOOLEAN</li>
<li>INTEGER_16</li>
<li>INTEGER_32</li>
<li>INTEGER_64</li>
<li>INTEGER_8</li>
<li>NATURAL_16</li>
<li>NATURAL_32</li>
<li>NATURAL_64</li>
<li>NATURAL_8</li>
<li>REAL_32</li>
<li>REAL_64</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>across</li>
<li>agent</li>
<li>alias</li>
<li>all</li>
<li>and</li>
<li>as</li>
<li>assign</li>
<li>attached</li>
<li>attribute</li>
<li>check</li>
<li>class</li>
<li>convert</li>
<li>create</li>
<li>current</li>
<li>debug</li>
<li>deferred</li>
<li>detachable</li>
<li>do</li>
<li>else</li>
<li>elseif</li>
<li>end</li>
<li>ensure</li>
<li>expanded</li>
<li>export</li>
<li>external</li>
<li>false</li>
<li>feature</li>
<li>from</li>
<li>frozen</li>
<li>if</li>
<li>implies</li>
<li>inherit</li>
<li>inspect</li>
<li>invariant</li>
<li>like</li>
<li>local</li>
<li>loop</li>
<li>not</li>
<li>note</li>
<li>obsolete</li>
<li>old</li>
<li>once</li>
<li>only</li>
<li>or</li>
<li>precursor</li>
<li>redefine</li>
<li>rename</li>
<li>require</li>
<li>rescue</li>
<li>result</li>
<li>retry</li>
<li>select</li>
<li>separate</li>
<li>then</li>
<li>true</li>
<li>tuple</li>
<li>undefine</li>
<li>until</li>
<li>variant</li>
<li>void</li>
<li>when</li>
<li>xor</li>
</ul>

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✓|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✓|ToolingExtension
|MockServer|✗|ToolingExtension

### Data Type Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Custom|✗|OAS2,OAS3
|Int32|✓|OAS2,OAS3
|Int64|✓|OAS2,OAS3
|Float|✓|OAS2,OAS3
|Double|✓|OAS2,OAS3
|Decimal|✓|ToolingExtension
|String|✓|OAS2,OAS3
|Byte|✓|OAS2,OAS3
|Binary|✓|OAS2,OAS3
|Boolean|✓|OAS2,OAS3
|Date|✓|OAS2,OAS3
|DateTime|✓|OAS2,OAS3
|Password|✓|OAS2,OAS3
|File|✓|OAS2
|Array|✓|OAS2,OAS3
|Maps|✓|ToolingExtension
|CollectionFormat|✓|OAS2
|CollectionFormatMulti|✓|OAS2
|Enum|✓|OAS2,OAS3
|ArrayOfEnum|✓|ToolingExtension
|ArrayOfModel|✓|ToolingExtension
|ArrayOfCollectionOfPrimitives|✓|ToolingExtension
|ArrayOfCollectionOfModel|✓|ToolingExtension
|ArrayOfCollectionOfEnum|✓|ToolingExtension
|MapOfEnum|✓|ToolingExtension
|MapOfModel|✓|ToolingExtension
|MapOfCollectionOfPrimitives|✓|ToolingExtension
|MapOfCollectionOfModel|✓|ToolingExtension
|MapOfCollectionOfEnum|✓|ToolingExtension

### Documentation Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Readme|✓|ToolingExtension
|Model|✓|ToolingExtension
|Api|✓|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|✓|OAS2,OAS3
|BasePath|✓|OAS2,OAS3
|Info|✓|OAS2,OAS3
|Schemes|✗|OAS2,OAS3
|PartialSchemes|✓|OAS2,OAS3
|Consumes|✓|OAS2
|Produces|✓|OAS2
|ExternalDocumentation|✓|OAS2,OAS3
|Examples|✓|OAS2,OAS3
|XMLStructureDefinitions|✗|OAS2,OAS3
|MultiServer|✗|OAS3
|ParameterizedServer|✗|OAS3
|ParameterStyling|✗|OAS3
|Callbacks|✗|OAS3
|LinkObjects|✗|OAS3

### Parameter Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Path|✓|OAS2,OAS3
|Query|✓|OAS2,OAS3
|Header|✓|OAS2,OAS3
|Body|✓|OAS2
|FormUnencoded|✓|OAS2
|FormMultipart|✓|OAS2
|Cookie|✗|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✓|OAS2,OAS3
|Union|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✓|OAS2,OAS3
|ApiKey|✓|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✓|OAS2,OAS3
|OAuth2_Password|✗|OAS2,OAS3
|OAuth2_ClientCredentials|✗|OAS2,OAS3
|OAuth2_AuthorizationCode|✗|OAS2,OAS3

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
