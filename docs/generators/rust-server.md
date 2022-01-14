---
title: Documentation for the rust-server Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | rust-server | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | Rust | |
| helpTxt | Generates a Rust client/server library (beta) using the openapi-generator project. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|packageName|Rust crate name (convention: snake_case).| |openapi_client|
|packageVersion|Rust crate version.| |null|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Vec|
|map|std::collections::HashMap|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>String</li>
<li>bool</li>
<li>char</li>
<li>f32</li>
<li>f64</li>
<li>i16</li>
<li>i32</li>
<li>i64</li>
<li>i8</li>
<li>isize</li>
<li>str</li>
<li>u16</li>
<li>u32</li>
<li>u64</li>
<li>u8</li>
<li>usize</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>alignof</li>
<li>as</li>
<li>become</li>
<li>box</li>
<li>break</li>
<li>const</li>
<li>continue</li>
<li>crate</li>
<li>do</li>
<li>else</li>
<li>enum</li>
<li>extern</li>
<li>false</li>
<li>final</li>
<li>fn</li>
<li>for</li>
<li>if</li>
<li>impl</li>
<li>in</li>
<li>let</li>
<li>loop</li>
<li>macro</li>
<li>match</li>
<li>mod</li>
<li>move</li>
<li>mut</li>
<li>offsetof</li>
<li>override</li>
<li>priv</li>
<li>proc</li>
<li>pub</li>
<li>pure</li>
<li>ref</li>
<li>return</li>
<li>self</li>
<li>sizeof</li>
<li>static</li>
<li>struct</li>
<li>super</li>
<li>trait</li>
<li>true</li>
<li>type</li>
<li>typeof</li>
<li>unsafe</li>
<li>unsized</li>
<li>use</li>
<li>virtual</li>
<li>where</li>
<li>while</li>
<li>yield</li>
</ul>

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✓|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✗|ToolingExtension
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
|Callbacks|✓|OAS3
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
|Polymorphism|✗|OAS2,OAS3
|Union|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✓|OAS2,OAS3
|ApiKey|✓|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✓|OAS3
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
|Custom|✓|OAS2,OAS3
