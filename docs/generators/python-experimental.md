---
title: Documentation for the python-experimental Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | python-experimental | pass this to the generate command after -g |
| generator stability | EXPERIMENTAL | |
| generator type | CLIENT | |
| generator language | Python | |
| generator language version | >=3.9 | |
| generator default templating engine | handlebars | |
| helpTxt | Generates a Python client library<br /><br />Features in this generator:<br />- type hints on endpoints and model creation<br />- model parameter names use the spec defined keys and cases<br />- robust composition (oneOf/anyOf/allOf) where paload data is stored in one instance only<br />- endpoint parameter names use the spec defined keys and cases<br />- inline schemas are supported at any location including composition<br />- multiple content types supported in request body and response bodies<br />- run time type checking<br />- Sending/receiving decimals as strings supported with type:string format: number -> DecimalSchema<br />- quicker load time for python modules (a single endpoint can be imported and used without loading others)<br />- all instances of schemas dynamically inherit from all matching schemas so one can use isinstance to check if validation passed<br />- composed schemas with type constraints supported (type:object + oneOf/anyOf/allOf)<br />- schemas are not coerced/cast. For example string + date are both stored as string, and there is a date accessor<br />    - Exceptions: int/float is stored as Decimal, When receiving data from headers it will start as str and may need to be cast for example to int | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|generateSourceCodeOnly|Specifies that only a library source code is to be generated.| |false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|library template (sub-template) to use: asyncio, tornado, urllib3| |urllib3|
|packageName|python package name (convention: snake_case).| |openapi_client|
|packageUrl|python package URL.| |null|
|packageVersion|python package version.| |1.0.0|
|projectName|python project name in setup.py (e.g. petstore-api).| |null|
|recursionLimit|Set the recursion limit. If not set, use the system default value.| |null|
|useNose|use the nose test framework| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|map|dict|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>bool</li>
<li>bytes</li>
<li>date</li>
<li>datetime</li>
<li>dict</li>
<li>file</li>
<li>file_type</li>
<li>float</li>
<li>int</li>
<li>list</li>
<li>none_type</li>
<li>object</li>
<li>str</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>all_params</li>
<li>and</li>
<li>as</li>
<li>assert</li>
<li>async</li>
<li>auth_settings</li>
<li>await</li>
<li>body_params</li>
<li>bool</li>
<li>break</li>
<li>class</li>
<li>continue</li>
<li>def</li>
<li>del</li>
<li>dict</li>
<li>elif</li>
<li>else</li>
<li>except</li>
<li>exec</li>
<li>false</li>
<li>file_type</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>form_params</li>
<li>from</li>
<li>frozendict</li>
<li>global</li>
<li>header_params</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>int</li>
<li>is</li>
<li>lambda</li>
<li>list</li>
<li>local_var_files</li>
<li>none</li>
<li>none_type</li>
<li>nonlocal</li>
<li>not</li>
<li>or</li>
<li>pass</li>
<li>path_params</li>
<li>print</li>
<li>property</li>
<li>query_params</li>
<li>raise</li>
<li>resource_path</li>
<li>return</li>
<li>self</li>
<li>str</li>
<li>true</li>
<li>try</li>
<li>tuple</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✗|ToolingExtension
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
|ParameterizedServer|✓|OAS3
|ParameterStyling|✓|OAS3
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
|Union|✓|OAS3

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
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✓|OAS2,OAS3
