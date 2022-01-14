---
title: Documentation for the crystal Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | crystal | pass this to the generate command after -g |
| generator stability | BETA | |
| generator type | CLIENT | |
| generator language | Crystal | |
| helpTxt | Generates a Crystal client library (beta). | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumUnknownDefaultCase|If the server adds new enum cases, that are unknown by an old spec/client, the client will fail to parse the network response.With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the server sends an enum case that is not known by the client/spec, they can safely fallback to this case.|<dl><dt>**false**</dt><dd>No changes to the enum's are made, this is the default option.</dd><dt>**true**</dt><dd>With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the enum case sent by the server is not known by the client/spec, can safely be decoded to this case.</dd></dl>|false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C#have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|shardAuthor|shard author (only one is supported).| |null|
|shardAuthorEmail|shard author email (only one is supported).| |null|
|shardDescription|shard description.| |This shard maps to a REST API|
|shardHomepage|shard homepage.| |http://org.openapitools|
|shardLicense|shard license.| |unlicense|
|shardName|shard name (e.g. twitter_client| |openapi_client|
|shardVersion|shard version.| |1.0.0|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|
|map|Hash|
|set|Set|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>::File</li>
<li>Array</li>
<li>Boolean</li>
<li>Date</li>
<li>Float</li>
<li>Hash</li>
<li>Integer</li>
<li>Object</li>
<li>String</li>
<li>Time</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>alias</li>
<li>annotation</li>
<li>as</li>
<li>as?</li>
<li>asm</li>
<li>begin</li>
<li>break</li>
<li>case</li>
<li>class</li>
<li>def</li>
<li>do</li>
<li>else</li>
<li>elsif</li>
<li>end</li>
<li>ensure</li>
<li>enum</li>
<li>extend</li>
<li>false</li>
<li>for</li>
<li>fun</li>
<li>if</li>
<li>in</li>
<li>include</li>
<li>instance</li>
<li>is_a?</li>
<li>lib</li>
<li>macro</li>
<li>module</li>
<li>next</li>
<li>nil</li>
<li>nil?</li>
<li>of</li>
<li>out</li>
<li>pointerof</li>
<li>private</li>
<li>protected</li>
<li>require</li>
<li>rescue</li>
<li>responds_to?</li>
<li>return</li>
<li>select</li>
<li>self</li>
<li>sizeof</li>
<li>struct</li>
<li>super</li>
<li>then</li>
<li>true</li>
<li>type</li>
<li>typeof</li>
<li>uninitialized</li>
<li>union</li>
<li>unless</li>
<li>until</li>
<li>verbatim</li>
<li>when</li>
<li>while</li>
<li>with</li>
<li>yield</li>
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
