---
title: Documentation for the dart-dio Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | dart-dio | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | Dart | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Dart Dio client library. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|dateLibrary|Specify Date library|<dl><dt>**core**</dt><dd>[DEFAULT] Dart core library (DateTime)</dd><dt>**timemachine**</dt><dd>Time Machine is date and time library for Flutter, Web, and Server with support for timezones, calendars, cultures, formatting and parsing.</dd></dl>|core|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumUnknownDefaultCase|If the server adds new enum cases, that are unknown by an old spec/client, the client will fail to parse the network response. With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the server sends an enum case that is not known by the client/spec, they can safely fallback to this case.|<dl><dt>**false**</dt><dd>No changes to the enums are made, this is the default option.</dd><dt>**true**</dt><dd>With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the enum case sent by the server is not known by the client/spec, can safely be decoded to this case.</dd></dl>|false|
|equalityCheckMethod|Specify equality check method. Takes effect only in case if serializationLibrary is json_serializable.|<dl><dt>**default**</dt><dd>[DEFAULT] Built in hash code generation method</dd><dt>**equatable**</dt><dd>Uses equatable library for equality checking</dd></dl>|default|
|finalProperties|Whether properties are marked as final when using Json Serializable for serialization| |true|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C# have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|patchOnly|Only apply Optional&lt;T&gt; to PATCH operation request bodies (requires useOptional=true)| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|pubAuthor|Author name in generated pubspec| |Author|
|pubAuthorEmail|Email address of the author in generated pubspec| |author@homepage|
|pubDescription|Description in generated pubspec| |OpenAPI API client|
|pubHomepage|Homepage in generated pubspec| |homepage|
|pubLibrary|Library name in generated code| |openapi.api|
|pubName|Name in generated pubspec| |openapi|
|pubPublishTo|Publish_to in generated pubspec| |null|
|pubRepository|Repository in generated pubspec| |null|
|pubVersion|Version in generated pubspec| |1.0.0|
|serializationLibrary|Specify serialization library|<dl><dt>**built_value**</dt><dd>[DEFAULT] built_value</dd><dt>**json_serializable**</dt><dd>[BETA] json_serializable</dd></dl>|built_value|
|skipCopyWith|Skip CopyWith when using Json Serializable for serialization| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |src|
|useEnumExtension|Allow the 'x-enum-values' extension for enums| |false|
|useOptional|Use Optional&lt;T&gt; to distinguish absent, null, and present for optional fields (Dart 3+)| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Object</li>
<li>String</li>
<li>bool</li>
<li>double</li>
<li>int</li>
<li>num</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>as</li>
<li>assert</li>
<li>async</li>
<li>augment</li>
<li>await</li>
<li>base</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>covariant</li>
<li>default</li>
<li>deferred</li>
<li>do</li>
<li>dynamic</li>
<li>else</li>
<li>enum</li>
<li>export</li>
<li>extends</li>
<li>extension</li>
<li>external</li>
<li>factory</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>for</li>
<li>function</li>
<li>get</li>
<li>hide</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>inout</li>
<li>interface</li>
<li>is</li>
<li>late</li>
<li>library</li>
<li>mixin</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>of</li>
<li>on</li>
<li>operator</li>
<li>out</li>
<li>part</li>
<li>patch</li>
<li>required</li>
<li>rethrow</li>
<li>return</li>
<li>sealed</li>
<li>set</li>
<li>show</li>
<li>source</li>
<li>static</li>
<li>super</li>
<li>switch</li>
<li>sync</li>
<li>this</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typedef</li>
<li>var</li>
<li>void</li>
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
|Authorizations|✓|ToolingExtension
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
|Uuid|✗|
|Array|✓|OAS2,OAS3
|Null|✗|OAS3
|AnyType|✗|OAS2,OAS3
|Object|✓|OAS2,OAS3
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
|Cookie|✓|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✓|OAS2,OAS3
|Union|✓|OAS3
|allOf|✓|OAS2,OAS3
|anyOf|✓|OAS3
|oneOf|✓|OAS3
|not|✗|OAS3

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
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
