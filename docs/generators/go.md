---
title: Documentation for the go Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | go | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | Go | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Go client library. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|enumClassPrefix|Prefix enum with class name| |false|
|enumUnknownDefaultCase|If the server adds new enum cases, that are unknown by an old spec/client, the client will fail to parse the network response. With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the server sends an enum case that is not known by the client/spec, they can safely fallback to this case.|<dl><dt>**false**</dt><dd>No changes to the enums are made, this is the default option.</dd><dt>**true**</dt><dd>With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the enum case sent by the server is not known by the client/spec, can safely be decoded to this case.</dd></dl>|false|
|generateInterfaces|Generate interfaces for api classes| |false|
|generateMarshalJSON|Generate MarshalJSON method| |true|
|generateUnmarshalJSON|Generate UnmarshalJSON method| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|isGoSubmodule|whether the generated Go module is a submodule| |false|
|packageName|Go package name (convention: lowercase).| |openapi|
|packageVersion|Go package version.| |1.0.0|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|structPrefix|whether to prefix struct with the class name. e.g. DeletePetOpts =&gt; PetApiDeletePetOpts| |false|
|useDefaultValuesForRequiredVars|Use default values for required variables when available| |false|
|useOneOfDiscriminatorLookup|Use the discriminator's mapping in oneOf to speed up the model lookup. IMPORTANT: Validation (e.g. one and only one match in oneOf's schemas) will be skipped.| |false|
|withAWSV4Signature|whether to include AWS v4 signature support| |false|
|withGoMod|Generate go.mod and go.sum| |true|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>bool</li>
<li>byte</li>
<li>complex128</li>
<li>complex64</li>
<li>float32</li>
<li>float64</li>
<li>int</li>
<li>int32</li>
<li>int64</li>
<li>interface{}</li>
<li>map[string]interface{}</li>
<li>rune</li>
<li>string</li>
<li>uint</li>
<li>uint32</li>
<li>uint64</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>bool</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>chan</li>
<li>complex128</li>
<li>complex64</li>
<li>const</li>
<li>continue</li>
<li>default</li>
<li>defer</li>
<li>else</li>
<li>error</li>
<li>fallthrough</li>
<li>float32</li>
<li>float64</li>
<li>for</li>
<li>func</li>
<li>go</li>
<li>goto</li>
<li>if</li>
<li>import</li>
<li>int</li>
<li>int16</li>
<li>int32</li>
<li>int64</li>
<li>int8</li>
<li>interface</li>
<li>map</li>
<li>nil</li>
<li>package</li>
<li>range</li>
<li>return</li>
<li>rune</li>
<li>select</li>
<li>string</li>
<li>struct</li>
<li>switch</li>
<li>type</li>
<li>uint</li>
<li>uint16</li>
<li>uint32</li>
<li>uint64</li>
<li>uint8</li>
<li>uintptr</li>
<li>var</li>
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
|Uuid|✗|
|Array|✓|OAS2,OAS3
|Null|✗|OAS3
|AnyType|✓|OAS2,OAS3
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
|ParameterizedServer|✓|OAS3
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
|Polymorphism|✗|OAS2,OAS3
|Union|✗|OAS3
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
|SignatureAuth|✓|OAS3
|AWSV4Signature|✓|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
