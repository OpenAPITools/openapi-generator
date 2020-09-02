---
title: Config Options for swift4-deprecated
sidebar_label: swift4-deprecated
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|disallowAdditionalPropertiesIfNotPresent|Specify the behavior when the 'additionalProperties' keyword is not present in the OAS document. If false: the 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications. If true: when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.This setting is currently ignored for OAS 2.0 documents:  1) When the 'additionalProperties' keyword is not present in a 2.0 schema, additional properties are NOT allowed.  2) Boolean values of the 'additionalProperties' keyword are ignored. It's as if additional properties are NOT allowed.Note: the root cause are issues #1369 and #1371, which must be resolved in the swagger-parser project.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is automatically set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|legacyDiscriminatorBehavior|This flag is used by OpenAPITools codegen to influence the processing of the discriminator attribute in OpenAPI documents. This flag has no impact if the OAS document does not use the discriminator attribute. The default value of this flag is set in each language-specific code generator (e.g. Python, Java, go...)using the method toModelName. Note to developers supporting a language generator in OpenAPITools; to fully support the discriminator attribute as defined in the OAS specification 3.x, language generators should set this flag to true by default; however this requires updating the mustache templates to generate a language-specific discriminator lookup function that iterates over {{#mappedModels}} and does not iterate over {{children}}, {{#anyOf}}, or {{#oneOf}}.|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|lenientTypeCast|Accept and cast values for simple types (string-&gt;bool, string-&gt;int, int-&gt;string)| |false|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.(default: false)| |null|
|objcCompatible|Add additional properties and methods for Objective-C compatibility (default: false)| |null|
|podAuthors|Authors used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podDocsetURL|Docset URL used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podSource|Source information used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift, Result are available.| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|
|unwrapRequired|Treat 'required' properties in response as non-optional (which would crash the app if api returns null as opposed to required option specified in json schema| |null|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Any</li>
<li>AnyObject</li>
<li>Bool</li>
<li>Character</li>
<li>Data</li>
<li>Date</li>
<li>Decimal</li>
<li>Double</li>
<li>Float</li>
<li>Int</li>
<li>Int32</li>
<li>Int64</li>
<li>String</li>
<li>URL</li>
<li>UUID</li>
<li>Void</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>#available</li>
<li>#colorLiteral</li>
<li>#column</li>
<li>#else</li>
<li>#elseif</li>
<li>#endif</li>
<li>#file</li>
<li>#fileLiteral</li>
<li>#function</li>
<li>#if</li>
<li>#imageLiteral</li>
<li>#line</li>
<li>#selector</li>
<li>#sourceLocation</li>
<li>Any</li>
<li>AnyObject</li>
<li>Array</li>
<li>Bool</li>
<li>COLUMN</li>
<li>Character</li>
<li>Class</li>
<li>ClosedRange</li>
<li>Codable</li>
<li>CountableClosedRange</li>
<li>CountableRange</li>
<li>Data</li>
<li>Decodable</li>
<li>Dictionary</li>
<li>Double</li>
<li>Encodable</li>
<li>Error</li>
<li>ErrorResponse</li>
<li>FILE</li>
<li>FUNCTION</li>
<li>Float</li>
<li>Float32</li>
<li>Float64</li>
<li>Float80</li>
<li>Int</li>
<li>Int16</li>
<li>Int32</li>
<li>Int64</li>
<li>Int8</li>
<li>LINE</li>
<li>OptionSet</li>
<li>Optional</li>
<li>Protocol</li>
<li>Range</li>
<li>Response</li>
<li>Self</li>
<li>Set</li>
<li>StaticString</li>
<li>String</li>
<li>Type</li>
<li>UInt</li>
<li>UInt16</li>
<li>UInt32</li>
<li>UInt64</li>
<li>UInt8</li>
<li>URL</li>
<li>Unicode</li>
<li>Void</li>
<li>_</li>
<li>as</li>
<li>associatedtype</li>
<li>associativity</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>continue</li>
<li>convenience</li>
<li>default</li>
<li>defer</li>
<li>deinit</li>
<li>didSet</li>
<li>do</li>
<li>dynamic</li>
<li>dynamicType</li>
<li>else</li>
<li>enum</li>
<li>extension</li>
<li>fallthrough</li>
<li>false</li>
<li>fileprivate</li>
<li>final</li>
<li>for</li>
<li>func</li>
<li>get</li>
<li>guard</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>indirect</li>
<li>infix</li>
<li>init</li>
<li>inout</li>
<li>internal</li>
<li>is</li>
<li>lazy</li>
<li>left</li>
<li>let</li>
<li>mutating</li>
<li>nil</li>
<li>none</li>
<li>nonmutating</li>
<li>open</li>
<li>operator</li>
<li>optional</li>
<li>override</li>
<li>postfix</li>
<li>precedence</li>
<li>prefix</li>
<li>private</li>
<li>protocol</li>
<li>public</li>
<li>repeat</li>
<li>required</li>
<li>rethrows</li>
<li>return</li>
<li>right</li>
<li>self</li>
<li>set</li>
<li>static</li>
<li>struct</li>
<li>subscript</li>
<li>super</li>
<li>switch</li>
<li>throw</li>
<li>throws</li>
<li>true</li>
<li>try</li>
<li>typealias</li>
<li>unowned</li>
<li>var</li>
<li>weak</li>
<li>where</li>
<li>while</li>
<li>willSet</li>
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
|Readme|✗|ToolingExtension
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
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
