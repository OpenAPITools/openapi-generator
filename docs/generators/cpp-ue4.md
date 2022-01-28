---
title: Documentation for the cpp-ue4 Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | cpp-ue4 | pass this to the generate command after -g |
| generator stability | BETA | |
| generator type | CLIENT | |
| generator language | C++ | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Unreal Engine 4 C++ Module (beta). | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|cppNamespace|C++ namespace (convention: name::space::for::api).| |OpenAPI|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumUnknownDefaultCase|If the server adds new enum cases, that are unknown by an old spec/client, the client will fail to parse the network response.With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the server sends an enum case that is not known by the client/spec, they can safely fallback to this case.|<dl><dt>**false**</dt><dd>No changes to the enum's are made, this is the default option.</dd><dt>**true**</dt><dd>With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the enum case sent by the server is not known by the client/spec, can safely be decoded to this case.</dd></dl>|false|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C#have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|optionalProjectFile|Generate Build.cs| |true|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|reservedWordPrefix|Prefix to prepend to reserved words in order to avoid conflicts| |r_|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|unrealModuleName|Name of the generated unreal module (optional)| |OpenAPI|
|variableNameFirstCharacterUppercase|Make first character of variable name uppercase (eg. value -&gt; Value)| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|HttpFileInput|#include &quot;OpenAPIHelpers.h&quot;|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>FDateTime</li>
<li>FGuid</li>
<li>FString</li>
<li>TArray</li>
<li>TArray&lt;uint8&gt;</li>
<li>TMap</li>
<li>TSharedPtr&lt;FJsonObject&gt;</li>
<li>TSharedPtr&lt;FJsonValue&gt;</li>
<li>bool</li>
<li>double</li>
<li>float</li>
<li>int32</li>
<li>int64</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>alignas</li>
<li>alignof</li>
<li>and</li>
<li>and_eq</li>
<li>asm</li>
<li>auto</li>
<li>bitand</li>
<li>bitor</li>
<li>bool</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>char16_t</li>
<li>char32_t</li>
<li>class</li>
<li>compl</li>
<li>concept</li>
<li>const</li>
<li>const_cast</li>
<li>constexpr</li>
<li>continue</li>
<li>decltype</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>dynamic_cast</li>
<li>else</li>
<li>enum</li>
<li>explicit</li>
<li>export</li>
<li>extern</li>
<li>false</li>
<li>float</li>
<li>for</li>
<li>friend</li>
<li>goto</li>
<li>if</li>
<li>inline</li>
<li>int</li>
<li>linux</li>
<li>long</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>noexcept</li>
<li>not</li>
<li>not_eq</li>
<li>nullptr</li>
<li>operator</li>
<li>or</li>
<li>or_eq</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>register</li>
<li>reinterpret_cast</li>
<li>requires</li>
<li>return</li>
<li>short</li>
<li>signed</li>
<li>sizeof</li>
<li>static</li>
<li>static_assert</li>
<li>static_cast</li>
<li>struct</li>
<li>switch</li>
<li>template</li>
<li>this</li>
<li>thread_local</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typedef</li>
<li>typeid</li>
<li>typename</li>
<li>union</li>
<li>unsigned</li>
<li>using</li>
<li>virtual</li>
<li>void</li>
<li>volatile</li>
<li>wchar_t</li>
<li>while</li>
<li>xor</li>
<li>xor_eq</li>
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
|Cookie|✓|OAS3

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
|OAuth2_Password|✓|OAS2,OAS3
|OAuth2_ClientCredentials|✓|OAS2,OAS3
|OAuth2_AuthorizationCode|✓|OAS2,OAS3

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
