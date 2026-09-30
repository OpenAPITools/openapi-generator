---
title: Documentation for the javascript Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | javascript | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | Javascript | |
| generator default templating engine | mustache | |
| helpTxt | Generates a JavaScript client library. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |null|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|emitJSDoc|generate JSDoc comments| |true|
|emitModelMethods|generate getters and setters for model properties| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumUnknownDefaultCase|If the server adds new enum cases, that are unknown by an old spec/client, the client will fail to parse the network response. With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the server sends an enum case that is not known by the client/spec, they can safely fallback to this case.|<dl><dt>**false**</dt><dd>No changes to the enums are made, this is the default option.</dd><dt>**true**</dt><dd>With this option enabled, each enum will have a new case, 'unknown_default_open_api', so that when the enum case sent by the server is not known by the client/spec, can safely be decoded to this case.</dd></dl>|false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|invokerPackage|root package for generated code| |null|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C# have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|library|library template (sub-template)|<dl><dt>**javascript**</dt><dd>JavaScript client library</dd><dt>**apollo**</dt><dd>Apollo REST DataSource</dd></dl>|javascript|
|licenseName|name of the license the project uses (Default: using info.license.name)| |null|
|modelPackage|package for generated models| |null|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|moduleName|module name for AMD, Node or globals (Default: generated from &lt;projectName&gt;)| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectDescription|description of the project (Default: using info.description or &quot;Client library of &lt;projectName&gt;&quot;)| |null|
|projectName|name of the project (Default: generated from info.title or &quot;openapi-js-client&quot;)| |null|
|projectVersion|version of the project (Default: using info.version or &quot;1.0.0&quot;)| |null|
|skipDefaultUserAgent|Skip setting default user-agent in ApiClient.js| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |src|
|useInheritance|use JavaScript prototype chains &amp; delegation for inheritance| |true|
|usePromises|use Promises as return values from the client API, instead of superagent callbacks| |false|
|useURLSearchParams|use JS build-in UrlSearchParams, instead of deprecated npm lib 'querystring'| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|
|list|Array|
|map|Object|
|set|Array|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Array</li>
<li>Blob</li>
<li>Boolean</li>
<li>Date</li>
<li>File</li>
<li>Number</li>
<li>Object</li>
<li>String</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>Array</li>
<li>Date</li>
<li>Infinity</li>
<li>Math</li>
<li>NaN</li>
<li>Number</li>
<li>Object</li>
<li>String</li>
<li>abstract</li>
<li>arguments</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>debugger</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>eval</li>
<li>export</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>function</li>
<li>goto</li>
<li>hasOwnProperty</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>isFinite</li>
<li>isNaN</li>
<li>isPrototypeOf</li>
<li>let</li>
<li>long</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>prototype</li>
<li>public</li>
<li>return</li>
<li>short</li>
<li>static</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>this</li>
<li>throw</li>
<li>throws</li>
<li>toString</li>
<li>transient</li>
<li>true</li>
<li>try</li>
<li>typeof</li>
<li>undefined</li>
<li>valueOf</li>
<li>var</li>
<li>void</li>
<li>volatile</li>
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
|allOf|✗|OAS2,OAS3
|anyOf|✗|OAS3
|oneOf|✗|OAS3
|not|✗|OAS3

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
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
