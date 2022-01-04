---
title: Config Options for typescript-fetch
sidebar_label: typescript-fetch
---

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumNameSuffix|Suffix that will be appended to all enum names.| |Enum|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |PascalCase|
|legacyDiscriminatorBehavior|Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C#have this enabled by default).|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|npmName|The name under which you want to publish generated npm package. Required to generate a full package| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|npmVersion|The version of your npm package. If not provided, using the version from the OpenAPI specification file.| |1.0.0|
|nullSafeAdditionalProps|Set to make additional properties types declare that their indexer may return undefined| |false|
|paramNaming|Naming convention for parameters: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|prefixParameterInterfaces|Setting this property to true will generate parameter interface declarations prefixed with API class name to avoid name conflicts.| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sagasAndRecords|Setting this property to true will generate additional files for use with redux-saga and immutablejs.| |false|
|snapshot|When setting this property to true, the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|supportsES6|Generate code that conforms to ES6.| |false|
|typescriptThreePlus|Setting this property to true will generate TypeScript 3.6+ compatible code.| |false|
|useSingleRequestParameter|Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.| |true|
|withInterfaces|Setting this property to true will generate interfaces next to the default class implementations.| |false|
|withoutRuntimeChecks|Setting this property to true will remove any runtime checks on the request and response payloads. Payloads will be casted to their expected types.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Array</li>
<li>Boolean</li>
<li>Date</li>
<li>Double</li>
<li>Error</li>
<li>File</li>
<li>Float</li>
<li>Integer</li>
<li>Long</li>
<li>Map</li>
<li>Object</li>
<li>ReadonlyArray</li>
<li>Set</li>
<li>String</li>
<li>any</li>
<li>boolean</li>
<li>number</li>
<li>object</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>ApiResponse</li>
<li>BASE_PATH</li>
<li>BaseAPI</li>
<li>BlobApiResponse</li>
<li>COLLECTION_FORMATS</li>
<li>Configuration</li>
<li>ConfigurationParameters</li>
<li>FetchAPI</li>
<li>FetchParams</li>
<li>HTTPBody</li>
<li>HTTPHeaders</li>
<li>HTTPMethod</li>
<li>HTTPQuery</li>
<li>Index</li>
<li>JSONApiResponse</li>
<li>Middleware</li>
<li>ModelPropertyNaming</li>
<li>RequestContext</li>
<li>RequestOpts</li>
<li>RequiredError</li>
<li>ResponseContext</li>
<li>ResponseTransformer</li>
<li>TextApiResponse</li>
<li>VoidApiResponse</li>
<li>abstract</li>
<li>await</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>configuration</li>
<li>const</li>
<li>continue</li>
<li>debugger</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>exists</li>
<li>export</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>formParams</li>
<li>function</li>
<li>goto</li>
<li>headerParams</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>let</li>
<li>long</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>queryParameters</li>
<li>requestOptions</li>
<li>return</li>
<li>short</li>
<li>static</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>this</li>
<li>throw</li>
<li>transient</li>
<li>true</li>
<li>try</li>
<li>typeof</li>
<li>useFormData</li>
<li>var</li>
<li>varLocalDeferred</li>
<li>varLocalPath</li>
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
|Union|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✗|OAS2,OAS3
|ApiKey|✗|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✗|OAS2,OAS3
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
