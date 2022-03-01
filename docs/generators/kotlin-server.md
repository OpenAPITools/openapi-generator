---
title: Documentation for the kotlin-server Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | kotlin-server | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | Kotlin | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Kotlin server. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |kotlin-server|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|featureAutoHead|Automatically provide responses to HEAD requests for existing routes that have the GET verb defined.| |true|
|featureCORS|Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org.| |false|
|featureCompression|Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response.| |true|
|featureConditionalHeaders|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |false|
|featureHSTS|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |true|
|featureLocations|Generates routes in a typed way, for both: constructing URLs and reading the parameters.| |true|
|featureMetrics|Enables metrics feature.| |true|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|interfaceOnly|Whether to generate only API interface stubs without the server files. This option is currently supported only when using jaxrs-spec library.| |false|
|library|library template (sub-template)|<dl><dt>**ktor**</dt><dd>ktor framework</dd><dt>**jaxrs-spec**</dt><dd>JAX-RS spec only</dd></dl>|ktor|
|modelMutable|Create mutable models| |false|
|packageName|Generated artifact package name.| |org.openapitools.server|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|returnResponse|Whether generate API interface should return javax.ws.rs.core.Response instead of a deserialized entity. Only useful if interfaceOnly is true. This option is currently supported only when using jaxrs-spec library.| |false|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson' or 'jackson'| |moshi|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|
|useBeanValidation|Use BeanValidation API annotations. This option is currently supported only when using jaxrs-spec library.| |false|
|useCoroutines|Whether to use the Coroutines. This option is currently supported only when using jaxrs-spec library.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|BigDecimal|java.math.BigDecimal|
|Date|java.time.LocalDate|
|DateTime|java.time.OffsetDateTime|
|File|java.io.File|
|LocalDate|java.time.LocalDate|
|LocalDateTime|java.time.LocalDateTime|
|LocalTime|java.time.LocalTime|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|kotlin.collections.ArrayList|
|list|kotlin.collections.ArrayList|
|map|kotlin.collections.HashMap|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>kotlin.Array</li>
<li>kotlin.Boolean</li>
<li>kotlin.Byte</li>
<li>kotlin.ByteArray</li>
<li>kotlin.Char</li>
<li>kotlin.Double</li>
<li>kotlin.Float</li>
<li>kotlin.Int</li>
<li>kotlin.Long</li>
<li>kotlin.Short</li>
<li>kotlin.String</li>
<li>kotlin.collections.List</li>
<li>kotlin.collections.Map</li>
<li>kotlin.collections.MutableList</li>
<li>kotlin.collections.MutableMap</li>
<li>kotlin.collections.MutableSet</li>
<li>kotlin.collections.Set</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>ApiResponse</li>
<li>abstract</li>
<li>actual</li>
<li>annotation</li>
<li>as</li>
<li>break</li>
<li>class</li>
<li>companion</li>
<li>const</li>
<li>constructor</li>
<li>continue</li>
<li>crossinline</li>
<li>data</li>
<li>delegate</li>
<li>do</li>
<li>dynamic</li>
<li>else</li>
<li>enum</li>
<li>expect</li>
<li>external</li>
<li>false</li>
<li>field</li>
<li>final</li>
<li>finally</li>
<li>for</li>
<li>fun</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>infix</li>
<li>init</li>
<li>inline</li>
<li>inner</li>
<li>interface</li>
<li>internal</li>
<li>is</li>
<li>it</li>
<li>lateinit</li>
<li>noinline</li>
<li>null</li>
<li>object</li>
<li>open</li>
<li>operator</li>
<li>out</li>
<li>override</li>
<li>package</li>
<li>param</li>
<li>private</li>
<li>property</li>
<li>protected</li>
<li>public</li>
<li>receiver</li>
<li>reified</li>
<li>return</li>
<li>sealed</li>
<li>setparam</li>
<li>super</li>
<li>suspend</li>
<li>tailrec</li>
<li>this</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typealias</li>
<li>typeof</li>
<li>val</li>
<li>value</li>
<li>var</li>
<li>vararg</li>
<li>when</li>
<li>where</li>
<li>while</li>
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
|Polymorphism|✗|OAS2,OAS3
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
