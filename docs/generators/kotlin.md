---
title: Documentation for the kotlin Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | kotlin | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | Kotlin | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Kotlin client. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|additionalModelTypeAnnotations|Additional annotations for model type(class level annotations). List separated by semicolon(;) or new line (Linux or Windows)| |null|
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |kotlin-client|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|collectionType|Option. Collection type to use|<dl><dt>**array**</dt><dd>kotlin.Array</dd><dt>**list**</dt><dd>kotlin.collections.List</dd></dl>|list|
|companionObject|Whether to generate companion objects in data classes, enabling companion extensions.| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**threetenbp-localdatetime**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, for legacy app only)</dd><dt>**kotlinx-datetime**</dt><dd>kotlinx-datetime (preferred for multiplatform)</dd><dt>**string**</dt><dd>String</dd><dt>**java8-localdatetime**</dt><dd>Java 8 native JSR310 (jvm only, for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (jvm only, preferred for jdk 1.8+)</dd><dt>**threetenbp**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, preferred for jdk &lt; 1.8)</dd></dl>|java8|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |original|
|explicitApi|Generates code with explicit access modifiers to comply with Kotlin Explicit API Mode.| |false|
|failOnUnknownProperties|Fail Jackson de-serialization on unknown properties| |false|
|generateOneOfAnyOfWrappers|Generate oneOf, anyOf schemas as wrappers. Only `jvm-retrofit2`(library) with `gson` or `kotlinx_serialization`(serializationLibrary) support this option.| |false|
|generateRoomModels|Generate Android Room database models in addition to API models (JVM Volley library only)| |false|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|idea|Add IntelliJ Idea plugin and mark Kotlin main and test folders as source folders.| |false|
|implicitHeaders|Skip header parameters in the generated API methods.| |false|
|library|Library template (sub-template) to use|<dl><dt>**jvm-ktor**</dt><dd>Platform: Java Virtual Machine. HTTP client: Ktor 1.6.7. JSON processing: Gson, Jackson (default).</dd><dt>**jvm-okhttp4**</dt><dd>[DEFAULT] Platform: Java Virtual Machine. HTTP client: OkHttp 4.2.0 (Android 5.0+ and Java 8+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-spring-webclient**</dt><dd>Platform: Java Virtual Machine. HTTP: Spring 5 (or 6 with useSpringBoot3 enabled) WebClient. JSON processing: Jackson.</dd><dt>**jvm-spring-restclient**</dt><dd>Platform: Java Virtual Machine. HTTP: Spring 6 RestClient. JSON processing: Jackson.</dd><dt>**jvm-retrofit2**</dt><dd>Platform: Java Virtual Machine. HTTP client: Retrofit 2.6.2.</dd><dt>**multiplatform**</dt><dd>Platform: Kotlin multiplatform. HTTP client: Ktor 1.6.7. JSON processing: Kotlinx Serialization: 1.2.1.</dd><dt>**jvm-volley**</dt><dd>Platform: JVM for Android. HTTP client: Volley 1.2.1. JSON processing: gson 2.8.9 (Deprecated)</dd><dt>**jvm-vertx**</dt><dd>Platform: Java Virtual Machine. HTTP client: Vert.x Web Client. JSON processing: Moshi, Gson or Jackson.</dd></dl>|jvm-okhttp4|
|mapFileBinaryToByteArray|Map File and Binary to ByteArray (default: false)| |false|
|modelMutable|Create mutable models| |false|
|moshiCodeGen|Whether to enable codegen with the Moshi library. Refer to the [official Moshi doc](https://github.com/square/moshi#codegen) for more info.| |false|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.| |false|
|nullableReturnType|Nullable return type| |false|
|omitGradlePluginVersions|Whether to declare Gradle plugin versions in build files.| |false|
|omitGradleWrapper|Whether to omit Gradle wrapper for creating a sub project.| |false|
|packageName|Generated artifact package name.| |org.openapitools.client|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|requestDateConverter|JVM-Option. Defines in how to handle date-time objects that are used for a request (as query or parameter)|<dl><dt>**toJson**</dt><dd>[DEFAULT] Date formatter option using a json converter.</dd><dt>**toString**</dt><dd>Use the 'toString'-method of the date-time object to retrieve the related string representation.</dd></dl>|toJson|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson' or 'jackson' or 'kotlinx_serialization'| |moshi|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|
|supportAndroidApiLevel25AndBelow|[WARNING] This flag will generate code that has a known security vulnerability. It uses `kotlin.io.createTempFile` instead of `java.nio.file.Files.createTempFile` in order to support Android API level 25 and below. For more info, please check the following links https://github.com/OpenAPITools/openapi-generator/security/advisories/GHSA-23x4-m842-fmwf, https://github.com/OpenAPITools/openapi-generator/pull/9284| |false|
|useCoroutines|Whether to use the Coroutines adapter with the retrofit2 library.| |false|
|useJackson3|Use Jackson 3 dependencies (tools.jackson package). Not yet supported for kotlin-client; reserved for future use.| |false|
|useNonAsciiHeaders|Allow to use non-ascii headers with the okhttp library| |false|
|useResponseAsReturnType|When using retrofit2 and coroutines, use `Response`&lt;`T`&gt; as return type instead of `T`.| |true|
|useRxJava3|Whether to use the RxJava3 adapter with the retrofit2 library.| |false|
|useSettingsGradle|Whether the project uses settings.gradle.| |false|
|useSpringBoot3|Whether to use the Spring Boot 3 with the jvm-spring-webclient library.| |false|

## SUPPORTED VENDOR EXTENSIONS

| Extension name | Description | Applicable for | Default value |
| -------------- | ----------- | -------------- | ------------- |
|x-class-extra-annotation|List of custom annotations to be added to model|MODEL|null
|x-field-extra-annotation|List of custom annotations to be added to property|FIELD, OPERATION_PARAMETER|null


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
<li>contract</li>
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
|Cookie|✗|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✗|OAS2,OAS3
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
|OAuth2_Password|✗|OAS2,OAS3
|OAuth2_ClientCredentials|✗|OAS2,OAS3
|OAuth2_AuthorizationCode|✓|OAS2,OAS3
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
