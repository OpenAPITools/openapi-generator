---
title: Documentation for the kotlin-spring Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | kotlin-spring | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | Kotlin | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Kotlin Spring application. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|additionalModelTypeAnnotations|Additional annotations for model type(class level annotations). List separated by semicolon(;) or new line (Linux or Windows)| |null|
|annotationLibrary|Select the complementary documentation annotation library.|<dl><dt>**none**</dt><dd>Do not annotate Model and Api with complementary annotations.</dd><dt>**swagger1**</dt><dd>Annotate Model and Api using the Swagger Annotations 1.x library.</dd><dt>**swagger2**</dt><dd>Annotate Model and Api using the Swagger Annotations 2.x library.</dd></dl>|swagger2|
|apiPackage|api package for generated code| |org.openapitools.api|
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |openapi-spring|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|basePackage|base package (invokerPackage) for generated code| |org.openapitools|
|beanQualifiers|Whether to add fully-qualifier class names as bean qualifiers in @Component and @RestController annotations. May be used to prevent bean names clash if multiple generated libraries (contexts) added to single project.| |false|
|configPackage|configuration package for generated code| |org.openapitools.configuration|
|delegatePattern|Whether to generate the server files using the delegate pattern| |false|
|documentationProvider|Select the OpenAPI documentation provider.|<dl><dt>**none**</dt><dd>Do not publish an OpenAPI specification.</dd><dt>**source**</dt><dd>Publish the original input OpenAPI specification.</dd><dt>**springfox**</dt><dd>Generate an OpenAPI 2 (fka Swagger RESTful API Documentation Specification) specification using SpringFox 2.x. Deprecated (for removal); use springdoc instead.</dd><dt>**springdoc**</dt><dd>Generate an OpenAPI 3 specification using SpringDoc.</dd></dl>|springdoc|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|exceptionHandler|generate default global exception handlers (not compatible with reactive. enabling reactive will disable exceptionHandler )| |true|
|gradleBuildFile|generate a gradle build file using the Kotlin DSL| |true|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|interfaceOnly|Whether to generate only API interface stubs without the server files.| |false|
|library|library template (sub-template)|<dl><dt>**spring-boot**</dt><dd>Spring-boot Server application.</dd><dt>**spring-cloud**</dt><dd>Spring-Cloud-Feign client with Spring-Boot auto-configured settings.</dd></dl>|spring-boot|
|modelMutable|Create mutable models| |false|
|modelPackage|model package for generated code| |org.openapitools.model|
|packageName|Generated artifact package name.| |org.openapitools|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|reactive|use coroutines for reactive behavior| |false|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson' or 'jackson' or 'kotlinx_serialization'| |moshi|
|serverPort|configuration the port in which the sever is to run on| |8080|
|serviceImplementation|generate stub service implementations that extends service interfaces. If this is set to true service interfaces will also be generated| |false|
|serviceInterface|generate service interfaces to go alongside controllers. In most cases this option would be used to update an existing project, so not to override implementations. Useful to help facilitate the generation gap pattern| |false|
|skipDefaultInterface|Whether to skip generation of default implementations for interfaces| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|
|title|server title name or client service name| |OpenAPI Kotlin Spring|
|useBeanValidation|Use BeanValidation API annotations to validate data types| |true|
|useFeignClientUrl|Whether to generate Feign client with url parameter.| |true|
|useSpringBoot3|Generate code and provide dependencies for use with Spring Boot 3.x. (Use jakarta instead of javax in imports). Enabling this option will also enable `useJakartaEe`.| |false|
|useSwaggerUI|Open the OpenApi specification in swagger-ui. Will also import and configure needed dependencies| |true|
|useTags|Whether to use tags for creating interface and controller class names| |false|

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
<li>ApiClient</li>
<li>ApiException</li>
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
|BearerToken|✗|OAS3
|OAuth2_Implicit|✓|OAS2,OAS3
|OAuth2_Password|✓|OAS2,OAS3
|OAuth2_ClientCredentials|✓|OAS2,OAS3
|OAuth2_AuthorizationCode|✓|OAS2,OAS3
|SignatureAuth|✗|OAS3

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
