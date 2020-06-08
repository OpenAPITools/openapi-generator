---
title: Config Options for scala-akka-http-server
sidebar_label: scala-akka-http-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|akkaHttpVersion|The version of akka-http| |10.1.10|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |null|
|artifactId|artifactId| |openapi-scala-akka-http-server|
|artifactVersion|artifact version in generated pom.xml. This also becomes part of the generated library's filename| |1.0.0|
|dateLibrary|Option. Date library to use|<dl><dt>**joda**</dt><dd>Joda (for legacy app)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (prefered for JDK 1.8+)</dd></dl>|java8|
|disallowAdditionalPropertiesIfNotPresent|Specify the behavior when the 'additionalProperties' keyword is not present in the OAS document
If false: the 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications. If true: when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.This setting is currently ignored for OAS 2.0 documents:  1) When the 'additionalProperties' keyword is not present in a 2.0 schema, additional properties are NOT allowed.  2) Boolean values of the 'additionalProperties' keyword are ignored. It's as if additional properties are NOT allowed.Note: the root cause are issues #1369 and #1371, which must be resolved in the swagger-parser project.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is automatically set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|groupId|groupId in generated pom.xml| |org.openapitools|
|invokerPackage|root package for generated code| |org.openapitools.server|
|legacyDiscriminatorBehavior|This flag is used by OpenAPITools codegen to influence the processing of the discriminator attribute in OpenAPI documents. This flag has no impact if the OAS document does not use the discriminator attribute. The default value of this flag is set in each language-specific code generator (e.g. Python, Java, go...)using the method toModelName. Note to developers supporting a language generator in OpenAPITools; to fully support the discriminator attribute as defined in the OAS specification 3.x, language generators should set this flag to true by default; however this requires updating the mustache templates to generate a language-specific discriminator lookup function that iterates over {{#mappedModels}} and does not iterate over {{children}}, {{#anyOf}}, or {{#oneOf}}.|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|modelPackage|package for generated models| |null|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |null|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|ListBuffer|scala.collection.mutable.ListBuffer|
|ListSet|scala.collection.immutable.ListSet|
|LocalDate|org.joda.time.*|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|ListBuffer|
|map|Map|
|set|Set|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Any</li>
<li>Array</li>
<li>Boolean</li>
<li>Double</li>
<li>Float</li>
<li>Int</li>
<li>List</li>
<li>Long</li>
<li>Map</li>
<li>Object</li>
<li>Seq</li>
<li>String</li>
<li>boolean</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>def</li>
<li>do</li>
<li>else</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>for</li>
<li>forsome</li>
<li>if</li>
<li>implicit</li>
<li>import</li>
<li>lazy</li>
<li>match</li>
<li>new</li>
<li>null</li>
<li>object</li>
<li>override</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>return</li>
<li>sealed</li>
<li>super</li>
<li>this</li>
<li>throw</li>
<li>trait</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>val</li>
<li>var</li>
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
|BearerToken|✓|OAS3
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
|Custom|✓|OAS2,OAS3
