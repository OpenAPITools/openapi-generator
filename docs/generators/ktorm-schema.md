---
title: Documentation for the ktorm-schema Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | ktorm-schema | pass this to the generate command after -g |
| generator stability | BETA | |
| generator type | SCHEMA | |
| generator language | Ktorm | |
| generator default templating engine | mustache | |
| helpTxt | Generates a kotlin-ktorm schema (beta) | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|addSurrogateKey|Adds the surrogate key for all models that don't already have a primary key (named by the above convention)| |false|
|additionalModelTypeAnnotations|Additional annotations for model type(class level annotations). List separated by semicolon(;) or new line (Linux or Windows)| |null|
|artifactId|Generated artifact id (name of jar).| |ktorm|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|defaultDatabaseName|Default database name for all queries| |sqlite.db|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', 'original', and 'bestEffortBacktick' (like 'original' but tries to wrap values in backticks before falling back to sanitizing, e.g. `name,asc` stays `name,asc` rather than becoming nameCommaAsc; useful for sort/order enums)| |original|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|identifierNamingConvention|Naming convention of Ktorm identifiers(table names and column names). This is not related to database name which is defined by defaultDatabaseName option|<dl><dt>**original**</dt><dd>Do not transform original names</dd><dt>**snake_case**</dt><dd>Use snake_case names</dd></dl>|original|
|implicitHeaders|Skip header parameters in the generated API methods.| |false|
|importModelPackageName|Package name of the imported models| |org.openapitools.database.models|
|modelMutable|Create mutable models| |false|
|packageName|Generated artifact package name.| |org.openapitools.database|
|primaryKeyConvention|Primary key naming convention| |id|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|

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
<li>abort</li>
<li>action</li>
<li>add</li>
<li>after</li>
<li>all</li>
<li>alter</li>
<li>always</li>
<li>analyze</li>
<li>and</li>
<li>any</li>
<li>as</li>
<li>asc</li>
<li>attach</li>
<li>autoincr</li>
<li>autoincrement</li>
<li>before</li>
<li>begin</li>
<li>between</li>
<li>bitand</li>
<li>bitnot</li>
<li>bitor</li>
<li>blob</li>
<li>by</li>
<li>cascade</li>
<li>case</li>
<li>cast</li>
<li>check</li>
<li>collate</li>
<li>column</li>
<li>comma</li>
<li>commit</li>
<li>concat</li>
<li>conflict</li>
<li>constraint</li>
<li>create</li>
<li>cross</li>
<li>current</li>
<li>current_date</li>
<li>current_time</li>
<li>current_timestamp</li>
<li>database</li>
<li>default</li>
<li>deferrable</li>
<li>deferred</li>
<li>delete</li>
<li>desc</li>
<li>detach</li>
<li>distinct</li>
<li>do</li>
<li>dot</li>
<li>drop</li>
<li>each</li>
<li>else</li>
<li>end</li>
<li>eq</li>
<li>escape</li>
<li>except</li>
<li>exclude</li>
<li>exclusive</li>
<li>exists</li>
<li>explain</li>
<li>fail</li>
<li>filter</li>
<li>first</li>
<li>float</li>
<li>following</li>
<li>for</li>
<li>foreign</li>
<li>from</li>
<li>full</li>
<li>ge</li>
<li>generated</li>
<li>glob</li>
<li>group</li>
<li>groups</li>
<li>gt</li>
<li>having</li>
<li>id</li>
<li>if</li>
<li>ignore</li>
<li>immediate</li>
<li>in</li>
<li>index</li>
<li>indexed</li>
<li>initially</li>
<li>inner</li>
<li>insert</li>
<li>instead</li>
<li>integer</li>
<li>intersect</li>
<li>into</li>
<li>is</li>
<li>isnull</li>
<li>join</li>
<li>key</li>
<li>last</li>
<li>le</li>
<li>left</li>
<li>like</li>
<li>limit</li>
<li>lp</li>
<li>lshift</li>
<li>lt</li>
<li>match</li>
<li>minus</li>
<li>natural</li>
<li>ne</li>
<li>no</li>
<li>not</li>
<li>nothing</li>
<li>notnull</li>
<li>null</li>
<li>nulls</li>
<li>of</li>
<li>offset</li>
<li>on</li>
<li>or</li>
<li>order</li>
<li>others</li>
<li>outer</li>
<li>over</li>
<li>partition</li>
<li>plan</li>
<li>plus</li>
<li>pragma</li>
<li>preceding</li>
<li>primary</li>
<li>query</li>
<li>raise</li>
<li>range</li>
<li>recursive</li>
<li>references</li>
<li>regexp</li>
<li>reindex</li>
<li>release</li>
<li>rem</li>
<li>rename</li>
<li>replace</li>
<li>restrict</li>
<li>right</li>
<li>rollback</li>
<li>row</li>
<li>rows</li>
<li>rp</li>
<li>rshift</li>
<li>savepoint</li>
<li>select</li>
<li>set</li>
<li>slash</li>
<li>star</li>
<li>string</li>
<li>table</li>
<li>temp</li>
<li>temporary</li>
<li>then</li>
<li>ties</li>
<li>to</li>
<li>transaction</li>
<li>trigger</li>
<li>unbounded</li>
<li>union</li>
<li>unique</li>
<li>update</li>
<li>using</li>
<li>vacuum</li>
<li>values</li>
<li>variable</li>
<li>view</li>
<li>virtual</li>
<li>when</li>
<li>where</li>
<li>window</li>
<li>with</li>
<li>without</li>
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
|Polymorphism|✗|OAS2,OAS3
|Union|✗|OAS3
|allOf|✗|OAS2,OAS3
|anyOf|✗|OAS3
|oneOf|✗|OAS3
|not|✗|OAS3

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
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✗|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
