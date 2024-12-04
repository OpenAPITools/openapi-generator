---
title: Documentation for the postgresql-schema Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | postgresql-schema | pass this to the generate command after -g |
| generator stability | BETA | |
| generator type | SCHEMA | |
| generator language | Postgresql | |
| generator default templating engine | mustache | |
| helpTxt | Generates a PostgreSQL schema based on the schema defined in the OpenAPI specification (v2, v3) | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|defaultDatabaseName|Default database name for all PostgreSQL queries| ||
|idAutoIncEnabled|Generates PostgreSQL sequences for autoincrement feature for integer 'id' fields.| |false|
|identifierNamingConvention|Naming convention of PostgreSQL identifiers(table names and column names). This is not related to database name which is defined by defaultDatabaseName option|<dl><dt>**original**</dt><dd>Do not transform original names</dd><dt>**snake_case**</dt><dd>Use snake_case names</dd></dl>|original|
|jsonDataType|Use special JSON PostgreSQL data type for complex model properties.| |json|
|namedParametersEnabled|Generates model prepared SQLs with named parameters, eg. :petName. Question mark placeholder used when option disabled.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>BigDecimal</li>
<li>ByteArray</li>
<li>Date</li>
<li>DateTime</li>
<li>URI</li>
<li>UUID</li>
<li>bool</li>
<li>boolean</li>
<li>byte</li>
<li>char</li>
<li>date</li>
<li>double</li>
<li>file</li>
<li>float</li>
<li>int</li>
<li>integer</li>
<li>long</li>
<li>mixed</li>
<li>number</li>
<li>short</li>
<li>string</li>
<li>void</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abort</li>
<li>absolute</li>
<li>access</li>
<li>action</li>
<li>add</li>
<li>admin</li>
<li>after</li>
<li>aggregate</li>
<li>all</li>
<li>also</li>
<li>alter</li>
<li>always</li>
<li>analyse</li>
<li>analyze</li>
<li>and</li>
<li>any</li>
<li>array</li>
<li>as</li>
<li>asc</li>
<li>assertion</li>
<li>assignment</li>
<li>asymmetric</li>
<li>at</li>
<li>attach</li>
<li>attribute</li>
<li>authorization</li>
<li>backward</li>
<li>before</li>
<li>begin</li>
<li>between</li>
<li>bigint</li>
<li>binary</li>
<li>bit</li>
<li>boolean</li>
<li>both</li>
<li>by</li>
<li>cache</li>
<li>call</li>
<li>called</li>
<li>cascade</li>
<li>cascaded</li>
<li>case</li>
<li>cast</li>
<li>catalog</li>
<li>chain</li>
<li>char</li>
<li>character</li>
<li>characteristics</li>
<li>check</li>
<li>checkpoint</li>
<li>class</li>
<li>close</li>
<li>cluster</li>
<li>coalesce</li>
<li>collate</li>
<li>collation</li>
<li>column</li>
<li>columns</li>
<li>comment</li>
<li>comments</li>
<li>commit</li>
<li>committed</li>
<li>concurrently</li>
<li>configuration</li>
<li>conflict</li>
<li>connection</li>
<li>constraint</li>
<li>constraints</li>
<li>content</li>
<li>continue</li>
<li>conversion</li>
<li>copy</li>
<li>cost</li>
<li>create</li>
<li>cross</li>
<li>csv</li>
<li>cube</li>
<li>current</li>
<li>current_catalog</li>
<li>current_date</li>
<li>current_role</li>
<li>current_schema</li>
<li>current_time</li>
<li>current_timestamp</li>
<li>current_user</li>
<li>cursor</li>
<li>cycle</li>
<li>data</li>
<li>database</li>
<li>day</li>
<li>deallocate</li>
<li>dec</li>
<li>decimal</li>
<li>declare</li>
<li>default</li>
<li>defaults</li>
<li>deferrable</li>
<li>deferred</li>
<li>define</li>
<li>definer</li>
<li>delete</li>
<li>delimiter</li>
<li>delimiters</li>
<li>depends</li>
<li>depth</li>
<li>desc</li>
<li>describe</li>
<li>detach</li>
<li>dictionary</li>
<li>disable</li>
<li>discard</li>
<li>distinct</li>
<li>do</li>
<li>document</li>
<li>domain</li>
<li>double</li>
<li>drop</li>
<li>each</li>
<li>else</li>
<li>enable</li>
<li>encoding</li>
<li>encrypted</li>
<li>end</li>
<li>enum</li>
<li>escape</li>
<li>event</li>
<li>except</li>
<li>exclude</li>
<li>excluding</li>
<li>exclusive</li>
<li>execute</li>
<li>exists</li>
<li>explain</li>
<li>extension</li>
<li>external</li>
<li>extract</li>
<li>false</li>
<li>family</li>
<li>fetch</li>
<li>filter</li>
<li>finalize</li>
<li>first</li>
<li>float</li>
<li>following</li>
<li>for</li>
<li>force</li>
<li>foreign</li>
<li>forward</li>
<li>freeze</li>
<li>from</li>
<li>full</li>
<li>function</li>
<li>functions</li>
<li>generated</li>
<li>global</li>
<li>grant</li>
<li>granted</li>
<li>greatest</li>
<li>group</li>
<li>grouping</li>
<li>handler</li>
<li>having</li>
<li>header</li>
<li>hold</li>
<li>hour</li>
<li>identity</li>
<li>if</li>
<li>ilike</li>
<li>immediate</li>
<li>immutable</li>
<li>implicit</li>
<li>import</li>
<li>in</li>
<li>including</li>
<li>increment</li>
<li>index</li>
<li>indexes</li>
<li>inherit</li>
<li>inherits</li>
<li>initially</li>
<li>inline</li>
<li>inner</li>
<li>inout</li>
<li>input</li>
<li>insensitive</li>
<li>insert</li>
<li>instead</li>
<li>int</li>
<li>integer</li>
<li>intersect</li>
<li>interval</li>
<li>into</li>
<li>invoker</li>
<li>is</li>
<li>isnull</li>
<li>isolation</li>
<li>join</li>
<li>key</li>
<li>label</li>
<li>language</li>
<li>large</li>
<li>last</li>
<li>lateral</li>
<li>leading</li>
<li>leakproof</li>
<li>least</li>
<li>left</li>
<li>level</li>
<li>like</li>
<li>limit</li>
<li>listen</li>
<li>load</li>
<li>local</li>
<li>localtime</li>
<li>localtimestamp</li>
<li>location</li>
<li>lock</li>
<li>locked</li>
<li>logged</li>
<li>mapping</li>
<li>match</li>
<li>materialized</li>
<li>maxvalue</li>
<li>method</li>
<li>minute</li>
<li>minvalue</li>
<li>mode</li>
<li>modify</li>
<li>month</li>
<li>move</li>
<li>name</li>
<li>names</li>
<li>national</li>
<li>natural</li>
<li>nchar</li>
<li>new</li>
<li>next</li>
<li>no</li>
<li>none</li>
<li>normalize</li>
<li>normalized</li>
<li>not</li>
<li>nothing</li>
<li>notify</li>
<li>notnull</li>
<li>nowait</li>
<li>null</li>
<li>nullable</li>
<li>nullif</li>
<li>numeric</li>
<li>object</li>
<li>of</li>
<li>off</li>
<li>offset</li>
<li>oids</li>
<li>old</li>
<li>on</li>
<li>only</li>
<li>operator</li>
<li>option</li>
<li>options</li>
<li>or</li>
<li>order</li>
<li>ordinality</li>
<li>others</li>
<li>out</li>
<li>outer</li>
<li>over</li>
<li>overlaps</li>
<li>overlay</li>
<li>overriding</li>
<li>owned</li>
<li>owner</li>
<li>parallel</li>
<li>parameter</li>
<li>parser</li>
<li>partial</li>
<li>partition</li>
<li>passing</li>
<li>password</li>
<li>placing</li>
<li>plans</li>
<li>policy</li>
<li>position</li>
<li>preceding</li>
<li>precision</li>
<li>prepare</li>
<li>prepared</li>
<li>preserve</li>
<li>primary</li>
<li>prior</li>
<li>privileges</li>
<li>procedural</li>
<li>procedure</li>
<li>program</li>
<li>publication</li>
<li>quote</li>
<li>range</li>
<li>read</li>
<li>real</li>
<li>reassign</li>
<li>recheck</li>
<li>recursive</li>
<li>ref</li>
<li>references</li>
<li>referencing</li>
<li>refresh</li>
<li>reindex</li>
<li>relative</li>
<li>release</li>
<li>rename</li>
<li>repeatable</li>
<li>replace</li>
<li>replica</li>
<li>reset</li>
<li>restart</li>
<li>restrict</li>
<li>returning</li>
<li>returns</li>
<li>revoke</li>
<li>right</li>
<li>role</li>
<li>rollback</li>
<li>rollup</li>
<li>routine</li>
<li>routines</li>
<li>row</li>
<li>rows</li>
<li>rule</li>
<li>savepoint</li>
<li>schema</li>
<li>schemas</li>
<li>scroll</li>
<li>search</li>
<li>second</li>
<li>security</li>
<li>select</li>
<li>sequence</li>
<li>sequences</li>
<li>serializable</li>
<li>server</li>
<li>session</li>
<li>session_user</li>
<li>set</li>
<li>setof</li>
<li>sets</li>
<li>share</li>
<li>show</li>
<li>similar</li>
<li>simple</li>
<li>skip</li>
<li>smallint</li>
<li>snapshot</li>
<li>some</li>
<li>sql</li>
<li>stable</li>
<li>standalone</li>
<li>start</li>
<li>statement</li>
<li>statistics</li>
<li>stdin</li>
<li>stdout</li>
<li>storage</li>
<li>strict</li>
<li>strip</li>
<li>subscription</li>
<li>substring</li>
<li>symmetric</li>
<li>sysid</li>
<li>system</li>
<li>table</li>
<li>tables</li>
<li>tablesample</li>
<li>tablespace</li>
<li>temp</li>
<li>template</li>
<li>temporary</li>
<li>text</li>
<li>then</li>
<li>ties</li>
<li>time</li>
<li>timestamp</li>
<li>to</li>
<li>trailing</li>
<li>transaction</li>
<li>transform</li>
<li>treat</li>
<li>trigger</li>
<li>trim</li>
<li>true</li>
<li>truncate</li>
<li>trusted</li>
<li>type</li>
<li>types</li>
<li>unbounded</li>
<li>uncommitted</li>
<li>unencrypted</li>
<li>union</li>
<li>unique</li>
<li>unknown</li>
<li>unlisten</li>
<li>unlogged</li>
<li>until</li>
<li>update</li>
<li>user</li>
<li>using</li>
<li>vacuum</li>
<li>valid</li>
<li>validate</li>
<li>validator</li>
<li>value</li>
<li>values</li>
<li>varchar</li>
<li>variadic</li>
<li>varying</li>
<li>verbose</li>
<li>version</li>
<li>view</li>
<li>views</li>
<li>volatile</li>
<li>when</li>
<li>where</li>
<li>whitespace</li>
<li>window</li>
<li>with</li>
<li>within</li>
<li>without</li>
<li>work</li>
<li>wrapper</li>
<li>write</li>
<li>xml</li>
<li>xmlattributes</li>
<li>xmlconcat</li>
<li>xmlelement</li>
<li>xmlexists</li>
<li>xmlforest</li>
<li>xmlnamespaces</li>
<li>xmlparse</li>
<li>xmlpi</li>
<li>xmlroot</li>
<li>xmlserialize</li>
<li>xmltable</li>
<li>year</li>
<li>yes</li>
<li>zone</li>
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
