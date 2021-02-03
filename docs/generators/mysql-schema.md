---
title: Config Options for mysql-schema
sidebar_label: mysql-schema
---

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|defaultDatabaseName|Default database name for all MySQL queries| ||
|identifierNamingConvention|Naming convention of MySQL identifiers(table names and column names). This is not related to database name which is defined by defaultDatabaseName option|<dl><dt>**original**</dt><dd>Do not transform original names</dd><dt>**snake_case**</dt><dd>Use snake_case names</dd></dl>|original|
|jsonDataTypeEnabled|Use special JSON MySQL data type for complex model properties. Requires MySQL version 5.7.8. Generates TEXT data type when disabled| |true|
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
<li>binary</li>
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
<li>accessible</li>
<li>add</li>
<li>all</li>
<li>alter</li>
<li>analyze</li>
<li>and</li>
<li>as</li>
<li>asc</li>
<li>asensitive</li>
<li>before</li>
<li>between</li>
<li>bigint</li>
<li>binary</li>
<li>blob</li>
<li>both</li>
<li>by</li>
<li>call</li>
<li>cascade</li>
<li>case</li>
<li>change</li>
<li>char</li>
<li>character</li>
<li>check</li>
<li>collate</li>
<li>column</li>
<li>condition</li>
<li>constraint</li>
<li>continue</li>
<li>convert</li>
<li>create</li>
<li>cross</li>
<li>cube</li>
<li>cume_dist</li>
<li>current_date</li>
<li>current_time</li>
<li>current_timestamp</li>
<li>current_user</li>
<li>cursor</li>
<li>database</li>
<li>databases</li>
<li>day_hour</li>
<li>day_microsecond</li>
<li>day_minute</li>
<li>day_second</li>
<li>dec</li>
<li>decimal</li>
<li>declare</li>
<li>default</li>
<li>delayed</li>
<li>delete</li>
<li>dense_rank</li>
<li>desc</li>
<li>describe</li>
<li>deterministic</li>
<li>distinct</li>
<li>distinctrow</li>
<li>div</li>
<li>double</li>
<li>drop</li>
<li>dual</li>
<li>each</li>
<li>else</li>
<li>elseif</li>
<li>empty</li>
<li>enclosed</li>
<li>escaped</li>
<li>except</li>
<li>exists</li>
<li>exit</li>
<li>explain</li>
<li>false</li>
<li>fetch</li>
<li>first_value</li>
<li>float</li>
<li>float4</li>
<li>float8</li>
<li>for</li>
<li>force</li>
<li>foreign</li>
<li>from</li>
<li>fulltext</li>
<li>function</li>
<li>generated</li>
<li>get</li>
<li>grant</li>
<li>group</li>
<li>grouping</li>
<li>groups</li>
<li>having</li>
<li>high_priority</li>
<li>hour_microsecond</li>
<li>hour_minute</li>
<li>hour_second</li>
<li>if</li>
<li>ignore</li>
<li>in</li>
<li>index</li>
<li>infile</li>
<li>inner</li>
<li>inout</li>
<li>insensitive</li>
<li>insert</li>
<li>int</li>
<li>int1</li>
<li>int2</li>
<li>int3</li>
<li>int4</li>
<li>int8</li>
<li>integer</li>
<li>interval</li>
<li>into</li>
<li>io_after_gtids</li>
<li>io_before_gtids</li>
<li>is</li>
<li>iterate</li>
<li>join</li>
<li>json_table</li>
<li>key</li>
<li>keys</li>
<li>kill</li>
<li>lag</li>
<li>last_value</li>
<li>lead</li>
<li>leading</li>
<li>leave</li>
<li>left</li>
<li>like</li>
<li>limit</li>
<li>linear</li>
<li>lines</li>
<li>load</li>
<li>localtime</li>
<li>localtimestamp</li>
<li>lock</li>
<li>long</li>
<li>longblob</li>
<li>longtext</li>
<li>loop</li>
<li>low_priority</li>
<li>master_bind</li>
<li>master_ssl_verify_server_cert</li>
<li>match</li>
<li>maxvalue</li>
<li>mediumblob</li>
<li>mediumint</li>
<li>mediumtext</li>
<li>middleint</li>
<li>minute_microsecond</li>
<li>minute_second</li>
<li>mod</li>
<li>modifies</li>
<li>natural</li>
<li>no_write_to_binlog</li>
<li>not</li>
<li>nth_value</li>
<li>ntile</li>
<li>null</li>
<li>numeric</li>
<li>of</li>
<li>on</li>
<li>optimize</li>
<li>optimizer_costs</li>
<li>option</li>
<li>optionally</li>
<li>or</li>
<li>order</li>
<li>out</li>
<li>outer</li>
<li>outfile</li>
<li>over</li>
<li>partition</li>
<li>percent_rank</li>
<li>persist</li>
<li>persist_only</li>
<li>precision</li>
<li>primary</li>
<li>procedure</li>
<li>purge</li>
<li>range</li>
<li>rank</li>
<li>read</li>
<li>read_write</li>
<li>reads</li>
<li>real</li>
<li>recursive</li>
<li>references</li>
<li>regexp</li>
<li>release</li>
<li>rename</li>
<li>repeat</li>
<li>replace</li>
<li>require</li>
<li>resignal</li>
<li>restrict</li>
<li>return</li>
<li>revoke</li>
<li>right</li>
<li>rlike</li>
<li>role</li>
<li>row</li>
<li>row_number</li>
<li>rows</li>
<li>schema</li>
<li>schemas</li>
<li>second_microsecond</li>
<li>select</li>
<li>sensitive</li>
<li>separator</li>
<li>set</li>
<li>show</li>
<li>signal</li>
<li>smallint</li>
<li>spatial</li>
<li>specific</li>
<li>sql</li>
<li>sql_big_result</li>
<li>sql_calc_found_rows</li>
<li>sql_small_result</li>
<li>sqlexception</li>
<li>sqlstate</li>
<li>sqlwarning</li>
<li>ssl</li>
<li>starting</li>
<li>stored</li>
<li>straight_join</li>
<li>system</li>
<li>table</li>
<li>terminated</li>
<li>then</li>
<li>tinyblob</li>
<li>tinyint</li>
<li>tinytext</li>
<li>to</li>
<li>trailing</li>
<li>trigger</li>
<li>true</li>
<li>undo</li>
<li>union</li>
<li>unique</li>
<li>unlock</li>
<li>unsigned</li>
<li>update</li>
<li>usage</li>
<li>use</li>
<li>using</li>
<li>utc_date</li>
<li>utc_time</li>
<li>utc_timestamp</li>
<li>values</li>
<li>varbinary</li>
<li>varchar</li>
<li>varcharacter</li>
<li>varying</li>
<li>virtual</li>
<li>when</li>
<li>where</li>
<li>while</li>
<li>window</li>
<li>with</li>
<li>write</li>
<li>xor</li>
<li>year_month</li>
<li>zerofill</li>
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
|Cookie|✓|OAS3

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
|JSON|✗|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
