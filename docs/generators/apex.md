---
title: Config Options for apex
sidebar_label: apex
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiVersion|The Metadata API version number to use for components in this package.| |null|
|buildMethod|The build method for this package.| |null|
|classPrefix|Prefix for generated classes. Set this to avoid overwriting existing classes in your org.| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|namedCredential|The named credential name for the HTTP callouts| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|List|
|map|Map|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Blob</li>
<li>Boolean</li>
<li>Date</li>
<li>Datetime</li>
<li>Decimal</li>
<li>Double</li>
<li>ID</li>
<li>Integer</li>
<li>Long</li>
<li>Object</li>
<li>String</li>
<li>Time</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>activate</li>
<li>and</li>
<li>any</li>
<li>array</li>
<li>as</li>
<li>asc</li>
<li>autonomous</li>
<li>begin</li>
<li>bigdecimal</li>
<li>blob</li>
<li>break</li>
<li>bulk</li>
<li>by</li>
<li>byte</li>
<li>case</li>
<li>cast</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>collect</li>
<li>commit</li>
<li>const</li>
<li>continue</li>
<li>convertcurrency</li>
<li>currency</li>
<li>date</li>
<li>datetime</li>
<li>decimal</li>
<li>default</li>
<li>delete</li>
<li>desc</li>
<li>do</li>
<li>else</li>
<li>end</li>
<li>enum</li>
<li>exception</li>
<li>exit</li>
<li>export</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>from</li>
<li>future</li>
<li>global</li>
<li>goto</li>
<li>group</li>
<li>having</li>
<li>hint</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>inner</li>
<li>insert</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>into</li>
<li>join</li>
<li>last_90_days</li>
<li>last_month</li>
<li>last_n_days</li>
<li>last_week</li>
<li>like</li>
<li>limit</li>
<li>list</li>
<li>long</li>
<li>loop</li>
<li>map</li>
<li>merge</li>
<li>new</li>
<li>next_90_days</li>
<li>next_month</li>
<li>next_n_days</li>
<li>next_week</li>
<li>not</li>
<li>null</li>
<li>nulls</li>
<li>number</li>
<li>object</li>
<li>of</li>
<li>on</li>
<li>or</li>
<li>outer</li>
<li>override</li>
<li>package</li>
<li>parallel</li>
<li>pragma</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>retrieve</li>
<li>return</li>
<li>returning</li>
<li>rollback</li>
<li>savepoint</li>
<li>search</li>
<li>select</li>
<li>set</li>
<li>short</li>
<li>sort</li>
<li>stat</li>
<li>static</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>system</li>
<li>testmethod</li>
<li>then</li>
<li>this</li>
<li>this_month</li>
<li>this_week</li>
<li>throw</li>
<li>time</li>
<li>today</li>
<li>tolabel</li>
<li>tomorrow</li>
<li>transaction</li>
<li>trigger</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>undelete</li>
<li>update</li>
<li>upsert</li>
<li>using</li>
<li>virtual</li>
<li>webservice</li>
<li>when</li>
<li>where</li>
<li>while</li>
<li>yesterday</li>
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
