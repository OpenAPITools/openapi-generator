---
title: Documentation for the cpp-oatpp-server Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | cpp-oatpp-server | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | C++ | |
| generator default templating engine | mustache | |
| helpTxt | Generates a C++ API server (based on oat++) | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|addExternalLibs|Add the Possibility to fetch and compile external Libraries needed by this Framework.| |true|
|reservedWordPrefix|Prefix to prepend to reserved words in order to avoid conflicts| |r_|
|variableNameFirstCharacterUppercase|Make first character of variable name uppercase (eg. value -&gt; Value)| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>oatpp::String</li>
<li>oatpp::Boolean</li>
<li>oatpp::Int32</li>
<li>oatpp::Int64</li>
<li>oatpp::Vector</li>
<li>oatpp::Fields</li>
<li>oatpp::UnorderedSet</li>
<li>oatpp::Object</li>
<li>oatpp::Float64</li>
<li>oatpp::Any</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>alignas</li>
<li>alignof</li>
<li>and</li>
<li>and_eq</li>
<li>asm</li>
<li>auto</li>
<li>bitand</li>
<li>bitor</li>
<li>bool</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>char16_t</li>
<li>char32_t</li>
<li>class</li>
<li>compl</li>
<li>concept</li>
<li>const</li>
<li>const_cast</li>
<li>constexpr</li>
<li>continue</li>
<li>decltype</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>dynamic_cast</li>
<li>else</li>
<li>enum</li>
<li>explicit</li>
<li>export</li>
<li>extern</li>
<li>false</li>
<li>float</li>
<li>for</li>
<li>friend</li>
<li>goto</li>
<li>if</li>
<li>inline</li>
<li>int</li>
<li>linux</li>
<li>long</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>noexcept</li>
<li>not</li>
<li>not_eq</li>
<li>nullptr</li>
<li>operator</li>
<li>or</li>
<li>or_eq</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>register</li>
<li>reinterpret_cast</li>
<li>requires</li>
<li>return</li>
<li>short</li>
<li>signed</li>
<li>sizeof</li>
<li>static</li>
<li>static_assert</li>
<li>static_cast</li>
<li>struct</li>
<li>switch</li>
<li>template</li>
<li>this</li>
<li>thread_local</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typedef</li>
<li>typeid</li>
<li>typename</li>
<li>union</li>
<li>unsigned</li>
<li>using</li>
<li>virtual</li>
<li>void</li>
<li>volatile</li>
<li>wchar_t</li>
<li>while</li>
<li>xor</li>
<li>xor_eq</li>
</ul>

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|âœ—|ToolingExtension
|Authorizations|âœ—|ToolingExtension
|UserAgent|âœ—|ToolingExtension
|MockServer|âœ—|ToolingExtension

### Data Type Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Custom|âœ—|OAS2,OAS3
|Int32|âœ“|OAS2,OAS3
|Int64|âœ“|OAS2,OAS3
|Float|âœ“|OAS2,OAS3
|Double|âœ“|OAS2,OAS3
|Decimal|âœ“|ToolingExtension
|String|âœ“|OAS2,OAS3
|Byte|âœ“|OAS2,OAS3
|Binary|âœ“|OAS2,OAS3
|Boolean|âœ“|OAS2,OAS3
|Date|âœ“|OAS2,OAS3
|DateTime|âœ“|OAS2,OAS3
|Password|âœ“|OAS2,OAS3
|File|âœ“|OAS2
|Uuid|âœ—|
|Array|âœ“|OAS2,OAS3
|Null|âœ—|OAS3
|AnyType|âœ—|OAS2,OAS3
|Object|âœ“|OAS2,OAS3
|Maps|âœ“|ToolingExtension
|CollectionFormat|âœ“|OAS2
|CollectionFormatMulti|âœ“|OAS2
|Enum|âœ“|OAS2,OAS3
|ArrayOfEnum|âœ“|ToolingExtension
|ArrayOfModel|âœ“|ToolingExtension
|ArrayOfCollectionOfPrimitives|âœ“|ToolingExtension
|ArrayOfCollectionOfModel|âœ“|ToolingExtension
|ArrayOfCollectionOfEnum|âœ“|ToolingExtension
|MapOfEnum|âœ“|ToolingExtension
|MapOfModel|âœ“|ToolingExtension
|MapOfCollectionOfPrimitives|âœ“|ToolingExtension
|MapOfCollectionOfModel|âœ“|ToolingExtension
|MapOfCollectionOfEnum|âœ“|ToolingExtension

### Documentation Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Readme|âœ“|ToolingExtension
|Model|âœ“|ToolingExtension
|Api|âœ“|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|âœ“|OAS2,OAS3
|BasePath|âœ“|OAS2,OAS3
|Info|âœ“|OAS2,OAS3
|Schemes|âœ—|OAS2,OAS3
|PartialSchemes|âœ“|OAS2,OAS3
|Consumes|âœ“|OAS2
|Produces|âœ“|OAS2
|ExternalDocumentation|âœ“|OAS2,OAS3
|Examples|âœ“|OAS2,OAS3
|XMLStructureDefinitions|âœ—|OAS2,OAS3
|MultiServer|âœ—|OAS3
|ParameterizedServer|âœ—|OAS3
|ParameterStyling|âœ—|OAS3
|Callbacks|âœ—|OAS3
|LinkObjects|âœ—|OAS3

### Parameter Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Path|âœ“|OAS2,OAS3
|Query|âœ“|OAS2,OAS3
|Header|âœ“|OAS2,OAS3
|Body|âœ“|OAS2
|FormUnencoded|âœ“|OAS2
|FormMultipart|âœ“|OAS2
|Cookie|âœ—|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|âœ“|OAS2,OAS3
|Composite|âœ“|OAS2,OAS3
|Polymorphism|âœ—|OAS2,OAS3
|Union|âœ—|OAS3
|allOf|âœ—|OAS2,OAS3
|anyOf|âœ—|OAS3
|oneOf|âœ—|OAS3
|not|âœ—|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|âœ—|OAS2,OAS3
|ApiKey|âœ—|OAS2,OAS3
|OpenIDConnect|âœ—|OAS3
|BearerToken|âœ—|OAS3
|OAuth2_Implicit|âœ—|OAS2,OAS3
|OAuth2_Password|âœ—|OAS2,OAS3
|OAuth2_ClientCredentials|âœ—|OAS2,OAS3
|OAuth2_AuthorizationCode|âœ—|OAS2,OAS3

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|âœ“|OAS2,OAS3
|XML|âœ“|OAS2,OAS3
|PROTOBUF|âœ—|ToolingExtension
|Custom|âœ—|OAS2,OAS3
