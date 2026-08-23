---
title: Documentation for the cpp-boost-beast-client Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | cpp-boost-beast-client | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | C++ | |
| generator default templating engine | mustache | |
| helpTxt | Generates a cpp-boost-beast client. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiPackage|C++ namespace for apis (convention: name.space.api).| |org.openapitools.client.api|
|compileWithValidation|Enable decode-time oneOf/anyOf/discriminator branch validation. Representation checks such as finite numeric conversion, integer range, and required properties remain active when disabled.| |true|
|formatAssertionPolicy|Schema `format` is annotation-only. The generator rejects unsupported assertion modes rather than changing branch match counts.|<dl><dt>**annotation**</dt><dd>Record format as an annotation</dd></dl>|annotation|
|modelPackage|C++ namespace for models (convention: name.space.model).| |org.openapitools.client.model|
|packageName|C++ package and library name.| |CppBoostBeastOpenAPIClient|
|sseSchemaMode|SSE schema interpretation mode for text/event-stream responses. 'representation' (default): the response schema describes the text/event-stream media representation; generate framed events with raw data strings, event type, id, and retry fields. 'jsonEventData': the response schema describes each JSON data field; decode each event's data payload against the schema. Use the x-sse-event-data-schema vendor extension for per-operation opt-in to typed event-data decoding.|<dl><dt>**representation**</dt><dd>Strict mode &mdash; schema describes media representation</dd><dt>**jsonEventData**</dt><dd>Schema describes each JSON event data payload</dd></dl>|representation|

## OAS 3.1 EXACT NUMERIC VALIDATION

Generated validators preserve each JSON number's original token and evaluate
numeric bounds, mathematical-integer membership, numeric `enum`/`const`, and
`multipleOf` with an arbitrary-precision decimal representation. Both the
mantissa and base-10 exponent are arbitrary precision; tokens such as
`1e2147483648` therefore remain valid inputs and never narrow through a
fixed-width exponent or `double` during validation.

One explicit resource limit applies: an individual JSON number token may be at
most 4096 bytes. Longer tokens fail with `std::length_error`. The legacy
`ExactNumber::add` and `divmod` helpers likewise reject decimal expansions over
4096 places, while generated range and `multipleOf` validation use non-expanding
algorithms. Public model storage is unchanged (`double`, `std::int32_t`, or
`std::int64_t` as mapped below), so exact validation does not imply lossless
round-tripping after a value has been converted into and mutated through a model.

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|AnyType|#include &quot;AnyType.h&quot;|
|Null|#include &lt;cstddef&gt;|
|boost::json::value|#include &lt;boost/json.hpp&gt;|
|int32_t|#include &lt;cstdint&gt;|
|int64_t|#include &lt;cstdint&gt;|
|std::map|#include &lt;map&gt;|
|std::monostate|#include &lt;variant&gt;|
|std::nullptr_t|#include &lt;cstddef&gt;|
|std::optional|#include &lt;optional&gt;|
|std::shared_ptr|#include &lt;memory&gt;|
|std::string|#include &lt;string&gt;|
|std::variant|#include &lt;variant&gt;|
|std::vector|#include &lt;vector&gt;|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>bool</li>
<li>char</li>
<li>double</li>
<li>float</li>
<li>int</li>
<li>long</li>
<li>std::int32_t</li>
<li>std::int64_t</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>NULL</li>
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
|Decimal|✗|ToolingExtension
|String|✓|OAS2,OAS3
|Byte|✗|OAS2,OAS3
|Binary|✗|OAS2,OAS3
|Boolean|✓|OAS2,OAS3
|Date|✗|OAS2,OAS3
|DateTime|✗|OAS2,OAS3
|Password|✗|OAS2,OAS3
|File|✓|OAS2
|Uuid|✗|
|Array|✓|OAS2,OAS3
|Null|✓|OAS3
|AnyType|✓|OAS2,OAS3
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
|MultiServer|✓|OAS3
|ParameterizedServer|✗|OAS3
|ParameterStyling|✓|OAS3
|Callbacks|✓|OAS3
|LinkObjects|✓|OAS3

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
|Union|✓|OAS3
|allOf|✓|OAS2,OAS3
|anyOf|✓|OAS3
|oneOf|✓|OAS3
|not|✓|OAS3

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
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
