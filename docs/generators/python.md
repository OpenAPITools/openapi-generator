---
title: Documentation for the python Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | python | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | CLIENT | |
| generator language | Python | |
| generator language version | 3.10+ | |
| generator default templating engine | mustache | |
| helpTxt | Generates a Python client library. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|buildSystem|Build system to use in pyproject.toml (setuptools, hatchling).| |setuptools|
|compatibleWithPythonLegacy|Enable compatibility with python-legacy. Currently, generated model field aliases preserve normalized Python constructor names while accepting wire names, and to_dict() emits every declared field, using None for missing attributes, under public names by default and wire names with serialize=True. Generic models expose openapi_types and attribute_map, reject unknown constructor keys, and use legacy display and equality helpers. Container conversion is limited to immediate list elements and dictionary values, matching python-legacy. Synchronous urllib3 operations keep async_req, _preload_content, tuple with_http_info() behavior, and integer _request_timeout inputs. JSON and request serialization remain unchanged.| |false|
|dateFormat|date format for query parameters| |%Y-%m-%d|
|datetimeFormat|datetime format for query parameters| |%Y-%m-%dT%H:%M:%S%z|
|disallowAdditionalPropertiesIfNotPresent|If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.</dd></dl>|true|
|generateSourceCodeOnly|Specifies that only a library source code is to be generated.| |false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|lazyImports|Enable lazy imports.| |false|
|library|library template (sub-template) to use: asyncio, tornado (deprecated), urllib3, httpx| |urllib3|
|mapNumberTo|Map number to Union[StrictFloat, StrictInt], StrictFloat, float or Decimal.| |Union[StrictFloat, StrictInt]|
|packageName|python package name (convention: snake_case).| |openapi_client|
|packageUrl|python package URL.| |null|
|packageVersion|python package version.| |1.0.0|
|poetry1|Fallback to formatting pyproject.toml to Poetry 1.x format.| |null|
|projectName|python project name in setup.py (e.g. petstore-api).| |null|
|recursionLimit|Set the recursion limit. If not set, use the system default value.| |null|
|setEnsureAsciiToFalse|When set to true, add `ensure_ascii=False` in json.dumps when creating the HTTP request body.| |false|
|supportHttpxSync|Generate synchronous '_sync' variants of each API method (httpx library only). Each '_sync' method simply calls the corresponding async method and waits for its completion, so both synchronous and asynchronous methods are available from the same API class.| |false|
|useIndependentImplicitClients|Give API instances without an explicit or registered default ApiClient an owned client with a copied Configuration.| |false|
|useOneOfDiscriminatorLookup|Use the discriminator's mapping in oneOf to speed up the model lookup. IMPORTANT: Validation (e.g. one and only one match in oneOf's schemas) will be skipped.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Dict</li>
<li>List</li>
<li>UUID</li>
<li>bool</li>
<li>bytes</li>
<li>date</li>
<li>datetime</li>
<li>decimal.Decimal</li>
<li>dict</li>
<li>float</li>
<li>int</li>
<li>list</li>
<li>none_type</li>
<li>object</li>
<li>str</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>all_params</li>
<li>and</li>
<li>as</li>
<li>assert</li>
<li>async</li>
<li>auth_settings</li>
<li>await</li>
<li>base64</li>
<li>body_params</li>
<li>break</li>
<li>class</li>
<li>continue</li>
<li>date</li>
<li>def</li>
<li>del</li>
<li>elif</li>
<li>else</li>
<li>except</li>
<li>exec</li>
<li>false</li>
<li>field</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>form_params</li>
<li>from</li>
<li>from_dict</li>
<li>from_json</li>
<li>global</li>
<li>header_params</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>is</li>
<li>json</li>
<li>lambda</li>
<li>local_var_files</li>
<li>model_computed_fields</li>
<li>model_config</li>
<li>model_construct</li>
<li>model_copy</li>
<li>model_dump</li>
<li>model_dump_json</li>
<li>model_extra</li>
<li>model_fields</li>
<li>model_fields_set</li>
<li>model_json_schema</li>
<li>model_parametrized_name</li>
<li>model_post_init</li>
<li>model_rebuild</li>
<li>model_validate</li>
<li>model_validate_json</li>
<li>model_validate_strings</li>
<li>none</li>
<li>nonlocal</li>
<li>not</li>
<li>or</li>
<li>pass</li>
<li>path_params</li>
<li>print</li>
<li>property</li>
<li>query_params</li>
<li>raise</li>
<li>resource_path</li>
<li>return</li>
<li>schema</li>
<li>self</li>
<li>to_dict</li>
<li>to_json</li>
<li>to_str</li>
<li>true</li>
<li>try</li>
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
|Uuid|✓|
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
|Polymorphism|✓|OAS2,OAS3
|Union|✗|OAS3
|allOf|✓|OAS2,OAS3
|anyOf|✓|OAS3
|oneOf|✓|OAS3
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
|OAuth2_AuthorizationCode|✗|OAS2,OAS3
|SignatureAuth|✓|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✓|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✓|OAS2,OAS3
