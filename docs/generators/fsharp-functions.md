---
title: Config Options for fsharp-functions
sidebar_label: fsharp-functions
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|disallowAdditionalPropertiesIfNotPresent|Specify the behavior when the 'additionalProperties' keyword is not present in the OAS document. If false: the 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications. If true: when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.This setting is currently ignored for OAS 2.0 documents:  1) When the 'additionalProperties' keyword is not present in a 2.0 schema, additional properties are NOT allowed.  2) Boolean values of the 'additionalProperties' keyword are ignored. It's as if additional properties are NOT allowed.Note: the root cause are issues #1369 and #1371, which must be resolved in the swagger-parser project.|<dl><dt>**false**</dt><dd>The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.</dd><dt>**true**</dt><dd>when the 'additionalProperties' keyword is not present in a schema, the value of 'additionalProperties' is automatically set to false, i.e. no additional properties are allowed. Note: this mode is not compliant with the JSON schema specification. This is the original openapi-generator behavior.</dd></dl>|true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|legacyDiscriminatorBehavior|This flag is used by OpenAPITools codegen to influence the processing of the discriminator attribute in OpenAPI documents. This flag has no impact if the OAS document does not use the discriminator attribute. The default value of this flag is set in each language-specific code generator (e.g. Python, Java, go...)using the method toModelName. Note to developers supporting a language generator in OpenAPITools; to fully support the discriminator attribute as defined in the OAS specification 3.x, language generators should set this flag to true by default; however this requires updating the mustache templates to generate a language-specific discriminator lookup function that iterates over {{#mappedModels}} and does not iterate over {{children}}, {{#anyOf}}, or {{#oneOf}}.|<dl><dt>**true**</dt><dd>The mapping in the discriminator includes descendent schemas that allOf inherit from self and the discriminator mapping schemas in the OAS document.</dd><dt>**false**</dt><dd>The mapping in the discriminator includes any descendent schemas that allOf inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values, and the discriminator mapping schemas in the OAS document AND Codegen validates that oneOf and anyOf schemas contain the required discriminator and throws an error if the discriminator is missing.</dd></dl>|true|
|licenseName|The name of the license| |NoLicense|
|licenseUrl|The URL of the license| |http://localhost|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageGuid|The GUID that will be associated with the C# project| |null|
|packageName|F# module name (convention: Title.Case).| |OpenAPI|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageVersion|F# package version.| |1.0.0|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |OpenAPI/src|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|IDictionary|System.Collections.Generic|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|list|
|list|list|
|map|IDictionary|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Collection</li>
<li>DataTimeOffset</li>
<li>DateTime</li>
<li>Dictionary</li>
<li>Double</li>
<li>ICollection</li>
<li>Int32</li>
<li>Int64</li>
<li>List</li>
<li>String</li>
<li>System.IO.Stream</li>
<li>bool</li>
<li>byte[]</li>
<li>char</li>
<li>decimal</li>
<li>dict</li>
<li>double</li>
<li>float</li>
<li>float32</li>
<li>int</li>
<li>int16</li>
<li>int64</li>
<li>list</li>
<li>nativeint</li>
<li>obj</li>
<li>seq</li>
<li>single</li>
<li>string</li>
<li>uint16</li>
<li>uint32</li>
<li>uint64</li>
<li>unativeint</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>abstract</li>
<li>and</li>
<li>as</li>
<li>assert</li>
<li>async</li>
<li>await</li>
<li>base</li>
<li>begin</li>
<li>bool</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>checked</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>decimal</li>
<li>default</li>
<li>delegate</li>
<li>do</li>
<li>done</li>
<li>double</li>
<li>downcast</li>
<li>downto</li>
<li>dynamic</li>
<li>elif</li>
<li>else</li>
<li>end</li>
<li>enum</li>
<li>event</li>
<li>exception</li>
<li>explicit</li>
<li>extern</li>
<li>false</li>
<li>finally</li>
<li>fixed</li>
<li>float</li>
<li>for</li>
<li>foreach</li>
<li>fun</li>
<li>function</li>
<li>if</li>
<li>in</li>
<li>inherit</li>
<li>inline</li>
<li>int</li>
<li>interface</li>
<li>internal</li>
<li>is</li>
<li>lazy</li>
<li>let</li>
<li>let!</li>
<li>localVarFileParams</li>
<li>localVarFormParams</li>
<li>localVarHeaderParams</li>
<li>localVarHttpContentType</li>
<li>localVarHttpContentTypes</li>
<li>localVarHttpHeaderAccept</li>
<li>localVarHttpHeaderAccepts</li>
<li>localVarPath</li>
<li>localVarPathParams</li>
<li>localVarPostBody</li>
<li>localVarQueryParams</li>
<li>localVarResponse</li>
<li>localVarStatusCode</li>
<li>lock</li>
<li>match</li>
<li>match!</li>
<li>member</li>
<li>module</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>not</li>
<li>null</li>
<li>of</li>
<li>open</li>
<li>option</li>
<li>or</li>
<li>override</li>
<li>params</li>
<li>private</li>
<li>public</li>
<li>raise</li>
<li>rec</li>
<li>return</li>
<li>return!</li>
<li>sealed</li>
<li>select</li>
<li>static</li>
<li>string</li>
<li>struct</li>
<li>then</li>
<li>to</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>upcast</li>
<li>use</li>
<li>use!</li>
<li>val</li>
<li>void</li>
<li>volatile</li>
<li>when</li>
<li>while</li>
<li>with</li>
<li>yield</li>
<li>yield!</li>
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
|Readme|✗|ToolingExtension
|Model|✓|ToolingExtension
|Api|✓|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|✗|OAS2,OAS3
|BasePath|✗|OAS2,OAS3
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
|JSON|✓|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
