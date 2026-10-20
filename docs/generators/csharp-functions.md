---
title: Documentation for the csharp-functions Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | csharp-functions | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | C# | |
| generator default templating engine | mustache | |
| helpTxt | Creates Azure function templates on top of the models/converters created by the C# codegens. This function is contained in a partial class. Default Get/Create/Patch/Post etc. methods are created with an underscore prefix. The assumption is that when the function is implemented, the partial class will be completed with another partial class. The implementing code should be located in a method of the same name, only without the underscore prefix. If no such method is found then the function will throw a Not Implemented exception. This setup allows the endpoints to be specified in the schema at build time, and separated from the implementing function. | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|aspNetCoreIntegration|For the isolated worker (azureFunctionsVersion=v4-isolated) only: use ASP.NET Core integration (HttpRequest/IActionResult). Set false for the built-in model (HttpRequestData/HttpResponseData). Ignored for the in-process model.| |true|
|azureFunctionsVersion|Azure functions version: v4, v3|<dl><dt>**v4**</dt><dd>Azure Functions v4 (in-process model)</dd><dt>**v4-isolated**</dt><dd>Azure Functions v4 (.NET isolated worker model, net10.0)</dd><dt>**v3**</dt><dd>Azure Functions v3</dd></dl>|v4|
|buildTarget|Target to build an application or library|<dl><dt>**program**</dt><dd>Generate code for a standalone server</dd><dt>**library**</dt><dd>Generate code for a server abstract class library</dd></dl>|program|
|classModifier|Class Modifier for function classes: Empty string or abstract.| ||
|enumNameSuffix|Suffix that will be appended to all enum names.| |Enum|
|enumValueSuffix|Suffix that will be appended to all enum values.| |Enum|
|generateBody|Generates method body.| |true|
|generateOpenApiAttributes|For the isolated worker (azureFunctionsVersion=v4-isolated) only: emit OpenAPI metadata attributes ([OpenApiOperation], [OpenApiParameter], [OpenApiRequestBody], [OpenApiResponseWithBody]) on the generated functions. Requires the Microsoft.Azure.Functions.Worker.Extensions.OpenApi package.| |false|
|licenseName|The name of the license| |NoLicense|
|licenseUrl|The URL of the license| |http://localhost|
|modelClassModifier|Model Class Modifier can be nothing or partial| |partial|
|netCoreVersion|.NET Core version: 6.0, 5.0, 3.1, 3.0|<dl><dt>**3.0**</dt><dd>.NET Core 3.0</dd><dt>**3.1**</dt><dd>.NET Core 3.1</dd><dt>**5.0**</dt><dd>.NET Core 5.0</dd><dt>**6.0**</dt><dd>.NET Core 6.0</dd><dt>**10.0**</dt><dd>.NET 10.0 (isolated worker only)</dd></dl>|3.1|
|newtonsoftVersion|Version for Newtonsoft.Json for .NET Core 3.0+| |3.0.0|
|nullableReferenceTypes|Use nullable annotations in the project. Only supported on C# 8 / ASP.NET Core 3.1 or newer.| |false|
|operationIsAsync|Set methods to async or sync (default).| |false|
|operationModifier|Operation Modifier can be virtual or abstract|<dl><dt>**virtual**</dt><dd>Keep method virtual</dd><dt>**abstract**</dt><dd>Make method abstract</dd></dl>|virtual|
|operationResultTask|Set methods result to Task&lt;&gt;.| |false|
|packageAuthors|Specifies Authors property in the .NET Core project file.| |OpenAPI|
|packageCopyright|Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |No Copyright|
|packageDescription|Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |A library generated from a OpenAPI doc|
|packageGuid|The GUID that will be associated with the C# project| |null|
|packageName|C# package name (convention: Title.Case).| |Org.OpenAPITools|
|packageTitle|Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.| |OpenAPI Library|
|packageVersion|C# package version.| |1.0.0|
|returnICollection|Return ICollection&lt;T&gt; instead of the concrete type.| |false|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |src|
|useCollection|Deserialize array types to Collection&lt;T&gt; instead of List&lt;T&gt;.| |false|
|useDateTimeForDate|Use DateTime to model date properties even if DateOnly supported. (.net 6.0+ only)| |false|
|useDateTimeOffset|Use DateTimeOffset to model date-time properties| |false|
|useNewtonsoft|Uses the Newtonsoft JSON library.| |true|

## SUPPORTED VENDOR EXTENSIONS

| Extension name | Description | Applicable for | Default value |
| -------------- | ----------- | -------------- | ------------- |
|x-zero-based-enum|When used on an enum, the index will not be generated and the default numbering will be used, zero-based|MODEL|false


## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|List|
|list|List|
|map|Dictionary|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>Boolean</li>
<li>Collection</li>
<li>DateOnly</li>
<li>DateOnly?</li>
<li>DateTime</li>
<li>DateTime?</li>
<li>DateTimeOffset</li>
<li>DateTimeOffset?</li>
<li>Decimal</li>
<li>Dictionary</li>
<li>Double</li>
<li>Float</li>
<li>Guid</li>
<li>Guid?</li>
<li>ICollection</li>
<li>Int32</li>
<li>Int64</li>
<li>List</li>
<li>Object</li>
<li>String</li>
<li>System.IO.Stream</li>
<li>bool</li>
<li>bool?</li>
<li>byte[]</li>
<li>decimal</li>
<li>decimal?</li>
<li>double</li>
<li>double?</li>
<li>float</li>
<li>float?</li>
<li>int</li>
<li>int?</li>
<li>long</li>
<li>long?</li>
<li>string</li>
<li>uint</li>
<li>uint?</li>
<li>ulong</li>
<li>ulong?</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>Client</li>
<li>Configuration</li>
<li>Environment</li>
<li>OperatingSystem</li>
<li>TimeZone</li>
<li>Version</li>
<li>abstract</li>
<li>as</li>
<li>async</li>
<li>await</li>
<li>base</li>
<li>bool</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>checked</li>
<li>class</li>
<li>client</li>
<li>const</li>
<li>continue</li>
<li>decimal</li>
<li>default</li>
<li>delegate</li>
<li>do</li>
<li>double</li>
<li>dynamic</li>
<li>else</li>
<li>enum</li>
<li>event</li>
<li>explicit</li>
<li>extern</li>
<li>false</li>
<li>finally</li>
<li>fixed</li>
<li>float</li>
<li>for</li>
<li>foreach</li>
<li>goto</li>
<li>if</li>
<li>implicit</li>
<li>in</li>
<li>int</li>
<li>interface</li>
<li>internal</li>
<li>is</li>
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
<li>long</li>
<li>namespace</li>
<li>new</li>
<li>null</li>
<li>object</li>
<li>operator</li>
<li>out</li>
<li>override</li>
<li>parameter</li>
<li>params</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>readonly</li>
<li>ref</li>
<li>return</li>
<li>sbyte</li>
<li>sealed</li>
<li>short</li>
<li>sizeof</li>
<li>stackalloc</li>
<li>static</li>
<li>string</li>
<li>struct</li>
<li>switch</li>
<li>system</li>
<li>this</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typeof</li>
<li>uint</li>
<li>ulong</li>
<li>unchecked</li>
<li>unsafe</li>
<li>ushort</li>
<li>using</li>
<li>var</li>
<li>virtual</li>
<li>void</li>
<li>volatile</li>
<li>while</li>
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
|BearerToken|✓|OAS3
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
