---
title: Config Options for objc
sidebar_label: objc
---

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|authorEmail|Email to use in the podspec file.| |team@openapitools.org|
|authorName|Name to use in the podspec file.| |OpenAPI|
|classPrefix|prefix for generated classes (convention: Abbreviation of pod name e.g. `HN` for `HackerNews`).`| |OAI|
|coreData|Should generate core data models| |false|
|gitRepoURL|URL for the git repo where this podspec should point to.| |https://github.com/openapitools/openapi-generator|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|podName|cocoapods package name (convention: CameCase).| |OpenAPIClient|
|podVersion|cocoapods package version.| |1.0.0|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|NSMutableArray|
|map|NSMutableDictionary|


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>BOOL</li>
<li>NSData</li>
<li>NSDate</li>
<li>NSNumber</li>
<li>NSObject</li>
<li>NSString</li>
<li>NSURL</li>
<li>bool</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>_packed</li>
<li>authsettings</li>
<li>auto</li>
<li>bodyparam</li>
<li>break</li>
<li>case</li>
<li>cgfloat</li>
<li>char</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>default</li>
<li>description</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>extern</li>
<li>float</li>
<li>for</li>
<li>formparams</li>
<li>goto</li>
<li>headerparams</li>
<li>id</li>
<li>if</li>
<li>implementation</li>
<li>int</li>
<li>interface</li>
<li>localvarfiles</li>
<li>long</li>
<li>nonatomic</li>
<li>nsinteger</li>
<li>nsnumber</li>
<li>nsobject</li>
<li>pathparams</li>
<li>property</li>
<li>protocol</li>
<li>queryparams</li>
<li>readonly</li>
<li>readwrite</li>
<li>register</li>
<li>requestcontenttype</li>
<li>resourcepath</li>
<li>responsecontenttype</li>
<li>retain</li>
<li>return</li>
<li>short</li>
<li>signed</li>
<li>sizeof</li>
<li>static</li>
<li>strong</li>
<li>struct</li>
<li>switch</li>
<li>typedef</li>
<li>union</li>
<li>unsafe_unretained</li>
<li>unsigned</li>
<li>void</li>
<li>volatile</li>
<li>weak</li>
<li>while</li>
</ul>

## FEATURE SET


### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✓|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✓|ToolingExtension
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
|Polymorphism|✓|OAS2,OAS3
|Union|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✓|OAS2,OAS3
|ApiKey|✓|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✓|OAS2,OAS3
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
