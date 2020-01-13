---
title: Config Options for swift5
sidebar_label: swift5
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift, Result, Combine are available.| |null|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.(default: false)| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|objcCompatible|Add additional properties and methods for Objective-C compatibility (default: false)| |null|
|podSource|Source information used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|podAuthors|Authors used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|lenientTypeCast|Accept and cast values for simple types (string-&gt;bool, string-&gt;int, int-&gt;string)| |false|
|library|Library template (sub-template) to use|<dl><dt>**urlsession**</dt><dd>[DEFAULT] HTTP client: URLSession</dd><dt>**alamofire**</dt><dd>HTTP client: Alamofire</dd><dl>|urlsession|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Character</li>
<li>Data</li>
<li>String</li>
<li>Double</li>
<li>Any</li>
<li>Int</li>
<li>URL</li>
<li>Date</li>
<li>Float</li>
<li>AnyObject</li>
<li>Decimal</li>
<li>Int64</li>
<li>Bool</li>
<li>Int32</li>
<li>Void</li>
<li>UUID</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>#elseif</li>
<li>#file</li>
<li>#imageLiteral</li>
<li>prefix</li>
<li>convenience</li>
<li>none</li>
<li>String</li>
<li>Int16</li>
<li>required</li>
<li>nil</li>
<li>CountableRange</li>
<li>Float32</li>
<li>Bool</li>
<li>else</li>
<li>let</li>
<li>catch</li>
<li>mutating</li>
<li>if</li>
<li>case</li>
<li>init</li>
<li>in</li>
<li>var</li>
<li>is</li>
<li>optional</li>
<li>infix</li>
<li>FUNCTION</li>
<li>enum</li>
<li>Float</li>
<li>as</li>
<li>Decodable</li>
<li>left</li>
<li>extension</li>
<li>internal</li>
<li>Set</li>
<li>lazy</li>
<li>guard</li>
<li>associatedtype</li>
<li>#available</li>
<li>Self</li>
<li>nonmutating</li>
<li>URL</li>
<li>weak</li>
<li>#function</li>
<li>default</li>
<li>Int32</li>
<li>get</li>
<li>where</li>
<li>typealias</li>
<li>override</li>
<li>Protocol</li>
<li>FILE</li>
<li>_</li>
<li>#selector</li>
<li>set</li>
<li>break</li>
<li>willSet</li>
<li>right</li>
<li>Int8</li>
<li>ErrorResponse</li>
<li>Type</li>
<li>AnyObject</li>
<li>Float64</li>
<li>deinit</li>
<li>throw</li>
<li>self</li>
<li>UInt</li>
<li>UInt32</li>
<li>open</li>
<li>return</li>
<li>#if</li>
<li>#column</li>
<li>COLUMN</li>
<li>#line</li>
<li>#endif</li>
<li>do</li>
<li>while</li>
<li>dynamicType</li>
<li>operator</li>
<li>precedence</li>
<li>StaticString</li>
<li>protocol</li>
<li>CountableClosedRange</li>
<li>continue</li>
<li>dynamic</li>
<li>associativity</li>
<li>Void</li>
<li>Unicode</li>
<li>static</li>
<li>#colorLiteral</li>
<li>subscript</li>
<li>indirect</li>
<li>Optional</li>
<li>Character</li>
<li>throws</li>
<li>Range</li>
<li>Double</li>
<li>fileprivate</li>
<li>true</li>
<li>final</li>
<li>try</li>
<li>Class</li>
<li>UInt16</li>
<li>rethrows</li>
<li>Float80</li>
<li>Encodable</li>
<li>#else</li>
<li>Dictionary</li>
<li>private</li>
<li>defer</li>
<li>import</li>
<li>ClosedRange</li>
<li>#fileLiteral</li>
<li>for</li>
<li>Any</li>
<li>Int</li>
<li>UInt8</li>
<li>switch</li>
<li>public</li>
<li>repeat</li>
<li>LINE</li>
<li>#sourceLocation</li>
<li>postfix</li>
<li>UInt64</li>
<li>class</li>
<li>false</li>
<li>Codable</li>
<li>Error</li>
<li>Data</li>
<li>unowned</li>
<li>Response</li>
<li>super</li>
<li>Array</li>
<li>inout</li>
<li>func</li>
<li>Int64</li>
<li>didSet</li>
<li>fallthrough</li>
<li>OptionSet</li>
</ul>
