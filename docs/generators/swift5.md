---
title: Config Options for swift5
sidebar_label: swift5
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|lenientTypeCast|Accept and cast values for simple types (string-&gt;bool, string-&gt;int, int-&gt;string)| |false|
|library|Library template (sub-template) to use|<dl><dt>**urlsession**</dt><dd>[DEFAULT] HTTP client: URLSession</dd><dt>**alamofire**</dt><dd>HTTP client: Alamofire</dd><dl>|urlsession|
|nonPublicApi|Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.(default: false)| |null|
|objcCompatible|Add additional properties and methods for Objective-C compatibility (default: false)| |null|
|podAuthors|Authors used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podSource|Source information used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift, Result, Combine are available.| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Any</li>
<li>AnyObject</li>
<li>Bool</li>
<li>Character</li>
<li>Data</li>
<li>Date</li>
<li>Decimal</li>
<li>Double</li>
<li>Float</li>
<li>Int</li>
<li>Int32</li>
<li>Int64</li>
<li>String</li>
<li>URL</li>
<li>UUID</li>
<li>Void</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>#available</li>
<li>#colorLiteral</li>
<li>#column</li>
<li>#else</li>
<li>#elseif</li>
<li>#endif</li>
<li>#file</li>
<li>#fileLiteral</li>
<li>#function</li>
<li>#if</li>
<li>#imageLiteral</li>
<li>#line</li>
<li>#selector</li>
<li>#sourceLocation</li>
<li>Any</li>
<li>AnyObject</li>
<li>Array</li>
<li>Bool</li>
<li>COLUMN</li>
<li>Character</li>
<li>Class</li>
<li>ClosedRange</li>
<li>Codable</li>
<li>CountableClosedRange</li>
<li>CountableRange</li>
<li>Data</li>
<li>Decodable</li>
<li>Dictionary</li>
<li>Double</li>
<li>Encodable</li>
<li>Error</li>
<li>ErrorResponse</li>
<li>FILE</li>
<li>FUNCTION</li>
<li>Float</li>
<li>Float32</li>
<li>Float64</li>
<li>Float80</li>
<li>Int</li>
<li>Int16</li>
<li>Int32</li>
<li>Int64</li>
<li>Int8</li>
<li>LINE</li>
<li>OptionSet</li>
<li>Optional</li>
<li>Protocol</li>
<li>Range</li>
<li>Response</li>
<li>Self</li>
<li>Set</li>
<li>StaticString</li>
<li>String</li>
<li>Type</li>
<li>UInt</li>
<li>UInt16</li>
<li>UInt32</li>
<li>UInt64</li>
<li>UInt8</li>
<li>URL</li>
<li>Unicode</li>
<li>Void</li>
<li>_</li>
<li>as</li>
<li>associatedtype</li>
<li>associativity</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>continue</li>
<li>convenience</li>
<li>default</li>
<li>defer</li>
<li>deinit</li>
<li>didSet</li>
<li>do</li>
<li>dynamic</li>
<li>dynamicType</li>
<li>else</li>
<li>enum</li>
<li>extension</li>
<li>fallthrough</li>
<li>false</li>
<li>fileprivate</li>
<li>final</li>
<li>for</li>
<li>func</li>
<li>get</li>
<li>guard</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>indirect</li>
<li>infix</li>
<li>init</li>
<li>inout</li>
<li>internal</li>
<li>is</li>
<li>lazy</li>
<li>left</li>
<li>let</li>
<li>mutating</li>
<li>nil</li>
<li>none</li>
<li>nonmutating</li>
<li>open</li>
<li>operator</li>
<li>optional</li>
<li>override</li>
<li>postfix</li>
<li>precedence</li>
<li>prefix</li>
<li>private</li>
<li>protocol</li>
<li>public</li>
<li>repeat</li>
<li>required</li>
<li>rethrows</li>
<li>return</li>
<li>right</li>
<li>self</li>
<li>set</li>
<li>static</li>
<li>struct</li>
<li>subscript</li>
<li>super</li>
<li>switch</li>
<li>throw</li>
<li>throws</li>
<li>true</li>
<li>try</li>
<li>typealias</li>
<li>unowned</li>
<li>var</li>
<li>weak</li>
<li>where</li>
<li>while</li>
<li>willSet</li>
</ul>
