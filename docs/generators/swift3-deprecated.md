---
title: Config Options for swift3-deprecated
sidebar_label: swift3-deprecated
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectName|Project name in Xcode| |null|
|responseAs|Optionally use libraries to manage response.  Currently PromiseKit, RxSwift are available.| |null|
|unwrapRequired|Treat 'required' properties in response as non-optional (which would crash the app if api returns null as opposed to required option specified in json schema| |null|
|objcCompatible|Add additional properties and methods for Objective-C compatibility (default: false)| |null|
|podSource|Source information used for Podspec| |null|
|podVersion|Version used for Podspec| |null|
|podAuthors|Authors used for Podspec| |null|
|podSocialMediaURL|Social Media URL used for Podspec| |null|
|podDocsetURL|Docset URL used for Podspec| |null|
|podLicense|License used for Podspec| |null|
|podHomepage|Homepage used for Podspec| |null|
|podSummary|Summary used for Podspec| |null|
|podDescription|Description used for Podspec| |null|
|podScreenshots|Screenshots used for Podspec| |null|
|podDocumentationURL|Documentation URL used for Podspec| |null|
|swiftUseApiNamespace|Flag to make all the API classes inner-class of {{projectName}}API| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|lenientTypeCast|Accept and cast values for simple types (string-&gt;bool, string-&gt;int, int-&gt;string)| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Float</li>
<li>AnyObject</li>
<li>Character</li>
<li>Int64</li>
<li>Bool</li>
<li>Int32</li>
<li>String</li>
<li>Void</li>
<li>Double</li>
<li>Any</li>
<li>Int</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>prefix</li>
<li>COLUMN</li>
<li>convenience</li>
<li>String</li>
<li>do</li>
<li>none</li>
<li>dynamicType</li>
<li>while</li>
<li>operator</li>
<li>precedence</li>
<li>required</li>
<li>nil</li>
<li>protocol</li>
<li>Bool</li>
<li>continue</li>
<li>else</li>
<li>dynamic</li>
<li>let</li>
<li>mutating</li>
<li>Void</li>
<li>associativity</li>
<li>if</li>
<li>case</li>
<li>init</li>
<li>static</li>
<li>Character</li>
<li>subscript</li>
<li>in</li>
<li>var</li>
<li>is</li>
<li>optional</li>
<li>infix</li>
<li>Double</li>
<li>FUNCTION</li>
<li>enum</li>
<li>Float</li>
<li>as</li>
<li>left</li>
<li>final</li>
<li>true</li>
<li>Class</li>
<li>extension</li>
<li>internal</li>
<li>private</li>
<li>import</li>
<li>lazy</li>
<li>for</li>
<li>Self</li>
<li>Any</li>
<li>nonmutating</li>
<li>Int</li>
<li>URL</li>
<li>switch</li>
<li>weak</li>
<li>default</li>
<li>public</li>
<li>Int32</li>
<li>get</li>
<li>LINE</li>
<li>where</li>
<li>override</li>
<li>postfix</li>
<li>typealias</li>
<li>FILE</li>
<li>Protocol</li>
<li>class</li>
<li>set</li>
<li>break</li>
<li>false</li>
<li>Error</li>
<li>Data</li>
<li>right</li>
<li>unowned</li>
<li>ErrorResponse</li>
<li>Response</li>
<li>super</li>
<li>AnyObject</li>
<li>inout</li>
<li>Type</li>
<li>deinit</li>
<li>func</li>
<li>Int64</li>
<li>didSet</li>
<li>self</li>
<li>fallthrough</li>
<li>return</li>
</ul>
