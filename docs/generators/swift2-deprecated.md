---
title: Config Options for swift2-deprecated
sidebar_label: swift2-deprecated
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
<li>Int</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>struct</li>
<li>prefix</li>
<li>convenience</li>
<li>String</li>
<li>none</li>
<li>required</li>
<li>nil</li>
<li>Bool</li>
<li>else</li>
<li>let</li>
<li>mutating</li>
<li>catch</li>
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
<li>left</li>
<li>extension</li>
<li>internal</li>
<li>lazy</li>
<li>guard</li>
<li>Self</li>
<li>nonmutating</li>
<li>weak</li>
<li>default</li>
<li>Int32</li>
<li>get</li>
<li>where</li>
<li>override</li>
<li>typealias</li>
<li>FILE</li>
<li>Protocol</li>
<li>set</li>
<li>break</li>
<li>willSet</li>
<li>right</li>
<li>ErrorResponse</li>
<li>AnyObject</li>
<li>Type</li>
<li>deinit</li>
<li>throw</li>
<li>self</li>
<li>return</li>
<li>open</li>
<li>COLUMN</li>
<li>do</li>
<li>dynamicType</li>
<li>while</li>
<li>operator</li>
<li>precedence</li>
<li>protocol</li>
<li>continue</li>
<li>dynamic</li>
<li>Void</li>
<li>associativity</li>
<li>static</li>
<li>Character</li>
<li>subscript</li>
<li>indirect</li>
<li>throws</li>
<li>Double</li>
<li>fileprivate</li>
<li>final</li>
<li>true</li>
<li>Class</li>
<li>try</li>
<li>rethrows</li>
<li>private</li>
<li>defer</li>
<li>import</li>
<li>for</li>
<li>Any</li>
<li>Int</li>
<li>switch</li>
<li>public</li>
<li>LINE</li>
<li>repeat</li>
<li>postfix</li>
<li>class</li>
<li>false</li>
<li>Data</li>
<li>unowned</li>
<li>super</li>
<li>inout</li>
<li>func</li>
<li>Int64</li>
<li>didSet</li>
<li>fallthrough</li>
</ul>
