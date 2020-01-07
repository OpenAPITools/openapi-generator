---
title: Config Options for javascript
sidebar_label: javascript
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sourceFolder|source folder for generated code| |src|
|invokerPackage|root package for generated code| |null|
|apiPackage|package for generated api classes| |null|
|modelPackage|package for generated models| |null|
|projectName|name of the project (Default: generated from info.title or &quot;openapi-js-client&quot;)| |null|
|moduleName|module name for AMD, Node or globals (Default: generated from &lt;projectName&gt;)| |null|
|projectDescription|description of the project (Default: using info.description or &quot;Client library of &lt;projectName&gt;&quot;)| |null|
|projectVersion|version of the project (Default: using info.version or &quot;1.0.0&quot;)| |null|
|licenseName|name of the license the project uses (Default: using info.license.name)| |null|
|usePromises|use Promises as return values from the client API, instead of superagent callbacks| |false|
|emitModelMethods|generate getters and setters for model properties| |false|
|emitJSDoc|generate JSDoc comments| |true|
|useInheritance|use JavaScript prototype chains &amp; delegation for inheritance| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|useES6|use JavaScript ES6 (ECMAScript 6) (beta). Default is ES6.| |true|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|
|list|Array|
|map|Object|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Array</li>
<li>Blob</li>
<li>Number</li>
<li>Object</li>
<li>String</li>
<li>Boolean</li>
<li>File</li>
<li>Date</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>date</li>
<li>synchronized</li>
<li>debugger</li>
<li>isfinite</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>hasownproperty</li>
<li>number</li>
<li>protected</li>
<li>continue</li>
<li>else</li>
<li>function</li>
<li>let</li>
<li>nan</li>
<li>catch</li>
<li>export</li>
<li>if</li>
<li>case</li>
<li>new</li>
<li>package</li>
<li>static</li>
<li>void</li>
<li>in</li>
<li>byte</li>
<li>double</li>
<li>var</li>
<li>finally</li>
<li>this</li>
<li>isprototypeof</li>
<li>throws</li>
<li>enum</li>
<li>eval</li>
<li>extends</li>
<li>null</li>
<li>transient</li>
<li>final</li>
<li>true</li>
<li>try</li>
<li>math</li>
<li>object</li>
<li>implements</li>
<li>private</li>
<li>const</li>
<li>import</li>
<li>string</li>
<li>valueof</li>
<li>for</li>
<li>interface</li>
<li>isnan</li>
<li>delete</li>
<li>long</li>
<li>switch</li>
<li>undefined</li>
<li>default</li>
<li>goto</li>
<li>public</li>
<li>native</li>
<li>array</li>
<li>yield</li>
<li>class</li>
<li>typeof</li>
<li>break</li>
<li>false</li>
<li>volatile</li>
<li>abstract</li>
<li>prototype</li>
<li>int</li>
<li>instanceof</li>
<li>super</li>
<li>with</li>
<li>boolean</li>
<li>throw</li>
<li>char</li>
<li>short</li>
<li>arguments</li>
<li>infinity</li>
<li>tostring</li>
<li>return</li>
</ul>
