---
title: Config Options for javascript
sidebar_label: javascript
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |null|
|emitJSDoc|generate JSDoc comments| |true|
|emitModelMethods|generate getters and setters for model properties| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|invokerPackage|root package for generated code| |null|
|licenseName|name of the license the project uses (Default: using info.license.name)| |null|
|modelPackage|package for generated models| |null|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|moduleName|module name for AMD, Node or globals (Default: generated from &lt;projectName&gt;)| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|projectDescription|description of the project (Default: using info.description or &quot;Client library of &lt;projectName&gt;&quot;)| |null|
|projectName|name of the project (Default: generated from info.title or &quot;openapi-js-client&quot;)| |null|
|projectVersion|version of the project (Default: using info.version or &quot;1.0.0&quot;)| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |src|
|useES6|use JavaScript ES6 (ECMAScript 6) (beta). Default is ES6.| |true|
|useInheritance|use JavaScript prototype chains &amp; delegation for inheritance| |true|
|usePromises|use Promises as return values from the client API, instead of superagent callbacks| |false|

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
<li>Boolean</li>
<li>Date</li>
<li>File</li>
<li>Number</li>
<li>Object</li>
<li>String</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>arguments</li>
<li>array</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>date</li>
<li>debugger</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>eval</li>
<li>export</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>function</li>
<li>goto</li>
<li>hasownproperty</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>infinity</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>isfinite</li>
<li>isnan</li>
<li>isprototypeof</li>
<li>let</li>
<li>long</li>
<li>math</li>
<li>nan</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>number</li>
<li>object</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>prototype</li>
<li>public</li>
<li>return</li>
<li>short</li>
<li>static</li>
<li>string</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>this</li>
<li>throw</li>
<li>throws</li>
<li>tostring</li>
<li>transient</li>
<li>true</li>
<li>try</li>
<li>typeof</li>
<li>undefined</li>
<li>valueof</li>
<li>var</li>
<li>void</li>
<li>volatile</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
