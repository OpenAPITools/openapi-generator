---
title: Config Options for javascript-flowtyped
sidebar_label: javascript-flowtyped
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |PascalCase|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|npmName|The name under which you want to publish generated npm package. Required to generate a full package| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|npmVersion|The version of your npm package. If not provided, using the version from the OpenAPI specification file.| |1.0.0|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|snapshot|When setting this property to true, the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|supportsES6|Generate code that conforms to ES6.| |false|

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
<li>Date</li>
<li>File</li>
<li>Object</li>
<li>boolean</li>
<li>number</li>
<li>string</li>
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
<li>formparams</li>
<li>function</li>
<li>goto</li>
<li>hasownproperty</li>
<li>headerparams</li>
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
<li>queryparameters</li>
<li>requestoptions</li>
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
<li>useformdata</li>
<li>valueof</li>
<li>var</li>
<li>varlocaldeferred</li>
<li>varlocalpath</li>
<li>void</li>
<li>volatile</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
