---
title: Config Options for typescript-angular
sidebar_label: typescript-angular
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiModulePrefix|The prefix of the generated ApiModule.| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|enumNameSuffix|Suffix that will be appended to all enum names. A special 'v4-compat' value enables the backward-compatible behavior (as pre v4.2.3)| |v4-compat|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |PascalCase|
|fileNaming|Naming convention for the output files: 'camelCase', 'kebab-case'.| |camelCase|
|modelFileSuffix|The suffix of the file of the generated model (model&lt;suffix&gt;.ts).| |null|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|modelSuffix|The suffix of the generated model.| |null|
|ngVersion|The version of Angular.| |8.0.0|
|npmName|The name under which you want to publish generated npm package. Required to generate a full package| |null|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|npmVersion|The version of your npm package. If not provided, using the version from the OpenAPI specification file.| |1.0.0|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|providedInRoot|Use this property to provide Injectables in root (it is only valid in angular version greater or equal to 6.0.0).| |false|
|serviceFileSuffix|The suffix of the file of the generated service (service&lt;suffix&gt;.ts).| |.service|
|serviceSuffix|The suffix of the generated service.| |Service|
|snapshot|When setting this property to true, the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|stringEnums|Generate string enums instead of objects for enum values.| |false|
|supportsES6|Generate code that conforms to ES6.| |false|
|taggedUnions|Use discriminators to create tagged unions instead of extending interfaces.| |false|
|useSingleRequestParameter|Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.| |false|
|withInterfaces|Setting this property to true will generate interfaces next to the default class implementations.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Array</li>
<li>Blob</li>
<li>Boolean</li>
<li>Date</li>
<li>Double</li>
<li>Error</li>
<li>File</li>
<li>Float</li>
<li>Integer</li>
<li>Long</li>
<li>Map</li>
<li>Object</li>
<li>String</li>
<li>any</li>
<li>boolean</li>
<li>number</li>
<li>object</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>await</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>debugger</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>export</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>formParams</li>
<li>function</li>
<li>goto</li>
<li>headerParams</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>in</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>let</li>
<li>long</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>queryParameters</li>
<li>requestOptions</li>
<li>return</li>
<li>short</li>
<li>static</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>this</li>
<li>throw</li>
<li>transient</li>
<li>true</li>
<li>try</li>
<li>typeof</li>
<li>useFormData</li>
<li>var</li>
<li>varLocalDeferred</li>
<li>varLocalPath</li>
<li>void</li>
<li>volatile</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
