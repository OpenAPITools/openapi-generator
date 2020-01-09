---
title: Config Options for typescript-redux-query
sidebar_label: typescript-redux-query
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|
|supportsES6|Generate code that conforms to ES6.| |false|
|npmName|The name under which you want to publish generated npm package. Required to generate a full package| |null|
|npmVersion|The version of your npm package. If not provided, using the version from the OpenAPI specification file.| |1.0.0|
|snapshot|When setting this property to true, the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm| |false|
|npmRepository|Use this property to set an url your private npmRepo in the package.json| |null|
|withInterfaces|Setting this property to true will generate interfaces next to the default class implementations.| |false|
|useSingleRequestParameter|Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|Array|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>string</li>
<li>Error</li>
<li>String</li>
<li>Double</li>
<li>any</li>
<li>Date</li>
<li>Integer</li>
<li>Array</li>
<li>Float</li>
<li>number</li>
<li>boolean</li>
<li>Long</li>
<li>Object</li>
<li>Boolean</li>
<li>File</li>
<li>Map</li>
<li>object</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>ModelPropertyNaming</li>
<li>synchronized</li>
<li>HTTPHeaders</li>
<li>Configuration</li>
<li>debugger</li>
<li>COLLECTION_FORMATS</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>BaseAPI</li>
<li>ApiResponse</li>
<li>varLocalPath</li>
<li>headerParams</li>
<li>queryParameters</li>
<li>protected</li>
<li>HTTPMethod</li>
<li>continue</li>
<li>else</li>
<li>function</li>
<li>ResponseContext</li>
<li>let</li>
<li>catch</li>
<li>RequestContext</li>
<li>export</li>
<li>if</li>
<li>case</li>
<li>new</li>
<li>package</li>
<li>static</li>
<li>void</li>
<li>in</li>
<li>formParams</li>
<li>byte</li>
<li>double</li>
<li>var</li>
<li>ResponseTransformer</li>
<li>useFormData</li>
<li>finally</li>
<li>this</li>
<li>Middleware</li>
<li>enum</li>
<li>varLocalDeferred</li>
<li>extends</li>
<li>null</li>
<li>transient</li>
<li>BlobApiResponse</li>
<li>final</li>
<li>true</li>
<li>try</li>
<li>implements</li>
<li>private</li>
<li>const</li>
<li>import</li>
<li>configuration</li>
<li>BASE_PATH</li>
<li>for</li>
<li>JSONApiResponse</li>
<li>interface</li>
<li>ConfigurationParameters</li>
<li>delete</li>
<li>long</li>
<li>switch</li>
<li>TextApiResponse</li>
<li>default</li>
<li>goto</li>
<li>public</li>
<li>native</li>
<li>yield</li>
<li>await</li>
<li>RequestOpts</li>
<li>class</li>
<li>typeof</li>
<li>break</li>
<li>false</li>
<li>volatile</li>
<li>abstract</li>
<li>requestOptions</li>
<li>RequiredError</li>
<li>int</li>
<li>instanceof</li>
<li>super</li>
<li>with</li>
<li>boolean</li>
<li>HTTPBody</li>
<li>throw</li>
<li>char</li>
<li>short</li>
<li>exists</li>
<li>HTTPQuery</li>
<li>return</li>
<li>VoidApiResponse</li>
</ul>
