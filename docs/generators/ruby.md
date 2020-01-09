---
title: Config Options for ruby
sidebar_label: ruby
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|gemName|gem name (convention: underscore_case).| |openapi_client|
|moduleName|top module name (convention: CamelCase, usually corresponding to gem name).| |OpenAPIClient|
|gemVersion|gem version.| |1.0.0|
|gemLicense|gem license. | |unlicense|
|gemRequiredRubyVersion|gem required Ruby version. | |&gt;= 1.9|
|gemHomepage|gem homepage. | |http://org.openapitools|
|gemSummary|gem summary. | |A ruby wrapper for the REST APIs|
|gemDescription|gem description. | |This gem maps to a REST API|
|gemAuthor|gem author (only one is supported).| |null|
|gemAuthorEmail|gem author email (only one is supported).| |null|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|HTTP library template (sub-template) to use|<dl><dt>**faraday**</dt><dd>Faraday (https://github.com/lostisland/faraday) (Beta support)</dd><dt>**typhoeus**</dt><dd>Typhoeus &gt;= 1.0.1 (https://github.com/typhoeus/typhoeus)</dd><dl>|typhoeus|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>string</li>
<li>String</li>
<li>Hash</li>
<li>Date</li>
<li>DateTime</li>
<li>int</li>
<li>Integer</li>
<li>Array</li>
<li>Float</li>
<li>array</li>
<li>Object</li>
<li>Boolean</li>
<li>File</li>
<li>map</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>next</li>
<li>defined?</li>
<li>def</li>
<li>_header_accept_result</li>
<li>_header_content_type</li>
<li>for</li>
<li>local_var_path</li>
<li>form_params</li>
<li>redo</li>
<li>do</li>
<li>elsif</li>
<li>while</li>
<li>when</li>
<li>nil</li>
<li>not</li>
<li>unless</li>
<li>and</li>
<li>else</li>
<li>yield</li>
<li>alias</li>
<li>end</li>
<li>class</li>
<li>if</li>
<li>rescue</li>
<li>case</li>
<li>retry</li>
<li>or</li>
<li>ensure</li>
<li>in</li>
<li>break</li>
<li>module</li>
<li>false</li>
<li>undef</li>
<li>then</li>
<li>_header_accept</li>
<li>super</li>
<li>header_params</li>
<li>__line__</li>
<li>auth_names</li>
<li>__file__</li>
<li>query_params</li>
<li>true</li>
<li>self</li>
<li>until</li>
<li>post_body</li>
<li>begin</li>
<li>send</li>
<li>return</li>
</ul>
