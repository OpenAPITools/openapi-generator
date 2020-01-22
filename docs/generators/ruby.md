---
title: Config Options for ruby
sidebar_label: ruby
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|gemAuthor|gem author (only one is supported).| |null|
|gemAuthorEmail|gem author email (only one is supported).| |null|
|gemDescription|gem description. | |This gem maps to a REST API|
|gemHomepage|gem homepage. | |http://org.openapitools|
|gemLicense|gem license. | |unlicense|
|gemName|gem name (convention: underscore_case).| |openapi_client|
|gemRequiredRubyVersion|gem required Ruby version. | |&gt;= 1.9|
|gemSummary|gem summary. | |A ruby wrapper for the REST APIs|
|gemVersion|gem version.| |1.0.0|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |true|
|library|HTTP library template (sub-template) to use|<dl><dt>**faraday**</dt><dd>Faraday (https://github.com/lostisland/faraday) (Beta support)</dd><dt>**typhoeus**</dt><dd>Typhoeus &gt;= 1.0.1 (https://github.com/typhoeus/typhoeus)</dd><dl>|typhoeus|
|moduleName|top module name (convention: CamelCase, usually corresponding to gem name).| |OpenAPIClient|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Array</li>
<li>Boolean</li>
<li>Date</li>
<li>DateTime</li>
<li>File</li>
<li>Float</li>
<li>Hash</li>
<li>Integer</li>
<li>Object</li>
<li>String</li>
<li>array</li>
<li>int</li>
<li>map</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>__file__</li>
<li>__line__</li>
<li>_header_accept</li>
<li>_header_accept_result</li>
<li>_header_content_type</li>
<li>alias</li>
<li>and</li>
<li>auth_names</li>
<li>begin</li>
<li>break</li>
<li>case</li>
<li>class</li>
<li>def</li>
<li>defined?</li>
<li>do</li>
<li>else</li>
<li>elsif</li>
<li>end</li>
<li>ensure</li>
<li>false</li>
<li>for</li>
<li>form_params</li>
<li>header_params</li>
<li>if</li>
<li>in</li>
<li>local_var_path</li>
<li>module</li>
<li>next</li>
<li>nil</li>
<li>not</li>
<li>or</li>
<li>post_body</li>
<li>query_params</li>
<li>redo</li>
<li>rescue</li>
<li>retry</li>
<li>return</li>
<li>self</li>
<li>send</li>
<li>super</li>
<li>then</li>
<li>true</li>
<li>undef</li>
<li>unless</li>
<li>until</li>
<li>when</li>
<li>while</li>
<li>yield</li>
</ul>
