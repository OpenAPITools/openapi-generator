---
title: Config Options for php-silex
sidebar_label: php-silex
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|List|java.util.*|
|LocalDate|org.joda.time.*|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Map|java.util.Map|
|Set|java.util.*|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|array|
|map|map|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>DateTime</li>
<li>boolean</li>
<li>double</li>
<li>float</li>
<li>int</li>
<li>integer</li>
<li>mixed</li>
<li>number</li>
<li>object</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>__halt_compiler</li>
<li>abstract</li>
<li>and</li>
<li>array</li>
<li>as</li>
<li>break</li>
<li>callable</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>clone</li>
<li>const</li>
<li>continue</li>
<li>declare</li>
<li>default</li>
<li>die</li>
<li>do</li>
<li>echo</li>
<li>else</li>
<li>elseif</li>
<li>empty</li>
<li>enddeclare</li>
<li>endfor</li>
<li>endforeach</li>
<li>endif</li>
<li>endswitch</li>
<li>endwhile</li>
<li>eval</li>
<li>exit</li>
<li>extends</li>
<li>final</li>
<li>for</li>
<li>foreach</li>
<li>function</li>
<li>global</li>
<li>goto</li>
<li>if</li>
<li>implements</li>
<li>include</li>
<li>include_once</li>
<li>instanceof</li>
<li>insteadof</li>
<li>interface</li>
<li>isset</li>
<li>list</li>
<li>namespace</li>
<li>new</li>
<li>or</li>
<li>print</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>require</li>
<li>require_once</li>
<li>return</li>
<li>static</li>
<li>switch</li>
<li>throw</li>
<li>trait</li>
<li>try</li>
<li>unset</li>
<li>use</li>
<li>var</li>
<li>while</li>
<li>xor</li>
</ul>
