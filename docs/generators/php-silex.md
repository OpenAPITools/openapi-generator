---
title: Config Options for php-silex
sidebar_label: php-silex
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|Set|java.util.*|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.*|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|Array|java.util.List|
|List|java.util.*|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|array|
|map|map|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>number</li>
<li>boolean</li>
<li>string</li>
<li>double</li>
<li>mixed</li>
<li>integer</li>
<li>float</li>
<li>int</li>
<li>DateTime</li>
<li>object</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>die</li>
<li>callable</li>
<li>declare</li>
<li>isset</li>
<li>use</li>
<li>echo</li>
<li>do</li>
<li>while</li>
<li>unset</li>
<li>empty</li>
<li>endwhile</li>
<li>protected</li>
<li>continue</li>
<li>else</li>
<li>elseif</li>
<li>function</li>
<li>endfor</li>
<li>trait</li>
<li>catch</li>
<li>if</li>
<li>case</li>
<li>new</li>
<li>static</li>
<li>var</li>
<li>require</li>
<li>require_once</li>
<li>list</li>
<li>include_once</li>
<li>exit</li>
<li>as</li>
<li>eval</li>
<li>extends</li>
<li>final</li>
<li>try</li>
<li>implements</li>
<li>private</li>
<li>const</li>
<li>for</li>
<li>global</li>
<li>interface</li>
<li>__halt_compiler</li>
<li>switch</li>
<li>foreach</li>
<li>default</li>
<li>goto</li>
<li>public</li>
<li>array</li>
<li>and</li>
<li>xor</li>
<li>class</li>
<li>include</li>
<li>or</li>
<li>endswitch</li>
<li>break</li>
<li>enddeclare</li>
<li>abstract</li>
<li>instanceof</li>
<li>print</li>
<li>throw</li>
<li>insteadof</li>
<li>clone</li>
<li>namespace</li>
<li>endif</li>
<li>endforeach</li>
<li>return</li>
</ul>
