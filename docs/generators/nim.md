---
title: Config Options for nim
sidebar_label: nim
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


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>bool</li>
<li>char</li>
<li>cstring</li>
<li>float</li>
<li>float32</li>
<li>float64</li>
<li>int</li>
<li>int16</li>
<li>int32</li>
<li>int64</li>
<li>int8</li>
<li>pointer</li>
<li>string</li>
<li>uint</li>
<li>uint16</li>
<li>uint32</li>
<li>uint64</li>
<li>uint8</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>addr</li>
<li>and</li>
<li>as</li>
<li>asm</li>
<li>bind</li>
<li>block</li>
<li>break</li>
<li>case</li>
<li>cast</li>
<li>concept</li>
<li>const</li>
<li>continue</li>
<li>converter</li>
<li>defer</li>
<li>discard</li>
<li>distinct</li>
<li>div</li>
<li>do</li>
<li>elif</li>
<li>else</li>
<li>end</li>
<li>enum</li>
<li>except</li>
<li>export</li>
<li>finally</li>
<li>for</li>
<li>from</li>
<li>func</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>include</li>
<li>interface</li>
<li>is</li>
<li>isnot</li>
<li>iterator</li>
<li>let</li>
<li>macro</li>
<li>method</li>
<li>mixin</li>
<li>mod</li>
<li>nil</li>
<li>not</li>
<li>notin</li>
<li>object</li>
<li>of</li>
<li>or</li>
<li>out</li>
<li>proc</li>
<li>ptr</li>
<li>raise</li>
<li>ref</li>
<li>return</li>
<li>shl</li>
<li>shr</li>
<li>static</li>
<li>template</li>
<li>try</li>
<li>tuple</li>
<li>type</li>
<li>using</li>
<li>var</li>
<li>when</li>
<li>while</li>
<li>xor</li>
<li>yield</li>
</ul>
