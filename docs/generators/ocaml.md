---
title: Config Options for ocaml
sidebar_label: ocaml
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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Yojson.Safe.t</li>
<li>bool</li>
<li>bytes</li>
<li>char</li>
<li>float</li>
<li>int</li>
<li>int32</li>
<li>int64</li>
<li>list</li>
<li>string</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>and</li>
<li>as</li>
<li>asr</li>
<li>assert</li>
<li>begin</li>
<li>class</li>
<li>constraint</li>
<li>do</li>
<li>done</li>
<li>downto</li>
<li>else</li>
<li>end</li>
<li>exception</li>
<li>external</li>
<li>false</li>
<li>for </li>
<li>fun</li>
<li>function</li>
<li>functor</li>
<li>if</li>
<li>in</li>
<li>include</li>
<li>inherit</li>
<li>initializer</li>
<li>land</li>
<li>lazy</li>
<li>let</li>
<li>lor</li>
<li>lsl</li>
<li>lsr</li>
<li>lxor</li>
<li>match</li>
<li>method</li>
<li>mod</li>
<li>module</li>
<li>mutable</li>
<li>new</li>
<li>nonrec</li>
<li>object</li>
<li>of</li>
<li>open</li>
<li>or</li>
<li>private</li>
<li>rec</li>
<li>result</li>
<li>sig</li>
<li>struct</li>
<li>then</li>
<li>to</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>val</li>
<li>virtual</li>
<li>when</li>
<li>while</li>
<li>with</li>
</ul>
