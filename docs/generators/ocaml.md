---
title: Config Options for ocaml
sidebar_label: ocaml
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
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>bool</li>
<li>string</li>
<li>int32</li>
<li>int64</li>
<li>bytes</li>
<li>char</li>
<li>Yojson.Safe.t</li>
<li>float</li>
<li>list</li>
<li>int</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>exception</li>
<li>struct</li>
<li>asr</li>
<li>mod</li>
<li>do</li>
<li>functor</li>
<li>type</li>
<li>while</li>
<li>when</li>
<li>rec</li>
<li>else</li>
<li>function</li>
<li>mutable</li>
<li>let</li>
<li>if</li>
<li>val</li>
<li>new</li>
<li>method</li>
<li>in</li>
<li>module</li>
<li>nonrec</li>
<li>then</li>
<li>done</li>
<li>as</li>
<li>external</li>
<li>true</li>
<li>try</li>
<li>begin</li>
<li>object</li>
<li>private</li>
<li>virtual</li>
<li>lsl</li>
<li>lazy</li>
<li>for </li>
<li>lsr</li>
<li>lor</li>
<li>sig</li>
<li>result</li>
<li>and</li>
<li>assert</li>
<li>of</li>
<li>land</li>
<li>end</li>
<li>class</li>
<li>lxor</li>
<li>include</li>
<li>or</li>
<li>downto</li>
<li>false</li>
<li>match</li>
<li>initializer</li>
<li>with</li>
<li>inherit</li>
<li>constraint</li>
<li>to</li>
<li>fun</li>
<li>open</li>
</ul>
