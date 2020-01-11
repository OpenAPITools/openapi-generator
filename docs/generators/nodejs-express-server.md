---
title: Config Options for nodejs-express-server
sidebar_label: nodejs-express-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|serverPort|TCP port to listen on.| |null|

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


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"></ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>const</li>
<li>import</li>
<li>debugger</li>
<li>for</li>
<li>do</li>
<li>while</li>
<li>delete</li>
<li>switch</li>
<li>default</li>
<li>continue</li>
<li>else</li>
<li>function</li>
<li>yield</li>
<li>let</li>
<li>catch</li>
<li>class</li>
<li>export</li>
<li>if</li>
<li>case</li>
<li>typeof</li>
<li>new</li>
<li>void</li>
<li>break</li>
<li>in</li>
<li>var</li>
<li>finally</li>
<li>this</li>
<li>instanceof</li>
<li>super</li>
<li>with</li>
<li>extends</li>
<li>throw</li>
<li>try</li>
<li>return</li>
</ul>
