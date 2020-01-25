---
title: Config Options for nodejs-server-deprecated
sidebar_label: nodejs-server-deprecated
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|exportedName|When the generated code will be deployed to Google Cloud Functions, this option can be used to update the name of the exported function. By default, it refers to the basePath. This does not affect normal standalone nodejs server code.| |null|
|googleCloudFunctions|When specified, it will generate the code which runs within Google Cloud Functions instead of standalone Node.JS server. See https://cloud.google.com/functions/docs/quickstart for the details of how to deploy the generated code.| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|serverPort|TCP port to listen on.| |null|
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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"></ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>break</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>debugger</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>else</li>
<li>export</li>
<li>extends</li>
<li>finally</li>
<li>for</li>
<li>function</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>instanceof</li>
<li>let</li>
<li>new</li>
<li>return</li>
<li>super</li>
<li>switch</li>
<li>this</li>
<li>throw</li>
<li>try</li>
<li>typeof</li>
<li>var</li>
<li>void</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
