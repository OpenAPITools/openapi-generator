---
title: Config Options for python-flask
sidebar_label: python-flask
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|controllerPackage|controller package| |controllers|
|defaultController|default controller| |default_controller|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|packageName|python package name (convention: snake_case).| |openapi_server|
|packageVersion|python package version.| |1.0.0|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|serverPort|TCP port to listen to in app.run| |8080|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|supportPython2|support python2| |false|
|useNose|use the nose test framework| |false|

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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Dict</li>
<li>List</li>
<li>bool</li>
<li>byte</li>
<li>bytearray</li>
<li>date</li>
<li>datetime</li>
<li>file</li>
<li>float</li>
<li>int</li>
<li>object</li>
<li>str</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>and</li>
<li>as</li>
<li>assert</li>
<li>break</li>
<li>class</li>
<li>continue</li>
<li>def</li>
<li>del</li>
<li>elif</li>
<li>else</li>
<li>except</li>
<li>exec</li>
<li>false</li>
<li>finally</li>
<li>for</li>
<li>from</li>
<li>global</li>
<li>if</li>
<li>import</li>
<li>in</li>
<li>is</li>
<li>lambda</li>
<li>none</li>
<li>nonlocal</li>
<li>not</li>
<li>or</li>
<li>pass</li>
<li>print</li>
<li>property</li>
<li>raise</li>
<li>return</li>
<li>self</li>
<li>true</li>
<li>try</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
