---
title: Config Options for python-blueplanet
sidebar_label: python-blueplanet
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|packageName|python package name (convention: snake_case).| |openapi_server|
|packageVersion|python package version.| |1.0.0|
|controllerPackage|controller package| |controllers|
|defaultController|default controller| |default_controller|
|supportPython2|support python2| |false|
|serverPort|TCP port to listen to in app.run| |8080|
|useNose|use the nose test framework| |false|

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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>str</li>
<li>date</li>
<li>datetime</li>
<li>file</li>
<li>bool</li>
<li>Dict</li>
<li>byte</li>
<li>bytearray</li>
<li>List</li>
<li>float</li>
<li>int</li>
<li>object</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>import</li>
<li>def</li>
<li>for</li>
<li>del</li>
<li>global</li>
<li>none</li>
<li>while</li>
<li>nonlocal</li>
<li>not</li>
<li>lambda</li>
<li>and</li>
<li>assert</li>
<li>else</li>
<li>continue</li>
<li>yield</li>
<li>property</li>
<li>raise</li>
<li>from</li>
<li>if</li>
<li>class</li>
<li>or</li>
<li>pass</li>
<li>break</li>
<li>in</li>
<li>finally</li>
<li>false</li>
<li>is</li>
<li>elif</li>
<li>with</li>
<li>as</li>
<li>print</li>
<li>true</li>
<li>self</li>
<li>except</li>
<li>try</li>
<li>exec</li>
<li>return</li>
</ul>
