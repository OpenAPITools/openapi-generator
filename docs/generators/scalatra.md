---
title: Config Options for scalatra
sidebar_label: scalatra
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|sourceFolder|source folder for generated code| |null|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.LocalDateTime|
|Set|scala.collection.immutable.Set|
|LocalTime|org.joda.time.LocalTime|
|HashMap|java.util.HashMap|
|ListBuffer|scala.collection.mutable.ListBuffer|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.LocalDate|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.DateTime|
|Array|java.util.List|
|ListSet|scala.collection.immutable.ListSet|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|set|Set|
|map|HashMap|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>String</li>
<li>Double</li>
<li>Any</li>
<li>Int</li>
<li>Array</li>
<li>Float</li>
<li>boolean</li>
<li>Long</li>
<li>Object</li>
<li>List</li>
<li>Boolean</li>
<li>Map</li>
<li>Seq</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>synchronized</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>type</li>
<li>protected</li>
<li>continue</li>
<li>else</li>
<li>catch</li>
<li>if</li>
<li>case</li>
<li>new</li>
<li>package</li>
<li>static</li>
<li>void</li>
<li>double</li>
<li>byte</li>
<li>finally</li>
<li>this</li>
<li>strictfp</li>
<li>throws</li>
<li>enum</li>
<li>extends</li>
<li>transient</li>
<li>final</li>
<li>try</li>
<li>implements</li>
<li>private</li>
<li>import</li>
<li>const</li>
<li>for</li>
<li>interface</li>
<li>long</li>
<li>switch</li>
<li>default</li>
<li>goto</li>
<li>public</li>
<li>native</li>
<li>assert</li>
<li>class</li>
<li>break</li>
<li>volatile</li>
<li>abstract</li>
<li>int</li>
<li>instanceof</li>
<li>super</li>
<li>boolean</li>
<li>throw</li>
<li>char</li>
<li>short</li>
<li>return</li>
</ul>
