---
title: Config Options for scala-akka
sidebar_label: scala-akka
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
|mainPackage|Top-level package name, which defines 'apiPackage', 'modelPackage', 'invokerPackage'| |org.openapitools.client|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ListBuffer|scala.collection.mutable.ListBuffer|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.*|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.DateTime|
|Array|java.util.List|
|ListSet|scala.collection.immutable.ListSet|
|UUID|java.util.UUID|
|File|java.io.File|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|set|Set|
|array|ListBuffer|
|map|Map|


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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>implicit</li>
<li>private</li>
<li>def</li>
<li>import</li>
<li>lazy</li>
<li>for</li>
<li>do</li>
<li>type</li>
<li>while</li>
<li>protected</li>
<li>else</li>
<li>yield</li>
<li>trait</li>
<li>catch</li>
<li>override</li>
<li>forsome</li>
<li>class</li>
<li>if</li>
<li>case</li>
<li>val</li>
<li>new</li>
<li>package</li>
<li>sealed</li>
<li>var</li>
<li>finally</li>
<li>false</li>
<li>match</li>
<li>this</li>
<li>abstract</li>
<li>super</li>
<li>with</li>
<li>extends</li>
<li>null</li>
<li>throw</li>
<li>final</li>
<li>true</li>
<li>try</li>
<li>return</li>
<li>object</li>
</ul>
