---
title: Config Options for scala-httpclient-deprecated
sidebar_label: scala-httpclient-deprecated
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
|modelPropertyNaming|Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name| |camelCase|

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
|DateTime|org.joda.time.*|
|Array|java.util.List|
|ListSet|scala.collection.immutable.ListSet|
|UUID|java.util.UUID|
|File|java.io.File|
|Seq|scala.collection.immutable.Seq|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|set|Set|
|array|ListBuffer|
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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>def</li>
<li>basepath</li>
<li>queryparams</li>
<li>do</li>
<li>type</li>
<li>while</li>
<li>path</li>
<li>protected</li>
<li>else</li>
<li>trait</li>
<li>catch</li>
<li>forsome</li>
<li>if</li>
<li>case</li>
<li>contenttypes</li>
<li>val</li>
<li>new</li>
<li>mp</li>
<li>package</li>
<li>sealed</li>
<li>var</li>
<li>finally</li>
<li>this</li>
<li>formparams</li>
<li>headerparams</li>
<li>extends</li>
<li>null</li>
<li>final</li>
<li>true</li>
<li>try</li>
<li>object</li>
<li>implicit</li>
<li>private</li>
<li>import</li>
<li>lazy</li>
<li>for</li>
<li>apiinvoker</li>
<li>yield</li>
<li>override</li>
<li>class</li>
<li>postbody</li>
<li>false</li>
<li>match</li>
<li>abstract</li>
<li>contenttype</li>
<li>super</li>
<li>with</li>
<li>throw</li>
<li>return</li>
</ul>
