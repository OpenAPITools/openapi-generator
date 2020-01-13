---
title: Config Options for scala-play-server
sidebar_label: scala-play-server
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
|routesFileName|Name of the routes file to generate.| |routes|
|basePackage|Base package in which supporting classes are generated.| |org.openapitools|
|skipStubs|If set, skips generation of stub classes.| |false|
|supportAsync|If set, wraps API return types with Futures and generates async actions.| |false|
|generateCustomExceptions|If set, generates custom exception types.| |true|
|useSwaggerUI|Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|Set|scala.collection.immutable.Set|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ListBuffer|scala.collection.mutable.ListBuffer|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|java.time.LocalDate|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|Array|java.util.List|
|ListSet|scala.collection.immutable.ListSet|
|OffsetDateTime|java.time.OffsetDateTime|
|List|java.util.*|
|TemporaryFile|play.api.libs.Files.TemporaryFile|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|
|Seq|scala.collection.immutable.Seq|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|set|Set|
|array|List|
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
<li>forSome</li>
<li>protected</li>
<li>else</li>
<li>yield</li>
<li>trait</li>
<li>catch</li>
<li>override</li>
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
