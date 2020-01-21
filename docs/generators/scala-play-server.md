---
title: Config Options for scala-play-server
sidebar_label: scala-play-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |null|
|basePackage|Base package in which supporting classes are generated.| |org.openapitools|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|generateCustomExceptions|If set, generates custom exception types.| |true|
|modelPackage|package for generated models| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|routesFileName|Name of the routes file to generate.| |routes|
|skipStubs|If set, skips generation of stub classes.| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |null|
|supportAsync|If set, wraps API return types with Futures and generates async actions.| |false|
|useSwaggerUI|Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|List|java.util.*|
|ListBuffer|scala.collection.mutable.ListBuffer|
|ListSet|scala.collection.immutable.ListSet|
|LocalDate|java.time.LocalDate|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Map|java.util.Map|
|OffsetDateTime|java.time.OffsetDateTime|
|Seq|scala.collection.immutable.Seq|
|Set|scala.collection.immutable.Set|
|TemporaryFile|play.api.libs.Files.TemporaryFile|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|List|
|map|Map|
|set|Set|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Any</li>
<li>Array</li>
<li>Boolean</li>
<li>Double</li>
<li>Float</li>
<li>Int</li>
<li>List</li>
<li>Long</li>
<li>Map</li>
<li>Object</li>
<li>Seq</li>
<li>String</li>
<li>boolean</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>case</li>
<li>catch</li>
<li>class</li>
<li>def</li>
<li>do</li>
<li>else</li>
<li>extends</li>
<li>false</li>
<li>final</li>
<li>finally</li>
<li>for</li>
<li>forSome</li>
<li>if</li>
<li>implicit</li>
<li>import</li>
<li>lazy</li>
<li>match</li>
<li>new</li>
<li>null</li>
<li>object</li>
<li>override</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>return</li>
<li>sealed</li>
<li>super</li>
<li>this</li>
<li>throw</li>
<li>trait</li>
<li>true</li>
<li>try</li>
<li>type</li>
<li>val</li>
<li>var</li>
<li>while</li>
<li>with</li>
<li>yield</li>
</ul>
