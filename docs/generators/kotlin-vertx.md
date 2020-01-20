---
title: Config Options for kotlin-vertx
sidebar_label: kotlin-vertx
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |null|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|modelMutable|Create mutable models| |false|
|packageName|Generated artifact package name.| |org.openapitools|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson'| |moshi|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|java.time.LocalDateTime|
|File|java.io.File|
|LocalDate|java.time.LocalDate|
|LocalDateTime|java.time.LocalDateTime|
|LocalTime|java.time.LocalTime|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|kotlin.arrayOf|
|list|kotlin.arrayOf|
|map|kotlin.mapOf|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>kotlin.Array</li>
<li>kotlin.Boolean</li>
<li>kotlin.Byte</li>
<li>kotlin.ByteArray</li>
<li>kotlin.Char</li>
<li>kotlin.Double</li>
<li>kotlin.Float</li>
<li>kotlin.Int</li>
<li>kotlin.Long</li>
<li>kotlin.Short</li>
<li>kotlin.String</li>
<li>kotlin.collections.List</li>
<li>kotlin.collections.Map</li>
<li>kotlin.collections.Set</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>as</li>
<li>break</li>
<li>class</li>
<li>continue</li>
<li>do</li>
<li>else</li>
<li>false</li>
<li>for</li>
<li>fun</li>
<li>if</li>
<li>in</li>
<li>interface</li>
<li>is</li>
<li>null</li>
<li>object</li>
<li>package</li>
<li>return</li>
<li>super</li>
<li>this</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typealias</li>
<li>typeof</li>
<li>val</li>
<li>var</li>
<li>when</li>
<li>while</li>
</ul>
