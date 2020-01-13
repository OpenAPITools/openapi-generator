---
title: Config Options for kotlin-server
sidebar_label: kotlin-server
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sourceFolder|source folder for generated code| |src/main/kotlin|
|packageName|Generated artifact package name.| |org.openapitools.server|
|apiSuffix|suffix for api classes| |Api|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|artifactId|Generated artifact id (name of jar).| |kotlin-server|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson'| |moshi|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|modelMutable|Create mutable models| |false|
|library|library template (sub-template)|<dl><dt>**ktor**</dt><dd>ktor framework</dd><dl>|ktor|
|featureAutoHead|Automatically provide responses to HEAD requests for existing routes that have the GET verb defined.| |true|
|featureConditionalHeaders|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |false|
|featureHSTS|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |true|
|featureCORS|Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org.| |false|
|featureCompression|Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|java.time.LocalDateTime|
|LocalTime|java.time.LocalTime|
|UUID|java.util.UUID|
|URI|java.net.URI|
|File|java.io.File|
|Timestamp|java.sql.Timestamp|
|LocalDate|java.time.LocalDate|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|java.time.LocalDateTime|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|kotlin.arrayOf|
|list|kotlin.arrayOf|
|map|kotlin.mapOf|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>kotlin.collections.List</li>
<li>kotlin.Float</li>
<li>kotlin.Double</li>
<li>kotlin.String</li>
<li>kotlin.Array</li>
<li>kotlin.Byte</li>
<li>kotlin.collections.Map</li>
<li>kotlin.Short</li>
<li>kotlin.Boolean</li>
<li>kotlin.Long</li>
<li>kotlin.Char</li>
<li>kotlin.ByteArray</li>
<li>kotlin.Int</li>
<li>kotlin.collections.Set</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>for</li>
<li>do</li>
<li>interface</li>
<li>while</li>
<li>when</li>
<li>continue</li>
<li>else</li>
<li>typealias</li>
<li>class</li>
<li>if</li>
<li>typeof</li>
<li>val</li>
<li>package</li>
<li>break</li>
<li>in</li>
<li>var</li>
<li>false</li>
<li>this</li>
<li>is</li>
<li>super</li>
<li>as</li>
<li>null</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>fun</li>
<li>return</li>
<li>object</li>
</ul>
