---
title: Config Options for kotlin
sidebar_label: kotlin
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |kotlin-client|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|collectionType|Option. Collection type to use|<dl><dt>**array**</dt><dd>kotlin.Array</dd><dt>**list**</dt><dd>kotlin.collections.List</dd><dl>|array|
|dateLibrary|Option. Date library to use|<dl><dt>**threetenbp-localdatetime**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, for legacy app only)</dd><dt>**string**</dt><dd>String</dd><dt>**java8-localdatetime**</dt><dd>Java 8 native JSR310 (jvm only, for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (jvm only, preferred for jdk 1.8+)</dd><dt>**threetenbp**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, preferred for jdk &lt; 1.8)</dd><dl>|java8|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|library|Library template (sub-template) to use|<dl><dt>**jvm-okhttp4**</dt><dd>[DEFAULT] Platform: Java Virtual Machine. HTTP client: OkHttp 4.2.0 (Android 5.0+ and Java 8+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-okhttp3**</dt><dd>Platform: Java Virtual Machine. HTTP client: OkHttp 3.12.4 (Android 2.3+ and Java 7+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-retrofit2**</dt><dd>Platform: Java Virtual Machine. HTTP client: Retrofit 2.6.2.</dd><dt>**multiplatform**</dt><dd>Platform: Kotlin multiplatform. HTTP client: Ktor 1.2.4. JSON processing: Kotlinx Serialization: 0.12.0.</dd><dl>|jvm-okhttp4|
|modelMutable|Create mutable models| |false|
|packageName|Generated artifact package name.| |org.openapitools.client|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|requestDateConverter|JVM-Option. Defines in how to handle date-time objects that are used for a request (as query or parameter)|<dl><dt>**toJson**</dt><dd>[DEFAULT] Date formater option using a json converter.</dd><dt>**toString**</dt><dd>Use the 'toString'-method of the date-time object to retrieve the related string representation.</dd><dl>|toJson|
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
