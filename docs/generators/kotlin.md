---
title: Config Options for kotlin
sidebar_label: kotlin
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sourceFolder|source folder for generated code| |src/main/kotlin|
|packageName|Generated artifact package name.| |org.openapitools.client|
|apiSuffix|suffix for api classes| |Api|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|artifactId|Generated artifact id (name of jar).| |kotlin-client|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson'| |moshi|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|modelMutable|Create mutable models| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**threetenbp-localdatetime**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, for legacy app only)</dd><dt>**string**</dt><dd>String</dd><dt>**java8-localdatetime**</dt><dd>Java 8 native JSR310 (jvm only, for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (jvm only, preferred for jdk 1.8+)</dd><dt>**threetenbp**</dt><dd>Threetenbp - Backport of JSR310 (jvm only, preferred for jdk &lt; 1.8)</dd><dl>|java8|
|collectionType|Option. Collection type to use|<dl><dt>**array**</dt><dd>kotlin.Array</dd><dt>**list**</dt><dd>kotlin.collections.List</dd><dl>|array|
|library|Library template (sub-template) to use|<dl><dt>**jvm-okhttp4**</dt><dd>[DEFAULT] Platform: Java Virtual Machine. HTTP client: OkHttp 4.2.0 (Android 5.0+ and Java 8+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-okhttp3**</dt><dd>Platform: Java Virtual Machine. HTTP client: OkHttp 3.12.4 (Android 2.3+ and Java 7+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-retrofit2**</dt><dd>Platform: Java Virtual Machine. HTTP client: Retrofit 2.6.2.</dd><dt>**multiplatform**</dt><dd>Platform: Kotlin multiplatform. HTTP client: Ktor 1.2.4. JSON processing: Kotlinx Serialization: 0.12.0.</dd><dl>|jvm-okhttp4|
|requestDateConverter|JVM-Option. Defines in how to handle date-time objects that are used for a request (as query or parameter)|<dl><dt>**toJson**</dt><dd>Date formater option using a json converter.</dd><dt>**toString**</dt><dd>[DEFAULT] Use the 'toString'-method of the date-time object to retrieve the related string representation.</dd><dl>|toString|
