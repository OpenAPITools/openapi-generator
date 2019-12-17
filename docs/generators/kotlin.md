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
|modelMutable|Create mutable models| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**string**</dt><dd>String</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (jvm only)</dd><dt>**threetenbp**</dt><dd>Threetenbp (jvm only)</dd><dl>|java8|
|collectionType|Option. Collection type to use|<dl><dt>**array**</dt><dd>kotlin.Array</dd><dt>**list**</dt><dd>kotlin.collections.List</dd><dl>|array|
|library|Library template (sub-template) to use|<dl><dt>**jvm-okhttp4**</dt><dd>[DEFAULT] Platform: Java Virtual Machine. HTTP client: OkHttp 4.2.0 (Android 5.0+ and Java 8+). JSON processing: Moshi 1.8.0.</dd><dt>**jvm-okhttp3**</dt><dd>Platform: Java Virtual Machine. HTTP client: OkHttp 3.12.4 (Android 2.3+ and Java 7+). JSON processing: Moshi 1.8.0.</dd><dt>**multiplatform**</dt><dd>Platform: Kotlin multiplatform. HTTP client: Ktor 1.2.4. JSON processing: Kotlinx Serialization: 0.12.0.</dd><dl>|jvm-okhttp4|
