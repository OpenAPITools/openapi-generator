
---
id: generator-opts-client-kotlin
title: Config Options for kotlin
sidebar_label: kotlin
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sourceFolder|source folder for generated code| |src/main/kotlin|
|packageName|Generated artifact package name.| |org.openapitools|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|artifactId|Generated artifact id (name of jar).| |null|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|dateLibrary|Option. Date library to use|<dl><dt>**string**</dt><dd>String</dd><dt>**java8**</dt><dd>Java 8 native JSR310</dd><dt>**threetenbp**</dt><dd>Threetenbp</dd><dl>|null|
|collectionType|Option. Collection type to use|<dl><dt>**array**</dt><dd>kotlin.Array</dd><dt>**list**</dt><dd>kotlin.collections.List</dd><dl>|null|
