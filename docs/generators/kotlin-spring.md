---
title: Config Options for kotlin-spring
sidebar_label: kotlin-spring
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|apiPackage|api package for generated code| |org.openapitools.api|
|apiSuffix|suffix for api classes| |Api|
|artifactId|Generated artifact id (name of jar).| |openapi-spring|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|basePackage|base package (invokerPackage) for generated code| |org.openapitools|
|delegatePattern|Whether to generate the server files using the delegate pattern| |false|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|exceptionHandler|generate default global exception handlers (not compatible with reactive. enabling reactive will disable exceptionHandler )| |true|
|gradleBuildFile|generate a gradle build file using the Kotlin DSL| |true|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|interfaceOnly|Whether to generate only API interface stubs without the server files.| |false|
|library|library template (sub-template)|<dl><dt>**spring-boot**</dt><dd>Spring-boot Server application.</dd><dl>|spring-boot|
|modelMutable|Create mutable models| |false|
|modelPackage|model package for generated code| |org.openapitools.model|
|packageName|Generated artifact package name.| |org.openapitools|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|reactive|use coroutines for reactive behavior| |false|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson'| |moshi|
|serverPort|configuration the port in which the sever is to run on| |8080|
|serviceImplementation|generate stub service implementations that extends service interfaces. If this is set to true service interfaces will also be generated| |false|
|serviceInterface|generate service interfaces to go alongside controllers. In most cases this option would be used to update an existing project, so not to override implementations. Useful to help facilitate the generation gap pattern| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |null|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |null|
|sourceFolder|source folder for generated code| |src/main/kotlin|
|swaggerAnnotations|generate swagger annotations to go alongside controllers and models| |false|
|title|server title name or client service name| |OpenAPI Kotlin Spring|
|useBeanValidation|Use BeanValidation API annotations to validate data types| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|BigDecimal|java.math.BigDecimal|
|Date|java.time.LocalDate|
|DateTime|java.time.OffsetDateTime|
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

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>ApiClient</li>
<li>ApiException</li>
<li>ApiResponse</li>
<li>as</li>
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
