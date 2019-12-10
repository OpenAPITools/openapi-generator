---
title: Config Options for kotlin-spring
sidebar_label: kotlin-spring
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sourceFolder|source folder for generated code| |src/main/kotlin|
|packageName|Generated artifact package name.| |org.openapitools|
|apiSuffix|suffix for api classes| |Api|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|artifactId|Generated artifact id (name of jar).| |openapi-spring|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|serializationLibrary|What serialization library to use: 'moshi' (default), or 'gson'| |moshi|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |null|
|modelMutable|Create mutable models| |false|
|title|server title name or client service name| |OpenAPI Kotlin Spring|
|basePackage|base package (invokerPackage) for generated code| |org.openapitools|
|serverPort|configuration the port in which the sever is to run on| |8080|
|modelPackage|model package for generated code| |org.openapitools.model|
|apiPackage|api package for generated code| |org.openapitools.api|
|exceptionHandler|generate default global exception handlers (not compatible with reactive. enabling reactive will disable exceptionHandler )| |true|
|gradleBuildFile|generate a gradle build file using the Kotlin DSL| |true|
|swaggerAnnotations|generate swagger annotations to go alongside controllers and models| |false|
|serviceInterface|generate service interfaces to go alongside controllers. In most cases this option would be used to update an existing project, so not to override implementations. Useful to help facilitate the generation gap pattern| |false|
|serviceImplementation|generate stub service implementations that extends service interfaces. If this is set to true service interfaces will also be generated| |false|
|useBeanValidation|Use BeanValidation API annotations to validate data types| |true|
|reactive|use coroutines for reactive behavior| |false|
|interfaceOnly|Whether to generate only API interface stubs without the server files.| |false|
|delegatePattern|Whether to generate the server files using the delegate pattern| |false|
|library|library template (sub-template)|<dl><dt>**spring-boot**</dt><dd>Spring-boot Server application.</dd><dl>|spring-boot|
