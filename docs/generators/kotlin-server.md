
---
id: generator-opts-server-kotlin-server
title: Config Options for kotlin-server
sidebar_label: kotlin-server
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
|library|library template (sub-template) to use|<dl><dt>**ktor**</dt><dd>ktor framework</dd><dl>|ktor|
|featureAutoHead|Automatically provide responses to HEAD requests for existing routes that have the GET verb defined.| |true|
|featureConditionalHeaders|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |false|
|featureHSTS|Avoid sending content if client already has same content, by checking ETag or LastModified properties.| |true|
|featureCORS|Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org.| |false|
|featureCompression|Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response.| |true|
