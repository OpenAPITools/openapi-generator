
---
id: generator-opts-server-kotlin-vertx
title: Config Options for kotlin-vertx
sidebar_label: kotlin-vertx
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sourceFolder|source folder for generated code| |src/main/kotlin|
|modelPackage|package for generated models| |org.openapitools.server.api.model|
|apiPackage|package for generated api classes| |org.openapitools.server.api.verticle|
|apiSuffix|suffix for api classes| |Api|
|groupId|Generated artifact package's organization (i.e. maven groupId).| |org.openapitools|
|artifactId|Generated artifact id (name of jar).| |kotlin-server|
|artifactVersion|Generated artifact's package version.| |1.0.0|
|enumPropertyNaming|Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'| |camelCase|
|parcelizeModels|toggle &quot;@Parcelize&quot; for generated models| |null|
