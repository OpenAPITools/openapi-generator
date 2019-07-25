
---
id: generator-opts-client-android
title: Config Options for android
sidebar_label: android
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |null|
|apiPackage|package for generated api classes| |null|
|invokerPackage|root package for generated code| |null|
|groupId|groupId for use in the generated build.gradle and pom.xml| |null|
|artifactId|artifactId for use in the generated build.gradle and pom.xml| |null|
|artifactVersion|artifact version for use in the generated build.gradle and pom.xml| |null|
|sourceFolder|source folder for generated code| |null|
|useAndroidMavenGradlePlugin|A flag to toggle android-maven gradle plugin.| |true|
|androidGradleVersion|gradleVersion version for use in the generated build.gradle| |null|
|androidSdkVersion|compileSdkVersion version for use in the generated build.gradle| |null|
|androidBuildToolsVersion|buildToolsVersion version for use in the generated build.gradle| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|library|library template (sub-template) to use|<dl><dt>**volley**</dt><dd>HTTP client: Volley 1.0.19 (default)</dd><dt>**httpclient**</dt><dd>HTTP client: Apache HttpClient 4.3.6. JSON processing: Gson 2.3.1. IMPORTANT: Android client using HttpClient is not actively maintained and will be depecreated in the next major release.</dd><dl>|null|
