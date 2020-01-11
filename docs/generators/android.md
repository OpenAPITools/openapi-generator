---
title: Config Options for android
sidebar_label: android
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
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

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|LocalDateTime|org.joda.time.*|
|Set|java.util.*|
|LocalTime|org.joda.time.*|
|HashMap|java.util.HashMap|
|ArrayList|java.util.ArrayList|
|URI|java.net.URI|
|Timestamp|java.sql.Timestamp|
|LocalDate|org.joda.time.*|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|Array|java.util.List|
|List|java.util.*|
|UUID|java.util.UUID|
|File|java.io.File|
|Map|java.util.Map|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|ArrayList|
|map|HashMap|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Integer</li>
<li>byte[]</li>
<li>Float</li>
<li>boolean</li>
<li>Long</li>
<li>Object</li>
<li>String</li>
<li>Boolean</li>
<li>Double</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>synchronized</li>
<li>basepath</li>
<li>do</li>
<li>float</li>
<li>while</li>
<li>localvarpath</li>
<li>protected</li>
<li>continue</li>
<li>else</li>
<li>localvarqueryparams</li>
<li>catch</li>
<li>if</li>
<li>case</li>
<li>new</li>
<li>package</li>
<li>static</li>
<li>void</li>
<li>double</li>
<li>byte</li>
<li>finally</li>
<li>this</li>
<li>strictfp</li>
<li>throws</li>
<li>enum</li>
<li>extends</li>
<li>null</li>
<li>transient</li>
<li>final</li>
<li>try</li>
<li>localvarbuilder</li>
<li>object</li>
<li>localvarcontenttypes</li>
<li>implements</li>
<li>private</li>
<li>import</li>
<li>const</li>
<li>for</li>
<li>interface</li>
<li>long</li>
<li>switch</li>
<li>default</li>
<li>goto</li>
<li>public</li>
<li>localvarheaderparams</li>
<li>native</li>
<li>localvarcontenttype</li>
<li>apiinvoker</li>
<li>assert</li>
<li>class</li>
<li>localvarformparams</li>
<li>break</li>
<li>localvarresponse</li>
<li>volatile</li>
<li>abstract</li>
<li>int</li>
<li>instanceof</li>
<li>super</li>
<li>boolean</li>
<li>throw</li>
<li>localvarpostbody</li>
<li>char</li>
<li>short</li>
<li>authnames</li>
<li>return</li>
</ul>
