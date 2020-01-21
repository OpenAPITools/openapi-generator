---
title: Config Options for android
sidebar_label: android
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|androidBuildToolsVersion|buildToolsVersion version for use in the generated build.gradle| |null|
|androidGradleVersion|gradleVersion version for use in the generated build.gradle| |null|
|androidSdkVersion|compileSdkVersion version for use in the generated build.gradle| |null|
|apiPackage|package for generated api classes| |null|
|artifactId|artifactId for use in the generated build.gradle and pom.xml| |null|
|artifactVersion|artifact version for use in the generated build.gradle and pom.xml| |null|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|groupId|groupId for use in the generated build.gradle and pom.xml| |null|
|invokerPackage|root package for generated code| |null|
|library|library template (sub-template) to use|<dl><dt>**volley**</dt><dd>HTTP client: Volley 1.0.19 (default)</dd><dt>**httpclient**</dt><dd>HTTP client: Apache HttpClient 4.3.6. JSON processing: Gson 2.3.1. IMPORTANT: Android client using HttpClient is not actively maintained and will be depecreated in the next major release.</dd><dl>|null|
|modelPackage|package for generated models| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |null|
|useAndroidMavenGradlePlugin|A flag to toggle android-maven gradle plugin.| |true|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |
|Array|java.util.List|
|ArrayList|java.util.ArrayList|
|BigDecimal|java.math.BigDecimal|
|Date|java.util.Date|
|DateTime|org.joda.time.*|
|File|java.io.File|
|HashMap|java.util.HashMap|
|List|java.util.*|
|LocalDate|org.joda.time.*|
|LocalDateTime|org.joda.time.*|
|LocalTime|org.joda.time.*|
|Map|java.util.Map|
|Set|java.util.*|
|Timestamp|java.sql.Timestamp|
|URI|java.net.URI|
|UUID|java.util.UUID|


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
|array|ArrayList|
|map|HashMap|


## LANGUAGE PRIMITIVES

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>Boolean</li>
<li>Double</li>
<li>Float</li>
<li>Integer</li>
<li>Long</li>
<li>Object</li>
<li>String</li>
<li>boolean</li>
<li>byte[]</li>
</ul>

## RESERVED WORDS

<ul data-columns="2" style="list-style-type: disc;-webkit-columns:2;-moz-columns:2;columns:2;-moz-column-fill:auto;column-fill:auto"><li>abstract</li>
<li>apiinvoker</li>
<li>assert</li>
<li>authnames</li>
<li>basepath</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>const</li>
<li>continue</li>
<li>default</li>
<li>do</li>
<li>double</li>
<li>else</li>
<li>enum</li>
<li>extends</li>
<li>final</li>
<li>finally</li>
<li>float</li>
<li>for</li>
<li>goto</li>
<li>if</li>
<li>implements</li>
<li>import</li>
<li>instanceof</li>
<li>int</li>
<li>interface</li>
<li>localvarbuilder</li>
<li>localvarcontenttype</li>
<li>localvarcontenttypes</li>
<li>localvarformparams</li>
<li>localvarheaderparams</li>
<li>localvarpath</li>
<li>localvarpostbody</li>
<li>localvarqueryparams</li>
<li>localvarresponse</li>
<li>long</li>
<li>native</li>
<li>new</li>
<li>null</li>
<li>object</li>
<li>package</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>return</li>
<li>short</li>
<li>static</li>
<li>strictfp</li>
<li>super</li>
<li>switch</li>
<li>synchronized</li>
<li>this</li>
<li>throw</li>
<li>throws</li>
<li>transient</li>
<li>try</li>
<li>void</li>
<li>volatile</li>
<li>while</li>
</ul>
