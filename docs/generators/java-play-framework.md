---
title: Config Options for java-play-framework
sidebar_label: java-play-framework
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|additionalModelTypeAnnotations|Additional annotations for model type(class level annotations)| |null|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|apiPackage|package for generated api classes| |controllers|
|artifactDescription|artifact description in generated pom.xml| |OpenAPI Java|
|artifactId|artifactId in generated pom.xml. This also becomes part of the generated library's filename| |openapi-java-playframework|
|artifactUrl|artifact URL in generated pom.xml| |https://github.com/openapitools/openapi-generator|
|artifactVersion|artifact version in generated pom.xml. This also becomes part of the generated library's filename| |1.0.0|
|basePackage|base package for generated code| |org.openapitools|
|bigDecimalAsString|Treat BigDecimal values as Strings to avoid precision loss.| |false|
|booleanGetterPrefix|Set booleanGetterPrefix| |get|
|configPackage|configuration package for generated code| |org.openapitools.configuration|
|controllerOnly|Whether to generate only API interface stubs without the server files.| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**joda**</dt><dd>Joda (for legacy app only)</dd><dt>**legacy**</dt><dd>Legacy java.util.Date (if you really have a good reason not to use threetenbp</dd><dt>**java8-localdatetime**</dt><dd>Java 8 using LocalDateTime (for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (preferred for jdk 1.8+) - note: this also sets &quot;java8&quot; to true</dd><dt>**threetenbp**</dt><dd>Backport of JSR310 (preferred for jdk &lt; 1.8)</dd><dl>|threetenbp|
|developerEmail|developer email in generated pom.xml| |team@openapitools.org|
|developerName|developer name in generated pom.xml| |OpenAPI-Generator Contributors|
|developerOrganization|developer organization in generated pom.xml| |OpenAPITools.org|
|developerOrganizationUrl|developer organization URL in generated pom.xml| |http://openapitools.org|
|disableHtmlEscaping|Disable HTML escaping of JSON strings when using gson (needed to avoid problems with byte[] fields)| |false|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|fullJavaUtil|whether to use fully qualified name for classes under java.util. This option only works for Java API client| |false|
|groupId|groupId in generated pom.xml| |org.openapitools|
|handleExceptions|Add a 'throw exception' to each controller function. Add also a custom error handler where you can put your custom logic| |true|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |false|
|invokerPackage|root package for generated code| |org.openapitools.api|
|java8|Option. Use Java8 classes instead of third party equivalents|<dl><dt>**true**</dt><dd>Use Java 8 classes such as Base64</dd><dt>**false**</dt><dd>Various third party libraries as needed</dd><dl>|false|
|licenseName|The name of the license| |Unlicense|
|licenseUrl|The URL of the license| |http://unlicense.org|
|modelPackage|package for generated models| |apimodels|
|parentArtifactId|parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentGroupId|parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentVersion|parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|scmConnection|SCM connection in generated pom.xml| |scm:git:git@github.com:openapitools/openapi-generator.git|
|scmDeveloperConnection|SCM developer connection in generated pom.xml| |scm:git:git@github.com:openapitools/openapi-generator.git|
|scmUrl|SCM URL in generated pom.xml| |https://github.com/openapitools/openapi-generator|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|snapshotVersion|Uses a SNAPSHOT version.|<dl><dt>**true**</dt><dd>Use a SnapShot Version</dd><dt>**false**</dt><dd>Use a Release Version</dd><dl>|null|
|sortModelPropertiesByRequiredFlag|Sort model properties to place required parameters before optional parameters.| |true|
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|sourceFolder|source folder for generated code| |/app|
|title|server title name or client service name| |openapi-java-playframework|
|useBeanValidation|Use BeanValidation API annotations| |true|
|useInterfaces|Makes the controllerImp implements an interface to facilitate automatic completion when updating from version x to y of your spec| |true|
|useSwaggerUI|Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies| |true|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|wrapCalls|Add a wrapper to each controller function to handle things like metrics, response modification, etc..| |true|

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
<li>apiclient</li>
<li>apiexception</li>
<li>apiresponse</li>
<li>assert</li>
<li>boolean</li>
<li>break</li>
<li>byte</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>class</li>
<li>configuration</li>
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
<li>localreturntype</li>
<li>localvaraccept</li>
<li>localvaraccepts</li>
<li>localvarauthnames</li>
<li>localvarcollectionqueryparams</li>
<li>localvarcontenttype</li>
<li>localvarcontenttypes</li>
<li>localvarcookieparams</li>
<li>localvarformparams</li>
<li>localvarheaderparams</li>
<li>localvarpath</li>
<li>localvarpostbody</li>
<li>localvarqueryparams</li>
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
<li>stringutil</li>
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
