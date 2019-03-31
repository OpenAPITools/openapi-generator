
---
id: generator-opts-client-java
title: Config Options for java
sidebar_label: java
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
|groupId|groupId in generated pom.xml| |null|
|artifactId|artifactId in generated pom.xml| |null|
|artifactVersion|artifact version in generated pom.xml| |null|
|artifactUrl|artifact URL in generated pom.xml| |null|
|artifactDescription|artifact description in generated pom.xml| |null|
|scmConnection|SCM connection in generated pom.xml| |null|
|scmDeveloperConnection|SCM developer connection in generated pom.xml| |null|
|scmUrl|SCM URL in generated pom.xml| |null|
|developerName|developer name in generated pom.xml| |null|
|developerEmail|developer email in generated pom.xml| |null|
|developerOrganization|developer organization in generated pom.xml| |null|
|developerOrganizationUrl|developer organization URL in generated pom.xml| |null|
|licenseName|The name of the license| |null|
|licenseUrl|The URL of the license| |null|
|sourceFolder|source folder for generated code| |null|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|bigDecimalAsString|Treat BigDecimal values as Strings to avoid precision loss.| |false|
|fullJavaUtil|whether to use fully qualified name for classes under java.util. This option only works for Java API client| |false|
|hideGenerationTimestamp|hides the timestamp when files were generated| |null|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**joda**</dt><dd>Joda (for legacy app only)</dd><dt>**legacy**</dt><dd>Legacy java.util.Date (if you really have a good reason not to use threetenbp</dd><dt>**java8-localdatetime**</dt><dd>Java 8 using LocalDateTime (for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (preferred for jdk 1.8+) - note: this also sets &quot;java8&quot; to true</dd><dt>**threetenbp**</dt><dd>Backport of JSR310 (preferred for jdk &lt; 1.8)</dd><dl>|null|
|java8|Option. Use Java8 classes instead of third party equivalents|<dl><dt>**true**</dt><dd>Use Java 8 classes such as Base64</dd><dt>**false**</dt><dd>Various third party libraries as needed</dd><dl>|null|
|disableHtmlEscaping|Disable HTML escaping of JSON strings when using gson (needed to avoid problems with byte[] fields)| |false|
|booleanGetterPrefix|Set booleanGetterPrefix (default value 'get')| |null|
|parentGroupId|parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentArtifactId|parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentVersion|parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|snapshotVersion|Uses a SNAPSHOT version.| |null|
|useRxJava|Whether to use the RxJava adapter with the retrofit2 library.| |false|
|useRxJava2|Whether to use the RxJava2 adapter with the retrofit2 library.| |false|
|parcelableModel|Whether to generate models for Android that implement Parcelable with the okhttp-gson library.| |false|
|usePlayWS|Use Play! Async HTTP client (Play WS API)| |false|
|playVersion|Version of Play! Framework (possible values &quot;play24&quot;, &quot;play25&quot; (default), &quot;play26&quot;)| |null|
|supportJava6|Whether to support Java6 with the Jersey1 library.| |false|
|useBeanValidation|Use BeanValidation API annotations| |false|
|performBeanValidation|Perform BeanValidation| |false|
|useGzipFeature|Send gzip-encoded requests| |false|
|useRuntimeException|Use RuntimeException instead of Exception| |false|
|feignVersion|Version of OpenFeign: '10.x', '9.x' (default)| |false|
|useReflectionEqualsHashCode|Use org.apache.commons.lang3.builder for equals and hashCode in the models. WARNING: This will fail under a security manager, unless the appropriate permissions are set up correctly and also there's potential performance impact.| |false|
|library|library template (sub-template) to use|<dl><dt>**jersey1**</dt><dd>HTTP client: Jersey client 1.19.x. JSON processing: Jackson 2.8.x. Enable Java6 support using '-DsupportJava6=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.</dd><dt>**feign**</dt><dd>HTTP client: OpenFeign 9.x or 10.x. JSON processing: Jackson 2.8.x. To enable OpenFeign 10.x, set the 'feignVersion' option to '10.x'</dd><dt>**jersey2**</dt><dd>HTTP client: Jersey client 2.25.1. JSON processing: Jackson 2.8.x</dd><dt>**okhttp-gson**</dt><dd>HTTP client: OkHttp 3.x. JSON processing: Gson 2.8.x. Enable Parcelable models on Android using '-DparcelableModel=true'. Enable gzip request encoding using '-DuseGzipFeature=true'.</dd><dt>**retrofit**</dt><dd>HTTP client: OkHttp 2.x. JSON processing: Gson 2.x (Retrofit 1.9.0). IMPORTANT NOTE: retrofit1.x is no longer actively maintained so please upgrade to 'retrofit2' instead.</dd><dt>**retrofit2**</dt><dd>HTTP client: OkHttp 3.x. JSON processing: Gson 2.x (Retrofit 2.3.0). Enable the RxJava adapter using '-DuseRxJava[2]=true'. (RxJava 1.x or 2.x)</dd><dt>**resttemplate**</dt><dd>HTTP client: Spring RestTemplate 4.x. JSON processing: Jackson 2.8.x</dd><dt>**webclient**</dt><dd>HTTP client: Spring WebClient 5.x. JSON processing: Jackson 2.9.x</dd><dt>**resteasy**</dt><dd>HTTP client: Resteasy client 3.x. JSON processing: Jackson 2.8.x</dd><dt>**vertx**</dt><dd>HTTP client: VertX client 3.x. JSON processing: Jackson 2.8.x</dd><dt>**google-api-client**</dt><dd>HTTP client: Google API client 1.x. JSON processing: Jackson 2.8.x</dd><dt>**rest-assured**</dt><dd>HTTP client: rest-assured : 3.x. JSON processing: Gson 2.x. Only for Java8</dd><dl>|okhttp-gson|
