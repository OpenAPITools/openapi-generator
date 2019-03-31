
---
id: generator-opts-server-jaxrs-cxf-cdi
title: Config Options for jaxrs-cxf-cdi
sidebar_label: jaxrs-cxf-cdi
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
|implFolder|folder for generated implementation code| |null|
|title|a title describing the application| |null|
|useBeanValidation|Use BeanValidation API annotations| |true|
|serverPort|The port on which the server should be started| |null|
|library|library template (sub-template) to use|<dl><dt>**&lt;default&gt;**</dt><dd>JAXRS</dd><dl>|&lt;default&gt;|
|generatePom|Whether to generate pom.xml if the file does not already exist.| |true|
|interfaceOnly|Whether to generate only API interface stubs without the server files.| |false|
|returnResponse|Whether generate API interface should return javax.ws.rs.core.Response instead of a deserialized entity. Only useful if interfaceOnly is true.| |false|
|useSwaggerAnnotations|Whether to generate Swagger annotations.| |true|
|useBeanValidation|Use BeanValidation API annotations| |true|
