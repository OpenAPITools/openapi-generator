---
title: Config Options for java-play-framework
sidebar_label: java-play-framework
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |apimodels|
|apiPackage|package for generated api classes| |controllers|
|invokerPackage|root package for generated code| |org.openapitools.api|
|groupId|groupId in generated pom.xml| |org.openapitools|
|artifactId|artifactId in generated pom.xml. This also becomes part of the generated library's filename| |openapi-java-playframework|
|artifactVersion|artifact version in generated pom.xml. This also becomes part of the generated library's filename| |1.0.0|
|artifactUrl|artifact URL in generated pom.xml| |https://github.com/openapitools/openapi-generator|
|artifactDescription|artifact description in generated pom.xml| |OpenAPI Java|
|scmConnection|SCM connection in generated pom.xml| |scm:git:git@github.com:openapitools/openapi-generator.git|
|scmDeveloperConnection|SCM developer connection in generated pom.xml| |scm:git:git@github.com:openapitools/openapi-generator.git|
|scmUrl|SCM URL in generated pom.xml| |https://github.com/openapitools/openapi-generator|
|developerName|developer name in generated pom.xml| |OpenAPI-Generator Contributors|
|developerEmail|developer email in generated pom.xml| |team@openapitools.org|
|developerOrganization|developer organization in generated pom.xml| |OpenAPITools.org|
|developerOrganizationUrl|developer organization URL in generated pom.xml| |http://openapitools.org|
|licenseName|The name of the license| |Unlicense|
|licenseUrl|The URL of the license| |http://unlicense.org|
|sourceFolder|source folder for generated code| |/app|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|bigDecimalAsString|Treat BigDecimal values as Strings to avoid precision loss.| |false|
|fullJavaUtil|whether to use fully qualified name for classes under java.util. This option only works for Java API client| |false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |false|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**joda**</dt><dd>Joda (for legacy app only)</dd><dt>**legacy**</dt><dd>Legacy java.util.Date (if you really have a good reason not to use threetenbp</dd><dt>**java8-localdatetime**</dt><dd>Java 8 using LocalDateTime (for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (preferred for jdk 1.8+) - note: this also sets &quot;java8&quot; to true</dd><dt>**threetenbp**</dt><dd>Backport of JSR310 (preferred for jdk &lt; 1.8)</dd><dl>|threetenbp|
|java8|Option. Use Java8 classes instead of third party equivalents|<dl><dt>**true**</dt><dd>Use Java 8 classes such as Base64</dd><dt>**false**</dt><dd>Various third party libraries as needed</dd><dl>|false|
|disableHtmlEscaping|Disable HTML escaping of JSON strings when using gson (needed to avoid problems with byte[] fields)| |false|
|booleanGetterPrefix|Set booleanGetterPrefix| |get|
|parentGroupId|parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentArtifactId|parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentVersion|parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|snapshotVersion|Uses a SNAPSHOT version.|<dl><dt>**true**</dt><dd>Use a SnapShot Version</dd><dt>**false**</dt><dd>Use a Release Version</dd><dl>|null|
|title|server title name or client service name| |openapi-java-playframework|
|configPackage|configuration package for generated code| |org.openapitools.configuration|
|basePackage|base package for generated code| |org.openapitools|
|controllerOnly|Whether to generate only API interface stubs without the server files.| |false|
|useBeanValidation|Use BeanValidation API annotations| |true|
|useInterfaces|Makes the controllerImp implements an interface to facilitate automatic completion when updating from version x to y of your spec| |true|
|handleExceptions|Add a 'throw exception' to each controller function. Add also a custom error handler where you can put your custom logic| |true|
|wrapCalls|Add a wrapper to each controller function to handle things like metrics, response modification, etc..| |true|
|useSwaggerUI|Add a route to /api which show your documentation in swagger-ui. Will also import needed dependencies| |true|
