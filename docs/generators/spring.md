---
title: Config Options for spring
sidebar_label: spring
---

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|sortParamsByRequiredFlag|Sort method arguments to place required parameters before optional parameters.| |true|
|ensureUniqueParams|Whether to ensure parameter names are unique in an operation (rename parameters that are not).| |true|
|allowUnicodeIdentifiers|boolean, toggles whether unicode identifiers are allowed in names or not, default is false| |false|
|prependFormOrBodyParameters|Add form or body parameters to the beginning of the parameter list.| |false|
|modelPackage|package for generated models| |org.openapitools.model|
|apiPackage|package for generated api classes| |org.openapitools.api|
|invokerPackage|root package for generated code| |org.openapitools.api|
|groupId|groupId in generated pom.xml| |org.openapitools|
|artifactId|artifactId in generated pom.xml. This also becomes part of the generated library's filename| |openapi-spring|
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
|sourceFolder|source folder for generated code| |src/main/java|
|serializableModel|boolean - toggle &quot;implements Serializable&quot; for generated models| |false|
|bigDecimalAsString|Treat BigDecimal values as Strings to avoid precision loss.| |false|
|fullJavaUtil|whether to use fully qualified name for classes under java.util. This option only works for Java API client| |false|
|hideGenerationTimestamp|Hides the generation timestamp when files are generated.| |false|
|withXml|whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML)| |false|
|dateLibrary|Option. Date library to use|<dl><dt>**joda**</dt><dd>Joda (for legacy app only)</dd><dt>**legacy**</dt><dd>Legacy java.util.Date (if you really have a good reason not to use threetenbp</dd><dt>**java8-localdatetime**</dt><dd>Java 8 using LocalDateTime (for legacy app only)</dd><dt>**java8**</dt><dd>Java 8 native JSR310 (preferred for jdk 1.8+) - note: this also sets &quot;java8&quot; to true</dd><dt>**threetenbp**</dt><dd>Backport of JSR310 (preferred for jdk &lt; 1.8)</dd><dl>|threetenbp|
|java8|Option. Use Java8 classes instead of third party equivalents|<dl><dt>**true**</dt><dd>Use Java 8 classes such as Base64. Use java8 default interface when a responseWrapper is used</dd><dt>**false**</dt><dd>Various third party libraries as needed</dd><dl>|false|
|disableHtmlEscaping|Disable HTML escaping of JSON strings when using gson (needed to avoid problems with byte[] fields)| |false|
|booleanGetterPrefix|Set booleanGetterPrefix| |get|
|parentGroupId|parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentArtifactId|parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|parentVersion|parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect| |null|
|snapshotVersion|Uses a SNAPSHOT version.|<dl><dt>**true**</dt><dd>Use a SnapShot Version</dd><dt>**false**</dt><dd>Use a Release Version</dd><dl>|null|
|title|server title name or client service name| |OpenAPI Spring|
|configPackage|configuration package for generated code| |org.openapitools.configuration|
|basePackage|base package (invokerPackage) for generated code| |org.openapitools|
|interfaceOnly|Whether to generate only API interface stubs without the server files.| |false|
|delegatePattern|Whether to generate the server files using the delegate pattern| |false|
|singleContentTypes|Whether to select only one produces/consumes content-type by operation.| |false|
|skipDefaultInterface|Whether to generate default implementations for java8 interfaces| |false|
|async|use async Callable controllers| |false|
|reactive|wrap responses in Mono/Flux Reactor types (spring-boot only)| |false|
|responseWrapper|wrap the responses in given type (Future,Callable,CompletableFuture,ListenableFuture,DeferredResult,HystrixCommand,RxObservable,RxSingle or fully qualified type)| |null|
|virtualService|Generates the virtual service. For more details refer - https://github.com/elan-venture/virtualan/wiki| |false|
|useTags|use tags for creating interface and controller classnames| |false|
|useBeanValidation|Use BeanValidation API annotations| |true|
|performBeanValidation|Use Bean Validation Impl. to perform BeanValidation| |false|
|implicitHeaders|Skip header parameters in the generated API methods using @ApiImplicitParams annotation.| |false|
|swaggerDocketConfig|Generate Spring OpenAPI Docket configuration class.| |false|
|apiFirst|Generate the API from the OAI spec at server compile time (API first approach)| |false|
|useOptional|Use Optional container for optional parameters| |false|
|hateoas|Use Spring HATEOAS library to allow adding HATEOAS links| |false|
|returnSuccessCode|Generated server returns 2xx code| |false|
|unhandledException|Declare operation methods to throw a generic exception and allow unhandled exceptions (useful for Spring `@ControllerAdvice` directives).| |false|
|library|library template (sub-template)|<dl><dt>**spring-boot**</dt><dd>Spring-boot Server application using the SpringFox integration.</dd><dt>**spring-mvc**</dt><dd>Spring-MVC Server application using the SpringFox integration.</dd><dt>**spring-cloud**</dt><dd>Spring-Cloud-Feign client with Spring-Boot auto-configured settings.</dd><dl>|spring-boot|
