
CONFIG OPTIONS for jaxrs-jersey

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	modelPackage
	    package for generated models

	apiPackage
	    package for generated api classes

	invokerPackage
	    root package for generated code

	groupId
	    groupId in generated pom.xml

	artifactId
	    artifactId in generated pom.xml

	artifactVersion
	    artifact version in generated pom.xml

	artifactUrl
	    artifact URL in generated pom.xml

	artifactDescription
	    artifact description in generated pom.xml

	scmConnection
	    SCM connection in generated pom.xml

	scmDeveloperConnection
	    SCM developer connection in generated pom.xml

	scmUrl
	    SCM URL in generated pom.xml

	developerName
	    developer name in generated pom.xml

	developerEmail
	    developer email in generated pom.xml

	developerOrganization
	    developer organization in generated pom.xml

	developerOrganizationUrl
	    developer organization URL in generated pom.xml

	licenseName
	    The name of the license

	licenseUrl
	    The URL of the license

	sourceFolder
	    source folder for generated code

	localVariablePrefix
	    prefix for generated code members and local variables

	serializableModel
	    boolean - toggle "implements Serializable" for generated models (Default: false)

	bigDecimalAsString
	    Treat BigDecimal values as Strings to avoid precision loss. (Default: false)

	fullJavaUtil
	    whether to use fully qualified name for classes under java.util. This option only works for Java API client (Default: false)

	hideGenerationTimestamp
	    hides the timestamp when files were generated

	withXml
	    whether to include support for application/xml content type and include XML annotations in the model (works with libraries that provide support for JSON and XML) (Default: false)

	dateLibrary
	    Option. Date library to use
	        joda - Joda (for legacy app only)
	        legacy - Legacy java.util.Date (if you really have a good reason not to use threetenbp
	        java8-localdatetime - Java 8 using LocalDateTime (for legacy app only)
	        java8 - Java 8 native JSR310 (preferred for jdk 1.8+) - note: this also sets "java8" to true
	        threetenbp - Backport of JSR310 (preferred for jdk < 1.8)

	java8
	    Option. Use Java8 classes instead of third party equivalents
	        true - Use Java 8 classes such as Base64
	        false - Various third party libraries as needed

	disableHtmlEscaping
	    Disable HTML escaping of JSON strings when using gson (needed to avoid problems with byte[] fields) (Default: false)

	booleanGetterPrefix
	    Set booleanGetterPrefix (default value 'get')

	parentGroupId
	    parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect

	parentArtifactId
	    parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect

	parentVersion
	    parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect

	implFolder
	    folder for generated implementation code

	title
	    a title describing the application

	useBeanValidation
	    Use BeanValidation API annotations (Default: true)

	serverPort
	    The port on which the server should be started

	library
	    library template (sub-template) to use (Default: jersey2)
	        jersey1 - Jersey core 1.x
	        jersey2 - Jersey core 2.x

	supportJava6
	    Whether to support Java6 with the Jersey1/2 library. (Default: false)

	useTags
	    use tags for creating interface and controller classnames (Default: false)

Back to the [generators list](README.md)
