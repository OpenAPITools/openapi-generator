
CONFIG OPTIONS for kotlin-spring

	sourceFolder
	    source folder for generated code (Default: src/main/kotlin)

	packageName
	    Generated artifact package name. (Default: org.openapitools)

	groupId
	    Generated artifact package's organization (i.e. maven groupId). (Default: org.openapitools)

	artifactId
	    Generated artifact id (name of jar).

	artifactVersion
	    Generated artifact's package version. (Default: 1.0.0)

	enumPropertyNaming
	    Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original' (Default: camelCase)

	title
	    server title name or client service name (Default: OpenAPI Kotlin Spring)

	basePackage
	    base package (invokerPackage) for generated code (Default: org.openapitools)

	serverPort
	    configuration the port in which the sever is to run on (Default: 8080)

	modelPackage
	    model package for generated code (Default: org.openapitools.model)

	apiPackage
	    api package for generated code (Default: org.openapitools.api)

	exceptionHandler
	    generate default global exception handlers (Default: true)

	gradleBuildFile
	    generate a gradle build file using the Kotlin DSL (Default: true)

	swaggerAnnotations
	    generate swagger annotations to go alongside controllers and models (Default: false)

	serviceInterface
	    generate service interfaces to go alongside controllers. In most cases this option would be used to update an existing project, so not to override implementations. Useful to help facilitate the generation gap pattern (Default: false)

	serviceImplementation
	    generate stub service implementations that extends service interfaces. If this is set to true service interfaces will also be generated (Default: false)

	useBeanValidation
	    Use BeanValidation API annotations to validate data types (Default: true)

	library
	    library template (sub-template) to use (Default: spring-boot)
	        spring-boot - Spring-boot Server application.

Back to the [generators list](README.md)
