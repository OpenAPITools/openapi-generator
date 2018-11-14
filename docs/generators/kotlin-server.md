
CONFIG OPTIONS for kotlin-server

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

	parcelizeModels
	    toggle "@Parcelize" for generated models

	library
	    library template (sub-template) to use (Default: ktor)
	        ktor - ktor framework

	featureAutoHead
	    Automatically provide responses to HEAD requests for existing routes that have the GET verb defined. (Default: true)

	featureConditionalHeaders
	    Avoid sending content if client already has same content, by checking ETag or LastModified properties. (Default: false)

	featureHSTS
	    Avoid sending content if client already has same content, by checking ETag or LastModified properties. (Default: true)

	featureCORS
	    Ktor by default provides an interceptor for implementing proper support for Cross-Origin Resource Sharing (CORS). See enable-cors.org. (Default: false)

	featureCompression
	    Adds ability to compress outgoing content using gzip, deflate or custom encoder and thus reduce size of the response. (Default: true)

Back to the [generators list](README.md)
