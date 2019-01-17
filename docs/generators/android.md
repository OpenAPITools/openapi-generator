
CONFIG OPTIONS for android

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
	    groupId for use in the generated build.gradle and pom.xml

	artifactId
	    artifactId for use in the generated build.gradle and pom.xml

	artifactVersion
	    artifact version for use in the generated build.gradle and pom.xml

	sourceFolder
	    source folder for generated code

	useAndroidMavenGradlePlugin
	    A flag to toggle android-maven gradle plugin. (Default: true)

	androidGradleVersion
	    gradleVersion version for use in the generated build.gradle

	androidSdkVersion
	    compileSdkVersion version for use in the generated build.gradle

	androidBuildToolsVersion
	    buildToolsVersion version for use in the generated build.gradle

	serializableModel
	    boolean - toggle "implements Serializable" for generated models (Default: false)

	library
	    library template (sub-template) to use
	        volley - HTTP client: Volley 1.0.19 (default)
	        httpclient - HTTP client: Apache HttpClient 4.3.6. JSON processing: Gson 2.3.1. IMPORTANT: Android client using HttpClient is not actively maintained and will be depecreated in the next major release.

Back to the [generators list](README.md)
