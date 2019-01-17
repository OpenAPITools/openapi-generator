
CONFIG OPTIONS for php

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

	variableNamingConvention
	    naming convention of variable name, e.g. camelCase. (Default: snake_case)

	invokerPackage
	    The main namespace to use for all classes. e.g. Yay\Pets

	packageName
	    The main package name for classes. e.g. GeneratedPetstore

	srcBasePath
	    The directory to serve as source root.

	gitUserId
	    Git user ID, e.g. openapitools.

	gitRepoId
	    Git repo ID, e.g. openapi-generator.

	artifactVersion
	    The version to use in the composer package version field. e.g. 1.2.3

	composerVendorName
	    The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets. IMPORTANT NOTE (2016/03): composerVendorName will be deprecated and replaced by gitUserId in the next openapi-generator release

	composerProjectName
	    The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client. IMPORTANT NOTE (2016/03): composerProjectName will be deprecated and replaced by gitRepoId in the next openapi-generator release

	hideGenerationTimestamp
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: true)

Back to the [generators list](README.md)
