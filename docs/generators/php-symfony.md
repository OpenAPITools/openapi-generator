
CONFIG OPTIONS for php-symfony

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
	    The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets

	bundleName
	    The name of the Symfony bundle. The template uses {{bundleName}}

	composerProjectName
	    The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

	phpLegacySupport
	    Should the generated code be compatible with PHP 5.x? (Default: true)

Back to the [generators list](README.md)
