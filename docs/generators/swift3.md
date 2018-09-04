
CONFIG OPTIONS for swift3

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	projectName
	    Project name in Xcode

	responseAs
	    Optionally use libraries to manage response.  Currently PromiseKit, RxSwift are available.

	unwrapRequired
	    Treat 'required' properties in response as non-optional (which would crash the app if api returns null as opposed to required option specified in json schema

	objcCompatible
	    Add additional properties and methods for Objective-C compatibility (default: false)

	podSource
	    Source information used for Podspec

	podVersion
	    Version used for Podspec

	podAuthors
	    Authors used for Podspec

	podSocialMediaURL
	    Social Media URL used for Podspec

	podDocsetURL
	    Docset URL used for Podspec

	podLicense
	    License used for Podspec

	podHomepage
	    Homepage used for Podspec

	podSummary
	    Summary used for Podspec

	podDescription
	    Description used for Podspec

	podScreenshots
	    Screenshots used for Podspec

	podDocumentationURL
	    Documentation URL used for Podspec

	swiftUseApiNamespace
	    Flag to make all the API classes inner-class of {{projectName}}API

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

	lenientTypeCast
	    Accept and cast values for simple types (string->bool, string->int, int->string) (Default: false)

Back to the [generators list](README.md)
