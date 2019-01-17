
CONFIG OPTIONS for haskell-http-client

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	cabalPackage
	    Set the cabal package name, which consists of one or more alphanumeric words separated by hyphens

	cabalVersion
	    Set the cabal version number, consisting of a sequence of one or more integers separated by dots

	baseModule
	    Set the base module namespace

	requestType
	    Set the name of the type used to generate requests

	configType
	    Set the name of the type used for configuration

	allowFromJsonNulls
	    allow JSON Null during model decoding from JSON (Default: true)

	allowToJsonNulls
	    allow emitting JSON Null during model encoding to JSON (Default: false)

	allowNonUniqueOperationIds
	    allow different API modules to contain the same operationId. Each API must be imported qualified (Default: false)

	generateLenses
	    Generate Lens optics for Models (Default: true)

	generateModelConstructors
	    Generate smart constructors (only supply required fields) for models (Default: true)

	generateEnums
	    Generate specific datatypes for OpenAPI enums (Default: true)

	generateFormUrlEncodedInstances
	    Generate FromForm/ToForm instances for models that are used by operations that produce or consume application/x-www-form-urlencoded (Default: true)

	inlineMimeTypes
	    Inline (hardcode) the content-type and accept parameters on operations, when there is only 1 option (Default: true)

	modelDeriving
	    Additional classes to include in the deriving() clause of Models

	strictFields
	    Add strictness annotations to all model fields (Default: true)

	useMonadLogger
	    Use the monad-logger package to provide logging (if false, use the katip logging package) (Default: false)

	dateTimeFormat
	    format string used to parse/render a datetime

	dateFormat
	    format string used to parse/render a date (Default: %Y-%m-%d)

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

Back to the [generators list](README.md)
