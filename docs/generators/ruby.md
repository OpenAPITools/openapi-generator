
CONFIG OPTIONS for ruby

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	gemName
	    gem name (convention: underscore_case). (Default: openapi_client)

	moduleName
	    top module name (convention: CamelCase, usually corresponding to gem name). (Default: OpenAPIClient)

	gemVersion
	    gem version. (Default: 1.0.0)

	gemLicense
	    gem license.  (Default: proprietary)

	gemRequiredRubyVersion
	    gem required Ruby version.  (Default: >= 1.9)

	gemHomepage
	    gem homepage.  (Default: http://org.openapitools)

	gemSummary
	    gem summary.  (Default: A ruby wrapper for the REST APIs)

	gemDescription
	    gem description.  (Default: This gem maps to a REST API)

	gemAuthor
	    gem author (only one is supported).

	gemAuthorEmail
	    gem author email (only one is supported).

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

Back to the [generators list](README.md)
