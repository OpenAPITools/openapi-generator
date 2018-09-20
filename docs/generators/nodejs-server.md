
CONFIG OPTIONS for nodejs-server

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	googleCloudFunctions
	    When specified, it will generate the code which runs within Google Cloud Functions instead of standalone Node.JS server. See https://cloud.google.com/functions/docs/quickstart for the details of how to deploy the generated code. (Default: false)

	exportedName
	    When the generated code will be deployed to Google Cloud Functions, this option can be used to update the name of the exported function. By default, it refers to the basePath. This does not affect normal standalone nodejs server code.

	serverPort
	    TCP port to listen on.

Back to the [generators list](README.md)
