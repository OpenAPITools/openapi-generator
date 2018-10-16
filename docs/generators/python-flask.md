
CONFIG OPTIONS for python-flask

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	packageName
	    python package name (convention: snake_case). (Default: openapi_server)

	packageVersion
	    python package version. (Default: 1.0.0)

	controllerPackage
	    controller package (Default: controllers)

	defaultController
	    default controller (Default: default_controller)

	supportPython2
	    support python2 (Default: false)

	serverPort
	    TCP port to listen to in app.run (Default: 8080)

Back to the [generators list](README.md)
