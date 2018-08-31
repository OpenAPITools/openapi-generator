
CONFIG OPTIONS for bash

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	curlOptions
	    Default cURL options

	processMarkdown
	    Convert all Markdown Markup into terminal formatting (Default: false)

	scriptName
	    The name of the script that will be generated (e.g. petstore-cli)

	generateBashCompletion
	    Whether to generate the Bash completion script (Default: false)

	generateZshCompletion
	    Whether to generate the Zsh completion script (Default: false)

	hostEnvironmentVariable
	    Name of environment variable where host can be defined (e.g. PETSTORE_HOST='http://api.openapitools.org:8080')

	basicAuthEnvironmentVariable
	    Name of environment variable where username and password can be defined (e.g. PETSTORE_CREDS='username:password')

	apiKeyAuthEnvironmentVariable
	    Name of environment variable where API key can be defined (e.g. PETSTORE_APIKEY='kjhasdGASDa5asdASD') (Default: false)

Back to the [generators list](README.md)
