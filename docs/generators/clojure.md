
CONFIG OPTIONS for clojure

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	projectName
	    name of the project (Default: generated from info.title or "openapi-clj-client")

	projectDescription
	    description of the project (Default: using info.description or "Client library of <projectNname>")

	projectVersion
	    version of the project (Default: using info.version or "1.0.0")

	projectUrl
	    URL of the project (Default: using info.contact.url or not included in project.clj)

	projectLicenseName
	    name of the license the project uses (Default: using info.license.name or not included in project.clj)

	projectLicenseUrl
	    URL of the license the project uses (Default: using info.license.url or not included in project.clj)

	baseNamespace
	    the base/top namespace (Default: generated from projectName)

Back to the [generators list](README.md)
