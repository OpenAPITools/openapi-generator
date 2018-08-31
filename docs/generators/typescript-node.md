
CONFIG OPTIONS for typescript-node

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	modelPropertyNaming
	    Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name (Default: camelCase)

	supportsES6
	    Generate code that conforms to ES6. (Default: false)

	npmName
	    The name under which you want to publish generated npm package

	npmVersion
	    The version of your npm package

	npmRepository
	    Use this property to set an url your private npmRepo in the package.json

	snapshot
	    When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm (Default: false)

Back to the [generators list](README.md)
