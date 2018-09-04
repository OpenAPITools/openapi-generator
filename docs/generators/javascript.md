
CONFIG OPTIONS for javascript

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	ensureUniqueParams
	    Whether to ensure parameter names are unique in an operation (rename parameters that are not). (Default: true)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	prependFormOrBodyParameters
	    Add form or body parameters to the beginning of the parameter list. (Default: false)

	sourceFolder
	    source folder for generated code (Default: src)

	localVariablePrefix
	    prefix for generated code members and local variables

	invokerPackage
	    root package for generated code

	apiPackage
	    package for generated api classes

	modelPackage
	    package for generated models

	projectName
	    name of the project (Default: generated from info.title or "openapi-js-client")

	moduleName
	    module name for AMD, Node or globals (Default: generated from <projectName>)

	projectDescription
	    description of the project (Default: using info.description or "Client library of <projectName>")

	projectVersion
	    version of the project (Default: using info.version or "1.0.0")

	licenseName
	    name of the license the project uses (Default: using info.license.name)

	usePromises
	    use Promises as return values from the client API, instead of superagent callbacks (Default: false)

	emitModelMethods
	    generate getters and setters for model properties (Default: false)

	emitJSDoc
	    generate JSDoc comments (Default: true)

	useInheritance
	    use JavaScript prototype chains & delegation for inheritance (Default: true)

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

	useES6
	    use JavaScript ES6 (ECMAScript 6) (beta). Default is ES5. (Default: false)

	modelPropertyNaming
	    Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name (Default: camelCase)

Back to the [generators list](README.md)
