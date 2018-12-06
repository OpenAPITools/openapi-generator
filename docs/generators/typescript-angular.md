
CONFIG OPTIONS for typescript-angular

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
	    The name under which you want to publish generated npm package. Required to generate a full angular package

	npmVersion
	    The version of your npm package. Default is '1.0.0'

	npmRepository
	    Use this property to set an url your private npmRepo in the package.json

	snapshot
	    When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm (Default: false)

	withInterfaces
	    Setting this property to true will generate interfaces next to the default class implementations. (Default: false)

	taggedUnions
	    Use discriminators to create tagged unions instead of extending interfaces. (Default: false)

	providedInRoot
	    Use this property to provide Injectables in root (it is only valid in angular version greater or equal to 6.0.0). (Default: false)

	ngVersion
	    The version of Angular. Default is '4.3'

	serviceSuffix
	    The suffix of the generated service. Default is 'Service'.

	serviceFileSuffix
	    The suffix of the file of the generated service (service<suffix>.ts). Default is '.service'.

	modelSuffix
	    The suffix of the generated model. Default is ''.

	modelFileSuffix
	    The suffix of the file of the generated model (model<suffix>.ts). Default is ''.

	fileNaming
	    Naming convention for the output files: 'camelCase', 'kebab-case'. Default is 'camelCase'.

Back to the [generators list](README.md)
