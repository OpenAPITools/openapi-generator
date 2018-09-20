
CONFIG OPTIONS for csharp-nancyfx

	packageName
	    C# package name (convention: Title.Case). (Default: Org.OpenAPITools)

	packageVersion
	    C# package version. (Default: 1.0.0)

	sourceFolder
	    source folder for generated code (Default: src)

	interfacePrefix
	    Prefix interfaces with a community standard or widely accepted prefix. (Default: )

	packageGuid
	    The GUID that will be associated with the C# project

	packageContext
	    Optionally overrides the PackageContext which determines the namespace (namespace=packageName.packageContext). If not set, packageContext will default to basePath.

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	optionalProjectFile
	    Generate {PackageName}.csproj. (Default: true)

	useDateTimeOffset
	    Use DateTimeOffset to model date-time properties (Default: false)

	useCollection
	    Deserialize array types to Collection<T> instead of List<T>. (Default: false)

	returnICollection
	    Return ICollection<T> instead of the concrete type. (Default: false)

	immutable
	    Enabled by default. If disabled generates model classes with setters (Default: true)

	writeModulePath
	    Enabled by default. If disabled, module paths will not mirror api base path (Default: true)

	asyncServer
	    Set to true to enable the generation of async routes/endpoints. (Default: false)

Back to the [generators list](README.md)
