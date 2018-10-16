
CONFIG OPTIONS for csharp

	packageName
	    C# package name (convention: Title.Case). (Default: Org.OpenAPITools)

	packageVersion
	    C# package version. (Default: 1.0.0)

	sourceFolder
	    source folder for generated code (Default: src)

	packageGuid
	    The GUID that will be associated with the C# project

	interfacePrefix
	    Prefix interfaces with a community standard or widely accepted prefix. (Default: I)

	targetFramework
	    The target .NET framework version. (Default: v4.5)
	        v3.5 - .NET Framework 3.5 compatible
	        v4.0 - .NET Framework 4.0 compatible
	        v4.5 - .NET Framework 4.5+ compatible
	        v5.0 - .NET Standard 1.3 compatible
	        uwp - Universal Windows Platform (IMPORTANT: this will be decommissioned and replaced by v5.0)

	modelPropertyNaming
	    Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name (Default: PascalCase)

	hideGenerationTimestamp
	    Hides the generation timestamp when files are generated. (Default: true)

	sortParamsByRequiredFlag
	    Sort method arguments to place required parameters before optional parameters. (Default: true)

	useDateTimeOffset
	    Use DateTimeOffset to model date-time properties (Default: false)

	useCollection
	    Deserialize array types to Collection<T> instead of List<T>. (Default: false)

	returnICollection
	    Return ICollection<T> instead of the concrete type. (Default: false)

	optionalMethodArgument
	    C# Optional method argument, e.g. void square(int x=10) (.net 4.0+ only). (Default: true)

	optionalAssemblyInfo
	    Generate AssemblyInfo.cs. (Default: true)

	optionalProjectFile
	    Generate {PackageName}.csproj. (Default: true)

	optionalEmitDefaultValues
	    Set DataMember's EmitDefaultValue. (Default: false)

	generatePropertyChanged
	    Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file. (Default: false)

	nonPublicApi
	    Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers. (Default: false)

	allowUnicodeIdentifiers
	    boolean, toggles whether unicode identifiers are allowed in names or not, default is false (Default: false)

	netCoreProjectFile
	    Use the new format (.NET Core) for .NET project files (.csproj). (Default: false)

	validatable
	    Generates self-validatable models. (Default: true)

Back to the [generators list](README.md)
