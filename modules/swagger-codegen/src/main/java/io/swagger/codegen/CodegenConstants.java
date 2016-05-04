package io.swagger.codegen;

/**
 * A class for storing constants that are used throughout the project.
 */
public class CodegenConstants {
    public static final String API_PACKAGE = "apiPackage";
    public static final String API_PACKAGE_DESC = "package for generated api classes";

    public static final String MODEL_PACKAGE = "modelPackage";
    public static final String MODEL_PACKAGE_DESC = "package for generated models";

    public static final String TEMPLATE_DIR = "templateDir";


    public static final String INVOKER_PACKAGE = "invokerPackage";
    public static final String INVOKER_PACKAGE_DESC = "root package for generated code";

    public static final String GROUP_ID = "groupId";
    public static final String GROUP_ID_DESC = "groupId in generated pom.xml";

    public static final String ARTIFACT_ID = "artifactId";
    public static final String ARTIFACT_ID_DESC = "artifactId in generated pom.xml";

    public static final String ARTIFACT_VERSION = "artifactVersion";
    public static final String ARTIFACT_VERSION_DESC = "artifact version in generated pom.xml";

    public static final String SOURCE_FOLDER = "sourceFolder";
    public static final String SOURCE_FOLDER_DESC = "source folder for generated code";

    public static final String IMPL_FOLDER = "implFolder";
    public static final String IMPL_FOLDER_DESC = "folder for generated implementation code";
    
    public static final String LOCAL_VARIABLE_PREFIX = "localVariablePrefix";
    public static final String LOCAL_VARIABLE_PREFIX_DESC = "prefix for generated code members and local variables";

    public static final String SERIALIZABLE_MODEL = "serializableModel";
    public static final String SERIALIZABLE_MODEL_DESC = "boolean - toggle \"implements Serializable\" for generated models";

    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING = "bigDecimalAsString";
    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING_DESC = "Treat BigDecimal values as Strings to avoid precision loss.";

    public static final String LIBRARY = "library";
    public static final String LIBRARY_DESC = "library template (sub-template)";

    public static final String SORT_PARAMS_BY_REQUIRED_FLAG = "sortParamsByRequiredFlag";
    public static final String SORT_PARAMS_BY_REQUIRED_FLAG_DESC = "Sort method arguments to place required parameters before optional parameters.";

    public static final String USE_DATETIME_OFFSET = "useDateTimeOffset";
    public static final String USE_DATETIME_OFFSET_DESC = "Use DateTimeOffset to model date-time properties";
	
    public static final String ENSURE_UNIQUE_PARAMS = "ensureUniqueParams";
    public static final String ENSURE_UNIQUE_PARAMS_DESC = "Whether to ensure parameter names are unique in an operation (rename parameters that are not).";

    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";
    public static final String POD_VERSION = "podVersion";

    public static final String OPTIONAL_METHOD_ARGUMENT = "optionalMethodArgument";
    public static final String OPTIONAL_METHOD_ARGUMENT_DESC = "Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).";

    public static final String OPTIONAL_ASSEMBLY_INFO = "optionalAssemblyInfo";
    public static final String OPTIONAL_ASSEMBLY_INFO_DESC = "Generate AssemblyInfo.cs.";

    public static final String USE_COLLECTION = "useCollection";
    public static final String USE_COLLECTION_DESC = "Deserialize array types to Collection<T> instead of List<T>.";

    public static final String RETURN_ICOLLECTION = "returnICollection";
    public static final String RETURN_ICOLLECTION_DESC = "Return ICollection<T> instead of the concrete type.";
    
    public static final String OPTIONAL_PROJECT_FILE = "optionalProjectFile";
    public static final String OPTIONAL_PROJECT_FILE_DESC = "Generate {PackageName}.csproj.";
    
    public static final String OPTIONAL_PROJECT_GUID = "packageGuid";
    public static final String OPTIONAL_PROJECT_GUID_DESC = "The GUID that will be associated with the C# project";
    
    public static final String MODEL_PROPERTY_NAMING = "modelPropertyNaming";
    public static final String MODEL_PROPERTY_NAMING_DESC = "Naming convention for the property: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name";

    public static final String DOTNET_FRAMEWORK = "targetFramework";
    public static final String DOTNET_FRAMEWORK_DESC = "The target .NET framework version.";

    public static enum MODEL_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, original}

    public static final String MODEL_NAME_PREFIX = "modelNamePrefix";
    public static final String MODEL_NAME_PREFIX_DESC = "Prefix that will be prepended to all model names. Default is the empty string.";

    public static final String MODEL_NAME_SUFFIX = "modelNameSuffix";
    public static final String MODEL_NAME_SUFFIX_DESC = "Suffix that will be appended to all model names. Default is the empty string.";

    public static final String OPTIONAL_EMIT_DEFAULT_VALUES = "optionalEmitDefaultValues";
    public static final String OPTIONAL_EMIT_DEFAULT_VALUES_DESC = "Set DataMember's EmitDefaultValue.";

    public static final String GIT_USER_ID = "gitUserId";
    public static final String GIT_USER_ID_DESC = "Git user ID, e.g. swagger-api.";

    public static final String GIT_REPO_ID = "gitRepoId";
    public static final String GIT_REPO_ID_DESC = "Git repo ID, e.g. swagger-codegen.";

    public static final String RELEASE_NOTE = "releaseNote";
    public static final String RELEASE_NOTE_DESC = "Release note, default to 'Minor update'.";

    public static final String HTTP_USER_AGENT = "httpUserAgent";
    public static final String HTTP_USER_AGENT_DESC = "HTTP user agent, e.g. codegen_csharp_api_client, default to 'Swagger-Codegen/{packageVersion}}/{language}'";

    public static final String SUPPORTS_ES6 = "supportsES6";
    public static final String SUPPORTS_ES6_DESC = "Generate code that conforms to ES6.";
}
