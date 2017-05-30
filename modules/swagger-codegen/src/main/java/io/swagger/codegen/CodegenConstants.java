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

    public static final String ALLOW_UNICODE_IDENTIFIERS = "allowUnicodeIdentifiers";
    public static final String ALLOW_UNICODE_IDENTIFIERS_DESC = "boolean, toggles whether unicode identifiers are allowed in names or not, default is false";

    public static final String INVOKER_PACKAGE = "invokerPackage";
    public static final String INVOKER_PACKAGE_DESC = "root package for generated code";

    public static final String PHP_INVOKER_PACKAGE = "phpInvokerPackage";
    public static final String PHP_INVOKER_PACKAGE_DESC = "root package for generated php code";

    public static final String PERL_MODULE_NAME = "perlModuleName";
    public static final String PERL_MODULE_NAME_DESC = "root module name for generated perl code";

    public static final String PYTHON_PACKAGE_NAME = "pythonPackageName";
    public static final String PYTHON_PACKAGE_NAME_DESC = "package name for generated python code";

    public static final String GROUP_ID = "groupId";
    public static final String GROUP_ID_DESC = "groupId in generated pom.xml";

    public static final String ARTIFACT_ID = "artifactId";
    public static final String ARTIFACT_ID_DESC = "artifactId in generated pom.xml";

    public static final String ARTIFACT_VERSION = "artifactVersion";
    public static final String ARTIFACT_VERSION_DESC = "artifact version in generated pom.xml";

    public static final String ARTIFACT_URL = "artifactUrl";
    public static final String ARTIFACT_URL_DESC = "artifact URL in generated pom.xml";

    public static final String ARTIFACT_DESCRIPTION = "artifactDescription";
    public static final String ARTIFACT_DESCRIPTION_DESC = "artifact description in generated pom.xml";

    public static final String SCM_CONNECTION = "scmConnection";
    public static final String SCM_CONNECTION_DESC = "SCM connection in generated pom.xml";

    public static final String SCM_DEVELOPER_CONNECTION = "scmDeveloperConnection";
    public static final String SCM_DEVELOPER_CONNECTION_DESC = "SCM developer connection in generated pom.xml";

    public static final String SCM_URL = "scmUrl";
    public static final String SCM_URL_DESC = "SCM URL in generated pom.xml";

    public static final String DEVELOPER_NAME = "developerName";
    public static final String DEVELOPER_NAME_DESC = "developer name in generated pom.xml";

    public static final String DEVELOPER_EMAIL = "developerEmail";
    public static final String DEVELOPER_EMAIL_DESC = "developer email in generated pom.xml";

    public static final String DEVELOPER_ORGANIZATION = "developerOrganization";
    public static final String DEVELOPER_ORGANIZATION_DESC = "developer organization in generated pom.xml";

    public static final String DEVELOPER_ORGANIZATION_URL = "developerOrganizationUrl";
    public static final String DEVELOPER_ORGANIZATION_URL_DESC = "developer organization URL in generated pom.xml";

    public static final String LICENSE_NAME = "licenseName";
    public static final String LICENSE_NAME_DESC = "The name of the license";

    public static final String LICENSE_URL = "licenseUrl";
    public static final String LICENSE_URL_DESC = "The URL of the license";

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

    public static final String PACKAGE_TITLE = "packageTitle";
    public static final String PACKAGE_TITLE_DESC = "Specifies an AssemblyTitle for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_PRODUCTNAME = "packageProductName";
    public static final String PACKAGE_PRODUCTNAME_DESC = "Specifies an AssemblyProduct for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_DESCRIPTION = "packageDescription";
    public static final String PACKAGE_DESCRIPTION_DESC = "Specifies a AssemblyDescription for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_COMPANY = "packageCompany";
    public static final String PACKAGE_COMPANY_DESC = "Specifies an AssemblyCompany for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";
    public static final String PACKAGE_AUTHORS = "packageAuthors";
    public static final String PACKAGE_AUTHORS_DESC = "Specifies Authors property in the .NET Core project file.";
    public static final String PACKAGE_COPYRIGHT = "packageCopyright";
    public static final String PACKAGE_COPYRIGHT_DESC = "Specifies an AssemblyCopyright for the .NET Framework global assembly attributes stored in the AssemblyInfo file.";

    public static final String POD_VERSION = "podVersion";

    public static final String OPTIONAL_METHOD_ARGUMENT = "optionalMethodArgument";
    public static final String OPTIONAL_METHOD_ARGUMENT_DESC = "Optional method argument, e.g. void square(int x=10) (.net 4.0+ only).";

    public static final String OPTIONAL_ASSEMBLY_INFO = "optionalAssemblyInfo";
    public static final String OPTIONAL_ASSEMBLY_INFO_DESC = "Generate AssemblyInfo.cs.";

    public static final String NETCORE_PROJECT_FILE = "netCoreProjectFile";
    public static final String NETCORE_PROJECT_FILE_DESC = "Use the new format (.NET Core) for .NET project files (.csproj).";

    public static final String USE_COLLECTION = "useCollection";
    public static final String USE_COLLECTION_DESC = "Deserialize array types to Collection<T> instead of List<T>.";

    public static final String INTERFACE_PREFIX = "interfacePrefix";
    public static final String INTERFACE_PREFIX_DESC = "Prefix interfaces with a community standard or widely accepted prefix.";

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

    public static final String EXCLUDE_TESTS = "excludeTests";
    public static final String EXCLUDE_TESTS_DESC = "Specifies that no tests are to be generated.";

    // Not user-configurable. System provided for use in templates.
    public static final String GENERATE_API_DOCS = "generateApiDocs";

    public static final String GENERATE_API_TESTS = "generateApiTests";
    public static final String GENERATE_API_TESTS_DESC = "Specifies that api tests are to be generated.";

    // Not user-configurable. System provided for use in templates.
    public static final String GENERATE_MODEL_DOCS = "generateModelDocs";

    public static final String GENERATE_MODEL_TESTS = "generateModelTests";
    public static final String GENERATE_MODEL_TESTS_DESC = "Specifies that model tests are to be generated.";

    public static final String HIDE_GENERATION_TIMESTAMP = "hideGenerationTimestamp";
    public static final String HIDE_GENERATION_TIMESTAMP_DESC = "Hides the generation timestamp when files are generated.";

    public static final String GENERATE_PROPERTY_CHANGED = "generatePropertyChanged";
    public static final String GENERATE_PROPERTY_CHANGED_DESC = "Specifies that models support raising property changed events.";

    public static final String NON_PUBLIC_API = "nonPublicApi";
    public static final String NON_PUBLIC_API_DESC = "Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.";

    public static final String IGNORE_FILE_OVERRIDE = "ignoreFileOverride";
    public static final String IGNORE_FILE_OVERRIDE_DESC = "Specifies an override location for the .swagger-codegen-ignore file. Most useful on initial generation.";

    public static final String REMOVE_OPERATION_ID_PREFIX = "removeOperationIdPrefix";
    public static final String REMOVE_OPERATION_ID_PREFIX_DESC = "Remove prefix of operationId, e.g. config_getId => getId";
}
