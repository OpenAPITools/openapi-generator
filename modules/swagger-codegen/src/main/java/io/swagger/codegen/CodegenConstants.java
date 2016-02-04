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

    public static enum MODEL_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, original}

}
