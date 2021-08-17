/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

/**
 * A class for storing constants that are used throughout the project.
 */
public class CodegenConstants {
    /* System Properties */
    // NOTE: We may want to move these to a separate class to avoid confusion or modification.
    public static final String APIS = "apis";
    public static final String MODELS = "models";
    public static final String SUPPORTING_FILES = "supportingFiles";
    public static final String MODEL_TESTS = "modelTests";
    public static final String MODEL_DOCS = "modelDocs";
    public static final String API_TESTS = "apiTests";
    public static final String API_DOCS = "apiDocs";

    public static final String WITH_XML = "withXml";
    public static final String SKIP_FORM_MODEL = "skipFormModel";
    /* /end System Properties */

    public static final String API_PACKAGE = "apiPackage";
    public static final String API_PACKAGE_DESC = "package for generated api classes";

    public static final String API_SUFFIX = "apiSuffix";
    public static final String API_SUFFIX_DESC = "suffix for api classes";

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

    public static final String MODULE_NAME = "moduleName";
    public static final String MODULE_NAME_DESC = "top module name (convention: CamelCase, usually corresponding to gem name).";

    public static final String GEM_NAME = "gemName";
    public static final String GEM_NAME_DESC = "gem name (convention: underscore_case).";

    public static final String PYTHON_PACKAGE_NAME = "pythonPackageName";
    public static final String PYTHON_PACKAGE_NAME_DESC = "package name for generated python code";

    public static final String PYTHON_ATTR_NONE_IF_UNSET = "pythonAttrNoneIfUnset";
    public static final String PYTHON_ATTR_NONE_IF_UNSET_DESC = "when accessing unset attribute, return `None` instead of raising `ApiAttributeError`";

    public static final String WITH_GO_CODEGEN_COMMENT = "withGoCodegenComment";
    public static final String WITH_GO_CODEGEN_COMMENT_DESC = "whether to include Go codegen comment to disable Go Lint and collapse by default in GitHub PRs and diffs";

    public static final String WITH_AWSV4_SIGNATURE_COMMENT = "withAWSV4Signature";
    public static final String WITH_AWSV4_SIGNATURE_COMMENT_DESC = "whether to include AWS v4 signature support";

    public static final String IS_GO_SUBMODULE = "isGoSubmodule";
    public static final String IS_GO_SUBMODULE_DESC = "whether the generated Go module is a submodule";

    public static final String GROUP_ID = "groupId";
    public static final String GROUP_ID_DESC = "groupId in generated pom.xml";

    public static final String ARTIFACT_ID = "artifactId";
    public static final String ARTIFACT_ID_DESC = "artifactId in generated pom.xml. This also becomes part of the generated library's filename";

    public static final String ARTIFACT_VERSION = "artifactVersion";
    public static final String ARTIFACT_VERSION_DESC = "artifact version in generated pom.xml. This also becomes part of the generated library's filename";

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

    public static final String LICENSE_ID = "licenseId";
    public static final String LICENSE_ID_DESC = "The identifier of the license";

    public static final String LICENSE_URL = "licenseUrl";
    public static final String LICENSE_URL_DESC = "The URL of the license";

    public static final String SOURCE_FOLDER = "sourceFolder";
    public static final String SOURCE_FOLDER_DESC = "source folder for generated code";

    public static final String IMPL_FOLDER = "implFolder";
    public static final String IMPL_FOLDER_DESC = "folder for generated implementation code";

    public static final String SERIALIZABLE_MODEL = "serializableModel";
    public static final String SERIALIZABLE_MODEL_DESC = "boolean - toggle \"implements Serializable\" for generated models";

    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING = "bigDecimalAsString";
    public static final String SERIALIZE_BIG_DECIMAL_AS_STRING_DESC = "Treat BigDecimal values as Strings to avoid precision loss.";

    public static final String LIBRARY = "library";
    public static final String LIBRARY_DESC = "library template (sub-template)";

    public static final String SORT_PARAMS_BY_REQUIRED_FLAG = "sortParamsByRequiredFlag";
    public static final String SORT_PARAMS_BY_REQUIRED_FLAG_DESC = "Sort method arguments to place required parameters before optional parameters.";

    public static final String SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG = "sortModelPropertiesByRequiredFlag";
    public static final String SORT_MODEL_PROPERTIES_BY_REQUIRED_FLAG_DESC = "Sort model properties to place required parameters before optional parameters.";

    public static final String PREPEND_FORM_OR_BODY_PARAMETERS = "prependFormOrBodyParameters";
    public static final String PREPEND_FORM_OR_BODY_PARAMETERS_DESC = "Add form or body parameters to the beginning of the parameter list.";

    public static final String USE_DATETIME_OFFSET = "useDateTimeOffset";
    public static final String USE_DATETIME_OFFSET_DESC = "Use DateTimeOffset to model date-time properties";

    public static final String ENSURE_UNIQUE_PARAMS = "ensureUniqueParams";
    public static final String ENSURE_UNIQUE_PARAMS_DESC = "Whether to ensure parameter names are unique in an operation (rename parameters that are not).";

    public static final String PROJECT_NAME = "projectName";
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_NAME_DESC = "package for generated classes (where supported)";

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

    public static final String OPTIONAL_EMIT_DEFAULT_VALUES = "optionalEmitDefaultValues";
    public static final String OPTIONAL_EMIT_DEFAULT_VALUES_DESC = "Set DataMember's EmitDefaultValue.";

    public static final String OPTIONAL_CONDITIONAL_SERIALIZATION = "conditionalSerialization";
    public static final String OPTIONAL_CONDITIONAL_SERIALIZATION_DESC = "Serialize only those properties which are initialized by user, accepted values are true or false, default value is false.";

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

    public static final String PARAM_NAMING = "paramNaming";
    public static final String PARAM_NAMING_DESC = "Naming convention for parameters: 'camelCase', 'PascalCase', 'snake_case' and 'original', which keeps the original name";

    public static final String DOTNET_FRAMEWORK = "targetFramework";
    public static final String DOTNET_FRAMEWORK_DESC = "The target .NET framework version. To target multiple frameworks, use `;` as the separator, e.g. `netstandard2.1;netcoreapp3.0`";

    public static final String NULLABLE_REFERENCE_TYPES = "nullableReferenceTypes";
    public static final String NULLABLE_REFERENCE_TYPES_DESC = "Use nullable annotations in the project. Only supported on C# 8 / ASP.NET Core 3.0 or newer.";

    public static final String TEMPLATING_ENGINE = "templatingEngine";
    public static final String TEMPLATING_ENGINE_DESC = "The templating engine plugin to use: \"mustache\" (default) or \"handlebars\" (beta)";

    public static enum PARAM_NAMING_TYPE {camelCase, PascalCase, snake_case, original}

    public static enum MODEL_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, original}

    public static enum ENUM_PROPERTY_NAMING_TYPE {camelCase, PascalCase, snake_case, original, UPPERCASE}

    public static final String ENUM_PROPERTY_NAMING = "enumPropertyNaming";
    public static final String ENUM_PROPERTY_NAMING_DESC = "Naming convention for enum properties: 'camelCase', 'PascalCase', 'snake_case', 'UPPERCASE', and 'original'";

    // Allow different language generators to offer an option of serialization library. Each language specific
    // Codegen constants should define a description and provide proper input validation for the value of serializationLibrary
    public static final String SERIALIZATION_LIBRARY = "serializationLibrary";

    public static final String API_NAME_PREFIX = "apiNamePrefix";
    public static final String API_NAME_PREFIX_DESC = "Prefix that will be appended to all API names ('tags'). Default: empty string. e.g. Pet => Pet.";

    public static final String API_NAME_SUFFIX = "apiNameSuffix";
    public static final String API_NAME_SUFFIX_DESC = "Suffix that will be appended to all API names ('tags'). Default: Api. e.g. Pet => PetApi. Note: Only ruby, python, jaxrs generators support this feature at the moment.";

    public static final String MODEL_NAME_PREFIX = "modelNamePrefix";
    public static final String MODEL_NAME_PREFIX_DESC = "Prefix that will be prepended to all model names.";

    public static final String MODEL_NAME_SUFFIX = "modelNameSuffix";
    public static final String MODEL_NAME_SUFFIX_DESC = "Suffix that will be appended to all model names.";

    public static final String ENUM_NAME_SUFFIX = "enumNameSuffix";
    public static final String ENUM_NAME_SUFFIX_DESC = "Suffix that will be appended to all enum names.";

    public static final String ENUM_VALUE_SUFFIX = "enumValueSuffix";
    public static final String ENUM_VALUE_SUFFIX_DESC = "Suffix that will be appended to all enum values. Note: For clients this may impact serialization and deserialization of enum values.";

    public static final String GIT_HOST = "gitHost";
    public static final String GIT_HOST_DESC = "Git host, e.g. gitlab.com.";

    public static final String GIT_USER_ID = "gitUserId";
    public static final String GIT_USER_ID_DESC = "Git user ID, e.g. openapitools.";

    public static final String GIT_REPO_ID = "gitRepoId";
    public static final String GIT_REPO_ID_DESC = "Git repo ID, e.g. openapi-generator.";

    public static final String RELEASE_NOTE = "releaseNote";
    public static final String RELEASE_NOTE_DESC = "Release note, default to 'Minor update'.";

    public static final String HTTP_USER_AGENT = "httpUserAgent";
    public static final String HTTP_USER_AGENT_DESC = "HTTP user agent, e.g. codegen_csharp_api_client, default to 'OpenAPI-Generator/{packageVersion}/{language}'";

    public static final String SUPPORTS_ES6 = "supportsES6";
    public static final String SUPPORTS_ES6_DESC = "Generate code that conforms to ES6.";

    public static final String SUPPORTS_ASYNC = "supportsAsync";
    public static final String SUPPORTS_ASYNC_DESC = "Generate code that supports async operations.";

    public static final String EXCLUDE_TESTS = "excludeTests";
    public static final String EXCLUDE_TESTS_DESC = "Specifies that no tests are to be generated.";

    public static final String SOURCECODEONLY_GENERATION = "generateSourceCodeOnly";
    public static final String SOURCECODEONLY_GENERATION_DESC = "Specifies that only a library source code is to be generated.";

    public static final String PARCELIZE_MODELS = "parcelizeModels";
    public static final String PARCELIZE_MODELS_DESC = "toggle \"@Parcelize\" for generated models";

    public static final String CASE_INSENSITIVE_RESPONSE_HEADERS = "caseInsensitiveResponseHeaders";
    public static final String CASE_INSENSITIVE_RESPONSE_HEADERS_DESC = "Make API response's headers case-insensitive";

    // Not user-configurable. System provided for use in templates.

    public static final String GENERATE_APIS = "generateApis";
    public static final String GENERATE_API_DOCS = "generateApiDocs";

    public static final String GENERATE_API_TESTS = "generateApiTests";
    public static final String GENERATE_API_TESTS_DESC = "Specifies that api tests are to be generated.";

    // Not user-configurable. System provided for use in templates.
    public static final String GENERATE_MODELS = "generateModels";
    public static final String GENERATE_MODEL_DOCS = "generateModelDocs";

    public static final String VIRTUAL_SERVICE = "virtualService";
    public static final String VIRTUAL_SERVICE_DESC = "Generate Spring boot rest service as virtual service with Virtualan";

    public static final String GENERATE_MODEL_TESTS = "generateModelTests";
    public static final String GENERATE_MODEL_TESTS_DESC = "Specifies that model tests are to be generated.";

    public static final String HIDE_GENERATION_TIMESTAMP = "hideGenerationTimestamp";
    public static final String HIDE_GENERATION_TIMESTAMP_DESC = "Hides the generation timestamp when files are generated.";

    public static final String GENERATE_PROPERTY_CHANGED = "generatePropertyChanged";
    public static final String GENERATE_PROPERTY_CHANGED_DESC = "Specifies that models support raising property changed events.";

    public static final String NON_PUBLIC_API = "nonPublicApi";
    public static final String NON_PUBLIC_API_DESC = "Generates code with reduced access modifiers; allows embedding elsewhere without exposing non-public API calls to consumers.";

    public static final String VALIDATABLE = "validatable";
    public static final String VALIDATABLE_DESC = "Generates self-validatable models.";

    public static final String IGNORE_FILE_OVERRIDE = "ignoreFileOverride";
    public static final String IGNORE_FILE_OVERRIDE_DESC = "Specifies an override location for the .openapi-generator-ignore file. Most useful on initial generation.";

    public static final String REMOVE_OPERATION_ID_PREFIX = "removeOperationIdPrefix";
    public static final String REMOVE_OPERATION_ID_PREFIX_DESC = "Remove prefix of operationId, e.g. config_getId => getId";

    public static final String REMOVE_OPERATION_ID_PREFIX_DELIMITER = "removeOperationIdPrefixDelimiter";
    public static final String REMOVE_OPERATION_ID_PREFIX_DELIMITER_DESC = "Character to use as a delimiter for the prefix. Default: '_'";

    public static final String REMOVE_OPERATION_ID_PREFIX_COUNT = "removeOperationIdPrefixCount";
    public static final String REMOVE_OPERATION_ID_PREFIX_COUNT_DESC = "Count of delimiter for the prefix. Use -1 for last Default: 1";

    public static final String SKIP_OPERATION_EXAMPLE = "skipOperationExample";
    public static final String SKIP_OPERATION_EXAMPLE_DESC = "Skip examples defined in operations to avoid out of memory errors.";

    public static final String STRIP_PACKAGE_NAME = "stripPackageName";
    public static final String STRIP_PACKAGE_NAME_DESC = "Whether to strip leading dot-separated packages from generated model classes";

    public static final String DOCEXTENSION = "docExtension";
    public static final String DOCEXTENSION_DESC = "The extension of the generated documentation files, defaults to markdown, .md";

    public static final String DATABASE_ADAPTER = "databaseAdapter";
    public static final String DATABASE_ADAPTER_DESC = "The adapter for database (e.g. mysql, sqlite). Default: sqlite";

    public static final String PARENT_GROUP_ID = "parentGroupId";
    public static final String PARENT_GROUP_ID_DESC = "parent groupId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect";

    public static final String PARENT_ARTIFACT_ID = "parentArtifactId";
    public static final String PARENT_ARTIFACT_ID_DESC = "parent artifactId in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect";

    public static final String PARENT_VERSION = "parentVersion";
    public static final String PARENT_VERSION_DESC = "parent version in generated pom N.B. parentGroupId, parentArtifactId and parentVersion must all be specified for any of them to take effect";

    public static final String ENABLE_POST_PROCESS_FILE = "enablePostProcessFile";
    public static final String ENABLE_POST_PROCESS_FILE_DESC = "Enable post-processing file using environment variables.";

    public static final String OPEN_API_SPEC_NAME = "openAPISpecName";

    public static final String GENERATE_ALIAS_AS_MODEL = "generateAliasAsModel";
    public static final String GENERATE_ALIAS_AS_MODEL_DESC = "Generate model implementation for aliases to map and array schemas. " +
        "An 'alias' is an array, map, or list which is defined inline in a OpenAPI document and becomes a model in the generated code. " +
        "A 'map' schema is an object that can have undeclared properties, i.e. the 'additionalproperties' attribute is set on that object. " +
        "An 'array' schema is a list of sub schemas in a OAS document";

    public static final String USE_COMPARE_NET_OBJECTS = "useCompareNetObjects";
    public static final String USE_COMPARE_NET_OBJECTS_DESC = "Use KellermanSoftware.CompareNetObjects for deep recursive object comparison. WARNING: this option incurs potential performance impact.";

    public static final String SNAPSHOT_VERSION = "snapshotVersion";
    public static final String SNAPSHOT_VERSION_DESC = "Uses a SNAPSHOT version.";

    public static final String EXCEPTION_ON_FAILURE = "returnExceptionOnFailure";
    public static final String EXCEPTION_ON_FAILURE_DESC = "Throw an exception on non success response codes";

    public static final String ENUM_CLASS_PREFIX = "enumClassPrefix";
    public static final String ENUM_CLASS_PREFIX_DESC = "Prefix enum with class name";

    public static final String PACKAGE_TAGS = "packageTags";
    public static final String PACKAGE_TAGS_DESC = "Tags to identify the package";

    public static final String REMOVE_ENUM_VALUE_PREFIX = "removeEnumValuePrefix";
    public static final String REMOVE_ENUM_VALUE_PREFIX_DESC = "Remove the common prefix of enum values";

    public static final String LEGACY_DISCRIMINATOR_BEHAVIOR = "legacyDiscriminatorBehavior";
    public static final String LEGACY_DISCRIMINATOR_BEHAVIOR_DESC = "Set to false for generators with better support for discriminators. (Python, Java, Go, PowerShell, C#have this enabled by default).";

    public static final String USE_SINGLE_REQUEST_PARAMETER = "useSingleRequestParameter";
    public static final String USE_SINGLE_REQUEST_PARAMETER_DESC = "Setting this property to true will generate functions with a single argument containing all API endpoint parameters instead of one argument per parameter.";

    public static final String DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT = "disallowAdditionalPropertiesIfNotPresent";
    public static final String DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC =
        "If false, the 'additionalProperties' implementation (set to true by default) is compliant with the OAS and JSON schema specifications. " +
        "If true (default), keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.";
    public static final String USE_ONEOF_DISCRIMINATOR_LOOKUP = "useOneOfDiscriminatorLookup";
    public static final String USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC = "Use the discriminator's mapping in oneOf to speed up the model lookup. IMPORTANT: Validation (e.g. one and only one match in oneOf's schemas) will be skipped.";
}
