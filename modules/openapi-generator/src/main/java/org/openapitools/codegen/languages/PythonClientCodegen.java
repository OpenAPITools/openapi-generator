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

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import lombok.Setter;
import org.apache.commons.lang3.Strings;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

/**
 * <p>Mustache templates are located in {@code src/main/resources/python/}.
 */
public class PythonClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    public static final String DEFAULT_LIBRARY = "urllib3";
    public static final String RECURSION_LIMIT = "recursionLimit";
    public static final String DATETIME_FORMAT = "datetimeFormat";
    public static final String DATE_FORMAT = "dateFormat";
    public static final String SET_ENSURE_ASCII_TO_FALSE = "setEnsureAsciiToFalse";
    public static final String POETRY1_FALLBACK = "poetry1";
    public static final String LAZY_IMPORTS = "lazyImports";
    public static final String BUILD_SYSTEM = "buildSystem";
    public static final String SUPPORT_HTTPX_SYNC = "supportHttpxSync";
    public static final String USE_INDEPENDENT_IMPLICIT_CLIENTS = "useIndependentImplicitClients";
    private static final Set<String> SYNC_API_LIFECYCLE_METHODS =
            Set.of("close", "__enter__", "__exit__");
    private static final Set<String> ASYNC_API_LIFECYCLE_METHODS =
            Set.of("close", "__aenter__", "__aexit__");
    private static final Set<String> HTTPX_SYNC_API_LIFECYCLE_METHODS =
            Set.of("close_sync", "__enter__", "__exit__");
    private static final Set<String> INDEPENDENT_API_MEMBER_NAMES =
            Set.of("_owned_api_client");
    public static final String COMPATIBLE_WITH_PYTHON_LEGACY = "compatibleWithPythonLegacy";
    private static final String COMPATIBLE_WITH_PYTHON_LEGACY_API = "compatibleWithPythonLegacyApi";
    // Snapshot of BaseModel's public API in the minimum supported Pydantic 2.11.
    // https://github.com/pydantic/pydantic/blob/v2.11.0/pydantic/main.py
    private static final Set<String> PYDANTIC_BASE_MODEL_MEMBER_NAMES = Set.of(
            "construct", "copy", "dict", "from_orm", "json",
            "model_computed_fields", "model_config", "model_construct", "model_copy", "model_dump",
            "model_dump_json", "model_extra", "model_fields", "model_fields_set", "model_json_schema",
            "model_parametrized_name", "model_post_init", "model_rebuild", "model_validate",
            "model_validate_json", "model_validate_strings", "parse_file", "parse_obj", "parse_raw",
            "schema", "schema_json", "update_forward_refs", "validate");
    private static final Set<String> GENERATED_MODEL_MEMBER_NAMES = Set.of(
            "from_dict", "from_json", "to_dict", "to_json", "to_str");
    private static final Set<String> LEGACY_MODEL_METADATA_MEMBER_NAMES =
            Set.of("attribute_map", "openapi_types");
    private static final Set<String> MODEL_FIELD_NAME_COLLISIONS;
    private static final Set<String> MODEL_PUBLIC_MEMBER_NAMES;
    private static final Set<String> MODEL_CLASS_BODY_NAMES = Set.of(
            "ConfigDict", "classmethod", "datetime", "field_validator", "property");
    private static final Set<String> PYDANTIC_PRIVATE_MEMBER_NAMES = Set.of(
            "_abc_impl", "_calculate_keys", "_copy_and_set_values", "_get_value", "_iter",
            "_setattr_handler");
    static {
        Set<String> fieldNames = new HashSet<>(PYDANTIC_BASE_MODEL_MEMBER_NAMES);
        fieldNames.addAll(GENERATED_MODEL_MEMBER_NAMES);
        MODEL_FIELD_NAME_COLLISIONS = Set.copyOf(fieldNames);

        Set<String> publicNames = new HashSet<>(PYDANTIC_BASE_MODEL_MEMBER_NAMES);
        publicNames.addAll(GENERATED_MODEL_MEMBER_NAMES);
        // BaseModel.schema() is deprecated, and nameMappings are used to
        // preserve schema fields. The generated property intentionally replaces it.
        publicNames.remove("schema");
        // Forwarding property type stubs use the built-in decorator.
        publicNames.add("property");
        MODEL_PUBLIC_MEMBER_NAMES = Set.copyOf(publicNames);
    }

    @Setter protected String packageUrl;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    @Setter protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup
    @Setter protected String datetimeFormat = "%Y-%m-%dT%H:%M:%S.%f%z";
    @Setter protected String dateFormat = "%Y-%m-%d";
    @Setter protected boolean setEnsureAsciiToFalse = false;
    @Setter protected boolean useIndependentImplicitClients = false;
    @Setter protected boolean compatibleWithPythonLegacy = false;

    private String testFolder;

    public PythonClientCodegen() {
        super();

        // force sortParamsByRequiredFlag to true to make the api method signature less complicated
        sortParamsByRequiredFlag = true;

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .includeSecurityFeatures(SecurityFeature.SignatureAuth)
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        // fields with these names would clash with the helper methods generated on every
        // model (a field named from_dict shadows the classmethod) or with pydantic's
        // model_* namespace (a field named model_config is clobbered by the ConfigDict
        // assignment and silently dropped), so mangle them like any other reserved word
        reservedWords.addAll(GENERATED_MODEL_MEMBER_NAMES);
        for (String memberName : PYDANTIC_BASE_MODEL_MEMBER_NAMES) {
            if (memberName.startsWith("model_")) {
                reservedWords.add(memberName);
            }
        }

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        // override type mapping in abstract python codegen
        typeMapping.put("array", "List");
        typeMapping.put("set", "List");
        typeMapping.put("map", "Dict");
        typeMapping.put("decimal", "decimal.Decimal");
        typeMapping.put("file", "bytes");
        typeMapping.put("binary", "bytes");
        typeMapping.put("ByteArray", "bytes");

        languageSpecificPrimitives.remove("file");
        languageSpecificPrimitives.add("decimal.Decimal");
        languageSpecificPrimitives.add("bytes");
        languageSpecificPrimitives.add("none_type");

        supportsInheritance = true;
        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separatorChar + "python";

        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");

        modelTestTemplateFiles.put("model_test.mustache", ".py");
        apiTestTemplateFiles.put("api_test.mustache", ".py");

        embeddedTemplateDir = templateDir = "python";

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        testFolder = "test";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue("openapi_client"));
        cliOptions.add(new CliOption(CodegenConstants.PROJECT_NAME, "python project name in setup.py (e.g. petstore-api)."));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(PACKAGE_URL, "python package URL."));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCECODEONLY_GENERATION, CodegenConstants.SOURCECODEONLY_GENERATION_DESC)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(SET_ENSURE_ASCII_TO_FALSE, "When set to true, add `ensure_ascii=False` in json.dumps when creating the HTTP request body.")
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(RECURSION_LIMIT, "Set the recursion limit. If not set, use the system default value."));
        cliOptions.add(new CliOption(MAP_NUMBER_TO, "Map number to Union[StrictFloat, StrictInt], StrictFloat, float or Decimal.")
                .defaultValue("Union[StrictFloat, StrictInt]"));
        cliOptions.add(new CliOption(DATETIME_FORMAT, "datetime format for query parameters")
                .defaultValue("%Y-%m-%dT%H:%M:%S%z"));
        cliOptions.add(new CliOption(DATE_FORMAT, "date format for query parameters")
                .defaultValue("%Y-%m-%d"));
        cliOptions.add(new CliOption(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(POETRY1_FALLBACK, "Fallback to formatting pyproject.toml to Poetry 1.x format."));
        cliOptions.add(new CliOption(LAZY_IMPORTS, "Enable lazy imports.").defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(BUILD_SYSTEM, "Build system to use in pyproject.toml (setuptools, hatchling).").defaultValue("setuptools"));
        cliOptions.add(CliOption.newBoolean(SUPPORT_HTTPX_SYNC, "Generate synchronous '_sync' variants of each API method (httpx library only). " +
                "Each '_sync' method simply calls the corresponding async method and waits for its completion, " +
                "so both synchronous and asynchronous methods are available from the same API class.").defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(USE_INDEPENDENT_IMPLICIT_CLIENTS,
                "Give API instances without an explicit or registered default ApiClient " +
                "an owned client with a copied Configuration.")
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(COMPATIBLE_WITH_PYTHON_LEGACY,
                "Enable compatibility with python-legacy. Currently, generated model field aliases preserve normalized Python constructor names while " +
                        "accepting wire names, and to_dict() emits every declared field, using None for missing attributes, " +
                        "under public names by default and wire names with serialize=True. Generic models expose " +
                        "openapi_types and attribute_map, reject unknown constructor keys, and use legacy display and equality helpers. " +
                        "Container conversion is limited to immediate list elements and dictionary values, matching python-legacy. " +
                        "Synchronous urllib3 operations keep async_req, _preload_content, tuple " +
                        "with_http_info() behavior, and integer _request_timeout inputs. JSON and request serialization remain unchanged.")
                .defaultValue(Boolean.FALSE.toString()));

        supportedLibraries.put("urllib3", "urllib3-based client");
        supportedLibraries.put("asyncio", "asyncio-based client");
        supportedLibraries.put("tornado", "tornado-based client (deprecated)");
        supportedLibraries.put("httpx", "httpx-based client");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use: asyncio, tornado (deprecated), urllib3, httpx");
        libraryOption.setDefault(DEFAULT_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(DEFAULT_LIBRARY);

        // option to change how we process + set the data in the 'additionalProperties' keyword.
        CliOption disallowAdditionalPropertiesIfNotPresentOpt = CliOption.newBoolean(
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT,
                CodegenConstants.DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_DESC).defaultValue(Boolean.TRUE.toString());
        Map<String, String> disallowAdditionalPropertiesIfNotPresentOpts = new HashMap<>();
        disallowAdditionalPropertiesIfNotPresentOpts.put("false",
                "The 'additionalProperties' implementation is compliant with the OAS and JSON schema specifications.");
        disallowAdditionalPropertiesIfNotPresentOpts.put("true",
                "Keep the old (incorrect) behaviour that 'additionalProperties' is set to false by default.");
        disallowAdditionalPropertiesIfNotPresentOpt.setEnum(disallowAdditionalPropertiesIfNotPresentOpts);
        cliOptions.add(disallowAdditionalPropertiesIfNotPresentOpt);
        this.setDisallowAdditionalPropertiesIfNotPresent(true);
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);

        super.processOpts();

        // map to Dot instead of Period
        specialCharReplacements.put(".", "Dot");

        Boolean excludeTests = false;

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.PROJECT_NAME)) {
            setProjectName((String) additionalProperties.get(CodegenConstants.PROJECT_NAME));
        } else {
            // default: set project based on package name
            // e.g. petstore_api (package name) => petstore-api (project name)
            setProjectName(packageName.replaceAll("_", "-"));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        if (additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = Boolean.valueOf(additionalProperties.get(CodegenConstants.EXCLUDE_TESTS).toString());
        }

        Boolean generateSourceCodeOnly = false;
        if (additionalProperties.containsKey(CodegenConstants.SOURCECODEONLY_GENERATION)) {
            generateSourceCodeOnly = Boolean.valueOf(additionalProperties.get(CodegenConstants.SOURCECODEONLY_GENERATION).toString());
        }

        if (generateSourceCodeOnly) {
            // tests in <package>/test
            testFolder = packagePath() + File.separatorChar + testFolder;
            // api/model docs in <package>/docs
            apiDocPath = packagePath() + "/" + apiDocPath;
            modelDocPath = packagePath() + "/" + modelDocPath;
        }
        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (additionalProperties.containsKey(SET_ENSURE_ASCII_TO_FALSE)) {
            additionalProperties.put(SET_ENSURE_ASCII_TO_FALSE, Boolean.valueOf(additionalProperties.get(SET_ENSURE_ASCII_TO_FALSE).toString()));
        }

        if (additionalProperties.containsKey(PACKAGE_URL)) {
            setPackageUrl((String) additionalProperties.get(PACKAGE_URL));
        }

        // check to see if setRecursionLimit is set and whether it's an integer
        if (additionalProperties.containsKey(RECURSION_LIMIT)) {
            try {
                Integer.parseInt((String) additionalProperties.get(RECURSION_LIMIT));
            } catch (NumberFormatException | NullPointerException e) {
                throw new IllegalArgumentException("recursionLimit must be an integer, e.g. 2000.");
            }
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP)) {
            setUseOneOfDiscriminatorLookup(convertPropertyToBooleanAndWriteBack(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP));
        } else {
            additionalProperties.put(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, useOneOfDiscriminatorLookup);
        }

        if (additionalProperties.containsKey(MAP_NUMBER_TO)) {
            setMapNumberTo(String.valueOf(additionalProperties.get(MAP_NUMBER_TO)));
        }

        if (additionalProperties.containsKey(DATETIME_FORMAT)) {
            setDatetimeFormat((String) additionalProperties.get(DATETIME_FORMAT));
        } else {
            additionalProperties.put(DATETIME_FORMAT, datetimeFormat);
        }

        if (additionalProperties.containsKey(DATE_FORMAT)) {
            setDateFormat((String) additionalProperties.get(DATE_FORMAT));
        } else {
            additionalProperties.put(DATE_FORMAT, dateFormat);
        }

        if (additionalProperties.containsKey(LAZY_IMPORTS)) {
            additionalProperties.put(LAZY_IMPORTS, Boolean.valueOf(additionalProperties.get(LAZY_IMPORTS).toString()));
        }

        if (additionalProperties.containsKey(COMPATIBLE_WITH_PYTHON_LEGACY)) {
            setCompatibleWithPythonLegacy(convertPropertyToBooleanAndWriteBack(COMPATIBLE_WITH_PYTHON_LEGACY));
        } else {
            additionalProperties.put(COMPATIBLE_WITH_PYTHON_LEGACY, compatibleWithPythonLegacy);
        }
        // Model compatibility applies to every library, but operation compatibility
        // matches the removed synchronous urllib3 python-legacy generator.
        if (usesLegacyApiCompatibility()) {
            additionalProperties.put(COMPATIBLE_WITH_PYTHON_LEGACY_API, true);
        } else {
            additionalProperties.remove(COMPATIBLE_WITH_PYTHON_LEGACY_API);
        }

        if (additionalProperties.containsKey(BUILD_SYSTEM)) {
            String buildSystem = (String) additionalProperties.get(BUILD_SYSTEM);
            if ("hatchling".equals(buildSystem)) {
                additionalProperties.put("hatchling", true);
            }
        }

        if (additionalProperties.containsKey(USE_INDEPENDENT_IMPLICIT_CLIENTS)) {
            setUseIndependentImplicitClients(
                    convertPropertyToBooleanAndWriteBack(USE_INDEPENDENT_IMPLICIT_CLIENTS));
        } else {
            additionalProperties.put(
                    USE_INDEPENDENT_IMPLICIT_CLIENTS, useIndependentImplicitClients);
        }

        String modelPath = packagePath() + File.separatorChar + modelPackage.replace('.', File.separatorChar);
        String apiPath = packagePath() + File.separatorChar + apiPackage.replace('.', File.separatorChar);

        String readmePath = "README.md";
        String readmeTemplate = "README.mustache";
        if (generateSourceCodeOnly) {
            readmePath = packagePath() + "_" + readmePath;
            readmeTemplate = "README_onlypackage.mustache";
        }
        supportingFiles.add(new SupportingFile(readmeTemplate, "", readmePath));

        if (!generateSourceCodeOnly) {
            supportingFiles.add(new SupportingFile("tox.mustache", "", "tox.ini"));
            supportingFiles.add(new SupportingFile("test-requirements.mustache", "", "test-requirements.txt"));
            supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));
            supportingFiles.add(new SupportingFile("setup_cfg.mustache", "", "setup.cfg"));

            supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
            supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
            supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
            supportingFiles.add(new SupportingFile("github-workflow.mustache", ".github/workflows", "python.yml"));
            supportingFiles.add(new SupportingFile("gitlab-ci.mustache", "", ".gitlab-ci.yml"));
            supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
            supportingFiles.add(new SupportingFile("pyproject.mustache", "", "pyproject.toml"));
            supportingFiles.add(new SupportingFile("py.typed.mustache", packagePath(), "py.typed"));
        }
        supportingFiles.add(new SupportingFile("configuration.mustache", packagePath(), "configuration.py"));
        supportingFiles.add(new SupportingFile("__init__package.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", modelPath, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__api.mustache", apiPath, "__init__.py"));
        // Generate the 'signing.py' module, but only if the 'HTTP signature' security scheme is specified in the OAS.
        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
                (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
            supportingFiles.add(new SupportingFile("signing.mustache", packagePath(), "signing.py"));
        }

        // If the package name consists of dots(openapi.client), then we need to create the directory structure like openapi/client with __init__ files.
        String[] packageNameSplits = packageName.split("\\.");
        String currentPackagePath = "";
        for (int i = 0; i < packageNameSplits.length - 1; i++) {
            if (i > 0) {
                currentPackagePath = currentPackagePath + File.separatorChar;
            }
            currentPackagePath = currentPackagePath + packageNameSplits[i];
            supportingFiles.add(new SupportingFile("__init__.mustache", currentPackagePath, "__init__.py"));
        }

        supportingFiles.add(new SupportingFile("exceptions.mustache", packagePath(), "exceptions.py"));

        if (Boolean.FALSE.equals(excludeTests)) {
            supportingFiles.add(new SupportingFile("__init__.mustache", testFolder, "__init__.py"));
        }

        supportingFiles.add(new SupportingFile("api_client.mustache", packagePath(), "api_client.py"));
        supportingFiles.add(new SupportingFile("api_response.mustache", packagePath(), "api_response.py"));

        if ("asyncio".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("asyncio/rest.mustache", packagePath(), "rest.py"));
            additionalProperties.put("async", "true");
            additionalProperties.put("asyncio", "true");
        } else if ("tornado".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("tornado/rest.mustache", packagePath(), "rest.py"));
            additionalProperties.put("tornado", "true");
        } else if ("httpx".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("httpx/rest.mustache", packagePath(), "rest.py"));
            additionalProperties.put("async", "true");
            additionalProperties.put("httpx", "true");
            if (Boolean.parseBoolean(String.valueOf(additionalProperties.get(SUPPORT_HTTPX_SYNC)))) {
                // generate synchronous '_sync' method variants alongside the async ones
                additionalProperties.put(SUPPORT_HTTPX_SYNC, true);
                supportingFiles.add(new SupportingFile("httpx/sync_helper.mustache", packagePath(), "sync_helper.py"));
            } else {
                additionalProperties.remove(SUPPORT_HTTPX_SYNC);
            }
        } else {
            supportingFiles.add(new SupportingFile("rest.mustache", packagePath(), "rest.py"));
        }

        // 'supportHttpxSync' only makes sense for the (async) httpx library
        if (!"httpx".equals(getLibrary())) {
            if (Boolean.parseBoolean(String.valueOf(additionalProperties.get(SUPPORT_HTTPX_SYNC)))) {
                LOGGER.warn("'{}' is only supported with the 'httpx' library and will be ignored.", SUPPORT_HTTPX_SYNC);
            }
            additionalProperties.remove(SUPPORT_HTTPX_SYNC);
        }

        modelPackage = this.packageName + "." + modelPackage;
        apiPackage = this.packageName + "." + apiPackage;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    @Override
    public String toModelImport(String name) {
        String modelImport;
        if (Strings.CS.startsWithAny(name, "import", "from")) {
            modelImport = name;
        } else {
            modelImport = "from ";
            if (!"".equals(modelPackage())) {
                modelImport += modelPackage() + ".";
            }
            modelImport += toModelFilename(name) + " import " + name;
        }
        return modelImport;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "python";
    }

    @Override
    public String getHelp() {
        return "Generates a Python client library.";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + File.separator + apiDocPath);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + File.separator + modelDocPath);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separatorChar + testFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separatorChar + testFolder;
    }

    public String packagePath() {
        return packageName.replace('.', File.separatorChar);
    }

    /**
     * Generate Python package name from String `packageName`
     * <p>
     * (PEP 0008) Python packages should also have short, all-lowercase names,
     * although the use of underscores is discouraged.
     *
     * @param packageName Package name
     * @return Python package name that conforms to PEP 0008
     */
    @SuppressWarnings("static-method")
    public String generatePackageName(String packageName) {
        return underscore(packageName.replaceAll("[^\\w]+", ""));
    }

    @Override
    public String generatorLanguageVersion() {
        return "3.10+";
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        final Schema additionalProperties = ModelUtils.getAdditionalProperties(schema);

        if (additionalProperties != null) {
            codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(
            OperationsMap objs, List<ModelMap> allModels) {
        if (useIndependentImplicitClients) {
            renameIndependentClientOperationMembers(
                    objs.getOperations().getOperation());
        }
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    private void renameIndependentClientOperationMembers(
            List<CodegenOperation> operations) {
        Set<String> apiMembers = independentClientApiMembers();
        Set<String> occupiedMembers = new HashSet<>(apiMembers);
        for (CodegenOperation operation : operations) {
            if (!apiMembers.contains(operation.operationId)) {
                occupiedMembers.addAll(generatedOperationMembers(operation.operationId));
            }
        }

        for (CodegenOperation operation : operations) {
            String originalName = operation.operationId;
            if (!apiMembers.contains(originalName)) {
                continue;
            }

            String candidate = "call_" + originalName;
            int suffix = 2;
            while (generatedOperationMembers(candidate).stream()
                    .anyMatch(occupiedMembers::contains)) {
                candidate = "call_" + originalName + "_" + suffix;
                suffix++;
            }

            LOGGER.warn("{} conflicts with a generated API member. Renamed to {}",
                    originalName, candidate);
            operation.operationId = candidate;
            operation.operationIdLowerCase = candidate.toLowerCase(Locale.ROOT);
            operation.operationIdCamelCase = camelize(candidate);
            operation.operationIdSnakeCase = underscore(candidate);
            occupiedMembers.addAll(generatedOperationMembers(candidate));
        }
    }

    private Set<String> independentClientApiMembers() {
        Set<String> members = new HashSet<>(INDEPENDENT_API_MEMBER_NAMES);
        boolean async = "asyncio".equals(getLibrary()) || "httpx".equals(getLibrary());
        members.addAll(async
                ? ASYNC_API_LIFECYCLE_METHODS
                : SYNC_API_LIFECYCLE_METHODS);
        if (supportsHttpxSync()) {
            members.addAll(HTTPX_SYNC_API_LIFECYCLE_METHODS);
        }
        return members;
    }

    private Set<String> generatedOperationMembers(String operationId) {
        Set<String> members = new HashSet<>(Set.of(
                operationId,
                operationId + "_with_http_info",
                "_" + operationId + "_serialize"));
        if (!usesLegacyApiCompatibility()) {
            members.add(operationId + "_without_preload_content");
        }
        if (supportsHttpxSync()) {
            members.add(operationId + "_sync");
            members.add(operationId + "_sync_with_http_info");
            members.add(operationId + "_sync_without_preload_content");
        }
        return members;
    }

    private boolean usesLegacyApiCompatibility() {
        return compatibleWithPythonLegacy && DEFAULT_LIBRARY.equals(getLibrary());
    }

    private boolean supportsHttpxSync() {
        return "httpx".equals(getLibrary())
                && Boolean.parseBoolean(String.valueOf(
                        additionalProperties.get(SUPPORT_HTTPX_SYNC)));
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        for (ModelsMap modelsMap : objs.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (model == null || !model.oneOf.isEmpty() || !model.anyOf.isEmpty()) {
                    continue;
                }
                List<CodegenProperty> generatedProperties = generatedProperties(model);
                boolean hidesStorageNames = generatedProperties.stream().anyMatch(property ->
                        property.vendorExtensions.containsKey(
                                CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE));
                if (hidesStorageNames) {
                    configurePublicNameInputs(model.vars);
                    configurePublicNameInputs(generatedProperties);
                    configureHiddenStorageNames(model, generatedProperties);
                }

                List<CodegenProperty> inputNameProperties = new ArrayList<>();
                boolean validatesInputNames = hidesStorageNames;
                for (CodegenProperty property : generatedProperties) {
                    if (property.vendorExtensions.containsKey(
                            CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_WIRE)) {
                        inputNameProperties.add(property);
                        if (property.vendorExtensions.containsKey(
                                CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME)) {
                            validatesInputNames = true;
                        }
                    }
                }
                if (hidesStorageNames || !inputNameProperties.isEmpty()) {
                    model.vendorExtensions.put(
                            CodegenConstants.X_PY_PREPROCESSES_INPUT_NAMES, true);
                    model.vendorExtensions.put(
                            CodegenConstants.X_PY_INPUT_NAME_PROPERTIES,
                            inputNameProperties);
                }
                if (validatesInputNames) {
                    model.vendorExtensions.put(
                            CodegenConstants.X_PY_VALIDATES_INPUT_NAMES, true);
                }
            }
        }

        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);
        for (ModelsMap modelsMap : processed.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel model = modelMap.getModel();
                if (model != null && model.oneOf.isEmpty() && model.anyOf.isEmpty()) {
                    for (CodegenProperty property : model.vars) {
                        if (property.vendorExtensions.containsKey(
                                CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE)) {
                            model.vendorExtensions.put(
                                    CodegenConstants.X_PY_HAS_PUBLIC_NAME_PROPERTIES, true);
                            break;
                        }
                    }
                    List<CodegenProperty> generatedProperties = generatedProperties(model);
                    if (compatibleWithPythonLegacy
                            || generatedProperties.stream().anyMatch(property ->
                                    property.vendorExtensions.containsKey(
                                            CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME))) {
                        validateModelPropertyNames(model, generatedProperties);
                    }
                }
            }
        }
        return processed;
    }

    private void configurePublicNameInputs(List<CodegenProperty> properties) {
        for (CodegenProperty property : properties) {
            String publicName = (String) property.vendorExtensions.getOrDefault(
                    CodegenConstants.X_PY_PUBLIC_NAME, property.name);
            property.vendorExtensions.put(CodegenConstants.X_PY_PUBLIC_NAME, publicName);
            property.vendorExtensions.put(
                    CodegenConstants.X_PY_PUBLIC_NAME_LITERAL,
                    toPythonStringLiteral(publicName));
            if (!publicName.equals(property.baseName)) {
                property.vendorExtensions.put(
                        CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_WIRE, true);
            }
        }
    }

    private static List<CodegenProperty> generatedProperties(CodegenModel model) {
        // model.allVars can include fields inherited from a schema-mapped
        // parent. Only generated models own input-name preprocessing here.
        Set<String> generatedPropertyBaseNames = new HashSet<>();
        for (CodegenModel ancestor = model; ancestor != null; ancestor = ancestor.parentModel) {
            for (CodegenProperty property : ancestor.vars) {
                generatedPropertyBaseNames.add(property.baseName);
            }
        }

        List<CodegenProperty> properties = new ArrayList<>();
        for (CodegenProperty property : model.allVars) {
            if (generatedPropertyBaseNames.contains(property.baseName)) {
                properties.add(property);
            }
        }
        return properties;
    }

    private void configureHiddenStorageNames(
            CodegenModel model, List<CodegenProperty> generatedProperties) {
        Set<String> inputNames = new HashSet<>();
        for (CodegenProperty property : model.allVars) {
            inputNames.add(property.baseName);
            inputNames.add((String) property.vendorExtensions.getOrDefault(
                    CodegenConstants.X_PY_PUBLIC_NAME, property.name));
        }

        Set<String> hiddenStorageNames = new HashSet<>();
        List<String> orderedHiddenStorageNames = new ArrayList<>();
        for (CodegenProperty property : generatedProperties) {
            if (property.vendorExtensions.containsKey(
                            CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE)
                    && !inputNames.contains(property.name)
                    && hiddenStorageNames.add(property.name)) {
                orderedHiddenStorageNames.add(property.name);
            }
        }
        if (!orderedHiddenStorageNames.isEmpty()) {
            model.vendorExtensions.put(
                    CodegenConstants.X_PY_HIDDEN_STORAGE_NAMES,
                    orderedHiddenStorageNames);
        }
    }

    private void validateModelPropertyNames(
            CodegenModel model, List<CodegenProperty> generatedProperties) {
        Set<String> generatedMembers = generatedModelMembers(model);
        // An unrelated nameMapping must not newly reject existing unmapped
        // fields that collide with Pydantic or generated members.
        Set<String> nameMappingGeneratedMembers =
                nameMappingGeneratedModelMembers(model);
        Map<String, CodegenProperty> inputNameOwners = new HashMap<>();
        Map<String, CodegenProperty> memberNameOwners = new HashMap<>();
        for (CodegenProperty property : generatedProperties) {
            String publicName = (String) property.vendorExtensions.getOrDefault(
                    CodegenConstants.X_PY_PUBLIC_NAME, property.name);
            boolean explicitPublicName = property.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME);
            boolean legacyMetadataCollision = compatibleWithPythonLegacy
                    && (LEGACY_MODEL_METADATA_MEMBER_NAMES.contains(publicName)
                    || LEGACY_MODEL_METADATA_MEMBER_NAMES.contains(property.name));
            boolean publicNameCollision = generatedMembers.contains(publicName)
                    && (explicitPublicName
                    || legacyMetadataCollision
                    || nameMappingGeneratedMembers.contains(publicName));
            boolean storageNameCollision = generatedMembers.contains(property.name)
                    && (explicitPublicName
                    || legacyMetadataCollision
                    || nameMappingGeneratedMembers.contains(property.name));
            if (publicNameCollision || storageNameCollision) {
                String generatedMemberName = publicNameCollision
                        ? publicName
                        : property.name;
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "property %s in model %s uses generated Python member name %s",
                        property.baseName, model.name, generatedMemberName));
            }
            if (explicitPublicName
                    && (!publicName.matches("[A-Za-z_][A-Za-z0-9_]*")
                    || PYTHON_KEYWORDS.contains(publicName)
                    || publicName.startsWith("__"))) {
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "property %s in model %s cannot use %s as its public Python name",
                        property.baseName, model.name, publicName));
            }
            if (explicitPublicName
                    && (!property.name.matches("[A-Za-z][A-Za-z0-9_]*")
                    || PYTHON_KEYWORDS.contains(property.name)
                    || MODEL_FIELD_NAME_COLLISIONS.contains(property.name)
                    || MODEL_CLASS_BODY_NAMES.contains(property.name)
                    || PYDANTIC_PRIVATE_MEMBER_NAMES.contains(property.name))) {
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "property %s in model %s has invalid generated Python field name %s",
                        property.baseName, model.name, property.name));
            }

            for (String inputName : List.of(property.baseName, publicName)) {
                CodegenProperty owner = inputNameOwners.putIfAbsent(inputName, property);
                if (owner != null
                        && !owner.baseName.equals(property.baseName)
                        && (explicitPublicName || owner.vendorExtensions.containsKey(
                                CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME))) {
                    throw new IllegalArgumentException(String.format(Locale.ROOT,
                            "properties %s and %s in model %s both accept input name %s",
                            owner.baseName, property.baseName, model.name, inputName));
                }
            }
            for (String memberName : List.of(property.name, publicName)) {
                CodegenProperty owner = memberNameOwners.putIfAbsent(memberName, property);
                if (owner != null
                        && !owner.baseName.equals(property.baseName)
                        && (explicitPublicName || owner.vendorExtensions.containsKey(
                                CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME))) {
                    throw new IllegalArgumentException(String.format(Locale.ROOT,
                            "properties %s and %s in model %s both use Python member name %s",
                            owner.baseName, property.baseName, model.name, memberName));
                }
            }
        }
        for (CodegenProperty property : generatedProperties) {
            if (!property.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE)) {
                continue;
            }
            CodegenProperty inputOwner = inputNameOwners.get(property.name);
            if (inputOwner != null && !inputOwner.baseName.equals(property.baseName)) {
                throw new IllegalArgumentException(String.format(Locale.ROOT,
                        "property %s in model %s uses generated storage name %s, "
                                + "which is an input name for property %s",
                        property.baseName, model.name, property.name, inputOwner.baseName));
            }
        }
    }

    private Set<String> generatedModelMembers(CodegenModel model) {
        Set<String> members = new HashSet<>(MODEL_PUBLIC_MEMBER_NAMES);
        if (compatibleWithPythonLegacy) {
            members.addAll(LEGACY_MODEL_METADATA_MEMBER_NAMES);
        }
        members.addAll(PYDANTIC_PRIVATE_MEMBER_NAMES);
        for (CodegenModel ancestor = model; ancestor != null; ancestor = ancestor.parentModel) {
            if (ancestor.isAdditionalPropertiesTrue) {
                members.add("additional_properties");
            }
            addMangledMember(members, ancestor.classname, "__properties");
            if (ancestor.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_PREPROCESSES_INPUT_NAMES)) {
                addMangledMember(members, ancestor.classname, "__preprocess_input_names");
            }
            if (ancestor.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_VALIDATES_INPUT_NAMES)) {
                addMangledMember(members, ancestor.classname, "__validate_input_names");
            }
            if (ancestor.hasChildren && ancestor.discriminator != null) {
                members.add("get_discriminator_value");
                addMangledMember(members, ancestor.classname, "__discriminator_property_name");
                addMangledMember(members, ancestor.classname, "__discriminator_value_class_map");
            }
            for (CodegenProperty property : ancestor.vars) {
                if (property.isEnum) {
                    members.add(property.name + "_validate_enum");
                }
                if (property.vendorExtensions.containsKey(CodegenConstants.X_REGEX)) {
                    members.add(property.name + "_validate_regular_expression");
                }
                if (property.vendorExtensions.containsKey(
                        CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE)) {
                    members.add("_" + ancestor.classname + "_" + property.name + "_public_type");
                }
            }
        }
        return members;
    }

    private Set<String> nameMappingGeneratedModelMembers(CodegenModel model) {
        Set<String> members = new HashSet<>();
        for (CodegenModel ancestor = model; ancestor != null; ancestor = ancestor.parentModel) {
            if (ancestor.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_PREPROCESSES_INPUT_NAMES)) {
                addMangledMember(members, ancestor.classname, "__preprocess_input_names");
            }
            if (ancestor.vendorExtensions.containsKey(
                    CodegenConstants.X_PY_VALIDATES_INPUT_NAMES)) {
                addMangledMember(members, ancestor.classname, "__validate_input_names");
            }
            for (CodegenProperty property : ancestor.vars) {
                if (!property.vendorExtensions.containsKey(
                        CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME)) {
                    continue;
                }
                boolean changesStorageName = !property.name.equals(
                        toVarNameWithoutNameMapping(property.baseName));
                if (changesStorageName && property.isEnum) {
                    members.add(property.name + "_validate_enum");
                }
                if (changesStorageName
                        && property.vendorExtensions.containsKey(CodegenConstants.X_REGEX)) {
                    members.add(property.name + "_validate_regular_expression");
                }
                if (property.vendorExtensions.containsKey(
                        CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE)) {
                    members.add("_" + ancestor.classname + "_" + property.name + "_public_type");
                }
            }
        }
        return members;
    }

    private static void addMangledMember(
            Set<String> members, String className, String privateName) {
        if (className != null) {
            members.add("_" + className.replaceFirst("^_+", "") + privateName);
        }
    }

    @Override
    public String toVarName(String name) {
        String mappedName = nameMapping.get(name);
        String fieldName = mappedName == null
                ? super.toVarName(name)
                : toVarNameWithoutNameMapping(mappedName);
        if (mappedName != null
                && (MODEL_FIELD_NAME_COLLISIONS.contains(fieldName)
                || MODEL_CLASS_BODY_NAMES.contains(fieldName)
                || PYDANTIC_PRIVATE_MEMBER_NAMES.contains(fieldName))) {
            fieldName = escapeReservedWord(fieldName);
        }
        return fieldName;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        // Only operation signatures gain this control argument. Keep model
        // field naming independent of the selected library.
        if (usesLegacyApiCompatibility()
                && "async_req".equals(parameter.paramName)) {
            parameter.paramName = escapeReservedWord(parameter.paramName);
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!model.oneOf.isEmpty() || !model.anyOf.isEmpty()) {
            return;
        }

        String publicName = nameMapping.get(property.baseName);
        if (publicName == null) {
            if (!compatibleWithPythonLegacy) {
                return;
            }
            publicName = property.name;
            property.vendorExtensions.put(CodegenConstants.X_PY_LEGACY_PUBLIC_NAME, true);
        } else {
            property.vendorExtensions.put(CodegenConstants.X_PY_EXPLICIT_PUBLIC_NAME, true);
        }
        property.vendorExtensions.put(CodegenConstants.X_PY_PUBLIC_NAME, publicName);
        property.vendorExtensions.put(
                CodegenConstants.X_PY_PUBLIC_NAME_LITERAL,
                toPythonStringLiteral(publicName));
        if (!publicName.equals(property.baseName)) {
            property.vendorExtensions.put(CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_WIRE, true);
        }
        if (!publicName.equals(property.name)) {
            property.vendorExtensions.put(
                    CodegenConstants.X_PY_PUBLIC_NAME_DIFFERS_FROM_STORAGE, true);
            // Pydantic 2 retains the deprecated BaseModel.schema() class method, so a
            // mapped schema property intentionally overrides its type signature:
            // https://github.com/pydantic/pydantic/blob/v2.11.0/pydantic/main.py
            if ("schema".equals(publicName)) {
                property.vendorExtensions.put(
                        CodegenConstants.X_PY_PUBLIC_NAME_OVERRIDES_BASE_MODEL, true);
            }
        }
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "var_" + name;
    }
}
