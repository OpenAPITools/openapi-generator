/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.github.curiousoddman.rgxgen.RgxGen;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.commons.lang3.tuple.Triple;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.ignore.CodegenIgnoreProcessor;
import org.openapitools.codegen.templating.*;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.openapitools.codegen.api.TemplateProcessor;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.OnceLogger.once;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonExperimentalClientCodegen extends AbstractPythonCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonExperimentalClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    public static final String DEFAULT_LIBRARY = "urllib3";
    // nose is a python testing framework, we use pytest if USE_NOSE is unset
    public static final String USE_NOSE = "useNose";
    public static final String RECURSION_LIMIT = "recursionLimit";

    protected String packageUrl;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean useNose = Boolean.FALSE;

    protected Map<Character, String> regexModifiers;

    private String testFolder;

    // A cache to efficiently lookup a Schema instance based on the return value of `toModelName()`.
    private Map<String, Schema> modelNameToSchemaCache;
    private DateTimeFormatter iso8601Date = DateTimeFormatter.ISO_DATE;
    private DateTimeFormatter iso8601DateTime = DateTimeFormatter.ISO_DATE_TIME;

    private String templateExtension;
    protected CodegenIgnoreProcessor ignoreProcessor;
    protected TemplateProcessor templateProcessor = null;

    public PythonExperimentalClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Simple,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union
                )
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .includeGlobalFeatures(
                        GlobalFeature.ParameterizedServer,
                        GlobalFeature.ParameterStyling
                )
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
                .excludeSchemaSupportFeatures(
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        modelPackage = "model";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separatorChar + "python";

        embeddedTemplateDir = templateDir = "python-experimental";

        testFolder = "test";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable name used in API methods (endpoints)
                        "all_params", "resource_path", "path_params", "query_params",
                        "header_params", "form_params", "local_var_files", "body_params", "auth_settings",
                        // @property
                        "property",
                        // python reserved words
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try", "self", "nonlocal", "None", "True",
                        "False", "async", "await",
                        // types
                        "float", "int", "str", "bool", "none_type", "dict", "frozendict", "list", "tuple", "file_type"));

        regexModifiers = new HashMap<Character, String>();
        regexModifiers.put('i', "IGNORECASE");
        regexModifiers.put('l', "LOCALE");
        regexModifiers.put('m', "MULTILINE");
        regexModifiers.put('s', "DOTALL");
        regexModifiers.put('u', "UNICODE");
        regexModifiers.put('x', "VERBOSE");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue("openapi_client"));
        cliOptions.add(new CliOption(CodegenConstants.PROJECT_NAME, "python project name in setup.py (e.g. petstore-api)."));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(PACKAGE_URL, "python package URL."));
        // this generator does not use SORT_PARAMS_BY_REQUIRED_FLAG
        // this generator uses the following order for endpoint paramters and model properties
        // required params
        // optional params which are set to unset as their default for method signatures only
        // optional params as **kwargs
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCECODEONLY_GENERATION, CodegenConstants.SOURCECODEONLY_GENERATION_DESC)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(USE_NOSE, "use the nose test framework").
                defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(RECURSION_LIMIT, "Set the recursion limit. If not set, use the system default value."));

        supportedLibraries.put("urllib3", "urllib3-based client");
        supportedLibraries.put("asyncio", "Asyncio-based client (python 3.5+)");
        supportedLibraries.put("tornado", "tornado-based client");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use: asyncio, tornado, urllib3");
        libraryOption.setDefault(DEFAULT_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(DEFAULT_LIBRARY);

        // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
        // In principle, this should be enabled by default for all code generators. However due to limitations
        // in other code generators, support needs to be enabled on a case-by-case basis.
        supportsAdditionalPropertiesWithComposedSchema = true;

        // When the 'additionalProperties' keyword is not present in a OAS schema, allow
        // undeclared properties. This is compliant with the JSON schema specification.
        this.setDisallowAdditionalPropertiesIfNotPresent(false);

        // this may set datatype right for additional properties
        instantiationTypes.put("map", "dict");

        languageSpecificPrimitives.add("file_type");
        languageSpecificPrimitives.add("none_type");
        typeMapping.put("decimal", "str");

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);

        super.processOpts();

        TemplatingEngineAdapter te = getTemplatingEngine();
        if (te instanceof HandlebarsEngineAdapter) {
            HandlebarsEngineAdapter hea = (HandlebarsEngineAdapter) te;
            hea.infiniteLoops(true);
            hea.setPrettyPrint(true);
        } else {
            throw new RuntimeException("Only the HandlebarsEngineAdapter is supported for this generator");
        }

        TemplatePathLocator commonTemplateLocator = new CommonTemplateContentLocator();
        TemplatePathLocator generatorTemplateLocator = new GeneratorTemplateContentLocator(this);
        TemplateManagerOptions templateManagerOptions = new TemplateManagerOptions(this.isEnableMinimalUpdate(),this.isSkipOverwrite());
        templateProcessor = new TemplateManager(
                templateManagerOptions,
                te,
                new TemplatePathLocator[]{generatorTemplateLocator, commonTemplateLocator}
        );
        templateExtension = te.getIdentifier();

        String ignoreFileLocation = this.getIgnoreFilePathOverride();
        if (ignoreFileLocation != null) {
            final File ignoreFile = new File(ignoreFileLocation);
            if (ignoreFile.exists() && ignoreFile.canRead()) {
                this.ignoreProcessor = new CodegenIgnoreProcessor(ignoreFile);
            } else {
                LOGGER.warn("Ignore file specified at {} is not valid. This will fall back to an existing ignore file if present in the output directory.", ignoreFileLocation);
            }
        }

        if (this.ignoreProcessor == null) {
            this.ignoreProcessor = new CodegenIgnoreProcessor(this.getOutputDir());
        }

        modelTemplateFiles.put("model." + templateExtension, ".py");
            apiTemplateFiles.put("api." + templateExtension, ".py");
        modelTestTemplateFiles.put("model_test." + templateExtension, ".py");
        apiTestTemplateFiles.put("api_test." + templateExtension, ".py");
        modelDocTemplateFiles.put("model_doc." + templateExtension, ".md");
        apiDocTemplateFiles.put("api_doc." + templateExtension, ".md");

        if (StringUtils.isEmpty(System.getenv("PYTHON_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PYTHON_POST_PROCESS_FILE not defined so the Python code may not be properly formatted. To define it, try 'export PYTHON_POST_PROCESS_FILE=\"/usr/local/bin/yapf -i\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

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
            apiDocPath = packagePath() + File.separatorChar + apiDocPath;
            modelDocPath = packagePath() + File.separatorChar + modelDocPath;
        }
        // make api and model doc path available in templates
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (additionalProperties.containsKey(PACKAGE_URL)) {
            setPackageUrl((String) additionalProperties.get(PACKAGE_URL));
        }

        if (additionalProperties.containsKey(USE_NOSE)) {
            setUseNose((String) additionalProperties.get(USE_NOSE));
        }

        // check to see if setRecursionLimit is set and whether it's an integer
        if (additionalProperties.containsKey(RECURSION_LIMIT)) {
            try {
                Integer.parseInt((String) additionalProperties.get(RECURSION_LIMIT));
            } catch (NumberFormatException | NullPointerException e) {
                throw new IllegalArgumentException("recursionLimit must be an integer, e.g. 2000.");
            }
        }

        String readmePath = "README.md";
        String readmeTemplate = "README." + templateExtension;
        if (generateSourceCodeOnly) {
            readmePath = packagePath() + "_" + readmePath;
            readmeTemplate = "README_onlypackage." + templateExtension;
        }
        supportingFiles.add(new SupportingFile(readmeTemplate, "", readmePath));

        if (!generateSourceCodeOnly) {
            supportingFiles.add(new SupportingFile("tox." + templateExtension, "", "tox.ini"));
            supportingFiles.add(new SupportingFile("test-requirements." + templateExtension, "", "test-requirements.txt"));
            supportingFiles.add(new SupportingFile("requirements." + templateExtension, "", "requirements.txt"));
            supportingFiles.add(new SupportingFile("setup_cfg." + templateExtension, "", "setup.cfg"));

            supportingFiles.add(new SupportingFile("git_push.sh." + templateExtension, "", "git_push.sh"));
            supportingFiles.add(new SupportingFile("gitignore." + templateExtension, "", ".gitignore"));
            supportingFiles.add(new SupportingFile("travis." + templateExtension, "", ".travis.yml"));
            supportingFiles.add(new SupportingFile("gitlab-ci." + templateExtension, "", ".gitlab-ci.yml"));
            supportingFiles.add(new SupportingFile("setup." + templateExtension, "", "setup.py"));
        }
        supportingFiles.add(new SupportingFile("configuration." + templateExtension, packagePath(), "configuration.py"));
        supportingFiles.add(new SupportingFile("__init__package." + templateExtension, packagePath(), "__init__.py"));

        // If the package name consists of dots(openapi.client), then we need to create the directory structure like openapi/client with __init__ files.
        String[] packageNameSplits = packageName.split("\\.");
        String currentPackagePath = "";
        for (int i = 0; i < packageNameSplits.length - 1; i++) {
            if (i > 0) {
                currentPackagePath = currentPackagePath + File.separatorChar;
            }
            currentPackagePath = currentPackagePath + packageNameSplits[i];
            supportingFiles.add(new SupportingFile("__init__." + templateExtension, currentPackagePath, "__init__.py"));
        }

        supportingFiles.add(new SupportingFile("exceptions." + templateExtension, packagePath(), "exceptions.py"));

        if (Boolean.FALSE.equals(excludeTests)) {
            supportingFiles.add(new SupportingFile("__init__." + templateExtension, testFolder, "__init__.py"));
        }

        supportingFiles.add(new SupportingFile("api_client." + templateExtension, packagePath(), "api_client.py"));

        if ("asyncio".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("asyncio/rest." + templateExtension, packagePath(), "rest.py"));
            additionalProperties.put("asyncio", "true");
        } else if ("tornado".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("tornado/rest." + templateExtension, packagePath(), "rest.py"));
            additionalProperties.put("tornado", "true");
        } else {
            supportingFiles.add(new SupportingFile("rest." + templateExtension, packagePath(), "rest.py"));
        }

        supportingFiles.add(new SupportingFile("schemas." + templateExtension, packagePath(), "schemas.py"));

        // add the models and apis folders
        supportingFiles.add(new SupportingFile("__init__models." + templateExtension, packagePath() + File.separatorChar + "models", "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__model." + templateExtension, packagePath() + File.separatorChar + modelPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__apis." + templateExtension, packagePath() + File.separatorChar + "apis", "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__api." + templateExtension, packagePath() + File.separatorChar + apiPackage, "__init__.py"));
        // Generate the 'signing.py' module, but only if the 'HTTP signature' security scheme is specified in the OAS.
        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
                (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
            supportingFiles.add(new SupportingFile("signing." + templateExtension, packagePath(), "signing.py"));
        }

        // default this to true so the python ModelSimple models will be generated
        ModelUtils.setGenerateAliasAsModel(true);
        LOGGER.info(CodegenConstants.GENERATE_ALIAS_AS_MODEL + " is hard coded to true in this generator. Alias models will only be generated if they contain validations or enums");

        // check library option to ensure only urllib3 is supported
        if (!DEFAULT_LIBRARY.equals(getLibrary())) {
            throw new RuntimeException("Only the `urllib3` library is supported in the refactored `python` client generator at the moment. Please fall back to `python-legacy` client generator for the time being. We welcome contributions to add back `asyncio`, `tornado` support to the `python` client generator.");
        }
    }

    public String endpointFilename(String templateName, String tag, String operationId) {
        return apiFileFolder() + File.separator + toApiFilename(tag) + "_endpoints" + File.separator + operationId + ".py";
    }

    protected File processTemplateToFile(Map<String, Object> templateData, String templateName, String outputFilename, boolean shouldGenerate, String skippedByOption) throws IOException {
        String adjustedOutputFilename = outputFilename.replaceAll("//", "/").replace('/', File.separatorChar);
        File target = new File(adjustedOutputFilename);
        if (ignoreProcessor.allowsFile(target)) {
            if (shouldGenerate) {
                Path outDir = java.nio.file.Paths.get(this.getOutputDir()).toAbsolutePath();
                Path absoluteTarget = target.toPath().toAbsolutePath();
                if (!absoluteTarget.startsWith(outDir)) {
                    throw new RuntimeException(String.format(Locale.ROOT, "Target files must be generated within the output directory; absoluteTarget=%s outDir=%s", absoluteTarget, outDir));
                }
                return this.templateProcessor.write(templateData,templateName, target);
            } else {
                this.templateProcessor.skip(target.toPath(), String.format(Locale.ROOT, "Skipped by %s options supplied by user.", skippedByOption));
                return null;
            }
        } else {
            this.templateProcessor.ignore(target.toPath(), "Ignored by rule in ignore file.");
            return null;
        }
    }

    /*
    I made this method because endpoint parameters not contain a lot of needed metadata
    It is very verbose to write all of this info into the api template
    This ingests all operations under a tag in the objs input and writes out one file for each endpoint
     */
    protected void generateEndpoints(Map<String, Object> objs) {
        if (!(Boolean) additionalProperties.get(CodegenConstants.GENERATE_APIS)) {
            return;
        }
        HashMap<String, Object> operations = (HashMap<String, Object>) objs.get("operations");
        ArrayList<CodegenOperation> codegenOperations = (ArrayList<CodegenOperation>) operations.get("operation");
        for (CodegenOperation co: codegenOperations) {
            for (Tag tag: co.tags) {
                String tagName = tag.getName();
                String pythonTagName = toVarName(tagName);
                Map<String, Object> operationMap = new HashMap<>();
                operationMap.put("operation", co);
                operationMap.put("imports", co.imports);
                operationMap.put("packageName", packageName);

                String templateName = "endpoint.handlebars";
                String filename = endpointFilename(templateName, pythonTagName, co.operationId);
                try {
                    File written = processTemplateToFile(operationMap, templateName, filename, true, CodegenConstants.APIS);
                } catch (IOException e) {
                    LOGGER.error("Error when writing template file {}", e.toString());
                }
            }
        }
    }

    /*
    We have a custom version of this method so for composed schemas and object schemas we add properties only if they
    are defined in that schema (x.properties). We do this because validation should be done independently in each schema
    If properties are hosted into composed schemas, they can collide or incorrectly list themself as required when
    they are not.
     */
    @Override
    protected void addVarsRequiredVarsAdditionalProps(Schema schema, IJsonSchemaValidationProperties property){
        setAddProps(schema, property);
        if (schema instanceof ComposedSchema && supportsAdditionalPropertiesWithComposedSchema) {
            // if schema has properties outside of allOf/oneOf/anyOf also add them
            ComposedSchema cs = (ComposedSchema) schema;
            if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
                if (cs.getOneOf() != null && !cs.getOneOf().isEmpty()) {
                    LOGGER.warn("'oneOf' is intended to include only the additional optional OAS extension discriminator object. " +
                            "For more details, see https://json-schema.org/draft/2019-09/json-schema-core.html#rfc.section.9.2.1.3 and the OAS section on 'Composition and Inheritance'.");
                }
                HashSet<String> requiredVars = new HashSet<>();
                if (schema.getRequired() != null) {
                    requiredVars.addAll(schema.getRequired());
                }
                addVars(property, property.getVars(), schema.getProperties(), requiredVars);
            }
            return;
        } else if (ModelUtils.isTypeObjectSchema(schema)) {
            HashSet<String> requiredVars = new HashSet<>();
            if (schema.getRequired() != null) {
                requiredVars.addAll(schema.getRequired());
                property.setHasRequired(true);
            }
            addVars(property, property.getVars(), schema.getProperties(), requiredVars);
            if (property.getVars() != null && !property.getVars().isEmpty()) {
                property.setHasVars(true);
            }
        }
        return;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-experimental";
    }

    @Override
    public String getHelp() {
        String newLine = System.getProperty("line.separator");
        return String.join("<br />",
                "Generates a Python client library",
                "",
                "Features in this generator:",
                "- type hints on endpoints and model creation",
                "- model parameter names use the spec defined keys and cases",
                "- robust composition (oneOf/anyOf/allOf) where paload data is stored in one instance only",
                "- endpoint parameter names use the spec defined keys and cases",
                "- inline schemas are supported at any location including composition",
                "- multiple content types supported in request body and response bodies",
                "- run time type checking",
                "- Sending/receiving decimals as strings supported with type:string format: number -> DecimalSchema",
                "- quicker load time for python modules (a single endpoint can be imported and used without loading others)",
                "- all instances of schemas dynamically inherit from all matching schemas so one can use isinstance to check if validation passed",
                "- composed schemas with type constraints supported (type:object + oneOf/anyOf/allOf)",
                "- schemas are not coerced/cast. For example string + date are both stored as string, and there is a date accessor",
                "    - Exceptions: int/float is stored as Decimal, When receiving data from headers it will start as str and may need to be cast for example to int");
    }

    @Override
    public Schema unaliasSchema(Schema schema, Map<String, String> usedImportMappings) {
        Map<String, Schema> allSchemas = ModelUtils.getSchemas(openAPI);
        if (allSchemas == null || allSchemas.isEmpty()) {
            // skip the warning as the spec can have no model defined
            //LOGGER.warn("allSchemas cannot be null/empty in unaliasSchema. Returned 'schema'");
            return schema;
        }

        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            String simpleRef = ModelUtils.getSimpleRef(schema.get$ref());
            if (usedImportMappings.containsKey(simpleRef)) {
                LOGGER.debug("Schema unaliasing of {} omitted because aliased class is to be mapped to {}", simpleRef, usedImportMappings.get(simpleRef));
                return schema;
            }
            Schema ref = allSchemas.get(simpleRef);
            if (ref == null) {
                once(LOGGER).warn("{} is not defined", schema.get$ref());
                return schema;
            } else if (ref.getEnum() != null && !ref.getEnum().isEmpty()) {
                // top-level enum class
                return schema;
            } else if (ModelUtils.isArraySchema(ref)) {
                if (ModelUtils.isGenerateAliasAsModel(ref)) {
                    return schema; // generate a model extending array
                } else {
                    return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            usedImportMappings);
                }
            } else if (ModelUtils.isComposedSchema(ref)) {
                return schema;
            } else if (ModelUtils.isMapSchema(ref)) {
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) // has at least one property
                    return schema; // treat it as model
                else {
                    if (ModelUtils.isGenerateAliasAsModel(ref)) {
                        return schema; // generate a model extending map
                    } else {
                        // treat it as a typical map
                        return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                                usedImportMappings);
                    }
                }
            } else if (ModelUtils.isObjectSchema(ref)) { // model
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) { // has at least one property
                    return schema;
                } else {
                    // free form object (type: object)
                    if (ModelUtils.hasValidation(ref)) {
                        return schema;
                    } else if (getAllOfDescendants(simpleRef, openAPI).size() > 0) {
                        return schema;
                    }
                    return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            usedImportMappings);
                }
            } else if (ModelUtils.hasValidation(ref)) {
                // non object non array non map schemas that have validations
                // are returned so we can generate those schemas as models
                // we do this to:
                // - preserve the validations in that model class in python
                // - use those validations when we use this schema in composed oneOf schemas
                return schema;
            } else if (Boolean.TRUE.equals(ref.getNullable()) && ref.getEnum() == null) {
                // non enum primitive with nullable True
                // we make these models so instances of this will be subclasses of this model
                return schema;
            } else {
                return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())), usedImportMappings);
            }
        }
        return schema;
    }

    public String pythonDate(Object dateValue) {
        String strValue = null;
        if (dateValue instanceof OffsetDateTime) {
            OffsetDateTime date = null;
            try {
                date = (OffsetDateTime) dateValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date` format for value {}", dateValue.toString());
                date = ((Date) dateValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = date.format(iso8601Date);
        } else {
            strValue = dateValue.toString();
        }
        return "isoparse('" + strValue + "').date()";
    }

    public String pythonDateTime(Object dateTimeValue) {
        String strValue = null;
        if (dateTimeValue instanceof OffsetDateTime) {
            OffsetDateTime dateTime = null;
            try {
                dateTime = (OffsetDateTime) dateTimeValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date-time` format for value {}", dateTimeValue.toString());
                dateTime = ((Date) dateTimeValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = dateTime.format(iso8601DateTime);
        } else {
            strValue = dateTimeValue.toString();
        }
        return "isoparse('" + strValue + "')";
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        Object defaultObject = null;
        if (p.getDefault() != null) {
            defaultObject = p.getDefault();
        }

        if (defaultObject == null) {
            return null;
        }

        String defaultValue = defaultObject.toString();
        if (ModelUtils.isDateSchema(p)) {
            defaultValue = pythonDate(defaultObject);
        } else if (ModelUtils.isDateTimeSchema(p)) {
            defaultValue = pythonDateTime(defaultObject);
        } else if (ModelUtils.isStringSchema(p) && !ModelUtils.isByteArraySchema(p) && !ModelUtils.isBinarySchema(p) && !ModelUtils.isFileSchema(p) && !ModelUtils.isUUIDSchema(p) && !ModelUtils.isEmailSchema(p)) {
            defaultValue = ensureQuotes(defaultValue);
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (Boolean.valueOf(defaultValue) == false) {
                defaultValue = "False";
            } else {
                defaultValue = "True";
            }
        }
        return defaultValue;
    }

    @Override
    public String toModelImport(String name) {
        // name looks like Cat
        return "from " + packagePath() + "." +  modelPackage() + "." + toModelFilename(name) + " import " + toModelName(name);
    }

    @Override
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        // fix the imports that each model has, add the module reference to the model
        // loops through imports and converts them all
        // from 'Pet' to 'from petstore_api.model.pet import Pet'

        HashMap<String, Object> val = (HashMap<String, Object>) objs.get("operations");
        ArrayList<CodegenOperation> operations = (ArrayList<CodegenOperation>) val.get("operation");
        ArrayList<HashMap<String, String>> imports = (ArrayList<HashMap<String, String>>) objs.get("imports");
        for (CodegenOperation operation : operations) {
            if (operation.imports.size() == 0) {
                continue;
            }
            String[] modelNames = operation.imports.toArray(new String[0]);
            operation.imports.clear();
            for (String modelName : modelNames) {
                operation.imports.add(toModelImport(modelName));
            }
        }
        generateEndpoints(objs);
        return objs;
    }

    /***
     * Override with special post-processing for all models.
     * we have a custom version of this method to:
     * - remove any primitive models that do not contain validations
     *      these models are unaliased as inline definitions wherever the spec has them as refs
     *      this means that the generated client does not use these models
     *      because they are not used we do not write them
     * - fix the model imports, go from model name to the full import string with toModelImport + globalImportFixer
     *
     * @param objs a map going from the model name to a object hoding the model info
     * @return the updated objs
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        super.postProcessAllModels(objs);

        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        for (String schemaName : allDefinitions.keySet()) {
            Schema refSchema = new Schema().$ref("#/components/schemas/" + schemaName);
            Schema unaliasedSchema = unaliasSchema(refSchema, importMapping);
            String modelName = toModelName(schemaName);
            if (unaliasedSchema.get$ref() == null) {
                continue;
            } else {
                HashMap<String, Object> objModel = (HashMap<String, Object>) objs.get(modelName);
                if (objModel != null) { // to avoid form parameter's models that are not generated (skipFormModel=true)
                    List<Map<String, Object>> models = (List<Map<String, Object>>) objModel.get("models");
                    for (Map<String, Object> model : models) {
                        CodegenModel cm = (CodegenModel) model.get("model");
                        String[] importModelNames = cm.imports.toArray(new String[0]);
                        cm.imports.clear();
                        for (String importModelName : importModelNames) {
                            cm.imports.add(toModelImport(importModelName));
                        }
                    }
                }
            }
        }

        return objs;
    }

    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter cp = super.fromParameter(parameter, imports);
        switch(parameter.getStyle()) {
            case MATRIX:
                cp.style = "MATRIX";
                break;
            case LABEL:
                cp.style = "LABEL";
                break;
            case FORM:
                cp.style = "FORM";
                break;
            case SIMPLE:
                cp.style = "SIMPLE";
                break;
            case SPACEDELIMITED:
                cp.style = "SPACE_DELIMITED";
                break;
            case PIPEDELIMITED:
                cp.style = "PIPE_DELIMITED";
                break;
            case DEEPOBJECT:
                cp.style = "DEEP_OBJECT";
                break;
        }
        // clone this so we can change some properties on it
        CodegenProperty schemaProp = cp.getSchema().clone();
        // parameters may have valid python names like some_val or invalid ones like Content-Type
        // we always set nameInSnakeCase to null so special handling will not be done for these names
        // invalid python names will be handled in python by using a TypedDict which will allow us to have a type hint
        // for keys that cannot be variable names to the schema baseName
        if (schemaProp != null) {
            schemaProp.nameInSnakeCase = null;
            schemaProp.baseName = toModelName(cp.baseName) + "Schema";
            cp.setSchema(schemaProp);
        }
        return cp;
    }

    private boolean isValidPythonVarOrClassName(String name) {
        return name.matches("^[_a-zA-Z][_a-zA-Z0-9]*$");
    }


    /**
     * Convert OAS Property object to Codegen Property object
     * We have a custom version of this method to always set allowableValues.enumVars on all enum variables
     * Together with unaliasSchema this sets primitive types with validations as models
     * This method is used by fromResponse
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty cp = super.fromProperty(name, p);
        if (cp.isAnyType && cp.isNullable) {
            cp.isNullable = false;
        }
        if (cp.isNullable && cp.complexType == null) {
            cp.setIsNull(true);
            cp.isNullable = false;
            cp.setHasMultipleTypes(true);
        }
        postProcessPattern(cp.pattern, cp.vendorExtensions);
        // if we have a property that has a difficult name, either:
        // 1. name is reserved, like class int float
        // 2. name is invalid in python like '3rd' or 'Content-Type'
        // set cp.nameInSnakeCase to a value so we can tell that we are in this use case
        // we handle this in the schema templates
        // templates use its presence to handle these badly named variables / keys
        if ((isReservedWord(name) || !isValidPythonVarOrClassName(name)) && !name.equals(cp.name)) {
            cp.nameInSnakeCase = cp.name;
        } else {
            cp.nameInSnakeCase = null;
        }
        if (cp.isEnum) {
            updateCodegenPropertyEnum(cp);
        }
        Schema unaliasedSchema = unaliasSchema(p, importMapping);
        if (cp.isPrimitiveType && unaliasedSchema.get$ref() != null) {
            cp.complexType = cp.dataType;
        }
        setAdditionalPropsAndItemsVarNames(cp);
        return cp;
    }

    private void setAdditionalPropsAndItemsVarNames(IJsonSchemaValidationProperties item) {
        if (item.getAdditionalProperties() != null) {
            item.getAdditionalProperties().setBaseName("_additional_properties");
        }
        if (item.getItems() != null) {
            item.getItems().setBaseName("_items");
        }
    }

    /**
     * checks if the data should be classified as "string" in enum
     * e.g. double in C# needs to be double-quoted (e.g. "2.8") by treating it as a string
     * In the future, we may rename this function to "isEnumString"
     *
     * @param dataType data type
     * @return true if it's a enum string
     */
    @Override
    public boolean isDataTypeString(String dataType) {
        return "str".equals(dataType);
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // we have a custom version of this method to omit overwriting the defaultValue
        Map<String, Object> allowableValues = var.allowableValues;

        // handle array
        if (var.mostInnerItems != null) {
            allowableValues = var.mostInnerItems.allowableValues;
        }

        if (allowableValues == null) {
            return;
        }

        List<Object> values = (List<Object>) allowableValues.get("values");
        if (values == null) {
            return;
        }

        String varDataType = var.mostInnerItems != null ? var.mostInnerItems.dataType : var.dataType;
        Schema referencedSchema = getModelNameToSchemaCache().get(varDataType);
        String dataType = (referencedSchema != null) ? getTypeDeclaration(referencedSchema) : varDataType;

        // put "enumVars" map into `allowableValues", including `name` and `value`
        List<Map<String, Object>> enumVars = buildEnumVars(values, dataType);

        // if "x-enum-varnames" or "x-enum-descriptions" defined, update varnames
        Map<String, Object> extensions = var.mostInnerItems != null ? var.mostInnerItems.getVendorExtensions() : var.getVendorExtensions();
        if (referencedSchema != null) {
            extensions = referencedSchema.getExtensions();
        }
        updateEnumVarsWithExtensions(enumVars, extensions, dataType);
        allowableValues.put("enumVars", enumVars);
        // overwriting defaultValue omitted from here
    }

    /***
     * We have a custom version of this method to produce links to models when they are
     * primitive type (not map, not array, not object) and include validations or are enums
     *
     * @param body requesst body
     * @param imports import collection
     * @param bodyParameterName body parameter name
     * @return the resultant CodegenParameter
     */
    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        CodegenParameter cp = super.fromRequestBody(body, imports, bodyParameterName);
        cp.baseName = "body";
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        if (schema.get$ref() == null) {
            return cp;
        }
        Schema unaliasedSchema = unaliasSchema(schema, importMapping);
        CodegenProperty unaliasedProp = fromProperty("body", unaliasedSchema);
        Boolean dataTypeMismatch = !cp.dataType.equals(unaliasedProp.dataType);
        Boolean baseTypeMismatch = !cp.baseType.equals(unaliasedProp.complexType) && unaliasedProp.complexType != null;
        if (dataTypeMismatch || baseTypeMismatch) {
            cp.dataType = unaliasedProp.dataType;
            cp.baseType = unaliasedProp.complexType;
        }
        return cp;
    }

    /***
     * Adds the body model schema to the body parameter
     * We have a custom version of this method so we can flip forceSimpleRef
     * to True based upon the results of unaliasSchema
     * With this customization, we ensure that when schemas are passed to getSchemaType
     * - if they have ref in them they are a model
     * - if they do not have ref in them they are not a model
     * and code is also customized to allow anyType request body schemas
     *
     * @param codegenParameter the body parameter
     * @param name model schema ref key in components
     * @param schema the model schema (not refed)
     * @param imports collection of imports
     * @param bodyParameterName body parameter name
     * @param forceSimpleRef if true use a model reference
     */
    @Override
    protected void addBodyModelSchema(CodegenParameter codegenParameter, String name, Schema schema, Set<String> imports, String bodyParameterName, boolean forceSimpleRef) {
        if (name != null) {
            Schema bodySchema = new Schema().$ref("#/components/schemas/" + name);
            Schema unaliased = unaliasSchema(bodySchema, importMapping);
            if (unaliased.get$ref() != null) {
                forceSimpleRef = true;
            }
        }

        CodegenModel codegenModel = null;
        if (StringUtils.isNotBlank(name)) {
            schema.setName(name);
            codegenModel = fromModel(name, schema);
        }
        if (codegenModel != null) {
            codegenParameter.isModel = true;
        }

        if (codegenModel != null && (codegenModel.hasVars || forceSimpleRef)) {
            if (StringUtils.isEmpty(bodyParameterName)) {
                codegenParameter.baseName = codegenModel.classname;
            } else {
                codegenParameter.baseName = bodyParameterName;
            }
            codegenParameter.paramName = toParamName(codegenParameter.baseName);
            codegenParameter.baseType = codegenModel.classname;
            codegenParameter.dataType = getTypeDeclaration(codegenModel.classname);
            codegenParameter.description = codegenModel.description;
            codegenParameter.isNullable = codegenModel.isNullable;
            imports.add(codegenParameter.baseType);
        } else {
            CodegenProperty codegenProperty = fromProperty("property", schema);

            if (codegenProperty != null && codegenProperty.getComplexType() != null && codegenProperty.getComplexType().contains(" | ")) {
                List<String> parts = Arrays.asList(codegenProperty.getComplexType().split(" \\| "));
                imports.addAll(parts);
                String codegenModelName = codegenProperty.getComplexType();
                codegenParameter.baseName = codegenModelName;
                codegenParameter.paramName = toParamName(codegenParameter.baseName);
                codegenParameter.baseType = codegenParameter.baseName;
                codegenParameter.dataType = getTypeDeclaration(codegenModelName);
                codegenParameter.description = codegenProperty.getDescription();
                codegenParameter.isNullable = codegenProperty.isNullable;
            } else {
                if (ModelUtils.isMapSchema(schema)) {// http body is map
                    LOGGER.error("Map should be supported. Please report to openapi-generator github repo about the issue.");
                } else if (codegenProperty != null) {
                    String codegenModelName, codegenModelDescription;

                    if (codegenModel != null) {
                        codegenModelName = codegenModel.classname;
                        codegenModelDescription = codegenModel.description;
                    } else {
                        codegenModelName = "anyType";
                        codegenModelDescription = "";
                    }

                    if (StringUtils.isEmpty(bodyParameterName)) {
                        codegenParameter.baseName = codegenModelName;
                    } else {
                        codegenParameter.baseName = bodyParameterName;
                    }

                    codegenParameter.paramName = toParamName(codegenParameter.baseName);
                    codegenParameter.baseType = codegenModelName;
                    codegenParameter.dataType = getTypeDeclaration(codegenModelName);
                    codegenParameter.description = codegenModelDescription;

                    if (codegenProperty.complexType != null) {
                        imports.add(codegenProperty.complexType);
                    }
                }
            }

            // set nullable
            setParameterNullable(codegenParameter, codegenProperty);
        }

    }


    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    public String toEnumVarName(String value, String datatype) {
        // our enum var names are keys in a python dict, so change spaces to underscores
        if (value.length() == 0) {
            return "EMPTY";
        }

        String intPattern = "^[-\\+]?\\d+$";
        String floatPattern = "^[-\\+]?\\d+\\.\\d+$";
        Boolean intMatch = Pattern.matches(intPattern, value);
        Boolean floatMatch = Pattern.matches(floatPattern, value);
        if (intMatch || floatMatch) {
            String plusSign = "^\\+.+";
            String negSign = "^-.+";
            if (Pattern.matches(plusSign, value)) {
                value = value.replace("+", "POSITIVE_");
            } else if (Pattern.matches(negSign, value)) {
                value = value.replace("-", "NEGATIVE_");
            } else {
                value = "POSITIVE_" + value;
            }
            if (floatMatch) {
                value = value.replace(".", "_PT_");
            }
            return value;
        }
        // Replace " " with _
        String usedValue = value.replaceAll("\\s+", "_").toUpperCase(Locale.ROOT);
        // strip first character if it is invalid
        usedValue = usedValue.replaceAll("^[^_a-zA-Z]", "");
        usedValue = usedValue.replaceAll("[^_a-zA-Z0-9]*", "");
        if (usedValue.length() == 0) {
            for (int i = 0; i < value.length(); i++){
                Character c = value.charAt(i);
                String charName = Character.getName(c.hashCode());
                usedValue += charNameToVarName(charName);
            }
            // remove trailing _
            usedValue = usedValue.replaceAll("[_]$", "");
        }
        return usedValue;
    }

    /**
     * Replace - and " " with _
     * Remove SIGN
     *
     * @param charName
     * @return
     */
    private String charNameToVarName(String charName) {
        String varName = charName.replaceAll("[\\-\\s]", "_");
        varName = varName.replaceAll("SIGN", "");
        return varName;
    }

    /**
     * Return the enum value in the language specified format
     * e.g. status becomes "status"
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized value for enum
     */
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "float".equals(datatype) || datatype.equals("int, float")) {
            return value;
        } else if ("bool".equals(datatype)) {
            return value.substring(0, 1).toUpperCase(Locale.ROOT) + value.substring(1);
        } else {
            return ensureQuotes(value);
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter p) {
        postProcessPattern(p.pattern, p.vendorExtensions);
        if (p.baseType != null && languageSpecificPrimitives.contains(p.baseType)) {
            // set baseType to null so the api docs will not point to a model for languageSpecificPrimitives
            p.baseType = null;
        }
    }

    /**
     * Sets the value of the 'model.parent' property in CodegenModel
     * We have a custom version of this function so we can add the dataType on the ArrayModel
     */
    @Override
    protected void addParentContainer(CodegenModel model, String name, Schema schema) {
        super.addParentContainer(model, name, schema);

        List<String> referencedModelNames = new ArrayList<String>();
        model.dataType = getTypeString(schema, "", "", referencedModelNames);
    }

    /**
     * Convert OAS Model object to Codegen Model object
     * We have a custom version of this method so we can:
     * - set the correct regex values for requiredVars + optionalVars
     * - set model.defaultValue and model.hasRequired per the three use cases defined in this method
     *
     * @param name the name of the model
     * @param sc   OAS Model object
     * @return Codegen Model object
     */
    @Override
    public CodegenModel fromModel(String name, Schema sc) {
        CodegenModel cm = super.fromModel(name, sc);
        Schema unaliasedSchema = unaliasSchema(sc, importMapping);
        if (unaliasedSchema != null) {
            if (ModelUtils.isDecimalSchema(unaliasedSchema)) { // type: string, format: number
                cm.isString = false;
                cm.isDecimal = true;
            }
        }

        if (cm.isNullable) {
            cm.setIsNull(true);
            cm.isNullable = false;
            cm.setHasMultipleTypes(true);
        }
        // TODO improve this imports addition code
        if (cm.isArray && cm.getItems() != null && cm.getItems().complexType != null) {
            cm.imports.add(cm.getItems().complexType);
        }
        if (cm.isArray && cm.getItems() != null && cm.getItems().mostInnerItems != null && cm.getItems().mostInnerItems.complexType != null) {
            cm.imports.add(cm.getItems().mostInnerItems.complexType);
        }
        Boolean isNotPythonModelSimpleModel = (ModelUtils.isComposedSchema(sc) || ModelUtils.isObjectSchema(sc) || ModelUtils.isMapSchema(sc));
        setAdditionalPropsAndItemsVarNames(cm);
        if (isNotPythonModelSimpleModel) {
            return cm;
        }
        String defaultValue = toDefaultValue(sc);
        if (sc.getDefault() != null) {
            cm.defaultValue = defaultValue;
        }
        return cm;
    }

    /**
     * Returns the python type for the property.
     *
     * @param schema property schema
     * @return string presentation of the type
     **/
    @SuppressWarnings("static-method")
    @Override
    public String getSchemaType(Schema schema) {
        String openAPIType = getSingleSchemaType(schema);
        if (typeMapping.containsKey(openAPIType)) {
            String type = typeMapping.get(openAPIType);
            return type;
        }
        return toModelName(openAPIType);
    }

    public String getModelName(Schema sc) {
        if (sc.get$ref() != null) {
            Schema unaliasedSchema = unaliasSchema(sc, importMapping);
            if (unaliasedSchema.get$ref() != null) {
                return toModelName(ModelUtils.getSimpleRef(sc.get$ref()));
            }
        }
        return null;
    }

    /**
     * Return a string representation of the Python types for the specified OAS schema.
     * Primitive types in the OAS specification are implemented in Python using the corresponding
     * Python primitive types.
     * Composed types (e.g. allAll, oneOf, anyOf) are represented in Python using list of types.
     * <p>
     * The caller should set the prefix and suffix arguments to empty string, except when
     * getTypeString invokes itself recursively. A non-empty prefix/suffix may be specified
     * to wrap the return value in a python dict, list or tuple.
     * <p>
     * Examples:
     * - "bool, date, float"  The data must be a bool, date or float.
     * - "[bool, date]"       The data must be an array, and the array items must be a bool or date.
     *
     * @param p                    The OAS schema.
     * @param prefix               prepended to the returned value.
     * @param suffix               appended to the returned value.
     * @param referencedModelNames a list of models that are being referenced while generating the types,
     *                             may be used to generate imports.
     * @return a comma-separated string representation of the Python types
     */
    private String getTypeString(Schema p, String prefix, String suffix, List<String> referencedModelNames) {
        String fullSuffix = suffix;
        if (")".equals(suffix)) {
            fullSuffix = "," + suffix;
        }
        if (StringUtils.isNotEmpty(p.get$ref())) {
            // The input schema is a reference. If the resolved schema is
            // a composed schema, convert the name to a Python class.
            Schema unaliasedSchema = unaliasSchema(p, importMapping);
            if (unaliasedSchema.get$ref() != null) {
                String modelName = toModelName(ModelUtils.getSimpleRef(p.get$ref()));
                if (referencedModelNames != null) {
                    referencedModelNames.add(modelName);
                }
                return prefix + modelName + fullSuffix;
            }
        }
        if (isAnyTypeSchema(p)) {
            return prefix + "bool, date, datetime, dict, float, int, list, str, none_type" + suffix;
        }
        // Resolve $ref because ModelUtils.isXYZ methods do not automatically resolve references.
        if (ModelUtils.isNullable(ModelUtils.getReferencedSchema(this.openAPI, p))) {
            fullSuffix = ", none_type" + suffix;
        }
        if (isFreeFormObject(p) && getAdditionalProperties(p) == null) {
            return prefix + "bool, date, datetime, dict, float, int, list, str" + fullSuffix;
        } else if (ModelUtils.isNumberSchema(p)) {
            return prefix + "int, float" + fullSuffix;
        } else if ((ModelUtils.isMapSchema(p) || "object".equals(p.getType())) && getAdditionalProperties(p) != null) {
            Schema inner = getAdditionalProperties(p);
            return prefix + "{str: " + getTypeString(inner, "(", ")", referencedModelNames) + "}" + fullSuffix;
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                // In OAS 3.0.x, the array "items" attribute is required.
                // In OAS >= 3.1, the array "items" attribute is optional such that the OAS
                // specification is aligned with the JSON schema specification.
                // When "items" is not specified, the elements of the array may be anything at all.
                // In that case, the return value should be:
                //    "[bool, date, datetime, dict, float, int, list, str, none_type]"
                // Using recursion to wrap the allowed python types in an array.
                Schema anyType = new Schema(); // A Schema without any attribute represents 'any type'.
                return getTypeString(anyType, "[", "]", referencedModelNames);
            } else {
                return prefix + getTypeString(inner, "[", "]", referencedModelNames) + fullSuffix;
            }
        }
        if (ModelUtils.isFileSchema(p)) {
            return prefix + "file_type" + fullSuffix;
        }
        String baseType = getSchemaType(p);
        return prefix + baseType + fullSuffix;
    }

    /**
     * Output the type declaration of a given name
     *
     * @param p property schema
     * @return a string presentation of the type
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        // this is used to set dataType, which defines a python tuple of classes
        // in Python we will wrap this in () to make it a tuple but here we
        // will omit the parens so the generated documentaion will not include
        // them
        return getTypeString(p, "", "", null);
    }

    @Override
    public String toInstantiationType(Schema property) {
        if (ModelUtils.isArraySchema(property) || ModelUtils.isMapSchema(property) || property.getAdditionalProperties() != null) {
            return getSchemaType(property);
        }
        return super.toInstantiationType(property);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        Schema addProps = getAdditionalProperties(schema);
        if (addProps != null) {
            // if AdditionalProperties exists, get its datatype and
            // store it in codegenModel.additionalPropertiesType.
            // The 'addProps' may be a reference, getTypeDeclaration will resolve
            // the reference.
            List<String> referencedModelNames = new ArrayList<String>();
            codegenModel.additionalPropertiesType = getTypeString(addProps, "", "", referencedModelNames);
            if (referencedModelNames.size() != 0) {
                // Models that are referenced in the 'additionalPropertiesType' keyword
                // must be added to the imports.
                codegenModel.imports.addAll(referencedModelNames);
            }
        }
        // If addProps is null, the value of the 'additionalProperties' keyword is set
        // to false, i.e. no additional properties are allowed.
    }

    /**
     * Gets an example if it exists
     *
     * @param sc input schema
     * @return the example value
     */
    protected Object getObjectExample(Schema sc) {
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        // TODO handle examples in object models in the future
        Boolean objectModel = (ModelUtils.isObjectSchema(schema) || ModelUtils.isMapSchema(schema) || ModelUtils.isComposedSchema(schema));
        if (objectModel) {
            return null;
        }
        if (schema.getExample() != null) {
            return schema.getExample();
        }
        if (schema.getDefault() != null) {
            return schema.getDefault();
        } else if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
            return schema.getEnum().get(0);
        }
        return null;
    }

    /***
     * Ensures that the string has a leading and trailing quote
     *
     * @param in input string
     * @return quoted string
     */
    private String ensureQuotes(String in) {
        Pattern pattern = Pattern.compile("\r\n|\r|\n");
        Matcher matcher = pattern.matcher(in);
        if (matcher.find()) {
            // if a string has a new line in it add triple quotes to make it a python multiline string
            return "'''" + in + "'''";
        }
        String strPattern = "^['\"].*?['\"]$";
        if (in.matches(strPattern)) {
            return in;
        }
        return "\"" + in + "\"";
    }

    public String toExampleValue(Schema schema, Object objExample) {
        String modelName = getModelName(schema);
        return toExampleValueRecursive(modelName, schema, objExample, 1, "", 0, new ArrayList<>());
    }

    private Boolean simpleStringSchema(Schema schema) {
        Schema sc = schema;
        String ref = schema.get$ref();
        if (ref != null) {
            sc = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        if (ModelUtils.isStringSchema(sc) && !ModelUtils.isDateSchema(sc) && !ModelUtils.isDateTimeSchema(sc) && !"Number".equalsIgnoreCase(sc.getFormat()) && !ModelUtils.isByteArraySchema(sc) && !ModelUtils.isBinarySchema(sc) && schema.getPattern() == null) {
            return true;
        }
        return false;
    }

    private MappedModel getDiscriminatorMappedModel(CodegenDiscriminator disc) {
        for (MappedModel mm : disc.getMappedModels()) {
            String modelName = mm.getModelName();
            Schema modelSchema = getModelNameToSchemaCache().get(modelName);
            if (ModelUtils.isObjectSchema(modelSchema)) {
                return mm;
            }
        }
        return null;
    }

    /***
     * Recursively generates string examples for schemas
     *
     * @param modelName the string name of the refed model that will be generated for the schema or null
     * @param schema the schema that we need an example for
     * @param objExample the example that applies to this schema, for now only string example are used
     * @param indentationLevel integer indentation level that we are currently at
     *                         we assume the indentaion amount is 4 spaces times this integer
     * @param prefix the string prefix that we will use when assigning an example for this line
     *               this is used when setting key: value, pairs "key: " is the prefix
     *               and this is used when setting properties like some_property='some_property_example'
     * @param exampleLine this is the current line that we are generatign an example for, starts at 0
     *                    we don't indentin the 0th line because using the example value looks like:
     *                    prop = ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     *                    and our example value is:
     *                    ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     * @param includedSchemas are a list of schemas that we have moved through to get here. If the new schemas that we
     *                        are looking at is in includedSchemas then we have hit a cycle.
     * @return the string example
     */
    private String toExampleValueRecursive(String modelName, Schema schema, Object objExample, int indentationLevel, String prefix, Integer exampleLine, List<Schema> includedSchemas) {
        boolean couldHaveCycle = includedSchemas.size() > 0 && potentiallySelfReferencingSchema(schema);
        // If we have seen the ContextAwareSchemaNode more than once before, we must be in a cycle.
        boolean cycleFound = false;
        if (couldHaveCycle) {
            cycleFound = includedSchemas.subList(0, includedSchemas.size()-1).stream().anyMatch(s -> schema.equals(s));
        }
        final String indentionConst = "    ";
        String currentIndentation = "";
        String closingIndentation = "";
        for (int i = 0; i < indentationLevel; i++) currentIndentation += indentionConst;
        if (exampleLine.equals(0)) {
            closingIndentation = currentIndentation;
            currentIndentation = "";
        } else {
            closingIndentation = currentIndentation;
        }
        String openChars = "";
        String closeChars = "";
        if (modelName != null) {
            openChars = modelName + "(";
            closeChars = ")";
        }

        String fullPrefix = currentIndentation + prefix + openChars;

        String example = null;
        if (objExample != null) {
            example = objExample.toString();
        }
        if (null != schema.get$ref()) {
            Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
            String ref = ModelUtils.getSimpleRef(schema.get$ref());
            Schema refSchema = allDefinitions.get(ref);
            if (null == refSchema) {
                LOGGER.warn("Unable to find referenced schema " + schema.get$ref() + "\n");
                return fullPrefix + "None" + closeChars;
            }
            String refModelName = getModelName(schema);
            return toExampleValueRecursive(refModelName, refSchema, objExample, indentationLevel, prefix, exampleLine, includedSchemas);
        } else if (ModelUtils.isNullType(schema) || isAnyTypeSchema(schema)) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return fullPrefix + "None" + closeChars;
        } else if (ModelUtils.isBooleanSchema(schema)) {
            if (objExample == null) {
                example = "True";
            } else {
                if ("false".equalsIgnoreCase(objExample.toString())) {
                    example = "False";
                } else {
                    example = "True";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateSchema(schema)) {
            if (objExample == null) {
                example = pythonDate("1970-01-01");
            } else {
                example = pythonDate(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            if (objExample == null) {
                example = pythonDateTime("1970-01-01T00:00:00.00Z");
            } else {
                example = pythonDateTime(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isBinarySchema(schema)) {
            if (objExample == null) {
                example = "/path/to/file";
            }
            example = "open('" + example + "', 'rb')";
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isByteArraySchema(schema)) {
            if (objExample == null) {
                example = "'YQ=='";
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isStringSchema(schema)) {
            if (objExample == null) {
                // a BigDecimal:
                if ("Number".equalsIgnoreCase(schema.getFormat())) {
                    example = "2";
                    return fullPrefix + example + closeChars;
                } else if (StringUtils.isNotBlank(schema.getPattern())) {
                    String pattern = schema.getPattern();
                    RgxGen rgxGen = new RgxGen(pattern);
                    // this seed makes it so if we have [a-z] we pick a
                    Random random = new Random(18);
                    String sample = rgxGen.generate(random);
                    // omit leading / and trailing /, omit trailing /i
                    Pattern valueExtractor = Pattern.compile("^/?(.+?)/?.?$");
                    Matcher m = valueExtractor.matcher(sample);
                    if (m.find()) {
                        example = m.group(m.groupCount());
                    } else {
                        example = "";
                    }
                } else if (schema.getMinLength() != null) {
                    example = "";
                    int len = schema.getMinLength().intValue();
                    for (int i = 0; i < len; i++) example += "a";
                } else if (ModelUtils.isUUIDSchema(schema)) {
                    example = "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
                } else {
                    example = "string_example";
                }
            }
            return fullPrefix + ensureQuotes(example) + closeChars;
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "1";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "3.14";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isArraySchema(schema)) {
            if (objExample instanceof Iterable) {
                // If the example is already a list, return it directly instead of wrongly wrap it in another list
                return fullPrefix + objExample.toString();
            }
            ArraySchema arrayschema = (ArraySchema) schema;
            Schema itemSchema = arrayschema.getItems();
            String itemModelName = getModelName(itemSchema);
            includedSchemas.add(schema);
            String itemExample = toExampleValueRecursive(itemModelName, itemSchema, objExample, indentationLevel + 1, "", exampleLine + 1, includedSchemas);
            if (StringUtils.isEmpty(itemExample) || cycleFound) {
                return fullPrefix + "[]";
            } else {
                return fullPrefix + "[" + "\n" + itemExample + "\n" + closingIndentation + "]" + closeChars;
            }
        } else if (ModelUtils.isMapSchema(schema)) {
            if (modelName == null) {
                fullPrefix += "dict(";
                closeChars = ")";
            }
            Object addPropsObj = schema.getAdditionalProperties();
            // TODO handle true case for additionalProperties
            if (addPropsObj instanceof Schema && !cycleFound) {
                Schema addPropsSchema = (Schema) addPropsObj;
                String key = "key";
                Object addPropsExample = getObjectExample(addPropsSchema);
                if (addPropsSchema.getEnum() != null && !addPropsSchema.getEnum().isEmpty()) {
                    key = addPropsSchema.getEnum().get(0).toString();
                }
                addPropsExample = exampleFromStringOrArraySchema(addPropsSchema, addPropsExample, key);
                String addPropPrefix = key + "=";
                if (modelName == null) {
                    addPropPrefix = ensureQuotes(key) + ": ";
                }
                String addPropsModelName = getModelName(addPropsSchema);
                includedSchemas.add(schema);
                example = fullPrefix + "\n" + toExampleValueRecursive(addPropsModelName, addPropsSchema, addPropsExample, indentationLevel + 1, addPropPrefix, exampleLine + 1, includedSchemas) + ",\n" + closingIndentation + closeChars;
            } else {
                example = fullPrefix + closeChars;
            }
            return example;
        } else if (ModelUtils.isObjectSchema(schema)) {
            if (modelName == null) {
                fullPrefix += "dict(";
                closeChars = ")";
            }
            if (cycleFound) {
                return fullPrefix + closeChars;
            }
            CodegenDiscriminator disc = createDiscriminator(modelName, schema, openAPI);
            if (disc != null) {
                MappedModel mm = getDiscriminatorMappedModel(disc);
                if (mm != null) {
                    String discPropNameValue = mm.getMappingName();
                    String chosenModelName = mm.getModelName();
                    // TODO handle this case in the future, this is when the discriminated
                    // schema allOf includes this schema, like Cat allOf includes Pet
                    // so this is the composed schema use case
                } else {
                    return fullPrefix + closeChars;
                }
            }
            return exampleForObjectModel(schema, fullPrefix, closeChars, null, indentationLevel, exampleLine, closingIndentation, includedSchemas);
        } else if (ModelUtils.isComposedSchema(schema)) {
            if (cycleFound) {
                return fullPrefix + closeChars;
            }
            // TODO add examples for composed schema models without discriminators

            CodegenDiscriminator disc = createDiscriminator(modelName, schema, openAPI);
            if (disc != null) {
                MappedModel mm = getDiscriminatorMappedModel(disc);
                if (mm != null) {
                    String discPropNameValue = mm.getMappingName();
                    String chosenModelName = mm.getModelName();
                    Schema modelSchema = getModelNameToSchemaCache().get(chosenModelName);
                    CodegenProperty cp = new CodegenProperty();
                    cp.setName(disc.getPropertyName());
                    cp.setExample(discPropNameValue);
                    return exampleForObjectModel(modelSchema, fullPrefix, closeChars, cp, indentationLevel, exampleLine, closingIndentation, includedSchemas);
                } else {
                    return fullPrefix + closeChars;
                }
            }
            return fullPrefix + closeChars;
        } else {
            LOGGER.warn("Type " + schema.getType() + " not handled properly in toExampleValue");
        }

        return example;
    }

    private boolean potentiallySelfReferencingSchema(Schema schema) {
        return null != schema.get$ref() || ModelUtils.isArraySchema(schema) || ModelUtils.isMapSchema(schema) || ModelUtils.isObjectSchema(schema) || ModelUtils.isComposedSchema(schema);
    }

    private String exampleForObjectModel(Schema schema, String fullPrefix, String closeChars, CodegenProperty discProp, int indentationLevel, int exampleLine, String closingIndentation, List<Schema> includedSchemas) {
        Map<String, Schema> requiredAndOptionalProps = schema.getProperties();
        if (requiredAndOptionalProps == null || requiredAndOptionalProps.isEmpty()) {
            return fullPrefix + closeChars;
        }

        String example = fullPrefix + "\n";
        for (Map.Entry<String, Schema> entry : requiredAndOptionalProps.entrySet()) {
            String propName = entry.getKey();
            Schema propSchema = entry.getValue();
            propName = toVarName(propName);
            String propModelName = null;
            Object propExample = null;
            if (discProp != null && propName.equals(discProp.name)) {
                propModelName = null;
                propExample = discProp.example;
            } else {
                propModelName = getModelName(propSchema);
                propExample = exampleFromStringOrArraySchema(propSchema, null, propName);
            }
            includedSchemas.add(schema);
            example += toExampleValueRecursive(propModelName, propSchema, propExample, indentationLevel + 1, propName + "=", exampleLine + 1, includedSchemas) + ",\n";
        }
        // TODO handle additionalProperties also
        example += closingIndentation + closeChars;
        return example;
    }

    private Object exampleFromStringOrArraySchema(Schema sc, Object currentExample, String propName) {
        if (currentExample != null) {
            return currentExample;
        }
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        Object example = getObjectExample(schema);
        if (example != null) {
            return example;
        } else if (simpleStringSchema(schema)) {
            return propName + "_example";
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arraySchema = (ArraySchema) schema;
            Schema itemSchema = arraySchema.getItems();
            example = getObjectExample(itemSchema);
            if (example != null) {
                return example;
            } else if (simpleStringSchema(itemSchema)) {
                return propName + "_example";
            }
        }
        return null;
    }


    /***
     *
     * Set the codegenParameter example value
     * We have a custom version of this function so we can invoke toExampleValue
     *
     * @param codegenParameter the item we are setting the example on
     * @param parameter the base parameter that came from the spec
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        Schema schema = parameter.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because parameter lacks a schema");
            return;
        }

        Object example = null;
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            example = codegenParameter.vendorExtensions.get("x-example");
        } else if (parameter.getExample() != null) {
            example = parameter.getExample();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty() && parameter.getExamples().values().iterator().next().getValue() != null) {
            example = parameter.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, parameter.getName());
        String finalExample = toExampleValue(schema, example);
        codegenParameter.example = finalExample;
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     * @param requestBody      Request body
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, RequestBody requestBody) {
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        }

        Content content = requestBody.getContent();

        if (content.size() > 1) {
            // @see ModelUtils.getSchemaFromContent()
            once(LOGGER).warn("Multiple MediaTypes found, using only the first one");
        }

        MediaType mediaType = content.values().iterator().next();
        Schema schema = mediaType.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because requestBody content lacks a schema");
            return;
        }

        Object example = null;
        if (mediaType.getExample() != null) {
            example = mediaType.getExample();
        } else if (mediaType.getExamples() != null && !mediaType.getExamples().isEmpty() && mediaType.getExamples().values().iterator().next().getValue() != null) {
            example = mediaType.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, codegenParameter.paramName);
        codegenParameter.example = toExampleValue(schema, example);
    }

    /**
     * Create a CodegenParameter for a Form Property
     * We have a custom version of this method so we can invoke
     * setParameterExampleValue(codegenParameter, parameter)
     * rather than setParameterExampleValue(codegenParameter)
     * This ensures that all of our samples are generated in
     * toExampleValueRecursive
     *
     * @param name           the property name
     * @param propertySchema the property schema
     * @param imports        our import set
     * @return the resultant CodegenParameter
     */
    @Override
    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter cp = super.fromFormProperty(name, propertySchema, imports);
        Parameter p = new Parameter();
        p.setSchema(propertySchema);
        p.setName(cp.paramName);
        setParameterExampleValue(cp, p);
        return cp;
    }

    /**
     * Return a map from model name to Schema for efficient lookup.
     *
     * @return map from model name to Schema.
     */
    protected Map<String, Schema> getModelNameToSchemaCache() {
        if (modelNameToSchemaCache == null) {
            // Create a cache to efficiently lookup schema based on model name.
            Map<String, Schema> m = new HashMap<String, Schema>();
            ModelUtils.getSchemas(openAPI).forEach((key, schema) -> {
                m.put(toModelName(key), schema);
            });
            modelNameToSchemaCache = Collections.unmodifiableMap(m);
        }
        return modelNameToSchemaCache;
    }

    @Override
    protected void setAddProps(Schema schema, IJsonSchemaValidationProperties property){
        if (schema.equals(new Schema())) {
            // if we are trying to set additionalProperties on an empty schema stop recursing
            return;
        }
        boolean additionalPropertiesIsAnyType = false;
        CodegenModel m = null;
        if (property instanceof CodegenModel) {
            m = (CodegenModel) property;
        }
        CodegenProperty addPropProp = null;
        boolean isAdditionalPropertiesTrue = false;
        if (schema.getAdditionalProperties() == null) {
            if (!disallowAdditionalPropertiesIfNotPresent) {
                isAdditionalPropertiesTrue = true;
                // pass in the hashCode as the name to ensure that the returned property is not from the cache
                // if we need to set indent on every one, then they need to be different
                addPropProp = fromProperty(String.valueOf(property.hashCode()),  new Schema());
                addPropProp.name = "";
                addPropProp.baseName = "";
                addPropProp.nameInSnakeCase = null;
                additionalPropertiesIsAnyType = true;
            }
        } else if (schema.getAdditionalProperties() instanceof Boolean) {
            if (Boolean.TRUE.equals(schema.getAdditionalProperties())) {
                isAdditionalPropertiesTrue = true;
                addPropProp = fromProperty(String.valueOf(property.hashCode()),  new Schema());
                addPropProp.name = "";
                addPropProp.baseName = "";
                addPropProp.nameInSnakeCase = null;
                additionalPropertiesIsAnyType = true;
            }
        } else {
            addPropProp = fromProperty(String.valueOf(property.hashCode()), (Schema) schema.getAdditionalProperties());
            addPropProp.name = "";
            addPropProp.baseName = "";
            addPropProp.nameInSnakeCase = null;
            if (isAnyTypeSchema((Schema) schema.getAdditionalProperties())) {
                additionalPropertiesIsAnyType = true;
            }
        }
        if (additionalPropertiesIsAnyType) {
            property.setAdditionalPropertiesIsAnyType(true);
        }
        if (m != null && isAdditionalPropertiesTrue) {
            m.isAdditionalPropertiesTrue = true;
        }
        if (ModelUtils.isComposedSchema(schema) && !supportsAdditionalPropertiesWithComposedSchema) {
            return;
        }
        if (addPropProp != null) {
            property.setAdditionalProperties(addPropProp);
        }
    }

    /**
     * Update property for array(list) container
     *
     * @param property      Codegen property
     * @param innerProperty Codegen inner property of map or list
     */
    @Override
    protected void updatePropertyForArray(CodegenProperty property, CodegenProperty innerProperty) {
        if (innerProperty == null) {
            if(LOGGER.isWarnEnabled()) {
                LOGGER.warn("skipping invalid array property {}", Json.pretty(property));
            }
            return;
        }
        property.dataFormat = innerProperty.dataFormat;
        if (languageSpecificPrimitives.contains(innerProperty.baseType)) {
            property.isPrimitiveType = true;
        }
        property.items = innerProperty;
        property.mostInnerItems = getMostInnerItems(innerProperty);
        // inner item is Enum
        if (isPropertyInnerMostEnum(property)) {
            // isEnum is set to true when the type is an enum
            // or the inner type of an array/map is an enum
            property.isEnum = true;
            // update datatypeWithEnum and default value for array
            // e.g. List<string> => List<StatusEnum>
            updateDataTypeWithEnumForArray(property);
            // set allowable values to enum values (including array/map of enum)
            property.allowableValues = getInnerEnumAllowableValues(property);
        }

    }

    protected void updatePropertyForString(CodegenProperty property, Schema p) {
        if (ModelUtils.isByteArraySchema(p)) {
            property.isByteArray = true;
            property.setIsString(false);
        } else if (ModelUtils.isBinarySchema(p)) {
            property.isBinary = true;
            property.isFile = true; // file = binary in OAS3
        } else if (ModelUtils.isUUIDSchema(p)) {
            property.isUuid = true;
        } else if (ModelUtils.isURISchema(p)) {
            property.isUri = true;
        } else if (ModelUtils.isEmailSchema(p)) {
            property.isEmail = true;
        } else if (ModelUtils.isDateSchema(p)) { // date format
            property.setIsString(false); // for backward compatibility with 2.x
            property.isDate = true;
        } else if (ModelUtils.isDateTimeSchema(p)) { // date-time format
            property.setIsString(false); // for backward compatibility with 2.x
            property.isDateTime = true;
        } else if (ModelUtils.isDecimalSchema(p)) { // type: string, format: number
            property.isDecimal = true;
            property.setIsString(false);
        }
        property.pattern = toRegularExpression(p.getPattern());
    }

    @Override
    protected void updatePropertyForObject(CodegenProperty property, Schema p) {
        addVarsRequiredVarsAdditionalProps(p, property);
    }

    @Override
    protected void updatePropertyForAnyType(CodegenProperty property, Schema p) {
        // The 'null' value is allowed when the OAS schema is 'any type'.
        // See https://github.com/OAI/OpenAPI-Specification/issues/1389
        if (Boolean.FALSE.equals(p.getNullable())) {
            LOGGER.warn("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", p.getName());
        }
        addVarsRequiredVarsAdditionalProps(p, property);
    }

    @Override
    protected void updateModelForObject(CodegenModel m, Schema schema) {
        if (schema.getProperties() != null || schema.getRequired() != null) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        // an object or anyType composed schema that has additionalProperties set
        addAdditionPropertiesToCodeGenModel(m, schema);
        // process 'additionalProperties'
        setAddProps(schema, m);
    }

    @Override
    protected void updateModelForAnyType(CodegenModel m, Schema schema) {
        // The 'null' value is allowed when the OAS schema is 'any type'.
        // See https://github.com/OAI/OpenAPI-Specification/issues/1389
        if (Boolean.FALSE.equals(schema.getNullable())) {
            LOGGER.error("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", m.name);
        }
        // todo add items support here in the future
        if (schema.getProperties() != null || schema.getRequired() != null) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        addAdditionPropertiesToCodeGenModel(m, schema);
        // process 'additionalProperties'
        setAddProps(schema, m);
    }

    @Override
    protected void updateModelForComposedSchema(CodegenModel m, Schema schema, Map<String, Schema> allDefinitions) {
        final ComposedSchema composed = (ComposedSchema) schema;

        // TODO revise the logic below to set discriminator, xml attributes
        if (composed.getAllOf() != null) {
            int modelImplCnt = 0; // only one inline object allowed in a ComposedModel
            int modelDiscriminators = 0; // only one discriminator allowed in a ComposedModel
            for (Schema innerSchema : composed.getAllOf()) { // TODO need to work with anyOf, oneOf as well
                if (m.discriminator == null && innerSchema.getDiscriminator() != null) {
                    LOGGER.debug("discriminator is set to null (not correctly set earlier): {}", m.name);
                    m.setDiscriminator(createDiscriminator(m.name, innerSchema, this.openAPI));
                    if (!this.getLegacyDiscriminatorBehavior()) {
                        m.addDiscriminatorMappedModelsImports();
                    }
                    modelDiscriminators++;
                }

                if (innerSchema.getXml() != null) {
                    m.xmlPrefix = innerSchema.getXml().getPrefix();
                    m.xmlNamespace = innerSchema.getXml().getNamespace();
                    m.xmlName = innerSchema.getXml().getName();
                }
                if (modelDiscriminators > 1) {
                    LOGGER.error("Allof composed schema is inheriting >1 discriminator. Only use one discriminator: {}", composed);
                }

                if (modelImplCnt++ > 1) {
                    LOGGER.warn("More than one inline schema specified in allOf:. Only the first one is recognized. All others are ignored.");
                    break; // only one schema with discriminator allowed in allOf
                }
            }
        }

        CodegenComposedSchemas cs = m.getComposedSchemas();
        if (cs != null) {
            if (cs.getAllOf() != null && !cs.getAllOf().isEmpty()) {
                for (CodegenProperty cp: cs.getAllOf()) {
                    if (cp.complexType != null) {
                        addImport(m, cp.complexType);
                    }
                }
            }
            if (cs.getOneOf() != null && !cs.getOneOf().isEmpty()) {
                for (CodegenProperty cp: cs.getOneOf()) {
                    if (cp.complexType != null) {
                        addImport(m, cp.complexType);
                    }
                }
            }
            if (cs.getAnyOf() != null && !cs.getAnyOf().isEmpty()) {
                for (CodegenProperty cp: cs.getAnyOf()) {
                    if (cp.complexType != null) {
                        addImport(m, cp.complexType);
                    }
                }
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    /*
     * The OpenAPI pattern spec follows the Perl convention and style of modifiers. Python
     * does not support this in as natural a way so it needs to convert it. See
     * https://docs.python.org/2/howto/regex.html#compilation-flags for details.
     */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            int i = pattern.lastIndexOf('/');

            //Must follow Perl /pattern/modifiers convention
            if (pattern.charAt(0) != '/' || i < 2) {
                throw new IllegalArgumentException("Pattern must follow the Perl "
                        + "/pattern/modifiers convention. " + pattern + " is not valid.");
            }

            String regex = pattern.substring(1, i).replace("'", "\\'");
            List<String> modifiers = new ArrayList<String>();

            for (char c : pattern.substring(i).toCharArray()) {
                if (regexModifiers.containsKey(c)) {
                    String modifier = regexModifiers.get(c);
                    modifiers.add(modifier);
                }
            }

            vendorExtensions.put("x-regex", regex);
            vendorExtensions.put("x-modifiers", modifiers);
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath);
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
    public String addRegularExpressionDelimiter(String pattern) {
        if (StringUtils.isEmpty(pattern)) {
            return pattern;
        }

        if (!pattern.matches("^/.*")) {
            // Perform a negative lookbehind on each `/` to ensure that it is escaped.
            return "/" + pattern.replaceAll("(?<!\\\\)\\/", "\\\\/") + "/";
        }

        return pattern;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separatorChar + packagePath() + File.separatorChar +  apiPackage();
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separatorChar + packagePath() + File.separatorChar +  modelPackage();
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separatorChar + testFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separatorChar + testFolder;
    }

    public void setUseNose(String val) {
        this.useNose = Boolean.parseBoolean(val);
    }

    public void setPackageUrl(String packageUrl) {
        this.packageUrl = packageUrl;
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

    /**
     * A custom version of this method is needed to ensure that the form object parameter is kept as-is
     * as an object and is not exploded into separate parameters
     * @param body the body that is being handled
     * @param imports the imports for this body
     * @return the list of length one containing a single type object CodegenParameter
     */
    @Override
    public List<CodegenParameter> fromRequestBodyToFormParameters(RequestBody body, Set<String> imports) {
        List<CodegenParameter> parameters = new ArrayList<>();
        LOGGER.debug("debugging fromRequestBodyToFormParameters= {}", body);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        schema = ModelUtils.getReferencedSchema(this.openAPI, schema);
        CodegenParameter cp = fromFormProperty("body", schema, imports);
        cp.setContent(getContent(body.getContent(), imports, "RequestBody"));
        cp.isFormParam = false;
        cp.isBodyParam = true;
        parameters.add(cp);
        return parameters;
    }

    /**
     * Custom version of this method so we can move the body parameter into bodyParam
     *
     * @param path       the path of the operation
     * @param httpMethod HTTP method
     * @param operation  OAS operation object
     * @param servers    list of servers
     * @return the resultant CodegenOperation instance
     */
    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation co = super.fromOperation(path, httpMethod, operation, servers);
        if (co.bodyParam == null) {
            for (CodegenParameter cp: co.allParams) {
                if (cp.isBodyParam) {
                    co.bodyParam = cp;
                    co.bodyParams.add(cp);
                }
            }
        }
        return co;
    }

    @Override
    public String defaultTemplatingEngine() {
        return "handlebars";
    }

    @Override
    public String generatorLanguageVersion() { return ">=3.9"; };
}
