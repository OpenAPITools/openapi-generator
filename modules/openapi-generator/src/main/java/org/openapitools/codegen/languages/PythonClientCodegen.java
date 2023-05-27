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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.escape;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    public static final String DEFAULT_LIBRARY = "urllib3";
    public static final String RECURSION_LIMIT = "recursionLimit";
    public static final String DATETIME_FORMAT = "datetimeFormat";
    public static final String DATE_FORMAT = "dateFormat";
    public static final String MAP_NUMBER_TO = "mapNumberTo";

    protected String packageUrl;
    protected String apiDocPath = "docs" + File.separator;
    protected String modelDocPath = "docs" + File.separator;
    protected boolean hasModelsToImport = Boolean.FALSE;
    protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup
    protected String datetimeFormat = "%Y-%m-%dT%H:%M:%S.%f%z";
    protected String dateFormat = "%Y-%m-%d";
    protected String mapNumberTo = "Union[StrictFloat, StrictInt]";

    protected Map<Character, String> regexModifiers;

    private String testFolder;

    // map of set (model imports)
    private HashMap<String, HashSet<String>> circularImports = new HashMap<>();
    // map of codegen models
    private HashMap<String, CodegenModel> codegenModelMap = new HashMap<>();

    public PythonClientCodegen() {
        super();

        // force sortParamsByRequiredFlag to true to make the api method signature less complicated
        sortParamsByRequiredFlag = true;

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
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

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        // override type mapping in abstract python codegen
        typeMapping.put("array", "List");
        typeMapping.put("set", "List");
        typeMapping.put("map", "Dict");
        typeMapping.put("decimal", "decimal.Decimal");
        typeMapping.put("file", "bytearray");
        typeMapping.put("binary", "bytearray");
        typeMapping.put("ByteArray", "bytearray");

        languageSpecificPrimitives.remove("file");
        languageSpecificPrimitives.add("decimal.Decimal");
        languageSpecificPrimitives.add("bytearray");
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

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // pydantic keyword
                        "schema", "base64", "json",
                        "date",
                        // @property
                        "property",
                        // python reserved words
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try", "self", "nonlocal", "None", "True",
                        "False", "async", "await"));

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
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCECODEONLY_GENERATION, CodegenConstants.SOURCECODEONLY_GENERATION_DESC)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(RECURSION_LIMIT, "Set the recursion limit. If not set, use the system default value."));
        cliOptions.add(new CliOption(MAP_NUMBER_TO, "Map number to Union[StrictFloat, StrictInt], StrictStr or float.")
                .defaultValue("Union[StrictFloat, StrictInt]"));
        cliOptions.add(new CliOption(DATETIME_FORMAT, "datetime format for query parameters")
                .defaultValue("%Y-%m-%dT%H:%M:%S%z"));
        cliOptions.add(new CliOption(DATE_FORMAT, "date format for query parameters")
                .defaultValue("%Y-%m-%d"));

        supportedLibraries.put("urllib3", "urllib3-based client");
        supportedLibraries.put("asyncio", "asyncio-based client");
        supportedLibraries.put("tornado", "tornado-based client (deprecated)");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use: asyncio, tornado (deprecated), urllib3");
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
        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

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
            additionalProperties.put("asyncio", "true");
        } else if ("tornado".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("tornado/rest.mustache", packagePath(), "rest.py"));
            additionalProperties.put("tornado", "true");
        } else {
            supportingFiles.add(new SupportingFile("rest.mustache", packagePath(), "rest.py"));
        }

        modelPackage = this.packageName + "." + modelPackage;
        apiPackage = this.packageName + "." + apiPackage;
    }

    public void setUseOneOfDiscriminatorLookup(boolean useOneOfDiscriminatorLookup) {
        this.useOneOfDiscriminatorLookup = useOneOfDiscriminatorLookup;
    }

    public boolean getUseOneOfDiscriminatorLookup() {
        return this.useOneOfDiscriminatorLookup;
    }

    @Override
    public String toModelImport(String name) {
        String modelImport;
        if (StringUtils.startsWithAny(name, "import", "from")) {
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
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            return getSchemaType(p) + "[str, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    /*
     * Gets the pydantic type given a Codegen Parameter
     *
     * @param cp codegen parameter
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @param modelImports model imports
     * @param exampleImports example imports
     * @param classname class name
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenParameter cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports,
                                   Set<String> exampleImports,
                                   String classname) {
        if (cp == null) {
            // if codegen parameter (e.g. map/dict of undefined type) is null, default to string
            LOGGER.warn("Codegen property is null (e.g. map/dict of undefined type). Default to typing.Any.");
            typingImports.add("Any");
            return "Any";
        }

        if (cp.isArray) {
            String constraints = "";
            if (cp.maxItems != null) {
                constraints += String.format(Locale.ROOT, ", max_items=%d", cp.maxItems);
            }
            if (cp.minItems != null) {
                constraints += String.format(Locale.ROOT, ", min_items=%d", cp.minItems);
            }
            if (cp.getUniqueItems()) {
                constraints += ", unique_items=True";
            }
            pydanticImports.add("conlist");
            return String.format(Locale.ROOT, "conlist(%s%s)",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, classname),
                    constraints);
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format(Locale.ROOT, "Dict[str, %s]",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, classname));
        } else if (cp.isString) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. constr(regex=r'/[a-z]/i', strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format(Locale.ROOT, "constr(%s)", StringUtils.join(fieldCustomization, ", "));
            } else {
                if ("password".equals(cp.getFormat())) { // TDOO avoid using format, use `is` boolean flag instead
                    pydanticImports.add("SecretStr");
                    return "SecretStr";
                } else {
                    pydanticImports.add("StrictStr");
                    return "StrictStr";
                }
            }
        } else if (cp.isNumber || cp.isFloat || cp.isDouble) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                List<String> intFieldCustomization = new ArrayList<>();

                // e.g. confloat(ge=10, le=100, strict=True)
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                        intFieldCustomization.add("lt=" + Math.ceil(Double.valueOf(cp.getMaximum()))); // e.g. < 7.59 becomes < 8
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                        intFieldCustomization.add("le=" + Math.floor(Double.valueOf(cp.getMaximum()))); // e.g. <= 7.59 becomes <= 7
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                        intFieldCustomization.add("gt=" + Math.floor(Double.valueOf(cp.getMinimum()))); // e.g. > 7.59 becomes > 7
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                        intFieldCustomization.add("ge=" + Math.ceil(Double.valueOf(cp.getMinimum()))); // e.g. >= 7.59 becomes >= 8
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    intFieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    pydanticImports.add("conint");
                    typingImports.add("Union");
                    return String.format(Locale.ROOT, "Union[%s(%s), %s(%s)]", "confloat",
                            StringUtils.join(fieldCustomization, ", "),
                            "conint",
                            StringUtils.join(intFieldCustomization, ", ")
                    );
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                } else { // float
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                }
            } else {
                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    typingImports.add("Union");
                    pydanticImports.add("StrictFloat");
                    pydanticImports.add("StrictInt");
                    return "Union[StrictFloat, StrictInt]";
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    pydanticImports.add("StrictFloat");
                    return "StrictFloat";
                } else {
                    return "float";
                }
            }
        } else if (cp.isInteger || cp.isLong || cp.isShort || cp.isUnboundedInteger) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conint(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                pydanticImports.add("conint");
                return String.format(Locale.ROOT, "%s(%s)", "conint",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictInt");
                return "StrictInt";
            }
        } else if (cp.isBinary || cp.isByteArray) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conbytes(min_length=2, max_length=10)
                fieldCustomization.add("strict=True");
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }

                pydanticImports.add("conbytes");
                pydanticImports.add("constr");
                typingImports.add("Union");
                return String.format(Locale.ROOT, "Union[conbytes(%s), constr(%<s)]", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                pydanticImports.add("StrictStr");
                typingImports.add("Union");
                return "Union[StrictBytes, StrictStr]";
            }
        } else if (cp.isBoolean) {
            pydanticImports.add("StrictBool");
            return "StrictBool";
        } else if (cp.isDecimal) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. condecimal(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("gt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("lt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }
                pydanticImports.add("condecimal");
                return String.format(Locale.ROOT, "%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal()";
            }
        } else if (cp.getIsAnyType()) {
            typingImports.add("Any");
            return "Any";
        } else if (cp.isDate || cp.isDateTime) {
            if (cp.isDate) {
                datetimeImports.add("date");
            }
            if (cp.isDateTime) {
                datetimeImports.add("datetime");
            }

            return cp.dataType;
        } else if (cp.isUuid) {
            return cp.dataType;
        } else if (cp.isFreeFormObject) { // type: object
            typingImports.add("Dict");
            typingImports.add("Any");
            return "Dict[str, Any]";
        } else if (!cp.isPrimitiveType) {
            // add model prefix
            hasModelsToImport = true;
            modelImports.add(cp.dataType);
            exampleImports.add(cp.dataType);
            return cp.dataType;
        } else if (cp.getContent() != null) {
            LinkedHashMap<String, CodegenMediaType> contents = cp.getContent();
            for (String key : contents.keySet()) {
                CodegenMediaType cmt = contents.get(key);
                // TODO process the first one only at the moment
                if (cmt != null)
                    return getPydanticType(cmt.getSchema(), typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, classname);
            }
            throw new RuntimeException("Error! Failed to process getPydanticType when getting the content: " + cp);
        } else {
            throw new RuntimeException("Error! Codegen Parameter not yet supported in getPydanticType: " + cp);
        }
    }

    /*
     * Gets the pydantic type given a Codegen Property
     *
     * @param cp codegen property
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @param modelImports model imports
     * @param exampleImports example imports
     * @param classname class name
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenProperty cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports,
                                   Set<String> exampleImports,
                                   String classname) {
        if (cp == null) {
            // if codegen property (e.g. map/dict of undefined type) is null, default to string
            LOGGER.warn("Codegen property is null (e.g. map/dict of undefined type). Default to typing.Any.");
            typingImports.add("Any");
            return "Any";
        }

        if (cp.isEnum) {
            pydanticImports.add("validator");
        }

        /* comment out the following since Literal requires python 3.8
           also need to put cp.isEnum check after isArray, isMap check
        if (cp.isEnum) {
            // use Literal for inline enum
            typingImports.add("Literal");
            List<String> values = new ArrayList<>();
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) cp.allowableValues.get("enumVars");
            if (enumVars != null) {
                for (Map<String, Object> enumVar : enumVars) {
                    values.add((String) enumVar.get("value"));
                }
            }
            return String.format(Locale.ROOT, "%sEnum", cp.nameInCamelCase);
        } else*/
        if (cp.isArray) {
            String constraints = "";
            if (cp.maxItems != null) {
                constraints += String.format(Locale.ROOT, ", max_items=%d", cp.maxItems);
            }
            if (cp.minItems != null) {
                constraints += String.format(Locale.ROOT, ", min_items=%d", cp.minItems);
            }
            if (cp.getUniqueItems()) {
                constraints += ", unique_items=True";
            }
            pydanticImports.add("conlist");
            typingImports.add("List"); // for return type
            return String.format(Locale.ROOT, "conlist(%s%s)",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, classname),
                    constraints);
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format(Locale.ROOT, "Dict[str, %s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, classname));
        } else if (cp.isString) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. constr(regex=r'/[a-z]/i', strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format(Locale.ROOT, "constr(%s)", StringUtils.join(fieldCustomization, ", "));
            } else {
                if ("password".equals(cp.getFormat())) { // TDOO avoid using format, use `is` boolean flag instead
                    pydanticImports.add("SecretStr");
                    return "SecretStr";
                } else {
                    pydanticImports.add("StrictStr");
                    return "StrictStr";
                }
            }
        } else if (cp.isNumber || cp.isFloat || cp.isDouble) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                List<String> intFieldCustomization = new ArrayList<>();

                // e.g. confloat(ge=10, le=100, strict=True)
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                        intFieldCustomization.add("lt=" + (int) Math.ceil(Double.valueOf(cp.getMaximum()))); // e.g. < 7.59 => < 8
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                        intFieldCustomization.add("le=" + (int) Math.floor(Double.valueOf(cp.getMaximum()))); // e.g. <= 7.59 => <= 7
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                        intFieldCustomization.add("gt=" + (int) Math.floor(Double.valueOf(cp.getMinimum()))); // e.g. > 7.59 => > 7
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                        intFieldCustomization.add("ge=" + (int) Math.ceil(Double.valueOf(cp.getMinimum()))); // e.g. >= 7.59 => >= 8
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    intFieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    pydanticImports.add("conint");
                    typingImports.add("Union");
                    return String.format(Locale.ROOT, "Union[%s(%s), %s(%s)]", "confloat",
                            StringUtils.join(fieldCustomization, ", "),
                            "conint",
                            StringUtils.join(intFieldCustomization, ", ")
                    );
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                } else { // float
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                }
            } else {
                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    typingImports.add("Union");
                    pydanticImports.add("StrictFloat");
                    pydanticImports.add("StrictInt");
                    return "Union[StrictFloat, StrictInt]";
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    pydanticImports.add("StrictFloat");
                    return "StrictFloat";
                } else {
                    return "float";
                }
            }
        } else if (cp.isInteger || cp.isLong || cp.isShort || cp.isUnboundedInteger) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conint(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                pydanticImports.add("conint");
                return String.format(Locale.ROOT, "%s(%s)", "conint",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictInt");
                return "StrictInt";
            }
        } else if (cp.isBinary || cp.isByteArray) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conbytes(min_length=2, max_length=10)
                fieldCustomization.add("strict=True");
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }

                pydanticImports.add("conbytes");
                pydanticImports.add("constr");
                typingImports.add("Union");
                return String.format(Locale.ROOT, "Union[conbytes(%s), constr(%<s)]", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                pydanticImports.add("StrictStr");
                typingImports.add("Union");
                return "Union[StrictBytes, StrictStr]";
            }
        } else if (cp.isBoolean) {
            pydanticImports.add("StrictBool");
            return "StrictBool";
        } else if (cp.isDecimal) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. condecimal(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("gt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("lt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }
                pydanticImports.add("condecimal");
                return String.format(Locale.ROOT, "%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal()";
            }
        } else if (cp.getIsAnyType()) {
            typingImports.add("Any");
            return "Any";
        } else if (cp.isDate || cp.isDateTime) {
            if (cp.isDate) {
                datetimeImports.add("date");
            }
            if (cp.isDateTime) {
                datetimeImports.add("datetime");
            }
            return cp.dataType;
        } else if (cp.isUuid) {
            return cp.dataType;
        } else if (cp.isFreeFormObject) { // type: object
            typingImports.add("Dict");
            typingImports.add("Any");
            return "Dict[str, Any]";
        } else if (!cp.isPrimitiveType || cp.isModel) { // model
            // skip import if it's a circular reference
            if (classname == null) {
                // for parameter model, import directly
                hasModelsToImport = true;
                modelImports.add(cp.dataType);
                exampleImports.add(cp.dataType);
            } else {
                if (circularImports.containsKey(cp.dataType)) {
                    if (circularImports.get(cp.dataType).contains(classname)) {
                        // cp.dataType import map of set contains this model (classname), don't import
                        LOGGER.debug("Skipped importing {} in {} due to circular import.", cp.dataType, classname);
                    } else {
                        // not circular import, so ok to import it
                        hasModelsToImport = true;
                        modelImports.add(cp.dataType);
                        exampleImports.add(cp.dataType);
                    }
                } else {
                    LOGGER.error("Failed to look up {} from the imports (map of set) of models.", cp.dataType);
                }
            }
            return cp.dataType;
        } else {
            throw new RuntimeException("Error! Codegen Property not yet supported in getPydanticType: " + cp);
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        hasModelsToImport = false;
        TreeSet<String> typingImports = new TreeSet<>();
        TreeSet<String> pydanticImports = new TreeSet<>();
        TreeSet<String> datetimeImports = new TreeSet<>();
        TreeSet<String> modelImports = new TreeSet<>();

        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();
        for (CodegenOperation operation : operations) {
            TreeSet<String> exampleImports = new TreeSet<>(); // import for each operation to be show in sample code
            List<CodegenParameter> params = operation.allParams;

            for (CodegenParameter param : params) {
                String typing = getPydanticType(param, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, null);
                List<String> fields = new ArrayList<>();
                String firstField = "";

                if (!param.required) { //optional
                    firstField = "None";
                    typing = "Optional[" + typing + "]";
                    typingImports.add("Optional");
                } else { // required
                    firstField = "...";
                    if (param.isNullable) {
                        typing = "Optional[" + typing + "]";
                        typingImports.add("Optional");
                    }
                }

                if (!StringUtils.isEmpty(param.description)) { // has description
                    fields.add(String.format(Locale.ROOT, "description=\"%s\"", param.description));
                }

                /* TODO support example
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format(Locale.ROOT, "example=%s", cp.getExample()));
                }*/

                String fieldCustomization;
                if ("None".equals(firstField)) {
                    fieldCustomization = null;
                } else { // required field
                    fieldCustomization = firstField;
                }

                if (!fields.isEmpty()) {
                    if (fieldCustomization != null) {
                        fields.add(0, fieldCustomization);
                    }
                    pydanticImports.add("Field");
                    fieldCustomization = String.format(Locale.ROOT, "Field(%s)", StringUtils.join(fields, ", "));
                } else {
                    fieldCustomization = "Field()";
                }

                if ("Field()".equals(fieldCustomization)) {
                    param.vendorExtensions.put("x-py-typing", typing);
                } else {
                    param.vendorExtensions.put("x-py-typing", String.format(Locale.ROOT, "Annotated[%s, %s]", typing, fieldCustomization));
                }
            }

            // update typing import for operation return type
            if (!StringUtils.isEmpty(operation.returnType)) {
                String typing = getPydanticType(operation.returnProperty, typingImports,
                        new TreeSet<>() /* skip pydantic import for return type */, datetimeImports, modelImports, exampleImports, null);
            }

            // add import for code samples
            // import models one by one
            if (!exampleImports.isEmpty()) {
                List<String> imports = new ArrayList<>();
                for (String exampleImport : exampleImports) {
                    imports.add("from " + packageName + ".models." + underscore(exampleImport) + " import " + exampleImport);
                }
                operation.vendorExtensions.put("x-py-example-import", imports);
            }
        }

        List<Map<String, String>> newImports = new ArrayList<>();

        // need datetime import
        if (!datetimeImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from datetime import %s\n", StringUtils.join(datetimeImports, ", ")));
            newImports.add(item);
        }

        // need pydantic imports
        if (!pydanticImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from pydantic import %s\n", StringUtils.join(pydanticImports, ", ")));
            newImports.add(item);
        }

        // need typing imports
        if (!typingImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from typing import %s\n", StringUtils.join(typingImports, ", ")));
            newImports.add(item);
        }

        // import models one by one
        if (!modelImports.isEmpty()) {
            for (String modelImport : modelImports) {
                Map<String, String> item = new HashMap<>();
                item.put("import", "from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                newImports.add(item);
            }
        }

        // reset imports with newImports
        objs.setImports(newImports);
        return objs;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        for (Map.Entry<String, ModelsMap> entry : objs.entrySet()) {
            // create hash map of codegen model
            CodegenModel cm = ModelUtils.getModelByName(entry.getKey(), objs);
            codegenModelMap.put(cm.classname, ModelUtils.getModelByName(entry.getKey(), objs));
        }

        // create circular import
        for (String m : codegenModelMap.keySet()) {
            createImportMapOfSet(m, codegenModelMap);
        }

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            entry.setValue(postProcessModelsMap(entry.getValue()));
        }

        return processed;
    }

    /**
     * Update circularImports with the model name (key) and its imports gathered recursively
     *
     * @param modelName       model name
     * @param codegenModelMap a map of CodegenModel
     */
    void createImportMapOfSet(String modelName, Map<String, CodegenModel> codegenModelMap) {
        HashSet<String> imports = new HashSet<>();
        circularImports.put(modelName, imports);

        CodegenModel cm = codegenModelMap.get(modelName);

        if (cm == null) {
            LOGGER.warn("Failed to lookup model in createImportMapOfSet: " + modelName);
            return;
        }

        List<CodegenProperty> codegenProperties = null;
        if (cm.oneOf != null && !cm.oneOf.isEmpty()) { // oneOf
            codegenProperties = cm.getComposedSchemas().getOneOf();
        } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) { // anyOF
            codegenProperties = cm.getComposedSchemas().getAnyOf();
        } else { // typical model
            codegenProperties = cm.vars;
        }

        for (CodegenProperty cp : codegenProperties) {
            String modelNameFromDataType = getModelNameFromDataType(cp);
            if (modelNameFromDataType != null) { // model
                imports.add(modelNameFromDataType); // update import
                // go through properties or sub-schemas of the model recursively to identify more (model) import if any
                updateImportsFromCodegenModel(modelNameFromDataType, codegenModelMap.get(modelNameFromDataType), imports);
            }
        }
    }

    /**
     * Update set of imports from codegen model recursivly
     *
     * @param modelName model name
     * @param cm        codegen model
     * @param imports   set of imports
     */
    public void updateImportsFromCodegenModel(String modelName, CodegenModel cm, Set<String> imports) {
        if (cm == null) {
            LOGGER.warn("Failed to lookup model in createImportMapOfSet " + modelName);
            return;
        }

        List<CodegenProperty> codegenProperties = null;
        if (cm.oneOf != null && !cm.oneOf.isEmpty()) { // oneOfValidationError
            codegenProperties = cm.getComposedSchemas().getOneOf();
        } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) { // anyOF
            codegenProperties = cm.getComposedSchemas().getAnyOf();
        } else { // typical model
            codegenProperties = cm.vars;
        }

        for (CodegenProperty cp : codegenProperties) {
            String modelNameFromDataType = getModelNameFromDataType(cp);
            if (modelNameFromDataType != null) { // model
                if (modelName.equals(modelNameFromDataType)) { // self referencing
                    continue;
                } else if (imports.contains(modelNameFromDataType)) { // circular import
                    continue;
                } else {
                    imports.add(modelNameFromDataType); // update import
                    // go through properties of the model recursively to identify more (model) import if any
                    updateImportsFromCodegenModel(modelNameFromDataType, codegenModelMap.get(modelNameFromDataType), imports);
                }
            }
        }
    }

    /**
     * Returns the model name (if any) from data type of codegen property.
     * Returns null if it's not a model.
     *
     * @param cp Codegen property
     * @return model name
     */
    private String getModelNameFromDataType(CodegenProperty cp) {
        if (cp.isArray) {
            return getModelNameFromDataType(cp.items);
        } else if (cp.isMap) {
            return getModelNameFromDataType(cp.items);
        } else if (!cp.isPrimitiveType || cp.isModel) {
            return cp.dataType;
        } else {
            return null;
        }
    }

    private ModelsMap postProcessModelsMap(ModelsMap objs) {
        // process enum in models
        objs = postProcessModelsEnum(objs);

        TreeSet<String> typingImports = new TreeSet<>();
        TreeSet<String> pydanticImports = new TreeSet<>();
        TreeSet<String> datetimeImports = new TreeSet<>();
        TreeSet<String> modelImports = new TreeSet<>();

        for (ModelMap m : objs.getModels()) {
            TreeSet<String> exampleImports = new TreeSet<>();
            List<String> readOnlyFields = new ArrayList<>();
            hasModelsToImport = false;
            int property_count = 1;
            typingImports.clear();
            pydanticImports.clear();
            datetimeImports.clear();

            CodegenModel model = m.getModel();

            // handle null type in oneOf
            if (model.getComposedSchemas() != null && model.getComposedSchemas().getOneOf() != null
                    && !model.getComposedSchemas().getOneOf().isEmpty()) {
                int index = 0;
                List<CodegenProperty> oneOfs = model.getComposedSchemas().getOneOf();
                for (CodegenProperty oneOf : oneOfs) {
                    if ("none_type".equals(oneOf.dataType)) {
                        oneOfs.remove(index);
                        break; // return earlier assuming there's only 1 null type defined
                    }
                    index++;
                }
            }

            List<CodegenProperty> codegenProperties = null;
            if (!model.oneOf.isEmpty()) { // oneOfValidationError
                codegenProperties = model.getComposedSchemas().getOneOf();
                typingImports.add("Any");
                typingImports.add("List");
                pydanticImports.add("Field");
                pydanticImports.add("StrictStr");
                pydanticImports.add("ValidationError");
                pydanticImports.add("validator");
            } else if (!model.anyOf.isEmpty()) { // anyOF
                codegenProperties = model.getComposedSchemas().getAnyOf();
                pydanticImports.add("Field");
                pydanticImports.add("StrictStr");
                pydanticImports.add("ValidationError");
                pydanticImports.add("validator");
            } else { // typical model
                codegenProperties = model.vars;
            }

            //loop through properties/schemas to set up typing, pydantic
            for (CodegenProperty cp : codegenProperties) {
                String typing = getPydanticType(cp, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, model.classname);
                List<String> fields = new ArrayList<>();
                String firstField = "";

                // is readOnly?
                if (cp.isReadOnly) {
                    readOnlyFields.add(cp.name);
                }

                if (!cp.required) { //optional
                    firstField = "None";
                    typing = "Optional[" + typing + "]";
                    typingImports.add("Optional");
                } else { // required
                    firstField = "...";
                    if (cp.isNullable) {
                        typing = "Optional[" + typing + "]";
                        typingImports.add("Optional");
                    }
                }

                // field
                if (cp.baseName != null && !cp.baseName.equals(cp.name)) { // base name not the same as name
                    fields.add(String.format(Locale.ROOT, "alias=\"%s\"", cp.baseName));
                }

                if (!StringUtils.isEmpty(cp.description)) { // has description
                    fields.add(String.format(Locale.ROOT, "description=\"%s\"", cp.description));
                }

                /* TODO review as example may break the build
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format(Locale.ROOT, "example=%s", cp.getExample()));
                }*/

                String fieldCustomization;
                if ("None".equals(firstField)) {
                    if (cp.defaultValue == null) {
                        fieldCustomization = "None";
                    } else {
                        if (cp.isArray || cp.isMap) {
                            // TODO handle default value for array/map
                            fieldCustomization = "None";
                        } else {
                            fieldCustomization = cp.defaultValue;
                        }
                    }
                } else { // required field
                    fieldCustomization = firstField;
                }

                if (!fields.isEmpty()) {
                    fields.add(0, fieldCustomization);
                    pydanticImports.add("Field");
                    fieldCustomization = String.format(Locale.ROOT, "Field(%s)", StringUtils.join(fields, ", "));
                }

                if ("...".equals(fieldCustomization)) {
                    // use Field() to avoid pylint warnings
                    pydanticImports.add("Field");
                    fieldCustomization = "Field(...)";
                }

                cp.vendorExtensions.put("x-py-typing", typing + " = " + fieldCustomization);

                // setup x-py-name for each oneOf/anyOf schema
                if (!model.oneOf.isEmpty()) { // oneOf
                    cp.vendorExtensions.put("x-py-name", String.format(Locale.ROOT, "oneof_schema_%d_validator", property_count++));
                } else if (!model.anyOf.isEmpty()) { // anyOf
                    cp.vendorExtensions.put("x-py-name", String.format(Locale.ROOT, "anyof_schema_%d_validator", property_count++));
                }
            }

            if (!model.isEnum) {
                pydanticImports.add("BaseModel");
            }

            // add parent model to import
            if (!StringUtils.isEmpty(model.parent)) {
                modelImports.add(model.parent);
            }

            // set enum type in extensions and update `name` in enumVars
            if (model.isEnum) {
                for (Map<String, Object> enumVars : (List<Map<String, Object>>) model.getAllowableValues().get("enumVars")) {
                    if ((Boolean) enumVars.get("isString")) {
                        model.vendorExtensions.putIfAbsent("x-py-enum-type", "str");
                        // update `name`, e.g.
                        enumVars.put("name", toEnumVariableName((String) enumVars.get("value"), "str"));
                    } else {
                        model.vendorExtensions.putIfAbsent("x-py-enum-type", "int");
                        enumVars.put("name", toEnumVariableName((String) enumVars.get("value"), "int"));
                    }
                }
            }

            // set the extensions if the key is absent
            model.getVendorExtensions().putIfAbsent("x-py-typing-imports", typingImports);
            model.getVendorExtensions().putIfAbsent("x-py-pydantic-imports", pydanticImports);
            model.getVendorExtensions().putIfAbsent("x-py-datetime-imports", datetimeImports);
            model.getVendorExtensions().putIfAbsent("x-py-readonly", readOnlyFields);

            // import models one by one
            if (!modelImports.isEmpty()) {
                Set<String> modelsToImport = new TreeSet<>();
                for (String modelImport : modelImports) {
                    if (modelImport.equals(model.classname)) {
                        // skip self import
                        continue;
                    }
                    modelsToImport.add("from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                }

                model.getVendorExtensions().putIfAbsent("x-py-model-imports", modelsToImport);
            }
        }

        return objs;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
    }

    /*
     * The OpenAPI pattern spec follows the Perl convention and style of modifiers. Python
     * does not support this in as natural a way so it needs to convert it. See
     * https://docs.python.org/2/howto/regex.html#compilation-flags for details.
     *
     * @param pattern (the String pattern to convert from python to Perl convention)
     * @param vendorExtensions (list of custom x-* properties for extra functionality-see https://swagger.io/docs/specification/openapi-extensions/)
     * @return void
     * @throws IllegalArgumentException if pattern does not follow the Perl /pattern/modifiers convention
     *
     * Includes fix for issue #6675
     */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            int i = pattern.lastIndexOf('/');

            // TOOD update the check below follow python convention
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

            vendorExtensions.put("x-regex", regex.replace("\"", "\\\""));
            vendorExtensions.put("x-pattern", pattern.replace("\"", "\\\""));
            vendorExtensions.put("x-modifiers", modifiers);
        }
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

    @Override
    public String generatorLanguageVersion() {
        return "3.7+";
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        final Schema additionalProperties = getAdditionalProperties(schema);

        if (additionalProperties != null) {
            codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
        }
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if ("int".equals(datatype) || "float".equals(datatype)) {
            return name;
        } else {
            return "\'" + name + "\'";
        }
    }

    public String toEnumVariableName(String name, String datatype) {
        if ("int".equals(datatype)) {
            return "NUMBER_" + name.replace("-", "MINUS_");
        }

        // remove quote e.g. 'abc' => abc
        name = name.substring(1, name.length() - 1);

        if (name.length() == 0) {
            return "EMPTY";
        }

        if (" ".equals(name)) {
            return "SPACE";
        }

        if ("_".equals(name)) {
            return "UNDERSCORE";
        }

        if (reservedWords.contains(name)) {
            name = name.toUpperCase(Locale.ROOT);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character)))) {
            name = underscore(escape(name, specialCharReplacements, Collections.singletonList("_"), "_")).toUpperCase(Locale.ROOT);
        } else {
            name = name.toUpperCase(Locale.ROOT);
        }

        name = name.replace(" ", "_");
        name = name.replaceFirst("^_", "");
        name = name.replaceFirst("_$", "");

        if (name.matches("\\d.*")) {
            name = "ENUM_" + name.toUpperCase(Locale.ROOT);
        }

        return name;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return value;
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

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "var_" + name;
    }

    public void setMapNumberTo(String mapNumberTo) {
        if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)
                || "StrictFloat".equals(mapNumberTo)
                || "float".equals(mapNumberTo)) {
            this.mapNumberTo = mapNumberTo;
        } else {
            throw new IllegalArgumentException("mapNumberTo value must be Union[StrictFloat, StrictInt], StrictStr or float");
        }
    }

    public void setDatetimeFormat(String datetimeFormat) {
        this.datetimeFormat = datetimeFormat;
    }

    public void setDateFormat(String dateFormat) {
        this.dateFormat = dateFormat;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }
}
