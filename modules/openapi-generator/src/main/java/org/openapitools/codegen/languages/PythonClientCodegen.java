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
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.underscore;

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

    @Setter protected String packageUrl;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    @Setter protected boolean useOneOfDiscriminatorLookup = false; // use oneOf discriminator's mapping for model lookup
    @Setter protected String datetimeFormat = "%Y-%m-%dT%H:%M:%S.%f%z";
    @Setter protected String dateFormat = "%Y-%m-%d";
    @Setter protected boolean setEnsureAsciiToFalse = false;

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
        cliOptions.add(new CliOption(MAP_NUMBER_TO, "Map number to Union[StrictFloat, StrictInt], StrictStr or float.")
                .defaultValue("Union[StrictFloat, StrictInt]"));
        cliOptions.add(new CliOption(DATETIME_FORMAT, "datetime format for query parameters")
                .defaultValue("%Y-%m-%dT%H:%M:%S%z"));
        cliOptions.add(new CliOption(DATE_FORMAT, "date format for query parameters")
                .defaultValue("%Y-%m-%d"));
        cliOptions.add(new CliOption(CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP, CodegenConstants.USE_ONEOF_DISCRIMINATOR_LOOKUP_DESC).defaultValue("false"));
        cliOptions.add(new CliOption(POETRY1_FALLBACK, "Fallback to formatting pyproject.toml to Poetry 1.x format."));
        cliOptions.add(new CliOption(LAZY_IMPORTS, "Enable lazy imports.").defaultValue(Boolean.FALSE.toString()));

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
        return "3.9+";
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        final Schema additionalProperties = ModelUtils.getAdditionalProperties(schema);

        if (additionalProperties != null) {
            codegenModel.additionalPropertiesType = getSchemaType(additionalProperties);
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
