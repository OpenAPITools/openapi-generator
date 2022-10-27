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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonNextgenClientCodegen extends AbstractPythonCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(PythonNextgenClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    public static final String DEFAULT_LIBRARY = "urllib3";
    public static final String RECURSION_LIMIT = "recursionLimit";
    public static final String PYTHON_ATTR_NONE_IF_UNSET = "pythonAttrNoneIfUnset";

    protected String packageUrl;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected boolean hasModelsToImport = Boolean.FALSE;

    protected Map<Character, String> regexModifiers;

    private String testFolder;

    public PythonNextgenClientCodegen() {
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
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;
        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separatorChar + "python";

        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");

        modelTestTemplateFiles.put("model_test.mustache", ".py");
        apiTestTemplateFiles.put("api_test.mustache", ".py");

        embeddedTemplateDir = templateDir = "python-nextgen";

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        testFolder = "test";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        "date",
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

        supportedLibraries.put("urllib3", "urllib3-based client");
        supportedLibraries.put("asyncio", "Asyncio-based client (python 3.5+)");
        supportedLibraries.put("tornado", "tornado-based client");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use: asyncio, tornado, urllib3");
        libraryOption.setDefault(DEFAULT_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(DEFAULT_LIBRARY);
    }

    @Override
    public void processOpts() {
        super.processOpts();

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
            supportingFiles.add(new SupportingFile("gitlab-ci.mustache", "", ".gitlab-ci.yml"));
            supportingFiles.add(new SupportingFile("setup.mustache", "", "setup.py"));
        }
        supportingFiles.add(new SupportingFile("configuration.mustache", packagePath(), "configuration.py"));
        supportingFiles.add(new SupportingFile("__init__package.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", modelPath, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__api.mustache", apiPath, "__init__.py"));

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

    /*
     * Gets the pydantic type given a Codegen Parameter
     *
     * @param cp codegen parameter
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenParameter cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports) {
        if (cp.isArray) {
            typingImports.add("List");
            return String.format("List[%s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports));
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format("Dict[str, %s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports));
        } else if (cp.isString || cp.isBinary || cp.isByteArray) {
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
                    fieldCustomization.add(String.format("regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format("constr(%s)", StringUtils.join(fieldCustomization, ", "));
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
                // e.g. confloat(ge=10, le=100, strict=True)
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

                pydanticImports.add("confloat");
                return String.format("%s(%s)", "confloat",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictFloat");
                return "StrictFloat";
            }
        } else if (cp.isInteger || cp.isLong || cp.isShort || cp.isUnboundedInteger) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conint(ge=10, le=100, strict=True)
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

                pydanticImports.add("conint");
                return String.format("%s(%s)", "conint",
                        StringUtils.join(fieldCustomization, ", "));

            } else {
                pydanticImports.add("StrictInt");
                return "StrictInt";
            }
        /* comment out the following as byte/binary is a string at the moment (path to the file, e.g. "/var/tmp/a.gif")
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

                pydanticImports.add("conbytes");
                return String.format("%s(%s)", "conbytes", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                return "StrictBytes";
            }*/
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
                return String.format("%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal";
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
            return cp.dataType;
        } else {
            throw new RuntimeException("Error! CodegenProperty not yet supported in getPydanticType: " + cp);
        }
    }

    /*
     * Gets the pydantic type given a Codegen Property
     *
     * @param cp codegen property
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenProperty cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports) {
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
            return String.format("Literal[%s]", StringUtils.join(values, ", "));
        } else if (cp.isArray) {
            typingImports.add("List");
            return String.format("List[%s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports));
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format("Dict[str, %s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports));
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
                    fieldCustomization.add(String.format("regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format("constr(%s)", StringUtils.join(fieldCustomization, ", "));
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
                // e.g. confloat(ge=10, le=100, strict=True)
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

                pydanticImports.add("confloat");
                return String.format("%s(%s)", "confloat",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictFloat");
                return "StrictFloat";
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
                return String.format("%s(%s)", "conint",
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

                pydanticImports.add("conbytes");
                return String.format("%s(%s)", "conbytes", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                return "StrictBytes";
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
                return String.format("%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal";
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
            return cp.dataType;
        } else {
            throw new RuntimeException("Error! CodegenParameter not yet supported in getPydanticType: " + cp);
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

            List<CodegenParameter> params = operation.allParams;
            for (CodegenParameter param : params) {
                String typing = getPydanticType(param, typingImports, pydanticImports, datetimeImports, modelImports);
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
                    fields.add(String.format("description=\"%s\"", param.description));
                }

                if (param.isArray && param.getUniqueItems()) { // a set
                    fields.add("unique_items=True");
                }

                /*
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format("example=%s", cp.getExample()));
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
                    fieldCustomization = String.format("Field(%s)", StringUtils.join(fields, ", "));
                } else {
                    fieldCustomization = "Field()";
                }

                if ("Field()".equals(fieldCustomization)) {
                    param.vendorExtensions.put("x-py-typing", typing);
                } else {
                    param.vendorExtensions.put("x-py-typing", String.format("Annotated[%s, %s]", typing, fieldCustomization));
                }
            }

            /* TODO return type
            operation.returnType

            List<CodegenResponse> responses = operation.responses;
            if (responses != null) {
                for (CodegenResponse resp : responses) {
                    if (resp.is2xx) {

                    }
                }
            }*/

            // set the extensions if the key is absent
            //model.getVendorExtensions().putIfAbsent("x-py-typing-imports", typingImports);
            //model.getVendorExtensions().putIfAbsent("x-py-pydantic-imports", pydanticImports);
            //model.getVendorExtensions().putIfAbsent("x-py-datetime-imports", datetimeImports);

            //if (hasModelsToImport || !StringUtils.isEmpty(model.parent)) {
            //    model.vendorExtensions.put("x-py-import-models", true);
            //}
        }

        List<Map<String, String>> newImports = new ArrayList<>();

        // need datetime import
        if (!datetimeImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format("from datetime import %s\n", StringUtils.join(datetimeImports, ", ")));
            newImports.add(item);
        }

        // need pydantic imports
        if (!pydanticImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format("from pydantic import %s\n", StringUtils.join(pydanticImports, ", ")));
            newImports.add(item);
        }

        // need typing imports
        if (!typingImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format("from typing import %s\n", StringUtils.join(typingImports, ", ")));
            newImports.add(item);
        }

        // need models import
        if (hasModelsToImport) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format("from %s import models", packageName));
            newImports.add(item);
        }

        // models import
        if (hasModelsToImport) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format("from %s import %s", modelPackage, StringUtils.join(modelImports, ", ")));
            newImports.add(item);
        }

        // reset imports with newImports
        objs.setImports(newImports);
        return objs;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // process enum in models
        objs = postProcessModelsEnum(objs);

        TreeSet<String> typingImports = new TreeSet<>();
        TreeSet<String> pydanticImports = new TreeSet<>();
        TreeSet<String> datetimeImports = new TreeSet<>();
        TreeSet<String> modelImports = new TreeSet<>();

        for (ModelMap m : objs.getModels()) {
            hasModelsToImport = false;
            int property_count = 1;
            typingImports.clear();
            pydanticImports.clear();
            datetimeImports.clear();

            CodegenModel model = m.getModel();
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
            //loop through properties/schemas to setup typing, pydantic
            for (CodegenProperty cp : codegenProperties) {
                String typing = getPydanticType(cp, typingImports, pydanticImports, datetimeImports, modelImports);
                List<String> fields = new ArrayList<>();
                String firstField = "";

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
                    fields.add(String.format("alias=\"%s\"", cp.baseName));
                }

                if (!StringUtils.isEmpty(cp.description)) { // has description
                    fields.add(String.format("description=\"%s\"", cp.description));
                }

                if (cp.isArray && cp.getUniqueItems()) { // a set
                    fields.add("unique_items=True");
                }

                /* TODO
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format("example=%s", cp.getExample()));
                }*/

                String fieldCustomization;
                if ("None".equals(firstField)) {
                    if (cp.defaultValue == null) {
                        fieldCustomization = "None";
                    } else {
                        fieldCustomization = cp.defaultValue;
                    }
                } else { // required field
                    fieldCustomization = firstField;
                }

                if (!fields.isEmpty()) {
                    fields.add(0, fieldCustomization);
                    pydanticImports.add("Field");
                    fieldCustomization = String.format("Field(%s)", StringUtils.join(fields, ", "));
                }

                cp.vendorExtensions.put("x-py-typing", typing + " = " + fieldCustomization);

                // setup x-py-name for each oneOf/anyOf schema
                if (!model.oneOf.isEmpty()) { // oneOf
                    cp.vendorExtensions.put("x-py-name", String.format("__oneof_schema_%d", property_count++));
                } else if (!model.anyOf.isEmpty()) { // anyOf
                    cp.vendorExtensions.put("x-py-name", String.format("__anyof_schema_%d", property_count++));
                }
            }

            if (!model.isEnum) {
                pydanticImports.add("BaseModel");
            }

            // set the extensions if the key is absent
            model.getVendorExtensions().putIfAbsent("x-py-typing-imports", typingImports);
            model.getVendorExtensions().putIfAbsent("x-py-pydantic-imports", pydanticImports);
            model.getVendorExtensions().putIfAbsent("x-py-datetime-imports", datetimeImports);
            model.getVendorExtensions().putIfAbsent("x-py-model-imports", modelImports);

            if (hasModelsToImport || !StringUtils.isEmpty(model.parent)) {
                model.vendorExtensions.put("x-py-import-models", true);
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

            vendorExtensions.put("x-regex", regex);
            vendorExtensions.put("x-modifiers", modifiers);
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "python-nextgen";
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
        if (name.length() == 0) {
            return "EMPTY";
        }

        if (name.trim().length() == 0) {
            return "SPACE_" + name.length();
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return (getSymbolName(name)).toUpperCase(Locale.ROOT);
        }

        // number
        if ("int".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return "NUMBER_" + varName;
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName) || enumName.matches("\\d.*")) { // reserved word or starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
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
        return "self::" + datatype + "_" + value;
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
}
