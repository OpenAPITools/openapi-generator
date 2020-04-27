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

import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonClientCodegen extends PythonAbstractCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    public static final String DEFAULT_LIBRARY = "urllib3";

    protected String projectName; // for setup.py, e.g. petstore-api
    protected String packageUrl;
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    private String testFolder;

    public PythonClientCodegen() {
        super("python");

        setPackageName("openapi_client");
        setPackageVersion("1.0.0");

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

        supportsInheritance = true;
        apiPackage = "api";
        outputFolder = "generated-code" + File.separatorChar + "python";

        apiTemplateFiles.put("api.mustache", ".py");

        modelTestTemplateFiles.put("model_test.mustache", ".py");
        apiTestTemplateFiles.put("api_test.mustache", ".py");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        testFolder = "test";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // TODO Override the mapping to byte array, until its safe to switch it.
        // mapped to String as a workaround
        typeMapping.put("binary", "str");
        typeMapping.put("ByteArray", "str");

        // These are reserved words added specifically for this generator.
        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable name used in API methods (endpoints)
                        "all_params", "resource_path", "path_params", "query_params",
                        "header_params", "form_params", "local_var_files", "body_params", "auth_settings"));

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue("openapi_client"));
        cliOptions.add(new CliOption(CodegenConstants.PROJECT_NAME, "python project name in setup.py (e.g. petstore-api)."));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(PACKAGE_URL, "python package URL."));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCECODEONLY_GENERATION, CodegenConstants.SOURCECODEONLY_GENERATION_DESC)
                .defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(USE_NOSE, "use the nose test framework").
                defaultValue(Boolean.FALSE.toString()));

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

        if (additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = Boolean.valueOf(additionalProperties.get(CodegenConstants.EXCLUDE_TESTS).toString());
        }

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

        Boolean generateSourceCodeOnly = false;
        if (additionalProperties.containsKey(CodegenConstants.SOURCECODEONLY_GENERATION)) {
            generateSourceCodeOnly = Boolean.valueOf(additionalProperties.get(CodegenConstants.SOURCECODEONLY_GENERATION).toString());
        }

        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

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

        if (additionalProperties.containsKey(USE_NOSE)) {
            setUseNose((String) additionalProperties.get(USE_NOSE));
        }

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
        supportingFiles.add(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + modelPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__api.mustache", packagePath() + File.separatorChar + apiPackage, "__init__.py"));

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

        modelPackage = packageName + "." + modelPackage;
        apiPackage = packageName + "." + apiPackage;

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

    @Override
    public String toModelTestFilename(String name) {
        return "test_" + toModelFilename(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // e.g. PhoneNumberApi.py => phone_number_api.py
        return underscore(name + "_" + apiNameSuffix);
    }

    @Override
    public String toApiName(String name) {
        return super.toApiName(name);
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_api";
        }
        return underscore(name + "_" + apiNameSuffix);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(sanitizeName(operationId));
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getPackageUrl() {
        return packageUrl;
    }

    public void setPackageUrl(String packageUrl) {
        this.packageUrl = packageUrl;
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
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (Boolean.valueOf(p.getDefault().toString()) == false)
                    return "False";
                else
                    return "True";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) p.getDefault()).find())
                    return "'''" + p.getDefault() + "'''";
                else
                    return "'" + ((String) p.getDefault()).replaceAll("'", "\'") + "'";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        }

        return null;
    }

    @Override
    public String toExampleValue(Schema schema) {
        return toExampleValueRecursive(schema, new ArrayList<String>(), 5);
    }

    private String toExampleValueRecursive(Schema schema, List<String> included_schemas, int indentation) {
        String indentation_string = "";
        for (int i=0 ; i< indentation ; i++) indentation_string += "    ";
        String example = super.toExampleValue(schema);

        if (ModelUtils.isNullType(schema) && null != example) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return "None";
        }
        // correct "true"s into "True"s, since super.toExampleValue uses "toString()" on Java booleans
        if (ModelUtils.isBooleanSchema(schema) && null!=example) {
            if ("false".equalsIgnoreCase(example)) example = "False";
            else example = "True";
        }

        // correct "&#39;"s into "'"s after toString()
        if (ModelUtils.isStringSchema(schema) && schema.getDefault() != null) {
            example = (String) schema.getDefault();
        }

        if (StringUtils.isNotBlank(example) && !"null".equals(example)) {
            if (ModelUtils.isStringSchema(schema)) {
                example = "'" + example + "'";
            }
            return example;
        }

        if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
        // Enum case:
            example = schema.getEnum().get(0).toString();
            if (ModelUtils.isStringSchema(schema)) {
                example = "'" + escapeText(example) + "'";
            }
            if (null == example)
                LOGGER.warn("Empty enum. Cannot built an example!");

            return example;
        } else if (null != schema.get$ref()) {
        // $ref case:
            Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
            String ref = ModelUtils.getSimpleRef(schema.get$ref());
            if (allDefinitions != null) {
                Schema refSchema = allDefinitions.get(ref);
                if (null == refSchema) {
                    return "None";
                } else {
                    String refTitle = refSchema.getTitle();
                    if (StringUtils.isBlank(refTitle) || "null".equals(refTitle)) {
                        refSchema.setTitle(ref);
                    }
                    if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                        included_schemas.add(schema.getTitle());
                    }
                    return toExampleValueRecursive(refSchema, included_schemas, indentation);
                }
            } else {
                LOGGER.warn("allDefinitions not defined in toExampleValue!\n");
            }
        }
        if (ModelUtils.isDateSchema(schema)) {
            example = "datetime.datetime.strptime('1975-12-30', '%Y-%m-%d').date()";
            return example;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            example = "datetime.datetime.strptime('2013-10-20 19:20:30.00', '%Y-%m-%d %H:%M:%S.%f')";
            return example;
        } else if (ModelUtils.isBinarySchema(schema)) {
            example = "bytes(b'blah')";
            return example;
        } else if (ModelUtils.isByteArraySchema(schema)) {
            example = "YQ==";
        } else if (ModelUtils.isStringSchema(schema)) {
            // a BigDecimal:
            if ("Number".equalsIgnoreCase(schema.getFormat())) {return "1";}
            if (StringUtils.isNotBlank(schema.getPattern())) return "'a'"; // I cheat here, since it would be too complicated to generate a string from a regexp
            int len = 0;
            if (null != schema.getMinLength()) len = schema.getMinLength().intValue();
            if (len < 1) len = 1;
            example = "";
            for (int i=0;i<len;i++) example += i;
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (schema.getMinimum() != null)
                example = schema.getMinimum().toString();
            else
                example = "56";
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (schema.getMinimum() != null)
                example = schema.getMinimum().toString();
            else
                example = "1.337";
        } else if (ModelUtils.isBooleanSchema(schema)) {
            example = "True";
        } else if (ModelUtils.isArraySchema(schema)) {
            if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                included_schemas.add(schema.getTitle());
            }
            ArraySchema arrayschema = (ArraySchema) schema;
            example = "[\n" + indentation_string + toExampleValueRecursive(arrayschema.getItems(), included_schemas, indentation+1) + "\n" + indentation_string + "]";
        } else if (ModelUtils.isMapSchema(schema)) {
            if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                included_schemas.add(schema.getTitle());
            }
            Object additionalObject = schema.getAdditionalProperties();
            if (additionalObject instanceof Schema) {
                Schema additional = (Schema) additionalObject;
                String the_key = "'key'";
                if (additional.getEnum() != null && !additional.getEnum().isEmpty()) {
                    the_key = additional.getEnum().get(0).toString();
                    if (ModelUtils.isStringSchema(additional)) {
                        the_key = "'" + escapeText(the_key) + "'";
                    }
                }
                example = "{\n" + indentation_string + the_key + " : " + toExampleValueRecursive(additional, included_schemas, indentation+1) + "\n" + indentation_string + "}";
            } else {
                example = "{ }";
            }
        } else if (ModelUtils.isObjectSchema(schema)) {
            if (StringUtils.isBlank(schema.getTitle())) {
                example = "None";
                return example;
            }

            // I remove any property that is a discriminator, since it is not well supported by the python generator
            String toExclude = null;
            if (schema.getDiscriminator()!=null) {
                toExclude = schema.getDiscriminator().getPropertyName();
            }
            
            example = packageName + ".models." + underscore(schema.getTitle())+"."+schema.getTitle()+"(";

            // if required only:
            // List<String> reqs = schema.getRequired();

            // if required and optionals
            List<String> reqs = new ArrayList<>();
            if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
                for (Object toAdd : schema.getProperties().keySet()) {
                    reqs.add((String) toAdd);
                }

                Map<String, Schema> properties = schema.getProperties();
                Set<String> propkeys = null;
                if (properties != null) propkeys = properties.keySet();
                if (toExclude != null && reqs.contains(toExclude)) {
                    reqs.remove(toExclude);
                }
                for (String toRemove : included_schemas) {
                    if (reqs.contains(toRemove)) {
                        reqs.remove(toRemove);
                    }
                }
                if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                    included_schemas.add(schema.getTitle());
                }
                if (null != schema.getRequired()) for (Object toAdd : schema.getRequired()) {
                    reqs.add((String) toAdd);
                }
                if (null != propkeys) for (String propname : propkeys) {
                    Schema schema2 = properties.get(propname);
                    if (reqs.contains(propname)) {
                        String refTitle = schema2.getTitle();
                        if (StringUtils.isBlank(refTitle) || "null".equals(refTitle)) {
                            schema2.setTitle(propname);
                        }
                        example += "\n" + indentation_string + underscore(propname) + " = " +
                                toExampleValueRecursive(schema2, included_schemas, indentation + 1) + ", ";
                    }
                }
            }
            example +=")";
        } else {
            LOGGER.warn("Type " + schema.getType() + " not handled properly in toExampleValue");
        }

        if (ModelUtils.isStringSchema(schema)) {
            example = "'" + escapeText(example) + "'";
        }

        return example;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equalsIgnoreCase(type) || "str".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Integer".equals(type) || "int".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("file".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "'" + escapeText(example) + "'";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = this.packageName + "." + type + "()";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "None";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "{'key': " + example + "}";
        }

        p.example = example;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        Schema schema = parameter.getSchema();

        if (parameter.getExample() != null) {
            codegenParameter.example = parameter.getExample().toString();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty()) {
            Example example = parameter.getExamples().values().iterator().next();
            if (example.getValue() != null) {
                codegenParameter.example = example.getValue().toString();
            }
        } else if (schema != null && schema.getExample() != null) {
            codegenParameter.example = schema.getExample().toString();
        }

        setParameterExampleValue(codegenParameter);
    }

    @Override
    public String sanitizeTag(String tag) {
        return sanitizeName(tag);
    }
}
