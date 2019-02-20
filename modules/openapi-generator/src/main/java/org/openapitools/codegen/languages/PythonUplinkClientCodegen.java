/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.core.util.Json;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenSecurity;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.mustache.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonUplinkClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientCodegen.class);

    public static final String PACKAGE_URL = "packageUrl";
    // public static final String DEFAULT_LIBRARY = "requests";
    public static final String CLIENT_CLASS_NAME = "clientClassName";

    static final String MEDIA_TYPE = "mediaType";

    protected String packageName; // e.g. petstore_api
    protected String packageVersion;
    protected String projectName; // for setup.py, e.g. petstore-api
    protected String packageUrl;

    protected String clientClassName;

    protected Map<Character, String> regexModifiers;

    protected String apiDocPath = "docs" + File.separator;
    protected String modelDocPath = "docs" + File.separator;
    private String testFolder, pkgSrcDir, apiSrcDir, modelSrcDir, docsSrcDir, testSrcDir;

    // marshmallow is the library used for model schemas. This is separate from typeMapping as parameters, etc don't use marshmallow.
    protected Map<String, String> marshmallowTypeMapping = new HashMap<String, String>();
    protected Set<String> marshmallowPrimitives = new HashSet<String>();

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-uplink";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Python client library based on the uplink api library (github.com/prkumar/uplink) and marshmallow schema de/serializer (github.com/marshmallow-code/marshmallow).";
    }

    /**
     * Class constructor
     */
    public PythonUplinkClientCodegen() {
        super();

        // clear import mapping (from default generator) as python does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;

        // directories
        modelPackage = "models";
        apiPackage = "api";
        testFolder = "tests";
        outputFolder = "generated-code" + File.separator + "python";

        pkgSrcDir = "package_name" + File.separator;
        apiSrcDir = pkgSrcDir + File.separator + "api" + File.separator;
        modelSrcDir = pkgSrcDir + File.separator + "models" + File.separator;
        docsSrcDir = "docs" + File.separator;
        testSrcDir = "test" + File.separator;

        embeddedTemplateDir = templateDir = "python-uplink";

        // standard codegen template files
        modelTemplateFiles.put(modelSrcDir + "model.py.mustache", ".py");
        apiTemplateFiles.put(apiSrcDir + "api.py.mustache", ".py");

        modelTestTemplateFiles.put(testSrcDir + "model_test.py.mustache", ".py");
        apiTestTemplateFiles.put(testSrcDir + "api_test.py.mustache", ".py");

        modelDocTemplateFiles.put(docsSrcDir + "model_doc.md.mustache", ".md");
        apiDocTemplateFiles.put(docsSrcDir + "api_doc.md.mustache", ".md");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("decimal");
        languageSpecificPrimitives.add("list");
        languageSpecificPrimitives.add("dict");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("str");
        languageSpecificPrimitives.add("datetime");
        languageSpecificPrimitives.add("date");
        languageSpecificPrimitives.add("object");
        languageSpecificPrimitives.add("file");

        typeMapping.clear();
        typeMapping.put("number", "float");
        typeMapping.put("float", "float");
        typeMapping.put("double", "float");
        typeMapping.put("int", "int"); // in default typeMapping, not in spec
        typeMapping.put("integer", "int");
        typeMapping.put("short", "int"); // in default typeMapping, not in spec
        typeMapping.put("long", "int");
        typeMapping.put("BigDecimal", "decimal");
        typeMapping.put("boolean", "bool");
        typeMapping.put("bool", "bool"); // not in spec
        typeMapping.put("array", "list");
        typeMapping.put("List", "list"); // in default typeMapping, not in spec
        typeMapping.put("map", "dict");
        // no separate detection of FreeFormObject
        typeMapping.put("char", "str"); // in default typeMapping, not in spec
        typeMapping.put("string", "str");
        typeMapping.put("str", "str");
        typeMapping.put("UUID", "str");
        // misc other formats are still string
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("date", "date");
        typeMapping.put("object", "object");
        typeMapping.put("file", "file");
        // binary, ByteArray could be bytes, but str is fine
        typeMapping.put("binary", "str");
        typeMapping.put("ByteArray", "str");


        marshmallowPrimitives.clear();
        // marshmallowPrimitives.add("Number");  // base type for Float, Integer
        marshmallowPrimitives.add("Float");
        marshmallowPrimitives.add("Integer");
        marshmallowPrimitives.add("Decimal"); // no equiv in OpenAPI/Swagger. Needs vendorExtension format
        marshmallowPrimitives.add("Boolean");
        marshmallowPrimitives.add("List");
        marshmallowPrimitives.add("Dict");
        marshmallowPrimitives.add("String");
        marshmallowPrimitives.add("Email");
        marshmallowPrimitives.add("UUID");
        marshmallowPrimitives.add("URL");
        marshmallowPrimitives.add("DateTime");
        marshmallowPrimitives.add("LocalDateTime"); // no equiv in OpenAPI/Swagger. Prob not needed.
        marshmallowPrimitives.add("Date");
        marshmallowPrimitives.add("Time");
        marshmallowPrimitives.add("TimeDelta");
        marshmallowPrimitives.add("FormattedString");
        marshmallowPrimitives.add("Nested"); // used for object schemas
        marshmallowPrimitives.add("Raw"); // ??
        marshmallowPrimitives.add("Constant"); // maybe not useful
        marshmallowPrimitives.add("Method"); // maybe not useful
        marshmallowPrimitives.add("Function"); // maybe not useful

        // key is a combo of strings returned by getSchemaType plus some extras defined by getMarshmallowSchemaType
        // If the value is in marshmallowPrimitives, use it. Otherwise, make sure the templates import the type.
        marshmallowTypeMapping.clear();
        marshmallowTypeMapping.put("number", "Float");
        marshmallowTypeMapping.put("float", "Float");
        marshmallowTypeMapping.put("double", "Float");
        marshmallowTypeMapping.put("int", "Integer");
        marshmallowTypeMapping.put("integer", "Integer");
        marshmallowTypeMapping.put("short", "Integer");
        marshmallowTypeMapping.put("long", "Integer");
        marshmallowTypeMapping.put("BigDecimal", "Decimal"); // type=string,format=number (precise eg currency)
        marshmallowTypeMapping.put("boolean", "Boolean");
        marshmallowTypeMapping.put("bool", "Boolean");
        // containers require special handling.
        marshmallowTypeMapping.put("array", "List");
        marshmallowTypeMapping.put("List", "List");
        marshmallowTypeMapping.put("map", "Dict");
        marshmallowTypeMapping.put("FreeFormObject", "Dict");
        // object
        // marshmallow String is always unicode
        marshmallowTypeMapping.put("char", "String");
        marshmallowTypeMapping.put("string", "String");
        marshmallowTypeMapping.put("str", "String");
        // marshmallowTypeMapping.put("password", "Raw"); // TODO: does this make sense? or String? or ...?
        // marshmallowTypeMapping.put("file", "Raw"); // TODO: custom File field
        // marshmallowTypeMapping.put("binary", "Raw"); // TODO: Maybe Bytes?
        // marshmallowTypeMapping.put("ByteArray", "Bytes"); // TODO: marshmallow doesn't have Bytes field
        // marshmallowTypeMapping.put("", "FormattedString"); // str.format() not a pattern string
        marshmallowTypeMapping.put("email", "Email");
        marshmallowTypeMapping.put("UUID", "UUID");
        marshmallowTypeMapping.put("url", "URL");
        marshmallowTypeMapping.put("DateTime", "DateTime");
        marshmallowTypeMapping.put("datetime", "DateTime");
        // marshmallowTypeMapping.put("", "LocalDateTime");
        marshmallowTypeMapping.put("date", "Date");
        marshmallowTypeMapping.put("time", "Time");
        // marshmallowTypeMapping.put("", "TimeDelta");

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable name used in API methods (endpoints)
                        // NONE
                        // @property
                        "property",
                        // python reserved words
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try", "self", "nonlocal", "None", "True", "False"));

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
        cliOptions.add(new CliOption(CLIENT_CLASS_NAME, "python client class name (eg PetstoreApiClient). Defaults to ApiClient."));
        // sortParamsByRequiredKey is not optional
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCECODEONLY_GENERATION, CodegenConstants.SOURCECODEONLY_GENERATION_DESC)
                .defaultValue(Boolean.FALSE.toString()));

        /*
        supportedLibraries.put("requests", "requests-based client");
        supportedLibraries.put("asyncio", "Asyncio-based client (python 3.5+)");
        supportedLibraries.put("tornado", "tornado-based client");
        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use: asyncio, tornado, requests");
        libraryOption.setDefault(DEFAULT_LIBRARY);
        cliOptions.add(libraryOption);
        setLibrary(DEFAULT_LIBRARY);
        */
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("PYTHON_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PYTHON_POST_PROCESS_FILE not defined so the Python code may not be properly formatted. To define it, try 'export PYTHON_POST_PROCESS_FILE=\"/usr/local/bin/yapf -i\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        Boolean excludeTests = false;

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        if (additionalProperties.containsKey(CodegenConstants.EXCLUDE_TESTS)) {
            excludeTests = Boolean.valueOf(additionalProperties.get(CodegenConstants.EXCLUDE_TESTS).toString());
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi_client_python_uplink");
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
        } else {
            setPackageVersion("0.1.0");
        }

        if (additionalProperties.containsKey(CLIENT_CLASS_NAME)) {
            setClientClassName((String) additionalProperties.get(CLIENT_CLASS_NAME));
        } else {
            setClientClassName("ApiClient");
        }

        Boolean generateSourceCodeOnly = false;
        if (additionalProperties.containsKey(CodegenConstants.SOURCECODEONLY_GENERATION)) {
            generateSourceCodeOnly = true;
        }

        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        additionalProperties.put(CLIENT_CLASS_NAME, clientClassName);

        if (generateSourceCodeOnly) {
            // tests in <package>/test
            testFolder = packageName + File.separator + testFolder;
            // api/model docs in <package>/docs
            apiDocPath = packageName + File.separator + apiDocPath;
            modelDocPath = packageName + File.separator + modelDocPath;
        }
        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        if (additionalProperties.containsKey(PACKAGE_URL)) {
            setPackageUrl((String) additionalProperties.get(PACKAGE_URL));
        }

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        String readmePath = "README.md";
        String readmeTemplate = "README.md.mustache";
        if (generateSourceCodeOnly) {
            readmePath = packageName + "_" + readmePath;
            readmeTemplate = "README_onlypackage.md.mustache";
        }
        supportingFiles.add(new SupportingFile(readmeTemplate, "", readmePath));

        if (!generateSourceCodeOnly) {
            supportingFiles.add(new SupportingFile("setup.py.mustache", "", "setup.py"));
            supportingFiles.add(new SupportingFile("requirements.txt", "", "requirements.txt"));
            supportingFiles.add(new SupportingFile("test-requirements.txt", "", "test-requirements.txt"));

            supportingFiles.add(new SupportingFile("tox.ini.mustache", "", "tox.ini"));
            supportingFiles.add(new SupportingFile("travis.yml.mustache", "", ".travis.yml"));

            supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
            supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        }

        // <packageName>/
        // <packageName>/<apiPackage>
        // <packageName>/<modelPackage>

        supportingFiles.add(new SupportingFile(pkgSrcDir + "__init__.py.mustache", packageName, "__init__.py"));
        supportingFiles.add(new SupportingFile(pkgSrcDir + "client.py.mustache", packageName, "client.py"));
        supportingFiles.add(new SupportingFile(pkgSrcDir + "decorators.py.mustache", packageName, "decorators.py"));
        supportingFiles.add(new SupportingFile(pkgSrcDir + "security.py.mustache", packageName, "security.py"));

        supportingFiles.add(new SupportingFile(apiSrcDir + "__init__.py.mustache", packageName + File.separator + apiPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile(apiSrcDir + "consumer.py.mustache", packageName + File.separator + apiPackage, "consumer.py"));

        supportingFiles.add(new SupportingFile(modelSrcDir + "__init__.py.mustache", packageName + File.separator + modelPackage, "__init__.py"));
        supportingFiles.add(new SupportingFile(modelSrcDir + "_schema.py.mustache", packageName + File.separator + modelPackage, "_schema.py"));
        supportingFiles.add(new SupportingFile(modelSrcDir + "_validate.py.mustache", packageName + File.separator + modelPackage, "_validate.py"));


        if (Boolean.FALSE.equals(excludeTests)) {
            supportingFiles.add(new SupportingFile(testSrcDir + "__init__.py.mustache", testFolder, "__init__.py"));
        }

        modelPackage = packageName + "." + modelPackage;
        apiPackage = packageName + "." + apiPackage;

        addMustacheLambdas(additionalProperties);
    }

    private void addMustacheLambdas(Map<String, Object> objs) {

        Map<String, Mustache.Lambda> lambdas = new ImmutableMap.Builder<String, Mustache.Lambda>()
                .put("lowercase", new LowercaseLambda().generator(this))
                .put("uppercase", new UppercaseLambda())
                .put("titlecase", new TitlecaseLambda())
                .put("camelcase", new CamelCaseLambda().generator(this))
                .put("camelcase_param", new CamelCaseLambda().generator(this).escapeAsParamName(true))
                .build();

        if (objs.containsKey("lambda")) {
            LOGGER.warn("A property named 'lambda' already exists. Mustache lambdas renamed from 'lambda' to '_lambda'. " +
                        "You'll likely need to use a custom template.");
            objs.put("_lambda", lambdas);
        } else {
            objs.put("lambda", lambdas);
        }
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
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

    public String toModelSchemaImport(String name) {
        String modelImport;
        if (StringUtils.startsWithAny(name, "import", "from")) {
            modelImport = name;
        } else {
            modelImport = toModelImport(name) + "Schema";
        }
        return modelImport;
    }

    public void postProcessModelImports(Set<String> modelImports, CodegenProperty property) {
        CodegenProperty cp = property;
        while (cp != null) {
            if (cp.isModel) {
                addModelSchemaImport(modelImports, cp.complexType);
            }
            cp = cp.items;
        }
    }

    protected void addModelSchemaImport(Set<String> imports, String type) {
        if (type != null) {
            if (needToImport(type + "Schema")) {
                // add the Schema import lines directly to avoid having to search for "Schema" later.
                // eg. a model named Schema would be a false positive with `SchemaSchema` being correct.
                imports.add(toModelSchemaImport(type));
            }
        }
    }

    /*
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> allProcessedModels = super.postProcessAllModels(objs);  // a TreeMap (ordered by key)
        // TODO: adding multiple models per file requires the following changes to openapi-generator
        //       1- update InlineModelResolver.java to add a var saying 'wasInline'
        //       2- figure out where to inject 'wasTopLevel' on parsing (probably in swagger-parser)
        // Then, loop through the models to do the top level and inline models, embedding inner models just like the spec.

        /*
         * allprocessedModesl = {
         *   model_name: {  (name from schema)
         *     package: config.ModelPackage(),
         *     models: [{
         *       model: CodegenModel,
         *       importPath: config.toModelImport(cm.classnamme),
         *     }, ...]
         *     imports: [{"import": importSet}, ...],
         *
         *     classname: toModelName(model_name),
         *     ...config.additionalProperties()...,
         *   }
         * }
         */

        // a clone includes whatever TreeMap logic is defined on objs.
        // Map<String, Object> result = allProcessedModels.clone();
        // TODO: result will include a subset of allProcessedModels: one per root (not inner) model on the spec
        // result.clear();

        /*
        for (Map.Entry<String, Object> entry : allProcessedModels.entrySet()) {
            String modelSpecName = entry.getKey();
            Map<String, Object> modelData = (Map<String, Object>) entry.getValue();

            List<Map<String, Object>> models = (List<Map<String, Object>>) modelData.get("models");
            // List<Map<String, String>> imports = (List<Map<String, String>>) modelData.get("imports");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
            }
        }

        return allProcessedModels;
        // return result;
    }
    */

    // to inject vendorExtensions into the vars of models (not the models themselves)
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty property = super.fromProperty(name, p);
        if (property == null) {
            return null;
        }
        // fromProperty is recursive if isArraySchema(p) or isMapSchema(p)

        p = ModelUtils.unaliasSchema(this.openAPI, p);

        // TODO: composed schemas: allOf, anyOf, oneOf?
        if (property.isPrimitiveType) {
            // freeFormObject is a primitive type

            String marshmallowType = getMarshmallowSchemaType(p);

            if (property.isContainer) {
                // isContainer+isPrimitiveType means that the deepest non-container type is primitive

                // as this is recursive for containers, the innerMost will have x-marshmallowFieldType
                // unless it is a container without any items.
                marshmallowType = (String) property.mostInnerItems.vendorExtensions.get("x-marshmallowFieldType");
            }

            if (marshmallowType == null) {
                LOGGER.warn("Please report this issue. There shouldn't be a primitive type that doesn't have a matching marshmallow type. " + getSchemaType(p));
            // } else if (!marshmallowPrimitives.containsKey(marshmallowType)) {
                //TODO: handle file, binary, ByteArray, password
            } else {
                property.vendorExtensions.put("x-marshmallowFieldType", "fields." + marshmallowType);
            }

        } else if (!property.isContainer && !property.isModel) {
            LOGGER.warn("Please report this issue. There shouldn't be a property that is not any of: primitive, container, model.");
        }
        // else if isContainer or isModel (and not isPrimitiveType)
        //      template should use complexType to get the model
        //      dataType is a type hint (getTypeHintDeclaration())

        return property;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
        // adjust model imports to include marshmallow schemas
        postProcessModelImports(model.imports, property);
    }

    // Generator calls this per-tag in processOperations
    // based on DartJaguarClientCodegen.java
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        /*
         * objs = {
         *   operations: {
         *        classname: config.toApiName(tag),
         *        pathPrefix: config.toApiVarName(tag),
         *        operation: List<CodegenOperation> (only the ops with the tag)
         *   },
         *   package: config.apiPackage(),
         *   imports: imports,
         *   hasImport: True (or absent)
         * }
         */
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        Boolean allOpsSendJson = true;
        Boolean allOpsSendForm = true;
        Boolean allOpsSendMultipart = true;
        Boolean allOpsReturnJson = true;
        // Boolean allOpsReturnXml = true;

        Boolean firstOp = true;
        Boolean useClassMimeType = false;
        Boolean useClassSecurity = false;
        List<Map<String, String>> allOpsSend = new ArrayList<>();
        List<Map<String, String>> allOpsReturn = new ArrayList<>();
        List<CodegenSecurity> allOpsAuthMethods = new ArrayList<>();

        for (CodegenOperation op : operationList) {
            Boolean isJson = true; //default to JSON
            Boolean isForm = false;
            Boolean isMultipart = false;
            Boolean returnsJson = true; //default to JSON
            // Boolean returnsXml = false;

            // consumes Content-Type (request body media type)
            if (op.consumes != null) {
                if (firstOp) {
                    allOpsSend = new ArrayList<>(op.consumes);
                }
                if (!allOpsSend.equals(op.consumes)) {
                    allOpsSend.clear();
                }
                for (Map<String, String> consume : op.consumes) {
                    if (consume.containsKey(MEDIA_TYPE)) {
                        String type = consume.get(MEDIA_TYPE);
                        isJson = isJsonMimeType(type) || isJsonVendorMimeType(type);
                        isForm = type.equalsIgnoreCase("application/x-www-form-urlencoded");
                        isMultipart = type.equalsIgnoreCase("multipart/form-data");
                        break;  // XXX: This only look at the first Content-Type
                    }
                }
            }

            // per-op serialization controls
            op.vendorExtensions.put("isJsonOp", isJson);
            op.vendorExtensions.put("isFormOp", isForm);
            op.vendorExtensions.put("isMultipartOp", isMultipart);

            if (!isJson) {
                allOpsSendJson = false;
            }
            if (!isForm) {
                allOpsSendForm = false;
            }
            if (!isMultipart) {
                allOpsSendMultipart = false;
            }

            // produces Accept (response body media type)
            if (op.produces != null) {
                if (firstOp) {
                    allOpsReturn = new ArrayList<>(op.produces);
                }
                if (!allOpsReturn.equals(op.produces)) {
                    allOpsReturn.clear();
                }
                for (Map<String, String> produce : op.produces) {
                    if (produce.containsKey(MEDIA_TYPE)) {
                        String type = produce.get(MEDIA_TYPE);
                        returnsJson = returnsJson || isJsonMimeType(type) || isJsonVendorMimeType(type);
                        // returnsXml = returnsXml || ...;
                    }
                }
            }

            // per-op deserialization controls
            op.vendorExtensions.put("OpReturnsJson", returnsJson);
            // op.vendorExtensions.put("OpReturnsXml", returnsXml);

            if (!returnsJson) {
                allOpsReturnJson = false;
            }
            /*if (!returnsXml) {
                allOpsReturnXml = false;
            }*/

            if (op.authMethods != null) {
                if (firstOp) {
                    allOpsAuthMethods = new ArrayList<>(op.authMethods);
                }
                if (!allOpsAuthMethods.equals(op.authMethods)) {
                    allOpsAuthMethods.clear();
                }
            }

            firstOp = false;
        }

        // class serialization controls
        operations.put("allOpsSendJson", allOpsSendJson);
        operations.put("allOpsSendForm", allOpsSendForm);
        operations.put("allOpsSendMultipart", allOpsSendMultipart);

        // class deserialization controls
        operations.put("allOpsReturnJson", allOpsReturnJson);
        // operations.put("allOpsReturnXml", allOpsReturnXml);

        // class mime type headers
        useClassMimeType = !allOpsSend.isEmpty() && !allOpsReturn.isEmpty();
        operations.put("useClassMimeType", useClassMimeType);
        if (useClassMimeType) {
            operations.put("classContentTypes", allOpsSend);
            operations.put("classAcceptMimeTypes", allOpsReturn);
        }

        useClassSecurity = !allOpsAuthMethods.isEmpty();
        operations.put("useClassSecurity", useClassSecurity);
        if (useClassSecurity) {
            operations.put("allOpsAuthMethods", allOpsAuthMethods);
        }

        return objs;
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
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> securitySchemeMap) {
        List<CodegenSecurity> codegenSecurities = super.fromSecurity(securitySchemeMap);
        
        Iterator<CodegenSecurity> it = codegenSecurities.iterator();
        while (it.hasNext()) {
            final CodegenSecurity security = it.next();
            String name = security.name;
            String securityClassName = camelize(name) + "Security";
            security.vendorExtensions.put("classname", securityClassName);

            final SecurityScheme scheme = securitySchemeMap.get(name);
            security.vendorExtensions.put("description", scheme.getDescription());
        }

        return codegenSecurities;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
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
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + testFolder;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);

            return getSchemaType(p) + "(str, " + getTypeDeclaration(inner) + ")";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = toModelName(openAPIType);
        }
        return type;
    }

    /**
     * Returns the Marshmallow Schema Type given the (unaliased) OpenAPI schema.
     *
     * This is only used in models, so the logic is separated from the rest of the typing system.
     *
     * @param p property schema
     * @return string presentation of the marshmallow type
     */
    public String getMarshmallowSchemaType(Schema p) {
        String openAPIType = getSchemaType(p);  // handles things like composed schemas

        // handle things not covered or glossed over by getSchemaType
        if ("object".equals(openAPIType) && ModelUtils.isFreeFormObject(p)) {
            openAPIType = "FreeFormObject";
        } else if ("string".equals(openAPIType)) {
            String pFormat = p.getFormat();
            if ("time".equals(pFormat)) {
                openAPIType = "time";
            }/* else if ("password".equals(pFormat)) {
                openAPIType = "password";
            }*/ else if ("email".equals(pFormat)) {
                openAPIType = "email";
            } else if ("uri".equals(pFormat)
                    || "uri-reference".equals(pFormat)
                    || "iri".equals(pFormat)
                    || "iri-reference".equals(pFormat)) {
                openAPIType = "url";
            }
            /* else if ("hostname".equals(pFormat)
                    || "idn-hostname".equals(pFormat))  {
                openAPIType = "";
            } else if ("ipv4".equals(pFormat)) {
                openAPIType = "";
            } else if ("ipv6".equals(pFormat)) {
                openAPIType = "";
            }*/
        }

        String type = null;
        if (marshmallowTypeMapping.containsKey(openAPIType)) {
            type = marshmallowTypeMapping.get(openAPIType);
        }
        return type;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // remove dollar sign
        name = name.replaceAll("$", "");

        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        // underscore the variable name
        // petId => pet_id
        name = underscore(name);

        // remove leading underscore
        name = name.replaceAll("^_*", "");

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "param_callback";
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        name = name.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(dropDots(toModelName(name)));
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
        return underscore(name) + "_api";
    }

    @Override
    public String toApiTestFilename(String name) {
        return "test_" + toApiFilename(name);
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public String toApiVarName(String name) {
        if (name.length() == 0) {
            return "default_api";
        }
        return underscore(name) + "_api";
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

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setPackageUrl(String packageUrl) {
        this.packageUrl = packageUrl;
    }

    public void setClientClassName(String clientClassName) {
        this.clientClassName = clientClassName;
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
            // include fallback to example, default defined as server only
            // example is not defined as server only
            if (p.getExample() != null) {
                if (Boolean.valueOf(p.getExample().toString()) == false)
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
            // default numbers are not yet returned by v2 spec openAPI results
            // https://github.com/swagger-api/swagger-parser/issues/971
            // include fallback to example, default defined as server only
            // example is not defined as server only
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            // default integers are not yet returned by v2 spec openAPI results
            // https://github.com/swagger-api/swagger-parser/issues/971
            // include fallback to example, default defined as server only
            // example is not defined as server only
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) p.getDefault()).find())
                    return "'''" + p.getDefault() + "'''";
                else
                    return "'" + p.getDefault() + "'";
            }
            // include fallback to example, default defined as server only
            // example is not defined as server only
            if (p.getExample() != null) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) p.getExample()).find())
                    return "'''" + p.getExample() + "'''";
                else
                    return "'" + p.getExample() + "'";
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
            // include fallback to example, default defined as server only
            // example is not defined as server only
            if (p.getExample() != null) {
                return p.getExample().toString();
            }
        }

        return null;
    }

    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter) {
        String example;

        // set the example value
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));

        // if not specified in x-example, try using the defaultValue
        } else if (codegenParameter.defaultValue == null) {
            example = codegenParameter.example;
        } else {
            codegenParameter.example = codegenParameter.defaultValue;
            return;
        }
        // otherwise generate an example

        String type = codegenParameter.baseType;
        if (type == null) {
            type = codegenParameter.dataType;
        }

        if (Boolean.TRUE.equals(codegenParameter.isBoolean)) { //|| "BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if (Boolean.TRUE.equals(codegenParameter.isInteger) || Boolean.TRUE.equals(codegenParameter.isLong)) { //|| "Integer".equals(type) || "int".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(codegenParameter.isFloat) || Boolean.TRUE.equals(codegenParameter.isDouble) || Boolean.TRUE.equals(codegenParameter.isNumber)) { //|| "Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        /*
        } else if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            codegenParameter.example = "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            codegenParameter.example = "BYTE_ARRAY_DATA_HERE";
        */
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) { //|| "file".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "'" + escapeText(example) + "'";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) { //|| "Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) { //|| "DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "'" + escapeText(example) + "'";
        /*
        } else if (Boolean.TRUE.equals(codegenParameter.isUuid)) {
            codegenParameter.example = "38400000-8cf0-11bd-b23e-10b96e4ef00d";
        */
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) { //|| "String".equalsIgnoreCase(type) || "str".equalsIgnoreCase(type)) {
            if (example == null) {
                example = codegenParameter.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        /*
        } else if (Boolean.TRUE.equals(codegenParameter.isFreeFormObject)) {
            codegenParameter.example = "Object";
        */
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = this.packageName + "." + type + "()";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(codegenParameter.isListContainer)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(codegenParameter.isMapContainer)) {
            example = "{'key': " + example + "}";
        }

        codegenParameter.example = example;
    }

    @Override
    public String sanitizeTag(String tag) {
        return sanitizeName(tag);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // remove multiline comment
        return input.replace("'''", "'_'_'");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String pythonPostProcessFile = System.getenv("PYTHON_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(pythonPostProcessFile)) {
            return; // skip if PYTHON_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with py extension
        if ("py".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = pythonPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }

    final private static Pattern JSON_MIME_PATTERN = Pattern.compile("(?i)application\\/json(;.*)?");
    final private static Pattern JSON_VENDOR_MIME_PATTERN = Pattern.compile("(?i)application\\/vnd.(.*)+json(;.*)?");

    /**
     * Check if the given MIME is a JSON MIME.
     * JSON MIME examples:
     * application/json
     * application/json; charset=UTF8
     * APPLICATION/JSON
     */
    static boolean isJsonMimeType(String mime) {
        return mime != null && (JSON_MIME_PATTERN.matcher(mime).matches());
    }

    /**
     * Check if the given MIME is a JSON Vendor MIME.
     * JSON MIME examples:
     * application/vnd.mycompany+json
     * application/vnd.mycompany.resourceA.version1+json
     */
    static boolean isJsonVendorMimeType(String mime) {
        return mime != null && JSON_VENDOR_MIME_PATTERN.matcher(mime).matches();
    }
}
