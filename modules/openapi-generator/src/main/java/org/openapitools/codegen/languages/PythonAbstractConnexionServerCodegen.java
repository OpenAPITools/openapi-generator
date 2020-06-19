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

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonAbstractConnexionServerCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonAbstractConnexionServerCodegen.class);

    public static final String CONTROLLER_PACKAGE = "controllerPackage";
    public static final String DEFAULT_CONTROLLER = "defaultController";
    public static final String SUPPORT_PYTHON2 = "supportPython2";
    // nose is a python testing framework, we use pytest if USE_NOSE is unset
    public static final String USE_NOSE = "useNose";
    public static final String PYTHON_SRC_ROOT = "pythonSrcRoot";
    static final String MEDIA_TYPE = "mediaType";

    protected int serverPort = 8080;
    protected String packageName;
    protected String packageVersion;
    protected String controllerPackage;
    protected String defaultController;
    protected Map<Character, String> regexModifiers;
    protected boolean fixBodyName;
    protected boolean useNose = Boolean.FALSE;
    protected String pythonSrcRoot;

    public PythonAbstractConnexionServerCodegen(String templateDirectory, boolean fixBodyNameValue) {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        fixBodyName = fixBodyNameValue;
        modelPackage = "models";
        testPackage = "test";

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("str");
        languageSpecificPrimitives.add("datetime");
        languageSpecificPrimitives.add("date");
        languageSpecificPrimitives.add("file");
        languageSpecificPrimitives.add("object");
        languageSpecificPrimitives.add("byte");
        languageSpecificPrimitives.add("bytearray");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("number", "float");
        typeMapping.put("long", "int");
        typeMapping.put("double", "float");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "str");
        typeMapping.put("date", "date");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("object", "object");
        typeMapping.put("file", "file");
        typeMapping.put("UUID", "str");
        typeMapping.put("URI", "str");
        typeMapping.put("byte", "bytearray");
        typeMapping.put("ByteArray", "bytearray");

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // @property
                        "property",
                        // python reserved words
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try", "self", "None", "True", "False", "nonlocal"));

        // set the output folder here
        outputFolder = "generated-code/connexion";

        apiTemplateFiles.put("controller.mustache", ".py");
        modelTemplateFiles.put("model.mustache", ".py");
        apiTestTemplateFiles().put("controller_test.mustache", ".py");

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = templateDirectory;

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("serverPort", serverPort);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("test-requirements.mustache", "", "test-requirements.txt"));
        supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));

        regexModifiers = new HashMap<Character, String>();
        regexModifiers.put('i', "IGNORECASE");
        regexModifiers.put('l', "LOCALE");
        regexModifiers.put('m', "MULTILINE");
        regexModifiers.put('s', "DOTALL");
        regexModifiers.put('u', "UNICODE");
        regexModifiers.put('x', "VERBOSE");

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue("openapi_server"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue("1.0.0"));
        cliOptions.add(new CliOption(CONTROLLER_PACKAGE, "controller package").
                defaultValue("controllers"));
        cliOptions.add(new CliOption(DEFAULT_CONTROLLER, "default controller").
                defaultValue("default_controller"));
        cliOptions.add(new CliOption(SUPPORT_PYTHON2, "support python2").
                defaultValue("false"));
        cliOptions.add(new CliOption("serverPort", "TCP port to listen to in app.run").
                defaultValue("8080"));
        cliOptions.add(CliOption.newBoolean(USE_NOSE, "use the nose test framework").
                defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(PYTHON_SRC_ROOT, "put python sources in this subdirectory of output folder (defaults to \"\" for). Use this for src/ layout.").
                defaultValue(""));
    }

    protected void addSupportingFiles() {
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("PYTHON_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PYTHON_POST_PROCESS_FILE not defined so the Python code may not be properly formatted. To define it, try 'export PYTHON_POST_PROCESS_FILE=\"/usr/local/bin/yapf -i\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        //apiTemplateFiles.clear();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi_server");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, this.packageName);
        }
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            setPackageVersion("1.0.0");
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, this.packageVersion);
        }
        if (additionalProperties.containsKey(CONTROLLER_PACKAGE)) {
            this.controllerPackage = additionalProperties.get(CONTROLLER_PACKAGE).toString();
        } else {
            this.controllerPackage = "controllers";
            additionalProperties.put(CONTROLLER_PACKAGE, this.controllerPackage);
        }
        if (additionalProperties.containsKey(DEFAULT_CONTROLLER)) {
            this.defaultController = additionalProperties.get(DEFAULT_CONTROLLER).toString();
        } else {
            this.defaultController = "default_controller";
            additionalProperties.put(DEFAULT_CONTROLLER, this.defaultController);
        }
        if (Boolean.TRUE.equals(additionalProperties.get(SUPPORT_PYTHON2))) {
            additionalProperties.put(SUPPORT_PYTHON2, Boolean.TRUE);
            typeMapping.put("long", "long");
        }
        if (additionalProperties.containsKey(USE_NOSE)) {
            setUseNose((String) additionalProperties.get(USE_NOSE));
        }
        if (additionalProperties.containsKey(PYTHON_SRC_ROOT)) {
            setPythonSrcRoot((String) additionalProperties.get(PYTHON_SRC_ROOT));
            additionalProperties.put(PYTHON_SRC_ROOT, pythonSrcRoot);
        } else {
            setPythonSrcRoot("");
        }
        supportingFiles.add(new SupportingFile("__main__.mustache", packagePath(), "__main__.py"));
        supportingFiles.add(new SupportingFile("util.mustache", packagePath(), "util.py"));
        supportingFiles.add(new SupportingFile("typing_utils.mustache", packagePath(), "typing_utils.py"));
        supportingFiles.add(new SupportingFile("__init__.mustache", packagePath() + File.separatorChar + packageToPath(controllerPackage), "__init__.py"));
        supportingFiles.add(new SupportingFile("security_controller_.mustache", packagePath() + File.separatorChar + packageToPath(controllerPackage), "security_controller_.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + packageToPath(modelPackage), "__init__.py"));
        supportingFiles.add(new SupportingFile("base_model_.mustache", packagePath() + File.separatorChar + packageToPath(modelPackage), "base_model_.py"));
        supportingFiles.add(new SupportingFile("openapi.mustache", packagePath() + File.separatorChar + "openapi", "openapi.yaml"));
        addSupportingFiles();

        modelPackage = packageName + "." + modelPackage;
        controllerPackage = packageName + "." + controllerPackage;
    }

    public void setUseNose(String val) {
        this.useNose = Boolean.valueOf(val);
    }

    public void setPythonSrcRoot(String val) {
        String pySrcRoot;
        if (val == null) {
            pySrcRoot = "";
        } else {
            pySrcRoot = val.replaceAll("[/\\\\]+$", "");
        }

        if (pySrcRoot.isEmpty() || pySrcRoot == ".") {
            this.pythonSrcRoot = "";
        } else {
            this.pythonSrcRoot = pySrcRoot + File.separator;
        }
    }

    public String pythonSrcOutputFolder() {
        return outputFolder + File.separator + pythonSrcRoot;
    }

    private static String packageToPath(String pkg) {
        return pkg.replace(".", File.separator);
    }

    private static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    @Override
    public String apiPackage() {
        return controllerPackage;
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Python server library using the Connexion project. By default, " +
                "it will also generate service classes -- which you can disable with the `-Dnoservice` environment variable.";
    }

    @Override
    public String toApiName(String name) {
        if (name == null || name.length() == 0) {
            return "DefaultController";
        }
        return camelize(name, false) + "Controller";
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(toApiName(name));
    }

    @Override
    public String toApiTestFilename(String name) {
        return "test_" + toApiFilename(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name; // add an underscore to the name
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        String pkgPath = apiPackage().replace('.', File.separatorChar);
        return pythonSrcOutputFolder() + pkgPath;
    }

    @Override
    public String modelFileFolder() {
        String pkgPath = modelPackage().replace('.', File.separatorChar);
        return pythonSrcOutputFolder() + pkgPath;
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

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = toModelName(schemaType);
        }
        return type;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        // need vendor extensions for x-openapi-router-controller
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            List<String> pathnames = new ArrayList(paths.keySet());
            for (String pathname : pathnames) {
                PathItem path = paths.get(pathname);
                // Fix path parameters to be in snake_case
                if (pathname.contains("{")) {
                    String fixedPath = new String();
                    for (String token : pathname.substring(1).split("/")) {
                        if (token.startsWith("{")) {
                            String snake_case_token = "{" + this.toParamName(token.substring(1, token.length() - 1)) + "}";
                            if (!token.equals(snake_case_token)) {
                                token = snake_case_token;
                            }
                        }
                        fixedPath += "/" + token;
                    }
                    if (!fixedPath.equals(pathname)) {
                        LOGGER.warn("Path '" + pathname + "' is not consistant with Python variable names. It will be replaced by '" + fixedPath + "'");
                        paths.remove(pathname);
                        path.addExtension("x-python-connexion-openapi-name", pathname);
                        paths.put(fixedPath, path);
                    }
                }
                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (HttpMethod method : operationMap.keySet()) {
                        Operation operation = operationMap.get(method);
                        String tag = "default";
                        if (operation.getTags() != null && operation.getTags().size() > 0) {
                            tag = operation.getTags().get(0);
                        }
                        String operationId = operation.getOperationId();
                        if (operationId == null) {
                            operationId = getOrGenerateOperationId(operation, pathname, method.toString());
                        }
                        operation.setOperationId(toOperationId(operationId));
                        if (operation.getExtensions() == null || operation.getExtensions().get("x-openapi-router-controller") == null) {
                            operation.addExtension(
                                    "x-openapi-router-controller",
                                    controllerPackage + "." + toApiFilename(tag)
                            );
                        }
                        if (operation.getParameters() != null) {
                            for (Parameter parameter : operation.getParameters()) {
                                String swaggerParameterName = parameter.getName();
                                String pythonParameterName = this.toParamName(swaggerParameterName);
                                if (!swaggerParameterName.equals(pythonParameterName)) {
                                    LOGGER.warn("Parameter name '" + swaggerParameterName + "' is not consistant with Python variable names. It will be replaced by '" + pythonParameterName + "'");
                                    parameter.addExtension("x-python-connexion-openapi-name", swaggerParameterName);
                                    parameter.setName(pythonParameterName);
                                }
                                if (swaggerParameterName.isEmpty()) {
                                    LOGGER.error("Missing parameter name in " + pathname + "." + parameter.getIn());
                                }
                            }
                        }
                        RequestBody body = operation.getRequestBody();
                        if (fixBodyName && body != null) {
                            if (body.getExtensions() == null || !body.getExtensions().containsKey("x-body-name")) {
                                String bodyParameterName = "body";
                                if (operation.getExtensions() != null && operation.getExtensions().containsKey("x-codegen-request-body-name")) {
                                    bodyParameterName = (String) operation.getExtensions().get("x-codegen-request-body-name");
                                } else {
                                    // Used by code generator
                                    operation.addExtension("x-codegen-request-body-name", bodyParameterName);
                                }
                                // Used by connexion
                                body.addExtension("x-body-name", bodyParameterName);
                            }
                        }
                    }
                }
            }
            // Sort path names after variable name fix
            List<String> fixedPathnames = new ArrayList(paths.keySet());
            Collections.sort(fixedPathnames);
            for (String pathname : fixedPathnames) {
                PathItem pathItem = paths.remove(pathname);
                paths.put(pathname, pathItem);
            }
        }
        addSecurityExtensions(openAPI);
    }

    private void addSecurityExtension(SecurityScheme securityScheme, String extensionName, String functionName) {
        if (securityScheme.getExtensions() == null || !securityScheme.getExtensions().containsKey(extensionName)) {
            securityScheme.addExtension(extensionName, functionName);
        }
    }

    private void addSecurityExtensions(OpenAPI openAPI) {
        Components components = openAPI.getComponents();
        if (components != null && components.getSecuritySchemes() != null) {
            Map<String, SecurityScheme> securitySchemes = components.getSecuritySchemes();
            for (String securityName : securitySchemes.keySet()) {
                SecurityScheme securityScheme = securitySchemes.get(securityName);
                String baseFunctionName = controllerPackage + ".security_controller_.";
                switch (securityScheme.getType()) {
                    case APIKEY:
                        addSecurityExtension(securityScheme, "x-apikeyInfoFunc", baseFunctionName + "info_from_" + securityName);
                        break;
                    case HTTP:
                        if ("basic".equals(securityScheme.getScheme())) {
                            addSecurityExtension(securityScheme, "x-basicInfoFunc", baseFunctionName + "info_from_" + securityName);
                        } else if ("bearer".equals(securityScheme.getScheme())) {
                            addSecurityExtension(securityScheme, "x-bearerInfoFunc", baseFunctionName + "info_from_" + securityName);
                        }
                        break;
                    case OPENIDCONNECT:
                        LOGGER.warn("Security type " + securityScheme.getType().toString() + " is not supported by connextion yet");
                    case OAUTH2:
                        addSecurityExtension(securityScheme, "x-tokenInfoFunc", baseFunctionName + "info_from_" + securityName);
                        addSecurityExtension(securityScheme, "x-scopeValidateFunc", baseFunctionName + "validate_scope_" + securityName);
                        break;
                    default:
                        LOGGER.warn("Unknown security type " + securityScheme.getType().toString());
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static List<Map<String, Object>> getOperations(Map<String, Object> objs) {
        List<Map<String, Object>> result = new ArrayList<Map<String, Object>>();
        Map<String, Object> apiInfo = (Map<String, Object>) objs.get("apiInfo");
        List<Map<String, Object>> apis = (List<Map<String, Object>>) apiInfo.get("apis");
        for (Map<String, Object> api : apis) {
            result.add((Map<String, Object>) api.get("operations"));
        }
        return result;
    }

    private static List<Map<String, Object>> sortOperationsByPath(List<CodegenOperation> ops) {
        Multimap<String, CodegenOperation> opsByPath = ArrayListMultimap.create();

        for (CodegenOperation op : ops) {
            opsByPath.put(op.path, op);
        }

        List<Map<String, Object>> opsByPathList = new ArrayList<Map<String, Object>>();
        for (Map.Entry<String, Collection<CodegenOperation>> entry : opsByPath.asMap().entrySet()) {
            Map<String, Object> opsByPathEntry = new HashMap<String, Object>();
            opsByPathList.add(opsByPathEntry);
            opsByPathEntry.put("path", entry.getKey());
            opsByPathEntry.put("operation", entry.getValue());
            List<CodegenOperation> operationsForThisPath = Lists.newArrayList(entry.getValue());
            operationsForThisPath.get(operationsForThisPath.size() - 1).hasMore = false;
            if (opsByPathList.size() < opsByPath.asMap().size()) {
                opsByPathEntry.put("hasMore", "true");
            }
        }

        return opsByPathList;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        // XXX - Revert the original parameter (and path) names to make sure we have
        //       a consistent REST interface across other server/client languages:
        //
        // XXX - Reverts `x-python-connexion-openapi-name` back to the original (query/path) parameter name.
        //       We do not want to have our REST API itself being converted to pythonic params.
        //       This would be incompatible with other server implementations.
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            List<String> pathnames = new ArrayList(paths.keySet());
            for (String pythonPathname : pathnames) {
                PathItem path = paths.get(pythonPathname);

                // Fix path parameters back to original casing
                Map<String, Object> pathExtensions = path.getExtensions();
                if (pathExtensions != null) {
                    // Get and remove the (temporary) vendor extension
                    String openapiPathname = (String) pathExtensions.remove("x-python-connexion-openapi-name");
                    if (openapiPathname != null && !openapiPathname.equals(pythonPathname)) {
                        LOGGER.info("Path '" + pythonPathname + "' is not consistant with the original OpenAPI definition. It will be replaced back by '" + openapiPathname + "'");
                        paths.remove(pythonPathname);
                        paths.put(openapiPathname, path);
                    }
                }

                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (HttpMethod method : operationMap.keySet()) {
                        Operation operation = operationMap.get(method);
                        if (operation.getParameters() != null) {
                            for (Parameter parameter : operation.getParameters()) {
                                Map<String, Object> parameterExtensions = parameter.getExtensions();
                                if (parameterExtensions != null) {
                                    // Get and remove the (temporary) vendor extension
                                    String swaggerParameterName = (String) parameterExtensions.remove("x-python-connexion-openapi-name");
                                    if (swaggerParameterName != null) {
                                        String pythonParameterName = parameter.getName();
                                        if (!swaggerParameterName.equals(pythonParameterName)) {
                                            LOGGER.info("Reverting name of parameter '" + pythonParameterName + "' of operation '" + operation.getOperationId() + "' back to '" + swaggerParameterName + "'");
                                            parameter.setName(swaggerParameterName);
                                        } else {
                                            LOGGER.debug("Name of parameter '" + pythonParameterName + "' of operation '" + operation.getOperationId() + "' was unchanged.");
                                        }
                                    } else {
                                        LOGGER.debug("x-python-connexion-openapi-name was not set on parameter '" + parameter.getName() + "' of operation '" + operation.getOperationId() + "'");
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Sort path names after variable name fix
            List<String> recoveredPathnames = new ArrayList(paths.keySet());
            Collections.sort(recoveredPathnames);
            for (String pathname : recoveredPathnames) {
                PathItem pathItem = paths.remove(pathname);
                paths.put(pathname, pathItem);
            }
        }

        generateJSONSpecFile(objs);
        generateYAMLSpecFile(objs);

        for (Map<String, Object> operations : getOperations(objs)) {
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");

            List<Map<String, Object>> opsByPathList = sortOperationsByPath(ops);
            operations.put("operationsByPath", opsByPathList);
        }
        return super.postProcessSupportingFileData(objs);
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
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(dropDots(toModelName(name)));
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

        return underscore(sanitizeName(operationId));
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
                if (p.getDefault().toString().equalsIgnoreCase("false"))
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
                return "'" + (String) p.getDefault() + "'";
            }
        }

        return null;
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
            if (p.minimum != null) {
                example = "" + (Integer.valueOf(p.minimum) + 1);
            }
            if (p.maximum != null) {
                example = "" + p.maximum;
            } else if (example == null) {
                example = "56";
            }

        } else if ("Long".equalsIgnoreCase(type)) {
            if (p.minimum != null) {
                example = "" + (Long.valueOf(p.minimum) + 1);
            }
            if (p.maximum != null) {
                example = "" + p.maximum;
            } else if (example == null) {
                example = "789";
            }
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (p.minimum != null) {
                example = "" + p.minimum;
            } else if (p.maximum != null) {
                example = "" + p.maximum;
            } else if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("file".equalsIgnoreCase(type)) {
            example = "(BytesIO(b'some file data'), 'file.txt')";
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
            example = "{}";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (p.items != null && p.items.defaultValue != null) {
            example = p.items.defaultValue;
        }
        if (example == null) {
            if (Boolean.TRUE.equals(p.isListContainer)) {
                example = "[]";
            } else {
                example = "None";
            }
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            if (Boolean.TRUE.equals(p.isBodyParam)) {
                example = "[" + example + "]";
            }
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "{'key': " + example + "}";
        }

        p.example = example;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public String packagePath() {
        String pkgPath = packageName.replace('.', File.separatorChar);
        return pythonSrcRoot + pkgPath;
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
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (StringUtils.isNotEmpty(property.pattern)) {
            addImport(model, "import re");
        }
        postProcessPattern(property.pattern, property.vendorExtensions);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                // Add additional filename information for imports
                mo.put("pyImports", toPyImports(cm, cm.imports));
            }
        }
        return result;
    }

    private List<Map<String, String>> toPyImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> pyImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> pyImport = new HashMap<>();
                pyImport.put("import", toModelImport(im));
                pyImports.add(pyImport);
            }
        }
        return pyImports;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation operation : operationList) {
            Map<String, String> skipTests = new HashMap<>();
            // Set flag to deactivate tests due to connexion issue.
            if (operation.consumes != null) {
                if (operation.consumes.size() == 1) {
                    Map<String, String> consume = operation.consumes.get(0);
                    if (!("application/json".equals(consume.get(MEDIA_TYPE))
                            || consume.get(MEDIA_TYPE).endsWith("+json"))) {
                        skipTests.put("reason", consume.get(MEDIA_TYPE) + " not supported by Connexion");
                        if ("multipart/form-data".equals(consume.get(MEDIA_TYPE))) {
                            operation.isMultipart = Boolean.TRUE;
                        }
                    }
                    operation.vendorExtensions.put("x-prefered-consume", consume);
                } else if (operation.consumes.size() > 1) {
                    Map<String, String> consume = operation.consumes.get(0);
                    skipTests.put("reason", "Connexion does not support multiple consummes. See https://github.com/zalando/connexion/pull/760");
                    operation.vendorExtensions.put("x-prefered-consume", consume);
                    if ("multipart/form-data".equals(consume.get(MEDIA_TYPE))) {
                        operation.isMultipart = Boolean.TRUE;
                    }
                }
            } else {
                // A body without consumes means '*/*' has been used instead of application/json
                if (operation.bodyParam != null) {
                    Map<String, String> consume = new HashMap<>();
                    consume.put(MEDIA_TYPE, "application/json");
                    operation.vendorExtensions.put("x-prefered-consume", consume);
                    skipTests.put("reason", "*/* not supported by Connexion. Use application/json instead. See https://github.com/zalando/connexion/pull/760");
                }
            }
            // Choose to consume 'application/json' if available, else choose the last one.
            if (operation.produces != null) {
                for (Map<String, String> produce : operation.produces) {
                    operation.vendorExtensions.put("x-prefered-produce", produce);
                    if (produce.get(MEDIA_TYPE).equals("application/json")) {
                        break;
                    }
                }
            }
            if (!skipTests.isEmpty()) {
                operation.vendorExtensions.put("x-skip-test", skipTests);
            }
            if (operation.requestBodyExamples != null) {
                for (Map<String, String> example : operation.requestBodyExamples) {
                    if (example.get("contentType") != null && example.get("contentType").equals("application/json")) {
                        operation.bodyParam.example = example.get("example");
                    }
                }
            }
        }
        return objs;
    }

    /*
     * The openapi pattern spec follows the Perl convention and style of modifiers. Python
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

    /*
     * We don't want to run `escapeText` on the pattern
     * but forward it directly to the Python implementation.
     */
    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

}
