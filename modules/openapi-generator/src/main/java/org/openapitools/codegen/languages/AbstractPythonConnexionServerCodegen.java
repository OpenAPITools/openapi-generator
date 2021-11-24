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

public abstract class AbstractPythonConnexionServerCodegen extends AbstractPythonCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractPythonConnexionServerCodegen.class);

    public static final String CONTROLLER_PACKAGE = "controllerPackage";
    public static final String DEFAULT_CONTROLLER = "defaultController";
    public static final String SUPPORT_PYTHON2 = "supportPython2";
    public static final String FEATURE_CORS = "featureCORS";
    // nose is a python testing framework, we use pytest if USE_NOSE is unset
    public static final String USE_NOSE = "useNose";
    public static final String PYTHON_SRC_ROOT = "pythonSrcRoot";
    public static final String USE_PYTHON_SRC_ROOT_IN_IMPORTS = "usePythonSrcRootInImports";
    public static final String MOVE_TESTS_UNDER_PYTHON_SRC_ROOT = "testsUsePythonSrcRoot";
    static final String MEDIA_TYPE = "mediaType";

    protected int serverPort = 8080;
    protected String controllerPackage;
    protected String defaultController;
    protected Map<Character, String> regexModifiers;
    protected boolean fixBodyName;
    protected boolean featureCORS = Boolean.FALSE;
    protected boolean useNose = Boolean.FALSE;
    protected String pythonSrcRoot;
    protected boolean usePythonSrcRootInImports = Boolean.FALSE;
    protected boolean moveTestsUnderPythonSrcRoot = Boolean.FALSE;

    public AbstractPythonConnexionServerCodegen(String templateDirectory, boolean fixBodyNameValue) {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        fixBodyName = fixBodyNameValue;
        modelPackage = "models";
        testPackage = "test";

        // TODO may remove these later to default to the setting in abstract python base class instead
        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");

        // set the output folder here
        outputFolder = "generated-code" + File.separatorChar + "connexion";

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
        cliOptions.add(new CliOption(SUPPORT_PYTHON2, "support python2. This option has been deprecated and will be removed in the 5.x release.").
                defaultValue("false"));
        cliOptions.add(new CliOption("serverPort", "TCP port to listen to in app.run").
                defaultValue("8080"));
        cliOptions.add(CliOption.newBoolean(FEATURE_CORS, "use flask-cors for handling CORS requests").
                defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(CliOption.newBoolean(USE_NOSE, "use the nose test framework").
                defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(PYTHON_SRC_ROOT, "put python sources in this subdirectory of output folder (defaults to \"\" for). Use this for src/ layout.").
                defaultValue(""));
        cliOptions.add(new CliOption(USE_PYTHON_SRC_ROOT_IN_IMPORTS, "include pythonSrcRoot in import namespaces.").
                defaultValue("false"));
        cliOptions.add(new CliOption(MOVE_TESTS_UNDER_PYTHON_SRC_ROOT, "generates test under the pythonSrcRoot folder.")
            .defaultValue("false"));
    }

    protected void addSupportingFiles() {
    }

    @Override
    public void processOpts() {
        super.processOpts();

        //apiTemplateFiles.clear();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi_server");
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
        if (additionalProperties.containsKey(FEATURE_CORS)) {
            setFeatureCORS((String) additionalProperties.get(FEATURE_CORS));
        }
        if (additionalProperties.containsKey(USE_NOSE)) {
            setUseNose((String) additionalProperties.get(USE_NOSE));
        }
        if (additionalProperties.containsKey(USE_PYTHON_SRC_ROOT_IN_IMPORTS)) {
            setUsePythonSrcRootInImports((String) additionalProperties.get(USE_PYTHON_SRC_ROOT_IN_IMPORTS));
        }
        if (additionalProperties.containsKey(MOVE_TESTS_UNDER_PYTHON_SRC_ROOT)) {
            setMoveTestsUnderPythonSrcRoot((String) additionalProperties.get(MOVE_TESTS_UNDER_PYTHON_SRC_ROOT));
        }
        if (additionalProperties.containsKey(PYTHON_SRC_ROOT)) {
            String pythonSrcRoot = (String) additionalProperties.get(PYTHON_SRC_ROOT);
            if (moveTestsUnderPythonSrcRoot) {
                testPackage = pythonSrcRoot + "." + testPackage;
            }
            if (usePythonSrcRootInImports) {
                // if we prepend the package name with the pythonSrcRoot we get the desired effect.
                // But, we also need to set pythonSrcRoot itself to "" to ensure all the paths are
                // what we expect.
                setPackageName(pythonSrcRoot + "." + packageName);
                pythonSrcRoot = "";
            }
            setPythonSrcRoot(pythonSrcRoot);
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

    public void setFeatureCORS(String val) {
        this.featureCORS = Boolean.parseBoolean(val);
    }

    public void setUseNose(String val) {
        this.useNose = Boolean.parseBoolean(val);
    }

    public void setPythonSrcRoot(String val) {
        String pySrcRoot;
        if (val == null) {
            pySrcRoot = "";
        } else {
            pySrcRoot = val.replaceAll("[/\\\\]+$", "");
        }

        if (pySrcRoot.isEmpty() || ".".equals(pySrcRoot)) {
            this.pythonSrcRoot = "";
        } else {
            this.pythonSrcRoot = pySrcRoot + File.separator;
        }
        additionalProperties.put(PYTHON_SRC_ROOT, StringUtils.defaultIfBlank(this.pythonSrcRoot, null));
    }

    public void setUsePythonSrcRootInImports(String val) {
        this.usePythonSrcRootInImports = Boolean.parseBoolean(val);
    }

    public void setMoveTestsUnderPythonSrcRoot(String val) {
        this.moveTestsUnderPythonSrcRoot = Boolean.parseBoolean(val);
    }

    public String pythonSrcOutputFolder() {
        return outputFolder + File.separator + pythonSrcRoot;
    }

    private static String packageToPath(String pkg) {
        return pkg.replace(".", File.separator);
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
    public void preprocessOpenAPI(OpenAPI openAPI) {
        // need vendor extensions for x-openapi-router-controller
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            List<String> pathnames = new ArrayList(paths.keySet());
            for (String pathname : pathnames) {
                PathItem path = paths.get(pathname);
                // Fix path parameters to be in snake_case
                if (pathname.contains("{")) {
                    String fixedPath = "";
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
                        LOGGER.warn(
                                "Path '{}' is not consistent with Python variable names. It will be replaced by '{}'",
                                pathname, fixedPath);
                        paths.remove(pathname);
                        path.addExtension("x-python-connexion-openapi-name", pathname);
                        paths.put(fixedPath, path);
                    }
                }
                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (Map.Entry<HttpMethod, Operation> operationMapEntry : operationMap.entrySet()) {
                        HttpMethod method = operationMapEntry.getKey();
                        Operation operation = operationMapEntry.getValue();
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
                                    LOGGER.warn(
                                            "Parameter name '{}' is not consistent with Python variable names. It will be replaced by '{}'",
                                            swaggerParameterName, pythonParameterName);
                                    parameter.addExtension("x-python-connexion-openapi-name", swaggerParameterName);
                                    parameter.setName(pythonParameterName);
                                }
                                if (swaggerParameterName.isEmpty()) {
                                    LOGGER.error("Missing parameter name in {}.{}", pathname, parameter.getIn());
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
            for (Map.Entry<String, SecurityScheme> securitySchemesEntry : securitySchemes.entrySet()) {
                String securityName = securitySchemesEntry.getKey();
                SecurityScheme securityScheme = securitySchemesEntry.getValue();
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
                        LOGGER.warn("Security type {} is not supported by connexion yet", securityScheme.getType().toString());
                    case OAUTH2:
                        addSecurityExtension(securityScheme, "x-tokenInfoFunc", baseFunctionName + "info_from_" + securityName);
                        addSecurityExtension(securityScheme, "x-scopeValidateFunc", baseFunctionName + "validate_scope_" + securityName);
                        break;
                    default:
                        LOGGER.warn("Unknown security type {}", securityScheme.getType().toString());
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
                        LOGGER.info(
                                "Path '{}' is not consistent with the original OpenAPI definition. It will be replaced back by '{}'",
                                pythonPathname, openapiPathname);
                        paths.remove(pythonPathname);
                        paths.put(openapiPathname, path);
                    }
                }

                Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
                if (operationMap != null) {
                    for (Map.Entry<HttpMethod, Operation> operationMapEntry : operationMap.entrySet()) {
                        HttpMethod method = operationMapEntry.getKey();
                        Operation operation = operationMapEntry.getValue();
                        if (operation.getParameters() != null) {
                            for (Parameter parameter : operation.getParameters()) {
                                Map<String, Object> parameterExtensions = parameter.getExtensions();
                                if (parameterExtensions != null) {
                                    // Get and remove the (temporary) vendor extension
                                    String swaggerParameterName = (String) parameterExtensions.remove("x-python-connexion-openapi-name");
                                    if (swaggerParameterName != null) {
                                        String pythonParameterName = parameter.getName();
                                        if (!swaggerParameterName.equals(pythonParameterName)) {
                                            LOGGER.info(
                                                    "Reverting name of parameter '{}' of operation '{}' back to '{}'",
                                                    pythonParameterName, operation.getOperationId(), swaggerParameterName);
                                            parameter.setName(swaggerParameterName);
                                        } else {
                                            LOGGER.debug("Name of parameter '{}' of operation '{}' was unchanged.",
                                                    pythonParameterName, operation.getOperationId());
                                        }
                                    } else {
                                        LOGGER.debug(
                                                "x-python-connexion-openapi-name was not set on parameter '{}' of operation '{}'",
                                                parameter.getName(), operation.getOperationId());
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

    public String packagePath() {
        String pkgPath = packageName.replace('.', File.separatorChar);
        return pythonSrcRoot + pkgPath;
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
                    operation.vendorExtensions.put("x-preferred-consume", consume);
                } else if (operation.consumes.size() > 1) {
                    Map<String, String> consume = operation.consumes.get(0);
                    skipTests.put("reason", "Connexion does not support multiple consumes. See https://github.com/zalando/connexion/pull/760");
                    operation.vendorExtensions.put("x-preferred-consume", consume);
                    if ("multipart/form-data".equals(consume.get(MEDIA_TYPE))) {
                        operation.isMultipart = Boolean.TRUE;
                    }
                }
            } else {
                // A body without consumes means '*/*' has been used instead of application/json
                if (operation.bodyParam != null) {
                    Map<String, String> consume = new HashMap<>();
                    consume.put(MEDIA_TYPE, "application/json");
                    operation.vendorExtensions.put("x-preferred-consume", consume);
                    skipTests.put("reason", "*/* not supported by Connexion. Use application/json instead. See https://github.com/zalando/connexion/pull/760");
                }
            }
            // Choose to consume 'application/json' if available, else choose the last one.
            if (operation.produces != null) {
                for (Map<String, String> produce : operation.produces) {
                    operation.vendorExtensions.put("x-preferred-produce", produce);
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
}
