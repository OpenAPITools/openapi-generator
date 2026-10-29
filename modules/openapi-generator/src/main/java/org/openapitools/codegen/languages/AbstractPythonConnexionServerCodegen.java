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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.Strings;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.model.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.openapitools.codegen.CodegenConstants.X_MODIFIERS;
import static org.openapitools.codegen.CodegenConstants.X_REGEX;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractPythonConnexionServerCodegen extends AbstractPythonCodegen implements CodegenConfig {
    private static class PythonBooleanSerializer extends JsonSerializer<Boolean> {
        @Override
        public void serialize(Boolean value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeRawValue(value ? "True" : "False");
        }
    }

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractPythonConnexionServerCodegen.class);

    public static final String CONTROLLER_PACKAGE = "controllerPackage";
    public static final String DEFAULT_CONTROLLER = "defaultController";
    public static final String FEATURE_CORS = "featureCORS";
    // nose is a python testing framework, we use pytest if USE_NOSE is unset
    public static final String USE_NOSE = "useNose";
    public static final String PYTHON_SRC_ROOT = "pythonSrcRoot";
    public static final String USE_PYTHON_SRC_ROOT_IN_IMPORTS = "usePythonSrcRootInImports";
    public static final String MOVE_TESTS_UNDER_PYTHON_SRC_ROOT = "testsUsePythonSrcRoot";
    static final String MEDIA_TYPE = "mediaType";
    // Effective style/explode of an object-typed (model) query or header parameter.
    static final String X_OBJECT_STYLE = "x-python-connexion-object-style";
    static final String X_OBJECT_EXPLODE = "x-python-connexion-object-explode";
    // Map<String, String> of wire values, used only by prepareObjectParameterExample (never by templates).
    static final String X_OBJECT_EXAMPLE = "x-python-connexion-object-example";
    // baseName escaped for a single-quoted Python literal.
    static final String X_QUOTED_NAME = "x-python-connexion-quoted-name";
    static final String HAS_OBJECT_PARAMS = "connexionObjectParams";

    // An object mapper that is used to convert an example string to
    // a "python-compliant" example string (boolean as True/False).
    final ObjectMapper MAPPER = new ObjectMapper();

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

        SimpleModule simpleModule = new SimpleModule();
        simpleModule.addSerializer(Boolean.class, new PythonBooleanSerializer());
        MAPPER.registerModule(simpleModule);

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
        if (additionalProperties.containsKey(FEATURE_CORS)) {
            setFeatureCORS(String.valueOf(additionalProperties.get(FEATURE_CORS)));
        }
        if (additionalProperties.containsKey(USE_NOSE)) {
            setUseNose(String.valueOf(additionalProperties.get(USE_NOSE)));
        }
        if (additionalProperties.containsKey(USE_PYTHON_SRC_ROOT_IN_IMPORTS)) {
            setUsePythonSrcRootInImports(String.valueOf(additionalProperties.get(USE_PYTHON_SRC_ROOT_IN_IMPORTS)));
        }
        if (additionalProperties.containsKey(MOVE_TESTS_UNDER_PYTHON_SRC_ROOT)) {
            setMoveTestsUnderPythonSrcRoot(String.valueOf(additionalProperties.get(MOVE_TESTS_UNDER_PYTHON_SRC_ROOT)));
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
        supportingFiles.add(new SupportingFile("security_controller.mustache", packagePath() + File.separatorChar + packageToPath(controllerPackage), "security_controller.py"));
        supportingFiles.add(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + packageToPath(modelPackage), "__init__.py"));
        supportingFiles.add(new SupportingFile("base_model.mustache", packagePath() + File.separatorChar + packageToPath(modelPackage), "base_model.py"));
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
        if (name == null || name.length() == 0 || "default".equalsIgnoreCase(name)) {
            // Operations without a tag are grouped under the "default" tag. Honor the
            // configurable defaultController module name for them (issue #1891); this
            // drives both the generated controller file name and connexion's
            // x-openapi-router-controller routing, keeping the two consistent.
            String controller = defaultController != null ? defaultController : "default_controller";
            return camelize(controller);
        }
        return camelize(name) + "Controller";
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
            Schema inner = ModelUtils.getSchemaItems(p);
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
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
                        String operationId = getOrGenerateOperationId(operation, pathname, method.toString());
                        operation.setOperationId(toOperationId(operationId));
                        if (operation.getExtensions() == null || operation.getExtensions().get("x-openapi-router-controller") == null) {
                            operation.addExtension(
                                    "x-openapi-router-controller",
                                    controllerPackage + "." + toApiFilename(tag)
                            );
                        }
                        if (operation.getParameters() != null) {
                            for (Parameter parameter : operation.getParameters()) {
                                if (StringUtils.isNotEmpty(parameter.get$ref())) {
                                    parameter = ModelUtils.getReferencedParameter(openAPI, parameter);
                                }
                                String swaggerParameterName = parameter.getName();
                                String pythonParameterName = this.toParamName(swaggerParameterName);
                                if (swaggerParameterName == null) {
                                    throw new RuntimeException("Please report the issue as the parameter name cannot be null: " + parameter);
                                }
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
                String baseFunctionName = controllerPackage + ".security_controller.";
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

    private static List<OperationMap> getOperations(Map<String, Object> objs) {
        List<OperationMap> result = new ArrayList<>();
        ApiInfoMap apiInfo = (ApiInfoMap) objs.get("apiInfo");
        for (OperationsMap api : apiInfo.getApis()) {
            result.add(api.getOperations());
        }
        return result;
    }

    private static List<Map<String, Object>> sortOperationsByPath(List<CodegenOperation> ops) {
        Multimap<String, CodegenOperation> opsByPath = ArrayListMultimap.create();

        for (CodegenOperation op : ops) {
            opsByPath.put(op.path, op);
        }

        List<Map<String, Object>> opsByPathList = new ArrayList<>();
        for (Map.Entry<String, Collection<CodegenOperation>> entry : opsByPath.asMap().entrySet()) {
            Map<String, Object> opsByPathEntry = new HashMap<>();
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

        for (OperationMap operations : getOperations(objs)) {
            List<CodegenOperation> ops = operations.getOperation();

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
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (StringUtils.isNotEmpty(property.pattern)) {
            addImport(model, "import re");
        }
        if (anyPropertyMatches(property, p -> p.isUuid)) {
            addImport(model, "from uuid import UUID");
        }
        postProcessPattern(property.pattern, property.vendorExtensions);
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        for (ModelsMap entry : result.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();
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
    public String toDefaultValue(Schema p) {
        Schema<?> unaliased = p != null ? ModelUtils.unaliasSchema(this.openAPI, p) : null;
        Object rawDefault = p != null && p.getDefault() != null ? p.getDefault()
                : (unaliased != null ? unaliased.getDefault() : null);
        Schema<?> target = unaliased != null ? unaliased : p;
        if (target != null && ModelUtils.isArraySchema(target) && rawDefault instanceof Iterable<?>) {
            return formatPythonDefaultValue(rawDefault, target);
        }
        return super.toDefaultValue(p);
    }

    private String formatPythonDefaultValue(Object value, Schema<?> schema) {
        Schema<?> target = schema != null ? ModelUtils.unaliasSchema(this.openAPI, schema) : null;
        if (value == null || (value instanceof com.fasterxml.jackson.databind.JsonNode
                && ((com.fasterxml.jackson.databind.JsonNode) value).isNull())) {
            return "None";
        }
        if (value instanceof com.fasterxml.jackson.databind.JsonNode) {
            com.fasterxml.jackson.databind.JsonNode node = (com.fasterxml.jackson.databind.JsonNode) value;
            if (node.isArray()) {
                Schema<?> itemSchema = target != null ? ModelUtils.getSchemaItems(target) : null;
                List<String> elements = new ArrayList<>();
                for (com.fasterxml.jackson.databind.JsonNode elem : node) {
                    elements.add(formatPythonDefaultValue(elem, itemSchema));
                }
                return "[" + String.join(", ", elements) + "]";
            }
            if (node.isObject()) {
                List<String> entries = new ArrayList<>();
                Iterator<Map.Entry<String, com.fasterxml.jackson.databind.JsonNode>> fields = node.fields();
                while (fields.hasNext()) {
                    Map.Entry<String, com.fasterxml.jackson.databind.JsonNode> entry = fields.next();
                    Schema<?> propSchema = null;
                    if (target != null && target.getProperties() != null) {
                        propSchema = (Schema<?>) target.getProperties().get(entry.getKey());
                    } else if (target != null && ModelUtils.getAdditionalProperties(target) != null) {
                        propSchema = ModelUtils.getAdditionalProperties(target);
                    }
                    entries.add(formatPythonStringLiteral(entry.getKey()) + ": "
                            + formatPythonDefaultValue(entry.getValue(), propSchema));
                }
                return "{" + String.join(", ", entries) + "}";
            }
            if (node.isBoolean() || (target != null && ModelUtils.isBooleanSchema(target))) {
                return node.asBoolean() ? "True" : "False";
            }
            if (node.isNumber() || (target != null && (ModelUtils.isIntegerSchema(target) || ModelUtils.isNumberSchema(target)))) {
                return node.asText();
            }
            return formatPythonStringLiteral(node.asText());
        }
        if (value instanceof Iterable<?>) {
            Schema<?> itemSchema = target != null ? ModelUtils.getSchemaItems(target) : null;
            List<String> elements = new ArrayList<>();
            for (Object elem : (Iterable<?>) value) {
                elements.add(formatPythonDefaultValue(elem, itemSchema));
            }
            return "[" + String.join(", ", elements) + "]";
        }
        if (value instanceof Map<?, ?>) {
            List<String> entries = new ArrayList<>();
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
                String key = String.valueOf(entry.getKey());
                Schema<?> propSchema = null;
                if (target != null && target.getProperties() != null) {
                    propSchema = (Schema<?>) target.getProperties().get(key);
                } else if (target != null && ModelUtils.getAdditionalProperties(target) != null) {
                    propSchema = ModelUtils.getAdditionalProperties(target);
                }
                entries.add(formatPythonStringLiteral(key) + ": "
                        + formatPythonDefaultValue(entry.getValue(), propSchema));
            }
            return "{" + String.join(", ", entries) + "}";
        }
        if (value instanceof Boolean || (target != null && ModelUtils.isBooleanSchema(target))) {
            return Boolean.parseBoolean(value.toString()) ? "True" : "False";
        }
        if (value instanceof Number || (target != null && (ModelUtils.isIntegerSchema(target) || ModelUtils.isNumberSchema(target)))) {
            return value.toString();
        }
        return formatPythonStringLiteral(value.toString());
    }

    /**
     * Whether the generated server ships param_parsing.py to deserialize object-typed query/header
     * parameters that Connexion leaves as raw strings (Connexion only parses style=deepObject itself).
     */
    protected boolean supportsObjectParameterParsing() {
        return true;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        super.setParameterExampleValue(codegenParameter, parameter);
        // This hook is the one place that sees both the CodegenParameter and the raw Parameter. The raw one
        // is needed because DefaultCodegen leaves CodegenParameter.style null when the spec omits it and sets
        // isExplode=false even for style=form, whose OpenAPI default is explode=true.
        if (!supportsObjectParameterParsing() || !codegenParameter.isModel
                || !(codegenParameter.isQueryParam || codegenParameter.isHeaderParam)
                // serialized via "content" (e.g. JSON), handled by the JSON header path instead
                || parameter.getContent() != null
                // param_parsing.py only rebuilds plain objects whose properties are all scalars
                // (style=simple/form cannot carry nested objects or arrays); not allOf/oneOf/anyOf
                || !isScalarObjectSchema(ModelUtils.getReferencedSchema(openAPI, parameter.getSchema()))) {
            return;
        }
        // OpenAPI defaults: query -> style=form, header -> style=simple; explode defaults to true only for form.
        String style = parameter.getStyle() != null ? parameter.getStyle().toString()
                : (codegenParameter.isQueryParam ? "form" : "simple");
        boolean supportedStyle = codegenParameter.isQueryParam
                ? "form".equals(style) || "deepObject".equals(style)
                : "simple".equals(style);
        if (!supportedStyle) {
            return;
        }
        boolean explode = parameter.getExplode() != null ? parameter.getExplode() : "form".equals(style);
        codegenParameter.vendorExtensions.put(X_OBJECT_STYLE, style);
        codegenParameter.vendorExtensions.put(X_OBJECT_EXPLODE, explode);
        // simple and non-exploded form put all properties in one comma-separated value
        boolean delimited = !"deepObject".equals(style) && !("form".equals(style) && explode);
        Map<String, String> explicitExample = explicitObjectExample(parameter, delimited);
        if (explicitExample != null) {
            codegenParameter.vendorExtensions.put(X_OBJECT_EXAMPLE, explicitExample);
        }
    }

    /**
     * The example the spec gives for an object parameter (parameter example/examples, then the schema's
     * example or default), as wire values, or null when there is none usable. A candidate is used whole
     * or not at all: one with a null/non-scalar value, a key that is not a property, or (when delimited)
     * a value containing "," or "=" is skipped, rather than sent with parts silently dropped.
     */
    private Map<String, String> explicitObjectExample(Parameter parameter, boolean delimited) {
        List<Object> candidates = new ArrayList<>();
        candidates.add(parameter.getExample());
        if (parameter.getExamples() != null && !parameter.getExamples().isEmpty()) {
            Example first = parameter.getExamples().values().iterator().next();
            candidates.add(first == null ? null : first.getValue());
        }
        Schema<?> schema = ModelUtils.getReferencedSchema(openAPI, parameter.getSchema());
        if (schema == null || schema.getProperties() == null) {
            return null;
        }
        candidates.add(schema.getExample());
        candidates.add(schema.getDefault());
        for (Object candidate : candidates) {
            if (candidate instanceof JsonNode) {
                // swagger-parser keeps object examples as Jackson nodes
                candidate = ((JsonNode) candidate).isObject() ? MAPPER.convertValue(candidate, Map.class) : null;
            }
            if (!(candidate instanceof Map) || ((Map<?, ?>) candidate).isEmpty()) {
                continue;
            }
            Map<String, String> values = new LinkedHashMap<>();
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) candidate).entrySet()) {
                Object value = entry.getValue();
                String key = String.valueOf(entry.getKey());
                if (value == null || value instanceof Map || value instanceof Collection
                        || !schema.getProperties().containsKey(key)) {
                    values = null;
                    break;
                }
                String text = value instanceof Boolean ? value.toString().toLowerCase(Locale.ROOT) : value.toString();
                if (delimited && (text.contains(",") || text.contains("="))) {
                    // OpenAPI defines no escaping for these inside a simple/form value
                    values = null;
                    break;
                }
                values.put(key, text);
            }
            if (values != null && !values.isEmpty()) {
                return values;
            }
        }
        return null;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
        Object openapiName = parameter.vendorExtensions.get("x-python-connexion-openapi-name");
        if (openapiName instanceof String) {
            parameter.baseName = (String) openapiName;
        }
        if (parameter.isHeaderParam || parameter.isCookieParam) {
            boolean isJsonContent = parameter.headerIsJsonMimeType
                    || isJsonMimeType(parameter.contentType)
                    || isJsonVendorMimeType(parameter.contentType);
            if (isJsonContent) {
                parameter.vendorExtensions.put("x-python-connexion-json-content", Boolean.TRUE);
            } else if (parameter.isArray) {
                if (parameter.items == null || !parameter.items.isModel) {
                    parameter.vendorExtensions.put("x-python-connexion-split-csv", Boolean.TRUE);
                }
                if (parameter.items != null) {
                    if (parameter.items.isInteger || parameter.items.isLong) {
                        parameter.vendorExtensions.put("x-python-connexion-cast-int-items", Boolean.TRUE);
                    } else if (parameter.items.isNumber || parameter.items.isFloat || parameter.items.isDouble) {
                        parameter.vendorExtensions.put("x-python-connexion-cast-float-items", Boolean.TRUE);
                    } else if (parameter.items.isBoolean) {
                        parameter.vendorExtensions.put("x-python-connexion-cast-bool-items", Boolean.TRUE);
                    }
                }
            } else if (parameter.isInteger || parameter.isLong) {
                parameter.vendorExtensions.put("x-python-connexion-cast-int", Boolean.TRUE);
            } else if (parameter.isNumber || parameter.isFloat || parameter.isDouble) {
                parameter.vendorExtensions.put("x-python-connexion-cast-float", Boolean.TRUE);
            } else if (parameter.isBoolean) {
                parameter.vendorExtensions.put("x-python-connexion-cast-bool", Boolean.TRUE);
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        postProcessConnexionOperationMap(objs.getOperations(), objs, allModels);
        return objs;
    }

    @Override
    public WebhooksMap postProcessWebhooksWithModels(WebhooksMap objs, List<ModelMap> allModels) {
        // Do not call super to avoid replacing DefaultGenerator imports with Pydantic imports.
        postProcessConnexionOperationMap(objs.getWebhooks(), objs, allModels);
        return objs;
    }

    /**
     * @param objs the enclosing operations/webhooks map; template-level flags (e.g. hasJsonHeaderParam) are set on it
     * @param allModels all models, used to build examples for object-typed parameters
     */
    protected void postProcessConnexionOperationMap(OperationMap operations, Map<String, Object> objs,
                                                    List<ModelMap> allModels) {
        List<CodegenOperation> operationList = operations.getOperation();

        boolean hasJsonHeaderParam = false;
        boolean hasObjectParams = false;
        boolean hasObjectHeaderParam = false;
        for (CodegenOperation operation : operationList) {
            List<CodegenParameter> handlerParams = new ArrayList<>();
            for (CodegenParameter param : operation.allParams) {
                if (!param.isHeaderParam && !param.isCookieParam) {
                    handlerParams.add(param);
                }
                if (Boolean.TRUE.equals(param.vendorExtensions.get("x-python-connexion-json-content"))) {
                    hasJsonHeaderParam = true;
                }
            }
            operation.vendorExtensions.put("x-handler-params", handlerParams);
            // header/cookie names go into single-quoted Python literals in the generated controller and test;
            // {{baseName}} would HTML-escape them (e.g. "X-A&B" -> "X-A&amp;B")
            for (List<CodegenParameter> params : Arrays.asList(operation.allParams, operation.headerParams,
                    operation.cookieParams)) {
                for (CodegenParameter param : params) {
                    param.vendorExtensions.put(X_QUOTED_NAME, escapeTextInSingleQuotes(param.baseName));
                }
            }
            // allParams, queryParams and headerParams hold separate copies of each parameter.
            for (List<CodegenParameter> params : Arrays.asList(operation.allParams, operation.queryParams,
                    operation.headerParams)) {
                for (CodegenParameter param : params) {
                    if (supportsObjectParameterParsing() && param.vendorExtensions.containsKey(X_OBJECT_STYLE)) {
                        hasObjectParams = true;
                        hasObjectHeaderParam |= param.isHeaderParam;
                        prepareObjectParameterExample(param, allModels);
                    }
                }
            }
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
                        // Make an example dictionary more python-like (Booleans True/False).
                        // If fails, use the original string.
                        try {
                            Map<String, Object> result = MAPPER.readValue(example.get("example"),
                                    new TypeReference<Map<String, Object>>() {
                                    });
                            operation.bodyParam.example = MAPPER.writeValueAsString(result);
                        } catch (IOException e) {
                            operation.bodyParam.example = example.get("example");
                        }
                    }
                }
            }
            for (CodegenParameter param : operation.allParams) {
                if ((param.isFreeFormObject || param.isAnyType) && "object".equals(param.dataType)) {
                    param.vendorExtensions.put("x-python-connexion-skip-from-dict", Boolean.TRUE);
                }
            }
        }
        if (hasJsonHeaderParam) {
            objs.put("hasJsonHeaderParam", Boolean.TRUE);
        }
        if (hasObjectHeaderParam) {
            objs.put("hasObjectHeaderParam", Boolean.TRUE);
        }
        if (hasObjectParams) {
            if (!Boolean.TRUE.equals(additionalProperties.get(HAS_OBJECT_PARAMS))) {
                additionalProperties.put(HAS_OBJECT_PARAMS, Boolean.TRUE);
                supportingFiles.add(new SupportingFile("param_parsing.mustache", packagePath(), "param_parsing.py"));
            }
        }
    }

    /**
     * Serializes the example of an object-typed query/header parameter the way the OpenAPI style
     * prescribes, so the generated controller test sends a request the server can deserialize:
     * header (simple) -> "k,v,k2,v2" (or "k=v,k2=v2" when exploded) as the example;
     * query (form, explode) -> one (property, value) pair per property;
     * query (deepObject) -> ("param[property]", value) pairs;
     * query (form, no explode) -> a single ("param", "k,v,k2,v2") pair.
     * Only primitive properties of the model are used.
     */
    private void prepareObjectParameterExample(CodegenParameter param, List<ModelMap> allModels) {
        CodegenModel model = null;
        String type = param.baseType != null ? param.baseType : param.dataType;
        for (ModelMap mm : allModels) {
            if (mm.getModel() != null && mm.getModel().classname.equals(type)) {
                model = mm.getModel();
                break;
            }
        }
        String style = (String) param.vendorExtensions.get(X_OBJECT_STYLE);
        boolean explode = Boolean.TRUE.equals(param.vendorExtensions.get(X_OBJECT_EXPLODE));
        boolean delimited = param.isHeaderParam || ("form".equals(style) && !explode);
        List<String[]> values = new ArrayList<>();
        @SuppressWarnings("unchecked")
        Map<String, String> explicitExample = (Map<String, String>) param.vendorExtensions.get(X_OBJECT_EXAMPLE);
        if (explicitExample != null) {
            for (Map.Entry<String, String> entry : explicitExample.entrySet()) {
                values.add(new String[]{entry.getKey(), entry.getValue()});
            }
        } else if (model != null) {
            for (CodegenProperty var : model.vars) {
                String value = wireExampleValue(var, delimited);
                if (value != null) {
                    values.add(new String[]{var.baseName, value});
                }
            }
        }
        if (delimited) {
            List<String> parts = new ArrayList<>();
            for (String[] kv : values) {
                parts.add(explode ? kv[0] + "=" + kv[1] : kv[0] + "," + kv[1]);
            }
            param.example = "'" + escapeTextInSingleQuotes(String.join(",", parts)) + "'";
            if (param.isQueryParam) {
                param.vendorExtensions.put("x-python-connexion-test-query-pairs",
                        Collections.singletonList(testPair(escapeTextInSingleQuotes(param.baseName), param.example)));
            }
            return;
        }
        List<Map<String, String>> pairs = new ArrayList<>();
        for (String[] kv : values) {
            String key = "deepObject".equals(style) ? param.baseName + "[" + kv[0] + "]" : kv[0];
            pairs.add(testPair(escapeTextInSingleQuotes(key), "'" + escapeTextInSingleQuotes(kv[1]) + "'"));
        }
        param.vendorExtensions.put("x-python-connexion-test-query-pairs", pairs);
    }

    /**
     * A "type: object" schema that is not composed and whose properties are all scalars (string, integer,
     * number, boolean, including enums and dates). "type" is required because Connexion itself reads
     * param_schema['type'] while parsing query parameters. Must match _is_object() in param_parsing.mustache.
     */
    private boolean isScalarObjectSchema(Schema<?> schema) {
        if (schema == null || ModelUtils.isComposedSchema(schema)
                || schema.getProperties() == null || schema.getProperties().isEmpty()) {
            return false;
        }
        if (!"object".equals(schemaType(schema))) {
            return false;
        }
        for (Schema<?> property : schema.getProperties().values()) {
            String propertyType = schemaType(ModelUtils.getReferencedSchema(openAPI, property));
            if (!"string".equals(propertyType) && !"integer".equals(propertyType)
                    && !"number".equals(propertyType) && !"boolean".equals(propertyType)) {
                return false;
            }
        }
        return true;
    }

    private static String schemaType(Schema<?> schema) {
        if (schema == null) {
            return null;
        }
        if (schema.getType() != null) {
            return schema.getType();
        }
        if (schema.getTypes() != null && schema.getTypes().size() == 1) {
            return schema.getTypes().iterator().next();
        }
        return null;
    }

    private static Map<String, String> testPair(String key, String value) {
        Map<String, String> pair = new HashMap<>();
        pair.put("key", key);
        pair.put("value", value);
        return pair;
    }

    /**
     * Example value of a primitive model property as it appears on the wire, or null to leave it out.
     * OpenAPI defines no escaping for "," and "=" inside style=simple/form values, so an example
     * containing them is replaced by a placeholder rather than producing a request that parses wrongly.
     */
    private String wireExampleValue(CodegenProperty var, boolean delimited) {
        String value = rawWireExampleValue(var);
        if (delimited && value != null && (value.contains(",") || value.contains("="))) {
            return var.isString ? var.baseName + "_example" : null;
        }
        return value;
    }

    private String rawWireExampleValue(CodegenProperty var) {
        if (var.isBoolean) {
            return var.example != null ? var.example.toLowerCase(Locale.ROOT) : "true";
        }
        if (var.isInteger || var.isLong) {
            return var.example != null ? var.example : "56";
        }
        if (var.isNumber || var.isFloat || var.isDouble) {
            return var.example != null ? var.example : "3.4";
        }
        // Property examples are Python literals here (e.g. 'abc'); the wire value is the bare text.
        String example = var.example;
        if (example != null && example.length() >= 2 && example.startsWith("'") && example.endsWith("'")) {
            example = example.substring(1, example.length() - 1).replace("\\'", "'");
        }
        if (var.isEnum && var._enum != null && !var._enum.isEmpty()) {
            return var._enum.contains(example) ? example : var._enum.get(0);
        }
        if (var.isString || var.isDate || var.isDateTime) {
            if (var.isDate && (example == null || !example.matches("\\d{4}-\\d{2}-\\d{2}"))) {
                return "2013-10-20";
            }
            if (var.isDateTime && (example == null || !example.matches("\\d{4}-\\d{2}-\\d{2}T.*"))) {
                return "2013-10-20T19:20:30+01:00";
            }
            return StringUtils.isEmpty(example) ? var.baseName + "_example" : example;
        }
        return null;
    }

    /*
     * The openapi pattern spec follows the Perl convention and style of modifiers. Python
     * does not support this in as natural a way so it needs to convert it. See
     * https://docs.python.org/2/howto/regex.html#compilation-flags for details.
     */
    @Override
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            pattern = addRegularExpressionDelimiter(pattern);
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

            vendorExtensions.put(X_REGEX, regex);
            vendorExtensions.put(X_MODIFIERS, modifiers);
        }
    }
}
