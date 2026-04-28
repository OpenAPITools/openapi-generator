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

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import org.apache.commons.lang3.Strings;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.underscore;

/**
 * <p>Mustache templates are located in {@code src/main/resources/python-fastapi/}.
 */
public class PythonFastAPIServerCodegen extends AbstractPythonCodegen {
    final Logger LOGGER = LoggerFactory.getLogger(PythonFastAPIServerCodegen.class);

    protected String sourceFolder;

    private static final String BASE_CLASS_SUFFIX = "base";
    private static final String SERVER_PORT = "serverPort";
    private static final String NAME = "python-fastapi";
    private static final int DEFAULT_SERVER_PORT = 8080;
    private static final String DEFAULT_PACKAGE_NAME = "openapi_server";
    private static final String DEFAULT_SOURCE_FOLDER = "src";
    private static final String DEFAULT_IMPL_FOLDER = "impl";
    private static final String DEFAULT_PACKAGE_VERSION = "1.0.0";
    private static final String X_FASTAPI_REQUEST_BODY_EXAMPLE = "x-python-fastapi-request-body-example";

    private String implPackage;

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getHelp() {
        return "Generates a Python FastAPI server (beta). Models are defined with the pydantic library";
    }

    public PythonFastAPIServerCodegen() {
        super();

        setSkipSortingOperations(true);

        modifyFeatureSet(features -> features.includeSecurityFeatures(
                SecurityFeature.OAuth2_AuthorizationCode,
                SecurityFeature.OAuth2_Password
        ));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata).stability(Stability.BETA).build();

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("serverPort", DEFAULT_SERVER_PORT);
        additionalProperties.put("baseSuffix", BASE_CLASS_SUFFIX);
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, DEFAULT_SOURCE_FOLDER);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, DEFAULT_PACKAGE_NAME);
        additionalProperties.put(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE, DEFAULT_IMPL_FOLDER);

        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        typeMapping.put("array", "List");
        typeMapping.put("map", "Dict");

        outputFolder = "generated-code" + File.separator + NAME;
        modelTemplateFiles.put("model.mustache", ".py");
        apiTemplateFiles.put("api.mustache", ".py");
        apiTemplateFiles.put("base_api.mustache", "_".concat(BASE_CLASS_SUFFIX).concat(".py"));
        embeddedTemplateDir = templateDir = NAME;
        apiPackage = "apis";
        modelPackage = "models";
        testPackage = "tests";
        implPackage = DEFAULT_IMPL_FOLDER;
        apiTestTemplateFiles().put("api_test.mustache", ".py");

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "python package name (convention: snake_case).")
                .defaultValue(DEFAULT_PACKAGE_NAME));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "python package version.")
                .defaultValue(DEFAULT_PACKAGE_VERSION));
        cliOptions.add(new CliOption(SERVER_PORT, "TCP port to listen to in app.run")
                .defaultValue(String.valueOf(DEFAULT_SERVER_PORT)));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, "directory for generated python source code")
                .defaultValue(DEFAULT_SOURCE_FOLDER));
        cliOptions.add(new CliOption(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE, "python package name for the implementation code (convention: snake_case).")
                .defaultValue(implPackage));
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (openAPI == null || openAPI.getPaths() == null) {
            return;
        }

        for (PathItem pathItem : openAPI.getPaths().values()) {
            for (Operation operation : pathItem.readOperations()) {
                Object example = getExplicitRequestBodyExample(operation.getRequestBody());
                if (example != null) {
                    operation.addExtension(X_FASTAPI_REQUEST_BODY_EXAMPLE, example);
                }
            }
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Skip sorting of operations via setSkipSortingOperations(true) in the constructor to preserve the order
        // found in the OpenAPI spec file.  See
        // https://fastapi.tiangolo.com/tutorial/path-params/?h=path#order-matters for details on why order matters.
        LOGGER.info("Skipping sorting of path operations, order matters, let the developer decide via their specification file.");

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.sourceFolder = ((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        if (additionalProperties.containsKey(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE)) {
            this.implPackage = ((String) additionalProperties.get(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE));
            // Prefix templating value with the package name
            additionalProperties.put(CodegenConstants.FASTAPI_IMPLEMENTATION_PACKAGE,
                    this.packageName + "." + this.implPackage);
        }

        modelPackage = packageName + "." + modelPackage;
        apiPackage = packageName + "." + apiPackage;
        implPackage = packageName + "." + implPackage;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("main.mustache", String.join(File.separator, new String[]{sourceFolder, packageName.replace('.', File.separatorChar)}), "main.py"));
        supportingFiles.add(new SupportingFile("docker-compose.mustache", "", "docker-compose.yaml"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("requirements.mustache", "", "requirements.txt"));
        supportingFiles.add(new SupportingFile("security_api.mustache", String.join(File.separator, new String[]{sourceFolder, packageName.replace('.', File.separatorChar)}), "security_api.py"));
        supportingFiles.add(new SupportingFile("extra_models.mustache", StringUtils.substringAfter(modelFileFolder(), outputFolder), "extra_models.py"));

        // Add __init__.py to all sub-folders under namespace pkg
        StringBuilder namespacePackagePath = new StringBuilder(String.join(File.separator, new String[]{sourceFolder, StringUtils.substringBefore(packageName, ".")}));
        for (String tmp : StringUtils.split(StringUtils.substringAfter(packageName, "."), '.')) {
            namespacePackagePath.append(File.separator).append(tmp);
            supportingFiles.add(new SupportingFile("__init__.mustache", namespacePackagePath.toString(), "__init__.py"));
        }
        supportingFiles.add(new SupportingFile("__init__.mustache", StringUtils.substringAfter(modelFileFolder(), outputFolder), "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__.mustache", StringUtils.substringAfter(apiFileFolder(), outputFolder), "__init__.py"));
        supportingFiles.add(new SupportingFile("__init__.mustache", StringUtils.substringAfter(apiImplFileFolder(), outputFolder), "__init__.py"));

        supportingFiles.add(new SupportingFile("conftest.mustache", testPackage.replace('.', File.separatorChar), "conftest.py"));

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("pyproject_toml.mustache", "", "pyproject.toml"));
        supportingFiles.add(new SupportingFile("setup_cfg.mustache", "", "setup.cfg"));
        supportingFiles.add(new SupportingFile(".flake8.mustache", "", ".flake8"));
    }

    @Override
    public String getName() {
        return NAME;
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
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);

        OperationMap operations = objs.getOperations();
        // Set will make sure that no duplicated items are used.
        Set<String> securityImports = new HashSet<>();
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (final CodegenOperation operation : ops) {
                List<CodegenSecurity> securityMethods = operation.authMethods;
                if (securityMethods != null) {
                    for (final CodegenSecurity securityMethod : securityMethods) {
                        securityImports.add(securityMethod.name);
                    }
                }

                setBodyParamExampleFromContent(operation);
            }
        }

        objs.put("securityImports", new ArrayList<>(securityImports));

        return objs;
    }

    private void setBodyParamExampleFromContent(CodegenOperation operation) {
        if (operation.bodyParam == null) {
            return;
        }

        Operation sourceOperation = findOpenAPIOperation(operation);
        if (sourceOperation == null || sourceOperation.getExtensions() == null) {
            clearBodyParamExample(operation);
            return;
        }

        Object example = sourceOperation.getExtensions().get(X_FASTAPI_REQUEST_BODY_EXAMPLE);
        if (example != null) {
            setBodyParamExample(operation, toPythonBodyLiteral(example));
        } else {
            clearBodyParamExample(operation);
        }
    }

    private Object getExplicitRequestBodyExample(RequestBody requestBody) {
        if (requestBody == null) {
            return null;
        }
        if (requestBody.get$ref() != null && openAPI != null && openAPI.getComponents() != null
                && openAPI.getComponents().getRequestBodies() != null) {
            requestBody = openAPI.getComponents().getRequestBodies().get(ModelUtils.getSimpleRef(requestBody.get$ref()));
        }
        if (requestBody == null || requestBody.getContent() == null || requestBody.getContent().get("application/json") == null) {
            return null;
        }

        MediaType mediaType = requestBody.getContent().get("application/json");
        Object example = mediaType.getExample();
        if (example == null && mediaType.getExamples() != null && !mediaType.getExamples().isEmpty()) {
            Example exampleObject = mediaType.getExamples().values().iterator().next();
            if (exampleObject.get$ref() != null && openAPI != null && openAPI.getComponents() != null && openAPI.getComponents().getExamples() != null) {
                Example referencedExample = openAPI.getComponents().getExamples().get(ModelUtils.getSimpleRef(exampleObject.get$ref()));
                if (referencedExample != null) {
                    example = referencedExample.getValue();
                }
            } else {
                example = exampleObject.getValue();
            }
        }

        return copyExample(example);
    }

    private Object copyExample(Object example) {
        if (example == null) {
            return null;
        }
        return Json.mapper().convertValue(example, Object.class);
    }

    private Operation findOpenAPIOperation(CodegenOperation operation) {
        if (openAPI == null || openAPI.getPaths() == null) {
            return null;
        }

        PathItem pathItem = openAPI.getPaths().get(operation.path);
        if (pathItem == null) {
            return null;
        }

        for (Map.Entry<PathItem.HttpMethod, Operation> entry : pathItem.readOperationsMap().entrySet()) {
            if (operation.httpMethod != null && operation.httpMethod.equalsIgnoreCase(entry.getKey().name())) {
                return entry.getValue();
            }
        }

        for (Operation sourceOperation : pathItem.readOperations()) {
            if (operation.operationIdOriginal != null && operation.operationIdOriginal.equals(sourceOperation.getOperationId())) {
                return sourceOperation;
            }
            if (operation.operationId != null && operation.operationId.equals(sourceOperation.getOperationId())) {
                return sourceOperation;
            }
        }

        return null;
    }

    private void clearBodyParamExample(CodegenOperation operation) {
        operation.bodyParam.vendorExtensions.remove("x-py-example");
        operation.bodyParam.vendorExtensions.remove("x-py-fastapi-example");
        for (CodegenParameter param : operation.allParams) {
            if (param.isBodyParam || Objects.equals(param.paramName, operation.bodyParam.paramName)) {
                param.vendorExtensions.remove("x-py-example");
                param.vendorExtensions.remove("x-py-fastapi-example");
            }
        }
        for (CodegenParameter param : operation.bodyParams) {
            if (param.isBodyParam || Objects.equals(param.paramName, operation.bodyParam.paramName)) {
                param.vendorExtensions.remove("x-py-example");
                param.vendorExtensions.remove("x-py-fastapi-example");
            }
        }
    }

    private void setBodyParamExample(CodegenOperation operation, String example) {
        operation.bodyParam.vendorExtensions.remove("x-py-example");
        operation.bodyParam.vendorExtensions.put("x-py-fastapi-example", example);
        for (CodegenParameter param : operation.allParams) {
            if (param.isBodyParam || Objects.equals(param.paramName, operation.bodyParam.paramName)) {
                param.vendorExtensions.remove("x-py-example");
                param.vendorExtensions.put("x-py-fastapi-example", example);
            }
        }
        for (CodegenParameter param : operation.bodyParams) {
            if (param.isBodyParam || Objects.equals(param.paramName, operation.bodyParam.paramName)) {
                param.vendorExtensions.remove("x-py-example");
                param.vendorExtensions.put("x-py-fastapi-example", example);
            }
        }
    }

    private String toPythonBodyLiteral(Object value) {
        if (value == null) {
            return "None";
        }
        if (value instanceof String) {
            return toPythonStringLiteral((String) value);
        }
        if (value instanceof Boolean) {
            return (Boolean) value ? "True" : "False";
        }
        if (value instanceof Number) {
            return value.toString();
        }
        if (value instanceof Map) {
            List<String> entries = new ArrayList<>();
            for (Map.Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
                entries.add(toPythonStringLiteral(String.valueOf(entry.getKey())) + ": " + toPythonBodyLiteral(entry.getValue()));
            }
            return "{" + StringUtils.join(entries, ", ") + "}";
        }
        if (value instanceof Iterable) {
            List<String> items = new ArrayList<>();
            for (Object item : (Iterable<?>) value) {
                items.add(toPythonBodyLiteral(item));
            }
            return "[" + StringUtils.join(items, ", ") + "]";
        }

        return toPythonStringLiteral(String.valueOf(value));
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> result = super.postProcessAllModels(objs);
        for (Map.Entry<String, ModelsMap> entry : result.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
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
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return objs;
    }

    @Override
    public String apiFileFolder() {
        return String.join(File.separator, new String[]{outputFolder, sourceFolder, apiPackage().replace('.', File.separatorChar)});
    }

    public String apiImplFileFolder() {
        return String.join(File.separator, new String[]{outputFolder, sourceFolder, implPackage.replace('.', File.separatorChar)});
    }

    @Override
    public String modelFileFolder() {
        return String.join(File.separator, new String[]{outputFolder, sourceFolder, modelPackage().replace('.', File.separatorChar)});
    }

    @Override
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator's contributed by Nikita Vakula (https://github.com/krjakbrjak)#");
        System.out.println("# Please support his work directly via https://paypal.me/krjakbrjak  \uD83D\uDE4F        #");
        System.out.println("################################################################################");
    }

    @Override
    public String generatorLanguageVersion() {
        return "3.10";
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "var_" + name;
    }
}
