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

import com.google.common.annotations.VisibleForTesting;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;

public abstract class AbstractJavaJAXRSServerCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    public static final String SERVER_PORT = "serverPort";
    public static final String USE_TAGS = "useTags";

    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    protected static final String X_MICROPROFILE_OPEN_API_RETURN_SCHEMA_CONTAINER = "x-microprofile-open-api-return-schema-container";
    protected static final String X_MICROPROFILE_OPEN_API_RETURN_UNIQUE_ITEMS = "x-microprofile-open-api-return-unique-items";
    protected static final String X_MICROPROFILE_OPEN_API_SCHEMA_TYPE = "x-microprofile-open-api-schema-type";
    protected static final String SCHEMA_TYPE_ARRAY = "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY";
    protected static final Map<String,String> ARRAY_OF_MICROPROFILE_OPEN_API_SCHEMA_TYPES;
    static {
        final Map<String, String> schemaTypes = new HashMap<>();
        schemaTypes.put("integer", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.INTEGER");
        schemaTypes.put("number", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.NUMBER");
        schemaTypes.put("boolean", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.BOOLEAN");
        schemaTypes.put("string", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.STRING");
        schemaTypes.put("object", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.OBJECT");
        schemaTypes.put("array", "org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY");
        ARRAY_OF_MICROPROFILE_OPEN_API_SCHEMA_TYPES = Collections.unmodifiableMap(schemaTypes);
    }

    protected String implFolder = "src/main/java";
    protected String testResourcesFolder = "src/test/resources";
    protected String title = "OpenAPI Server";
    protected String serverPort = "8080";

    protected boolean useBeanValidation = true;
    protected boolean useTags = false;

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractJavaJAXRSServerCodegen.class);

    public AbstractJavaJAXRSServerCodegen() {
        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-server";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(DATE_LIBRARY, this.getDateLibrary());
        updateOption(CodegenConstants.SOURCE_FOLDER, this.getSourceFolder());

        additionalProperties.put("title", title);
        // java inflector uses the jackson lib
        additionalProperties.put(JACKSON, "true");

        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC).defaultValue(implFolder));
        cliOptions.add(new CliOption("title", "a title describing the application").defaultValue(title));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations",useBeanValidation));
        cliOptions.add(new CliOption(SERVER_PORT, "The port on which the server should be started").defaultValue(serverPort));
        cliOptions.add(CliOption.newBoolean(USE_TAGS, "use tags for creating interface and controller classnames"));
    }


    // ===============
    // COMMONS METHODS
    // ===============

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.IMPL_FOLDER)) {
            implFolder = (String) additionalProperties.get(CodegenConstants.IMPL_FOLDER);
        }

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        if (additionalProperties.containsKey(USE_TAGS)) {
            setUseTags(convertPropertyToBoolean(USE_TAGS));
        }

        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        final String basePath = StringUtils.substringBefore(StringUtils.removeStart(resourcePath, "/"), "/");
        if (!StringUtils.isEmpty(basePath)) {
            co.subresourceOperation = !co.path.isEmpty();
        }
        if (useTags) {
            super.addOperationToGroup(tag, resourcePath, operation, co, operations);
        } else {
            co.baseName = basePath;
            if (StringUtils.isEmpty(co.baseName) || StringUtils.containsAny(co.baseName, "{", "}")) {
                co.baseName = "default";
            }
            final List<CodegenOperation> opList = operations.computeIfAbsent(co.baseName, k -> new ArrayList<>());
            opList.add(co);
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /* TODO there should be no need for the following logic
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }
        */

        if (!this.additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
            // 8080 is the default value for a JEE Server:
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, serverPort));
        }

        if (openAPI.getPaths() != null) {
            for (Map.Entry<String, PathItem> openAPIGetPathsEntry : openAPI.getPaths().entrySet()) {
                String pathname = openAPIGetPathsEntry.getKey();
                PathItem path = openAPIGetPathsEntry.getValue();
                if (path.readOperations() != null) {
                    for (Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                tags.add(value);
                            }
                            if (operation.getTags().size() > 0) {
                                String tag = operation.getTags().get(0);
                                operation.setTags(Arrays.asList(tag));
                            }
                            operation.addExtension("x-tags", tags);
                        }
                    }
                }
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationsMap updatedObjs = jaxrsPostProcessOperations(objs);
        OperationMap operations = updatedObjs.getOperations();
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation co : ops) {
                handleImplicitHeaders(co);
            }
        }
        return updatedObjs;
    }

    static OperationsMap jaxrsPostProcessOperations(OperationsMap objs) {
        OperationMap operations = objs.getOperations();
        String commonPath = null;
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                if (operation.hasConsumes == Boolean.TRUE) {
                    Map<String, String> firstType = operation.consumes.get(0);
                    if (firstType != null) {
                        if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                            operation.isMultipart = Boolean.TRUE;
                        }
                    }
                }

                boolean isMultipartPost = false;
                List<Map<String, String>> consumes = operation.consumes;
                if (consumes != null) {
                    for (Map<String, String> consume : consumes) {
                        String mt = consume.get("mediaType");
                        if (mt != null) {
                            if (mt.startsWith("multipart/form-data")) {
                                isMultipartPost = true;
                            }
                        }
                    }
                }

                for (CodegenParameter parameter : operation.allParams) {
                    if (isMultipartPost) {
                        parameter.vendorExtensions.put("x-multipart", "true");
                    }
                }

                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    for (CodegenResponse resp : responses) {
                        if ("0".equals(resp.code)) {
                            resp.code = "200";
                        }

                        if (resp.baseType == null) {
                            resp.dataType = "void";
                            resp.baseType = "Void";
                            // set vendorExtensions.x-java-is-response-void to true as baseType is set to "Void"
                            resp.vendorExtensions.put("x-java-is-response-void", true);
                        }

                        if ("array".equals(resp.containerType)) {
                            resp.containerType = "List";
                            resp.vendorExtensions.put(X_MICROPROFILE_OPEN_API_RETURN_SCHEMA_CONTAINER, SCHEMA_TYPE_ARRAY);
                        } else if ("set".equals(resp.containerType)) {
                            resp.containerType = "Set";
                            resp.vendorExtensions.put(X_MICROPROFILE_OPEN_API_RETURN_SCHEMA_CONTAINER, SCHEMA_TYPE_ARRAY);
                            resp.vendorExtensions.put(X_MICROPROFILE_OPEN_API_RETURN_UNIQUE_ITEMS, true);
                        } else if ("map".equals(resp.containerType)) {
                            resp.containerType = "Map";
                        }

                        if (resp.getResponseHeaders() != null) {
                            handleHeaders(resp.getResponseHeaders());
                        }
                    }
                }

                if (operation.returnBaseType == null) {
                    operation.returnType = "void";
                    operation.returnBaseType = "Void";
                    // set vendorExtensions.x-java-is-response-void to true as returnBaseType is set to "Void"
                    operation.vendorExtensions.put("x-java-is-response-void", true);
                }

                if ("array".equals(operation.returnContainer)) {
                    operation.returnContainer = "List";
                } else if ("set".equals(operation.returnContainer)) {
                    operation.returnContainer = "Set";
                } else if ("map".equals(operation.returnContainer)) {
                    operation.returnContainer = "Map";
                }

                if (commonPath == null) {
                    commonPath = operation.path;
                } else {
                    commonPath = getCommonPath(commonPath, operation.path);
                }
            }
            for (CodegenOperation co : ops) {
                co.path = StringUtils.removeStart(co.path, commonPath);
                co.subresourceOperation = co.path.length() > 1;
            }
            objs.put("commonPath", "/".equals(commonPath) ? StringUtils.EMPTY : commonPath);
        }
        return objs;
    }

    private static void handleHeaders(List<CodegenParameter> headers) {
        for (CodegenParameter header : headers) {
            if (header.getSchema() != null && header.getSchema().getOpenApiType() != null) {
                final String schemaType = ARRAY_OF_MICROPROFILE_OPEN_API_SCHEMA_TYPES.get(header.getSchema().getOpenApiType());
                if (schemaType != null) {
                    header.vendorExtensions.put(X_MICROPROFILE_OPEN_API_SCHEMA_TYPE, schemaType);
                }
            }
        }
    }

    @Override
    public String toApiName(final String name) {
        String computed = name;
        if (computed.length() > 0) {
            computed = sanitizeName(computed);
        }
        return super.toApiName(computed);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);

        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf(File.separator);
            result = result.substring(0, ix) + "/impl" + result.substring(ix, result.length() - 5) + "ServiceImpl.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Factory.mustache")) {
            int ix = result.lastIndexOf(File.separator);
            result = result.substring(0, ix) + "/factories" + result.substring(ix, result.length() - 5) + "ServiceFactory.java";
            result = result.replace(apiFileFolder(), implFileFolder(implFolder));
        } else if (templateName.endsWith("Service.mustache")) {
            int ix = result.lastIndexOf('.');
            result = result.substring(0, ix) + "Service.java";
        }
        return result;
    }

    private static String getCommonPath(String path1, String path2) {
        final String[] parts1 = StringUtils.split(path1, "/");
        final String[] parts2 = StringUtils.split(path2, "/");
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < Math.min(parts1.length, parts2.length); i++) {
            if (!parts1[i].equals(parts2[i])) {
                break;
            }
            builder.append("/").append(parts1[i]);
        }
        return builder.toString();
    }

    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    @Override
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

    @VisibleForTesting
    public void setUseTags(boolean useTags) {
        this.useTags = useTags;
    }
}
