/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractJavaJAXRSServerCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    public static final String SERVER_PORT = "serverPort";
    /**
     * Name of the sub-directory in "src/main/resource" where to find the
     * Mustache template for the JAX-RS Codegen.
     */
    protected static final String JAXRS_TEMPLATE_DIRECTORY_NAME = "JavaJaxRS";
    protected String implFolder = "src/main/java";
    protected String testResourcesFolder = "src/test/resources";
    protected String title = "OpenAPI Server";
    protected String serverPort = "8080";
    protected boolean useBeanValidation = true;

    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractJavaJAXRSServerCodegen.class);

    public AbstractJavaJAXRSServerCodegen() {
        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "org.openapitools.api";
        artifactId = "openapi-jaxrs-server";
        dateLibrary = "legacy"; //TODO: add joda support to all jax-rs
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";

        // clioOptions default redifinition need to be updated
        updateOption(CodegenConstants.INVOKER_PACKAGE, this.getInvokerPackage());
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(this.DATE_LIBRARY, this.getDateLibrary());

        additionalProperties.put("title", title);
        // java inflector uses the jackson lib
        additionalProperties.put("jackson", "true");

        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC).defaultValue(implFolder));
        cliOptions.add(new CliOption("title", "a title describing the application").defaultValue(title));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations",useBeanValidation));
        cliOptions.add(new CliOption(SERVER_PORT, "The port on which the server should be started").defaultValue(serverPort));
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
            this.setUseBeanValidation(convertPropertyToBoolean(USE_BEANVALIDATION));
        }

        writePropertyBack(USE_BEANVALIDATION, useBeanValidation);

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
            URL url = URLPathUtils.getServerURL(openAPI);
            // 8080 is the default value for a JEE Server:
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, serverPort));
        }

        if (openAPI.getPaths() != null) {
            for (String pathname : openAPI.getPaths().keySet()) {
                PathItem path = openAPI.getPaths().get(pathname);
                if (path.readOperations() != null) {
                    for (Operation operation : path.readOperations()) {
                        if (operation.getTags() != null) {
                            List<Map<String, String>> tags = new ArrayList<Map<String, String>>();
                            for (String tag : operation.getTags()) {
                                Map<String, String> value = new HashMap<String, String>();
                                value.put("tag", tag);
                                value.put("hasMore", "true");
                                tags.add(value);
                            }
                            if (tags.size() > 0) {
                                tags.get(tags.size() - 1).remove("hasMore");
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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        return jaxrsPostProcessOperations(objs);
    }

    static Map<String, Object> jaxrsPostProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            String commonBaseName = null;
            boolean baseNameEquals = true;
            @SuppressWarnings("unchecked")
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
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
                        } else if ("map".equals(resp.containerType)) {
                            resp.containerType = "Map";
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
                } else if ("map".equals(operation.returnContainer)) {
                    operation.returnContainer = "Map";
                }
                
                if(commonBaseName == null) {
                    commonBaseName = operation.baseName;
                } else if(!commonBaseName.equals(operation.baseName)) {
                    baseNameEquals = false;
                }
            }
            if(baseNameEquals) {
                objs.put("commonPath", commonBaseName);
            } else {
                for (CodegenOperation operation : ops) {
                    if(operation.baseName != null) {
                        operation.path = "/" + operation.baseName + operation.path;
                        operation.baseName = null;
                    }
                }
                objs.put("commonPath", null);
            }
        }
        return objs;
    }

    @Override
    public String toApiName(final String name) {
        String computed = name;
        if (computed.length() == 0) {
            return "DefaultApi";
        }
        computed = sanitizeName(computed);
        return camelize(computed) + "Api";
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

    private String implFileFolder(String output) {
        return outputFolder + "/" + output + "/" + apiPackage().replace('.', '/');
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }


}
