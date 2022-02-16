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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.PathItem.HttpMethod;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import org.openapitools.codegen.utils.URLPathUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JavaVertXServerCodegen extends AbstractJavaCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(JavaVertXServerCodegen.class);

    protected String resourceFolder = "src/main/resources";
    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    public static final String ROOT_PACKAGE = "rootPackage";

    public static final String RX_INTERFACE_OPTION = "rxInterface";
    public static final String RX_VERSION_2_OPTION = "rxVersion2";
    public static final String VERTX_SWAGGER_ROUTER_VERSION_OPTION = "vertxSwaggerRouterVersion";

    /**
     * A Java Vert.X generator. It uses java8 date API. It can be configured with 2 CLI options :
     * <p>
     * rxInterface : type Boolean if true, API interfaces are generated with RX and methods return
     * Single and Comparable. default : false
     * <p>
     * vertxSwaggerRouterVersion : type String Specify the version of the swagger router library
     */
    public JavaVertXServerCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
            .stability(Stability.DEPRECATED)
            .build();

        // set the output folder here
        outputFolder = "generated-code" + File.separator + "javaVertXServer";

        modelTemplateFiles.clear();
        modelTemplateFiles.put("model.mustache", ".java");

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiVerticle.mustache", "Verticle.java");
        apiTemplateFiles.put("apiException.mustache", "Exception.java");

        embeddedTemplateDir = templateDir = "JavaVertXServer";

        apiPackage = rootPackage + ".verticle";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-java-vertx-server";
        artifactVersion = apiVersion;
        this.setDateLibrary("java8");

        // clioOptions default redefinition need to be updated
        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.ARTIFACT_VERSION, this.getArtifactVersion());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(DATE_LIBRARY, this.getDateLibrary());

        additionalProperties.put(ROOT_PACKAGE, rootPackage);

        cliOptions.add(CliOption.newBoolean(RX_INTERFACE_OPTION,
                "When specified, API interfaces are generated with RX "
                        + "and methods return Single<> and Comparable."));
        cliOptions.add(CliOption.newBoolean(RX_VERSION_2_OPTION,
                "When specified in combination with rxInterface, "
                        + "API interfaces are generated with RxJava2."));
        cliOptions.add(CliOption.newString(VERTX_SWAGGER_ROUTER_VERSION_OPTION,
                "Specify the version of the swagger router library"));

    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the generator to select
     * the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "java-vertx";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with help tips,
     * parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a java-Vert.X Server library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        LOGGER.warn("IMPORTANT: This generator has been deprecated. Please use `java-vertx-web` instead");

        apiTestTemplateFiles.clear();

        importMapping.remove("JsonCreator");
        importMapping.remove("com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonInclude", "com.fasterxml.jackson.annotation.JsonInclude");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("MainApiException", rootPackage + ".MainApiException");

        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("openapi.mustache", resourceFolder, "openapi.json"));
        supportingFiles.add(new SupportingFile("MainApiVerticle.mustache",
                sourceFolder + File.separator + rootPackage.replace(".", File.separator),
                "MainApiVerticle.java"));
        supportingFiles.add(new SupportingFile("MainApiException.mustache",
                sourceFolder + File.separator + rootPackage.replace(".", File.separator),
                "MainApiException.java"));

        supportingFiles.add(new SupportingFile("vertx-default-jul-logging.mustache",
                resourceFolder, "vertx-default-jul-logging.properties")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
                .doNotOverwrite());
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!model.isEnum) {
            model.imports.add("JsonInclude");
            model.imports.add("JsonProperty");
            if (model.hasEnums) {
                model.imports.add("JsonValue");
            }
        }

    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> newObjs = super.postProcessOperationsWithModels(objs, allModels);
        Map<String, Object> operations = (Map<String, Object>) newObjs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                operation.httpMethod = operation.httpMethod.toLowerCase(Locale.ROOT);

                if ("Void".equalsIgnoreCase(operation.returnType)) {
                    operation.returnType = null;
                }

                if (operation.getHasPathParams()) {
                    operation.path = camelizePath(operation.path);
                }

            }
        }
        return newObjs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }


    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation codegenOperation =
                super.fromOperation(path, httpMethod, operation, servers);
        codegenOperation.imports.add("MainApiException");
        return codegenOperation;
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);
        codegenModel.imports.remove("ApiModel");
        codegenModel.imports.remove("ApiModelProperty");
        return codegenModel;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        // add server port from the swagger file, 8080 by default
        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        this.additionalProperties.put("serverPort", URLPathUtils.getPort(url, 8080));

        // retrieve api version from swagger file, 1.0.0-SNAPSHOT by default
        // set in super.preprocessOpenAPI
        /*
        if (openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
            artifactVersion = apiVersion = openAPI.getInfo().getVersion();
        } else {
            artifactVersion = apiVersion;
        }*/

        /*
         * manage operation & custom serviceId because operationId field is not
         * required and may be empty
         */
        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            for (Entry<String, PathItem> entry : paths.entrySet()) {
                manageOperationNames(entry.getValue(), entry.getKey());
            }
        }
        this.additionalProperties.remove("gson");
    }

    private void manageOperationNames(PathItem path, String pathname) {
        String serviceIdTemp;

        Map<HttpMethod, Operation> operationMap = path.readOperationsMap();
        if (operationMap != null) {
            for (Entry<HttpMethod, Operation> entry : operationMap.entrySet()) {
                serviceIdTemp = computeServiceId(pathname, entry);
                entry.getValue().addExtension("x-serviceid", serviceIdTemp);
                entry.getValue().addExtension("x-serviceid-varname",
                        serviceIdTemp.toUpperCase(Locale.ROOT) + "_SERVICE_ID");
            }
        }
    }

    private String computeServiceId(String pathname, Entry<HttpMethod, Operation> entry) {
        String operationId = entry.getValue().getOperationId();
        return (operationId != null) ? operationId
                : entry.getKey().name()
                + pathname.replaceAll("-", "_").replaceAll("/", "_").replaceAll("[{}]", "");
    }

    protected String extractPortFromHost(String host) {
        if (host != null) {
            int portSeparatorIndex = host.indexOf(':');
            if (portSeparatorIndex >= 0 && portSeparatorIndex + 1 < host.length()) {
                return host.substring(portSeparatorIndex + 1);
            }
        }
        return "8080";
    }

    private String camelizePath(String path) {
        String word = path;
        Pattern pattern = Pattern.compile("\\{([^/]*)\\}");
        Matcher matcher = pattern.matcher(word);
        while (matcher.find()) {
            word = matcher.replaceFirst(":" + matcher.group(1));
            matcher = pattern.matcher(word);
        }
        pattern = Pattern.compile("(_)(.)");
        matcher = pattern.matcher(word);
        while (matcher.find()) {
            word = matcher.replaceFirst(matcher.group(2).toUpperCase(Locale.ROOT));
            matcher = pattern.matcher(word);
        }
        return word;
    }
}
