package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class KotlinVertxServerCodegen extends AbstractJavaCodegen {

    protected String rootPackage = "org.openapitools.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    public static final String ROOT_PACKAGE = "rootPackage";

    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(KotlinVertxServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "kotlin-vertx";
    }

    public String getHelp() {
        return "Generates a kotlin-vertx server.";
    }

    public KotlinVertxServerCodegen() {
        super();

        importMapping.remove("JsonCreator");
        importMapping.remove("com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonInclude", "com.fasterxml.jackson.annotation.JsonInclude");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");

        outputFolder = "generated-code" + File.separator + "kotlin-vertx";
        modelTemplateFiles.put("model.mustache", ".java");

        apiTestTemplateFiles.clear();
        modelDocTemplateFiles.clear();
        apiDocTemplateFiles.clear();
        supportingFiles.clear();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("api.mustache", ".kt");
        apiTemplateFiles.put("apiVerticle.mustache", "Verticle.kt");
        apiTemplateFiles.put("apiError.mustache","Error.java");
//        apiTemplateFiles.put("apiException.mustache", "Exception.java");

        this.setDateLibrary("java8");

        embeddedTemplateDir = templateDir = "kotlin-vertx-server";
        apiPackage = rootPackage + ".verticle";
        modelPackage = rootPackage + ".model";
        artifactId = "openapi-java-vertx-server";
        artifactVersion = apiVersion;
//        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        updateOption(CodegenConstants.ARTIFACT_ID, this.getArtifactId());
        updateOption(CodegenConstants.ARTIFACT_VERSION, this.getArtifactVersion());
        updateOption(CodegenConstants.API_PACKAGE, apiPackage);
        updateOption(CodegenConstants.MODEL_PACKAGE, modelPackage);
        updateOption(this.DATE_LIBRARY, this.getDateLibrary());
        additionalProperties.put(ROOT_PACKAGE, rootPackage);
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
        URL url = URLPathUtils.getServerURL(openAPI);
        this.additionalProperties.put("serverPort", URLPathUtils.getPort(url, 8080));

        Map<String, PathItem> paths = openAPI.getPaths();
        if (paths != null) {
            for (Map.Entry<String, PathItem> entry : paths.entrySet()) {
                manageOperationNames(entry.getValue(), entry.getKey());
            }
        }
        this.additionalProperties.remove("gson");
    }

    private void manageOperationNames(PathItem path, String pathname) {
        String serviceIdTemp;

        Map<PathItem.HttpMethod, Operation> operationMap = path.readOperationsMap();
        if (operationMap != null) {
            for (Map.Entry<PathItem.HttpMethod, Operation> entry : operationMap.entrySet()) {
                serviceIdTemp = computeServiceId(pathname, entry);
//                if(entry.getValue().getRequestBody()!=null){
//                    entry.getValue().addExtension("body",entry.getValue().getRequestBody());
//                }
                entry.getValue().addExtension("x-serviceid", serviceIdTemp);
                entry.getValue().addExtension("x-serviceid-varname",
                        serviceIdTemp.toUpperCase(Locale.ROOT) + "_SERVICE_ID");
            }
        }
    }

    private String computeServiceId(String pathname, Map.Entry<PathItem.HttpMethod, Operation> entry) {
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
