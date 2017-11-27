package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.utils.URLPathUtil;
import io.swagger.oas.models.OpenAPI;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.PathItem;
import io.swagger.oas.models.media.Schema;
import io.swagger.util.Json;

import java.io.File;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static io.swagger.codegen.CodegenModel.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class JavaVertXServerCodegen extends AbstractJavaCodegen {

    protected String resourceFolder = "src/main/resources";
    protected String rootPackage = "io.swagger.server.api";
    protected String apiVersion = "1.0.0-SNAPSHOT";

    public static final String ROOT_PACKAGE = "rootPackage";

    public static final String RX_INTERFACE_OPTION = "rxInterface";
    public static final String VERTX_SWAGGER_ROUTER_VERSION_OPTION = "vertxSwaggerRouterVersion";

    /**
     * A Java Vert.X generator. It uses java8 date API. It can be configured with 2 CLI options :
     * 
     * rxInterface : type Boolean if true, API interfaces are generated with RX and methods return
     * Single and Comparable. default : false
     * 
     * vertxSwaggerRouterVersion : type String Specify the version of the swagger router library
     */
    public JavaVertXServerCodegen() {
        super();

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

        additionalProperties.put(ROOT_PACKAGE, rootPackage);

        groupId = "io.swagger";
        artifactId = "swagger-java-vertx-server";
        artifactVersion = apiVersion;

        this.setDateLibrary("java8");

        cliOptions.add(CliOption.newBoolean(RX_INTERFACE_OPTION,
                "When specified, API interfaces are generated with RX "
                        + "and methods return Single<> and Comparable."));
        cliOptions.add(CliOption.newString(VERTX_SWAGGER_ROUTER_VERSION_OPTION,
                "Specify the version of the swagger router library"));

    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the generator to select
     * the library with the -l flag.
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
        supportingFiles.add(new SupportingFile("swagger.mustache", resourceFolder, "swagger.json"));
        supportingFiles.add(new SupportingFile("MainApiVerticle.mustache",
                sourceFolder + File.separator + rootPackage.replace(".", File.separator),
                "MainApiVerticle.java"));
        supportingFiles.add(new SupportingFile("MainApiException.mustache",
                sourceFolder + File.separator + rootPackage.replace(".", File.separator),
                "MainApiException.java"));

        writeOptional(outputFolder, new SupportingFile("vertx-default-jul-logging.mustache",
                resourceFolder, "vertx-default-jul-logging.properties"));
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        boolean isEnum = getBooleanValue(model.getVendorExtensions(), IS_ENUM_EXT_NAME);
        if (!isEnum) {
            model.imports.add("JsonInclude");
            model.imports.add("JsonProperty");
            if (model.hasEnums) {
                model.imports.add("JsonValue");
            }
        }

    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> newObjs = super.postProcessOperations(objs);
        Map<String, Object> operations = (Map<String, Object>) newObjs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                operation.httpMethod = operation.httpMethod.toLowerCase();

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
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, Map<String, Schema> definitions, OpenAPI openAPI) {
        CodegenOperation codegenOperation = super.fromOperation(path, httpMethod, operation, definitions, openAPI);
        codegenOperation.imports.add("MainApiException");
        return codegenOperation;
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema, Map<String, Schema> allSchemas) {
        CodegenModel codegenModel = super.fromModel(name, schema, allSchemas);
        codegenModel.imports.remove("ApiModel");
        codegenModel.imports.remove("ApiModelProperty");
        return codegenModel;

    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        // add full swagger definition in a mustache parameter
        String swaggerDef = Json.pretty(openAPI);
        this.additionalProperties.put("fullSwagger", swaggerDef);

        // add server port from the swagger file, 8080 by default
        String port = null;
        final URL url = URLPathUtil.getServerURL(openAPI);
        if(url != null) {
            port = String.valueOf(url.getPort());
        }  else {
            port = "8080";
        }
        this.additionalProperties.put("serverPort", port);

        // retrieve api version from swagger file, 1.0.0-SNAPSHOT by default
        if (openAPI.getInfo() != null && openAPI.getInfo().getVersion() != null) {
            artifactVersion = apiVersion = openAPI.getInfo().getVersion();
        } else {
            artifactVersion = apiVersion;
        }

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

        Map<PathItem.HttpMethod, Operation> operationMap = path.readOperationsMap();
        if (operationMap != null) {
            for (Entry<PathItem.HttpMethod, Operation> entry : operationMap.entrySet()) {
                serviceIdTemp = computeServiceId(pathname, entry);
                entry.getValue().addExtension("x-serviceid", serviceIdTemp);
                entry.getValue().addExtension("x-serviceid-varname", serviceIdTemp.toUpperCase() + "_SERVICE_ID");
            }
        }
    }

    private String computeServiceId(String pathname, Entry<PathItem.HttpMethod, Operation> entry) {
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
            word = matcher.replaceFirst(matcher.group(2).toUpperCase());
            matcher = pattern.matcher(word);
        }
        return word;
    }
}
