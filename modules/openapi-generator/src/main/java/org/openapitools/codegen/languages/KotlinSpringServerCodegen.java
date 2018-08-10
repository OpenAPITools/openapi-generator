package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;

/**
 * TODO Config:
 * - handle "INVOKER_PACKAGE" and "HIDE_GENERATION_TIMESTAMP"
 * - integrate Spring Fox
 * TODO Model generation:
 * - serializableModel by implementing java.io.Serializable
 * - enable optional bean validation using javax.validation.Valid (currently must be valid)
 * TODO Controller generation:
 * - Handle implicit headers, also within api.mustache
 * - Handle tags
 * - If we handle optional, decide to make use of Java Optional, or Kotlin?
 */
public class KotlinSpringServerCodegen extends AbstractKotlinCodegen {

    private static Logger LOGGER =
            LoggerFactory.getLogger(KotlinSpringServerCodegen.class);

    private static final HashSet<String> VARIABLE_RESERVED_WORDS =
            new HashSet<String>(Arrays.asList(
                    "ApiClient",
                    "ApiException",
                    "ApiResponse"
            ));

    public static final String TITLE = "title";
    public static final String SERVER_PORT = "serverPort";
    public static final String BASE_PACKAGE = "basePackage";
    public static final String CONFIG_PACKAGE = "configPackage";
    public static final String SPRING_BOOT = "spring-boot";

    protected String resourceFolder = "src/main/resources";

    protected String basePackage;
    protected String configPackage;
    protected String serverPort = "8080";
    protected String title = "OpenAPI Kotlin Spring";

    public KotlinSpringServerCodegen() {
        super();

        reservedWords.addAll(VARIABLE_RESERVED_WORDS);

        outputFolder = "generated-code/kotlin-spring";
        apiTestTemplateFiles.clear(); // TODO: add test template
        embeddedTemplateDir = templateDir = "kotlin-spring";

        artifactId = "openapi-spring";
        basePackage = "org.openapitools";
        apiPackage = "org.openapitools.api";
        modelPackage = "org.openapitools.model";
        configPackage = "org.openapitools.conf";

        // spring uses the jackson lib
        additionalProperties.put("jackson", "true");

        addOption(TITLE, "server title name or client service name", title);
        addOption(BASE_PACKAGE, "base package for generated code", basePackage);
        addOption(CONFIG_PACKAGE, "configuration package for generated code", configPackage);
        addOption(SERVER_PORT, "configuration the port in which the sever is to run on", serverPort);
        addOption(CodegenConstants.MODEL_PACKAGE, "model package for generated code", modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "api package for generated code", apiPackage);

        supportedLibraries.put(SPRING_BOOT, "Spring-boot Server application.");
        setLibrary(SPRING_BOOT);

        CliOption cliOpt = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        cliOpt.setDefault(SPRING_BOOT);
        cliOpt.setEnum(supportedLibraries);
        cliOptions.add(cliOpt);

        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-spring";
    }

    @Override
    public String getHelp() {
        return "Generates a Kotlin Spring application.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // optional jackson mappings for BigDecimal support
        importMapping.put("ToStringSerializer", "com.fasterxml.jackson.databind.ser.std.ToStringSerializer");
        importMapping.put("JsonSerialize", "com.fasterxml.jackson.databind.annotation.JsonSerialize");

        // Java import mappings
        importMapping.put("Objects", "java.util.Objects");
        importMapping.put("IOException", "java.io.IOException");

        // Swagger import mappings
        importMapping.put("ApiModel", "io.swagger.annotations.ApiModel");
        importMapping.put("ApiModelProperty", "io.swagger.annotations.ApiModelProperty");

        // Jackson import mappings
        importMapping.put("JsonValue", "com.fasterxml.jackson.annotation.JsonValue");
        importMapping.put("JsonCreator", "com.fasterxml.jackson.annotation.JsonCreator");
        importMapping.put("SerializedName", "com.google.gson.annotations.SerializedName");
        importMapping.put("JsonProperty", "com.fasterxml.jackson.annotation.JsonProperty");
        importMapping.put("JsonSubTypes", "com.fasterxml.jackson.annotation.JsonSubTypes");
        importMapping.put("JsonTypeInfo", "com.fasterxml.jackson.annotation.JsonTypeInfo");
        // import JsonCreator if JsonProperty is imported
        // used later in recursive import in postProcessingModels
        importMapping.put("com.fasterxml.jackson.annotation.JsonProperty", "com.fasterxml.jackson.annotation.JsonCreator");

        // TODO when adding invokerPackage
        //importMapping.put("StringUtil", invokerPackage + ".StringUtil");

        if (!additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            additionalProperties.put(CodegenConstants.LIBRARY, library);
        }

        if (additionalProperties.containsKey(BASE_PACKAGE)) {
            this.setBasePackage((String) additionalProperties.get(BASE_PACKAGE));
        } else {
            additionalProperties.put(BASE_PACKAGE, basePackage);
        }

        if (additionalProperties.containsKey(SERVER_PORT)) {
            this.setServerPort((String) additionalProperties.get(SERVER_PORT));
        } else {
            additionalProperties.put(SERVER_PORT, serverPort);
        }

        if (additionalProperties.containsKey(CONFIG_PACKAGE)) {
            this.setConfigPackage((String) additionalProperties.get(CONFIG_PACKAGE));
        } else {
            additionalProperties.put(CONFIG_PACKAGE, configPackage);
        }

        supportingFiles.add(new SupportingFile("buildGradleKts.mustache", "", "build.gradle.kts"));
        supportingFiles.add(new SupportingFile("settingsGradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("application.mustache", resourceFolder, "application.yaml"));

        if (library.equals(SPRING_BOOT)) {
            supportingFiles.add(new SupportingFile("openapi2SpringBoot.mustache",
                    sanitizeDirectory(sourceFolder + File.separator + basePackage), "Application.kt"));
        }

        // add lambda for mustache templates
        additionalProperties.put("lambdaEscapeDoubleQuote",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\"", Matcher.quoteReplacement("\\\""))));
        additionalProperties.put("lambdaRemoveLineBreak",
                (Mustache.Lambda) (fragment, writer) -> writer.write(fragment.execute().replaceAll("\\r|\\n", "")));
    }

    public String getBasePackage() {
        return this.basePackage;
    }

    public void setBasePackage(String basePackage) {
        this.basePackage = basePackage;
    }

    public String getConfigPackage() {
        return this.configPackage;
    }

    public void setConfigPackage(String configPackage) {
        this.configPackage = configPackage;
    }

    public String getServerPort() {
        return this.serverPort;
    }

    public void setServerPort(String serverPort) {
        this.serverPort = serverPort;
    }

    @Override
    public String toModelName(final String name) {
        // TODO This logic should be cleanup and moved to parent `AbstractKotlinCodegen`. See TODO in super.escapeReservedWord()
        final String preProcessedName = super.toModelName(name);

        final String nameNoTicks = preProcessedName.replace("`", "");
        if (VARIABLE_RESERVED_WORDS.contains(nameNoTicks)) {
            final String modelName = "Model" + nameNoTicks;
            LOGGER.warn(nameNoTicks + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }
        return preProcessedName;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        /* TODO the following logic should not need anymore in OAS 3.0
        if ("/".equals(swagger.getBasePath())) {
            swagger.setBasePath("");
        }
        */

        if (!additionalProperties.containsKey(TITLE)) {
            // From the title, compute a reasonable name for the package and the API
            String title = openAPI.getInfo().getTitle();

            // Drop any API suffix
            if (title != null) {
                title = title.trim().replace(" ", "-");
                if (title.toUpperCase().endsWith("API"))
                    title = title.substring(0, title.length() - 3);

                this.title = camelize(sanitizeName(title), true);
            }
            additionalProperties.put(TITLE, this.title);
        }

        if (!additionalProperties.containsKey(SERVER_PORT)) {
            URL url = URLPathUtils.getServerURL(openAPI);
            this.additionalProperties.put(SERVER_PORT, URLPathUtils.getPort(url, 8080));
        }

        // TODO: Handle tags
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if ("null".equals(property.example))
            property.example = null;

        //Add imports for Jackson
        if (!Boolean.TRUE.equals(model.isEnum)) {
            model.imports.add("JsonProperty");
            if (Boolean.TRUE.equals(model.hasEnums))
                model.imports.add("JsonValue");

        } else {
            //Needed imports for Jackson's JsonCreator
            if (additionalProperties.containsKey("jackson"))
                model.imports.add("JsonCreator");
        }
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        //Add imports for Jackson
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        List<Object> models = (List<Object>) objs.get("models");

        models.stream()
                .map(mo -> (Map<String, Object>) mo)
                .map(mo -> (CodegenModel) mo.get("model"))
                .filter(cm -> Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null)
                .forEach(cm -> {
                    cm.imports.add(importMapping.get("JsonValue"));
                    Map<String, String> item = new HashMap<>();
                    item.put("import", importMapping.get("JsonValue"));
                    imports.add(item);
                });

        return objs;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            ops.forEach(operation -> {
                List<CodegenResponse> responses = operation.responses;
                if (responses != null) {
                    responses.forEach(resp -> {

                        if ("0".equals(resp.code))
                            resp.code = "200";

                        doDataTypeAssignment(resp.dataType, new DataTypeAssigner() {
                            @Override
                            public void setReturnType(final String returnType) {
                                resp.dataType = returnType;
                            }

                            @Override
                            public void setReturnContainer(final String returnContainer) {
                                resp.containerType = returnContainer;
                            }
                        });
                    });
                }
                doDataTypeAssignment(operation.returnType, new DataTypeAssigner() {

                    @Override
                    public void setReturnType(final String returnType) {
                        operation.returnType = returnType;
                    }

                    @Override
                    public void setReturnContainer(final String returnContainer) {
                        operation.returnContainer = returnContainer;
                    }
                });
//                if(implicitHeaders){
//                    removeHeadersFromAllParams(operation.allParams);
//                }
            });
        }

        return objs;
    }

    private interface DataTypeAssigner {
        void setReturnType(String returnType);

        void setReturnContainer(String returnContainer);
    }

    /**
     * @param returnType       The return type that needs to be converted
     * @param dataTypeAssigner An object that will assign the data to the respective fields in the model.
     */
    private void doDataTypeAssignment(final String returnType, DataTypeAssigner dataTypeAssigner) {
        if (returnType == null) {
            dataTypeAssigner.setReturnType("Unit");
        } else if (returnType.startsWith("kotlin.Array")) {
            int end = returnType.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring("kotlin.Array<".length(), end).trim());
                dataTypeAssigner.setReturnContainer("List");
            }
        } else if (returnType.startsWith("kotlin.collections.Map")) {
            int end = returnType.lastIndexOf(">");
            if (end > 0) {
                dataTypeAssigner.setReturnType(returnType.substring("kotlin.collections.Map<".length(), end).split(",")[1].trim());
                dataTypeAssigner.setReturnContainer("Map");
            }
        }
    }

    private static String sanitizeDirectory(String in) {
        return in.replace(".", File.separator);
    }
}
